(*
    "The contents of this file are subject to the Mozilla Public License
    Version 1.1 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at
    http://www.mozilla.org/MPL/

    Software distributed under the License is distributed on an "AS IS"
    basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
    License for the specific language governing rights and limitations
    under the License.

    The Initial Developer of the Original Code is
      Henri Gourvest <hgourvest@progdigy.com>.
*)

unit PDGHTTPStub;
{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
{$I PDGAppServer.inc}
interface
uses PDGSocketStub, {$IFDEF FPC}sockets,{$ELSE}Winsock,{$ENDIF} PDGUtils, classes, json;

type

  THttpMethod = (mUNKNOW, mOPTIONS, mGET, mHEAD, mPOST, mPUT, mDELETE, mTRACE, mCONNECT);

  TConnectionField = (conClose, conKeepAlive);

  THTTPMessage = class(TJsonObject)
  private
    FContent: TPooledMemoryStream;
    function GetContentString: string;
  public
    property Content: TPooledMemoryStream read FContent;
    property ContentString: string read GetContentString;

    constructor Create(jt: TJsonType = json_type_object); override;
    destructor Destroy; override;

    procedure Clear; override;
  end;

  THTTPStub = class(TSocketStub)
  private
    FRequest: THTTPMessage;
    FResponse: THTTPMessage;
    FMVC: TJsonObject;
    function DecodeFields(str: PChar): boolean;
    function DecodeCommand(str: PChar): boolean;
  protected
    function CreateMVC: TJsonObject; virtual;
    function DecodeContent: boolean; virtual;
    procedure doBeforeProcessRequest(ctx: TJsonObject); virtual;
    procedure doAfterProcessRequest(ctx: TJsonObject); virtual;
    procedure ProcessRequest(ctx: TJsonObject); virtual;
  public
    property Request: THTTPMessage read FRequest;
    property Response: THTTPMessage read FResponse;
    property MVC: TJsonObject read FMVC;
    function Run: Cardinal; override;
    procedure WriteLine(str: string);
    procedure WriteString(const str: string);
    procedure SendEmpty;
    procedure SendFile(const filename: string);
    procedure SendStream(Stream: TStream);
    procedure SendString(const data: string);
    constructor CreateStub(AOwner: TSocketServer; ASocket: longint; AAddress: TSockAddr); override;
    destructor Destroy; override;
  end;

const
  CR = #13;
  LF = #10;
  SP = #32; // space
  HT = #9;  // backspace
  NL = #0;  // NULL
  SL = '/';
  PT = '.';
  CRLF = CR+LF;

//  HttpResponseStrings : array[THttpResponseCode] of string = (
//    '100 Continue',
//    '101 Switching Protocols',
//
//    '200 OK',
//    '201 Created',
//    '202 Accepted',
//    '203 Non-Authoritative Information',
//    '204 No Content',
//    '205 Reset Content',
//    '206 Partial Content',
//
//    '300 Multiple Choices',
//    '301 Moved Permanently',
//    '302 Found',
//    '303 See Other',
//    '304 Not Modified',
//    '305 Use Proxy',
//    '306 unused',
//    '307 Temporary Redirect',
//    '400 Bad Request',
//    '401 Authorization Required',
//    '402 Payment Required',
//    '403 Forbidden',
//    '404 Not Found',
//    '405 Method Not Allowed',
//    '406 Not Acceptable',
//    '407 Proxy Authentication Required',
//    '408 Request Time-out',
//    '409 Conflict',
//    '410 Gone',
//    '411 Length Required',
//    '412 Precondition Failed',
//    '413 Request Entity Too Large',
//    '414 Request-URI Too Large',
//    '415 Unsupported Media Type',
//    '416 Requested Range Not Satisfiable',
//    '417 Expectation Failed',
//
//    '500 Internal Server Error',
//    '501 Method Not Implemented',
//    '502 Bad Gateway',
//    '503 Service Temporarily Unavailable',
//    '504 Gateway Time-out',
//    '505 HTTP Version Not Supported'
//  );

const
(* default limit on bytes in Request-Line (Method+URI+HTTP-version) *)
  DEFAULT_LIMIT_REQUEST_LINE = 8190;
(* default limit on bytes in any one header field  *)
  DEFAULT_LIMIT_REQUEST_FIELDSIZE = 8190;
(* default limit on number of request header fields *)
  DEFAULT_LIMIT_REQUEST_FIELDS = 100;

function HTTPInterprete(src: PChar; named: boolean = false; sep: char = ';'): TJsonObject;
function HTTPDecode(const AStr: String): String;

implementation
uses SysUtils, StrUtils;

function HTTPInterprete(src: PChar; named: boolean; sep: char): TJsonObject;
var
  strlist: TStringList;
  j: integer;
begin
  strlist := TStringList.Create;
  try
    strlist.Delimiter := sep;
    strlist.DelimitedText := src;
    if named then
    begin    
      Result := TJsonObject.Create(json_type_object);
      //with Result.AsObject do
        for j := 0 to strlist.Count - 1 do
          Result.S[HTTPDecode(strlist.Names[j])] := PChar(trim(HTTPDecode(strlist.ValueFromIndex[j])));
    end else
    begin
      Result := TJsonObject.Create(json_type_array);
      with Result.AsArray do
        for j := 0 to strlist.Count - 1 do
          Add(TJsonObject.Create(trim(HTTPDecode(strlist.Strings[j]))));
    end;
  finally
    strlist.Free;
  end;
end;

function HTTPDecode(const AStr: String): String;
var
  Sp, Rp, Cp: PChar;
  S: String;
begin
  SetLength(Result, Length(AStr));
  Sp := PChar(AStr);
  Rp := PChar(Result);
  while Sp^ <> #0 do
  begin
    case Sp^ of
      '+': Rp^ := ' ';
      '%': begin
             Inc(Sp);
             if Sp^ = '%' then
               Rp^ := '%'
             else
             begin
               Cp := Sp;
               Inc(Sp);
               if (Cp^ <> #0) and (Sp^ <> #0) then
               begin
                 S := '$' + Cp^ + Sp^;
                 Rp^ := Chr(StrToInt(S));
               end
               else
               begin
                 Result := '';
                 Exit;
               end;
             end;
           end;
    else
      Rp^ := Sp^;
    end;
    Inc(Rp);
    Inc(Sp);
  end;
  SetLength(Result, Rp - PChar(Result));
end;

function HTTPGetAuthorization(str: string): TJsonObject;
var
  i: integer;
begin
  Result := nil;
  if str <> '' then
  begin
    i := pos('Basic ', str);
    if i = 1  then
    begin
      str := Base64ToStr(copy(str, 7, Length(str) - 6));
      i := pos(':', str);
      if i > 0 then
      begin
        Result := TJsonObject.Create;
        Result.AsObject.Put('user', TJsonObject.Create(copy(str, 1, i-1)));
        Result.AsObject.Put('pass', TJsonObject.Create(copy(str, i+1, Length(str)-i)));
      end;
    end;
  end;
end;

{ THTTPMessage }

procedure THTTPMessage.Clear;
begin
  inherited;
  FContent.Clear;
end;

constructor THTTPMessage.Create(jt: TJsonType = json_type_object);
begin
  Inherited create(jt);
  FContent := TPooledMemoryStream.Create;
end;

destructor THTTPMessage.Destroy;
begin
  inherited;
  FContent.Free;
end;

function THTTPMessage.GetContentString: string;
begin
  Result := StreamToString(FContent);
end;

{ THTTPStub }

function THTTPStub.DecodeFields(str: PChar): boolean;
var
  p: PChar;
  prop: string;
begin
  p := StrScan(str, ':');
  if p = nil then
    Result := false else
    with FRequest['@env'] do
    begin
      prop := LowerCase(Copy(str, 1, p-str));
      AsObject.Put(PChar(prop), TJsonObject.Create(p+2));
      Result := true;
    end;
end;

function THTTPStub.DecodeContent: boolean;
var
  ContentLength: integer;
begin
  ContentLength := FRequest.I['env.content-length'];
  if ContentLength > 0 then
  begin
    FRequest.FContent.Size := ContentLength;
    FRequest.FContent.LoadFromSocket(SocketHandle, false);
  end;
  result := true;
end;

function THTTPStub.DecodeCommand(str: PChar): boolean;
  function DecodeURI(uri: PChar; len: integer; out data: string): boolean;
  const hexcodes = ['0'..'9', 'A'..'F', 'a'..'f'];
  var i: integer;
  begin
    data := '';
    while len > 0 do
    begin
      if (uri^ = '%') then
      begin
        // PARANOIA !!
        if (len > 2) and
          (uri[1] in hexcodes) and
          (uri[2] in hexcodes) and
          TryStrToInt('$' + uri[1] + uri[2], i) and
          (i in [32..255]) then
            begin
              data := data + char(i);
              inc(uri, 3);
              dec(len, 3);
            end else
            begin
              Result := False;
              Exit;
            end;
      end else
      begin
        data := data + uri^;
        inc(uri, 1);
        dec(len, 1);
      end;
    end;
    Result := true;
  end;
var
  marker: PChar;
  param, value: string;
  i: integer;
begin
  result := false;
  marker := StrScan(str, SP);
  if marker = nil then exit;
  FRequest.s['method'] := PChar(copy(str, 0, marker - str));
  str := marker;

  // SP
  if (str^ <> SP) then
    exit;

  // URI
  inc(str);
  marker := Str;
  while not (Str^ in [SP, NL, '?']) do
    inc(str);
  if (str > marker) and (str^ <> NL) then
  begin
    if DecodeURI(marker, str - marker, value) then
      FRequest.s['uri'] := PChar(value) else
      exit;
  end else
    exit;

  // decode parametters
  if str^ = '?' then
  begin
    inc(str);
    marker := Str;
    param := '';
    value := '';
    while true do
      case str^ of
        '&', SP, NL: begin
               if (param <> '') and (str > marker) then
               begin
                 if not DecodeURI(marker, str - marker, value) then exit;
                 FRequest['@params'].s[HTTPDecode(param)] := PChar(HTTPDecode(value));
               end;
               if (str^ in [SP, NL]) then
                 Break;
               param := '';
               value := '';
               inc(Str);
               marker := Str;
             end;
        '=': begin
               if (str > marker) then
                 if not DecodeURI(marker, str - marker, param) then
                   Exit;
               inc(Str);
               marker := Str;
             end;
      else
        inc(Str);
        continue;
      end;

  end;

  // SP expected
  if (str^ <> SP) then exit;

  // HTTP/
  inc(str);
  if not ((str[0] = 'H') and (str[1] = 'T') and (str[2] = 'T') and
     (str[3] = 'P') and (str[4] = SL)) then
    exit;
  str := @str[5];

  // version major
  marker := str;
  while (str^ in ['0'..'9']) do inc(str);
  if (str > marker) and (str^ <> NL) then
  begin
    if TryStrToInt(copy(marker, 0, str - marker), i) then
      FRequest.I['http-version.major'] := i else
      exit;
  end else
    exit;

  // .
  if (str^ <> PT) then
    exit;
  inc(str);

  // version minor
  marker := str;
  while (str^ in ['0'..'9']) do inc(str);
  if (str > marker) then
  begin
    if TryStrToInt(copy(marker, 0, str - marker), i) then
      FRequest.I['http-version.minor']  := i else
      exit;
  end else
    exit;

  if (str^ <> NL) then exit;

  result := true;
end;

function THTTPStub.Run: Cardinal;
var
  buffer: string;
  cursor, line, len: integer;
  c: char;
  ctx: TJsonObject;
begin
  result := 0;
  cursor := 0;
  len := 0;
  line := 0;
  while not Stopped do
  begin
    inc(cursor);
    if cursor > len then
    begin
      inc(len, 255);
      SetLength(buffer, len);
    end;

    // check sizes
    if ((line = 0) and (cursor >= DEFAULT_LIMIT_REQUEST_LINE)) or
       ((line > 0) and (cursor >= DEFAULT_LIMIT_REQUEST_FIELDSIZE)) or
       (line > DEFAULT_LIMIT_REQUEST_FIELDS) then
      Exit;

    if receive(SocketHandle, c, 1, 0) <> 1 then exit;
    case c of
    CR: dec(cursor){->LF};
    LF: begin
          if cursor = 1 then
          begin
            if not DecodeContent then
              exit;

            try
              ctx := TJsonObject.Create;
              try
                doBeforeProcessRequest(ctx);
                try
                  ProcessRequest(ctx); // <<<<<<<<<<<<<<<
                finally
                  doAfterProcessRequest(ctx);
                end;
              finally
                ctx.Free;
              end;
            except
              on E: Exception do
              begin
              {$ifdef madExcept}
                HandleException(etNormal, E);
              {$endif}
              end;
            end;

            line := 0;
            cursor := 0;
          end else
          begin
            buffer[cursor] := NL;
            if line = 0 then
            begin
              if not DecodeCommand(Pointer(Buffer)) then
                exit;
            end else
            begin
              if not DecodeFields(Pointer(Buffer)) then
                exit;
            end;
            cursor := 0;
            inc(line);
          end;
        end;
    else
      buffer[cursor] := c;
    end;
  end;
end;

function THTTPStub.CreateMVC: TJsonObject;
begin
  Result := TJsonObject.Create;
end;

constructor THTTPStub.CreateStub(AOwner: TSocketServer; ASocket: longint;
  AAddress: TSockAddr);
begin
  inherited;
  FRequest := THTTPMessage.Create;
  FResponse := THTTPMessage.Create;
  FMVC := CreateMVC;
end;

destructor THTTPStub.Destroy;
begin
  FRequest.Free;
  FResponse.Free;
  FMVC.Free;
  inherited;
end;

procedure THTTPStub.doAfterProcessRequest(ctx: TJsonObject);
var
  ite: TJsonObjectIter;
begin
   WriteLine('HTTP/1.1 ' + Response.S['response']);
   if JsonFindFirst(Response['env'], ite) then
   repeat
     WriteLine(ite.key + ': ' + ite.val.AsString);
   until not JsonFindNext(ite);
   JsonFindClose(ite);

   if Response['sendfile'] <> nil then
     SendFile(Response.S['sendfile']) else
     SendStream(Response.Content);

   Response.Clear;
   Request.Clear;
end;

procedure THTTPStub.doBeforeProcessRequest(ctx: TJsonObject);
begin
  FRequest['cookies'] := HTTPInterprete(Request.S['env.cookie'], true);
  FRequest['content-type'] := HTTPInterprete(Request.S['env.content-type']);
  FRequest['authorization'] := HTTPGetAuthorization(Request.S['env.authorization']);
  FRequest['accept'] := HTTPInterprete(Request.S['env.accept'], false, ',');


  FResponse.I['response'] :=  200;

  FRequest.AddRef;
  ctx['request'] := FRequest;
  FResponse.AddRef;
  ctx['response'] := FResponse;
end;

procedure THTTPStub.ProcessRequest(ctx: TJsonObject);
begin

end;

procedure THTTPStub.SendEmpty;
begin
  WriteLine('Content-Length: 0');
  WriteLine('');
end;

procedure THTTPStub.SendFile(const filename: string);
var
  stream: TFileStream;
begin
  if FileExists(filename) then
  begin
    stream := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
    try
      SendStream(stream);
    finally
      stream.Free;
    end;
  end else
    SendEmpty;
end;

procedure THTTPStub.SendString(const data: string);
begin
  WriteLine(format('Content-Length: %d', [length(data)]));
  WriteLine('');
  WriteString(data);
end;

procedure THTTPStub.WriteLine(str: string);
begin
  str := str + CRLF;
  send(SocketHandle, PChar(str)^, length(str), 0);
end;

procedure THTTPStub.WriteString(const str: string);
begin
  send(SocketHandle, PChar(str)^, length(str), 0);
end;

procedure THTTPStub.SendStream(Stream: TStream);
var
  size: Integer;
  buffer: array[0..1023] of byte;
begin
  WriteLine(format('Content-Length: %d', [stream.size]));
  WriteLine('');
  Stream.Seek(0, soFromBeginning);
  size := stream.Read(buffer, sizeof(buffer));
  while size > 0 do
  begin
    send(SocketHandle, buffer, size, 0);
    size := stream.Read(buffer, sizeof(buffer));
  end;
end;

end.


