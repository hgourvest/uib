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
  THttpResponseCode = (rc100, rc101, rc200, rc201, rc202, rc203, rc204, rc205,
    rc206, rc300, rc301, rc302, rc303, rc304, rc305, rc306, rc307, rc400, rc401,
    rc402, rc403, rc404, rc405, rc406, rc407, rc408, rc409, rc410, rc411, rc412,
    rc413, rc414, rc415, rc416, rc417, rc500, rc501, rc502, rc503, rc504, rc505);

  THttpMethod = (mUNKNOW, mOPTIONS, mGET, mHEAD, mPOST, mPUT, mDELETE, mTRACE, mCONNECT);

  TConnectionField = (conClose, conKeepAlive);

  THTTPRequest = class(TJsonObject)
  private
    FContent: TPooledMemoryStream;
    function GetContentString: string;
  public
    property Content: TPooledMemoryStream read FContent;
    property ContentString: string read GetContentString;
    function GetAuthorization(out user, pass: string): boolean;
    function GetCookies(const name: string): string;
    procedure Clear;
    constructor Create(jt: TJsonType = json_type_object); override; 
    destructor Destroy; override;
  end;

  THTTPStub = class(TSocketStub)
  private
    FHeader: THTTPRequest;
    function DecodeFields(str: PChar): boolean;
    function DecodeCommand(str: PChar): boolean;
  protected
    function DecodeContent: boolean; virtual;
    procedure ProcessRequest; virtual;
  public
    property Header: THTTPRequest read FHeader;
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

  HttpResponseStrings : array[THttpResponseCode] of string = (
    '100 Continue',
    '101 Switching Protocols',

    '200 OK',
    '201 Created',
    '202 Accepted',
    '203 Non-Authoritative Information',
    '204 No Content',
    '205 Reset Content',
    '206 Partial Content',

    '300 Multiple Choices',
    '301 Moved Permanently',
    '302 Found',
    '303 See Other',
    '304 Not Modified',
    '305 Use Proxy',
    '306 unused',
    '307 Temporary Redirect',
    '400 Bad Request',
    '401 Authorization Required',
    '402 Payment Required',
    '403 Forbidden',
    '404 Not Found',
    '405 Method Not Allowed',
    '406 Not Acceptable',
    '407 Proxy Authentication Required',
    '408 Request Time-out',
    '409 Conflict',
    '410 Gone',
    '411 Length Required',
    '412 Precondition Failed',
    '413 Request Entity Too Large',
    '414 Request-URI Too Large',
    '415 Unsupported Media Type',
    '416 Requested Range Not Satisfiable',
    '417 Expectation Failed',

    '500 Internal Server Error',
    '501 Method Not Implemented',
    '502 Bad Gateway',
    '503 Service Temporarily Unavailable',
    '504 Gateway Time-out',
    '505 HTTP Version Not Supported'
  );

const
(* default limit on bytes in Request-Line (Method+URI+HTTP-version) *)
  DEFAULT_LIMIT_REQUEST_LINE = 8190;
(* default limit on bytes in any one header field  *)
  DEFAULT_LIMIT_REQUEST_FIELDSIZE = 8190;
(* default limit on number of request header fields *)
  DEFAULT_LIMIT_REQUEST_FIELDS = 100;

implementation
uses SysUtils, StrUtils;

{ THTTPRequest }

procedure THTTPRequest.Clear;
begin
  if IsType(json_type_object) then
    AsObject.Clear;
  inherited;
end;

constructor THTTPRequest.Create(jt: TJsonType = json_type_object);
begin
  Inherited create(jt);
  FContent := TPooledMemoryStream.Create;
  Clear;
end;

destructor THTTPRequest.Destroy;
begin
  inherited;
  FContent.Free;
end;

function THTTPRequest.GetAuthorization(out user, pass: string): boolean;
var
  str: string;
  i: integer;
begin
  Result := False;
  str := s['Authorization'];
  if str <> '' then
  begin
    i := pos('Basic ', str);
    if i = 1  then
    begin
      str := Base64ToStr(copy(str, 7, Length(str) - 6));
      i := pos(':', str);
      if i > 0 then
      begin
        user := copy(str, 1, i-1);
        pass := copy(str, i+1, Length(str)-i);
        Result := True;
      end;
    end;
  end;
end;

function THTTPRequest.GetContentString: string;
begin
  SetLength(Result, FContent.Size);
  if FContent.Size > 0 then
  begin
    FContent.Seek(0, soFromBeginning);
    FContent.Read(Result[1], FContent.Size);
  end;
end;

function THTTPRequest.GetCookies(const name: string): string;
var
  strlist: TStringList;
begin
  strlist := TStringList.Create;
  try
    strlist.Delimiter := ';';
    strlist.DelimitedText := s['Cookie'];
    Result := strlist.Values[name];
  finally
    strlist.Free;
  end;
end;

{ THTTPStub }

function THTTPStub.DecodeFields(str: PChar): boolean;
var
  p: PChar;
begin
  p := StrScan(str, ':');
  if p = nil then
    Result := false else
    begin
      FHeader.S[Copy(str, 1, p-str)] := p+2;
      Result := true;
    end;
end;

function THTTPStub.DecodeContent: boolean;
var
  ContentLength: integer;
{$IFDEF CONSOLEAPP}
  buffer: array[0..1023] of char;
  size: integer;
{$ENDIF}
begin
  ContentLength := FHeader.I['Content-Length'];
  if ContentLength > 0 then
  begin
    FHeader.FContent.Size := ContentLength;
    FHeader.FContent.LoadFromSocket(SocketHandle, false);
{$IFDEF CONSOLEAPP}
    repeat
      size := FHeader.FContent.Read(buffer, sizeof(buffer));
      if size > 0 then
      begin
        if size < sizeof(buffer) then buffer[size] := #0;
        writeln(buffer);
      end;
    until size < sizeof(buffer);
{$ENDIF}
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
  FHeader.s['FMethod'] := PChar(copy(str, 0, marker - str));
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
      FHeader.s['FURI'] := PChar(value) else
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
                 FHeader['@FParams'].s[param] := PChar(value);
                 //Field := THTTPField.Create(param, value);
                 //FHeader.FParams.Insert(Field);
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
      FHeader.I['FVersion.major'] := i else
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
      FHeader.I['FVersion.minor']  := i else
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
begin
  result := 0;
  cursor := 0;
  len := 0;
  line := 0;
  FHeader.Clear;
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
            {$IFDEF CONSOLEAPP}
            writeln('---------------------------------------');
            {$ENDIF}
            ProcessRequest; // <<<<<<<<<<<<<<<
            line := 0;
            cursor := 0;
            FHeader.Clear;
          end else
          begin
            buffer[cursor] := NL;
            {$IFDEF CONSOLEAPP}
            writeln(PChar(Buffer));
            {$ENDIF}
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

constructor THTTPStub.CreateStub(AOwner: TSocketServer; ASocket: longint;
  AAddress: TSockAddr);
begin
  inherited;
  FHeader := THTTPRequest.Create;
end;

destructor THTTPStub.Destroy;
begin
  FHeader.Free;
  inherited;
end;

procedure THTTPStub.ProcessRequest;
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


