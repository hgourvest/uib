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
{$I PDGAppServer.inc}
interface
uses PDGSocketStub, Winsock, PDGUtils, davl;

type
  THttpResponseCode = (rc100, rc101, rc200, rc201, rc202, rc203, rc204, rc205,
    rc206, rc300, rc301, rc302, rc303, rc304, rc305, rc306, rc307, rc400, rc401,
    rc402, rc403, rc404, rc405, rc406, rc407, rc408, rc409, rc410, rc411, rc412,
    rc413, rc414, rc415, rc416, rc417, rc500, rc501, rc502, rc503, rc504, rc505);

  THttpMethod = (mUNKNOW, mOPTIONS, mGET, mHEAD, mPOST, mPUT, mDELETE, mTRACE, mCONNECT);

  TConnectionField = (conClose, conKeepAlive);

  THTTPFieldList = class;

  THTTPField = class(TAvlHandle)
  private
    FName: string;
    FValue: string;
  public
    property Name: string read FName;
    property Value: string read FValue;
    constructor Create(const Name, Value: string); virtual;
  end;

  THTTPFieldList = class(TAvlTree)
  private
    FCaseSensitive: boolean;
  protected
    function CompareNodeNode(node1, node2: TAvlHandle): integer; override;
    function CompareKeyNode(k: TAvlKey; h: TAvlHandle): integer; override;
  public
    function GetIntValue(const name: string; const Default: Integer = 0): Integer;
    function GetStringValue(const name: string; const Default: String = ''): string;
    constructor Create(CaseSensitive: boolean); reintroduce; virtual;
  end;

  THTTPRequest = class(THTTPFieldList)
  private
    FMethod: THttpMethod;
    FURI: string;
    FVersion: record
      major: integer;
      minor: integer;
    end;
    FParams: THTTPFieldList;
    FContent: TPooledMemoryStream;
  public
    property Method: THttpMethod read FMethod;
    property URI: string read FURI;
    property Params: THTTPFieldList read FParams;
    property Content: TPooledMemoryStream read FContent;
    procedure Clear; override;
    constructor Create(CaseSensitive: boolean); override;
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
    constructor CreateStub(AOwner: TSocketServer; Socket: TSocket;
      Address: TSockAddrIn); override;
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
uses SysUtils, Windows, StrUtils;

{ THTTPField }

constructor THTTPField.Create(const Name, Value: string);
begin
  FName := Name;
  FValue := Value;
end;

{ THTTPRequest }

procedure THTTPRequest.Clear;
begin
  FMethod := mUNKNOW;
  FVersion.major := 0;
  FVersion.minor := 0;
  FURI := '';
  FContent.Clear;
  FParams.Clear;
  inherited;
end;

constructor THTTPRequest.Create(CaseSensitive: boolean);
begin
  inherited Create(CaseSensitive);
  FParams := THTTPFieldList.Create(true);
  FContent := TPooledMemoryStream.Create;
  Clear;
end;

destructor THTTPRequest.Destroy;
begin
  inherited;
  FParams.Free;
  FContent.Free;
end;

{ THTTPFieldList }

function THTTPFieldList.CompareKeyNode(k: TAvlKey; h: TAvlHandle): integer;
begin
  if FCaseSensitive then
    Result := CompareStr(PChar(k), THTTPField(h).FName) else
    Result := CompareText(PChar(k), THTTPField(h).FName);
end;

function THTTPFieldList.CompareNodeNode(node1, node2: TAvlHandle): integer;
begin
  if FCaseSensitive then
    Result := CompareStr(THTTPField(node1).FName, THTTPField(node2).FName) else
    Result := CompareText(THTTPField(node1).FName, THTTPField(node2).FName);
end;

constructor THTTPFieldList.Create(CaseSensitive: boolean);
begin
  inherited Create;
  FCaseSensitive := CaseSensitive;
end;

function THTTPFieldList.GetIntValue(const name: string;
  const Default: Integer): Integer;
var
  f: THTTPField;
begin
  f := THTTPField(Search(PChar(name)));
  if (f = nil) or not TryStrToInt(f.FValue, Result) then
    Result := Default;
end;

function THTTPFieldList.GetStringValue(const name,
  Default: String): string;
var
  f: THTTPField;
begin
  f := THTTPField(Search(PChar(name)));
  if (f = nil) then
    Result := Default else
    Result := f.FValue;
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
      FHeader.Insert(THTTPField.Create(Copy(str, 1, p-str), p+2));
      Result := true;
    end;
end;

function THTTPStub.DecodeContent: boolean;
var
  ContentLength: integer;
begin
  ContentLength := FHeader.GetIntValue('Content-Length', 0);
  if ContentLength > 0 then
  begin
    FHeader.FContent.Size := ContentLength;
    FHeader.FContent.LoadFromSocket(SocketHandle, false);
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
  Field: THTTPField;
begin
  result := false;
  case str^ of
    'G': if (str[1] = 'E') and (str[2] = 'T') then
         begin
           FHeader.FMethod := mGET;
           str := @str[3];
         end else
           exit;
    'P': case str[1] of
           'O': if (str[2] = 'S') and (str[3] = 'T') then
                begin
                  FHeader.FMethod := mPOST;
                  str := @str[4];
                end else
                  exit;
           'U': if (str[2] = 'T') then
                begin
                  FHeader.FMethod := mPUT;
                  str := @str[3];
                end else
                  exit;
           else
             exit;
           end;
    'C': if (str[1] = 'O') and (str[2] = 'N') and (str[3] = 'N') and
           (str[4] = 'E') and (str[5] = 'C') and (str[6] = 'T') then
         begin
           FHeader.FMethod := mCONNECT;
           str := @str[7];
         end else
           exit;
    'D': if (str[1] = 'E') and (str[2] = 'L') and (str[3] = 'E') and
            (str[4] = 'T') and (str[5] = 'E') then
         begin
           FHeader.FMethod := mDELETE;
           str := @str[6];
         end else
           exit;
    'H': if (str[1] = 'E') and (str[2] = 'A') and (str[3] = 'D') then
         begin
           FHeader.FMethod := mHEAD;
           str := @str[4];
         end else
           exit;
    'O': if (str[1] = 'P') and (str[2] = 'T') and (str[3] = 'I') and
           (str[4] = 'O') and (str[5] = 'N') and (str[6] = 'S') then
         begin
           FHeader.FMethod := mOPTIONS;
           str := @str[7];
         end else
           exit;
    'T': if (str[1] = 'R') and (str[2] = 'A') and (str[3] = 'C') and (str[4] = 'E') then
         begin
           FHeader.FMethod := mTRACE;
           str := @str[5];
         end else
           exit;
  else
    exit;
  end;

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
    if not DecodeURI(marker, str - marker, FHeader.FURI) then
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
                 Field := THTTPField.Create(param, value);
                 FHeader.FParams.Insert(Field);
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
    if not TryStrToInt(copy(marker, 0, str - marker), FHeader.FVersion.major) then
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
    if not TryStrToInt(copy(marker, 0, str - marker), FHeader.FVersion.minor) then
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

    if recv(SocketHandle, c, 1, 0) <> 1 then exit;
    case c of
    CR: dec(cursor){->LF};
    LF: begin
          if cursor = 1 then
          begin
            if not DecodeContent then
              exit;
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

constructor THTTPStub.CreateStub(AOwner: TSocketServer; Socket: TSocket;
  Address: TSockAddrIn);
begin
  inherited;
  FHeader := THTTPRequest.Create(false);
end;

destructor THTTPStub.Destroy;
begin
  FHeader.Free;
  inherited;
end;

procedure THTTPStub.ProcessRequest;
begin

end;

end.


