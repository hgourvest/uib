unit WebServer;
{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
interface
uses PDGHTTPStub, PDGSocketStub, PDGUtils, {$IFDEF FPC}sockets,{$ELSE}Winsock, {$ENDIF}Windows,
  jvuib, jvuiblib, davl, json, SyncObjs, classes;

type
  THTTPServer = class(TSocketServer)
  protected
    function doOnCreateStub(Socket: longint; AAddress: TSockAddr): TSocketStub; override;
  end;

  THTTPConnexion = class(THTTPStub)
  protected
    procedure ProcessRequest; override;
  public
    constructor CreateStub(AOwner: TSocketServer; Socket: longint;
      AAddress: TSockAddr); override;
  end;

  THTTPSession = class
  private
    FRefCount: Integer;
    FCriticalSection: TRtlCriticalSection;
    FCessionId: string;
    FHttpDir: string;
    FPasswordNeeded: boolean;
    FJsonRPCServices: TJsonRpcServiceList;
    procedure rpc_echo(Params: TJsonObject; out Result: TJsonObject);
    procedure rpc_getdata(Params: TJsonObject; out Result: TJsonObject);
  protected
    function AddRef: integer;
    procedure Release;
    procedure Lock;
    procedure UnLock;
  public
    constructor Create(const ACookie: string); virtual;
    destructor Destroy; override;
    procedure ProcessRequest(Connexion: THTTPStub); virtual;
  end;

  THTTPSessionNode = class(TAvlHandle)
  private
    FCookie: string;
    FLastActive: Cardinal;
    FSession: THTTPSession;
  public
    constructor Create(const ACookie: string; ASession: THTTPSession); virtual;
    destructor Destroy; override;
  end;

  THTTPSessionList = class(TAvlTree)
  private
    FCriticalSection: TRtlCriticalSection;
  protected
    function CompareNodeNode(node1, node2: TAvlHandle): integer; override;
    function CompareKeyNode(k: TAvlKey; h: TAvlHandle): integer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetSession(const ACookie: string): THTTPSession;
    function CreateSession: THTTPSession;
    procedure ClearSessions(Timeout: Cardinal);
    procedure Lock;
    procedure Unlock;
  end;

  THTTPSessionCleaner = class(TPDGThread)
  protected
    function Run: Cardinal; override;
  end;

  TMyPool = class(TConnexionPool)
  private
    FDatabaseName: string;
    FUserName: string;
    FPassWord: string;
    FSQLDialect: Integer;
  protected
    procedure ConfigureConnexion(Database: TJvUIBDataBase); override;
  public
    constructor Create(MaxSize: Integer = 0); override;
  end;

implementation
uses SysUtils, PDGService, inifiles{$ifdef madExcept}, madExcept {$endif};

const
  ReadTimeOut: Integer = 60000; // 1 minute

var
  SessionList: THTTPSessionList;
  pool: TMyPool;

{$IFDEF FPC}
function GetTickCount : Cardinal;
  var
     h,m,s,s1000 : word;
  begin
     decodetime(time,h,m,s,s1000);
     result:=Cardinal(h*3600000+m*60000+s*1000+s1000);
  end;
{$ENDIF}

{ TMyPool }

procedure TMyPool.ConfigureConnexion(Database: TJvUIBDataBase);
begin
  Database.DatabaseName := FDatabaseName;
  Database.UserName := FUserName;
  Database.PassWord := FPassWord;
  Database.SQLDialect := FSQLDialect;
  // one thread should try to connect to database at the same time
  // this eliminate dead lock when there is too many thread trying to connect
  Database.Connected := true;
end;

constructor TMyPool.Create(MaxSize: Integer = 0);
var inifile: TIniFile;
begin
  inherited Create(MaxSize);
  inifile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'AppServer.ini');
  try
    FDatabaseName := inifile.ReadString('DATABASE', 'DatabaseName', '');
    FUserName := inifile.ReadString('DATABASE', 'UserName', 'SYSDBA');
    FPassWord := inifile.ReadString('DATABASE', 'PassWord', 'masterkey');
    FSQLDialect := inifile.ReadInteger('DATABASE', 'SQLDialect', 3);
  finally
    inifile.Free;
  end;
end;

{ THTTPServer }

procedure THTTPConnexion.ProcessRequest;
var
  Cookie: string;
  Session: THTTPSession;
begin
  Cookie := Header.GetCookies('PDGCookie');

  if Cookie <> '' then
    Session := SessionList.GetSession(Cookie) else
    Session := nil;
  if Session = nil then
    Session := SessionList.CreateSession;

  try
    Session.Lock;
    try
      Session.ProcessRequest(Self);
    finally
      Session.UnLock;
    end;
  finally
    Session.Release;
  end;
end;

constructor THTTPConnexion.CreateStub(AOwner: TSocketServer; Socket: longint;
  AAddress: TSockAddr);
begin
  inherited;
  // connexion timout
{$IFDEF FPC}
  fpsetsockopt(Socket, SOL_SOCKET, SO_RCVTIMEO, @ReadTimeOut, SizeOf(ReadTimeOut));
{$ELSE}
  setsockopt(Socket, SOL_SOCKET, SO_RCVTIMEO, @ReadTimeOut, SizeOf(ReadTimeOut));
{$ENDIF}
end;

{ THTTPServer }

function THTTPServer.doOnCreateStub(Socket: longint;
  AAddress: TSockAddr): TSocketStub;
begin
  Result := THTTPConnexion.CreateStub(Self, Socket, AAddress);
end;

{ THTTPSessionList }

procedure THTTPSessionList.ClearSessions(Timeout: Cardinal);
var
  ite: TAvlIterator;
  node: THTTPSessionNode;
  list: TList;
  i: integer;
begin
  Lock;
  try
    ite := TAvlIterator.Create(Self);
    list := TList.Create;
    try
      ite.First;
      node := THTTPSessionNode(ite.GetIter);
      while node <> nil do
      begin
        if (node.FSession.AddRef = 2) and ((GetTickCount - node.FLastActive) > Timeout) then
          list.Add(node);
        node.FSession.Release;
        ite.Next;
        node := THTTPSessionNode(ite.GetIter);
      end;
      for i := 0 to list.Count - 1 do
      begin
        node := THTTPSessionNode(Remove(PChar(THTTPSessionNode(list[i]).FCookie)));
        if node <> nil then
          node.Free;
      end;
    finally
      ite.Free;
      list.Free;
    end;
  finally
    Unlock;
  end;
end;

function THTTPSessionList.CompareKeyNode(k: TAvlKey;
  h: TAvlHandle): integer;
begin
  Result := CompareStr(PChar(k), THTTPSessionNode(h).FCookie);
end;

function THTTPSessionList.CompareNodeNode(node1,
  node2: TAvlHandle): integer;
begin
  Result := CompareStr(THTTPSessionNode(node1).FCookie, THTTPSessionNode(node2).FCookie);
end;

constructor THTTPSessionList.Create;
begin
  inherited;
{$IFDEF FPC}
  InitCriticalSection(FCriticalSection);
{$ELSE}
  InitializeCriticalSection(FCriticalSection);
{$ENDIF}
end;

function THTTPSessionList.CreateSession: THTTPSession;
var
  c: string;
  s: THTTPSession;
  n: THTTPSessionNode;
begin
  c := 'C' + IntToStr(GetTickCount);
  s := THTTPSession.Create(c);
  s.AddRef;
  n := THTTPSessionNode.Create(c, s);
  n.FLastActive := GetTickCount;
  Lock;
  try
    Insert(n);
  finally
    Unlock;
  end;
  Result := s;
end;

destructor THTTPSessionList.Destroy;
begin
{$IFDEF FPC}
  DoneCriticalSection(FCriticalSection);
{$ELSE}
  DeleteCriticalSection(FCriticalSection);
{$ENDIF}
  inherited;
end;

function THTTPSessionList.GetSession(
  const ACookie: string): THTTPSession;
var
  node: THTTPSessionNode;
begin
  Lock;
  try
    node := THTTPSessionNode(Search(PChar(ACookie)));
    if node = nil then
      Result := nil else
      begin
        node.FLastActive := GetTickCount;
        Result := node.FSession;
        Result.AddRef;
      end;
  finally
    Unlock;
  end;
end;

procedure THTTPSessionList.Lock;
begin
  EnterCriticalSection(FCriticalSection);
end;

procedure THTTPSessionList.Unlock;
begin
  LeaveCriticalSection(FCriticalSection);
end;

{ THTTPSessionNode }

constructor THTTPSessionNode.Create(const ACookie: string;
  ASession: THTTPSession);
begin
  FCookie := ACookie;
  FLastActive := GetTickCount;
  FSession := ASession;
end;

destructor THTTPSessionNode.Destroy;
begin
  FSession.Release;
  inherited;
end;

{ THTTPSessionCleaner }

function THTTPSessionCleaner.Run: Cardinal;
var
  i: integer;
begin
  // remove expired sessions
  while not Stopped do
  begin
    SessionList.ClearSessions(60000*30);//30min
    for i := 1 to 60*30 do
      if not stopped then
        sleep(1000) else
        Break;
  end;
  Result := 0;
end;

{ THTTPSession }

function THTTPSession.AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

constructor THTTPSession.Create(const ACookie: string);
var
  srv: TJsonRpcService;
begin
  FCessionId := ACookie;
  FPasswordNeeded := true;
  FRefCount := 1;
  FJsonRPCServices := TJsonRpcServiceList.Create('application');
{$IFDEF FPC}
  InitCriticalSection(FCriticalSection);
{$ELSE}
  InitializeCriticalSection(FCriticalSection);
{$ENDIF}
  FHttpDir := ExtractFilePath(ParamStr(0)) + 'HTTP';

  srv := TJsonRpcService.Create('application');
  srv.RegisterMethod('echo', self, @THTTPSession.rpc_echo);
  srv.RegisterMethod('getdata', self, @THTTPSession.rpc_getdata);
  FJsonRPCServices.RegisterService(srv);

end;

destructor THTTPSession.Destroy;
begin
{$IFDEF FPC}
  DoneCriticalSection(FCriticalSection);
{$ELSE}
  DeleteCriticalSection(FCriticalSection);
{$ENDIF}
  FJsonRPCServices.Free;
  inherited;
end;

procedure THTTPSession.Lock;
begin
  AddRef;
  EnterCriticalSection(FCriticalSection);
end;

procedure THTTPSession.Release;
begin
  if InterlockedDecrement(FRefCount) = 0 then Destroy;
end;

procedure THTTPSession.UnLock;
begin
  LeaveCriticalSection(FCriticalSection);
  Release;
end;

procedure THTTPSession.ProcessRequest(Connexion: THTTPStub);
  procedure WriteHeader(code: THttpResponseCode);
  begin
    Connexion.WriteLine('HTTP/1.1 ' + HttpResponseStrings[code]);
    Connexion.WriteLine('Server: progdigy server/1.0');
    Connexion.WriteLine('Set-Cookie: PDGCookie=' + FCessionId);
  end;
var
  Ext: string;
  user, pass, service: string;
  obj: TJsonObject;
begin
  with Connexion do
  try
    // Authenticate
    if FPasswordNeeded then
    begin
      Header.GetAuthorization(user, pass);
      if not((user = 'user') and (pass = 'pass')) then
      begin
        WriteHeader(rc401); // Authorization Required
        WriteLine('WWW-Authenticate: Basic');
        SendString('');
        exit;
      end else
        FPasswordNeeded := false;
    end;

    case Header.Method of
    mGET:
      begin
        if Header.URI[Length(Header.URI)] in ['/','\'] then
        begin
          if FileExists(FHttpDir + Header.URI + 'index.html') then
            Header.URI := Header.URI + 'index.html' else
          if FileExists(FHttpDir + Header.URI + 'index.htm') then
            Header.URI := Header.URI + 'index.htm';
        end;

        if FileExists(FHttpDir + Header.URI) then
        begin
          WriteHeader(rc200);
          ext := UpperCase(ExtractFileExt(Header.URI));
          if ext = '.HTM' then WriteLine('Content-Type: text/html') else
          if ext = '.HTML' then WriteLine('Content-Type: text/html') else
          if ext = '.CSS' then WriteLine('Content-Type: text/css') else
          if ext = '.XML' then WriteLine('Content-Type: text/xml') else
          if ext = '.PNG' then WriteLine('Content-Type: image/png') else
          if ext = '.JPG' then WriteLine('Content-Type: image/jpeg') else
          if ext = '.JPEG' then WriteLine('Content-Type: image/jpeg') else
          if ext = '.GIF' then WriteLine('Content-Type: image/gif') else
            WriteLine('Content-Type: text/plain');

          SendFile(FHttpDir + Header.URI);
        end else
        begin
          WriteHeader(rc404);
          SendString(HttpResponseStrings[rc404]);
        end;
      end;
    mPOST:
      begin
        if (Header.GetStringValue('Accept') = 'application/json') then
        begin
          service := copy(Header.URI, 2, Length(Header.URI)-1);
          obj := FJsonRPCServices.Invoke(PChar(service), PChar(Header.ContentString));
          WriteHeader(rc200);
          WriteLine(format('Content-Length: %d', [obj.CalcSize]));
          WriteLine('');
          obj.SaveTo(Connexion.SocketHandle);
          obj.Release;
        end else
        begin
          WriteHeader(rc404);
          SendString(HttpResponseStrings[rc404]);
        end;
      end else
      begin
        WriteHeader(rc404);
        SendString(HttpResponseStrings[rc404]);
      end;
    end;
  except
    on E: Exception do
    begin
    {$ifdef madExcept}
      HandleException(etNormal, E);
    {$endif}
      //SendString('[' + E.ClassName + '] ' + E.Message);
    end;
  end;
end;

procedure THTTPSession.rpc_echo(Params: TJsonObject;
  out Result: TJsonObject);
begin
  Result := Params;
  Result.AddRef;
end;

function QueryToJson(qr: TJvUIBQuery): TJsonObject;
var
  meta, data, rec: TJsonObject;
  i: integer;
begin
  qr.Open;
  Result := TJsonObject.Create(json_type_object);

  meta := TJsonObject.Create(json_type_array);
  Result.AsObject.Put('meta', meta);
  for i := 0 to qr.Fields.FieldCount - 1 do
    meta.AsArray.add(TJsonObject.create(PChar(qr.Fields.AliasName[i])));

  data := TJsonObject.Create(json_type_array);
  Result.AsObject.Put('data', data);
  while not qr.Eof do
  begin
    rec := TJsonObject.create(json_type_array);
    data.AsArray.Add(rec);

    for i := 0 to qr.Fields.FieldCount - 1 do
      if qr.Fields.IsNull[i] then
        rec.AsArray.Add(nil) else
      case qr.Fields.FieldType[i] of
        uftChar, uftVarchar, uftCstring: rec.AsArray.Add(TJsonObject.Create(PChar(qr.Fields.AsString[i])));
        uftSmallint, uftInteger, uftInt64: rec.AsArray.Add(TJsonObject.Create(qr.Fields.AsInteger[i]));
        uftNumeric, uftFloat, uftDoublePrecision: rec.AsArray.Add(TJsonObject.Create(qr.Fields.AsDouble[i]));
        //uftBlob, uftBlobId: ;  b64?
        uftTimestamp, uftDate, uftTime: rec.AsArray.Add(TJsonObject.Create(PChar(qr.Fields.AsString[i])));
        {$IFDEF IB7_UP}
        uftBoolean: rec.AsArray.Add(TJsonObject.Create(PChar(qr.Fields.AsBoolean[i])));
        {$ENDIF}
      else
        rec.AsArray.Add(nil);
      end;
    qr.next;
  end;
end;

procedure THTTPSession.rpc_getdata(Params: TJsonObject;
  out Result: TJsonObject);
var
  db: TJvUIBDataBase;
  tr: TJvUIBTransaction;
  qr: TJvUIBQuery;
begin
  db := pool.GetConnexion;
  tr := TJvUIBTransaction.Create(nil);
  qr := TJvUIBQuery.Create(nil);
  try
    tr.DataBase := db;
    qr.Transaction := tr;
    qr.SQL.Text := 'select * from ' + Params.AsArray.get(0).AsString;
    qr.CachedFetch := false;
    Result := QueryToJson(qr);
  finally
    qr.Free;
    tr.Free;
    pool.FreeConnexion;
  end;
end;

initialization
  pool := TMyPool.Create(0);
  SessionList := THTTPSessionList.Create;
  Application.CreateThread(THTTPSessionCleaner);
  Application.CreateServer(THTTPServer, 80);

finalization
  while TPDGThread.ThreadCount > 0 do sleep(100);
  pool.Free;
  SessionList.Free;

end.
