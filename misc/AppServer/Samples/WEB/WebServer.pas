unit WebServer;
{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
interface
uses PDGHTTPStub, PDGSocketStub, PDGUtils, {$IFDEF FPC}sockets,{$ELSE}Winsock, {$ENDIF}Windows,
  jvuib, jvuiblib, json, SyncObjs, classes;

type
  THTTPServer = class(TSocketServer)
  protected
    function doOnCreateStub(Socket: longint; AAddress: TSockAddr): TSocketStub; override;
  end;

  THTTPMethods = class(TJsonObject)
  private
    procedure application_getdata_controler(Params: TJsonObject; var Result: TJsonObject);
    procedure application_getdata_json(Params: TJsonObject; var Result: TJsonObject);
    procedure application_getdata_html(Params: TJsonObject; var Result: TJsonObject);
  public
    procedure Redirect(const location: string);
    function isPost: boolean;
    constructor Create(jt: TJsonType = json_type_object); override;
  end;

  THTTPConnexion = class(THTTPStub)
  protected
    FFormats: TJsonObject;
    procedure doBeforeProcessRequest(ctx: TJsonObject); override;
    procedure doAfterProcessRequest(ctx: TJsonObject); override;
    procedure ProcessRequest(ctx: TJsonObject); override;
    function CreateMVC: TJsonObject; override;
  public
    constructor CreateStub(AOwner: TSocketServer; Socket: longint;
      AAddress: TSockAddr); override;
    destructor Destroy; override;
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
uses SysUtils, PDGService{$ifdef madExcept}, madExcept {$endif};

const
  ReadTimeOut: Integer = 60000; // 1 minute
  COOKIE_NAME = 'PDGCookie';

var
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
begin
  inherited Create(MaxSize);
  with TJsonObject.Parse(
    PChar(
      FileToString(
        ExtractFilePath(ParamStr(0)) + 'appserver.json'))) do
  try
    FDatabaseName := s['database.databasename'];
    FUserName := s['database.username'];
    FPassWord := s['database.password'];
    FSQLDialect := i['database.sqldialect'];
  finally
    Free;
  end;
end;

{ THTTPServer }

function THTTPConnexion.CreateMVC: TJsonObject;
begin
  Result := THTTPMethods.Create;
end;

constructor THTTPConnexion.CreateStub(AOwner: TSocketServer; Socket: longint;
  AAddress: TSockAddr);
begin
  inherited;
  FFormats := TJsonObject.Create;

  FFormats.S['htm'] := 'text/html';
  FFormats.S['html'] := 'text/html';
  FFormats.S['xml'] := 'text/xml';
  FFormats.S['json'] := 'text/json';
  FFormats.S['png'] := 'image/png';
  FFormats.S['jpeg'] := 'image/jpeg';
  FFormats.S['jpg'] := 'image/jpeg';
  FFormats.S['gif'] := 'image/gif';
  FFormats.S['css'] := 'text/css';
  FFormats.S['js'] := 'text/javascript';

  // connexion timout
{$IFDEF FPC}
  fpsetsockopt(Socket, SOL_SOCKET, SO_RCVTIMEO, @ReadTimeOut, SizeOf(ReadTimeOut));
{$ELSE}
  setsockopt(Socket, SOL_SOCKET, SO_RCVTIMEO, @ReadTimeOut, SizeOf(ReadTimeOut));
{$ENDIF}
end;

destructor THTTPConnexion.Destroy;
begin
  FFormats.Free;
  inherited;
end;

procedure THTTPConnexion.doAfterProcessRequest(ctx: TJsonObject);
begin
  Response.S['env.Set-Cookie'] := PChar(COOKIE_NAME + '=' + StrTobase64(ctx['session'].AsJSon));
  inherited;
end;

procedure THTTPConnexion.doBeforeProcessRequest(ctx: TJsonObject);
var
  obj: TJsonObject;
  function interprete(v: PChar; name: string): boolean;
  var
    p: PChar;
    str: string;
  begin
    str := trim(v);
    if str <> '' then
    begin
      p := StrScan(PChar(str), '.');
      if p <> nil then
      begin
        ctx.S['params.format'] := p + 1;
        setlength(str, p - PChar(str));
      end;
      ctx['params'].S[name] := PChar(str);
      Result := true;
    end else
      Result := false
  end;
begin
  inherited;
  // default values
  ctx.B['session.authenticate'] := true;
  // decode session from cookie
  ctx['session'].Merge(Base64ToStr(Request['cookies'].S[COOKIE_NAME]));

  // get parametters
  ctx['params'] := TJsonObject.Create;
  ctx['params'].Merge(Request['params'], true);
  if (Request.S['method'] = 'POST') then
    if(Request.S['content-type[0]'] = 'application/json') then
    begin
      ctx['params'].Merge(Request.ContentString);
      ctx.S['params.format'] := 'json';
    end else
    if(Request.S['content-type[0]'] = 'application/x-www-form-urlencoded') then
    begin
      obj := HTTPInterprete(PChar(Request.ContentString), true, '&');
      try
        ctx['params'].Merge(obj, true);
        ctx.S['params.format'] := 'html';
      finally
        obj.Free;
      end;
    end
//    else
//    if(Request.S['content-type[0]'] = 'multipart/form-data') then
//    begin
//
//      ctx.S['params.format'] := 'html';
//    end
    ;

   with HTTPInterprete(Request.S['uri'], false, '/') do
   begin
     if interprete(AsArray.S[1], 'controler') then
     if interprete(AsArray.S[2], 'action') then
        interprete(AsArray.S[3], 'id');
     Free;
   end;

  // default action is index
  if (ctx['params.controler'] <> nil) and (ctx['params.action'] = nil) then
    (ctx.S['params.action'] := 'index');

  // detect format
  if (ctx['params.format'] = nil) then
    ctx.S['params.format'] := 'html';

  //writeln(ctx.asjson(true));
//  writeln(Request.ContentString);
//  Request.Content.SaveToFile('c:\test.txt');
end;

procedure THTTPConnexion.ProcessRequest(ctx: TJsonObject);
var
  user, pass: string;
  str: string;
  path: string;
  obj: TJsonObject;
  proc: TJsonMethod;
begin
  inherited;

  // Authenticate
  if ctx.B['session.authenticate'] then
  begin
    user := Request.S['authorization.user'];
    pass := Request.S['authorization.pass'];
    if not((user = 'user') and (pass = 'pass')) then
    begin
      Response.I['response'] := 401;
      Response.S['env.WWW-Authenticate'] := 'Basic';
      exit;
    end else
      ctx.B['session.authenticate'] := false;
  end;

  if ctx['params.controler'] <> nil then
    with ctx['params'] do
    begin
      // controler
      TMethod(proc).Code := MVC[S['controler']][S['action']].M['controler'];
      if TMethod(proc).Code <> nil then
      begin
        TMethod(proc).Data := ctx;
        obj := nil;
        proc(ctx['params'], obj);
      end;

      // redirect ? ...
      if Response.I['response'] = 300 then Exit;

      // view
      TMethod(proc).Code := MVC[S['controler']][S['action']].M[S['format']];
      if TMethod(proc).Code <> nil then
      begin
        TMethod(proc).Data := ctx;
        obj := nil;
        proc(ctx['params'], obj);
        Response.S['env.Content-Type'] := FFormats.S[S['format']];
        exit;
      end;
    end;

  str := Request.S['uri'];
  path := ExtractFilePath(ParamStr(0)) + 'HTTP';

  if str[Length(str)] in ['/','\'] then
  begin
    if FileExists(path + str + 'index.html') then
      Request.S['uri'] := PChar(Request.S['uri'] + 'index.html') else
    if FileExists(path + Request.S['uri'] + 'index.htm') then
      Request.S['uri'] := PChar(Request.S['uri'] + 'index.htm');
  end;

  if FileExists(path + Request.S['uri']) then
  begin
    Response.S['env.Content-Type'] := FFormats.S[ctx.S['params.format']];
    Response.S['sendfile'] := PChar(path + Request.S['uri']);
    exit;
  end else
    Response.I['response'] :=  404;

end;

{ THTTPServer }

function THTTPServer.doOnCreateStub(Socket: longint;
  AAddress: TSockAddr): TSocketStub;
begin
  Result := THTTPConnexion.CreateStub(Self, Socket, AAddress);
end;

{ THTTPMethods }

function QueryToJson(qr: TJvUIBQuery): TJsonObject;
var
  meta, data, rec: TJsonObject;
  i: integer;
  str: string;
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
        uftBlob, uftBlobId:
          begin
            qr.ReadBlob(i, str);
            if qr.Fields.Data.sqlvar[i].SqlSubType = 1 then
              rec.AsArray.Add(TJsonObject.Create(PChar(str))) else
              rec.AsArray.Add(TJsonObject.Create(PChar(StrTobase64(str))));
          end;
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

procedure THTTPMethods.application_getdata_html(Params: TJsonObject;
  var Result: TJsonObject);
begin
  THTTPMessage(O['response']).Content.WriteString('<html><body><pre>', false);
  O['dataset'].SaveTo(THTTPMessage(O['response']).Content, true);
  THTTPMessage(O['response']).Content.WriteString('</pre></body></html>', false);
end;

procedure THTTPMethods.application_getdata_json(Params: TJsonObject;
  var Result: TJsonObject);
begin
  O['dataset'].SaveTo(THTTPMessage(O['response']).Content);
end;

constructor THTTPMethods.create(jt: TJsonType = json_type_object);
begin
  inherited;
  M['application.getdata.controler'] := @THTTPMethods.application_getdata_controler;
  M['application.getdata.json'] := @THTTPMethods.application_getdata_json;
  M['application.getdata.html'] := @THTTPMethods.application_getdata_html;
end;

function THTTPMethods.isPost: boolean;
begin
  Result := S['request.method'] = 'POST'
end;

procedure THTTPMethods.Redirect(const location: string);
begin
  I['response.response'] := 300;
  S['response.env.Location'] := PChar(Location);
end;

procedure THTTPMethods.application_getdata_controler(Params: TJsonObject;
  var Result: TJsonObject);
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
    qr.SQL.Text := 'select * from ' + Params.S['id'];
    qr.CachedFetch := false;
    O['dataset'] := QueryToJson(qr);
  finally
    qr.Free;
    tr.Free;
    pool.FreeConnexion;
  end;
end;

initialization
  pool := TMyPool.Create(0);
  Application.CreateServer(THTTPServer, 81);

finalization
  while TPDGThread.ThreadCount > 0 do sleep(100);
  pool.Free;

end.
