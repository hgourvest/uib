unit WebServer;
{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
interface
uses PDGHTTPStub, PDGSocketStub, PDGUtils, {$IFDEF FPC}sockets,{$ELSE}Winsock, {$ENDIF}Windows,
  jvuib, jvuiblib, superobject, SyncObjs, classes;

type
  THTTPServer = class(TSocketServer)
  protected
    function doOnCreateStub(Socket: longint; AAddress: TSockAddr): TSocketStub; override;
  end;

  THTTPConnexion = class(THTTPStub)
  protected
    FFormats: ISuperObject;
    procedure doBeforeProcessRequest(ctx: ISuperObject); override;
    procedure doAfterProcessRequest(ctx: ISuperObject); override;
    procedure ProcessRequest(ctx: ISuperObject); override;
    function CreateMVC: ISuperObject; override;
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
  with TSuperObject.Parse(
    PChar(
      FileToString(
        ExtractFilePath(ParamStr(0)) + 'appserver.json'))) do
  begin
    FDatabaseName := s['database.databasename'];
    FUserName := s['database.username'];
    FPassWord := s['database.password'];
    FSQLDialect := i['database.sqldialect'];
  end;
end;

{ THTTPServer }

constructor THTTPConnexion.CreateStub(AOwner: TSocketServer; Socket: longint;
  AAddress: TSockAddr);
begin
  inherited;
  FFormats := TSuperObject.Create;

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
  FFormats := nil;
  inherited;
end;

procedure THTTPConnexion.doAfterProcessRequest(ctx: ISuperObject);
begin
  Response.S['env.Set-Cookie'] := PChar(COOKIE_NAME + '=' + StrTobase64(ctx['session'].AsJSon));
  inherited;
end;

procedure THTTPConnexion.doBeforeProcessRequest(ctx: ISuperObject);
var
  obj: ISuperObject;
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
  ctx['params'] := TSuperObject.Create;
  ctx['params'].Merge(Request['params'], true);
  if (Request.S['method'] = 'POST') then
    if(Request.S['accept[0]'] = 'application/json') then
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
        obj := nil;
      end;
    end;

   obj := HTTPInterprete(PChar(Request.S['uri']), false, '/');
   begin
     if interprete(PChar(obj.AsArray.S[1]), 'controler') then
     if interprete(PChar(obj.AsArray.S[2]), 'action') then
        interprete(PChar(obj.AsArray.S[3]), 'id');
   end;

  // default action is index
  if (ctx['params.controler'] <> nil) and (ctx['params.action'] = nil) then
    (ctx.S['params.action'] := 'index');

  // detect format
  if (ctx['params.format'] = nil) then
    ctx.S['params.format'] := 'html';
end;

procedure THTTPConnexion.ProcessRequest(ctx: ISuperObject);
var
  user, pass: string;
  str: string;
  path: string;
  obj: ISuperObject;
  proc: TSuperMethod;
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
      Response['@env'].AsObject.Put('WWW-Authenticate', TSuperObject.Create('Basic'));
      exit;
    end else
      ctx.B['session.authenticate'] := false;
  end;

  if ctx['params.controler'] <> nil then
    with ctx['params'] do
    begin
      // controler
      proc := MVC.M[S['controler'] + '.' + S['action'] + '.controler'];
      if assigned(proc) then
      begin
        obj := nil;
        proc(ctx, ctx['params'], obj);
      end;

      // redirect ? ...
      if Response.I['response'] = 300 then Exit;

      // view
      proc := MVC.M[S['controler'] + '.' + S['action'] + '.' + S['format']];
      if assigned(proc) then
      begin
        obj := nil;
        proc(ctx, ctx['params'], obj);
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

{ HTTP Methods }

function QueryToJson(qr: TJvUIBQuery): ISuperObject;
var
  meta, data, rec: ISuperObject;
  i: integer;
  str: string;
begin
  qr.Open;
  Result := TSuperObject.Create(stObject);

  meta := TSuperObject.Create(stArray);
  Result.AsObject.Put('meta', meta);
  for i := 0 to qr.Fields.FieldCount - 1 do
    meta.AsArray.add(TSuperObject.create(PChar(qr.Fields.AliasName[i])));

  data := TSuperObject.Create(stArray);
  Result.AsObject.Put('data', data);
  while not qr.Eof do
  begin
    rec := TSuperObject.create(stArray);
    data.AsArray.Add(rec);
        for i := 0 to qr.Fields.FieldCount - 1 do
      if qr.Fields.IsNull[i] then
        rec.AsArray.Add(nil) else
      case qr.Fields.FieldType[i] of
        uftChar, uftVarchar, uftCstring: rec.AsArray.Add(TSuperObject.Create(PChar(qr.Fields.AsString[i])));
        uftSmallint, uftInteger, uftInt64: rec.AsArray.Add(TSuperObject.Create(qr.Fields.AsInteger[i]));
        uftNumeric, uftFloat, uftDoublePrecision: rec.AsArray.Add(TSuperObject.Create(qr.Fields.AsDouble[i]));
        uftBlob, uftBlobId:
          begin
            qr.ReadBlob(i, str);
            if qr.Fields.Data.sqlvar[i].SqlSubType = 1 then
              rec.AsArray.Add(TSuperObject.Create(PChar(str))) else
              rec.AsArray.Add(TSuperObject.Create(PChar(StrTobase64(str))));
          end;
        uftTimestamp, uftDate, uftTime: rec.AsArray.Add(TSuperObject.Create(PChar(qr.Fields.AsString[i])));
        {$IFDEF IB7_UP}
        uftBoolean: rec.AsArray.Add(ISuperObject.Create(PChar(qr.Fields.AsBoolean[i])));
        {$ENDIF}
      else
        rec.AsArray.Add(nil);
      end;
    qr.next;
  end;
end;

procedure HTTPOutput(this, obj: ISuperObject; format: boolean); overload;
begin
  obj.SaveTo(THTTPMessage(this['response'].DataPtr).Content, format);
end;

procedure HTTPOutput(this: ISuperObject; const str: string); overload;
begin
  THTTPMessage(this['response'].DataPtr).Content.WriteString(str, false);
end;

procedure HTTPCompress(this: ISuperObject);
begin
  this.B['response.compress'] := true;
  this.S['response.env.Content-Encoding'] := 'deflate';
end;

function HTTPIsPost(this: ISuperObject): boolean;
begin
  Result := This.S['request.method'] = 'POST'
end;

procedure HTTPRedirect(This: ISuperObject; const location: string);
begin
  This.I['response.response'] := 300;
  This.S['response.env.Location'] := PChar(Location);
end;


procedure application_getdata_html(This, Params: ISuperObject; var Result: ISuperObject);
begin
  HTTPOutput(this, '<html><body><pre>');
  HTTPOutput(this, this['dataset'], true);
  HTTPOutput(this, '</pre></body></html>');
end;

procedure application_getdata_json(This, Params: ISuperObject; var Result: ISuperObject);
begin
  HTTPOutput(this, this['dataset'], false);
end;

procedure application_getdata_controler(This, Params: ISuperObject;
  var Result: ISuperObject);
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
    this['dataset'] := QueryToJson(qr);
  finally
    qr.Free;
    tr.Free;
    pool.FreeConnexion;
  end;
end;

function THTTPConnexion.CreateMVC: ISuperObject;
begin
  Result := TSuperObject.Create;
  Result.M['application.getdata.controler'] := @application_getdata_controler;
  Result.M['application.getdata.json'] := @application_getdata_json;
  Result.M['application.getdata.html'] := @application_getdata_html;
end;

initialization
  pool := TMyPool.Create(0);
  Application.CreateServer(THTTPServer, 81);

finalization
  while TPDGThread.ThreadCount > 0 do sleep(100);
  pool.Free;
end.
