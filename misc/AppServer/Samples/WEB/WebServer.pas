unit WebServer;
{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
interface
uses 
  PDGHTTPStub, PDGSocketStub, PDGUtils, 
{$IFDEF FPC}sockets,{$ELSE}Winsock, {$ENDIF}
{$IFDEF MSWINDOWS}Windows,{$ENDIF}
  superobject, SyncObjs, classes, mypool, myapp_controller, myapp_view;

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

procedure HTTPOutput(this, obj: ISuperObject; format: boolean); overload;
procedure HTTPOutput(this: ISuperObject; const str: string); overload;
procedure HTTPCompress(this: ISuperObject; level: integer = 5);
function HTTPIsPost(this: ISuperObject): boolean;
procedure HTTPRedirect(This: ISuperObject; const location: string);

implementation
uses SysUtils, PDGService{$ifdef madExcept}, madExcept {$endif};

const
  ReadTimeOut: Integer = 60000; // 1 minute
  COOKIE_NAME = 'PDGCookie';

procedure HTTPOutput(this, obj: ISuperObject; format: boolean); overload;
begin
  obj.SaveTo(THTTPMessage(this['response'].DataPtr).Content, format);
end;

procedure HTTPOutput(this: ISuperObject; const str: string); overload;
begin
  THTTPMessage(this['response'].DataPtr).Content.WriteString(str, false);
end;

procedure HTTPCompress(this: ISuperObject; level: integer = 5);
begin
  this.B['response.compress'] := true;
  this.I['response.compresslevel'] := level;
  this.S['response.env.Content-Encoding'] := 'deflate';
end;

function HTTPIsPost(this: ISuperObject): boolean;
begin
  Result := This.S['request.method'] = 'POST'
end;

procedure HTTPRedirect(This: ISuperObject; const location: string);
begin
  This.I['response.response'] := 302;
  This.S['response.env.Location'] := Location;
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
  Response.S['env.Set-Cookie'] := COOKIE_NAME + '=' + StrTobase64(ctx['session'].AsJSon);
  Response.S['Cache-Control'] := 'no-cache';
  inherited;
end;

procedure THTTPConnexion.doBeforeProcessRequest(ctx: ISuperObject);
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
var
  obj: ISuperObject;
begin
  inherited;
  // default values
  ctx.B['session.authenticate'] := true;
  // decode session from cookie
  obj := Request['cookies.' + COOKIE_NAME];
  if obj <> nil then
  case obj.DataType of
    stString: ctx['session'].Merge(Base64ToStr(obj.AsString));
    stArray: ctx['session'].Merge(Base64ToStr(obj.AsArray.S[0]));
  end;

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
     if interprete(PChar(obj.AsArray.S[1]), 'controller') then
     if interprete(PChar(obj.AsArray.S[2]), 'action') then
        interprete(PChar(obj.AsArray.S[3]), 'id');
   end;

  // default action is index
  if (ctx['params.controller'] <> nil) and (ctx['params.action'] = nil) then
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
  valide: ISuperObject;
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

  if ctx['params.controller'] <> nil then
    with ctx['params'] do
    begin
      // controller

      valide := MVC[S['controller'] + '.' + S['action'] + '.validate'];
      if (valide <> nil) and (ctx['params'] <> nil) then
         if not ctx['params'].Validate(valide, MVC['schema']) then
           Exit;

      proc := MVC.M[S['controller'] + '.' + S['action'] + '.controller'];
      if assigned(proc) then
      begin
        obj := nil;
        proc(ctx, ctx['params'], obj);
      end;

      // redirect ? ...
      if Response.I['response'] = 302 then Exit;

      // view
      proc := MVC.M[S['controller'] + '.' + S['action'] + '.' + S['format']];
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
      Request.S['uri'] := Request.S['uri'] + 'index.html' else
    if FileExists(path + Request.S['uri'] + 'index.htm') then
      Request.S['uri'] := Request.S['uri'] + 'index.htm';
  end;

  if FileExists(path + Request.S['uri']) then
  begin
    Response.S['env.Content-Type'] := FFormats.S[ctx.S['params.format']];
    Response.S['sendfile'] := path + Request.S['uri'];
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

function THTTPConnexion.CreateMVC: ISuperObject;
begin
  Result := TSuperObject.Create;

  Result.O['schema'] :=
    so('[{type: map, name: mvc, mapping: {controller: {type: str}, action: {type: str}, format: {type: str}}}]');

  app_controller_initialize(Result);
  app_view_initialize(Result);

end;
    
initialization
  Application.CreateServer(THTTPServer, 81);

end.
