unit PDGDB;
{$ifdef FPC}
{$mode ObjFpc}{$H+}
{$endif}
interface
uses superobject, classes, PDGUtils;

type
  IPDGConnectionPool = interface;
  IPDGConnection = interface;
  IPDGContext = interface;
  IPDGCommand = interface;
  IPDGBlob = interface;

  IPDGConnectionPool = interface
    ['{27621D9A-AAE9-4E24-82F5-A18D84E415F3}']
    function GetConnection: IPDGConnection;
  end;

  IPDGConnection = interface
  ['{843E105A-B8E0-42A9-AFA0-CF5AA843DB8B}']
    function newContext(Options: ISuperObject = nil): IPDGContext; overload;
    function newCommand(Options: ISuperObject = nil): IPDGCommand; overload;
    function newContext(const Options: string): IPDGContext; overload;
    function newCommand(const Options: string): IPDGCommand; overload;
  end;

  IPDGContext = interface
  ['{51992399-2D1A-47EF-9DB1-C5654325F41B}']
    function newCommand(Options: ISuperObject = nil): IPDGCommand; overload;
    function newCommand(const Options: string): IPDGCommand; overload;
    function Execute(Command: IPDGCommand; params: ISuperObject = nil): ISuperObject; overload;
    function Execute(Command: IPDGCommand; params: array of const): ISuperObject; overload;
    function Execute(Command: IPDGCommand; const params: string): ISuperObject; overload;
    function Execute(Command: IPDGCommand; const params: Variant): ISuperObject; overload;
  end;

  IPDGCommand = interface
  ['{A39B974A-96EA-4047-A57B-A2B3EBE7BABD}']
    function Execute(params: ISuperObject = nil; context: IPDGContext = nil): ISuperObject; overload;
    function Execute(params: array of const; context: IPDGContext = nil): ISuperObject; overload;
    function Execute(const params: string; context: IPDGContext = nil): ISuperObject; overload;
    function Execute(const params: Variant; context: IPDGContext = nil): ISuperObject; overload;
  end;

  IPDGBlob = interface
  ['{F478FC21-00B3-49C7-8531-85572AD3C98E}']
    function getData: TStream;
  end;

  // Abstact classes

  TPDGConnection = class(TSuperObject, IPDGConnection)
  protected
    function newContext(Options: ISuperObject = nil): IPDGContext; overload; virtual; abstract;
    function newContext(const Options: string): IPDGContext; overload; virtual;
    function newCommand(Options: ISuperObject = nil): IPDGCommand; overload; virtual;
    function newCommand(const Options: string): IPDGCommand; overload; virtual;
  end;

  TPDGContext = class(TSuperObject, IPDGContext)
  protected
    function newCommand(Options: ISuperObject = nil): IPDGCommand; overload; virtual; abstract;
    function newCommand(const Options: string): IPDGCommand; overload; virtual;
    function Execute(Command: IPDGCommand; params: ISuperObject = nil): ISuperObject; overload; virtual;
    function Execute(Command: IPDGCommand; params: array of const): ISuperObject; overload; virtual;
    function Execute(Command: IPDGCommand; const params: string): ISuperObject; overload; virtual;
    function Execute(Command: IPDGCommand; const params: Variant): ISuperObject; overload; virtual;
  end;

  TPDGCommand = class(TSuperObject, IPDGCommand)
  protected
    function Execute(params: ISuperObject = nil; context: IPDGContext = nil): ISuperObject; overload; virtual; abstract;
    function Execute(params: array of const; context: IPDGContext = nil): ISuperObject; overload; virtual;
    function Execute(const params: string; context: IPDGContext = nil): ISuperObject; overload; virtual;
    function Execute(const params: Variant; context: IPDGContext = nil): ISuperObject; overload; virtual;
  end;

  TPDGBinary = class(TSuperObject, IPDGBlob)
  private
    FStream: TPooledMemoryStream;
  public
    constructor Create(stream: TStream = nil); reintroduce; overload;
    constructor Create(const filename: string); reintroduce; overload;
    constructor Create(buffer: Pointer; len: Integer); reintroduce; overload;
    destructor Destroy; override;
    function Clone: ISuperObject; override;
    function Write(writer: TSuperWriter; format: boolean; level: integer): Integer; override;
    function getData: TStream;

    function AsBoolean: Boolean; override; // true if length > 0
    function AsInteger: SuperInt; override; // stream length
  end;

  function blob(stream: TStream = nil): ISuperObject; overload;
  function blob(const filename: string): ISuperObject; overload;
  function blob(buffer: Pointer; len: Integer): ISuperObject; overload;

implementation

function blob(stream: TStream = nil): ISuperObject; overload;
begin
  Result := TPDGBinary.Create(stream);
end;

function blob(const filename: string): ISuperObject; overload;
begin
  Result := TPDGBinary.Create(filename);
end;

function blob(buffer: Pointer; len: Integer): ISuperObject; overload;
begin
  Result := TPDGBinary.Create(buffer, len);
end;

{ TPDGConnection }

function TPDGConnection.newCommand(Options: ISuperObject): IPDGCommand;
begin
  Result := newContext.newCommand(Options);
end;

function TPDGConnection.newCommand(const Options: string): IPDGCommand;
begin
  Result := newContext.newCommand(Options);
end;

function TPDGConnection.newContext(const Options: string): IPDGContext;
begin
  Result := newContext(SO(Options));
end;

{ TPDGContext }

function TPDGContext.Execute(Command: IPDGCommand;
  const params: Variant): ISuperObject;
begin
  Result := Command.Execute(so(params), Self);
end;

function TPDGContext.newCommand(const Options: string): IPDGCommand;
var
  opt: ISuperObject;
begin
  opt := TSuperObject.Parse(PChar(Options), false);
  if opt <> nil then
    Result := newCommand(opt) else
    Result := newCommand(TSuperObject.Create(PChar(Options)));
end;

function TPDGContext.Execute(Command: IPDGCommand;
  const params: string): ISuperObject;
begin
  Result := Command.Execute(so(params), Self);
end;

function TPDGContext.Execute(Command: IPDGCommand;
  params: array of const): ISuperObject;
begin
  Result := Command.Execute(SA(params), Self);
end;

function TPDGContext.Execute(Command: IPDGCommand;
  params: ISuperObject = nil): ISuperObject;
begin
  Result := Command.Execute(params, Self);
end;

{ TPDGCommand }

function TPDGCommand.Execute(const params: Variant;
  context: IPDGContext): ISuperObject;
begin
  Result := Execute(SO(params), context);
end;

function TPDGCommand.Execute(const params: string;
  context: IPDGContext): ISuperObject;
begin
  Result := Execute(SO(params), context);
end;

function TPDGCommand.Execute(params: array of const;
  context: IPDGContext): ISuperObject;
begin
  Result := Execute(SA(params), context);
end;

{ TPDGBinary }

function TPDGBinary.AsBoolean: Boolean;
begin
  Result := FStream.Size > 0;
end;

function TPDGBinary.AsInteger: SuperInt;
begin
  Result := FStream.Size;
end;

function TPDGBinary.Clone: ISuperObject;
var
  blob: TPDGBinary;
begin
  blob := TPDGBinary.Create;
  blob.FStream.LoadFromStream(FStream);
  Result := blob;
end;

constructor TPDGBinary.Create(stream: TStream);
begin
  inherited Create('[BINARY]');
  FStream := TPooledMemoryStream.Create;
  if Stream <> nil then
    FStream.LoadFromStream(stream);
end;

constructor TPDGBinary.Create(const filename: string);
begin
  inherited Create('[BINARY]');
  FStream := TPooledMemoryStream.Create;
  if filename <> '' then
    FStream.LoadFromFile(filename);
end;

constructor TPDGBinary.Create(buffer: Pointer; len: Integer);
begin
  inherited Create('[BINARY]');
  FStream := TPooledMemoryStream.Create;
  if (buffer <> nil) and (len > 0) then
    FStream.Write(buffer^, len);
end;

destructor TPDGBinary.Destroy;
begin
  FStream.Free;
  inherited;
end;

function TPDGBinary.getData: TStream;
begin
  Result := FStream;
end;

function TPDGBinary.Write(writer: TSuperWriter; format: boolean;
  level: integer): Integer;
const
  Base64Code: PChar = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var
  V: array[0..2] of byte;
  C: array[0..3] of char;
begin
  FStream.Seek(0, soFromBeginning);
  Result := 0;
  inc(Result, writer.Append('"', 1));
  while true do
    case FStream.Read(V, 3) of
    3: begin
         C[0] := Base64Code[(V[0] shr 2) and $3F];
         C[1] := Base64Code[((V[0] shl 4) and $3F) or V[1] shr 4];
         C[2] := Base64Code[((V[1] shl 2) and $3F) or V[2] shr 6];
         C[3] := Base64Code[V[2] and $3F];
         inc(Result, writer.Append(@C, 4));
       end;
    2: begin
         C[0] := Base64Code[(V[0] shr 2) and $3F];
         C[1] := Base64Code[((V[0] shl 4) and $3F) or V[1] shr 4];
         C[2] := Base64Code[((V[1] shl 2) and $3F) or 0    shr 6];
         inc(Result, writer.Append(@C, 3));
         inc(Result, writer.Append('=', 1));
         Break;
       end;
    1: begin
         C[0] := Base64Code[(V[0] shr 2) and $3F];
         C[1] := Base64Code[((V[0] shl 4) and $3F) or 0 shr 4];
         inc(Result, writer.Append(@C, 2));
         inc(Result, writer.Append('==', 2));
         Break;
       end;
    0: begin
         if FStream.Position = 0 then
           inc(Result, writer.Append('A===', 4));
         Break;
       end;
    end;
  inc(Result, writer.Append('"', 1));
end;

end.
