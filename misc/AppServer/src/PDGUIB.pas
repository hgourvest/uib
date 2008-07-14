unit PDGUIB;
{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface
uses 
{$IFDEF MSWINDOWS}
  windows,
{$ENDIF}
  PDGDB, uibase, uiblib, superobject, syncobjs, PDGUtils;

type
  TPDGUIBConnectionPool = class(TSuperObject, IPDGConnectionPool)
  private
    FCriticalSection: TCriticalSection;
  protected
    function GetConnection: IPDGConnection;
  public
    constructor Create(Options: ISuperObject); reintroduce; 
    destructor Destroy; override;
  end;

  TPDGUIBConnection = class(TSuperObject, IPDGConnection)
  private
    FLibrary: TUIBLibrary;
    FDbHandle: IscDbHandle;
    FCommandes: ISuperObject;
  protected
    function newContext(Options: ISuperObject = nil): IPDGContext; overload;
    function newContext(const Options: string): IPDGContext; overload;
    function newCommand(Options: ISuperObject = nil): IPDGCommand; overload;
    function newCommand(const Options: string): IPDGCommand; overload;
  public
    constructor Create(Options: ISuperObject); reintroduce;
    destructor Destroy; override;
  end;

  TPDGUIBContext = class(TSuperObject, IPDGContext)
  private
    FTrHandle: IscTrHandle;
    FConnection: TPDGUIBConnection;
  protected
    function newCommand(Options: ISuperObject = nil): IPDGCommand; overload;
    function newCommand(const Options: string): IPDGCommand; overload;
    function Execute(Command: IPDGCommand; params: ISuperObject = nil): ISuperObject; overload;
    function Execute(Command: IPDGCommand; params: array of const): ISuperObject; overload;
    function Execute(Command: IPDGCommand; const params: string): ISuperObject; overload;
    function Execute(Command: IPDGCommand; const params: Variant): ISuperObject; overload;
  public
    constructor Create(Connection: TPDGUIBConnection; Options: ISuperObject); reintroduce;
    destructor Destroy; override;
  end;

  TPDGCommand = class(TSuperObject, IPDGCommand)
  private
    FStHandle: IscStmtHandle;
    FConnection: TPDGUIBConnection;
    FSQLResult: TSQLResult;
    FSQLParams: TSQLParams;
    FStatementType: TUIBStatementType;
  protected
    function Execute(params: ISuperObject = nil; context: IPDGContext = nil): ISuperObject; overload;
    function Execute(params: array of const; context: IPDGContext = nil): ISuperObject; overload;
    function Execute(const params: string; context: IPDGContext = nil): ISuperObject; overload;
    function Execute(const params: Variant; context: IPDGContext = nil): ISuperObject; overload;
  public
    constructor Create(Connection: TPDGUIBConnection; Context: TPDGUIBContext; Options: ISuperObject); reintroduce;
    destructor Destroy; override;
  end;

implementation
uses sysutils;

{ TPDGUIBConnection }

constructor TPDGUIBConnection.Create(Options: ISuperObject);
var
  param: ISuperObject;
  option: string;
begin
  inherited Create(stObject);
  FDbHandle := nil;
  
  DataPtr := Self;
  Merge(Options, true);
  FCommandes := TSuperObject.Create(stObject);

  FLibrary := TUIBLibrary.Create;

  param := O['library'];
  if param <> nil then
    FLibrary.Load(param.AsString) else
    FLibrary.Load(GDS32DLL);

  option := 'sql_dialect=3';

  param := O['username'];
  if param <> nil then
    option := option + ';user_name='+param.AsString;

  param := O['password'];
  if param <> nil then
    option := option + ';password='+param.AsString;

  param := O['databasename'];
  if param <> nil then
    FLibrary.AttachDatabase(param.AsString, FDbHandle, option) else
    FDbHandle := nil;
end;

destructor TPDGUIBConnection.Destroy;
begin
  if FDbHandle <> nil then
    FLibrary.DetachDatabase(FDbHandle);
  FLibrary.Free;
  inherited;
end;

function TPDGUIBConnection.newCommand(Options: ISuperObject): IPDGCommand;
begin
  Result := newContext.newCommand(Options);
end;

function TPDGUIBConnection.newContext(Options: ISuperObject): IPDGContext;
begin
  Result := TPDGUIBContext.Create(Self, Options);
end;

function TPDGUIBConnection.newCommand(const Options: string): IPDGCommand;
begin
  Result := newContext.newCommand(Options);
end;

function TPDGUIBConnection.newContext(const Options: string): IPDGContext;
begin
  Result := newContext(SO(Options));
end;

{ TPDGUIBContext }

constructor TPDGUIBContext.Create(Connection: TPDGUIBConnection; Options: ISuperObject);
begin
  inherited Create(stObject);
  DataPtr := Self;
  Merge(Options, true);
  FConnection := Connection;
  AsObject.Put('connection', Connection);
  FTrHandle := nil;
  with FConnection, FLibrary do
    TransactionStart(FTrHandle, FDbHandle);
end;

destructor TPDGUIBContext.Destroy;
var
  obj: ISuperObject;
begin
  obj := AsObject.Get('rollback');
  if ObjectIsType(obj, stBoolean) and obj.AsBoolean then
    FConnection.FLibrary.TransactionRollback(FTrHandle) else
    FConnection.FLibrary.TransactionCommit(FTrHandle);
  inherited Destroy;
end;

function TPDGUIBContext.Execute(Command: IPDGCommand;
  const params: Variant): ISuperObject;
begin
  Result := Command.Execute(so(params), Self);
end;

function TPDGUIBContext.newCommand(const Options: string): IPDGCommand;
var
  opt: ISuperObject;
begin
  opt := TSuperObject.Parse(PChar(Options), false);
  if opt <> nil then
    Result := newCommand(opt) else
    Result := newCommand(TSuperObject.Create(PChar(Options)));
end;

function TPDGUIBContext.Execute(Command: IPDGCommand;
  const params: string): ISuperObject;
begin
  Result := Command.Execute(so(params), Self);
end;

function TPDGUIBContext.Execute(Command: IPDGCommand;
  params: array of const): ISuperObject;
begin
  Result := Command.Execute(so(params), Self);
end;

function TPDGUIBContext.Execute(Command: IPDGCommand;
  params: ISuperObject = nil): ISuperObject;
begin
  Result := Command.Execute(params, Self);
end;

function TPDGUIBContext.newCommand(Options: ISuperObject): IPDGCommand;
begin
  Result := TPDGCommand.Create(FConnection, Self, Options);
end;

{ TPDGCommand }

constructor TPDGCommand.Create(Connection: TPDGUIBConnection;
  Context: TPDGUIBContext; Options: ISuperObject);
begin
  inherited Create(stObject);
  DataPtr := Self;
  if ObjectIsType(Options, stString) then
    O['sql'] := Options else
    Merge(Options, true);
  FConnection := Connection;
  AsObject.Put('connection', Connection);

  FSQLResult := TSQLResult.Create(0, false, true);
  FSQLParams := TSQLParams.Create;

  FStHandle := nil;
  with FConnection, FLibrary do
  begin
    DSQLAllocateStatement(FDbHandle, FStHandle);
    FStatementType := DSQLPrepare(FDbHandle, Context.FTrHandle, FStHandle,
      FSQLParams.Parse(self.S['sql']), 3, FSQLResult);
    if (FSQLParams.FieldCount > 0) then
      DSQLDescribeBind(FStHandle, 3, FSQLParams);
  end;
end;

destructor TPDGCommand.Destroy;
begin
  FSQLResult.Free;
  FSQLParams.Free;
  FConnection.FLibrary.DSQLFreeStatement(FStHandle, DSQL_drop);
  inherited;
end;

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
  Result := Execute(SO(params), context);
end;

function TPDGCommand.Execute(params: ISuperObject; context: IPDGContext): ISuperObject;
var
  dfArray, dfFirstOne: boolean;
  str: string;

  function getone: ISuperObject;
  var
    i: integer;
  begin
    if dfArray then
    begin
      Result := TSuperObject.Create(stArray);
      for i := 0 to FSQLResult.FieldCount - 1 do
        if FSQLResult.IsNull[i] then
          Result.AsArray.Add(nil) else
        case FSQLResult.FieldType[i] of
          uftChar, uftVarchar, uftCstring: Result.AsArray.Add(TSuperObject.Create(PChar(FSQLResult.AsString[i])));
          uftSmallint, uftInteger, uftInt64: Result.AsArray.Add(TSuperObject.Create(FSQLResult.AsInteger[i]));
          uftNumeric, uftFloat, uftDoublePrecision: Result.AsArray.Add(TSuperObject.Create(FSQLResult.AsDouble[i]));
          uftBlob, uftBlobId:
            begin
              FSQLResult.ReadBlob(i, str);
              if FSQLResult.Data^.sqlvar[i].SqlSubType = 1 then
                Result.AsArray.Add(TSuperObject.Create(PChar(str))) else
                Result.AsArray.Add(TSuperObject.Create(PChar(StrTobase64(str))));
            end;
          uftTimestamp, uftDate, uftTime: Result.AsArray.Add(TSuperObject.Create(PChar(FSQLResult.AsString[i])));
          {$IFDEF IB7_UP}
           uftBoolean: Result.AsArray.Add(ISuperObject.Create(PChar(FSQLResult.AsBoolean[i])));
           {$ENDIF}
         else
           Result.AsArray.Add(nil);
         end;
    end else
    begin
      Result := TSuperObject.Create(stObject);
      for i := 0 to FSQLResult.FieldCount - 1 do
        if FSQLResult.IsNull[i] then
          Result[FSQLResult.AliasName[i]] := nil else
        case FSQLResult.FieldType[i] of
          uftChar, uftVarchar, uftCstring: Result[FSQLResult.AliasName[i]] := TSuperObject.Create(PChar(FSQLResult.AsString[i]));
          uftSmallint, uftInteger, uftInt64: Result[FSQLResult.AliasName[i]] := TSuperObject.Create(FSQLResult.AsInteger[i]);
          uftNumeric, uftFloat, uftDoublePrecision: Result[FSQLResult.AliasName[i]] := TSuperObject.Create(FSQLResult.AsDouble[i]);
          uftBlob, uftBlobId:
            begin
              FSQLResult.ReadBlob(i, str);
              if FSQLResult.Data^.sqlvar[i].SqlSubType = 1 then
                Result[FSQLResult.AliasName[i]] := TSuperObject.Create(PChar(str)) else
                Result[FSQLResult.AliasName[i]] := TSuperObject.Create(PChar(StrTobase64(str)));
            end;
          uftTimestamp, uftDate, uftTime: Result[FSQLResult.AliasName[i]] := TSuperObject.Create(PChar(FSQLResult.AsString[i]));
          {$IFDEF IB7_UP}
           uftBoolean: Result[FSQLResult.AliasName[i]] := ISuperObject.Create(PChar(FSQLResult.AsBoolean[i]));
          {$ENDIF}
         else
           Result[FSQLResult.AliasName[i]] := nil;
         end;
    end;
  end;

  procedure SetParam(index: Integer; value: ISuperObject);
  begin
    if ObjectIsType(value, stNull) then
      FSQLParams.IsNull[index] := true else
      case FSQLParams.FieldType[index] of
        uftNumeric: FSQLParams.AsDouble[index] := value.AsDouble;
        uftChar, uftVarchar, uftCstring: FSQLParams.AsString[index] := value.AsString;
        uftSmallint: FSQLParams.AsSmallint[index] := value.AsInteger;
        uftInteger: FSQLParams.AsInteger[index] := value.AsInteger;
        uftFloat: FSQLParams.AsSingle[index] := value.AsDouble;
        uftDoublePrecision: FSQLParams.AsDouble[index] := value.AsDouble;
        uftDate, uftTime, uftTimestamp: FSQLParams.AsDateTime[index] := JavaToDelphiDateTime(value.AsInteger);
        uftInt64: FSQLParams.AsInt64[index] := value.AsInteger;
      else
        raise Exception.Create('not yet implemented');
      end;
  end;

  procedure Process;
  begin
    with FConnection, FLibrary, TPDGUIBContext((context as ISuperObject).DataPtr) do
      if FSQLResult.FieldCount > 0 then
      begin
        DSQLSetCursorName(FStHandle, 'C' + inttostr(PtrInt(FStHandle)));
        try
          if (FStatementType = stExecProcedure) then
          begin
            DSQLExecute2(FTrHandle, FStHandle, 3, FSQLParams, FSQLResult);
            dfFirstOne := true;
          end else
            DSQLExecute(FTrHandle, FStHandle, 3, FSQLParams);

          if not dfFirstOne then
          begin
            Result := TSuperObject.Create(stArray);
            while DSQLFetchWithBlobs(FDbHandle, FTrHandle, FStHandle, 3, FSQLResult) do
              Result.AsArray.Add(getone);
          end else
            if DSQLFetchWithBlobs(FDbHandle, FTrHandle, FStHandle, 3, FSQLResult) then
              Result := getone else
              Result := nil;
        finally
          DSQLFreeStatement(FStHandle, DSQL_close);
        end;
      end else
      begin
        DSQLExecute(FTrHandle, FStHandle, 3, FSQLParams);
        Result := nil;
      end;
  end;
var
  j: integer;
  f: TSuperObjectIter;
begin
  dfFirstOne := B['firstone'];
  dfArray := B['array'];

  if context = nil then
    context := FConnection.newContext;

  for j := 0 to FSQLParams.FieldCount - 1 do
    FSQLParams.IsNull[j] := true;
  try
    if FSQLParams.FieldCount > 0 then
    begin
      if ObjectIsType(params, stArray) then
      begin
        with params.AsArray do
        begin
          if (Length = FSQLParams.FieldCount) and not(ObjectGetType(O[0]) in [stObject, stArray]) then
          begin
            for j := 0 to Length - 1 do
              SetParam(j, O[j]);
            Process;
          end else
            if FSQLResult.FieldCount > 0 then
            begin
              Result := TSuperObject.Create(stArray);
              for j := 0 to Length - 1 do
                Result.AsArray.Add(Execute(O[j], context));
            end else
            begin
              for j := 0 to Length - 1 do
                Execute(O[j], context);
            end;
        end;
      end else
      if ObjectIsType(params, stObject) then
      begin
        if ObjectFindFirst(params, f) then
        repeat
          SetParam(FSQLParams.GetFieldIndex(f.key), f.val);
        until not ObjectFindNext(f);
        ObjectFindClose(f);
        Process;
      end else
      begin
        SetParam(0, params);
        Process;
      end;
    end else
      Process;
  except
    (context as ISuperObject).B['rollback'] := true;
    raise;
  end;
end;

{ TPDGUIBConnectionPool }

constructor TPDGUIBConnectionPool.Create(Options: ISuperObject);
begin
  inherited Create(stObject);
  DataPtr := Self;
  O['options'] := Options;
  O['pool'] := TSuperObject.Create(stArray);

  FCriticalSection := TCriticalSection.Create;
end;

destructor TPDGUIBConnectionPool.Destroy;
begin
  FCriticalSection.Free;
  inherited;
end;

function TPDGUIBConnectionPool.GetConnection: IPDGConnection;
var
  ar: TSuperArray;
  cnx: ISuperObject;
  j, k: Integer;
begin
  Result := nil;

  FCriticalSection.Enter;
  try
    ar := O['pool'].AsArray;
    for j := 0 to ar.Length - 1 do
    begin
      cnx := ar.O[j];
      k := cnx._AddRef;
      try
        if k = 3 then
        begin
          Result := cnx as IPDGConnection;
          Exit;
        end;
      finally
        cnx._Release;
	cnx := nil;
      end;
    end;
    if Result = nil then
      Result := TPDGUIBConnection.Create(O['options']);
    ar.Add(Result as ISuperObject);
  finally
    FCriticalSection.Leave;
  end;
end;

end.



