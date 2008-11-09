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
    constructor Create(Options: ISuperObject); reintroduce; overload;
    constructor Create(const Options: string); reintroduce; overload;
    destructor Destroy; override;
  end;

  TPDGUIBConnection = class(TPDGConnection)
  private
    FLibrary: TUIBLibrary;
    FDbHandle: IscDbHandle;
    FCommandes: ISuperObject;
    FCharacterSet: TCharacterSet;
  protected
    function newContext(Options: ISuperObject = nil): IPDGContext; override;
  public
    constructor Create(Options: ISuperObject); reintroduce; overload;
    constructor Create(const Options: string); reintroduce; overload;
    destructor Destroy; override;
  end;

  TPDGUIBContext = class(TPDGContext)
  private
    FTrHandle: IscTrHandle;
    FConnection: TPDGUIBConnection;
  protected
    function newCommand(Options: ISuperObject = nil): IPDGCommand; override;
  public
    constructor Create(Connection: TPDGUIBConnection; Options: ISuperObject); reintroduce;
    destructor Destroy; override;
  end;

  TPDGUIBCommand = class(TPDGCommand)
  private
    FStHandle: IscStmtHandle;
    FConnection: TPDGUIBConnection;
    FSQLResult: TSQLResult;
    FSQLParams: TSQLParams;
    FStatementType: TUIBStatementType;
  protected
    function Execute(params: ISuperObject = nil; context: IPDGContext = nil): ISuperObject; override;
    function GetInputMeta: ISuperObject; override;
    function GetOutputMeta: ISuperObject; override;
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
  option: AnsiString;
begin
  inherited Create(stObject);
  FDbHandle := nil;

  DataPtr := Self;
  Merge(Options, true);
  FCommandes := TSuperObject.Create(stObject);

  FLibrary := TUIBLibrary.Create;

  param := O['library'];
  if param <> nil then
    FLibrary.Load(string(param.AsString)) else
    FLibrary.Load(GDS32DLL);

  option := 'sql_dialect=3';

  param := O['username'];
  if param <> nil then
    option := option + ';user_name=' + param.AsString;

  param := O['password'];
  if param <> nil then
    option := option + ';password=' + param.AsString;

  param := O['characterset'];
  if param <> nil then
  begin
    FCharacterSet := StrToCharacterSet(param.AsString);
    option := option + ';lc_ctype=' + CharacterSetStr[FCharacterSet];
  end else
    FCharacterSet := csNONE;

  param := O['databasename'];
  if param <> nil then
    FLibrary.AttachDatabase(param.AsString, FDbHandle, option) else
    FDbHandle := nil;

end;

constructor TPDGUIBConnection.Create(const Options: string);
begin
  Create(SO(Options));
end;

destructor TPDGUIBConnection.Destroy;
begin
  if FDbHandle <> nil then
    FLibrary.DetachDatabase(FDbHandle);
  FLibrary.Free;
  inherited;
end;

function TPDGUIBConnection.newContext(Options: ISuperObject): IPDGContext;
begin
  Result := TPDGUIBContext.Create(Self, Options);
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

function TPDGUIBContext.newCommand(Options: ISuperObject): IPDGCommand;
begin
  Result := TPDGUIBCommand.Create(FConnection, Self, Options);
end;

{ TPDGUIBCommand }

constructor TPDGUIBCommand.Create(Connection: TPDGUIBConnection;
  Context: TPDGUIBContext; Options: ISuperObject);
begin
  inherited Create(stObject);
  DataPtr := Self;
  if ObjectIsType(Options, stString) then
    O['sql'] := Options else
    Merge(Options, true);
  FConnection := Connection;
  AsObject.Put('connection', Connection);

  FSQLResult := TSQLResult.Create(Connection.FCharacterSet, 0, false, true);
  FSQLParams := TSQLParams.Create(Connection.FCharacterSet);

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

destructor TPDGUIBCommand.Destroy;
begin
  FSQLResult.Free;
  FSQLParams.Free;
  FConnection.FLibrary.DSQLFreeStatement(FStHandle, DSQL_drop);
  inherited;
end;

function TPDGUIBCommand.Execute(params: ISuperObject; context: IPDGContext): ISuperObject;
var
  dfArray, dfFirstOne: boolean;
  str: AnsiString;

  function getone: ISuperObject;
  var
    i: integer;
    blob: IPDGBlob;
  begin
    if dfArray then
    begin
      Result := TSuperObject.Create(stArray);
      for i := 0 to FSQLResult.FieldCount - 1 do
        if FSQLResult.IsNull[i] then
          Result.AsArray.Add(nil) else
        case FSQLResult.FieldType[i] of
          uftChar, uftVarchar, uftCstring: Result.AsArray.Add(TSuperObject.Create(PAnsiChar(FSQLResult.AsAnsiString[i])));
          uftSmallint, uftInteger, uftInt64: Result.AsArray.Add(TSuperObject.Create(FSQLResult.AsInteger[i]));
          uftNumeric, uftFloat, uftDoublePrecision: Result.AsArray.Add(TSuperObject.Create(FSQLResult.AsDouble[i]));
          uftBlob, uftBlobId:
            begin
              if FSQLResult.Data^.sqlvar[i].SqlSubType = 1 then
              begin
                FSQLResult.ReadBlob(i, str);
                Result.AsArray.Add(TSuperObject.Create(str));
              end else
              begin
                blob := TPDGBinary.Create;
                FSQLResult.ReadBlob(i, blob.getData);
                Result.AsArray.Add(blob as ISuperObject);
              end;
            end;
          uftTimestamp, uftDate, uftTime: Result.AsArray.Add(TSuperObject.Create(DelphiToJavaDateTime(FSQLResult.AsDateTime[i])));
        {$IFDEF IB7_UP}
          uftBoolean: Result.AsArray.Add(TSuperObject.Create(PChar(FSQLResult.AsBoolean[i])));
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
          uftChar, uftVarchar, uftCstring: Result[FSQLResult.AliasName[i]] := TSuperObject.Create(PAnsiChar(FSQLResult.AsAnsiString[i]));
          uftSmallint, uftInteger, uftInt64: Result[FSQLResult.AliasName[i]] := TSuperObject.Create(FSQLResult.AsInteger[i]);
          uftNumeric, uftFloat, uftDoublePrecision: Result[FSQLResult.AliasName[i]] := TSuperObject.Create(FSQLResult.AsDouble[i]);
          uftBlob, uftBlobId:
            begin
              if FSQLResult.Data^.sqlvar[i].SqlSubType = 1 then
              begin
                FSQLResult.ReadBlob(i, str);
                Result[FSQLResult.AliasName[i]] := TSuperObject.Create(str);
              end else
              begin
                blob := TPDGBinary.Create;
                FSQLResult.ReadBlob(i, blob.getData);
                Result[FSQLResult.AliasName[i]] := blob as ISuperObject;
              end;
            end;
          uftTimestamp, uftDate, uftTime: Result[FSQLResult.AliasName[i]] := TSuperObject.Create(DelphiToJavaDateTime(FSQLResult.AsDateTime[i]));
        {$IFDEF IB7_UP}
          uftBoolean: Result[FSQLResult.AliasName[i]] := TSuperObject.Create(PChar(FSQLResult.AsBoolean[i]));
        {$ENDIF}
         else
           Result[FSQLResult.AliasName[i]] := nil;
         end;
    end;
  end;

  procedure SetParam(index: Integer; value: ISuperObject);
  var
    BlobHandle: IscBlobHandle;
    blob: IPDGBlob;
    str: AnsiString;
  begin
    if ObjectIsType(value, stNull) then
      FSQLParams.IsNull[index] := true else
      case FSQLParams.FieldType[index] of
        uftNumeric: FSQLParams.AsDouble[index] := value.AsDouble;
        uftChar, uftVarchar, uftCstring: FSQLParams.AsAnsiString[index] := value.AsString;
        uftSmallint: FSQLParams.AsSmallint[index] := value.AsInteger;
        uftInteger: FSQLParams.AsInteger[index] := value.AsInteger;
        uftFloat: FSQLParams.AsSingle[index] := value.AsDouble;
        uftDoublePrecision: FSQLParams.AsDouble[index] := value.AsDouble;
        uftDate, uftTime, uftTimestamp: FSQLParams.AsDateTime[index] := JavaToDelphiDateTime(value.AsInteger);
        uftInt64: FSQLParams.AsInt64[index] := value.AsInteger;
        uftBlob, uftBlobId:
          with FConnection, FLibrary, TPDGUIBContext((context as ISuperObject).DataPtr) do
          begin
            BlobHandle := nil;
            FSQLParams.AsQuad[Index] := BlobCreate(FDbHandle, FTrHandle, BlobHandle);
            if value.QueryInterface(IPDGBlob, blob) = 0 then
              BlobWriteStream(BlobHandle, blob.getData) else
              begin
                str := value.AsString;
                BlobWriteString(BlobHandle, str);
              end;
            BlobClose(BlobHandle);
          end;
      else
        raise Exception.Create('not yet implemented');
      end;
  end;

  procedure Process;
  begin
    with FConnection, FLibrary, TPDGUIBContext((context as ISuperObject).DataPtr) do
      if FSQLResult.FieldCount > 0 then
      begin
        DSQLSetCursorName(FStHandle, AnsiChar('C') + AnsiString(IntToStr(PtrInt(FStHandle))));
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

function TPDGUIBCommand.GetInputMeta: ISuperObject;
var
  j: Integer;
  rec: ISuperObject;
  prm: PUIBSQLVar;
begin
  if FSQLParams.FieldCount > 0 then
  begin
    Result := TSuperObject.Create(stArray);
    with Result.AsArray do
      for j := 0 to FSQLParams.FieldCount - 1 do
      begin
        rec := TSuperObject.Create(stObject);
        prm := @FSQLParams.Data.sqlvar[j];
        if prm.ParamNameLength > 0 then
          rec.S['name'] := copy(prm.ParamName, 1, prm.ParamNameLength);
        add(rec);
        case FSQLParams.FieldType[j] of
          uftChar, uftVarchar, uftCstring:
          begin
            rec.S['type'] := 'str';
            rec.I['length'] := FSQLParams.SQLLen[j];
          end;
          uftSmallint, uftInteger, uftInt64: rec.S['type'] := 'int';
          uftNumeric, uftFloat, uftDoublePrecision: rec.S['type'] := 'float';
          uftBlob, uftBlobId:
          begin
            if FSQLParams.Data^.sqlvar[j].SqlSubType = 1 then
              rec.S['type'] := 'str' else
              rec.S['type'] := 'bin';
          end;
          uftTimestamp: rec.S['type'] := 'timestamp';
          uftDate: rec.S['type'] := 'date';
          uftTime: rec.S['type'] := 'time';
          {$IFDEF IB7_UP}
          uftBoolean: rec.S['type'] := 'bool';
          {$ENDIF}
        end;
        if not FSQLParams.IsNullable[j] then
          rec.B['notnull'] := true;
      end;
  end else
    Result := nil;
end;

function TPDGUIBCommand.GetOutputMeta: ISuperObject;
var
  j: Integer;
  rec: ISuperObject;
  dfArray: Boolean;
begin
  if FSQLResult.FieldCount > 0 then
  begin
    dfArray := B['array'];
    if dfArray then
      Result := TSuperObject.Create(stArray) else
      Result := TSuperObject.Create(stObject);

      for j := 0 to FSQLResult.FieldCount - 1 do
      begin
        rec := TSuperObject.Create(stObject);
        if dfArray then
        begin
          rec.S['name'] := FSQLResult.AliasName[j];
          Result.asArray.add(rec);
        end else
          Result.AsObject.Put(PAnsiChar(FSQLResult.AliasName[j]), rec);

        case FSQLResult.FieldType[j] of
          uftChar, uftVarchar, uftCstring:
          begin
            rec.S['type'] := 'str';
            rec.I['length'] := FSQLResult.SQLLen[j];
          end;
          uftSmallint, uftInteger, uftInt64: rec.S['type'] := 'int';
          uftNumeric, uftFloat, uftDoublePrecision: rec.S['type'] := 'float';
          uftBlob, uftBlobId:
          begin
            if FSQLResult.Data^.sqlvar[j].SqlSubType = 1 then
              rec.S['type'] := 'str' else
              rec.S['type'] := 'bin';
          end;
          uftTimestamp: rec.S['type'] := 'timestamp';
          uftDate: rec.S['type'] := 'date';
          uftTime: rec.S['type'] := 'time';
          {$IFDEF IB7_UP}
          uftBoolean: rec.S['type'] := 'bool';
          {$ENDIF}
        end;
        if not FSQLResult.IsNullable[j] then
          rec.B['notnull'] := true;
      end;
  end else
    Result := nil;
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

constructor TPDGUIBConnectionPool.Create(const Options: string);
begin
  Create(SO(Options));
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
