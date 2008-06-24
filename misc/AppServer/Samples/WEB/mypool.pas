unit mypool;
{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface
uses PDGUtils, SuperObject, uib;

type
  TMyPool = class(TConnexionPool)
  private
    FDatabaseName: string;
    FUserName: string;
    FPassWord: string;
    FSQLDialect: Integer;
  protected
    procedure ConfigureConnexion(Database: TUIBDataBase); override;
  public
    constructor Create(MaxSize: Integer = 0); override;
  end;

type
  TDataFormat = (dfArray, dfFirstOne, dfMeta);
  TDataFormats = set of TDataFormat;

  function QueryToJson(qr: TUIBQuery; df: TDataFormats): ISuperObject;

var
  pool: TMyPool;

implementation
uses SysUtils, uiblib, PDGSocketStub;

{ TMyPool }

procedure TMyPool.ConfigureConnexion(Database: TUIBDataBase);
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

function QueryToJson(qr: TUIBQuery; df: TDataFormats): ISuperObject;
var
  meta, data: ISuperObject;
  str: string;

  function getone: ISuperObject;
  var
    i: integer;
  begin
    if (dfArray in df) then
    begin
      Result := TSuperObject.Create(stArray);
      for i := 0 to qr.Fields.FieldCount - 1 do
        if qr.Fields.IsNull[i] then
          Result.AsArray.Add(nil) else
        case qr.Fields.FieldType[i] of
          uftChar, uftVarchar, uftCstring: Result.AsArray.Add(TSuperObject.Create(PChar(qr.Fields.AsString[i])));
          uftSmallint, uftInteger, uftInt64: Result.AsArray.Add(TSuperObject.Create(qr.Fields.AsInteger[i]));
          uftNumeric, uftFloat, uftDoublePrecision: Result.AsArray.Add(TSuperObject.Create(qr.Fields.AsDouble[i]));
          uftBlob, uftBlobId:
            begin
              qr.ReadBlob(i, str);
              if qr.Fields.Data^.sqlvar[i].SqlSubType = 1 then
                Result.AsArray.Add(TSuperObject.Create(PChar(str))) else
                Result.AsArray.Add(TSuperObject.Create(PChar(StrTobase64(str))));
            end;
          uftTimestamp, uftDate, uftTime: Result.AsArray.Add(TSuperObject.Create(PChar(qr.Fields.AsString[i])));
          {$IFDEF IB7_UP}
           uftBoolean: Result.AsArray.Add(ISuperObject.Create(PChar(qr.Fields.AsBoolean[i])));
           {$ENDIF}
         else
           Result.AsArray.Add(nil);
         end;
    end else
    begin
      Result := TSuperObject.Create(stObject);
      for i := 0 to qr.Fields.FieldCount - 1 do
        if qr.Fields.IsNull[i] then
          Result[qr.Fields.AliasName[i]] := nil else
        case qr.Fields.FieldType[i] of
          uftChar, uftVarchar, uftCstring: Result[qr.Fields.AliasName[i]] := TSuperObject.Create(PChar(qr.Fields.AsString[i]));
          uftSmallint, uftInteger, uftInt64: Result[qr.Fields.AliasName[i]] := TSuperObject.Create(qr.Fields.AsInteger[i]);
          uftNumeric, uftFloat, uftDoublePrecision: Result[qr.Fields.AliasName[i]] := TSuperObject.Create(qr.Fields.AsDouble[i]);
          uftBlob, uftBlobId:
            begin
              qr.ReadBlob(i, str);
              if qr.Fields.Data^.sqlvar[i].SqlSubType = 1 then
                Result[qr.Fields.AliasName[i]] := TSuperObject.Create(PChar(str)) else
                Result[qr.Fields.AliasName[i]] := TSuperObject.Create(PChar(StrTobase64(str)));
            end;
          uftTimestamp, uftDate, uftTime: Result[qr.Fields.AliasName[i]] := TSuperObject.Create(PChar(qr.Fields.AsString[i]));
          {$IFDEF IB7_UP}
           uftBoolean: Result[qr.Fields.AliasName[i]] := ISuperObject.Create(PChar(qr.Fields.AsBoolean[i]));
           {$ENDIF}
         else
           Result[qr.Fields.AliasName[i]] := nil;
         end;
    end;
  end;

var
  i: Integer;
  rec: ISuperObject;
begin
  qr.Open;
  if (dfMeta in df) then
  begin
    Result := TSuperObject.Create(stObject);
    if (dfArray in df) then
      meta := TSuperObject.Create(stArray) else
      meta := TSuperObject.Create(stObject);
    Result.AsObject.Put('meta', meta);
    for i := 0 to qr.Fields.FieldCount - 1 do
    begin
      rec := TSuperObject.Create(stObject);
      if (dfArray in df) then
      begin
        rec.S['name'] := qr.Fields.AliasName[i];
        meta.AsArray.add(rec);
      end else
        meta[qr.Fields.AliasName[i]] := rec;
      case qr.Fields.FieldType[i] of
        uftChar, uftVarchar, uftCstring:
        begin
          rec.S['type'] := 'str';
          rec.I['length'] := qr.Fields.SQLLen[i];
        end;
        uftSmallint, uftInteger, uftInt64: rec.S['type'] := 'int';
        uftNumeric, uftFloat, uftDoublePrecision: rec.S['type'] := 'float';
        uftBlob, uftBlobId:
        begin
          if qr.Fields.Data^.sqlvar[i].SqlSubType = 1 then
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
      if not qr.Fields.IsNullable[i] then
        rec.B['notnull'] := true;
    end;
  end;

  if not (dfFirstOne in df) then
  begin
    data := TSuperObject.Create(stArray);
    if Result <> nil then
      Result.AsObject.Put('data', data) else
      Result := data;
    while not qr.Eof do
    begin
      data.AsArray.Add(getone);
      qr.next;
    end;
  end else
  begin
    if not qr.Eof then
      data := getone else
      data := nil;
    if Result <> nil then
      Result.AsObject.Put('data', data) else
      Result := data;
  end;

end;

initialization
  pool := TMyPool.Create(0);

finalization
  while TPDGThread.ThreadCount > 0 do sleep(100);
  pool.Free;

end.
