unit mypool;
{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface
uses PDGUtils, SuperObject, jvUIB;

type
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

  function QueryToJson(qr: TJvUIBQuery): ISuperObject;

var
  pool: TMyPool;

implementation
uses SysUtils, jvuiblib, PDGSocketStub;

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
  begin
    rec := TSuperObject.Create;
    rec.S['name'] := qr.Fields.AliasName[i];
    case qr.Fields.FieldType[i] of
      uftChar, uftVarchar, uftCstring: rec.S['type'] := 'string';
      uftSmallint, uftInteger, uftInt64: rec.S['type'] := 'int';
      uftNumeric, uftFloat, uftDoublePrecision: rec.S['type'] := 'float';
      uftBlob, uftBlobId:
      begin
        if qr.Fields.Data^.sqlvar[i].SqlSubType = 1 then
          rec.S['type'] := 'string' else
          rec.S['type'] := 'binary';
      end;
      uftTimestamp, uftDate, uftTime: rec.S['type'] := 'timestamp';
      {$IFDEF IB7_UP}
      uftBoolean: rec.S['type'] := 'bool';
      {$ENDIF}
    end;
    meta.AsArray.add(rec);
  end;

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
            if qr.Fields.Data^.sqlvar[i].SqlSubType = 1 then
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

initialization
  pool := TMyPool.Create(0);

finalization
  while TPDGThread.ThreadCount > 0 do sleep(100);
  pool.Free;

end.
