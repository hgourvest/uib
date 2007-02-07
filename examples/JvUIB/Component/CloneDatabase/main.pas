unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, jvuib, jvuibmetadata;

type
  TMainForm = class(TForm)
    DataBase1: TJvUIBDataBase;
    CopyButton: TButton;
    Transaction1: TJvUIBTransaction;
    Query1: TJvUIBQuery;
    DataBase2: TJvUIBDataBase;
    Transaction2: TJvUIBTransaction;
    Log: TListBox;
    Source: TButton;
    Destination: TButton;
    procedure CopyButtonClick(Sender: TObject);
    procedure SourceClick(Sender: TObject);
    procedure DestinationClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  MainForm: TMainForm;

implementation
uses jvuibase, jvuiblib, jvuibdatabaseedit;

{$R *.dfm}

procedure TMainForm.SourceClick(Sender: TObject);
begin
  with TUIBDatabaseEditForm.Create(self) do
  begin
    Database := Self.DataBase1;
    ShowModal;
    Free;
  end;
end;

procedure TMainForm.DestinationClick(Sender: TObject);
begin
  with TUIBDatabaseEditForm.Create(self) do
  begin
    Database := Self.DataBase2;
    ShowModal;
    Free;
  end;
end;

procedure TMainForm.CopyButtonClick(Sender: TObject);
var
  metadb: TMetaDataBase;
  i, j, k, l, m: integer;
  sql: string;
  dbhandle: IscDbHandle;
  trhandle: IscTrHandle;
  sthandle: IscStmtHandle;
  blhandle: IscBlobHandle;
  procedure AddLog(const str: string);
  begin
    Log.Items.Add(str);
    Log.ItemIndex := Log.Items.Count - 1;
    Application.ProcessMessages;
  end;

  procedure ExecuteImmediate(const sql: string);
  begin
    try
      Transaction2.ExecuteImmediate(sql);
    except
      AddLog('--- failed ---');
    end;
  end;
begin
  log.Clear;
  DataBase2.Connected := false;
  Screen.Cursor := crHourGlass;
  metadb := TMetaDataBase.Create(nil,-1);
  try
    metadb.LoadFromDatabase(Transaction1);
    DataBase2.CharacterSet := metadb.DefaultCharset;
    DataBase2.SQLDialect := DataBase1.InfoDbSqlDialect;

    if FileExists(DataBase2.DatabaseName) then
      DeleteFile(DataBase2.DatabaseName);
    DataBase2.CreateDatabase(DataBase1.InfoPageSize);

    // ROLES
    for i := 0 to metadb.RolesCount - 1 do
    begin
      AddLog('Create role: ' + metadb.Roles[i].Name);
      ExecuteImmediate(metadb.Roles[i].AsDDL);
    end;

    // UDF
    for i := 0 to metadb.UDFSCount - 1 do
    begin
      AddLog('Create UDF: ' + metadb.UDFS[i].Name);
      ExecuteImmediate(metadb.UDFS[i].AsDDL);
    end;

    // DOMAINS
    for i := 0 to metadb.DomainsCount - 1 do
    begin
      AddLog('Create Domain: ' + metadb.Domains[i].Name);
      ExecuteImmediate(metadb.Domains[i].AsDDL);
    end;

    // GENERATORS
    for i := 0 to metadb.GeneratorsCount - 1 do
    begin
      AddLog('Create Generator: ' + metadb.Generators[i].Name);
      ExecuteImmediate(metadb.Generators[i].AsCreateDLL);
      ExecuteImmediate(metadb.Generators[i].AsAlterDDL);
    end;

    // EXEPTIONS
    for i := 0 to metadb.ExceptionsCount - 1 do
    begin
      AddLog('Create Exception: ' + metadb.Exceptions[i].Name);
      ExecuteImmediate(metadb.Exceptions[i].AsDDL);
    end;

    // EMPTY PROCEDURES
    for i := 0 to metadb.ProceduresCount - 1 do
    begin
      AddLog('Create Empty Procedure: ' + metadb.Procedures[i].Name);
      ExecuteImmediate(metadb.Procedures[i].AsCreateEmptyDDL);
    end;

    // TABLES
    for i := 0 to metadb.TablesCount - 1 do
    begin
      AddLog('Create Table: ' + metadb.Tables[i].Name);
      ExecuteImmediate(metadb.Tables[i].AsDDLNode);
    end;

    // VIEWS
    for i := 0 to metadb.ViewsCount - 1 do
    begin
      AddLog('Create View: ' + metadb.Views[i].Name);
      ExecuteImmediate(metadb.Views[i].AsDDLNode);
    end;

    // TABLES DATA
    dbhandle := DataBase2.DbHandle;
    Transaction2.Commit;
    Transaction2.StartTransaction;
    trhandle := Transaction2.TrHandle;
    for i := 0 to metadb.TablesCount - 1 do
    try
      AddLog('Fill Table: ' + metadb.Tables[i].Name);
      sql := 'select ';
      k := 0;
      for j := 0 to metadb.Tables[i].FieldsCount - 1 do
        if metadb.Tables[i].Fields[j].ComputedSource = '' then
        begin
          if (k = 0) then
            sql := sql + metadb.Tables[i].Fields[j].Name else
            sql := sql + ', ' + metadb.Tables[i].Fields[j].Name;
          inc(k);
        end;
      sql := sql + ' from ' + metadb.Tables[i].Name;
      Query1.SQL.Text := sql;
      Query1.Open;

      if not (Query1.Eof) then
      begin
        sql := format('INSERT INTO %s (%s', [metadb.Tables[i].Name, Query1.Fields.SqlName[0]]);
        for j := 1 to Query1.Fields.FieldCount - 1 do
           sql := sql + ', ' + Query1.Fields.SqlName[j];
        sql := sql + ') VALUES (?';
        for j := 1 to Query1.Fields.FieldCount - 1 do
          sql := sql + ',?';
        sql := sql + ');';
        with DataBase2.Lib do
        begin
          sthandle := nil;
          DSQLAllocateStatement(dbhandle, sthandle);
          DSQLPrepare(dbhandle, trhandle, sthandle, sql, 3, nil);
          m := 0;
          while not Query1.Eof do
          begin
            inc(m);
            // recreate blobs
            for k := 0 to Query1.Fields.FieldCount - 1 do
              case Query1.Fields.FieldType[k] of
                uftBlob, uftBlobId:
                  begin
                    if (not Query1.Fields.IsNull[k]) then
                    begin
                      blhandle := nil;
                      TSQLDA(Query1.Fields).AsQuad[k] := BlobCreate(dbhandle, trhandle, blhandle);
                      BlobWriteSegment(blhandle, Query1.Fields.BlobData[k].Size, Query1.Fields.BlobData[k].Buffer);
                      BlobClose(blhandle);
                    end;
                  end;
              end;
            // recreate array
            for k := 0 to Query1.Fields.ArrayCount - 1 do
              if (not Query1.Fields.IsNull[Query1.Fields.ArrayInfos[k].index]) then
              begin
                l := Query1.Fields.ArrayInfos[k].index;
                TSQLDA(Query1.Fields).AsQuad[l] := QuadNull;
                TSQLDA(Query1.Fields).IsNull[l] := false;
                ArrayPutSlice(
                  dbhandle,
                  trhandle,
                  PGDSQuad(Query1.Fields.Data.sqlvar[l].SqlData)^,
                  Query1.Fields.ArrayInfos[k].info,
                  Query1.Fields.ArrayData[l],
                  Query1.Fields.ArrayInfos[k].size);
              end;
            DSQLExecute(trhandle, sthandle, 3, Query1.Fields);
            if ((m mod 500) = 0) then
              Transaction2.CommitRetaining;
            Query1.Next;

          end;
          DSQLFreeStatement(sthandle, DSQL_drop);
        end;
      end;
      Query1.Close(etmStayIn);
    except
      AddLog('--- failed ---');
      continue;
    end;

    // UNIQUE
    for i := 0 to metadb.TablesCount - 1 do
    for j := 0 to metadb.Tables[i].UniquesCount - 1 do
    begin
      AddLog('Create Unique: ' + metadb.Tables[i].Uniques[j].Name);
      ExecuteImmediate(metadb.Tables[i].Uniques[j].AsDDL);
    end;

    // PRIMARY
    for i := 0 to metadb.TablesCount - 1 do
    for j := 0 to metadb.Tables[i].PrimaryCount - 1 do
    begin
      AddLog('Create Primary: ' + metadb.Tables[i].Primary[j].Name);
      ExecuteImmediate(metadb.Tables[i].Primary[j].AsDDL);
    end;

    // FOREIGN
    for i := 0 to metadb.TablesCount - 1 do
    for j := 0 to metadb.Tables[i].ForeignCount - 1 do
    begin
      AddLog('Create Foreign: ' + metadb.Tables[i].Foreign[j].Name);
      ExecuteImmediate(metadb.Tables[i].Foreign[j].AsDDL);
    end;

    // INDICES
    for i := 0 to metadb.TablesCount - 1 do
    for j := 0 to metadb.Tables[i].IndicesCount - 1 do
    begin
      AddLog('Create Indice: ' + metadb.Tables[i].Indices[j].Name);
      ExecuteImmediate(metadb.Tables[i].Indices[j].AsDDL);
    end;

    // CHECKS
    for i := 0 to metadb.TablesCount - 1 do
    for j := 0 to metadb.Tables[i].ChecksCount - 1 do
    begin
      AddLog('Create Check: ' + metadb.Tables[i].Checks[j].Name);
      ExecuteImmediate(metadb.Tables[i].Checks[j].AsDDL);
    end;

    // TABLE TRIGGERS
    for i := 0 to metadb.TablesCount - 1 do
    for j := 0 to metadb.Tables[i].TriggersCount - 1 do
    begin
      AddLog('Create Trigger: ' + metadb.Tables[i].Triggers[j].Name);
      ExecuteImmediate(metadb.Tables[i].Triggers[j].AsDDL);
      if not metadb.Tables[i].Triggers[j].Active then
        metadb.Tables[i].Triggers[j]
    end;

    // VIEW TRIGGERS
    for i := 0 to metadb.ViewsCount - 1 do
    for j := 0 to metadb.Views[i].TriggersCount - 1 do
    begin
      AddLog('Create Trigger: ' + metadb.Views[i].Triggers[j].Name);
      ExecuteImmediate(metadb.Views[i].Triggers[j].AsDDL);
    end;

    // ALTER PROCEDURES
    for i := 0 to metadb.ProceduresCount - 1 do
    begin
      AddLog('Alter Procedure: ' + metadb.Procedures[i].Name);
      ExecuteImmediate(metadb.Procedures[i].AsAlterDDL);
    end;

    // GRANTS
    for i := 0 to metadb.RolesCount - 1 do
    begin
      for j := 0 to metadb.Roles[i].GrantsCount - 1 do
      begin
         AddLog('Grant To Role: ' + metadb.Roles[i].Grants[j].Name);
         ExecuteImmediate(metadb.Roles[i].Grants[j].AsDDL);
      end;
    end;

    for i := 0 to metadb.TablesCount - 1 do
    begin
      for j := 0 to metadb.Tables[i].GrantsCount - 1 do
      begin
        AddLog('Grant To Table: ' + metadb.Tables[i].Grants[j].Name);
        ExecuteImmediate(metadb.Tables[i].Grants[j].AsDDL);
      end;
      for j := 0 to metadb.Tables[i].FieldsGrantsCount - 1 do
      begin
        AddLog('Grant To TableField: ' + metadb.Tables[i].FieldsGrants[j].Name);
        ExecuteImmediate(metadb.Tables[i].FieldsGrants[j].AsDDL);
      end;
    end;

    for i := 0 to metadb.ViewsCount - 1 do
    begin
      for j := 0 to metadb.Views[i].GrantsCount - 1 do
      begin
        AddLog('Grant To View: ' + metadb.Views[i].Grants[j].Name);
        ExecuteImmediate(metadb.Views[i].Grants[j].AsDDL);
      end;
      for j := 0 to metadb.Views[i].FieldsGrantsCount - 1 do
      begin
        AddLog('Grant To ViewField: ' + metadb.Views[i].FieldsGrants[j].Name);
        ExecuteImmediate(metadb.Tables[i].FieldsGrants[j].AsDDL);
      end;
    end;

    for i := 0 to metadb.ProceduresCount - 1 do
    begin
      for j := 0 to metadb.Procedures[i].GrantsCount - 1 do
      begin
        AddLog('Grant To Procedure: ' + metadb.Procedures[i].Grants[j].Name);
        ExecuteImmediate(metadb.Procedures[i].Grants[j].AsDDL);
      end;
    end;


    Transaction2.Commit;
  finally
    metadb.Free;
    Screen.Cursor := crDefault;
  end;
  AddLog('done :)');
end;

end.
