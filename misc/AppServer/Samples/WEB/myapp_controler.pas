unit myapp_controler;
{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface
uses superobject;

procedure app_controler_initialize(mvc: ISuperObject);

implementation
uses SysUtils, webserver, mypool, uib;

{ HTTP Methods }

//**************************************************************
// getdata
//**************************************************************

procedure application_getdata_controler(This, Params: ISuperObject;
  var Result: ISuperObject);
var
  db: TUIBDataBase;
  tr: TUIBTransaction;
  qr: TUIBQuery;
begin
  db := pool.GetConnexion;
  tr := TUIBTransaction.Create(nil);
  qr := TUIBQuery.Create(nil);
  try
    tr.DataBase := db;
    qr.Transaction := tr;
    qr.CachedFetch := false;
    qr.SQL.Text := 'select * from ' + Params.S['id'];
    this['dataset'] := QueryToJson(qr, [dfMeta, dfArray]);
  finally
    qr.Free;
    tr.Free;
    pool.FreeConnexion;
  end;
  HTTPCompress(this);
end;

//**************************************************************
// Country
//**************************************************************
procedure country_index(This, Params: ISuperObject; var Result: ISuperObject);
var
  db: TUIBDataBase;
  tr: TUIBTransaction;
  qr: TUIBQuery;
begin
  db := pool.GetConnexion;
  tr := TUIBTransaction.Create(nil);
  qr := TUIBQuery.Create(nil);
  try
    tr.DataBase := db;
    qr.Transaction := tr;
    qr.CachedFetch := false;
    qr.SQL.Text := 'select country, currency from country order by 1';
    this['dataset'] := QueryToJson(qr, [dfArray, dfMeta]);
  finally
    qr.Free;
    tr.Free;
    pool.FreeConnexion;
  end;
  HTTPCompress(this);
end;

procedure country_add(This, Params: ISuperObject; var Result: ISuperObject);
var
  db: TUIBDataBase;
  tr: TUIBTransaction;
  qr: TUIBQuery;
begin
  if HTTPIsPost(this) then
  begin
    db := pool.GetConnexion;
    tr := TUIBTransaction.Create(nil);
    qr := TUIBQuery.Create(nil);
    try
      tr.DataBase := db;
      qr.Transaction := tr;
      qr.CachedFetch := false;
      qr.SQL.Text := 'INSERT INTO COUNTRY (country, currency) VALUES (?,?)';
      qr.Params.AsString[0] := Params.S['country'];
      qr.Params.AsString[1] := Params.S['currency'];
      qr.Execute;
      HTTPredirect(this,'/country/index');
    finally
      qr.Free;;
      tr.Free;
      pool.FreeConnexion;
    end;
  end;
  HTTPCompress(this);
end;

procedure country_del(This, Params: ISuperObject; var Result: ISuperObject);
var
  db: TUIBDataBase;
  tr: TUIBTransaction;
  qr: TUIBQuery;
begin
  try
    db := pool.GetConnexion;
    tr := TUIBTransaction.Create(nil);
    qr := TUIBQuery.Create(nil);
    try
      tr.DataBase := db;
      qr.Transaction := tr;
      qr.CachedFetch := false;
      qr.SQL.Text := 'DELETE FROM COUNTRY WHERE COUNTRY = ?';
      qr.Params.AsString[0] := Params.S['id'];
      qr.Execute;
      HTTPredirect(this,'/country/index');
    finally
      qr.Free;;
      tr.Free;
      pool.FreeConnexion;
    end;
  except
    on E: Exception do
    begin
      Params.S['action'] := 'index';
      This.S['error'] := E.Message;
      country_index(This, Params, Result);
    end;
  end;
  HTTPCompress(this);
end;

procedure country_edit(This, Params: ISuperObject; var Result: ISuperObject);
var
  db: TUIBDataBase;
  tr: TUIBTransaction;
  qr: TUIBQuery;
begin
  try
    db := pool.GetConnexion;
    tr := TUIBTransaction.Create(nil);
    qr := TUIBQuery.Create(nil);
    try
      tr.DataBase := db;
      qr.Transaction := tr;
      qr.CachedFetch := false;
      if HTTPIsPost(this) then
      begin
        qr.SQL.Text := 'UPDATE COUNTRY SET CURRENCY = ? WHERE COUNTRY = ?';
        qr.Params.AsString[0] := Params.S['formulaire.currency'];
        qr.Params.AsString[1] := Params.S['formulaire.country'];
        qr.Execute;
        HTTPredirect(this,'/country/index');
      end else
      begin
        qr.SQL.Text := 'SELECT COUNTRY, CURRENCY FROM COUNTRY WHERE COUNTRY = ?';
        qr.Params.AsString[0] := Params.S['id'];
        qr.Open;
        if not qr.Eof then
        begin
          This.S['country'] := qr.Fields.AsString[0];
          This.S['currency'] := qr.Fields.AsString[1];
        end else
          raise Exception.Create('country not found');
      end;
    finally
      qr.Free;
      tr.Free;
      pool.FreeConnexion;
    end;
  except
    on E: Exception do
    begin
      Params.S['action'] := 'index';
      This.S['error'] := E.Message;
      country_index(This, Params, Result);
    end;
  end;
  HTTPCompress(this);
end;

//**************************************************************
// initialization
//**************************************************************

procedure app_controler_initialize(mvc: ISuperObject);
begin


  mvc.O['application.getdata.validate'] :=
    SO('{type: map, inherit: mvc, mapping: {id: {type: text}}}');

  mvc.M['application.getdata.controler'] := @application_getdata_controler;

  mvc.M['country.index.controler'] := @country_index;
  mvc.O['country.add.validate'] :=
    SO('{type: map, inherit: mvc, mapping: {country: {type: text},currency: {type: text}}}');
  mvc.M['country.add.controler'] := @country_add;
  mvc.M['country.del.controler'] := @country_del;
  mvc.M['country.edit.controler'] := @country_edit;

end;

end.
