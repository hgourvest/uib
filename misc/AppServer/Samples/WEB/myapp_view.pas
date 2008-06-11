unit myapp_view;

interface
uses superobject;

procedure app_view_initialize(mvc: ISuperObject);

implementation
uses sysutils, webserver;

//**************************************************************
// getdata
//**************************************************************

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

procedure application_getdata_txt(This, Params: ISuperObject; var Result: ISuperObject);
begin
  HTTPOutput(this, this['dataset'], true);
end;

//**************************************************************
// country
//**************************************************************
procedure country_index_json(This, Params: ISuperObject; var Result: ISuperObject);
begin
  HTTPOutput(This, This['{error: error, dataset: dataset}'], false);
end;

procedure country_index_html(This, Params: ISuperObject; var Result: ISuperObject);
var
  data, line: TSuperArray;

  i: integer;
begin
  HTTPOutput(this, '<html><body><pre>');
  if This['error'] <> nil then
  begin
    HTTPOutput(This, format('<b>%s</b><br/>', [This.S['error']]));
  end;

  data := this['dataset.data'].AsArray;
  HTTPOutput(this, '<table><th><tr><td>Country</td><td>Currency</td><td>Suppr</td><td>Edit</td></tr></th>');
  for i :=  0 to data.Length - 1 do
  begin
    line := data.O[i].asArray;
    HTTPOutput(this, Format('<tr><td>%s</td><td>%s</td><td><a href="/country/del/%0:s">suppr</a></td><td><a href="/country/edit/%0:s">edit</a></td></tr>',
      [line.S[0] , line.S[1]]));
  end;
  HTTPOutput(this, '</table>');
  HTTPOutput(this, '<form action="/country/add" method="POST"><input type="text" name="country"/><input type="text" name="currency"/><input type="submit"/></form>');
  HTTPOutput(this, '</pre></body></html>');
end;

procedure country_edit_html(This, Params: ISuperObject; var Result: ISuperObject);
begin
  HTTPOutput(this, '<html><body><pre>');
  HTTPOutput(this, Format('<form action="/country/edit" method="POST"><input type="text" name="formulaire.country" value="%s"/><input type="text" name="formulaire.currency" value="%s"/><input type="submit"/></form>',
   [This.S['country'],This.S['currency']]) );
  HTTPOutput(this, '</pre></body></html>');
end;

//**************************************************************
// getconnection
//**************************************************************

procedure app_view_initialize(mvc: ISuperObject);
begin
  mvc.M['application.getdata.json'] := @application_getdata_json;
  mvc.M['application.getdata.txt'] := @application_getdata_txt;
  mvc.M['application.getdata.html'] := @application_getdata_html;

  mvc.M['country.index.html'] := @country_index_html;
  mvc.M['country.index.json'] := @country_index_json;
  mvc.M['country.edit.html'] := @country_edit_html;

end;

end.
