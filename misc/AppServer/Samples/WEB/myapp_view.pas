unit myapp_view;

interface
uses superobject;

procedure app_view_initialize(mvc: ISuperObject);

implementation
uses
sysutils, webserver;

//**************************************************************
// getdata
//**************************************************************

procedure application_getdata_html(const This, Params: ISuperObject; var Result: ISuperObject);
begin
  HTTPOutput(this, '<html><body><pre>');
  HTTPOutput(this, this['dataset'], true);
  HTTPOutput(this, '</pre></body></html>');
end;

procedure application_getdata_json(const This, Params: ISuperObject; var Result: ISuperObject);
begin
  HTTPOutput(this, this['dataset'], false);
end;

procedure application_getdata_txt(const This, Params: ISuperObject; var Result: ISuperObject);
begin
  HTTPOutput(this, this['dataset'], true);
end;

//**************************************************************
// country
//**************************************************************
procedure country_index_json(const This, Params: ISuperObject; var Result: ISuperObject);
begin
  HTTPOutput(This, This['{error: error, dataset: dataset}'], false);
end;

procedure country_index_html(const This, Params: ISuperObject; var Result: ISuperObject);
begin
  lua_render(this, this['dataset'], 'script/country.lua');
end;

procedure country_edit_html(const This, Params: ISuperObject; var Result: ISuperObject);
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
