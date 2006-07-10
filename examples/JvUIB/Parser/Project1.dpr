program Project1;

{%File 'JvUIBSQLLexer.l'}
{%File 'JvUIBSQLParser.y'}

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  JvUIBSQLParser in 'JvUIBSQLParser.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
