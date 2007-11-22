program CloneDB;

uses
  FastMM4,
  Forms,
  main in 'main.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Clony&Pumpy - The Famous Firebird Database Tool';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
