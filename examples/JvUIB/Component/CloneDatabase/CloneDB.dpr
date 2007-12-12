program CloneDB;

uses
  Forms,
  exceptionform in 'exceptionform.pas' {ExceptionDialog},
  main in 'main.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Clony&Pumpy - The Famous Firebird Database Tool';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
