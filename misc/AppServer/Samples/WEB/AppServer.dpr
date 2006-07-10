program AppServer;

{$IFDEF CONSOLEAPP}
  {$APPTYPE CONSOLE}
{$ENDIF}
uses
  madExcept,
  madLinkDisAsm,
  WinSvc,
  PDGService in '..\..\src\PDGService.pas',
  PDGSocketStub in '..\..\src\PDGSocketStub.pas',
  WebServer in 'WebServer.pas';

begin
  Application.Name := 'PDGIWEBSRV';
  Application.DisplayName := 'Progdigy WEB Server';
  Application.Run;
end.
