program AppServer;

{$IFDEF CONSOLEAPP}
{$IFNDEF FPC}
  {$APPTYPE CONSOLE}
{$ENDIF}
{$ENDIF}
uses
  //FastMM4,
{$IFDEF madExcept}
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
{$ENDIF}
{$IFDEF UNIX}
  cthreads,
{$ENDIF}
  PDGService in '..\..\src\PDGService.pas',
  PDGSocketStub in '..\..\src\PDGSocketStub.pas',
  WebServer in 'WebServer.pas',
  PDGHTTPStub in '..\..\src\PDGHTTPStub.pas',
  myapp_controller in 'myapp_controller.pas',
  mypool in 'mypool.pas',
  myapp_view in 'myapp_view.pas';

begin
  Application.Name := 'PDGWEBSRV';
  Application.DisplayName := 'Progdigy WEB Server';
  Application.Run;
end.
