program AppServer;

{$IFDEF CONSOLEAPP}
  {$APPTYPE CONSOLE}
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
  myapp_controler in 'myapp_controler.pas',
  mypool in 'mypool.pas',
  myapp_view in 'myapp_view.pas';

begin
  Application.Name := 'PDGIWEBSRV';
  Application.DisplayName := 'Progdigy WEB Server';
  Application.Run;
end.
