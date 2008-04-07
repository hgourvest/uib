program AppServer;

{$IFDEF CONSOLEAPP}
  {$APPTYPE CONSOLE}
{$ENDIF}
uses
  //fastmm4,
  PDGService in '..\..\src\PDGService.pas',
  PDGSocketStub in '..\..\src\PDGSocketStub.pas',
  WebServer in 'WebServer.pas',
  PDGHTTPStub in '..\..\src\PDGHTTPStub.pas';

begin
  Application.Name := 'PDGIWEBSRV';
  Application.DisplayName := 'Progdigy WEB Server';
  Application.Run;
end.
