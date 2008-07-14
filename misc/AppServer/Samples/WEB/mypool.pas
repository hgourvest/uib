unit mypool;
{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface
uses PDGDB, PDGUIB, SuperObject;

var
  pool: IPDGConnectionPool;

implementation
uses SysUtils, PDGSocketStub, PDGUtils;

initialization
  with SO(FileToString(ExtractFilePath(ParamStr(0)) + 'appserver.json')) do
    pool := TPDGUIBConnectionPool.Create(O['database']);
  
finalization
  while TPDGThread.ThreadCount > 0 do sleep(100);
  pool := nil;

end.
