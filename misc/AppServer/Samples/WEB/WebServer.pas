unit WebServer;

interface
uses PDGHTTPStub, PDGSocketStub, Winsock;

type
  TWebServer = class(TSocketServer)
  protected
    function doOnCreateStub(Socket: TSocket; Address: TSockAddrIn): TSocketStub; override;
  end;

  TWebClient = class(THTTPStub)
  protected
    procedure ProcessRequest; override;
  public
    constructor CreateStub(AOwner: TSocketServer; Socket: TSocket;
      Address: TSockAddrIn); override;
  end;

implementation
uses SysUtils, PDGService;

const
  ReadTimeOut: Integer = 60000; // 1 minute

{ TWebServer }

procedure TWebClient.ProcessRequest;
  procedure WriteLine(const str: string);
  begin
    send(SocketHandle, PChar(str + CRLF)^, length(str) + 2, 0);
  end;
  procedure WriteString(const str: string);
  begin
    send(SocketHandle, PChar(str)^, length(str), 0);
  end;
  procedure SendEmpty;
  begin
    WriteLine('Content-Length: 0');
    WriteLine('');
  end;
  procedure SendFile(const filename: string);
  var
    handle, size: Integer;
    buffer: array[0..1023] of byte;
  begin
    if FileExists(filename) then
    begin
      handle := FileOpen(filename, fmOpenRead or fmShareDenyWrite);
      size := FileSeek(handle, 0, 2);
      FileSeek(handle, 0, 0);
      WriteLine(format('Content-Length: %d', [size]));
      WriteLine('');
      size := FileRead(handle, buffer, sizeof(buffer));
      while size > 0 do
      begin
        send(SocketHandle, buffer, size, 0);
        size := FileRead(handle, buffer, sizeof(buffer));
      end;
      FileClose(handle);
    end else
      SendEmpty;
  end;
  procedure SendString(const data: string);
  begin
    WriteLine(format('Content-Length: %d', [length(data)]));
    WriteLine('');
    WriteString(data);
  end;
begin

  WriteLine('HTTP/1.1 ' + HttpResponseStrings[rc200]);
  WriteLine('Server: progdigy server/1.0');
  WriteLine('Content-Type: text/html');

  if Header.URI = '/favicon.ico' then
    SendFile(ExtractFilePath(ParamStr(0)) + 'favicon.ico') else
    SendString('<b>This is a simple http server</b>');
end;

constructor TWebClient.CreateStub(AOwner: TSocketServer; Socket: TSocket;
  Address: TSockAddrIn);
begin
  inherited;
  setsockopt(Socket, SOL_SOCKET, SO_RCVTIMEO, @ReadTimeOut, SizeOf(ReadTimeOut));
end;

{ TWebServer }

function TWebServer.doOnCreateStub(Socket: TSocket;
  Address: TSockAddrIn): TSocketStub;
begin
  Result := TWebClient.CreateStub(Self, Socket, Address);
end;

initialization
  Application.CreateServer(TWebServer, 80);

end.
