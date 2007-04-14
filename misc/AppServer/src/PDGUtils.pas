(*
    "The contents of this file are subject to the Mozilla Public License
    Version 1.1 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at
    http://www.mozilla.org/MPL/

    Software distributed under the License is distributed on an "AS IS"
    basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
    License for the specific language governing rights and limitations
    under the License.

    The Initial Developer of the Original Code is
      Henri Gourvest <hgourvest@progdigy.com>.
*)

unit PDGUtils;

{$ALIGN ON}
{$MINENUMSIZE 4}

interface
uses Classes, SysUtils, jvuib
{$IFDEF MSWINDOWS}
, windows
{$ENDIF}
{$IFDEF FPC}
, sockets
, paszlib
{$ELSE}
, WinSock
, PDGZLib
{$ENDIF}
, syncobjs
;

type

  ERemoteError = class(Exception)
  end;

  TPooledMemoryStream = class(TStream)
  private
    FPageSize: integer;
    FList: TList;
    FSize: Integer;
    FPosition: Integer;
  protected
    procedure SetSize(NewSize: Longint); override;
  public
    procedure Clear;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure WriteString(const str: string);
    procedure WriteInteger(const V: Integer);
    function ReadString: string;
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    function SaveToSocket(socket: longint): boolean;
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    function LoadFromSocket(socket: longint; readsize: boolean = true): boolean;
    constructor Create(PageSize: integer = 1024); virtual;
    destructor Destroy; override;
  end;

  TConnexionPool = class
  private
    FActiveCount: integer;
    FCriticalSection: TCriticalSection;
    FList: TList;
    FMaxSize: integer;
    function GetActiveCount: integer;
    function GetCount: integer;
  protected
    procedure ConfigureConnexion(Database: TJvUIBDataBase); virtual; abstract;
  public
    constructor Create(MaxSize: Integer = 0); virtual;
    destructor Destroy; override;
    function GetConnexion: TJvUIBDatabase;
    procedure FreeConnexion;
    function TryDisconnect: boolean;
    function AdjustSize(count: integer): Integer;
    property ActiveCount: integer read GetActiveCount;
    property Count: integer read GetCount;
  end;

function InterLockedRead(var Value: Integer): Integer;

function CompressStream(inStream, outStream: TStream; level: Integer = Z_DEFAULT_COMPRESSION): boolean; overload;
function DecompressStream(inStream, outStream: TStream): boolean; overload;

function CompressStream(inStream: TStream; outSocket: longint; level: Integer = Z_DEFAULT_COMPRESSION): boolean; overload;
function DecompressStream(inSocket: longint; outStream: TStream): boolean; overload;


implementation
{.$IFDEF FPC}
function InterlockedCompareExchange(var Destination: Pointer; Exchange: Pointer; Comperand: Pointer): Pointer stdcall; external 'kernel32.dll' name 'InterlockedCompareExchange';
{.$ENDIF}

function InterlockedRead(var Value: Integer): Integer;
begin
  Result := Integer(InterlockedCompareExchange(Pointer(Value), nil, nil));
end;

type
  PConnexion = ^TConnexion;
  TConnexion = record
    Database: TJvUIBDataBase;
    ThreadId: Cardinal;
  end;

const
  bufferSize = 32768;

function receive(s: longint; var Buf; len, flags: Integer): Integer;
var
 p: PChar;
 r, l: integer;
begin
 Result := 0;
 p := @Buf;
 l := len;
 r := recv(s, p^, l, flags);
 while (r > 0) and (r < l) do
 begin
   inc(Result, r);
   dec(l, r);
   inc(p, r);
   r := recv(s, p^, l, flags);
 end;
 inc(Result, r);
end;

function CompressStream(inStream, outStream: TStream; level: Integer): boolean;
var
  zstream: TZStream;
  zresult: Integer;
  inBuffer: array[0..bufferSize - 1] of byte;
  outBuffer: array[0..bufferSize - 1] of byte;
  inSize: Integer;
  outSize: Integer;
label
  error;
begin
  Result := False;
  FillChar(zstream, SizeOf(zstream), 0);
  if DeflateInit(zstream, level) < Z_OK then
    exit;
  inSize := inStream.Read(inBuffer, bufferSize);
  while inSize > 0 do
  begin
    zstream.next_in := @inBuffer;
    zstream.avail_in := inSize;
    repeat
      zstream.next_out := @outBuffer;
      zstream.avail_out := bufferSize;
      if deflate(zstream, Z_NO_FLUSH) < Z_OK then
        goto error;
      outSize := bufferSize - zstream.avail_out;
      outStream.Write(outBuffer, outSize);
    until (zstream.avail_in = 0) and (zstream.avail_out > 0);
    inSize := inStream.Read(inBuffer, bufferSize);
  end;
  repeat
    zstream.next_out := @outBuffer;
    zstream.avail_out := bufferSize;
    zresult := deflate(zstream, Z_FINISH);
    if zresult < Z_OK then
      goto error;
    outSize := bufferSize - zstream.avail_out;
    outStream.Write(outBuffer, outSize);
  until (zresult = Z_STREAM_END) and (zstream.avail_out > 0);
  Result := deflateEnd(zstream) >= Z_OK;
  exit;
  error:
  deflateEnd(zstream);
end;

function DecompressStream(inStream, outStream: TStream): boolean;
var
  zstream: TZStream;
  zresult: Integer;
  inBuffer: array[0..bufferSize - 1] of byte;
  outBuffer: array[0..bufferSize - 1] of byte;
  inSize: Integer;
  outSize: Integer;
label
  error;
begin
  Result := False;
  FillChar(zstream, SizeOf(zstream), 0);
  if InflateInit(zstream) < Z_OK then
    exit;
  inSize := inStream.Read(inBuffer, bufferSize);
  while inSize > 0 do
  begin
    zstream.next_in := @inBuffer;
    zstream.avail_in := inSize;
    repeat
      zstream.next_out := @outBuffer;
      zstream.avail_out := bufferSize;
      if inflate(zstream, Z_NO_FLUSH) < Z_OK then
        goto error;
      outSize := bufferSize - zstream.avail_out;
      outStream.Write(outBuffer, outSize);
    until (zstream.avail_in = 0) and (zstream.avail_out > 0);
    inSize := inStream.Read(inBuffer, bufferSize);
  end;
  repeat
    zstream.next_out := @outBuffer;
    zstream.avail_out := bufferSize;
    zresult := inflate(zstream, Z_FINISH);
    if zresult < Z_OK then
      goto error;
    outSize := bufferSize - zstream.avail_out;
    outStream.Write(outBuffer, outSize);
  until (zresult = Z_STREAM_END) and (zstream.avail_out > 0);
  Result := inflateEnd(zstream) >= Z_OK;
  exit;
  error:
  inflateEnd(zstream);
end;

function CompressStream(inStream: TStream; outSocket: longint; level: Integer): boolean;
var
  zstream: TZStream;
  zresult: Integer;
  inBuffer: array[0..bufferSize - 1] of byte;
  outBuffer: array[0..bufferSize - 1] of byte;
  inSize: Integer;
  outSize: Integer;
label
  error;
begin
  Result := False;
  FillChar(zstream, SizeOf(zstream), 0);
  if DeflateInit(zstream, level) < Z_OK then
    Exit;
  inSize := inStream.Read(inBuffer, bufferSize);
  while inSize > 0 do
  begin
    zstream.next_in := @inBuffer;
    zstream.avail_in := inSize;
    repeat
      zstream.next_out := @outBuffer;
      zstream.avail_out := bufferSize;
      if deflate(zstream, Z_NO_FLUSH) < Z_OK then
        goto error;

      outSize := bufferSize - zstream.avail_out;
      if outSize > 0 then
      begin
        if send(outSocket, outSize, sizeof(outSize), 0) <> sizeof(outSize) then
          goto error;
        if send(outSocket, outBuffer, outSize, 0) <> outSize then
          goto error;
      end;
    until (zstream.avail_in = 0) and (zstream.avail_out > 0);
    inSize := inStream.Read(inBuffer, bufferSize);
  end;
  repeat
    zstream.next_out := @outBuffer;
    zstream.avail_out := bufferSize;
    zresult := deflate(zstream, Z_FINISH);
    if zresult < Z_OK then
      goto error;
    outSize := bufferSize - zstream.avail_out;
    if outSize > 0 then
    begin
      if send(outSocket, outSize, sizeof(outSize), 0) <> sizeof(outSize) then
        goto error;
      if send(outSocket, outBuffer, outSize, 0) <> outSize then
        goto error;
    end;
  until (zresult = Z_STREAM_END) and (zstream.avail_out > 0);
  outsize := 0;
  if send(outSocket, outSize, sizeof(outSize), 0) <> sizeof(outSize) then
    goto error;
  Result := deflateEnd(zstream) >= Z_OK;
  Exit;
  error:
  deflateEnd(zstream);
end;

function DecompressStream(inSocket: longint; outStream: TStream): boolean;
var
  zstream: TZStream;
  zresult: Integer;
  inBuffer: array[0..bufferSize - 1] of byte;
  outBuffer: array[0..bufferSize - 1] of byte;
  inSize: Integer;
  outSize: Integer;
label
  error;
begin
  Result := False;
  FillChar(zstream, SizeOf(zstream), 0);
  if InflateInit(zstream) < Z_OK then
    exit;
  if receive(inSocket, insize, sizeof(insize), 0) <> sizeof(insize) then
    goto error;
  if insize > 0 then
    if receive(inSocket, inBuffer, insize, 0) <> insize then
      goto error;
  while inSize > 0 do
  begin
    zstream.next_in := @inBuffer;
    zstream.avail_in := inSize;
    repeat
      zstream.next_out := @outBuffer;
      zstream.avail_out := bufferSize;
      if inflate(zstream, Z_NO_FLUSH) < Z_OK then
        goto error;
      outSize := bufferSize - zstream.avail_out;
      if outSize > 0 then
        outStream.Write(outBuffer, outSize);
    until (zstream.avail_in = 0) and (zstream.avail_out > 0);
    if receive(inSocket, insize, sizeof(insize), 0) <> sizeof(insize) then
      goto error;
    if insize > 0 then
    begin
      if receive(inSocket, inBuffer, insize, 0) <> insize then
        goto error;
    end;
  end;
  repeat
    zstream.next_out := @outBuffer;
    zstream.avail_out := bufferSize;
    zresult := inflate(zstream, Z_FINISH);
    if zresult < Z_OK then
      goto error;
    outSize := bufferSize - zstream.avail_out;
    outStream.Write(outBuffer, outSize);
  until (zresult = Z_STREAM_END) and (zstream.avail_out > 0);
  Result := inflateEnd(zstream) >= Z_OK;
  exit;
  error:
  inflateEnd(zstream);
end;

{ TPooledMemoryStream }

procedure TPooledMemoryStream.Clear;
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
    FreeMem(FList[i]);
  FList.Clear;
  FSize := 0;
  FPosition := 0;
end;

constructor TPooledMemoryStream.Create(PageSize: integer);
begin
  Assert(PageSize > 0);
  FPageSize := PageSize;
  FList := TList.Create;
  FSize := 0;
  FPosition := 0;
end;

destructor TPooledMemoryStream.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

procedure TPooledMemoryStream.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

function TPooledMemoryStream.LoadFromSocket(socket: longint; readsize: boolean = true): boolean;
var
  s, count, i: integer;
begin
  Result := False;
  if readsize then
  begin
    if receive(socket, count, sizeof(count), 0) <> sizeof(count) then Exit;
    SetSize(count);
  end else
    count := Size;
  i := 0;
  while count > 0 do
  begin
    if count > FPageSize then
      s := FPageSize else
      s := count;
    if receive(socket, FList[i]^, s, 0) <> s then exit;
    dec(count, s);
    inc(i);
  end;
  Result := true;
end;

procedure TPooledMemoryStream.LoadFromStream(Stream: TStream);
var
  s, count, i: integer;
begin
  Stream.Position := 0;
  SetSize(Stream.Size);
  count := FSize;
  i := 0;
  while count > 0 do
  begin
    if count > FPageSize then
      s := FPageSize else
      s := count;
    stream.ReadBuffer(FList[i]^, s);
    dec(count, s);
    inc(i);
  end;
end;

function TPooledMemoryStream.Read(var Buffer; Count: Integer): Longint;
var
  Pos, n: Integer;
  p, c: Pointer;
begin
  if (FPosition >= 0) and (Count >= 0) then
  begin
    Pos := FPosition + Count;
    if Pos > 0 then
    begin
      if Pos > FSize then
        count := FSize - FPosition;
      Result := Count;
      c := @buffer;
      n := FPageSize - (FPosition mod FPageSize);
      if n > count then n := count;
      while n > 0 do
      begin
        p := Pointer(Integer(FList[FPosition div FPageSize]) + (FPosition mod FPageSize));
        Move(p^, c^, n);
        dec(count, n);
        inc(Integer(c), n);
        inc(FPosition, n);
        if count >= FPageSize then
          n := FPageSize else
          n := count;
      end;
      Exit;
    end;
  end;
  Result := 0;
end;

function TPooledMemoryStream.ReadString: string;
var
  s: Integer;
begin
  Result := '';
  Read(s, sizeof(s));
  if s > 0 then
  begin
    SetLength(Result, s);
    Read(Result[1], s);
  end;
end;

procedure TPooledMemoryStream.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

function TPooledMemoryStream.SaveToSocket(socket: longint): boolean;
var
  s, count, i: integer;
begin
  Result := False;
  count := FSize;
  if send(socket, count, sizeof(count), 0) <> sizeof(count) then Exit;
  i := 0;
  while count > 0 do
  begin
    if count >= FPageSize then
      s := FPageSize else
      s := count;
    if send(socket, FList[i]^, s, 0) <> s then Exit;
    dec(count, s);
    inc(i);
  end;
  Result := True;
end;

procedure TPooledMemoryStream.SaveToStream(Stream: TStream);
var
  s, count, i: integer;
begin
  count := FSize;
  i := 0;
  while count > 0 do
  begin
    if count >= FPageSize then
      s := FPageSize else
      s := count;
    stream.WriteBuffer(FList[i]^, s);
    dec(count, s);
    inc(i);
  end;
end;

function TPooledMemoryStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent: Inc(FPosition, Offset);
    soFromEnd: FPosition := FSize + Offset;
  end;
  Result := FPosition;
end;

procedure TPooledMemoryStream.SetSize(NewSize: Integer);
var
  count, i: integer;
  p: Pointer;
begin
  if (NewSize mod FPageSize) > 0 then
    count := (NewSize div FPageSize) + 1 else
    count := (NewSize div FPageSize);
  if (count > FList.Count) then
  begin
    for i := FList.Count to count - 1 do
    begin
      GetMem(p, FPageSize);
      FList.Add(p);
    end;
  end else
  if (count < FList.Count) then
  begin
    for i := FList.Count - 1 downto Count do
    begin
      FreeMem(FList[i]);
      FList.Delete(i);
    end;
  end;
  FSize := NewSize;
  if FPosition > FSize then
    Seek(0, soFromEnd);
end;

function TPooledMemoryStream.Write(const Buffer; Count: Integer): Longint;
var
  Pos, n: Integer;
  p, c: Pointer;
begin
  if (FPosition >= 0) and (Count >= 0) then
  begin
    Pos := FPosition + Count;
    if Pos > 0 then
    begin
      Result := Count;
      if Pos > FSize then
        SetSize(Pos);
      c := @buffer;
      n := FPageSize - (FPosition mod FPageSize);
      if n > count then n := count;
      while n > 0 do
      begin
        p := Pointer(Integer(FList[FPosition div FPageSize]) + (FPosition mod FPageSize));
        Move(c^, p^, n);
        dec(count, n);
        inc(Integer(c), n);
        inc(FPosition, n);
        if count >= FPageSize then
          n := FPageSize else
          n := count;
      end;
      Exit;
    end;
  end;
  Result := 0;
end;

procedure TPooledMemoryStream.WriteInteger(const V: Integer);
begin
  Write(v, sizeof(v));
end;

procedure TPooledMemoryStream.WriteString(const str: string);
var
  s: Integer;
begin
  s := Length(str);
  Write(s, sizeof(s));
  if s > 0 then
    Write(str[1], s)
end;

{ TConnexionPool }

constructor TConnexionPool.Create(MaxSize: Integer = 0);
begin
  FMaxSize := MaxSize;
  FCriticalSection := TCriticalSection.Create;
  FList := TList.Create;              
  FActiveCount := 0;
end;

destructor TConnexionPool.Destroy;
var i: integer;
begin
  // Wait for theads leaving
  FCriticalSection.Free;
  for i := 0 to FList.Count - 1 do
  begin
    PConnexion(FList[i])^.Database.Free;
    FreeMem(FList[i]);
  end;
  FList.Free;
  inherited;
end;

function TConnexionPool.TryDisconnect: boolean;
var i: integer;
begin
  FCriticalSection.Enter;
  try
    if FActiveCount > 0 then
    begin
      Result := False;
      Exit;
    end else
    begin
      for i := 0 to FList.Count - 1 do
      begin
        Assert(PConnexion(FList[i])^.ThreadId = 0);
        PConnexion(FList[i])^.Database.Connected := False;
      end;
      Result := True;
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TConnexionPool.FreeConnexion;
var
  i: integer;
  tid: Cardinal;
begin
  FCriticalSection.Enter;
  try
    tid := GetCurrentThreadId;
    for i := 0 to FList.Count - 1 do
      if PConnexion(FList[i])^.ThreadId = tid then
      begin
        PConnexion(FList[i])^.ThreadId := 0;
        if (FMaxSize > 0) and (FActiveCount > FMaxSize) then
        begin
          PConnexion(FList[i])^.Database.Free;
          FreeMem(FList[i]);
          FList.Delete(i);
        end;
        dec(FActiveCount);
        Break;
      end;
  finally
    FCriticalSection.Leave;
  end;
end;

function TConnexionPool.GetConnexion: TJvUIBDatabase;
var
  i: integer;
  tid: cardinal;
  p: PConnexion;
begin
  FCriticalSection.Enter;
  try
    // one connexion per thread
    tid := GetCurrentThreadId;
    for i := 0 to FList.Count - 1 do
      if PConnexion(FList[i])^.ThreadId = tid then
      begin
        Result := PConnexion(FList[i])^.Database;
        Exit;
      end;

    // get free slot
    for i := 0 to FList.Count - 1 do
      if PConnexion(FList[i])^.ThreadId = 0 then
      begin
        PConnexion(FList[i])^.ThreadId := tid;
        Result := PConnexion(FList[i])^.Database;
        inc(FActiveCount);
        Exit;
      end;

    // Create new slot
    GetMem(p, SizeOf(TConnexion));
    p^.ThreadId := tid;
    inc(FActiveCount);
    p^.Database := TJvUIBDataBase.Create(nil);
    ConfigureConnexion(p^.Database);
    FList.Add(p);
    Result := p^.Database;
  finally
    FCriticalSection.Leave;
  end;
end;

function TConnexionPool.GetActiveCount: integer;
begin
  FCriticalSection.Enter;
  try
    result := FActiveCount;
  finally
    FCriticalSection.Leave;
  end;
end;

function TConnexionPool.GetCount: integer;
begin
  FCriticalSection.Enter;
  try
    result := FList.Count;
  finally
    FCriticalSection.Leave;
  end;
end;

function TConnexionPool.AdjustSize(count: integer): Integer;
var i: integer;
begin
  Result := 0;
  FCriticalSection.Enter;
  try
    if FActiveCount > count then
      count := FList.Count - FActiveCount else
      count := FList.Count - count;
    for i := 0 to FList.Count - 1 do
      if count > 0 then
      begin
        if PConnexion(FList[i])^.ThreadId = 0 then
        begin
          dec(count);
          if PConnexion(FList[i])^.Database.Connected then
          begin
            inc(Result);
            PConnexion(FList[i])^.Database.Connected := False;
          end;
        end;
      end else
        Break;
  finally
    FCriticalSection.Leave;
  end;
end;
      
end.
