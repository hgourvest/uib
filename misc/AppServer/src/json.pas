(*
 *                         JSON Toolkit
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * Unit owner : Henri Gourvest <hgourvest@progdigy.com>
 *
 * This unit is inspired from the json c api:
 *   Michael Clark <michael@metaparadigm.com>
 *   http://oss.metaparadigm.com/json-c/
 *)

unit json;
{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
interface
uses Classes;

// this option affect case sensitivity.
{.$DEFINE JSON_CASE_INSENSITIVE}

{$IFNDEF FPC}
type
  PtrInt = longint;
  PtrUInt = Longword;
{$ENDIF}

const
  // golden prime used in hash functions
  LH_PRIME = $9e370001;

  // sentinel pointer value for empty slots
  LH_EMPTY = Pointer(-1);

  // sentinel pointer value for freed slots
  LH_FREED = Pointer(-2);

  ARRAY_LIST_DEFAULT_SIZE = 32;

  JSON_OBJECT_DEF_HASH_ENTIRES = 16;

type
  // forward declarations
  TJsonObject = class;

  // An entry in the hash table
  PLHEntry = ^TLHEntry;
  TLHEntry = record
    k: Pointer; // The key.
    v: Pointer; // The value.
    h: cardinal;
    next: PLHEntry; // The next entry
    prev: PLHEntry; // The previous entry.
  end;
  TLHEntryArray = array[0..(high(PtrInt) div sizeof(TLHEntry))-1] of TLHEntry;
  PLHEntryArray = ^TLHEntryArray;

  TObjectArray = array[0..(high(PtrInt) div sizeof(TJsonObject))-1] of TJsonObject;
  PObjectArray = ^TObjectArray;

  // The hash table structure.
  TJsonTable = class
  private
    FSize: Cardinal; // Size of our hash.
    FCount: Integer; // Numbers of entries.
    FCollisions: Integer; // Number of collisions.
    FResizes: Integer; // Number of resizes.
    FLookups: Integer; // Number of lookups.
    FInserts: Integer; // Number of inserts.
    FDeletes: Integer; // Number of deletes.
    FName: PChar; // Name of the hash table.
    FHead: PLHEntry; // The first entry.
    FTail: PLHEntry; // The last entry.
    FTable: PLHEntryArray;
  protected
    class function doHash(k: Pointer): Cardinal; virtual; abstract;
    class function doEqual(k1, k2: Pointer): integer; virtual; abstract;
    class procedure doDeleteEntry(e: PLHEntry); virtual;
    procedure InternalResize(new_size: Integer); virtual;
    function InternalInsert(k, v: Pointer): Integer; virtual;
    function InternalAdd(k, v: Pointer): Integer; virtual;
    function InternalDelete(k: Pointer): Integer; virtual;
    function InternalDeleteEntry(e: PLHEntry): Integer; virtual;
    function InternalLookup(k: Pointer): Pointer; virtual;
    function InternalLookupEntry(k: Pointer): PLHEntry; virtual;
  public
    constructor Create(AName: PChar; size: Integer = JSON_OBJECT_DEF_HASH_ENTIRES); virtual;
    destructor Destroy; override;
    property Name: PChar read FName;
    property Count: Integer read FCount;
    property Head: PLHEntry read FHead;
    property Tail: PLHEntry read FTail;
  end;

  TJsonTableClass = class of TJsonTable;

  TJsonTablePointer = class(TJsonTable)
  protected
    class function doHash(k: Pointer): Cardinal; override;
    class function doEqual(k1, k2: Pointer): integer; override;
  end;

  TJsonTableString = class(TJsonTable)
  protected
    class function doHash(k: Pointer): Cardinal; override;
    class function doEqual(k1, k2: Pointer): integer; override;
  protected
    function InternalAdd(k, v: Pointer): Integer; override;
  public
    function Delete(k: PChar): Integer;
    function LookupEntry(k: PChar): PLHEntry; virtual;
  end;


  TJsonTableObject = class(TJsonTableString)
  protected
    class procedure doDeleteEntry(e: PLHEntry); override;
  public
    procedure Put(k: PChar; v: TJsonObject);
    function Get(k: PChar): TJsonObject;
    property Items[k: PChar]: TJsonObject read Get write Put; default;
  end;

  TJsonRpcMethod = procedure(Params: TJsonObject; out Result: TJsonObject) of object;

  TJsonRpcService = class(TJsonTableString)
  protected
    class procedure doDeleteEntry(e: PLHEntry); override;
  public
    constructor Create(Aname: PChar); reintroduce; virtual;
    destructor Destroy; override;
    procedure RegisterMethod(Aname: PChar; Sender: TObject; Method: Pointer);
    procedure Invoke(Obj: TJsonTableObject; out Result: TJsonObject; out error: string);
  end;

  TJsonRpcClass = class of TJsonRpcService;

  TJsonRpcServiceList = class(TJsonTableString)
  protected
    class procedure doDeleteEntry(e: PLHEntry); override;
  public
    procedure RegisterService(obj: TJsonRpcService);
    function Invoke(service: TJsonRpcService; Obj: TJsonObject; var error: string): TJsonObject; overload;
    function Invoke(service: PChar; s: PChar): TJsonObject; overload;
  end;

  TJsonArray = class
  private
    FArray: PObjectArray;
    FLength: Integer;
    FSize: Integer;
    function Expand(max: Integer): Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add(Data: TJsonObject): Integer;
    function Get(i: Integer): TJsonObject;
    procedure Put(i: Integer; Data: TJsonObject);
    property Length: Integer read FLength;
    property Items[i: Integer]: TJsonObject read Get write Put; default;
  end;

  TJsonWriter = class
  protected
    // abstact methods to overide
    function Append(buf: PChar; Size: Integer): Integer; overload; virtual; abstract;
    function Append(buf: PChar): Integer; overload; virtual; abstract;
    procedure Reset; virtual; abstract;
  public
    function Write(obj: TJsonObject; format: boolean; level: integer): Integer; virtual;
  end;

  TJsonWriterString = class(TJsonWriter)
  private
    FBuf: PChar;
    FBPos: integer;
    FSize: integer;
  protected
    function Append(buf: PChar; Size: Integer): Integer; overload; override;
    function Append(buf: PChar): Integer; overload; override;
    procedure Reset; override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Data: PChar read FBuf;
    property Size: Integer read FSize;
    property Position: integer read FBPos;
  end;

  TJsonWriterStream = class(TJsonWriter)
  private
    FStream: TStream;
  protected
    function Append(buf: PChar; Size: Integer): Integer; override;
    function Append(buf: PChar): Integer; override;
    procedure Reset; override;
  public
    constructor Create(AStream: TStream); reintroduce; virtual;
  end;

  TJsonWriterFake = class(TJsonWriter)
  private
    FSize: Integer;
  protected
    function Append(buf: PChar; Size: Integer): Integer; override;
    function Append(buf: PChar): Integer; override;
    procedure Reset; override;
  public
    constructor Create; reintroduce; virtual;
    property size: integer read FSize;
  end;

  TJsonWriterSock = class(TJsonWriter)
  private
    FSocket: longint;
    FSize: Integer;
  protected
    function Append(buf: PChar; Size: Integer): Integer; override;
    function Append(buf: PChar): Integer; override;
    procedure Reset; override;
  public
    constructor Create(ASocket: longint); reintroduce; virtual;
    property Socket: longint read FSocket;
    property Size: Integer read FSize;
  end;

  // supported object types
  TJsonType = (
    json_type_null,
    json_type_boolean,
    json_type_double,
    json_type_int,
    json_type_object,
    json_type_array,
    json_type_string
  );

  TJsonObject = class
  private
    FJsonType: TJsonType;
    Fpb: TJsonWriterString;
    FRefCount: Integer;
    FDataPtr: Pointer;
    o: record
      case TJsonType of
        json_type_boolean: (c_boolean: boolean);
        json_type_double: (c_double: double);
        json_type_int: (c_int: integer);
        json_type_object: (c_object: TJsonTableObject);
        json_type_array: (c_array: TJsonArray);
        json_type_string: (c_string: PChar);
      end;
    function GetJsonType: TJsonType;
  public
    // refcount
    function AddRef: Integer;
    function Release: Integer;

    // Writers
    function SaveTo(stream: TStream; format: boolean = false): integer; overload;
    function SaveTo(const FileName: string; format: boolean = false): integer; overload;
    function SaveTo(socket: longint; format: boolean = false): integer; overload;
    function CalcSize(format: boolean = false): integer;
    function AsJSon(format: boolean = false): PChar;

    // parser
    class function Parse(s: PChar): TJsonObject;

    // constructors / destructor
    constructor Create(jt: TJsonType = json_type_object); overload; virtual;
    constructor Create(b: boolean); overload; virtual;
    constructor Create(i: Integer); overload; virtual;
    constructor Create(d: double); overload; virtual;
    constructor Create(p: PChar); overload; virtual;
    constructor Create(const s: string); overload; virtual;
    destructor Destroy; override;
    procedure Free; reintroduce; // objects are refcounted

    function AsBoolean: Boolean;
    function AsInteger: Integer;
    function AsDouble: Double;
    function AsString: PChar;
    function AsArray: TJsonArray;
    function AsObject: TJsonTableObject;

    // clone a node
    function Clone: TJsonObject;

    // the json data type
    function IsType(AType: TJsonType): boolean;
    property JsonType: TJsonType read GetJsonType;
    // a data pointer to link to something ele, a treeview for example
    property DataPtr: Pointer read FDataPtr write FDataPtr;
  end;

  TJsonObjectIter = record
    key: PChar;
    val: TJsonObject;
    entry: PLHEntry;
  end;

  TJsonTokenerError = (
    json_tokener_success,
    json_tokener_error_parse_unexpected,
    json_tokener_error_parse_null,
    json_tokener_error_parse_boolean,
    json_tokener_error_parse_number,
    json_tokener_error_parse_array,
    json_tokener_error_parse_object,
    json_tokener_error_parse_string,
    json_tokener_error_parse_comment,
    json_tokener_error_parse_eof
  );

  TJsonTokenerState = (
    json_tokener_state_eatws,
    json_tokener_state_start,
    json_tokener_state_finish,
    json_tokener_state_null,
    json_tokener_state_comment_start,
    json_tokener_state_comment,
    json_tokener_state_comment_eol,
    json_tokener_state_comment_end,
    json_tokener_state_string,
    json_tokener_state_string_escape,
    json_tokener_state_escape_unicode,
    json_tokener_state_boolean,
    json_tokener_state_number,
    json_tokener_state_array,
    json_tokener_state_array_sep,
    json_tokener_state_object,
    json_tokener_state_object_field_start,
    json_tokener_state_object_field,
    json_tokener_state_object_field_end,
    json_tokener_state_object_value,
    json_tokener_state_object_sep
  );




function JsonIsError(obj: TJsonObject): boolean;
function JsonIsValid(obj: TJsonObject): boolean;

function JsonFindFirst(obj: TJsonObject; var F: TJsonObjectIter): boolean;
function JsonFindNext(var F: TJsonObjectIter): boolean;

implementation
uses sysutils
{$IFDEF UNIX}
  libc,
{$ENDIF}
{$IFDEF FPC}
, sockets
{$ELSE}
, WinSock
{$ENDIF};

const
  json_number_chars = '0123456789.+-e';
  json_number_chars_set = ['0'..'9','.','+','-','e'];
  json_hex_chars = '0123456789abcdef';
  json_hex_chars_set = ['0'..'9','a'..'f'];

{$ifdef MSWINDOWS}
  function sprintf(buffer, format: PChar): longint; varargs; cdecl; external 'msvcrt.dll';
{$endif}

function FloatToStr(d: Double): string;
begin
  setlength(Result, sprintf(nil,'%lf', d));
  sprintf(PChar(Result), '%lf', d);
end;

function strdup(s: PChar): PChar;
var
  l: Integer;
begin
  if s <> nil then
  begin
    l := StrLen(s);
    GetMem(Result, l+1);
    move(s^, Result^, l);
    Result[l] := #0;
  end else
    Result := nil;
end;

function JsonIsError(obj: TJsonObject): boolean;
begin
  Result := PtrUInt(obj) > Cardinal(-4000);
end;

function JsonIsValid(obj: TJsonObject): boolean;
begin
  Result := not ((obj = nil) or (PtrUInt(obj) > Cardinal(-4000)))
end;

function JsonFindFirst(obj: TJsonObject; var F: TJsonObjectIter): boolean;
begin
  F.entry := obj.AsObject.FHead;
  if F.entry <> nil then
  begin
    F.key := PChar(F.entry^.k);
    F.val := TJsonObject(F.entry^.v);
    Result := True;
  end else
    Result := False;
end;

function JsonFindNext(var F: TJsonObjectIter): boolean;
begin
  F.entry := F.entry^.next;
  if F.entry <> nil then
  begin
    F.key := PChar(F.entry^.k);
    F.val := TJsonObject(F.entry^.v);
    Result := True;
  end else
    Result := False;
end;

{ TJsonObject }

constructor TJsonObject.Create(jt: TJsonType);
begin
  inherited Create;
  FRefCount := 1;
  FDataPtr := nil;
  FJsonType := jt;
  case FJsonType of
    json_type_object: o.c_object := TJsonTableObject.Create('', JSON_OBJECT_DEF_HASH_ENTIRES);
    json_type_array: o.c_array := TJsonArray.Create;
  else
    o.c_object := nil;
  end;
  Fpb := nil;
end;

constructor TJsonObject.Create(b: boolean);
begin
  Create(json_type_boolean);
  o.c_boolean := b;
end;

constructor TJsonObject.Create(i: Integer);
begin
  Create(json_type_int);
  o.c_int := i;
end;

constructor TJsonObject.Create(d: double);
begin
  Create(json_type_double);
  o.c_double := d;
end;

function TJsonObject.AddRef: Integer;
begin
  if JsonIsValid(Self) then
  begin
    inc(FRefCount);
    Result := FRefCount;
  end else
    Result := 0;
end;

constructor TJsonObject.Create(p: PChar);
begin
  Create(json_type_string);
  o.c_string := strdup(p);
end;

destructor TJsonObject.Destroy;
begin
  Assert(FRefCount = 0, '');
  case FJsonType of
    json_type_object: o.c_object.Free;
    json_type_string: FreeMem(o.c_string);
    json_type_array: o.c_array.Free;
  end;
  if Fpb <> nil then Fpb.Free;
  inherited;
end;

function TJsonObject.Release: Integer;
begin
  if JsonIsValid(Self) then
  begin
    dec(FRefCount);
    Result := FRefCount;
    if FRefCount = 0 then
      Destroy;
  end else
    Result := 0;
end;

function TJsonObject.IsType(AType: TJsonType): boolean;
begin
  Result := AType = FJsonType; 
end;

function TJsonObject.AsBoolean: boolean;
begin
  if JsonIsValid(Self) then
  case FJsonType of
    json_type_boolean: Result := o.c_boolean;
    json_type_int: Result := (o.c_int <> 0);
    json_type_double: Result := (o.c_double <> 0);
    json_type_string: Result := (strlen(o.c_string) <> 0);
  else
    Result := True;
  end else
    Result := False;
end;

function TJsonObject.AsInteger: Integer;
var
  cint, code: integer;
begin
  if JsonIsValid(Self) then
  case FJsonType of
    json_type_int: Result := o.c_int;
    json_type_double: Result := round(o.c_double);
    json_type_boolean: Result := ord(o.c_boolean);
    json_type_string:
      begin
        Val(o.c_string, cint, code);
        if code = 0 then
          Result := cint else
          Result := 0;
      end;
  else
    Result := 0;
  end else
    Result := 0;
end;

function TJsonObject.AsDouble: Double;
var
  code: integer;
  cdouble: double;
begin
  if JsonIsValid(Self) then
  case FJsonType of
    json_type_double: Result := o.c_double;
    json_type_int: Result := o.c_int;
    json_type_boolean: Result := ord(o.c_boolean);
    json_type_string:
      begin
        Val(o.c_string, cdouble, code);
        if code = 0 then
          Result := cdouble else
          Result := 0.0;
      end;
  else
    Result := 0.0;
  end else
    Result := 0.0;
end;

function TJsonObject.AsString: PChar;
begin
  if JsonIsValid(Self) then
  begin
    if FJsonType = json_type_string then
      Result := o.c_string else
      Result := AsJSon(false);
  end else
    Result := nil;
end;

function TJsonObject.AsArray: TJsonArray;
begin
  if JsonIsValid(Self) then
  begin
    if FJsonType = json_type_array then
      Result := o.c_array else
      Result := nil;
  end else
    Result := nil;
end;

function TJsonObject.AsObject: TJsonTableObject;
begin
  if JsonIsValid(Self) then
  begin
    if FJsonType = json_type_object then
      Result := o.c_object else
      Result := nil;
  end else
    Result := nil
end;

function TJsonObject.AsJSon(format: boolean): PChar;
begin
  if not JsonIsValid(Self) then
    Result := 'null' else
  begin
    if(Fpb = nil) then
      Fpb := TJsonWriterString.Create else
      Fpb.Reset;

    if(Fpb.Write(self, format, 0) < 0) then
    begin
      Result := '';
      Exit;
    end;
    Result := Fpb.FBuf;
  end;
end;

class function TJsonObject.Parse(s: PChar): TJsonObject;
  type
    PCursor = ^TCursor;
    TCursor = record
      source: PChar;
      pos: integer;
      pb: TJsonWriterString;
    end;
  function json_tokener_do_parse(this: PCursor): TJsonObject;

    function hexdigit(x: char): byte;
    begin
      if x <= '9' then
        Result := byte(x) - byte('0') else
        Result := (byte(x) and 7) + 9;
    end;

    function ReadInteger(p: PChar; var v: integer; stop: integer): boolean;
    var
      c: integer;
    begin
      Val(p, v, c);
      Result := (c = stop+1) or (c = 0);
    end;

    function ReadDouble(p: PChar; var v: Double; stop: integer): boolean;
    var
      c: integer;
    begin
      Val(p, v, c);
      Result := (c = stop+1) or (c = 0);
    end;

  var
    state, saved_state: TJsonTokenerState;
    err: TJsonTokenerError;
    current, obj: TJsonObject;
    obj_field_name: PChar;
    quote_char: char;
    deemed_double, start_offset: Integer;
    c: char;

    utf_out: array[0..2] of byte;
    ucs_char: Cardinal;

    numi: integer;
    numd: double;
  label out;
  begin
    start_offset := 0;
    quote_char := #0;
    deemed_double := 0;

    err := json_tokener_success;
    current := nil;
    obj_field_name := nil;
    state := json_tokener_state_eatws;
    saved_state := json_tokener_state_start;

    repeat
      c := this^.source[this^.pos];
      case state of
        json_tokener_state_eatws:
          begin
            if c in [' ', #8,#10,#13,#9] then
              inc(this^.pos) else
              if (c = '/') then
              begin
                state := json_tokener_state_comment_start;
                start_offset := this^.pos;
                inc(this^.pos);
              end else
                state := saved_state;
          end;
        json_tokener_state_start:
          begin
            case c of
              '{':
                begin
                  state := json_tokener_state_eatws;
                  saved_state := json_tokener_state_object;
                  current := TJsonObject.Create(json_type_object);
                  inc(this^.pos);
                 end;
              '[':
                begin
                  state := json_tokener_state_eatws;
                  saved_state := json_tokener_state_array;
                  current := TJsonObject.Create(json_type_array);
                  inc(this^.pos);
                end;
              'N', 'n':
                begin
                  state := json_tokener_state_null;
                  start_offset := this^.pos;
                  inc(this^.pos)
                end;
              '"','''':
                begin
                  quote_char := c;
                  this^.pb.Reset;
                  state := json_tokener_state_string;
                  inc(this^.pos);
                  start_offset := this^.pos;
                end;
              'T','t','F','f':
                begin
                  state := json_tokener_state_boolean;
                  start_offset := this^.pos;
                  inc(this^.pos);
                end;
              '0'..'9','-':
                begin
                  deemed_double := 0;
                  state := json_tokener_state_number;
                  start_offset := this^.pos;
                  inc(this^.pos);
                end;
            else
              err := json_tokener_error_parse_unexpected;
              goto out;
            end;
          end;
        json_tokener_state_finish: goto out;
        json_tokener_state_null:
          begin
            if(StrLIComp('null', this^.source + start_offset, this^.pos - start_offset) <> 0) then
            begin
              Result := TJsonObject(-ord(json_tokener_error_parse_null));
              Exit;
            end;
            if(this^.pos - start_offset = 4) then
            begin
              current := nil;
              saved_state := json_tokener_state_finish;
              state := json_tokener_state_eatws;
            end else
              inc(this^.pos);
          end;

        json_tokener_state_comment_start:
          begin
            if (c = '*') then state := json_tokener_state_comment else
            if (c = '/') then state := json_tokener_state_comment_eol else
            begin
              err := json_tokener_error_parse_comment;
              goto out;
            end;
            inc(this^.pos);
          end;

        json_tokener_state_comment:
          begin
            if (c = '*') then state := json_tokener_state_comment_end;
            inc(this^.pos);
          end;

        json_tokener_state_comment_eol:
          begin
            if (c = #10) then
              state := json_tokener_state_eatws;
            inc(this^.pos);
          end;

        json_tokener_state_comment_end:
          begin
            if (c = '/') then
              state := json_tokener_state_eatws else
              state := json_tokener_state_comment;
            inc(this^.pos);
          end;

        json_tokener_state_string:
          begin
            if (c = quote_char) then
            begin
              this^.pb.Append(this^.source + start_offset, this^.pos - start_offset);
              current := TJsonObject.Create(this^.pb.FBuf);
              saved_state := json_tokener_state_finish;
              state := json_tokener_state_eatws;
            end else
            if (c = '\') then
            begin
              saved_state := json_tokener_state_string;
              state := json_tokener_state_string_escape;
            end;
            inc(this^.pos);
          end;

        json_tokener_state_string_escape:
          begin
            case c of
              '"','\':
                begin
                  this^.pb.Append(this^.source + start_offset, this^.pos - start_offset - 1);
                  start_offset := this^.pos;
                  inc(this^.pos);
                  state := saved_state;
                end;
              'b','n','r','t':
                begin
                  this^.pb.Append(this^.source + start_offset, this^.pos - start_offset - 1);
                  if(c = 'b') then this^.pb.Append(#8, 1) else
                  if(c = 'n') then this^.pb.Append(#10, 1) else
                  if(c = 'r') then this^.pb.Append(#13, 1) else
                  if(c = 't') then this^.pb.Append(#9, 1);
                  inc(this^.pos);
                  start_offset := this^.pos;
                  state := saved_state;
                 end;
              'u':
                begin
                  this^.pb.Append(this^.source + start_offset, this^.pos - start_offset - 1);
                  inc(this^.pos);
                  start_offset := this^.pos;
                  state := json_tokener_state_escape_unicode;
                end;
            else
              err := json_tokener_error_parse_string;
              goto out;
            end;
          end;

        json_tokener_state_escape_unicode:
          begin
            if c in json_hex_chars_set then
            begin
              inc(this^.pos);
              if(this^.pos - start_offset = 4) then
              begin
                ucs_char :=
                (hexdigit((this^.source + start_offset)^) shl 12) +
                (hexdigit((this^.source + start_offset + 1)^) shl 8) +
                (hexdigit((this^.source + start_offset + 2)^) shl 4) +
                hexdigit((this^.source + start_offset + 3)^);
                if (ucs_char < $80) then
                begin
                  utf_out[0] := ucs_char;
                  this^.pb.Append(@utf_out, 1);
                end else
                if (ucs_char < $800) then
                begin
                  utf_out[0] := $c0 or (ucs_char shr 6);
                  utf_out[1] := $80 or (ucs_char and $3f);
                  this^.pb.Append(@utf_out, 2);
                end else
                begin
                  utf_out[0] := $e0 or (ucs_char shr 12);
                  utf_out[1] := $80 or ((ucs_char shr 6) and $3f);
                  utf_out[2] := $80 or (ucs_char and $3f);
                  this^.pb.Append(@utf_out, 3);
                end;
                start_offset := this^.pos;
                state := saved_state;
              end;
            end else
            begin
              err := json_tokener_error_parse_string;
              goto out;
            end;
          end;

        json_tokener_state_boolean:
          begin
            if(StrLIComp('true', this^.source + start_offset, this^.pos - start_offset) = 0) then
            begin
              if(this^.pos - start_offset = 4) then
              begin
                current := TJsonObject.Create(true);
                saved_state := json_tokener_state_finish;
                state := json_tokener_state_eatws;
              end else
              inc(this^.pos);
            end else if(StrLIComp('false', this^.source + start_offset, this^.pos - start_offset) = 0) then
            begin
              if(this^.pos - start_offset = 5) then
              begin
                current := TJsonObject.Create(false);
                saved_state := json_tokener_state_finish;
                state := json_tokener_state_eatws;
              end else
                inc(this^.pos);
            end else
            begin
              err := json_tokener_error_parse_boolean;
              goto out;
            end;
          end;
        json_tokener_state_number:
          begin
            if (c = #0) or (not (c in json_number_chars_set)) then
            begin
              if (deemed_double = 0) and ReadInteger(this^.source + start_offset, numi, this^.pos - start_offset) then
                current := TJsonObject.Create(numi) else
                if(deemed_double <> 0) and ReadDouble(this^.source + start_offset, numd, this^.pos - start_offset) then
                  current := TJsonObject.Create(numd) else
                  begin
                    err := json_tokener_error_parse_number;
                    goto out;
                  end;
              saved_state := json_tokener_state_finish;
              state := json_tokener_state_eatws;
            end else
            begin
              if(c = '.') or (c = 'e') then deemed_double := 1;
              inc(this^.pos);
            end;
          end;

        json_tokener_state_array:
          begin
            if (c = ']') then
            begin
              inc(this^.pos);
              saved_state := json_tokener_state_finish;
              state := json_tokener_state_eatws;
            end else
            begin
              obj := json_tokener_do_parse(this);
              if (JsonIsError(obj)) then
              begin
                err := TJsonTokenerError(-PtrInt(obj));
                goto out;
              end;
              current.o.c_array.Add(obj);
              saved_state := json_tokener_state_array_sep;
              state := json_tokener_state_eatws;
            end;
          end;

        json_tokener_state_array_sep:
          begin
            if (c = ']') then
            begin
              inc(this^.pos);
              saved_state := json_tokener_state_finish;
              state := json_tokener_state_eatws;
            end else if (c = ',') then
            begin
              inc(this^.pos);
              saved_state := json_tokener_state_array;
              state := json_tokener_state_eatws;
            end else
            begin
              if current <> nil then
                current.Release;
              Result := TJsonObject(-ord(json_tokener_error_parse_array));
              Exit;
            end;
          end;

        json_tokener_state_object:
          begin
            state := json_tokener_state_object_field_start;
            start_offset := this^.pos;
          end;

        json_tokener_state_object_field_start:
          begin
            if (c = '}') then
            begin
              inc(this^.pos);
              saved_state := json_tokener_state_finish;
              state := json_tokener_state_eatws;
            end else
            if (c = '"') or (c = '''') then
            begin
              quote_char := c;
              this^.pb.Reset;
              state := json_tokener_state_object_field;
              inc(this^.pos);
              start_offset := this^.pos;
            end else
            begin
              err := json_tokener_error_parse_object;
              goto out;
            end;
          end;

        json_tokener_state_object_field:
          begin
            if (c = quote_char) then
            begin
              this^.pb.Append(this^.source + start_offset, this^.pos - start_offset);
              obj_field_name := strdup(this^.pb.FBuf);
              saved_state := json_tokener_state_object_field_end;
              state := json_tokener_state_eatws;
            end else
            if (c = '\') then
            begin
              saved_state := json_tokener_state_object_field;
              state := json_tokener_state_string_escape;
            end;
            inc(this^.pos);
          end;

        json_tokener_state_object_field_end:
          begin
            if (c = ':') then
            begin
              inc(this^.pos);
              saved_state := json_tokener_state_object_value;
              state := json_tokener_state_eatws;
            end else
            begin
              Result := TJsonObject(-ord(json_tokener_error_parse_object));
              Exit;
            end;
          end;

        json_tokener_state_object_value:
          begin
            obj := json_tokener_do_parse(this);
            if (JsonIsError(obj)) then
            begin
              err := TJsonTokenerError(-PtrInt(obj));
              goto out;
            end;
            current.o.c_object.InternalAdd(obj_field_name, obj);
            FreeMem(obj_field_name);
            obj_field_name := nil;
            saved_state := json_tokener_state_object_sep;
            state := json_tokener_state_eatws;
          end;

        json_tokener_state_object_sep:
          begin
            if (c = '}') then
            begin
              inc(this^.pos);
              saved_state := json_tokener_state_finish;
              state := json_tokener_state_eatws;
            end else
            if (c = ',') then
            begin
              inc(this^.pos);
              saved_state := json_tokener_state_object;
              state := json_tokener_state_eatws;
            end else
            begin
              err := json_tokener_error_parse_object;
              goto out;
            end;
          end;
      end;
    until c = #0;

    if(state <> json_tokener_state_finish) and
       (saved_state <> json_tokener_state_finish) then
      err := json_tokener_error_parse_eof;

  out:
    FreeMem(obj_field_name);
    if(err = json_tokener_success) then
    begin
      Result := current;
      Exit;
    end;
    if current <> nil then
      current.Release;
    Result := TJsonObject(-ord(err));
  end;
var
  obj: TJsonObject;
  cur: TCursor;
begin
  cur.source := s;
  cur.pos := 0;
  cur.pb := TJsonWriterString.Create;
  obj := json_tokener_do_parse(@cur);
  cur.pb.Free;
  Result := obj;
end;

function TJsonObject.SaveTo(stream: TStream; format: boolean): integer;
var
  pb: TJsonWriterStream;
begin
  pb := TJsonWriterStream.Create(stream);
  if(pb.Write(self, format, 0) < 0) then
  begin
    pb.Reset;
    pb.Free;
    Result := 0;
    Exit;
  end;
  Result := stream.Size;
  pb.Free;
end;

function TJsonObject.CalcSize(format: boolean): integer;
var
  pb: TJsonWriterFake;
begin
  pb := TJsonWriterFake.Create;
  if(pb.Write(self, format, 0) < 0) then
  begin
    pb.Free;
    Result := 0;
    Exit;
  end;
  Result := pb.FSize;
  pb.Free;
end;

function TJsonObject.SaveTo(socket: Integer; format: boolean): integer;
var
  pb: TJsonWriterSock;
begin
  pb := TJsonWriterSock.Create(socket);
  if(pb.Write(self, format, 0) < 0) then
  begin
    pb.Free;
    Result := 0;
    Exit;
  end;
  Result := pb.FSize;
  pb.Free;
end;

procedure TJsonObject.Free;
begin
  Release;
end;

constructor TJsonObject.Create(const s: string);
begin
  Create(json_type_string);
  o.c_string := strdup(PChar(s));
end;

function TJsonObject.Clone: TJsonObject;
var
  ite: TJsonObjectIter;
  arr: TJsonArray;
  i: integer;
begin
  if not JsonIsValid(Self) then
    result := nil else
  case FJsonType of
    json_type_boolean: Result := TJsonObject.Create(o.c_boolean);
    json_type_double: Result := TJsonObject.Create(o.c_double);
    json_type_int: Result := TJsonObject.Create(o.c_int);
    json_type_string: Result := TJsonObject.Create(o.c_string);
    json_type_object:
      begin
        Result := TJsonObject.Create(json_type_object);
        if JsonFindFirst(self, ite) then
        with Result.AsObject do
        repeat
          Put(ite.key, ite.val.Clone);
        until not JsonFindNext(ite);
      end;
    json_type_array:
      begin
        Result := TJsonObject.Create(json_type_array);
        arr := AsArray;
        with Result.AsArray do
        for i := 0 to arr.Length - 1 do
          Add(arr.Get(i).Clone);
      end;
  else
    Result := nil;
  end;
end;

function TJsonObject.GetJsonType: TJsonType;
begin
  if JsonIsValid(Self) then
    Result := FJsonType else
    Result := json_type_null; 
end;

function TJsonObject.SaveTo(const FileName: string; format: boolean): integer;
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(FileName, fmCreate);
  try
    Result := SaveTo(stream, format);
  finally
    stream.Free;
  end;
end;

{ TJsonArray }

function TJsonArray.Add(Data: TJsonObject): Integer;
begin
  Result := FLength;
  Put(Result, data);
end;

constructor TJsonArray.Create;
begin
  inherited Create;
  FSize := ARRAY_LIST_DEFAULT_SIZE;
  FLength := 0;
  GetMem(FArray, sizeof(Pointer) * FSize);
  FillChar(FArray^, sizeof(Pointer) * FSize, 0);
end;

destructor TJsonArray.Destroy;
var
  i: Integer;
begin
  for i := 0 to FLength - 1 do
    if FArray^[i] <> nil then
       FArray^[i].Release;
  FreeMem(FArray);
  inherited;
end;

function TJsonArray.Expand(max: Integer): Integer;
var
  new_size: Integer;
begin
  if (max < FSize) then
  begin
    Result := 0;
    Exit;
  end;
  if max < FSize shl 1 then
    new_size := FSize shl 1 else
    new_size := max;
  ReallocMem(FArray, new_size * sizeof(Pointer));
  FillChar(Pointer(PtrInt(FArray) + (FSize *sizeof(Pointer)))^, (new_size - FSize)*sizeof(Pointer), 0);
  FSize := new_size;
  Result := 0;
end;

function TJsonArray.Get(i: Integer): TJsonObject;
begin
  if(i >= FLength) then
    Result := nil else
    Result := FArray^[i];
end;

procedure TJsonArray.Put(i: Integer; Data: TJsonObject);
begin
  if(Expand(i) <> 0) then
    Exit;
  if(FArray^[i] <> nil) then
    TJsonObject(FArray^[i]).Release;
  FArray^[i] := data;
  if(FLength <= i) then FLength := i + 1;
end;

{ TJsonTable }

constructor TJsonTable.Create(AName: PChar; size: Integer);
var
  i: Integer;
begin
  FCount := 0;
  FSize := size;
  FName := name;
  GetMem(FTable, size * sizeof(TLHEntry));
  FillChar(FTable^, size * sizeof(TLHEntry), 0);
  for i := 0 to size - 1 do
    FTable^[i].k := LH_EMPTY;
  FCollisions := 0;
  FResizes := 0;
  FLookups := 0;
  FInserts := 0;
  FDeletes := 0;
  FHead := nil;
  FTail := nil;
end;

class procedure TJsonTable.doDeleteEntry(e: PLHEntry);
begin
  FreeMem(e^.k);
end;

destructor TJsonTable.Destroy;
var
  c: PLHEntry;
begin
  c := FHead;
  while c <> nil do
  begin
      doDeleteEntry(c);
    c := c^.next;
  end;
  if FTable <> nil then
    FreeMem(FTable);
  inherited;
end;

function TJsonTable.InternalInsert(k, v: Pointer): Integer;
var
  h, n, f: Cardinal;
begin
// INSERT
  inc(FInserts);
  if(FCount > FSize * 0.66) then
    InternalResize(FSize * 2);


  // lookup and delete
  h := doHash(k);
  n := h mod FSize;
  f := 0; // free entry

  inc(FLookups);
  while true do
  begin
    if FTable^[n].k = LH_EMPTY then
      begin
        f := n;
        Break
      end else
    if (FTable^[n].k = LH_FREED) then f := n else
    if (FTable^[n].h = h) and (doEqual(FTable^[n].k, k) <> 0) then
    begin
      f := n;
      // delete entry
      InternalDeleteEntry(@FTable^[n]);
      Break;
    end;
    inc(n);
    if(n = FSize) then n := 0;
  end;

  if f > 0 then
    n := f else
    n := h mod FSize;

  while true do
  begin
    if (FTable^[n].k = LH_EMPTY) or (FTable^[n].k = LH_FREED) then Break;
    inc(FCollisions);
    inc(n);
    if(n = FSize) then n := 0;
  end;
  FTable^[n].k := k;
  FTable^[n].v := v;
  FTable^[n].h := h;

  inc(FCount);

  if(FHead = nil) then
  begin
    FTail := @FTable^[n];
    FHead := FTail;
    FTable^[n].prev := nil;
    FTable^[n].next := nil;
  end else
  begin
    FTail^.next := @FTable^[n];
    FTable^[n].prev := FTail;
    FTable^[n].next := nil;
    FTail := @FTable^[n];
  end;
  Result := 0;
end;

function TJsonTable.InternalDelete(k: Pointer): Integer;
var
  e: PLHEntry;
begin
  e := InternalLookupEntry(k);
  if(e = nil) then
    Result := -1 else
    Result := InternalDeleteEntry(e);
end;

function TJsonTable.InternalDeleteEntry(e: PLHEntry): Integer;
var
  n: PtrInt;
begin
  n := (PtrInt(e) - PtrInt(FTable)) div sizeof(TLHEntry);
  if (n < 0) then
  begin
    Result := -2;
    Exit;
  end;

  if(FTable^[n].k = LH_EMPTY) or (FTable^[n].k = LH_FREED) then
  begin
    Result := -1;
    Exit;
  end;
  dec(FCount);
  doDeleteEntry(e);
  FTable^[n].v := nil;
  FTable^[n].k := LH_FREED;
  FTable^[n].h := 0;

  if(FTail = @FTable^[n]) and (FHead = @FTable^[n]) then
  begin
    FHead := nil;
    FTail := nil;
  end else if (FHead = @FTable^[n]) then
  begin
    FHead^.next^.prev := nil;
    FHead := FHead^.next;
  end else if (FTail = @FTable^[n]) then
  begin
    FTail^.prev^.next := nil;
    FTail := FTail^.prev;
  end else
  begin
    FTable^[n].prev^.next := FTable^[n].next;
    FTable^[n].next^.prev := FTable^[n].prev;
  end;
  FTable^[n].next := nil;
  FTable^[n].prev := nil;
  Result := 0;
end;


function TJsonTable.InternalLookup(k: Pointer): Pointer;
var
  e: PLHEntry;
begin
  e := InternalLookupEntry(k);
  if (e <> nil) then
    Result := e^.v else
    Result := nil;
end;

procedure TJsonTable.InternalResize(new_size: Integer);
var
  new_t: TJsonTable;
  ent: PLHEntry;
begin
  new_t := TJsonTableClass(ClassType).Create(FName, new_size);
  ent := FHead;
  while(ent <> nil) do
  begin
    new_t.Internalinsert(ent^.k, ent^.v);
    ent := ent^.next;
  end;
  FreeMem(FTable);
  FTable := new_t.FTable;
  new_t.FTable := nil;
  FSize := new_size;
  FHead := new_t.FHead;
  FTail := new_t.FTail;
  new_t.FHead := nil;
  new_t.FTail := nil;
  inc(FResizes);
  new_t.Free;
end;

function TJsonTable.InternalLookupEntry(k: Pointer): PLHEntry;
var
  h, n: Cardinal;
begin
  h := doHash(k);
  n := h mod FSize;

  inc(FLookups);
  while true do
  begin
    if FTable^[n].k = LH_EMPTY then
      begin
        Result := nil;
        Exit;
      end else
    if (FTable^[n].k = LH_FREED) then {nop} else
    if (FTable^[n].h = h) and (doEqual(FTable^[n].k, k) <> 0) then
    begin
      Result := @FTable^[n];
      Exit;
    end;
    inc(n);
    if(n = FSize) then n := 0;
  end;
  Result := nil;
end;

function TJsonTable.InternalAdd(k, v: Pointer): Integer;
begin
  Result := InternalInsert(k, v);
end;

{ TJsonTableString }

function TJsonTableString.Delete(k: PChar): Integer;
begin
  Result := InternalDelete(k);
end;

class function TJsonTableString.doEqual(k1, k2: Pointer): integer;
begin
{$IFDEF JSON_CASE_INSENSITIVE}
  Result := Integer(StrIComp(k1, k2) = 0);
{$ELSE}
  Result := Integer(StrComp(k1, k2) = 0);
{$ENDIF}
end;

class function TJsonTableString.doHash(k: Pointer): Cardinal;
var
  h: Cardinal;
  data: PChar;
begin
  h := 0;
  data := k;
  if data <> nil then
    while(data^ <> #0 ) do
    begin
  {$IFDEF JSON_CASE_INSENSITIVE}
      h := h*129 + byte(UpCase(data^)) + LH_PRIME;
  {$ELSE}
      h := h*129 + byte(data^) + LH_PRIME;
  {$ENDIF}
      inc(data);
    end;
  Result := h;
end;

function TJsonTableString.LookupEntry(k: PChar): PLHEntry;
begin
  Result := InternalLookupEntry(k)
end;

function TJsonTableString.InternalAdd(k, v: Pointer): Integer;
begin
  result := Internalinsert(strdup(k), v);
end;

{ TJsonTablePointer }

class function TJsonTablePointer.doEqual(k1, k2: Pointer): integer;
begin
  Result := Integer(k1 = k2);
end;

class function TJsonTablePointer.doHash(k: Pointer): Cardinal;
begin
  Result := Cardinal(((PtrUInt(k) * LH_PRIME) shr 4) and High(Cardinal));
end;

{ TJsonTableObject }

class procedure TJsonTableObject.doDeleteEntry(e: PLHEntry);
begin
  inherited;
  if e^.v <> nil then
    TJsonObject(e^.v).Release;
end;

procedure TJsonTableObject.Put(k: PChar; v: TJsonObject);
begin
  InternalAdd(k, v);
end;

function TJsonTableObject.Get(k: PChar): TJsonObject;
begin
   Result := TJsonObject(InternalLookup(k));
end;

{ TJsonWriterString }

function TJsonWriterString.Append(buf: PChar; Size: Integer): Integer;
var
  new_size: Integer;
  function max(a, b: Integer): integer; begin if a > b then  Result := a else Result := b end;
begin
  Result := size;
  if Size = 0 then exit;
  if(FSize - FBPos <= size) then
  begin
    new_size := max(FSize * 2, FBPos + size + 8);
    ReallocMem(FBuf, new_size);
    FSize := new_size;
  end;
  move(PChar(buf)^, Pointer(PtrInt(FBuf) + FBPos)^, size);
  inc(FBPos, size);
  FBuf[FBPos] := #0;
end;

function TJsonWriterString.Append(buf: PChar): Integer;
begin
  Result := Append(buf, strlen(buf));
end;

constructor TJsonWriterString.Create;
begin
  inherited;
  FSize := 32;
  FBPos := 0;
  GetMem(FBuf, FSize);
end;

destructor TJsonWriterString.Destroy;
begin
  if FBuf <> nil then
    FreeMem(FBuf, FSize);
  inherited;
end;

procedure TJsonWriterString.Reset;
begin
  FBuf[0] := #0;
  FBPos := 0;
end;

{ TJsonWriterStream }

function TJsonWriterStream.Append(buf: PChar; Size: Integer): Integer;
begin
  Result := FStream.Write(buf^, Size);
end;

function TJsonWriterStream.Append(buf: PChar): Integer;
begin
  Result := FStream.Write(buf^, StrLen(buf));
end;

constructor TJsonWriterStream.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
end;

procedure TJsonWriterStream.Reset;
begin
  FStream.Size := 0;
end;

{ TJsonWriterFake }

function TJsonWriterFake.Append(buf: PChar; Size: Integer): Integer;
begin
  inc(FSize, Size);
  Result := FSize;
end;

function TJsonWriterFake.Append(buf: PChar): Integer;
begin
  inc(FSize, Strlen(buf));
  Result := FSize;
end;

constructor TJsonWriterFake.Create;
begin
  inherited Create;
  FSize := 0;
end;

procedure TJsonWriterFake.Reset;
begin
  FSize := 0;
end;

{ TJsonWriterSock }

function TJsonWriterSock.Append(buf: PChar; Size: Integer): Integer;
begin
  Result := send(FSocket, buf^, size, 0);
  inc(FSize, Result);
end;

function TJsonWriterSock.Append(buf: PChar): Integer;
begin
  Result := send(FSocket, buf^, strlen(buf), 0);
  inc(FSize, Result);
end;

constructor TJsonWriterSock.Create(ASocket: Integer);
begin
  inherited Create;
  FSocket := ASocket;
  FSize := 0;
end;

procedure TJsonWriterSock.Reset;
begin
  FSize := 0;
end;

{ TJsonRpcService }

type
  PRpcMethod = ^TRpcMethod;
  TRpcMethod = record
    name: PChar;
    method: TJsonRpcMethod;
  end;

constructor TJsonRpcService.Create(Aname: PChar);
begin
  inherited Create(name, JSON_OBJECT_DEF_HASH_ENTIRES);
  FName := strdup(Aname);
end;

destructor TJsonRpcService.Destroy;
begin
  FreeMem(FName);
  inherited;
end;

class procedure TJsonRpcService.doDeleteEntry(e: PLHEntry);
var
  p: PRpcMethod;
begin
  inherited;
  p := PRpcMethod(e^.v);
  if p <> nil then
  begin
    if p^.name <> nil then
      FreeMem(p^.name);
    FreeMem(p);
  end;
end;

procedure TJsonRpcService.Invoke(Obj: TJsonTableObject; out Result: TJsonObject; out error: string);
var
  Method: TJsonObject;
  Params: TJsonObject;
  p: PRpcMethod;
begin
  Result := nil;
  // find Method
  Method := TJsonObject(obj.InternalLookup(PChar('method')));
  if Method = nil then
  begin
    Error := 'Procedure not found.';
    Exit;
  end;
  p := InternalLookup(Method.AsString);
  if p = nil then
  begin
    error := format('Method %s not found.', [Method.AsString]);
    Exit;
  end;

  // find params
  Params := TJsonObject(obj.InternalLookup(PChar('params')));
  if not (JsonIsValid(Params) and (Params.JsonType = json_type_array)) then
  begin
    Error := 'Params must be an array.';
    Exit;
  end;

  // Call method
  try
    p^.method(Params, Result);
  except
    on E: Exception do
    begin
      Error := '[' + E.ClassName + ']: ' + E.Message;
    end;
  end;
end;

procedure TJsonRpcService.RegisterMethod(Aname: PChar;
  Sender: TObject; Method: Pointer);
var
  p: PRpcMethod;
begin
  GetMem(p, sizeof(TRpcMethod));
  p^.name := strdup(AName);
  TMethod(p^.method).Code := Method;
  TMethod(p^.method).Data := sender;
  InternalAdd(p^.name, p);
end;

{ TJsonRpcServiceList }

class procedure TJsonRpcServiceList.doDeleteEntry(e: PLHEntry);
begin
  inherited;
  if (e^.v) <> nil then
    TJsonRpcService(e^.v).Free;
end;

function TJsonRpcServiceList.Invoke(service: TJsonRpcService; Obj: TJsonObject; var error: string): TJsonObject;
var
  Table: TJsonTableObject;
begin
  Result := nil;
  if (Obj.JsonType <> json_type_object) then
  begin
    error := 'Request is invalid.';
    Exit;
  end;
  // find Table;
  Table := Obj.AsObject;
  if Table = nil then
  begin
    error := 'Request is empty.';
    Exit;
  end;

  service.Invoke(Table, Result, error);
end;

function TJsonRpcServiceList.Invoke(service: PChar; s: PChar): TJsonObject;
var
  js, ret, id: TJsonObject;
  p: TJsonRpcService;
  error: string;
begin

  p := TJsonRpcService(InternalLookup(service));
  if p = nil then
  begin
    Result := TJsonObject.Create(json_type_object);
    with Result.AsObject do
    begin
      Put('result', nil);
      Put('error', TJsonObject.Create('Service not found.'));
      Put('id', nil);
    end;
    Exit;
  end;

  js := TJsonObject.Parse(s);
  if JsonIsValid(js) then
  begin
    ret := Invoke(p, js, error);
    Result := TJsonObject.Create(json_type_object);
    with Result.AsObject do
    begin
      Put('result', ret);
      if error = '' then
        Put('error', nil) else
        Put('error', TJsonObject.Create(PChar(Error)));
      id := js.AsObject.Get('id');
      if JsonIsValid(id) then id.AddRef;
      Put('id', id);
    end;
    js.Release;
  end else
  begin
    Result := TJsonObject.Create(json_type_object);
    with Result.AsObject do
    begin
      Put('result', nil);
      Put('error', TJsonObject.Create('Json parsing error.'));
      Put('id', nil);
    end;
  end;
end;

procedure TJsonRpcServiceList.RegisterService(obj: TJsonRpcService);
begin
  InternalAdd(obj.FName, obj);
end;

{ TJsonWriter }

function TJsonWriter.Write(obj: TJsonObject; format: boolean; level: integer): Integer;
  function Escape(str: PChar): Integer;
  var
    pos, start_offset: Integer;
    c: char;
    buf: array[0..5] of char;
  begin
    pos := 0; start_offset := 0;
    repeat
      c := str[pos];
      case c of
        #0: break;
        #8,#10,#13,#9,'"':
          begin
            if(pos - start_offset > 0) then
              Append(str + start_offset, pos - start_offset);
            if(c = #8) then Append('\b', 2)
            else if (c = #10) then Append('\n', 2)
            else if (c = #13) then Append('\r', 2)
            else if (c = #9) then Append('\t', 2)
            else if (c = '"') then Append('\"', 2);
            inc(pos);
            start_offset := pos;
          end;
      else
        if (c < ' ') then
        begin
    if(pos - start_offset > 0) then
      Append(str + start_offset, pos - start_offset);
      buf := '\u00';
      buf[4] := json_hex_chars[ord(c) shr 4];
      buf[5] := json_hex_chars[ord(c) and $f];
      Append(buf, 6);
      inc(pos);
      start_offset := pos;
    end else inc(pos);
      end;
    until c = #0;
    if(pos - start_offset > 0) then
      Append(str + start_offset, pos - start_offset);
    Result := 0;
  end;

  procedure Indent(i: shortint; r: boolean);
  begin
    inc(level, i);
    if r then
    begin
      Append(#10, 1);
      for i := 0 to level - 1 do
        Append(' ', 1);
    end;
  end;
var
  i: Integer;
  iter: TJsonObjectIter;
  s: string;
  val: TJsonObject;
begin
  if not JsonIsValid(obj) then
    Result := Append('null', 4) else
  case obj.FJsonType of
    json_type_object:
      begin
        i := 0;
        Append('{', 1);
        if format then indent(1, false);
        if JsonFindFirst(obj, iter) then
        repeat
          if(i <> 0) then
            Append(',', 1);
          if format then Indent(0, true);
          Append('"', 1);
          Escape(iter.key);
          Append('":', 2);
          if(iter.val = nil) then
            Append('null', 4) else
            write(iter.val, format, level);
          inc(i);
        until not JsonFindNext(iter);
        if format then Indent(-1, true);
        Result := Append('}', 1);
      end;
    json_type_boolean:
      begin
        if (obj.o.c_boolean) then
          Result := Append('true', 4) else
          Result := Append('false', 5);
      end;
    json_type_int:
      begin
        str(obj.o.c_int, s);
        Result := Append(PChar(s));
      end;
    json_type_double:
      begin
        s := FloatToStr(obj.o.c_double);
        Result := Append(PChar(s));
      end;
    json_type_string:
      begin
        Append('"', 1);
        Escape(obj.o.c_string);
        Append('"', 1);
        Result := 0;
      end;
    json_type_array:
      begin
        Append('[', 1);
        if format then Indent(1, true);
        i := 0;
        while i < obj.o.c_array.FLength do
        begin
          if (i <> 0) then
            Append(',', 1);
          val :=  obj.o.c_array.Get(i);
          if(val = nil) then
            Append('null', 4) else
            write(val, format, level);
          inc(i);
        end;
        if format then Indent(-1, false);
        Result := Append(']', 1);
      end;
  else
    Result := 0;
  end;
end;

end.
