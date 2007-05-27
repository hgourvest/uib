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
 * This unit is inspired from the json c lib:
 *   Michael Clark <michael@metaparadigm.com>
 *   http://oss.metaparadigm.com/json-c/
 *
 *  CHANGES:
 *  v0.2
 *   + Hashed string list replaced with a faster AVL tree
 *   + JsonInt data type can be changed to int64 
 *   + JavaToDelphiDateTime and DelphiToJavaDateTime helper fonctions
 *   + from json-c v0.7
 *     + Add escaping of backslash to json output
 *     + Add escaping of foward slash on tokenizing and output
 *     + Changes to internal tokenizer from using recursion to
 *       using a depth state structure to allow incremental parsing
 *  v0.1
 *   + first release
 *)


unit json;
{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
interface
uses Classes;

// this option affect case sensitivity.
{.$DEFINE JSON_CASE_INSENSITIVE}
{$DEFINE JSON_LARGE_INT}

type
{$IFNDEF FPC}
  PtrInt = longint;
  PtrUInt = Longword;
{$ENDIF}
{$IFDEF JSON_LARGE_INT}
  JsonInt = Int64;
{$ELSE}
  JsonInt = Integer;
{$ENDIF}

const
  JSON_ARRAY_LIST_DEFAULT_SIZE = 32;
  JSON_TOKENER_MAX_DEPTH = 32;

  JSON_AVL_MAX_DEPTH = sizeof(longint) * 8;
  JSON_AVL_MASK_HIGH_BIT = not ((not longword(0)) shr 1);

type
  // forward declarations
  TJsonObject = class;

(* AVL Tree
 *  This is a "special" autobalanced AVL tree
 *  It use a hash value for fast compare
 *)

  TJsonAvlSize = longword;

  TJsonAvlBitArray = set of 0..JSON_AVL_MAX_DEPTH - 1;

  TJsonAvlSearchType = (stEQual, stLess, stGreater);
  TJsonAvlSearchTypes = set of TJsonAvlSearchType;

  TJsonAvlEntry = class
  private
    FGt, FLt: TJsonAvlEntry;
    FBf: integer;
    FHash: Cardinal;
    FName: PChar;
    FObj: Pointer;
  public
    class function Hash(k: PChar): Cardinal; virtual;
    constructor Create(AName: PChar; Obj: Pointer); virtual;
    destructor Destroy; override;
    property Name: PChar read FName;
    property Obj: Pointer read FObj;
  end;

  TJsonAvlTree = class
  private
    FRoot: TJsonAvlEntry;
    FCount: Integer;
    function balance(bal: TJsonAvlEntry): TJsonAvlEntry;
  protected
    procedure doDeleteEntry(Entry: TJsonAvlEntry); virtual;
    function CompareNodeNode(node1, node2: TJsonAvlEntry): integer; virtual;
    function CompareKeyNode(k: PChar; h: TJsonAvlEntry): integer; virtual;
    function Insert(h: TJsonAvlEntry): TJsonAvlEntry; virtual;
    function Search(k: PChar; st: TJsonAvlSearchTypes = [stEqual]): TJsonAvlEntry; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function IsEmpty: boolean;
    procedure Clear; virtual;
    procedure Delete(k: PChar);
    property count: Integer read FCount;
  end;

  TJsonTableString = class(TJsonAvlTree)
  protected
    procedure doDeleteEntry(Entry: TJsonAvlEntry); override;
  public
    function Put(k: PChar; Obj: TJsonObject): TJsonObject;
    function Get(k: PChar): TJsonObject;
  end;


  TJsonAvlIterator = class
  private
    FTree: TJsonAvlTree;
    FBranch: TJsonAvlBitArray;
    FDepth: longint;
    FPath: array[0..JSON_AVL_MAX_DEPTH - 2] of TJsonAvlEntry;
  public
    constructor Create(tree: TJsonAvlTree); virtual;
    procedure Search(k: PChar; st: TJsonAvlSearchTypes = [stEQual]);
    procedure First;
    procedure Last;
    function GetIter: TJsonAvlEntry;
    procedure Next;
    procedure Prior;
  end;

  TJsonObjectArray = array[0..(high(PtrInt) div sizeof(TJsonObject))-1] of TJsonObject;
  PJsonObjectArray = ^TJsonObjectArray;


  TJsonRpcMethod = procedure(Params: TJsonObject; out Result: TJsonObject) of object;
  PJsonRpcMethod = ^TJsonRpcMethod;

  TJsonRpcService = class(TJsonAvlTree)
  protected
    procedure doDeleteEntry(Entry: TJsonAvlEntry); override;
  public
    procedure RegisterMethod(Aname: PChar; Sender: TObject; Method: Pointer);
    procedure Invoke(Obj: TJsonTableString; out Result: TJsonObject; out error: string);
  end;

  TJsonRpcClass = class of TJsonRpcService;

  TJsonRpcServiceList = class(TJsonAvlTree)
  protected
    procedure doDeleteEntry(Entry: TJsonAvlEntry); override;
  public
    procedure RegisterService(AName: PChar; obj: TJsonRpcService);
    function Invoke(service: TJsonRpcService; Obj: TJsonObject; var error: string): TJsonObject; overload;
    function Invoke(service: PChar; s: PChar): TJsonObject; overload;
  end;

  TJsonArray = class
  private
    FArray: PJsonObjectArray;
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


  TJsonTokenerError = (
    json_tokener_success,
    json_tokener_continue,
    json_tokener_error_depth,
    json_tokener_error_parse_eof,
    json_tokener_error_parse_unexpected,
    json_tokener_error_parse_null,
    json_tokener_error_parse_boolean,
    json_tokener_error_parse_number,
    json_tokener_error_parse_array,
    json_tokener_error_parse_object_key_name,
    json_tokener_error_parse_object_key_sep,
    json_tokener_error_parse_object_value_sep,
    json_tokener_error_parse_string,
    json_tokener_error_parse_comment
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
    json_tokener_state_array_add,
    json_tokener_state_array_sep,
    json_tokener_state_object_field_start,
    json_tokener_state_object_field,
    json_tokener_state_object_field_end,
    json_tokener_state_object_value,
    json_tokener_state_object_value_add,
    json_tokener_state_object_sep
  );

  PJsonTokenerSrec = ^TJsonTokenerSrec;
  TJsonTokenerSrec = record
    state, saved_state: TJsonTokenerState;
    obj: TJsonObject;
    current: TJsonObject;
    obj_field_name: PChar;
  end;

  TJsonTokener = class
  public
    str: PChar;
    pb: TJsonWriterString;
    depth, is_double, st_pos, char_offset: Integer;
    err:  TJsonTokenerError;
    ucs_char: Cardinal;
    quote_char: char;
    stack: array[0..JSON_TOKENER_MAX_DEPTH-1] of TJsonTokenerSrec;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ResetLevel(adepth: integer);
    procedure Reset;
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
        json_type_int: (c_int: JsonInt);
        json_type_object: (c_object: TJsonTableString);
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
    class function ParseEx(tok: TJsonTokener; str: PChar; len: integer): TJsonObject;

    // constructors / destructor
    constructor Create(jt: TJsonType = json_type_object); overload; virtual;
    constructor Create(b: boolean); overload; virtual;
    constructor Create(i: JsonInt); overload; virtual;
    constructor Create(d: double); overload; virtual;
    constructor Create(p: PChar); overload; virtual;
    constructor Create(const s: string); overload; virtual;
    destructor Destroy; override;
    procedure Free; reintroduce; // objects are refcounted

    function AsBoolean: Boolean;
    function AsInteger: JsonInt;
    function AsDouble: Double;
    function AsString: PChar;
    function AsArray: TJsonArray;
    function AsObject: TJsonTableString;

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
    Ite: TJsonAvlIterator;
  end;

function JsonIsError(obj: TJsonObject): boolean;
function JsonIsValid(obj: TJsonObject): boolean;

function JsonFindFirst(obj: TJsonObject; var F: TJsonObjectIter): boolean;
function JsonFindNext(var F: TJsonObjectIter): boolean;
procedure JsonFindClose(var F: TJsonObjectIter);

function JavaToDelphiDateTime(const dt: int64; gmt: integer): TDateTime;
function DelphiToJavaDateTime(const dt: TDateTime; gmt: integer): int64;

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

function JavaToDelphiDateTime(const dt: int64; gmt: integer): TDateTime;
begin
  Result := 25569 + ((dt + (gmt*3600000)) / 86400000);
end;

function DelphiToJavaDateTime(const dt: TDateTime; gmt: integer): int64;
begin
  Result := Round((dt - 25569) * 86400000) - (gmt*3600000);
end;

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
var
  i: TJsonAvlEntry;
begin
  F.Ite := TJsonAvlIterator.Create(obj.o.c_object);
  F.Ite.First;
  i := F.Ite.GetIter;
  if i <> nil then
  begin
    f.key := i.FName;
    f.val := TJsonObject(i.FObj);
    Result := true;
  end else
    Result := False;
end;

function JsonFindNext(var F: TJsonObjectIter): boolean;
var
  i: TJsonAvlEntry;
begin
  F.Ite.Next;
  i := F.Ite.GetIter;
  if i <> nil then
  begin
    f.key := i.FName;
    f.val := TJsonObject(i.FObj);
    Result := true;
  end else
    Result := False;
end;

procedure JsonFindClose(var F: TJsonObjectIter);
begin
  F.Ite.Free;
end;

{ TJsonObject }

constructor TJsonObject.Create(jt: TJsonType);
begin
  inherited Create;
  FRefCount := 1;
  FDataPtr := nil;
  FJsonType := jt;
  case FJsonType of
    json_type_object: o.c_object := TJsonTableString.Create;
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

constructor TJsonObject.Create(i: JsonInt);
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

function TJsonObject.AsInteger: JsonInt;
var
  code: integer;
  cint: JsonInt;
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

function TJsonObject.AsObject: TJsonTableString;
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
var
  tok: TJsonTokener;
  obj: TJsonObject;
begin
  tok := TJsonTokener.Create;
  obj := ParseEx(tok, s, -1);
  if(tok.err <> json_tokener_success) then
    obj := TJsonObject(-ord(tok.err));
  tok.Free;
  Result := obj;
end;

class function TJsonObject.ParseEx(tok: TJsonTokener; str: PChar; len: integer): TJsonObject;

  function hexdigit(x: char): byte;
  begin
    if x <= '9' then
      Result := byte(x) - byte('0') else
      Result := (byte(x) and 7) + 9;
  end;
  function min(v1, v2: integer): integer; begin if v1 < v2 then result := v1 else result := v2 end;

var
  obj: TJsonObject;
  c: char;
  utf_out: array[0..2] of byte;

  numi: JsonInt;
  numd: double;
  code: integer;
  TokRec: PJsonTokenerSrec;
label out, redo_char;
begin
  obj := nil;
  TokRec := @tok.stack[tok.depth];

  tok.char_offset := 0;
  tok.err := json_tokener_success;

  repeat
    if (tok.char_offset = len) then
    begin
      if (tok.depth = 0) and (TokRec^.state = json_tokener_state_eatws) and
         (TokRec^.saved_state = json_tokener_state_finish) then
        tok.err := json_tokener_success else
        tok.err := json_tokener_continue;
      goto out;
    end;

    c := str^;
redo_char:
    case TokRec^.state of
    json_tokener_state_eatws:
      begin
        if c in [' ', #8,#10,#13,#9] then {nop} else
        if (c = '/') then
        begin
          tok.pb.Reset;
          tok.pb.Append(@c, 1);
          TokRec^.state := json_tokener_state_comment_start;
        end else begin
          TokRec^.state := TokRec^.saved_state;
          goto redo_char;
        end
      end;

    json_tokener_state_start:
      case c of
      '"',
      '''':
        begin
          TokRec^.state := json_tokener_state_string;
          tok.pb.Reset;
          tok.quote_char := c;
        end;
      '0'..'9',
      '-':
        begin
          TokRec^.state := json_tokener_state_number;
          tok.pb.Reset;
          tok.is_double := 0;
          goto redo_char;
        end;
      '{':
        begin
          TokRec^.state := json_tokener_state_eatws;
          TokRec^.saved_state := json_tokener_state_object_field_start;
          TokRec^.current := TJsonObject.Create(json_type_object);
        end;
      '[':
        begin
          TokRec^.state := json_tokener_state_eatws;
          TokRec^.saved_state := json_tokener_state_array;
          TokRec^.current := TJsonObject.Create(json_type_array);
        end;
      'N',
      'n':
        begin
          TokRec^.state := json_tokener_state_null;
          tok.pb.Reset;
          tok.st_pos := 0;
          goto redo_char;
        end;
      'T',
      't',
      'F',
      'f':
        begin
          TokRec^.state := json_tokener_state_boolean;
          tok.pb.Reset;
          tok.st_pos := 0;
          goto redo_char;
        end;
      else
        tok.err := json_tokener_error_parse_unexpected;
        goto out;
      end;

    json_tokener_state_finish:
      begin
        if(tok.depth = 0) then goto out;
        TokRec^.current.AddRef;
        obj := TokRec^.current;
        tok.ResetLevel(tok.depth);
        dec(tok.depth);
        TokRec := @tok.stack[tok.depth]; 
        goto redo_char;
      end;
    json_tokener_state_null:
      begin
        tok.pb.Append(@c, 1);
{$IFDEF JSON_CASE_INSENSITIVE}
        if (StrLIComp('null', tok.pb.FBuf, min(tok.st_pos + 1, 4)) = 0) then
{$ELSE}
        if (StrLComp('null', tok.pb.FBuf, min(tok.st_pos + 1, 4)) = 0) then
{$ENDIF}
        begin
          if (tok.st_pos = 4) then
          begin
            TokRec^.current := nil;
            TokRec^.saved_state := json_tokener_state_finish;
            TokRec^.state := json_tokener_state_eatws;
            goto redo_char;
          end;
        end else
        begin
          tok.err := json_tokener_error_parse_null;
          goto out;
        end;
        inc(tok.st_pos);
      end;

    json_tokener_state_comment_start:
      begin
        if(c = '*') then
        begin
          TokRec^.state := json_tokener_state_comment;
        end else
        if (c = '/') then
        begin
          TokRec^.state := json_tokener_state_comment_eol;
        end else
        begin
          tok.err := json_tokener_error_parse_comment;
          goto out;
        end;
        tok.pb.Append(@c, 1);
      end;

    json_tokener_state_comment:
      begin
        if(c = '*') then
          TokRec^.state := json_tokener_state_comment_end;
        tok.pb.Append(@c, 1);
      end;

    json_tokener_state_comment_eol:
      begin
        if (c = #10) then
        begin
          //mc_debug("json_tokener_comment: %s\n", tok.pb.buf);
          TokRec^.state := json_tokener_state_eatws;
        end else
        begin
          tok.pb.Append(@c, 1);
        end
      end;

    json_tokener_state_comment_end:
      begin
        tok.pb.Append(@c, 1);
        if (c = '/') then
        begin
          //mc_debug("json_tokener_comment: %s\n", tok.pb.buf);
          TokRec^.state := json_tokener_state_eatws;
        end else
        begin
          TokRec^.state := json_tokener_state_comment;
        end
      end;

    json_tokener_state_string:
      begin
        if (c = tok.quote_char) then
        begin
          TokRec^.current := TJsonObject.Create(tok.pb.Fbuf);
          TokRec^.saved_state := json_tokener_state_finish;
          TokRec^.state := json_tokener_state_eatws;
        end else
        if (c = '\') then
        begin
          TokRec^.saved_state := json_tokener_state_string;
          TokRec^.state := json_tokener_state_string_escape;
        end else
        begin
          tok.pb.Append(@c, 1);
        end
      end;

    json_tokener_state_string_escape:
      case c of
      '"',
      '\',
      '/':
        begin
          tok.pb.Append(@c, 1);
          TokRec^.state := TokRec^.saved_state;
        end;
      'b',
      'n',
      'r',
      't':
        begin
          if(c = 'b') then tok.pb.Append(#8, 1)
          else if(c = 'n') then tok.pb.Append(#10, 1)
          else if(c = 'r') then tok.pb.Append(#13, 1)
          else if(c = 't') then tok.pb.Append(#9, 1);
          TokRec^.state := TokRec^.saved_state;
        end;
      'u':
        begin
          tok.ucs_char := 0;
          tok.st_pos := 0;
          TokRec^.state := json_tokener_state_escape_unicode;
        end;
      else
        tok.err := json_tokener_error_parse_string;
        goto out;
      end;

    json_tokener_state_escape_unicode:
      begin
        if (c in json_hex_chars_set) then
        begin
          inc(tok.ucs_char, (cardinal(hexdigit(c)) shl ((3-tok.st_pos)*4)));
          inc(tok.st_pos);
          if (tok.st_pos = 4) then
          begin
            if (tok.ucs_char < $80) then
            begin
              utf_out[0] := tok.ucs_char;
              tok.pb.Append(@utf_out, 1);
            end else
            if (tok.ucs_char < $800) then
            begin
              utf_out[0] := $c0 or (tok.ucs_char shr 6);
              utf_out[1] := $80 or (tok.ucs_char and $3f);
              tok.pb.Append(@utf_out, 2);
            end else
            begin
              utf_out[0] := $e0 or (tok.ucs_char shr 12);
              utf_out[1] := $80 or ((tok.ucs_char shr 6) and $3f);
              utf_out[2] := $80 or (tok.ucs_char and $3f);
              tok.pb.Append(@utf_out, 3);
            end;
            TokRec^.state := TokRec^.saved_state;
          end
        end else
        begin
          tok.err := json_tokener_error_parse_string;
          goto out;
        end
      end;

    json_tokener_state_boolean:
      begin
        tok.pb.Append(@c, 1);
{$IFDEF JSON_CASE_INSENSITIVE}
        if (StrLIComp('true', tok.pb.FBuf, min(tok.st_pos + 1, 4)) = 0) then
{$ELSE}
        if (StrLComp('true', tok.pb.FBuf, min(tok.st_pos + 1, 4)) = 0) then
{$ENDIF}
        begin
          if (tok.st_pos = 4) then
          begin
            TokRec^.current := TJsonObject.Create(true);
            TokRec^.saved_state := json_tokener_state_finish;
            TokRec^.state := json_tokener_state_eatws;
            goto redo_char;
          end
        end else
{$IFDEF JSON_CASE_INSENSITIVE}
        if (StrLIComp('false', tok.pb.FBuf, min(tok.st_pos + 1, 5)) = 0) then
{$ELSE}
        if (StrLComp('false', tok.pb.FBuf, min(tok.st_pos + 1, 5)) = 0) then
{$ENDIF}
        begin
          if (tok.st_pos = 5) then
          begin
            TokRec^.current := TJsonObject.Create(false);
            TokRec^.saved_state := json_tokener_state_finish;
            TokRec^.state := json_tokener_state_eatws;
            goto redo_char;
          end
        end else
        begin
          tok.err := json_tokener_error_parse_boolean;
          goto out;
        end;
        inc(tok.st_pos);
      end;

    json_tokener_state_number:
      begin
        if (c in json_number_chars_set) then
        begin
          tok.pb.Append(@c, 1);
          if (c in ['.','e']) then tok.is_double := 1;
        end else
        begin
          if (tok.is_double = 0) then
          begin
            val(tok.pb.FBuf, numi, code);
            TokRec^.current := TJsonObject.Create(numi);
          end else
          if (tok.is_double <> 0) then
          begin
            val(tok.pb.FBuf, numd, code);
            TokRec^.current := TJsonObject.Create(numd);
          end else
          begin
            tok.err := json_tokener_error_parse_number;
            goto out;
          end;
          TokRec^.saved_state := json_tokener_state_finish;
          TokRec^.state := json_tokener_state_eatws;
          goto redo_char;
        end
      end;

    json_tokener_state_array:
      begin
        if (c = ']') then
        begin
          TokRec^.saved_state := json_tokener_state_finish;
          TokRec^.state := json_tokener_state_eatws;
        end else
        begin
          if(tok.depth >= JSON_TOKENER_MAX_DEPTH-1) then
          begin
            tok.err := json_tokener_error_depth;
            goto out;
          end;
          TokRec^.state := json_tokener_state_array_add;
          inc(tok.depth);
          tok.ResetLevel(tok.depth);
          TokRec := @tok.stack[tok.depth];
          goto redo_char;
        end
      end;

    json_tokener_state_array_add:
      begin
        TokRec^.current.AsArray.Add(obj);
        TokRec^.saved_state := json_tokener_state_array_sep;
        TokRec^.state := json_tokener_state_eatws;
        goto redo_char;
      end;  

    json_tokener_state_array_sep:
      begin
        if (c = ']') then
        begin
          TokRec^.saved_state := json_tokener_state_finish;
          TokRec^.state := json_tokener_state_eatws;
        end else
        if (c = ',') then
        begin
          TokRec^.saved_state := json_tokener_state_array;
          TokRec^.state := json_tokener_state_eatws;
        end else
        begin
          tok.err := json_tokener_error_parse_array;
          goto out;
        end
      end;

    json_tokener_state_object_field_start:
      begin
        if (c = '}') then
        begin
          TokRec^.saved_state := json_tokener_state_finish;
          TokRec^.state := json_tokener_state_eatws;
        end else
        if (c in ['"', '''']) then
        begin
          tok.quote_char := c;
          tok.pb.Reset;
          TokRec^.state := json_tokener_state_object_field;
        end else
        begin
          tok.err := json_tokener_error_parse_object_key_name;
          goto out;
        end
      end;

    json_tokener_state_object_field:
      begin
        if (c = tok.quote_char) then
        begin
          TokRec^.obj_field_name := strdup(tok.pb.FBuf);
          TokRec^.saved_state := json_tokener_state_object_field_end;
          TokRec^.state := json_tokener_state_eatws;
        end else
        if (c = '\') then
        begin
          TokRec^.saved_state := json_tokener_state_object_field;
          TokRec^.state := json_tokener_state_string_escape;
        end else
        begin
          tok.pb.Append(@c, 1);
        end
      end;

    json_tokener_state_object_field_end:
      begin
        if (c = ':') then
        begin
          TokRec^.saved_state := json_tokener_state_object_value;
          TokRec^.state := json_tokener_state_eatws;
        end else
        begin
          tok.err := json_tokener_error_parse_object_key_sep;
          goto out;
        end
      end;

    json_tokener_state_object_value:
      begin
        if (tok.depth >= JSON_TOKENER_MAX_DEPTH-1) then
        begin
          tok.err := json_tokener_error_depth;
          goto out;
        end;
        TokRec^.state := json_tokener_state_object_value_add;
        inc(tok.depth);
        tok.ResetLevel(tok.depth);
        TokRec := @tok.stack[tok.depth];
        goto redo_char;
      end;

    json_tokener_state_object_value_add:
      begin
        TokRec^.current.AsObject.Put(TokRec^.obj_field_name, obj);
        FreeMem(TokRec^.obj_field_name);
        TokRec^.obj_field_name := nil;
        TokRec^.saved_state := json_tokener_state_object_sep;
        TokRec^.state := json_tokener_state_eatws;
        goto redo_char;
      end;

    json_tokener_state_object_sep:
      begin
        if (c = '}') then
        begin
          TokRec^.saved_state := json_tokener_state_finish;
          TokRec^.state := json_tokener_state_eatws;
        end else
        if (c = ',') then
        begin
          TokRec^.saved_state := json_tokener_state_object_field_start;
          TokRec^.state := json_tokener_state_eatws;
        end else
        begin
          tok.err := json_tokener_error_parse_object_value_sep;
          goto out;
        end
      end;
    end;
    inc(str);
    inc(tok.char_offset);
  until c = #0;

  if(TokRec^.state <> json_tokener_state_finish) and
     (TokRec^.saved_state <> json_tokener_state_finish) then
    tok.err := json_tokener_error_parse_eof;

 out:
  if(tok.err = json_tokener_success) then
  begin
    TokRec^.current.AddRef;
    Result := TokRec^.current;
    Exit;
  end;
  //mc_debug("json_tokener_parse_ex: error %s at offset %d\n",
  //json_tokener_errors[tok.err], tok.char_offset);
  Result := nil;
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
        JsonFindClose(ite);
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
  FSize := JSON_ARRAY_LIST_DEFAULT_SIZE;
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

{ TJsonWriterString }

function TJsonWriterString.Append(buf: PChar; Size: Integer): Integer;
  function max(a, b: Integer): integer; begin if a > b then  Result := a else Result := b end;
begin
  Result := size;
  if Size > 0 then
  begin
    if (FSize - FBPos <= size) then
    begin
      FSize := max(FSize * 2, FBPos + size + 8);
      ReallocMem(FBuf, FSize);
    end;
    // fast move
    case size of
    1: FBuf[FBPos] := buf^;
    2: PWord(@FBuf[FBPos])^ := PWord(buf)^;
    4: PInteger(@FBuf[FBPos])^ := PInteger(buf)^;
    8: PInt64(@FBuf[FBPos])^ := PInt64(buf)^;
    else
      move(buf^, FBuf[FBPos], size);
    end;
    inc(FBPos, size);
    FBuf[FBPos] := #0;
  end;
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

procedure TJsonRpcService.doDeleteEntry(Entry: TJsonAvlEntry);
begin
  Freemem(Entry.FObj);
  inherited;
end;

procedure TJsonRpcService.Invoke(Obj: TJsonTableString; out Result: TJsonObject; out error: string);
var
  Method: TJsonObject;
  Params: TJsonObject;
  p: TJsonAvlEntry;
begin
  Result := nil;
  // find Method
  Method := TJsonObject(obj.Get(PChar('method')));
  if Method = nil then
  begin
    Error := 'Procedure not found.';
    Exit;
  end;
  p := search(Method.AsString);
  if p = nil then
  begin
    error := format('Method %s not found.', [Method.AsString]);
    Exit;
  end;

  // find params
  Params := obj.Get(PChar('params'));
  if not (JsonIsValid(Params) and (Params.JsonType = json_type_array)) then
  begin
    Error := 'Params must be an array.';
    Exit;
  end;

  // Call method
  try
    PJsonRpcMethod(p.FObj)^(Params, Result);
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
  p: PJsonRpcMethod;
begin
  GetMem(p, sizeof(TJsonRpcMethod));
  TMethod(p^).Code := Method;
  TMethod(p^).Data := sender;
  Insert(TJsonAvlEntry.Create(Aname, p));
end;

{ TJsonRpcServiceList }

procedure TJsonRpcServiceList.doDeleteEntry(Entry: TJsonAvlEntry);
begin
  TObject(Entry.FObj).Free;
  inherited;
end;

function TJsonRpcServiceList.Invoke(service: TJsonRpcService; Obj: TJsonObject; var error: string): TJsonObject;
var
  Table: TJsonTableString;
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
  p: TJsonAvlEntry;
  error: string;
begin

  p := Search(service);
  if (p = nil) then
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
    ret := Invoke(TJsonRpcService(p.FObj), js, error);
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

procedure TJsonRpcServiceList.RegisterService(AName: PChar; obj: TJsonRpcService);
begin
  Insert(TJsonAvlEntry.Create(AName, obj));
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
        #8,#10,#13,#9,'"','\','/':
          begin
            if(pos - start_offset > 0) then
              Append(str + start_offset, pos - start_offset);
            if(c = #8) then Append('\b', 2)
            else if (c = #10) then Append('\n', 2)
            else if (c = #13) then Append('\r', 2)
            else if (c = #9) then Append('\t', 2)
            else if (c = '"') then Append('\"', 2)
            else if (c = '\') then Append('\\', 2)
            else if (c = '/') then Append('\/', 2);
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
        end else
          inc(pos);
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
        JsonFindClose(iter);
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

{ TJsonTokener }

constructor TJsonTokener.Create;
begin
  pb := TJsonWriterString.Create;
  Reset;
end;

destructor TJsonTokener.Destroy;
begin
  Reset;
  pb.Free;
  inherited;
end;

procedure TJsonTokener.Reset;
var
  i: integer;
begin
  for i := depth downto 0 do
    ResetLevel(i);
  depth := 0;
  err := json_tokener_success;
end;

procedure TJsonTokener.ResetLevel(adepth: integer);
begin
  stack[adepth].state := json_tokener_state_eatws;
  stack[adepth].saved_state := json_tokener_state_start;
  stack[adepth].current.Release;
  stack[adepth].current := nil;
  FreeMem(stack[adepth].obj_field_name);
  stack[adepth].obj_field_name := nil;
end;

{ TJsonAvlTree }

constructor TJsonAvlTree.Create;
begin
  FRoot := nil;
  FCount := 0;
end;

destructor TJsonAvlTree.Destroy;
begin
  Clear;
  inherited;
end;

function TJsonAvlTree.IsEmpty: boolean;
begin
  result := FRoot = nil;
end;

function TJsonAvlTree.balance(bal: TJsonAvlEntry): TJsonAvlEntry;
var
  deep, old: TJsonAvlEntry;
  bf: integer;
begin
  if (bal.FBf > 0) then
  begin
    deep := bal.FGt;
    if (deep.FBf < 0) then
    begin
      old := bal;
      bal := deep.FLt;
      old.FGt := bal.FLt;
      deep.FLt := bal.FGt;
      bal.FLt := old;
      bal.FGt := deep;
      bf := bal.FBf;
      if (bf <> 0) then
      begin
        if (bf > 0) then
        begin
          old.FBf := -1;
          deep.FBf := 0;
        end else
        begin
          deep.FBf := 1;
          old.FBf := 0;
        end;
        bal.FBf := 0;
      end else
      begin
        old.FBf := 0;
        deep.FBf := 0;
      end;
    end else
    begin
      bal.FGt := deep.FLt;
      deep.FLt := bal;
      if (deep.FBf = 0) then
      begin
        deep.FBf := -1;
        bal.FBf := 1;
      end else
      begin
        deep.FBf := 0;
        bal.FBf := 0;
      end;
      bal := deep;
    end;
  end else
  begin
    (* "Less than" subtree is deeper. *)

    deep := bal.FLt;
    if (deep.FBf > 0) then
    begin
      old := bal;
      bal := deep.FGt;
      old.FLt := bal.FGt;
      deep.FGt := bal.FLt;
      bal.FGt := old;
      bal.FLt := deep;

      bf := bal.FBf;
      if (bf <> 0) then
      begin
        if (bf < 0) then
        begin
          old.FBf := 1;
          deep.FBf := 0;
        end else
        begin
          deep.FBf := -1;
          old.FBf := 0;
        end;
        bal.FBf := 0;
      end else
      begin
        old.FBf := 0;
        deep.FBf := 0;
      end;
    end else
    begin
      bal.FLt := deep.FGt;
      deep.FGt := bal;
      if (deep.FBf = 0) then
      begin
        deep.FBf := 1;
        bal.FBf := -1;
      end else
      begin
        deep.FBf := 0;
        bal.FBf := 0;
      end;
      bal := deep;
    end;
  end;
  Result := bal;
end;

function TJsonAvlTree.Insert(h: TJsonAvlEntry): TJsonAvlEntry;
var
  unbal, parentunbal, hh, parent: TJsonAvlEntry;
  depth, unbaldepth: longint;
  cmp: integer;
  unbalbf: integer;
  branch: TJsonAvlBitArray;
begin
  inc(FCount);
  h.FLt := nil;
  h.FGt := nil;
  h.FBf := 0;
  branch := [];

  if (FRoot = nil) then
    FRoot := h
  else
  begin
    unbal := nil;
    parentunbal := nil;
    depth := 0;
    unbaldepth := 0;
    hh := FRoot;
    parent := nil;
    repeat
      if (hh.FBf <> 0) then
      begin
        unbal := hh;
        parentunbal := parent;
        unbaldepth := depth;
      end;
      if hh.FHash <> h.FHash then
        cmp := hh.FHash - h.FHash else
        cmp := CompareNodeNode(h, hh);
      if (cmp = 0) then
      begin
        Result := hh;
        doDeleteEntry(h);
        dec(FCount);
        exit;
      end;
      parent := hh;
      if (cmp > 0) then
      begin
        hh := hh.FGt;
        include(branch, depth);
      end else
      begin
        hh := hh.FLt;
        exclude(branch, depth);
      end;
      inc(depth);
    until (hh = nil);

    if (cmp < 0) then
      parent.FLt := h else
      parent.FGt := h;

    depth := unbaldepth;

    if (unbal = nil) then
      hh := FRoot
    else
    begin
      if depth in branch then
        cmp := 1 else
        cmp := -1;
      inc(depth);
      unbalbf := unbal.FBf;
      if (cmp < 0) then
        dec(unbalbf) else
        inc(unbalbf);
      if cmp < 0 then
        hh := unbal.FLt else
        hh := unbal.FGt;
      if ((unbalbf <> -2) and (unbalbf <> 2)) then
      begin
        unbal.FBf := unbalbf;
        unbal := nil;
      end;
    end;

    if (hh <> nil) then
      while (h <> hh) do
      begin
        if depth in branch then
          cmp := 1 else
          cmp := -1;
        inc(depth);
        if (cmp < 0) then
        begin
          hh.FBf := -1;
          hh := hh.FLt;
        end else (* cmp > 0 *)
        begin
          hh.FBf := 1;
          hh := hh.FGt;
        end;
      end;

    if (unbal <> nil) then
    begin
      unbal := balance(unbal);
      if (parentunbal = nil) then
        FRoot := unbal
      else
      begin
        depth := unbaldepth - 1;
        if depth in branch then
          cmp := 1 else
          cmp := -1;
        if (cmp < 0) then
          parentunbal.FLt := unbal else
          parentunbal.FGt := unbal;
      end;
    end;
  end;
  result := h;
end;

function TJsonAvlTree.Search(k: PChar; st: TJsonAvlSearchTypes): TJsonAvlEntry;
var
  cmp, target_cmp: integer;
  match_h, h: TJsonAvlEntry;
  ha: Cardinal;
begin
  ha := TJsonAvlEntry.Hash(k);

  match_h := nil;
  h := FRoot;

  if (stLess in st) then
    target_cmp := 1 else
    if (stGreater in st) then
      target_cmp := -1 else
      target_cmp := 0;

  while (h <> nil) do
  begin
    cmp := h.FHash - ha;
    if cmp = 0 then
      cmp := CompareKeyNode(k, h);
    if (cmp = 0) then
    begin
      if (stEqual in st) then
      begin
        match_h := h;
        break;
      end;
      cmp := -target_cmp;
    end
    else
    if (target_cmp <> 0) then
      if ((cmp xor target_cmp) and JSON_AVL_MASK_HIGH_BIT) = 0 then
        match_h := h;
    if cmp < 0 then
      h := h.FLt else
      h := h.FGt;
  end;
  result := match_h;
end;

procedure TJsonAvlTree.Delete(k: PChar);
var
  depth, rm_depth: longint;
  branch: TJsonAvlBitArray;
  h, parent, child, path, rm, parent_rm: TJsonAvlEntry;
  cmp, cmp_shortened_sub_with_path, reduced_depth, bf: integer;
  ha: Cardinal;
begin
  ha := TJsonAvlEntry.Hash(k);
  cmp_shortened_sub_with_path := 0;
  branch := [];

  depth := 0;
  h := FRoot;
  parent := nil;
  while true do
  begin
    if (h = nil) then
      exit;
    cmp := h.FHash - ha;
    if cmp = 0 then
      cmp := CompareKeyNode(k, h);
    if (cmp = 0) then
      break;
    parent := h;
    if (cmp > 0) then
    begin
      h := h.FGt;
      include(branch, depth)
    end else
    begin
      h := h.FLt;
      exclude(branch, depth)
    end;
    inc(depth);
    cmp_shortened_sub_with_path := cmp;
  end;
  rm := h;
  parent_rm := parent;
  rm_depth := depth;

  if (h.FBf < 0) then
  begin
    child := h.FLt;
    exclude(branch, depth);
    cmp := -1;
  end else
  begin
    child := h.FGt;
    include(branch, depth);
    cmp := 1;
  end;
  inc(depth);

  if (child <> nil) then
  begin
    cmp := -cmp;
    repeat
      parent := h;
      h := child;
      if (cmp < 0) then
      begin
        child := h.FLt;
        exclude(branch, depth);
      end else
      begin
        child := h.FGt;
        include(branch, depth);
      end;
      inc(depth);
    until (child = nil);

    if (parent = rm) then
      cmp_shortened_sub_with_path := -cmp else
      cmp_shortened_sub_with_path := cmp;

    if cmp > 0 then
      child := h.FLt else
      child := h.FGt;
  end;

  if (parent = nil) then
    FRoot := child else
    if (cmp_shortened_sub_with_path < 0) then
      parent.FLt := child else
      parent.FGt := child;

  if parent = rm then
    path := h else
    path := parent;

  if (h <> rm) then
  begin
    h.FLt := rm.FLt;
    h.FGt := rm.FGt;
    h.FBf := rm.FBf;
    if (parent_rm = nil) then
      FRoot := h
    else
    begin
      depth := rm_depth - 1;
      if (depth in branch) then
        parent_rm.FGt := h else
        parent_rm.FLt := h;
    end;
  end;

  if (path <> nil) then
  begin
    h := FRoot;
    parent := nil;
    depth := 0;
    while (h <> path) do
    begin
      if (depth in branch) then
      begin
        child := h.FGt;
        h.FGt := parent;
      end else
      begin
        child := h.FLt;
        h.FLt := parent;
      end;
      inc(depth);
      parent := h;
      h := child;
    end;

    reduced_depth := 1;
    cmp := cmp_shortened_sub_with_path;
    while true do
    begin
      if (reduced_depth <> 0) then
      begin
        bf := h.FBf;
        if (cmp < 0) then
          inc(bf) else
          dec(bf);
        if ((bf = -2) or (bf = 2)) then
        begin
          h := balance(h);
          bf := h.FBf;
        end else
          h.FBf := bf;
        reduced_depth := integer(bf = 0);
      end;
      if (parent = nil) then
        break;
      child := h;
      h := parent;
      dec(depth);
      if depth in branch then
        cmp := 1 else
        cmp := -1;
      if (cmp < 0) then
      begin
        parent := h.FLt;
        h.FLt := child;
      end else
      begin
        parent := h.FGt;
        h.FGt := child;
      end;
    end;
    FRoot := h;
  end;
  if rm <> nil then
  begin
    doDeleteEntry(rm);
    dec(FCount);
  end;
end;

procedure TJsonAvlTree.Clear;
var
  node1, node2: TJsonAvlEntry;
begin
  node1 := FRoot;
  while node1 <> nil do
  begin
    if (node1.FLt = nil) then
    begin
      node2 := node1.FGt;
      doDeleteEntry(node1);
    end
    else
    begin
      node2 := node1.FLt;
      node1.FLt := node2.FGt;
      node2.FGt := node1;
    end;
    node1 := node2;
  end;
  FRoot := nil;
  FCount := 0;
end;

function TJsonAvlTree.CompareKeyNode(k: PChar; h: TJsonAvlEntry): integer;
begin
{$IFDEF JSON_CASE_INSENSITIVE}
    Result := StrIComp(k, h.FName);
{$ELSE}
    Result := StrComp(k, h.FName);
{$ENDIF}
end;

function TJsonAvlTree.CompareNodeNode(node1, node2: TJsonAvlEntry): integer;
begin
{$IFDEF JSON_CASE_INSENSITIVE}
    Result := StrIComp(node1.FName, node2.FName);
{$ELSE}
    Result := StrComp(node1.FName, node2.FName);
{$ENDIF}
end;

{ TJsonAvlIterator }

(* Initialize depth to invalid value, to indicate iterator is
** invalid.   (Depth is zero-base.)  It's not necessary to initialize
** iterators prior to passing them to the "start" function.
*)

constructor TJsonAvlIterator.Create(tree: TJsonAvlTree);
begin
  FDepth := not 0;
  FTree := tree;
end;

procedure TJsonAvlIterator.Search(k: PChar; st: TJsonAvlSearchTypes);
var
  h: TJsonAvlEntry;
  d: longint;
  cmp, target_cmp: integer;
  ha: Cardinal;
begin
  ha := TJsonAvlEntry.Hash(k);
  h := FTree.FRoot;
  d := 0;
  FDepth := not 0;
  if (h = nil) then
    exit;

  if (stLess in st) then
    target_cmp := 1 else
      if (stGreater in st) then
        target_cmp := -1 else
          target_cmp := 0;

  while true do
  begin
    cmp := h.FHash - ha;
    if cmp = 0 then
      cmp := FTree.CompareKeyNode(k, h);
    if (cmp = 0) then
    begin
      if (stEqual in st) then
      begin
        FDepth := d;
        break;
      end;
      cmp := -target_cmp;
    end
    else
    if (target_cmp <> 0) then
      if ((cmp xor target_cmp) and JSON_AVL_MASK_HIGH_BIT) = 0 then
        FDepth := d;
    if cmp < 0 then
      h := h.FLt else
      h := h.FGt;
    if (h = nil) then
      break;
    if (cmp > 0) then
      include(FBranch, d) else
      exclude(FBranch, d);
    FPath[d] := h;
    inc(d);
  end;
end;

procedure TJsonAvlIterator.First;
var
  h: TJsonAvlEntry;
begin
  h := FTree.FRoot;
  FDepth := not 0;
  FBranch := [];
  while (h <> nil) do
  begin
    if (FDepth <> not 0) then
      FPath[FDepth] := h;
    inc(FDepth);
    h := h.FLt;
  end;
end;

procedure TJsonAvlIterator.Last;
var
  h: TJsonAvlEntry;
begin
  h := FTree.FRoot;
  FDepth := not 0;
  FBranch := [0..JSON_AVL_MAX_DEPTH - 1];
  while (h <> nil) do
  begin
    if (FDepth <> not 0) then
      FPath[FDepth] := h;
    inc(FDepth);
    h := h.FGt;
  end;
end;

function TJsonAvlIterator.GetIter: TJsonAvlEntry;
begin
  if (FDepth = not 0) then
  begin
    result := nil;
    exit;
  end;
  if FDepth = 0 then
    Result := FTree.FRoot else
    Result := FPath[FDepth - 1];
end;

procedure TJsonAvlIterator.Next;
var
  h: TJsonAvlEntry;
begin
  if (FDepth <> not 0) then
  begin
    if FDepth = 0 then
      h := FTree.FRoot.FGt else
      h := FPath[FDepth - 1].FGt;

    if (h = nil) then
      repeat
        if (FDepth = 0) then
        begin
          FDepth := not 0;
          break;
        end;
        dec(FDepth);
      until (not (FDepth in FBranch))
    else
    begin
      include(FBranch, FDepth);
      FPath[FDepth] := h;
      inc(FDepth);
      while true do
      begin
        h := h.FLt;
        if (h = nil) then
          break;
        exclude(FBranch, FDepth);
        FPath[FDepth] := h;
        inc(FDepth);
      end;
    end;
  end;
end;

procedure TJsonAvlIterator.Prior;
var
  h: TJsonAvlEntry;
begin
  if (FDepth <> not 0) then
  begin
    if FDepth = 0 then
      h := FTree.FRoot.FLt else
      h := FPath[FDepth - 1].FLt;
    if (h = nil) then
      repeat
        if (FDepth = 0) then
        begin
          FDepth := not 0;
          break;
        end;
        dec(FDepth);
      until (FDepth in FBranch)
    else
    begin
      exclude(FBranch, FDepth);
      FPath[FDepth] := h;
      inc(FDepth);
      while true do
      begin
        h := h.FGt;
        if (h = nil) then
          break;
        include(FBranch, FDepth);
        FPath[FDepth] := h;
        inc(FDepth);
      end;
    end;
  end;
end;

procedure TJsonAvlTree.doDeleteEntry(Entry: TJsonAvlEntry);
begin
  Entry.Free;
end;

{ TJsonAvlEntry }

constructor TJsonAvlEntry.Create(AName: PChar; Obj: Pointer);
begin
  FName := strdup(AName);
  FObj := Obj;
  FHash := Hash(FName);
end;

destructor TJsonAvlEntry.Destroy;
begin
  FreeMem(FName);
  inherited;
end;

class function TJsonAvlEntry.Hash(k: PChar): Cardinal;
var
  h: Cardinal;
begin
  h := 0;
  if k <> nil then
    while(k^ <> #0 ) do
    begin
  {$IFDEF JSON_CASE_INSENSITIVE}
      h := h*129 + byte(UpCase(k^)) + $9e370001;
  {$ELSE}
      h := h*129 + byte(k^) + $9e370001;
  {$ENDIF}
      inc(k);
    end;
  Result := h;
end;

{ TJsonTableString }

procedure TJsonTableString.doDeleteEntry(Entry: TJsonAvlEntry);
begin
  TJsonObject(Entry.FObj).Release;
  inherited;
end;

function TJsonTableString.Get(k: PChar): TJsonObject;
var
  e: TJsonAvlEntry;
begin
  e := Search(k);
  if e <> nil then
    Result := TJsonObject(e.FObj) else
    Result := nil
end;

function TJsonTableString.Put(k: PChar; Obj: TJsonObject): TJsonObject;
begin
  Result := TJsonObject(Insert(TJsonAvlEntry.Create(k, obj)).FObj);
end;

end.
