(********************************************************************************)
(*                        UNIFIED INTERBASE (UIB)                               *)
(*                                                                              *)
(* The contents of this file are subject to the Mozilla Public License Version  *)
(* 1.1 (the "License"); you may not use this file except in compliance with the *)
(* License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ *)
(*                                                                              *)
(* Software distributed under the License is distributed on an "AS IS" basis,   *)
(* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for *)
(* the specific language governing rights and limitations under the License.    *)
(*                                                                              *)
(* Unit owner : Henri Gourvest <hgourvest@progdigy.com>                         *)
(* Contributor:                                                                 *)
(*     Volkan Ceylan <volkance@hotmail.com>                                     *)
(*     Olivier Guilbaud <oguilb@free.fr>                                        *)
(********************************************************************************)

unit jvuibdataset;

{$I jvuib.inc}

interface

uses
  SysUtils, Classes, DB, jvuib, jvuiblib, jvuibase, jvuibconst;

type

  TUIBBookMark = record
    Bookmark: Longint;
    BookmarkFlag: TBookmarkFlag;
  end;
  PUIBBookMark = ^TUIBBookMark;

{$IFNDEF FPC}
  TUIBFieldDef = class(TFieldDef)
  private
    FOrigin: string;
  public
    property Origin: string read FOrigin write FOrigin;
  end;
{$ENDIF}

{$IFNDEF FPC}
  TUIBBCDField = class(TBCDField)
  protected
    class procedure CheckTypeSize(Value: Integer); override;
    function GetAsCurrency: Currency; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetDataSize: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Size default 8;
  end;
{$ENDIF}

  TJvUIBCustomDataSet = class(TDataSet)
  private
    FStatement: TJvUIBQuery;
    FOnClose: TEndTransMode;
    FIsLast, FIsFirst: boolean;
    FCurrentRecord: Integer;
    FComplete: boolean;
    FIsOpen: Boolean;
    FRecordSize : Integer;
    FRecordBufferSize: Integer;
    procedure OnStatementClose(Sender: TObject);
    function GetOnError: TEndTransMode;
    function GetSQL: TStrings;
    function GetTransaction: TJvUIBTransaction;
    function GetUniDirectional: boolean;
    procedure SetOnClose(const Value: TEndTransMode);
    procedure SetOnError(const Value: TEndTransMode);
    procedure SetSQL(const Value: TStrings);
    procedure SetTransaction(const Value: TJvUIBTransaction);
    procedure SetUniDirectional(const Value: boolean);
    function GetFetchBlobs: boolean;
    procedure SetFetchBlobs(const Value: boolean);
    procedure SetDatabase(const Value: TJvUIBDataBase);
    function GetDatabase: TJvUIBDataBase;
    function GetParams: TSQLParams;
    function GetInternalFields: TSQLResult;
    function GetBufferChunks: Cardinal;
    procedure SetBufferChunks(const Value: Cardinal);
    function GetRowsAffected: Cardinal;
  protected
  {$IFNDEF FPC}
    function GetFieldClass(FieldType: TFieldType): TFieldClass; override;
  {$ENDIF}
    property BufferChunks: Cardinal read GetBufferChunks write SetBufferChunks default 1000;
    procedure InternalOpen; override;
    procedure InternalClose; override;
    function IsCursorOpen: Boolean; override;

    function AllocRecordBuffer: PChar; override;
    procedure InternalInitRecord(Buffer: PChar); override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    function GetRecordSize: Word; override;

    function GetRecord(Buffer: PChar; GetMode: TGetMode;
      DoCheck: Boolean): TGetResult; override;
    procedure InternalFirst; override;
    procedure InternalLast; override;
    function GetRecNo: Longint; override;
    function GetRecordCount: Longint; override;
    procedure SetRecNo(Value: Integer); override;

    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalSetToRecord(Buffer: PChar); override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;

    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    function GetCanModify: Boolean; override;

    procedure InternalRefresh; override;

    {$IFNDEF FPC}
    procedure SetActive(Value: Boolean); override;
    procedure CreateFields; override;
    {$ENDIF}


    property Transaction: TJvUIBTransaction read GetTransaction write SetTransaction;
    property Database: TJvUIBDataBase read GetDatabase write SetDatabase;
    property UniDirectional: boolean read  GetUniDirectional write SetUniDirectional default False;
    property OnClose: TEndTransMode read FOnClose write SetOnClose default etmCommit;
    property OnError: TEndTransMode read GetOnError write SetOnError default etmRollback;
    property SQL: TStrings read GetSQL write SetSQL;
    property FetchBlobs: boolean read GetFetchBlobs write SetFetchBlobs default False;
    property Params: TSQLParams read GetParams;
    property RowsAffected: Cardinal read GetRowsAffected;

{$IFNDEF COMPILER5_UP}
    function BCDToCurr(BCD: Pointer; var Curr: Currency): Boolean; {$IFNDEF FPC}override;{$ENDIF}
    function CurrToBCD(const Curr: Currency; BCD: Pointer; Precision,
      Decimals: Integer): Boolean; {$IFNDEF FPC}override;{$ENDIF}
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; overload; override;
    function GetFieldData(FieldNo: Integer; Buffer: Pointer): Boolean; overload;{$IFNDEF FPC} override; {$ENDIF}
  {$IFNDEF FPC}
    function GetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean): Boolean; overload; override;
  {$ENDIF}  
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    procedure Execute;
    procedure ExecSQL;
    procedure BuildStoredProc(const StoredProc: string; forSelect: boolean);

    procedure ReadBlob(const Index: Word; Stream: TStream); overload;
    procedure ReadBlob(const Index: Word; var str: string); overload;
    procedure ReadBlob(const Index: Word; var Value: Variant); overload;
    procedure ReadBlob(const name: string; Stream: TStream); overload;
    procedure ReadBlob(const name: string; var str: string); overload;
    procedure ReadBlob(const name: string; var Value: Variant); overload;

    procedure ParamsSetBlob(const Index: Word; Stream: TStream); overload;
    procedure ParamsSetBlob(const Index: Word; var str: string); overload;
    procedure ParamsSetBlob(const Index: Word; Buffer: Pointer; Size: Word); overload;
    procedure ParamsSetBlob(const Name: string; Stream: TStream); overload;
    procedure ParamsSetBlob(const Name: string; var str: string); overload;
    procedure ParamsSetBlob(const Name: string; Buffer: Pointer; Size: Word); overload;

    property InternalFields: TSQLResult read GetInternalFields;
  end;

  TJvUIBDataSet = class(TJvUIBCustomDataSet)
  public
    property Params;
    property RowsAffected;
  published
    property BufferChunks;
    property Transaction;
    property Database;
    property UniDirectional;
    property OnClose;
    property OnError;
    property SQL;
    property FetchBlobs;
    property Active;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeScroll;
    property AfterScroll;
  end;

implementation
{$IFDEF COMPILER6_UP}
  uses fmtbcd, Variants, Math;
{$ENDIF}

{ TJvUIBCustomDataSet }

procedure TJvUIBCustomDataSet.InternalOpen;
begin
  FRecordSize := SizeOf(Integer);
  InternalInitFieldDefs;
  if DefaultFields then
    CreateFields;
  BindFields (True);
  FStatement.Open(False);
  FCurrentRecord := -1;
  FComplete := False;
  FRecordBufferSize := FRecordSize + sizeof (TUIBBookMark);
  BookmarkSize := sizeOf (Integer);
  FIsOpen := True;
end;

procedure TJvUIBCustomDataSet.InternalClose;
begin
  BindFields (False);
  if DefaultFields then
    DestroyFields;
  FStatement.Close(FOnClose);
  FIsOpen := False;
  FCurrentRecord := -1;
  FComplete := False;
end;

function TJvUIBCustomDataSet.IsCursorOpen: Boolean;
begin
  Result := FIsOpen;
end;

procedure TJvUIBCustomDataSet.InternalGotoBookmark (Bookmark: Pointer);
var
  ReqBookmark: Integer;
begin
  ReqBookmark := Integer(Bookmark^);
    FCurrentRecord := ReqBookmark
end;

procedure TJvUIBCustomDataSet.InternalSetToRecord (Buffer: PChar);
var
  ReqBookmark: Integer;
begin
  ReqBookmark := PUIBBookMark(Buffer + FRecordSize).Bookmark;
  InternalGotoBookmark (@ReqBookmark);
end;

function TJvUIBCustomDataSet.GetBookmarkFlag (
  Buffer: PChar): TBookmarkFlag;
begin
  Result := PUIBBookMark(Buffer + FRecordSize).BookmarkFlag;
end;

procedure TJvUIBCustomDataSet.SetBookmarkFlag (Buffer: PChar;
  Value: TBookmarkFlag);
begin
  PUIBBookMark(Buffer + FRecordSize).BookmarkFlag := Value;
end;

procedure TJvUIBCustomDataSet.InternalFirst;
begin
  FStatement.First;
  FIsFirst := not FStatement.Eof;
  FCurrentRecord := 0;
end;

procedure TJvUIBCustomDataSet.InternalLast;
begin
  FStatement.Last;
  FIsLast := True;
  FComplete := True;
  FCurrentRecord := FStatement.Fields.RecordCount - 1;
end;

procedure TJvUIBCustomDataSet.GetBookmarkData (
  Buffer: PChar; Data: Pointer);
begin
  Integer(Data^) :=
    PUIBBookMark(Buffer + FRecordSize).Bookmark;
end;

procedure TJvUIBCustomDataSet.SetBookmarkData (
  Buffer: PChar; Data: Pointer);
begin
  PUIBBookMark(Buffer + FRecordSize).Bookmark :=
    Integer(Data^);
end;

function TJvUIBCustomDataSet.GetRecordCount: Longint;
begin
  CheckActive;
  Result := FStatement.Fields.RecordCount;
end;

function TJvUIBCustomDataSet.GetRecNo: Longint;
begin
  UpdateCursorPos;
  Result := FCurrentRecord + 1;
end;

procedure TJvUIBCustomDataSet.SetRecNo(Value: Integer);
begin
  CheckBrowseMode;
  if (Value >= 1) and (Value <= FStatement.Fields.RecordCount) then
  begin
    FCurrentRecord := Value - 1;
    Resync([]);
  end;
end;

function TJvUIBCustomDataSet.GetRecord(Buffer: PChar;
  GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  if (FCurrentRecord <> -1) and FStatement.CachedFetch and
   (FCurrentRecord < FStatement.Fields.RecordCount) then
      FStatement.Fields.CurrentRecord := FCurrentRecord;

  Result := grOK;

  case GetMode of
    gmNext:
      begin
        if FIsFirst then
        begin
          FIsFirst := False;
        end else
          begin
            if (FCurrentRecord < FStatement.Fields.RecordCount - 1) then
            begin
              FStatement.Fields.CurrentRecord := FCurrentRecord + 1;
              inc(FCurrentRecord);
            end else
              if not FComplete then
              begin
                if (FStatement.StatementType = stSelect) then
                begin
                  FStatement.Next;
                  if FStatement.Eof then
                  begin
                    Result := grEOF;
                    FComplete := True;
                  end else
                    inc(FCurrentRecord);
                end else
                begin
                  FComplete := true;
                  Result := grEOF;
                end;
              end else
               Result := grEOF;
          end;
      end;
    gmPrior:
      begin
        if FIsLast then
          FIsLast := False else
        if FStatement.Fields.CurrentRecord <= 0 then
          Result := grBOF else
          begin
            FStatement.Prior;
            dec(FCurrentRecord);
          end;
      end;
    gmCurrent:
      begin
        if (FCurrentRecord >= FStatement.Fields.RecordCount) then
          result := grError 
      end;
  end;

  PInteger(Buffer)^ := FCurrentRecord;
  with PUIBBookMark(Buffer + FRecordSize)^ do
  begin
    case Result of
      grOK:  BookmarkFlag := bfCurrent;
      grBOF: BookmarkFlag := bfBOF;
      grEOF: BookmarkFlag := bfEOF;
    end;
    Bookmark := PInteger (Buffer)^;
  end;
end;

procedure TJvUIBCustomDataSet.InternalInitRecord(Buffer: PChar);
begin
  FillChar(Buffer^, FRecordBufferSize, #0);
end;

procedure TJvUIBCustomDataSet.FreeRecordBuffer (var Buffer: PChar);
begin
  FreeMem (Buffer);
end;

function TJvUIBCustomDataSet.GetRecordSize: Word;
begin
  Result := FRecordSize;
end;

function TJvUIBCustomDataSet.AllocRecordBuffer: PChar;
begin
  GetMem(Result, FRecordBufferSize);
end;

procedure TJvUIBCustomDataSet.InternalHandleException;
begin
end;

function TJvUIBCustomDataSet.GetOnError: TEndTransMode;
begin
  Result := FStatement.OnError;
end;

function TJvUIBCustomDataSet.GetSQL: TStrings;
begin
  Result := FStatement.SQL;
end;

function TJvUIBCustomDataSet.GetTransaction: TJvUIBTransaction;
begin
  Result := FStatement.Transaction;
end;

function TJvUIBCustomDataSet.GetUniDirectional: boolean;
begin
  Result := not FStatement.CachedFetch;
end;

procedure TJvUIBCustomDataSet.SetOnClose(const Value: TEndTransMode);
begin
  FOnClose := Value;
end;

procedure TJvUIBCustomDataSet.SetOnError(const Value: TEndTransMode);
begin
  FStatement.OnError := Value;
end;

procedure TJvUIBCustomDataSet.SetSQL(const Value: TStrings);
begin
  CheckInactive;
  FStatement.SQL.Assign(Value);
  DataEvent(dePropertyChange, 0);
end;

procedure TJvUIBCustomDataSet.SetTransaction(
  const Value: TJvUIBTransaction);
begin
  FStatement.Transaction := Value;
end;

procedure TJvUIBCustomDataSet.SetUniDirectional(const Value: boolean);
begin
{$IFDEF COMPILER6_UP}
  inherited SetUniDirectional(Value);
{$ENDIF}
  FStatement.CachedFetch := not Value;
end;

constructor TJvUIBCustomDataSet.Create(AOwner: TComponent);
begin
  FStatement := TJvUIBQuery.Create(nil);
  FStatement.OnClose := OnStatementClose;
  FOnClose := etmCommit;
  inherited Create(AOwner);
  FIsLast := False;
  FIsFirst := False;
end;

destructor TJvUIBCustomDataSet.Destroy;
begin
  inherited Destroy;
  FStatement.Free;
end;

procedure TJvUIBCustomDataSet.InternalInitFieldDefs;
var
  i: Integer;
{$IFDEF FPC}
  aName    : string;
  FieldNo  : Integer;
  Required : Boolean;
  DataType : TFieldType;
  Size     : Word;
  Precision: Integer;
{$ELSE}
  count  : Integer;
  TmpName: string;
{$ENDIF}
begin
  FStatement.Prepare;
  {$IFNDEF FPC}
  FieldDefs.BeginUpdate;
  {$ENDIF}
  FieldDefs.Clear;
  try
    for i := 0 to FStatement.Fields.FieldCount - 1 do
    with {$IFNDEF FPC} TUIBFieldDef.Create(FieldDefs),{$ENDIF} FStatement.Fields do
    begin
    {$IFNDEF FPC}
      count := 1;
      TmpName := AliasName[i];
      while TDefCollection(Collection).IndexOf(TmpName) >= 0 do
      begin
        TmpName := TmpName + inttostr(count);
        inc(count);
      end;
      Name := TmpName;
      FOrigin := RelName[i] + '.' + SqlName[i];
    {$ELSE}
      AName := AliasName[i];
      Precision:=-1;
    {$ENDIF}
      FieldNo := i;
      Required := not IsNullable[i];
      case FieldType[i] of
        uftNumeric:
          begin
            case SQLType[i] of
              SQL_SHORT:
                begin
                  {$IFDEF COMPILER6_UP}
                  if -Data.sqlvar[i].SqlScale > 4 then
                    DataType := ftFMTBcd else
                  {$ENDIF}
                  {$IFDEF FPC}
                  if -Data.sqlvar[i].SqlScale > 4 then
                    DataType := ftFloat else
                  {$ENDIF}
                  DataType := ftBCD;
                  Size := -Data.sqlvar[i].SqlScale;
                  if Size = 4 then
                    Precision := 5 else
                    Precision := 4;
                end;
              SQL_LONG:
                begin
                  {$IFDEF COMPILER6_UP}
                  if -Data.sqlvar[i].SqlScale > 4 then
                    DataType := ftFMTBcd else
                  {$ENDIF}
                  {$IFDEF FPC}
                  if -Data.sqlvar[i].SqlScale > 4 then
                    DataType := ftFloat else
                  {$ENDIF}
                    DataType := ftBCD;
                  Size := -Data.sqlvar[i].SqlScale;
                  if Size = 9 then
                    Precision := 10 else
                    Precision := 9;
                end;
              SQL_INT64,
              SQL_QUAD:
                begin
                  {$IFDEF COMPILER6_UP}
                  if -Data.sqlvar[i].SqlScale > 4 then
                    DataType := ftFMTBcd else
                  {$ENDIF}
                  {$IFDEF FPC}
                  if -Data.sqlvar[i].SqlScale > 4 then
                    DataType := ftFloat else
                  {$ENDIF}
                    DataType := ftBCD;
                  Size := -Data.sqlvar[i].SqlScale;
                  if Size = 18 then
                    Precision := 19 else
                    Precision := 18;
                end;
              SQL_DOUBLE:
                DataType := ftFloat; // possible
            end;
          end;
        uftChar,
        uftCstring,
        uftVarchar:
          begin
            DataType := ftString;
            Size := SQLLen[i];
          end;
        uftSmallint: DataType := ftSmallint;
        uftInteger : DataType := ftInteger;
        uftFloat,
        uftDoublePrecision: DataType := ftFloat;
        uftTimestamp: DataType := ftDateTime;
        uftBlob, uftBlobId:
          begin
            if Data.sqlvar[i].SqlSubType = 1 then
              DataType := ftMemo else
              DataType := ftBlob;
            Size := SizeOf(TIscQuad);
          end;
        uftDate : DataType := ftDate;
        uftTime : DataType := ftTime;
        uftInt64:
        {$IFDEF FPC}
          DataType := ftInteger; // :(
        {$ELSE}
          DataType := ftLargeint;
        {$ENDIF}
      {$IFDEF IB7_UP}
        uftBoolean: DataType := ftBoolean;
      {$ENDIF}
      else
        DataType := ftUnknown;
      end;

      {$IFDEF FPC}
      //Add new defs
      FieldDefs.Add(aName,DataType,Size,Required);
      //If Precision is specified, update the definition
      if Precision<>-1 then
          FieldDefs.Items[FieldNo].Precision:=Precision;
      {$ENDIF}
    end; 
  finally
    {$IFNDEF FPC}
    FieldDefs.EndUpdate;
    {$ENDIF}
  end;
end;

function TJvUIBCustomDataSet.GetFieldData(FieldNo: Integer;
  Buffer: Pointer): Boolean;
var
  FieldType: TUIBFieldType;
begin
  dec(FieldNo);
  Result := False;

  if (FStatement.StatementType = stSelect) then
  begin
    if (FCurrentRecord < 0) then
      Exit;
    FStatement.Fields.GetRecord(PInteger(ActiveBuffer)^);
  end;

  if FStatement.Fields.IsNull[FieldNo] then
    Exit;

  if Buffer = nil then
  begin
    Result := True;
    Exit;
  end;
  FieldType := FStatement.Fields.FieldType[FieldNo];
  with FStatement.Fields.Data.sqlvar[FieldNo] do
    case FieldType of
      uftNumeric:
        begin
          case FStatement.Fields.SQLType[FieldNo] of
            SQL_SHORT:
              begin
              {$IFDEF COMPILER6_UP}
                if (sqlScale >= -4) then
                  Currency(Buffer^) := PSmallint(sqldata)^ / scaledivisor[sqlscale] else
                  TBCD(Buffer^) := strToBcd(FloatToStr(PSmallint(sqldata)^ / scaledivisor[sqlscale]));
              {$ELSE}
                {$IFDEF COMPILER5_UP}
                  if (sqlScale >= -4) then
                    Currency(Buffer^) := PSmallint(sqldata)^ / scaledivisor[sqlscale] else
                    CurrToBcd(PSmallint(sqldata)^/scaledivisor[sqlscale], TBCD(Buffer^));
                {$ELSE}
                  {$IFDEF FPC}
                    if (sqlscale = -4) then
                      PInt64(Buffer)^ := PSmallint(sqldata)^ else
                      if sqlscale > -4 then
                        PInt64(Buffer)^ := PSmallint(sqldata)^ * CurrencyDivisor[sqlscale] else
                        PDouble(Buffer)^ := PSmallint(sqldata)^ / scaledivisor[sqlscale];
                  {$ELSE}
                     unexpected
                  {$ENDIF}
                {$ENDIF}
              {$ENDIF}
              end;
            SQL_LONG:
              begin
              {$IFDEF COMPILER6_UP}
                if (sqlScale >= -4) then
                  Currency(Buffer^) := PInteger(sqldata)^ / scaledivisor[sqlscale] else
                  TBCD(Buffer^) := strToBcd(FloatToStr(PInteger(sqldata)^ / scaledivisor[sqlscale]));
              {$ELSE}
                {$IFDEF COMPILER5_UP}
                  if (sqlScale >= -4) then
                    Currency(Buffer^) := PInteger(sqldata)^ / scaledivisor[sqlscale] else
                    CurrToBcd(PInteger(sqldata)^/scaledivisor[sqlscale], TBCD(Buffer^));
                {$ELSE}
                  {$IFDEF FPC}
                    if (sqlscale = -4) then
                      PInt64(Buffer)^ := PInteger(sqldata)^ else
                      if sqlscale > -4 then
                        PInt64(Buffer)^ := PInteger(sqldata)^ * CurrencyDivisor[sqlscale] else
                        PDouble(Buffer)^ := PInteger(sqldata)^ / scaledivisor[sqlscale];
                  {$ELSE}
                    unexpected
                  {$ENDIF}
                {$ENDIF}
              {$ENDIF}
              end;
            SQL_INT64,
            SQL_QUAD:
              begin
              {$IFDEF COMPILER6_UP}
                if (sqlscale = -4) then
                  PInt64(Buffer)^ := PInt64(sqldata)^ else
                  if sqlscale > -4 then
                    PInt64(Buffer)^ := PInt64(sqldata)^ * CurrencyDivisor[sqlscale] else
                    TBCD(Buffer^) := strToBcd(FloatToStr(PInt64(sqldata)^ / scaledivisor[sqlscale]));
              {$ELSE}
                {$IFDEF COMPILER5_UP}
                if (sqlscale = -4) then
                  PInt64(Buffer)^ := PInt64(sqldata)^ else
                  if sqlscale > -4 then
                    PInt64(Buffer)^ := FastInt64Mul(PInt64(sqldata), @CurrencyDivisor[sqlscale]) else
                    CurrToBcd(PInt64(sqldata)^/scaledivisor[sqlscale], TBCD(Buffer^));
                {$ELSE}
                  {$IFDEF FPC}
                    if (sqlscale = -4) then
                      PInt64(Buffer)^ := PInt64(sqldata)^ else
                      if sqlscale > -4 then
                        PInt64(Buffer)^ := PInt64(sqldata)^ * CurrencyDivisor[sqlscale] else
                        PDouble(Buffer)^ := PInt64(sqldata)^ / scaledivisor[sqlscale];
                  {$ELSE}
                    unexpected
                  {$ENDIF}
                {$ENDIF}
              {$ENDIF}
              end;
            SQL_DOUBLE:
              PDouble(Buffer)^ := PDouble(sqldata)^;
          else
            raise Exception.Create(EUIB_UNEXPECTEDCASTERROR);
          end;
        end;
      uftChar,
      uftCstring:
        begin
          Move(sqldata^, Buffer^, SqlLen);
          PChar(Buffer)[SqlLen] := #0;
        end;
      uftVarchar:
        begin
          Move(PVary(sqldata).vary_string, Buffer^, PVary(sqldata).vary_length);
          PChar(Buffer)[PVary(sqldata).vary_length] := #0;
        end;
      uftSmallint: PSmallint(Buffer)^ := PSmallint(sqldata)^;
      uftInteger : PInteger(Buffer)^ := PInteger(sqldata)^;
      uftFloat:
          PDouble(Buffer)^ := PSingle(sqldata)^;
      uftDoublePrecision:
          PDouble(Buffer)^ := PDouble(sqldata)^;
      uftTimestamp:
        begin
          {$IFDEF FPC}
            DecodeTimeStamp(PIscTimeStamp(sqldata), PDouble(Buffer)^);
          {$ELSE}
            DecodeTimeStamp(PIscTimeStamp(sqldata),  TTimeStamp(Buffer^));
            Double(Buffer^) := TimeStampToMSecs(TTimeStamp(Buffer^));
          {$ENDIF}
        end;
      uftBlob, uftBlobId:
        begin
          if Buffer <> nil then
          begin
            FStatement.ReadBlob(FieldNo, TStream(Buffer));
            TStream(Buffer).Seek(0, soFromBeginning);
          end;
        end;
      uftDate:
        {$IFDEF FPC}
          PDouble(Buffer)^ := PInteger(sqldata)^ - DateOffset;
        {$ELSE}
          PInteger(Buffer)^ := PInteger(sqldata)^ - DateOffset + 693594;
        {$ENDIF}
      uftTime:
        {$IFDEF FPC}
          PDouble(Buffer)^ := PCardinal(sqldata)^ / TimeCoeff;
        {$ELSE}
          PInteger(Buffer)^ := PCardinal(sqldata)^ div 10;
        {$ENDIF}
      uftInt64:
        {$IFDEF FPC}
          PInteger(Buffer)^ := PInt64(sqldata)^;
        {$ELSE}
          PInt64(Buffer)^ := PInt64(sqldata)^;
        {$ENDIF}
    {$IFDEF IB7_UP}
      uftBoolean:
        {$IFDEF FPC}
          Boolean(Buffer^) := PSmallInt(sqldata)^ = ISC_TRUE;
        {$ELSE}
          WordBool(Buffer^) := PSmallInt(sqldata)^ = ISC_TRUE;
        {$ENDIF}
    {$ENDIF}
    else
      raise EUIBError.Create(EUIB_UNEXPECTEDERROR);
    end;
  Result := True;
end;

function TJvUIBCustomDataSet.GetFieldData(Field: TField;
  Buffer: Pointer): Boolean;
begin
  CheckActive;
  Result := GetFieldData(Field.FieldNo, Buffer);
end;

function TJvUIBCustomDataSet.GetCanModify: Boolean;
begin
  Result := False;
end;

procedure TJvUIBCustomDataSet.OnStatementClose(Sender: TObject);
begin
  Close;
end;

function TJvUIBCustomDataSet.CreateBlobStream(Field: TField;
  Mode: TBlobStreamMode): TStream;
begin
  if (Mode = bmRead) then
  begin
    Result := TMemoryStream.Create;
    GetFieldData(Field, Result);
  end else
    Result := nil;
end;

function TJvUIBCustomDataSet.GetFetchBlobs: boolean;
begin
  Result := FStatement.FetchBlobs;
end;

procedure TJvUIBCustomDataSet.SetFetchBlobs(const Value: boolean);
begin
  FStatement.FetchBlobs := Value;
end;

procedure TJvUIBCustomDataSet.Execute;
begin
  FStatement.Execute;
end;

procedure TJvUIBCustomDataSet.ExecSQL;
begin
  FStatement.ExecSQL;
end;

{$IFNDEF FPC}
procedure TJvUIBCustomDataSet.SetActive(Value: Boolean);
begin
  inherited;
  if not Value then
    FStatement.Close(FOnClose);
end;
{$ENDIF}

{$IFNDEF COMPILER5_UP}
function TJvUIBCustomDataSet.BCDToCurr(BCD: Pointer;
  var Curr: Currency): Boolean;
begin
  Curr := PCurrency(BCD)^;
  result := True;
end;

function TJvUIBCustomDataSet.CurrToBCD(const Curr: Currency; BCD: Pointer;
  Precision, Decimals: Integer): Boolean;
begin
  PCurrency(BCD)^ := Curr;
  Result := True;
end;
{$ENDIF}

procedure TJvUIBCustomDataSet.SetDatabase(const Value: TJvUIBDataBase);
begin
  FStatement.DataBase := Value;
end;

function TJvUIBCustomDataSet.GetDatabase: TJvUIBDataBase;
begin
  Result := FStatement.DataBase;
end;

procedure TJvUIBCustomDataSet.ParamsSetBlob(const Name: string;
  Stream: TStream);
begin
  FStatement.ParamsSetBlob(Name, Stream);
end;

procedure TJvUIBCustomDataSet.ParamsSetBlob(const Name: string;
  var str: string);
begin
  FStatement.ParamsSetBlob(Name, str);
end;

procedure TJvUIBCustomDataSet.ParamsSetBlob(const Name: string;
  Buffer: Pointer; Size: Word);
begin
  FStatement.ParamsSetBlob(Name, Buffer, Size);
end;

procedure TJvUIBCustomDataSet.ParamsSetBlob(const Index: Word;
  Stream: TStream);
begin
  FStatement.ParamsSetBlob(Index, Stream);
end;

procedure TJvUIBCustomDataSet.ParamsSetBlob(const Index: Word;
  var str: string);
begin
  FStatement.ParamsSetBlob(Index, str);
end;

procedure TJvUIBCustomDataSet.ParamsSetBlob(const Index: Word;
  Buffer: Pointer; Size: Word);
begin
  FStatement.ParamsSetBlob(Index, Buffer, Size);
end;

procedure TJvUIBCustomDataSet.ReadBlob(const name: string;
  Stream: TStream);
begin
  FStatement.ReadBlob(name, Stream);
end;

procedure TJvUIBCustomDataSet.ReadBlob(const name: string;
  var str: string);
begin
  FStatement.ReadBlob(name, str);
end;

procedure TJvUIBCustomDataSet.ReadBlob(const name: string;
  var Value: Variant);
begin
  FStatement.ReadBlob(name, Value);
end;

procedure TJvUIBCustomDataSet.ReadBlob(const Index: Word; Stream: TStream);
begin
  FStatement.ReadBlob(Index, Stream);
end;

procedure TJvUIBCustomDataSet.ReadBlob(const Index: Word; var str: string);
begin
  FStatement.ReadBlob(Index, str);
end;

procedure TJvUIBCustomDataSet.ReadBlob(const Index: Word;
  var Value: Variant);
begin
  FStatement.ReadBlob(Index, Value);
end;

function TJvUIBCustomDataSet.GetParams: TSQLParams;
begin
  Result := FStatement.Params;
end;

function TJvUIBCustomDataSet.GetInternalFields: TSQLResult;
begin
  Result := FStatement.Fields;
end;

function TJvUIBCustomDataSet.GetBufferChunks: Cardinal;
begin
  Result := FStatement.BufferChunks;
end;

procedure TJvUIBCustomDataSet.SetBufferChunks(const Value: Cardinal);
begin
  FStatement.BufferChunks := Value;
end;

function TJvUIBCustomDataSet.GetRowsAffected: Cardinal;
begin
  Result := FStatement.RowsAffected;
end;

procedure TJvUIBCustomDataSet.InternalRefresh;
var RecCount: Integer;
begin
  if FStatement.Fields <> nil then
    RecCount := FStatement.Fields.RecordCount else
    RecCount := 0;
  FStatement.Open;
  While (RecCount > 1) and not FStatement.Eof do
  begin
    FStatement.Next;
    dec(RecCount);
  end;
end;

{$IFNDEF FPC}
procedure TJvUIBCustomDataSet.CreateFields;
var
  i : Integer;
  fd: TFieldDef;
begin
  inherited;
  for i := 0 to Fields.Count - 1 do
    with Fields.Fields[i] do
    begin
      fd := FieldDefList.Find(FieldName);
      if (fd <> nil) and (fd is TUIBFieldDef) then
        Origin := TUIBFieldDef(fd).Origin;
    end;
end;
{$ENDIF}

procedure TJvUIBCustomDataSet.BuildStoredProc(const StoredProc: string;
  forSelect: boolean);
begin
  Close;
  FStatement.BuildStoredProc(StoredProc, forSelect);
end;

{$IFNDEF FPC}
function TJvUIBCustomDataSet.GetFieldClass(
  FieldType: TFieldType): TFieldClass;
begin
  if FieldType = ftBCD then
    result := TUIBBCDField else
    result := inherited GetFieldClass(FieldType);
end;
{$ENDIF}

{$IFNDEF FPC}
function TJvUIBCustomDataSet.GetFieldData(Field: TField; Buffer: Pointer;
  NativeFormat: Boolean): Boolean;
begin
  if (Field.DataType = ftBCD) and not NativeFormat then
    Result := GetFieldData(Field, Buffer) else
    Result := inherited GetFieldData(Field, Buffer, NativeFormat);
end;
{$ENDIF}

{ TUIBBCDField }

{$IFNDEF FPC}
constructor TUIBBCDField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Size := 8;
end;

class procedure TUIBBCDField.CheckTypeSize(Value: Integer);
begin
end;

function TUIBBCDField.GetAsCurrency: Currency;
begin
  if not GetValue(Result) then
    Result := 0;
end;

function TUIBBCDField.GetAsString: string;
var
  curr: System.Currency;
begin
  if GetValue(curr) then
    Result := CurrToStr(curr) else
    Result := '';
end;

function TUIBBCDField.GetAsVariant: Variant;
var
  curr: System.Currency;
begin
  if GetValue(curr) then
    Result := curr else
    Result := Null;
end;

function TUIBBCDField.GetDataSize: Integer;
begin
  Result := 8;
end;
{$ENDIF}

end.
