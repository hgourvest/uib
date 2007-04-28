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
(* Contributor: Olivier Guilbaud <oguilb@free.fr>                               *)
(*                                                                              *)
(********************************************************************************)

unit jvuib;

{$I jvuib.inc}

(*------------------------------------------------------------------------------
  This is a cascading programming style.

..............oOo...............................oOo.........oOo.................
 States        |    Operations                   |  Commands | Components
..............oOo...............................oOo.........oOo.................
 qsDataBase    |  BeginDataBase(L)               |           | TUIBDataBase
--------------------------------------------------------------------------------
 qsTransaction |  |-> BeginTransaction           |           | TUIBTransaction
--------------------------------------------------------------------------------
 qsExecImme    |      |-> BeginExecImme .........|.[ExecSQL] | TUIBQuery
 qsStatement   |      |-> BeginStatement         |           |
 qsPrepare     |      |   |-> BeginPrepare       |           |
 qsExecute     |      |   |   |-> BeginExecute   |.[Execute] |
               |      |   |   |   |-> Next ......|.[Open]    |
               |      |   |   |   |   |          |           |
               | R <- E   E   E   E   E          | [Fields]  |
               |          |   |   |   |          |           |
 qsExecute     |          |   |   |<- EndExecute |.[Close]   |
 qsPrepare     |          |   |<- EndPrepare     |           |
 qsStatement   |          |<- EndStatement       |           |
 -------------------------------------------------------------------------------
 qsTransaction |          EndTransaction         |           | TUIBTransaction
..............oOo...............................oOo.........oOo.................
 LEGEND
   E  = Except
   R  = Raise
   -> = Call
------------------------------------------------------------------------------*)

interface
uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  SyncObjs, Classes, Contnrs, SysUtils, jvuiblib, jvuibase,
  jvuibsqlparser, jvuibconst;

type

 {Oo.........................................................................oO
                              TJvUIBComponent

      Synchronise Databases, Transactions and Queries.

    TUIBLibrary    | TUIBDatabase     | TUIBTransaction  | TUIBQuery
   ==========================================================================
    Lock <---------|------------------|------------------|-----------------o
                   | Lock <-----------|------------------|---------------o
                   |                  | Lock <-----------|-------------o
                   |                  |                  | Lock   <--o
                   |                  |                  | UnLock <--o
                   |                  | UnLock <---------|-------------o
                   | UnLock <---------|------------------|---------------o
    UnLock <-------|------------------|------------------|-----------------o

      Note: With Interbase 7, no need to synchronise anything but removing
      Synchronisation you have exactly the same performance than IB6.01 with
      Synchronisation on a single CPU !

  Oo.........................................................................oO}

{ All UIB components inherith from this class to encapsulate Critical Sections.
  Critical Sections make UIB THread Safe. }
{$IFDEF UIB_NO_COMPONENT}
  TJvUIBComponent = class(TObject)
{$ELSE}
  TJvUIBComponent = class(TComponent)
{$ENDIF}
  private
  {$IFDEF UIBTHREADSAFE}
    FCriticalsection: TCriticalSection;
  {$ENDIF}
  public
    { @exclude }
    constructor Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent); override{$ELSE}; virtual{$ENDIF};
    { @exclude }
    destructor Destroy; override;
  {$IFDEF UIBTHREADSAFE}
    { Lock the critical Section. }
    procedure Lock; virtual;
    { UnLock the critical Section. }
    procedure UnLock; virtual;
  {$ENDIF}
  end;

  // Forward declarations
  TJvUIBTransaction = class;
  TJvUIBQuery = class;
  TJvUIBStatement = class;
  TJvUIBDataBase = class;
  TJvUIBEvents = class;

  { The list of MetaData Objects returned by TJvUIBDatabase.GetMetadata function. }
  TMetaDataOptions = class(TPersistent)
  private
    FObjects: TOIDDatabases;
    FTables: TOIDTables;
    FViews: TOIDViews;
    FProcedures: TOIDProcedures;
    FUDFs: TOIDUDFs;
    FRoles: TOIDRoles;
    FSysInfos: boolean;
  public
   { @exclude }
    constructor Create;
  published
    { Metadata objects (Procedure, Generator, Exception, UDF, Role). }
    property Objects: TOIDDatabases read FObjects write FObjects default ALLObjects;
    { Table properties (TableField, Primary, Foreign, TableTrigger, Unique, Index, Check)}
    property Tables: TOIDTables read FTables write FTables default ALLTables;
    { View properties (Fields & Triggers)}
    property Views: TOIDViews read FViews write FViews default AllViews;
    { Procedure properties (input & output parametters). }
    property Procedures: TOIDProcedures read FProcedures write FProcedures default AllProcedures;
    { UDFs properties (Fields). }
    property UDFs: TOIDUDFs read FUDFs write FUDFs default AllUDFs;
    { Roles properties (Grants). }
    property Roles: TOIDRoles read FRoles write FRoles default AllRoles;
    { Include System tables, triggers and domains. }
    property SysInfos: boolean read FSysInfos write FSysInfos default False;
  end;

  TShutdownOption = (sdCache, sdAttachment, sdTransaction, sdForce);
  TShutdownOptions = set of TShutdownOption;

  PTableOperation = ^TTableOperation;
  TTableOperation = packed record
    TableId: Word;
    Count: Integer;
  end;

  TOnInfoTableOpCount = procedure(Sender: TObject; TableOp: PTableOperation) of object;
  TOnInfoIntegerCount = procedure(Sender: TObject; Value: Integer) of object;
  TOnInfoStringCount = procedure(Sender: TObject; Value: string) of object;

  TJvUIBDataBase = class(TJvUIBComponent)
  private
    FLibrary: TUIBLibrary;
    FLiBraryName: TFileName;
    FDbHandle: IscDbHandle;
    FHandleShared: boolean;
    FParams: TStrings;
    FDatabaseName: TFileName;
    FAfterConnect: TNotifyEvent;
    FAfterDisconnect: TNotifyEvent;
    FBeforeConnect: TNotifyEvent;
    FBeforeDisconnect: TNotifyEvent;
    FTransactions: TList;
    FOnConnectionLost: TNotifyEvent;
    FExceptions: TList;
    FMetadata: TObject;
    FEventNotifiers: TList;

    FMetaDataOptions: TMetaDataOptions;
    FOnInfoReadSeqCount: TOnInfoTableOpCount;
    FOnInfoReadIdxCount: TOnInfoTableOpCount;
    FOnInfoUpdateCount: TOnInfoTableOpCount;
    FOnInfoInsertCount: TOnInfoTableOpCount;
    FOnInfoDeleteCount: TOnInfoTableOpCount;
    FOnInfoBackoutCount: TOnInfoTableOpCount;
    FOnInfoPurgeCount: TOnInfoTableOpCount;
    FOnInfoExpungeCount: TOnInfoTableOpCount;
    FOnInfoActiveTransactions: TOnInfoIntegerCount;
    FOnInfoLimbo: TOnInfoIntegerCount;
    FOnInfoUserNames: TOnInfoStringCount;

    function ReadParamString(Param: String; Default: String = ''): String;
    procedure WriteParamString(Param: String; Value: String);
    function ReadParamInteger(Param: String; Default: Integer): Integer;
    procedure WriteParamInteger(Param: String; Value: Integer);
    procedure SetParams(const Value: TStrings);
    procedure SetDatabaseName(const Value: TFileName);
    procedure SetConnected(const Value: boolean);
    function GetConnected: boolean;
    procedure SetSQLDialect(const Value: Integer);
    function GetSQLDialect: Integer;
    function GetCharacterSet: TCharacterSet;
    procedure SetCharacterSet(const Value: TCharacterSet);
    function GetPassWord: string;
    function GetUserName: string;
    procedure SetPassWord(const Value: string);
    procedure SetUserName(const Value: string);
    procedure AddTransaction(Transaction: TJvUIBTransaction);
    procedure RemoveTransaction(Transaction: TJvUIBTransaction);
    procedure ClearTransactions;
    procedure CloseTransactions;
    procedure SetDbHandle(const Value: IscDbHandle);
    procedure SetLibraryName(const Lib: TFileName);
    function GetTransactions(const Index: Cardinal): TJvUIBTransaction;
    function GetTransactionsCount: Cardinal;
    function GetSegmentSize: Word;
    procedure SetSegmentSize(const Value: Word);
    function GetShutdown: TShutdownOptions;
    procedure SetShutdown(const Value: TShutdownOptions);

    procedure UnRegisterEvents;
    procedure RegisterEvents;
    procedure RemoveEventNotifier(Event: TJvUIBEvents);
    procedure AddEventNotifier(Event: TJvUIBEvents);

    function GetInfoIntValue(const item: Integer): integer;
{$IFDEF FB20_UP}
    function GetInfoDateTimeValue(const item: Integer): TDateTime;
{$ENDIF}
    function GetInfoBooleanValue(const item: Integer): boolean;
    function GetInfoStringValue(const item: integer): string;
    function GetInfoOperationsCount(const item: Integer): Integer;
    function GetInfoIntCount(const item: Integer): Integer;
    function GetInfoStringCount(const item: Integer): Integer;
    function GetInfoDbId(const Index: Integer): string;
    function GetRole: string;
    procedure SetRole(const Value: string);
    procedure ClearEvents;
  protected
    procedure DoOnConnectionLost(Lib: TUIBLibrary); virtual;
    procedure DoOnGetDBExceptionClass(Number: Integer; out Excep: EUIBExceptionClass); virtual;
  public
    { Constructor method. }
    constructor Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent){$ENDIF}; override;
    { Destructor method. }
    destructor Destroy; override;
    { Execute a SQL statement without the need to have the database connected,
      it is usefull to create a database by SQL. }
    procedure ExecuteImmediate(const Statement: string);
    { Remove all Interbase Exception class registered using 'RegistedException'. }
    procedure ClearExceptions;
    { Associate an Interbase Exception with a Delphi exception, ID is the Exception Identifier number. }
    procedure RegisterException(Excpt: EUIBExceptionClass; ID: Integer); overload;
    { Associate an Interbase Exception with a Delphi exception, Name is the Interbase Exception name. }
    function RegisterException(Excpt: EUIBExceptionClass; const Name: string): Integer; overload;
    { Remove the Registered Exception number. }
    procedure UnRegisterException(Number: Integer);
    { Remove the Registered Exception class. }
    procedure UnRegisterExceptions(Excpt: EUIBExceptionClass);
    { Create a database with a default page size of 2048. }
    procedure CreateDatabase(PageSize: Integer = 2048);
    { Return a TMetaDatabase class corresponding to the current connection. }
    function GetMetadata(Refresh: boolean = False): TObject;

    { Activate all Triggers }
    procedure ActiveAllTriggers;
    { Deactivate all Triggers }
    procedure DeactiveAllTriggers;
    { Recompute selectivity of all indices }
    procedure RecomputeSelectivityIndices;
    { Recompile all procedures, usefull when indices are modified }
    procedure RecompileAllProcedures;
    { Recompile all triggers, usefull when indices are modified }
    procedure RecompileAllTriggers;

    { The DbHandle can be used to share the current connection with other Interbase components like IBX. }
    property DbHandle: IscDbHandle read FDbHandle write SetDbHandle;
    { Determine if the DbHandle is initialized by another component. }
    property IsHandleShared : boolean read FHandleShared;
    { List all transactions connected to the database component. }
    property Transactions[const Index: Cardinal]: TJvUIBTransaction read GetTransactions;
    { Number of connected transactions. }
    property TransactionsCount: Cardinal read GetTransactionsCount;
    { Can be used to access the low level API. }
    property Lib: TUIBLibrary read FLibrary;

    { Number of page reads. }
    property InfoReads: Integer index isc_info_reads read GetInfoIntValue;
    { Number of page writes. }
    property InfoWrites: Integer index isc_info_writes read GetInfoIntValue;
    { Number of reads from the memory buffer cache. }
    property InfoFetches: Integer index isc_info_fetches read GetInfoIntValue;
    { Number of writes to the memory buffer cache. }
    property InfoMarks: Integer index isc_info_marks read GetInfoIntValue;
    { Number of bytes per page of the attached database, use with
      "InfoAllocation" property to determine the size of the database. }
    property InfoPageSize: Integer index isc_info_page_size read GetInfoIntValue;
    { Number of memory buffers currently allocated. }
    property InfoNumBuffers: Integer index isc_info_num_buffers read GetInfoIntValue;
    { Page buffers from header page }
    property InfoSetPageBuffers: Integer index isc_info_set_page_buffers read GetInfoIntValue;
    { Amount of server memory (in bytes) currently in use. }
    property InfoCurrentMemory: Integer index isc_info_current_memory read GetInfoIntValue;
    { Maximum amount of memory (in bytes) used at one time since the first
      process attached to the database. }
    property InfoMaxMemory: Integer index isc_info_max_memory read GetInfoIntValue;
    { Attachment ID. }
    property InfoAttachmentId: Integer index isc_info_attachment_id read GetInfoIntValue;
    { On-disk structure (ODS) major version number. }
    property InfoOdsVersion: Integer index isc_info_ods_version read GetInfoIntValue;
    { On-disk structure (ODS) minor version number. }
    property InfoOdsMinorVersion: Integer index isc_info_ods_minor_version read GetInfoIntValue;
    { Number of database pages allocated. }
    property InfoAllocation: Integer index isc_info_allocation read GetInfoIntValue;
    { Number of transactions that are committed between "sweeps" to remove
      database record versions that are no longer needed. }
    property InfoSweepInterval: Integer index isc_info_sweep_interval read GetInfoIntValue;
    { false indicates space is reserved on each database page for holding
        backup versions of modified records [Default]
    { true indicates no space is reserved for such records. }
    property InfoNoReserve: boolean index isc_info_no_reserve read GetInfoBooleanValue;
    { Specify the mode in which database writes are performed
      (false for asynchronous, true for synchronous). }
    property InfoForcedWrites: boolean index isc_info_forced_writes read GetInfoBooleanValue;
    { Number of database page errors. }
    property InfoPageErrors: Integer index isc_info_page_errors read GetInfoIntValue;
    { Number of Blob page errors. }
    property InfoBPageErrors: Integer index isc_info_bpage_errors read GetInfoIntValue;
    { Number of record level errors. }
    property InfoRecordErrors: Integer index isc_info_record_errors read GetInfoIntValue;
    { Number of data page errors. }
    property InfoDPageErrors: Integer index isc_info_dpage_errors read GetInfoIntValue;
    { Number of index page errors. }
    property InfoIPageErrors: Integer index isc_info_ipage_errors read GetInfoIntValue;
    { Number of pointer page errors. }
    property InfoPPageErrors: Integer index isc_info_ppage_errors read GetInfoIntValue;
    { Number of transaction page errors. }
    property InfoTPageErrors: Integer index isc_info_tpage_errors read GetInfoIntValue;
    { Database SQL Dialect (1,2,3). }
    property InfoDbSqlDialect: Integer index isc_info_db_sql_dialect read GetInfoIntValue;
    { Is the database read only ? }
    property InfoDbReadOnly: boolean index isc_info_db_read_only read GetInfoBooleanValue;
    { Database size in pages. }
    property InfoDbSizeInPages: Integer index isc_info_db_size_in_pages read GetInfoIntValue;
    { Database file name. }
    property InfoDbFileName: string index 1 read GetInfoDbId;
    { Database site name. }
    property InfoDbSiteName: string index 2 read GetInfoDbId;
    { Database implementation number, cf. isc_info_db_impl_XXX. }
    property InfoImplementation: Integer index isc_info_implementation read GetInfoIntValue;
    { Database version (level) number. }
    property InfoBaseLevel: Integer index isc_info_base_level read GetInfoIntValue;
    { Version identification string of the database implementation. }
    property InfoVersion: string index isc_info_isc_version read GetInfoStringValue;
    { Number of sequential sequential table scans (row reads) done on each
      table since the database was last attached. }
    property InfoReadSeqCount: Integer index isc_info_read_seq_count read GetInfoOperationsCount;
    { Number of reads done via an index since the database was last attached. }
    property InfoReadIdxCount: Integer index isc_info_read_idx_count read GetInfoOperationsCount;
    { Number of database updates since the database was last attached. }
    property InfoUpdateCount: Integer index isc_info_update_count read GetInfoOperationsCount;
    { Number of inserts into the database since the database was last attached. }
    property InfoInsertCount: Integer index isc_info_insert_count read GetInfoOperationsCount;
    { Number of database deletes since the database was last attached. }
    property InfoDeleteCount: Integer index isc_info_delete_count read GetInfoOperationsCount;
    { Number of removals of a version of a record. }
    property InfoBackoutCount: Integer index isc_info_backout_count read GetInfoOperationsCount;
    { Number of removals of old versions of fully mature records (records that
      are committed, so that older ancestor versions are no longer needed). }
    property InfoPurgeCount: Integer index isc_info_purge_count read GetInfoOperationsCount;
    { Number of removals of a record and all of its ancestors, for records
      whose deletions have been committed. }
    property InfoExpungeCount: Integer index isc_info_expunge_count read GetInfoOperationsCount;
    property InfoLimbo: integer index isc_info_limbo read GetInfoIntCount;
    { Number of users currently attached to the database.
      Use this property with the "OnInfoUserNames" event to retrieve the user names. }
    property InfoUserNames: Integer index isc_info_user_names read GetInfoStringCount;
  {$IFDEF FB102ORYF867}
    { Cached "oldest interesting" transaction. }
    property InfoOldestTransaction: Integer index isc_info_oldest_transaction read GetInfoIntValue;
    { Cached "oldest active" transaction. }
    property InfoOldestActive: Integer index isc_info_oldest_active read GetInfoIntValue;
    { Cached "oldest snapshot" of all active transactions. }
    property InfoOldestSnapshot: Integer index isc_info_oldest_snapshot read GetInfoIntValue;
    { Next transaction id. }
    property InfoNextTransaction: Integer index isc_info_next_transaction read GetInfoIntValue;
    { Database provider. }
    property InfoDbProvider: Integer index isc_info_db_provider read GetInfoIntValue;
    { Database Class. }
    property InfoDbClass: Integer index isc_info_db_class read GetInfoIntValue;
    { User's charset specified in parameters. }
    property InfoAttCharset: Integer index frb_info_att_charset read GetInfoIntValue;
    { Firebird version. }
    property InfoFirebirdVersion: string index isc_info_firebird_version read GetInfoStringValue;
    { Return number of active transactions. }
    property InfoActiveTransactions: Integer index isc_info_active_transactions read GetInfoIntCount;
  {$ENDIF}
  {$IFDEF FB20_UP}
    property InfoActiveTransactionsCount: Integer index isc_info_active_tran_count read GetInfoIntValue;
    property InfoCreationDate: TDateTime index isc_info_creation_date read GetInfoDateTimeValue;
  {$ENDIF}
  {$IFDEF IB7_UP}
    property InfoDbReads: Integer index isc_info_db_reads read GetInfoIntValue;
    property InfoDbWrites: Integer index isc_info_db_writes read GetInfoIntValue;
    property InfoDbFetches: Integer index isc_info_db_fetches read GetInfoIntValue;
    property InfoDbMarks: Integer index isc_info_db_marks read GetInfoIntValue;
    property InfoDbGroupCommit: boolean index isc_info_db_group_commit read GetInfoBooleanValue;
  {$ENDIF}
  {$IFDEF IB71_UP}
    property InfoAttCharset: Integer index isc_info_att_charset read GetInfoIntValue;
    property InfoSvrMinVer: Integer index isc_info_svr_min_ver read GetInfoIntValue;
  {$ENDIF}
  published
    { DataBase connection parametters. }
    property Params: TStrings read FParams write SetParams;
    { Database file name. }
    property DatabaseName: TFileName read FDatabaseName write SetDatabaseName;
    { The SQL dialect gives access to DSQL features, set the dialect to 1 or 3.
      Dialect 3 gives access to features introduced in InterBase 6. }
    property SQLDialect: Integer read GetSQLDialect write SetSQLDialect default 3;
    { Character set to be utilized. }
    property CharacterSet: TCharacterSet read GetCharacterSet write SetCharacterSet default csNONE;
    { Set the user name. Default = SYSDBA. }
    property UserName: string read GetUserName write SetUserName;
    { Set the Password. Default = masterkey. }
    property PassWord: string read GetPassWord write SetPassWord;
    { Define wich library the connection use.}
    property LibraryName: TFileName read FLiBraryName write SetLibraryName;
    { This event occur after the component is connected to database. }
    property AfterConnect: TNotifyEvent read FAfterConnect write FAfterConnect;
    { This event occur before the component is connected to database. }
    property BeforeConnect: TNotifyEvent read FBeforeConnect write FBeforeConnect;
    { This event occur after the component is disconnected from database. }
    property AfterDisconnect: TNotifyEvent read FAfterDisconnect write FAfterDisconnect;
    { This event occur before the component is disconnected from database. }
    property BeforeDisconnect: TNotifyEvent read FBeforeDisconnect write FBeforeDisconnect;
    { When connection lost, Database, Transactions and Queries are automatically closed.
      Only one exception is raised to terminate the current stack and this event occur. }
    property OnConnectionLost: TNotifyEvent read FOnConnectionLost write FOnConnectionLost;
    { The blob segment size used to write in database, this parametter depend on hard drive. }
    property SegmentSize: Word read GetSegmentSize write SetSegmentSize default 16*1024;
    { The list of MetaData Objects returned by GetMetadata. }
    property MetaDataOptions: TMetaDataOptions read FMetaDataOptions;
    { Helpful property to shudown other database connections. }
    property Shutdown: TShutdownOptions read GetShutdown write SetShutdown default [];
    { Connect or disconnect a database. }
    property Connected: boolean read GetConnected write SetConnected default False;
    property Role: string read GetRole write SetRole;

    property OnInfoReadSeqCount: TOnInfoTableOpCount read FOnInfoReadSeqCount write FOnInfoReadSeqCount;
    property OnInfoReadIdxCount: TOnInfoTableOpCount read FOnInfoReadIdxCount write FOnInfoReadIdxCount;
    property OnInfoUpdateCount: TOnInfoTableOpCount read FOnInfoUpdateCount write FOnInfoUpdateCount;
    property OnInfoInsertCount: TOnInfoTableOpCount read FOnInfoInsertCount write FOnInfoInsertCount;
    property OnInfoDeleteCount: TOnInfoTableOpCount read FOnInfoDeleteCount write FOnInfoDeleteCount;
    property OnInfoBackoutCount: TOnInfoTableOpCount read FOnInfoBackoutCount write FOnInfoBackoutCount;
    property OnInfoPurgeCount: TOnInfoTableOpCount read FOnInfoPurgeCount write FOnInfoPurgeCount;
    property OnInfoExpungeCount: TOnInfoTableOpCount read FOnInfoExpungeCount write FOnInfoExpungeCount;
    property OnInfoActiveTransactions: TOnInfoIntegerCount read FOnInfoActiveTransactions write FOnInfoActiveTransactions;
    property OnInfoLimbo: TOnInfoIntegerCount read FOnInfoLimbo write FOnInfoLimbo;
    property OnInfoUserNames: TOnInfoStringCount read FOnInfoUserNames write FOnInfoUserNames;
  end;

  { Describe how a transaction is closed. }
  TEndTransMode = (
    etmDefault,          // Use default Transaction Action
    etmStayIn,           // keep transaction without commit or rollback
    etmCommit,           // commit transaction
    etmCommitRetaining,  // commit transaction and keep transaction handle
    etmRollback,         // rollback transaction
    etmRollbackRetaining // rollback transaction and keep transaction handle
  );

  { Indicate the Query state.
    order is important ! }
  TQueryState = (
    qsDataBase,    // have a database handle
    qsTransaction, // have a transaction handle
    qsExecImme,    // Query executed immediately without the need of statement handle
    qsStatement,   // have a statement handle
    qsPrepare,     // Query prepared
    qsExecute      // Query executed 
  );

  {Oo.......................................................................oO
                                  TUIBTransaction
   Oo.......................................................................oO}

  // Transaction parameters
  TTransParam = (
    { prevents a transaction from accessing tables if they are written to by
      other transactions.}
    tpConsistency,
    { allows concurrent transactions to read and write shared data. }
    tpConcurrency,
    { Concurrent, shared access of a specified table among all transactions. }
    tpShared,
    { Concurrent, restricted access of a specified table. }
    tpProtected,
    tpExclusive,
    { Specifies that the transaction is to wait until the conflicting resource
      is released before retrying an operation [Default]. }
    tpWait,
    { Specifies that the transaction is not to wait for the resource to be
      released, but instead, should return an update conflict error immediately. }
    tpNowait,          
    { Read-only access mode that allows a transaction only to select data from tables. }
    tpRead,
    { Read-write access mode of that allows a transaction to select, insert,
      update, and delete table data [Default]. }
    tpWrite,
    { Read-only access of a specified table. Use in conjunction with tpShared,
      tpProtected, and tpExclusive to establish the lock option. }
    tpLockRead,
    { Read-write access of a specified table. Use in conjunction with tpShared,
      tpProtected, and tpExclusive to establish the lock option [Default]. }
    tpLockWrite,
    tpVerbTime,
    tpCommitTime,
    tpIgnoreLimbo,
    { Unlike a concurrency transaction, a read committed transaction sees changes
      made and committed by transactions that were active after this transaction started. }
    tpReadCommitted,
    tpAutoCommit,
    { Enables an tpReadCommitted transaction to read only the latest committed
      version of a record. }
    tpRecVersion,
    tpNoRecVersion,
    tpRestartRequests,
    tpNoAutoUndo
  {$IFDEF FB20_UP}
    ,tpLockTimeout
  {$ENDIF}
  );

  { Set of transaction parameters. }
  TTransParams = set of TTransParam;
  {This evenet occur before to end the transaction, you can change the ETM parametter.}
  TOnEndTransaction = procedure(Sender: TObject; var Mode: TEndTransMode) of object;

  { The Transaction component. }
  TJvUIBTransaction = class(TJvUIBComponent)
  private
    FDataBase: TJvUIBDataBase;
    FDataBases: TList;
    FTrHandle: IscTrHandle;
    FSQLComponent: TList;
    FStatements: Integer;
    FOptions   : TTransParams;
    FLockRead  : string;
    FLockWrite : string;
    FSQLDialect: Integer;
    FOnStartTransaction: TNotifyEvent;
    FOnEndTransaction: TOnEndTransaction;
    FAutoRetain: boolean;
    FAutoStart: boolean;
    FAutoStop: boolean;
    FDefaultAction: TEndTransMode;
  {$IFDEF FB20_UP}
    FLockTimeout: Word;
  {$ENDIF}
    function GetInTransaction: boolean;
    function TPB: string;
    function GetOptions: TTransParams;
    procedure SetOptions(const Value: TTransParams);
    function GetLockRead: string;
    function GetLockWrite: string;
    procedure SetLockRead(const Value: string);
    procedure SetLockWrite(const Value: string);
    function GetDataBase: TJvUIBDataBase;
    procedure BeginDataBase;
    procedure BeginTransaction(Auto: boolean = True);
    function EndTransaction(ETM: TEndTransMode; From: TJvUIBStatement;
      Auto: boolean): boolean;
    procedure AddSQLComponent(Component: TJvUIBStatement);
    procedure RemoveSQLComponent(Component: TJvUIBStatement);
    procedure ClearSQLComponents;
    procedure Close(const Mode: TEndTransMode; Auto: boolean);
    function GetStatements(const Index: Integer): TJvUIBStatement;
    function GetStatementsCount: Integer;
    procedure ClearDataBases;
    function GetDatabases(const Index: Integer): TJvUIBDataBase;
    function GetDatabasesCount: Integer;
    function GetAutoRetain: boolean;
    procedure SetAutoRetain(const Value: boolean);
    procedure SetDefaultAction(const Value: TEndTransMode);
  protected
  {$IFNDEF UIB_NO_COMPONENT}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  {$ENDIF}
    procedure SetDataBase(const ADatabase: TJvUIBDataBase); virtual;
  public
    { Constructor method. }
    constructor Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent){$ENDIF}; override;
    { Destructor method.}
    destructor Destroy; override;
{$IFDEF UIBTHREADSAFE}
    { cf TjvUIBComponent.Lock }
    procedure Lock; override;
    { cf TjvUIBComponent.UnLock }
    procedure UnLock; override;
{$ENDIF}
    { Add a database to the transaction. }
    procedure AddDataBase(ADataBase: TJvUIBDataBase);
    { Remove a database from a transaction. }
    procedure RemoveDatabase(ADataBase: TJvUIBDataBase); overload;
    { Remove a database from a transaction. }
    procedure RemoveDatabase(Index: Integer); overload;
    {Start Transaction.}
    Procedure StartTransaction;
    {Commit transaction.}
    procedure Commit;
    {Commit transaction but keep transaction handle.}
    procedure CommitRetaining;
    {Rollback transaction.}
    procedure RollBack;
    {Rollback transaction but keep transaction handle.}
    procedure RollBackRetaining;
    procedure ExecuteImmediate(const sql: string);
{$IFDEF IB71_UP}
    { Interbase 7.1 spceficic, Release a savepoint.
      On Firebird 1.5 this must be call by SQL.}
    procedure SavepointRelease(const Name: string);
    { Interbase 7.1 spceficic, RollBack a savepoint.
      On Firebird 1.5 this must be call by SQL.}
    procedure SavepointRollback(const Name: string; Option: Word = 0);
    { Interbase 7.1 spceficic, Start a savepoint.
      On Firebird 1.5 this must be call by SQL.}
    procedure SavepointStart(const Name: string);
{$ENDIF}
    {Indicate if the transaction is active.}
    property InTransaction: boolean read GetInTransaction;
    { Transaction handle.}
    property TrHandle: IscTrHandle read FTrHandle;
    { Queries connected to this transaction.}
    property Statements[const Index: Integer]: TJvUIBStatement read GetStatements;
    { Number of Queries connected to this transaction.}
    property StatementsCount: Integer read GetStatementsCount;
    { Get all databases attached to the transaction. }
    property Databases[const Index: Integer]: TJvUIBDataBase read GetDatabases;
    { How many databases attached to the transaction. }
    property DatabasesCount: Integer read GetDatabasesCount;
  published
    {Database connection.}
    property DataBase  : TJvUIBDataBase read GetDataBase write SetDataBase;
    {Transaction parametters.}
    property Options   : TTransParams   read GetOptions    write SetOptions default [tpConcurrency,tpWait,tpWrite];
    {List of the tables to lock for read, tpLockRead option must set. ex: 'Table1;Table2'}
    property LockRead  : string         read GetLockRead   write SetLockRead;
    {List of the tables to lock for write, tpLockWrite option must set. ex: 'Table1;Table2'}
    property LockWrite : string         read GetLockWrite  write SetLockWrite;
    {This event occur after a transaction is started.}
    property OnStartTransaction: TNotifyEvent read FOnStartTransaction write FOnStartTransaction;
    {This evenet occur before to end the transaction, you can change the ETM parametter.}
    property OnEndTransaction: TOnEndTransaction read FOnEndTransaction write FOnEndTransaction;
    {If false, commit and rollback close all connected statements and finally close transaction.
     If True, commit and rollback are modified to commitretaining or rollbackretaining if at least one statement is open.}
    property AutoRetain: boolean read GetAutoRetain write SetAutoRetain default False;
    {If True, transaction automatically started when needed.
     if False you must explicitely call "starttransaction".}
    property AutoStart: boolean read FAutoStart write FAutoStart default True;
    {default = false, if True you need to close transaction explicitly.}
    property AutoStop: boolean read FAutoStop write FAutoStop default True;
    {Transaction default action if closed automaticaly, commit or rollback only.}
    property DefaultAction: TEndTransMode read FDefaultAction write SetDefaultAction default etmCommit;
  {$IFDEF FB20_UP}
    property LockTimeout: Word read FLockTimeout write FLockTimeout default 0;
  {$ENDIF}
  end;

  { Simple query component. }
  TJvUIBStatement = class(TJvUIBComponent)
  private
    FCurrentState: TQueryState;
    FTransaction: TJvUIBTransaction;
    FDataBase: TJvUIBDataBase;
    FStHandle: IscStmtHandle;
    FOnError: TEndTransMode;
    FCursorName: string;
    FSQLResult: TSQLResult;
    FCachedFetch: boolean;
    FFetchBlobs: boolean;
    FBufferChunks: Cardinal;
    FQuickScript: boolean;
    FSQL: TStrings;
    FParsedSQL: string;
    FParameter: TSQLParams;
    FParseParams: boolean;
    FOnClose: TNotifyEvent;
    FStatementType: TUIBStatementType;
    FUseCursor: boolean;
    function GetPlan: string;
    function GetStatementType: TUIBStatementType;
    procedure SetSQL(const Value: TStrings);
    procedure DoSQLChange(Sender: TObject);
    function GetFields: TSQLResult;
    function GetEof: boolean;
    function FindDataBase: TJvUIBDataBase;
    function GetRowsAffected: Cardinal;
    function GetBof: boolean;
  protected
    procedure SetTransaction(const Transaction: TJvUIBTransaction); virtual;
    procedure SetDataBase(ADataBase: TJvUIBDataBase);
  {$IFNDEF UIB_NO_COMPONENT}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  {$ENDIF}

    procedure BeginTransaction; virtual;
    procedure BeginStatement; virtual;
    procedure BeginPrepare; virtual;
    procedure BeginExecute; virtual;
    procedure BeginExecImme; virtual;

    procedure EndTransaction(const ETM: TEndTransMode; Auto: boolean); virtual;
    procedure EndStatement(const ETM: TEndTransMode; Auto: boolean); virtual;
    procedure EndPrepare(const ETM: TEndTransMode; Auto: boolean); virtual;
    procedure EndExecute(const ETM: TEndTransMode; Auto: boolean); virtual;
    procedure EndExecImme(const ETM: TEndTransMode; Auto: boolean); virtual;

    procedure InternalNext; virtual;
    procedure InternalPrior; virtual;
    procedure InternalClose(const Mode: TEndTransMode; Auto: boolean); virtual;

    function  ParamsClass: TSQLParamsClass; virtual;
    function  ResultClass: TSQLResultClass; virtual;

    procedure InternalGetBlobSize(sqlda: TSQLDA; const Index: Word; out Size: Cardinal);
    procedure InternalReadBlob(sqlda: TSQLDA; const Index: Word; Stream: TStream); overload;
    procedure InternalReadBlob(sqlda: TSQLDA; const Index: Word; var str: string); overload;
    procedure InternalReadBlob(sqlda: TSQLDA; const Index: Word; var Value: Variant); overload;
    procedure InternalReadBlob(sqlda: TSQLDA; const Index: Word; Buffer: Pointer); overload;

    property QuickScript: boolean read FQuickScript write FQuickScript  default False;

  public
    { Constructor method. }
    constructor Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent){$ENDIF}; override;
    { Destructor method. }
    destructor Destroy; override;
{$IFDEF UIBTHREADSAFE}
    { cf TJvUIBComponent.Lock }
    procedure Lock; override;
    { cf TJvUIBComponent.UnLock }
    procedure UnLock; override;
{$ENDIF}
    { Close the statement. You can commit or rollback the transaction when closing. }
    procedure Close(const Mode: TEndTransMode = etmStayIn); virtual;
    { Fetch all records returned by the query. }
    procedure CloseCursor;
    procedure FetchAll;
    { Open the query and fetch the first record if FetchFirst = true. }
    procedure Open(FetchFirst: boolean = True);
    { Prepare the query. }
    procedure Prepare;
    { Execute the query. }
    procedure Execute;
    { Execute the query or the script (QuickScript = true) immediately. }
    procedure ExecSQL;
    { Get the next record. }
    procedure Next;
    { Get the prior record. }
    procedure Prior;
    { Get the last record. }
    procedure Last;
    { Get the first record. }
    procedure First;
    { Read a the blob in a stream by index. }
    procedure ReadBlob(const Index: Word; Stream: TStream); overload;
    { Read a the blob in a string by index. }
    procedure ReadBlob(const Index: Word; var str: string); overload;
    { Read a the blob in a Variant by index. }
    procedure ReadBlob(const Index: Word; var Value: Variant); overload;
    { Read a the blob in a PREALLOCATED buffer by index. }
    procedure ReadBlob(const Index: Word; Buffer: Pointer); overload;
    { Read a the blob in a stream by name. }
    procedure ReadBlob(const name: string; Stream: TStream); overload;
    { Read a the blob in a string by name. }
    procedure ReadBlob(const name: string; var str: string); overload;
    { Read a the blob in a Variant by name. }
    procedure ReadBlob(const name: string; var Value: Variant); overload;
    { Read a the blob in a PREALLOCATED buffer by name. }
    procedure ReadBlob(const name: string; Buffer: Pointer); overload;

    { The the blob value of a parametter using a Stream. }
    procedure ParamsSetBlob(const Index: Word; Stream: TStream); overload;
    { The the blob value of a parametter using a string. }
    procedure ParamsSetBlob(const Index: Word; var str: string); overload;
    { The the blob value of a parametter using a Buffer. }
    procedure ParamsSetBlob(const Index: Word; Buffer: Pointer; Size: Cardinal); overload;

    { The the blob value of a parametter using a Stream. }
    procedure ParamsSetBlob(const Name: string; Stream: TStream); overload;
    { The the blob value of a parametter using a string. }
    procedure ParamsSetBlob(const Name: string; var str: string); overload;
    { The the blob value of a parametter using a Buffer. }
    procedure ParamsSetBlob(const Name: string; Buffer: Pointer; Size: Cardinal); overload;

    { Get the the blob size of the current record. }
    function FieldBlobSize(const Index: Word): Cardinal;
    { Get the blob size of the corresonding parametter. }
    function ParamBlobSize(const Index: Word): Cardinal;

    { The internal statement handle. }
    property StHandle: IscStmtHandle read FStHandle;
    { Use fields to read the current record. }
    property Fields: TSQLResult read GetFields;
    { use Params to set parametters, the param names are set dynamically
      parsing the SQL query, by default the param values are null string.
      The first time you set a parametter value, the field type is defined.  }
    property Params: TSQLParams read FParameter;
    { All UIB statements declare a unique cursor name, another query can use
      this cursor to modify the current cursor, this feature is for unidirectionnal
      statements !!.<br>
      ex: UPDATE proj_dept_budget SET projected_budget = :value WHERE CURRENT OF %s; }
    property CursorName: string read FCursorName;
    { Indicate the current state of the query. }
    property CurrentState: TQueryState read FCurrentState;
    { if true there isn't anymore record to fetch. }
    property Eof: boolean read GetEof;
    property Bof: boolean read GetBof;
    { @exclude }
    property ParseParams: boolean read FParseParams write FParseParams;
    { The plan used internally by interbase (the query must be prepared). }
    property Plan: string read GetPlan;
    { Get the current statement type (the query must be prepared). }
    property StatementType: TUIBStatementType read GetStatementType;
    { Return the number of rows affected by the query (stInsert, stUpdate or stDelete). }
    property RowsAffected: Cardinal read GetRowsAffected;
    property UseCursor: boolean read FUseCursor write FUseCursor default True;
  published
    { The sql query. }
    property SQL: TStrings read FSQL write SetSQL;
    { Transaction of the query. }
    property Transaction: TJvUIBTransaction read FTransaction write SetTransaction;
    { Connected database, in most cases you don't need to set this property, it is
      only needed if the transaction concern more than one database. }
    property DataBase: TJvUIBDataBase read FDataBase write SetDataBase;
    { If an error occur, this action is applied to the connected transaction. }
    property OnError: TEndTransMode read FOnError write FOnError default etmRollback;
    { If true all record are saved in memory. }
    property CachedFetch: boolean read FCachedFetch write FCachedFetch default True;
    { If true the blob data is fetched with the record. }
    property FetchBlobs: boolean read FFetchBlobs write FFetchBlobs default False;
    { Use BufferChunks to get or set the number of records for which the query
      allocates buffer space at any time. When the query’s buffer is full,
      trying to fetch an additional record causes the dataset to reallocate
      the buffer so that it has enough memory to hold an additional BufferChunks
      records. <br>
      Note: When CachedFetch is False, BufferChunks has no meaning. }
    property BufferChunks: Cardinal read FBufferChunks write FBufferChunks default 1000;
    { OnClose event. }
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  end;

  {Oo.......................................................................oO
                                  TUIBQuery
   Oo.......................................................................oO}
  { The query component. }
  TJvUIBQuery = class(TJvUIBStatement)
  public
    { Helper method to buid the SQL query needed to execute the stored procedure.
      Input data type found using this method. }
    procedure BuildStoredProc(const StoredProc: string; forSelect: boolean = true);
  published
    { If true you can use this component as a fast script component where each line is a query.
      You must use the ExecSQL method ! }
    property QuickScript;
  end;

  { Parsing event, occur on each query executed. }
  TOnParse = procedure(Sender: TObject; NodeType: TSQLStatement;
    const Statement: string) of object;

  { The script component. }
  TJvUIBScript = class(TJvUIBComponent)
  private
    FQuery: TJvUIBQuery;
    FScript: TStrings;
    FAutoDDL: boolean;
    FOnParse: TOnParse;
    FOnComment: TOnComment;
    procedure SetTransaction(const Value: TJvUIBTransaction);
    function GetTransaction: TJvUIBTransaction;
    procedure SetScript(const Value: TStrings);
  public
    constructor Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent){$ENDIF}; override;
    destructor Destroy; override;
    procedure ExecuteScript;
  published
    property Transaction: TJvUIBTransaction read GetTransaction write SetTransaction;
    property Script: TStrings read FScript write SetScript;
    property AutoDDL: boolean read FAutoDDL write FAutoDDL default True;
    property OnParse: TOnParse read FOnParse write FOnParse;
    property OnComment: TOnComment read FOnComment write FOnComment;
  end;

  TUIBProtocol = (
    proLocalHost,
    proTCPIP,
    proNetBEUI
  );

  TJvUIBService = class(TJvUIBComponent)
  private
    FLiBraryName: string;
    FUserName: string;
    FPassWord: string;
    FHost    : string;
    FProtocol: TUIBProtocol;
    procedure SetLibraryName(const Lib: String);
  protected
    FLibrary: TUIBLibrary;
    FHandle  : IscSvcHandle;
    procedure BeginService; virtual;
    procedure EndService; virtual;
    function CreateParam(code: char; const Value: string): string; overload;
    function CreateParam(code: char; Value: Integer): string; overload;
  public
    constructor Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent){$ENDIF}; override;
    destructor Destroy; override;
  published
    property UserName: string read FUserName write FUserName;
    property PassWord: string read FPassWord write FPassWord;
    property Host: string read FHost write FHost;
    property Protocol: TUIBProtocol read FProtocol write FProtocol default proLocalHost;
    { Define wich library the connection use.}
    property LibraryName: string read FLiBraryName write SetLibraryName;
  end;

  TVerboseEvent = procedure(Sender: TObject; Message: string) of object;

  TJvUIBBackupRestore = class(TJvUIBService)
  private
    FBackupFiles: TStrings;
    FDatabase: TFileName;
    FOnVerbose: TVerboseEvent;
    FVerbose: boolean;
    procedure SetBackupFiles(const Value: TStrings);
    function CreateStartSPB: string; virtual; abstract;
  public
    constructor Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent){$ENDIF}; override;
    destructor Destroy; override;
    procedure Run;
  published
    property BackupFiles: TStrings read FBackupFiles write SetBackupFiles;
    property Database: TFileName read FDatabase write FDatabase;
    property OnVerbose: TVerboseEvent read FOnVerbose write FOnVerbose;
    property Verbose: boolean read FVerbose write FVerbose default false;
  end;

  TBackupOption = (boIgnoreChecksums, boIgnoreLimbo, boMetadataOnly,
    boNoGarbageCollection, boOldMetadataDesc, boNonTransportable,
    boConvertExtTables, boExpand);
  TBackupOptions = set of TBackupOption;

  TJvUIBBackup = class(TJvUIBBackupRestore)
  private
    FOptions: TBackupOptions;
    function CreateStartSPB: string; override;
  published
    property Options: TBackupOptions read FOptions write FOptions default [];
  end;

  TRestoreOption = (roDeactivateIndexes, roNoShadow, roNoValidityCheck,
    roOneRelationAtATime, roReplace, roCreateNewDB, roUseAllSpace
    {$IFDEF IB71_UP},roValidate{$ENDIF});

  TRestoreOptions = set of TRestoreOption;

  TJvUIBRestore = class(TJvUIBBackupRestore)
  private
    FOptions: TRestoreOptions;
    FPageSize: Cardinal;
    function CreateStartSPB: string; override;
  public
    constructor Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent){$ENDIF}; override;
  published
    property Options: TRestoreOptions read FOptions write FOptions default [roCreateNewDB];
    property PageSize: Cardinal read FPageSize write FPageSize default 0;
  end;


  TSecurityAction = (saAddUser, saDeleteUser, saModifyUser, saDisplayUser, saDisplayUsers);
  TSecurityParam = (spRole, spUser, spPass, spFirstName, spMiddleName, spLastName, spUserID, spGroupID);
  TSecurityParams = set of TSecurityParam;

  TUserInfo = class(TObject)
  public
    UserName: string;
    FirstName: string;
    MiddleName: string;
    LastName: string;
    GroupID: Integer;
    UserID: Integer;
  end;

  TJvUIBSecurity = class(TJvUIBService)
  private
    FIntegerParams: array[ord(spUserID)..ord(spGroupID)] of Integer;
    FStringParams: array[ord(spRole)..ord(spLastName)] of string;
    FModifiedParams: TSecurityParams;
    FUserInfos: TObjectList;
    procedure ClearParams;
    function GetIntegerParam(aParam: Integer): Integer;
    function GetStringParam(aParam: Integer): string;
    function GetUserInfo(aIndex: Integer): TUserInfo;
    function GetUserInfoCount: Integer;
    procedure SetIntegerParam(aParam: Integer; const aValue: Integer);
    procedure SetStringParam(aParam: Integer; const aValue: string);
    procedure RunAction(aAction: TSecurityAction);
  public
    constructor Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent){$ENDIF}; override;
    destructor Destroy; override;
    { tell the server to add the user specified to the security database }
    procedure AddUser;
    { tell the sever to remove the user specified from the security database }
    procedure DeleteUser;
    { tell the server to make modifications to the record in the security database specified by the user }
    procedure ModifyUser;
    { retrieves user information for the specified user }
    procedure DisplayUser;
    { retrieves user information for all users }
    procedure DisplayUsers;
    { information for user(s) retrieved by DisplayUser(s) }
    property UserInfo[Index: Integer] : TUserInfo read GetUserInfo;
    { number of user(s) retrieved by DisplayUser(s) }
    property UserInfoCount: Integer read GetUserInfoCount;
  published
    { SQL role name to use when connecting to the security database }
    property Role: string index ord(spRole) read GetStringParam write SetStringParam;
    { The username to add, modify or remove from the security database (max 31 chars) }
    property User: string index ord(spUser) read GetStringParam write SetStringParam;
    { Password for the user being added/modified (max 32 only first 8 used) }
    property Pass: string index ord(spPass) read GetStringParam write SetStringParam;
    { The first name of the user being added/modified }
    property FirstName: string index ord(spFirstName) read GetStringParam write SetStringParam;
    { The middle name of the user being added/modified }
    property MiddleName: string index ord(spMiddleName) read GetStringParam write SetStringParam;
    { The last name of the user being added/modified }
    property LastName: string index ord(spLastName) read GetStringParam write SetStringParam;
    { an integer that specifies a user ID of the user being added/modified }
    property UserID: Integer index ord(spUserID) read GetIntegerParam write SetIntegerParam;
    { an integer that specifies a group ID of the user being added/modified }
    property GroupID: Integer index ord(spGroupID) read GetIntegerParam write SetIntegerParam;
  end;

  TRepairOption = (roValidateDB, roValidateFull, roSweepDB, roMendDB,
    roListLimboTrans, roCheckDB, roIgnoreChecksum, roKillShadows);
  TRepairOptions = set of TRepairOption;

  TJvUIBRepair = class(TJvUIBService)
  private
    FOptions: TRepairOptions;
    FDatabase: string;
  protected
    function CreateStartSPB: string; virtual;
  public
    procedure Run;
  published
    property Options: TRepairOptions read FOptions write FOptions;
    property Database: string read FDatabase write FDatabase;
  end;

  TOnEvent = procedure(Sender: TObject; const EventName: string; Count: Integer;
    var Cancel: boolean) of object;
  TOnExceptionEvent = procedure(Error: Exception) of object;

  TJvUIBEventThread = class;

  TJvUIBEvents = class(TJvUIBComponent)
  private
    FOnEvent: TOnEvent;
    FOnException: TOnExceptionEvent;
    FThreads: TList;
    FDatabase: TJvUIBDataBase;
    FEvents: TStrings;
    FAutoRegister: boolean;
    FThreadException : boolean;
    FRegistered : boolean;
    FSyncMainThread: boolean;
    procedure SetDatabase(value: TJvUIBDataBase);
    procedure SetEvents(Value: TStrings);
    procedure SetRegistered(const Value: boolean);
  protected
  {$IFNDEF UIB_NO_COMPONENT}
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  {$ENDIF}
  public
    constructor Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent){$ENDIF}; override;
    destructor Destroy; override;
    procedure RegisterEvents; virtual;
    procedure UnRegisterEvents; virtual;
    procedure SetAutoRegister(const Value: boolean);
  published
    property AutoRegister: boolean read FAutoRegister write SetAutoRegister;
    property Database: TJvUIBDataBase read FDatabase write SetDatabase;
    property Events: TStrings read FEvents write SetEvents;
    property Registered: boolean read FRegistered write SetRegistered;
    property SyncMainThread: boolean read FSyncMainThread write FSyncMainThread default true;
    property OnEvent: TOnEvent read FOnEvent write FOnEvent;
    property OnException: TOnExceptionEvent read FOnException write FOnException;
  end;

  TJvUIBEventThread = class(TThread)
  private
    FCurrentEvent: Integer;
    FEventID: Integer;
    FEventBuffer: PChar;
    FEventBufferLen: Smallint;
    FResultBuffer: PChar;
    FSignal: TSimpleEvent;
    FQueueEvent: boolean;
    FBlock: integer;
    FOwner: TJvUIBEvents;
    FExceptObject: TObject;
    FExceptAddr: Pointer;
    FCancelAlerts : boolean;
    FStatusVector: TStatusVector;
    FSyncMainThread: boolean;
    function HandleException: boolean;
    function FindDataBase: TJvUIBDataBase;
    procedure SyncEventQueue;
    procedure SyncOnEvent;
    procedure SyncHandleException;
    procedure SyncTerminate(sender: TObject);
  protected
    procedure Execute; override;
  public
    constructor Create(Owner: TJvUIBEvents; Block: Integer;
      SyncMainThread: boolean); virtual;
    destructor Destroy; override;
  end;

  TShutdownMode = (smForced, smDenyTransaction, smDenyAttachment);

  TJvUIBConfig = class(TJvUIBService)
  private
    FDatabaseName: TFileName;
  public
    procedure ShutdownDatabase(Options: TShutdownMode; Wait: Integer);
    procedure SetSweepInterval(Value: Integer);
    procedure SetDBSqlDialect(Value: Integer);
    procedure SetPageBuffers(Value: Integer);
    procedure ActivateShadow;
    procedure BringDatabaseOnline;
    procedure SetReserveSpace(Value: Boolean);
    procedure SetAsyncMode(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
  published
    property DatabaseName: TFileName read FDatabaseName write FDatabaseName;
  end;

  TJvUIBServerInfo = class(TJvUIBService)
  private
    FOnInfoAttachments: TOnInfoIntegerCount;  
    FOnInfoDatabases: TOnInfoIntegerCount;
    FOnInfoDbName: TOnInfoStringCount;
  public
    procedure GetServerInfo;
  published
    property OnInfoAttachments: TOnInfoIntegerCount read FOnInfoAttachments write FOnInfoAttachments;
    property OnInfoDatabases: TOnInfoIntegerCount read FOnInfoDatabases write FOnInfoDatabases;  
    property OnInfoDbName: TOnInfoStringCount read FOnInfoDbName write FOnInfoDbName;
  end;

implementation
uses jvuibmetadata;
//{$IFDEF FPC}{$IFDEF UNIX},cthreads{$ENDIF}{$ENDIF}

type
  PExceptionInfo = ^TExceptionInfo;
  TExceptionInfo = record
    ExepClass: EUIBExceptionClass;
    ID: Integer;
  end;

{$IFDEF UNIX}
const
  INFINITE = $FFFFFFFF;
{$ENDIF}

{ TJvUIBDataBase }

procedure TJvUIBDataBase.AddTransaction(Transaction: TJvUIBTransaction);
begin
  if (FTransactions = nil) then
    FTransactions := TList.Create;
  FTransactions.Add(Transaction);
end;

procedure TJvUIBDataBase.ClearTransactions;
begin
  while (FTransactions <> nil) do
    TJvUIBTransaction(FTransactions.Last).RemoveDatabase(Self); 
end;

procedure TJvUIBDataBase.CloseTransactions;
var i: Integer;
begin
  if (FTransactions <> nil) then
    for i := 0 to FTransactions.Count - 1 do
      TJvUIBTransaction(FTransactions.Items[i]).Close(etmDefault, True);
end;

constructor TJvUIBDataBase.Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent){$ENDIF};
begin
  inherited;
  FLibrary := TUIBLibrary.Create;
  FLiBraryName := GetClientLibrary;
  FLibrary.OnConnectionLost := DoOnConnectionLost;
  FLibrary.OnGetDBExceptionClass := DoOnGetDBExceptionClass;
  FDbHandle := nil;
  FHandleShared := False;
  FParams := TStringList.Create;
  SQLDialect := 3;
  CharacterSet := csNONE;
  FExceptions := TList.Create;
  FEventNotifiers := TList.Create;
  FMetadata := nil;
  FMetaDataOptions := TMetaDataOptions.Create;
end;

destructor TJvUIBDataBase.Destroy;
begin
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    Connected := False;
    ClearTransactions;
    ClearEvents;
    TStringList(FParams).Free;
    ClearExceptions;
    FExceptions.Free;
    FEventNotifiers.Free;
    FLibrary.Free;
    FMetaDataOptions.Free;
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
  inherited;
end;

procedure TJvUIBDataBase.DoOnConnectionLost(Lib: TUIBLibrary);
begin
  Lib.RaiseErrors := False;
  try
    Connected := False;
  finally
    Lib.RaiseErrors := True;
    if Assigned(FOnConnectionLost) then
      FOnConnectionLost(Self);
  end;
end;

function TJvUIBDataBase.GetCharacterSet: TCharacterSet;
var
  i: TCharacterSet;
  S: String;
begin
  S := trim(UpperCase(ReadParamString('lc_ctype', 'NONE')));
  Result := csNONE;
  for i := low(TCharacterSet) to high(TCharacterSet) do
    if (S = CharacterSetStr[i]) then
    begin
      Result := i;
      Break;
    end;
end;

function TJvUIBDataBase.GetConnected: boolean;
begin
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    result := FDbHandle <> nil;
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
end;

function TJvUIBDataBase.GetPassWord: string;
begin
  result := ReadParamString('password');
end;

function TJvUIBDataBase.GetSQLDialect: Integer;
begin
  try
    Result := ReadParamInteger('sql_dialect', 3);
  except
    WriteParamInteger('sql_dialect', 3);
    raise;
  end;
end;

procedure TJvUIBDataBase.ExecuteImmediate(const Statement: string);
begin
  FLibrary.Load(FLiBraryName);
  FLibrary.DSQLExecuteImmediate(Statement, SQLDialect);
end;

procedure TJvUIBDataBase.CreateDatabase(PageSize: Integer = 2048);
var TrHandle: IscTrHandle;
const
  CreateDb = 'CREATE DATABASE ''%s'' USER ''%s'' PASSWORD ''%s'' '+
    'PAGE_SIZE %d DEFAULT CHARACTER SET %s';
begin
  TrHandle := nil;
  Connected := False;
  FLibrary.Load(FLiBraryName);
  FLibrary.DSQLExecuteImmediate(FDbHandle, TrHandle,
    Format(CreateDb, [DatabaseName, UserName, PassWord, PageSize,
    CharacterSetStr[CharacterSet]]), SQLDialect);
end;

function TJvUIBDataBase.GetUserName: string;
begin
  result := ReadParamString('user_name');
end;

function TJvUIBDataBase.ReadParamInteger(Param: String;
  Default: Integer): Integer;
begin
  Result := StrToInt(ReadParamString(Param, IntToStr(Default)));
end;

function TJvUIBDataBase.ReadParamString(Param, Default: String): String;
var
  I: Integer;
begin
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    I := FParams.IndexOfName(Param);
    if I >= 0 then
    begin
      Result := Copy(FParams[I], Length(Param) + 2, Maxint);
      Exit;
    end;
    Result := Default;
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
end;

procedure TJvUIBDataBase.RemoveTransaction(Transaction: TJvUIBTransaction);
begin
  if (FTransactions <> nil) then
  begin
    FTransactions.Remove(Transaction);
    if FTransactions.Count = 0 then
    begin
      FTransactions.free;
      FTransactions := nil;
    end;
  end;
end;

procedure TJvUIBDataBase.SetCharacterSet(const Value: TCharacterSet);
begin
  WriteParamString('lc_ctype', CharacterSetStr[Value]);
end;

procedure TJvUIBDataBase.SetConnected(const Value: boolean);
begin
  if (Value = Connected) then Exit;
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    with FLibrary do
    case Value of
      True  :
        begin
          if Assigned(BeforeConnect) then BeforeConnect(Self);
          FLibrary.Load(FLiBraryName);
          if not FHandleShared then
            AttachDatabase(FDatabaseName, FDbHandle, FParams.Text, BreakLine);
          RegisterEvents;  
          if Assigned(AfterConnect) then AfterConnect(Self);
        end;
      False :
        begin
          if Assigned(BeforeDisconnect) then BeforeDisconnect(Self);
          CloseTransactions;
          UnRegisterEvents;
          if FMetadata <> nil then
            FreeAndNil(FMetadata);
          if FHandleShared then
          begin
            FDbHandle := nil;
            FHandleShared := False;
          end else
            DetachDatabase(FDbHandle);
          if Assigned(AfterDisconnect) then AfterDisconnect(Self);
        end;
    end;
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
end;

procedure TJvUIBDataBase.SetDatabaseName(const Value: TFileName);
begin
  FDatabaseName := Value;
{$IFNDEF UIB_NO_COMPONENT}
  if (csDesigning in ComponentState) then
    Connected := False;
{$ENDIF}
end;

procedure TJvUIBDataBase.SetDbHandle(const Value: IscDbHandle);
begin
  if (FDbHandle = nil) or ((FDbHandle <> nil) and FHandleShared) then
  begin
    FLibrary.Load(FLiBraryName);
    FDbHandle := Value;
    FHandleShared := (FDbHandle <> nil);
  end else
    raise Exception.Create(EUIB_DBHANDLEALREADYSET);
end;

procedure TJvUIBDataBase.SetLibraryName(const Lib: TFileName);
begin
  SetConnected(False);
  FLibrary.UnLoad;
  FLiBraryName := Lib;
end;

function TJvUIBDataBase.GetTransactions(const Index: Cardinal): TJvUIBTransaction;
begin
  if FTransactions <> nil then
    Result := FTransactions.Items[Index] else
    raise EListError.CreateFmt(EUIB_INDEXERROR,[Index]);
end;

function TJvUIBDataBase.GetTransactionsCount: Cardinal;
begin
  if FTransactions <> nil then
    Result := FTransactions.Count else
    Result := 0;
end;

procedure TJvUIBDataBase.SetParams(const Value: TStrings);
begin
  FParams.Assign(Value);
end;

procedure TJvUIBDataBase.SetPassWord(const Value: string);
begin
  WriteParamString('password', Value);
end;

procedure TJvUIBDataBase.SetSQLDialect(const Value: Integer);
begin
  WriteParamInteger('sql_dialect', Value);
end;

procedure TJvUIBDataBase.SetUserName(const Value: string);
begin
  WriteParamString('user_name', Value);
end;

procedure TJvUIBDataBase.WriteParamInteger(Param: String; Value: Integer);
begin
  WriteParamString(Param, IntToStr(Value));
end;

procedure TJvUIBDataBase.WriteParamString(Param, Value: String);
var
  I: Integer;
  S: string;
begin
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    S := Param + '=' + Value;
    I := FParams.IndexOfName(Param);
    if I >= 0 then
      FParams[I] := S
    else
      FParams.Add(S);
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
end;

procedure TJvUIBDataBase.ClearExceptions;
var i: Integer;
begin
  for i := 0 to FExceptions.Count - 1 do
    FreeMem(FExceptions[i]);
  FExceptions.Clear;
end;

procedure TJvUIBDataBase.RegisterException(Excpt: EUIBExceptionClass;
  ID: Integer);
var
  ExcepInfo: PExceptionInfo;
  i: Integer;
begin
  for i := 0 to FExceptions.Count - 1 do
    if PExceptionInfo(FExceptions[i]).ID = ID then
      raise Exception.CreateFmt(EUIB_EXPTIONREGISTERED, [ID]);
  GetMem(ExcepInfo, SizeOf(TExceptionInfo));
  ExcepInfo.ExepClass := Excpt;
  ExcepInfo.ID := ID;
  FExceptions.Add(ExcepInfo);
end;

function TJvUIBDataBase.RegisterException(Excpt: EUIBExceptionClass;
  const Name: string): Integer;
var
  Transaction: TJvUIBTransaction;
  Query: TJvUIBQuery;
begin
  Result := -1;
  Transaction := TJvUIBTransaction.Create{$IFNDEF UIB_NO_COMPONENT}(nil){$ENDIF};
  Query := TJvUIBQuery.Create{$IFNDEF UIB_NO_COMPONENT}(nil){$ENDIF};
  try
    Transaction.DataBase := Self;
    Query.Transaction := Transaction;
    Query.CachedFetch := False;
    Query.SQL.Text := 'SELECT RDB$EXCEPTION_NUMBER FROM RDB$EXCEPTIONS WHERE RDB$EXCEPTION_NAME = ?';
    Query.Params.AsString[0] := UpperCase(Name);
    Query.Open;
    if not Query.Eof then
    begin
      Result := Query.Fields.AsInteger[0];
      RegisterException(Excpt, Result);
    end;
    Query.Close(etmCommit);
    if (Result = - 1) then
      raise Exception.CreateFmt(EUIB_EXCEPTIONNOTFOUND, [Name]);
  finally
    Query.Free;
    Transaction.Free;
  end;
end;

procedure TJvUIBDataBase.UnRegisterException(Number: Integer);
var i: Integer;
begin
  for i := 0 to FExceptions.Count - 1 do
    if PExceptionInfo(FExceptions[i]).ID = Number then
    begin
      FreeMem(FExceptions[i]);
      FExceptions.Delete(i);
      Break;
    end;
end;

procedure TJvUIBDataBase.UnRegisterExceptions(Excpt: EUIBExceptionClass);
var i: Integer;
begin
  i := 0;
  while i < FExceptions.Count do
  begin
    if (PExceptionInfo(FExceptions[i]).ExepClass = Excpt) then
    begin
      FreeMem(FExceptions[i]);
      FExceptions.Delete(i);
    end else
    inc(i);
  end;
end;

procedure TJvUIBDataBase.DoOnGetDBExceptionClass(Number: Integer; out Excep: EUIBExceptionClass);
var i: Integer;
begin
  for i := 0 to FExceptions.Count - 1 do
    if (PExceptionInfo(FExceptions[i]).ID = Number) then
    begin
      Excep := PExceptionInfo(FExceptions[i]).ExepClass;
      Exit;
    end;
  Excep := EUIBException;
end;

function TJvUIBDataBase.GetMetadata(Refresh: boolean = False): TObject;
var
  Transaction: TJvUIBTransaction;
begin
  if Refresh and (FMetadata <> nil) then
    FreeAndNil(FMetadata);
  if (FMetadata = nil) then
  begin
    Transaction := TJvUIBTransaction.Create{$IFNDEF UIB_NO_COMPONENT}(nil){$ENDIF};
    try
      Transaction.Database := Self;
      FMetadata := TMetaDataBase.Create(nil, -1);
      with TMetaDataBase(FMetadata) do
      begin
        OIDDatabases := FMetaDataOptions.Objects;
        OIDTables := FMetaDataOptions.Tables;
        OIDViews := FMetaDataOptions.Views;
        OIDProcedures := FMetaDataOptions.Procedures;
        OIDUDFs := FMetaDataOptions.UDFs;
        OIDRoles := FMetaDataOptions.Roles;
        SysInfos := FMetaDataOptions.FSysInfos
      end;
      try
        TMetaDataBase(FMetadata).LoadFromDatabase(Transaction);
        Transaction.Commit;
      except
        FreeAndNil(FMetadata);
        raise;
      end;
    finally
      Transaction.Free;
    end;
  end;
  Result := FMetadata;
end;

function TJvUIBDataBase.GetSegmentSize: Word;
begin
  Result := FLibrary.SegMentSize;
end;

procedure TJvUIBDataBase.SetSegmentSize(const Value: Word);
begin
  FLibrary.SegMentSize := Value;
end;

function TJvUIBDataBase.GetShutdown: TShutdownOptions;
begin
  try
    case sizeof(TShutdownOptions) of
      1: PByte(@result)^ := ReadParamInteger('shutdown', 0);
      2: PWord(@result)^ := ReadParamInteger('shutdown', 0);
      4: PInteger(@result)^ := ReadParamInteger('shutdown', 0);
    end;
  except
    WriteParamInteger('shutdown', 0);
  end;
end;

procedure TJvUIBDataBase.SetShutdown(const Value: TShutdownOptions);
begin
  case sizeof(TShutdownOptions) of
    1: WriteParamInteger('shutdown', PByte(@Value)^);
    2: WriteParamInteger('shutdown', PWord(@Value)^);
    4: WriteParamInteger('shutdown', PInteger(@Value)^);
  end;
end;

function TJvUIBDataBase.GetInfoIntValue(const item: Integer): integer;
begin
  SetConnected(true);
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    case item of
      isc_info_implementation,
      isc_info_base_level:
      result := byte(FLibrary.DatabaseInfoString(FDbHandle, item, 8)[5]);
    else
      result := FLibrary.DatabaseInfoIntValue(FDbHandle, char(item));
    end;
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
end;

{$IFDEF FB20_UP}
function TJvUIBDataBase.GetInfoDateTimeValue(const item: Integer): TDateTime;
begin
  SetConnected(true);
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    result := FLibrary.DatabaseInfoDateTime(FDbHandle, item);
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
end;
{$ENDIF}

function TJvUIBDataBase.GetInfoBooleanValue(const item: Integer): boolean;
begin
  result := GetInfoIntValue(item) <> 0;
end;

function TJvUIBDataBase.GetInfoStringValue(const item: integer): string;
var size: byte;
begin
  SetConnected(true);
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    result := FLibrary.DatabaseInfoString(FDbHandle, item, 256);
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
  case Item of
    isc_info_cur_logfile_name, isc_info_wal_prv_ckpt_fname:
      begin
        size := byte(result[4]);
        Move(result[5], result[1], size);
        SetLength(Result, size);
      end;
  else
    size := byte(result[5]);
    Move(result[6], result[1], size);
    SetLength(Result, size);
  end;
end;

function TJvUIBDataBase.GetInfoOperationsCount(
  const item: Integer): Integer;
var
  data: string;
  i: Integer;
  p: PTableOperation;
begin
  SetConnected(true);
  result := 0;
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    data := FLibrary.DatabaseInfoString(FDbHandle, Item, 8);
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
  for i := 0 to PWord(@data[2])^ div sizeof(TTableOperation) - 1 do
  begin
    p := PTableOperation(@data[4+ i * sizeof(TTableOperation)]);
    inc(result, p^.Count);
    case item of
      isc_info_read_seq_count: if assigned(FOnInfoReadSeqCount) then FOnInfoReadSeqCount(self, p);
      isc_info_read_idx_count: if assigned(FOnInfoReadIdxCount) then FOnInfoReadIdxCount(self, p);
      isc_info_update_count  : if assigned(FOnInfoUpdateCount) then FOnInfoUpdateCount(self, p);
      isc_info_insert_count  : if assigned(FOnInfoInsertCount) then FOnInfoInsertCount(self, p);
      isc_info_delete_count  : if assigned(FOnInfoDeleteCount) then FOnInfoDeleteCount(self, p);
      isc_info_backout_count : if assigned(FOnInfoBackoutCount) then FOnInfoBackoutCount(self, p);
      isc_info_purge_count   : if assigned(FOnInfoPurgeCount) then FOnInfoPurgeCount(self, p);
      isc_info_expunge_count : if assigned(FOnInfoExpungeCount) then FOnInfoExpungeCount(self, p);
    end;
  end;
end;

function TJvUIBDataBase.GetInfoIntCount(const item: Integer): Integer;
var
  data: string;
  p: PChar;
begin
  SetConnected(true);
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    data := FLibrary.DatabaseInfoString(FDbHandle, item, 256);
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
  p := PChar(data);
  result := 0;
  while byte(p^) = item do
  begin
    inc(result, 1);
  {$IFDEF FB102ORYF867}
    case item of
      isc_info_active_transactions:
        if assigned(FOnInfoActiveTransactions) then
          FOnInfoActiveTransactions(self, PInteger(@p[3])^);
    end;
  {$ENDIF}
    inc(p, 7);
  end;
end;

function TJvUIBDataBase.GetInfoStringCount(const item: Integer): Integer;
var
  data: string;
  p: PChar;
  len: integer;
begin
  SetConnected(true);
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    data := FLibrary.DatabaseInfoString(FDbHandle, item, 256);
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
  p := PChar(data);
  result := 0;
  while byte(p^) = item do
  begin
    inc(result, 1);
    len := byte(p[3]);
    inc(p, 4);
    case item of
      isc_info_user_names:
        if assigned(FOnInfoUserNames) then
          FOnInfoUserNames(self, copy(p, 0, len));
    end;
    inc(p, len);
  end;
end;

function TJvUIBDataBase.GetInfoDbId(const Index: Integer): string;
var
  data: string;
  p: PChar;
  i: Integer;
begin
  SetConnected(true);
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    Data := FLibrary.DatabaseInfoString(FDBHandle, isc_info_db_id, 1024);
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
  p := @data[5];
  for i := 1 to ord(data[4]) do
  begin
    if Index = i then
    begin
      result := copy(p+1, 0, ord(p^));
      Break;
    end;
    inc(p, ord(p^)+1);
  end;
end;

procedure TJvUIBDataBase.UnRegisterEvents;
var i: Integer;
begin
  for i := 0 to FEventNotifiers.Count - 1 do
    TJvUIBEvents(FEventNotifiers.Items[i]).UnRegisterEvents;
end;

procedure TJvUIBDataBase.RegisterEvents;
var i: Integer;
begin
  for i := 0 to FEventNotifiers.Count - 1 do
    with TJvUIBEvents(FEventNotifiers.Items[i]) do
      if AutoRegister then
        RegisterEvents;
end;

procedure TJvUIBDataBase.RemoveEventNotifier(Event: TJvUIBEvents);
var
  i : integer;
begin
  i := FEventNotifiers.IndexOf(Event);
  if (i >= 0) then
    FEventNotifiers.Delete(i);
end;

procedure TJvUIBDataBase.AddEventNotifier(Event: TJvUIBEvents);
begin
  FEventNotifiers.Add(Event);
end;

procedure TJvUIBDataBase.ActiveAllTriggers;
var
  triggers: TJvUIBQuery;
  tr: TJvUIBTransaction;
begin
  tr := TJvUIBTransaction.Create{$IFNDEF UIB_NO_COMPONENT}(nil){$ENDIF};
  triggers := TJvUIBQuery.Create{$IFNDEF UIB_NO_COMPONENT}(nil){$ENDIF};
  try
    tr.DataBase := Self;
    triggers.CachedFetch := false;
    triggers.Transaction := tr;
    triggers.SQL.Text :=
      'SELECT T.RDB$TRIGGER_NAME FROM RDB$TRIGGERS T '+
      'LEFT JOIN RDB$CHECK_CONSTRAINTS C ON C.RDB$TRIGGER_NAME = T.RDB$TRIGGER_NAME '+
      'WHERE ((T.RDB$SYSTEM_FLAG = 0) OR (T.RDB$SYSTEM_FLAG IS NULL)) '+
      'AND (C.RDB$TRIGGER_NAME IS NULL) AND (T.RDB$TRIGGER_INACTIVE = 1)';
    triggers.Open;
    while not triggers.Eof do
    begin
      tr.ExecuteImmediate(format('ALTER TRIGGER %s ACTIVE', [triggers.Fields.AsString[0]]));
      triggers.Next;
    end;
  finally
    triggers.Free;
    tr.Free;
  end;
end;

procedure TJvUIBDataBase.DeactiveAllTriggers;
var
  triggers: TJvUIBQuery;
  tr: TJvUIBTransaction;
begin
  tr := TJvUIBTransaction.Create{$IFNDEF UIB_NO_COMPONENT}(nil){$ENDIF};
  triggers := TJvUIBQuery.Create{$IFNDEF UIB_NO_COMPONENT}(nil){$ENDIF};
  try
    tr.DataBase := Self;
    triggers.CachedFetch := false;
    triggers.Transaction := tr;
    triggers.SQL.Text :=
      'SELECT T.RDB$TRIGGER_NAME FROM RDB$TRIGGERS T '+
      'LEFT JOIN RDB$CHECK_CONSTRAINTS C ON C.RDB$TRIGGER_NAME = T.RDB$TRIGGER_NAME '+
      'WHERE ((T.RDB$SYSTEM_FLAG = 0) OR (T.RDB$SYSTEM_FLAG IS NULL)) '+
      'AND (C.RDB$TRIGGER_NAME IS NULL) AND (T.RDB$TRIGGER_INACTIVE = 0)';
    triggers.Open;
    while not triggers.Eof do
    begin
      tr.ExecuteImmediate(format('ALTER TRIGGER %s INACTIVE', [triggers.Fields.AsString[0]]));
      triggers.Next;
    end;
  finally
    triggers.Free;
    tr.Free;
  end;
end;

procedure TJvUIBDataBase.RecomputeSelectivityIndices;
var
  indices: TJvUIBQuery;
  tr: TJvUIBTransaction;
begin
  tr := TJvUIBTransaction.Create{$IFNDEF UIB_NO_COMPONENT}(nil){$ENDIF};
  indices := TJvUIBQuery.Create{$IFNDEF UIB_NO_COMPONENT}(nil){$ENDIF};
  try
    tr.DataBase := Self;
    indices.CachedFetch := false;
    indices.Transaction := tr;
    indices.SQL.Text := 'SELECT RDB$INDEX_NAME FROM RDB$INDICES';
    indices.Open;
    while not indices.Eof do
    begin
      tr.ExecuteImmediate(format('SET STATISTICS INDEX %s', [indices.Fields.AsString[0]]));
      indices.Next;
    end;
  finally
    indices.Free;
    tr.Free;
  end;
end;

procedure TJvUIBDataBase.RecompileAllProcedures;
var
  Meta: TMetaDataBase;
  tr: TJvUIBTransaction;
  i: integer;
begin
  meta := TMetaDataBase.Create(nil, -1);
  tr := TJvUIBTransaction.Create{$IFNDEF UIB_NO_COMPONENT}(nil){$ENDIF};
  try
    tr.Database := Self;
    meta.OIDDatabases := [OIDProcedure];
    Meta.LoadFromDatabase(tr);
    for i := 0 to Meta.ProceduresCount - 1 do
      tr.ExecuteImmediate(Meta.Procedures[i].AsAlterDDL);
  finally
    meta.Free;
    tr.Free;
  end;
end;

procedure TJvUIBDataBase.RecompileAllTriggers;
var
  triggers: TJvUIBQuery;
  tr: TJvUIBTransaction;
begin
  tr := TJvUIBTransaction.Create{$IFNDEF UIB_NO_COMPONENT}(nil){$ENDIF};
  triggers := TJvUIBQuery.Create{$IFNDEF UIB_NO_COMPONENT}(nil){$ENDIF};
  try
    tr.DataBase := Self;
    triggers.CachedFetch := false;
    triggers.FetchBlobs := true;
    triggers.Transaction := tr;
    triggers.SQL.Text :=
      'SELECT T.RDB$TRIGGER_NAME, T.RDB$TRIGGER_SOURCE FROM RDB$TRIGGERS T '+
      'LEFT JOIN RDB$CHECK_CONSTRAINTS C ON C.RDB$TRIGGER_NAME = T.RDB$TRIGGER_NAME '+
      'WHERE ((T.RDB$SYSTEM_FLAG = 0) OR (T.RDB$SYSTEM_FLAG IS NULL)) AND (C.RDB$TRIGGER_NAME IS NULL)';
    triggers.Open;
    while not triggers.Eof do
    begin
      with triggers.Fields do
        tr.ExecuteImmediate(format('ALTER TRIGGER %s'#13'%s', [AsString[0], AsString[1]]));
      triggers.Next;
    end;
  finally
    triggers.Free;
    tr.Free;
  end;
end;

function TJvUIBDataBase.GetRole: string;
begin
  Result := ReadParamString('sql_role_name');
end;

procedure TJvUIBDataBase.SetRole(const Value: string);
begin
  WriteParamString('sql_role_name', Value);
end;

procedure TJvUIBDataBase.ClearEvents;
var
  i: integer;
begin
  for i := 0 to FEventNotifiers.Count - 1 do
    TJvUIBEvents(FEventNotifiers[i]).SetDatabase(nil);
end;

{ TJvUIBStatement }

procedure TJvUIBStatement.SetTransaction(const Transaction: TJvUIBTransaction);
begin
  if (FTransaction <> Transaction) then
  begin
    if (FTransaction <> nil) then
    begin
      if FTransaction.AutoRetain then
        InternalClose(etmDefault, True) else
        InternalClose(etmStayIn, True);
      FTransaction.RemoveSQLComponent(Self);
    end;
    FTransaction := Transaction;
    if (Transaction <> nil) then
      Transaction.AddSQLComponent(Self);
    FCurrentState := qsDataBase;
  end;
end;

procedure TJvUIBStatement.SetDataBase(ADataBase: TJvUIBDataBase);
begin
  if (FDataBase <> ADataBase) then
  begin
    if (FTransaction <> nil) then
    begin
      if FTransaction.AutoRetain then
        InternalClose(etmDefault, True) else
        InternalClose(etmStayIn, True);
    end;
    FDataBase := ADataBase;
  end;
end;

procedure TJvUIBStatement.BeginTransaction;
begin
  if FTransaction <> nil then
    FTransaction.BeginTransaction else
    raise Exception.Create(EUIB_TRANSACTIONNOTDEF);
  FCurrentState := qsTransaction;
end;

procedure TJvUIBStatement.Close(const Mode: TEndTransMode);
begin
  InternalClose(Mode, False);
end;

procedure TJvUIBStatement.Open(FetchFirst: boolean = True);
begin
  // if you reopen the same query I Close
  // the cursor, clean sql result and
  // execute the query again to save
  // the prepare time !
  if (FCurrentState = qsExecute) then
    CloseCursor; 

  if FetchFirst then
    InternalNext else
    BeginExecute;
end;

procedure TJvUIBStatement.Next;
begin
  if (FCurrentState <> qsExecute) then
    raise Exception.Create(EUIB_MUSTBEOPEN);
  InternalNext;
end;

procedure TJvUIBStatement.Prior;
begin
  InternalPrior;
end;

procedure TJvUIBStatement.Last;
begin
  FetchAll;
end;

procedure TJvUIBStatement.First;
begin
  if (FSQLResult <> nil) and
   (FSQLResult.RecordCount > 0) then
   FSQLResult.CurrentRecord := 0;
end;

procedure TJvUIBStatement.FetchAll;
begin
  while not Eof do Next;
end;

procedure TJvUIBStatement.Execute;
begin
  BeginExecute;
end;

procedure TJvUIBStatement.ExecSQL;
begin
  if FCurrentState > qsExecImme then
    BeginExecute else // it shouldn't happen ...
    BeginExecImme;
end;

procedure TJvUIBStatement.Prepare;
begin
  if (FCurrentState < qsPrepare) then
  BeginPrepare
end;

procedure TJvUIBStatement.InternalNext;
begin
  if (FCurrentState < qsExecute) then
    BeginExecute;

  if FSQLResult.FieldCount > 0 then
  if Fields.ScrollEOF then
    Fields.Next else
  begin
  {$IFDEF UIBTHREADSAFE}
    Lock;
    try
  {$ENDIF}
      with FindDataBase, FLibrary do
      try
        if FSQLResult.FetchBlobs then
          DSQLFetchWithBlobs(FDbHandle, FTransaction.FTrHandle, FStHandle, FTransaction.FSQLDialect, FSQLResult) else
          DSQLFetch(FDbHandle, FTransaction.FTrHandle, FStHandle, FTransaction.FSQLDialect, FSQLResult);
      except
        if FOnError <> etmStayIn then
          EndExecute(FOnError, False);
        raise;
      end;
  {$IFDEF UIBTHREADSAFE}
    finally
      UnLock;
    end;
  {$ENDIF}
  end;
end;

procedure TJvUIBStatement.InternalPrior;
begin
  if Fields.CachedFetch then
  begin
    if Fields.CurrentRecord > 0 then
      Fields.CurrentRecord := Fields.CurrentRecord - 1;
  end else
    raise Exception.Create(EUIB_CACHEDFETCHNOTSET);
end;

procedure TJvUIBStatement.EndTransaction(const ETM: TEndTransMode; Auto: boolean);
begin
  if FTransaction <> nil then
  begin
    if FTransaction.EndTransaction(ETM, Self, Auto) then
      FCurrentState := qsDataBase;
  end else
    raise Exception.Create(EUIB_TRANSACTIONNOTDEF);
end;

procedure TJvUIBStatement.BeginStatement;
begin
  BeginTransaction;
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    with FindDataBase.FLibrary do
    try
      FStHandle := nil;
      DSQLAllocateStatement(FindDataBase.FDbHandle, FStHandle);
    except
      EndTransaction(FOnError, False);
      raise;
    end;
    inc(FTransaction.FStatements);
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
  FCurrentState := qsStatement;
end;

procedure TJvUIBStatement.EndStatement(const ETM: TEndTransMode; Auto: boolean);
begin
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    with FindDataBase.FLibrary do
      DSQLFreeStatement(FStHandle, DSQL_drop);

    FStHandle := nil;
    Dec(FTransaction.FStatements);
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
  FCurrentState := qsTransaction;
  if (ETM <> etmStayIn) then
    EndTransaction(ETM, Auto);

  if Assigned(FOnClose) then
    FOnClose(Self);    
end;

procedure TJvUIBStatement.BeginPrepare;
begin
  if (FStHandle = nil) then BeginStatement;
  FSQLResult := ResultClass.Create(0, FCachedFetch, FFetchBlobs, FBufferChunks);
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    with FindDataBase, FLibrary do
    try
      if (FQuickScript or (not FParseParams)) then
        FStatementType := DSQLPrepare(FDbHandle, FTransaction.FTrHandle, FStHandle,
          FSQL.Text, FTransaction.FSQLDialect, FSQLResult) else
        FStatementType := DSQLPrepare(FDbHandle, FTransaction.FTrHandle, FStHandle,
          FParsedSQL, FTransaction.FSQLDialect, FSQLResult);
        FCursorName := 'C' + inttostr(Integer(FStHandle));
        if FUseCursor then
          DSQLSetCursorName(FStHandle, FCursorName);
    except
      FSQLResult.free;
      FSQLResult := nil;
      EndStatement(FOnError, False);
      raise;
    end;
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
  FCurrentState := qsPrepare;
end;

procedure TJvUIBStatement.EndPrepare(const ETM: TEndTransMode; Auto: boolean);
begin
  FSQLResult.free;
  FSQLResult := nil;
  FCurrentState := qsStatement;
  EndStatement(ETM, Auto);
end;

procedure TJvUIBStatement.BeginExecute;
begin
  if (FSQLResult = nil) then BeginPrepare;
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    with FindDataBase.FLibrary do
    try
      if (FStatementType = stExecProcedure) then
        DSQLExecute2(FTransaction.FTrHandle, FStHandle,
          FTransaction.FSQLDialect, FParameter, FSQLResult) else
        DSQLExecute(FTransaction.FTrHandle, FStHandle,
          FTransaction.FSQLDialect, FParameter);
    except
      if (FOnError <> etmStayIn) then
        EndPrepare(FOnError, False);
      raise;
    end;
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
  FCurrentState := qsExecute;
end;

procedure TJvUIBStatement.EndExecute(const ETM: TEndTransMode; Auto: boolean);
begin
  FCurrentState := qsPrepare;
  EndPrepare(ETM, Auto);
end;

procedure TJvUIBStatement.BeginExecImme;
var
  I: Integer;
  procedure ExecuteQuery(const AQuery: String; Params: TSQLParams);
  begin
    if (Trim(AQuery) = '') then exit;
  {$IFDEF UIBTHREADSAFE}
    Lock;
    try
  {$ENDIF}
      with FindDataBase.FLibrary do
      try
        DSQLExecuteImmediate(FindDataBase.FDbHandle, FTransaction.FTrHandle,
          AQuery, FTransaction.FSQLDialect, Params);
      except
        if (FOnError <> etmStayIn) then
          EndExecImme(FOnError, False);
        raise;
      end;
  {$IFDEF UIBTHREADSAFE}
    finally
      UnLock;
    end;
  {$ENDIF}
  end;
begin
  BeginTransaction;
  if FQuickScript then
    for i := 0 to FSQL.Count - 1 do
    begin
      ExecuteQuery(FSQL.Strings[i], nil);
    end else
      if FParseParams then
        ExecuteQuery(FParsedSQL, FParameter) else
        ExecuteQuery(FSQL.Text, FParameter);
  FCurrentState := qsExecImme;
end;

procedure TJvUIBStatement.EndExecImme(const ETM: TEndTransMode; Auto: boolean);
begin
  FCurrentState := qsTransaction;
  if (ETM <> etmStayIn) then
    EndTransaction(ETM, Auto);
end;

function TJvUIBStatement.ParamsClass: TSQLParamsClass;
begin
  Result := TSQLParams;
end;

function TJvUIBStatement.ResultClass: TSQLResultClass;
begin
  Result := TSQLResult;
end;

{$IFDEF UIBTHREADSAFE}
procedure TJvUIBStatement.Lock;
begin
  inherited;
    Ftransaction.Lock;
end;
{$ENDIF}

{$IFDEF UIBTHREADSAFE}
procedure TJvUIBStatement.UnLock;
begin
    Ftransaction.UnLock;
  inherited;
end;
{$ENDIF}

procedure TJvUIBStatement.SetSQL(const Value: TStrings);
begin
  FSQL.Assign(Value);
end;

function TJvUIBStatement.GetPlan: string;
begin
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    if (FCurrentState < qsPrepare) then
      Raise Exception.Create(EUIB_MUSTBEPREPARED)else
        Result := FindDataBase.FLibrary.DSQLInfoPlan(FStHandle);
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock
  end;
{$ENDIF}
end;

function TJvUIBStatement.GetStatementType: TUIBStatementType;
begin
  if (FCurrentState < qsPrepare) then
    Raise Exception.Create(EUIB_MUSTBEPREPARED) else
    Result := FStatementType;
end;

procedure TJvUIBStatement.DoSQLChange(Sender: TObject);
begin
  InternalClose(etmStayIn, True);
  if (not FQuickScript or FParseParams) then
    FParsedSQL := FParameter.Parse(FSQL.Text);
end;

function TJvUIBStatement.GetFields: TSQLResult;
begin
  if (FSQLResult = nil) then
    raise Exception.Create(EUIB_QUERYNOTOPEN);
  Result := FSQLResult;
end;

function TJvUIBStatement.GetEof: boolean;
begin
  if Assigned(FSQLResult) then
    Result := FSQLResult.Eof else
    Result := True;
end;

function TJvUIBStatement.GetBof: boolean;
begin
  if Assigned(FSQLResult) then
    Result := FSQLResult.Bof else
    Result := True;
end;

function TJvUIBStatement.FindDataBase: TJvUIBDataBase;
begin
  if FDataBase <> nil then
    result := FDataBase else
     if FTransaction <> nil then
       result := FTransaction.FDataBase else
       raise Exception.Create(EUIB_DATABASENOTDEF);  
end;

{$IFNDEF UIB_NO_COMPONENT}
procedure TJvUIBStatement.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if ((AComponent = FTransaction) and (Operation = opRemove)) then
    SetTransaction(nil);
  if ((AComponent = FDataBase) and (Operation = opRemove)) then
    SetDataBase(nil);
end;
{$ENDIF}

constructor TJvUIBStatement.Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent){$ENDIF};
begin
  inherited;
  FUseCursor := True;
  FCurrentState := qsDataBase;
{$IFNDEF UIB_NO_COMPONENT}
  if (AOwner is TJvUIBTransaction) then
    Transaction := TJvUIBTransaction(AOwner) else
{$ENDIF}
    FTransaction := nil;
  FSQL         := TStringList.Create;
  TStringList(FSQL).OnChange := DoSQLChange;
  FCachedFetch := True;
  FetchBlobs   := False;
  FQuickScript := False;
  FOnError     := etmRollback;
  FParameter   := ParamsClass.Create;
  FCursorName  := '';
  FBufferChunks := 1000;
  FParseParams := True;
end;

destructor TJvUIBStatement.Destroy;
begin
  FSQL.Free;
  FParameter.free;
  FParameter := nil;
  SetTransaction(nil);
  inherited;
end;

procedure TJvUIBStatement.ReadBlob(const Index: Word; var Str: string);
begin
  if Fields.FetchBlobs then
    Fields.ReadBlob(Index, Str) else
    InternalReadBlob(Fields, Index, str);
end;

procedure TJvUIBStatement.ReadBlob(const Index: Word; Stream: TStream);
begin
  if Fields.FetchBlobs then
    Fields.ReadBlob(Index, Stream) else
    InternalReadBlob(Fields, Index, Stream);
end;

procedure TJvUIBStatement.ReadBlob(const Index: Word; var Value: Variant);
begin
  if Fields.FetchBlobs then
    Fields.ReadBlob(Index, Value) else
    InternalReadBlob(Fields, Index, Value);
end;

procedure TJvUIBStatement.ReadBlob(const name: string; Stream: TStream);
begin
  ReadBlob(Fields.GetFieldIndex(name), Stream);
end;

procedure TJvUIBStatement.ReadBlob(const name: string; var str: string);
begin
  ReadBlob(Fields.GetFieldIndex(name), str);
end;

procedure TJvUIBStatement.ReadBlob(const name: string; var Value: Variant);
begin
  ReadBlob(Fields.GetFieldIndex(name), Value);
end;

procedure TJvUIBStatement.ParamsSetBlob(const Index: Word; Stream: TStream);
var BlobHandle: IscBlobHandle;
begin
  if (FCurrentState < qsTransaction) then
    BeginTransaction;
  BlobHandle := nil;
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
  with FindDataBase.FLibrary do
  begin
    Params.AsQuad[Index] := BlobCreate(FindDataBase.FDbHandle,
      FTransaction.FTrHandle, BlobHandle);
    try
      BlobWriteStream(BlobHandle, Stream);
    finally
      BlobClose(BlobHandle);
    end;
  end;
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
end;

procedure TJvUIBStatement.ParamsSetBlob(const Index: Word; var str: string);
var BlobHandle: IscBlobHandle;
begin
  if (FCurrentState < qsTransaction) then
    BeginTransaction;
  BlobHandle := nil;
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    with FindDataBase.FLibrary do
    begin
      Params.AsQuad[Index] := BlobCreate(FindDataBase.FDbHandle,
        FTransaction.FTrHandle, BlobHandle);
      try
        BlobWriteString(BlobHandle, str);
      finally
        BlobClose(BlobHandle);
      end;
    end;
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
end;

procedure TJvUIBStatement.ParamsSetBlob(const Index: Word; Buffer: Pointer;
  Size: Cardinal);
var BlobHandle: IscBlobHandle;
begin
  if (FCurrentState < qsTransaction) then
    BeginTransaction;
  BlobHandle := nil;
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    with FindDataBase.FLibrary do
    begin
      Params.AsQuad[Index] := BlobCreate(FindDataBase.FDbHandle,
        FTransaction.FTrHandle, BlobHandle);
      try
        BlobWriteSegment(BlobHandle, Size, Buffer);
      finally
        BlobClose(BlobHandle);
      end;
    end;
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
end;

procedure TJvUIBStatement.ParamsSetBlob(const Name: string; Stream: TStream);
var BlobHandle: IscBlobHandle;
begin
  if (FCurrentState < qsTransaction) then
    BeginTransaction;
  BlobHandle := nil;
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    with FindDataBase.FLibrary do
    begin
      Params.ByNameAsQuad[Name] := BlobCreate(FindDataBase.FDbHandle,
        FTransaction.FTrHandle, BlobHandle);
      try
        BlobWriteStream(BlobHandle, Stream);
      finally
        BlobClose(BlobHandle);
      end;
    end;
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
end;

procedure TJvUIBStatement.ParamsSetBlob(const Name: string; var str: string);
var BlobHandle: IscBlobHandle;
begin
  if (FCurrentState < qsTransaction) then
    BeginTransaction;
  BlobHandle := nil;
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    with FindDataBase.FLibrary do
    begin
      Params.ByNameAsQuad[Name] := BlobCreate(FindDataBase.FDbHandle,
        FTransaction.FTrHandle, BlobHandle);
      try
        BlobWriteString(BlobHandle, str);
      finally
        BlobClose(BlobHandle);
      end;
    end;
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
end;

procedure TJvUIBStatement.ParamsSetBlob(const Name: string; Buffer: Pointer; Size: Cardinal);
var BlobHandle: IscBlobHandle;
begin
  if (FCurrentState < qsTransaction) then
    BeginTransaction;
  BlobHandle := nil;
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    with FindDataBase.FLibrary do
    begin
      Params.ByNameAsQuad[Name] := BlobCreate(FindDataBase.FDbHandle,
        FTransaction.FTrHandle, BlobHandle);
      try
        BlobWriteSegment(BlobHandle, Size, Buffer);
      finally
        BlobClose(BlobHandle);
      end;
    end;
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
end;

procedure TJvUIBStatement.InternalReadBlob(sqlda: TSQLDA; const Index: Word;
  Stream: TStream);
var
  BlobHandle: IscBlobHandle;
begin
  if (not sqlda.IsBlob[Index]) then
    raise EUIBConvertError.Create(EUIB_CASTERROR);
  if (not sqlda.IsNull[Index]) then
  begin
  {$IFDEF UIBTHREADSAFE}
    Lock;
    try
  {$ENDIF}
      with FindDataBase.FLibrary do
      begin
        BlobHandle := nil;
        BlobOpen(FindDataBase.FDbHandle, FTransaction.FTrHandle,
          BlobHandle, sqlda.AsQuad[Index]);
        try
          BlobSaveToStream(BlobHandle, Stream);
        finally
          BlobClose(BlobHandle);
        end;
      end;
  {$IFDEF UIBTHREADSAFE}
    finally
      UnLock;
    end;
  {$ENDIF}
  end;
end;

procedure TJvUIBStatement.InternalReadBlob(sqlda: TSQLDA; const Index: Word;
  var str: string);
var
  BlobHandle: IscBlobHandle;
begin
  if (not sqlda.IsBlob[Index]) then
    raise EUIBConvertError.Create(EUIB_CASTERROR);
  if sqlda.IsNull[Index] then
     str := '' else
  begin
  {$IFDEF UIBTHREADSAFE}
    Lock;
    try
  {$ENDIF}
      with FindDataBase.FLibrary do
      begin
        BlobHandle := nil;
        BlobOpen(FindDataBase.FDbHandle, FTransaction.FTrHandle,
          BlobHandle, sqlda.AsQuad[Index]);
        try
          BlobReadString(BlobHandle, str);
        finally
          BlobClose(BlobHandle);
        end;
      end;
  {$IFDEF UIBTHREADSAFE}
    finally
      UnLock;
    end;
  {$ENDIF}
  end;
end;

procedure TJvUIBStatement.InternalReadBlob(sqlda: TSQLDA; const Index: Word;
  var Value: Variant);
var
  BlobHandle: IscBlobHandle;
begin
  if (not sqlda.IsBlob[Index]) then
    raise EUIBConvertError.Create(EUIB_CASTERROR);
  if (not sqlda.IsNull[Index]) then
  begin
  {$IFDEF UIBTHREADSAFE}
    Lock;
    try
  {$ENDIF}
      with FindDataBase.FLibrary do
      begin
        BlobHandle := nil;
        BlobOpen(FindDataBase.FDbHandle, FTransaction.FTrHandle,
          BlobHandle, sqlda.AsQuad[Index]);
        try
          BlobReadVariant(BlobHandle, Value);
        finally
          BlobClose(BlobHandle);
        end;
      end;
  {$IFDEF UIBTHREADSAFE}
    finally
      UnLock;
    end;
  {$ENDIF}
  end;
end;

procedure TJvUIBStatement.InternalClose(const Mode: TEndTransMode;
  Auto: boolean);
begin
  case FCurrentState of
    qsStatement : EndStatement(Mode, Auto);
    qsExecImme  : EndExecImme(Mode, Auto);
    qsPrepare   : EndPrepare(Mode, Auto);
    qsExecute   : EndExecute(Mode, Auto);
  end;
end;

procedure TJvUIBStatement.InternalGetBlobSize(sqlda: TSQLDA; const Index: Word; out Size: Cardinal);
var
  BlobHandle: IscBlobHandle;
begin
  if (not sqlda.IsBlob[Index]) then
    raise EUIBConvertError.Create(EUIB_CASTERROR);
  if (not sqlda.IsNull[Index]) then
  begin
  {$IFDEF UIBTHREADSAFE}
    Lock;
    try
  {$ENDIF}
      with FindDataBase.FLibrary do
      begin
        BlobHandle := nil;
        BlobOpen(FindDataBase.FDbHandle, FTransaction.FTrHandle,
          BlobHandle, sqlda.AsQuad[Index]);
        try
          BlobSize(BlobHandle, Size);
        finally
          BlobClose(BlobHandle);
        end;
      end;
  {$IFDEF UIBTHREADSAFE}
    finally
      UnLock;
    end;
  {$ENDIF}
  end;
end;

function TJvUIBStatement.FieldBlobSize(const Index: Word): Cardinal;
begin
  if Fields.FetchBlobs then
    Result := Fields.GetBlobSize(Index) else
    InternalGetBlobSize(Fields, Index, Result);
end;

function TJvUIBStatement.ParamBlobSize(const Index: Word): Cardinal;
begin
  InternalGetBlobSize(Params, Index, Result);
end;

procedure TJvUIBStatement.ReadBlob(const Index: Word; Buffer: Pointer);
begin
  if Fields.FetchBlobs then
    Fields.ReadBlob(Index, Buffer) else
    InternalReadBlob(Fields, Index, Buffer);
end;

procedure TJvUIBStatement.ReadBlob(const name: string; Buffer: Pointer);
begin
  ReadBlob(Fields.GetFieldIndex(name), Buffer);
end;

procedure TJvUIBStatement.InternalReadBlob(sqlda: TSQLDA;
  const Index: Word; Buffer: Pointer);
var
  BlobHandle: IscBlobHandle;
begin
  if (not sqlda.IsBlob[Index]) then
    raise EUIBConvertError.Create(EUIB_CASTERROR);
  if sqlda.IsNull[Index] then
     Exit else
  begin
  {$IFDEF UIBTHREADSAFE}
    Lock;
    try
  {$ENDIF}
      with FindDataBase.FLibrary do
      begin
        BlobHandle := nil;
        BlobOpen(FindDataBase.FDbHandle, FTransaction.FTrHandle,
          BlobHandle, sqlda.AsQuad[Index]);
        try
          BlobReadSizedBuffer(BlobHandle, Buffer);
        finally
          BlobClose(BlobHandle);
        end;
      end;
  {$IFDEF UIBTHREADSAFE}
    finally
      UnLock;
    end;
  {$ENDIF}
  end;
end;

function TJvUIBStatement.GetRowsAffected: Cardinal;
begin
{$IFDEF UIBTHREADSAFE}
  Result := 0;
  Lock;
  try
{$ENDIF}
    if (FCurrentState < qsPrepare) then
      Raise Exception.Create(EUIB_MUSTBEPREPARED) else
      Result := FindDataBase.FLibrary.DSQLInfoRowsAffected(FStHandle, FStatementType);
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock
  end;
{$ENDIF}
end;

procedure TJvUIBStatement.CloseCursor;
begin
  if (FCurrentState = qsExecute) then
  begin
  {$IFDEF UIBTHREADSAFE}
    Lock;
    try
  {$ENDIF}
      try
        FSQLResult.ClearRecords;
        with FindDataBase.FLibrary do
          DSQLFreeStatement(FStHandle, DSQL_close);
      except
        InternalClose(FOnError, False);
        raise;
      end;
      FCurrentState := qsPrepare;
  {$IFDEF UIBTHREADSAFE}
    finally
      UnLock;
    end;
  {$ENDIF}
  end;
end;

{ TJvUIBQuery }

procedure TJvUIBQuery.BuildStoredProc(const StoredProc: string; forSelect: boolean = true);
var
  i, r: Integer;
  Str: string;
begin
  InternalClose(etmStayIn, True);
  r := 0;
  TStringList(FSQL).OnChange := nil;
  try
    Params.Clear;
    FParsedSQL :=
      'SELECT RDB$FIELD_TYPE, RDB$PARAMETER_NAME, RDB$FIELD_SCALE, RDB$PARAMETER_TYPE '+
       'FROM RDB$PROCEDURE_PARAMETERS PRM JOIN RDB$FIELDS FLD ON '+
       'PRM.RDB$FIELD_SOURCE = FLD.RDB$FIELD_NAME '+
      'WHERE '+
          'PRM.RDB$PROCEDURE_NAME = ''' + SQLUnQuote(StoredProc) + ''' '+
      'ORDER BY RDB$PARAMETER_TYPE, PRM.RDB$PARAMETER_NUMBER';
    Open;
    try
      while not Eof do
      begin
        with Fields do
        if AsSmallint[3] = 0 then
        begin
          if AsSmallint[2] < 0 then
          begin
            case Fields.AsSmallint[0] of
              blr_short:  Params.AddFieldType(Trim(AsString[1]), uftNumeric, - AsSmallint[2], 4);
              blr_long :  Params.AddFieldType(Trim(AsString[1]), uftNumeric, - AsSmallint[2], 7);
              blr_int64,
              blr_quad,
              blr_double: Params.AddFieldType(Trim(AsString[1]), uftNumeric, - AsSmallint[2], 15);
            else
              Raise Exception.Create(EUIB_UNEXPECTEDERROR);
            end;
          end else
          case Fields.AsSmallint[0] of
            blr_text,
            blr_text2,
            blr_varying,
            blr_varying2,
            blr_cstring,
            blr_cstring2  : Params.AddFieldType(Trim(AsString[1]), uftChar);
            blr_float,
            blr_d_float   : Params.AddFieldType(Trim(AsString[1]), uftFloat);
            blr_short     : Params.AddFieldType(Trim(AsString[1]), uftSmallint);
            blr_long      : Params.AddFieldType(Trim(AsString[1]), uftInteger);
            blr_quad      : Params.AddFieldType(Trim(AsString[1]), uftQuad);
            blr_double    : Params.AddFieldType(Trim(AsString[1]), uftDoublePrecision);
            blr_timestamp : Params.AddFieldType(Trim(AsString[1]), uftTimestamp);
            blr_blob,
            blr_blob_id   : Params.AddFieldType(Trim(AsString[1]), uftBlob);
            blr_sql_date  : Params.AddFieldType(Trim(AsString[1]), uftDate);
            blr_sql_time  : Params.AddFieldType(Trim(AsString[1]), uftTime);
            blr_int64     : Params.AddFieldType(Trim(AsString[1]), uftInt64);
          {$IFDEF IB7_UP}
            blr_boolean_dtype : Params.AddFieldType(Trim(AsString[1]), uftBoolean);
          {$ENDIF}
          else
            // shouldn't occur but ...
            raise Exception.Create(EUIB_UNEXPECTEDERROR);
          end
        end else
          inc(r);
        Next;
      end;
      if (Params.FieldCount > 0) then
      begin
        FParsedSQL := ' (';
        Str        := ' (';
        for i := 0 to Params.FieldCount - 1 do
        begin
          FParsedSQL := FParsedSQL + '?,';
          Str        := Str        + ':'+ Params.FieldName[i] +','
        end;
        FParsedSQL[Length(FParsedSQL)] := ')';
        Str[Length(Str)] := ')';
        if ((r > 0) and forSelect) then
          begin
            FParsedSQL := 'SELECT * FROM ' + SQLQuote(StoredProc) + FParsedSQL;
            FSQL.Text  := 'SELECT * FROM ' + SQLQuote(StoredProc) + Str;
          end else
          begin
            FParsedSQL := 'EXECUTE PROCEDURE ' + SQLQuote(StoredProc) + FParsedSQL;
            FSQL.Text  := 'EXECUTE PROCEDURE ' + SQLQuote(StoredProc) + Str;
          end;
      end else
        begin
          if r > 0 then
            FParsedSQL := 'SELECT * FROM ' + SQLQuote(StoredProc) else
            FParsedSQL := 'EXECUTE PROCEDURE ' + SQLQuote(StoredProc);
          FSQL.Text := FParsedSQL;
        end;
    except
      FParsedSQL := '';
      Params.Clear;
      InternalClose(FOnError, False);
      raise;
    end;
  finally
    InternalClose(etmStayIn, True);
    TStringList(FSQL).OnChange := DoSQLChange;
  end;
end;

{ TJvUIBTransaction }

constructor TJvUIBTransaction.Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent){$ENDIF};
begin
  inherited;
  FOptions     := [tpConcurrency,tpWait,tpWrite];
  FTrHandle := nil;
  FStatements  := 0;
  FDataBases   := TList.Create;
  FAutoRetain  := False;
  FAutoStart   := True;
  FAutoStop    := True;
  FDefaultAction := etmCommit;
{$IFDEF FB20_UP}
  FLockTimeout := 0;
{$ENDIF}
end;

destructor TJvUIBTransaction.Destroy;
begin
  ClearSQLComponents;
  Close(etmDefault, True);
  ClearDataBases;
  FDataBases.Free;
  inherited;
end;

{$IFNDEF UIB_NO_COMPONENT}
procedure TJvUIBTransaction.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if ((AComponent is TJvUIBDataBase) and (Operation = opRemove)) then
    RemoveDatabase(TJvUIBDataBase(AComponent));
end;
{$ENDIF}

procedure TJvUIBTransaction.SetDataBase(const ADatabase: TJvUIBDataBase);
begin
  RemoveDatabase(FDataBase);
  AddDataBase(ADatabase);
  FDataBase := ADatabase;
end;

procedure TJvUIBTransaction.Close(const Mode: TEndTransMode; Auto: boolean);
var
  i: Integer;
begin
{$IFDEF UIBTHREADSAFE}
  lock;
  try
{$ENDIF}
    if (FStatements > 0) and (FSQLComponent <> nil) then
      for i := 0 to FSQLComponent.Count -1 do
        TJvUIBQuery(FSQLComponent.Items[i]).InternalClose(etmStayIn, Auto);
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
  EndTransaction(Mode, nil, Auto);
end;

function TJvUIBTransaction.GetStatements(const Index: Integer): TJvUIBStatement;
begin
  if FSQLComponent <> nil then
    Result := FSQLComponent.Items[Index] else
    raise EListError.CreateFmt(EUIB_INDEXERROR,[Index]);
end;

function TJvUIBTransaction.GetStatementsCount: Integer;
begin
  if FSQLComponent <> nil then
    Result := FSQLComponent.Count else
    Result := 0;
end;

procedure TJvUIBTransaction.ClearDataBases;
var i: Integer;
begin
  FDataBase := nil;
  for i := 0 to FDataBases.Count - 1 do
    TJvUIBDataBase(FDataBases[i]).RemoveTransaction(Self);
  FDataBases.Clear;
end;

function TJvUIBTransaction.GetDatabases(const Index: Integer): TJvUIBDataBase;
begin
  Result := FDataBases[Index];
end;

function TJvUIBTransaction.GetDatabasesCount: Integer;
begin
  Result := FDataBases.Count;
end;

procedure TJvUIBTransaction.BeginDataBase;
var i: Integer;
begin
  if (FDataBase = nil) then
    raise Exception.Create(EUIB_DATABASENOTDEF);
  for i := 0 to FDataBases.Count - 1 do
    TJvUIBDataBase(FDataBases[i]).Connected := True;
end;

procedure TJvUIBTransaction.BeginTransaction(Auto: boolean = True);
type
  TEBDynArray = array of TISCTEB;
var
  Buffer: Pointer;
  i: Integer;
  ATPB: string;
begin
  BeginDataBase;
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    with FDataBase.FLibrary do
    if (FTrHandle = nil) then
    begin
      If Auto and (not FAutoStart) then
        raise EUIBException.Create(EUIB_EXPLICITTRANS);

      if FDataBases.Count = 1 then
      begin
        TransactionStart(FTrHandle, FDataBase.FDbHandle, TPB);
      end else
      begin
        GetMem(Buffer,  SizeOf(TISCTEB) * FDataBases.Count);
        try
          ATPB := TPB;
          for i := 0 to FDataBases.Count - 1 do
            with TEBDynArray(Buffer)[i] do
            begin
              Handle  := @TJvUIBDatabase(FDataBases[i]).FDbHandle;
              Len     := Length(ATPB);
              Address := PChar(ATPB);
            end;
          TransactionStartMultiple(FTrHandle, FDataBases.Count, Buffer);
        finally
          FreeMem(Buffer);
        end;
      end;
      if Assigned(FOnStartTransaction) then
        FOnStartTransaction(Self);
    end;
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
end;

function TJvUIBTransaction.EndTransaction(ETM: TEndTransMode; From: TJvUIBStatement;
  Auto: boolean): boolean;
var i: Integer;
begin
  Result := False;
  // don't lock if it is not necessary
  if (ETM = etmStayIn) then Exit;
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    // Default Action
    if (ETM = etmDefault) then ETM := FDefaultAction;
    if (FTrHandle <> nil) then
      with FDataBase.FLibrary do
      try
        if Assigned(FOnEndTransaction) then
          FOnEndTransaction(Self, ETM);
       { If there is Statements alive I must keep handle only if FAutoRetain = True.}
        if (FStatements > 0) and FAutoRetain then
          case ETM of
            etmCommit   : ETM := etmCommitRetaining;
            etmRollback : ETM := etmRollbackRetaining;
          end else
            if (ETM in [etmCommit, etmRollback]) then
            begin
              if (FStatements > 0) and (FSQLComponent <> nil) then
                for i := 0 to FSQLComponent.Count -1 do
                  if (From <> FSQLComponent.Items[i]) then
                    TJvUIBQuery(FSQLComponent.Items[i]).InternalClose(etmStayIn, Auto);
            end;

        Assert( FAutoStop or (not Auto), EUIB_NOAUTOSTOP);

        case ETM of
          etmCommit            :
            begin
              TransactionCommit(FTrHandle);
              Result := True;
            end;
          etmCommitRetaining   : TransactionCommitRetaining(FTrHandle);
          etmRollback          :
            begin
              TransactionRollback(FTrHandle);
              Result := True;
            end;
          etmRollbackRetaining : TransactionRollbackRetaining(FTrHandle);
        end;
      except
        case ETM of
          etmCommit, etmRollback :
            TransactionRollback(FTrHandle);
          etmCommitRetaining, etmRollbackRetaining :
            TransactionRollbackRetaining(FTrHandle);
        end;
        raise;
      end;
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
end;

procedure TJvUIBTransaction.AddSQLComponent(Component: TJvUIBStatement);
begin
  if (FSQLComponent = nil) then
    FSQLComponent := TList.Create;
  FSQLComponent.Add(Component);
end;

procedure TJvUIBTransaction.ClearSQLComponents;
begin
  while (FSQLComponent <> nil) do
    TJvUIBQuery(FSQLComponent.Last).SetTransaction(nil);
end;

procedure TJvUIBTransaction.RemoveSQLComponent(Component: TJvUIBStatement);
begin
  if (FSQLComponent <> nil) then
  begin
    FSQLComponent.Remove(Component);
    if (FSQLComponent.Count = 0) then
    begin
      FSQLComponent.free;
      FSQLComponent := nil;
    end;
  end;
end;

{$IFDEF UIBTHREADSAFE}
procedure TJvUIBTransaction.Lock;
var i: Integer;
begin
  inherited;
  for i := 0 to FDataBases.Count - 1 do
    TJvUIBDataBase(FDataBases[i]).Lock;
end;
{$ENDIF}

{$IFDEF UIBTHREADSAFE}
procedure TJvUIBTransaction.UnLock;
var i: Integer;
begin
  for i := 0 to FDataBases.Count - 1 do
    TJvUIBDataBase(FDataBases[i]).UnLock;
  inherited;
end;
{$ENDIF}

procedure TJvUIBTransaction.AddDataBase(ADataBase: TJvUIBDataBase);
var i: Integer;
begin
  if (ADataBase <> nil) then
  begin
    for i := 0 to FDataBases.Count - 1 do
      if FDataBases[i] = ADataBase then
        Exit;
    Close(etmDefault, True);
    FDataBases.Add(ADataBase);
    ADataBase.AddTransaction(Self);
    FSQLDialect := ADatabase.SQLDialect;
  end;
end;

procedure TJvUIBTransaction.RemoveDatabase(ADataBase: TJvUIBDataBase);
var
  i: Integer;
begin
  if (ADataBase <> nil) then
  begin
    if ADataBase = FDataBase then
      FDataBase := nil;
    for i := 0 to FDataBases.Count - 1 do
      if FDataBases[i] = ADataBase then
      begin
        Close(etmDefault, True);
        ADataBase.RemoveTransaction(Self);
        FDataBases.Delete(i);
        Exit;
      end;
  end;
end;

procedure TJvUIBTransaction.RemoveDatabase(Index: Integer);
begin
  with TJvUIBDataBase(FDataBases[Index]) do
  begin
    Close(etmDefault, True);
    RemoveTransaction(Self);
    FDataBases.Delete(Index);
  end;
end;

procedure TJvUIBTransaction.Commit;
begin
  EndTransaction(etmCommit, nil, False);
end;

procedure TJvUIBTransaction.CommitRetaining;
begin
  EndTransaction(etmCommitRetaining, nil, False);
end;

procedure TJvUIBTransaction.RollBack;
begin
  EndTransaction(etmRollback, nil, False);
end;

procedure TJvUIBTransaction.RollBackRetaining;
begin
  EndTransaction(etmRollbackRetaining, nil, False);
end;

{$IFDEF IB71_UP}
procedure TJvUIBTransaction.SavepointRelease(const Name: string);
begin
  BeginTransaction;
  FDataBase.FLibrary.SavepointRelease(FTrHandle, Name);
end;

procedure TJvUIBTransaction.SavepointRollback(const Name: string; Option: Word = 0);
begin
  BeginTransaction;
  FDataBase.FLibrary.SavepointRollback(FTrHandle, Name, Option);
end;

procedure TJvUIBTransaction.SavepointStart(const Name: string);
begin
  BeginTransaction;
  FDataBase.FLibrary.SavepointStart(FTrHandle, Name);
end;
{$ENDIF}

function TJvUIBTransaction.GetInTransaction: boolean;
begin
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    Result := (FTrHandle <> nil);
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
end;

function TJvUIBTransaction.TPB: string;
var
  tp: TTransParam;
procedure ParseStrOption(const code: Char; const Value: string);
var
  P, Start: PChar;
  S: string;
begin
  P := Pointer(Value);
  if P <> nil then
    while P^ <> #0 do
    begin
      Start := P;
      while not (P^ in [#0, ';']) do Inc(P);
      if (P - Start) > 0 then
      begin
        SetString(S, Start, P - Start);
        Result := Result + code + Char(P - Start) + S;
      end;
      if P^ =';' then inc(P);
    end;
end;
begin
  if FOptions = [tpConcurrency,tpWait,tpWrite] then
    result := ''
  else
    begin
      Result := isc_tpb_version3;
      for tp := Low(TTransParam) to High(TTransParam) do
        if (tp in FOptions) then
        begin
          case tp of
            tpLockRead    : ParseStrOption(Char(Ord(tp)+1), FLockRead);
            tpLockWrite   : ParseStrOption(Char(Ord(tp)+1), FLockWrite);
          {$IFDEF FB20_UP}
            tpLockTimeout : Result := Result + Char(Ord(tp)+1) + PChar(@FLockTimeout)[0] + PChar(@FLockTimeout)[1];
          {$ENDIF}
          else
            Result := Result + Char(Ord(tp)+1);
          end;
        end;
    end;
end;

function TJvUIBTransaction.GetOptions: TTransParams;
begin
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    Result := FOptions;
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
end;

procedure TJvUIBTransaction.SetOptions(const Value: TTransParams);
begin
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    FOptions := Value;
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
end;

function TJvUIBTransaction.GetLockRead: string;
begin
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    Result := FLockRead;
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
end;

function TJvUIBTransaction.GetLockWrite: string;
begin
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    Result := FLockWrite;
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
end;

procedure TJvUIBTransaction.SetLockRead(const Value: string);
begin
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    FLockRead := Value;
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
end;

procedure TJvUIBTransaction.SetLockWrite(const Value: string);
begin
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    FLockWrite := Value;
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
end;

function TJvUIBTransaction.GetDataBase: TJvUIBDataBase;
begin
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    Result := FDataBase;
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
end;

function TJvUIBTransaction.GetAutoRetain: boolean;
begin
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    Result := FAutoRetain;
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
end;

procedure TJvUIBTransaction.SetAutoRetain(const Value: boolean);
begin
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    FAutoRetain := Value;
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
end;

procedure TJvUIBTransaction.StartTransaction;
begin
  BeginTransaction(False);
end;

procedure TJvUIBTransaction.SetDefaultAction(const Value: TEndTransMode);
begin
  Assert(Value in [etmCommit, etmRollBack], 'Commit or Rollback only.');
  FDefaultAction := Value;
end;

procedure TJvUIBTransaction.ExecuteImmediate(const sql: string);
begin
{$IFDEF UIBTHREADSAFE}
  Lock;
  try
{$ENDIF}
    BeginTransaction;
    FDataBase.FLibrary.DSQLExecuteImmediate(FDataBase.FDbHandle,
      FTrHandle, sql, FSQLDialect);
{$IFDEF UIBTHREADSAFE}
  finally
    UnLock;
  end;
{$ENDIF}
end;

{ TJvUIBComponent }

constructor TJvUIBComponent.Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent){$ENDIF};
begin
  inherited;
{$IFDEF UIBTHREADSAFE}
  FCriticalsection := TCriticalSection.Create;
{$ENDIF}
end;

destructor TJvUIBComponent.Destroy;
begin
{$IFDEF UIBTHREADSAFE}
  FCriticalsection.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF UIBTHREADSAFE}
procedure TJvUIBComponent.Lock;
begin
  FCriticalsection.Enter;
end;
{$ENDIF}

{$IFDEF UIBTHREADSAFE}
procedure TJvUIBComponent.UnLock;
begin
  FCriticalsection.Leave;
end;
{$ENDIF}

{ TJvUIBService }

procedure TJvUIBService.BeginService;
var SPB: string;
  procedure AddString(id: char; const Value: string);
  begin
    if (Value <> '') then
      SPB := SPB + id + Char(length(Value)) + Value;
  end;
begin
  SPB := isc_spb_version + isc_spb_current_version;
  AddString(isc_spb_user_name, FUserName);
  AddString(isc_spb_password, FPassWord);
  FLibrary.Load(FLiBraryName);
  case FProtocol of
    proLocalHost : FLibrary.ServiceAttach('service_mgr', FHandle, SPB);
    proTCPIP     : FLibrary.ServiceAttach(Fhost + ':service_mgr', FHandle, SPB);
    proNetBEUI   : FLibrary.ServiceAttach('\\'+ Fhost + '\service_mgr', FHandle, SPB);
  end;
end;

constructor TJvUIBService.Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent){$ENDIF};
begin
  inherited;
  FLibrary := TUIBLibrary.Create;
  FLiBraryName := GDS32DLL;
  FProtocol := proLocalHost;
  FHandle := nil;
end;

destructor TJvUIBService.Destroy;
begin
  inherited;
  FLibrary.Free;
end;

procedure TJvUIBService.SetLibraryName(const Lib: String);
begin
  FLibrary.UnLoad;
  FLiBraryName := Lib;
end;

procedure TJvUIBService.EndService;
begin
  FLibrary.ServiceDetach(FHandle);
end;

function TJvUIBService.CreateParam(code: char;
  const Value: string): string;
var Len: Word;
begin
  Len := Length(Value);
  if len > 0 then
    Result := code + PChar(@Len)[0] + PChar(@Len)[1] + Value else
    result := '';
end;

function TJvUIBService.CreateParam(code: char; Value: Integer): string;
begin
  result := code + PChar(@Value)[0] + PChar(@Value)[1] + PChar(@Value)[2] + PChar(@Value)[3];
end;

{ TJvUIBBackupRestore }

constructor TJvUIBBackupRestore.Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent){$ENDIF};
begin
  inherited;
  FBackupFiles := TStringList.Create;
  FVerbose := false;
end;

destructor TJvUIBBackupRestore.Destroy;
begin
  FBackupFiles.Free;
  inherited;
end;

procedure TJvUIBBackupRestore.SetBackupFiles(const Value: TStrings);
begin
  FBackupFiles.Assign(Value);
end;

procedure TJvUIBBackupRestore.Run;
var
  Buffer: string;
  Len: Word;
begin
  BeginService;
  try
    FLibrary.ServiceStart(FHandle, CreateStartSPB);
    if FVerbose then
    begin
      SetLength(Buffer, 1024);
      while true do
      begin
        FLibrary.ServiceQuery(FHandle, '', isc_info_svc_line, Buffer);
        if (Buffer[1] <> isc_info_svc_line) then
          raise Exception.Create(EUIB_UNEXPECTEDERROR);
        Len := PWord(@Buffer[2])^;
        if (len > 0)  then
        begin
          if Assigned(FOnVerbose) then
            FOnVerbose(self, copy(Buffer, 4, len));
        end else
          Break;
      end;
    end;
  finally
    EndService;
  end;
end;

{ TJvUIBBackup }

function TJvUIBBackup.CreateStartSPB: string;
var
  Len: Word;
  i: Integer;
  FileName: string;
  FileLength: Integer;
  function GetValue(Index: Integer): string;
  begin
    if Index >= 0 then
      Result := Copy(FBackupFiles.Strings[Index], Length(FBackupFiles.Names[Index]) + 2, MaxInt) else
      Result := '';
  end;
begin
  // backup service   ibservices
  Result := isc_action_svc_backup;

  // DB Name
  Result := Result + isc_spb_dbname;
  Len := Length(FDatabase);
  Result := Result + PChar(@Len)[0] + PChar(@Len)[1];
  Result := Result + FDatabase;

  for i := 0 to FBackupFiles.Count - 1 do
  begin
    FileName := FBackupFiles.Names[i];
    if FileName = '' then
      FileName := FBackupFiles[i];
    if FileName <> '' then
    begin
      // Backup file
      Result := Result + isc_spb_bkp_file;
      Len := Length(FileName);
      Result := Result + PChar(@Len)[0] + PChar(@Len)[1];
      Result := Result + FileName;
      // Backup file length
      if TryStrToInt(GetValue(i), FileLength) then
      begin
        Result := Result + isc_spb_bkp_length;
        Result := Result + PChar(@FileLength)[0] + PChar(@FileLength)[1] +
          PChar(@FileLength)[2] + PChar(@FileLength)[3];
      end;
    end;
  end;

  if FVerbose then
    Result := Result + isc_spb_verbose;

  if (FOptions <> []) then
    Result := Result + isc_spb_options + PChar(@FOptions)^ + #0#0#0;
end;

{ TJvUIBRestore }

constructor TJvUIBRestore.Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent){$ENDIF};
begin
  inherited;
  FOptions := [roCreateNewDB];
  FPageSize := 0;
end;

function TJvUIBRestore.CreateStartSPB: string;
var
  Len: Word;
  i: Integer;
  FileName: string;
  Opts: Cardinal;
begin
  // backup service   ibservices
  Result := isc_action_svc_restore;

  for i := 0 to FBackupFiles.Count - 1 do
  begin
    FileName := FBackupFiles[i];
    if FileName <> '' then
    begin
      // Backup file
      Result := Result + isc_spb_bkp_file;
      Len := Length(FileName);
      Result := Result + PChar(@Len)[0] + PChar(@Len)[1];
      Result := Result + FileName;
    end;
  end;

  // DB Name
  Result := Result + isc_spb_dbname;
  Len := Length(FDatabase);
  Result := Result + PChar(@Len)[0] + PChar(@Len)[1];
  Result := Result + FDatabase;

  if FVerbose then
    Result := Result + isc_spb_verbose;

  if (FOptions <> []) then
  begin
    Opts := PByte(@FOptions)^ shl 8;
    Result := Result + isc_spb_options + PChar(@Opts)[0] +
      PChar(@Opts)[1] + PChar(@Opts)[2] + PChar(@Opts)[3];
  end;

  if FPageSize > 0 then
    Result := Result + isc_spb_res_page_size + PChar(@FPageSize)[0] +
      PChar(@FPageSize)[1] + PChar(@FPageSize)[2] + PChar(@FPageSize)[3];
end;

{ TJvUIBSecurity }

constructor TJvUIBSecurity.Create{$IFNDEF UIB_NO_COMPONENT}(aOwner: TComponent){$ENDIF};
begin
  inherited;
  FUserInfos := TObjectList.Create(True);
end;

destructor TJvUIBSecurity.Destroy;
begin
  inherited;
  FUserInfos.Free;
end;

procedure TJvUIBSecurity.AddUser;
begin
  RunAction(saAddUser);
end;

procedure TJvUIBSecurity.DeleteUser;
begin
  RunAction(saDeleteUser);
end;

procedure TJvUIBSecurity.ModifyUser;
begin
  RunAction(saModifyUser);
end;

procedure TJvUIBSecurity.DisplayUser;
begin
  RunAction(saDisplayUser);
end;

procedure TJvUIBSecurity.DisplayUsers;
begin
  RunAction(saDisplayUsers);
end;

procedure TJvUIBSecurity.ClearParams;
var
  P : Integer;
begin
  for P := Low(FIntegerParams) to High(FIntegerParams) do
    FIntegerParams[P] := 0;
  for P := Low(FStringParams) to High(FStringParams) do
    FStringParams[P] := '';
  FModifiedParams := [];
end;

function TJvUIBSecurity.GetUserInfoCount: Integer;
begin
  Result := FUserInfos.Count;
end;

function TJvUIBSecurity.GetUserInfo(aIndex: Integer): TUserInfo;
begin
  Result := TUserInfo(FUserInfos[aIndex]);
end;

function TJvUIBSecurity.GetIntegerParam(aParam: Integer): Integer;
begin
  Result := FIntegerParams[aParam];
end;

function TJvUIBSecurity.GetStringParam(aParam: Integer): string;
begin
  Result := FStringParams[aParam];
end;

procedure TJvUIBSecurity.SetIntegerParam(aParam: Integer; const aValue: Integer);
begin
  FIntegerParams[aParam] := aValue;
  Include(FModifiedParams, TSecurityParam(aParam));
end;

procedure TJvUIBSecurity.SetStringParam(aParam: Integer; const aValue: string);
begin
  FStringParams[aParam] := aValue;
  Include(FModifiedParams, TSecurityParam(aParam));
end;

procedure TJvUIBSecurity.RunAction(aAction: TSecurityAction);
const
  IscActions : array[TSecurityAction] of Char = (
    isc_action_svc_add_user, isc_action_svc_delete_user, isc_action_svc_modify_user,
    isc_action_svc_display_user, isc_action_svc_display_user);
  IscParams : array[TSecurityParam] of Char = (
    isc_spb_sql_role_name, isc_spb_sec_username, isc_spb_sec_password,
    isc_spb_sec_firstname, isc_spb_sec_middlename, isc_spb_sec_lastname,
    isc_spb_sec_userid, isc_spb_sec_groupid);
var
  StartParams, Buffer: string; Position: Integer;

  procedure AddStringParam(P: TSecurityParam);
  var
    Value : string;
    Len : UShort;
  begin
    Value := GetStringParam(ord(P));
    Len := Length(Value);
    StartParams := StartParams + IscParams[P] + PChar(@Len)[0] + PChar(@Len)[1] + Value;
  end;

  procedure AddIntegerParam(P: TSecurityParam);
  var
    Value : Integer;
  begin
    Value := GetIntegerParam(ord(P));
    StartParams := StartParams + IscParams[P] + PChar(@Value)[0] + PChar(@Value)[1] +
                                                PChar(@Value)[2] + PChar(@Value)[3];
  end;

  procedure ParseCode(Code: Char);
  begin
    if (Buffer[Position] <> Code) then
      raise Exception.Create(EUIB_SERVICESPARSING);
    Inc(Position);
  end;

  function ParseString(P: TSecurityParam): string;
  var
    Len: UShort;
  begin
    ParseCode(IscParams[P]);
    Len := PWord(@Buffer[Position])^;
    Position := Position + 2;
    SetString(Result, PChar(@Buffer[Position]), Len);
    Position := Position + Len;
  end;

  function ParseInteger(P: TSecurityParam): Integer;
  begin
    ParseCode(IscParams[P]);
    Result := PInteger(@Buffer[Position])^;
    Position := Position + 4;
  end;

var
  P: TSecurityParam;
  S: string;
  U: TUserInfo;
begin
  StartParams := IscActions[aAction];

  if spRole in FModifiedParams then
    AddStringParam(spRole);

  if aAction <> saDisplayUsers then
  begin { actions, other than saDisplayUsers, require a valid user name }
    S := UserName;
    if (S = '') or (Pos(' ', S) > 0) then
      raise Exception.CreateFmt(EUIB_INVALIDUSERNAME, [S]);
    AddStringParam(spUser);
  end;

  if aAction in [saAddUser, saModifyUser] then
  { only saAddUser and saModifyUser actions require these,
    and only when explicitly set }
  begin
    for P := spPass to spLastName do
      if P in FModifiedParams then
        AddStringParam(P);
    for P := spUserID to spGroupID do
      if P in FModifiedParams then
        AddIntegerParam(P);
  end;

  BeginService;
  try
    FLibrary.ServiceStart(FHandle, StartParams);
    if aAction in [saDisplayUser, saDisplayUsers] then
    begin
      SetLength(Buffer, 32000);
      FLibrary.ServiceQuery(FHandle, '', Char(isc_info_svc_get_users), Buffer);
      Position := 1;
      ParseCode(isc_info_svc_get_users);
      FUserInfos.Clear;
      Inc(Position, 2); // skip combined length info
      while Buffer[Position] <> Char(isc_info_end) do
      begin
        U := TUserInfo.Create;
        with U do
        try
          UserName := ParseString(spUser);
          FirstName := ParseString(spFirstName);
          MiddleName := ParseString(spMiddleName);
          LastName := ParseString(spLastName);
          UserID := ParseInteger(spUserID);
          GroupID := ParseInteger(spGroupID);
          FUserInfos.Add(U);
        except
          U.Free;
          raise;
        end;
      end;
    end;
  finally
    EndService;
  end;
  ClearParams;
end;

{ TJvUIBScript }

constructor TJvUIBScript.Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent){$ENDIF};
begin
  inherited;
  FQuery := TJvUIBQuery.Create{$IFNDEF UIB_NO_COMPONENT}(nil){$ENDIF};
  FQuery.ParseParams := False;
  FScript := TStringList.Create;
  FAutoDDL := True;
end;

destructor TJvUIBScript.Destroy;
begin
  FQuery.Free;
  FScript.Free;
  inherited;
end;

procedure TJvUIBScript.ExecuteScript;
var
  Parser: TJVUIBSQLParser;
  st: TSQLStatement;
  j: TCharacterSet;
  Dialect: Integer;
  TrHandle: IscTrHandle;
  str: string;
  Found: boolean;
  procedure CheckDatabase;
  begin
    if (Transaction = nil) then
       raise Exception.Create(EUIB_TRANSACTIONNOTDEF);
  end;

begin
  Parser := TJVUIBSQLParser.Create(FScript);
  Parser.OnComment := FOnComment;
  try
    while true do
    begin
      st := Parser.NextStatement;
      if st = ssEOF then
        Break;
      if Assigned(FOnParse) then
        FOnParse(self, st, Parser.Statement);
      case st of
        ssSetSqlDialect:
          begin
            CheckDatabase;
            if TryStrToInt(Parser.Params.Values['DIALECT'], Dialect) then
              FQuery.FindDataBase.SQLDialect := Dialect else
              raise Exception.Create(EUIB_PARSESQLDIALECT);
          end;
        ssSetNames:
          begin
            CheckDatabase;
            str := Parser.Params.Values['CHARACTER'];
            Found := false;
            for j := low(TCharacterSet) to high(TCharacterSet) do
              if (CompareText(CharacterSetStr[j], str) = 0) then
              begin
                FQuery.FindDataBase.CharacterSet := j;
                found := true;
                Break;
              end;
            if not found then
              raise Exception.Create(EUIB_PARSESETNAMES);
          end;
        ssCreateDatabase:
          begin
            CheckDatabase;
            FQuery.FindDataBase.Connected := False;
            TrHandle := nil;
            with FQuery.FindDataBase do
            begin
              FLibrary.Load(FLiBraryName);
              // I MUST provide the real DB Handle (not nil)
              // because altering forein key can fail otherwise.
            {$IFDEF UIBTHREADSAFE}
              FQuery.FindDataBase.Lock;
              try
            {$ENDIF}
                FLibrary.DSQLExecuteImmediate(
                  FDbHandle, TrHandle, Parser.Statement, SQLDialect);
            {$IFDEF UIBTHREADSAFE}
              finally
                FQuery.FindDataBase.UnLock;
              end;
            {$ENDIF}
            end;
            FQuery.FindDataBase.DatabaseName := Parser.Params.Values['DATABASE'];
            FQuery.FindDataBase.UserName := Parser.Params.Values['USER'];
            FQuery.FindDataBase.PassWord := Parser.Params.Values['PASSWORD'];
          end;
        ssConnect:
          with FQuery.FindDataBase do
          begin
            Connected := False;
            DatabaseName := Parser.Params.Values['DATABASE'];
            UserName     := Parser.Params.Values['USER'];
            PassWord     := Parser.Params.Values['PASSWORD'];
            Connected := True;
          end;
        ssAutoDDL:
          begin
            if (Parser.Params.Values['AUTODDL'] = 'ON') then
              FAutoDDL := True else
              FAutoDDL := False;
          end;
        ssCommit:
          begin
            Transaction.Commit;
          end;
        ssRollback:
          begin
            Transaction.RollBack;
          end;
      {$IFDEF IB71_UP}
        ssSetSavepoint:
          Transaction.SavepointStart(Parser.Params.Values['SYMBOL']);
        ssReleaseSavepoint:
          Transaction.SavepointRelease(Parser.Params.Values['SYMBOL']);
        ssUndoSavepoint:
          Transaction.SavepointRollback(Parser.Params.Values['SYMBOL']);
      {$ENDIF}
        ssSelect, // perhaps a select statement execute a procedure ...
        ssInsertInto,
        ssDelete,
        ssUpdate:
          begin
            FQuery.SQL.Text := trim(Parser.Statement);
            FQuery.ExecSQL;
            FQuery.Close(etmStayIn);
          end;
      else
        // DDL ...
        FQuery.SQL.Text := trim(Parser.Statement);
        FQuery.ExecSQL; // faster for ddl
        if FAutoDDL then
          FQuery.Close(etmCommit) else
          FQuery.Close(etmStayIn);
      end;
    end;
  finally
    FQuery.Close(etmStayIn);
    Parser.Free;
  end;
end;

function TJvUIBScript.GetTransaction: TJvUIBTransaction;
begin
  Result := FQuery.Transaction;
end;

procedure TJvUIBScript.SetScript(const Value: TStrings);
begin
  FScript.Assign(Value);
end;

procedure TJvUIBScript.SetTransaction(const Value: TJvUIBTransaction);
begin
  FQuery.Transaction := Value;
end;

{ TMetaDataOptions }

constructor TMetaDataOptions.Create;
begin
  inherited;
  FObjects := ALLOBjects;
  FTables := ALLTables;
  FViews := ALLViews;
  FProcedures := ALLProcedures;
  FUDFs := ALLUDFs;
  FRoles := ALLRoles;
  FSysInfos := False;
end;

{ TJvUIBRepair }

function TJvUIBRepair.CreateStartSPB: string;
var
  Len: Word;
  Param: byte;
begin
  result := isc_action_svc_repair;

  // DB Name
  Result := Result + isc_spb_dbname;
  Len := Length(FDatabase);
  Result := Result + PChar(@Len)[0] + PChar(@Len)[1];
  Result := Result + FDatabase;

  if (roSweepDB in FOptions) then
    Param := isc_spb_rpr_sweep_db else
    Param := 0;
  if (roValidateDB in FOptions) then
    Param := Param or isc_spb_rpr_validate_db;

  if (Param <> 0) then
    Result := Result + isc_spb_options + char(Param) + #0#0#0;

  if (roListLimboTrans in FOptions) then
    Param := isc_spb_rpr_list_limbo_trans else
    Param := 0;
  if (roCheckDB in FOptions) then
    Param := Param or isc_spb_rpr_check_db;
  if (roIgnoreChecksum in FOptions) then
    Param := Param or isc_spb_rpr_ignore_checksum;
  if (roKillShadows in FOptions) then
    Param := Param or isc_spb_rpr_kill_shadows;
  if (roMendDB in FOptions) then
    Param := Param or isc_spb_rpr_mend_db;
  if (roValidateFull in FOptions) then
  begin
    Param := Param or isc_spb_rpr_full;
     if not (roMendDB in FOptions) then
       Param := Param or isc_spb_rpr_validate_db;
  end;
  if (Param <> 0) then
    Result := Result + isc_spb_options + char(Param) + #0#0#0;

end;

procedure TJvUIBRepair.Run;
begin
  BeginService;
  try
    FLibrary.ServiceStart(FHandle, CreateStartSPB);
  finally
    EndService;
  end;
end;

{ TJvUIBEvents }

constructor TJvUIBEvents.Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent){$ENDIF};
begin
  inherited;
  FSyncMainThread := true;
  FThreadException := False;
  FDatabase := nil;
  FAutoRegister := False;
  FEvents := TStringList.Create;
  FThreads := TList.Create;
end;

destructor TJvUIBEvents.Destroy;
begin
  try
    if Registered then
      UnRegisterEvents;
  except
  end;
  if Assigned(FDatabase) then
    FDatabase.RemoveEventNotifier(Self);
  FThreads.Free;
  FEvents.Free;
  inherited Destroy;
end;

{$IFNDEF UIB_NO_COMPONENT}
procedure TJvUIBEvents.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDatabase) then
  begin
    if Registered then
      UnRegisterEvents;
    FDatabase := nil;
  end;
end;
{$ENDIF}

procedure TJvUIBEvents.RegisterEvents;
var
  i: Integer;
begin
{$IFNDEF UIB_NO_COMPONENT}
  if (csDesigning in ComponentState) then
    Exit;
{$ENDIF}
  if (FThreads.Count = 0) then
  begin
    if (FEvents.Count > 0) then
    begin
      for i := 0 to ((FEvents.Count - 1) div 15) do
        FThreads.Add(TJvUIBEventThread.Create(Self, i, FSyncMainThread));
    end;
  end;
end;

procedure TJvUIBEvents.SetEvents(value: TStrings);
begin
  FEvents.Assign(value);
end;

procedure TJvUIBEvents.SetDatabase(value: TJvUIBDataBase);
var
  WasRegistered: boolean;
begin
  if (Value <> FDatabase) then
  begin
{$IFNDEF UIB_NO_COMPONENT}
    if (csDesigning in ComponentState) then
      FDatabase := Value else
{$ENDIF}
    begin
      WasRegistered := Registered;
      if WasRegistered then
        UnRegisterEvents;
      try
        if Assigned(FDatabase) then
          FDatabase.RemoveEventNotifier(Self);
        FDatabase := Value;
        if Assigned(FDatabase) then
          FDatabase.AddEventNotifier(Self);
      finally
        if WasRegistered and Assigned(FDatabase) then
          RegisterEvents;
      end;
    end;
  end;
end;

procedure TJvUIBEvents.SetRegistered(const Value : boolean);
begin
  FRegistered := Value;
{$IFNDEF UIB_NO_COMPONENT}
  if (csDesigning in ComponentState) then
    Exit;
{$ENDIF}
  if (Value) then
    RegisterEvents else
    UnRegisterEvents;
end;

procedure TJvUIBEvents.UnregisterEvents;
var
  i: Integer;
begin
{$IFNDEF UIB_NO_COMPONENT}
  if (csDesigning in ComponentState) then
    Exit;
{$ENDIF}
  for i := FThreads.Count - 1 downto 0 do
    with TJvUIBEventThread(FThreads[i]) do
    begin
      FThreads.Delete(i);
      if not Terminated then
        free;
    end;
end;

procedure TJvUIBEvents.SetAutoRegister(const Value: boolean);
begin
  if FAutoRegister <> Value then
  begin
    FAutoRegister := Value;
    if FAutoRegister and (not Registered) and
       Assigned(FDatabase) and FDatabase.Connected then
      RegisterEvents;
  end;
end;

{ TJvUIBEventThread }

function TJvUIBEventThread.FindDataBase: TJvUIBDataBase;
begin
  if (FOwner <> nil) and (FOwner.Database <> nil) then
    result := FOwner.Database else
    raise Exception.Create(EUIB_DATABASENOTDEF);
end;

procedure EventCallback(UserData: Pointer; Length: Smallint; Updated: PChar); cdecl;
begin
  if (Assigned(UserData) and Assigned(Updated)) then
  with TJvUIBEventThread(UserData) do
  begin
    Move(Updated^, FResultBuffer^, Length);
    FQueueEvent := True;
    FSignal.SetEvent;
  end;
end;

procedure TJvUIBEventThread.SyncOnEvent;
begin
  FOwner.FOnEvent(FOwner, FOwner.FEvents[((FBlock * 15) + FCurrentEvent)],
    FStatusVector[FCurrentEvent], FCancelAlerts)
end;

procedure TJvUIBEventThread.SyncHandleException;
begin
  ShowException(FExceptObject, FExceptAddr);
end;

function TJvUIBEventThread.HandleException: boolean;
begin
  if (not FOwner.FThreadException) then
  begin
    Result := True;
    FOwner.FThreadException := True;
    FExceptObject := ExceptObject;
    FExceptAddr := ExceptAddr;
    try
      if not (FExceptObject is EAbort) then
        if FSyncMainThread then
          Synchronize(SyncHandleException) else
          SyncHandleException;
    finally
      FExceptObject := nil;
      FExceptAddr := nil;
    end;
  end else
    Result := False;
end;

procedure TJvUIBEventThread.Execute;
var
  arr: array[0..14] of PChar;
  count, i: integer;
  first: boolean;
begin
  FEventBuffer := nil;
  FResultBuffer := nil;
  FEventBufferLen := 0;
  first := True;
  FQueueEvent := false;
  count := (FOwner.FEvents.Count - (FBlock * 15));
  if count > 15 then
    count := 15;
  for i := 0 to count - 1 do
    Arr[i] := PChar(FOwner.FEvents[i + FBlock * 15]);
  with FindDataBase.FLibrary do
    FEventBufferLen := EventBlock(FEventBuffer, FResultBuffer, count,
      Arr[0], Arr[1], Arr[2], Arr[3], Arr[4], Arr[5], Arr[6], Arr[7], Arr[8],
      Arr[9], Arr[10], Arr[11], Arr[12], Arr[13], Arr[14]);
  FSignal.ResetEvent;
  if FSyncMainThread then
    Synchronize(SyncEventQueue) else
    SyncEventQueue;
  try
    while not Terminated do
    begin
      FSignal.WaitFor(INFINITE);
      if (FQueueEvent or first) then
      begin
        FindDataBase.FLibrary.EventCounts(FStatusVector, FEventBufferLen,
          FEventBuffer, FResultBuffer);
        if (Assigned(FOwner.FOnEvent) and (not first)) then
        begin
          FCancelAlerts := false;
          FCurrentEvent := 0;
          while (FCurrentEvent < count) do
          begin
            if (FStatusVector[FCurrentEvent] <> 0) then
              if FSyncMainThread then
                Synchronize(SyncOnEvent) else
                SyncOnEvent;
            inc(FCurrentEvent);
          end;
        end;
        first := False;
        FQueueEvent := False;
        FSignal.ResetEvent;
        if FSyncMainThread then
          Synchronize(SyncEventQueue) else
          SyncEventQueue;
      end;
    end;
    ReturnValue := 0;
  except
    if HandleException then
      ReturnValue := 1 else
      ReturnValue := 0;
  end;
end;

constructor TJvUIBEventThread.Create(Owner: TJvUIBEvents;
  Block: Integer; SyncMainThread: boolean);
begin
  inherited Create(True);
  FSyncMainThread := SyncMainThread;
  FCurrentEvent := 0;
  FEventID := 0;
  FOwner := Owner;
  FExceptObject := nil;
  FExceptAddr := nil;
  FCancelAlerts := false;
  FSignal := TSimpleEvent.Create;
  FBlock := Block;
  OnTerminate := SyncTerminate;
  Resume;
end;

destructor TJvUIBEventThread.Destroy;
var
  db: TJvUIBDataBase;
begin
  Terminate;
  FSignal.SetEvent;
  WaitFor;
  try
    db := FindDataBase;
    with db, Flibrary do
    begin
    {$IFDEF UIBTHREADSAFE}
      db.Lock;
      try
    {$ENDIF}
        EventCancel(FDbHandle, FEventID);
    {$IFDEF UIBTHREADSAFE}
      finally
        db.UnLock;
      end;
    {$ENDIF}
      IscFree(FEventBuffer);
      IscFree(FResultBuffer);
    end;
  except
    if HandleException then
      ReturnValue := 1 else
      ReturnValue := 0;
  end;
  FSignal.Free;
  inherited Destroy;
end;

procedure TJvUIBEventThread.SyncEventQueue;
var db: TJvUIBDataBase;
begin
  try
    db := FindDataBase;
    with db, FLibrary do
  {$IFDEF UIBTHREADSAFE}
    begin
      db.Lock;
      try
  {$ENDIF}
        EventQueue(FdbHandle, FEventID, FEventBufferLen, FEventBuffer,
          @EventCallback, self);
  {$IFDEF UIBTHREADSAFE}
      finally
        db.UnLock;
      end;
    end;
  {$ENDIF}
  except
    on E : Exception do
      if Assigned(FOwner.FOnException) then
          FOwner.FOnException(E);
  end;
end;

procedure TJvUIBEventThread.SyncTerminate(sender: TObject);
var
  ThreadIdx: Integer;
begin
  with FOwner, FThreads do
  begin
    ThreadIdx := IndexOf(self);
    if (ThreadIdx > -1) then
      Delete(ThreadIdx);
    if (ReturnValue = 1) then
    begin
      if Registered then
        UnRegisterEvents;
      FThreadException := False;
    end
  end;
end;

{ TJvUIBConfig }

procedure TJvUIBConfig.ActivateShadow;
begin
  BeginService;
  try
    FLibrary.ServiceStart(FHandle, isc_action_svc_properties +
      CreateParam(isc_spb_dbname, FDatabaseName) +
      CreateParam(isc_spb_options, isc_spb_prp_activate));
  finally
    EndService;
  end;
end;

procedure TJvUIBConfig.BringDatabaseOnline;
begin
  BeginService;
  try
    FLibrary.ServiceStart(FHandle, isc_action_svc_properties +
      CreateParam(isc_spb_dbname, FDatabaseName) +
      CreateParam(isc_spb_options, isc_spb_prp_db_online));
  finally
    EndService;
  end;
end;

procedure TJvUIBConfig.SetAsyncMode(Value: Boolean);
const
  AsyncMode: array[boolean] of char =
    (isc_spb_prp_wm_sync, isc_spb_prp_wm_async);
begin
  BeginService;
  try
    FLibrary.ServiceStart(FHandle, isc_action_svc_properties +
      CreateParam(isc_spb_dbname, FDatabaseName) +
      isc_spb_prp_write_mode + AsyncMode[Value]);
  finally
    EndService;
  end;
end;

procedure TJvUIBConfig.SetDBSqlDialect(Value: Integer);
begin
  BeginService;
  try
    FLibrary.ServiceStart(FHandle, isc_action_svc_properties +
      CreateParam(isc_spb_dbname, FDatabaseName) +
      CreateParam(isc_spb_prp_set_sql_dialect, Value));
  finally
    EndService;
  end;
end;

procedure TJvUIBConfig.SetPageBuffers(Value: Integer);
begin
  BeginService;
  try
    FLibrary.ServiceStart(FHandle, isc_action_svc_properties +
      CreateParam(isc_spb_dbname, FDatabaseName) +
      CreateParam(isc_spb_prp_page_buffers, Value));
  finally
    EndService;
  end;
end;

procedure TJvUIBConfig.SetReadOnly(Value: Boolean);
const
  ReadOnly: array[boolean] of char =
    (isc_spb_prp_am_readwrite, isc_spb_prp_am_readonly);
begin
  BeginService;
  try
    FLibrary.ServiceStart(FHandle, isc_action_svc_properties +
      CreateParam(isc_spb_dbname, FDatabaseName) +
      isc_spb_prp_access_mode + ReadOnly[Value]);
  finally
    EndService;
  end;
end;

procedure TJvUIBConfig.SetReserveSpace(Value: Boolean);
const
  ReserveSpace: array[boolean] of char =
    (isc_spb_prp_res_use_full, isc_spb_prp_res);
begin
  BeginService;
  try
    FLibrary.ServiceStart(FHandle, isc_action_svc_properties +
      CreateParam(isc_spb_dbname, FDatabaseName) +
      isc_spb_prp_reserve_space + ReserveSpace[Value]);
  finally
    EndService;
  end;
end;

procedure TJvUIBConfig.SetSweepInterval(Value: Integer);
begin
  BeginService;
  try
    FLibrary.ServiceStart(FHandle, isc_action_svc_properties +
      CreateParam(isc_spb_dbname, FDatabaseName) +
      CreateParam(isc_spb_prp_sweep_interval, Value));
  finally
    EndService;
  end;
end;

procedure TJvUIBConfig.ShutdownDatabase(Options: TShutdownMode;
  Wait: Integer);
const
  ShutdownMode: array[TShutdownMode] of char =
    (isc_spb_prp_shutdown_db, isc_spb_prp_deny_new_transactions, isc_spb_prp_deny_new_attachments);
begin
  BeginService;
  try
    FLibrary.ServiceStart(FHandle, isc_action_svc_properties +
      CreateParam(isc_spb_dbname, FDatabaseName) +
      CreateParam(ShutdownMode[Options], Wait));
  finally
    EndService;
  end;
end;

{ TJvUIBServerInfo }

procedure TJvUIBServerInfo.GetServerInfo;
var
  Buffer: String;
  Code: Byte;
  Position: Integer;
  Value: Integer;
  DbName: String;

  function ParseByte: Byte;
  begin
    Result := PByte(@Buffer[Position])^;
    Inc(Position);
  end;

  function ParseWord: Word;
  begin
    Result := PWord(@Buffer[Position])^;
    Inc(Position, 2);
  end;

  function ParseInteger: Integer;
  begin
    Result := PInteger(@Buffer[Position])^;
    Inc(Position, 4);
  end;

  function ParseString: String;
  var
    Len: Word;
  begin
    Result := '';
    Len := ParseWord;
    SetLength(Result, Len);
    Move(Buffer[Position], Result[1], Len);
    Inc(Position, Len);
  end;

begin
  BeginService;
  try
    {
      Hope 2048 bytes is enought : 12 bytes are used to return number of
      attachments and databases header and footer of the data opacket.
      2036 bytes remains for databases names.
      Each is at least 3 + length_of_db_name bytes.
      Each db_name can be 2^16 bytes. I think an average value for db_name can
      be 50 bytes. So that Firebird Service API can return around 40 database
      names. Feel free to adjust according to you needs. You will get an
      EUIB_UNEXPECTEDERROR if the database names cannot fit into Buffer due to
      isc_info_truncated.
    }
    SetLength(Buffer,2048);
    FLibrary.ServiceQuery(FHandle, '', isc_info_svc_svr_db_info, Buffer);
  finally
    EndService;
  end;

  { Check header }
  if Buffer[1] <> isc_info_svc_svr_db_info then
    raise Exception.Create(EUIB_UNEXPECTEDERROR);

  { Parse response }
  Position := 2;
  while Buffer[Position] <> Char(isc_info_flag_end) do
  begin
    Code := ParseByte;
    case Code of
    isc_info_truncated:
      raise Exception.Create(EUIB_UNEXPECTEDERROR);
    isc_spb_num_att:
    begin
      Value := ParseInteger;
      if Assigned(FOnInfoAttachments) then
        FOnInfoAttachments(Self, Value);
    end;
    isc_spb_num_db:
    begin
      Value := ParseInteger;
      if Assigned(FOnInfoDatabases) then
        FOnInfoDatabases(Self, Value);
    end;
    Byte(isc_spb_dbname):
      begin
        DbName := ParseString;
        if Assigned(FOnInfoDbName) then
          FOnInfoDbName(Self,DbName);
      end;
    end;
  end;
end;

{$IFNDEF FPC}
initialization
  IsMultiThread := true;
{$ENDIF}

end.
