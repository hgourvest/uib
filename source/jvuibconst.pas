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
(* contributor: Olivier Guilbaud <oguilb@free.fr>                               *)
(*                                                                              *)
(********************************************************************************)
(* $Id: jvuibconst.pas,v 1.10 2005/10/29 21:01:36 progdigy Exp $                 *)
(********************************************************************************)

{$I jvuib.inc}

unit jvuibconst;

interface

{$IFNDEF DELPHI6_UP}
{$IFNDEF BCB}
const
  S_OK    = $00000000;
  S_FALSE = $00000001;
{$ENDIF BCB}
{$ENDIF DELPHI6_UP}


type
  // JvUIB Server Commands
  TServerCommand = (scGetClassObject, scInvokeMethod);

  // Metadata Object Identifiers
  TOIDDatabase = (OIDDomain, OIDTable, OIDView, OIDProcedure, OIDGenerator,
    OIDException, OIDUDF, OIDRole, OIDDBCharset);
  TOIDDatabases = set of TOIDDatabase;

  TOIDTable = (OIDTableField, OIDPrimary, OIDForeign, OIDTableTrigger,
    OIDUnique,OIDIndex, OIDCheck);
  TOIDTables = set of TOIDTable;

  TOIDView = (OIDViewFields, OIDViewTrigers);
  TOIDViews = set of TOIDView;

  TOIDProcedure = (OIDProcFieldIn, OIDProcFieldOut);
  TOIDProcedures = set of TOIDProcedure;

  TOIDUDF = (OIDUDFField);
  TOIDUDFs = set of TOIDUDF;

const
  ALLOBjects = [OIDDomain, OIDTable, OIDView, OIDProcedure, OIDGenerator,
    OIDException, OIDUDF, OIDRole, OIDDBCharset];
  ALLTables = [OIDTableField, OIDPrimary, OIDForeign, OIDTableTrigger,
    OIDUnique,OIDIndex, OIDCheck];
  ALLViews = [OIDViewFields, OIDViewTrigers];
  ALLProcedures = [OIDProcFieldIn, OIDProcFieldOut];
  ALLUDFs = [OIDUDFField];
{$IFDEF UNIX}
  BreakLine = #10;
{$ELSE}
  BreakLine = #13;
{$ENDIF}
  NewLine = BreakLine + BreakLine;

const
  {$IFDEF UIBLANG_EN}
  sUIBTrue  = 'True';
  sUIBFalse = 'False';

  // UIB Errors
  EUIB_INVALIDEIBVERSION   = 'Incorrect Database Server version, check compiler options.';
  EUIB_CANTLOADLIB         = 'Can''t load library: %s.';
  EUIB_DBHANDLEALREADYSET  = 'Database handle already assigned, first disconnect database.';
  EUIB_TRANSACTIONNOTDEF   = 'Transaction not assigned.';
  EUIB_DATABASENOTDEF      = 'Database not assigned.';
  EUIB_QUERYNOTOPEN        = 'Query not open.';
  EUIB_CASTERROR           = 'Cast error.';
  EUIB_UNEXPECTEDERROR     = 'Unexpected error.';
  EUIB_FIELDNUMNOTFOUND    = 'Field num: %d not found.';
  EUIB_FIELDSTRNOTFOUND    = 'Field "%s" not found.';
  EUIB_PARAMSTRNOTFOUND    = 'Parameter "%s" not found.';
  EUIB_BLOBFIELDNOTFOUND   = 'Blob field num: %d not found.';
  EUIB_FETCHBLOBNOTSET     = 'FetchBlob property must be set to use this method.';
  EUIB_INDEXERROR          = 'Index out of bound (%d)';
  EUIB_SIZENAME            = 'Size name too big (%s)';
  EUIB_MUSTBEPREPARED      = 'The query must be prepared first.';
  EUIB_MUSTBEOPEN          = 'The query must be opened first.';
  EUIB_EXPLICITTRANS       = 'Transaction must be started explicitly.';
  EUIB_EXCEPTIONNOTFOUND   = 'Exception name %s, not found.';
  EUIB_EXPTIONREGISTERED   = 'Exception: %d already registered';
  EUIB_NOAUTOSTOP          = 'Transaction must be closed explicitly.';
  EUIB_NOGENERATOR         = 'Generator %s not found.';
  EUIB_NOFIELD             = 'Field not found.';
  EUIB_TABLESTRNOTFOUND    = 'Table "%s" not found.';
  EUIB_DOMAINSTRNOTFOUND   = 'Domain %s not found.';
  EUIB_PROCSTRNOTFOUND     = 'Procedure %s not found.';
  EUIB_CACHEDFETCHNOTSET   = 'CachedFetch property not set to True.';
  EUIB_PARSESQLDIALECT     = 'Parse error: SET SQL DIALECT';
  EUIB_PARSESETNAMES       = 'Parse error: SET NAMES';
  EUIB_CHARSETNOTFOUND     = 'CharacterSet %s not found.';
  EUIB_UNEXPECTEDCASTERROR = 'Unexpected cast error.';
  EUIB_INVALIDUSERNAME     = 'Invalid user name : "%s".';
  EUIB_SERVICESPARSING     = 'Error while parsing Services API output.';
  {$ENDIF UIBLANG_EN}

  {$IFDEF UIBLANG_FR}
  sUIBTrue  = 'Vrai';
  sUIBFalse = 'Faux';

  // UIB Errors
  EUIB_INVALIDEIBVERSION   = 'Version de base de donnйes incorrecte, vйrifiez les options de compilation.';
  EUIB_CANTLOADLIB         = 'Impossible de charger la DLL: %s.';
  EUIB_DBHANDLEALREADYSET  = 'Le handle de la base de donnйes est dйjа dйfini, dйconnectez d''abord la base de donnйes.';
  EUIB_TRANSACTIONNOTDEF   = 'La transaction n''est pas dйfinie';
  EUIB_DATABASENOTDEF      = 'La base de donnйes n''est pas dйfinie.';
  EUIB_QUERYNOTOPEN        = 'La requкte n''est pas encore ouverte.';
  EUIB_CASTERROR           = 'Transtypage incorrect.';
  EUIB_UNEXPECTEDERROR     = 'Erreur innatendue.';
  EUIB_FIELDNUMNOTFOUND    = 'Le champ numйro: %d ''esxiste pas.';
  EUIB_FIELDSTRNOTFOUND    = 'Le champ "%s" n''existe pas.';
  EUIB_PARAMSTRNOTFOUND    = 'Le Parametre "%s" n''existe pas.';
  EUIB_BLOBFIELDNOTFOUND   = 'Le champ Blob numйro: %d n''existe pas.';
  EUIB_FETCHBLOBNOTSET     = 'La propriйtй FetchBlob doit кtre activйe pour utiliser cette mйthode.';
  EUIB_INDEXERROR          = 'Indice de liste hors limites (%d)';
  EUIB_SIZENAME            = 'La taille du nom est trop grande (%s)';
  EUIB_MUSTBEPREPARED      = 'La requиte doit d''abord кtre prйparйe.';
  EUIB_MUSTBEOPEN          = 'La requиte doit d''abord кtre ouverte.';
  EUIB_EXPLICITTRANS       = 'La transaction doit кtre dйmarrйe explicitement.';
  EUIB_EXCEPTIONNOTFOUND   = 'L''exception %s, n''existe pas.';
  EUIB_EXPTIONREGISTERED   = 'L''exception %d a dйjа йtй enregistrйe.';
  EUIB_NOAUTOSTOP          = 'La transaction doit кtre fermйe explicitement.';
  EUIB_NOGENERATOR         = 'Generator %s not found.';
  EUIB_NOFIELD             = 'Field not found.';
  EUIB_TABLESTRNOTFOUND    = 'Table "%s" not found.';
  EUIB_DOMAINSTRNOTFOUND   = 'Domain %s not found.';
  EUIB_PROCSTRNOTFOUND     = 'Procedure %s not found.';
  EUIB_CACHEDFETCHNOTSET   = 'CachedFetch property not set to True.';
  EUIB_PARSESQLDIALECT     = 'Parse error: SET SQL DIALECT';
  EUIB_PARSESETNAMES       = 'Parse error: SET NAMES';
  EUIB_CHARSETNOTFOUND     = 'CharacterSet %s not found.';
  EUIB_UNEXPECTEDCASTERROR = 'Unexpected cast error.';
  EUIB_INVALIDUSERNAME     = 'Invalid user name : "%s".';
  EUIB_SERVICESPARSING     = 'Error while parsing Services API output.';

  {$ENDIF UIBLANG_FR}

  {$IFDEF UIBLANG_CZ}
  sUIBTrue  = 'Ano';
  sUIBFalse = 'Ne';

  // UIB Errors
  EUIB_INVALIDEIBVERSION   = 'Nekorektnн verze databбzovйho serveru, zkontrolujte nastavenн kompileru.';
  EUIB_CANTLOADLIB         = 'Nelze naинst knihovnu: %s.';
  EUIB_DBHANDLEALREADYSET  = 'Kanбl databбze je jiћ pшipraven, nejdшнve odpojte databбzi.';
  EUIB_TRANSACTIONNOTDEF   = 'Transakce nenн pшiшazena.';
  EUIB_DATABASENOTDEF      = 'Databбze nenн pшiшazena.';
  EUIB_QUERYNOTOPEN        = 'Dotaz nenн otevшen.';
  EUIB_CASTERROR           = 'Chyba bsazenн.';
  EUIB_UNEXPECTEDERROR     = 'Neznбmб chyba.';
  EUIB_FIELDNUMNOTFOUND    = 'Poloћka инslo: %d neexistuje.';
  EUIB_FIELDSTRNOTFOUND    = 'Poloћka "%s" neexistuje.';
  EUIB_PARAMSTRNOTFOUND    = 'Parametr "%s" neexistuje.';
  EUIB_BLOBFIELDNOTFOUND   = 'Blob zбznam инslo: %d neexistuje.';
  EUIB_FETCHBLOBNOTSET     = 'Vlastnost FetchBlob musн bэt nastavena pro pouћitн tйto metody.';
  EUIB_INDEXERROR          = 'Index je mimo rozsah (%d)';
  EUIB_SIZENAME            = 'Velikost nбzvu je pшнliљ velkб (%s)';
  EUIB_MUSTBEPREPARED      = 'Dotaz musн bэt nejdшнve pшipraven (prepared).';
  EUIB_MUSTBEOPEN          = 'The query must be opened first.';
  EUIB_EXPLICITTRANS       = 'Transaction must be started explicitly.';
  EUIB_EXCEPTIONNOTFOUND   = 'Exception name %s, not found.';
  EUIB_EXPTIONREGISTERED   = 'Exception: %d already registered';
  EUIB_NOAUTOSTOP          = 'Transaction must be closed explicitly.';
  EUIB_NOGENERATOR         = 'Generator %s not found.';
  EUIB_NOFIELD             = 'Field not found.';
  EUIB_TABLESTRNOTFOUND    = 'Table "%s" not found.';
  EUIB_DOMAINSTRNOTFOUND   = 'Domain %s not found.';
  EUIB_PROCSTRNOTFOUND     = 'Procedure %s not found.';
  EUIB_CACHEDFETCHNOTSET   = 'CachedFetch property not set to True.';
  EUIB_PARSESQLDIALECT     = 'Parse error: SET SQL DIALECT';
  EUIB_PARSESETNAMES       = 'Parse error: SET NAMES';
  EUIB_CHARSETNOTFOUND     = 'CharacterSet %s not found.';
  EUIB_UNEXPECTEDCASTERROR = 'Unexpected cast error.';
  EUIB_INVALIDUSERNAME     = 'Invalid user name : "%s".';
  EUIB_SERVICESPARSING     = 'Error while parsing Services API output.';

  {$ENDIF UIBLANG_CZ}

  {$IFDEF UIBLANG_DE}
  sUIBTrue  = 'Wahr';
  sUIBFalse = 'Falsch';

  // UIB Errors
  EUIB_INVALIDEIBVERSION   = 'Falsche Version des Datenbankservers. Bitte ueberprьfen sie die Compileroptionen.';
  EUIB_CANTLOADLIB         = 'Kann Bibliothek %s nicht laden.';
  EUIB_DBHANDLEALREADYSET  = 'Datenbank-Handle bereits zugewiesen. Bitte erst Verbindung zur Datenbank trennen.';
  EUIB_TRANSACTIONNOTDEF   = 'Transaktion nicht zugewiesen.';
  EUIB_DATABASENOTDEF      = 'Datenbank nicht zugewiesen.';
  EUIB_QUERYNOTOPEN        = 'Abfrage nicht geцffnet.';
  EUIB_CASTERROR           = 'Fehler bei Typumwandlung.';
  EUIB_UNEXPECTEDERROR     = 'Unerwarteter Fehler.';
  EUIB_FIELDNUMNOTFOUND    = 'Feld Nummer %d nicht gefunden.';
  EUIB_FIELDSTRNOTFOUND    = 'Feld "%s" nicht gefunden.';
  EUIB_PARAMSTRNOTFOUND    = 'Parameter "%s" nicht gefunden.';
  EUIB_BLOBFIELDNOTFOUND   = 'Blob-Feld Nummer: %d nicht gefunden.';
  EUIB_FETCHBLOBNOTSET     = 'Die Eigenschaft FetchBlob muss gesetzt sein um diese Methode zu nutzen.';
  EUIB_INDEXERROR          = 'Index ausserhalb des gьltigen Bereichs (%d).';
  EUIB_SIZENAME            = 'Name ist zu lang (%s).';
  EUIB_MUSTBEPREPARED      = 'Die Abfrage muss erst vorbereitet werden.';
  EUIB_MUSTBEOPEN          = 'Die Abfrage muss erst geцffnet werden.';
  EUIB_EXPLICITTRANS       = 'Die Transaktion muss explizit gestartet werden.';
  EUIB_EXCEPTIONNOTFOUND   = 'Ausnahme "%s" nicht gefunden.';
  EUIB_EXPTIONREGISTERED   = 'Ausnahme %d bereits registriert.';
  EUIB_NOAUTOSTOP          = 'Die Transaktion muss explizit beendet werden.';
  EUIB_NOGENERATOR         = 'Generator %s nicht gefunden.';
  EUIB_NOFIELD             = 'Feld nicht gefunden.';
  EUIB_TABLESTRNOTFOUND    = 'Tabelle "%s" nicht gefunden.';
  EUIB_DOMAINSTRNOTFOUND   = 'Domдne %s nicht gefunden.';
  EUIB_PROCSTRNOTFOUND     = 'Prozedur %s nicht gefunden.';
  EUIB_CACHEDFETCHNOTSET   = 'CachedFetch property not set to True.';
  EUIB_PARSESQLDIALECT     = 'Parse error: SET SQL DIALECT';
  EUIB_PARSESETNAMES       = 'Parse error: SET NAMES';
  EUIB_CHARSETNOTFOUND     = 'CharacterSet %s not found.';
  EUIB_UNEXPECTEDCASTERROR = 'Unexpected cast error.';
  EUIB_INVALIDUSERNAME     = 'Invalid user name : "%s".';
  EUIB_SERVICESPARSING     = 'Error while parsing Services API output.';

  {$ENDIF UIBLANG_DE}

  {$IFDEF UIBLANG_RU}
  sUIBTrue  = 'Да';
  sUIBFalse = 'Нет';

  // UIB Errors
  EUIB_INVALIDEIBVERSION   = 'Некорректная версия сервера БД, проверьте опции компилятора.';
  EUIB_CANTLOADLIB         = 'Не могу загрузить библиотеку DLL: %s.';
  EUIB_DBHANDLEALREADYSET  = 'Дескриптор базы данных уже установлен, сначала разорвите текущее соединение с базой данных.';
  EUIB_TRANSACTIONNOTDEF   = 'Свойство Transaction не установлено.';
  EUIB_DATABASENOTDEF      = 'Свойство Database не установлено.';
  EUIB_QUERYNOTOPEN        = 'Запрос не открыт.';
  EUIB_CASTERROR           = 'Ошибка приведения типа.';
  EUIB_UNEXPECTEDERROR     = 'Неизвестная ошибка.';
  EUIB_FIELDNUMNOTFOUND    = 'Поле номер: %d не найдено.';
  EUIB_FIELDSTRNOTFOUND    = 'Поле "%s" не найдено.';
  EUIB_PARAMSTRNOTFOUND    = 'Параметр "%s" не найден.';
  EUIB_BLOBFIELDNOTFOUND   = 'Blob поле номер: %d не найдено.';
  EUIB_FETCHBLOBNOTSET     = 'Свойство FetchBlob должно быть установлено для выполнения этого метода.';
  EUIB_INDEXERROR          = 'Index за пределами допустимых значений (%d)';
  EUIB_SIZENAME            = 'Слишком длинное название (%s)';
  EUIB_MUSTBEPREPARED      = 'Необходимо сначала подготовить (prepare) запрос.';
  EUIB_MUSTBEOPEN          = 'Необходимо сначала открыть запрос.';
  EUIB_EXPLICITTRANS       = 'Не выполнен старт транзакции.';
  EUIB_EXCEPTIONNOTFOUND   = 'Исключение %s не найдено.';
  EUIB_EXPTIONREGISTERED   = 'Исключение: %d уже зарегистрировано';
  EUIB_NOAUTOSTOP          = 'Transaction must be closed explicitly.';
  EUIB_NOGENERATOR         = 'Generator %s not found.';
  EUIB_NOFIELD             = 'Field not found.';
  EUIB_TABLESTRNOTFOUND    = 'Table "%s" not found.';
  EUIB_DOMAINSTRNOTFOUND   = 'Domain %s not found.';
  EUIB_PROCSTRNOTFOUND     = 'Procedure %s not found.';
  EUIB_CACHEDFETCHNOTSET   = 'CachedFetch property not set to True.';
  EUIB_PARSESQLDIALECT     = 'Parse error: SET SQL DIALECT';
  EUIB_PARSESETNAMES       = 'Parse error: SET NAMES';
  EUIB_CHARSETNOTFOUND     = 'CharacterSet %s not found.';
  EUIB_UNEXPECTEDCASTERROR = 'Unexpected cast error.';
  EUIB_INVALIDUSERNAME     = 'Invalid user name : "%s".';
  EUIB_SERVICESPARSING     = 'Error while parsing Services API output.';

  {$ENDIF UIBLANG_RU}

  {$IFDEF UIBLANG_ES}
  sUIBTrue             = 'Si';
  sUIBFalse            = 'No';

  // UIB Errors
  EUIB_INVALIDEIBVERSION   = 'Versiуn incorrecta del Servidor de Base de Datos, verifica las opciones del compilador.';
  EUIB_CANTLOADLIB         = 'Imposible cargar la DLL: %s.';
  EUIB_DBHANDLEALREADYSET  = 'El handle de la base de datos estб asignado, primero desconecte la base de datos.';
  EUIB_TRANSACTIONNOTDEF   = 'La transacciуn no estб asignada.';
  EUIB_DATABASENOTDEF      = 'La Base de Datos no estб asignada.';
  EUIB_QUERYNOTOPEN        = 'El query estб cerrado.';
  EUIB_CASTERROR           = 'Error en conversiуn.';
  EUIB_UNEXPECTEDERROR     = 'Error inesperado.';
  EUIB_FIELDNUMNOTFOUND    = 'Campo nъmero: %d no encontrado.';
  EUIB_FIELDSTRNOTFOUND    = 'Campo "%s" no encontrado.';
  EUIB_PARAMSTRNOTFOUND    = 'Parametro "%s" no  encontrado.';
  EUIB_BLOBFIELDNOTFOUND   = 'Campo Blob nъmero: %d no  encontrado.';
  EUIB_FETCHBLOBNOTSET     = 'La propiedad FetchBlob debe habilitarse para usar estй mйtodo';
  EUIB_INDEXERROR          = 'Нndice fuera de lнmite (%d)';
  EUIB_SIZENAME            = 'Nombre demasiado largo (%s)';
  EUIB_MUSTBEPREPARED      = 'El query debe prepararse primero.';
  EUIB_MUSTBEOPEN          = 'El query debe ser abierto primero.';
  EUIB_EXPLICITTRANS       = 'La transaccisn debe ser iniciada explmcitamente.';
  EUIB_EXCEPTIONNOTFOUND   = 'Excepcion con nombre %s, no fue encontrada.';
  EUIB_EXPTIONREGISTERED   = 'Excepcion: %d ya esta registrada.';
  EUIB_NOAUTOSTOP          = 'La transaccisn debe ser cerrada explmcitamente.';
  EUIB_NOGENERATOR         = 'Generator %s not found.';
  EUIB_NOFIELD             = 'Field not found.';
  EUIB_TABLESTRNOTFOUND    = 'Table "%s" not found.';
  EUIB_DOMAINSTRNOTFOUND   = 'Domain %s not found.';
  EUIB_PROCSTRNOTFOUND     = 'Procedure %s not found.';
  EUIB_CACHEDFETCHNOTSET   = 'CachedFetch property not set to True.';
  EUIB_PARSESQLDIALECT     = 'Parse error: SET SQL DIALECT';
  EUIB_PARSESETNAMES       = 'Parse error: SET NAMES';
  EUIB_CHARSETNOTFOUND     = 'CharacterSet %s not found.';
  EUIB_UNEXPECTEDCASTERROR = 'Unexpected cast error.';
  EUIB_INVALIDUSERNAME     = 'Invalid user name : "%s".';
  EUIB_SERVICESPARSING     = 'Error while parsing Services API output.';

  {$ENDIF UIBLANG_ES}

  {$IFDEF UIBLANG_TR}
  sUIBTrue  = 'Doрru';
  sUIBFalse = 'Yanlэю';

  // UIB Errors
  EUIB_INVALIDEIBVERSION   = 'Hatalэ Veritabanэ Sunucusu sьrьmь, derleyici seзeneklerini kontrol ediniz.';
  EUIB_CANTLOADLIB         = 'Kitaplэk yьklenemiyor: %s.';
  EUIB_DBHANDLEALREADYSET  = 'Veritabanэ tanэtэcэsэ atanmэю durumda, цncelikle veritabanэ baрlantэsэnэ kesiniz.';
  EUIB_TRANSACTIONNOTDEF   = 'Эюlem grubu atanmamэю.';
  EUIB_DATABASENOTDEF      = 'Veritabanэ atanmamэю.';
  EUIB_QUERYNOTOPEN        = 'Sorgu aзэk deрil.';
  EUIB_CASTERROR           = 'Tip atama hatasэ.';
  EUIB_UNEXPECTEDERROR     = 'Beklenmeyen hata.';
  EUIB_FIELDNUMNOTFOUND    = '%d numaralэ saha bulunamadэ.';
  EUIB_FIELDSTRNOTFOUND    = '"%s" sahasэ bulunamadэ.';
  EUIB_PARAMSTRNOTFOUND    = '"%s" parametresi bulunamadэ.';
  EUIB_BLOBFIELDNOTFOUND   = '%d numaralэ Blob sahasэ bulunamadэ.';
  EUIB_FETCHBLOBNOTSET     = 'Bu metodu kullanmak iзin FetchBlob niteliрi ayarlanmalэdэr.';
  EUIB_INDEXERROR          = 'Indeks sэnэr dэюэnda (%d)';
  EUIB_SIZENAME            = 'Ad bьyьklьрь зok yьksek (%s)';
  EUIB_MUSTBEPREPARED      = 'Sorgu цncelikle hazэrlanmalэdэr.';
  EUIB_MUSTBEOPEN          = 'Sorgu цncelikle aзэlmalэdэr.';
  EUIB_EXPLICITTRANS       = 'Эюlem grubu elle baюlatэlmalэdэr.';
  EUIB_EXCEPTIONNOTFOUND   = 'Kural dэюэ nesnesi "%s" bulunamadэ.';
  EUIB_EXPTIONREGISTERED   = 'Kural dэюэ: %d zaten kayэtlэ.';
  EUIB_NOAUTOSTOP          = 'Эюlem grubu elle kapatэlmalэdэr.';
  EUIB_NOGENERATOR         = '%s ьreteci bulunamadэ.';
  EUIB_NOFIELD             = 'Saha bulunamadэ.';
  EUIB_TABLESTRNOTFOUND    = '"%s" tablosu bulunamadэ.';
  EUIB_DOMAINSTRNOTFOUND   = '"%s" etki alanэ bulunamadэ.';
  EUIB_PROCSTRNOTFOUND     = '%s yordamэ bulunamadэ.';
  EUIB_CACHEDFETCHNOTSET   = 'CachedFetch niteliрi True olarak belirlenmemiю.';
  EUIB_PARSESQLDIALECT     = 'Ayrэюtэrma hatasэ: SET SQL DIALECT';
  EUIB_PARSESETNAMES       = 'Ayrэюtэrma hatasэ: SET NAMES';
  EUIB_CHARSETNOTFOUND     = '%s karakter seti bulunamadэ.';
  EUIB_UNEXPECTEDCASTERROR = 'Beklenmeyen tip atama hatasэ.';
  EUIB_INVALIDUSERNAME     = 'Geзersiz kullanэcэ adэ : "%s".';
  EUIB_SERVICESPARSING     = 'Services API зэktэsэ ayrэюtэrэlэrken hata oluюtu.';

  {$ENDIF UIBLANG_TR}

implementation

end.
