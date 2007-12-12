object MainForm: TMainForm
  Left = 198
  Top = 107
  Caption = 'Clony&Pumpy - The Famous Firebird Databases Tool'
  ClientHeight = 516
  ClientWidth = 542
  Color = clBtnFace
  Constraints.MinHeight = 500
  Constraints.MinWidth = 550
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    542
    516)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 143
    Width = 386
    Height = 89
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Clone options'
    TabOrder = 1
    object cbReplace: TCheckBox
      Left = 8
      Top = 16
      Width = 148
      Height = 17
      Caption = 'Replace dest. database'
      TabOrder = 0
    end
    object cbMetadataOnly: TCheckBox
      Left = 8
      Top = 39
      Width = 97
      Height = 17
      Caption = 'Metadata only'
      TabOrder = 1
    end
    object cbPageSize: TComboBox
      Left = 219
      Top = 35
      Width = 145
      Height = 21
      Style = csDropDownList
      Enabled = False
      ItemHeight = 13
      TabOrder = 2
    end
    object cbOverrideSourcePageSize: TCheckBox
      Left = 201
      Top = 16
      Width = 182
      Height = 17
      Caption = 'Override src. database page size'
      TabOrder = 3
      OnClick = cbOverrideSourcePageSizeClick
    end
    object cbIgnoreConstraints: TCheckBox
      Left = 8
      Top = 62
      Width = 249
      Height = 17
      Caption = 'Do not restore indices and relational constraints'
      TabOrder = 4
    end
    object cbFailsafeClone: TCheckBox
      Left = 268
      Top = 62
      Width = 113
      Height = 17
      Hint = 'Commit after each record - slower'
      Caption = 'Secure data pump'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
    end
  end
  object GroupBox2: TGroupBox
    Left = 400
    Top = 143
    Width = 134
    Height = 135
    Anchors = [akTop, akRight]
    Caption = 'Options'
    TabOrder = 2
    object cbVerbose: TCheckBox
      Left = 8
      Top = 16
      Width = 97
      Height = 17
      Caption = 'Verbose'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object cbCloseWhenDone: TCheckBox
      Left = 8
      Top = 39
      Width = 113
      Height = 17
      Caption = 'Close after finished'
      TabOrder = 1
    end
    object cbInternalNames: TCheckBox
      Left = 9
      Top = 62
      Width = 97
      Height = 17
      Caption = 'Internal Names'
      TabOrder = 2
    end
  end
  object Log: TMemo
    Left = 8
    Top = 284
    Width = 526
    Height = 224
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 238
    Width = 386
    Height = 40
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Pump options'
    TabOrder = 3
    object cbEmptyTables: TCheckBox
      Left = 8
      Top = 16
      Width = 97
      Height = 17
      Caption = 'Empty tables'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object cbFailsafePump: TCheckBox
      Left = 268
      Top = 16
      Width = 113
      Height = 17
      Hint = 'Commit after each record - slower'
      Caption = 'Secure data pump'
      Checked = True
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 1
    end
  end
  object GroupBox4: TGroupBox
    Left = 8
    Top = 8
    Width = 526
    Height = 129
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Databases'
    TabOrder = 4
    DesignSize = (
      526
      129)
    object Label1: TLabel
      Left = 11
      Top = 76
      Width = 383
      Height = 13
      Caption = 
        '(Beware of database connection options like SQLDialect, Charset,' +
        ' Role Name ...)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clInactiveCaptionText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object btStartClone: TButton
      Left = 190
      Top = 95
      Width = 75
      Height = 25
      Anchors = [akTop]
      Caption = 'Clone'
      TabOrder = 0
      OnClick = btStartCloneClick
    end
    object btStartPump: TButton
      Left = 271
      Top = 95
      Width = 75
      Height = 25
      Anchors = [akTop]
      Caption = 'Pump'
      TabOrder = 1
      OnClick = btStartPumpClick
    end
    object btDstDatabase: TButton
      Left = 443
      Top = 47
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Destination ...'
      TabOrder = 2
      OnClick = btDstDatabaseClick
    end
    object btSrcDatabase: TButton
      Left = 443
      Top = 16
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Source ...'
      TabOrder = 3
      OnClick = btSrcDatabaseClick
    end
    object edSrcDatabase: TEdit
      Left = 8
      Top = 18
      Width = 429
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 4
      OnChange = edSrcDatabaseChange
    end
    object edDstDatabase: TEdit
      Left = 8
      Top = 49
      Width = 429
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 5
      OnChange = edDstDatabaseChange
    end
  end
  object SrcDatabase: TJvUIBDataBase
    Params.Strings = (
      'password=masterkey'
      'user_name=SYSDBA')
    UserName = 'SYSDBA'
    PassWord = 'masterkey'
    LibraryName = 'gds32.dll'
    Left = 24
    Top = 304
  end
  object SrcTransaction: TJvUIBTransaction
    DataBase = SrcDatabase
    Options = [tpConsistency, tpRead]
    Left = 24
    Top = 336
  end
  object SrcQuery: TJvUIBQuery
    Transaction = SrcTransaction
    DataBase = SrcDatabase
    CachedFetch = False
    FetchBlobs = True
    Left = 24
    Top = 368
  end
  object DstDatabase: TJvUIBDataBase
    Params.Strings = (
      'password=masterkey'
      'user_name=SYSDBA')
    UserName = 'SYSDBA'
    PassWord = 'masterkey'
    LibraryName = 'gds32.dll'
    Left = 56
    Top = 304
  end
  object DstTransaction: TJvUIBTransaction
    DataBase = DstDatabase
    Options = [tpNowait, tpWrite, tpReadCommitted, tpNoRecVersion, tpNoAutoUndo]
    Left = 56
    Top = 336
  end
  object XPManifest1: TXPManifest
    Left = 56
    Top = 400
  end
end
