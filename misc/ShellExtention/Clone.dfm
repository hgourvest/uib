object CloneForm: TCloneForm
  Left = 529
  Top = 335
  ActiveControl = btStart
  Caption = 'Clone'
  ClientHeight = 378
  ClientWidth = 531
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  DesignSize = (
    531
    378)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 43
    Height = 13
    Caption = 'Clone file'
  end
  object Log: TMemo
    Left = 8
    Top = 120
    Width = 513
    Height = 225
    TabOrder = 0
  end
  object btStart: TButton
    Left = 368
    Top = 352
    Width = 75
    Height = 25
    Caption = 'Start'
    Default = True
    TabOrder = 1
    OnClick = btStartClick
    OnKeyPress = FormKeyPress
  end
  object btClose: TButton
    Left = 448
    Top = 352
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 2
    OnClick = btCloseClick
    OnKeyPress = FormKeyPress
  end
  object cbSave: TCheckBox
    Left = 8
    Top = 351
    Width = 105
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Save as default'
    TabOrder = 3
    OnKeyPress = FormKeyPress
  end
  object edCloneFile: TEdit
    Left = 8
    Top = 24
    Width = 457
    Height = 21
    TabOrder = 4
    OnChange = ConfigChange
    OnKeyPress = FormKeyPress
  end
  object btBrowse: TButton
    Left = 464
    Top = 24
    Width = 25
    Height = 21
    Caption = '...'
    TabOrder = 5
    OnClick = btBrowseClick
    OnKeyPress = FormKeyPress
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 48
    Width = 185
    Height = 65
    Caption = 'Options '
    TabOrder = 6
    object cbReplace: TCheckBox
      Left = 8
      Top = 16
      Width = 73
      Height = 17
      Caption = 'Replace'
      TabOrder = 0
      OnKeyPress = FormKeyPress
    end
    object cbMetadataOnly: TCheckBox
      Left = 8
      Top = 32
      Width = 97
      Height = 17
      Caption = 'Metadata Only'
      TabOrder = 1
      OnKeyPress = FormKeyPress
    end
  end
  object cbCloseWhenDone: TCheckBox
    Left = 200
    Top = 96
    Width = 113
    Height = 17
    Caption = 'Close after finished'
    TabOrder = 7
    OnKeyPress = FormKeyPress
  end
  object cbVerbose: TCheckBox
    Left = 200
    Top = 56
    Width = 97
    Height = 17
    Caption = 'Verbose'
    Checked = True
    State = cbChecked
    TabOrder = 8
    OnKeyPress = FormKeyPress
  end
  object Source: TJvUIBDataBase
    Params.Strings = (
      'sql_dialect=3'
      'lc_ctype=NONE')
    LibraryName = 'gds32.dll'
    Left = 16
    Top = 128
  end
  object Destination: TJvUIBDataBase
    Params.Strings = (
      'sql_dialect=3'
      'lc_ctype=NONE')
    LibraryName = 'gds32.dll'
    Left = 16
    Top = 160
  end
  object SaveDialog: TSaveDialog
    Filter = 'Backup file|*.gdb;*.fdb;*.ib'
    Left = 16
    Top = 192
  end
  object SrcTransaction: TJvUIBTransaction
    DataBase = Source
    Left = 48
    Top = 128
  end
  object DstTransaction: TJvUIBTransaction
    DataBase = Destination
    Left = 48
    Top = 160
  end
  object SrcQuery: TJvUIBQuery
    Transaction = SrcTransaction
    CachedFetch = False
    FetchBlobs = True
    Left = 80
    Top = 128
  end
end
