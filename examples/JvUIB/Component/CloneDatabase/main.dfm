object MainForm: TMainForm
  Left = 198
  Top = 107
  Width = 509
  Height = 476
  Caption = 'Clone Database'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object CopyButton: TButton
    Left = 160
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Copy >>>'
    TabOrder = 0
    OnClick = CopyButtonClick
  end
  object Log: TListBox
    Left = 0
    Top = 40
    Width = 501
    Height = 409
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    Items.Strings = (
      'Destination database will be destroyed if exist !')
    TabOrder = 1
  end
  object Source: TButton
    Left = 0
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Source ...'
    TabOrder = 2
    OnClick = SourceClick
  end
  object Destination: TButton
    Left = 80
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Destination ...'
    TabOrder = 3
    OnClick = DestinationClick
  end
  object DataBase1: TJvUIBDataBase
    Params.Strings = (
      'sql_dialect=3'
      'lc_ctype=NONE'
      'sql_role_name='
      'user_name=SYSDBA'
      'password=masterkey')
    DatabaseName = 'D:\EMPLOYEE.FDB'
    UserName = 'SYSDBA'
    PassWord = 'masterkey'
    LibraryName = 'gds32.dll'
    Left = 8
    Top = 64
  end
  object Transaction1: TJvUIBTransaction
    DataBase = DataBase1
    Options = [tpConsistency, tpRead]
    Left = 8
    Top = 96
  end
  object Query1: TJvUIBQuery
    Transaction = Transaction1
    DataBase = DataBase1
    CachedFetch = False
    FetchBlobs = True
    Left = 8
    Top = 128
  end
  object DataBase2: TJvUIBDataBase
    Params.Strings = (
      'sql_dialect=3'
      'lc_ctype=NONE'
      'user_name=SYSDBA'
      'password=masterkey')
    DatabaseName = 'D:\EMPLOYEE2.FDB'
    UserName = 'SYSDBA'
    PassWord = 'masterkey'
    LibraryName = 'gds32.dll'
    Left = 40
    Top = 64
  end
  object Transaction2: TJvUIBTransaction
    DataBase = DataBase2
    Left = 40
    Top = 96
  end
end
