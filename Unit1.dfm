object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 669
  ClientWidth = 570
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox1: TListBox
    Left = 0
    Top = 129
    Width = 570
    Height = 540
    Align = alClient
    ItemHeight = 13
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 570
    Height = 129
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 1
    object Button1: TButton
      Left = 487
      Top = 98
      Width = 75
      Height = 25
      Caption = 'Parse'
      TabOrder = 0
      OnClick = Button1Click
    end
    object LabeledEdit1: TLabeledEdit
      Left = 8
      Top = 20
      Width = 554
      Height = 21
      EditLabel.Width = 66
      EditLabel.Height = 13
      EditLabel.Caption = 'Uniface string'
      TabOrder = 1
    end
    object CheckBox1: TCheckBox
      Left = 135
      Top = 67
      Width = 17
      Height = 17
      TabOrder = 2
      OnClick = CheckBox1Click
    end
    object LabeledEdit2: TLabeledEdit
      Left = 8
      Top = 65
      Width = 121
      Height = 21
      EditLabel.Width = 50
      EditLabel.Height = 13
      EditLabel.Caption = 'GetItemID'
      Enabled = False
      TabOrder = 3
    end
    object CheckBox2: TCheckBox
      Left = 285
      Top = 67
      Width = 17
      Height = 17
      TabOrder = 4
      OnClick = CheckBox2Click
    end
    object LabeledEdit3: TLabeledEdit
      Left = 158
      Top = 65
      Width = 121
      Height = 21
      EditLabel.Width = 38
      EditLabel.Height = 13
      EditLabel.Caption = 'PutItem'
      Enabled = False
      TabOrder = 5
    end
  end
  object Button2: TButton
    Left = 406
    Top = 98
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 2
    OnClick = Button2Click
  end
end
