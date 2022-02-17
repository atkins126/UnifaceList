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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 570
    Height = 669
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      TabVisible = False
      object ListBox1: TListBox
        Left = 0
        Top = 129
        Width = 562
        Height = 530
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        ExplicitWidth = 570
        ExplicitHeight = 540
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 562
        Height = 129
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Panel1'
        ShowCaption = False
        TabOrder = 1
        ExplicitTop = 8
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
        object Button2: TButton
          Left = 406
          Top = 98
          Width = 75
          Height = 25
          Caption = 'Clear'
          TabOrder = 6
          OnClick = Button2Click
        end
        object Button3: TButton
          Left = 325
          Top = 98
          Width = 75
          Height = 25
          Caption = 'Test'
          TabOrder = 7
          OnClick = Button3Click
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 1
      TabVisible = False
      inline Frame21: TFrame2
        Left = 0
        Top = 0
        Width = 562
        Height = 659
        Align = alClient
        TabOrder = 0
        ExplicitLeft = -20
        inherited Panel1: TPanel
          Width = 562
          inherited GetItemId: TLabeledEdit
            EditLabel.ExplicitLeft = 0
            EditLabel.ExplicitTop = -16
            EditLabel.ExplicitWidth = 49
          end
          inherited PutItemIdID: TLabeledEdit
            EditLabel.ExplicitLeft = 0
            EditLabel.ExplicitTop = -16
            EditLabel.ExplicitWidth = 59
          end
          inherited PutItemIdValue: TLabeledEdit
            EditLabel.ExplicitLeft = 0
            EditLabel.ExplicitTop = -16
            EditLabel.ExplicitWidth = 74
          end
          inherited PutItemValue: TLabeledEdit
            EditLabel.ExplicitLeft = 0
            EditLabel.ExplicitTop = -16
            EditLabel.ExplicitWidth = 64
          end
        end
        inherited Panel2: TPanel
          Width = 562
          ExplicitTop = 169
          inherited CurrentItem: TLabeledEdit
            Width = 545
            EditLabel.ExplicitLeft = 0
            EditLabel.ExplicitTop = -16
          end
        end
        inherited Memo1: TMemo
          Width = 562
          Height = 442
          ExplicitLeft = 0
          ExplicitTop = 217
          ExplicitWidth = 582
          ExplicitHeight = 347
        end
      end
    end
  end
end
