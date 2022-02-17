object Frame2: TFrame2
  Left = 0
  Top = 0
  Width = 582
  Height = 564
  TabOrder = 0
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 582
    Height = 169
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 0
    object Label1: TLabel
      Left = 240
      Top = 49
      Width = 39
      Height = 13
      Caption = 'GetItem'
    end
    object Label2: TLabel
      Left = 8
      Top = 124
      Width = 38
      Height = 13
      Caption = 'PutItem'
    end
    object LabeledEdit1: TLabeledEdit
      Left = 8
      Top = 20
      Width = 465
      Height = 21
      EditLabel.Width = 66
      EditLabel.Height = 13
      EditLabel.Caption = 'Uniface string'
      TabOrder = 0
    end
    object Create: TButton
      Left = 479
      Top = 18
      Width = 75
      Height = 25
      Caption = 'Create'
      TabOrder = 1
      OnClick = CreateClick
    end
    object GetItemId: TLabeledEdit
      Left = 8
      Top = 64
      Width = 121
      Height = 21
      EditLabel.Width = 50
      EditLabel.Height = 13
      EditLabel.Caption = 'GetItemID'
      TabOrder = 2
    end
    object Button1: TButton
      Left = 135
      Top = 62
      Width = 75
      Height = 25
      Caption = 'Execute'
      TabOrder = 3
      OnClick = Button1Click
    end
    object PutItemIdID: TLabeledEdit
      Left = 8
      Top = 102
      Width = 121
      Height = 21
      EditLabel.Width = 64
      EditLabel.Height = 13
      EditLabel.Caption = 'PutItemID-ID'
      TabOrder = 4
    end
    object Button2: TButton
      Left = 262
      Top = 98
      Width = 75
      Height = 25
      Caption = 'Execute'
      TabOrder = 5
      OnClick = Button2Click
    end
    object PutItemIdValue: TLabeledEdit
      Left = 135
      Top = 102
      Width = 121
      Height = 21
      EditLabel.Width = 79
      EditLabel.Height = 13
      EditLabel.Caption = 'PutItemID-Value'
      TabOrder = 6
    end
    object GetItem: TSpinEdit
      Left = 240
      Top = 64
      Width = 121
      Height = 22
      MaxValue = 1000
      MinValue = 1
      TabOrder = 7
      Value = 1
    end
    object Button3: TButton
      Left = 367
      Top = 62
      Width = 75
      Height = 25
      Caption = 'Execute'
      TabOrder = 8
      OnClick = Button3Click
    end
    object PutItem: TSpinEdit
      Left = 8
      Top = 143
      Width = 121
      Height = 22
      MaxValue = 1000
      MinValue = 0
      TabOrder = 9
      Value = 0
    end
    object Button4: TButton
      Left = 262
      Top = 141
      Width = 75
      Height = 25
      Caption = 'Execute'
      TabOrder = 10
      OnClick = Button4Click
    end
    object PutItemValue: TLabeledEdit
      Left = 135
      Top = 143
      Width = 121
      Height = 21
      EditLabel.Width = 68
      EditLabel.Height = 13
      EditLabel.Caption = 'PutItem-Value'
      TabOrder = 11
    end
    object Button5: TButton
      Left = 479
      Top = 49
      Width = 75
      Height = 25
      Caption = 'Up in list'
      TabOrder = 12
      OnClick = Button5Click
    end
    object Button6: TButton
      Left = 479
      Top = 138
      Width = 75
      Height = 25
      Caption = 'Close'
      TabOrder = 13
      OnClick = Button6Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 169
    Width = 582
    Height = 48
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 1
    ExplicitTop = 129
    DesignSize = (
      582
      48)
    object CurrentItem: TLabeledEdit
      Left = 8
      Top = 20
      Width = 565
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      EditLabel.Width = 59
      EditLabel.Height = 13
      EditLabel.Caption = 'CurrentItem'
      Enabled = False
      TabOrder = 0
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 217
    Width = 582
    Height = 347
    Align = alClient
    TabOrder = 2
    ExplicitLeft = 200
    ExplicitTop = 240
    ExplicitWidth = 185
    ExplicitHeight = 89
  end
end
