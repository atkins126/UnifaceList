unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Samples.Spin, UnifaceList;

type
  TFrame2 = class(TFrame)
    Panel1: TPanel;
    LabeledEdit1: TLabeledEdit;
    Create: TButton;
    Panel2: TPanel;
    CurrentItem: TLabeledEdit;
    GetItemId: TLabeledEdit;
    Button1: TButton;
    PutItemIdID: TLabeledEdit;
    Button2: TButton;
    PutItemIdValue: TLabeledEdit;
    GetItem: TSpinEdit;
    Label1: TLabel;
    Button3: TButton;
    PutItem: TSpinEdit;
    Button4: TButton;
    PutItemValue: TLabeledEdit;
    Label2: TLabel;
    Button5: TButton;
    Memo1: TMemo;
    Button6: TButton;
    procedure CreateClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Private declarations }
    FList: TUnifaceList;
    FOnClose: TNotifyEvent;
    procedure AppendLine(const aText:String);
    procedure SetCurrentItem(const aItem:TunifaceList);

    procedure GettedItem(const aItem:TunifaceList);
    procedure PutItemMethod(const aValue: String; const aIndex: Integer);
    procedure PutItemId(const aId,aValue:String);
    function ValidCurrent():Boolean;
  public
    { Public declarations }
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  end;

implementation
uses UnifaceListCommon;

{$R *.dfm}

procedure TFrame2.Button1Click(Sender: TObject);
begin
  if(ValidCurrent())then
  begin
    GettedItem(FList.GetItemID(GetItemId.Text));
  end;
end;

procedure TFrame2.Button2Click(Sender: TObject);
begin
  PutItemId(PutItemIdID.Text, PutItemIDValue.Text);
end;

procedure TFrame2.Button3Click(Sender: TObject);
begin
  if(ValidCurrent())then
  begin
    GettedItem(FList.GetItem(GetItem.Value));
  end;
end;

procedure TFrame2.Button4Click(Sender: TObject);
begin
  PutItemMethod(PutItemValue.Text, PutItem.Value);
end;

procedure TFrame2.Button5Click(Sender: TObject);
begin
  if ValidCurrent() then
  begin
    if(Assigned(FList.Parent))then
    begin
      SetCurrentItem(FList.Parent);
    end
    else
    begin
      AppendLine('No parent exists');
    end;
  end;
end;

procedure TFrame2.Button6Click(Sender: TObject);
begin
  if(Assigned(FOnClose))then
  begin
    FOnClose(self);
  end;
end;

procedure TFrame2.SetCurrentItem(const aItem: TUnifaceList);
begin
  FList:=aItem;
  CurrentItem.Text:=FList.Value;
  AppendLine('Assigned: '+FList.UnifaceString);
end;

procedure TFrame2.CreateClick(Sender: TObject);
var item:TUnifaceList;
begin
  Memo1.Clear();
  item:=TUnifaceList.Create();
  item.Value:=LabeledEdit1.Text;
  SetCurrentItem(item);
end;

procedure TFrame2.AppendLine(const aText: string);
begin
  Memo1.Lines.Add(GoldToReadable(aText));
end;

procedure TFrame2.GettedItem(const aItem: TUnifaceList);
begin
  if(Assigned(aItem))then
  begin
    SetCurrentItem(aItem);
  end
  else
  begin
    AppendLine('Item not found.');
  end;
end;

procedure TFrame2.PutItemId(const aId: string; const aValue: string);
begin
  if(ValidCurrent())then
  begin
    AppendLine('Putting item with id '+aId+' and value '+aValue);
    FList.PutItemId(aId, aValue);
    GettedItem(FList.GetItemID(aId));
  end;
end;

procedure TFrame2.PutItemMethod(const aValue: String; const aIndex: Integer);
begin
  if(ValidCurrent())then
  begin
    AppendLine('Putting item on '+IntToStr(aIndex)+' with value '+aValue);
    FList.PutItem(aValue, aIndex);
    GettedItem(FList.GetItem(aIndex));
  end;
end;

function TFrame2.ValidCurrent(): Boolean;
begin
  Result:=true;
  if (not Assigned(FList))then
  begin
    AppendLine('No current item');
    Result:=false;
  end;
end;

end.
