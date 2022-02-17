unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    ListBox1: TListBox;
    Panel1: TPanel;
    Button1: TButton;
    LabeledEdit1: TLabeledEdit;
    CheckBox1: TCheckBox;
    LabeledEdit2: TLabeledEdit;
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
    procedure AppendLine(const aValue:String);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses System.RegularExpressions, UnifaceListCommon, UnifaceList;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var source, target:String;
    list, item:TUnifaceList;
begin
  source:=ReadableToGold(LabeledEdit1.Text);
  list:=TUnifaceList.Create();
  //list.Parse(source);
  list.Value:=source;
  ListBox1.Clear;
  AppendLine(LabeledEdit1.Text);
  target:=list.UnifaceString;
  AppendLine(GoldToReadable(target));

  if(CheckBox1.Checked)then
  begin
    AppendLine('Item with id '+LabeledEdit2.Text+':');
    item:=list.GetItemID(LabeledEdit2.Text);
    if(Assigned(item))then
    begin
      AppendLine(#9+GoldToReadable(item.Value));
    end
    else
    begin
      AppendLine(#9+'Not found.');
    end;
  end;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  LabeledEdit2.Enabled:=CheckBox1.Checked;
end;

procedure TForm1.AppendLine(const aValue: string);
begin
  ListBox1.Items.Add(aValue);
end;

end.
