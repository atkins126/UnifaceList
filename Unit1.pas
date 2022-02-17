unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Unit2,
  Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    ListBox1: TListBox;
    Panel1: TPanel;
    Button1: TButton;
    LabeledEdit1: TLabeledEdit;
    CheckBox1: TCheckBox;
    LabeledEdit2: TLabeledEdit;
    CheckBox2: TCheckBox;
    LabeledEdit3: TLabeledEdit;
    Button2: TButton;
    Button3: TButton;
    TabSheet2: TTabSheet;
    Frame21: TFrame2;
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    procedure AppendLine(const aValue:String);
    procedure OnFrameClosed(Sender: TObject);
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
  list.Value:=source;
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

  if(CheckBox2.Checked)then
  begin
    AppendLine('Adding value: '+LabeledEdit3.Text);
    item:=list.GetItem(1);
    if(Assigned(item))then
    begin
      item.PutItem(LabeledEdit3.Text);
      target:=list.UnifaceString;
      AppendLine(GoldToReadable(target));
    end;
    list.PutItem(LabeledEdit3.Text+'x');
    target:=list.UnifaceString;
    AppendLine(GoldToReadable(target));
  end;
  AppendLine('=====================================');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ListBox1.Clear;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  PageControl1.Pages[1].Visible:=true;
  PageControl1.Pages[0].Visible:=false;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  LabeledEdit2.Enabled:=CheckBox1.Checked;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  LabeledEdit3.Enabled:=CheckBox2.Checked;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  PageControl1.Pages[1].Visible:=true;
  PageControl1.Pages[0].Visible:=false;
  Frame21.OnClose:=OnFrameClosed;
end;

procedure TForm1.AppendLine(const aValue: string);
begin
  ListBox1.Items.Add(aValue);
end;

procedure TForm1.OnFrameClosed(Sender:TObject);
begin
  PageControl1.Pages[0].Visible:=true;
  PageControl1.Pages[1].Visible:=false;
end;

end.
