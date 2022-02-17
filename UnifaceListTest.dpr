program UnifaceListTest;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  UnifaceList in 'UnifaceList.pas',
  UnifaceKeyValuePair in 'UnifaceKeyValuePair.pas',
  UnifaceListCommon in 'UnifaceListCommon.pas',
  Unit2 in 'Unit2.pas' {Frame2: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
