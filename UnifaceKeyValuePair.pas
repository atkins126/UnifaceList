unit UnifaceKeyValuePair;

interface

uses Classes;

type
  TUnifaceKeyValuePair=class
    private
      FKey: String;
      FValue: String;
      FHasKey: Boolean;
    public
      Constructor Create(const aValue:String);

      property Key: String read FKey;
      property Value: String read FValue;
      property HasKey: Boolean read FHasKey;
  end;

implementation

uses System.RegularExpressions, UnifaceListCommon, SysUtils;

var
  regEx:TRegEx;

Constructor TUnifaceKeyValuePair.Create(const aValue: string);
var firstMatch:TMatch;
    offset:Integer;
begin
  FKey:=NO_KEY;
  FHasKey:=false;
  FValue:='';
  firstMatch:=regEx.Match(aValue);
  if(firstMatch.Success)then
  begin
    offset:=firstMatch.Index-1;
    FKey:=aValue.Substring(0,offset);
    Inc(offset);
    FValue:=aValue.Remove(0, offset);
    FHasKey:=true;
  end
  else
  begin
    FValue:=aValue;
  end;
end;

initialization
  regEx:=TRegEx.Create('(?<!%)=',[roCompiled]);
finalization

end.
