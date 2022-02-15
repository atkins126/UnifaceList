unit UnifaceList;

interface

uses Classes, SysUtils, System.Generics.Collections;

type
	TUnifaceListType=(ultValue, ultList);
  TUnifaceKeyValue = class
  private
    FKey:String;
    FValue:String;
  public
    Constructor Create(const aText:String);
    property Key: String read FKey;
    property Value: String read FValue;
  end;

	TUnifaceList = class
		FItems: TList<TUnifaceList>;
		FType: TUnifaceListType;
		FValue: String;
		FKey: String;
    Constructor Create();overload;
    Constructor Create(const aUnifaceList:String);overload;
    Constructor Create(const aKey,aUnifaceList:String);overload;

    procedure SplitNameAndValue(const aText:String);
	end;


implementation

uses System.RegularExpressions;

var
 regExReplace: TRegEx;
 regExSplit: TRegEx;
 regExSplitKeyValue: TRegEx;

const NO_KEY = '*#*#*#*EMPTY*#*#*#*';
const GOLD_SEMICOLON=';';//#27
const GOLD_EXCLAMATION='!';//#21
const ESCAPE_KEY='%';

constructor TUnifaceList.Create();
begin
	FKey:=NO_KEY;
	FItems:=TList<TUnifaceList>.Create();
end;

constructor TUnifaceList.Create(const aUnifaceList:String);
var parts: TArray<String>;
    i:Integer;
    item:TUnifaceList;
    keyValue: TUnifaceKeyValue;
begin
	parts:= regExSplit.Split(aUnifaceList);
	if(Length(parts)>1)then
	begin
	  FType:=ultList;
    for i := 0 to High(parts) do
    begin
      item:=TUnifaceList.Create(parts[i]);
    end;
	//FItems.addRange(split.foreach(Create(splitItem)));
	end
  else
  begin
		FType:=ultValue;
    keyValue:=TUnifaceKeyValue.Create(parts[0]);
    FKey:=keyValue.Key;
    FValue:=keyValue.Value;
  end;
end;

constructor TUnifaceList.Create(const aKey, aUnifaceList:String);
begin
	Create(aKey);
	FKey:=aKey;
end;

constructor TUnifaceKeyValue.Create(const aText: string);
var parts:TArray<String>;
    keyValueType:Integer;
begin
  keyValueType:=regExSplitKeyValue.Matches(aText).Count;
  case keyValueType of
    0:begin
      FKey:=NO_KEY;
      FValue:=aText;
    end;
    1:begin
      parts:=regExSplitKeyValue.Split(aText);
      FKey:=parts[0];
      if(Length(parts)=2)then FValue:=parts[1]
      else FValue:='';
    end
    else raise Exception.Create('Unsupported');
  end;
end;

initialization
  regExReplace:= TRegEx.Create(GOLD_EXCLAMATION+GOLD_SEMICOLON, [roCompiled]);
  regExSplit:= TRegEx.Create('(?<!'+GOLD_EXCLAMATION+')'+GOLD_SEMICOLON, [roCompiled]);
  regExSplitKeyValue:= TRegEx.Create('(?<!'+ESCAPE_KEY+')'+'=', [roCompiled]);
finalization


end.

