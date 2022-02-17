unit UnifaceList;

interface

uses Classes, SysUtils, System.Generics.Collections, UnifaceKeyValuePair;

type
	TUnifaceList = class
    private
      FItems: TList<TUnifaceList>;
      FValue: String;
      FKey: String;
      Constructor Create(const aPair: TUnifaceKeyValuePair);overload;

      function Split(const aPair: TUnifaceKeyValuePair; const aText:String):TUnifaceList;
      function IsValidIndex(const aIndex:Integer):Boolean;
      function GetIsList():Boolean;
      function GetIsEmpty(): Boolean;
      function GetSeparator(const aLevel:Integer):String;
      function GetValueCore(const aLevel: Integer):String;
      function GetUnifaceString():String;
      function GetUnifaceStringCore(const aLevel:Integer):String;
      function ItemsToStrings(const aLevel:Integer):TStringList;
      function GetSingleValue(const aLevel:Integer): String;
      function GetValue():String;

      procedure SetValue(aValue:String);
      procedure Clear();
    public
      Constructor Create();overload;
      Destructor Destroy();override;

      property Key: String read FKey;
      property Value: String read GetValue write SetValue;
      property IsList: Boolean read GetIsList;
      property IsEmpty: Boolean read GetIsEmpty;
      property UnifaceString: String read GetUnifaceString;

      procedure Parse(const aText: String);
      function GetItem(const aIndex: Integer):TUnifaceList;
      function GetItemID(const aId: String): TUnifaceList;

      procedure PutItem(const aValue:String; const aIndex: Integer=0);overload;
      procedure PutItem(const aItem: TUnifaceList; const aIndex: Integer=0);overload;

      procedure PutItemID(const aId,aValue:String);overload;
      procedure PutItemID(const aId: String; const aItem: TUnifaceList);overload;
  end;

implementation

uses System.RegularExpressions, UnifaceListCommon;

var
 regExReplace: TRegEx;
 regExSplit: TRegEx;

Destructor TUnifaceList.Destroy();
begin
  Clear();  
  inherited;
end;

procedure TUnifaceList.Clear();
var i:Integer;
begin
  FKey:='';
  FValue:='';
  for I := FItems.Count-1 downto 0 do
  begin  
    FItems[i].Clear();
    if(FItems[i].IsEmpty)then FItems.Remove(FItems[i]);
  end;
  FItems.Clear();
end;

constructor TUnifaceList.Create();
begin
	FKey:='';
  FValue:='';
	FItems:=TList<TUnifaceList>.Create();
end;

constructor TUnifaceList.Create(const aPair: TUnifaceKeyValuePair);
begin
  Create();
  FValue:=aPair.Value;
  if(aPair.HasKey)then
  begin
    FKey:=aPair.Key;
  end;
end;

procedure TUnifaceList.Parse(const aText: String);
var parts: TArray<String>;
    part, partItem: String;
    pair: TUnifaceKeyValuePair;
begin
  parts:=regExSplit.Split(aText);
  for part in parts do
  begin
    pair:= TUnifaceKeyValuePair.Create(part);
    partItem:=regExReplace.Replace(pair.Value, GOLD_SEMICOLON);
    if(regExSplit.IsMatch(partItem))then
    begin
      FItems.Add(Split(pair, partItem));
    end
    else
    begin
      FItems.Add(TUnifaceList.Create(pair));
    end;
  end;
end;

function TUnifaceList.Split(const aPair: TUnifaceKeyValuePair; const aText: string): TUnifaceList;
var parts: TArray<String>;
    part, partItem: String;
    item:TUnifaceList;
begin
  Result:=TUnifaceList.Create();
  if(aPair.HasKey)then
  begin
    result.FKey:=aPair.Key;
  end;

  parts:=regExSplit.Split(aText);
  for part in parts do
  begin
    partItem:=regExReplace.Replace(part, GOLD_SEMICOLON);
    item:=TUnifaceList.Create();
    item.Parse(partItem);
    result.FItems.Add(item);
  end;
end;

function TUnifaceList.IsValidIndex(const aIndex: Integer): Boolean;
begin
  Result:=(aIndex>=0) and (aIndex<=FItems.Count-1);
end;

function TUnifaceList.GetIsList(): Boolean;
begin
  Result:=FItems.Count>1;
end;

function TUnifaceList.GetIsEmpty(): Boolean;
begin
  Result:=FItems.Count=0;
end;

function TUnifaceList.GetItem(const aIndex: Integer): TUnifaceList;
var arrayIndex:Integer;
begin
  Result:=nil;
  arrayIndex:=aIndex-1;
  if(IsValidIndex(arrayIndex))then
  begin
    Result:=FItems[arrayIndex];
  end;
end;

function TUnifaceList.GetItemID(const aId: string): TUnifaceList;
var item: TUnifaceList;
begin
  Result:=nil;
  for item in FItems do
  begin
    if(item.Key=aId)then
    begin
      Result:=item;
      break;
    end;
  end;
end;

function TUnifaceList.GetValueCore(const aLevel: Integer): string;
var separator: String;
begin
  separator:=GetSeparator(aLevel);
  if(IsList)then
  begin
    Result:=JoinStrings(ItemsToStrings(aLevel+1), separator);
  end
  else
  begin
    Result:=GetSingleValue(aLevel+1);
  end;
end;

function TUnifaceList.GetSeparator(const aLevel: Integer): string;
begin
  Result:=GOLD_SEMICOLON;
  Result:=Result.PadLeft(aLevel+1, GOLD_EXCLAMATION);
end;

function TUnifaceList.GetUnifaceStringCore(const aLevel: Integer): string;
var aValue: String;
begin
  aValue:=GetValueCore(aLevel);
  if(FKey<>'')then Result:=IdValueString(FKey, aValue)
  else Result:=aValue;
end;

function TUnifaceList.ItemsToStrings(const aLevel:Integer): TStringList;
var item:TUnifaceList;
begin
  Result:=TStringList.Create();
  for item in FItems do
  begin
    Result.Add(item.GetUnifaceStringCore(aLevel));
  end;
end;

function TUnifaceList.GetUnifaceString(): String;
begin
  Result:=GetUnifaceStringCore(0);
end;

function TUnifaceList.GetSingleValue(const aLevel: Integer): String;
begin
  if(FItems.Count=1)then
  begin
    Result:=FItems[0].GetUnifaceStringCore(aLevel);
  end
  else
  begin
    Result:=Fvalue;
  end;
end;

function TUnifaceList.GetValue():String;
begin
  Result:=GetValueCore(0);
end;

procedure TUnifaceList.SetValue(aValue: String);
begin
  Parse(aValue);
end;

procedure TUnifaceList.PutItem(const aValue: String; const aIndex: Integer = 0);
var item:TUnifaceList;
begin
  item:=TUnifaceList.Create();
  item.Value:=aValue;
  PutItem(item,aIndex);
end;

procedure TUnifaceList.PutItem(const aItem: TUnifaceList; const aIndex: Integer = 0);
var aValueListItem:TUnifaceList;
begin
  if(IsEmpty)then
  begin
    aValueListItem:=TUnifaceList.Create();
    aValueListItem.FValue:=FValue;
    aValueListItem.FKey:=FKey;
    FValue:='';
    FItems.Add(aValueListItem);
  end;
  if(aIndex>0)then FItems.Insert(aIndex-1, aItem)
  else FItems.Add(aItem);
end;

procedure TUnifaceList.PutItemID(const aId: String; const aValue: String);
  var item:TUnifaceList;
begin
  item:=TUnifaceList.Create();
  item.Value:=IdValueString(aId, aValue);
  PutItemId(aId,item);
end;

procedure TUnifaceList.PutItemID(const aId: String; const aItem: TUnifaceList);
var item:TUnifaceList;
    index:Integer;
begin
  item:=GetItemID(aId);
  if(Assigned(item))then
  begin
    index:=FItems.IndexOf(item);
    FItems[index]:=item;
  end
  else
  begin
    FItems.Add(item);
  end;
end;


initialization
  regExReplace:= TRegEx.Create(GOLD_EXCLAMATION+GOLD_SEMICOLON, [roCompiled]);
  regExSplit:= TRegEx.Create('(?<!'+GOLD_EXCLAMATION+')'+GOLD_SEMICOLON, [roCompiled]);
finalization


end.

