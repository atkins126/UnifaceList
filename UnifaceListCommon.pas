unit UnifaceListCommon;

interface

uses System.Generics.Collections, Classes;

const GOLD_EXCLAMATION=#21;
const GOLD_SEMICOLON=#27;
const GOLD_EXCLAMATION_READABLE='!';
const GOLD_SEMICOLON_READABLE=';';
const RECORD_SEPARATOR=#30;

function JoinStrings(const aStrings: TStringList; aDelimiter: String): String;
function GoldToReadable(const aText: String):String;
function ReadableToGold(const aText: String):String;

implementation

uses System.RegularExpressions, SysUtils;

var regEx:TRegEx;
var isAssigned:Boolean;

function GetRegEx():TRegEx;
begin
  if (not isAssigned) then
  begin
    regEx:=TRegEx.Create(RECORD_SEPARATOR,[roCompiled]);
    isAssigned:=true;
  end;

  Result:=regEx;
end;

function JoinStrings(const aStrings: TStringList; aDelimiter: String): String;
begin
  aStrings.Delimiter:=RECORD_SEPARATOR;
  aStrings.StrictDelimiter:=true;
  aStrings.QuoteChar:=#0;
  Result:=GetRegEx().Replace(aStrings.DelimitedText, aDelimiter);
end;

function GoldToReadable(const aText: String):String;
begin
  Result:=aText
    .Replace(GOLD_EXCLAMATION, GOLD_EXCLAMATION_READABLE)
    .Replace(GOLD_SEMICOLON, GOLD_SEMICOLON_READABLE);
end;

function ReadableToGold(const aText: String):String;
begin
  Result:=aText
    .Replace(GOLD_EXCLAMATION_READABLE, GOLD_EXCLAMATION)
    .Replace(GOLD_SEMICOLON_READABLE, GOLD_SEMICOLON);
end;

initialization
  isAssigned:=false;
finalization

end.
