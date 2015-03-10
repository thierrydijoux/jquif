{
@abstract(Class for HTML Template )
@author(Thierry DIJOUX <tjr.dijoux@gmail.com>)
Class for HTML Template.
}
unit HtmlTemplate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, strUtils;

Type
  { @abstract(Tag Item)
  Tag Item }
  TTagItem = class
  private
    FTagName: string;
    FTagValue: string;
  public
    { Name of the tag in the template to be replaced }
    property TagName: string read FTagName write FTagName;
    { Value of the tag to be replaced in the template }
    property TagValue: string read FTagValue write FTagValue;
  end;

  { @abstract(Maintains a list of tags)
  Maintains a list of tags }
  TTags = class(TObjectList)
  private
    FOwnsObjects: Boolean;
  protected
    function GetItem(Index: Integer): TTagItem;
    procedure SetItem(Index: Integer; AObject: TTagItem);
  public
    { Add a tag in the list }
    function Add(AObject: TTagItem): Integer;
    { Add a tag in the list. Another way to add it }
    function Add(ATagName: string; ATagValue: string): integer; overload;
    function Remove(AObject: TTagItem): Integer;
    function IndexOf(AObject: TTagItem): Integer;
    procedure Insert(Index: Integer; AObject: TTagItem);
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
    property Items[Index: Integer]: TTagItem read GetItem write SetItem; default;
  end;

  { @abstract(class for manipulating template)
  class for manipulating template }
  THtmlTemplate = class
  private
    FFileName: string;
    FContent: TStrings;
    FExtraCss: TStrings;
    FExtraJavaScript: TStrings;
    FExtraContent: TStrings;
    FTags: TTags;
    function GetContent: string;
    function GetExtraCss: string;
    function GetExtraJavaScript: string;
    function GetExtraContent: string;
  public
    { Load the template }
    function Load: boolean;
    { Add HTML content }
    procedure AddExtraContent(AContent: string);
    { Add javascript script }
    procedure AddExtraJavaScript(AJavaScript: string);
    { Add Css }
    procedure AddExtraCss(ACss: string);
    { Create the object with the template to load }
    constructor Create(ATemplateName: string);
    destructor Destroy; override;
    { Return the generated HTML }
    property Content: string read GetContent;
    { List of tags }
    property Tags: TTags read FTags;
    { Return the generated CSS }
    property ExtraCss: string read GetExtraCss;
    { Return the generated javascript }
    property ExtraJavaScript: string read GetExtraJavaScript;
    { Return the generated HTML }
    property ExtraContent: string read GetExtraContent;
  end;

implementation

{ THtmlTemplate }

function THtmlTemplate.GetExtraContent: string;
begin
  if not Assigned(FExtraContent) then
    result:= ''
  else
    result:= FExtraContent.Text;
end;

procedure THtmlTemplate.AddExtraContent(AContent: string);
begin
  if not Assigned(FExtraContent) then
    FExtraContent:= TStringList.Create;
  FExtraContent.Add(AContent);
end;

function THtmlTemplate.GetExtraCss: string;
begin
  if not Assigned(FExtraCss) then
    result:= ''
  else
    result:= FExtraCss.Text;
end;

procedure THtmlTemplate.AddExtraCss(ACss: string);
begin
  if not Assigned(FExtraCss) then
    FExtraCss:= TStringList.Create;
  FExtraCss.Add(ACss);
end;

function THtmlTemplate.GetExtraJavaScript: string;
begin
  if not Assigned(FExtraJavaScript) then
    result:= ''
  else
    result:= FExtraJavaScript.Text;
end;

procedure THtmlTemplate.AddExtraJavaScript(AJavaScript: string);
begin
  if not Assigned(FExtraJavaScript) then
    FExtraJavaScript:= TStringList.Create;
  FExtraJavaScript.Add(AJavaScript);
end;

function THtmlTemplate.GetContent: string;
Var
  i: integer;
  Htm: string;
begin
  Htm:= FContent.Text;
  if Assigned(FExtraCss) then
    Htm:= AnsiReplaceStr(Htm, '<!--extracss-->', FExtraCss.Text);
  if Assigned(FExtraJavaScript) then
    Htm:= AnsiReplaceStr(Htm, '<!--extrajs-->', FExtraJavaScript.Text);
  if Assigned(FExtraContent) then
    Htm:= AnsiReplaceStr(Htm, '<!--extracontent-->', FExtraContent.Text);
  for i:= 0 to FTags.Count -1 do
    Htm:= AnsiReplaceStr(Htm, '<!--'+FTags.Items[i].TagName+'-->', FTags.Items[i].TagValue);
  result:= Htm;
end;

function THtmlTemplate.Load: boolean;
begin
  result:= false;

  if Length(FFileName) = 0 then
    raise exception.Create('THtmlTemplate.Load: file name is empty !');

  if not FileExists(FFileName) then
    raise exception.Create('THtmlTemplate.Load: the file ' + FFileName + ' does not exists !');

  if not Assigned(FContent) then
    FContent:= TStringList.Create;

  FContent.LoadFromFile(FFileName);
  result:= true;
end;

constructor THtmlTemplate.Create(ATemplateName: string);
begin
  inherited Create;
  FFileName:= ATemplateName;
  FTags:= TTags.Create(true);
end;

destructor THtmlTemplate.Destroy;
begin
  FTags.Free;
  if Assigned(FContent) then
    FContent.Free;
  if Assigned(FExtraJavaScript) then
    FExtraJavaScript.Free;
  if Assigned(FExtraCss) then
    FExtraCss.Free;
  if Assigned(FExtraContent) then
    FExtraContent.Free;
  inherited Destroy;
end;

{ TTags }

function TTags.Add(ATagName: string; ATagValue: string): integer;
Var
  NewTag: TTagItem;
begin
  NewTag:= TTagItem.Create;
  NewTag.TagName:= ATagName;
  NewTag.TagValue:= ATagValue;
  result:= Add(NewTag);
end;

function TTags.Add(AObject: TTagItem): Integer;
begin
  Result:= inherited Add(AObject);
end;

function TTags.GetItem(Index: Integer): TTagItem;
begin
  Result:= TTagItem(inherited Items[Index]);
end;

function TTags.IndexOf(AObject: TTagItem): Integer;
begin
  Result:= inherited IndexOf(AObject);
end;

procedure TTags.Insert(Index: Integer; AObject: TTagItem);
begin
  inherited Insert(Index, AObject);
end;

function TTags.Remove(AObject: TTagItem): Integer;
begin
  Result:= inherited Remove(AObject);
end;

procedure TTags.SetItem(Index: Integer; AObject: TTagItem);
begin
  inherited Items[Index] := AObject;
end;

end.

