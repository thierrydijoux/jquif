unit HtmlTemplate;
{< @abstract(Class for HTML Template )
   @author(Thierry DIJOUX <tjr.dijoux@gmail.com>)
   Class for HTML Template.
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Contnrs, StrUtils, JQBase;

type
  { @abstract(Tag Item) }
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
        FOwnsObjects: boolean;
    protected
        function GetItem(Index: integer): TTagItem;
        procedure SetItem(Index: integer; AObject: TTagItem);
    public
        { Add a tag in the list }
        function Add(AObject: TTagItem): integer;
        { Add a tag in the list. Another way to add it }
        function Add(ATagName: string; ATagValue: string): integer; overload;
        function Remove(AObject: TTagItem): integer;
        function IndexOf(AObject: TTagItem): integer;
        procedure Insert(Index: integer; AObject: TTagItem);
        property OwnsObjects: boolean read FOwnsObjects write FOwnsObjects;
        property Items[Index: integer]: TTagItem read GetItem write SetItem; default;
    end;

  { @abstract(class for manipulating template)
    Class for manipulating template }
    THtmlTemplate = class
    private
        FFileName: string;
        FContent: TStrings;
        FExtraCss: TStrings;
        FExtraJavaScriptH: TStrings; //< in then header (default)
        FExtraJavaScriptT: TStrings; //< in the body top
        FExtraJavaScriptB: TStrings; //< in the body bottom
        FExtraContent: TStrings;
        FTags: TTags;
        function GetContent: string;
        function GetExtraCss: string;
        function GetExtraJavaScript(location: ExtraJSloc): string;
        function GetExtraContent: string;
    public
        { Create the object with the template to load }
        constructor Create(ATemplateName: string);
        destructor Destroy; override;
        { Load the template }
        function Load: boolean;
        { Add HTML content }
        procedure AddExtraContent(AContent: string);
        { Add javascript script }
        procedure AddExtraJavaScript(AScript: string; location: ExtraJSloc = locHeader); overload;
        procedure AddExtraJavaScript(AObject: TJQBase; location: ExtraJSloc = locHeader); overload;
        { Add Css }
        procedure AddExtraCss(ACss: string);
        { Add and TJQBase or descendant object }
        procedure AddObject(AObject: TJQBase);
        { Return the generated HTML }
        property Content: string read GetContent;
        { List of tags }
        property Tags: TTags read FTags;
        { Return the generated CSS }
        property ExtraCss: string read GetExtraCss;
        { Return the generated javascript }
        property ExtraJavaScript[location: ExtraJSloc]: string read GetExtraJavaScript;
        { Return the generated HTML }
        property ExtraContent: string read GetExtraContent;
    end;

implementation

{ TTags ---------------------------------------------------------------------- }

function TTags.Add(ATagName: string; ATagValue: string): integer;
var newTag: TTagItem;
begin
    newTag:=TTagItem.Create;
    newTag.TagName:=ATagName;
    newTag.TagValue:=ATagValue;
    Result:=Add(newTag);
end;

function TTags.Add(AObject: TTagItem): integer;
begin
    Result:=inherited Add(AObject);
end;

function TTags.GetItem(Index: integer): TTagItem;
begin
    Result:=TTagItem(inherited Items[Index]);
end;

function TTags.IndexOf(AObject: TTagItem): integer;
begin
    Result:=inherited IndexOf(AObject);
end;

procedure TTags.Insert(Index: integer; AObject: TTagItem);
begin
    inherited Insert(Index, AObject);
end;

function TTags.Remove(AObject: TTagItem): integer;
begin
    Result:=inherited Remove(AObject);
end;

procedure TTags.SetItem(Index: integer; AObject: TTagItem);
begin
    inherited Items[Index]:=AObject;
end;

{ THtmlTemplate -------------------------------------------------------------- }

constructor THtmlTemplate.Create(ATemplateName: string);
begin
    inherited Create;
    FFileName:=ATemplateName;
    FTags:=TTags.Create(True);
end;

destructor THtmlTemplate.Destroy;
begin
    FTags.Free;
    if Assigned(FContent) then
        FContent.Free;
    if Assigned(FExtraJavaScriptH) then
        FExtraJavaScriptH.Free;
    if Assigned(FExtraJavaScriptT) then
        FExtraJavaScriptT.Free;
    if Assigned(FExtraJavaScriptB) then
        FExtraJavaScriptB.Free;
    if Assigned(FExtraCss) then
        FExtraCss.Free;
    if Assigned(FExtraContent) then
        FExtraContent.Free;
    inherited Destroy;
end;

function THtmlTemplate.GetExtraContent: string;
begin
    if not Assigned(FExtraContent) then
        Result:=''
    else
        Result:=FExtraContent.Text;
end;

procedure THtmlTemplate.AddExtraContent(AContent: string);
begin
    if not Assigned(FExtraContent) then FExtraContent:=TStringList.Create;
    FExtraContent.Add(AContent);
end;

function THtmlTemplate.GetExtraCss: string;
begin
    if not Assigned(FExtraCss) then
        Result:=''
    else
        Result:=FExtraCss.Text;
end;

procedure THtmlTemplate.AddExtraCss(ACss: string);
begin
    if not Assigned(FExtraCss) then FExtraCss:=TStringList.Create;
    FExtraCss.Add(ACss);
end;

// Warning: The CSS code assumes that stylesheets images are in /css/images
procedure THtmlTemplate.AddObject(AObject: TJQBase);
begin
    AddExtraContent(AObject.Content);
    AddExtraJavaScript(AObject,locHeader);
    AddExtraJavaScript(AObject,locBodyTop);
    AddExtraJavaScript(AObject,locBodyBottom);
    AddExtraCss(AObject.Css);
end;

function THtmlTemplate.GetExtraJavaScript(location: ExtraJSloc): string;
begin
    Result:='';
    case location of
        locHeader: if assigned(FExtraJavaScriptH) then
                Result:=FExtraJavaScriptH.Text;
        locBodyTop: if assigned(FExtraJavaScriptT) then
                Result:=FExtraJavaScriptT.Text;
        locBodyBottom: if assigned(FExtraJavaScriptB) then
                Result:=FExtraJavaScriptB.Text;
    end;
end;

procedure THtmlTemplate.AddExtraJavaScript(AScript: string; location: ExtraJSloc);
begin
    if AScript='' then exit;
    case location of
        locHeader: begin
            if not assigned(FExtraJavaScriptH) then
                FExtraJavaScriptH:=TStringList.Create;
            FExtraJavaScriptH.Add(AScript);
        end;
        locBodyTop: begin
            if not assigned(FExtraJavaScriptT) then
                FExtraJavaScriptT:=TStringList.Create;
            FExtraJavaScriptT.Add(AScript);
        end;
        locBodyBottom: begin
            if not assigned(FExtraJavaScriptB) then
                FExtraJavaScriptB:=TStringList.Create;
            FExtraJavaScriptB.Add(AScript);
        end;
    end;
end;

procedure THtmlTemplate.AddExtraJavaScript(AObject: TJQBase; location: ExtraJSloc);
var AScript: string;
begin
    AScript:=AObject.JavaScript[location];
    AddExtraJavaScript(AScript, location);
end;

function THtmlTemplate.GetContent: string;
var i: integer;
    Htm: string;
begin
    Htm := FContent.Text;
    if Assigned(FExtraCss) then
        Htm := AnsiReplaceStr(Htm, '<!--extraCss-->', FExtraCss.Text);
    if Assigned(FExtraJavaScriptH) then
        Htm := AnsiReplaceStr(Htm, '<!--extraJs.header-->', FExtraJavaScriptH.Text);
    if Assigned(FExtraJavaScriptT) then
        Htm := AnsiReplaceStr(Htm, '<!--extraJs.body.top-->', FExtraJavaScriptT.Text);
    if Assigned(FExtraJavaScriptB) then
        Htm := AnsiReplaceStr(Htm, '<!--extraJs.body.bottom-->', FExtraJavaScriptB.Text);
    if Assigned(FExtraContent) then
        Htm := AnsiReplaceStr(Htm, '<!--extraContent-->', FExtraContent.Text);
    for i := 0 to FTags.Count - 1 do begin
        Htm := AnsiReplaceStr(Htm, '<!--' + FTags.Items[i].TagName + '-->',
            FTags.Items[i].TagValue);
    end;
    Result := Htm;
end;

function THtmlTemplate.Load: boolean;
begin
    Result := False;
    if Length(FFileName) = 0 then
        raise Exception.Create('THtmlTemplate.Load: file name is empty !');
    if not FileExists(FFileName) then
        raise Exception.Create('THtmlTemplate.Load: the file ' + FFileName +
            ' does not exists !');
    if not Assigned(FContent) then FContent:=TStringList.Create;
    FContent.LoadFromFile(FFileName);
    Result := True;
end;

end.
