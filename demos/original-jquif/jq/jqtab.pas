unit JQTab;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, jqBase;

Type

  TJQTabItem = class
  private
    FTitle: string;
    FTabName: string;
    FContent: TStrings;
    FUseAjax: boolean;
    FAjaxURL: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddContent(AValue: string);
    property Title: string read FTitle write FTitle;
    property Content: TStrings read FContent write FContent;
    property UseAjax: boolean read FUseAjax write FUseAjax;
    property AjaxURL: string read FAjaxURL write FAjaxURL;
  end;

  TJqTab = class(TJQBase)
  private
    FTabList: TObjectList;
    FIsCollapsible: boolean;
    FOpenOnMouseOver: boolean;
    function GetTab(AIndex: integer): TJQTabItem;
    function GetCount: integer;
  protected
    function GetContent: string; override;
    function GetJs: string; override;
  public
    constructor create;
    destructor destroy; override;
    function AddTab(ATabName: string): integer;
    procedure Clear;
    property Tab[AItemIndex: integer]: TJQTabItem read GetTab;
    property TabCount: integer read GetCount;
    property Collapsible: boolean read FIsCollapsible write FIsCollapsible;
    property OpenOnMouseOver: boolean read FOpenOnMouseOver write FOpenOnMouseOver;
  end;

implementation

{ TJqTab }

function TJqTab.GetContent: string;
Var
  i: integer;
begin
  FContent.Clear;
  FContent.Add('<div id="' + FId + '">');
  FContent.Add('	<ul>');
  for i:= 0 to FTabList.Count -1 do
    if TJQTabItem(FTabList.Items[i]).UseAjax then
      FContent.Add('<li><a href="' + TJQTabItem(FTabList.Items[i]).AjaxURL + '">' + TJQTabItem(FTabList.Items[i]).Title +'</a></li>')
    else
      FContent.Add('<li><a href="#tabs-' + intToStr(i+1) + '">' + TJQTabItem(FTabList.Items[i]).Title +'</a></li>');
  FContent.Add('	</ul>');
  for i:= 0 to FTabList.Count -1 do
  begin
    if not TJQTabItem(FTabList.Items[i]).UseAjax then
    begin
      FContent.Add('	<div id="tabs-' + intToStr(i+1) + '">');
      FContent.Add(TJQTabItem(FTabList.Items[i]).Content.Text);
      FContent.Add('	</div>');
    end;
  end;
  FContent.Add('</div>');
  result:= FContent.Text;
end;

function TJqTab.GetJs: string;
Var
  jsAjax: boolean;
  i: integer;
begin
  FJs.Clear;
  FJs.Add('<script type="text/javascript">');
  FJs.Add('	$(function() {');
  FJs.Add('		$( "#' + FId + '" ).tabs(');
  if FIsCollapsible then
    FJs.Add('{ collapsible: true }');
  if FOpenOnMouseOver then
    if FIsCollapsible then
      FJs.Add(',{ event: "mouseover" }')
    else
      FJs.Add('{ event: "mouseover" }');

  FJs.Add(');');
  FJs.Add('	});');
  FJs.Add('</script>');
  for i:= 0 to TabCount -1 do
    if Tab[i].UseAjax then
    begin
       jsAjax:= true;
       break;
    end;

  if jsAjax then
  begin
  FJs.Add('<script type="text/javascript">');
  FJs.Add('	$(function() {');
  FJs.Add('		$( "#' + FId + '" ).tabs({');
  FJs.Add('			ajaxOptions: {');
  FJs.Add('				error: function( xhr, status, index, anchor ) {');
  FJs.Add('					$( anchor.hash ).html(');
  FJs.Add('						"Couldn''t load this tab. We''ll try to fix this as soon as possible. " +');
  FJs.Add('						"If this wouldn''t be a demo." );');
  FJs.Add('				}');
  FJs.Add('			}');
  FJs.Add('		});');
  FJs.Add('	});');
  FJs.Add('</script>');
  end;



  result:= FJs.Text;
end;

function TJqTab.GetTab(AIndex: integer): TJQTabItem;
begin
  result:= TJQTabItem(FTabList.Items[AIndex]);
end;

function TJqTab.GetCount: integer;
begin
  result:= FTabList.Count;
end;

function TJqTab.AddTab(ATabName: string): integer;
Var
  NewTab: TJQTabItem;
begin
  NewTab:= TJQTabItem.Create;
  NewTab.Title:= ATabName;
  result:= FTabList.Add(NewTab);
end;

procedure TJqTab.Clear;
begin
  FTabList.Clear;
end;

constructor TJqTab.create;
begin
  inherited Create;
  FTabList:= TObjectList.create(true);
  FIsCollapsible:= false;
end;

destructor TJqTab.destroy;
begin
  FTabList.Free;
  inherited destroy;
end;

{ TJQTabItem }

procedure TJQTabItem.AddContent(AValue: string);
begin
  FContent.Add(AValue);
end;

constructor TJQTabItem.Create;
begin
  FContent:= TStringList.Create;
end;

destructor TJQTabItem.Destroy;
begin
  FContent.Free;
  inherited destroy;
end;

end.

