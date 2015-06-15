{@abstract(Class for JQTab)
@author(Thierry DIJOUX <tjr.dijoux@gmail.com>)
Class for JQTab.}
unit JQTab;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, jqBase;

Type

  {
    @abstract(Class for JQTab item)
    Class for JQTab item.
  }
  TJQTabItem = class
  private
    FTitle: string;
    FContent: TStrings;
    FUseAjax: boolean;
    FAjaxURL: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddContent(AValue: string);
    // Title of the tab
    property Title: string read FTitle write FTitle;
    // Content of the tab
    property Content: TStrings read FContent write FContent;
    // Use ajax to fill the tab
    property UseAjax: boolean read FUseAjax write FUseAjax;
    // URL for the ajax call
    property AjaxURL: string read FAjaxURL write FAjaxURL;
  end;

  {
    @abstract(Class for JQTab)
    Class for JQTab.
  }
  TJQTab = class(TJQBase)
  private
    FTabList: TObjectList;
    FIsCollapsible: boolean;
    FOpenOnMouseOver: boolean;
    function GetTab(AIndex: integer): TJQTabItem;
    function GetCount: integer;
  protected
    function GetContent: string; override;
    function GetJavaScript(location: ExtraJSloc): string; override;
    function GetCss: string; override;
  public
    constructor Create;
    destructor Destroy; override;
    function AddTab(ATabName: string): integer;
    procedure Clear;
    property Tab[AItemIndex: integer]: TJQTabItem read GetTab;
    property TabCount: integer read GetCount;
    // Tab is collapsible or not
    property Collapsible: boolean read FIsCollapsible write FIsCollapsible;
    // Opens tab automatically on mouse over
    property OpenOnMouseOver: boolean read FOpenOnMouseOver write FOpenOnMouseOver;
  end;

implementation

{ TJQTab }

constructor TJQTab.Create;
begin
    inherited Create;
    FTabList:=TObjectList.create(true);
    FIsCollapsible:=false;
end;

destructor TJQTab.Destroy;
begin
    FTabList.Free;
    inherited Destroy;
end;

function TJQTab.GetContent: string;
var i : integer;
begin
    FContent.Clear;
    FContent.Add('<div id="' + FId + '">');
    FContent.Add('  <ul>');
    for i:=0 to FTabList.Count-1 do begin
        if TJQTabItem(FTabList.Items[i]).UseAjax then begin
            FContent.Add('<li><a href="' + TJQTabItem(FTabList.Items[i]).AjaxURL + '">' + TJQTabItem(FTabList.Items[i]).Title +'</a></li>')
        end else begin
            FContent.Add('<li><a href="#tabs-' + intToStr(i+1) + '">' + TJQTabItem(FTabList.Items[i]).Title +'</a></li>');
        end;
    end;
    FContent.Add('  </ul>');
    for i:=0 to FTabList.Count-1 do begin
        if not TJQTabItem(FTabList.Items[i]).UseAjax then begin
            FContent.Add(' <div id="tabs-' + intToStr(i+1) + '">');
            FContent.Add(TJQTabItem(FTabList.Items[i]).Content.Text);
            FContent.Add(' </div>');
        end;
    end;
    FContent.Add('</div>');
    Result:=FContent.Text;
end;

function TJQTab.GetJavaScript(location: ExtraJSloc): string;
var jsAjax : boolean;
    i : integer;
begin
    if location<>locHeader then begin
        Result:='';
        exit;
    end;
    FJsHeader.Clear;
    FJsHeader.Add('<script>');
    FJsHeader.Add(' $(function() {');
    FJsHeader.Add('  $( "#' + FId + '" ).tabs(');
    if FIsCollapsible then FJsHeader.Add('{ collapsible: true }');
    if FOpenOnMouseOver then begin
        if FIsCollapsible then FJsHeader.Add(',{ event: "mouseover" }')
                          else FJsHeader.Add('{ event: "mouseover" }');
    end;
    FJsHeader.Add(');');
    FJsHeader.Add(' });');
    FJsHeader.Add('</script>');
    for i:=0 to TabCount-1 do begin
        if Tab[i].UseAjax then begin
            jsAjax:= true;
            break;
        end;
    end;
    if jsAjax then begin
        FJsHeader.Add('<script>');
        FJsHeader.Add(' $(function() {');
        FJsHeader.Add('  $( "#' + FId + '" ).tabs({');
        FJsHeader.Add('   ajaxOptions: {');
        FJsHeader.Add('     error: function( xhr, status, index, anchor ) {');
        FJsHeader.Add('     $( anchor.hash ).html(');
        FJsHeader.Add('     "Couldn''t load this tab. We''ll try to fix this as soon as possible. " +');
        FJsHeader.Add('     "If this wouldn''t be a demo." );');
        FJsHeader.Add('     }');
        FJsHeader.Add('   }');
        FJsHeader.Add('  });');
        FJsHeader.Add(' });');
        FJsHeader.Add('</script>');
    end;
    Result:=FJsHeader.Text;
end;

function TJQTab.GetCss: string;
begin
    FCss.Clear;
    Result:=FCss.Text;
end;

function TJQTab.GetTab(AIndex: integer): TJQTabItem;
begin
    Result:=TJQTabItem(FTabList.Items[AIndex]);
end;

function TJQTab.GetCount: integer;
begin
    Result:=FTabList.Count;
end;

function TJQTab.AddTab(ATabName: string): integer;
var NewTab: TJQTabItem;
begin
    NewTab:=TJQTabItem.Create;
    NewTab.Title:=ATabName;
    Result:=FTabList.Add(NewTab);
end;

procedure TJQTab.Clear;
begin
    FTabList.Clear;
end;

{ TJQTabItem }

constructor TJQTabItem.Create;
begin
    FContent:=TStringList.Create;
end;

destructor TJQTabItem.Destroy;
begin
    FContent.Free;
    inherited Destroy;
end;

procedure TJQTabItem.AddContent(AValue: string);
begin
    FContent.Add(AValue);
end;

end.

