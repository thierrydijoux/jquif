unit JQAccordion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JQBase, BaseList;

Type

  TResizer = record
    Height: integer;
    Width: integer;
    MinHeight: integer;
    Preview: boolean;
    Animate: boolean;
  end;

  TJQAccordionItem = class
  private
    FTitle: string;
    FContent: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddContent(AValue: string);
    property Title: string read FTitle write FTitle;
    property Content: TStrings read FContent write FContent;
  end;

  TJQAccordion = class(TJQBase)
  private
    FAccordionList: TBaseList;
    FIsCollapsible: boolean;
    FOpenOnMouseOver: boolean;
    FUseResizer: boolean;
    FResizer: TResizer;
    function GetAccordion(AIndex: integer): TJQAccordionItem;
    function GetCount: integer;
  protected
    function GetContent: string; override;
    function GetJavaScript(location: ExtraJSloc): string; override;
    function GetCss: string; override;
  public
    constructor Create;
    destructor Destroy; override;
    function AddAccordion(ATabName: string): integer;
    procedure UseResizer(AWidth, AHeight, AMinHeight: integer; APreview, AAnimate: boolean);
    property Tab[AItemIndex: integer]: TJQAccordionItem read GetAccordion;
    property TabCount: integer read GetCount;
    property Collapsible: boolean read FIsCollapsible write FIsCollapsible;
    property OpenOnMouseOver: boolean read FOpenOnMouseOver write FOpenOnMouseOver;
  end;

implementation

{ TJQAccordion }

constructor TJQAccordion.Create;
begin
    inherited Create;
    FAccordionList:=TBaseList.Create;
    FIsCollapsible:=false;
    FUseResizer:=false;
end;

destructor TJQAccordion.Destroy;
begin
    FAccordionList.Free;
    inherited Destroy;
end;

procedure TJQAccordion.UseResizer(AWidth, AHeight, AMinHeight: integer; APreview, AAnimate: boolean);
begin
    FUseResizer:= true;
    FResizer.Height:= AHeight;
    FResizer.Width:= AWidth;
    FResizer.MinHeight:= AMinHeight;
    FResizer.Preview:= APreview;
    FResizer.Animate:= AAnimate;
end;

function TJQAccordion.GetContent: string;
var i: integer;
begin
    FContent.Clear;
    if FUseResizer then begin
        FContent.Add('<div id="accordionResizer" style="padding:10px; width:' + inttostr(FResizer.Width) + 'px; height:' + inttostr(FResizer.Height) + 'px;" class="ui-widget-content">');
    end;
    FContent.Add('<div id="' + FId + '">');
    for i:=0 to FAccordionList.Count-1 do begin
        FContent.Add('<h3><a href="#accordion-' + intToStr(i+1) + '">' + TJQAccordionItem(FAccordionList.Items[i]).Title +'</a></h3>');
        FContent.Add('<div>');
        FContent.Add(TJQAccordionItem(FAccordionList.Items[i]).Content.Text);
        FContent.Add('</div>');
    end;
    if FUseResizer then begin
        FContent.Add('</div>');
    end;
    result:=FContent.Text;
end;

function TJQAccordion.GetJavaScript(location: ExtraJSloc): string;
var BoolPreview, BoolAnimate: string;
begin
    if location<>locHeader then begin
        Result:='';
        exit;
    end;
    BoolPreview:= 'false';
    BoolAnimate:= 'false';
    if FResizer.Preview then BoolPreview:= 'true';
    if FResizer.Animate then BoolAnimate:= 'true';
    FJsHeader.Clear;
    FJsHeader.Add('<script>');
    FJsHeader.Add('	$(function() {');
    FJsHeader.Add('		$( "#' + FId + '" ).accordion(');
    FJsHeader.Add('{ animated: "slide" }');
    if FIsCollapsible then FJsHeader.Add('{ collapsible: true }');
    if FOpenOnMouseOver then begin
        if FIsCollapsible then FJsHeader.Add(',{ event: "mouseover" }')
                          else FJsHeader.Add('{ event: "mouseover" }');
    end;
    FJsHeader.Add(');');
    FJsHeader.Add(' });');
    if FUseResizer then begin
        FJsHeader.Add('$(function() {');
        FJsHeader.Add('  $( "#accordionResizer" ).resizable({');
        FJsHeader.Add('    ghost: ' + BoolPreview + ',');
        FJsHeader.Add('    helper: "ui-resizable-helper",');
        FJsHeader.Add('    animate: ' + BoolAnimate);
        FJsHeader.Add('  });');
        FJsHeader.Add(' });');
        FJsHeader.Add('$(function() {');
        FJsHeader.Add('  $( "#accordionResizer" ).resizable({');
        FJsHeader.Add('   minHeight: ' + inttostr(FResizer.MinHeight) + ',');
        FJsHeader.Add('   resize: function() {');
        FJsHeader.Add('    $( "#accordion" ).accordion( "resize" );');
        FJsHeader.Add('   }');
        FJsHeader.Add('  });');
        FJsHeader.Add(' });');
    end;
    FJsHeader.Add('</script>');
    result:=FJsHeader.Text;
end;

function TJQAccordion.GetCss: string;
begin
    FCss.Clear;
    Result:=FCss.Text;
end;

function TJQAccordion.GetAccordion(AIndex: integer): TJQAccordionItem;
begin
    result:=TJQAccordionItem(FAccordionList.Items[AIndex]);
end;

function TJQAccordion.GetCount: integer;
begin
    result:=FAccordionList.Count;
end;

function TJQAccordion.AddAccordion(ATabName: string): integer;
var NewTab: TJQAccordionItem;
begin
    NewTab:=TJQAccordionItem.Create;
    NewTab.Title:=ATabName;
    result:=FAccordionList.Add(NewTab);
end;

{ TJQAccordionItem }

constructor TJQAccordionItem.Create;
begin
    FContent:=TStringList.Create;
end;

destructor TJQAccordionItem.Destroy;
begin
    FContent.Free;
    inherited Destroy;
end;

procedure TJQAccordionItem.AddContent(AValue: string);
begin
    FContent.Add(AValue);
end;

end.

