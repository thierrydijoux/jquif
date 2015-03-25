unit JQAccordion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, jqBase, BaseList;

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
    function GetJs: string; override;
  public
    constructor create;
    destructor destroy; override;
    function AddAccordion(ATabName: string): integer;
    procedure UseResizer(AWidth, AHeight, AMinHeight: integer; APreview, AAnimate: boolean);
    property Tab[AItemIndex: integer]: TJQAccordionItem read GetAccordion;
    property TabCount: integer read GetCount;
    property Collapsible: boolean read FIsCollapsible write FIsCollapsible;
    property OpenOnMouseOver: boolean read FOpenOnMouseOver write FOpenOnMouseOver;
  end;

implementation

{ TJQAccordion }

function TJQAccordion.GetContent: string;
Var
  i: integer;
begin
  FContent.Clear;
  if FUseResizer then
    FContent.Add('<div id="accordionResizer" style="padding:10px; width:' + inttostr(FResizer.Width) + 'px; height:' + inttostr(FResizer.Height) + 'px;" class="ui-widget-content">');
  FContent.Add('<div id="' + FId + '">');
  for i:= 0 to FAccordionList.Count -1 do
  begin
    FContent.Add('<h3><a href="#accordion-' + intToStr(i+1) + '">' + TJQAccordionItem(FAccordionList.Items[i]).Title +'</a></h3>');
    FContent.Add('	<div>');
    FContent.Add(TJQAccordionItem(FAccordionList.Items[i]).Content.Text);
    FContent.Add('	</div>');
  end;
  if FUseResizer then
      FContent.Add('	</div>');
  result:= FContent.Text;
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

function TJQAccordion.GetJs: string;
Var
  BoolPreview, BoolAnimate: string;
begin
  BoolPreview:= 'false';
  BoolAnimate:= 'false';
  if FResizer.Preview then
    BoolPreview:= 'true';

  if FResizer.Animate then
    BoolAnimate:= 'true';


  FJs.Clear;
  FJs.Add('<script>');
  FJs.Add('	$(function() {');
  FJs.Add('		$( "#' + FId + '" ).accordion(');
  FJs.Add('{ animated: "slide" }');
  if FIsCollapsible then
    FJs.Add('{ collapsible: true }');
  if FOpenOnMouseOver then
    if FIsCollapsible then
      FJs.Add(',{ event: "mouseover" }')
    else
      FJs.Add('{ event: "mouseover" }');

  FJs.Add(');');
  FJs.Add('	});');
  if FUseResizer then
  begin

    FJs.Add('$(function() {');
    FJs.Add('		$( "#accordionResizer" ).resizable({');
    FJs.Add('			ghost: ' + BoolPreview + ',');
    FJs.Add('			helper: "ui-resizable-helper",');
    FJs.Add('                   animate: ' + BoolAnimate);
    FJs.Add('		});');
    FJs.Add('	});');

    FJs.Add('$(function() {');
    FJs.Add('		$( "#accordionResizer" ).resizable({');
    FJs.Add('			minHeight: ' + inttostr(FResizer.MinHeight) + ',');
    FJs.Add('			resize: function() {');
    FJs.Add('				$( "#accordion" ).accordion( "resize" );');
    FJs.Add('			}');
    FJs.Add('		});');
    FJs.Add('	});');
  end;

  FJs.Add('</script>');

  result:= FJs.Text;
end;

function TJQAccordion.GetAccordion(AIndex: integer): TJQAccordionItem;
begin
  result:= TJQAccordionItem(FAccordionList.Items[AIndex]);
end;

function TJQAccordion.GetCount: integer;
begin
  result:= FAccordionList.Count;
end;

function TJQAccordion.AddAccordion(ATabName: string): integer;
Var
  NewTab: TJQAccordionItem;
begin
  NewTab:= TJQAccordionItem.Create;
  NewTab.Title:= ATabName;
  result:= FAccordionList.Add(NewTab);
end;

constructor TJQAccordion.create;
begin
  inherited Create;
  FAccordionList:= TBaseList.Create;
  FIsCollapsible:= false;
  FUseResizer:= false;
end;

destructor TJQAccordion.destroy;
begin
  FAccordionList.Free;
  inherited destroy;
end;

{ TJQAccordionItem }

procedure TJQAccordionItem.AddContent(AValue: string);
begin
  FContent.Add(AValue);
end;

constructor TJQAccordionItem.Create;
begin
  FContent:= TStringList.Create;
end;

destructor TJQAccordionItem.Destroy;
begin
  FContent.Free;
  inherited destroy;
end;

end.

