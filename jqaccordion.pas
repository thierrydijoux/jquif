unit JQAccordion;
{< @abstract(jQuery-UI Accordion Widget)
   @author(Mario Guerra <mguerra13@gmail.com>)
   See http://api.jqueryui.com/accordion/
   API corresponds to jQuery-UI version 1.11.4
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, JQBase, BaseList;

type
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
       FResizer: TResizer;
       FActivePage: integer;
       FAnimation: string;
       FIsCollapsible: boolean;
       FOpenOnMouseOver: boolean;
       FUseResizer: boolean;
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
       function GetContentHeader:string;
       function GetContentBottom:string;
       // AItemIndex is 1 based
       function GetContentItemHeader(AItemIndex: integer):string;
       function GetContentItemBottom(AItemIndex: integer):string;
       property Tab[AItemIndex: integer]: TJQAccordionItem read GetAccordion;
       property TabCount: integer read GetCount;
       // Which panel appears initially open with a 1 based index
       // Setting to 0 will collapse all panels (requires collapsible to be true)
       property ActivePage: integer read FActivePage write FActivePage;
       // If and how to animate changing panels, could be false or any of the
       // easing functions defined in http://api.jqueryui.com/easings
       property Animation: string read FAnimation write FAnimation;
       // Whether all the sections can be closed at once, allows collapsing the active section
       property Collapsible: boolean read FIsCollapsible write FIsCollapsible;
       // The headers will react to mouseover in order to activate the associated panel
       property OpenOnMouseOver: boolean read FOpenOnMouseOver write FOpenOnMouseOver;
    end;

implementation

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

{ TJQAccordion }

constructor TJQAccordion.Create;
begin
    inherited Create;
    FAccordionList:=TBaseList.Create;
    FActivePage:=1;
    FAnimation:='swing';
    FIsCollapsible:=true;
    FOpenOnMouseOver:=false;
    FUseResizer:=false;
end;

destructor TJQAccordion.Destroy;
begin
    FAccordionList.Free;
    inherited Destroy;
end;

function TJQAccordion.AddAccordion(ATabName: string): integer;
var NewTab: TJQAccordionItem;
begin
    NewTab:=TJQAccordionItem.Create;
    NewTab.Title:=ATabName;
    Result:=FAccordionList.Add(NewTab);
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

function TJQAccordion.GetAccordion(AIndex: integer): TJQAccordionItem;
begin
    Result:=TJQAccordionItem(FAccordionList.Items[AIndex]);
end;

function TJQAccordion.GetCount: integer;
begin
    Result:=FAccordionList.Count;
end;

function TJQAccordion.GetContentHeader:string;
begin
    Result:='';
    if FUseResizer then begin
        Result:=Result+'<div id="'+FId+'Resizer" class="ui-widget-content" '+
                       '  style="padding:10px; width:'+IntToStr(FResizer.Width)+'px; '+
                       '         height:'+IntToStr(FResizer.Height)+'px;">';
    end;
    Result:=Result+'<div id="'+FId+'">';
end;

function TJQAccordion.GetContentBottom:string;
begin
    Result:='</div>';
    if FUseResizer then begin
        Result:=Result+'</div>';
    end;
end;

// AItemIndex is 1 based
function TJQAccordion.GetContentItemHeader(AItemIndex: integer):string;
begin
    Result:='<h3>'+TJQAccordionItem(FAccordionList.Items[AItemIndex-1]).Title+'</h3>'+
            '<div>';
end;

function TJQAccordion.GetContentItemBottom(AItemIndex: integer):string;
begin
    Result:='</div>';
end;

function TJQAccordion.GetContent: string;
var i: integer;
begin
    // The generated HTML is splitted into four public parts to gain flexibility
    // and the hability to put this component inside a JQForm
    FContent.Clear;
    FContent.Add(GetContentHeader);
    for i:=0 to FAccordionList.Count-1 do begin
        FContent.Add(GetContentItemHeader(i+1));
        FContent.Add(TJQAccordionItem(FAccordionList.Items[i]).Content.Text);
        FContent.Add(GetContentItemBottom(i+1));
    end;
    FContent.Add(GetContentBottom);
    result:=FContent.Text;
end;

function TJQAccordion.GetJavaScript(location: ExtraJSloc): string;
begin
    if location<>locBodyBottom then begin
        Result:='';
        exit;
    end;
    FJsBottom.Clear;
    FJsBottom.Add('<script>');
    FJsBottom.Add('  $(function() {');
    FJsBottom.Add('    $( "#'+FId+'" ).accordion({');
    if FActivePage=0 then FJsBottom.Add(' active: false')
                     else FJsBottom.Add(' active: '+IntToStr(FActivePage-1));  // active is 0 based
    if FAnimation<>'' then FJsBottom.Add(', animate: "'+FAnimation+'"');
    if FIsCollapsible then begin
        FJsBottom.Add(', collapsible: true');  // default is false
        if FOpenOnMouseOver then FJsBottom.Add(', event: "mouseover"'); // default is "click"
    end;
    FJsBottom.Add('    });');
    FJsBottom.Add('  });');
    if FUseResizer then begin
        FJsBottom.Add('$(function() {');
        FJsBottom.Add('  $( "#'+FId+'Resizer" ).resizable({');
        FJsBottom.Add('    helper: "resizable-helper" '); // default is "false"
        if FResizer.Preview then FJsBottom.Add(', ghost: true'); // default is false
        if FResizer.Animate then FJsBottom.Add(', animate: true'); // default is false
        FJsBottom.Add('  });');
        FJsBottom.Add('});');
        FJsBottom.Add('$(function() {');
        FJsBottom.Add('  $( "#'+FId+'Resizer" ).resizable({');
        FJsBottom.Add('    minHeight: ' + inttostr(FResizer.MinHeight) + ',');
        FJsBottom.Add('    resize: function() {');
        FJsBottom.Add('      $( "#'+FId+'" ).accordion( "resize" );');
        FJsBottom.Add('    }');
        FJsBottom.Add('  });');
        FJsBottom.Add('});');
    end;
    FJsBottom.Add('</script>');
    result:=FJsBottom.Text;
end;

function TJQAccordion.GetCss: string;
begin
    FCss.Clear;
    Result:=FCss.Text;
end;

end.

