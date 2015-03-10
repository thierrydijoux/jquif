{
@abstract(Class for Events)
@author(Thierry DIJOUX <tjr.dijoux@gmail.com>)
Class for Events.
}

unit jqEvents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
  { Type of event }
  TJsEvent = (
           // On Click event
           jeClick,
           // On MouseEnter event
           jsMouseEnter,
           // On MouseLeave event
           jsMouseLeave,
           // On MouseOver
           jsMouseOver);

  TJavaScriptEvent = class
  protected
    FEventType: TJsEvent;
    FScript: TStrings;
    FSelector: string;
    FGeneratedScript: TStrings;
    function EventTypeAsString: string;
    function GetGeneratedScript: string;
  public
    constructor Create(AEventType: TJsEvent);
    destructor Destroy; override;
    property Selector: string read FSelector write FSelector;
    property Script: TStrings read FScript write FScript;
    property GeneratedScript: string read GetGeneratedScript;
  end;

implementation

function TJavaScriptEvent.GetGeneratedScript: string;
begin
  FGeneratedScript.Clear;
  FGeneratedScript.Add('<script type="text/javascript">');
  FGeneratedScript.Add('$("' + FSelector + '").delegate("", "' + EventTypeAsString + '", function() {');
  FGeneratedScript.Add(FScript.Text);
  FGeneratedScript.Add('});');
  FGeneratedScript.Add('</script>');
  result:= FGeneratedScript.Text;
end;

function TJavaScriptEvent.EventTypeAsString: string;
begin
  case FEventType of
    jeClick: result:='click';
    jsMouseEnter: result:='mouseenter';
    jsMouseLeave: result:= 'mouseleave';
    jsMouseOver: result:= 'mouseover';
//    else: result:= 'error';
  end;
end;

constructor TJavaScriptEvent.Create(AEventType: TJsEvent);
begin
  FEventType:= AEventType;
  FScript:= TStringList.Create;
  FGeneratedScript:= TStringList.Create;
end;

destructor TJavaScriptEvent.Destroy;
begin
  FGeneratedScript.Free;
  FScript.Free;
  inherited destroy;
end;

end.

