unit JQEvents;
{< @abstract(Class for Events)
   @author(Thierry DIJOUX <tjr.dijoux@gmail.com>)
   Class for Events
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type
    // Type of JQUERY event: See https://api.jquery.com/on/
    // List of posible events: http://www.w3.org/TR/DOM-Level-3-Events/#event-types-list
    TJsEvent = (
        jeClick,       //< On Click event
        jeDoubleClick, //< On Doble-click event
        jeMouseEnter,  //< On MouseEnter event
        jeMouseLeave,  //< On MouseLeave event
        jeMouseOver,   //< On MouseOver
        jeFocus,       //< On receive focus
        jeBlur         //< On lost focus
        );

    TJavaScriptEvent = class
    protected
        FEventType: TJsEvent;
        FScript: TStrings;
        FSelector: string;
        FData: string;
        FGeneratedScript: TStrings;
        function EventTypeAsString: string;
        function GetGeneratedScript: string;
    public
        constructor Create(AEventType: TJsEvent);
        destructor Destroy; override;
        property Selector: string read FSelector write FSelector;
        property Data: string read FData write FData;
        property Script: TStrings read FScript write FScript;
        property GeneratedScript: string read GetGeneratedScript;
    end;

implementation

constructor TJavaScriptEvent.Create(AEventType: TJsEvent);
begin
    FEventType:=AEventType;
    FScript:=TStringList.Create;
    FSelector:='';
    FData:='';
    FGeneratedScript:=TStringList.Create;
end;

destructor TJavaScriptEvent.Destroy;
begin
    FGeneratedScript.Free;
    FScript.Free;
    inherited Destroy;
end;

function TJavaScriptEvent.GetGeneratedScript: string;
begin
    FGeneratedScript.Clear;
    FGeneratedScript.Add('<script>');
    FGeneratedScript.Add('  $("#'+FSelector+'").on('+
                         '  "'+EventTypeAsString+'", null, ');
    if FData<>'' then FGeneratedScript.Add(FData+', ')
                 else FGeneratedScript.Add('null, ');
    FGeneratedScript.Add('  function(){'+FScript.Text);
    FGeneratedScript.Add('  });');
    FGeneratedScript.Add('</script>');
    Result:=FGeneratedScript.Text;
end;

function TJavaScriptEvent.EventTypeAsString: string;
begin
    case FEventType of
        jeClick: Result:='click';
        jeDoubleClick: Result:='dblclick';
        jeMouseEnter: Result:='mouseenter';
        jeMouseLeave: Result:='mouseleave';
        jeMouseOver: Result:='mouseover';
        jeFocus: Result:='focus';
        jeBlur: Result:='blur';
    else Result:='invalidEvent';
    end;
end;



end.
