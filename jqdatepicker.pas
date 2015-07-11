unit JQDatePicker;
{< @abstract(Date picker widget)
   @author(Thierry DIJOUX <tjr.dijoux@gmail.com>)
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, JQBase;

type
    // The type determines the HTML render
    TJQDatePickerType = (dptObject, dptInput, dptButton);

    TJQDatePicker = class(TJQBase)
    private
        Ftype: TJQDatePickerType;
        FCaption: string;
        FName: string;
        FValue: string;
        FLanguage: string; //< valid values are ISO 639-1 two-letter codes
        function LanguageFile(code: string): string;
    protected
        function GetContent: string; override;
        function GetJavaScript(location: ExtraJSloc): string; override;
        function GetCss: string; override;
    public
        constructor Create(AType: TJQDatePickerType);
        destructor Destroy; override;
        property Caption: string read FCaption write FCaption;
        property Name: string read FName write FName;
        property Value: string read FValue write FValue;
        property Language: string read FLanguage write FLanguage;
    end;

implementation

constructor TJQDatePicker.Create(AType: TJQDatePickerType);
begin
    inherited Create;
    FType:=AType;
    FCaption:='';
    FName:='';
    FValue:='';
    FLanguage:='en';
end;

destructor TJQDatePicker.Destroy;
begin
    inherited Destroy;
end;

function TJQDatePicker.GetContent: string;
var AValue: string;
begin
    FContent.Clear;
    case FType of
        dptObject: begin
            // There is no HTML render, only the JavaScript code for DatePicker
            // is added by caller to another object like TInputText or TJQButton.
            // In this case the ID of DatePicker should be the same as the object.
        end;
        dptInput: begin
            // HTML render similar to TInputText.GetHtml
            FContent.Add('<label for="'+FId+'">'+FCaption+'</label>');
            if FValue<>'' then AValue:='value="'+FValue+'" ' else AValue:='';
            FContent.Add('<input type="text" class="'+FClasse+'" id="'+FId+'" '+
                         'name="'+FName+'" '+AValue+'>');
        end;
        dptButton: begin
            // HTML render similar to TJQDivButton.GetContent
            FContent.Add('<div role="button" '+
                         'class="ui-button '+FClasse+'" id="'+FId+'">'+
                         '<span>'+FCaption+'</span>'+
                         '</div>');
        end;
    end;
    Result:=FContent.Text;
end;

function TJQDatePicker.GetJavaScript(location: ExtraJSloc): string;
begin
    case location of
        locHeader: begin
            FJsHeader.Clear;
            FJsHeader.Add('<script src="../js/i18n/datepicker/'+LanguageFile(FLanguage)+'"></script>');
            Result:=FJsHeader.Text;
        end;
        locBodyBottom: begin
            FJsBottom.Clear;
            FJsBottom.Add('<script>');
            FJsBottom.Add('  $(function() {');
            FJsBottom.Add('    $( "#'+FId+'" ).datepicker( ');
            FJsBottom.Add('       $.datepicker.regional["'+FLanguage+'"] ');
            FJsBottom.Add('    );');
            FJsBottom.Add('  });');
            FJsBottom.Add('</script>');
            Result:=FJsBottom.Text;
        end;
    else begin
            Result:='';
        end;
    end;
end;

function TJQDatePicker.GetCss: string;
begin
    FCss.Clear;
    Result:=FCss.Text;
end;

// Valid codes are ISO 639-1 two-letter codes (with optional country code)
// See https://github.com/jquery/jquery-ui/tree/master/ui/i18n
function TJQDatePicker.LanguageFile(code: string): string;
begin
    Result:='datepicker-'+code+'.js'
end;

end.

