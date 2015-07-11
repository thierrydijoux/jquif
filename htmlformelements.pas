unit HtmlFormElements;
{< @abstract(Elements that goes inside a form)
   @author(Thierry DIJOUX <tjr.dijoux@gmail.com>)
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type
    // These names should be the HTML5 types for input elements.
    // See http://www.w3.org/TR/html-markup/input for list of valid types.
    // Almost all have the <input> tag, but etTextArea and etSelect have specific HTML markup.
    TElementType = (etNone,
                    etText, etPassword, etCheckbox, etRadio, etButton,
                    etSubmit, etReset, etFile, etHidden, etImage,
                    etDateTime, etDateTimeLocal, etDate, etMonth,
                    etTime, etWeek, etNumber, etRange, etEmail, etURL,
                    etSearch, etTel, etColor,
                    etTextArea, etSelect );

    // Rules and messages from jQuery Validation Plugin as of version 1.13.1
    // See http://jqueryvalidation.org/documentation for the list of
    // current built-in validation methods.
    // vmRemote id for an Ajax server request (XMLHttpRequest) to validate the
    // field. See http://jqueryvalidation.org/remote-method
    // Note that this remote Ajax request could be made manually inside a vmCustom method.
    // vmCustom is for a custom validation method in JavaScript code, see also
    // the additional-methods.js found in jQuery Validation Plugin distribution.
    TValidationMethods = (vmRequired, vmMinlength, vmMaxlength, vmRangelength,
        vmMin, vmMax, vmRange, vmEmail, vmUrl, vmDate, vmDateISO,
        vmNumber, vmDigits, vmCreditcard, vmEqualTo,
        vmRemote, vmCustom );

    // Abstract class not intended to be instantiated.
    TBaseElement = class(TObject)
        STD, ETD: string[5];
        procedure tableMarks(AInTable: boolean);
    protected
        FId: string;
        FName: string;
        FClasse: string;
        FElementType: TElementType;
        FToolTips: string;
        FExtraParam: string;
        function GetHtml(AInTable: boolean): string; virtual; abstract;
    public
        property Id: string read FId write FId;
        property Name: string read FName write FName;
        property Classe: string read FClasse write FClasse;
        property ElementType: TElementType read FElementType;
        property ToolTips: string read FToolTips write FToolTips;
        property ExtraParam: string read FExtraParam write FExtraParam;
        property GeneratedHtml[AInTable: boolean]: string read GetHtml;
    end;

    // This element is used to place arbitrary HTML code between other input
    // elements in a form. This code should provide the proper tags if the
    // form has its rows in a table
    TInlineHTML = class(TBaseElement)
    protected
        FContent: string;
    protected
        function GetHtml(AInTable: boolean): string; override;
    public
        property Content: string read FContent write FContent;
    end;

    // Common ancestor class for all types of input elements in a form.
    // Is an abstract class not intended to be instantiated.
    // Introduces element validation via jQuery Validation Plugin.
    TBaseInput = class(TBaseElement)
    protected
        FCaption: string;
        FValue: string;
        FValidationH: string; //< Inserted as HTML5 input parameters
        FValidationR: string; //< Inserted as validation rules
        FValidationM: string; //< Inserted as validation messages
        FValidationC: string; //< For custom methods
        function GetHtml(AInTable: boolean): string; override;
    public
        constructor Create;
        property Caption: string read FCaption write FCaption;
        property Value: string read FValue write FValue;
        property GeneratedRules: string read FValidationR;
        property GeneratedMessages: string read FValidationM;
        property GeneratedCustom: string read FValidationC write FValidationC;
        function VMethodAsString(AMethod:TValidationMethods): string;
        procedure AddValidation(AMethod:TValidationMethods; ARule:string; AMessage:string);
    end;

    TInputText = class(TBaseInput)
    public
        constructor Create;
    end;

    TInputPassword = class(TBaseInput)
    public
        constructor Create;
    end;

    TTextArea = class(TBaseInput)
    protected
        function GetHtml(AInTable: boolean): string; override;
    public
        constructor Create;
    end;

    TSelectItem = record
        Caption: string;
        Value: string;
        selected: boolean;
        disabled: boolean;
    end;

    TSelect = class(TBaseInput)
    protected
        FItemsList: array of TSelectItem;  // zero based
        function GetHtml(AInTable: boolean): string; override;
    public
        constructor Create;
        constructor Create(AItemList: TStringList); overload;
        destructor Destroy; override;
        function AddItem(ACaption: string; AValue: string): integer;
        function AddItem(ACaption: string; AValue: string;
            ASelected, ADisabled: boolean): integer;
        procedure SelectedItem(AIndex: integer); overload;
        procedure SelectedItem(AValue: string); overload;
    end;

    TInputRadio = class(TBaseInput)
    public
        constructor Create;
    end;

    TInputCheckBox = class(TBaseInput)
    public
        constructor Create;
    end;

    TFile = class(TBaseInput)
    public
        constructor Create;
    end;

    TInputSubmit = class(TBaseInput)
    public
        constructor Create;
    end;

    TInputReset = class(TBaseInput)
    public
        constructor Create;
    end;

    TInputHidden = class(TBaseInput)
    public
        constructor Create;
    end;

implementation

uses
    TypInfo;

{ TBaseElement --------------------------------------------------------------- }

procedure TBaseElement.tableMarks(AInTable: boolean);
begin
    if AInTable then begin
        STD:= '<td>';
        ETD:= '</td>';
    end else begin
        STD:= '';
        ETD:= '';
    end;
end;

{ TInlineHTML ------------------------------------------------------------------ }

// If AInTable the Content should provide by itself the <tr></tr> and inners <td></td> if needed
function TInlineHTML.GetHtml(AInTable: boolean): string;
begin
    tableMarks(AInTable);
    Result:=FContent;
end;

{ TBaseInput ----------------------------------------------------------------- }

constructor TBaseInput.Create;
begin
    FElementType:= etNone;
end;

function TBaseInput.GetHtml(AInTable: boolean): string;
var html, AType, AValue, AClass: string;
begin
    html:='';
    AValue:='';
    AClass:='';
    AType:=GetEnumName(TypeInfo(TElementType),ord(FElementType));
    AType:=lowercase(copy(AType,3,100));
    if FValue<>'' then AValue:='value="'+FValue+'" ';
    if FClasse<>'' then AClass:='class="'+FClasse+'" ';
    tableMarks(AInTable);
    html:=STD;
    if FCaption<>'' then begin
        html:=html+'<label for="'+FId+'">'+FCaption+'</label>';
    end;
    html:=html+ETD;
    html:=html+STD+
          '<input type="'+Atype+'" name="'+FName+'" id="'+FId+'" '+
          AValue + AClass + FValidationH+' ' + FToolTips+' '+ FExtraParam+ '>'+
          ETD;
    if AInTable then begin
        // If the "label for" is present (with proper id and class=error)
        // it is automatically used by the Validation Plugin.
        html:=html+STD+
              '<label for="'+FId+'" id="'+FId+'-error" class="error"></label>'+
              ETD;
    end else begin
        // When not in a table the "label for" is automatically generated
        // by the Validation Plugin with the proper id and class=error
    end;
    Result:=Html;
end;

function TBaseInput.VMethodAsString(AMethod:TValidationMethods): string;
var auxS : string;
begin
    auxS:=GetEnumName(TypeInfo(TValidationMethods),ord(AMethod));
    auxS:=lowerCase(copy(auxS,3,1))+copy(auxS,4,100);
    Result:=auxS;
end;

// The following are inserted as HTML5 input parameters: required, minlength, maxlength, min, max
// The others will be put inside validate() rules as json data. Special treatment
// for custom rules and for equalTo and for fields requiring another field.
// All messages are going inside validate() as json data.
procedure TBaseInput.AddValidation(AMethod:TValidationMethods; ARule:string; AMessage:string);
var auxM : string;
begin
    auxM:=VMethodAsString(AMethod);
    case AMethod of
        vmRequired: begin
            if ARule='true' then begin
                FValidationH:=FValidationH+' '+auxM;
            end else if ARule<>'false' then begin
                // This is used for one field requiring another field (by Id)
                if FValidationR<>'' then FValidationR:=FValidationR+',';
                FValidationR:=FValidationR+' '+FName+': {'+auxM+': "#'+ARule+'"}';
            end;
          end;
        vmMinlength, vmMaxlength, vmMin, vmMax: begin
            FValidationH:=FValidationH+' '+auxM+'="'+ARule+'"';
          end;
        vmEqualTo: begin
            // EqualTo requires the id of the field being referenced
            if FValidationR<>'' then FValidationR:=FValidationR+',';
            FValidationR:=FValidationR+' '+FName+': {'+auxM+': "#'+ARule+'"}';
          end;
        vmNumber,vmDate,vmDateISO,vmEmail,vmUrl : begin
            if ARule='true' then begin
                if FValidationR<>'' then FValidationR:=FValidationR+',';
                FValidationR:=FValidationR+' '+FName+': {'+auxM+': '+ARule+'}';
            end;
          end;
        vmCustom : begin
            // Here the rule is the name of the custom method
            if FValidationR<>'' then FValidationR:=FValidationR+',';
            FValidationR:=FValidationR+' '+FName+': {'+ARule+': true}';
          end;
        else begin
            // vmRemote is included in this default case
            if FValidationR<>'' then FValidationR:=FValidationR+',';
            FValidationR:=FValidationR+' '+FName+': {'+auxM+': "'+ARule+'"}';
          end;
    end;
    if AMessage<>'<default>' then begin
        if FValidationM<>'' then FValidationM:=FValidationM+',';
        FValidationM:=FValidationM+' '+FName+': {'+auxM+': "'+AMessage+'"}';
    end;
end;

{ TInputText ----------------------------------------------------------------- }

constructor TInputText.Create;
begin
    FElementType := etText;
end;

{ TInputPassword ------------------------------------------------------------- }

constructor TInputPassword.Create;
begin
    FElementType := etPassword;
end;

{ TTextArea ------------------------------------------------------------------ }

constructor TTextArea.Create;
begin
    FElementType := etTextArea;
end;

function TTextArea.GetHtml(AInTable: boolean): string;
var html, AValue, AClass: string;
begin
    html:='';
    AValue:='';
    AClass:='';
    if FValue<>'' then AValue:='value="'+FValue+'" ';
    if FClasse<>'' then AClass:='class="'+FClasse+'" ';
    tableMarks(AInTable);
    html:=STD;
    if FCaption<>'' then begin
        html:=html+'<label for="'+FId+'">'+FCaption+'</label>';
    end;
    html:=html+ETD;
    html:=html+STD+
          '<textarea name="'+FName+'" id="'+FId+'" '+
          AValue + AClass + FValidationH+' ' + FToolTips+' ' + FExtraParam+'>'+
          '</textarea>'+ETD;
    if AInTable then begin
        // If the "label for" is present (with proper id and class=error)
        // it is automatically used by the Validation Plugin.
        html:=html+STD+
              '<label for="'+FId+'" id="'+FId+'-error" class="error"></label>'+
              ETD;
    end else begin
        // When not in a table the "label for" is automatically generated
        // by the Validation Plugin with the proper id and class=error
    end;
    Result:=html;
end;

{ TSelect -------------------------------------------------------------------- }

constructor TSelect.Create;
begin
    FElementType := etSelect;
    SetLength(FItemsList, 0);
end;

constructor TSelect.Create(AItemList: TStringList);
var i: integer;
begin
    inherited Create;
    try
        SetLength(FItemsList, 0);
        for i := 0 to AItemList.Count - 1 do begin
            // The pairs are name=value (name is the caption shown)
            AddItem(AItemList.Names[i], AItemList.Values[AItemList.Names[i]]);
        end;
    except
        on e: Exception do
            raise Exception.Create('TSelect Create: ' + e.Message);
    end;
end;

destructor TSelect.Destroy;
begin
    SetLength(FItemsList, 0);
    inherited Destroy;
end;

function TSelect.GetHtml(AInTable: boolean): string;
var html, OneItem, AClass: string;
    i: integer;
begin
    html:='';
    AClass:='';
    if FClasse<>'' then AClass:='class="'+FClasse+'" ';
    tableMarks(AInTable);
    html:=html+STD;
    if FCaption<>'' then begin
        html:=html+'<label for="'+FId+'">'+FCaption+'</label>';
    end;
    html:=html+ETD;
    html:=html+STD+'<select name="'+FName+'" id="' + FId +'" '+ AClass +
                   FValidationH+' ' + FToolTips+' ' + FExtraParam + '>';
    for i:=0 to Length(FItemsList)-1 do begin
        OneItem:= '<option value="' + FItemsList[i].Value + '" ';
        if FItemsList[i].selected then
            OneItem:= OneItem + 'selected ';
        if FItemsList[i].disabled then
            OneItem:= OneItem + 'disabled ';
        OneItem:=OneItem + '>' + FItemsList[i].Caption + '</option>';
        html:=html+OneItem;
    end;
    html:=html+'</select>'+ETD;
    if AInTable then begin
        // If the "label for" is present (with proper id and class=error)
        // it is automatically used by the Validation Plugin.
        html:=html+STD+
              '<label for="'+FId+'" id="'+FId+'-error" class="error"></label>'+
              ETD;
    end else begin
        // When not in a table the "label for" is automatically generated
        // by the Validation Plugin with the proper id and class=error
    end;
    Result:=html;
end;

function TSelect.AddItem(ACaption: string; AValue: string): integer;
begin
    Result:=AddItem(ACaption, AValue, False, False);
end;

function TSelect.AddItem(ACaption: string; AValue: string;
    ASelected, ADisabled: boolean): integer;
var Count: integer;
begin
    Count:=Length(FItemsList);
    Inc(Count);
    SetLength(FItemsList, Count);
    FItemsList[Count-1].Caption:=ACaption;
    FItemsList[Count-1].Value:=AValue;
    FItemsList[Count-1].selected:=ASelected;
    FItemsList[Count-1].disabled:=ADisabled;
    Result:=Count-1;
end;

// Marks with "selected" the element at index AIndex (base 1)
procedure TSelect.SelectedItem(AIndex: integer);
begin
    FItemsList[AIndex-1].selected:=true;
end;

// Marks with "selected" the first element with value AValue
// If no such element is found, nothing is done
procedure TSelect.SelectedItem(AValue: string);
var i: integer;
begin
    for i:=0 to Length(FItemsList)-1 do begin
        if FItemsList[i].Value=AValue then begin
            FItemsList[i].selected:=true;
            break;
        end;
    end;
end;

{ TInputRadio ---------------------------------------------------------------- }

constructor TInputRadio.Create;
begin
    FElementType := etRadio;
end;

{ TInputCheckBox ------------------------------------------------------------- }

constructor TInputCheckBox.Create;
begin
    FElementType := etCheckBox;
end;

{ TFile ---------------------------------------------------------------------- }

constructor TFile.Create;
begin
    FElementType := etFile;
end;

{ TInputSubmit --------------------------------------------------------------- }

constructor TInputSubmit.Create;
begin
    FElementType := etSubmit;
end;

{ TInputReset ---------------------------------------------------------------- }

constructor TInputReset.Create;
begin
    FElementType := etReset;
end;

{ TInputHidden --------------------------------------------------------------- }

constructor TInputHidden.Create;
begin
    FElementType := etHidden;
end;

end.
