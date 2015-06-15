unit JQForm;
{< @abstract(Render HTML form with input elements)
   @author(Mario Guerra <mguerra13@gmail.com>)
   JQForms modelates the contents and behaivor of HTML forms and its input elements.
   It uses jQuery Validation plugin version 1.13.1, see http://jqueryvalidation.org,
   and can do validations frontside with JavaScript or remote serverside validations.
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Contnrs, HtmlFormElements, JQBase;

type
    // The HTML method the form will use to send data to the server
    TFormMethod = (fmGet, fmPost);
    // To add HTML code before the form, inside the form, or after the form
    TPosHtml = (phBefore, phInline, phAfter);
    // The elements of the form are inside rows of a table or paragraphs
    TRowsInside = (riNone, riTable, riParagraph);

    TJQForm = class(TJQBase)
    private
        FAction: string;
        FExtraParam: string;
        FFormMethod: TFormMethod;
        FBeforeContent: TStrings;
        FAfterContent: TStrings;
        FElements: TObjectList;
        FLegend: string;
        FRowsInside: TRowsInside;
        FLanguage : string;
        FValidationOkText: string;
        FValidationIcons: boolean;
        FValidationRemote : boolean; //< At least one field have remote Ajax validation
        function LanguageFile(code: string): string;
    protected
        function GetContent: string; override;
        function GetJavaScript(location: ExtraJSloc): string; override;
        function GetCss: string; override;
    public
        constructor Create;
        destructor Destroy; override;
        // The HTML id attibute is the main reference for elements.
        // The id attribute specifies a unique id for an HTML element (the value must be
        // unique within the HTML document). The id attribute is most used to point to a
        // style in a style sheet, and by JavaScript (via the HTML DOM) to manipulate the
        // element with the specific id.
        // See http://www.w3schools.com/tags/att_global_id.asp
        procedure AddEdit(AId: string; ACaption: string; AName: string; AValue: string; AClass: string = '');
        procedure AddPassword(AId: string; ACaption: string; AName: string; AValue: string; AClass: string = '');
        procedure AddTextArea(AId: string; ACaption: string; AName: string; AValue: string; AClass: string = '');
        procedure AddSelect(AId: string; ACaption: string; AName: string; AValueList: TStringList; AClass: string = '');
        procedure AddRadio(AId: string; ACaption: string; AName: string; AValue: string; AClass: string = '');
        procedure AddCheckBox(AId: string; ACaption: string; AName: string; AValue: string; AClass: string = '');
        procedure AddFile(AId: string; ACaption: string; AName: string; AValue: string; AClass: string = '');
        procedure AddSubmitButton(AId: string; ACaption: string; AName: string; AValue: string; AClass: string = '');
        procedure AddResetButton(AId: string; ACaption: string; AName: string; AValue: string; AClass: string = '');
        procedure AddHidden(AId: string; ACaption: string; AName: string; AValue: string; AClass: string = '');
        procedure AddHtml(AText: string; APos: TPosHtml = phInline);
        procedure AddToolTip(AId: string; ATitle: string; APlaceholder: string = '');
        procedure AddParameters(AId: string; AParameters: string);
        procedure AddValidation(AId: string; AMethod: TValidationMethods; ARule:string = 'true'; AMessage:string = '<default>');
        property Action: string read FAction write FAction;
        property FormMethod: TFormMethod read FFormMethod write FFormMethod;
        property Caption: string read FLegend write FLegend;
        property RowsInside: TRowsInside read FRowsInside write FRowsInside;
        property Language: string read FLanguage write FLanguage;
        property ValidationOkText: string read FValidationOkText write FValidationOkText;
        property ValidationIcons: boolean read FValidationIcons write FValidationIcons;
        property ExtraParam: string read FExtraParam write FExtraParam;
    end;

implementation

constructor TJQForm.Create;
begin
    inherited Create;
    FBeforeContent:=TStringList.Create;
    FAfterContent:=TStringList.Create;
    FElements:=TObjectList.Create;
    FFormMethod:=fmPost;
    FLegend:='';
    FRowsInside:=riTable;
    FLanguage:='en';
    FValidationOkText:='ok';
    FValidationIcons:=true;
    FValidationRemote:=false;
end;

destructor TJQForm.Destroy;
begin
    FBeforeContent.Free;
    FAfterContent.Free;
    FElements.Free;
    inherited Destroy;
end;

procedure TJQForm.AddEdit(AId: string; ACaption: string; AName: string; AValue: string; AClass: string);
var newElem: TInputText;
begin
    newElem := TInputText.Create;
    newElem.Id := AId;
    newElem.Caption := ACaption;
    newElem.Name := AName;
    newElem.Value := AValue;
    newElem.Classe := AClass;
    FElements.Add(newElem);
end;

procedure TJQForm.AddPassword(AId: string; ACaption: string; AName: string; AValue: string; AClass: string);
var newElem: TInputPassword;
begin
    newElem := TInputPassword.Create;
    newElem.Id := AId;
    newElem.Caption := ACaption;
    newElem.Name := AName;
    newElem.Value := AValue;
    newElem.Classe := AClass;
    FElements.Add(newElem);
end;

procedure TJQForm.AddTextArea(AId: string; ACaption: string; AName: string; AValue: string; AClass: string);
var newElem: TTextArea;
begin
    newElem := TTextArea.Create;
    newElem.Id := AId;
    newElem.Caption := ACaption;
    newElem.Name := AName;
    newElem.Value := AValue;
    newElem.Classe := AClass;
    FElements.Add(newElem);
end;

procedure TJQForm.AddSelect(AId: string; ACaption: string; AName: string; AValueList: TStringList; AClass: string);
var newElem: TSelect;
begin
    newElem := TSelect.Create(AValueList);
    newElem.Id := AId;
    newElem.Caption := ACaption;
    newElem.Name := AName;
    // Value is a TStringList
    newElem.Classe := AClass;
    FElements.Add(newElem);
end;

procedure TJQForm.AddRadio(AId: string; ACaption: string; AName: string; AValue: string; AClass: string);
var newElem: TInputRadio;
begin
    newElem := TInputRadio.Create;
    newElem.Id := AId;
    newElem.Caption := ACaption;
    newElem.Name := AName;
    newElem.Value := AValue;
    newElem.Classe := AClass;
    FElements.Add(newElem);
end;

procedure TJQForm.AddCheckBox(AId: string; ACaption: string; AName: string; AValue: string; AClass: string);
var newElem: TInputCheckBox;
begin
    newElem := TInputCheckBox.Create;
    newElem.Id := AId;
    newElem.Caption := ACaption;
    newElem.Name := AName;
    newElem.Value := AValue;
    newElem.Classe := AClass;
    FElements.Add(newElem);
end;

// In <input> type file the value parameter is not used by browsers for security reasons
procedure TJQForm.AddFile(AId: string; ACaption: string; AName: string; AValue: string; AClass: string);
var newElem: TFile;
begin
    newElem := TFile.Create;
    newElem.Id := AId;
    newElem.Caption := ACaption;
    newElem.Name := AName;
    newElem.Value := AValue;
    newElem.Classe := AClass;
    FElements.Add(newElem);
end;

procedure TJQForm.AddSubmitButton(AId: string; ACaption: string; AName: string; AValue: string; AClass: string);
var newElem: TInputSubmit;
begin
    newElem := TInputSubmit.Create;
    newElem.Id := AId;
    newElem.Caption := ACaption;
    newElem.Name := AName;
    newElem.Value := AValue;
    newElem.Classe := AClass;
    FElements.Add(newElem);
end;

procedure TJQForm.AddResetButton(AId: string; ACaption: string; AName: string; AValue: string; AClass: string);
var newElem: TInputReset;
begin
    newElem := TInputReset.Create;
    newElem.Id := AId;
    newElem.Caption := ACaption;
    newElem.Name := AName;
    newElem.Value := AValue;
    newElem.Classe := AClass;
    FElements.Add(newElem);
end;

procedure TJQForm.AddHidden(AId: string; ACaption: string; AName: string; AValue: string; AClass: string);
var newElem: TInputHidden;
begin
    newElem := TInputHidden.Create;
    newElem.Id := AId;
    newElem.Caption := ACaption;
    newElem.Name := AName;
    newElem.Value := AValue;
    newElem.Classe := AClass;
    FElements.Add(newElem);
end;

procedure TJQForm.AddHtml(AText: string; APos: TPosHtml);
var newElem: TInlineHTML;
begin
    case APos of
        phBefore: begin
            FBeforeContent.Add(AText);
        end;
        phInline: begin
            newElem := TInlineHTML.Create;
            newElem.Content := AText;
            FElements.Add(newElem);
        end;
        phAfter: begin
            FAfterContent.Add(AText);
        end;
    end;
end;

// The ToolTips uses the id (not the name) of the element.
// The ATitle parameter is shown as a floating tooltip or hint over the element
// The APlaceholder parameter is shown inside the element as a grayed text
// If ATitle is defined it supersedes the default error message when required = true.
procedure TJQForm.AddToolTip(AId: string; ATitle: string; APlaceholder: string);
var okFound : boolean;
    i : integer;
    auxS : string;
begin
    okFound:=false;
    for i:=0 to FElements.Count-1 do begin
        if (FElements.Items[i] as TBaseElement).Id=AId then begin
            auxS:='';
            if ATitle<>'' then auxS:=auxS+'title="'+ATitle+'"';
            if APlaceholder<>'' then auxS:=auxS+' placeholder="'+APlaceholder+'"';
            (FElements.Items[i] as TBaseElement).ToolTips:=auxS;
            okFound := true;
            break;
        end;
    end;
    if not okFound then raise Exception.Create('Element with id = '+AId+' not found !');
end;

// The parameters uses the id (not the name) of the element.
procedure TJQForm.AddParameters(AId: string; AParameters: string);
var okFound : boolean;
    i : integer;
begin
    okFound:=false;
    for i:=0 to FElements.Count-1 do begin
        if (FElements.Items[i] as TBaseElement).Id=AId then begin
            (FElements.Items[i] as TBaseElement).ExtraParam:=AParameters;
            okFound := true;
            break;
        end;
    end;
    if not okFound then raise Exception.Create('Element with id = '+AId+' not found !');
end;

(* With jQuery Validation Plugin as of version 1.13.1, there are three different
   ways to achieve field validation, with almost identical final behavoir. For example,
   if we want to validate a text field with a minumun 2 characters and required:

   A) As HTML5 markup, the browser will do the job, without needing any plugin
      <input type="text" name="nombre" id="idName" required minlength="2">

   B) As extended HTML parameters which would be interpreted by Validation plugin
      <input type="text" name="nombre" id="idName" data-rule-required="true" data-rule-minlength="2">
      <script>
      	$("#idFormu").validate();
      </script>

   C) All validation expressed as plugin rules, nothing in HTML
      <input type="text" name="nombre" id="idName">
      <script>
	$("#idFormu").validate(
             { rules: { nombre: { required: true, minlength: 2 } } }
	);
      </script>

   In this JQUIF library we took a mixed approach, the following are inserted
   as HTML5 input parameters: required, minlength, maxlength, min, max (as in A).
   The others types of validation will be put inside validate() rules as json data
   (as in C). All messages are going inside validate() as json data.
   This way if there are troubles with JavaScript not enabled or other JS errors,
   the validation of these types will be enforced by the browser.
*)

// The Validation Plugin internally uses the name (not the id) of the element for purpose of validation.
// If AMessage is missing, the special <default> implies that the default message (localized) is shown.
// If AMessage is empty string, then no message are shown (and no icon also).
// When required = true, the error message prevalence order is:
//   1) The AMessage defined here (not empty, and with ARule='true')
//   2) The tooltip ATitle if defined
//   3) The default (localized) message
procedure TJQForm.AddValidation(AId: string; AMethod: TValidationMethods; ARule:string; AMessage:string);
var okFound : boolean;
    i : integer;
    customName,customMeth : string;
begin
    okFound:=false;
    for i:=0 to FElements.Count-1 do begin
        if (FElements.Items[i] as TBaseElement).Id=AId then begin
            if AMethod<>vmCustom then begin
                (FElements.Items[i] as TBaseInput).AddValidation(AMethod,ARule,AMessage);
            end else begin
                // Only one custom method per element
                customName:=(FElements.Items[i] as TBaseInput).Id+'Custom';
                // ARule contains the inner JavaScript function code, it could use
                // the arguments value, element, param. It must resolve to boolean
                // true when the field is valid.
                customMeth:='$.validator.addMethod("'+customName+'", function(value, element, param) {'+
                            ARule+'}, "'+AMessage+'"); ';
                (FElements.Items[i] as TBaseInput).GeneratedCustom:=customMeth;
                // The following to be inside the validation rules, without message
                ARule:=customName;
                (FElements.Items[i] as TBaseInput).AddValidation(AMethod,ARule,'<default>');
            end;
            if AMethod=vmRemote then FValidationRemote:=true;
            okFound := true;
            break;
        end;
    end;
    if not okFound then raise Exception.Create('Element with id = '+AId+' not found !');
end;

// If the fields will be in rows of a table, the fieldset tags should go
// surounding all the table
function TJQForm.GetContent: string;
var i: integer;
    method: string;
    inTable : boolean;
begin
    FContent.Clear;
    FContent.Add(FBeforeContent.Text);
    case FFormMethod of
        fmPost: method:='post';
        fmGet: method:='get';
    end;
    FContent.Add('<form method="' + method + '" class="' + FClasse + '" '+
                 'id="' + FId + '" action="' + FAction +'" ' + FExtraParam + '>');
    if FLegend<>'' then FContent.Add('<fieldset><legend>'+FLegend+'</legend>');
    inTable:=(FRowsInside=riTable);
    if inTable then FContent.Add('<table>');
    for i:=0 to FElements.Count-1 do begin
        if not (FElements.Items[i] is TInlineHTML) then begin
            if inTable then FContent.Add('<tr>')
            else if FRowsInside=riParagraph then FContent.Add('<p>');
        end;
        // The inTable is needed for the inners <td> inside a row
        FContent.Add((FElements.Items[i] as TBaseElement).GeneratedHtml[inTable]);
        if not (FElements.Items[i] is TInlineHTML) then begin
            if inTable then FContent.Add('</tr>')
            else if FRowsInside=riParagraph then FContent.Add('</p>');
        end;
    end;
    if inTable then FContent.Add('</table>');
    if FLegend<>'' then FContent.Add('</fieldset>');
    FContent.Add('</form>');
    FContent.Add(FAfterContent.Text);
    Result:=FContent.Text;
end;

// Here we put validation rules and validation messages
// All this code should be at the end of page, or be put inside $(document).ready(function(){ }
// The order is important: First the rules, then the messages and last success
function TJQForm.GetJavaScript(location: ExtraJSloc): string;
var i: integer;
    auxS, auxE : string;
begin
    case location of
        locHeader: begin
            FJsHeader.Clear;
            FJsHeader.Add('<script src="../js/jquery.validate-1.13.1.min.js"></script>');
            FJsHeader.Add('<script src="../js/i18n/validate/'+LanguageFile(FLanguage)+'"></script>');
            Result:=FJsHeader.Text;
        end;
        locBodyBottom: begin
            FJsBottom.Clear;
            FJsBottom.Add('<script>');
            // Custom methods
            auxS:='';
            for i:=0 to FElements.Count-1 do begin
                if FElements.Items[i] is TBaseInput then begin
                    auxE:=(FElements.Items[i] as TBaseInput).GeneratedCustom;
                    if auxE<>'' then begin
                        if auxS<>'' then auxS:=auxS+#13;
                        auxS:=auxS+auxE;
                    end;
                end;
            end;
            if auxS<>'' then begin
                FJsBottom.Add(auxS);
            end;
            // Main validate procedure
            FJsBottom.Add('  $("#'+FId+'").validate( {');
            if FValidationRemote then FJsBottom.Add('onkeyup: false,'); // remains onBlur and OnSubmit validation
            auxS:='';
            for i:=0 to FElements.Count-1 do begin
                if FElements.Items[i] is TBaseInput then begin
                    auxE:=(FElements.Items[i] as TBaseInput).GeneratedRules;
                    if auxE<>'' then begin
                        if auxS<>'' then auxS:=auxS+',';
                        auxS:=auxS+auxE;
                    end;
                end;
            end;
            if auxS<>'' then begin
                FJsBottom.Add('rules: { '+auxS+' },');
            end;
            auxS:='';
            for i:=0 to FElements.Count-1 do begin
                if FElements.Items[i] is TBaseInput then begin
                    auxE:=(FElements.Items[i] as TBaseInput).GeneratedMessages;
                    if auxE<>'' then begin
                        if auxS<>'' then auxS:=auxS+',';
                        auxS:=auxS+auxE;
                    end;
                end;
            end;
            if auxS<>'' then begin
                FJsBottom.Add('messages: { '+auxS+' },');
            end;
            FJsBottom.Add('    success: ');
            FJsBottom.Add('      function(label) {');
            FJsBottom.Add('        label.addClass("valid").text("'+FValidationOkText+'");');
            FJsBottom.Add('    }');
            FJsBottom.Add('  });');
            FJsBottom.Add('</script>');
            Result:=FJsBottom.Text;
        end;
    else begin
            Result:='';
        end;
    end;
end;

function TJQForm.GetCss: string;
var space : string;
begin
    FCss.Clear;
    FCss.Add('<link rel="stylesheet" href="../css/cmxformTemplate.css">');
    FCss.Add('<link rel="stylesheet" href="../css/cmxform.css">');
    if not FValidationIcons then begin
        Result:=FCss.Text;
        exit;
    end;
    space:='22px';
    FCss.Add('<style>');
    FCss.Add('  form.'+FClasse+' label.error {');
    if FRowsInside=riTable then FCss.Add('margin-left: '+space+';');
    FCss.Add('    background:url("/css/images/unchecked.gif");');
    FCss.Add('    background-repeat: no-repeat;');
    FCss.Add('    background-position: left center;');
    FCss.Add('    padding-left: '+space+';');
    FCss.Add('  }');
    FCss.Add('  form.'+FClasse+' label.valid {');
    if FRowsInside=riTable then FCss.Add('margin-left: '+space+';');
    FCss.Add('    background:url("/css/images/checked.gif");');
    FCss.Add('    background-repeat: no-repeat;');
    FCss.Add('    background-position: left center;');
    FCss.Add('    padding-left: '+space+';');
    FCss.Add('  }');
    FCss.Add('</style>');
    Result:=FCss.Text;
end;

// Valid codes are ISO 639-1 two-letter codes (with optional country code)
// JavaScript files comes with jQuery Validation plugin
function TJQForm.LanguageFile(code: string): string;
begin
    Result:='messages_'+code+'.js'
end;

end.
