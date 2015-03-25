unit JQForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, HtmlFormElements, JQBase;

Type

  TFormMethod = (fmGet, fmPost);
  TPosHtml = (phBegin, phEnd);

  TJQForm = class(TJQBase)
  private
    FAction: String;
    FExtraParam: string;
    FFormMethod: TFormMethod;
    FBeginContent: TStrings;
    FEndContent: TStrings;
    FElements: TObjectList;
    FFormInTable: boolean;
  protected
    function GetContent: string; override;
    function GetJs: string; override;
    function GetCss: string; override;
  public
    constructor Create;
    destructor Destroy; override;
    // Add an input text
    procedure AddEdit(ACaption: string; AName: string; AClass: string; AId: string; ARequired: boolean; AErrorMsg: string);
    procedure AddEditNumber(ACaption: string; AName: string; AClass: string; AId: string; ARequired: boolean; AErrorMsg: string);
    procedure AddEditDate(ACaption: string; AName: string; AClass: string; AId: string; ARequired: boolean; AErrorMsg: string);
    procedure AddPassword(ACaption: string; AName: string; AClass: string; AId: string; ARequired: boolean; AErrorMsg: string);
    procedure AddTextArea(ACaption: string; AName: string; AClass: string; AId: string; ARequired: boolean; AErrorMsg: string);
    procedure AddSelect(ACaption: string; AName: string; AClass: string; AId: string; ARequired: boolean; AErrorMsg: string; Const AValueList: TStringList);
    procedure AddRadio(ACaption: string; AName: string; AClass: string; AId: string; AValue: string; ARequired: boolean; AErrorMsg: string);
    procedure AddCheckBox(ACaption: string; AName: string; AClass: string; AId: string; AValue: string; ARequired: boolean; AErrorMsg: string);
    procedure AddSubmitButton(AClass: string; AName: string; AValue: string);
    procedure AddResetButton(AClass: string; AValue: string);
    procedure AddFile(ACaption: string; AName: string; AClass: string; AId: string; ARequired: boolean; AErrorMsg: string);
    procedure AddHtml(AText: string; APos: TPosHtml);
    property Action: String read FAction write FAction;
    property FormMethod: TFormMethod read FFormMethod write FFormMethod;
    property FormInTable: boolean read FFormInTable write FFormInTable;
    property ExtraParam: string read FExtraParam write FExtraParam;
  end;

implementation

procedure TJQForm.AddHtml(AText: string; APos: TPosHtml);
begin
  Case APos of
    phBegin:
      begin
        FBeginContent.Add(AText);
      end;
    phEnd:
      begin
        FEndContent.Add(AText);
      end;
  end;
end;

procedure TJQForm.AddFile(ACaption: string; AName: string; AClass: string; AId: string; ARequired: boolean; AErrorMsg: string);
Var
  NewEdit: TFile;
begin
  NewEdit:= TFile.Create;
  NewEdit.Caption:= ACaption;
  NewEdit.Classe:= AClass;
  NewEdit.Name:= AName;
  NewEdit.Id:=AId;
  NewEdit.Required:=ARequired;
  NewEdit.ErrorMessage:=AErrorMsg;
  FElements.Add(NewEdit);
end;

procedure TJQForm.AddEditDate(ACaption: string; AName: string; AClass: string; AId: string; ARequired: boolean; AErrorMsg: string);
Var
  NewEdit: TInputText;
begin
  NewEdit:= TInputText.CreateAsDate;
  NewEdit.Caption:= ACaption;
  NewEdit.Classe:= AClass;
  NewEdit.Name:= AName;
  NewEdit.Id:=AId;
  NewEdit.Required:=ARequired;
  NewEdit.ErrorMessage:=AErrorMsg;
  FElements.Add(NewEdit);
end;

procedure TJQForm.AddEditNumber(ACaption: string; AName: string; AClass: string; AId: string; ARequired: boolean; AErrorMsg: string);
Var
  NewEdit: TInputText;
begin
  NewEdit:= TInputText.CreateAsNumber;
  NewEdit.Caption:= ACaption;
  NewEdit.Classe:= AClass;
  NewEdit.Name:= AName;
  NewEdit.Id:=AId;
  NewEdit.Required:=ARequired;
  NewEdit.ErrorMessage:=AErrorMsg;
  FElements.Add(NewEdit);
end;

procedure TJQForm.AddEdit(ACaption: string; AName: string; AClass: string; AId: string; ARequired: boolean; AErrorMsg: string);
Var
  NewEdit: TInputText;
begin
  NewEdit:= TInputText.Create;
  NewEdit.Caption:= ACaption;
  NewEdit.Classe:= AClass;
  NewEdit.Name:= AName;
  NewEdit.Id:=AId;
  NewEdit.Required:=ARequired;
  NewEdit.ErrorMessage:=AErrorMsg;
  FElements.Add(NewEdit);
end;

procedure TJQForm.AddSelect(ACaption: string; AName: string; AClass: string; AId: string; ARequired: boolean; AErrorMsg: string; Const AValueList: TStringList);
Var
  NewEdit: TSelect;
begin
  NewEdit:= TSelect.Create(AValueList);
  NewEdit.Caption:= ACaption;
  NewEdit.Classe:= AClass;
  NewEdit.Name:= AName;
  NewEdit.Id:=AId;
  NewEdit.Required:=ARequired;
  NewEdit.ErrorMessage:=AErrorMsg;
  FElements.Add(NewEdit);
end;

procedure TJQForm.AddTextArea(ACaption: string; AName: string; AClass: string; AId: string; ARequired: boolean; AErrorMsg: string);
Var
  NewEdit: TTextArea;
begin
  NewEdit:= TTextArea.Create;
  NewEdit.Caption:= ACaption;
  NewEdit.Classe:= AClass;
  NewEdit.Name:= AName;
  NewEdit.Id:=AId;
  NewEdit.Required:=ARequired;
  NewEdit.ErrorMessage:=AErrorMsg;
  FElements.Add(NewEdit);
end;

procedure TJQForm.AddPassword(ACaption: string; AName: string; AClass: string; AId: string; ARequired: boolean; AErrorMsg: string);
Var
  NewEdit: TInputPassword;
begin
  NewEdit:= TInputPassword.Create;
  NewEdit.Caption:= ACaption;
  NewEdit.Classe:= AClass;
  NewEdit.Name:= AName;
  NewEdit.Id:=AId;
  NewEdit.ErrorMessage:=AErrorMsg;
  NewEdit.Required:=ARequired;
  FElements.Add(NewEdit);
end;

procedure TJQForm.AddCheckBox(ACaption: string; AName: string; AClass: string; AId: string; AValue: string; ARequired: boolean; AErrorMsg: string);
Var
  NewEdit: TInputCheckBox;
begin
  NewEdit:= TInputCheckBox.Create;
  NewEdit.Caption:= ACaption;
  NewEdit.Classe:= AClass;
  NewEdit.Name:= AName;
  NewEdit.Value:=AValue;
  NewEdit.Id:=AId;
  NewEdit.ErrorMessage:=AErrorMsg;
  NewEdit.Required:=ARequired;
  FElements.Add(NewEdit);
end;

procedure TJQForm.AddRadio(ACaption: string; AName: string; AClass: string; AId: string; AValue: string; ARequired: boolean; AErrorMsg: string);
Var
  NewEdit: TInputRadio;
begin
  NewEdit:= TInputRadio.Create;
  NewEdit.Caption:= ACaption;
  NewEdit.Classe:= AClass;
  NewEdit.Name:=AName;
  NewEdit.Value:=AValue;
  NewEdit.Id:=AId;
  NewEdit.ErrorMessage:=AErrorMsg;
  NewEdit.Required:=ARequired;
  FElements.Add(NewEdit);
end;

procedure TJQForm.AddSubmitButton(AClass: string; AName: string; AValue: string);
Var
  NewEdit: TInputSubmit;
begin
  NewEdit:= TInputSubmit.Create;
  NewEdit.Classe:= AClass;
  NewEdit.Value:= AValue;
  NewEdit.Name:= AName;;
  FElements.Add(NewEdit);
end;

procedure TJQForm.AddResetButton(AClass: string; AValue: string);
Var
  NewEdit: TInputReset;
begin
  NewEdit:= TInputReset.Create;
  NewEdit.Classe:= AClass;
  NewEdit.Value:= AValue;
  FElements.Add(NewEdit);
end;

function TJQForm.GetJs: string;
begin
  FJs.Clear;
  FJs.Add('<script type="text/javascript">');
  FJs.Add('	$(document).ready(function() {');
// using ajax validation
  FJs.Add('		$("#' + FId + '").validate({');
  FJs.Add('			success: function(label) {');
  FJs.Add('				label.html("&nbsp;").addClass("checked");');
  FJs.Add('			},');

  FJs.Add('	submitHandler: function(form) {');
  FJs.Add('	  jQuery(form).ajaxSubmit({');
  FJs.Add('	    target: "#result"');
  FJs.Add('	  })');
  FJs.Add('	}');
  FJs.Add('		})');
  FJs.Add('	});');
  FJs.Add('</script>');
  result:= FJs.Text;
end;

function TJQForm.GetCss: string;
begin

  inherited;
  FCss.Add('<style type="text/css">');
  FCss.Add('form.' + FClasse + ' .status {');
  FCss.Add('padding-top: 2px;');
  FCss.Add('padding-left: 8px;');
  FCss.Add('vertical-align: top;');
  FCss.Add('width: 246px;');
  FCss.Add('white-space: nowrap;');
  FCss.Add('}');

  FCss.Add('form.' + FClasse + ' label.error {');
  FCss.Add('background:url("<!--csspath-->images/unchecked.png") no-repeat 0px 0px;');
//  FCss.Add('margin-left: 5px;');
  FCss.Add('padding-left: 25px;');
  FCss.Add('font-weight:bold;');
  FCss.Add('color:#5f83b9;');
  FCss.Add('width: 250px;');
  FCss.Add('}');
  FCss.Add('form.' + FClasse + ' label.checked {');
  FCss.Add('background:url("<!--csspath-->images/checked.png") no-repeat 0px 0px;');
  FCss.Add('}');
  FCss.Add('#warning { display: none; }');
  FCss.Add('</style>');

  result:= FCss.Text;
end;

function TJQForm.GetContent: string;
Var
  i: integer;
  Method: string;
//  Required: string;
begin
  FContent.Clear;

  Case FFormMethod of
    fmPost: Method:= 'post';
    fmGet: Method:= 'get';
  end;

  FContent.Add('<form method="' + Method + '" class="' + FClasse + '" id="' + FId + '" action="' + FAction +'" ' + FExtraParam + '>');
//  FContent.Add('<fieldset>');
  if FFormInTable then
    FContent.Add('<table>');

  for i:= 0 to FElements.Count -1 do
  begin
    if FFormInTable then
      FContent.Add('<tr>');

  FContent.Add(TBaseElement(FElements.Items[i]).GeneratedHtml[FFormInTable]);

{
    if FElements.Items[i].ClassType = TInputText then
      FContent.Add(TBaseElement(FElements.Items[i]).GeneratedHtml[FFormInTable]);
    if FElements.Items[i].ClassType = TInputPassword then
      FContent.Add(TInputPassword(FElements.Items[i]).GeneratedHtml[FFormInTable]);
    if FElements.Items[i].ClassType = TInputSubmit then
      FContent.Add(TBaseElement(FElements.Items[i]).GeneratedHtml[FFormInTable]);
    if FElements.Items[i].ClassType = TInputReset then
      FContent.Add(TBaseElement(FElements.Items[i]).GeneratedHtml[FFormInTable]);
}
    if FFormInTable then
    begin
//      FContent.Add('<td></td>');
      FContent.Add('</tr>');
    end;
  end;

  if FFormInTable then
    FContent.Add('</table>');
//  FContent.Add('</fieldset>');
  FContent.Add('</form>');

  FContent.Add('<br /><br /><div id="result" ></div><br /><br />');

  if FBeginContent.Text <> '' then
    FContent.Insert(1, FBeginContent.Text);
  if FEndContent.Text <> '' then
    FContent.Insert(FContent.Count -2, FEndContent.Text);
  result:= FContent.Text;
end;

constructor TJQForm.Create;
begin
  inherited Create;
  FBeginContent:= TStringList.Create;
  FEndContent:= TStringList.Create;
  FElements:= TObjectList.create(true);
  FFormMethod:= fmPost;
  FFormInTable:= True;
end;

destructor TJQForm.Destroy;
begin
  FBeginContent.Free;
  FEndContent.Free;
  FElements.Free;
  inherited destroy;
end;

end.

