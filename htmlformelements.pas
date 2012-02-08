// TODO Add validator for input as number, date

unit HtmlFormElements;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type

  TElementType = (etText, etNumber, etDate, etPassword, etButton, etSubmit, etTextArea, etFile, etHidden, etCheckbox, etRadio, etSelect);

  TBaseElement = class(TObject)
  protected
    FId: string;
    FName: string;
    FClass: string;
    FElementType: TElementType;
    FExtraParam: string;
    function GetHtml(AInTable: boolean): string; virtual; abstract;
  public
    property Id: string read FId write FId;
    property Name: string read FName write FName;
    property Classe: string read FClass write FClass;
    property ElementType: TElementType read FElementType;
    property ExtraParam: string read FExtraParam write FExtraParam;
    property GeneratedHtml[AInTable: boolean]: string read GetHtml;
  end;

  TBaseInput = class(TBaseElement)
  protected
    FLabel: string;
    FValue: string;
    FRequired: boolean;
    FErrorMessage: string;
  public
    property Caption: string read FLabel write FLabel;
    property Value: string read FValue write FValue;
    property Required: boolean read FRequired write FRequired;
    property ErrorMessage: string read FErrorMessage write FErrorMessage;
  end;

  TInput = class(TBaseInput)
  Protected
    FSize: integer;
  public
    property Size: integer read FSize write FSize;
  end;

  TSelect = class(TBaseInput)
  protected
    FItemsList: TStringList;
    function GetHtml(AInTable: boolean): string; override;
  public
    constructor Create;
    constructor Create(AItemList: TStringList); overload;
    destructor Destroy; override;
    function AddItem(AValue: string; ALabel: string): integer;
  end;

  TFile = class(TBaseInput)
  protected
    function GetHtml(AInTable: boolean): string; override;
  public
    constructor Create;
  end;

  TInputText = class(TInput)
  protected
    function GetHtml(AInTable: boolean): string; override;
  public
    constructor Create;
    constructor CreateAsNumber; overload;
    constructor CreateAsDate; overload;
  end;

  TInputNumber = class(TBaseInput)
  Protected
    FSize: integer;
  public
    property Size: integer read FSize write FSize;
  end;

  TTextArea = class(TBaseInput)
  protected
    FRows: integer;
    FCols: integer;
    function GetHtml(AInTable: boolean): string; override;
  public
    constructor Create;
    property Rows: integer read FRows write FRows;
    property Cols: integer read FCols write FCols;
  end;

  TInputRadio = class(TBaseInput)
  protected
    function GetHtml(AInTable: boolean): string; override;
  public
    constructor Create;
  end;

  TInputCheckBox = class(TBaseInput)
  protected
    function GetHtml(AInTable: boolean): string; override;
  public
    constructor Create;
  end;

  TInputPassword = class(TInput)
  protected
    function GetHtml(AInTable: boolean): string; override;
  public
    constructor Create;
  end;

  TInputSubmit = class(TBaseInput)
  protected
    function GetHtml(AInTable: boolean): string; override;
  public
    constructor Create;
  end;

  TInputReset = class(TBaseInput)
  protected
    function GetHtml(AInTable: boolean): string; override;
  public
    constructor Create;
  end;


implementation


{ TSelect }
//  <select id="jungle" name="jungle" title="Please select something!" validate="required:true">



function TSelect.GetHtml(AInTable: boolean): string;
Var
  Html: TStrings;
  Data, DataRequired: string;
  STD, ETD: string;
  i: integer;
begin
  Html:= TStringList.Create;

  if AInTable then
  begin
    STD:='<td>';
    ETD:='</td>';
  end;

  // Having some value ?
  Data:= '';
  if FValue <> '' then
    Data:= 'value="' + FValue + '"';

  // is it required ?
  Case FRequired of
    true: DataRequired:= '{required:true, messages:{required:''' + FErrorMessage + '''}}';
    false: DataRequired:= 'notrequired';
  end;
{
		<select id="jungle" name="jungle" title="Please select something!" validate="required:true">
			<option value=""></option>
			<option value="1">Buga</option>
			<option value="2">Baga</option>
			<option value="3">Oi</option>
		</select>
}

  if FLabel <> '' then
    Html.Add(STD + '<label for="' + FId + '">' + FLabel + '</label>' + ETD);

  Html.Add(STD + '<select id="' + FId + '" name="' + FName + '" class="' + DataRequired + '"' + FExtraParam + '>');
  for i:= 0 to FItemsList.Count -1 do
    Html.Add('<option value="' + FItemsList.Names[i] + '">' + FItemsList.Values[FItemsList.Names[i]] + '</option>');

  Html.Add('</select>' + ETD);

  result:= Html.Text;

  Html.Free;
end;

function TSelect.AddItem(AValue: string; ALabel: string): integer;
begin
  result:= FItemsList.Add(AValue + '=' + ALabel);
end;

destructor TSelect.Destroy;
begin
  FItemsList.Free;
  inherited destroy;
end;

constructor TSelect.Create(AItemList: TStringList);
Var
  i: integer;
begin
  inherited Create;
  try
    if not Assigned(FItemsList) then FItemsList:= TStringList.Create;
    for i:= 0 to AItemList.Count -1 do
      AddItem(AItemList.Names[i], AItemList.Values[AItemList.Names[i]]);
  except on e:exception do
    Raise Exception.Create('TSelect.Create(AItemList: TStrings): ' + e.Message);
  end;
end;

constructor TSelect.Create;
begin
  FElementType:= etSelect;
  FItemsList:= TStringList.Create;
end;

{ TTextArea }

function TTextArea.GetHtml(AInTable: boolean): string;
Var
  Html: TStrings;
  Data, DataRequired, DataSize: string;
  STD, ETD: string;
begin
  Html:= TStringList.Create;
  if AInTable then
  begin
    STD:='<td>';
    ETD:='</td>';
  end;

  DataSize:= '';
  if FRows > 0 then
    DataSize:= 'rows="' + intToStr(FRows) + '"';
  if FCols > 0 then
    DataSize:= 'cols="' + intToStr(FCols) + '"';

  // Having some value ?
  Data:= '';
  if FValue <> '' then
    Data:= 'value="' + FValue + '" ';

  // is it required ?
  Case FRequired of
    true: DataRequired:= '{required:true, messages:{required:''' + FErrorMessage + '''}}';
    false: DataRequired:= 'notrequired';
  end;

  if FLabel <> '' then
    Html.Add(STD + '<label for="' + FId + '">' + FLabel + '</label>' + ETD);

  //<textarea id="ccomment" name="comment" class="required"></textarea>
  Html.Add(STD + '<textarea id="' + FId + '" name="' + FName + '" class="' + DataRequired + '"' + Data + DataSize + FExtraParam + ' ></textarea>' + ETD);
  result:= Html.Text;
  Html.Free;
end;

constructor TTextArea.Create;
begin
  FElementType:= etTextArea;
end;

{ TInputCheckBox }

function TInputCheckBox.GetHtml(AInTable: boolean): string;
Var
  Html: TStrings;
  Data, DataRequired: string;
  STD, ETD: string;
begin
  Html:= TStringList.Create;
  if AInTable then
  begin
    STD:='<td>';
    ETD:='</td>';
  end;

  // Having some value ?
  Data:= '';
  if FValue <> '' then
    Data:= 'value="' + FValue + '" ';

  // is it required ?
  Case FRequired of
    true: DataRequired:= '{required:true, messages:{required:''' + FErrorMessage + '''}}';
    false: DataRequired:= 'notrequired';
  end;

  if FLabel <> '' then
    Html.Add(STD + '<label for="' + FId + '">' + FLabel + '</label>' + ETD);

  Html.Add(STD + '<input id="' + FId + '" type="checkbox" name="' + FName + '" class="' + DataRequired + '"' + Data + FExtraParam + ' />' + ETD);

  result:= Html.Text;
  Html.Free;
end;

constructor TInputCheckBox.Create;
begin
  FElementType:= etCheckBox;
end;

{ TInputRadio }

function TInputRadio.GetHtml(AInTable: boolean): string;
Var
  Html: TStrings;
  Data, DataRequired: string;
  STD, ETD: string;
begin
  Html:= TStringList.Create;
  if AInTable then
  begin
    STD:='<td>';
    ETD:='</td>';
  end;

  // Having some value ?
  Data:= '';
  if FValue <> '' then
    Data:= 'value="' + FValue + '" ';

  // is it required ?
  Case FRequired of
    true: DataRequired:= '{required:true, messages:{required:''' + FErrorMessage + '''}}';
    false: DataRequired:= 'notrequired';
  end;

  if FLabel <> '' then
    Html.Add(STD + '<label for="' + FId + '">' + FLabel + '</label>' + ETD);

  Html.Add(STD + '<input id="' + FId + '" type="radio" name="' + FName + '" class="' + DataRequired + '"' + Data + FExtraParam + ' />' + ETD);
  result:= Html.Text;
  Html.Free;
end;

constructor TInputRadio.Create;
begin
  FElementType:= etRadio;
end;

{ TInputReset }

function TInputReset.GetHtml(AInTable: boolean): string;
Var
  Html: TStrings;
  STD, ETD: string;
begin
  Html:= TStringList.Create;
  if AInTable then
  begin
    STD:='<td>';
    ETD:='</td>';
  end;
  Html.Add(STD + '<input class="' + FClass + '" type="reset" value="' + FValue + '"' + FExtraParam + ' />' + ETD);
  result:= Html.Text;
  Html.Free;
end;

constructor TInputReset.Create;
begin
  FElementType:= etSubmit;
end;

{ TInputSubmit }

function TInputSubmit.GetHtml(AInTable: boolean): string;
Var
  Html: TStrings;
  STD, ETD: string;
begin
  Html:= TStringList.Create;
  if AInTable then
  begin
    STD:='<td>';
    ETD:='</td>';
  end;
  Html.Add(STD + '<input class="' + FClass + '" type="submit" value="' + FValue + '" Name="' + FName + '" ' + FExtraParam + ' />' + ETD);
  result:= Html.Text;
  Html.Free;
end;

constructor TInputSubmit.Create;
begin
  FElementType:= etSubmit;
end;


{ TInputPassword }

function TInputPassword.GetHtml(AInTable: boolean): string;
Var
  Html: TStrings;
  Data, DataRequired, DataSize: string;
  STD, ETD: string;
begin
  Html:= TStringList.Create;

  if AInTable then
  begin
    STD:='<td>';
    ETD:='</td>';
  end;

  // Having some value ?
  Data:= '';
  if FValue <> '' then
    Data:= 'value="' + FValue + '" ';

  DataSize:= '';
  if FSize > 0 then
    DataSize:= 'size="' + inttostr(FSize) + '" ';

  // is it required ?
  Case FRequired of
    true: DataRequired:= '{required:true, messages:{required:''' + FErrorMessage + '''}}';
    false: DataRequired:= 'notrequired';
  end;

  if FLabel <> '' then
    Html.Add(STD + '<label for="' + FId + '">' + FLabel + '</label>' + ETD);

  Html.Add(STD + '<input id="' + FId + '" type="password" name="' + FName + '" class="' + DataRequired + '"' + Data + DataSize + FExtraParam + ' />' + ETD);

  result:= Html.Text;

  Html.Free;
end;

constructor TInputPassword.Create;
begin
  FElementType:= etPassword;
end;

{ TFile }

function TFile.GetHtml(AInTable: boolean): string;
Var
  Html: TStrings;
  Data, DataRequired, DataSize: string;
  STD, ETD: string;
begin
  Html:= TStringList.Create;

  if AInTable then
  begin
    STD:='<td>';
    ETD:='</td>';
  end;

  // is it required ?
  Case FRequired of
    true: DataRequired:= '{required:true, messages:{required:''' + FErrorMessage + '''}}';
    false: DataRequired:= 'notrequired';
  end;

  if FLabel <> '' then
    Html.Add(STD + '<label for="' + FId + '">' + FLabel + '</label>' + ETD);

  Html.Add(STD + '<input id="' + FId + '" type="file" name="' + FName + '" class="' + DataRequired + '"' + FExtraParam + ' />' + ETD);
  Html.Add('<td class="status"></td>');
  result:= Html.Text;

  Html.Free;

end;

constructor TFile.Create;
begin
  FElementType:= etFile;
end;

{ TInputNumber }


{ TInputText }

function TInputText.GetHtml(AInTable: boolean): string;
Var
  Html: TStrings;
  Data, DataRequired, DataSize: string;
  NumberValidation, NumberValidationError: string;
  DateValidation, DateValidationError: string;
  STD, ETD: string;
begin
  Html:= TStringList.Create;

  if AInTable then
  begin
    STD:='<td>';
    ETD:='</td>';
  end;

  // Having some value ?
  Data:= '';
  if FValue <> '' then
    Data:= 'value="' + FValue + '"';

  DataSize:= '';
  if FSize > 0 then
    DataSize:= 'size="' + inttostr(FSize) + '" ';

  Case FElementType of
    etNumber:
    begin
      NumberValidation:= 'number:true';
      NumberValidationError:= 'number:'' Veuillez saisir des chiffres''';
    end;
    etDate:
    begin
      DateValidation:= 'date:true';
      DateValidationError:= 'date:'' Veuillez entrer une date valide (JJ/MM/AAAA)''';
    end;
  end;

  // is it required ?
  Case FRequired of
    true:
    begin
      if NumberValidation <> '' then
        DataRequired:= '{required:true,' + NumberValidation + ', messages:{required:''' + FErrorMessage + ''',' + NumberValidationError + '}}'
      else
        DataRequired:= '{required:true, messages:{required:''' + FErrorMessage + '''}}';
      if DateValidation <> '' then
        DataRequired:= '{required:true,' + DateValidation + ', messages:{required:''' + FErrorMessage + ''',' + DateValidationError + '}}'
      else
        DataRequired:= '{required:true, messages:{required:''' + FErrorMessage + '''}}';
    end;
    false:
    begin
      DataRequired:= 'notrequired';
      if NumberValidation <> '' then
        DataRequired:= '{' + NumberValidation + ', messages:{' + NumberValidationError + '}}';
      if DateValidation <> '' then
        DataRequired:= '{' + DateValidation + ', messages:{' + DateValidationError + '}}';
    end;
  end;

  if FLabel <> '' then
    Html.Add(STD + '<label for="' + FId + '">' + FLabel + '</label>' + ETD);

  Html.Add(STD + '<input id="' + FId + '" type="text" name="' + FName + '" class="' + DataRequired + '"' + Data + DataSize + FExtraParam + ' />' + ETD);
  Html.Add('<td class="status"></td>');
  Html.Insert(0, '<div class="ui-widget">');
  Html.Add('</div>');
  result:= Html.Text;

  Html.Free;
end;

constructor TInputText.Create;
begin
  FElementType:= etText;
end;

constructor TInputText.CreateAsNumber;
begin
  FElementType:= etNumber;
end;

constructor TInputText.CreateAsDate;
begin
  FElementType:= etDate;
end;

end.

