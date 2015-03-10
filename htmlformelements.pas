// TODO Add validator for input as number, date

unit HtmlFormElements;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type

  TElementType = (etText, etNumber, etDate, etPassword, etButton, etSubmit, etTextArea, etFile, etHidden, etCheckbox, etRadio, etSelect);

  TBaseElement = class(TObject)
    STR,ETR : string[5];
    STD,ETD : string[5];
    procedure tableMarks(AInTable: boolean);
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

  TSelectItem = record
       value : string;
       caption : string;
       selected : boolean;
       disabled : boolean;
  end;

  TSelect = class(TBaseInput)
  protected
    FItemsList: Array of TSelectItem;  // zero based
    function GetHtml(AInTable: boolean): string; override;
  public
    constructor Create;
    constructor Create(AItemList: TStringList); overload;
    destructor Destroy; override;
    function AddItem(AValue: string; ALabel: string): integer;
    function AddItem(AValue: string; ALabel: string; ASelected, ADisabled: boolean): integer;
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


procedure TBaseElement.tableMarks(AInTable: boolean);
begin
   if AInTable then begin
      STR:='<tr>';
      ETR:='</tr>';
      STD:='<td>';
      ETD:='</td>';
   end else begin
      STR:='';
      ETR:='';
      STD:='';
      ETD:='';
   end;
end;

{ TSelect }
// validate is a jQuery tag
//  <select id="jungle" name="jungle" title="Please select something!" validate="required:true">
//	<option value=""></option>
//	<option value="1">Buga</option>
//	<option value="2">Baga</option>
//	<option value="3">Oi</option>
// </select>
function TSelect.GetHtml(AInTable: boolean): string;
Var
  Html: TStrings;
  OneItem, DataRequired: string;
  i: integer;
begin
  Html:= TStringList.Create;

  // is it required ?
  Case FRequired of
    true: DataRequired:= '{required:true, messages:{required:''' + FErrorMessage + '''}}';
    false: DataRequired:= 'notrequired';
  end;
  tableMarks(AInTable);
  Html.Add(STR);
  if FLabel <> '' then begin
    Html.Add(STD + '<label for="' + FId + '">' + FLabel + '</label>' + ETD);
  end;
  Html.Add(STD + '<select id="' + FId + '" name="' + FName +
                 '" class="' + FClass + '" validate="' + DataRequired + '" ' +
                 FExtraParam + '>');

  for i:= 0 to Length(FItemsList) -1 do begin
      OneItem:= '<option value="' + FItemsList[i].value + '" ';
      if FItemsList[i].selected then OneItem:= OneItem + 'selected ';
      if FItemsList[i].disabled then OneItem:= OneItem + 'disabled ';
      OneItem:= OneItem + '>' + FItemsList[i].caption + '</option>';
      Html.Add(OneItem);
  end;
  Html.Add('</select>' + ETD);
  Html.Add(ETR);
  result:= Html.Text;
  Html.Free;
end;

function TSelect.AddItem(AValue: string; ALabel: string): integer;
begin
  result:= AddItem(AValue, ALabel, false, false);
end;

function TSelect.AddItem(AValue: string; ALabel: string; ASelected, ADisabled: boolean): integer;
var count : integer;
begin
  count:=Length(FItemsList);
  inc(count);
  SetLength(FItemsList, count);
  FItemsList[count-1].value:= AValue;
  FItemsList[count-1].caption:= ALabel;
  FItemsList[count-1].selected:= ASelected;
  FItemsList[count-1].disabled:= ADisabled;
  result:= count-1;
end;

destructor TSelect.Destroy;
begin
  SetLength(FItemsList, 0);
  inherited destroy;
end;

constructor TSelect.Create(AItemList: TStringList);
Var
  i: integer;
begin
  inherited Create;
  try
    SetLength(FItemsList, 0);
    for i:= 0 to AItemList.Count -1 do
      AddItem(AItemList.Names[i], AItemList.Values[AItemList.Names[i]]);
  except on e:exception do
    Raise Exception.Create('TSelect.Create(AItemList: TStrings): ' + e.Message);
  end;
end;

constructor TSelect.Create;
begin
  FElementType:= etSelect;
  SetLength(FItemsList, 0);
end;

{ TTextArea }

function TTextArea.GetHtml(AInTable: boolean): string;
Var
  Html: TStrings;
  Data, DataRequired, DataSize: string;
begin
  Html:= TStringList.Create;

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

  tableMarks(AInTable);
  Html.Add(STR);
  if FLabel <> '' then
    Html.Add(STD + '<label for="' + FId + '">' + FLabel + '</label>' + ETD);

  Html.Add(STD + '<textarea id="' + FId + '" name="' + FName +
                 '" class="' + FClass + '" validate="' +DataRequired + '" ' +
                 Data + DataSize + ' '+ FExtraParam + ' ></textarea>' + ETD);
  Html.Add(ETR);
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
begin
  Html:= TStringList.Create;

  // Having some value ?
  Data:= '';
  if FValue <> '' then
    Data:= 'value="' + FValue + '" ';

  // is it required ?
  Case FRequired of
    true: DataRequired:= '{required:true, messages:{required:''' + FErrorMessage + '''}}';
    false: DataRequired:= 'notrequired';
  end;

  tableMarks(AInTable);
  Html.Add(STR);
  if FLabel <> '' then
    Html.Add(STD + '<label for="' + FId + '">' + FLabel + '</label>' + ETD);

  Html.Add(STD + '<input id="' + FId + '" type="checkbox" name="' + FName +
                 '" class="' + FClass + '" validate="' + DataRequired + '" ' +
                 Data + ' ' + FExtraParam + ' />' + ETD);
  Html.Add(ETR);
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
begin
  Html:= TStringList.Create;

  // Having some value ?
  Data:= '';
  if FValue <> '' then
    Data:= 'value="' + FValue + '" ';

  // is it required ?
  Case FRequired of
    true: DataRequired:= '{required:true, messages:{required:''' + FErrorMessage + '''}}';
    false: DataRequired:= 'notrequired';
  end;

  tableMarks(AInTable);
  Html.Add(STR);
  if FLabel <> '' then
    Html.Add(STD + '<label for="' + FId + '">' + FLabel + '</label>' + ETD);

  Html.Add(STD + '<input id="' + FId + '" type="radio" name="' + FName +
                 '" class="' + FClass + '" validate="' + DataRequired + '" ' +
                 Data + ' ' + FExtraParam + ' />' + ETD);
  Html.Add(ETR);
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
begin
  Html:= TStringList.Create;
  tableMarks(AInTable);
  Html.Add(STR);
  Html.Add(STD + FLabel + ETD);
  Html.Add(STD + '<input class="' + FClass + '" type="reset" value="' + FValue + '" ' +
                 FExtraParam + ' />' + ETD);
  Html.Add(ETR);
  result:= Html.Text;
  Html.Free;
end;

constructor TInputReset.Create;
begin
  FElementType:= etSubmit;
end;

{ TInputSubmit }

function TInputSubmit.GetHtml(AInTable: boolean): string;
Var Html: TStrings;
begin
  Html:= TStringList.Create;
  tableMarks(AInTable);
  Html.Add(STR);
  Html.Add(STD + FLabel + ETD);
  Html.Add(STD + '<input class="' + FClass + '" type="submit" value="' + FValue +
               '" Name="' + FName + '" ' + FExtraParam + ' />' + ETD);
  Html.Add(ETR);
  result:= Html.Text;
  Html.Free;
end;

constructor TInputSubmit.Create;
begin
  FElementType:= etSubmit;
end;


{ TInputPassword }

function TInputPassword.GetHtml(AInTable: boolean): string;
Var Html: TStrings;
    Data, DataRequired, DataSize: string;
begin
  Html:= TStringList.Create;

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

  tableMarks(AInTable);
  Html.Add(STR);
  if FLabel <> '' then
    Html.Add(STD + '<label for="' + FId + '">' + FLabel + '</label>' + ETD);

  Html.Add(STD + '<input id="' + FId + '" type="password" name="' + FName +
                 '" class="' + FClass + '" validate="' + DataRequired + '" ' +
                 Data + DataSize + ' ' + FExtraParam + ' />' + ETD);
  Html.Add(ETR);
  result:= Html.Text;
  Html.Free;
end;

constructor TInputPassword.Create;
begin
  FElementType:= etPassword;
end;

{ TFile }

function TFile.GetHtml(AInTable: boolean): string;
Var Html: TStrings;
    DataRequired: string;
begin
  Html:= TStringList.Create;

  // is it required ?
  Case FRequired of
    true: DataRequired:= '{required:true, messages:{required:''' + FErrorMessage + '''}}';
    false: DataRequired:= 'notrequired';
  end;

  tableMarks(AInTable);
  Html.Add(STR);
  if FLabel <> '' then
    Html.Add(STD + '<label for="' + FId + '">' + FLabel + '</label>' + ETD);

  Html.Add(STD + '<input id="' + FId + '" type="file" name="' + FName +
                 '" class="' + FClass + '" validate="' + DataRequired + '" ' +
                 FExtraParam + ' />' + ETD);
  Html.Add('<td class="status"></td>');
  Html.Add(ETR);
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
begin
  Html:= TStringList.Create;

  // Having some value ?
  Data:= '';
  if FValue <> '' then
    Data:= 'value="' + FValue + '" ';

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

  tableMarks(AInTable);
  Html.Add(STR);
  if FLabel <> '' then
    Html.Add(STD + '<label for="' + FId + '">' + FLabel + '</label>' + ETD);

  Html.Add(STD + '<input id="' + FId + '" type="text" name="' + FName +
                 '" class="' + FClass + '" validate="' + DataRequired + '" ' +
                 Data + DataSize + ' ' + FExtraParam + ' />' + ETD);
  Html.Add('<td class="status"></td>');
  Html.Insert(0, '<div class="ui-widget">');
  Html.Add('</div>');
  Html.Add(ETR);
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

