unit HtmlFormElements;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type
    TElementType = (etText, etNumber, etDate, etPassword, etButton,
        etSubmit, etTextArea, etFile, etHidden, etCheckbox, etRadio, etSelect);

    TBaseElement = class(TObject)
        STR, ETR: string[5];
        STD, ETD: string[5];
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

    TFile = class(TBaseInput)
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

    TInput = class(TBaseInput)
    protected
        FSize: integer;
    public
        property Size: integer read FSize write FSize;
    end;

    // TODO Add validator for input as number, date
    TInputText = class(TInput)
    protected
        function GetHtml(AInTable: boolean): string; override;
    public
        constructor Create;
        constructor CreateAsNumber; overload;
        constructor CreateAsDate; overload;
    end;

    TInputPassword = class(TInput)
    protected
        function GetHtml(AInTable: boolean): string; override;
    public
        constructor Create;
    end;

    TSelectItem = record
        Value: string;
        Caption: string;
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
        function AddItem(AValue: string; ALabel: string): integer;
        function AddItem(AValue: string; ALabel: string;
            ASelected, ADisabled: boolean): integer;
    end;

implementation

{ TBaseElement --------------------------------------------------------------- }

procedure TBaseElement.tableMarks(AInTable: boolean);
begin
    if AInTable then begin
        STR := '<tr>';
        ETR := '</tr>';
        STD := '<td>';
        ETD := '</td>';
    end else begin
        STR := '';
        ETR := '';
        STD := '';
        ETD := '';
    end;
end;

{ TBaseInput ----------------------------------------------------------------- }

{ TTextArea ------------------------------------------------------------------ }

constructor TTextArea.Create;
begin
    FElementType := etTextArea;
end;

function TTextArea.GetHtml(AInTable: boolean): string;
var Html: TStrings;
    Data, DataRequired, DataSize: string;
begin
    Html:=TStringList.Create;
    DataSize:='';
    Data:='';
    DataRequired := '';
    if FRows>0 then DataSize:='rows="' + IntToStr(FRows) + '"';
    if FCols>0 then DataSize:='cols="' + IntToStr(FCols) + '"';
    if FValue<>'' then Data:='value="' + FValue + '" ';
    if FRequired then DataRequired:='required'; // messages:{required:''' + FErrorMessage + '''}}';
    tableMarks(AInTable);
    Html.Add(STR);
    if FLabel<>'' then
        Html.Add(STD + '<label for="' + FId + '">' + FLabel + '</label>' + ETD);

    Html.Add(STD + '<textarea id="' + FId + '" name="' + FName +
        '" class="' + FClass + '" ' + DataRequired + ' ' +
        Data + DataSize + ' ' + FExtraParam + ' ></textarea>' + ETD);
    Html.Add(ETR);
    Result := Html.Text;
    Html.Free;
end;

{ TInputRadio ---------------------------------------------------------------- }

constructor TInputRadio.Create;
begin
    FElementType := etRadio;
end;

function TInputRadio.GetHtml(AInTable: boolean): string;
var Html: TStrings;
    Data, DataRequired: string;
begin
    Html:=TStringList.Create;
    Data:='';
    DataRequired:='';
    if FValue<>'' then Data:='value="' + FValue + '" ';
    if FRequired then DataRequired:='required'; // messages:{required:''' + FErrorMessage + '''}}';
    tableMarks(AInTable);
    Html.Add(STR);
    if FLabel <> '' then
        Html.Add(STD + '<label for="' + FId + '">' + FLabel + '</label>' + ETD);

    Html.Add(STD + '<input id="' + FId + '" type="radio" name="' +
        FName + '" class="' + FClass + '" ' + DataRequired + ' ' +
        Data + ' ' + FExtraParam + ' />' + ETD);
    Html.Add(ETR);
    Result := Html.Text;
    Html.Free;
end;

{ TInputCheckBox ------------------------------------------------------------- }

constructor TInputCheckBox.Create;
begin
    FElementType := etCheckBox;
end;

function TInputCheckBox.GetHtml(AInTable: boolean): string;
var Html: TStrings;
    Data, DataRequired: string;
begin
    Html:=TStringList.Create;
    Data:='';
    DataRequired:='';
    if FValue<>'' then Data:='value="' + FValue + '" ';
    if FRequired then DataRequired:='required'; // messages:{required:''' + FErrorMessage + '''}}';
    tableMarks(AInTable);
    Html.Add(STR);
    if FLabel<>'' then
        Html.Add(STD + '<label for="' + FId + '">' + FLabel + '</label>' + ETD);

    Html.Add(STD + '<input id="' + FId + '" type="checkbox" name="' +
        FName + '" class="' + FClass + '" ' + DataRequired + ' ' +
        Data + ' ' + FExtraParam + ' />' + ETD);
    Html.Add(ETR);
    Result := Html.Text;
    Html.Free;
end;

{ TFile ---------------------------------------------------------------------- }

constructor TFile.Create;
begin
    FElementType := etFile;
end;

function TFile.GetHtml(AInTable: boolean): string;
var Html: TStrings;
    DataRequired: string;
begin
    Html:=TStringList.Create;
    DataRequired:='';
    if FRequired then DataRequired:='required'; // messages:{required:''' + FErrorMessage + '''}}';
    tableMarks(AInTable);
    Html.Add(STR);
    if FLabel<>'' then
        Html.Add(STD + '<label for="' + FId + '">' + FLabel + '</label>' + ETD);

    Html.Add(STD + '<input id="' + FId + '" type="file" name="' + FName +
        '" class="' + FClass + '" ' + DataRequired + ' ' +
        FExtraParam + ' />' + ETD);
    Html.Add('<td class="status"></td>');
    Html.Add(ETR);
    Result := Html.Text;
    Html.Free;
end;

{ TInputSubmit --------------------------------------------------------------- }

constructor TInputSubmit.Create;
begin
    FElementType := etSubmit;
end;

function TInputSubmit.GetHtml(AInTable: boolean): string;
var Html: TStrings;
begin
    Html := TStringList.Create;
    tableMarks(AInTable);
    Html.Add(STR);
    Html.Add(STD + FLabel + ETD);
    Html.Add(STD + '<input class="' + FClass + '" type="submit" value="' +
        FValue + '" Name="' + FName + '" ' + FExtraParam + ' />' + ETD);
    Html.Add(ETR);
    Result := Html.Text;
    Html.Free;
end;

{ TInputReset ---------------------------------------------------------------- }

constructor TInputReset.Create;
begin
    FElementType := etSubmit;
end;

function TInputReset.GetHtml(AInTable: boolean): string;
var Html: TStrings;
begin
    Html := TStringList.Create;
    tableMarks(AInTable);
    Html.Add(STR);
    Html.Add(STD + FLabel + ETD);
    Html.Add(STD + '<input class="' + FClass + '" type="reset" value="' +
        FValue + '" ' + FExtraParam + ' />' + ETD);
    Html.Add(ETR);
    Result := Html.Text;
    Html.Free;
end;

{ TInputText ----------------------------------------------------------------- }

constructor TInputText.Create;
begin
    FElementType := etText;
end;

constructor TInputText.CreateAsNumber;
begin
    FElementType := etNumber;
end;

constructor TInputText.CreateAsDate;
begin
    FElementType := etDate;
end;

function TInputText.GetHtml(AInTable: boolean): string;
var Html: TStrings;
    Data, DataRequired, DataSize: string;
    NumberValidation, NumberValidationError: string;
    DateValidation, DateValidationError: string;
begin
    Html:=TStringList.Create;
    Data:='';
    DataSize := '';
    if FValue<>'' then Data:='value="' + FValue + '" ';
    if FSize>0 then DataSize:='size="' + IntToStr(FSize) + '" ';
    case FElementType of
        etNumber: begin
            NumberValidation := 'number:true';
            NumberValidationError := 'number:'' You must enter only numbers''';
        end;
        etDate: begin
            DateValidation := 'date:true';
            DateValidationError := 'date:'' You must enter a valid date (DD/MM/YYYY)''';
        end;
    end;
    case FRequired of
        True: begin
            if NumberValidation <> '' then
                DataRequired := '{required:true,' + NumberValidation +
                    ', messages:{required:''' + FErrorMessage + ''',' + NumberValidationError + '}}'
            else
                DataRequired := '{required:true, messages:{required:''' + FErrorMessage + '''}}';
            if DateValidation <> '' then
                DataRequired := '{required:true,' + DateValidation +
                    ', messages:{required:''' + FErrorMessage + ''',' + DateValidationError + '}}'
            else
                DataRequired := '{required:true, messages:{required:''' + FErrorMessage + '''}}';
        end;
        False: begin
            DataRequired := '';
            if NumberValidation <> '' then
                DataRequired := '{' + NumberValidation + ', messages:{' +
                    NumberValidationError + '}}';
            if DateValidation <> '' then
                DataRequired := '{' + DateValidation + ', messages:{' +
                    DateValidationError + '}}';
        end;
    end;
    tableMarks(AInTable);
    Html.Add(STR);
    if FLabel <> '' then
        Html.Add(STD + '<label for="' + FId + '">' + FLabel + '</label>' + ETD);

    Html.Add(STD + '<input id="' + FId + '" type="text" name="' + FName +
        '" class="' + FClass + '" ' + DataRequired + ' ' +
        Data + DataSize + ' ' + FExtraParam + ' />' + ETD);
    Html.Add('<td class="status"></td>');
    Html.Insert(0, '<div class="ui-widget">');
    Html.Add('</div>');
    Html.Add(ETR);
    Result := Html.Text;
    Html.Free;
end;

{ TInputPassword ------------------------------------------------------------- }

constructor TInputPassword.Create;
begin
    FElementType := etPassword;
end;

function TInputPassword.GetHtml(AInTable: boolean): string;
var Html: TStrings;
    Data, DataRequired, DataSize: string;
begin
    Html:=TStringList.Create;
    Data:='';
    DataSize:='';
    DataRequired:='';
    if FValue<>'' then Data:='value="' + FValue + '" ';
    if FSize>0 then DataSize:='size="' + IntToStr(FSize) + '" ';
    if FRequired then DataRequired:='required'; // messages:{required:''' + FErrorMessage + '''}}';
    tableMarks(AInTable);
    Html.Add(STR);
    if FLabel <> '' then
        Html.Add(STD + '<label for="' + FId + '">' + FLabel + '</label>' + ETD);

    Html.Add(STD + '<input id="' + FId + '" type="password" name="' +
        FName + '" class="' + FClass + '" ' + DataRequired + ' ' +
        Data + DataSize + ' ' +
        FExtraParam + ' />' + ETD);
    Html.Add(ETR);
    Result := Html.Text;
    Html.Free;
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
        for i := 0 to AItemList.Count - 1 do
            AddItem(AItemList.Names[i], AItemList.Values[AItemList.Names[i]]);
    except
        on e: Exception do
            raise Exception.Create('TSelect.Create(AItemList: TStrings): ' + e.Message);
    end;
end;

destructor TSelect.Destroy;
begin
    SetLength(FItemsList, 0);
    inherited Destroy;
end;

function TSelect.GetHtml(AInTable: boolean): string;
var Html: TStrings;
    OneItem, DataRequired: string;
    i: integer;
begin
    Html:=TStringList.Create;
    DataRequired:='';
    if FRequired then DataRequired:='required'; // messages:{required:''' + FErrorMessage + '''}}';
    tableMarks(AInTable);
    Html.Add(STR);
    if FLabel <> '' then begin
        Html.Add(STD + '<label for="' + FId + '">' + FLabel + '</label>' + ETD);
    end;
    Html.Add(STD + '<select id="' + FId + '" name="' + FName +
        '" class="' + FClass + '" ' + DataRequired + ' ' + FExtraParam + '>');

    for i := 0 to Length(FItemsList) - 1 do begin
        OneItem := '<option value="' + FItemsList[i].Value + '" ';
        if FItemsList[i].selected then
            OneItem := OneItem + 'selected ';
        if FItemsList[i].disabled then
            OneItem := OneItem + 'disabled ';
        OneItem := OneItem + '>' + FItemsList[i].Caption + '</option>';
        Html.Add(OneItem);
    end;
    Html.Add('</select>' + ETD);
    Html.Add(ETR);
    Result := Html.Text;
    Html.Free;
end;

function TSelect.AddItem(AValue: string; ALabel: string): integer;
begin
    Result:=AddItem(AValue, ALabel, False, False);
end;

function TSelect.AddItem(AValue: string; ALabel: string;
    ASelected, ADisabled: boolean): integer;
var Count: integer;
begin
    Count:=Length(FItemsList);
    Inc(Count);
    SetLength(FItemsList, Count);
    FItemsList[Count - 1].Value:=AValue;
    FItemsList[Count - 1].Caption:=ALabel;
    FItemsList[Count - 1].selected:=ASelected;
    FItemsList[Count - 1].disabled:=ADisabled;
    Result := Count - 1;
end;

end.
