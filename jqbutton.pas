unit JqButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jqBase, Contnrs;

Type
  { Button type for a toggle button }
  TToggleButtonType = (
                    { Radio button }
                    tbRadio,
                    { CheckBox button }
                    tbCheckBox);

  { Simple button class }
  TJQButton = class(TJQBase)
  private
    FRole: string;
  protected
    // Enable or not the button
    FEnabled: boolean;
    // Use icon button or not
    FButtonTextOnly: boolean;
    // Caption of the button
    FCaption: string;
    function GetContent: string; override;
    function GetJs: string; override;
  public
    constructor Create(AButtonTextOnly: boolean); overload;
    // Caption of the button
    property Caption: string read FCaption write FCaption;
    // Enable or disable the button
    property Enabled: boolean read FEnabled write FEnabled;
  end;

  TJQToggleButtonItem = class
  private
    FCaption: string;
    FChecked: boolean;
  public
    property Caption: string read FCaption write FCaption;
    property Checked: boolean read FChecked write FChecked;
  end;

  TJQToggleButton = class(TJQButton)
  private
    FItems: TObjectList;
    FToggleButtonType: TToggleButtonType;
    function GetItem(AIndex: integer): TJQToggleButtonItem;
    function GetCount: integer;
  protected
    function GetContent: string; override;
    function GetJs: string; override;
    function GetCss: string; override;
  public
    constructor Create(AButtonType:TToggleButtonType);
    destructor destroy; override;
    procedure Clear;
    function AddItem(ACaption: string; AChecked: boolean = false): integer;
    property Item[AItemIndex: integer]: TJQToggleButtonItem read GetItem;
    property Count: integer read GetCount;
  end;

  TJQDivButton = class(TJQButton)
  protected
    function GetContent: string; override;
    function GetJs: string; override;
  public
    constructor Create;
  end;

  TJQHRefButton = class(TJQButton)
  private
    FHref: string;
  protected
    function GetContent: string; override;
    function GetJs: string; override;
  public
    constructor Create;
    property Href: string read FHref write FHref;
  end;

  TJQIconButton = class(TJQButton)
  private
    FLeftRightIcons: boolean;
  protected
    function GetContent: string; override;
    function GetJs: string; override;
  public
    constructor Create(ALeftRightIcons: boolean); overload;
  end;

implementation

{ TJQToggleButton }

function TJQToggleButton.GetContent: string;
Var
  i: integer;
  divid: string;
  ValChecked: string;
begin
  case FToggleButtonType of
    tbRadio: divid:= FId;
    tbCheckBox: divid:= FId + 'G';
  end;

  FContent.Add('<div id="' + divid + '">');
  for i:= 0 to FItems.Count -1 do
  begin

    if TJQToggleButtonItem(FItems.Items[i]).Checked then
      ValChecked:= 'checked'
    else
      ValChecked:= '';
    case FToggleButtonType of
      tbRadio:
        FContent.Add('     <input type="radio" id="radio' + intToStr(i+1) + '" ' + ValChecked +' name="radio" /><label for="radio' + intToStr(i+1) + '">' + TJQToggleButtonItem(FItems.Items[i]).Caption + '</label>');
      tbCheckBox:
        FContent.Add('     <input type="checkbox" id="cbx' + intToStr(i+1) + '" ' + ValChecked + ' /><label for="cbx' + intToStr(i+1) + '">' + TJQToggleButtonItem(FItems.Items[i]).Caption + '</label>');
    end;
  end;
  FContent.Add('     </div>');
  result:= FContent.Text;

end;

function TJQToggleButton.GetJs: string;
begin
  FJs.Clear;
  FJs.Add('<script  type="text/javascript">');
  FJs.Add('$(function() {');
  case FToggleButtonType of
    tbRadio:
    begin
      FJs.Add('$( "#' + FId + '" ).buttonset();');
    end;
    tbCheckBox:
    begin
      FJs.Add('$( "#' + FId + '" ).button(');
      if Not FEnabled then
        FJs.Add('{ disabled: true }');
      FJs.Add(');');
      FJs.Add('$( "#' + FId + 'G" ).buttonset();');
    end;
  end;
  FJs.Add('	});');
  FJs.Add('	</script>');
  Result:= FJs.Text
end;
function TJQToggleButton.GetCss: string;
begin
  inherited;
  if FToggleButtonType = tbCheckBox then
  begin
    FCss.Add('<style type="text/css">');
    FCss.Add('	#' + FId + 'G { margin-top: 2em; }');
    FCss.Add('</style>');
  end;
end;

function TJQToggleButton.AddItem(ACaption: string; AChecked: boolean = false): integer;
Var
  NewItem: TJQToggleButtonItem;
begin
  NewItem:= TJQToggleButtonItem.Create;
  NewItem.Caption:= ACaption;
  NewItem.Checked:= AChecked;
  FItems.Add(NewItem);
end;

function TJQToggleButton.GetItem(AIndex: integer): TJQToggleButtonItem;
begin
  result:= TJQToggleButtonItem(FItems.Items[AIndex]);
end;

function TJQToggleButton.GetCount: integer;
begin
  result:= FItems.Count;
end;

procedure TJQToggleButton.Clear;
begin
  FItems.Clear;
end;

constructor TJQToggleButton.Create(AButtonType:TToggleButtonType);
begin
  inherited create;
  FItems:= TObjectList.Create(true);
  FToggleButtonType:= AButtonType;
end;

destructor TJQToggleButton.destroy;
begin
  FItems.Free;
  inherited destroy;
end;

{ TJQIconButton }

function TJQIconButton.GetJs: string;
begin
  FJs.Clear;
  FJs.Add('<script type="text/javascript">');
  FJs.Add('			$(function(){');
  FJs.Add('				$("#' + FId + '").button(');
  if Not FEnabled then
    FJs.Add('{ disabled: true },');
  FJs.Add('{');
  FJs.Add('					icons: {');
  if FLeftRightIcons then
  begin
      FJs.Add('						primary: ''ui-icon-wrench'',');
      FJs.Add('						secondary: ''ui-icon-triangle-1-s''');
  end
  else
    FJs.Add('						primary: ''ui-icon-wrench''');
  FJs.Add('					}');
  FJs.Add('				})});');
  FJs.Add('		</script>');
  result:= FJs.Text;

end;

constructor TJQIconButton.Create(ALeftRightIcons: boolean);
begin
  inherited Create;
  FContent:= TStringList.Create;
  FJs:= TStringList.Create;
  FLeftRightIcons:= ALeftRightIcons;
  FButtonTextOnly:= False;
  FRole:= 'role="button"';
  FClasse:= 'class="ui-button ui-widget ui-state-default ui-corner-all ';
  if FLeftRightIcons then
    FClasse:= FClasse + ' ui-button-text-icons'
  else
    FClasse:= FClasse + ' ui-button-text-icon-primary';
  FClasse:= FClasse + '"';
end;

function TJQIconButton.GetContent: string;
begin
  FContent.Clear;
  FContent.Text:= '<div ' + FRole + ' ' + FClasse + ' id="' + FId + '">' + '<span class="ui-button-icon-primary ui-icon ui-icon-wrench"></span><span class="ui-button-text">' + FCaption + '</span>';
  if self.FLeftRightIcons then
    FContent.Text:= FContent.Text + '<span class="ui-button-icon-secondary ui-icon ui-icon-triangle-1-s"></span>';
  FContent.Text:= FContent.Text + '</div>';
  result:= FContent.Text;
end;


{ TJQHRefButton }

function TJQHRefButton.GetJs: string;
begin
  FJs.Clear;
  FJs.Add('<script type="text/javascript">');
  FJs.Add('			$(function(){');
  FJs.Add('$("#' + FId + '").button(');
  if Not FEnabled then
    FJs.Add('{ disabled: true }');
  FJs.Add(');');
  FJs.Add('});');
  FJs.Add('		</script>');
  result:= FJs.Text;
end;

constructor TJQHRefButton.Create;
begin
  inherited Create(True);
end;

function TJQHRefButton.GetContent: string;
begin
  FContent.Clear;
  FContent.Text:= '<a ' + FRole + ' ' + FClasse + ' id="' + FId + '" href="' + FHRef + '">' + '<span class="ui-button-text">' + FCaption + '</span></a>';
  result:= FContent.Text;
end;

{ TJQDivButton }

function TJQDivButton.GetJs: string;
begin
  FJs.Clear;
  FJs.Add('<script type="text/javascript">');
  FJs.Add('			$(function(){');
  FJs.Add('$("#' + FId + '").button(');
  if Not FEnabled then
    FJs.Add('{ disabled: true }');
  FJs.Add(');');
  FJs.Add('});');
  FJs.Add('		</script>');
  result:= FJs.Text;
end;

constructor TJQDivButton.Create;
begin
  inherited Create(True);
end;

function TJQDivButton.GetContent: string;
begin
  FContent.Clear;
  FContent.Text:= '<div ' + FRole + ' ' + FClasse + ' id="' + FId + '">' + '<span class="ui-button-text">' + FCaption + '</span></div>';
  result:= FContent.Text;
end;

{ TJQButton }

function TJQButton.GetContent: string;
begin
  FContent.Clear;
  FContent.Text:= '<button  id="' + FId + '">' + FCaption + '</button>';
  result:= FContent.Text;
end;

function TJQButton.GetJs: string;
begin
  FJs.Clear;
  FJs.Add('<script type="text/javascript">');
  FJs.Add('			$(function(){');
  FJs.Add('$("#' + FId + '").button(');
  if Not FEnabled then
    FJs.Add('{ disabled: true }');
  FJs.Add(');');
  FJs.Add('});');
  FJs.Add('		</script>');
  result:= FJs.Text;
end;

constructor TJQButton.Create(AButtonTextOnly: boolean);
begin
  inherited create;
  FButtonTextOnly:= AButtonTextOnly;
  FRole:= 'role="button"';
  FClasse:= 'class="ui-button ui-widget ui-state-default ui-corner-all';
  if FButtonTextOnly then
    FClasse:= FClasse + ' ui-button-text-only';
  FClasse:= FClasse + '"';
  FEnabled:= True
end;

end.

