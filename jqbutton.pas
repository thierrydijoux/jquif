unit JQButton;
{< @abstract(Class for Buttons)
   @author(Thierry DIJOUX <tjr.dijoux@gmail.com>)
   Class for Buttons
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, JQBase, Contnrs;

type
  { Button types for HTML <button> tag buttons, used in JQButton and JQIconButton
    See https://www.w3.org/wiki/HTML/Elements/button }
    TButtonType = ( btButton, btSubmit, btReset );

  { Button types for a toggle buttons, used in TJQToggleButton }
    TToggleButtonType = ( tbRadio, tbCheckBox );

  { @abstract(Simple button class)
    Simple button class }
    TJQButton = class(TJQBase)
    private
        // Role should be "button" by default as a widget role, Href button role
        // should be "link", and Toogle button role should be "radio" or "checkbox".
        // See http://www.w3.org/TR/wai-aria/roles#widget_roles
        FRole: string;
        FType: TButtonType;
    protected
        // Enable or not the button
        FEnabled: boolean;
        // Caption of the button
        FCaption: string;
        function GetContent: string; override;
        function GetJavaScript(location: ExtraJSloc): string; override;
        function GetCss: string; override;
    public
        constructor Create;
        // Caption of the button
        property Caption: string read FCaption write FCaption;
        // Enable or disable the button
        property Enabled: boolean read FEnabled write FEnabled;
        // The widget role of this object
        property ButtonRole: string read FRole write FRole;
        // The button type of this object
        property ButtonType: TButtonType read FType write FType;
    end;

  { See http://api.jqueryui.com/theming/icons for the list of possible icons
    Following icons from JQUERY-UI as of version 1.11.4 }
    iconUI = (iconUI_none, iconUI_alert, iconUI_arrow1E, iconUI_arrow1N, iconUI_arrow1Ne,
              iconUI_arrow1Nw, iconUI_arrow1S, iconUI_arrow1Se, iconUI_arrow1Sw,
              iconUI_arrow1W, iconUI_arrow2EW, iconUI_arrow2NS, iconUI_arrow2NeSw,
              iconUI_arrow2SeNw, iconUI_arrow4, iconUI_arrow4Diag, iconUI_arrowrefresh1E,
              iconUI_arrowrefresh1N, iconUI_arrowrefresh1S, iconUI_arrowrefresh1W,
              iconUI_arrowreturn1E, iconUI_arrowreturn1N, iconUI_arrowreturn1S,
              iconUI_arrowreturn1W, iconUI_arrowreturnthick1E, iconUI_arrowreturnthick1N,
              iconUI_arrowreturnthick1S, iconUI_arrowreturnthick1W, iconUI_arrowstop1E,
              iconUI_arrowstop1N, iconUI_arrowstop1S, iconUI_arrowstop1W,
              iconUI_arrowthick1E, iconUI_arrowthick1N, iconUI_arrowthick1Ne,
              iconUI_arrowthick1Nw, iconUI_arrowthick1S, iconUI_arrowthick1Se,
              iconUI_arrowthick1Sw, iconUI_arrowthick1W, iconUI_arrowthick2EW,
              iconUI_arrowthick2NS, iconUI_arrowthick2NeSw, iconUI_arrowthick2SeNw,
              iconUI_arrowthickstop1E, iconUI_arrowthickstop1N, iconUI_arrowthickstop1S,
              iconUI_arrowthickstop1W, iconUI_battery0, iconUI_battery1, iconUI_battery2,
              iconUI_battery3, iconUI_blank, iconUI_bookmark, iconUI_bullet,
              iconUI_calculator, iconUI_calendar, iconUI_cancel,
              iconUI_carat1E, iconUI_carat1N, iconUI_carat1Ne, iconUI_carat1Nw,
              iconUI_carat1S, iconUI_carat1Se, iconUI_carat1Sw, iconUI_carat1W,
              iconUI_carat2EW, iconUI_carat2NS, iconUI_cart, iconUI_check,
              iconUI_circleArrowE, iconUI_circleArrowN, iconUI_circleArrowS,
              iconUI_circleArrowW, iconUI_circleCheck, iconUI_circleClose,
              iconUI_circleMinus, iconUI_circlePlus, iconUI_circleTriangleE,
              iconUI_circleTriangleN, iconUI_circleTriangleS, iconUI_circleTriangleW,
              iconUI_circleZoomin, iconUI_circleZoomout, iconUI_circlesmallClose,
              iconUI_circlesmallMinus, iconUI_circlesmallPlus, iconUI_clipboard,
              iconUI_clock, iconUI_close, iconUI_closethick, iconUI_comment,
              iconUI_contact, iconUI_copy, iconUI_disk, iconUI_document,
              iconUI_documentB, iconUI_eject, iconUI_extlink, iconUI_flag,
              iconUI_folderCollapsed, iconUI_folderOpen, iconUI_gear,
              iconUI_gripDiagonalSe, iconUI_gripDottedHorizontal, iconUI_gripDottedVertical,
              iconUI_gripSolidHorizontal, iconUI_gripSolidVertical, iconUI_gripsmallDiagonalSE,
              iconUI_heart, iconUI_help, iconUI_home, iconUI_image, iconUI_info,
              iconUI_key, iconUI_lightbulb, iconUI_link, iconUI_locked, iconUI_mailClosed,
              iconUI_mailOpen, iconUI_minus, iconUI_minusthick, iconUI_newwin,
              iconUI_note, iconUI_notice, iconUI_pause, iconUI_pencil, iconUI_person,
              iconUI_pinS, iconUI_pinW, iconUI_play, iconUI_plus, iconUI_plusthick,
              iconUI_power, iconUI_print, iconUI_radioOff, iconUI_radioOn, iconUI_refresh,
              iconUI_scissors, iconUI_script, iconUI_search, iconUI_seekEnd,
              iconUI_seekFirst, iconUI_seekNext, iconUI_seekPrev, iconUI_shuffle,
              iconUI_signal, iconUI_signalDiag, iconUI_squaresmallClose, iconUI_squaresmallMinus,
              iconUI_squaresmallPlus, iconUI_star, iconUI_stop, iconUI_suitcase,
              iconUI_tag, iconUI_transferEW, iconUI_transferthickEW, iconUI_trash,
              iconUI_triangle1E, iconUI_triangle1N, iconUI_triangle1NE, iconUI_triangle1NW,
              iconUI_triangle1S, iconUI_triangle1SE, iconUI_triangle1SW, iconUI_triangle1W,
              iconUI_triangle2EW, iconUI_triangle2NS, iconUI_unlocked, iconUI_video,
              iconUI_volumeOff, iconUI_volumeOn, iconUI_wrench, iconUI_zoomin, iconUI_zoomout );

  { @abstract(Button with icons)
     Icon button }
    TJQIconButton = class(TJQButton)
    private
        FIconLeft : iconUI;  //< primary icon
        FIconRight : iconUI; //< secondary icon
        FIconsOnly : boolean;
    protected
        function GetContent: string; override;
        function GetJavaScript(location: ExtraJSloc): string; override;
    public
        constructor Create; overload;
        property IconRight: iconUI read FIconRight write FIconRight;
        property IconLeft: iconUI read FIconLeft write FIconLeft;
        property IconsOnly: boolean read FIconsOnly write FIconsOnly;
    end;

  { @abstract(Div button)
     Div button }
    TJQDivButton = class(TJQButton)
    protected
        function GetContent: string; override;
    public
        constructor Create;
    end;

  { @abstract(HRef button)
    HRef button }
    TJQHRefButton = class(TJQButton)
    private
        FHref: string;
        FTarget: string;
    protected
        function GetContent: string; override;
    public
        constructor Create;
        property Href: string read FHref write FHref;
        property Target: string read FTarget write FTarget;
    end;

  { @abstract(Button for the TJQToggleButton class)
    Button for the TJQToggleButton class }
    TJQToggleButtonItem = class
    private
        FValue: string;
        FCaption: string;
        FChecked: boolean;
    public
        // Value of the button for forms
        property Value: string read FValue write FValue;
        // Caption of the button
        property Caption: string read FCaption write FCaption;
        // Button is checked on not
        property Checked: boolean read FChecked write FChecked;
    end;

  { @abstract(Maintains a set of toggle buttons)
    Maintains a set of toggle buttons }
    TJQToggleButton = class(TJQButton)
    private
        FName: string;
        FItems: TObjectList;
        FToggleButtonType: TToggleButtonType;
        function GetItem(AIndex: integer): TJQToggleButtonItem;
        function GetCount: integer;
    protected
        function GetContent: string; override;
        function GetJavaScript(location: ExtraJSloc): string; override;
    public
        constructor Create(AButtonType: TToggleButtonType);
        destructor Destroy; override;
        property Name: string read FName write FName;
        procedure Clear;
        function AddItem(ACaption: string; AChecked: boolean = False): integer;
        function AddItem(ACaption,AValue: string; AChecked: boolean = False): integer;
        property Item[AItemIndex: integer]: TJQToggleButtonItem read GetItem;
        property Count: integer read GetCount;
    end;

    function IconAsString(AIcon: iconUI): string;

implementation

uses
    TypInfo;

{ TJQButton ------------------------------------------------------------------ }

constructor TJQButton.Create;
begin
    inherited Create;
    FRole:='button';
    FType:=btButton;
    FEnabled:=True;
    FCaption:='Click me!';
    FClasse:='';
end;

function TJQButton.GetContent: string;
var auxT: string;
begin
    case FType of
        btButton: auxT:='button';
        btSubmit: auxT:='submit';
        btReset: auxT:='reset';
    else auxT:='';
    end;
    FContent.Clear;
    FContent.Text:='<button type="'+auxT+'" role="'+FRole+'" '+
                   'class="ui-button '+FClasse+'" id="'+FId+'">'+
                   '<span>'+FCaption+'</span>'+
                   '</button>';
    Result:=FContent.Text;
end;

// JavaScript code nedeed for jQuery UI styles
function TJQButton.GetJavaScript(location: ExtraJSloc): string;
begin
    if location<>locHeader then begin
        Result:='';
        exit;
    end;
    FJsHeader.Clear;
    FJsHeader.Add('<script>');
    FJsHeader.Add('  $(function(){');
    FJsHeader.Add('    $("#'+FId+'").button(');
    if not FEnabled then FJsHeader.Add('{ disabled: true }');
    FJsHeader.Add('    );');
    FJsHeader.Add('  });');
    FJsHeader.Add('</script>');
    Result:=FJsHeader.Text;
end;

function TJQButton.GetCss: string;
begin
    FCss.Clear;
    Result:=FCss.Text;
end;

{ TJQIconButton -------------------------------------------------------------- }

constructor TJQIconButton.Create;
begin
    inherited Create;
    FIconLeft:=iconUI_none;
    FIconRight:=iconUI_none;
    FIconsOnly:=false;
end;

function TJQIconButton.GetContent: string;
begin
    Result:=inherited GetContent;
end;

function TJQIconButton.GetJavaScript(location: ExtraJSloc): string;
var auxS : string;
begin
    if location<>locHeader then begin
        Result:='';
        exit;
    end;
    FJsHeader.Clear;
    FJsHeader.Add('<script>');
    FJsHeader.Add('  $(function(){');
    FJsHeader.Add('    $("#'+FId+'").button(');
    if not FEnabled then FJsHeader.Add('{ disabled: true },');
    if FIconsOnly then FJsHeader.Add('{ text: false },');
    auxS:='';
    if FIconLeft<>iconUI_none then begin
        auxS:='{ icons: { primary: "'+IconAsString(FIconLeft)+'" ';
        if FIconRight<>iconUI_none then begin
            auxS:=auxS+', secondary: "'+IconAsString(FIconRight)+'" ';
        end;
        auxS:=auxS+'} }';
    end else begin
        if FIconRight<>iconUI_none then begin
            auxS:='{ icons: { secondary: "'+IconAsString(FIconRight)+'" ';
            auxS:=auxS+'} }';
        end;
    end;
    FJsHeader.Add(auxS+' );');
    FJsHeader.Add('  });');
    FJsHeader.Add('</script>');
    Result:=FJsHeader.Text;
end;

function IconAsString(AIcon: iconUI): string;
var auxS : string;
    i : integer;
begin
    auxS:=GetEnumName(TypeInfo(iconUI),ord(AIcon));
    auxS:=copy(auxS,8,100);
    i:=1;
    while i<=length(auxS) do begin
        if auxS[i]=upCase(auxS[i]) then begin
            Insert('-', auxS, i);
            inc(i);
        end;
        inc(i);
    end;
    Result:='ui-icon-'+lowerCase(auxS);
end;

{ TJQDivButton --------------------------------------------------------------- }

constructor TJQDivButton.Create;
begin
    inherited Create;
end;

function TJQDivButton.GetContent: string;
begin
    FContent.Clear;
    FContent.Text:='<div role="'+FRole+'" '+
                   'class="ui-button '+FClasse+'" id="'+FId+'">'+
                   '<span>'+FCaption+'</span>'+
                   '</div>';
    Result:=FContent.Text;
end;

{ TJQHRefButton -------------------------------------------------------------- }

constructor TJQHRefButton.Create;
begin
    inherited Create;
    FRole:='link';
    FHref:='';
    FTarget:='';
end;

function TJQHRefButton.GetContent: string;
var xtarget : string;
begin
    xtarget:='';
    if FTarget<>'' then xtarget:='target="'+FTarget+'"';
    FContent.Clear;
    FContent.Text:='<a role="'+FRole+'" '+
                   'class="ui-button '+FClasse+'" id="'+FId+'" '+
                   'href="'+FHRef+'" '+xtarget+'>'+
                   '<span>'+FCaption+'</span>'+
                   '</a>';
    Result:=FContent.Text;
end;

{ TJQToggleButton ------------------------------------------------------------ }

constructor TJQToggleButton.Create(AButtonType: TToggleButtonType);
begin
    inherited Create;
    FName:='group';
    FItems:=TObjectList.Create(True);
    FToggleButtonType:=AButtonType;
end;

destructor TJQToggleButton.Destroy;
begin
    FItems.Free;
    inherited Destroy;
end;

function TJQToggleButton.GetContent: string;
var i: integer;
    valChecked: string;
begin
    case FToggleButtonType of
        tbRadio:    FRole:='radio';
        tbCheckBox: FRole:='checkbox';
    end;
    FContent.Add('<div role="'+FRole+'" '+
                 'class="ui-buttonset '+FClasse+'" id="'+FId+'">');
    for i:=0 to FItems.Count-1 do begin
        valChecked:='';
        if TJQToggleButtonItem(FItems.Items[i]).Checked then valChecked:='checked';
        // The type of the input is the same as the role
        // The name is used when retrieving the value selected from a form
        FContent.Add('<input type="'+FRole+'" name="'+FName+'" '+
                     'id="'+FName+IntToStr(i+1)+'" '+
                     'value="'+TJQToggleButtonItem(FItems.Items[i]).Value+'" '+
                     valChecked+'>');
        FContent.Add('<label for="'+FName+IntToStr(i+1)+'">'+
                     TJQToggleButtonItem(FItems.Items[i]).Caption+
                     '</label>');
    end;
    FContent.Add('</div>');
    Result:=FContent.Text;
end;

function TJQToggleButton.GetJavaScript(location: ExtraJSloc): string;
begin
    if location<>locHeader then begin
        Result:='';
        exit;
    end;
    FJsHeader.Clear;
    FJsHeader.Add('<script>');
    FJsHeader.Add('  $(function(){');
    FJsHeader.Add('    $("#'+FId+'").buttonset(');
    FJsHeader.Add('    );');
    FJsHeader.Add('  });');
    FJsHeader.Add('</script>');
    Result:=FJsHeader.Text;
end;

function TJQToggleButton.AddItem(ACaption: string; AChecked: boolean=False): integer;
begin
    Result:=AddItem(ACaption, ACaption, AChecked);
end;

function TJQToggleButton.AddItem(ACaption,AValue: string; AChecked: boolean=False): integer;
var newItem: TJQToggleButtonItem;
begin
    newItem:=TJQToggleButtonItem.Create;
    newItem.Value:=AValue;
    newItem.Caption:=ACaption;
    newItem.Checked:=AChecked;
    Result:=FItems.Add(newItem);
end;

function TJQToggleButton.GetItem(AIndex: integer): TJQToggleButtonItem;
begin
    Result:=TJQToggleButtonItem(FItems.Items[AIndex]);
end;

function TJQToggleButton.GetCount: integer;
begin
    Result:=FItems.Count;
end;

procedure TJQToggleButton.Clear;
begin
    FItems.Clear;
end;

end.
