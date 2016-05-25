unit JQDialog;
{< @abstract(Class for JQuery dialog)
   @author(Thierry DIJOUX <tjr.dijoux@gmail.com>)
   Class for JQuery dialog.
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, JQBase, JQButton, BaseList;

type
  { @abstract(Class for button in the dialog box)
    Class for button in the dialog box. }
  TJQDialogButton = class
  private
    FCaption: string;
    FDefault: boolean;
    FIcon: iconUI;
    FScript: TStrings;
  public
    constructor Create;
    constructor Create(ACaption: string); overload;
    destructor Destroy; override;
    // Caption of the button
    property Caption: string read FCaption write FCaption;
    // Default button for ENTER key
    property Default: boolean read FDefault write FDefault;
    // Optional icon to display
    property Icon: iconUI read FIcon write FIcon;
    // Script (in JavaScript) inside the button
    property Script: TStrings read FScript write FScript;
  end;

  { @abstract(List of TJQDialogButton)
    List of TJQDialogButton }
  TButtons = class(TBaseList)
  protected
    function GetItems(AIndex: integer): TJQDialogButton; reintroduce;
    procedure SetItems(AIndex: integer; const Value: TJQDialogButton); reintroduce;
  public
    function Add(AObject: TJQDialogButton): integer;
    function Add(ACaption: string; ADefault:boolean=false; AIcon:iconUI=iconUI_none): integer; overload;
    property Items[i:integer]: TJQDialogButton read GetItems write SetItems;
  end;

  { @abstract(Class for the JQDialog)
    Class for the JQDialog }
  TJQDialog = class(TJQBase)
  private
    FTitle: string;
    FMessage: string;
    FIcon: iconUI;
    FModal: boolean;
    FResizable: boolean;
    FWidth: integer;
    FHeight: integer;
    FAutoOpen: boolean;
    FButtons: TButtons;
  protected
    function GetContent: string; override;
    function GetJavaScript(location: ExtraJSloc): string; override;
    function GetCss: string; override;
  public
    constructor Create(AModal: boolean); overload;
    destructor Destroy; override;
    // Title of the dialog box
    property Title: string read FTitle write FTitle;
    // Message to display
    property Message: string read FMessage write FMessage;
    // Optional icon to display
    property Icon: iconUI read FIcon write FIcon;
    // Show modal or not
    property Modal: boolean read FModal write FModal;
    // If user can resize the dialog
    property Resizable: boolean read FResizable write FResizable;
    // Witdh of the dialog box
    property Width: integer read FWidth write FWidth;
    // Height of the dialog box
    property Height: integer read FHeight write FHeight;
    // Auto open when page is loaded
    property AutoOpen: boolean read FAutoOpen write FAutoOpen;
    // List of buttons
    property Buttons: TButtons read FButtons;
  end;

implementation

{ TButtons }

function TButtons.Add(ACaption: string; ADefault:boolean=false; AIcon:iconUI=iconUI_none): integer;
var NewButton: TJQDialogButton;
begin
    NewButton:=TJQDialogButton.Create(ACaption);
    NewButton.Default:=ADefault;
    NewButton.Icon:=AIcon;
    result:=self.Add(NewButton);
end;

function TButtons.GetItems(AIndex: integer): TJQDialogButton;
begin
    result:=TJQDialogButton(inherited GetItems(AIndex));
end;

procedure TButtons.SetItems(AIndex: integer; const Value: TJQDialogButton);
begin
    inherited SetItems(AIndex, Value);
end;

function TButtons.Add(AObject: TJQDialogButton): integer;
begin
    result:=inherited Add(AObject);
end;

{ TJQDialogButtons }

constructor TJQDialogButton.Create;
begin
    FCaption:='';
    FDefault:=false;
    FIcon:=iconUI_none;
    FScript:=TStringList.Create;
end;

constructor TJQDialogButton.Create(ACaption: string);
begin
    FCaption:=ACaption;
    FDefault:=false;
    FIcon:=iconUI_none;
    FScript:=TStringList.Create;
end;

destructor TJQDialogButton.Destroy;
begin
    FScript.Free;
    inherited Destroy;
end;

{ TJQDialog }

constructor TJQDialog.Create(AModal: boolean);
begin
    inherited create;
    FIcon:=iconUI_none;
    FHeight:=0; // takes the ui-dialog default
    FWidth:=0;  // takes the ui-dialog default
    FModal:=AModal;
    FResizable:=false;
    FAutoOpen:=false;
    FButtons:=TButtons.Create;
end;

destructor TJQDialog.Destroy;
begin
    FButtons.Free;
    inherited Destroy;
end;

function TJQDialog.GetContent: string;
begin
    FContent.clear;
    FContent.Add('<div id="' + FId + '" title="' + FTitle + '">');
    FContent.Add('<p><span ');
    if FIcon<>iconUI_none then FContent.Add('class="ui-icon '+IconAsString(FIcon)+'" ');
    FContent.Add('style="float:left; margin:0 7px 20px 0;"></span>');
    FContent.Add(FMessage + '</p>');
    FContent.Add('</div>');
    result:=FContent.Text;
end;

function TJQDialog.GetJavaScript(location: ExtraJSloc): string;
var i,defbut : integer;
    comma : char;
begin
    if location<>locHeader then begin
        Result:='';
        exit;
    end;
    FJsHeader.Clear;
    FJsHeader.Add('<script>');
    FJsHeader.Add(' $(function() {');
    FJsHeader.Add('     $( "#dialog:ui-dialog" ).dialog( "destroy" );');
    FJsHeader.Add('     $( "#' + FId + '" ).dialog({');
    if FWidth>0 then FJsHeader.Add(' width:' + IntToStr(FWidth) + ',');
    if FHeight>0 then FJsHeader.Add(' height:' + IntToStr(FHeight) + ',');
    if FAutoOpen then FJsHeader.Add(' autoOpen: true,')
                 else FJsHeader.Add(' autoOpen: false,');
    if FModal then FJsHeader.Add(' modal: true,')
              else FJsHeader.Add(' modal: false,');
    if FResizable then FJsHeader.Add(' resizable: true')
                  else FJsHeader.Add(' resizable: false');
    comma:=' ';
    if Buttons.Count>0 then begin
        defbut:=0;
        FJsHeader.Add(', buttons: [');
        for i:=0 to Buttons.Count-1 do begin
            FJsHeader.Add(comma+'{ text: "'+Buttons.Items[i].Caption+'", ');
            if FIcon<>iconUI_none then FJsHeader.Add('icons: { primary: "'+IconAsString(Buttons.Items[i].Icon)+'" }, ');
            FJsHeader.Add('click: function() {'+Buttons.Items[i].Script.Text+'} }');
            comma:=',';
            if Buttons.Items[i].Default then defbut:=i+1;
        end;
        FJsHeader.Add('], ');
        FJsHeader.Add('focus: function(event,ui) { '+
                      '  $(this).keypress( function(e) { '+
                      '    if (e.keyCode === $.ui.keyCode.ENTER) { '+
                      '      $(this).parent().find("button:eq('+IntToStr(defbut)+')").trigger("click");'+
                      '      return false;'+
                      '    } '+
                      '  } ); '+
                      '}');
    end;
    FJsHeader.Add('  });');
    FJsHeader.Add(' });');
    FJsHeader.Add('</script>');
    result:=FJsHeader.Text;
end;

function TJQDialog.GetCss: string;
begin
    FCss.Clear;
    Result:=FCss.Text;
end;

end.

