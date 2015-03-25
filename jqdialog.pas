{
@abstract(Class for JQuery dialog)
@author(Thierry DIJOUX <tjr.dijoux@gmail.com>)
Class for JQuery dialog.
}
unit jqDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jqBase, BaseList;

Type

  { @abstract(Class for button in the dialog box)
    Class for button in the dialog box. }
  TJQDialogButton = class
  private
    FCaption: string;
    FScript: TStrings;
  public
    constructor Create;
    constructor Create(ACaption : string); overload;
    destructor Destroy; override;
    // Caption of the button
    property Caption: string read FCaption write FCaption;
    // Script (in javascript) in the button
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
    function Add(ACaption: string): integer; overload;
    property Items[i:integer]: TJQDialogButton read GetItems write SetItems;
  end;

  { @abstract(Class for the JQDialog)
    Class for the JQDialog }
  TJQDialog = class(TJQBase)
  private
    FTitle: string;
    FMessage: string;
    FModal: boolean;
    FResizable: boolean;
    FHeight: integer;
    FAutoOpen: boolean;
    FButtons: TButtons;
  protected
    function GetContent: string; override;
    function GetJs: string; override;
  public
    constructor Create(AModal: boolean); overload;
    destructor Destroy; override;
    // Title of the dialog box
    property Title: string read FTitle write FTitle;
    // Message to display
    property Message: string read FMessage write FMessage;
    // Height of the dialog box
    property Height: integer read FHeight write FHeight;
    // Show modal or not
    property Modal: boolean read FModal write FModal;
    // Auto open when page is loaded
    property AutoOpen: boolean read FAutoOpen write FAutoOpen;
    // List of buttons
    property Buttons: TButtons read FButtons;
  end;

implementation

{ TButtons }

function TButtons.Add(ACaption: string): integer;
Var
  NewButton: TJQDialogButton;
begin
  NewButton:= TJQDialogButton.Create(ACaption);
  result:= self.Add(NewButton);
end;

function TButtons.GetItems(AIndex: integer): TJQDialogButton;
begin
  result:= TJQDialogButton(inherited GetItems(AIndex));
end;

procedure TButtons.SetItems(AIndex: integer; const Value: TJQDialogButton);
begin
  inherited SetItems(AIndex, Value);
end;

function TButtons.Add(AObject: TJQDialogButton): integer;
begin
  result:= inherited Add(AObject);
end;

{ TJQDialogButtons }

constructor TJQDialogButton.Create;
begin
  FScript:= TStringList.Create;
end;
constructor TJQDialogButton.Create(ACaption : string);
begin
  FScript:= TStringList.Create;
  FCaption:= ACaption;
end;

destructor TJQDialogButton.Destroy;
begin
  FScript.Free;
  inherited destroy;
end;

{ TJQDialog }

function TJQDialog.GetContent: string;
begin
  FContent.clear;
  FContent.Add('<div id="' + FId + '" title="' + FTitle + '">');
  FContent.Add('<p><span class="ui-icon ui-icon-alert" style="float:left; margin:0 7px 20px 0;"></span>');
  FContent.Add(FMessage + '</p>');
  FContent.Add('</div>');
  result:= FContent.Text;
end;

function TJQDialog.GetJs: string;
Var
  i: integer;
begin
  FJs.clear;
  FJs.Add('<script>');
  FJs.Add('	$(function() {');
  FJs.Add('                  $( "#dialog:ui-dialog" ).dialog( "destroy" );');
  FJs.Add('                  $( "#' + FId + '" ).dialog({');
  if FHeight > 0 then
    FJs.Add('                     height:' + inttostr(FHeight) + ',');
  if FAutoOpen then
    FJs.Add('                       autoOpen: true,')
  else
    FJs.Add('                       autoOpen: false,');
  if FModal then
    FJs.Add('                     modal: true,')
  else
    FJs.Add('                     modal: false,');
  if FResizable then
    FJs.Add('                     resizable: true')
  else
    FJs.Add('                     resizable: false');
  if Buttons.Count > 0 then
  begin
    FJs.Add(',');
    FJs.Add('buttons: {');
    for i:= 0 to Buttons.Count -1 do
    begin;
      FJs.Add(Buttons.Items[i].Caption + ': function() {');
      FJs.Add(Buttons.Items[i].Script.Text);
      FJs.Add('}');
    end;
    FJs.Add('}');
  end;
  FJs.Add('                     });');
  FJs.Add('                });');
  FJs.Add('</script>');
  result:= FJs.Text;
end;

constructor TJQDialog.Create(AModal: boolean);
begin
  inherited create;
  FModal:= AModal;
  FResizable:= false;
  FAutoOpen:= false;
  FButtons:= TButtons.Create;
end;

destructor TJQDialog.Destroy;
begin
  FButtons.Free;
  inherited destroy;
end;

end.

