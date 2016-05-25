// Demo for the JqUIF classes

unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Math,
  SpiderCGI, SpiderUtils, SpiderAction, JQForm, JQGrid, JQButton,
  JqDatePicker, JQAutoComplete, HtmlTemplate, jqTab, JQAccordion, JQResizer,
  jqEvents, jqDialog;

const
  ExePath = '/cgi-bin/';

type

  { TDMMAin }

  TDMMAin = class(TDataModule)
    saGetData: TSpiderAction;
    saValidateForm: TSpiderAction;
    saLoadExternalFile: TSpiderAction;
    SpiderCGI1: TSpiderCGI;
    procedure DataModuleCreate(Sender: TObject);
    procedure saGetDataRequest(Sender: TObject; Request: TSpiderRequest;
      var Response: TSpiderResponse);
    procedure saLoadExternalFileRequest(Sender: TObject;
      Request: TSpiderRequest; var Response: TSpiderResponse);
    procedure saValidateFormRequest(Sender: TObject; Request: TSpiderRequest;
      var Response: TSpiderResponse);
    procedure SpiderCGI1Request(Sender: TObject; Request: TSpiderRequest;
      var Response: TSpiderResponse);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  DMMAin: TDMMAin;

implementation


{$R *.lfm}

{ TDMMAin }

procedure TDMMAin.DataModuleCreate(Sender: TObject);
begin
  SpiderCGI1.Execute;
end;

// fake procedure to return data as XML to the JQGrid
procedure TDMMAin.saGetDataRequest(Sender: TObject; Request: TSpiderRequest;
  var Response: TSpiderResponse);
Var
  i: integer;
  Start, Page, Limit, Count, TotalPages: integer;
begin

  Count:= 10001;
  if Request.Query('rows') <> '' then
    Limit:= StrToInt(Request.Query('rows'))
  else
    Limit:= 0;

  if Request.Query('page') <> '' then
    Page:= StrToInt(Request.Query('page'))
  else
    Page:= 1;

  if (Count > 0) and (Limit > 0) then
    TotalPages:= ceil(Count/Limit)
  else
    TotalPages:= 0;

  if (Page > TotalPages) then
    Page:= TotalPages;

  Start:= Limit * Page - Limit;
  if Start < 0 Then Start:= 0;

  Response.ContentType:='Content-type: text/xml;charset=utf-8';
  Response.Add('<?xml version ="1.0" encoding="utf-8"?>');
  Response.Add('<rows>');
  Response.Add('  <page>' + IntToStr(page) + '</page>');
  Response.Add('  <total>' + IntToStr(TotalPages) + '</total>');
  Response.Add('  <records>' + IntToStr(Count) + '</records>');
  for i:= start to Limit + Start do
  begin
    Response.Add('    <row id = ''' + inttostr(i) + '''>');
    Response.Add('      <cell>ID' + inttostr(i) + '</cell>');
    Response.Add('      <cell>Debug: ' + 'Limit=' + IntToStr(Limit) + 'Start=' + IntToStr(Start) + '</cell>');
    Response.Add('    </row>');
  end;
  Response.Add('</rows>');
end;

// Return the content of the file to be display in the jqtab
procedure TDMMAin.saLoadExternalFileRequest(Sender: TObject;
  Request: TSpiderRequest; var Response: TSpiderResponse);
Var
  TheFile: string;
begin
  if Request.Query('file') <> '' then
  begin
    theFile:= 'jqdata/' + Request.Query('file');
    if not fileExists(TheFile) then
      Response.Add('<strong>Error, the file ' + TheFile + ' does not exists !</strong>')
    else
      Response.Content.LoadFromFile(TheFile);
  end
  else
    Response.Add('<strong>Error, no file specified !</strong>');
end;

// Simulate an ajax call when the submit button of the form is clicked
procedure TDMMAin.saValidateFormRequest(Sender: TObject;
  Request: TSpiderRequest; var Response: TSpiderResponse);
begin
  Response.Add('your user is <strong>' + Request.Form('user') + '</strong>');
end;

procedure TDMMAin.SpiderCGI1Request(Sender: TObject; Request: TSpiderRequest;
  var Response: TSpiderResponse);
Var
  JQForm: TJQForm;
  JQGrid: TJQGrid;
  sl: TStringList;
  button: TJQDivButton;
  buttonHRef: TJQHRefButton;
  ButtonIcon2: TJQIconButton;
  ButtonIcon: TJQIconButton;
  DTP: TJqDatePicker;
  JQAutoComplete: TJQAutoComplete;
  j: integer;
  Template: THtmlTemplate;
  tab: TJQTab;
  accordion: TJQAccordion;
  jresizer: TJQResizer;
  tgb: TJQToggleButton;
  tgb2: TJQToggleButton;
  jqEvent: TJavaScriptEvent;
  btndialog: TJQButton;
  jqEventbtndialog: TJavaScriptEvent;
  simpleDialog: TJQDialog;
  dlgbtn: TJQDialogButton;
begin
  // JQForm
  JQForm:= TJQForm.Create;
  JQForm.Action:='/cgi-bin/jquery/validate';
  JQForm.Classe:='cmsform';
  JQForm.Id:='form';
  JQForm.AddEdit('User', 'user', 'user', 'cuser', true, 'Enter a user name !');
  JQForm.AddRadio('Radio 1', 'cradio', 'radioobli', 'r1', 'robli', true, 'Please select one !');
  JQForm.AddRadio('Radio 2', 'cradio', 'radioobli', 'r2', 'robli', true, 'Please select one !');
  JQForm.AddCheckBox('CheckBox 1', 'ccbx', 'cbxb', 'c1', 'robli', true, 'Please select one !');
  JQForm.AddCheckBox('CheckBox 2', 'ccbx', 'ccbx', 'c2', 'robli', true, 'Please select one !');
  JQForm.AddTextArea('Text Area', 'txta', 'ctxta', 'ttt', false, '');
  sl:= TStringList.Create;
  sl.Add('item1=Item 1');
  sl.Add('item2=Item 2');
  sl.Add('item3=Item 3');
  JQForm.AddSelect('Select', 'sel1', 'csell', 'sell', false, '', sl);
  sl.Free;
  JQForm.AddPassword('Password', 'aaa', 'cpwd', 'pwd', true, 'Please enter a password !');
  JQForm.AddSubmitButton('submit', 'submit', 'Submit');
  JQForm.AddResetButton('reset', 'Clear');

  // Create the template object and load the template
  Template:= THtmlTemplate.Create('jqdata/template.html');
  // Assigning the tags to be replace
  Template.Tags.Add('<!--jspath-->', '/js/');
  Template.Tags.Add('<!--csspath-->', '/jquerydata/');
  Template.Load;

  // Adding content to the template
  Template.AddExtraContent('<h2>Form demo</h2>');
  Template.AddExtraContent(JQForm.Content);
  Template.AddExtraJavaScript(JQForm.JavaScript);
  Template.AddExtraCss(JQForm.Css);

  // JQGrid
  JQGrid:= TJQGrid.Create;
  JQGrid.Width:= 500;
  JQGrid.Rows:= 20;
  JQGrid.HtmlTableID:= 'list';
  JQGrid.Url:= '/cgi-bin/jquery/GetData';

  With JQGrid.AddColumn Do
  begin
    Title:= 'Column 1';
    Name:= 'Col1';
    IsSortable:= true;
  end;
  With JQGrid.AddColumn Do
  begin
    Title:= 'Column 2';
    Name:= 'Col2';
    IsSortable:= false;
  end;

  Template.AddExtraJavaScript(JQGrid.Content);

  // buttons
  Template.AddExtraContent('<h2>Button demo</h2>');
  button:= TJQDivButton.Create;
  Button.Id:='divButton';
  button.Caption:= 'Div Button';
  Template.AddExtraContent(button.Content);
  Template.AddExtraJavaScript(Button.JavaScript);

  buttonHRef:= TJQHRefButton.Create;
  buttonHRef.Id:='linkButton';
  buttonHRef.Caption:= 'Link Button';
  buttonHRef.Href:='/cgi-bin/jquery/#';

  Template.AddExtraJavaScript(buttonHRef.JavaScript);

  ButtonIcon:= TJQIconButton.Create(False);
  ButtonIcon.Id:='leftIconButton';
  ButtonIcon.Caption:='Icon Button';
  ButtonIcon.Enabled:= false;
  Template.AddExtraContent(ButtonIcon.Content);
  Template.AddExtraJavaScript(ButtonIcon.JavaScript);

  Template.AddExtraContent('<h2>Button  with event demo</h2>');
  Template.AddExtraContent('Click to show the message !<br /><br />');
  ButtonIcon2:= TJQIconButton.Create(True);
  ButtonIcon2.Id:='BothIconButton';
  ButtonIcon2.Caption:='Icon Button';
  ButtonIcon2.Enabled:= True;
  Template.AddExtraContent(ButtonIcon2.Content);
  Template.AddExtraJavaScript(ButtonIcon2.JavaScript);

  // Assigning an click event to the ButtonIcon2
  jqEvent:= TJavaScriptEvent.Create(jeClick);
  jqEvent.Selector:= '#' + ButtonIcon2.Id;
  jqEvent.Script.Add('alert(''hi from event on ButtonIcon2'');');
  Template.AddExtraJavaScript(jqEvent.GeneratedScript);
  jqEvent.Free;

  // DatePicker
  Template.AddExtraContent('<h2>Date picker demo</h2>');
  DTP:= TJqDatePicker.Create;
  DTP.Id:= 'datepicker';
  Template.AddExtraContent(DTP.Content);
  Template.AddExtraJavaScript(DTP.JavaScript);

  // Resizer
  Template.AddExtraContent('<h2>Resizer demo</h2>');
  jresizer:= TJQResizer.Create;
  jresizer.Id:='resize';
  jresizer.Caption:= 'Resize Me !!!';
  jresizer.Width:=150;
  jresizer.Height:=150;
  Template.AddExtraContent(jresizer.Content);
  Template.AddExtraJavaScript(jresizer.JavaScript);
  Template.AddExtraCss(jresizer.Css);

  // Autocomplete
  Template.AddExtraContent('<h2>Autocomplete demo</h2>');
  Template.AddExtraContent('<strong>Type "I" to activate </strong><br />');
  JQAutoComplete:= TJQAutoComplete.Create;
  JQAutoComplete.Id:= 'autocomp';
  JQAutoComplete.SourceListName:='List1';
  JQAutoComplete.InputLabel:='Test';
  for j:= 0 to 10 do
    JQAutoComplete.Values.Add('Item ' + inttostr(j));
  Template.AddExtraContent(JQAutoComplete.Content);
  Template.AddExtraJavaScript(JQAutoComplete.JavaScript);

  // jqTab
  Template.AddExtraContent('<h2>Tab demo</h2>');
  tab:= TJQTab.create;
  tab.Id:='tabs';
  tab.AddTab('Tab 1');
  tab.AddTab('Tab 2 (Ajax call)');
  tab.AddTab('Tab 3');
  for j:= 0 to tab.TabCount -1 do
    Tab.Tab[j].AddContent('lorem ipsum' + inttostr(j));

  Tab.Tab[0].AddContent(buttonHRef.Content);
  Tab.Tab[1].UseAjax:= True;
  Tab.Tab[1].AjaxURL:= '/cgi-bin/jquery/load?file=test.htm';
  Template.AddExtraContent(tab.Content);
  Template.AddExtraJavaScript(tab.JavaScript);

  // Toogle button
  Template.AddExtraContent('<h2>Toggle button</h2>');
  Template.AddExtraContent('<hr />');
  Template.AddExtraContent('<h3>Radio style</h3>');
  tgb:= TJQToggleButton.Create(tbRadio);
  tgb.Id:='tgb1';
  tgb.AddItem('Radio 1', true);
  tgb.AddItem('Radio 2');
  Template.AddExtraContent(tgb.Content);
  Template.AddExtraJavaScript(tgb.JavaScript);

  Template.AddExtraContent('<br /><br /><h3>CheckBox style</h3>');
  tgb:= TJQToggleButton.Create(tbCheckBox);
  tgb.Id:='tgb2';
  tgb.AddItem('CheckBox 1', true);
  tgb.AddItem('CheckBox 2');
  Template.AddExtraContent(tgb.Content);
  Template.AddExtraJavaScript(tgb.JavaScript);
  Template.AddExtraCss(tgb.Css);

  // Dialog
  Template.AddExtraContent('<h2>Dialog demo</h2>');
  Template.AddExtraContent('<hr />');
  simpleDialog:= TJQDialog.Create(false);
  SimpleDialog.Title:= 'A Simple Message';
  SimpleDialog.Modal:= false;
  SimpleDialog.Id:='sd1';
  SimpleDialog.Message:='Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin sit amet metus eros';
  SimpleDialog.Buttons.Add('ok');
  SimpleDialog.Buttons.Items[SimpleDialog.Buttons.Count -1].Script.Add('$( this ).dialog( "close" );');;
  Template.AddExtraContent(SimpleDialog.Content);
  Template.AddExtraJavaScript(SimpleDialog.JavaScript);
  SimpleDialog.Free;

  btndialog:= TJQButton.Create(true);
  btndialog.Caption:='Simple message';
  btndialog.Id:= 'btndial1';
  Template.AddExtraContent(btndialog.Content);
  Template.AddExtraJavaScript(btndialog.JavaScript);

  jqEventbtndialog:= TJavaScriptEvent.Create(jeClick);
  jqEventbtndialog.Selector:= '#' + btndialog.Id;
  jqEventbtndialog.Script.Add('$( "#sd1" ).dialog( "open" );');
  jqEventbtndialog.Script.Add('return false;');

  Template.AddExtraJavaScript(jqEventbtndialog.GeneratedScript);
  jqEventbtndialog.Free;
  btndialog.Free;

  // Accordion
  Template.AddExtraContent('<h2>Accordion demo</h2>');
  Template.AddExtraContent('<hr />');
  accordion:= TJQAccordion.create;
  accordion.Id:= 'Accordion';
  Accordion.UseResizer(350, 220, 140, true, true);
  accordion.OpenOnMouseOver:=false;
  accordion.AddAccordion('Accordion 1');
  accordion.AddAccordion('Accordion 2');
  accordion.AddAccordion('Accordion 3');

  for j:= 0 to accordion.TabCount -1 do
    accordion.Tab[j].AddContent('lorem ipsum' + inttostr(j));
  Template.AddExtraContent(accordion.Content);
  Template.AddExtraJavaScript(accordion.JavaScript);

  tgb.Free;
  response.Add(Template.Content);
  jresizer.Free;
  accordion.free;
  tab.free;
  Template.Free;
  JQAutoComplete.Free;
  DTP.Free;
  Button.Free;
  buttonHRef.Free;
  ButtonIcon.Free;
  ButtonIcon2.Free;
  JQGrid.Free;
end;

end.

