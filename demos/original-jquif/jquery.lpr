Program jquery;

Uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  CThreads,
{$ENDIF}{$ENDIF}
main, JQForm, HtmlFormElements, JQGrid, JqButton,
JqDatePicker, jqAutoComplete, JQBase, HtmlTemplate, JQTab,
JQAccordion, JQResizer, jqEvents, jqDialog, BaseList;

{$R *.res}

begin
  DMMain:= TDMMAin.Create(nil)
end.
