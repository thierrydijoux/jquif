{
@abstract(Class for JQTable)
@author(Thierry DIJOUX <tjr.dijoux@gmail.com>)
Class for JQTable.
}
unit JQGrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

Type
  { Type of data to use with the grid}
  TDataType = (
            { XML format }
            dtXml,
            { Json format }
            dtJson);

  { Type of Ajax call when asking data from server}
  TAjaxCall = (
            { Post method }
            acPost,
            { Get method }
            acGet);

  { Type of alignement }
  TColAlign = (
            { Align left }
            caLeft,
            { Align right }
            caRight,
            { Align center }
            caCenter);

  { @abstract(Define a column in the grid)
  Define a column in the grid }
  TColumn = class(TObject)
  private
    FTitle: string;
    FName: string;
    FWidth: integer;
    FAlign: TColAlign;
    FIsSortable: boolean;
  public
    { Title of the column }
    property Title: string read FTitle write FTitle;
    { Name of the data column }
    property Name: string read FName write FName;
    { Width column }
    property Width: integer read FWidth write FWidth;
    { Alignement }
    property Align: TColAlign read FAlign write FAlign;
    { Define if the column is sortable }
    property IsSortable: boolean read FIsSortable write FIsSortable;
  end;

  {@abstract(Grid class)
  Grid Class }
  TJQGrid = Class(TObject)
  private
    FTitle: string;
    FUrl: string;
    FDataType: TDataType;
    FAjaxCallMethod: TAjaxCall;
    FLanguage: string;
    FDisplayRowNumber: Boolean;
    FPager: string;
    FHtmlTableID: string;
    FColumns: TObjectList;
    FRows: integer;
    FWidth: integer;
    FContent: TStrings;
    Function GetContent: string;
  public
    constructor Create;
    destructor Destroy; override;

    function AddColumn: TColumn;
    procedure AddColumn(ATitle: string; AName: string; AWidth: integer; AALign: TColAlign; AIsSortable: boolean);
    // URL for ajax query
    property Url: string read FUrl write FUrl;
    // Ajax communication : xml or json
    property DataType: TDataType read FDataType write FDataType;
    // Ajax call method : get or post
    property AjaxCallMethod: TAjaxCall read FAjaxCallMethod write FAjaxCallMethod;
    // language of the grid ui (en, fr, ...)
    property Language: string read FLanguage write FLanguage;
    // Number of rows to display
    property DisplayRowNumber: boolean read FDisplayRowNumber write FDisplayRowNumber;
    // Name of the div class for the pager
    property Pager: string read FPager write FPager;
    // ID of the div table
    property HtmlTableID: string read FHtmlTableID write FHtmlTableID;
    // The generated HTML
    property Content: string read GetContent;
    // Title of the grid
    property Title: string read FTitle write FTitle;
    property Rows: integer read FRows write FRows;
    // Width of the grid
    property Width: integer read FWidth write FWidth;
  end;

implementation

{ TJQGrid }

Function TJQGrid.GetContent: string;
Var
  Html: TStrings;
  Separator, Temp: string;
  i: integer;
begin
  Html:= TStringList.Create;
  Html.Add('<script type="text/javascript">');
//  Html.Add('$(function(){');
  Html.Add('	$(document).ready(function() {');
  Html.Add('$("#' + FHtmlTableID + '").jqGrid({');
  Html.Add('url:''' + FUrl + ''',');
  Html.Add('datatype: ''xml'' ,');
  Case FAjaxCallMethod of
    acPost: Html.Add('mtype: ''POST'',');
    acGet: Html.Add('mtype: ''GET'',');
  end;

  Temp:= 'colNames:[';
  for i:= 0 to FColumns.Count -1 do
  begin
    if i = FColumns.Count -1 then
      Separator:= ''
    else
      Separator:= ',';
    Temp:= Temp + '''' + TColumn(FColumns.Items[i]).Title + '''' + Separator;
  end;
  Temp:= Temp + '],';
  Html.Add(Temp);
  Html.Add('colModel :[');
  for i:= 0 to FColumns.Count -1 do
  begin
    if i = FColumns.Count -1 then
      Separator:= ''
    else
      Separator:= ',';
    Temp:= '{name:''' + TColumn(FColumns.Items[i]).Name + ''', index:''' + TColumn(FColumns.Items[i]).Name + '''';
    if TColumn(FColumns.Items[i]).Width > 0 then
      Temp:= Temp + ', width:' + inttostr(TColumn(FColumns.Items[i]).Width);
    Case TColumn(FColumns.Items[i]).Align of
      caLeft: Temp:= Temp + ', align:''left''';
      caRight: Temp:= Temp + ', align:''right''';
      caCenter: Temp:= Temp + ', align:''center''';
    end;
    if TColumn(FColumns.Items[i]).IsSortable then
      Temp:= Temp + ', sortable:true'
    else
      Temp:= Temp + ', sortable:false';

    Temp:= Temp + '}' + Separator;
    Html.Add(Temp);
  end;
  Html.Add('],');
  Html.Add('pager: ''#' + FPager + ''',');
  Html.Add('rowNum:' + inttostr(FRows) + ',');
  if FWidth > 0 then
    Html.Add('width:' + inttostr(FWidth) + ',');
  Html.Add('rowList:[10,20,30,50,100],');
  Html.Add('viewrecords: true,');
  Html.Add('gridview: true,');
  if FDisplayRowNumber then
    Html.Add('rownumbers:true,');
  Html.Add('caption: ''' + FTitle + '''');

  Html.Add('}).navGrid(''#' + FPager + ''',{edit:false,add:false,del:false});');
  Html.Add('});');
  Html.Add('</script>');


  result:= Html.Text;
  Html.Free;
end;

procedure TJQGrid.AddColumn(ATitle: string; AName: string; AWidth: integer; AALign: TColAlign; AIsSortable: boolean);
Var
  NewCol: TColumn;
begin
  NewCol:= TColumn.Create;
  NewCol.Title:= ATitle;
  NewCol.Name:= AName;
  NewCol.Width:= AWidth;
  NewCol.Align:= AAlign;
  NewCol.IsSortable:= AIsSortable;
  FColumns.Add(NewCol);
end;

function TJQGrid.AddColumn: TColumn;
Var
  NewCol: TColumn;
  i: integer;
begin
  NewCol:= TColumn.Create;
  i:= FColumns.Add(NewCol);
  result:= TColumn(FColumns.Items[i]);
end;

constructor TJQGrid.Create;
begin
  FContent:= TStringList.Create;
  FColumns:= TObjectList.create(true);
  FDataType:= dtXml;
  FAjaxCallMethod:= acGet;
  FPager:= 'pager';
  FRows:= 10;
  FWidth:= 0;
end;

destructor TJQGrid.Destroy;
begin
  FContent.Free;
  FColumns.Free;
  inherited destroy;
end;

end.

