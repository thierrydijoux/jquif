unit JQGrid;
{< @abstract(Class implementing tabular data)
   @author(Thierry DIJOUX <tjr.dijoux@gmail.com>)
   Class for a table showing remote data via Ajax.
   It uses the jQuery plugin JQGrid version 4.7.0, being this version the last
   with a free licence MIT/GPL2, see http://www.trirand.com/blog/?p=1433
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Contnrs, JQBase;

type
    { Type of data to use with the grid}
    TDataType = (dtXML, dtJson);
    { Type of Ajax call when asking data from server}
    TAjaxCall = (acPost, acGet);
    { Type of alignement }
    TColAlign = (caNone, caLeft, caRight, caCenter);

    { @abstract(Define a column in the grid) Define a column in the grid }
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

    {@abstract(Grid class) Grid Class }
    TJQGrid = class(TJQBase)
    private
        FTitle: string;
        FAjaxUrl: string;
        FAjaxDataType: TDataType;
        FAjaxCallMethod: TAjaxCall;
        FLanguage: string;
        FDisplayRowNumber: Boolean;
        FPager: string;
        FColumns: TObjectList;
        FRows: integer;
        FWidth: integer;
        function LanguageFile(code: string): string;
    protected
        function GetContent: string; override;
        function GetJavaScript(location: ExtraJSloc): string; override;
        function GetCss: string; override;
    public
        constructor Create;
        destructor Destroy; override;
        function AddColumn: TColumn;
        procedure AddColumn(ATitle: string; AName: string; AWidth: integer; AALign: TColAlign; AIsSortable: boolean);
        // URL for ajax query
        property AjaxUrl: string read FAjaxUrl write FAjaxUrl;
        // Ajax communication : xml or json
        property AjaxDataType: TDataType read FAjaxDataType write FAjaxDataType;
        // Ajax call method : get or post
        property AjaxCallMethod: TAjaxCall read FAjaxCallMethod write FAjaxCallMethod;
        // language of the grid ui (en, fr, ...)
        property Language: string read FLanguage write FLanguage;
        // Number of rows to display
        property DisplayRowNumber: boolean read FDisplayRowNumber write FDisplayRowNumber;
        // Name of the div class for the pager
        property Pager: string read FPager write FPager;
        // Title of the grid
        property Title: string read FTitle write FTitle;
        property Rows: integer read FRows write FRows;
        // Width of the grid
        property Width: integer read FWidth write FWidth;
    end;

implementation

{TColumn}

procedure TJQGrid.AddColumn(ATitle: string; AName: string; AWidth: integer; AALign: TColAlign; AIsSortable: boolean);
var NewCol : TColumn;
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
var NewCol : TColumn;
    i : integer;
begin
    NewCol:=TColumn.Create;
    i:=FColumns.Add(NewCol);
    Result:=TColumn(FColumns.Items[i]);
end;

{ TJQGrid }

constructor TJQGrid.Create;
begin
    inherited Create;
    FColumns:=TObjectList.create(true);
    FAjaxDataType:=dtXML;
    FAjaxCallMethod:=acGet;
    FAjaxUrl:='';
    FLanguage:='en';
    FPager:='pager';
    FRows:=10;
    FWidth:=0;
end;

destructor TJQGrid.Destroy;
begin
    FColumns.Free;
    inherited Destroy;
end;

function TJQGrid.GetContent: string;
begin
    FContent.Clear;
    FContent.Add('<table id="'+FId+'" class="'+FClasse+'">');
    FContent.Add('  <tr>');
    FContent.Add('    <td></td>');
    FContent.Add('  </tr>');
    FContent.Add('</table>');
    FContent.Add('<div id="'+FPager+'"></div>');
    Result:=FContent.Text;
end;

function TJQGrid.GetJavaScript(location: ExtraJSloc): string;
var separator, temp : string;
    i : integer;
begin
    case location of
        locHeader: begin
            FJsHeader.Clear;
            FJsHeader.Add('<script src="../js/jquery.jqGrid-4.7.0.min.js"></script>');
            FJsHeader.Add('<script src="../js/i18n/jqgrid/'+LanguageFile(FLanguage)+'"></script>');
            Result:=FJsHeader.Text;
        end;
        locBodyBottom: begin
            FJsBottom.Clear;
            FJsBottom.Add('<script>');
            FJsBottom.Add('$("#'+FId+'").jqGrid( {');
            FJsBottom.Add('  url: "'+FAjaxUrl +'",');
            case FAjaxDataType of
                dtXML:  FJsBottom.Add('  datatype: "xml" ,');
                dtJson: FJsBottom.Add('  datatype: "json" ,');
            end;
            case FAjaxCallMethod of
                acPost: FJsBottom.Add('  mtype: "POST",');
                acGet:  FJsBottom.Add('  mtype: "GET",');
            end;
            temp:='  colNames: [';
            for i:=0 to FColumns.Count-1 do begin
                if i=FColumns.Count-1 then separator:=''
                                      else separator:=',';
                temp:=temp+'"'+TColumn(FColumns.Items[i]).Title+'"'+separator;
            end;
            temp:=temp + '],';
            FJsBottom.Add(temp);
            FJsBottom.Add('  colModel: [');
            for i:=0 to FColumns.Count-1 do begin
                if i=FColumns.Count-1 then separator:=''
                                      else separator:=',';
                temp:='{name: "'+TColumn(FColumns.Items[i]).Name+'", '+
                       'index: "'+TColumn(FColumns.Items[i]).Name+'"';
                if TColumn(FColumns.Items[i]).Width > 0 then begin
                    temp:=temp+ ', width: ' + inttostr(TColumn(FColumns.Items[i]).Width);
                end;
                case TColumn(FColumns.Items[i]).Align of
                    caLeft:   temp:=temp + ', align: left';
                    caRight:  temp:=temp + ', align: right';
                    caCenter: temp:=temp + ', align: center';
                end;
                if TColumn(FColumns.Items[i]).IsSortable then temp:=temp+', sortable: true'
                                                         else temp:=temp+', sortable: false';
                temp:=temp+'}'+separator;
                FJsBottom.Add(temp);
            end;
            FJsBottom.Add('],');
            FJsBottom.Add('  pager: "#'+FPager+'",');
            FJsBottom.Add('  rowNum: '+IntToStr(FRows)+',');
            if FWidth>0 then FJsBottom.Add('width: '+IntToStr(FWidth)+',');
            FJsBottom.Add('  rowList: [10,20,30,50,100],');
            FJsBottom.Add('  viewrecords: true,');
            FJsBottom.Add('  gridview: true,');
            if FDisplayRowNumber then FJsBottom.Add('rownumbers: true,');
            FJsBottom.Add('  caption: "'+FTitle+'"');
            FJsBottom.Add('} );');
            FJsBottom.Add('$("#'+FId+'").jqGrid( ');
            FJsBottom.Add('  navGrid, "#'+FPager+'", {edit:false,add:false,del:false} ');
            FJsBottom.Add(');');
            FJsBottom.Add('</script>');
            Result:=FJsBottom.Text;
        end;
    else begin
            Result:='';
        end;
    end;
end;

function TJQGrid.GetCss: string;
begin
    FCss.Clear;
    FCss.Add('<link rel="stylesheet" href="../css/ui.jqgrid.css">');
    Result:=FCss.Text;
end;

// Valid codes are ISO 639-1 two-letter codes (with optional country code)
// JavaScript files comes with jqGrid i18n distibution
function TJQGrid.LanguageFile(code: string): string;
begin
    code:=LowerCase(code);
    code:=StringReplace(code,'_','-',[rfReplaceAll]);
    Result:='grid.locale-'+code+'.js'
end;

end.

