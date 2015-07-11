unit JQTable;
{< @abstract(Renders HTML tables with optional styling)
   @author(Mario Guerra <mguerra13@gmail.com>)
   JQTable contains all the cells data in a string matrix,
   and could colour individual cells serverside when populating HTML data.
   Also, it could style the table with jQuery plugin DataTables version 1.10.7,
   see http://www.datatables.net/, doing jQuery-UI style integration too.

   TODO: Add ajax capability to populate data (only json) with DataTables
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fgl, JQBase;

type
    TTableStyle = (tsNone, tsDataTables);

    TStringRow = class(specialize TFPGList<string>)
    private
        function GetIt(Index: Integer): string;
        procedure PutIt(Index: Integer; const Item: string);
    public
        property Items[i: Integer]: string read GetIt write PutIt; default;
    end;

    TStringMatrix = class(specialize TFPGObjectList<TStringRow>)
    private
        function GetIt(Index: Integer): TStringRow;
    public
        constructor Create;
        property Items[i: Integer]: TStringRow read GetIt; default;
    end;

    TJQTable = class(TJQBase)
    private
        FNumCols: integer;
        FNumRows: integer;
        FNumRecords: integer;        //< could be less than the number of rows
        FCaption: string;            //< title for the whole table
        FWidth: array of integer;    //< widths of columns
        FTitle: array of string;     //< titles of columns
        FFooter: array of string;    //< footers of columns
        FAlignment: array of string; //< alignment of columns
        FVisible: array of boolean;  //< visibility of columns
        // Column type: 0=normal, 1=input, 2=hiperlink, 3=button, 4=checkbox 5=with color
        FColumnType: array of integer;
        // Parameter depending on column type: 1=width, 4=checked
        FColumnParam: array of integer;
        FCell: TStringMatrix;
        FTableStyle: TTableStyle;
        FLanguage: string;
        // The following fields are options of DataTables plugin
        DTpaging: boolean;
        DTpageLenght: integer;
        DTlengthChange: boolean;
        DTsearching: boolean;
        DTordering: boolean;
        DTinfo: boolean;
        procedure Clear;
        procedure SetNumCols(num: integer);
        procedure SetWidth(col: integer; value: integer);
        function GetTotalWidth: integer;
        procedure SetTitle(col: integer; value: string);
        procedure SetFooter(col: integer; value: string);
        procedure SetAlignment(col: integer; value: string);
        function GetVisible(col: integer): boolean;
        procedure SetVisible(col: integer; value: boolean);
        procedure SetColumnType(col: integer; value: integer);
        procedure SetColumnParam(col: integer; value: integer);
        function GetCell(row,col: integer): string;
        procedure SetCell(row,col: integer; value: string);
        function LanguageFile(code: string): string;
    protected
        function GetContent: string; override;
        function GetJavaScript(location: ExtraJSloc): string; override;
        function GetCss: string; override;
    public
        constructor Create;
        destructor Destroy; override;
        property NumCols: integer read FNumCols write SetNumCols;
        property NumRows: integer read FNumRows;
        property NumRecords: integer read FNumRecords write FNumRecords;
        property Width[col: integer]: integer write SetWidth;
        property TotalWidth: integer read GetTotalWidth;
        property Caption: string write FCaption;
        property Title[col: integer]: string write SetTitle;
        property Footer[col: integer]: string write SetFooter;
        property Alignment[col: integer]: string write SetAlignment;
        property Visible[col: integer]: boolean read GetVisible write SetVisible;
        property ColumnType[col: integer]: integer write SetColumnType;
        property ColumnParam[col: integer]: integer write SetColumnParam;
        property Cell[row,col: integer]: string read GetCell write SetCell;
        property TableStyle: TTableStyle read FTableStyle write FTableStyle;
        property Language: string read FLanguage write FLanguage;
        property EnablePaging: boolean write DTpaging;
        property PageLenght: integer write DTpageLenght;
        property EnableLenghtChange: boolean write DTlengthChange;
        property EnableFiltering: boolean write DTsearching;
        property EnableSorting: boolean write DTordering;
        property EnableSummaryInfo: boolean write DTinfo;
    end;

implementation

{ TStringRow }

function TStringRow.GetIt(Index: Integer): string;
begin
    if Index>=Count then Result:='' else Result:=Get(Index);
end;

procedure TStringRow.PutIt(Index: Integer; const Item: string);
begin
    if Index>=Count then Count:=Index+1;
    Put(Index,Item);
end;

{ TStringMatrix }

constructor TStringMatrix.Create;
begin
    inherited Create(true);
end;

function TStringMatrix.GetIt(Index: Integer): TStringRow;
var OldCount,i: integer;
begin
    if index>=Count then begin
        OldCount:=Count;
        Count:=Index + 1;
        for i:=OldCount to Count-1 do begin
            Put(i,TStringRow.Create);
        end;
    end;
    Result:=Get(index);
end;

{ TJQTable }

constructor TJQTable.Create;
begin
    inherited Create;
    FNumCols:=0;
    FNumRows:=0;
    FCaption:='';
    FNumRecords:=0;
    FCell:=TStringMatrix.Create;
    FTableStyle:=tsNone;
    FLanguage:='en';
    DTpaging:=true;
    DTpageLenght:=10;
    DTlengthChange:=false;
    DTsearching:=false;
    DTordering:=false;
    DTinfo:=false;
end;

destructor TJQTable.Destroy;
begin
    SetLength(FWidth,0);
    SetLength(FTitle,0);
    SetLength(FFooter,0);
    SetLength(FAlignment,0);
    SetLength(FVisible,0);
    SetLength(FColumnType,0);
    SetLength(FColumnParam,0);
    FCell.Clear;
    FCell.Free;
    inherited Destroy;
end;

procedure TJQTable.Clear;
begin
    FNumCols:=0;
    FNumRows:=0;
    FCaption:='';
    FCell.Clear;
end;

procedure TJQTable.SetNumCols(num:integer);
var col : integer;
begin
    FNumCols:=num;
    SetLength(FWidth,num);
    SetLength(FTitle,num);
    SetLength(FFooter,num);
    SetLength(FAlignment,num);
    SetLength(FVisible,num);
    SetLength(FColumnType,num);
    SetLength(FColumnParam,num);
    for col:=1 to num do begin
        FWidth[col-1]:=70;
        FTitle[col-1]:='';
        FFooter[col-1]:='';
        FAlignment[col-1]:='right';
        FVisible[col-1]:=true;
        FColumnType[col-1]:=0;
        FColumnParam[col-1]:=0;
    end;
end;

procedure TJQTable.SetWidth(col: integer; value: integer);
begin
    FWidth[col-1]:=value;
end;

function TJQTable.GetTotalWidth: integer;
var i,total: integer;
begin
    total:=10; // 10 pixels more for table borders
    for i:=1 to FNumCols do begin
        if FVisible[i-1] then total:=total+FWidth[i-1];
    end;
    Result:=total;
end;

procedure TJQTable.SetTitle(col: integer; value: string);
begin
    FTitle[col-1]:=value;
end;

procedure TJQTable.SetFooter(col: integer; value: string);
begin
    FFooter[col-1]:=value;
end;

procedure TJQTable.SetAlignment(col: integer; value: string);
begin
    FAlignment[col-1]:=value;
end;

function TJQTable.GetVisible(col: integer): boolean;
begin
    Result:=FVisible[col-1];
end;

procedure TJQTable.SetVisible(col: integer; value: boolean);
begin
    FVisible[col-1]:=value;
end;

// Ajusta el tipo de columna de la tabla, y puede ser uno de los siguientes:
//    0 = normal     (default with optional color)
//    1 = input box
//    2 = hiperlink  (cell at right with the link)
//    3 = button     (cell at right with the onclick event)
//    4 = checkbox
procedure TJQTable.SetColumnType(col: integer; value: integer);
begin
    FColumnType[col-1]:=value;
end;

// El parámetro depende del tipo de columna, por ahora las dos opciones son:
//   para tipo = 1, value es el ancho del texto en caracteres
//   para tipo = 4, value es si está checked(=1) o no(=0)
// Ver detalle de implementacion en el metodo GetContent
procedure TJQTable.SetColumnParam(col: integer; value: integer);
begin
    FColumnParam[col-1]:=value;
end;

function TJQTable.GetCell(row,col: integer): string;
begin
    Result:=FCell[row-1][col-1];
end;

procedure TJQTable.SetCell(row,col: integer; value: string);
begin
    if row>FNumRows then FNumRows:=row;
    if col>FNumCols then FNumCols:=col;
    if (row>0) and (col>0) then begin
        FCell[row-1][col-1]:=value;
    end;
end;

function TJQTable.GetContent: string;
var color, color1, color2, colorC: string;
    fila, celda: string;
    i,k,posi: integer;
    atLeastOne: boolean;
begin
    // The following row colors and sequence are the used in DataTables CSS
    color1:='#F9F9F9';
    color2:='#FFFFFF';
    if FTableStyle=tsDataTables then begin
        FClasse:=FClasse+' display';
    end;
    FContent.Clear;
    FContent.Add('<table id="'+FId+'" class="'+FClasse+'" width='+IntToStr(TotalWidth)+'>');
    if FCaption<>'' then FContent.Add('<caption>'+FCaption+'</caption>');
    FContent.Add('<thead>');
    fila:='';
    atLeastOne:=false;
    for i:=1 to NumCols do begin
        if not FVisible[i-1] then continue;
        fila:=fila+'<th width='+IntToStr(FWidth[i-1])+' align='+FAlignment[i-1]+' >'+
                     FTitle[i-1]+'</th>';
        if (not atLeastOne) and (FTitle[i-1]<>'') then atLeastOne:=true;
    end;
    if atLeastOne then FContent.Add('<tr>'+fila+'</tr>');
    FContent.Add('</thead>');
    FContent.Add('<tbody>');
    for k:=1 to FNumRows do begin
        if odd(k) then color:=color1 else color:=color2;
        fila:='<tr bgcolor='+color+'>';
        for i:=1 to FNumCols do begin
            if not FVisible[i-1] then continue;
            colorC:='';  // No color cell by default
            case FColumnType[i-1] of
                0: begin // Normal cell (optional background color following #COLOR#)
                       celda:=Cell[k,i];
                       posi:=pos('#COLOR#', celda);
                       if posi>0 then begin
                           colorC:=copy(celda,posi+6,20); // includes the second #
                           celda:=copy(celda,1,posi-1);
                       end;
                   end;
                1: begin // Cell for data input
                       celda:='<input type=text size='+IntToStr(FColumnParam[i-1])+
                              ' name=enter'+IntToStr(i)+'_'+IntToStr(k)+
                              ' value="'+Cell[k,i]+'">';
                   end;
                2: begin // Cell with hiperlink (link in the following cell)
                       celda:='<a href="'+Cell[k,i+1]+'">'+Cell[k,i]+'</a>';
                   end;
                3: begin // Cell with a button (onclick event in the following cell)
                       celda:='<input type=button '+
                              ' name=boton'+IntToStr(i)+'_'+IntToStr(k)+
                              ' value="'+Cell[k,i]+'" '+' onclick="'+Cell[k,i+1]+'" >';
                   end;
                4: begin // Cell with a checkbox
                       celda:='<input type=checkbox '+
                              ' name=check'+IntToStr(i)+'_'+IntToStr(k)+
                              ' value="'+Cell[k,i]+'" ';
                       if FColumnParam[i-1]=0 then celda:=celda+'>'
                                              else celda:=celda+' checked >';
                   end;
                else begin
                       celda:='';
                   end;
            end;
            fila:=fila+'<td ';
            if colorC<>'' then fila:=fila+'bgcolor='+colorC+' ';
            fila:=fila+'width='+IntToStr(FWidth[i-1])+' align='+FAlignment[i-1]+' >';
            fila:=fila+celda;
            fila:=fila+'</td>';
        end;
        fila:=fila+'</tr>';
        FContent.Add(fila);
    end;
    FContent.Add('</tbody>');
    FContent.Add('<tfoot>');
    fila:='';
    atLeastOne:=false;
    for i:=1 to NumCols do begin
        if not FVisible[i-1] then continue;
        fila:=fila+'<th width='+IntToStr(FWidth[i-1])+' align='+FAlignment[i-1]+' >'+
                     FFooter[i-1]+'</th>';
        if (not atLeastOne) and (FFooter[i-1]<>'') then atLeastOne:=true;
    end;
    if atLeastOne then FContent.Add('<tr>'+fila+'</tr>');
    FContent.Add('</tfoot>');
    FContent.Add('</table>');
    Result:=FContent.Text;
end;

function TJQTable.GetJavaScript(location: ExtraJSloc): string;
begin
    case location of
        locHeader: begin
            FJsHeader.Clear;
            case FTableStyle of
                tsDataTables: begin
                    FJsHeader.Add('<script src="../js/jquery.dataTables-1.10.7.min.js"></script>');
                    // The following is for JQUERY-UI integration (same styles as JQUERY-UI)
                    // See http://www.datatables.net/manual/styling/jqueryui
                    FJsHeader.Add('<script src="../js/dataTables.jqueryui.min.js"></script>');
                end;
            end;
            Result:=FJsHeader.Text;
        end;
        locBodyBottom: begin
            FJsBottom.Clear;
            case FTableStyle of
                tsDataTables: begin
                    // DataTable initialization procedure
                    FJsBottom.Add('<script>');
                    FJsBottom.Add('  $("#'+FId+'").DataTable( {');
                    FJsBottom.Add('     order: [], '); // no initial order
                    if FLanguage<>'en' then begin
                        FJsBottom.Add(' language: {url: "/js/i18n/datatables/'+
                                          LanguageFile(FLanguage)+'"}, ');
                    end;
                    FJsBottom.Add('  paging: '+BoolToStr(DTpaging)+', ');
                    FJsBottom.Add('  pageLength: '+IntToStr(DTpageLenght)+', ');
                    FJsBottom.Add('  lengthChange: '+BoolToStr(DTlengthChange)+', ');
                    FJsBottom.Add('  searching: '+BoolToStr(DTsearching)+', ');
                    FJsBottom.Add('  ordering: '+BoolToStr(DTordering)+', ');
                    FJsBottom.Add('  info: '+BoolToStr(DTinfo)+' ');
                    FJsBottom.Add('} );');
                    FJsBottom.Add('</script>');
                end;
            end;
            Result:=FJsBottom.Text;
        end;
    else begin
            Result:='';
        end;
    end;
end;

function TJQTable.GetCss: string;
begin
    FCss.Clear;
    if FTableStyle=tsDataTables then begin
        FCss.Add('<link rel="stylesheet" href="../css/jquery.dataTables.css">');
        // The following is for JQUERY-UI integration  (same styles as JQUERY-UI)
        // See http://www.datatables.net/manual/styling/jqueryui
        FCss.Add('<link rel="stylesheet" href="../css/dataTables.jqueryui.css">');
        // The following makes the toolbars as wide as the table (we take off the 8px padding)
        // and centered as the table does in the dataTables_wrapper div
        FCss.Add('<style>'+
                 '  .dataTables_wrapper .ui-toolbar {'+
                 '      width: '+IntToStr(TotalWidth)+'px; '+
                 '      margin-left: auto; '+
                 '      margin-right: auto; '+
                 '      padding: 0px;'+
                 '  }'+
                 '</style>');
    end;
    Result:=FCss.Text;
end;

// Valid codes are ISO 639-1 two-letter codes (with optional country code).
// Json data files are provided by DataTables i18n plugin
function TJQTable.LanguageFile(code: string): string;
begin
    case code of
        'zh': Result:='Chinese-traditional.json';
        'cs': Result:='Czech.json';
        'fr': Result:='French.json';
        'de': Result:='German.json';
        'it': Result:='Italian.json';
        'ja': Result:='Japanese.json';
        'pt': Result:='Portuguese.json';
        'pt_BR': Result:='Portuguese-Brasil.json';
        'ru': Result:='Russian.json';
        'es': Result:='Spanish.json';
    else Result:='';
    end;
end;

end.

