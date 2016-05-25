unit JQResizer;
{< @abstract(An HTML div that could be resized)
   @author(Thierry DIJOUX <tjr.dijoux@gmail.com>)
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JQBase;

Type
  TJQResizer = class(TJQBase)
  private
    FCaption: string;
    FWidth: integer;
    FHeight: integer;
  protected
    function GetContent: string; override;
    function GetJavaScript(location: ExtraJSloc): string; override;
    function GetCss: string; override;
  public
    property Caption: string read FCaption write FCaption;
    property Width: integer read FWidth write FWidth;
    property Height: integer read FHeight write FHeight;
  end;

implementation

function TJQResizer.GetContent: string;
begin
    FContent.Clear;
    FContent.Add('<div id="' + FId + '" class="ui-widget-content">');
    FContent.Add('  <h3 class="ui-widget-header">' + FCaption + '</h3>');
    FContent.Add('</div>');
    Result:=FContent.Text;
end;

function TJQResizer.GetJavaScript(location: ExtraJSloc): string;
begin
    if location<>locHeader then begin
        Result:='';
        exit;
    end;
    FJsHeader.Clear;
    FJsHeader.Add('<script>');
    FJsHeader.Add(' $(function() {');
    FJsHeader.Add('  $( "#' + FId + '" ).resizable();');
    FJsHeader.Add('  });');
    FJsHeader.Add('</script>');
    Result:=FJsHeader.Text;
end;

function TJQResizer.GetCss: string;
begin
    if (FWidth>0) and (FHeight>0) then begin
        FCss.Add('<style>');
        FCss.Add('#' + FId + ' { width: ' + inttostr(FWidth) + 'px; height:' + inttostr(FHeight) + 'px; padding: 0.5em; }');
        FCss.Add('#' + FId + ' h3 { text-align: center; margin: 0; }');
        FCss.Add('</style>');
        Result:=FCss.Text;
    end else begin
        Result:= '';
    end;
end;

end.

