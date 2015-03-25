unit JQResizer;

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
    function GetJs: string; override;
    function GetCss: string; override;
  public
    property Caption: string read FCaption write FCaption;
    property Width: integer read FWidth write FWidth;
    property Height: integer read FHeight write FHeight;
  end;

implementation

function TJQResizer.GetCss: string;
begin
  inherited GetCss;
  if (FWidth > 0) and (FHeight > 0) then
  begin
    FCss.Add('<style>');
    FCss.Add('#' + FId + ' { width: ' + inttostr(FWidth) + 'px; height:' + inttostr(FHeight) + 'px; padding: 0.5em; }');
    FCss.Add('#' + FId + ' h3 { text-align: center; margin: 0; }');
    FCss.Add('</style>');
    Result:= FCss.Text;
  end
  else
    Result:= '';
end;

function TJQResizer.GetContent: string;
begin
  inherited;
  FContent.Clear;
  FContent.Add('<div id="' + FId + '" class="ui-widget-content">');
  FContent.Add('  <h3 class="ui-widget-header">' + FCaption + '</h3>');
  FContent.Add('</div>');
  result:= FContent.Text;
end;

function TJQResizer.GetJs: string;
begin
  FJs.Clear;
  FJs.Add('<script>');
  FJs.Add('	$(function() {');
  FJs.Add('		$( "#' + FId + '" ).resizable();');
  FJs.Add('	});');
  FJs.Add('	</script>');
  result:= FJs.Text;
end;

end.

