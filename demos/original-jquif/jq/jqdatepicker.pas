unit JqDatePicker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jqBase;

Type
  TJQDatePicker = class(TJQBase)
  private
    FRole: string;
  protected
    function GetContent: string; override;
    function GetJs: string; override;
  public
    constructor Create;
  end;

implementation

{ TJQDatePicker }

function TJQDatePicker.GetContent: string;
begin
  FContent.Clear;
//  FContent.Text:= '<div ' + FRole + ' ' + FClasse + ' id="' + FId + '">' + '<span class="ui-button-text">' + FCaption + '</span></div>';
  result:= '<div id="' + FId + '"></div>';
end;

function TJQDatePicker.GetJs: string;
begin
  FJs.Clear;
  FJs.Add('<script type="text/javascript">');
  FJs.Add('	$(function() {');
  FJs.Add('	$.datepicker.setDefaults( $.datepicker.regional[ "" ] );');
  FJs.Add('	$( "#' + FId + '" ).datepicker( $.datepicker.regional[ "fr" ] );');
  FJs.Add('	});');
  FJs.Add('	</script>');
  result:= FJs.Text;
end;

constructor TJQDatePicker.Create;
begin
  inherited Create;
  FRole:= 'role="button"';
  FClasse:= 'class="ui-datepicker-inline ui-datepicker ui-widget ui-widget-content ui-helper-clearfix ui-corner-all"';
end;

{
<div class="hasDatepicker" id="datepicker"><div style="display: block;" class="ui-datepicker-inline ui-datepicker ui-widget ui-widget-content ui-helper-clearfix ui-corner-all">
<div class="ui-datepicker-header ui-widget-header ui-helper-clearfix ui-corner-all">
<a class="ui-datepicker-prev ui-corner-all" onclick="DP_jQuery_1327919812441.datepicker._adjustDate('#datepicker', -1, 'M');" title="Prev">
<span class="ui-icon ui-icon-circle-triangle-w">Prev</span></a>
<a class="ui-datepicker-next ui-corner-all" onclick="DP_jQuery_1327919812441.datepicker._adjustDate('#datepicker', +1, 'M');" title="Next">
<span class="ui-icon ui-icon-circle-triangle-e">Next</span></a>
<div class="ui-datepicker-title"><span class="ui-datepicker-month">January</span>&nbsp;<span class="ui-datepicker-year">2012</span></div></div><table class="ui-datepicker-calendar"><thead><tr><th class="ui-datepicker-week-end"><span title="Sunday">Su</span></th><th><span title="Monday">Mo</span></th><th><span title="Tuesday">Tu</span></th><th><span title="Wednesday">We</span></th><th><span title="Thursday">Th</span></th><th><span title="Friday">Fr</span></th><th class="ui-datepicker-week-end"><span title="Saturday">Sa</span></th></tr></thead><tbody><tr><td class=" ui-datepicker-week-end " onclick="DP_jQuery_1327919812441.datepicker._selectDay('#datepicker',0,2012, this);return false;"><a class="ui-state-default" href="#">1</a></td><td class=" " onclick="DP_jQuery_1327919812441.datepicker._selectDay('#datepicker',0,2012, this);return false;"><a class="ui-state-default" href="#">2</a></td><td class=" " onclick="DP_jQuery_1327919812441.datepicker._selectDay('#datepicker',0,2012, this);return false;"><a class="ui-state-default" href="#">3</a></td><td class=" " onclick="DP_jQuery_1327919812441.datepicker._selectDay('#datepicker',0,2012, this);return false;"><a class="ui-state-default" href="#">4</a></td><td class=" " onclick="DP_jQuery_1327919812441.datepicker._selectDay('#datepicker',0,2012, this);return false;"><a class="ui-state-default" href="#">5</a></td><td class=" " onclick="DP_jQuery_1327919812441.datepicker._selectDay('#datepicker',0,2012, this);return false;"><a class="ui-state-default" href="#">6</a></td><td class=" ui-datepicker-week-end " onclick="DP_jQuery_1327919812441.datepicker._selectDay('#datepicker',0,2012, this);return false;"><a class="ui-state-default" href="#">7</a></td></tr><tr><td class=" ui-datepicker-week-end " onclick="DP_jQuery_1327919812441.datepicker._selectDay('#datepicker',0,2012, this);return false;"><a class="ui-state-default" href="#">8</a></td><td class=" " onclick="DP_jQuery_1327919812441.datepicker._selectDay('#datepicker',0,2012, this);return false;"><a class="ui-state-default" href="#">9</a></td><td class=" " onclick="DP_jQuery_1327919812441.datepicker._selectDay('#datepicker',0,2012, this);return false;"><a class="ui-state-default" href="#">10</a></td><td class=" " onclick="DP_jQuery_1327919812441.datepicker._selectDay('#datepicker',0,2012, this);return false;"><a class="ui-state-default" href="#">11</a></td><td class=" " onclick="DP_jQuery_1327919812441.datepicker._selectDay('#datepicker',0,2012, this);return false;"><a class="ui-state-default" href="#">12</a></td><td class=" " onclick="DP_jQuery_1327919812441.datepicker._selectDay('#datepicker',0,2012, this);return false;"><a class="ui-state-default" href="#">13</a></td><td class=" ui-datepicker-week-end " onclick="DP_jQuery_1327919812441.datepicker._selectDay('#datepicker',0,2012, this);return false;"><a class="ui-state-default" href="#">14</a></td></tr><tr><td class=" ui-datepicker-week-end " onclick="DP_jQuery_1327919812441.datepicker._selectDay('#datepicker',0,2012, this);return false;"><a class="ui-state-default" href="#">15</a></td><td class=" " onclick="DP_jQuery_1327919812441.datepicker._selectDay('#datepicker',0,2012, this);return false;"><a class="ui-state-default" href="#">16</a></td><td class=" " onclick="DP_jQuery_1327919812441.datepicker._selectDay('#datepicker',0,2012, this);return false;"><a class="ui-state-default" href="#">17</a></td><td class=" " onclick="DP_jQuery_1327919812441.datepicker._selectDay('#datepicker',0,2012, this);return false;"><a class="ui-state-default" href="#">18</a></td><td class=" " onclick="DP_jQuery_1327919812441.datepicker._selectDay('#datepicker',0,2012, this);return false;"><a class="ui-state-default" href="#">19</a></td><td class=" " onclick="DP_jQuery_1327919812441.datepicker._selectDay('#datepicker',0,2012, this);return false;"><a class="ui-state-default" href="#">20</a></td><td class=" ui-datepicker-week-end " onclick="DP_jQuery_1327919812441.datepicker._selectDay('#datepicker',0,2012, this);return false;"><a class="ui-state-default" href="#">21</a></td></tr><tr><td class=" ui-datepicker-week-end " onclick="DP_jQuery_1327919812441.datepicker._selectDay('#datepicker',0,2012, this);return false;"><a class="ui-state-default" href="#">22</a></td><td class=" " onclick="DP_jQuery_1327919812441.datepicker._selectDay('#datepicker',0,2012, this);return false;"><a class="ui-state-default" href="#">23</a></td><td class=" " onclick="DP_jQuery_1327919812441.datepicker._selectDay('#datepicker',0,2012, this);return false;"><a class="ui-state-default" href="#">24</a></td><td class=" " onclick="DP_jQuery_1327919812441.datepicker._selectDay('#datepicker',0,2012, this);return false;"><a class="ui-state-default" href="#">25</a></td><td class=" " onclick="DP_jQuery_1327919812441.datepicker._selectDay('#datepicker',0,2012, this);return false;"><a class="ui-state-default" href="#">26</a></td><td class=" " onclick="DP_jQuery_1327919812441.datepicker._selectDay('#datepicker',0,2012, this);return false;"><a class="ui-state-default" href="#">27</a></td><td class=" ui-datepicker-week-end " onclick="DP_jQuery_1327919812441.datepicker._selectDay('#datepicker',0,2012, this);return false;"><a class="ui-state-default" href="#">28</a></td></tr><tr><td class=" ui-datepicker-week-end " onclick="DP_jQuery_1327919812441.datepicker._selectDay('#datepicker',0,2012, this);return false;"><a class="ui-state-default" href="#">29</a></td><td class=" ui-datepicker-days-cell-over  ui-datepicker-current-day ui-datepicker-today" onclick="DP_jQuery_1327919812441.datepicker._selectDay('#datepicker',0,2012, this);return false;"><a class="ui-state-default ui-state-highlight ui-state-active" href="#">30</a></td><td class=" " onclick="DP_jQuery_1327919812441.datepicker._selectDay('#datepicker',0,2012, this);return false;"><a class="ui-state-default" href="#">31</a></td><td class=" ui-datepicker-other-month ui-datepicker-unselectable ui-state-disabled">&nbsp;</td><td class=" ui-datepicker-other-month ui-datepicker-unselectable ui-state-disabled">&nbsp;</td><td class=" ui-datepicker-other-month ui-datepicker-unselectable ui-state-disabled">&nbsp;</td><td class=" ui-datepicker-week-end ui-datepicker-other-month ui-datepicker-unselectable ui-state-disabled">&nbsp;</td></tr></tbody></table></div></div>
}
end.

