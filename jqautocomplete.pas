unit jqAutoComplete;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jqBase;

Type

  TJQAutoComplete = class(TJQBase)
  private
    FLabel: string;
    FSourceListName: string;
    FValues: TStrings;
    function GetContent: string; override;
    function GetJs: string; override;
  public
    constructor Create;
    destructor Destroy; override;
    property Contents: string read GetContent;
    property Id: string read FId write FId;
    property InputLabel: string read FLabel write FLabel;
    property SourceListName: string read FSourceListName write FSourceListName;
    property Values: TStrings read FValues write FValues;
    property Js: String read GetJs;
  end;


{
<div class="ui-widget">
	<label for="countries">Countries: </label>
	<input aria-haspopup="true" aria-autocomplete="list" role="textbox" autocomplete="off" class="ui-autocomplete-input" id="countries">
</div>


<script type="text/javascript">
			$(function(){

				// Autocomplete
				var countryList = ["Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Antarctica", "Antigua and Barbuda", "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bermuda", "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil", "Brunei", "Bulgaria", "Burkina Faso", "Burma", "Burundi", "Cambodia", "Cameroon", "Canada", "Cape Verde", "Central African Republic", "Chad", "Chile", "China", "Colombia", "Comoros", "Congo, Democratic Republic", "Congo, Republic of the", "Costa Rica", "Cote d'Ivoire", "Croatia", "Cuba", "Cyprus", "Czech Republic", "Denmark", "Djibouti", "Dominica", "Dominican Republic", "East Timor", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Estonia", "Ethiopia", "Fiji", "Finland", "France", "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Greece", "Greenland", "Grenada", "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras", "Hong Kong", "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", "Israel", "Italy", "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya", "Kiribati", "Korea, North", "Korea, South", "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein", "Lithuania", "Luxembourg", "Macedonia", "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Marshall Islands", "Mauritania", "Mauritius", "Mexico", "Micronesia", "Moldova", "Mongolia", "Morocco", "Monaco", "Mozambique", "Namibia", "Nauru", "Nepal", "Netherlands", "New Zealand", "Nicaragua", "Niger", "Nigeria", "Norway", "Oman", "Pakistan", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Poland", "Portugal", "Qatar", "Romania", "Russia", "Rwanda", "Samoa", "San Marino", " Sao Tome", "Saudi Arabia", "Senegal", "Serbia and Montenegro", "Seychelles", "Sierra Leone", "Singapore", "Slovakia", "Slovenia", "Solomon Islands", "Somalia", "South Africa", "Spain", "Sri Lanka", "Sudan", "Suriname", "Swaziland", "Sweden", "Switzerland", "Syria", "Taiwan", "Tajikistan", "Tanzania", "Thailand", "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan", "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom", "United States", "Uruguay", "Uzbekistan", "Vanuatu", "Venezuela", "Vietnam", "Yemen", "Zambia", "Zimbabwe"];
				$("#countries").autocomplete({
					source: countryList
				});
                        });
</script>
}

implementation

constructor TJQAutoComplete.Create;
begin
  inherited Create;
  FValues:= TStringList.Create;
end;

destructor TJQAutoComplete.Destroy;
begin
  FValues.Free;
  inherited destroy;
end;

function TJQAutoComplete.GetContent: string;
begin
  FContent.Clear;
  FContent.Add('<div class="ui-widget">');
  FContent.Add('<label for="' + FId + '">'+ FLabel + ': </label>');
  FContent.Add('<input aria-haspopup="true" aria-autocomplete="list" role="textbox" autocomplete="off" class="ui-autocomplete-input" id="' + FId + '">');
  FContent.Add('</div>');
  result:= FContent.Text;
end;

function TJQAutoComplete.GetJs: string;
Var
  ListValue: string;
  i: integer;
begin
  for i:= 0 to FValues.Count -1 do
  begin
    ListValue:= ListValue + '"' + FValues.Strings[i] + '"';
    if i < FValues.Count -1 then ListValue:= ListValue + ','
  end;
  FJs.Clear;
  FJs.Add('<script type="text/javascript">');
  FJs.Add('			$(function(){');
  FJs.Add('				var ' + FSourceListName +' = [' + ListValue + '];');
  FJs.Add('				$("#' + FId + '").autocomplete({');
  FJs.Add('					source: ' + FSourceListName);
  FJs.Add('				});');
  FJs.Add('                      });');
  FJs.Add('</script>');
  Result:= FJs.Text
end;

end.

