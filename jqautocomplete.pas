unit jqAutoComplete;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, JQBase;

type
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
        property Js: string read GetJs;
    end;

implementation

constructor TJQAutoComplete.Create;
begin
    inherited Create;
    FValues:=TStringList.Create;
end;

destructor TJQAutoComplete.Destroy;
begin
    FValues.Free;
    inherited Destroy;
end;

function TJQAutoComplete.GetContent: string;
begin
    FContent.Clear;
    FContent.Add('<div class="ui-widget">');
    FContent.Add('<label for="'+FId+'">'+FLabel+': </label>');
    FContent.Add('<input aria-haspopup="true" aria-autocomplete="list" '+
                 'role="textbox" autocomplete="off" class="ui-autocomplete-input" '+
                 'id="'+FId+'">');
    FContent.Add('</div>');
    Result:=FContent.Text;
end;

function TJQAutoComplete.GetJs: string;
var ListValue: string;
    i: integer;
begin
    for i:=0 to FValues.Count-1 do begin
        ListValue:=ListValue+'"'+FValues.Strings[i]+'"';
        if i<FValues.Count-1 then ListValue:=ListValue+',';
    end;
    FJs.Clear;
    FJs.Add('<script>');
    FJs.Add('  $(function(){');
    FJs.Add('    var '+FSourceListName+' = ['+ListValue+'];');
    FJs.Add('    $("#'+FId+'").autocomplete({');
    FJs.Add('      source: '+FSourceListName);
    FJs.Add('    });');
    FJs.Add('  });');
    FJs.Add('</script>');
    Result := FJs.Text;
end;

end.
