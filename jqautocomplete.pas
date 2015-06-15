unit JQAutoComplete;

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
    protected
        function GetContent: string; override;
        function GetJavaScript(location: ExtraJSloc): string; override;
        function GetCss: string; override;
    public
        constructor Create;
        destructor Destroy; override;
        property Contents: string read GetContent;
        property Id: string read FId write FId;
        property InputLabel: string read FLabel write FLabel;
        property SourceListName: string read FSourceListName write FSourceListName;
        property Values: TStrings read FValues write FValues;
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

function TJQAutoComplete.GetJavaScript(location: ExtraJSloc): string;
var ListValue: string;
    i: integer;
begin
    if location<>locHeader then begin
        Result:='';
        exit;
    end;
    ListValue:='';
    for i:=0 to FValues.Count-1 do begin
        ListValue:=ListValue+'"'+FValues.Strings[i]+'"';
        if i<FValues.Count-1 then ListValue:=ListValue+',';
    end;
    FJsHeader.Clear;
    FJsHeader.Add('<script>');
    FJsHeader.Add('  $(function(){');
    FJsHeader.Add('    var '+FSourceListName+' = ['+ListValue+'];');
    FJsHeader.Add('    $("#'+FId+'").autocomplete({');
    FJsHeader.Add('      source: '+FSourceListName);
    FJsHeader.Add('    });');
    FJsHeader.Add('  });');
    FJsHeader.Add('</script>');
    Result:=FJsHeader.Text;
end;

function TJQAutoComplete.GetCss: string;
begin
    FCss.Clear;
    Result:=FCss.Text;
end;

end.
