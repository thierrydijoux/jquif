unit JQBase;
{< @abstract(Base class for all jQuery ui objects)
   @author(Thierry DIJOUX <tjr.dijoux@gmail.com>)
   Base class for jQuery-UI object
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type
    ExtraJSloc = (locNone, locHeader, locBodyTop, locBodyBottom);

    { Base class for JQUERY-UI object }
    TJQBase = class
    protected
        // class of the html element
        FClasse: string;
        // id of the html element
        FId: string;
        // HTML content
        FContent: TStrings;
        // Generated javascript
        FJsHeader: TStrings;  //< To be put inside <head></head>
        FJsTop: TStrings;     //< To be put at the begining of <body></body>
        FJsBottom: TStrings;  //< To be put at the end of <body></body>
        // Css content
        FCss: TStrings;
        // Return the generated html
        function GetContent: string; virtual; abstract;
        // Return the generated javascript
        function GetJavaScript(location: ExtraJSloc): string; virtual; abstract;
        // Return the generated css
        function GetCss: string; virtual; abstract;
    public
        constructor Create;
        destructor Destroy; override;
        // HTML Content
        property Content: string read GetContent;
        // Id of the html element
        property Id: string read FId write FId;
        // class of the html element
        property Classe: string read FClasse write FClasse;
        // Generated Javascript
        property JavaScript[location: ExtraJSloc]: string read GetJavaScript;
        // Generated css
        property Css: string read GetCss;
    end;

implementation

constructor TJQBase.Create;
begin
    FClasse:='';
    FId:='';
    FContent:=TStringList.Create;
    FJsHeader:=TStringList.Create;
    FJsTop:=TStringList.Create;
    FJsBottom:=TStringList.Create;
    FCss:=TStringList.Create;
end;

destructor TJQBase.Destroy;
begin
    FContent.Free;
    FJsHeader.Free;
    FJsTop.Free;
    FJsBottom.Free;
    FCss.Free;
    inherited Destroy;
end;

end.

