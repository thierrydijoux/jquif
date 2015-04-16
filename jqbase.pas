{ @abstract(Base class for all jquery ui objects)
  @author(Thierry DIJOUX <tjr.dijoux@gmail.com>)
  base class for JQuery ui object }
unit JQBase;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type
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
        FJs: TStrings;
        // Css content
        FCss: TStrings;
        // Return the generated html
        function GetContent: string; virtual; abstract;
        // Return the generated javascript
        function GetJs: string; virtual; abstract;
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
        property JavaScript: string read GetJs;
        // Generated css
        property Css: string read GetCss;
    end;

implementation

constructor TJQBase.Create;
begin
    FClasse := '';
    FId := '';
    FContent := TStringList.Create;
    FJs := TStringList.Create;
    FCss := TStringList.Create;
end;

destructor TJQBase.Destroy;
begin
    FContent.Free;
    FJs.Free;
    FCss.Free;
    inherited Destroy;
end;

end.

