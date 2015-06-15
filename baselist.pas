unit BaseList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

Type

  TBaseList = class
  private
    function GetCount: integer; virtual;
  protected
    FObjectList: TObjectList;
    function GetItems(i: integer): TObject; virtual;
    procedure SetItems(i: integer; const AValue: TObject); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(AObject: TObject): integer;
    property Count: integer read GetCount;
    property Items[i:integer]: TObject read GetItems write SetItems; default;
  end;

implementation

constructor TBaseList.Create;
begin
    FObjectList:=TObjectList.create(true);
end;

destructor TBaseList.Destroy;
begin
    FObjectList.Free;
    inherited Destroy;
end;

procedure TBaseList.Clear;
begin
    Clear;
end;

function TBaseList.Add(AObject: TObject): integer;
begin
    result:=FObjectList.Add(AObject);
end;

function TBaseList.GetCount: integer;
begin
    result:=FObjectList.Count;
end;

function TBaseList.GetItems(i: integer): TObject;
begin
    result:=FObjectList.Items[i];
end;

procedure TBaseList.SetItems(i: integer; const AValue: TObject);
begin
    FObjectList.Items[i]:=AValue;
end;

end.



