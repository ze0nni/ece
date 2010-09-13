unit VForthModule;

interface

uses
  ComObj,
  Classes;

type
  TVForthModule = class(TInterfacedObject)
  private
    FProps : TStringList;
  protected

  public
    constructor Create;
    destructor Destroy; override;
    procedure SetProp(AProp, AValue : string); stdcall;
    function GetProp(AProp : string) : string; stdcall;
  end;

implementation

{ TVForthModule }

constructor TVForthModule.Create;
begin
  inherited;
  FProps := TStringList.Create;
end;

destructor TVForthModule.Destroy;
begin
  if Assigned(FProps) then
    FProps.Free;
  inherited;
end;

function TVForthModule.GetProp(AProp: string): string;
var
  index: Integer;
begin
  index := FProps.IndexOfName(AProp);
  if index = -1 then
    exit('')
  else
  Result := FProps.ValueFromIndex[index];
end;

procedure TVForthModule.SetProp(AProp, AValue: string);
var
  index: Integer;
begin
  index := FProps.IndexOfName(AProp);
  if index =-1 then
    FProps.Add(AProp + '=' + AValue)
  else
    FProps.ValueFromIndex[index] := AValue;
end;

end.
