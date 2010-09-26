unit VForthVariantArray;
{$IFDEF fpc}{$MODE delphi}{$ENDIF}
interface

uses
  VForth,
  SysUtils,
  Classes,
  Contnrs,
  VForthVariants;

type
  TArrayVariant = class(TCustomVForthVariant, IVForthVariant)
  private
    FItems: TInterfaceList;
  protected
    function GetVariantType: TVariantType; override; stdcall;
    function GetSize: Integer; override; stdcall;
    procedure SetSize(const Value: Integer); override; stdcall;
    function GetItems(const index: Integer): IVForthVariant; override; stdcall;
    procedure SetItems(const index: Integer; const Value: IVForthVariant);
      override; stdcall;
    function GetStringValue: string; virtual; stdcall;
  public
    constructor Create;
    destructor Destroy; override;
    function Convert(AVt: TVariantType): IVForthVariant; override; stdcall;
  end;

implementation

{ TArrayVariant }

function TArrayVariant.Convert(AVt: TVariantType): IVForthVariant;
var
  i: Integer;
  r: string;
begin
  case AVt of
    vtArray:
      begin

      end;
    else
      inherited;
  end;
end;

constructor TArrayVariant.Create;
begin
  inherited;
  FItems := TInterfaceList.Create;
end;

destructor TArrayVariant.Destroy;
begin
  if Assigned(FItems) then
    FItems.Free;
  inherited;
end;

function TArrayVariant.GetItems(const index: Integer): IVForthVariant;
begin
  Result := IVForthVariant(FItems[index]);
end;

function TArrayVariant.GetSize: Integer;
begin
  Result := FItems.Count;
end;

function TArrayVariant.GetStringValue: string;
begin
  Result := IntToStr(Size)
end;

function TArrayVariant.GetVariantType: TVariantType;
begin
  Result := vtArray;
end;

procedure TArrayVariant.SetItems(const index: Integer;
  const Value: IVForthVariant);
begin
  FItems[index] := Value;
end;

procedure TArrayVariant.SetSize(const Value: Integer);
begin
  if Value < 0 then
    raise EVForthVariantError.CreateFmt('Cant set size (%s)', [Value]);

  while FItems.Count <> Value do
  begin
    if FItems.Count < Value then
      FItems.Add(nil)
    else
      FItems.Delete(FItems.Count - 1);
  end;
end;

end.
