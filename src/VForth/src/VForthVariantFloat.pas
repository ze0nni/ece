unit VForthVariantFloat;

interface

uses
  VForth,
  SysUtils,
  Classes,
  Contnrs,
  VForthVariants;

type
  TFloatVariant = class(TCustomVForthVariant, IVForthVariant)
  private
    FValue : Double;
  protected
    function GetVariantType: TVariantType; override; stdcall;
    procedure SetFloatValue(const Value: Double); override; stdcall;
    procedure SetIntValue(const Value: Integer); override; stdcall;
    procedure SetStringValue(const Value: string); override; stdcall;
    function GetFloatValue: Double; override; stdcall;
    function GetIntValue: Integer; override; stdcall;
    function GetStringValue: string; override; stdcall;
  public
    function Convert(AVt: TVariantType) : IVForthVariant; override; stdcall;
  end;

implementation

{ TFloatVariant }

function TFloatVariant.Convert(AVt: TVariantType): IVForthVariant;
begin
  case AVt of
    vtInteger:
      Result := CreateIntegerVariant(IntValue);
    vtFloat:
      Result := CreateFloatVariant(FloatValue);
    {TODO -oOnni -cGeneral : vtNatural}
    {TODO -oOnni -cGeneral : vtComplex}
    vtString:
      Result := CreateStringVariant(StringValue);
    else
      inherited;
  end;
end;

function TFloatVariant.GetFloatValue: Double;
begin
  Result := FValue;
end;

function TFloatVariant.GetIntValue: Integer;
begin
  Result := trunc(FValue);
end;

function TFloatVariant.GetStringValue: string;
begin
  Result := FloatToStr(FValue)
end;

function TFloatVariant.GetVariantType: TVariantType;
begin
  Result := vtFloat;
end;

procedure TFloatVariant.SetFloatValue(const Value: Double);
begin
  FValue := Value;
end;

procedure TFloatVariant.SetIntValue(const Value: Integer);
begin
  FValue := Value;
end;

procedure TFloatVariant.SetStringValue(const Value: string);
begin
  inherited;
  FValue := StrToFloat(Value);
end;

end.
