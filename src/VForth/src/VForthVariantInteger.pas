unit VForthVariantInteger;
{$IFDEF fpc}{$MODE delphi}{$ENDIF}
interface

uses
  VForth,
  SysUtils,
  Classes,
  Contnrs,
  VForthVariants;

type
  TIntegerVariant = class(TCustomVForthVariant, IVForthVariant)
  private
    FValue : Integer;
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

{ TIntegerVariant }

function TIntegerVariant.Convert(AVt: TVariantType): IVForthVariant;
begin
  case AVt of
    vtInteger:
      Result := CreateIntegerVariant(IntValue);
    vtFloat:
      Result := CreateFloatVariant(FloatValue);
    vtNatural:
      Result := CreateNaturalVariant(IntValue, 1);
    {TODO -oOnni -cGeneral : vtComplex}
    vtString :
      Result := CreateStringVariant(StringValue);
    else
      inherited;
  end;
end;

function TIntegerVariant.GetFloatValue: Double;
begin
  Result := FValue;
end;

function TIntegerVariant.GetIntValue: Integer;
begin
  Result := FValue;
end;

function TIntegerVariant.GetStringValue: string;
begin
  Result := IntToStr(FValue);
end;

function TIntegerVariant.GetVariantType: TVariantType;
begin
  Result := vtInteger;
end;

procedure TIntegerVariant.SetFloatValue(const Value: Double);
begin
  FValue := Trunc(Value);
end;

procedure TIntegerVariant.SetIntValue(const Value: Integer);
begin
  FValue := Value;
end;

procedure TIntegerVariant.SetStringValue(const Value: string);
begin
  inherited;
  FValue := StrToInt(Value);
end;

end.
