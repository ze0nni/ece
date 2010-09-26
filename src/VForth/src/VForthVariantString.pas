unit VForthVariantString;
{$IFDEF fpc}{$MODE delphi}{$ENDIF}
interface

uses
  VForth,
  SysUtils,
  Classes,
  Contnrs,
  VForthVariants;

type
  TStringVariant = class(TCustomVForthVariant, IVForthVariant)
  private
    FValue : string;
  protected
    function GetVariantType: TVariantType; override; stdcall;
    procedure SetFloatValue(const Value: Double); override; stdcall;
    procedure SetIntValue(const Value: Integer); override; stdcall;
    procedure SetStringValue(const Value: string); override; stdcall;
    function GetFloatValue: Double; override; stdcall;
    function GetIntValue: Integer; override; stdcall;
    function GetStringValue: string; override; stdcall;
  public
    function Convert(AVt: TVariantType): IVForthVariant; override; stdcall;
  end;

implementation

{ TStringVariant }

function TStringVariant.Convert(AVt: TVariantType): IVForthVariant;
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
  end;
end;

function TStringVariant.GetFloatValue: Double;
begin
  Result := StrToFloat(FValue)
end;

function TStringVariant.GetIntValue: Integer;
begin
  Result := StrToInt(FValue)
end;

function TStringVariant.GetStringValue: string;
begin
  Result := FValue;
end;

function TStringVariant.GetVariantType: TVariantType;
begin
  Result := vtString;
end;

procedure TStringVariant.SetFloatValue(const Value: Double);
begin
  FValue := FloatToStr(Value)
end;

procedure TStringVariant.SetIntValue(const Value: Integer);
begin
  FValue := IntToStr(Value)
end;

procedure TStringVariant.SetStringValue(const Value: string);
begin
  FValue := Value;
end;

end.
