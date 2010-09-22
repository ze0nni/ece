unit VForthModuleMath;
{$IFDEF fpc}{$MODE delphi}{$ENDIF}
interface

uses
  VForthModule,
  SysUtils,
  Math,
  VForth;

type
  EVForthModuleMathException = class(Exception)

  end;

  TVForthModuleMath = class(TVForthModule, IVForthModule)
  private

  protected

  public
    procedure Register(AMachine: IVForthMachine); stdcall;
  end;

implementation

uses
  VForthAthom,
  VForthVariants;

const
  AngleMode = 'AngleMode';
  AngleModeDeg = 'deg';
  AngleModeRad = 'rad';
  AngleModeGrad = 'grad';

  { TVForthModuleMath }

  // копирует n-й эоемент стека в вершину
procedure VfAdd(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
var
  v0, v1: IVForthVariant;
begin
  v0 := AMachine.Pop;
  v1 := AMachine.Pop;
  case GetPriorityVariantType(v1.VariantType, v0.VariantType) of
    vtInteger:
      AMachine.PushInt(v1.IntValue + v0.IntValue);
    vtFloat:
      AMachine.PushFloat(v1.FloatValue + v0.FloatValue);
    vtString:
      AMachine.PushString(v1.StringValue + v0.StringValue);
  else
    raise EVForthModuleMathException.Create('Bad types.');
  end;
end;

procedure VfAddS(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
var
  v0, v1: IVForthVariant;
begin
  v0 := AMachine.Pop;
  v1 := AMachine.Pop;
  AMachine.PushString(v1.StringValue + v0.StringValue);
end;

procedure VfSign(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
var
  v0: IVForthVariant;
begin
  v0 := AMachine.Pop;
  AMachine.PushFloat(sign(v0.FloatValue));
end;

procedure VfSub(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
var
  v0, v1: IVForthVariant;
begin
  v0 := AMachine.Pop;
  v1 := AMachine.Pop;
  case GetPriorityVariantType(v1.VariantType, v0.VariantType) of
    vtInteger:
      AMachine.PushInt(v1.IntValue - v0.IntValue);
    vtFloat:
      AMachine.PushFloat(v1.FloatValue - v0.FloatValue);
  else
    raise EVForthModuleMathException.Create('Bad types.');
  end;
end;

procedure VfMult(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
var
  v0, v1: IVForthVariant;
begin
  v0 := AMachine.Pop;
  v1 := AMachine.Pop;
  case GetPriorityVariantType(v1.VariantType, v0.VariantType) of
    vtInteger:
      AMachine.PushInt(v1.IntValue * v0.IntValue);
    vtFloat:
      AMachine.PushFloat(v1.FloatValue * v0.FloatValue);
  else
    raise EVForthModuleMathException.Create('Bad types.');
  end;
end;

procedure VfDivF(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
var
  v0, v1: IVForthVariant;
begin
  v0 := AMachine.Pop;
  v1 := AMachine.Pop;
  AMachine.PushFloat(v1.FloatValue / v0.FloatValue);
end;

procedure VfDiv(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
var
  v0, v1: IVForthVariant;
begin
  v0 := AMachine.Pop;
  v1 := AMachine.Pop;
  AMachine.PushInt(v1.IntValue mod v0.IntValue);
end;

procedure VfMod(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
var
  v0, v1: IVForthVariant;
begin
  v0 := AMachine.Pop;
  v1 := AMachine.Pop;
  AMachine.PushInt(v1.IntValue mod v0.IntValue);
end;

procedure VfDivMod(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
var
  v0, v1: IVForthVariant;
begin
  v0 := AMachine.Pop;
  v1 := AMachine.Pop;
  AMachine.PushInt(v1.IntValue div v0.IntValue);
  AMachine.PushInt(v1.IntValue mod v0.IntValue);
end;

procedure VfAbs(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
var
  v0: IVForthVariant;
begin
  v0 := AMachine.Pop;
  case v0.VariantType of
    vtInteger:
      AMachine.PushInt(abs(v0.IntValue));
    vtFloat:
      AMachine.PushFloat(abs(v0.FloatValue));
  else
    raise EVForthModuleMathException.Create('Bad types.');
  end;
end;

procedure VfNegate(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
var
  v0: IVForthVariant;
begin
  v0 := AMachine.Pop;
  case v0.VariantType of
    vtInteger:
      AMachine.PushInt(-v0.IntValue);
    vtFloat:
      AMachine.PushFloat(-v0.FloatValue);
  else
    raise EVForthModuleMathException.Create('Bad types.');
  end;
end;

procedure VfPower(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
var
  v0, v1: IVForthVariant;
begin
  v0 := AMachine.Pop;
  v1 := AMachine.Pop;
  AMachine.PushFloat(power(v1.FloatValue, v0.FloatValue));
end;

procedure VfLn(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
var
  v0: IVForthVariant;
begin
  v0 := AMachine.Pop;
  AMachine.PushFloat(Ln(v0.FloatValue));
end;

procedure VfExp(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
var
  v0: IVForthVariant;
begin
  v0 := AMachine.Pop;
  AMachine.PushFloat(Exp(v0.FloatValue));
end;

procedure VfFrac(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
var
  v0: IVForthVariant;
begin
  v0 := AMachine.Pop;
  AMachine.PushFloat(Frac(v0.FloatValue));
end;

// Тригонометрические
{ TODO -oOnni -cGeneral : Предпочтительные единицы измерения угла Deg\Rad\Grad }

function ConvertAngle(Value: Double; AModule: IVForthModule): Double;
var
  am: String;
begin ;
  am := AModule.GetProp(AngleMode);

  if am = AngleModeDeg then
    Result := DegToRad(Value)
  else if am = AngleModeGrad then
    Result := GradToRad(Value)
  else
    Result := Value;
end;

procedure VfDegMode(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
begin
  AAthom.Module.SetProp(AngleMode, AngleModeDeg);
end;

procedure VfRadMode(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
begin
  AAthom.Module.SetProp(AngleMode, AngleModeRad);
end;

procedure VfGradMode(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
begin
  AAthom.Module.SetProp(AngleMode, AngleModeGrad);
end;

procedure VfAngleMode(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
begin
  AMachine.PushString(AAthom.Module.GetProp(AngleMode));
end;

procedure VfSin(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
var
  v0: IVForthVariant;
begin
  v0 := AMachine.Pop;
  AMachine.PushFloat(Sin(ConvertAngle(v0.FloatValue, AAthom.Module)));
end;

procedure VfCos(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
var
  v0: IVForthVariant;
begin
  v0 := AMachine.Pop;
  AMachine.PushFloat(Cos(ConvertAngle(v0.FloatValue, AAthom.Module)));
end;

procedure VfTan(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
var
  v0: IVForthVariant;
begin
  v0 := AMachine.Pop;
  AMachine.PushFloat(Tan(ConvertAngle(v0.FloatValue, AAthom.Module)));
end;

procedure VfCoTan(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
var
  v0: IVForthVariant;
begin
  v0 := AMachine.Pop;
  AMachine.PushFloat(Cotan(ConvertAngle(v0.FloatValue, AAthom.Module)));
end;

procedure VfArcSin(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
var
  v0: IVForthVariant;
begin
  v0 := AMachine.Pop;
  AMachine.PushFloat(ArcSin(ConvertAngle(v0.FloatValue, AAthom.Module)));
end;

procedure VfArcCos(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
var
  v0: IVForthVariant;
begin
  v0 := AMachine.Pop;
  AMachine.PushFloat(ArcCos(ConvertAngle(v0.FloatValue, AAthom.Module)));
end;

procedure VfArcTan(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
var
  v0: IVForthVariant;
begin
  v0 := AMachine.Pop;
  AMachine.PushFloat(ArcTan(ConvertAngle(v0.FloatValue, AAthom.Module)));
end;

procedure VfArcTanh(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
var
  v0: IVForthVariant;
begin
  v0 := AMachine.Pop;
  AMachine.PushFloat(ArcTanh(ConvertAngle(v0.FloatValue, AAthom.Module)));
end;

procedure VfArcTan2(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
var
  v0, v1: IVForthVariant;
begin
  v0 := AMachine.Pop;
  v1 := AMachine.Pop;
  { TODO -oOnni -cGeneral : Разобрать что это есть }
  AMachine.PushFloat(ArcTan2(ConvertAngle(v1.FloatValue, AAthom.Module),
      ConvertAngle(v0.FloatValue, AAthom.Module)));
end;

procedure VfRandom(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
begin
  AMachine.PushFloat(Random);
end;

procedure VfRandSeed(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
begin
  RandSeed := AMachine.PopInt;
end;

procedure VfGetRandSeed(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
begin
  AMachine.PushInt(RandSeed);
end;

procedure VfRandomize(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
begin
  Randomize;
end;



procedure TVForthModuleMath.Register(AMachine: IVForthMachine);
begin
  // Мера угла - градусы
  SetProp(AngleMode, AngleModeDeg);

  AMachine.AddAthom(CreateVForthSystemAthom('+', self, VfAdd));
  AMachine.AddAthom(CreateVForthSystemAthom('&', self, VfAddS));
  AMachine.AddAthom(CreateVForthSystemAthom('-', self, VfSub));
  AMachine.AddAthom(CreateVForthSystemAthom('*', self, VfMult));
  AMachine.AddAthom(CreateVForthSystemAthom('/', self, VfDivF));
  AMachine.AddAthom(CreateVForthSystemAthom('div', self, VfDiv));
  AMachine.AddAthom(CreateVForthSystemAthom('mod', self, VfMod));
  AMachine.AddAthom(CreateVForthSystemAthom('divmod', self, VfDivMod));

  AMachine.AddAthom(CreateVForthSystemAthom('abs', self, VfAbs));
  AMachine.AddAthom(CreateVForthSystemAthom('sign', self, VfSign));
  AMachine.AddAthom(CreateVForthSystemAthom('negate', self, VfNegate));

  AMachine.AddAthom(CreateVForthSystemAthom('power', self, VfPower));
  AMachine.AddAthom(CreateVForthSystemAthom('ln', self, VfLn));
  AMachine.AddAthom(CreateVForthSystemAthom('exp', self, VfExp));
  AMachine.AddAthom(CreateVForthSystemAthom('frac', self, VfExp));

  AMachine.AddAthom(CreateVForthSystemAthom('DegMode', self, VfDegMode));
  AMachine.AddAthom(CreateVForthSystemAthom('RadMode', self, VfRadMode));
  AMachine.AddAthom(CreateVForthSystemAthom('GradMode', self, VfGradMode));
  AMachine.AddAthom(CreateVForthSystemAthom('AngleMode', self, VfAngleMode));

  AMachine.AddAthom(CreateVForthSystemAthom('sin', self, VfSin));
  AMachine.AddAthom(CreateVForthSystemAthom('cos', self, VfCos));
  AMachine.AddAthom(CreateVForthSystemAthom('tan', self, VfTan));
  AMachine.AddAthom(CreateVForthSystemAthom('conat', self, VfCoTan));
  AMachine.AddAthom(CreateVForthSystemAthom('arcsin', self, VfArcSin));
  AMachine.AddAthom(CreateVForthSystemAthom('arccos', self, VfArcCos));
  AMachine.AddAthom(CreateVForthSystemAthom('arctan', self, VfArcTan));
  AMachine.AddAthom(CreateVForthSystemAthom('arctanh', self, VfArcTanh));
  AMachine.AddAthom(CreateVForthSystemAthom('arctan2', self, VfArcTan2));

  AMachine.AddAthom(CreateVForthSystemAthom('random', self, VfRandom));
  AMachine.AddAthom(CreateVForthSystemAthom('randomize', self, VfRandomize));
  AMachine.AddAthom(CreateVForthSystemAthom('randseed', self, VfRandSeed));
  AMachine.AddAthom(CreateVForthSystemAthom('randseed@', self, VfGetRandSeed));
end;

end.
