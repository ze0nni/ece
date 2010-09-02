unit ifopMatch;

interface


procedure RegisterDictionary(AKernel : TObject);

implementation

uses
  SysUtils,
  Math,
  IfopKernel,
  IfopVariant;


procedure ifopAdd(Kernel: TIfopKernel);
var
  v1, v2 : TIfopVariant;
begin
  v1 := Kernel.Pop;
  v2 := Kernel.Pop;
  case v2.VariantType of
    vtInteger:
      v2.IntValue := v2.IntValue + v1.IntValue;
    vbPointer:
      {TODO -oOnni -cGeneral : Float math};
    vtFloat:
      v2.FloatValue := v2.FloatValue + v1.FloatValue;
    vtString:
      v2.StrValue := v2.StrValue + v1.StrValue;
    else
      raise Exception.Create('Bad types for add');
  end;
  Kernel.Push(v2);
  v1.Free;
end;

procedure ifopSub(Kernel: TIfopKernel);
var
  v1, v2 : TIfopVariant;
begin
  v1 := Kernel.Pop;
  v2 := Kernel.Pop;
  case v2.VariantType of
    vtInteger:
      v2.IntValue := v2.IntValue - v1.IntValue;
    vbPointer:
      {TODO -oOnni -cGeneral : Float math};
    vtFloat:
      v2.FloatValue := v2.FloatValue - v1.FloatValue;
    else
      raise Exception.Create('Bad types for sub');
  end;
  Kernel.Push(v2);
  v1.Free;
end;

procedure ifopMult(Kernel: TIfopKernel);
var
  v1, v2 : TIfopVariant;
begin
  v1 := Kernel.Pop;
  v2 := Kernel.Pop;
  case v2.VariantType of
    vtInteger:
      v2.IntValue := v2.IntValue * v1.IntValue;
    vbPointer:
      {TODO -oOnni -cGeneral : Float math};
    vtFloat:
      v2.FloatValue := v2.FloatValue * v1.FloatValue;
    else
      raise Exception.Create('Bad types for mult');
  end;
  Kernel.Push(v2);
  v1.Free;
end;

procedure ifopDiv(Kernel: TIfopKernel);
var
  v1, v2 : TIfopVariant;
begin
  v1 := Kernel.Pop;
  v2 := Kernel.Pop;
  case v2.VariantType of
    vtInteger:
      v2.IntValue := v2.IntValue div v1.IntValue;
    vbPointer:
      {TODO -oOnni -cGeneral : Float math};
    vtFloat:
      v2.FloatValue := v2.FloatValue / v1.FloatValue;
    else
      raise Exception.Create('Bad types for div');
  end;
  Kernel.Push(v2);
  v1.Free;
end;

procedure ifopMod(Kernel: TIfopKernel);
var
  v1, v2 : TIfopVariant;
begin
  v1 := Kernel.Pop;
  v2 := Kernel.Pop;
  case v2.VariantType of
    vtInteger:
      v2.IntValue := v2.IntValue mod v1.IntValue;
    else
      raise Exception.Create('Bad types for mod');
  end;
  Kernel.Push(v2);
  v1.Free;
end;

procedure ifopDivMod(Kernel: TIfopKernel);
var
  v1, v2 : TIfopVariant;
  m, d : Integer;
begin
  v1 := Kernel.Pop;
  v2 := Kernel.Pop;
  case v2.VariantType of
    vtInteger:
    begin
      m := v2.IntValue mod v1.IntValue;
      d := v2.IntValue div v1.IntValue;
      v2.IntValue := m;
      v1.IntValue := d;
    end
    else
      raise Exception.Create('Bad types for /mod');
  end;
  Kernel.Push(v2);//Снизу остаток
  Kernel.Push(v1);//Сверху результат
end;

procedure ifopAbs(Kernel: TIfopKernel);
var
  v1: TIfopVariant;
begin
  v1 := Kernel.Pop;
  case v1.VariantType of
    vtInteger:
      v1.IntValue := abs(v1.IntValue);
    vbPointer:
      {TODO -oOnni -cGeneral : Float math};
    vtFloat:
      v1.FloatValue := abs(v1.FloatValue);
    else
      raise Exception.Create('Bad types for abs');
  end;
  Kernel.Push(v1);
end;

procedure ifopNegate(Kernel: TIfopKernel);
var
  v1: TIfopVariant;
begin
  v1 := Kernel.Pop;
  case v1.VariantType of
    vtInteger:
      v1.IntValue := -(v1.IntValue);
    vbPointer:
      {TODO -oOnni -cGeneral : Float math};
    vtFloat:
      v1.FloatValue := -(v1.FloatValue);
    else
      raise Exception.Create('Bad types for negate');
  end;
  Kernel.Push(v1);
end;

procedure ifopShr(Kernel: TIfopKernel);
var
  v1: TIfopVariant;
begin
  v1 := Kernel.Pop;
  case v1.VariantType of
    vtInteger:
      v1.IntValue := (v1.IntValue shr 1);
    else
      raise Exception.Create('Bad types for shr');
  end;
  Kernel.Push(v1);
end;

procedure ifopShl(Kernel: TIfopKernel);
var
  v1: TIfopVariant;
begin
  v1 := Kernel.Pop;
  case v1.VariantType of
    vtInteger:
      v1.IntValue := (v1.IntValue shl 1);
    else
      raise Exception.Create('Bad types for shl');
  end;
  Kernel.Push(v1);
end;

procedure ifopEqu(Kernel : TIfopKernel);
var
  v1: TIfopVariant;
  v2: TIfopVariant;
  vr: TIfopVariant;
begin
  v1 := Kernel.Pop;
  v2 := Kernel.Pop;
  vr := TifopIntegerVariant.Create;
  case v2.VariantType of
    vtInteger: vr.IntValue := integer(v1.IntValue = v2.IntValue) ;
    vbPointer: {TODO -oOwner -cGeneral : ActionItem};
    vtFloat: vr.IntValue := integer(v1.FloatValue = v2.FloatValue);
    vtString: vr.IntValue := integer(v1.StrValue = v2.StrValue);
    vtObject: {TODO -oOwner -cGeneral : ActionItem};
    vtInterface: {TODO -oOwner -cGeneral : ActionItem};
    else
      raise Exception.Create('Bad types for Equ');
  end;
  Kernel.Push(vr);
  v1.Free;
  v2.Free;
end;

procedure ifopLo(Kernel : TIfopKernel);
var
  v1: TIfopVariant;
  v2: TIfopVariant;
  vr: TIfopVariant;
begin
  v1 := Kernel.Pop;
  v2 := Kernel.Pop;
  vr := TifopIntegerVariant.Create;
  case v2.VariantType of
    vtInteger: vr.IntValue := integer(v1.IntValue > v2.IntValue);
    vbPointer: {TODO -oOwner -cGeneral : ActionItem};
    vtFloat: vr.IntValue := integer(v1.FloatValue > v2.FloatValue);
    vtString: vr.IntValue := integer(v1.StrValue > v2.StrValue);
    else
      raise Exception.Create('Bad types for Equ');
  end;
  Kernel.Push(vr);
  v1.Free;
  v2.Free;
end;

procedure ifopHi(Kernel : TIfopKernel);
var
  v1: TIfopVariant;
  v2: TIfopVariant;
  vr: TIfopVariant;
begin
  v1 := Kernel.Pop;
  v2 := Kernel.Pop;
  vr := TifopIntegerVariant.Create;
  case v2.VariantType of
    vtInteger: vr.IntValue := integer(v1.IntValue < v2.IntValue);
    vbPointer: {TODO -oOwner -cGeneral : ActionItem};
    vtFloat: vr.IntValue := integer(v1.FloatValue < v2.FloatValue);
    vtString: vr.IntValue := integer(v1.StrValue < v2.StrValue);
    else
      raise Exception.Create('Bad types for Equ');
  end;
  Kernel.Push(vr);
  v1.Free;
  v2.Free;
end;

procedure ifopLoE(Kernel : TIfopKernel);
var
  v1: TIfopVariant;
  v2: TIfopVariant;
  vr: TIfopVariant;
begin
  v1 := Kernel.Pop;
  v2 := Kernel.Pop;
  vr := TifopIntegerVariant.Create;
  case v2.VariantType of
    vtInteger: vr.IntValue := integer(v1.IntValue >= v2.IntValue);
    vbPointer: {TODO -oOwner -cGeneral : ActionItem};
    vtFloat: vr.IntValue := integer(v1.FloatValue >= v2.FloatValue);
    vtString: vr.IntValue := integer(v1.StrValue >= v2.StrValue);
    else
      raise Exception.Create('Bad types for Equ');
  end;
  Kernel.Push(vr);
  v1.Free;
  v2.Free;
end;

procedure ifopHiE(Kernel : TIfopKernel);
var
  v1: TIfopVariant;
  v2: TIfopVariant;
  vr: TIfopVariant;
begin
  v1 := Kernel.Pop;
  v2 := Kernel.Pop;
  vr := TifopIntegerVariant.Create;
  case v2.VariantType of
    vtInteger: vr.IntValue := integer(v1.IntValue <= v2.IntValue);
    vbPointer: {TODO -oOwner -cGeneral : ActionItem};
    vtFloat: vr.IntValue := integer(v1.FloatValue <= v2.FloatValue);
    vtString: vr.IntValue := integer(v1.StrValue <= v2.StrValue);
    else
      raise Exception.Create('Bad types for Equ');
  end;
  Kernel.Push(vr);
  v1.Free;
  v2.Free;
end;

////////////////////////////////////////////////////////////////////////////////

procedure ifopAnd(Kernel : TIfopKernel);
begin
  Kernel.PushInt(Kernel.PopInt and Kernel.PopInt);
end;

procedure ifopOr(Kernel : TIfopKernel);
begin
  Kernel.PushInt(Kernel.PopInt or Kernel.PopInt);
end;

procedure ifopNot(Kernel : TIfopKernel);
begin
  Kernel.PushInt(not Kernel.PopInt);
end;

procedure ifopXor(Kernel : TIfopKernel);
begin
  Kernel.PushInt(Kernel.PopInt xor Kernel.PopInt);
end;

////////////////////////////////////////////////////////////////////////////////

procedure ifopGetAngleMode(Kernel : TIfopKernel);
begin
  case Kernel.AngleMode of
    amDeg: Kernel.PushStr('deg');
    amRad: Kernel.PushStr('rad');
    amGrad: Kernel.PushStr('grad');
  end;
end;

procedure ifopDegMod(Kernel : TIfopKernel);
begin
  Kernel.AngleMode := amDeg;
end;

procedure ifopRadMod(Kernel : TIfopKernel);
begin
  Kernel.AngleMode := amRad;
end;

procedure ifopGradMod(Kernel : TIfopKernel);
begin
  Kernel.AngleMode := amGrad;
end;

procedure ifopSin(Kernel : TIfopKernel);
begin
  Kernel.PushFloat(Sin(Kernel.FloatToAngle(Kernel.PopFloat)));
end;

procedure ifopCos(Kernel : TIfopKernel);
begin
  Kernel.PushFloat(Cos(Kernel.FloatToAngle(Kernel.PopFloat)));
end;

procedure ifopTg(Kernel : TIfopKernel);
begin
  Kernel.PushFloat(Tan(Kernel.FloatToAngle(Kernel.PopFloat)));
end;

procedure ifopCTg(Kernel : TIfopKernel);
begin
  Kernel.PushFloat(ArcTan(Kernel.FloatToAngle(Kernel.PopFloat)));
end;

procedure ifopDegToRad(Kernel : TIfopKernel);
begin
  Kernel.PushFloat(DegToRad(Kernel.PopFloat));
end;

procedure ifopRadToDeg(Kernel : TIfopKernel);
begin
  Kernel.PushFloat(RadToDeg(Kernel.PopFloat));
end;

procedure ifopPi(Kernel : TIfopKernel);
begin
  Kernel.PushFloat(Pi);
end;

procedure ifopSqr(Kernel : TIfopKernel);
begin
  Kernel.PushFloat(Sqr(Kernel.PopFloat));
end;

procedure ifopSqrt(Kernel : TIfopKernel);
begin
  Kernel.PushFloat(Sqrt(Kernel.PopFloat));
end;

procedure ifopPower(Kernel : TIfopKernel);
var
  v1, v2 : Double;
begin
  v1 := Kernel.PopFloat;
  v2 := Kernel.PopFloat;
  Kernel.PushFloat(Power(v2, v1));
end;

procedure ifopLogn(Kernel : TIfopKernel);
var
  v1, v2 : Double;
begin
  v1 := Kernel.PopFloat;
  v2 := Kernel.PopFloat;
  Kernel.PushFloat(logn(v2, v1));
end;

procedure ifopRnd(Kernel : TIfopKernel);
begin
  Kernel.PushFloat(random);
end;

procedure ifopRandomize(Kernel : TIfopKernel);
begin
  Randomize;
end;

procedure ifopRndSeed(Kernel : TIfopKernel);
begin
  RandSeed := Kernel.PopInt;
end;

procedure RegisterDictionary(AKernel : TObject);
var
  Kernel : TIfopKernel;
begin
  Kernel := TIfopKernel(AKernel);
  //Регистрация функций
  Kernel.AddKeyword('+', @ifopAdd);
  Kernel.AddKeyword('-', @ifopSub);
  Kernel.AddKeyword('*', @ifopMult);
  Kernel.AddKeyword('/', @ifopDiv);
  Kernel.AddKeyword('mod', @ifopMod);
  Kernel.AddKeyword('/mod', @ifopDivMod);
  Kernel.AddKeyword('abs', @ifopAbs);
  Kernel.AddKeyword('negate', @ifopNegate);
  Kernel.AddKeyword('shr', @ifopShr);
  Kernel.AddKeyword('shl', @ifopShl);

  Kernel.AddKeyword('=', @ifopEqu);
  Kernel.AddKeyword('>', @ifopHi);
  Kernel.AddKeyword('<', @ifopLo);
  Kernel.AddKeyword('>=', @ifopHiE);
  Kernel.AddKeyword('<=', @ifopLoE);

  Kernel.AddKeyword('and', @ifopAnd);
  Kernel.AddKeyword('or', @ifopOr);
  Kernel.AddKeyword('not', @ifopNot);
  Kernel.AddKeyword('xor', @ifopXor);

  Kernel.AddKeyword('AngleMode', @ifopGetAngleMode);
  Kernel.AddKeyword('DegMode', @ifopDegMod);
  Kernel.AddKeyword('RadMode', @ifopRadMod);
  Kernel.AddKeyword('GradMode', @ifopGradMod);

  Kernel.AddKeyword('sin', @ifopSin);
  Kernel.AddKeyword('cos', @ifopCos);
  Kernel.AddKeyword('tan', @ifopTg);
  Kernel.AddKeyword('arctan', @ifopCtg);

  Kernel.AddKeyword('rad2deg', @ifopRadToDeg);
  Kernel.AddKeyword('deg2rad', @ifopDegToRad);

  Kernel.AddKeyword('pi', @ifopPi);

  Kernel.AddKeyword('sqr', @ifopSqr);
  Kernel.AddKeyword('sqrt', @ifopSqrt);
  Kernel.AddKeyword('power', @ifopPower);
  Kernel.AddKeyword('logn', @ifopLogn);

  Kernel.AddKeyword('rnd', @ifopRnd);
  Kernel.AddKeyword('randomize', @ifopRandomize);
  Kernel.AddKeyword('RndSeed', @ifopRndSeed);
end;

end.
