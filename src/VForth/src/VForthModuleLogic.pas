unit VForthModuleLogic;

interface

uses
  Windows,
  SysUtils,
  VForthModule,
  VForth;

type
  EVForthModuleLogicError = class(Exception)

  end;

  TVForthModuleLogic = class(TVForthModule, IVForthModule)
  private

  protected

  public
    procedure Register(AMachine: IVForthMachine); stdcall;
  end;

implementation

uses
  VForthAthom,
  VForthVariants;

{ TVForthModuleLogic }

// Выхоидт из вложенных конструкций
procedure SeekForExit(AMachine: IVForthMachine);
var
  Level: Integer;
  tkIndex: Integer;
  tkCount: Integer;
  TkLine: String;
  TkLen: Integer;
begin
  // Если попадем на else до бежим пока не встретим then
  Level := 1;
  tkIndex := AMachine.CourientTkIndex;
  tkCount := AMachine.tkCount;
  //
  repeat
    inc(tkIndex);
    TkLine := AMachine.GetTk(tkIndex);
    TkLen := Length(TkLine);
    if TkLen = 2 then
    // Если это слово из 2 букв
    begin
      if CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, Pchar(TkLine),
        TkLen, 'if', 2) = CSTR_EQUAL then
      begin
        inc(Level)
      end
      else if CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, Pchar(TkLine)
          , TkLen, 'do', 2) = CSTR_EQUAL then
      begin
        inc(Level)
      end
    end
    else if TkLen = 4 then
    begin
      if CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, Pchar(TkLine),
        TkLen, 'then', 4) = CSTR_EQUAL then
      begin
        dec(Level)
      end
      else if CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, Pchar(TkLine)
          , TkLen, 'loop', 4) = CSTR_EQUAL then
      begin
        dec(Level)
      end;
    end
    else if TkLen = 5 then
    begin
      begin
        if CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, Pchar(TkLine),
          TkLen, 'begin', 5) = CSTR_EQUAL then
        begin
          inc(Level)
        end
        else if CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, Pchar
            (TkLine), TkLen, 'until', 5) = CSTR_EQUAL then
        begin
          dec(Level)
        end;
      end;
    end;
  until (Level = 0) or (tkIndex >= tkCount);
  AMachine.CourientTkIndex := tkIndex;
end;

procedure VfDo(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  v0, v1: IVForthVariant;
  s: TForthStack;
begin
  try
    { TODO -oOnni -cGeneral : Отдельный стек для счетчиков }
    v0 := AMachine.Pop;
    v1 := AMachine.Pop;
    // Переходим на системный стек
    s := AMachine.Stack;
    AMachine.Stack := fsSystem;
    AMachine.Push(v1);
    AMachine.Push(v0);
//    if v1.IntValue = v0.IntValue then
//    begin
//      SeekForExit(AMachine);
//    end;
    // AMachine.Push(AMachine.DataStack[1].Convert(vtInteger));
    AMachine.PushAddr(AMachine.CourientTkIndex);
  finally
    // Возвращаемя на старый стек
    AMachine.Stack := s;
  end;
end;

procedure VfLoop(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  v0, v1: IVForthVariant;
  v1v: Integer;
  s: TForthStack;
begin
  try
    s := AMachine.Stack;
    AMachine.Stack := fsSystem;

    v0 := AMachine.DataStack[0];
    v1 := AMachine.DataStack[1];
    v1v := v1.IntValue;
    if v1v > v0.IntValue then
    begin
      v1.IntValue := v1v - 1;
      // AMachine.PushInt(v1v - 1);
      AMachine.CourientTkIndex := AMachine.ReturnAddr;
    end
    else if v1v < v0.IntValue then
    begin
      v1.IntValue := v1v + 1;
      // AMachine.PushInt(v1v + 1);
      AMachine.CourientTkIndex := AMachine.ReturnAddr;
    end
    else
    // if v1v = v0.IntValue then
    begin
      AMachine.PopAddr;
      AMachine.Pop;
      AMachine.Pop;
    end;
  finally
    AMachine.Stack := s;
  end;
end;

// IF THEN

procedure VfIf(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  Level: Integer;
  tkIndex: Integer;
  tkCount: Integer;
  TkLine: String;
  TkLen: Integer;
begin
  if AMachine.PopInt <> 0 then
  begin
    // Условие выполняется
    exit;
  end
  else
  begin
    // Условие не выполняется идем к else или then
    Level := 1;
    tkIndex := AMachine.CourientTkIndex;
    tkCount := AMachine.tkCount;
    //
{$REGION 'Выходим или ищем else'}
    repeat
      inc(tkIndex);
      TkLine := AMachine.GetTk(tkIndex);
      TkLen := Length(TkLine);
      if TkLen = 2 then
      // Если это слово из 2 букв
      begin
        if CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, Pchar(TkLine),
          TkLen, 'if', 2) = CSTR_EQUAL then
        begin
          inc(Level)
        end
      end
      else if TkLen = 4 then
      // или это слово из 2 букв
      begin
        if CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, Pchar(TkLine),
          TkLen, 'else', 4) = CSTR_EQUAL then
        begin
          // Только если else на нашем уровне, мы переходим не него
          if Level = 1 then
            dec(Level)
        end
        else if CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, Pchar
            (TkLine), TkLen, 'then', 4) = CSTR_EQUAL then
        begin
          dec(Level)
        end;
      end;
    until (Level = 0) or (tkIndex >= tkCount);
{$ENDREGION}
    AMachine.CourientTkIndex := tkIndex;
  end;
end;

procedure VfElse(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  Level: Integer;
  tkIndex: Integer;
  tkCount: Integer;
  TkLine: String;
  TkLen: Integer;
begin
  SeekForExit(AMachine);
end;

procedure VfThen(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  // Then это просто метка
  { TODO -oOnni -cGeneral : Возможно нужно добавить работу со стеком адресов }
end;

procedure VfExit(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  SeekForExit(AMachine);
end;

// procedure VfQuit(AMachine: IVForthMachine; AAthom: IVForthAthom;
// PAthomStr: PWideChar); stdcall;
// begin
//
// end;
//
// procedure VfAbort(AMachine: IVForthMachine; AAthom: IVForthAthom;
// PAthomStr: PWideChar); stdcall;
// begin
//
// end;

procedure VfRaise(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  vs: TForthStack;
  v0: IVForthVariant;
begin
  try
    vs := AMachine.Stack;
    v0 := AMachine.Pop;
    AMachine.Stack := fsException;
    AMachine.Push(v0);
    raise Exception.Create(v0.StringValue);
  finally
    AMachine.Stack := vs;
  end;
end;

procedure VfBegin(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  AMachine.PushAddr(AMachine.CourientTkIndex);
end;

procedure VfUntil(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  if AMachine.Pop.BoolValue then
  begin
    AMachine.PopAddr;
  end
  else
  begin
    AMachine.CourientTkIndex := AMachine.ReturnAddr;
  end;
end;
// Compare

procedure VfEq(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  v0, v1: IVForthVariant;
begin
  v0 := AMachine.Pop;
  v1 := AMachine.Pop;
  case GetPriorityVariantType(v1.VariantType, v0.VariantType) of
    vtInteger, vtFloat:
      AMachine.PushInt(Integer(v1.FloatValue = v0.FloatValue));
    { todo :vtNatural: ; }
    { todo :vtComplex: ; }
    vtString:
      AMachine.PushInt(Integer(v1.StringValue = v0.StringValue));
  else
    raise EVForthModuleLogicError.Create('Bad types')
  end;
end;

procedure VfNEq(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  v0, v1: IVForthVariant;
begin
  v0 := AMachine.Pop;
  v1 := AMachine.Pop;
  case GetPriorityVariantType(v1.VariantType, v0.VariantType) of
    vtInteger, vtFloat:
      AMachine.PushInt(Integer(v1.FloatValue <> v0.FloatValue));
    { todo :vtNatural: ; }
    { todo :vtComplex: ; }
    vtString:
      AMachine.PushInt(Integer(v1.StringValue <> v0.StringValue));
  else
    raise EVForthModuleLogicError.Create('Bad types')
  end;
end;

procedure VfHi(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  v0, v1: IVForthVariant;
begin
  v0 := AMachine.Pop;
  v1 := AMachine.Pop;
  case GetPriorityVariantType(v1.VariantType, v0.VariantType) of
    vtInteger, vtFloat:
      AMachine.PushInt(Integer(v1.FloatValue > v0.FloatValue));
    { todo :vtNatural: ; }
    { todo :vtComplex: ; }
    vtString:
      AMachine.PushInt(Integer(v1.StringValue > v0.StringValue));
  else
    raise EVForthModuleLogicError.Create('Bad types')
  end;
end;

procedure VfLo(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  v0, v1: IVForthVariant;
begin
  v0 := AMachine.Pop;
  v1 := AMachine.Pop;
  case GetPriorityVariantType(v1.VariantType, v0.VariantType) of
    vtInteger, vtFloat:
      AMachine.PushInt(Integer(v1.FloatValue < v0.FloatValue));
    { todo :vtNatural: ; }
    { todo :vtComplex: ; }
    vtString:
      AMachine.PushInt(Integer(v1.StringValue < v0.StringValue));
  else
    raise EVForthModuleLogicError.Create('Bad types')
  end;
end;

procedure VfLoE(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  v0, v1: IVForthVariant;
begin
  v0 := AMachine.Pop;
  v1 := AMachine.Pop;
  case GetPriorityVariantType(v1.VariantType, v0.VariantType) of
    vtInteger, vtFloat:
      AMachine.PushInt(Integer(v1.FloatValue <= v0.FloatValue));
    { todo :vtNatural: ; }
    { todo :vtComplex: ; }
    vtString:
      AMachine.PushInt(Integer(v1.StringValue <= v0.StringValue));
  else
    raise EVForthModuleLogicError.Create('Bad types')
  end;
end;

procedure VfHiE(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  v0, v1: IVForthVariant;
begin
  v0 := AMachine.Pop;
  v1 := AMachine.Pop;
  case GetPriorityVariantType(v1.VariantType, v0.VariantType) of
    vtInteger, vtFloat:
      AMachine.PushInt(Integer(v1.FloatValue >= v0.FloatValue));
    { todo :vtNatural: ; }
    { todo :vtComplex: ; }
    vtString:
      AMachine.PushInt(Integer(v1.StringValue >= v0.StringValue));
  else
    raise EVForthModuleLogicError.Create('Bad types')
  end;
end;

// Bin

procedure VfAnd(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  v0, v1: IVForthVariant;
begin
  v0 := AMachine.Pop;
  v1 := AMachine.Pop;
  AMachine.PushInt(v1.IntValue and v0.IntValue);
end;

procedure VfOr(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  v0, v1: IVForthVariant;
begin
  v0 := AMachine.Pop;
  v1 := AMachine.Pop;
  AMachine.PushInt(v1.IntValue or v0.IntValue);
end;

procedure VfXor(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  v0, v1: IVForthVariant;
begin
  v0 := AMachine.Pop;
  v1 := AMachine.Pop;
  AMachine.PushInt(v1.IntValue xor v0.IntValue);
end;

procedure VfNot(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  v0: IVForthVariant;
begin
  v0 := AMachine.Pop;
  AMachine.PushInt(not v0.IntValue);
end;

procedure VfShl(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  v0, v1: IVForthVariant;
begin
  v0 := AMachine.Pop;
  v1 := AMachine.Pop;
  AMachine.PushInt(v1.IntValue shl v0.IntValue);
end;

procedure VfShr(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  v0, v1: IVForthVariant;
begin
  v0 := AMachine.Pop;
  v1 := AMachine.Pop;
  AMachine.PushInt(v1.IntValue shr v0.IntValue);
end;

// Logic

procedure VfAndL(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  v0, v1: IVForthVariant;
begin
  v0 := AMachine.Pop;
  v1 := AMachine.Pop;
  AMachine.PushInt(Integer(v1.BoolValue and v0.BoolValue));
end;

procedure VfOrL(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  v0, v1: IVForthVariant;
begin
  v0 := AMachine.Pop;
  v1 := AMachine.Pop;
  AMachine.PushInt(Integer(v1.BoolValue or v0.BoolValue));
end;

procedure VfXorL(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  v0, v1: IVForthVariant;
begin
  v0 := AMachine.Pop;
  v1 := AMachine.Pop;
  AMachine.PushInt(Integer(v1.BoolValue xor v0.BoolValue));
end;

procedure VfNotL(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  v0: IVForthVariant;
begin
  v0 := AMachine.Pop;
  AMachine.PushInt(Integer(not v0.BoolValue));
end;

procedure TVForthModuleLogic.Register(AMachine: IVForthMachine);
begin

  AMachine.AddAthom(CreateVForthSystemAthom('if', self, VfIf));
  AMachine.AddAthom(CreateVForthSystemAthom('else', self, VfElse));
  AMachine.AddAthom(CreateVForthSystemAthom('then', self, VfThen));

  AMachine.AddAthom(CreateVForthSystemAthom('exit', self, VfExit));
  // AMachine.AddAthom(CreateVForthSystemAthom('quit', self, VfQuit));
  // AMachine.AddAthom(CreateVForthSystemAthom('abort', self, VfAbort));

  AMachine.AddAthom(CreateVForthSystemAthom('raise', self, VfRaise));

  AMachine.AddAthom(CreateVForthSystemAthom('do', self, VfDo));
  AMachine.AddAthom(CreateVForthSystemAthom('loop', self, VfLoop));

  AMachine.AddAthom(CreateVForthSystemAthom('begin', self, VfBegin));
  AMachine.AddAthom(CreateVForthSystemAthom('until', self, VfUntil));

  AMachine.AddAthom(CreateVForthSystemAthom('=', self, VfEq));
  AMachine.AddAthom(CreateVForthSystemAthom('<>', self, VfNEq));
  AMachine.AddAthom(CreateVForthSystemAthom('>', self, VfHi));
  AMachine.AddAthom(CreateVForthSystemAthom('<', self, VfLo));
  AMachine.AddAthom(CreateVForthSystemAthom('>=', self, VfHiE));
  AMachine.AddAthom(CreateVForthSystemAthom('<=', self, VfLoE));

  AMachine.AddAthom(CreateVForthSystemAthom('and*', self, VfAnd));
  AMachine.AddAthom(CreateVForthSystemAthom('or*', self, VfOr));
  AMachine.AddAthom(CreateVForthSystemAthom('xor*', self, VfXor));
  AMachine.AddAthom(CreateVForthSystemAthom('not*', self, VfNot));
  AMachine.AddAthom(CreateVForthSystemAthom('shl*', self, VfShl));
  AMachine.AddAthom(CreateVForthSystemAthom('shr*', self, VfShr));

  AMachine.AddAthom(CreateVForthSystemAthom('and', self, VfAndL));
  AMachine.AddAthom(CreateVForthSystemAthom('or', self, VfOrL));
  AMachine.AddAthom(CreateVForthSystemAthom('xor', self, VfXorL));
  AMachine.AddAthom(CreateVForthSystemAthom('not', self, VfNotL));
end;

end.
