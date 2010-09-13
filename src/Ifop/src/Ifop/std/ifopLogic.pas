unit ifopLogic;

interface

procedure RegisterDictionary(AKernel: TObject);

implementation

uses
  Windows,
  SysUtils,
  IfopKernel,
  IfopVariant;

procedure ifopDo(Kernel: TIfopKernel);
var
  v1, v0: TIfopVariant;
begin
  v1 := Kernel.Stack[1];
  v0 := Kernel.Stack[0];
  if v1.IntValue <> v0.IntValue then
  begin
    { TODO -oOnni -cGeneral : Переходим на LOOP }
  end;
  Kernel.PushRet(Kernel.CourientTkLine);
  Kernel.Push(v1.GetClone);
end;

procedure ifopLoop(Kernel: TIfopKernel);
var
  v1, v0: TIfopVariant;
begin
  v1 := Kernel.Stack[1];
  v0 := Kernel.Stack[0];
  if v1.IntValue <> v0.IntValue then
  begin
    if v1.IntValue > v0.IntValue then
      v1.IntValue := v1.IntValue - 1
    else
      v1.IntValue := v1.IntValue + 1;
    Kernel.CourientTkLine := Kernel.GetRet;
    Kernel.Push(v1.GetClone);
  end
  else
  begin
    Kernel.PopRet;
    Kernel.PopInt;
    Kernel.PopInt;
  end;
end;

procedure ifopBegin(Kernel: TIfopKernel);
begin
  Kernel.PushRet(Kernel.CourientTkLine);
end;

procedure ifopAgain(Kernel: TIfopKernel);
begin
  Kernel.PushRet(Kernel.CourientTkLine);
end;

procedure ifopUntil(Kernel: TIfopKernel);
begin
  case Kernel.PopInt of
    0:
      begin
        Kernel.CourientTkLine := Kernel.GetRet;
      end;
  else
    begin
      Kernel.PopRet;
    end;
  end;
end;

procedure ifopIf(Kernel: TIfopKernel);
var
  p: Integer;
  ln: Integer;
  token: string;
  ifThenLevel : Integer;
begin
  if Kernel.PopInt = 0 then
  // Условие не выполняется (false) ищем нащ then
  begin
    p := Kernel.CourientTkLine;
    ifThenLevel := 1;
    repeat
      inc(p);
      token := Kernel.CourientTk[p];
      ln := Length(pchar(token));
      if ln in [2, 4] then
      begin
        if CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, 'if', 2, pchar(token), ln) = CSTR_EQUAL then
        begin
          inc(ifThenLevel)
        end else
            begin

            end;
        if CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, 'then', 4, pchar(token), ln) = CSTR_EQUAL then
        begin
          dec(ifThenLevel)
        end;
      end;
    until (p = Kernel.CourientTk.Count - 1) or (ifThenLevel = 0);
    if ifThenLevel <> 0 then
      raise Exception.Create('"if" exists but "then" not found.');
    Kernel.CourientTkLine := p;
  end;
end;

procedure ifopThen(Kernel: TIfopKernel);
begin

end;

procedure ifopElse(Kernel: TIfopKernel);
begin

end;

procedure RegisterDictionary(AKernel: TObject);
var
  Kernel: TIfopKernel;
  i: Integer;
begin
  Kernel := TIfopKernel(AKernel);
  // Регистрация функций
  Kernel.AddKeyword('do', @ifopDo);
  Kernel.AddKeyword('loop', @ifopLoop);
  Kernel.AddKeyword('begin', @ifopBegin);
  Kernel.AddKeyword('until', @ifopUntil);
  { todo: Kernel.AddKeyword('again', @ifopUntil); }
  Kernel.AddKeyword('if', @ifopIf);
  Kernel.AddKeyword('then', @ifopThen);
  Kernel.AddKeyword('else', @ifopElse);
end;

end.
