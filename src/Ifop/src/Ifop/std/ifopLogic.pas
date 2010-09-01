unit ifopLogic;

interface

procedure RegisterDictionary(AKernel: TObject);

implementation

uses
  IfopKernel;

procedure ifopDo(Kernel: TIfopKernel);
begin
  Kernel.PushRet(Kernel.CourientTkLine);
end;

procedure ifopLoop(Kernel: TIfopKernel);
begin
  if Kernel.Stack[1].IntValue <> Kernel.Stack[0].IntValue then
  begin
    Kernel.Stack[1].IntValue := Kernel.Stack[1].IntValue - 1;
    Kernel.CourientTkLine := Kernel.GetRet
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
begin

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
  {todo: Kernel.AddKeyword('again', @ifopUntil);}
  Kernel.AddKeyword('if', @ifopIf);
  Kernel.AddKeyword('then', @ifopThen);
  Kernel.AddKeyword('else', @ifopElse);
end;

end.
