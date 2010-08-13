unit ifopSystem;

interface

procedure RegisterDictionary(AKernel: TObject);

implementation

uses
  IfopKernel;

//Завершение выполнения текущей задачи и возврат управления на терминал.
procedure ifopQuit(Kernel: TIfopKernel);
begin
  Kernel.isScriptEnd  := true;
end;

procedure RegisterDictionary(AKernel: TObject);
var
  Kernel: TIfopKernel;
  i: Integer;
begin
  Kernel := TIfopKernel(AKernel);
  // Регистрация функций
  Kernel.AddKeyword('quit', @ifopQuit);
end;

end.
