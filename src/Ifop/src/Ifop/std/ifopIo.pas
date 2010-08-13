unit ifopIo;

interface

procedure RegisterDictionary(AKernel: TObject);

implementation

uses
  Windows,
  SysUtils,
  IfopVariant,
  IfopKernel;

//Вывод числе их стека не экран
procedure ifopPrintTopValue(Kernel: TIfopKernel);
var
  v1 : TIfopVariant;
begin
  v1 := Kernel.Pop;
  Kernel.stdout(v1.StrValue, false);
  v1.Free;
end;

//Возврат каретки и перевод строки.
procedure ifopCr(Kernel: TIfopKernel);
begin
  Kernel.stdout('', True);
end;

//Вывод заданного числа пробелов.
procedure ifopSpaces(Kernel: TIfopKernel);
var
  i: Integer;
begin
  for i := 0 to Kernel.Pop.IntValue - 1 do
    Kernel.stdout(#32, false);
end;

//Вывод одного пробела.
procedure ifopSpace(Kernel: TIfopKernel);
begin
  Kernel.stdout(#32, false);
end;

var
  IOFile : HFILE;

procedure ifopFOpen(Kernel: TIfopKernel);
begin
  IOFile := CreateFile(Pchar(Kernel.PopStr), GENERIC_READ or GENERIC_WRITE,
    0,
    nil,
    OPEN_EXISTING,
    0,
    0);
  if IOFile <> 0 then
    Kernel.PushInt(1)
    else
    Kernel.PushInt(0);
end;

procedure ifopFNew(Kernel: TIfopKernel);
begin
  IOFile := CreateFile(Pchar(Kernel.PopStr), GENERIC_READ or GENERIC_WRITE,
    0,
    nil,
    CREATE_NEW,
    0,
    0);
  if IOFile <> 0 then
    Kernel.PushInt(1)
    else
    Kernel.PushInt(0);
end;

procedure ifopFWrite(Kernel: TIfopKernel);
var
  s : string;
  v : Cardinal;
begin
  s := Kernel.PopStr;
  if not WriteFile(IOFile, s[1], Length(s) * Sizeof(Char), v, nil)
    then
      raise Exception.Create('File write error');
end;

procedure ifopFClose(Kernel: TIfopKernel);
begin
  Kernel.PushInt(integer(CloseHandle(IOFile)))
end;

procedure RegisterDictionary(AKernel: TObject);
var
  Kernel: TIfopKernel;
  i: Integer;
begin
  Kernel := TIfopKernel(AKernel);
  // Регистрация функций
  Kernel.AddKeyword('.', @ifopPrintTopValue);
  Kernel.AddKeyword('cr', @ifopCr);
  Kernel.AddKeyword('spaces', @ifopSpaces);
  Kernel.AddKeyword('space', @ifopSpace);

  Kernel.AddKeyword('fopen', @ifopFOpen);
  Kernel.AddKeyword('fnew', @ifopFNew);
  Kernel.AddKeyword('fclose', @ifopFClose);
  Kernel.AddKeyword('fwrite', @ifopFWrite);
end;

initialization

finalization
  CloseHandle(IOFile)
end.
