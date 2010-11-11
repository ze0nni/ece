unit VForthModuleSystem;
{$IFDEF fpc}{$MODE delphi}{$ENDIF}

interface

uses
  SysUtils,
  Classes,
  Windows,
  ShellApi,
  VForthModule,
  VForth;

type
  EVForthModuleSystemError = class(Exception)

  end;

  TVForthModuleSystem = class(TVForthModule, IVForthModule)
  private

  protected

  public
    procedure Register(AMachine: IVForthMachine); stdcall;
  end;

implementation

uses
  VForthAthom,
  VForthVariants,
  VForthVariantArray;

// Переключение между стеками
// Пользователь имее право преключаться только между первыми 7-ю
// с 0-го пл 7-й
procedure VfStack(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  AMachine.Stack := TForthStack(AMachine.PopInt);
end;

procedure VfGetStack(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  AMachine.PushInt(Integer(AMachine.Stack));
end;

// Перестановка двух верхних элементов стека
procedure VfSwap(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  v: IVForthVariant;
begin
  v := AMachine.DataStack[0];
  AMachine.DataStack[0] := AMachine.DataStack[1];
  AMachine.DataStack[1] := v;
end;

// Дублирование верхнего элемента стека
procedure VfDup(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  v: IVForthVariant;
begin
  v := AMachine.DataStack[0];
  AMachine.Push(v.Convert(v.VariantType));
end;

// Дублирование ссылки верхнего элемента стека
procedure VfDupVar(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  v: IVForthVariant;
begin
  v := AMachine.DataStack[0];
  AMachine.Push(v);
end;

// Копирование второго элемента и размещение копии в вершине стека.
procedure VfOver(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  v: IVForthVariant;
begin
  v := AMachine.DataStack[1];
  AMachine.Push(v.Convert(v.VariantType));
end;

// Размещение третьего элемента в вершине стека.
procedure VfRot(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  AMachine.Push(AMachine.PopEx(2));
end;

// Удаление из стека верхнего элемента.
procedure VfDrop(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  AMachine.Pop;
end;

// копирует n-й эоемент стека в вершину
procedure VfPick(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  v: IVForthVariant;
begin
  v := AMachine.DataStack[AMachine.PopInt];
  AMachine.Push(v.Convert(v.VariantType));
end;

// Удаляет из стека второе сверху число, сдвигая вершину на его место
procedure VfNip(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  AMachine.PopEx(1)
end;

// Вращает N верхних чисел стека. 1 ROLL аналогично SWAP
procedure VfRoll(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  AMachine.Push(AMachine.PopEx(AMachine.PopInt));
end;

procedure VfPickVar(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  v: IVForthVariant;
begin
  v := AMachine.DataStack[AMachine.PopInt];
  AMachine.Push(v);
end;

// Выводим стек на экран
procedure VfViewStack(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  i: Integer;
  v: IVForthVariant;
begin
  for i := 0 to AMachine.DataStackSize - 1 do
  begin
    v := AMachine.DataStack[i];
    if v.Name = '' then
      AMachine.StdOut(VariantTypeToString(v.VariantType)
          + '=' + v.StringValue + #13#10)
    else
      AMachine.StdOut(VariantTypeToString(v.VariantType)
          + '(' + v.Name + ')=' + v.StringValue + #13#10);
  end;
end;

// Очищаем стек
procedure VfClearStack(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  i: Integer;
begin
  for i := 0 to AMachine.DataStackSize - 1 do
    AMachine.Pop;
end;

procedure VfViewDictionary(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  i: Integer;
  A: IVForthAthom;
begin
  AMachine.StdOut('Dictionary size: ' + IntToStr(AMachine.AthomsCount)
      + #13#10);
  for i := 0 to AMachine.AthomsCount - 1 do
  begin
    A := AMachine.AthomByIndex[i];
    AMachine.StdOut(A.Name + #9);
  end;
end;

procedure VfForget(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  AAthomStr: string;
begin
  AAthomStr := AMachine.GetTk(AMachine.CourientTkIndex + 1);
  AMachine.CourientTkIndex := AMachine.CourientTkIndex + 1;
  AMachine.Forget(AAthomStr);
end;

procedure VfHelp(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  AAthomStr: string;
  i: Integer;
begin
  AAthomStr := '';
  i := AMachine.CourientTkIndex;
  while i < AMachine.TkCount - 1 do
  begin
    inc(i);
    if AAthomStr = '' then
      AAthomStr := AMachine.GetTk(i)
    else
      AAthomStr := AAthomStr + #32 + AMachine.GetTk(i);
  end;
  AMachine.CourientTkIndex := AMachine.TkCount - 1;
  { TODO -oOnno -cGeneral : Вызов справки }
  // Должен искать до конца строки тип
  // help вещественные числа
  if AAthomStr <> '' then
    AMachine.StdErr('Topic "' + AAthomStr + '" not found.')
  else
  begin
    AMachine.StdOut('VForth machine v0.0'#13#10);
    AMachine.StdOut('Input "help <keywords line>" for get help.'#13#10);
    AMachine.StdOut('Input ".d" to view dictionary');
  end;
end;

//
procedure VfEcecuteStr(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  AMachine.AddCode(AMachine.PopString);
end;

// Назначаем переменной в верштне стека имя
procedure VfCreate(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  AVariableName: string;
begin
  AVariableName := AMachine.GetTk(AMachine.CourientTkIndex + 1);
  AMachine.CourientTkIndex := AMachine.CourientTkIndex + 1;
  AMachine.DataStack[0].Name := AVariableName;
end;

// Устанавливае имя переменной из вершины стека
procedure VfSetVariable(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  AVariableName: string;
  v0, v1: IVForthVariant;
begin
  v0 := AMachine.Pop;
  v1 := AMachine.Pop;
  case v0.VariantType of
    vtInteger:
      v0.IntValue := v1.IntValue;
    vtFloat:
      v0.FloatValue := v1.FloatValue;
    vtString:
      v0.StringValue := v1.StringValue
    else
      raise EVForthModuleSystemError.Create('Can''t set varible');
  end;
end;

// Делаем копию значения переменной из стека
procedure VfGetVariable(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  v0: IVForthVariant;
begin
  v0 := AMachine.Pop;
  AMachine.Push(v0.Convert(v0.VariantType));
end;

// Создаем массив переменных
procedure VfVector(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  AVariableName: string;
  arr: TArrayVariant;
begin
  // размер Array
  arr := TArrayVariant.Create;
  arr.Size := AMachine.PopInt;
  AMachine.Push(arr);
end;

// Устанавливае имя переменной из вершины стека
// Значение индекс мессив v!
procedure VfVectorSetVariable(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  v0, v1, v2: IVForthVariant;
begin
  v0 := AMachine.Pop;
  v1 := AMachine.Pop;
  v2 := AMachine.Pop;
  v0.Items[v1.IntValue] := v2;
end;

// Делаем копию значения переменной из стека
// индекс мессив v@
procedure VfVectorGetVariable(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  v0, v1: IVForthVariant;
begin
  v0 := AMachine.Pop;
  v1 := AMachine.Pop;
  AMachine.Push(v0.Items[v1.IntValue]);
end;

procedure VfGetVectorSize(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  AMachine.PushInt(AMachine.Pop.Size);
end;

procedure VfSetVectorSize(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  v0, v1: IVForthVariant;
begin
  v0 := AMachine.Pop;
  v1 := AMachine.Pop;
  v0.Size := v1.IntValue;
end;

// Структуры

procedure VfRecord(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  // Кладет в стек не адрес возврата, а текущий размерстека
  AMachine.PushAddr(AMachine.DataStackSize);
end;

procedure VfEnd(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  NewSize: Integer;
  A: TArrayVariant;
  i: Integer;
begin
  // Создает новую переменную перемещаем в нее весь добавленный стек
  NewSize := AMachine.DataStackSize - AMachine.PopAddr;
  if NewSize <= 0 then
    raise EVForthModuleSystemError.Create(
      'Can''t create record. Stack must grow.');
  A := TArrayVariant.Create;
  A.Size := NewSize;
  for i := 0 to NewSize - 1 do
  begin
    A.Items[i] := AMachine.Pop;
  end;
  AMachine.Push(A);
end;

// Приведение к типу

procedure VfCInt(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  AMachine.Push(AMachine.Pop.Convert(vtInteger));
end;

procedure VfCFloat(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  AMachine.Push(AMachine.Pop.Convert(vtFloat));
end;

procedure VfCNatural(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  AMachine.Push(AMachine.Pop.Convert(vtNatural));
end;

procedure VfCComplex(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  AMachine.Push(AMachine.Pop.Convert(vtComplex));
end;

procedure VfCStr(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  AMachine.Push(AMachine.Pop.Convert(vtString));
end;

procedure VfCChar(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  AMachine.PushString(Char(AMachine.PopInt));
end;

// Работа со строками
procedure VfEscapeStr(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  OldLine, NewLine: WideString;
  Len, NewLen: Integer;
  CChar: PWideChar;
begin
  OldLine := AMachine.PopString;
  Len := Length(OldLine);
  SetLength(NewLine, Len);
  NewLen := 1;
  CChar := PWideChar(OldLine);
  repeat
    if CChar^ = '\' then
    begin
      inc(CChar);
      case CChar^ of
        'n':
          begin
            NewLine[NewLen] := #13;
            NewLine[NewLen + 1] := #10;
            inc(NewLen);
          end;
        't':
          NewLine[NewLen] := #9;
        's':
          NewLine[NewLen] := #23;
        '\':
          NewLine[NewLen] := '\';
      end;
      inc(NewLen);
    end
    else
    begin
      NewLine[NewLen] := CChar^;
      inc(NewLen);
    end;
    inc(CChar);
  until CChar^ = #0;
  SetLength(NewLine, NewLen - 1);
  AMachine.PushString(NewLine);
end;

procedure VfFormatStr(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  OldLine, NewLine: WideString;
  Len, NewLen: Integer;
  CChar: PWideChar;
  procedure InsertFStr(str: WideString);
  begin
    // Увеличиваем размер буфера Если не хватает
    if Length(NewLine) < Length(str) + NewLen then
      SetLength(NewLine, Length(NewLine) + Len + Length(str) * SizeOf(WideChar)
        );

    CopyMemory(@NewLine[NewLen], PWideChar(str), Length(str) * SizeOf(WideChar)
      );
    inc(NewLen, Length(str) - 1);
  end;

begin
  OldLine := AMachine.PopString;
  Len := Length(OldLine);
  // Делаем буфер в две длины
  SetLength(NewLine, Len * 2);
  ZeroMemory(PWideChar(NewLine), Len * 2);
  NewLen := 1;
  CChar := PWideChar(OldLine);
  repeat
    if CChar^ = '%' then
    begin
      inc(CChar);
      case CChar^ of
        'd':
          InsertFStr(IntToStr(AMachine.PopInt));
        's':
          InsertFStr(AMachine.PopString);
        // 'a': InsertFStr(AMachine.PopInt); //распечатка массива
      end;
      inc(NewLen);
    end
    else
    begin
      NewLine[NewLen] := CChar^;
      inc(NewLen);
    end;
    inc(CChar);
  until CChar^ = #0;
  SetLength(NewLine, NewLen - 1);
  AMachine.PushString(NewLine);
end;

procedure VfGetStrLen(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  v: IVForthVariant;
begin
  v := AMachine.DataStack[0];
  AMachine.PushInt(Length(v.StringValue));
end;

procedure VfSetStrLen(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  v: IVForthVariant;
  s: WideString;
begin
  v := AMachine.DataStack[1];
  s := v.StringValue;
  SetLength(s, AMachine.PopInt);
  v.StringValue := s;
end;

procedure VfStrUpper(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  v: IVForthVariant;
begin
  v := AMachine.DataStack[0];
  v.StringValue := UpperCase(v.StringValue)
end;

procedure VfStrLower(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  v: IVForthVariant;
begin
  v := AMachine.DataStack[0];
  v.StringValue := LowerCase(v.StringValue)
end;

procedure VfStrGetChar(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  v: IVForthVariant;
begin
  v := AMachine.DataStack[1];
  AMachine.PushString(v.StringValue[AMachine.PopInt]);
end;

procedure VfStrSetChar(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  v: IVForthVariant;
  v0, v1: IVForthVariant;
  s: WideString;
begin
  v := AMachine.DataStack[2];
  v0 := AMachine.Pop;
  v1 := AMachine.Pop;
  s := v.StringValue;
  s[v0.IntValue] := v1.StringValue[1];
  v.StringValue := s;
end;

procedure VfWin32Type(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  str: string;
  wt: TWin32Type;
begin
{$IFDEF fpc}
  str := PAthomStr;
{$ELSE}
  str := LowerCase(PAthomStr);
{$ENDIF}
  if str = 'w32bool' then
    wt := wtBool
  else if str = 'w32byte' then
    wt := wtByte
  else if str = 'w32word' then
    wt := wtWord
  else if str = 'w32int' then
    wt := wtInt
  else if str = 'w32chara' then
    wt := wtCharA
  else if str = 'w32charw' then
    wt := wtCharW
  else if str = 'w32pchara' then
    wt := wtPCharA
  else if str = 'w32pcharw' then
    wt := wtPCharW
  else if str = 'w32pointer' then
    wt := wtPointer;
  AMachine.DataStack[0].Win32Type := wt;
end;

procedure VfImport(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(AMachine.PopString);
    AMachine.AddCode(sl.Text);
  finally
    sl.Free;
  end;
end;

procedure VfLoadLibrary(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  AMachine.LoadLibrary(AMachine.PopString);
end;

procedure VfExecute(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  pr, cmd: string;
  sif : TStartupInfo;
  pif : TProcessInformation;
begin
  pr := AMachine.PopString;
  cmd := AMachine.PopString;
  ZeroMemory(@sif, SizeOf(sif));
  sif.dwFlags := STARTF_USESTDHANDLES;
  sif.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
  sif.hStdOutput := GetStdHandle(STD_OUTPUT_HANDLE);
  sif.hStdError := GetStdHandle(STD_ERROR_HANDLE);
  AMachine.PushInt(Integer(CreateProcess(Pchar(pr), Pchar(cmd), nil, nil, false, 0, nil, nil, sif, pif)));
end;

procedure VfRun(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  pr, cmd: string;
begin
  pr := AMachine.PopString;
  cmd := AMachine.PopString;
  AMachine.PushInt(ShellExecute(0, 'open', PChar(pr), PChar(cmd), nil,
      SW_SHOWNORMAL));
end;
{ TVForthModuleSystem }

procedure TVForthModuleSystem.Register(AMachine: IVForthMachine);
begin
  AMachine.AddAthom(CreateVForthSystemAthom('stack', Self, VfStack));
  AMachine.AddAthom(CreateVForthSystemAthom('@stack', Self, VfGetStack));

  AMachine.AddAthom(CreateVForthSystemAthom('swap', Self, VfSwap));
  AMachine.AddAthom(CreateVForthSystemAthom('dup', Self, VfDup));
  AMachine.AddAthom(CreateVForthSystemAthom('dup*', Self, VfDupVar));
  AMachine.AddAthom(CreateVForthSystemAthom('over', Self, VfOver));
  AMachine.AddAthom(CreateVForthSystemAthom('rot', Self, VfRot));
  AMachine.AddAthom(CreateVForthSystemAthom('drop', Self, VfDrop));

  AMachine.AddAthom(CreateVForthSystemAthom('pick', Self, VfPick));
  AMachine.AddAthom(CreateVForthSystemAthom('pick*', Self, VfPickVar));

  AMachine.AddAthom(CreateVForthSystemAthom('nip', Self, VfNip));
  AMachine.AddAthom(CreateVForthSystemAthom('roll', Self, VfRoll));

  AMachine.AddAthom(CreateVForthSystemAthom('.s', Self, VfViewStack));
  AMachine.AddAthom(CreateVForthSystemAthom('.c', Self, VfClearStack));
  AMachine.AddAthom(CreateVForthSystemAthom('.d', Self, VfViewDictionary));
  AMachine.AddAthom(CreateVForthSystemAthom('forget', Self, VfForget));
  AMachine.AddAthom(CreateVForthSystemAthom('help', Self, VfHelp));
  AMachine.AddAthom(CreateVForthSystemAthom('execute', Self, VfEcecuteStr));

  AMachine.AddAthom(CreateVForthSystemAthom('var', Self, VfCreate));
  AMachine.AddAthom(CreateVForthSystemAthom('!', Self, VfSetVariable));
  AMachine.AddAthom(CreateVForthSystemAthom('@', Self, VfGetVariable));
  AMachine.AddAthom(CreateVForthSystemAthom('vector', Self, VfVector));
  AMachine.AddAthom(CreateVForthSystemAthom('!v', Self, VfVectorSetVariable));
  AMachine.AddAthom(CreateVForthSystemAthom('@v', Self, VfVectorGetVariable));
  AMachine.AddAthom(CreateVForthSystemAthom('!vs', Self, VfSetVectorSize));
  AMachine.AddAthom(CreateVForthSystemAthom('@vs', Self, VfGetVectorSize));

  AMachine.AddAthom(CreateVForthSystemAthom('record', Self, VfRecord));
  AMachine.AddAthom(CreateVForthSystemAthom('end', Self, VfEnd));

  AMachine.AddAthom(CreateVForthSystemAthom('cint', Self, VfCInt));
  AMachine.AddAthom(CreateVForthSystemAthom('cfloat', Self, VfCFloat));
  AMachine.AddAthom(CreateVForthSystemAthom('cnatural', Self, VfCNatural));
  AMachine.AddAthom(CreateVForthSystemAthom('ccomplex', Self, VfCComplex));
  AMachine.AddAthom(CreateVForthSystemAthom('cstr', Self, VfCStr));
  AMachine.AddAthom(CreateVForthSystemAthom('cchar', Self, VfCChar));

  AMachine.AddAthom(CreateVForthSystemAthom('escapestr', Self, VfEscapeStr));
  AMachine.AddAthom(CreateVForthSystemAthom('formatstr', Self, VfFormatStr));

  AMachine.AddAthom(CreateVForthSystemAthom('@strlen', Self, VfGetStrLen));
  AMachine.AddAthom(CreateVForthSystemAthom('!strlen', Self, VfSetStrLen));
  AMachine.AddAthom(CreateVForthSystemAthom('strupper', Self, VfStrUpper));
  AMachine.AddAthom(CreateVForthSystemAthom('strlower', Self, VfStrLower));
  AMachine.AddAthom(CreateVForthSystemAthom('@c', Self, VfStrGetChar));
  AMachine.AddAthom(CreateVForthSystemAthom('!c', Self, VfStrSetChar));

  AMachine.AddAthom(CreateVForthSystemAthom('w32Bool', Self, VfWin32Type));
  AMachine.AddAthom(CreateVForthSystemAthom('w32Byte', Self, VfWin32Type));
  AMachine.AddAthom(CreateVForthSystemAthom('w32Word', Self, VfWin32Type));
  AMachine.AddAthom(CreateVForthSystemAthom('w32Int', Self, VfWin32Type));
  AMachine.AddAthom(CreateVForthSystemAthom('w32CharA', Self, VfWin32Type));
  AMachine.AddAthom(CreateVForthSystemAthom('w32CharW', Self, VfWin32Type));
  AMachine.AddAthom(CreateVForthSystemAthom('w32PCharA', Self, VfWin32Type));
  AMachine.AddAthom(CreateVForthSystemAthom('w32PCharW', Self, VfWin32Type));
  AMachine.AddAthom(CreateVForthSystemAthom('w32Pointer', Self, VfWin32Type));

  AMachine.AddAthom(CreateVForthSystemAthom('Import', Self, VfImport));
  AMachine.AddAthom(CreateVForthSystemAthom('LoadLibrary', Self, VfLoadLibrary));

  AMachine.AddAthom(CreateVForthSystemAthom('Exec', Self, VfExecute));
  AMachine.AddAthom(CreateVForthSystemAthom('Run', Self, VfRun));
end;

end.
