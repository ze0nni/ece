unit IfopKernel;

interface

uses
  SysUtils,
  Classes,
  Windows,
  Math,
  IfopVariant,
  ifopSystem,
  ifopInformation,
  ifopMatch,
  ifopIo,
  ifopStack,
  ifopOle,
  ifopRegExp,
  ifopLogic;

const
  IfopVersionMajor = 0;
  IfopVersionMinor = 0;
  IfopVersionRelease = 0;
  IfopVersionBuild = 1;

type
  TifopKernelDictionaryItem = class;

  TAngleMode = (amDeg, amRad, amGrad);

  PStdOutProc = procedure(Data: Pointer; Text: string; AReturn: Boolean);
  PStdErrProc = procedure(Data: Pointer; Text: string; AReturn: Boolean);

  TIfopArg = (argByte, argWord, argInteger, ArgString);

  TIfopKernel = class
  private
    // Стек
    FStack: TList;
    // Стек адресов возврата
    FRetStack: TList;
    // Словарь
    FDictionary: TStringList;
    FDictionaryForgetLimit: Integer;
    // Тут будут храниться строки при добавлении
    FCode: TStringList;
    // Тут будут "откомплированные" инструкции
    FCommands: TList;
    FisScriptEnd: Boolean;
    FAbortCmd : Boolean;
    FAngleMode: TAngleMode;
    // IO
    FStdOutData: Pointer;
    FStdErrData: Pointer;
    FStdOutProc: PStdOutProc;
    FStdErrProc: PStdErrProc;
    // Текущая очередь команд
    FCourientTk: TStringList;
    FCourientTkLine: Integer;
    //
    procedure RegisterStdDictionarys;
    function GetDictionarySize: Integer;
    function GetDictionary(const Index: Integer): TifopKernelDictionaryItem;
    function GetStackSize: Integer;
    function GetStack(const index: Integer): TIfopVariant;
    procedure SetStack(const index: Integer; const Value: TIfopVariant);
    procedure SetisScriptEnd(const Value: Boolean);
    procedure SetAngleMode(const Value: TAngleMode);
    function FloatToRad(const Val: Double): Double;
    procedure SetCourientTkLine(const Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    // Добавляет код к основному тексту
    procedure AddCode(ACode: string);
    // Процедура добавляет к словарю вызов стандартной функции
    procedure AddKeyword(const AKeyword: string; AProc: Pointer);
    procedure AddMethod(const AKeyword: string; AObject, AProc: Pointer;
      ArgsIn, ArgsOut: array of TIfopArg);
    procedure Forget(AAThom: string);
    //
    procedure stdin(var AString: string; ReturnLn: Boolean = true);
    procedure stdout(AString: string; ReturnLn: Boolean = true);
    procedure stderr(AString: string; ReturnLn: Boolean = true);

    function ExecuteToken(const AToken: string): Boolean; inline;
    // io
    procedure SetStdOut(AData: Pointer; AProc: PStdOutProc);
    procedure SetStdErr(AData: Pointer; AProc: PStdErrProc);
    // Stack
    property StackSize: Integer read GetStackSize;
    property Stack[const index: Integer]
      : TIfopVariant read GetStack write SetStack;
    procedure Push(const AItem: TIfopVariant); overload;
    function Pop: TIfopVariant; overload;
    // Ret
    procedure PushRet(const Addr: Integer);
    function PopRet: Integer;
    function GetRet: Integer;
    //
    property CourientTkLine
      : Integer read FCourientTkLine write SetCourientTkLine;
    property CourientTk: TStringList read FCourientTk;
    //
    procedure PushInt(const Val: Integer);
    function PopInt: Integer;
    procedure PushFloat(const Val: Double);
    function PopFloat: Double;
    procedure PushStr(const Val: string);
    function PopStr: string;
    // Dictionary
    property DictionarySize: Integer read GetDictionarySize;
    property Dictionary[const Index: Integer]
      : TifopKernelDictionaryItem read GetDictionary;
    //
    property AngleMode: TAngleMode read FAngleMode write SetAngleMode;
    function FloatToAngle(const Val: Double): Double;
    //
    property isScriptEnd: Boolean read FisScriptEnd write SetisScriptEnd;
    procedure Abort;
  end;

  // Процедура вызова функций ядра
  PDictionaryProcedure = procedure(AKernel: TIfopKernel);

  // Это запись в словаре форта
  TKernelDictionaryItemType = (itProcedure, // Вызов функции ядра
    itKeyword, // Выполнение последовательности инструкций ядра
    itNativeProcedure, // выполнение внешней функции
    itNativeMethod); // выполнение метода какого-то внешнего объекта как функции

  TifopKernelDictionaryItem = class
  private
    FItemType: TKernelDictionaryItemType;
    FProcedure: PDictionaryProcedure;
    FMethod: Pointer;
    FMethodObject: Pointer;
    FKernel: TIfopKernel;
    FKeywordName: string;
    FLine: string;
    procedure SetKeywordName(const Value: string);
  public
    constructor Create(AKernel: TIfopKernel; AProcedure: Pointer); overload;
    constructor Create(AKernel: TIfopKernel; ALine: string); overload;
    constructor Create(AKernel: TIfopKernel; AObject, AProc: Pointer;
      ArgIn, ArgOut: array of TIfopArg); overload;
    property ItemType: TKernelDictionaryItemType read FItemType;

    property Kernel: TIfopKernel read FKernel;

    procedure Execute;

    property KeywordName: string read FKeywordName write SetKeywordName;
    property Line: string read FLine;
  end;

implementation

{ TIfopKernel }

procedure SeparateString(ls: TStringList; Code: String);
var
  SPos: PChar;
  CPos: PChar;
  EPos: PChar;
  isinStr: Boolean;
begin
  Code := Code + #32;
  SPos := PChar(Code);
  CPos := PChar(Code);
  EPos := PChar(Code) + length(Code) - 1;

  // Заменяем все "лишние" символы пробелам
//  while CPos <> EPos do
//  begin
//    if CPos^ in [#9, #10, #13] then
//      CPos^ := #32;
//    inc(CPos);
//  end;
//  CPos := SPos;

  repeat
    if CPos = EPos then
      break;

    // Пропускаем пробелы
{$REGION 'Пустые строки'}
    if CPos^ in [#9, #10, #13, #32] then
    begin
      while (CPos^ in [#9, #10, #13, #32]) and (CPos <> EPos) do
        inc(CPos);
      continue;
    end;
{$ENDREGION}
{$REGION 'Комментарии'}
    if CPos^ = '(' then
    begin
      SPos := CPos;
      // inc(CPos);
      repeat
        inc(CPos);
      until CPos^ = ')';
      // ls.Add(Copy(SPos, 0, Cpos - Spos + 1));
      inc(CPos);
      continue;
    end;
{$ENDREGION}
{$REGION 'Строки'}
    if CPos^ = '"' then
    begin
      SPos := CPos;
      // inc(CPos);
      repeat
        inc(CPos);
      until CPos^ = '"';
      ls.Add(Copy(SPos, 0, CPos - SPos + 1));
      inc(CPos);
      continue;
    end;
{$ENDREGION}
{$REGION 'Декларация атома'}
    if CPos^ = ':' then
    begin
      SPos := CPos;
      repeat
        inc(CPos);
        // Строка
        if CPos^ = '"' then
        begin
          repeat
            inc(CPos)
          until (CPos^ = '"') or (CPos = EPos);
        end;
        // или коомментарий
        if CPos^ = '(' then
        begin
          repeat
            inc(CPos)
          until (CPos^ = ')') or (CPos = EPos);
        end;
        { TODO -oOnni -cGeneral : Атом внутри атома }
      until (CPos^ = ';') or (CPos = EPos);

      ls.Add(Copy(SPos, 0, CPos - SPos));
      inc(CPos);
      continue;
    end;
{$ENDREGION}
{$REGION 'Атомы'}
    if not(CPos^ in [#9, #10, #13, #32]) then
    begin
      SPos := CPos;
      repeat
        inc(CPos);
      until (CPos^ in [#9, #10, #13, #32]) or (CPos = EPos);
      ls.Add(Copy(SPos, 0, CPos - SPos));
      inc(CPos);
      continue;
    end;
{$ENDREGION}
    // Continue;
  until CPos >= EPos;
end;

procedure TIfopKernel.Abort;
begin
  FAbortCmd := true;
end;

procedure TIfopKernel.AddCode(ACode: string);
var
  Tk: TStringList;
  i: Integer;
  Token: String;
  N: Integer;
  F: Double;
  NewItem: TIfopVariant;
  NewAtom: TifopKernelDictionaryItem;
  FLastLine: Integer;
begin
  FAbortCmd := false;
  if ACode = '' then
    exit;
  FLastLine := FCourientTkLine;
  try
    Tk := TStringList.Create;
    SeparateString(Tk, ACode);
    // ACode := StringReplace(ACode, #9, #32, [rfReplaceAll]);
    // Tk.Text := StringReplace(ACode, #32, #13#10, [rfReplaceAll]);
    i := -1;
    repeat
      if FAbortCmd then
      begin
        FAbortCmd := false;
        exit;
      end;
      // Переходим на следующую строку
      inc(i);
      if i > Tk.Count - 1 then
        break;
      // Получаем следующий токен
      Token := Tk[i];
      // Текущая строка и текущая очередь команд
      FCourientTkLine := i;
      FCourientTk := Tk;

      // Если это атом из библиотеки то выполняем его и переходим к следующему
      if ExecuteToken(Token) then
      begin
        // кое кто может поменять текущее положение ;)
        i := FCourientTkLine;
        continue;
      end;
      // Добавляем новый атом в словарь
      if (Token[1] = ':') then
      begin
        NewAtom := TifopKernelDictionaryItem.Create(Self, Token);
        FDictionary.InsertObject(0, NewAtom.KeywordName, NewAtom);
        continue;
      end;
      // Добавляем строку в стек
      if (Token[1] = '"') and (Token[length(Token)] = '"') then
      begin
        NewItem := TifopStringVariant.Create;
        NewItem.StrValue := Copy(Token, 2, length(Token) - 2);
        Push(NewItem);
        continue;
      end;
      // Если это целое число, то добавляем его в стек
      if TryStrToInt(Token, N) then
      begin
        NewItem := TifopIntegerVariant.Create;
        NewItem.IntValue := N;
        Push(NewItem);
        continue;
      end;
      // Добавляем вещественное число в стек
      if TryStrToFloat(Token, F) then
      begin
        NewItem := TifopFloatVariant.Create;
        NewItem.FloatValue := F;
        Push(NewItem);
        continue;
      end;
      // Иначе пишем что не знаем что это за слово
      raise Exception.Create(Format('Uncnown token "%s"', [Token]));
    until false;

  finally
    FCourientTkLine := FLastLine;
    Tk.Free;
  end;
end;

procedure TIfopKernel.RegisterStdDictionarys;
begin
  IfopVariant.RegisterDictionary(Self);
  ifopMatch.RegisterDictionary(Self);
  ifopInformation.RegisterDictionary(Self);
  ifopIo.RegisterDictionary(Self);
  ifopStack.RegisterDictionary(Self);
  ifopSystem.RegisterDictionary(Self);
  ifopOle.RegisterDictionary(Self);
  ifopRegExp.RegisterDictionary(Self);
  ifopLogic.RegisterDictionary(Self);
  FDictionaryForgetLimit := FDictionary.Count;
end;

procedure TIfopKernel.SetAngleMode(const Value: TAngleMode);
begin
  FAngleMode := Value;
end;

procedure TIfopKernel.SetCourientTkLine(const Value: Integer);
begin
  FCourientTkLine := Value;
end;

procedure TIfopKernel.SetisScriptEnd(const Value: Boolean);
begin
  FisScriptEnd := Value;
end;

procedure TIfopKernel.SetStack(const index: Integer; const Value: TIfopVariant);
begin
  FStack[index] := Value;
end;

procedure TIfopKernel.SetStdErr(AData: Pointer; AProc: PStdErrProc);
begin
  FStdErrData := AData;
  FStdErrProc := @AProc;
end;

procedure TIfopKernel.SetStdOut(AData: Pointer; AProc: PStdOutProc);
begin
  FStdOutData := AData;
  FStdOutProc := @AProc;
end;

procedure TIfopKernel.stderr(AString: string; ReturnLn: Boolean = true);
begin
  if @FStdErrProc <> nil then
  begin
    FStdErrProc(FStdErrData, AString, ReturnLn);
    exit;
  end;
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
    FOREGROUND_RED or FOREGROUND_INTENSITY);
  if ReturnLn then
    writeln(AString)
  else
    Write(AString);
end;

procedure TIfopKernel.stdin(var AString: string; ReturnLn: Boolean = true);
begin
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
    FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_RED or
      FOREGROUND_INTENSITY);
  Write(AString);
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
    FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_RED);
  Readln(AString);
  if ReturnLn then
    writeln;
end;

procedure TIfopKernel.stdout(AString: string; ReturnLn: Boolean = true);
begin
  if @FStdOutProc <> nil then
  begin
    FStdOutProc(FStdOutData, AString, ReturnLn);
    exit;
  end;
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
    // FOREGROUND_BLUE or
    FOREGROUND_GREEN or
    // FOREGROUND_RED or
      FOREGROUND_INTENSITY);
  if ReturnLn then
    writeln(AString)
  else
    Write(AString);
end;

procedure TIfopKernel.AddKeyword(const AKeyword: string; AProc: Pointer);
var
  NewItem: TifopKernelDictionaryItem;
begin
  NewItem := TifopKernelDictionaryItem.Create(Self, AProc);
  NewItem.KeywordName := AKeyword;
  FDictionary.InsertObject(0, AKeyword, NewItem);
end;

procedure TIfopKernel.AddMethod(const AKeyword: string;
  AObject, AProc: Pointer; ArgsIn, ArgsOut: array of TIfopArg);
var
  NewItem: TifopKernelDictionaryItem;
begin
  NewItem := TifopKernelDictionaryItem.Create
    (Self, AObject, AProc, ArgsIn, ArgsOut);
  NewItem.KeywordName := AKeyword;
  FDictionary.InsertObject(0, AKeyword, NewItem);
end;

constructor TIfopKernel.Create;
begin
  inherited Create;
  FStack := TList.Create;
  FRetStack := TList.Create;
  FDictionary := TStringList.Create;
  FCode := TStringList.Create;
  FCommands := TList.Create;
  RegisterStdDictionarys
end;

procedure ClearListWithObjects(AList: TList); overload;
var
  i: Integer;
begin
  for i := 0 to AList.Count - 1 do
    if AList[i] <> nil then
      TObject(AList[i]).Free;
  AList.Clear;
end;

procedure ClearListWithObjects(AList: TStringList); overload;
var
  i: Integer;
begin
  for i := 0 to AList.Count - 1 do
    if Assigned(AList.Objects[i]) then
      AList.Objects[i].Free;
  AList.Clear;
end;

destructor TIfopKernel.Destroy;
begin
  if Assigned(FStack) then
  begin
    { DONE -oOnni -cGeneral : FStack.Clear }
    ClearListWithObjects(FStack);
    FStack.Free;
  end;
  if Assigned(FDictionary) then
  begin
    { DONE -oOnni -cGeneral : FDictionary.Clear }
    ClearListWithObjects(FDictionary);
    FDictionary.Free;
  end;
  if Assigned(FCode) then
  begin
    { DONE -oOnni -cGeneral : FCode.Clear }
    ClearListWithObjects(FCode);
    FCode.Free;
  end;
  if Assigned(FCommands) then
  begin
    { DONE -oOnni -cGeneral : FCommands.Clear }
    ClearListWithObjects(FCommands);
    FCommands.Free;
  end;

  if (Assigned(FRetStack)) then
  begin
    FRetStack.Free;
  end;

  inherited;
end;

function TIfopKernel.ExecuteToken(const AToken: string): Boolean;
var
  Index: Integer;
begin
  Index := FDictionary.IndexOf(AToken);
  if Index = -1 then
    exit(false);
  TifopKernelDictionaryItem(FDictionary.Objects[Index]).Execute;
  Result := true;
end;

function TIfopKernel.FloatToAngle(const Val: Double): Double;
begin
  case AngleMode of
    amDeg:
      Result := DegToRad(Val);
    amRad:
      Result := Val;
    amGrad:
      Result := GradToRad(Val);
  end;
end;

function TIfopKernel.FloatToRad(const Val: Double): Double;
begin

end;

procedure TIfopKernel.Forget(AAThom: string);
var
  index: Integer;
  i: Integer;
  item: TifopKernelDictionaryItem;
begin
  index := FDictionary.IndexOf(AAThom);
  if index = -1 then
    raise Exception.Create
      (Format('Athom "%s" not found in dictionary', [AAThom]));
  if (FDictionary.Count - index) <= FDictionaryForgetLimit then
    raise Exception.Create(Format('Can''t forget athom "%s"', [AAThom]));
  for i := 0 to index do
  begin
    item := TifopKernelDictionaryItem(FDictionary.Objects[0]);
    item.Free;
    FDictionary.Delete(0);
  end;
end;

function TIfopKernel.GetDictionary(const Index: Integer)
  : TifopKernelDictionaryItem;
begin
  Result := TifopKernelDictionaryItem(FDictionary.Objects[Index]);
end;

function TIfopKernel.GetDictionarySize: Integer;
begin
  Result := FDictionary.Count;
end;

function TIfopKernel.GetRet: Integer;
begin
  Result := Integer(FRetStack[0]);
end;

function TIfopKernel.GetStack(const index: Integer): TIfopVariant;
begin
  Result := FStack[Index];
end;

function TIfopKernel.GetStackSize: Integer;
begin
  Result := FStack.Count;
end;

function TIfopKernel.Pop: TIfopVariant;
begin
  if FStack.Count = 0 then
    raise Exception.Create('Stack is empty');
  Result := FStack[0];
  FStack.Delete(0);
end;

function TIfopKernel.PopFloat: Double;
var
  v: TIfopVariant;
begin
  v := Pop;
  try
    Result := v.FloatValue;
  finally
    v.Free;
  end;
end;

function TIfopKernel.PopInt: Integer;
var
  v: TIfopVariant;
begin
  v := Pop;
  try
    Result := v.IntValue;
  finally
    v.Free;
  end;
end;

function TIfopKernel.PopRet: Integer;
begin
  Result := Integer(FRetStack[0]);
  FRetStack.Delete(0);
end;

function TIfopKernel.PopStr: string;
var
  v: TIfopVariant;
begin
  v := Pop;
  try
    Result := v.StrValue;
  finally
    v.Free;
  end;
end;

procedure TIfopKernel.Push(const AItem: TIfopVariant);
begin
  FStack.Insert(0, AItem);
end;

procedure TIfopKernel.PushFloat(const Val: Double);
var
  v: TIfopVariant;
begin
  try
    v := TifopFloatVariant.Create;
    v.FloatValue := Val;
  finally
    Push(v);
  end;
end;

procedure TIfopKernel.PushInt(const Val: Integer);
var
  v: TIfopVariant;
begin
  try
    v := TifopIntegerVariant.Create;
    v.IntValue := Val;
  finally
    Push(v);
  end;
end;

procedure TIfopKernel.PushRet(const Addr: Integer);
begin
  FRetStack.Insert(0, Pointer(Addr));
end;

procedure TIfopKernel.PushStr(const Val: string);
var
  v: TIfopVariant;
begin
  try
    v := TifopStringVariant.Create;
    v.StrValue := Val;
  finally
    Push(v);
  end;
end;

{ TKernelDictionaryItem }

constructor TifopKernelDictionaryItem.Create(AKernel: TIfopKernel;
  AProcedure: Pointer);
begin
  inherited Create;
  FKernel := AKernel;
  FItemType := itProcedure;
  FProcedure := AProcedure;
end;

constructor TifopKernelDictionaryItem.Create(AKernel: TIfopKernel;
  ALine: string);
var
  SpPos: Integer;
begin
  inherited Create;
  FKernel := AKernel;
  FItemType := itKeyword;
  if ALine[1] = ':' then
    Delete(ALine, 1, 1);
  if ALine[length(ALine)] = ';' then
    Delete(ALine, length(ALine), 1);
  // SpPos := Pos(#32, ALine);
  SpPos := 1;
  repeat
    inc(SpPos);
    if ALine[SpPos] in [#9, #10, #13, #32] then
      break;
  until SpPos = length(ALine);

  if (SpPos = 0) or (SpPos = length(ALine)) then
    raise Exception.Create('Empty athom');
  FKeywordName := Copy(ALine, 1, SpPos - 1);
  Delete(ALine, 1, SpPos);
  FLine := ALine;
end;

constructor TifopKernelDictionaryItem.Create(AKernel: TIfopKernel;
  AObject, AProc: Pointer; ArgIn, ArgOut: array of TIfopArg);
begin
  FMethod := AProc;
  FMethodObject := AObject;
  FItemType := itNativeMethod;
end;

procedure TifopKernelDictionaryItem.Execute;
var
  obj: Pointer;
  proc: Pointer;
begin
  case ItemType of
    itProcedure:
      begin
        FProcedure(FKernel);
      end;
    itKeyword:
      begin
        FKernel.AddCode(Line);
      end;
    itNativeMethod:
      begin

      end;
  end;

end;

procedure TifopKernelDictionaryItem.SetKeywordName(const Value: string);
begin
  FKeywordName := Value;
end;

end.
