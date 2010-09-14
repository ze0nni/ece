unit VForthMachine;

interface

{ TODO -oOnni -cGeneral : При работе с "быстрым стеком"
  неправильно удаляются атомы }
{$DEFINE QAthomStack}

uses
  VForth,
  VForthModule,
  Windows,
  SysUtils,
  Classes,
  Contnrs;

type
  EVForthMachineError = class(Exception)
  end;

  // Структура хранит
  PQStruct = ^TQStruct;

  TQStruct = record
    Time: Cardinal; // дату последнег изменения стека атомов
    Index: Integer; // Индекс вызываемого атома
    isVar: Boolean; // Признак того, что нужно искать переменную
  end;

  TVForthMachine = class(TVForthModule, IVForthMachine, IVForthModule)
  private
    // Стек переменных
    FDataStack: TInterfaceList;
    FStack: TForthStack;
    FStacks: array [TForthStack] of TInterfaceList;
    // Стек адресов возврата
    FAdressStack: TList;
    // Стек атомов
    FAthomStack: TInterfaceList;
    // Стек для быстройго доступа к атомам
{$IFDEF QAthomStack}
    FQAthomStack: TStringList;
{$ENDIF}
    // Стек переменных
    FVaribleStack: TInterfaceList;
    // Управление
    FCourientTkIndex: Integer;
    FCourientTk: TStringList;
    // IO
    FIO: IVForthIO;
    // Время последнего обновления стека атомов
    FLastAthomsUpdateTime: Cardinal;
    function GetAthom(const AAthom: String): IVForthAthom; stdcall;
    function TryGetAthom(Const AAthom: string; var obj: IVForthAthom): Boolean;
      stdcall;
{$IFDEF QAthomStack}
    function TryGetAthomQ(Const AAthom: string; var obj: IVForthAthom): Boolean;
{$ENDIF}
    function GetDataStack(const index: Integer): IVForthVariant; stdcall;
    procedure SetDataStack(const index: Integer; const Value: IVForthVariant);
      stdcall;

    class procedure ParseString(ACode: string; TkList: TStringList);
    procedure ExecuteTkList(TkList: TStringList);
    function GetCourientTkIndex: Integer; stdcall;
    procedure SetCourientTkIndex(const Value: Integer); stdcall;
    function GetTkCount: Integer; stdcall;
    function GetDataStackSize: Integer; stdcall;
    function GetAthomsCount: Integer; stdcall;
    function GetAthomByIndex(const AAthom: Integer): IVForthAthom; stdcall;
    function GetVarible(AVaribleName: string): IVForthVariant; stdcall;
    function GetStack: TForthStack; stdcall;
    procedure SetStack(const Value: TForthStack); stdcall;
  protected

  public
    constructor Create;
    destructor Destroy; override;

    procedure Register(AMachine: IVForthMachine); stdcall;

    procedure SetIo(AIO: IVForthIO); stdcall;
    function StdIn: string; stdcall;
    procedure StdOut(str: string); stdcall;
    procedure StdErr(str: string); stdcall;

    procedure LoadModule(AModule: IVForthModule); stdcall;
    procedure AddAthom(AAthom: IVForthAthom); stdcall;
    procedure AddCode(ACode: string); stdcall;

    property DataStack[const index: Integer]
      : IVForthVariant read GetDataStack write SetDataStack;
    property DataStackSize: Integer read GetDataStackSize;
    property Athom[const AAthom: String]: IVForthAthom read GetAthom;
    property AthomByIndex[const AAthom: Integer]
      : IVForthAthom read GetAthomByIndex;
    property AthomsCount: Integer read GetAthomsCount;
    procedure Forget(AAthom: string); stdcall;

    property Varible[AVaribleName: string]: IVForthVariant read GetVarible;

    property Stack: TForthStack read GetStack write SetStack;

    procedure Push(AVariant: IVForthVariant); stdcall;
    function Pop: IVForthVariant; stdcall;
    procedure PushEx(index: Integer; AVariant: IVForthVariant); stdcall;
    function PopEx(index: Integer): IVForthVariant; stdcall;
    procedure PushInt(AVariant: Integer); stdcall;
    function PopInt: Integer; stdcall;
    procedure PushFloat(AVariant: Double); stdcall;
    function PopFloat: Double; stdcall;
    procedure PushString(AVariant: string); stdcall;
    function PopString: string; stdcall;
    procedure PushNatural(AVariant1, AVariant2: Integer); stdcall;
    procedure PushComplex(AVariant1, AVariant2: Double); stdcall;
    // Переменные

    // Стек адресов
    procedure PushAddr(AValue: Integer); stdcall;
    function ReturnAddr: Integer; stdcall;
    function PopAddr: Integer; stdcall;
    property CourientTkIndex: Integer read GetCourientTkIndex write
      SetCourientTkIndex;
    property TkCount: Integer read GetTkCount;
    function GetTk(index: Integer): string; stdcall;
  end;

implementation

uses
  VForthAthom,
  VForthVariants;

type
  TVForthAthom = class(TInterfacedObject, IVForthAthom)
  private
    FTk: TStringList;
    FMachine: TVForthMachine;
    FName: string;
    FModule: IVForthModule;
    function GetName: String; stdcall;
    function GetModule: IVForthModule; stdcall;
  public
    destructor Destroy; override;
    property Name: string read GetName;
    property Module: IVForthModule read GetModule;
    procedure Execute(AMachine: IVForthMachine; PAthomStr: PWideChar); stdcall;
  end;

resourcestring
  StrStackIsEmpty = 'Stack is empty';
  StrAthomSNotFound = 'Athom "%s" not found.';
  StrStackItemDOutOf = 'Stack item (%d) out of range.';

  { TVForthMachine }

const
  // Токен не определен
  TK_NULL = TObject(0);
  // Переменные
  TK_INTEGER = TObject(1);
  TK_FLOAT = TObject(2);
  TK_NATURAL = TObject(3);
  TK_COMPLEX = TObject(4);
  TK_STRING = TObject(5);
  // Атом
  TK_ATHOM = TObject(6);
  // Объявление нового атома
  TK_NEWATHOM = TObject(7);
  // Переменная
  TK_VARIABLE = TObject(8);

const
  SpaceChars = [#9, #10, #13, #32];

type
  TTokenType = (tkNull = Integer(TK_NULL), tkInteger = Integer(TK_INTEGER),
    tkFloat = Integer(TK_FLOAT), tkNatural = Integer(TK_NATURAL),
    tkComplex = Integer(TK_COMPLEX), tkString = Integer(TK_STRING),
    tkAthom = Integer(TK_ATHOM), tkNewAthom = Integer(TK_NEWATHOM),
    tkVariable = Integer(TK_VARIABLE));

class procedure TVForthMachine.ParseString(ACode: string; TkList: TStringList);

var
  SChar, CChar, EChar: PChar;
{$REGION 'Процедуры для парсинга'}
  procedure ScanForSpaces;
  begin
    repeat
      inc(CChar)
    until (not(CChar^ in SpaceChars)) or (CChar = EChar);
  end;

  function ScanForString: string;
  begin
    SChar := CChar;
    repeat
      inc(CChar);
      { TODO -oOnni -cGeneral : Вложенные ::;; }
    until (CChar^ = '"') or (CChar = EChar);
    // Первый и последний символы не входят в состав
    Result := Copy(SChar, 2, CChar - SChar - 1);
    inc(CChar);
  end;

  function ScanForNewAthom: string;
  begin
    SChar := CChar;
    repeat
      inc(CChar);
      { TODO -oOnni -cGeneral : Вложенные ::;; }
    until (CChar^ = ';') or (CChar = EChar);
    // Первый и последний символы не входят в состав
    Result := Copy(SChar, 2, CChar - SChar - 1);
    inc(CChar);
  end;

  function ScanForComments: string;
  begin
    SChar := CChar;
    repeat
      inc(CChar);
      // комметрий заканчивается на #32')'
    until ((CChar^ = ')') and ((CChar - 1)^ in SpaceChars)) or (CChar = EChar);
    // Первый и последний символы не входят в состав
    Result := Copy(SChar, 2, CChar - SChar - 1);
    inc(CChar);
  end;

  function ScanForAthom: string;
  begin
    SChar := CChar;
    repeat
      inc(CChar)
    until (CChar^ in SpaceChars) or (CChar = EChar);
    Result := Copy(SChar, 1, CChar - SChar);
    inc(CChar); // пропускаем пробел за нами
  end;

  procedure ScanForAthomAndAdd;
  var
    Tk: String;
    n: Integer;
    f: Double;
  begin
    Tk := ScanForAthom;
    // Проверка на целое
    if TryStrToInt(Tk, n) then
      TkList.AddObject(Tk, TK_INTEGER)
    else
    // Проверка на вещественное
      if TryStrToFloat(Tk, f) then
      TkList.AddObject(Tk, TK_FLOAT)
    else
      // Ни чего не подошло? значит просто атом
      TkList.AddObject(Tk, TK_ATHOM)
  end;
{$ENDREGION}

begin
  SChar := PChar(ACode);
  CChar := SChar;
  EChar := SChar + Length(ACode);
  while (CChar < EChar) do
  begin
    case CChar^ of
      // пропускаем пробелы
      #9, #10, #13, #32:
        begin
          ScanForSpaces;
          continue;
        end;
      // Ищем строки
      '"':
        begin
          TkList.AddObject(ScanForString, TK_STRING);
          continue;
        end;
      // Ищем объявления атомов
      ':':
        begin
          TkList.AddObject(ScanForNewAthom, TK_NEWATHOM);
          continue;
        end;
      // Ищем комментарии
      '(':
        begin
          // комметрий начинается с '('#32
          if (CChar < EChar - 1) and ((CChar + 1)^ in SpaceChars) then
            ScanForComments
          else
            // Если нет, то ищем атомы
            ScanForAthomAndAdd;
          continue;
        end;
    else
      // Если ни чего не нащли ищем просто атомы (или то что можно положить в стек)
      begin
        ScanForAthomAndAdd
        // continue
      end;
    end;
  end
end;

procedure TVForthMachine.AddAthom(AAthom: IVForthAthom);
var
  index: Integer;
  pI: ^IInterface;
begin
  FAthomStack.Insert(0, AAthom);
  // Работа с "быстрым стеком"
{$IFDEF QAthomStack}
  index := FQAthomStack.IndexOf(AAthom.Name);
  if index = -1 then
  begin
    new(pI);
    pI^ := AAthom;
    FQAthomStack.AddObject(AAthom.Name, TObject(pI));
    FQAthomStack.Sort;
    FQAthomStack.Sorted := true;
    // Обновляем
    FLastAthomsUpdateTime := GetTickCount;
  end
  else
  begin
    pI := Pointer(FQAthomStack.Objects[index]);
    pI^ := AAthom;
  end;
{$ENDIF}
end;

procedure TVForthMachine.AddCode(ACode: string);
var
  TkList: TStringList;
begin
  try
    TkList := TStringList.Create;
    ParseString(ACode, TkList);
    ExecuteTkList(TkList);
  finally
    TkList.Free;
  end;
end;

procedure TVForthMachine.ExecuteTkList(TkList: TStringList);
var
  i: Integer;
  TkLine: string;
  NewAthom: TVForthAthom;
  NewTkName: String;
  NewTkCode: String;
  SpPos: Integer;
  FLastTkIndex: Integer;
  A: IVForthAthom;
begin
  FLastTkIndex := FCourientTkIndex;
  FCourientTk := TkList;
  i := -1;
  while i < TkList.Count - 1 do
  begin
    inc(i);
    FCourientTkIndex := i; // Ветвление и циклы
    TkLine := TkList[i];
    case TTokenType(TkList.Objects[i]) of
      tkNull:
        raise EVForthMachineError.Create('Unknown token');
      tkInteger:
        PushInt(StrToInt(TkLine));
      tkFloat:
        PushFloat(StrToFloat(TkLine));
      tkNatural:
        ;
      tkComplex:
        ;
      tkString:
        PushString(TkLine);
      tkAthom:
        begin
{$IFDEF QAthomStack}
          if TryGetAthomQ(TkLine, A) then
{$ELSE}
            if TryGetAthom(TkLine, A) then
{$ENDIF}
            begin
              A.Execute(Self, PWideChar(TkLine))
            end
            else
            begin
              Push(Varible[TkLine]);
            end;
          i := FCourientTkIndex; // Ветвление и циклы
        end;
      tkNewAthom:
        begin
{$REGION 'Регистрируем новое слово'}
          // тут мы добавляем новый и новый TStringList, будучу уже отпарсеным
          // он выполняется на порядок быстрее
          SpPos := 0;
          repeat
            inc(SpPos)
          until (TkLine[SpPos] in SpaceChars) or (SpPos = Length(TkLine) + 1);
          NewTkName := Copy(TkLine, 1, SpPos - 1);
          Delete(TkLine, 1, SpPos);
          NewTkCode := TkLine;

          NewAthom := TVForthAthom.Create;
          NewAthom.FMachine := Self;
          NewAthom.FTk := TStringList.Create;
          ParseString(TkLine, NewAthom.FTk);
          NewAthom.FName := NewTkName;
          AddAthom(NewAthom);
{$ENDREGION}
        end;
    else
      { TODO -oOnni -cGeneral : NewAthom }
      begin
      end;
    end;
  end;
  FCourientTkIndex := FLastTkIndex;
end;

procedure TVForthMachine.Forget(AAthom: string);
var
  i: Integer;
  A: IVForthAthom;
  AName: string;
  j: Integer;
  index: Integer;
  pI: ^IInterface;
begin
  for i := 0 to FAthomStack.Count - 1 do
  begin
    A := GetAthomByIndex(i);
    AName := A.Name;
    if Windows.CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, PChar(AAthom)
        , Length(AAthom), PChar(AName), Length(AName)) = CSTR_EQUAL then
    begin
      // Забываем все что было после
      for j := 0 to i do
      begin
{$IFDEF QAthomStack}
        // Удаляем из "Быстрого" стека
        index := FQAthomStack.IndexOf(AthomByIndex[0].Name);
        if index <> -1 then
        begin
          pI := Pointer(FQAthomStack.Objects[index]);
          pI^ := nil;
          // FQAthomStack.Delete(index);
        end;
{$ENDIF}
        // Удаляем из словаря
        FAthomStack.Delete(0);
{$IFDEF QAthomStack}
        // Ищем ставим предыдущий атом
        if index <> -1 then
        begin
          if TryGetAthom(AAthom, IVForthAthom(pI^)) then
          begin
            FQAthomStack.Objects[index] := TObject(pI);
          end
          else
          begin
            Dispose(pI);
            FQAthomStack.Delete(index);
            // Обновляем
            FLastAthomsUpdateTime := GetTickCount;
          end;
        end;
{$ENDIF}
      end;
      // сортируем
{$IFDEF QAthomStack}
      FQAthomStack.Sort;
      FQAthomStack.Sorted := true;
{$ENDIF}
      // Выходим
      exit;
    end;
  end;
  // Ошибка же
  raise EVForthMachineError.CreateFmt('Can''t forget athom "%s"', [AAthom]);
end;

constructor TVForthMachine.Create;
var
  s: TForthStack;
begin
  inherited;
  for s := low(s) to high(s) do
    FStacks[s] := TInterfaceList.Create;

  Stack := fsUser; // FDataStack := FStacks[fsUser]
  FAdressStack := TList.Create;
  FAthomStack := TInterfaceList.Create;
{$IFDEF QAthomStack}
  FQAthomStack := TStringList.Create;
{$ENDIF}
  FVaribleStack := TInterfaceList.Create;
end;

destructor TVForthMachine.Destroy;
var
  s: TForthStack;
  i: Integer;
  pI: ^IInterface;
begin
  if Assigned(FVaribleStack) then
    FVaribleStack.Free;
  if Assigned(FAthomStack) then
    FAthomStack.Free;
{$IFDEF QAthomStack}
  if Assigned(FQAthomStack) then
  begin
    { DONE -oOnni -cGeneral : FQAthomStack.Clear }
    for i := 0 to FQAthomStack.Count - 1 do
    begin
      pI := Pointer(FQAthomStack[i]);
      pI^ := nil;
      Dispose(pI);
    end;
    FQAthomStack.Clear;
    FQAthomStack.Free;
  end;
{$ENDIF}
  if Assigned(FAdressStack) then
    FAdressStack.Free;
  FDataStack := nil;
  for s := low(s) to high(s) do
    if Assigned(FStacks[s]) then
      FStacks[s].Free;
  inherited;
end;

function TVForthMachine.GetAthom(const AAthom: String): IVForthAthom;
var
  Athom: IVForthAthom;
  i: Integer;
  len: Integer;
  AName: String;
begin
  if not TryGetAthom(AAthom, Result) then

    raise EVForthMachineError.CreateFmt(StrAthomSNotFound, [AAthom]);
end;

function TVForthMachine.TryGetAthom(const AAthom: string; var obj: IVForthAthom)
  : Boolean;
var
  Athom: IVForthAthom;
  i: Integer;
  len: Integer;
  AName: String;
begin
  len := Length(AAthom);
  for i := 0 to FAthomStack.Count - 1 do
  begin
    Athom := IVForthAthom(FAthomStack[i]);
    AName := Athom.Name;
    if Windows.CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, PChar(AAthom)
        , len, PChar(AName), Length(AName)) = CSTR_EQUAL then
    begin
      obj := Athom;
      exit(true);
    end;
  end;
  exit(false);
end;
{$IFDEF QAthomStack}

function TVForthMachine.TryGetAthomQ(const AAthom: string;
  var obj: IVForthAthom): Boolean;
var
  Athom: IVForthAthom;
  index: Integer;
  pI: ^IInterface;
begin
  index := FQAthomStack.IndexOf(AAthom);
  if index = -1 then
    exit(false)
  else
  begin
    pI := Pointer(FQAthomStack.Objects[index]);
    obj := IVForthAthom(pI^);
    Result := true;
  end;
end;
{$ENDIF}

function TVForthMachine.GetAthomByIndex(const AAthom: Integer): IVForthAthom;
begin
  Result := IVForthAthom(FAthomStack[AAthom]);
end;

function TVForthMachine.GetAthomsCount: Integer;
begin
  Result := FAthomStack.Count;
end;

function TVForthMachine.GetCourientTkIndex: Integer;
begin
  Result := FCourientTkIndex;
end;

function TVForthMachine.GetDataStack(const index: Integer): IVForthVariant;
begin
  if (index >= FDataStack.Count) or (index < 0) then
    raise EVForthMachineError.CreateFmt(StrStackItemDOutOf, [index]);
  Result := IVForthVariant(FDataStack[index]);
end;

function TVForthMachine.GetDataStackSize: Integer;
begin
  Result := FDataStack.Count;
end;

function TVForthMachine.GetStack: TForthStack;
begin
  Result := FStack;
end;

function TVForthMachine.GetTk(index: Integer): string;
begin
  Result := FCourientTk[index];
end;

function TVForthMachine.GetTkCount: Integer;
begin
  Result := FCourientTk.Count;
end;

function TVForthMachine.GetVarible(AVaribleName: string): IVForthVariant;
var
  i: Integer;
  v: IVForthVariant;
  Ln: Integer;
  Vname: string;
  VLen: Integer;
begin
  Ln := Length(AVaribleName);
  for i := 0 to FDataStack.Count - 1 do
  begin
    v := DataStack[i];
    Vname := v.Name;
    VLen := Length(Vname);
    if VLen = 0 then
      continue;
    if Windows.CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, PChar
        (AVaribleName), Ln, PChar(Vname), VLen) = CSTR_EQUAL then
    begin
      exit(v);
    end;
  end;
  raise EVForthMachineError.CreateFmt
    ('Variable "%s" not found', [AVaribleName]);
end;

procedure TVForthMachine.LoadModule(AModule: IVForthModule);
begin
  AModule.Register(Self);
end;

function TVForthMachine.Pop: IVForthVariant;
begin
  if FDataStack.Count = 0 then
    raise EVForthMachineError.Create(StrStackIsEmpty);
  Result := IVForthVariant(FDataStack[0]);
  FDataStack.Delete(0);
end;

function TVForthMachine.PopAddr: Integer;
begin
  Result := ReturnAddr;
  FAdressStack.Delete(0);
end;

function TVForthMachine.PopEx(index: Integer): IVForthVariant;
begin
  Result := DataStack[index];
  FDataStack.Delete(index);
end;

function TVForthMachine.PopFloat: Double;
begin
  Result := Pop.FloatValue;
end;

function TVForthMachine.PopInt: Integer;
begin
  Result := Pop.IntValue;
end;

function TVForthMachine.PopString: string;
begin
  Result := Pop.StringValue;
end;

procedure TVForthMachine.Push(AVariant: IVForthVariant);
begin
  FDataStack.Insert(0, AVariant);
end;

procedure TVForthMachine.PushAddr(AValue: Integer);
begin
  FAdressStack.Insert(0, Pointer(AValue));
end;

procedure TVForthMachine.PushComplex(AVariant1, AVariant2: Double);
begin
  Push(CreateComplexVariant(AVariant1, AVariant2));
end;

procedure TVForthMachine.PushEx(index: Integer; AVariant: IVForthVariant);
begin
  if (index > FDataStack.Count) or (index < 0) then
    raise EVForthMachineError.CreateFmt(StrStackItemDOutOf, [index]);
  FDataStack.Insert(index, AVariant);
end;

procedure TVForthMachine.PushFloat(AVariant: Double);
begin
  Push(CreateFloatVariant(AVariant));
end;

procedure TVForthMachine.PushInt(AVariant: Integer);
begin
  Push(CreateIntegerVariant(AVariant));
end;

procedure TVForthMachine.PushNatural(AVariant1, AVariant2: Integer);
begin
  Push(CreateNaturalVariant(AVariant1, AVariant2));
end;

procedure TVForthMachine.PushString(AVariant: string);
begin
  Push(CreateStringVariant(AVariant));
end;

procedure TVForthMachine.Register(AMachine: IVForthMachine);
begin

end;

function TVForthMachine.ReturnAddr: Integer;
begin
  if FAdressStack.Count = 0 then
    raise EVForthMachineError.Create('Address stack is empty.');
  Result := Integer(FAdressStack[0]);
end;

procedure TVForthMachine.SetCourientTkIndex(const Value: Integer);
begin
  FCourientTkIndex := Value;
end;

procedure TVForthMachine.SetDataStack(const index: Integer;
  const Value: IVForthVariant);
begin
  if (index >= FDataStack.Count) or (index < 0) then
    raise EVForthMachineError.CreateFmt(StrStackItemDOutOf, [index]);
  FDataStack[index] := Value;
end;

procedure TVForthMachine.SetIo(AIO: IVForthIO);
begin
  FIO := AIO;
end;

procedure TVForthMachine.SetStack(const Value: TForthStack);
begin
  if Value in [ low(TForthStack) .. High(TForthStack)] then
  begin
    FDataStack := FStacks[Value];
    FStack := Value;
  end
  else
    raise EVForthMachineError.CreateFmt
      ('Bad stack index (%d)', [Integer(Value)]);
end;

procedure TVForthMachine.StdErr(str: string);
begin
  try
    FIO.StdErr(str);
  except
    AllocConsole;
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
      FOREGROUND_RED or FOREGROUND_INTENSITY);
    Write(str);
  end;
end;

function TVForthMachine.StdIn: string;
var
  s: string;
begin
  try
    Result := FIO.StdIn;
  except
    AllocConsole;
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
      FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_RED or
        FOREGROUND_INTENSITY);
    write('> ');
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
      FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_RED);
    Readln(s);
    Result := s;
  end;
end;

procedure TVForthMachine.StdOut(str: string);
begin
  try
    FIO.StdOut(str);
  except
    AllocConsole;
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
      FOREGROUND_GREEN or FOREGROUND_INTENSITY);
    Write(str);
  end;
end;

{ TVForthAthom }

destructor TVForthAthom.Destroy;
begin
  if Assigned(FTk) then
    FTk.Free;
  inherited;
end;

procedure TVForthAthom.Execute(AMachine: IVForthMachine; PAthomStr: PWideChar);
begin
  FMachine.ExecuteTkList(FTk);
end;

function TVForthAthom.GetModule: IVForthModule;
begin
  Result := FModule;
end;

function TVForthAthom.GetName: String;
begin
  Result := FName;
end;

end.
