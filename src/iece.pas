unit iece;
{$IFDEF fpc}{$MODE delphi}{$ENDIF}
{$I EceLanguage.inc}

interface

uses
{$IFDEF forth}
  VForth,
{$ENDIF}
  Windows,
  Contnrs,
  SysUtils,
  Classes;

const
  DISPATCH_SUB = 1;
  DISPATCH_FUNCTION = 2;
  DISPATCH_GET = 3;
  DISPATCH_SET = 4;

type
  IEceDocument = interface;

  IEceApplication = interface
    function _GetHandle: HWND; safecall;
    function _GetDocumentsCount: integer; safecall;
    function _GetDocuments(AIndex: integer; var ADocument: IEceDocument)
      : integer; safecall;
    procedure _UpdateCaption; safecall;
{$IFDEF forth}
    function GetModule: IVForthModule; stdcall;
{$ENDIF}
    procedure _FocusToActiveDocument; stdcall;
  end;

  IEceDocument = interface
    function UseHotkey(ctrl, shift, alt: BOOL; key: Word): BOOL; stdcall;
    function _GetHandle: HWND; safecall;
    procedure _BeginUpdate; safecall;
    procedure _EndUpdate; safecall;
  end;

  IEceLine = interface;
  IGutter = interface;
  ICaret = interface;

  IEceEditor = interface
    function _GetHandle: HWND; safecall;
    function _GetLinesCount: integer; safecall;
    function _GetLines(AIndex: integer): IEceLine; safecall;
    function _GetGutter: IGutter; safecall;
    function _GetCaret: ICaret; safecall;
    function _AddLine: IEceLine; safecall;
    function _InsertLine(Index: integer): IEceLine; safecall;
    procedure _DeleteLine(Index: integer); safecall;
  end;

  IEceLine = interface
    function _GetText: string; safecall;
    function _SetText(Text: string): integer; safecall;
    function _GetIndex: integer; safecall;
  end;

  IGutter = interface

  end;

  ICaret = interface
    function _GetX: integer; safecall;
    function _GetY: integer; safecall;
    function _SetX(value: integer): integer; safecall;
    function _SetY(value: integer): integer; safecall;
  end;

  IEcePlugin = interface
    function Load(App: IEceApplication): boolean; safecall;
  end;

  IEceEditorPlugin = interface
    function Load(Editor: IEceEditor): boolean; safecall;
  end;

  TNameItem = class
    Name: string;
    id: integer;
  end;

  TPropArr = array of OleVariant;

  TEceInterfacedObject = class(TInterfacedObject, IDispatch)
  private
    FNamesList: TStringList;
    { FRefCount : cardinal; }
  private
    function GetTypeInfoCount(out Count: integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: integer; out TypeInfo): HResult;
      stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: integer; const IID: TGUID; LocaleID: integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult;
      stdcall;
  protected
    // Самая главная функция - обеспечивает обращение к свойствам и методам
    function InvokeName(DispID: integer; const IID: TGUID; LocaleID: integer;
      Flags: Word; Params: TPropArr; var VarResult, ExcepInfo, ArgErr: TPropArr)
      : HResult; virtual; abstract;
  public
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: LongInt; stdcall;
    function _Release: LongInt; stdcall;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterName(AName: string; AId: integer);
    function GetNameId(Const AName: string): integer;
  end;

implementation

uses
  EditorWindow;

var
  SyncObject: TRTLCriticalSection;

function TEceInterfacedObject._AddRef: LongInt;
begin
  { EnterCriticalSection(SyncObject);
    inc(FRefCount);
    LeaveCriticalSection(SyncObject) }
  Result := S_OK;
end;

function TEceInterfacedObject._Release: LongInt;
begin
  { EnterCriticalSection(SyncObject);
    if FRefCount = 0 then exit;
    dec(FRefCount);
    if FRefCount = 0 then Destroy;
    LeaveCriticalSection( SyncObject); }
  Result := S_OK;
end;

destructor TEceInterfacedObject.Destroy;
var
  i: integer;
begin
  if Assigned(FNamesList) then
  begin
    for i := 0 to FNamesList.Count - 1 do
      FNamesList.Objects[i].Free;
    FNamesList.Free;
  end;
  inherited;
end;

function TEceInterfacedObject.Invoke(DispID: integer; const IID: TGUID;
  LocaleID: integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
// type
// TPropArr = array of OleVariant;
var
  P: TPropArr absolute Params;
  R: TPropArr absolute VarResult;
  E: TPropArr absolute ExcepInfo;
  Er: TPropArr absolute ArgErr;
begin
  try
    Result := InvokeName(DispID, IID, LocaleID, Flags, P, R, E, Er)
    // Ну, пока так, а причину эксепшенов надо выяснить
      except on E: EEditorException
    do raise Exception.Create(E.Message);
  else
    // Иначе ничего =)
  end;
  { TODO -oOnni -cBug : Программа вылетает сдесь без отладки, возможно причина в
    потере каких-то ссылок на интерйейсы, тем более что так все работает: }
  // AllocConsole;
  // Writeln(p[0]);
  // Result := S_OK;
  // exit;
end;

function TEceInterfacedObject.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: integer; DispIDs: Pointer): HResult;
Type
  TStringsArr = Array of PWideChar;
  TDispArr = Array of integer;
var
  NamesArr: TStringsArr absolute Names;
  DispArr: TDispArr absolute DispIDs;
  i: integer;
  id: integer;
begin
  for i := 0 to NameCount - 1 do
  begin
    id := GetNameId(NamesArr[i]);
    // Выходим и показываем сообщение об ошибке
    if id = -1 then
      Exit(DISP_E_UNKNOWNNAME);
    // Возвращаем ID свойства
    DispArr[i] := id;
  end;
  Result := S_OK;
end;

function TEceInterfacedObject.GetNameId(const AName: string): integer;
var
  index: integer;
begin
  index := FNamesList.IndexOf(AName);
  if index = -1 then
    Exit(-1);
  Result := TNameItem(FNamesList.Objects[index]).id;
end;

function TEceInterfacedObject.GetTypeInfo(Index, LocaleID: integer;
  out TypeInfo): HResult;
begin
  Result := S_OK;
end;

function TEceInterfacedObject.GetTypeInfoCount(out Count: integer): HResult;
begin
  Result := S_OK;
end;

function TEceInterfacedObject.QueryInterface(const IID: TGUID; out Obj)
  : HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

procedure TEceInterfacedObject.RegisterName(AName: string; AId: integer);
var
  Item: TNameItem;
begin
  if not Assigned(FNamesList) then
    FNamesList := TStringList.Create;

  Item := TNameItem.Create;
  Item.Name := AName;
  Item.id := AId;
  FNamesList.AddObject(AName, Item);
  FNamesList.Sort;
  FNamesList.Sorted := true;
end;

constructor TEceInterfacedObject.Create;
begin
  inherited;
  FNamesList := TStringList.Create;
end;

initialization

{ InitializeCriticalSection(SyncObject); }
finalization

end.
