Program scrtest;

{$APPTYPE CONSOLE}
{$ifdef fpc}{$MODE delphi}{$endif}

uses
  Classes,
  SysUtils,
  Windows,
  ComObj,
  TypInfo;

type
  PSetStrProp = ^TSetStrProp;
  TSetStrProp =  procedure (sender : TObject; const Value: string);

  TEceInterfacedObject = class(TInterfacedObject, IDispatch)
  private
    FB: string;
    FC: string;
    FA: string;
    procedure SetTextToAdd(const Value: String);
    procedure SetA(const Value: string);
    procedure SetB(const Value: string);
    procedure SetC(const Value: string);
    { FRefCount : cardinal; }
  public
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult;
      stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult;
      stdcall;
    constructor Create;
  published
    property A : string read FA write SetA;
    property B : string read FB write SetB;
    property C : string read FC write SetC;
  end;

{ TEceInterfacedObject }

constructor TEceInterfacedObject.Create;
begin

end;

function TEceInterfacedObject.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
var
   S: PWideChar;
   pif: PPropInfo;
begin
   // Получаем имя функции или свойства
   S := PWideChar(Names^);
   pif := GetPropInfo(Self.ClassInfo, s);
   if pif <> nil then
     PInteger(DispIDs)^ := Integer(pif^.SetProc);
   Result := S_OK;
end;

function TEceInterfacedObject.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Result := S_OK;
end;

function TEceInterfacedObject.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Result := S_OK;
end;

function TEceInterfacedObject.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
type
  PParams = ^TParams;
  TParams = array of variant;
var
  Pars : PParams;
  SetPropProc : TSetStrProp;
begin
  Result := S_OK;
  SetPropProc := Pointer(DispID);
  Pars := @Params;
  SetPropProc(self, pars^[0]);
  //Вот тут то и происходит вызов методов по индексу
end;

procedure TEceInterfacedObject.SetA(const Value: string);
begin
  FA := Value;
end;

procedure TEceInterfacedObject.SetB(const Value: string);
begin
  FB := Value;
end;

procedure TEceInterfacedObject.SetC(const Value: string);
begin
  FC := Value;
end;

procedure TEceInterfacedObject.SetTextToAdd(const Value: String);
begin
  Write(Value);
end;

var
  sc : OleVariant;
  obj : TEceInterfacedObject;
begin
  CoInitializeEx(nil, 0);
  sc := CreateOleObject('MSScriptControl.ScriptControl');
  sc.Language := 'VBScript';

  obj := TEceInterfacedObject.Create;
  sc.AddObject('obj', obj as IDispatch, false);

  //Вызываем методы
  sc.AddCode('obj.A = "Value 1"');
  sc.AddCode('obj.B = "Value 2"');
  sc.AddCode('obj.C = "Value 3"');
  //Читаем значения
  Writeln(obj.A);
  Writeln(obj.B);
  Writeln(obj.C);

  readln;
end.
