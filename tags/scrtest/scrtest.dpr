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
  TEceInterfacedObject = class(TInterfacedObject, IDispatch)
  private
    procedure SetTextToAdd(const Value: String);
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
    property TextToAdd : String Write SetTextToAdd;
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
     PInteger(DispIDs)^ := pif^.Index;
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
begin
  Result := S_OK;
  //Вот тут то и происходит вызов методов по индексу
  Pars := @Params;
  Writeln(pars^[0], #9, DispID);
end;

procedure TEceInterfacedObject.SetTextToAdd(const Value: String);
begin
  Write(Value);
end;

var
  sc : OleVariant;
  obj : IDispatch;
begin
  CoInitializeEx(nil, 0);
  sc := CreateOleObject('MSScriptControl.ScriptControl');
  sc.Language := 'VBScript';

  obj := TEceInterfacedObject.Create;
  sc.AddObject('obj', obj, false);

  sc.AddCode('obj.TextToAdd = "Hello!"');
  sc.AddCode('obj.SetTextToAdd "Hello!", "World!"');
  readln;
end.
