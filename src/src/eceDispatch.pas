unit eceDispatch;

interface

uses
  ComObj,
  ActiveX,
  SysUtils,
  Windows;

Type
  TEceIDispatch = class(TInterfacedObject, IDispatch)
  private
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
  end;

implementation

{ TEceIDispatch }

function TEceIDispatch.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
type
  TDispIDsArray = array[0..0] of TDISPID;
  PDispIDsArray = ^TDispIDsArray;
var
  IDs: PDispIDsArray absolute DispIDs;
  i: integer;
  Name: WideString;
begin
  // Не поддерживаем именованные аргументы
  if NameCount > 1 then Result := DISP_E_UNKNOWNNAME
  else
    if NameCount < 1 then Result := E_INVALIDARG
    else Result := S_OK;
  for i := 0 to NameCount - 1 do
    IDs[i] := DISPID_UNKNOWN;
  if NameCount = 1 then
    begin
      Name := PWideChar(Names^);
      if UpperCase(Name) = 'PRINT' then IDs[0] := 1
      else Result := DISP_E_UNKNOWNNAME;
    end;
end;

function TEceIDispatch.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Pointer(TypeInfo) := nil;
  Result := E_NOTIMPL;
end;

function TEceIDispatch.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Count := 0;
  Result := S_OK;
end;

function TEceIDispatch.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
const // этих констант нет в модулях Delphi
  VARIANT_ALPHABOOL = 2;  // Лог. значения представлять литералами
  VARIANT_LOCALBOOL = 16; // Лог. литералы на местном языке
var
  P: TDISPPARAMS absolute Params;
  i: integer;
  S: string;
  V: OleVariant;
begin
  if (DispID = 1{DISPID_PRINT}) and (Flags = DISPATCH_METHOD) then
    begin
      S := '';
      // Параметры в массиве в обратном порядке!
      for i := P.cArgs - 1 downto 0 do
        begin
          // Преобразуем параметр в строку
          Result := VariantChangeType(V, OleVariant(P.rgvarg[i]),
                VARIANT_ALPHABOOL or VARIANT_LOCALBOOL, VT_BSTR);
          // Ошибку преобразования вернем как ошибку метода
          if Result <> S_OK then exit;
          if S <> '' then S := S + ' ';
          S := S + V;
        end;
      MessageBeep(0);
      Result := S_OK;
    end
  else Result := DISP_E_MEMBERNOTFOUND;

end;

end.
