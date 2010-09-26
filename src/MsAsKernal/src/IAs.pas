unit IAs;

interface

uses
  Windows,
  Classes,
  SysUtils,
  ActiveX,
  ComObj;

type
  IActiveScriptSite = interface(IUnknown)
  function GetLCID( // Запрос языка носителя
    out plcid: LCID
  ): HResult; stdcall;
  function GetItemInfo(  // Запрос прикладного объекта
    pstrName: LPCOLESTR;      // имя объекта
    dwReturnMask: DWORD;      // запрашиваемая информация
    out ppiunkItem: IUnknown; // интерфейс объекта
    out ppti: ITypeInfo       // инфомация о типе объекта
  ): HResult; stdcall;
  function GetDocVersionString(  // Запрос версии сценария
    out pbstrVersion: WideString
  ): HResult; stdcall;
  function OnScriptTerminate(  // Уведомление о завершении
    var pvarResult: OleVariant;  // возвращаемое значение
    var pexcepinfo: EXCEPINFO    // информация об ошибке
  ): HResult; stdcall;
  function OnStateChange(  // Уведомление об изменении состояния
    ssScriptState: SCRIPTSTATE // новое состояние
  ): HResult; stdcall;
  function OnScriptError(  // Уведомление об ошибке
    const pscripterror: IActiveScriptError
  ): HResult; stdcall;
    // Начало исполнения
  function OnEnterScript: HResult; stdcall;
    // Окончание исполнения
  function OnLeaveScript: HResult; stdcall;
end;





implementation

end.
