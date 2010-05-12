unit iece;
{$IFDEF fpc}{$MODE delphi}{$ENDIF}

interface

uses
  Windows,
  Classes;

type
  IEceDocument = interface;

  IEceApplication = interface
    function _GetHandle: HWND; safecall;
    function _GetDocumentsCount : integer; safecall;
    function _GetDocuments(AIndex : Integer; var ADocument : IEceDocument) : Integer; safecall;
  end;

  IEceDocument = interface
    function _GetHandle: HWND; safecall;
  end;

  IEceLine = interface;
  IGutter = interface;
  ICaret = interface;

  IEceEditor = interface
    function _GetHandle: HWND; safecall;
    procedure _BeginUpdate ; safecall;
    procedure _EndUpdate ; safecall;
    function _GetLinesCount : Integer; safecall;
    function _GetLines(AIndex : Integer) : IEceLine; safecall;
    function _GetGutter : IGutter; safecall;
    function _GetCaret : ICaret; safecall;
    function _AddLine : IEceLine; safecall;
    function _InsertLine(Index : Integer) : IEceLine; safecall;
    procedure _DeleteLine(Index : Integer); safecall;
  end;

  IEceLine = interface
    function _GetText : string; safecall;
    function _SetText(Text : string) : Integer; safecall;
    function _GetIndex : Integer; safecall;
  end;

  IGutter = interface

  end;

  ICaret = interface
    function _GetX : Integer; safecall;
    function _GetY : Integer; safecall;
    function _SetX(value : Integer) : Integer; safecall;
    function _SetY(value : Integer) : Integer; safecall;
  end;

  IEcePlugin = interface
    function Load(App : IEceApplication) : boolean; safecall;
  end;

  IEceEditorPlugin = interface
    function Load(Editor :IEceEditor) : boolean; safecall;
  end;

  TEceInterfacedObject = class(TPersistent, IDispatch)
  private
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
  public
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: LongInt; stdcall;
    function _Release: LongInt; stdcall;
    constructor Create;
  end;

implementation

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

function TEceInterfacedObject.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
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

constructor TEceInterfacedObject.Create;
begin
  inherited;

end;

initialization

{ InitializeCriticalSection(SyncObject); }
finalization

end.
