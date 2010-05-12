unit eceApplication;

interface

uses
  Windows,
  Messages,
  ComObj,
  ActiveX,
  SysUtils,
  eceDocument,
  zeList,
  ScriptKernal,
  activescp,
  Scripts,
  iece;

type
  TEceApplication = Class(TInterfacedObject, IEceApplication, IDispatch)
  private
    ISelf : IEceApplication;
    FHandle : HWND;
    FDocuments : TList;

    ScriptKernal : TScriptKernal;

    function GetExeName : string; stdcall;
    function GetHandle : HWND; stdcall;
    function GetTitle : string; stdcall;

    function GetDocumentsCount : integer; stdcall;
    function GetDocuments(index : integer) : IEceDocument; stdcall;
    function GetActiveDocument : IEceDocument; stdcall;
  {IDispatch}
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
  private
    Procedure Resize;
  protected

  public
    constructor Create;
    destructor Destroy; override;
    property Handle : Hwnd read GetHandle;

    property DocumentsCount : integer read GetDocumentsCount;
    function AddDocument(FileName : String) : BOOL; stdcall;
    property ActiveDpcument : IEceDocument read GetActiveDocument;

    procedure Minimize; stdcall;
    procedure Maximize; stdcall;
    procedure Restore; stdcall;
  end;

procedure MessagesProcess;

implementation

procedure MessagesProcess;
var
  msg : TMsg;
begin
  while GetMessage(msg, 0, 0, 0) do
  begin
    TranslateMessage(msg);
    DispatchMessage(msg);
  end;
end;

{ TEceApplication }

function TEceApplication.AddDocument(FileName: String): BOOL;
var
  Doc : TEceDocument;
begin
  Doc := TEceDocument.Create(self);
  FDocuments.Add(Doc);
end;

constructor TEceApplication.Create;
begin
  inherited Create;
  ISelf := Self;
  FDocuments := TList.Create;
  FHandle := CreateWindowEx(WS_EX_APPWINDOW,
                            WC_ECEAPPLICATION,
                            'ece',
                            WS_OVERLAPPEDWINDOW or WS_VISIBLE,
                            CW_USEDEFAULT,
                            CW_USEDEFAULT,
                            CW_USEDEFAULT,
                            CW_USEDEFAULT,
                            0,
                            0,
                            HInstance,
                            nil);
  SetWindowLong(Handle, 0, integer(self));

  ScriptKernal := TScriptKernal.Create(slVBScript);
  ScriptKernal.Window := GetDesktopWindow;

  ScriptKernal.Execute('')
end;

destructor TEceApplication.Destroy;
begin
  ScriptKernal.Free;

  FDocuments.Free;

  SetWindowLong(Handle, 0, 0);
  DestroyWindow(Handle);

  ISelf := nil;
  inherited;
end;

function TEceApplication.GetActiveDocument: IEceDocument;
begin
  if DocumentsCount >= 1 then
  Result := GetDocuments(0)
  else
  Result := nil;
end;

function TEceApplication.GetDocuments(index: integer): IEceDocument;
var
  Doc : TEceDocument;
begin
  if (index < 0)or(index > FDocuments.Count - 1) then exit(nil);
  Doc := TEceDocument(FDocuments[index]);
  Result := Doc;
end;

function TEceApplication.GetDocumentsCount: integer;
begin
  Result := FDocuments.Count;
end;

function TEceApplication.GetExeName: string;
begin
  Result := ParamStr(0);
end;

function TEceApplication.GetHandle: HWND;
begin
  Result := FHandle;
end;

////////////////////////////////////////////////////////////////////////////////

function TEceApplication.GetIDsOfNames(const IID: TGUID; Names: Pointer;
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
      if UpperCase(Name) = 'BEEP' then IDs[0] := 1
      else Result := DISP_E_UNKNOWNNAME;
    end;
end;

function TEceApplication.Invoke(DispID: Integer; const IID: TGUID;
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
  if (DispID = 1) and (Flags = DISPATCH_METHOD) then
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

////////////////////////////////////////////////////////////////////////////////

function TEceApplication.GetTitle: string;
begin

end;

function TEceApplication.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Pointer(TypeInfo) := nil;
  Result := E_NOTIMPL;
end;

function TEceApplication.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Count := 0;
  Result := S_OK;
end;


procedure TEceApplication.Maximize;
begin
  ShowWindow(Handle, SW_MAXIMIZE);
end;

procedure TEceApplication.Minimize;
begin
  ShowWindow(Handle, SW_MINIMIZE);
end;

procedure TEceApplication.Resize;
var
  Rt : TRect;
  Doc : IEceDocument;
begin
  GetClientRect(Handle, Rt);
  SetWindowPos(ActiveDpcument.GetHandle, 0, 0, 0, Rt.Right, rt.Bottom, 0)
end;

procedure TEceApplication.Restore;
begin
  ShowWindow(Handle, SW_RESTORE);
end;

{$REGION 'ApplicationProc'}
function ApplicationProc(Wnd : HWND; msg : Cardinal; wParam, lParam : Integer) : Integer; stdcall;
var PaintStuct : TPaintStruct;
    Wc : TEceApplication;
begin
Wc := TEceApplication(GetWindowLong(Wnd, 0));
case msg of
	WM_CREATE : {$REGION 'WM_CREATE'}
		begin

		end; {$ENDREGION 'WM_CREATE'}
	WM_DESTROY : {$REGION 'WM_DESTROY'}
		begin
      if Wc <> nil then
      begin
        Wc._Release;
      end;
			PostQuitMessage(0);
		end; {$ENDREGION 'WM_DESTROY'}
	WM_PAINT : {$REGION 'WM_PAINT'}
		begin
			BeginPaint(Wnd, PaintStuct);

			EndPaint(Wnd, PaintStuct);
		end; {$ENDREGION 'WM_PAINT'}
  WM_SIZE : {$REGION 'WM_SIZE'}
		begin
			if Wc <> nil then Wc.Resize;
		end; {$ENDREGION 'WM_SIZE'}
	else
		begin
		  Result := DefWindowProc(Wnd, msg, wParam, lParam);
		end;
	end
end;
{$ENDREGION}

var
  Wc : TWndClass;
initialization
  ZeroMemory(@wc, SizeOf(wc));
  wc.style := CS_DBLCLKS or CS_OWNDC;
  wc.lpfnWndProc := @ApplicationProc;
  wc.cbClsExtra := 0;
  wc.cbWndExtra := 4;
  wc.hInstance := hInstance;
  wc.hIcon := LoadIcon(0, IDI_APPLICATION);
  wc.hCursor := LoadCursor(0, IDC_ARROW);
  wc.hbrBackground := COLOR_BTNFACE + 1;
  wc.lpszClassName := WC_ECEAPPLICATION;
  if not Boolean(RegisterClass(wc)) then
  begin
    MessageBox(0, PChar('Class <' + wc.lpszClassName + '> is not register'), nil, MB_ICONERROR or MB_SYSTEMMODAL);
    ExitProcess(0)
  end;
  
finalization

end.
