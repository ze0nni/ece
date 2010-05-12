unit eceDocument;

interface

uses
  Windows,
  Messages,
  eceLines,
  Iece;

type
  TEceDocument = class(TInterfacedObject, IEceDocument, IEceTextProcessor)
  private
    ISelf : IEceDocument;
    FHandle : HWND;
    FApplication : IEceApplication;
    Flines : TEceLines;
  {IEceDocument}
    function GetFileName : string; stdcall;
    function GetHandle : HWND; stdcall;
  {IEceTextProcessor}
    Function GetLinesCount : integer; stdcall;
    Function GetLines(index : integer) : IEceLine; stdcall;
  public
    Constructor Create(AApplication : IEceApplication);
    Destructor Destroy; override;

    Property Handle : HWND Read FHandle;
  end;

implementation

{ TEceDocument }

constructor TEceDocument.Create(AApplication : IEceApplication);
begin
  Inherited Create;
  ISelf := self;
  Flines := TEceLines.Create;

  FApplication := AApplication;
  FHandle := CreateWindowEx(WS_EX_CLIENTEDGE,
                            WC_ECEDOCUMENT,
                            '*',
                            WS_VISIBLE or WS_CHILD,
                            0,
                            0,
                            0,
                            0,
                            FApplication.GetHandle,
                            0,
                            HInstance,
                            nil);
end;

destructor TEceDocument.Destroy;
begin
  SetWindowLong(Handle, 0, 0);
  DestroyWindow(Handle);

  Flines.Free;
  ISelf := nil;
  inherited;
end;

function TEceDocument.GetFileName: string;
begin

end;

function TEceDocument.GetHandle: HWND;
begin
  Result := FHandle;
end;

function TEceDocument.GetLines(index: integer): IEceLine;
begin
  {TODO :  }  
end;

function TEceDocument.GetLinesCount: integer;
begin
    Result := Flines.Count;
end;

{$REGION 'DocumentProc'}
function DocumentProc(Wnd : HWND; msg : Cardinal; wParam, lParam : Integer) : Integer; stdcall;
var PaintStuct : TPaintStruct;
    Wc : TEceDocument;
begin
Wc := TEceDocument(GetWindowLong(Wnd, 0));
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
		end; {$ENDREGION 'WM_DESTROY'}
//	WM_PAINT : {$REGION 'WM_PAINT'}
//		begin
//			BeginPaint(Wnd, PaintStuct);
//
//			EndPaint(Wnd, PaintStuct);
//		end; {$ENDREGION 'WM_PAINT'}
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
  wc.lpfnWndProc := @DocumentProc;
  wc.cbClsExtra := 0;
  wc.cbWndExtra := 4;
  wc.hInstance := hInstance;
  wc.hIcon := LoadIcon(0, IDI_APPLICATION);
  wc.hCursor := LoadCursor(0, IDC_ARROW);
  wc.hbrBackground := COLOR_WINDOW + 1;
  wc.lpszClassName := WC_ECEDOCUMENT;
  if not Boolean(RegisterClass(wc)) then
  begin
    MessageBox(0, PChar('Class <' + wc.lpszClassName + '> is not register'), nil, MB_ICONERROR or MB_SYSTEMMODAL);
    ExitProcess(0)
  end;
  
finalization

end.
