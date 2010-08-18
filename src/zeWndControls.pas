// ************************************************************
//
// Windows Controls
// Copyright (c) 2010  zeDevel
//
// �����������: ���������� �������  ze0nni@gmail.com
//
// ************************************************************
// ������ ��� �������� ������� ���� �� ������ ������� OP
// ************************************************************

unit zeWndControls;
{$IFDEF fpc}{$MODE delphi}{$ENDIF}

interface

uses
  Windows,
  Messages,
  Classes,
  SysUtils,
  // zeError,
  Iece;

type
  TScrollBar = class;

  TzeWndControl = class(TEceInterfacedObject)
  private
    FHandle: Cardinal;
    FVScroll: TScrollBar;
    FHScroll: TScrollBar;
    FIsDestroy: Boolean;
  protected
    procedure CreateParams(var Param: CreateStruct); virtual;
    procedure DefaultHandler(var Message); override;

    property VScroll: TScrollBar read FVScroll;
    property HScroll: TScrollBar read FHScroll;

    procedure wmDestroy(var msg: TWMDestroy);
    message WM_DESTROY;
    procedure wmVscroll(var msg: TWMVSCROLL);
    message WM_VSCROLL;
    procedure wmHscroll(var msg: TWMHSCROLL);
    message WM_HSCROLL;

    procedure onVscroll(pos: integer; EndScroll: Boolean); virtual;
    procedure onHscroll(pos: integer; EndScroll: Boolean); virtual;
  public
    Constructor Create(Parent: Cardinal);
    Destructor Destroy; override;

    procedure SetFocus; virtual;
    procedure KillFocus; virtual;

    property Handle: Cardinal read FHandle;
  end;

  TScrollBar = class
  private
    FControll: TzeWndControl;
    FBar: integer;
    FDisablenoScroll: Boolean;
    Function GetPos: integer;
    procedure SetPos(const value: integer);
    Function GetMin: integer;
    procedure SetMin(const value: integer);
    Function GetMax: integer;
    procedure SetMax(const value: integer);
    Function GetPage: integer;
    procedure SetPage(const value: integer);
    Function GetDisableNoScroll: Boolean;
    procedure SetDisableNoScroll(const value: Boolean);
  public
    constructor Create(AControll: TzeWndControl; ABar: integer);
    Property pos: integer read GetPos Write SetPos;
    property Min: integer read GetMin Write SetMin;
    property Max: integer read GetMax Write SetMax;
    property Page: integer read GetPage Write SetPage;
    property DisableNoScroll: Boolean read GetDisableNoScroll Write
      SetDisableNoScroll;
  end;

implementation

const
  WndClassObject = 'WndClassObject';

  // ������� ���������
function zeWndControlProc(Wnd: Cardinal; msg, WParam, LParam: integer)
  : integer; stdcall;
var
  PMsg: TMessage;
  Obj: TzeWndControl;
begin
  { TODO -oOnni -cGeneral : Try..except? }
  // ��������� ���������
  // PMsg.hwnd := Wnd;
  PMsg.msg := msg;
  PMsg.WParam := WParam;
  PMsg.LParam := LParam;
  PMsg.Result := 0;
  // PMsg.Time := GetTickCount;
  // PMsg.Pt : TPoint; ???
  // ����� ������� ������, �������� ��� �����������
  Obj := Pointer(GetProp(Wnd, WndClassObject));
  // � ���� ��������� =)
  if Obj <> nil then
  begin
    Obj.Dispatch(PMsg);
    Result := PMsg.Result;
  end
  else
    Result := DefWindowProc(Wnd, msg, WParam, LParam);
end;

{ TzeWndControl }

Constructor TzeWndControl.Create(Parent: Cardinal);
var
  Param: CreateStruct;
begin
  FIsDestroy := false;
  Param.hwndParent := Parent;
  CreateParams(Param);
  if Param.lpszName = nil then
    Param.lpszName := '';

  FHandle := CreateWindowEx(Param.dwExStyle, Param.lpszClass, Param.lpszName,
    Param.Style, Param.x, Param.y, Param.cx, Param.cy, Param.hwndParent,
    Param.hMenu, Param.hInstance, Param.lpCreateParams);
  // ������������ ������
  if FHandle = 0 then
    raise Exception.Create('������ ��� �������� ����');
  //
  SetProp(FHandle, WndClassObject, integer(self));
  // ������� �������
  FVScroll := TScrollBar.Create(self, SB_VERT);
  FHScroll := TScrollBar.Create(self, SB_HORZ);

  SetWindowLong(FHandle, GWL_WNDPROC, integer(@zeWndControlProc));
end;

procedure TzeWndControl.DefaultHandler(var Message);
var
  msg: PMessage;
begin
  msg := @Message;
  msg^.Result := DefWindowProc(FHandle, msg^.msg, msg^.WParam, msg^.LParam);
end;

Destructor TzeWndControl.Destroy;
begin
  FIsDestroy := true;
  DestroyWindow(Handle);
  if Assigned(FVScroll) then
    FVScroll.Free;
  if Assigned(FHScroll) then
    FHScroll.Free;
  inherited;
end;

procedure TzeWndControl.CreateParams(var Param: CreateStruct);
var
  p: Cardinal;
begin
  p := Param.hwndParent;
  ZeroMemory(@Param, SizeOf(Param));

  if p <> 0 then
  begin
    Param.hwndParent := p;
    Param.Style := WS_VISIBLE or WS_CHILD;
    Param.cy := 0;
    Param.cx := 0;
    Param.y := 0;
    Param.x := 0;
  end
  else
  begin
    Param.Style := 0;
    Param.Style := WS_VISIBLE or WS_OVERLAPPEDWINDOW;
    Param.cy := CW_USEDEFAULT;
    Param.cx := CW_USEDEFAULT;
    Param.y := CW_USEDEFAULT;
    Param.x := CW_USEDEFAULT;
  end;

  // Param.lpszName := nil;
  Param.lpszClass := '#32770';
  Param.hInstance := hInstance;
end;

procedure TzeWndControl.wmVscroll(var msg: TWMVSCROLL);
  function GetRealScrollPosition: integer;
  var
    SI: TScrollInfo;
    Code: integer;
  begin
    SI.cbSize := SizeOf(TScrollInfo);
    SI.fMask := SIF_TRACKPOS;
    Code := SB_VERT;
    Result := msg.pos;
    if GetScrollInfo(Handle, Code, SI) then
      Result := SI.nTrackPos;
  end;

begin
  case msg.ScrollCode of
    SB_BOTTOM:
      VScroll.pos := VScroll.Max;
    SB_ENDSCROLL:
      ;
    SB_LINEDOWN:
      VScroll.pos := VScroll.pos + 1;
    SB_LINEUP:
      VScroll.pos := VScroll.pos - 1;
    SB_PAGEDOWN:
      VScroll.pos := VScroll.pos + VScroll.Page;
    SB_PAGEUP:
      VScroll.pos := VScroll.pos - VScroll.Page;
    SB_THUMBPOSITION, SB_THUMBTRACK:
      VScroll.pos := GetRealScrollPosition;

    SB_TOP:
      VScroll.pos := 0;
  end;
  if msg.ScrollCode in [SB_THUMBPOSITION, SB_THUMBTRACK] then
    onVscroll(VScroll.pos, false)
  else
    onVscroll(VScroll.pos, true)
end;

procedure TzeWndControl.wmHscroll(var msg: TWMHSCROLL);
  function GetRealScrollPosition: integer;
  var
    SI: TScrollInfo;
    Code: integer;
  begin
    SI.cbSize := SizeOf(TScrollInfo);
    SI.fMask := SIF_TRACKPOS;
    Code := SB_HORZ;
    Result := msg.pos;
    if GetScrollInfo(Handle, Code, SI) then
      Result := SI.nTrackPos;
  end;

begin
  case msg.ScrollCode of
    SB_BOTTOM:
      HScroll.pos := HScroll.Max;
    SB_ENDSCROLL:
      ;
    SB_LINEDOWN:
      HScroll.pos := HScroll.pos + 1;
    SB_LINEUP:
      HScroll.pos := HScroll.pos - 1;
    SB_PAGEDOWN:
      HScroll.pos := HScroll.pos + HScroll.Page;
    SB_PAGEUP:
      HScroll.pos := HScroll.pos - HScroll.Page;
    SB_THUMBPOSITION, SB_THUMBTRACK:
      HScroll.pos := GetRealScrollPosition;
    SB_TOP:
      HScroll.pos := 0;
  end;
  if msg.ScrollCode in [SB_THUMBPOSITION, SB_THUMBTRACK] then
    onHscroll(HScroll.pos, false)
  else
    onHscroll(HScroll.pos, true)
end;

procedure TzeWndControl.wmDestroy(var msg: TWMDestroy);
begin
  if not FIsDestroy then
  begin
    FIsDestroy := true;
    SetWindowLong(Handle, 0, 0);
    Free;
  end;
end;

procedure TzeWndControl.SetFocus;
begin
  Windows.SetFocus(Handle);
end;

procedure TzeWndControl.KillFocus;
begin

end;

procedure TzeWndControl.onVscroll(pos: integer; EndScroll: Boolean);
begin

end;

procedure TzeWndControl.onHscroll(pos: integer; EndScroll: Boolean);
begin

end;

{ TScrollBar }

constructor TScrollBar.Create(AControll: TzeWndControl; ABar: integer);
begin
  FControll := AControll;
  FBar := ABar;
end;

Function TScrollBar.GetPos: integer;
begin
  Result := GetScrollPos(FControll.Handle, FBar);
end;

procedure TScrollBar.SetPos(const value: integer);
begin
  SetScrollPos(FControll.Handle, FBar, value, true);
end;

Function TScrollBar.GetMin: integer;
var
  sif: SCROLLINFO;
begin
  ZeroMemory(@sif, SizeOf(sif));
  sif.cbSize := SizeOf(sif);
  sif.fMask := SIF_RANGE;
  GetScrollInfo(FControll.Handle, FBar, sif);
  Result := sif.nMin;
end;

procedure TScrollBar.SetMin(const value: integer);
var
  sif: SCROLLINFO;
begin
  ZeroMemory(@sif, SizeOf(sif));
  sif.cbSize := SizeOf(sif);
  GetScrollInfo(FControll.Handle, FBar, sif);

  if DisableNoScroll then
    sif.fMask := SIF_RANGE or SIF_DISABLENOSCROLL
  else
    sif.fMask := SIF_RANGE;

  sif.nMin := value;
  SetScrollInfo(FControll.Handle, FBar, sif, true);
end;

Function TScrollBar.GetMax: integer;
var
  sif: SCROLLINFO;
begin
  ZeroMemory(@sif, SizeOf(sif));
  sif.cbSize := SizeOf(sif);
  sif.fMask := SIF_RANGE;
  GetScrollInfo(FControll.Handle, FBar, sif);
  Result := sif.nMax;
end;

procedure TScrollBar.SetMax(const value: integer);
var
  sif: SCROLLINFO;
begin
  ZeroMemory(@sif, SizeOf(sif));
  sif.cbSize := SizeOf(sif);
  GetScrollInfo(FControll.Handle, FBar, sif);

  if DisableNoScroll then
    sif.fMask := SIF_RANGE or SIF_DISABLENOSCROLL
  else
    sif.fMask := SIF_RANGE;

  sif.nMax := value;
  SetScrollInfo(FControll.Handle, FBar, sif, true);
end;

Function TScrollBar.GetPage: integer;
var
  sif: SCROLLINFO;
begin
  ZeroMemory(@sif, SizeOf(sif));
  sif.cbSize := SizeOf(sif);
  sif.fMask := SIF_PAGE;
  GetScrollInfo(FControll.Handle, FBar, sif);
  Result := sif.nPage;
  if Result = 0 then
    Result := 1;
end;

procedure TScrollBar.SetPage(const value: integer);
var
  sif: SCROLLINFO;
begin
  ZeroMemory(@sif, SizeOf(sif));
  sif.cbSize := SizeOf(sif);

  if DisableNoScroll then
    sif.fMask := SIF_PAGE or SIF_DISABLENOSCROLL
  else
    sif.fMask := SIF_PAGE;

  sif.nPage := value;
  SetScrollInfo(FControll.Handle, FBar, sif, true);
end;

Function TScrollBar.GetDisableNoScroll: Boolean;
var
  sif: SCROLLINFO;
begin
  ZeroMemory(@sif, SizeOf(sif));
  sif.cbSize := SizeOf(sif);
  sif.fMask := SIF_DISABLENOSCROLL;
  GetScrollInfo(FControll.Handle, FBar, sif);
  Result := (sif.fMask and SIF_DISABLENOSCROLL) <> 0

end;

procedure TScrollBar.SetDisableNoScroll(const value: Boolean);
var
  sif: SCROLLINFO;
begin
  ZeroMemory(@sif, SizeOf(sif));
  sif.cbSize := SizeOf(sif);
  if value then
    sif.fMask := SIF_DISABLENOSCROLL;
  SetScrollInfo(FControll.Handle, FBar, sif, true);
end;

end.
