// ************************************************************
//
// Windows Controls
// Copyright (c) 2010  zeDevel
//
// Разработчик: Благодарев Евгений  ze0nni@gmail.com
//
// ************************************************************
// Модуль для создания классов окон на основе классов OP
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
    function GetLeft: Integer;
    procedure SetLeft(const Value: Integer);
    function GetTop: Integer;
    procedure SetTop(const Value: Integer);
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
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

    procedure onVscroll(pos: Integer; EndScroll: Boolean); virtual;
    procedure onHscroll(pos: Integer; EndScroll: Boolean); virtual;
  public
    Constructor Create(Parent: Cardinal);
    Destructor Destroy; override;

    procedure SetFocus; virtual;
    procedure KillFocus; virtual;

    property Handle: Cardinal read FHandle;

    property Left: Integer read GetLeft write SetLeft;
    property Top: Integer read GetTop write SetTop;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
  end;

  TScrollBar = class
  private
    FControll: TzeWndControl;
    FBar: Integer;
    FDisablenoScroll: Boolean;
    Function GetPos: Integer;
    procedure SetPos(const Value: Integer);
    Function GetMin: Integer;
    procedure SetMin(const Value: Integer);
    Function GetMax: Integer;
    procedure SetMax(const Value: Integer);
    Function GetPage: Integer;
    procedure SetPage(const Value: Integer);
    Function GetDisableNoScroll: Boolean;
    procedure SetDisableNoScroll(const Value: Boolean);
  public
    constructor Create(AControll: TzeWndControl; ABar: Integer);
    Property pos: Integer read GetPos Write SetPos;
    property Min: Integer read GetMin Write SetMin;
    property Max: Integer read GetMax Write SetMax;
    property Page: Integer read GetPage Write SetPage;
    property DisableNoScroll: Boolean read GetDisableNoScroll Write
      SetDisableNoScroll;
  end;

implementation

const
  WndClassObject = 'WndClassObject';

  // Оконная процедура
function zeWndControlProc(Wnd: Cardinal; msg, WParam, LParam: Integer)
  : Integer; stdcall;
var
  PMsg: TMessage;
  Obj: TzeWndControl;
begin
  { TODO -oOnni -cGeneral : Try..except? }
  // Заполняем структуру
  // PMsg.hwnd := Wnd;
  PMsg.msg := msg;
  PMsg.WParam := WParam;
  PMsg.LParam := LParam;
  PMsg.Result := 0;
  // PMsg.Time := GetTickCount;
  // PMsg.Pt : TPoint; ???
  // Далее находим Объект, которому оно принадлежит
  Obj := Pointer(GetProp(Wnd, WndClassObject));
  // И шлем сообщение =)
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
  // Обрабатываем ошибку
  if FHandle = 0 then
    raise Exception.Create('Ошибка при создании окна');
  //
  SetProp(FHandle, WndClassObject, Integer(self));
  // Создаем скроллы
  FVScroll := TScrollBar.Create(self, SB_VERT);
  FHScroll := TScrollBar.Create(self, SB_HORZ);

  SetWindowLong(FHandle, GWL_WNDPROC, Integer(@zeWndControlProc));
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

function TzeWndControl.GetHeight: Integer;
var
  rt: Trect;
begin
  GetWindowRect(Handle, rt);
  Result := rt.Bottom - rt.Top;
end;

function TzeWndControl.GetLeft: Integer;
var
  rt: Trect;
begin
  GetWindowRect(Handle, rt);
  Result := rt.Left;
end;

function TzeWndControl.GetTop: Integer;
var
  rt: Trect;
begin
  GetWindowRect(Handle, rt);
  Result := rt.Top;
end;

function TzeWndControl.GetWidth: Integer;
var
  rt: Trect;
begin
  GetWindowRect(Handle, rt);
  Result := rt.Right - rt.Left;
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
  function GetRealScrollPosition: Integer;
  var
    SI: TScrollInfo;
    Code: Integer;
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
  function GetRealScrollPosition: Integer;
  var
    SI: TScrollInfo;
    Code: Integer;
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

procedure TzeWndControl.SetHeight(const Value: Integer);
var
  rt: Trect;
begin
  GetWindowRect(Handle, rt);
  SetWindowPos(Handle, 0, 0, 0, rt.Right - rt.Left, Value,
    SWP_NOMOVE or SWP_NOACTIVATE);
end;

procedure TzeWndControl.SetLeft(const Value: Integer);
var
  rt: Trect;
begin
  GetWindowRect(Handle, rt);
  SetWindowPos(Handle, 0, Value, rt.Top, 0, 0, SWP_NOSIZE or SWP_NOACTIVATE);
end;

procedure TzeWndControl.SetTop(const Value: Integer);
var
  rt: Trect;
begin
  GetWindowRect(Handle, rt);
  SetWindowPos(Handle, 0, rt.left, Value, 0, 0, SWP_NOSIZE or SWP_NOACTIVATE);
end;

procedure TzeWndControl.SetWidth(const Value: Integer);
var
  rt: Trect;
begin
  GetWindowRect(Handle, rt);
  SetWindowPos(Handle, 0, 0, 0, Value, rt.Bottom - rt.Top,
    SWP_NOMOVE or SWP_NOACTIVATE);
end;

procedure TzeWndControl.KillFocus;
begin

end;

procedure TzeWndControl.onVscroll(pos: Integer; EndScroll: Boolean);
begin

end;

procedure TzeWndControl.onHscroll(pos: Integer; EndScroll: Boolean);
begin

end;

{ TScrollBar }

constructor TScrollBar.Create(AControll: TzeWndControl; ABar: Integer);
begin
  FControll := AControll;
  FBar := ABar;
end;

Function TScrollBar.GetPos: Integer;
begin
  Result := GetScrollPos(FControll.Handle, FBar);
end;

procedure TScrollBar.SetPos(const Value: Integer);
begin
  SetScrollPos(FControll.Handle, FBar, Value, true);
end;

Function TScrollBar.GetMin: Integer;
var
  sif: SCROLLINFO;
begin
  ZeroMemory(@sif, SizeOf(sif));
  sif.cbSize := SizeOf(sif);
  sif.fMask := SIF_RANGE;
  GetScrollInfo(FControll.Handle, FBar, sif);
  Result := sif.nMin;
end;

procedure TScrollBar.SetMin(const Value: Integer);
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

  sif.nMin := Value;
  SetScrollInfo(FControll.Handle, FBar, sif, true);
end;

Function TScrollBar.GetMax: Integer;
var
  sif: SCROLLINFO;
begin
  ZeroMemory(@sif, SizeOf(sif));
  sif.cbSize := SizeOf(sif);
  sif.fMask := SIF_RANGE;
  GetScrollInfo(FControll.Handle, FBar, sif);
  Result := sif.nMax;
end;

procedure TScrollBar.SetMax(const Value: Integer);
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

  sif.nMax := Value;
  SetScrollInfo(FControll.Handle, FBar, sif, true);
end;

Function TScrollBar.GetPage: Integer;
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

procedure TScrollBar.SetPage(const Value: Integer);
var
  sif: SCROLLINFO;
begin
  ZeroMemory(@sif, SizeOf(sif));
  sif.cbSize := SizeOf(sif);

  if DisableNoScroll then
    sif.fMask := SIF_PAGE or SIF_DISABLENOSCROLL
  else
    sif.fMask := SIF_PAGE;

  sif.nPage := Value;
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

procedure TScrollBar.SetDisableNoScroll(const Value: Boolean);
var
  sif: SCROLLINFO;
begin
  ZeroMemory(@sif, SizeOf(sif));
  sif.cbSize := SizeOf(sif);
  if Value then
    sif.fMask := SIF_DISABLENOSCROLL;
  SetScrollInfo(FControll.Handle, FBar, sif, true);
end;

end.
