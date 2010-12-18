unit zepages;
{$IFDEF fpc}{$MODE delphi}{$ENDIF}

interface

{$IFNDEF FPC}
{$DEFINE USETHEMES}
{$ENDIF}

uses
  windows,
  messages,
  classes,
  // zegeneric,
  sysutils,
  // zeerror,
  zewndcontrols,
  iece
{$IFDEF USETHEMES},
  Themes,
  UxTheme
{$ENDIF};

type
  TPage = class;

  TPages = class(tzewndcontrol)
  private
    FApplication : IEceApplication;
    FPages: TList;
    FActivePage: integer;
    function GetPages(const index: integer): TPage;
    procedure SetActivePage(const value: integer);
    function TabFromXY(x, y: integer): integer;
    function GetPagesCount: integer;
  protected
    procedure wmpaint(var msg: twmpaint);
    message wm_paint;
    procedure WmLButtonDown(var msg: TWMLButtonDown);
    message WM_LBUTTONDOWN;
    procedure wmSize(var msg: TWMSize);
    message WM_SIZE;
  public
    constructor Create(parent: cardinal; AApp : IEceApplication);
    destructor Destroy; override;

    procedure InsertPage(aindex: integer; apage: TPage);
    procedure UpdatePosition;

    property Pages[const index: integer]: TPage read GetPages;
    property PagesCount : integer read GetPagesCount;
    property PageIndex: integer read FActivePage write SetActivePage;

    function AddPage(ATitle: string; AData: TObject): integer;
    procedure DeletePage(AIndex: Integer);
  end;

  TPage = class
  private
    FPages: TPages;
    FTitle: string;
    FWidth: integer;
    FTextSize: TSize;
    FLeft: integer;
    FData: TObject;
    procedure settitle(const value: string);
    function GetisActive: Boolean;
    function GetTabRect: TRect;
    procedure SetData(const value: TObject);
  public
    constructor Create(apages: TPages);
    destructor Destroy; override;
    property Title: string read FTitle write settitle;
    property Width: integer read FWidth;
    property Left: integer read FLeft;
    property Data: TObject read FData write SetData;

    procedure DrawTab(dc: HDC; rt: TRect);

    property isActive: Boolean read GetisActive;
    property TabRect: TRect Read GetTabRect;
    procedure Invalidate;
  end;

implementation

const
  c_offset_top = 2;
  c_offset_left = 2;
  c_tab_space = 1;

  { tpages }

function TPages.AddPage(ATitle: string; AData: TObject): integer;
var
  Page: TPage;
begin
  Page := TPage.Create(Self);
  Page.Title := ATitle;
  Page.Data := AData;
  FPages.Add(Page);
  UpdatePosition;
end;

constructor TPages.Create(parent: cardinal; AApp : IEceApplication);
var
  newpage: TPage;
begin
  inherited Create(parent);
  FApplication := AApp;

  FPages := TList.Create;

  // newpage := tpage.create(self);
  // newpage.title := 'new *';
  // insertpage(1, newpage);
  //
  // newpage := tpage.create(self);
  // newpage.title := 'new (2)*';
  // insertpage(2, newpage);
end;

procedure TPages.DeletePage(AIndex: Integer);
begin
  FPages.Delete(AIndex);
  InvalidateRect(handle, nil, false)
end;

destructor TPages.Destroy;
var
  i: integer;
begin
  for i := 0 to FPages.Count - 1 do
    TPage(FPages[i]).Free;
  FPages.Free;
  inherited;
end;

function TPages.GetPages(const index: integer): TPage;
begin
  result := TPage(FPages[index]);
end;

function TPages.GetPagesCount: integer;
begin
  Result := FPages.Count;
end;

procedure TPages.SetActivePage(const value: integer);
begin
  if FActivePage = value then exit;
  try
    Pages[FActivePage].Invalidate;
  except
  end;
  FActivePage := value;
  try
    Pages[FActivePage].Invalidate;
  except
  end;
  FApplication._SetActiveDocumentIndex(FActivePage);
end;

procedure TPages.InsertPage(aindex: integer; apage: TPage);
begin
  FPages.insert(aindex, apage);
  UpdatePosition
end;

procedure TPages.UpdatePosition;
var
  i, ALeft: integer;
begin
  ALeft := c_offset_left;
  for i := 0 to FPages.Count - 1 do
  begin
    With Pages[i] do
    begin
      FLeft := ALeft;
      inc(ALeft, Width + c_tab_space);
    end;
  end;
end;

function TPages.TabFromXY(x, y: integer): integer;
var
  i: integer;
  pt: TPoint;
begin
  pt.x := x { - offsetX } ;
  pt.y := y;

  for i := 0 to FPages.Count - 1 do
  begin
    if PtInRect(Pages[i].TabRect, pt) then
      exit(i);
  end;
  result := -1;
end;

procedure TPages.WmLButtonDown(var msg: TWMLButtonDown);
var
  Index: integer;
begin
  index := TabFromXY(msg.XPos, msg.YPos);
  if index <> -1 then
    PageIndex := index;
end;

procedure TPages.wmpaint(var msg: twmpaint);
var
  ps: tpaintstruct;
  rt, dtr: TRect;
  i: integer;
  Page: TPage;
  brush: hbrush;
  flags, flags2: cardinal;
{$IFDEF USETHEMES}
  TabTheme: TThemedButton;
  TabThemeDetails: TThemedElementDetails;
{$ENDIF}
begin
  getclientrect(handle, rt);
  beginpaint(handle, ps);
  // Заливаем фон
{$IFDEF USETHEMES}
  if IsThemeActive then
  begin
    TabTheme := tbPushButtonNormal;
    TabThemeDetails := ThemeServices.GetElementDetails(TabTheme);
    ThemeServices.DrawEdge(ps.HDC, TabThemeDetails, rt, BDR_SUNKENINNER,
      BF_BOTTOM or BF_MIDDLE);
  end
  else
{$ENDIF}
  begin
    // fillrect(ps.hdc, rt, getsyscolorbrush(COLOR_BTNFACE));
    DrawEdge(ps.HDC, rt, BDR_RAISEDINNER, BF_BOTTOM or BF_MIDDLE);
  end;

  rt.top := c_offset_top;
  // rt.bottom := rt.bottom + c_offset_top + 2;
  for i := 0 to FPages.Count - 1 do
  begin
    Page := Pages[i];
    rt.Left := Page.Left;
    rt.right := rt.Left + Page.Width;

    Page.DrawTab(ps.HDC, rt);
  end;

  getclientrect(handle, rt);
  rt.Left := rt.right - rt.Bottom;
  // Кнопки
  // {$IFDEF USETHEMES}
  // if IsThemeActive then
  // begin
  // TabTheme := tbPushButtonNormal;
  // TabThemeDetails := ThemeServices.GetElementDetails(TabTheme);
  // ThemeServices.DrawElement(ps.hdc, TabThemeDetails, rt);
  // OffsetRect(rt, -rt.Bottom, 0);
  // ThemeServices.DrawElement(ps.hdc, TabThemeDetails, rt);
  // end
  // else
  // {$ENDIF}
  // begin
  // //fillrect(ps.hdc, rt, getsyscolorbrush(COLOR_BTNFACE));
  // DrawEdge(ps.hdc, rt, BDR_RAISED, BF_RECT or BF_MIDDLE);
  // OffsetRect(rt, -rt.Bottom, 0);
  // DrawEdge(ps.hdc, rt, BDR_RAISED, BF_RECT or BF_MIDDLE);
  // end;
  EndPaint(handle, ps);
end;

procedure TPages.wmSize(var msg: TWMSize);
begin
  InvalidateRect(handle, nil, false);
  inherited;
end;

{ TPage }

Constructor TPage.Create(apages: TPages);
begin
  inherited Create;
  FPages := apages;
end;

Destructor TPage.Destroy;
begin
  inherited;
end;

procedure TPage.DrawTab(dc: HDC; rt: TRect);
var
{$IFDEF USETHEMES}
  TabTheme: TThemedTab;
  TabThemeDetails: TThemedElementDetails;
  CloseBtnTheme: TThemedWindow;
  CloseBtnThemeDetails: TThemedElementDetails;
{$ENDIF}
  flags, flags2: integer;
  TextRt: TRect;
  BtnRect: TRect;
begin
  SelectObject(dc, GetStockObject(DEFAULT_GUI_FONT));
  SetBkMode(dc, TRANSPARENT);
{$IFDEF USETHEMES}
  if IsThemeActive then
  begin
    if isActive then
    begin
      TabTheme := ttTabItemSelected;
    end
    else
    begin
      TabTheme := ttTabItemNormal;
      inc(rt.top, c_tab_space);
      dec(rt.Bottom);
    end;

    TabThemeDetails := ThemeServices.GetElementDetails(TabTheme);
    ThemeServices.DrawElement(dc, TabThemeDetails, rt);

    TextRt := rt;
    dec(TextRt.right, 18);
    inc(TextRt.Left, 2);
    ThemeServices.DrawText(dc, TabThemeDetails, Title, TextRt,
      DT_CENTER or DT_SINGLELINE or DT_VCENTER or DT_PATH_ELLIPSIS, 0);

    BtnRect := rt;
    BtnRect.Left := rt.right - 16;

    dec(BtnRect.right, 2);
    BtnRect.Bottom := BtnRect.top + 16;
    inc(BtnRect.top, 2);

    CloseBtnTheme := twCloseButtonNormal;
    CloseBtnThemeDetails := ThemeServices.GetElementDetails(CloseBtnTheme);
    ThemeServices.DrawElement(dc, CloseBtnThemeDetails, BtnRect);
  end
  else
{$ENDIF}
  begin
    if isActive then
    begin
      flags := BDR_RAISED;
      flags2 := BF_MIDDLE or BF_RECT and not BF_BOTTOM;
      SetTextColor(dc, GetSysColor(COLOR_BTNTEXT));
    end
    else
    begin
      flags := BDR_RAISEDINNER;
      flags2 := BF_MIDDLE or BF_RECT and not BF_BOTTOM;
      inc(rt.top, c_offset_top);
      dec(rt.Bottom);
      SetTextColor(dc, GetSysColor(COLOR_GRAYTEXT));
    end;
    DrawEdge(dc, rt, flags, flags2);
    TextRt := rt;
    dec(TextRt.right, 18);
    inc(TextRt.Left, 2);
    DrawText(dc, PChar(Title), Length(Title), TextRt,
      DT_CENTER or DT_SINGLELINE or DT_VCENTER or DT_PATH_ELLIPSIS);
  end;
end;

function TPage.GetisActive: Boolean;
begin
  result := FPages.FPages.IndexOf(Self) = FPages.FActivePage;
end;

function TPage.GetTabRect: TRect;
begin
  GetWindowRect(FPages.handle, result);
  result.Left := Left;
  result.top := c_offset_top;
  result.right := Left + Width;
end;

procedure TPage.Invalidate;
var
  rt: TRect;
begin
  rt := TabRect;
  OffsetRect(rt, { OffsetX } 0, 0);
  InvalidateRect(FPages.handle, @rt, false);
end;

procedure TPage.SetData(const value: TObject);
begin
  FData := value;
end;

procedure TPage.settitle(const value: string);
var
  dc: HDC;
begin
  FTitle := value;

  dc := GetDc(FPages.handle);
  SelectObject(dc, GetStockObject(DEFAULT_GUI_FONT));
  GetTextExtentPoint32(dc, PChar(FTitle), Length(FTitle), FTextSize);
  FWidth := FTextSize.cx + 4 + 16;
  ReleaseDc(FPages.handle, dc);
  if FWidth > 256 then
    FWidth := 256;
  if FWidth < 64 then
    FWidth := 64;

  FPages.UpdatePosition;
end;

initialization

{$IFDEF USETHEMES} InitThemeLibrary; {$ENDIF}

finalization

{$IFDEF USETHEMES} FreeThemeLibrary; {$ENDIF}

end.
