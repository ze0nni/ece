unit zepages;
{$ifdef fpc}{$mode delphi}{$endif}
interface

uses
	windows,
	messages,
	classes,
  zegeneric,
	sysutils,
	zeerror,
  zewndcontrols,
  iece,
  Themes,
  UxTheme;

type
    tpage = class;

	  tpages = class(tzewndcontrol)
    private
      fpages : tlist;
      FActivePage : integer;
      function getpages(const index : integer) : tpage;
      procedure SetActivePage(const value : integer);
      function TabFromXY(x, y: integer): integer;
      protected
      procedure wmpaint(var msg : twmpaint); message wm_paint;
      procedure WmLButtonDown(var msg : TWMLButtonDown); message WM_LBUTTONDOWN;
      procedure wmSize(var msg : TWMSize); message WM_SIZE;
    public
      constructor create(parent : cardinal);
      destructor destroy; override;

      procedure insertpage(aindex : integer; apage : tpage);
      procedure UpdatePosition;

      property pages[const index : integer] : tpage read getpages;
      property PageIndex : integer read FActivePage write SetActivePage;
    end;
    
    tpage = class
    private
      fpages : tpages;
      ftitle : string;
      fwidth : integer;
      FTextSize : TSize;
      fleft : integer;
      procedure settitle(const value : string);
      function GetisActive: Boolean;
    function GetTabRect: TRect;
    public
      constructor create(apages : tpages);
      destructor destroy; override;
      property title : string read ftitle write settitle;
      property width : integer read fwidth;
      property left : integer read fleft;

      procedure DrawTab(dc : HDC; rt : TRect);

      property isActive : Boolean read GetisActive;
      property TabRect : TRect Read GetTabRect;
      procedure Invalidate;
    end;
implementation

const
    c_offset_top = 2;
    c_offset_left = 2;
    c_tab_space = 1;

{ tpages }

constructor tpages.create(parent : cardinal);
var
    newpage : tpage;
begin
    inherited;
    fpages := tlist.create;

    newpage := tpage.create(self);
    newpage.title := ParamStr(1);
    insertpage(0, newpage);

//    newpage := tpage.create(self);
//    newpage.title := 'new *';
//    insertpage(1, newpage);
//
//    newpage := tpage.create(self);
//    newpage.title := 'new (2)*';
//    insertpage(2, newpage);
end;

destructor tpages.destroy;
begin
    { todo:fpages.clear }
    fpages.free;
    inherited;    
end;

function tpages.getpages(const index : integer) : tpage;
begin
    result := tpage(fpages[index]);
end;

procedure TPages.SetActivePage(const value : integer);
begin
    if FActivePage = value then exit;

    try
      pages[FActivePage].Invalidate;
    except end;
    FActivePage := value;
    try
      pages[FActivePage].Invalidate;
    except end;
end;

procedure tpages.insertpage(aindex : integer; apage : tpage);
begin
    fpages.insert(aindex, apage);
    UpdatePosition
end;

procedure TPages.UpdatePosition;
var
    i, ALeft : integer;
begin
    ALeft := c_offset_left;
    for i := 0 to fpages.count - 1 do
    begin
        With Pages[i] do
        begin
            FLeft := ALeft;
            inc(ALeft, Width + c_tab_space);
        end;
    end;
end;

function TPages.TabFromXY(x, y : integer) : integer;
var
  i: Integer;
  pt : TPoint;
begin
  pt.X := x { - offsetX };
  pt.Y := y;

  for i := 0 to fpages.Count - 1 do
  begin
    if PtInRect(Pages[i].TabRect, pt) then exit(i);
  end;
  result := -1;
end;

procedure tpages.WmLButtonDown(var msg: TWMLButtonDown);
var
   Index : Integer;
begin
  index := TabFromXY(msg.XPos, msg.YPos);
  if index <> -1 then  
  PageIndex := index;
end;

procedure tpages.wmpaint(var msg : twmpaint);
var
    ps : tpaintstruct;
    rt,dtr : trect;
    i : integer;
    page : tpage;
    brush : hbrush;
    flags, flags2 : cardinal;

    TabTheme : TThemedButton;
    TabThemeDetails : TThemedElementDetails;
begin
    getclientrect(handle, rt);
    beginpaint(handle, ps);
        //Заливаем фон
        if IsThemeActive then
        begin
          TabTheme := tbPushButtonNormal;
          TabThemeDetails := ThemeServices.GetElementDetails(TabTheme);
          ThemeServices.DrawEdge(ps.hdc, TabThemeDetails, rt, BDR_SUNKENINNER, BF_BOTTOM or BF_MIDDLE);
        end
          else
        begin
          //fillrect(ps.hdc, rt, getsyscolorbrush(COLOR_BTNFACE));
          DrawEdge(ps.hdc, rt, BDR_RAISEDINNER, BF_BOTTOM or BF_MIDDLE);
        end;

        rt.top := c_offset_top;
        //rt.bottom := rt.bottom + c_offset_top + 2;
        for i := 0 to fpages.count - 1 do
        begin
            page := pages[i];
            rt.left := Page.left;
            rt.right := rt.left + page.width;

            page.DrawTab(Ps.hdc, rt);
        end;

        getclientrect(handle, rt);
        rt.Left := rt.Right - rt.Bottom;
        //
        if IsThemeActive then
        begin
          TabTheme := tbPushButtonNormal;
          TabThemeDetails := ThemeServices.GetElementDetails(TabTheme);
          ThemeServices.DrawElement(ps.hdc, TabThemeDetails, rt);
          OffsetRect(rt, -rt.Bottom, 0);
          ThemeServices.DrawElement(ps.hdc, TabThemeDetails, rt);
        end
          else
        begin
          //fillrect(ps.hdc, rt, getsyscolorbrush(COLOR_BTNFACE));
          DrawEdge(ps.hdc, rt, BDR_RAISED, BF_RECT or BF_MIDDLE);
          OffsetRect(rt, -rt.Bottom, 0);
          DrawEdge(ps.hdc, rt, BDR_RAISED, BF_RECT or BF_MIDDLE);
        end;
    EndPaint(handle, ps);
end;

procedure tpages.wmSize(var msg: TWMSize);
begin
  InvalidateRect(Handle, nil, false);
  inherited;
end;

{ TPage }

Constructor TPage.Create(APages : TPages);
begin
    inherited Create;
    FPages := APages;
end;

Destructor TPage.Destroy;
begin
    inherited;
end;

procedure tpage.DrawTab(dc: HDC; rt : TRect);
var
  TabTheme : TThemedTab;
  TabThemeDetails : TThemedElementDetails;
  CloseBtnTheme : TThemedWindow;
  CloseBtnThemeDetails : TThemedElementDetails;
  Flags, Flags2 : Integer;
  TextRt : TRect;
  BtnRect : TRect;
begin
  SelectObject(dc, GetStockObject(DEFAULT_GUI_FONT));
  SetBkMode(Dc, TRANSPARENT);

  if IsThemeActive then
  begin
    if isActive then
    begin
      TabTheme := ttTabItemSelected
    end
      else
    begin
      TabTheme := ttTabItemNormal;
      inc(rt.Top, c_tab_space);
      dec(rt.Bottom);
    end;

    TabThemeDetails := ThemeServices.GetElementDetails(TabTheme);
    ThemeServices.DrawElement(dc, TabThemeDetails, rt);

    TextRt := rt;
    dec(TextRt.Right, 18);
    Inc(TextRt.Left, 2);
    ThemeServices.DrawText(dc, TabThemeDetails, title, TextRt, DT_CENTER or DT_SINGLELINE or DT_VCENTER or DT_PATH_ELLIPSIS, 0);

    BtnRect := rt;
    BtnRect.Left := rt.Right-16;

    Dec(BtnRect.Right, 2);
    BtnRect.Bottom := BtnRect.Top + 16;
    Inc(BtnRect.Top, 2);


    CloseBtnTheme := twCloseButtonNormal;
    CloseBtnThemeDetails := ThemeServices.GetElementDetails(CloseBtnTheme);
    ThemeServices.DrawElement(dc, CloseBtnThemeDetails, BtnRect);
  end
  else
  begin
    if isActive then
    begin
      Flags := BDR_RAISED;
      Flags2 := BF_MIDDLE or BF_RECT and not BF_BOTTOM;
    end
      else
    begin
      Flags := BDR_RAISEDINNER;
      Flags2 := BF_MIDDLE or BF_RECT and not BF_BOTTOM;
      inc(rt.Top, c_offset_top);
      dec(rt.Bottom);
    end;
    SetTextColor(dc, GetSysColor(COLOR_BTNTEXT));
    DrawEdge(dc, rt, Flags, Flags2);
    DrawText(dc, Title, Length(Title), rt, DT_CENTER or DT_SINGLELINE or DT_VCENTER);
  end;
end;

function tpage.GetisActive: Boolean;
begin
  Result := fpages.fpages.IndexOf(Self) = fpages.FActivePage;
end;

function tpage.GetTabRect: TRect;
begin
  GetWindowRect(fpages.Handle, Result);
  Result.Left := left;
  Result.Top := c_offset_top;
  Result.Right := left + width;
end;

procedure tpage.Invalidate;
var
  rt : TRect;
begin
  rt := TabRect;
  OffsetRect(rt, {OffsetX}0, 0);
  InvalidateRect(fpages.Handle, @rt, false);
end;

procedure TPage.SetTitle(const value : string);
var
    dc : hdc;
begin
    FTitle := value;

    dc := GetDc(FPages.handle);
        SelectObject(dc, GetStockObject(DEFAULT_GUI_FONT));
        GetTextExtentPoint32(dc, PChar(FTitle), Length(FTitle), FTextSize);
        FWidth := FTextSize.cx+4+16;
    ReleaseDc(fpages.handle, dc);
    if fwidth > 256 then fwidth := 256;
    if fwidth < 64 then fwidth := 64;
    
    FPages.UpdatePosition;
end;

initialization
  InitThemeLibrary;
finalization
  FreeThemeLibrary;
end.
