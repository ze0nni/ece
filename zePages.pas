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
    iece;

type
    tpage = class;

	tpages = class(tzewndcontrol)
    private
        fpages : tlist;
        FActivePage : integer;
        function getpages(const index : integer) : tpage;
        procedure SetActivePage(const value : integer);
    protected
        procedure wmpaint(var msg : twmpaint); message wm_paint;
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
    public
        constructor create(apages : tpages);
        destructor destroy; override;
        property title : string read ftitle write settitle;
        property width : integer read fwidth;
        property left : integer read fleft;
    end;
implementation

const
    c_offset_top = 2;
    c_offset_left = 2;
    c_tab_space = 2;

{ tpages }

constructor tpages.create(parent : cardinal);
var
    newpage : tpage;
begin
    inherited;
    fpages := tlist.create;

    newpage := tpage.create(self);
    newpage.title := 'new document*';
    insertpage(0, newpage);
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
    FActivePage := value;
    InvalidateRect(handle, nil, false);
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
            inc(ALeft, Width + c_tab_space)
        end;
    end;
end;

procedure tpages.wmpaint(var msg : twmpaint);
var
    ps : tpaintstruct;
    rt,dtr : trect;
    i : integer;
    page : tpage;
    brush : hbrush;
    flags, flags2 : cardinal;
begin
    getclientrect(handle, rt);
    beginpaint(handle, ps);
        //Заливаем фон
        fillrect(ps.hdc, rt, getsyscolorbrush(color_appworkspace));

        rt.top := c_offset_top;
        rt.bottom := rt.bottom + c_offset_top + 2;
        for i := 0 to fpages.count - 1 do
        begin
            page := pages[i];
            rt.left := page.left;
            rt.right := rt.left + page.width;

            if i = pageindex then
            begin
                flags := edge_raised;
                flags2 := BF_RECT or BF_MIDDLE and not BF_BOTTOM;
                SetTextColor(Ps.hdc, GetSysColor(COLOR_BTNTEXT));
            end
            else
            begin
                flags := BDR_RAISEDINNER;
                flags2 := BF_MIDDLE or BF_RECT;
                SetTextColor(Ps.hdc, GetSysColor(COLOR_BTNSHADOW));
            end;

                DrawEdge(ps.hdc, rt, flags, flags2);
                SetBkMode(ps.hdc, TRANSPARENT);

                dtr := rt;
                inc(dtr.left, 2);
                dec(dtr.Right, 2);
                inc(dtr.Top, 2);
                DrawText(ps.hdc, PChar(Page.Title), Length(Page.Title), dtr, 0);
        end;

    EndPaint(handle, ps);
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

procedure TPage.SetTitle(const value : string);
var
    dc : hdc;
begin
    FTitle := value;

    dc := GetDc(FPages.handle);
        GetTextExtentPoint32(dc, PChar(FTitle), Length(FTitle), FTextSize);
        FWidth := FTextSize.cx+4+16;
    ReleaseDc(fpages.handle, dc);
    FPages.UpdatePosition;
end;

end.
