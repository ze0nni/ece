unit EditorWindow;
{$ifdef fpc}{$mode delphi}{$endif}
interface

uses
	Windows,
	Messages,
	Classes,
	SysUtils,
	zeError,
	iece,
	zeWndControls,
	DocumentWindow;

type
	TGutter = class;

	TLine = class;

	TCaret = class;

  TEceEditorWindow = class(TEceDocumentWindow, IDispatch, IEceDocument,
    IEceEditor)
  private
    FFonts: array [0 .. 3] of HFont;
    FFontExtraSpace: array [0 .. 3] of integer;
    FLineModificationChecker: integer;
    FCharSize: TSize;
    FGutter: TGutter;
    FCaret: TCaret;
    FBackBuffer: HBitmap;
    FLines: TList;
    FVisibleLines: TList;
    FUpdateLockCount: integer;
    FOffsetX: Integer;
    FOffsetY: Integer;

    FPlugins : TInterfaceList;

    Function GetCount: integer;
    Function GetStrings(const Index: integer): string;
    procedure SetStrings(const Index: integer; const value: string);
    function GetLines(const index: integer): TLine;
    procedure SetFont(AFont: String; Size: integer);
    procedure SetOffsetX(const value: Integer);
    procedure SetOffsetY(const value: Integer);
    function GetEditorRect: TRect;
    function GetCharsInHeight: integer;
    function GetCharsInWidth: integer;
    procedure LineModification;
  protected
    procedure CreateParams(var Param: CreateStruct); override;
    procedure wmPaint(var msg: TMessage);
    message WM_PAINT;
    procedure wmSize(var msg: TWMSize);
    message WM_SIZE;
    procedure wmGetDlgCode(var msg: TWmGetDlgCode);
    message WM_GETDLGCODE;
    procedure wmKeyDown(var msg: TWmKeyDown);
    message WM_KEYDOWN;
    procedure wmChar(var msg: TWmChar);
    message WM_CHAR;
    procedure wmSetCursor(var msg: TWmSetCursor);
    message WM_SETCURSOR;

    procedure wmLbuttonDown(var msg: TWmLbuttonDown);
    message WM_LBUTTONDOWN;

    procedure onVscroll(pos: integer; EndScroll: boolean); override;
    procedure onHscroll(pos: integer; EndScroll: boolean); override;
  protected
    procedure _BeginUpdate ; safecall;
    procedure _EndUpdate ; safecall;
    function _GetLinesCount : Integer; safecall;
    function _GetLines(AIndex : Integer) : IEceLine; safecall;
    function _GetGutter : IGutter; safecall;
    function _GetCaret : ICaret; safecall;
    function _AddLine : IEceLine; safecall;
    function _InsertLine(Index : Integer) : IEceLine; safecall;
    procedure _DeleteLine(Index : Integer); safecall;
  public
    Constructor Create(AParent: Cardinal);
    Destructor Destroy; override;

    procedure Invalidate;

    procedure SetFocus; override;
    procedure KillFocus; override;

    procedure SaveToFile(AFileName: string); override;
    procedure LoadFromFile(AFileName: string); override;

    property Caret: TCaret Read FCaret;
    property Gutter: TGutter read FGutter;

    Property Count: integer Read GetCount;
    property Strings[const index: integer]
      : string Read GetStrings Write SetStrings;
    property Lines[const index: integer]: TLine read GetLines;
    procedure Clear;
    function AddString(const ANewString: string): integer;
    function InsertString(Const ANewString: string; AIndex: integer): integer;

    function AddLine: TLine;
    function InsertLine(AIndex: integer): TLine;
    procedure DeleteLine(const index: integer);

    Procedure BeginUpdate;
    Procedure EndUpdate;

    property CharWidth: integer read FCharSize.Cx;
    property CharHeight: integer read FCharSize.Cy;
    property OffsetX: Integer Read FOffsetX Write SetOffsetX;
    property OffsetY: Integer Read FOffsetY Write SetOffsetY;

    property EditorRect: TRect Read GetEditorRect;
    property CharsInHeight: integer Read GetCharsInHeight;
    property CharsInWidth: integer Read GetCharsInWidth;

    procedure LoadPlugin(AFileName : string);
  end;

  TGutter = class(TEceInterfacedObject, IDispatch, IGutter)
  private
    FSize: Cardinal;
    FEditor: TEceEditorWindow;
    Function GetSize: Cardinal;
  public
    Constructor Create(AEditor: TEceEditorWindow);
    Procedure Draw(DC: HDC; Rt: TRect);
    Property Size: Cardinal read GetSize;
  end;

  TLine = class(TEceInterfacedObject, IDispatch, IEceLine)
  private
    FText: String;
    FVisible: boolean;
    { указывает на строку, до которой, блок свернут }
    FRolllUpFor: TLine;
    { Список вернутых строк }
    FRollUpLines: TList;
    { строка, которая посмела свернуть }
    FRollUpIn: TLine;
    FLineIndex: integer;
    FLineModificationChecker: integer;
    FEditor: TEceEditorWindow;
    FisRollUp: boolean;
    Function GetText: String;
    Procedure SetText(const value: String);
    procedure SetVisible(const value: boolean);
    function GetLineIndex: integer;
    function GetLength: integer;
    Function GetisRollBlock: boolean;
    Procedure SetIsRollUp(const value: boolean);
  protected
    function _GetText : string; safecall;
    function _SetText(Text : string) : Integer; safecall;
    function _GetIndex : Integer; safecall;
  public
    Constructor Create(AEditor: TEceEditorWindow);
    Destructor Destroy; override;
    Procedure Insert(const AString: String; AChar: integer);
    procedure Delete(AChar, ACount: integer);
    Procedure BreakLine(AChar: integer);
    procedure Draw(dc : HDC; cx, cy, StartChar : Integer);
    Procedure Invalidate;
    property isRollBlock: boolean read GetisRollBlock;
    property isRollUp: boolean read FisRollUp write SetIsRollUp;
    property Text: String Read GetText write SetText;
    property Visible: boolean read FVisible Write SetVisible;
    property LineIndex: integer read GetLineIndex;
    property Length: integer read GetLength;
  end;

  TCaretStyle = (csNormal, csClassic);

  TCaret = class(TEceInterfacedObject, IDispatch, ICaret)
	private
		FEditor : TEceEditorWindow;
    Fx, Fy: integer;
    FStyle: TCaretStyle;
    Procedure SetX(Const value: integer);
    Procedure SetY(Const value: integer);
    procedure SetStyle(Const value: TCaretStyle);
  protected
    function _GetX : Integer; safecall;
    function _GetY : Integer; safecall;
    function _SetX(value : Integer) : Integer; safecall;
    function _SetY(value : Integer) : Integer; safecall;
  public
    Constructor Create(AEditor: TEceEditorWindow);
    property X: integer read Fx Write SetX;
    property Y: integer read Fy write SetY;
    property Style: TCaretStyle read FStyle Write SetStyle;
    procedure Show;
    procedure Hide;
    procedure Update;
  end;

implementation

{ TEceEditorWindow }

procedure TEceEditorWindow.CreateParams(var Param : CreateStruct);
begin
	inherited;
	Param.Style := Param.Style or WS_VSCROLL or WS_HSCROLL;
	Param.dwExStyle := WS_EX_CLIENTEDGE;
end;

procedure TEceEditorWindow.wmPaint(var msg : TMessage);
var
	Ps : TPaintStruct;
	Rt : TRect;
	CDC : HDC;
	LineStart, LineEnd, i,j,lleft : Integer;
	LineTop, LineLen : Integer;
  LineO : TLine;
	Line : Pchar;
begin
	BeginPaint(Handle, ps);
		CDC := CreateCompatibleDC(ps.Hdc);
		SelectObject(CDC, FBackBuffer);


		FillRect(CDC, EditorRect, 0);
		FGutter.Draw(CDC, Ps.rcPaint);

		LineStart := OffsetY;
		LineEnd := LineStart + (EditorRect.Bottom div CharHeight) + 1;
		if LineEnd > FVisibleLines.Count - 1 then LineEnd := Count - 1;

		LineTop := 0;
		rt := EditorRect;
		rt.Bottom := CharHeight;
		for i := LineStart to LineEnd do
		begin
            if i > FVisibleLines.Count - 1 then continue;
            LineO := TLine(FVisibleLines[i]);
            if LineO = nil then continue;
            LLeft := rt.Left;
            LineO.Draw(cdc, Rt.Left, Rt.Top, OffsetX);
			OffsetRect(rt, 0, CharHeight);
		end;

		BitBlt(ps.HDC,
			Ps.rcPaint.Left,
			Ps.rcPaint.Top,
			Ps.rcPaint.Right - Ps.rcPaint.Left,
			Ps.rcPaint.Bottom - Ps.rcPaint.Top,
			CDC,
			Ps.rcPaint.Left,
			Ps.rcPaint.Top,
			SRCCOPY);

		DeleteDc(CDC);
	EndPaint(Handle, ps);
end;

procedure TEceEditorWindow.wmSize(var msg : TWMSize);
var
	Rt : TRect;
begin
    if FUpdateLockCount <> 0 then exit;

	//Создаем задний буфер
	DeleteObject(FBackBuffer);

	GetWindowRect(Handle, Rt);
	FBackBuffer := CreateBitmap(Rt.Right, Rt.Bottom, 1, 32, nil);

	//Изменяем размер страницы скролла

	VScroll.Page := EditorRect.Bottom div CharHeight;
	HScroll.Page := (EditorRect.Right - EditorRect.Left) div CharWidth;

	onVScroll(VScroll.Pos, true);
	onHScroll(HScroll.Pos, true);

	InvalidateRect(Handle, nil, false);
end;

function TEceEditorWindow._AddLine: IEceLine;
begin
  Result := AddLine;
end;

procedure TEceEditorWindow._BeginUpdate;
begin
  BeginUpdate;
end;

procedure TEceEditorWindow._DeleteLine(Index: Integer);
begin
  DeleteLine(Index);
end;

procedure TEceEditorWindow._EndUpdate;
begin
  EndUpdate;
end;

function TEceEditorWindow._GetCaret: ICaret;
begin
  Result := Caret;
end;

function TEceEditorWindow._GetGutter: IGutter;
begin
  Result := Gutter;
end;

function TEceEditorWindow._GetLines(AIndex: Integer): IEceLine;
begin
    Result := Lines[AIndex];
end;

function TEceEditorWindow._GetLinesCount: Integer;
begin
  Result := FLines.Count
end;

function TEceEditorWindow._InsertLine(Index: Integer): IEceLine;
begin
  Result :=InsertLine(Index);
end;

procedure TEceEditorWindow.wmGetDlgCode(var msg : TWmGetDlgCode);
begin
	msg.Result := 	DLGC_HASSETSEL or
			DLGC_WANTALLKEYS or
			DLGC_WANTARROWS or
			DLGC_WANTCHARS or
			DLGC_WANTMESSAGE or
			DLGC_WANTTAB;
end;

procedure TEceEditorWindow.wmKeyDown(var msg : TWmKeyDown);
begin
	case msg.CharCode of
		VK_UP:		Caret.Y := Caret.Y - 1;
		VK_DOWN:	Caret.Y := Caret.Y + 1;
		VK_LEFT:	Caret.X := Caret.X - 1;
		VK_RIGHT:	Caret.X := Caret.X + 1;
        VK_HOME:
            begin
                Caret.x := 0;
            end;
        VK_END:
            begin
                Caret.x := Lines[Caret.Y].Length;
            end;
        VK_DELETE:
            begin
                lines[Caret.y].Delete(Caret.x+1,1);
            end;
	end;
end;

procedure TEceEditorWindow.wmChar(var msg : TWmChar);
begin
    inherited;
    try
        case msg.CharCode of
        VK_RETURN:
            begin
                { тут надо еще сделать автоотступы и все такое }
                lines[Caret.y].BreakLine(Caret.x+1);
                Caret.X := 0;
                Caret.Y := Caret.Y + 1;
            end;
        VK_BACK:
            begin
                if caret.x > 0 then
                begin
                    //Удаляем символ
                    lines[Caret.y].Delete(Caret.x,1);
                    Caret.x := Caret.x - 1;
                end
                else
                begin
                   if Caret.y > 0 then
                   begin
                       Lines[Caret.y-1].Text := Lines[Caret.y-1].Text + Lines[Caret.y].Text;
                       Caret.x := Lines[Caret.y - 1].Length - Lines[Caret.y].Length;
                       DeleteLine(Caret.y);
                       Caret.y := Caret.y - 1;
                       LineModification;
                       Invalidate;
                    end;
                end;
            end;
        VK_ESCAPE:
            begin

            end;
        VK_TAB:
            begin
                lines[Caret.y].Insert('    ', Caret.x+1);
                Caret.x := Caret.x + 4;
            end;
        else
            lines[Caret.y].Insert(Char(msg.CharCode), Caret.x+1);
            Caret.x := Caret.x + 1;
        end
    except

    end;
end;


procedure TEceEditorWindow.wmSetCursor(var msg : TWmSetCursor);
var
	pt : TPoint;
begin
	GetCursorPos(pt);
	ScreenToClient(Handle, pt);

	if msg.Hittest = 1 then
	begin
		if pt.x < Gutter.Size then
		begin
			SetCursor(LoadCursor(0, IDC_ARROW));
		end
		else
		begin
			SetCursor(LoadCursor(0, IDC_IBEAM));
		end;
	end
	else
	inherited;
end;

procedure TEceEditorWindow.wmLbuttonDown(var msg : TWmLButtonDown);
var
    LineO : TLine;
begin
    if (msg.xPos < Gutter.Size)and(msg.xPos > Gutter.Size - CharHeight) then
    { Нажали по кнопке сворачивания блока }
    begin
        try
            LineO := TLine(FVisibleLines[(Msg.yPos div CharHeight) + OffsetY]);
            if LineO.isRollBlock then
                LineO.isRollUp := not LineO.isRollUp;
        except

        end;
    end;
end;

procedure TEceEditorWindow.SetFocus;
begin
    Inherited;
    Caret.Show;
    Caret.Update;
end;

procedure TEceEditorWindow.KillFocus;
begin
    Inherited;
    Caret.Hide;
end;

procedure TEceEditorWindow.SaveToFile(AFileName : string);
var
	i : integer;
	f : TextFile;
begin
	AssignFile(f, AFileName);
	Rewrite(f);
	for i := 0 to Count - 1 do
		WriteLn(f, Strings[i]);
	CloseFile(f);
end;

procedure TEceEditorWindow.LoadFromFile(AFileName : string);
var
	f : TextFile;
	ln : String;
	Len, MaxLen :  integer;
begin
    try
        BeginUpdate;
        DocumentState := DsLoading;
	    Clear;
    	AssignFile(f, AFileName);
    	Reset(f);

    	MaxLen := 0;
    	While not eof(f) do
    	begin

	    	ReadLn(f, ln);
		    Ln := StringReplace(ln, #9, #32#32#32#32, [rfReplaceAll]);
    		AddString(ln);

	    	Len := Length(ln);
    		if Len > Maxlen then MaxLen := Len;
    	end;
    	CloseFile(f);

        { Сворачиваем свои жалкие блоки }
        Lines[4].FRolllUpFor := Lines[12];
        Lines[14].FRolllUpFor := Lines[172];

	    HScroll.Max := MaxLen;
    finally
        DocumentState := DsReady;
        EndUpdate;
    end;
end;

procedure TEceEditorWindow.LoadPlugin(AFileName: string);
type
  PGetPlugin = function : IEceEditorPlugin; safecall;
var
  hPlugin : HMODULE;
  LoadProc : PGetPlugin;
  Plugin : IEceEditorPlugin;
begin
  hPlugin := LoadLibrary(PChar(AFileName));
  if hPlugin = 0 then raise EErr.Create('Не удалось загрузить модуль '+AFileName);

  LoadProc := GetProcAddress(hPlugin, 'GetPlugin');
  if @LoadProc = nil then
  begin
    FreeLibrary(hPlugin);
    raise EErr.Create('GetPlugin не найден в таблице экспорта модуля '+AFileName);
  end;

  Plugin := LoadProc;
  Plugin.Load(Self);

  FPlugins.Add(Plugin);
end;

Constructor TEceEditorWindow.Create(AParent : Cardinal);
begin
	Inherited;
	//Устанавливаем шрифт
	SetFont('Lucida Console', 16);
	//Создаем строки
	FLines := TList.Create;
    FVisibleLines := TList.Create;
	AddLine;

	FGutter := TGutter.Create(Self);
	FCaret := TCaret.Create(self);

	//В целях профилактики
	SendMessage(Handle, WM_SIZE, 0, 0);

  FPlugins := TInterfaceList.Create;
end;

Destructor TEceEditorWindow.Destroy;
begin
	if Assigned(FCaret) then FCaret.free;
	if Assigned(FGutter) then FGutter.free;
  if Assigned(FVisibleLines) then FVisibleLines.Free;

  if Assigned(FLines) then
  begin
      {todo: Clear}
    FLines.Free;
  end;

  if Assigned(FPlugins) then
  begin
    FPlugins.Clear;
    FPlugins.Free;
  end;

	inherited;
end;

procedure TEceEditorWindow.Invalidate;
begin
     InvalidateRect(Handle, Nil, false);
end;

Function TEceEditorWindow.GetCount : Integer;
begin
	Result := Flines.Count;
end;

Function TEceEditorWindow.GetStrings(const Index : integer) : string;
begin
	Result := Lines[index].Text;
end;

procedure TEceEditorWindow.SetStrings(const Index : integer; const value : string);
begin
	Lines[index].Text := value;
	{todo: строка сама уведомит нас о том, что изменилась}
end;

function TEceEditorWindow.GetLines(const index : integer) : TLine;
begin
	if (index < 0) or (index > count - 1) then
	raise EErr.Create('GetLines: Неверный индекс строки');
	Result := Tline(FLines[index]);
end;

procedure TEceEditorWindow.Clear;
begin
	while Count <> 0 do
		DeleteLine(0);
end;

function TEceEditorWindow.AddString(const ANewString : String) : integer;
begin
	InsertString(ANewString, FLines.Count);
end;

function TEceEditorWindow.InsertString(Const ANewString : string; AIndex : integer): integer;
begin
    with InsertLine(AIndex) do
        Text := ANewString;
    Result := AIndex;
end;

function TEceEditorWindow.AddLine : TLine;
var
    index : integer;
begin
	Result := InsertLine(FLines.Count);
end;

function TEceEditorWindow.InsertLine(AIndex : integer) : TLine;
begin
    Result := TLine.Create(Self);
    try
        FLines.Insert(AIndex, Result);
        { Тут мы не просто вставляем, а еще и делаем проверку - видно ли строку над и под ней }
        FVisibleLines.Insert(AIndex, Result);
    except
        Result.Free;
        raise EErr.Create('InsertLine: Неверный индекс строки');
    end;

    if FUpdateLockCount > 0 Then exit;
    LineModification;
    SendMessage(Handle, WM_SIZE, 0, 0);
end;

procedure TEceEditorWindow.DeleteLine(const index : integer);
var
	Line : TLine;
    LIndex : Integer;
begin
	Line := Lines[index]; //Ни каких проверок. У вслучае чего-тут исключение

    LIndex := FVisibleLines.IndexOf(Line);
    if LIndex <> -1 then
    begin
        FVisibleLines.Delete(LIndex);
    end;

    FLines.Delete(index);
	Line.Free;
	LineModification;
end;

Procedure TEceEditorWindow.BeginUpdate;
begin
    inc(FUpdateLockCount)
end;

Procedure TEceEditorWindow.EndUpdate;
begin
    dec(FUpdateLockCount);
    if FUpdateLockCount < 0 then
        FUpdateLockCount := 0
    else
    if FUpdateLockCount = 0 then
    begin
        LineModification;
        SendMessage(Handle, WM_SIZE, 0, 0);
        Invalidate;
    end;
end;


procedure TEceEditorWindow.SetFont(AFont : String; Size : Integer);
var
	BoldVal : integer;
	dc :  HDC;
	i : integer;
	Sz : TSize;
begin
	BoldVal  := 600;

	for i := 0 to 3 do DeleteObject(FFOnts[i]);

	//нормальный, жырный,курсив, и жирный курсив. по порядку
	FFonts[0] := CreateFont(Size, 0, 0, 0, 0,    	 0, 0, 0, DEFAULT_CHARSET, 0, 0, 0, 0,Pchar(AFont));
	FFonts[1] := CreateFont(Size, 0, 0, 0, BoldVal,  0, 0, 0, DEFAULT_CHARSET, 0, 0, 0, 0,Pchar(AFont));
	FFonts[2] := CreateFont(Size, 0, 0, 0, 0,        1, 0, 0, DEFAULT_CHARSET, 0, 0, 0, 0,Pchar(AFont));
	FFonts[3] := CreateFont(Size, 0, 0, 0, BoldVal,  1, 0, 0, DEFAULT_CHARSET, 0, 0, 0, 0,Pchar(AFont));

	dc := CreateCompatibleDC(0);

	SelectObject(dc, FFonts[0]);
	GetTextExtentPoint32(dc, '#', 1, FCharSize);

	FFontExtraSpace[0] := 0;

	//Вычитываем сдвиг для символов каждого из начертаний
	for i := 1 to 3 do
	begin
		SelectObject(dc, FFonts[i]);
		GetTextExtentPoint32(dc, '#', 1, sz);
		FFontExtraSpace[i] := FCharSize.cx - sz.cx;
	end;

	DeleteDc(dc);
end;

procedure TEceEditorWindow.SetOffsetX(const value : Integer);
var
	OffS : integer;
	Rt : TRect;
	CDC : HDC;
begin
	if FOffsetX = Value then exit;

	Offs := FOffsetX - Value;
	Rt := EditorRect;
	ScrollWindow(	Handle,
			Offs * CharWidth,
			0,
			nil,
			@Rt);

	FOffsetX := Value;
	HScroll.Pos := FOffsetX;

	//Что бы не вылезти за границы и все синхронизировать
	if HScroll.Pos <> FOffsetX then
	begin
		HScroll.Pos := FOffsetX;
		OffsetX := HScroll.Pos;
	end;
end;

procedure TEceEditorWindow.SetOffsetY(const value : Integer);
var
	Offs : integer;
	Rt : TRect;
begin
	if FOffsetY = Value then exit;

	Offs := FOffsetY - Value;
	Rt := EditorRect;
    rt.Left := 0;
	ScrollWindow(	Handle,
			0,
			Offs * CharHeight,
			nil,
			nil);
	FOffsetY := Value;
	VScroll.Pos := FOffsetY;
	//Что бы не вылезти за границы и все синхронизировать
	if VScroll.Pos <> FOffsetY then
	begin
		VScroll.Pos := FOffsetY;
		OffsetY := VScroll.Pos;
	end;
end;

procedure TEceEditorWindow.onVscroll(pos : integer; EndScroll : boolean);
begin
	OffsetY := pos;
end;

procedure TEceEditorWindow.onHscroll(pos : integer; EndScroll : boolean);
begin
	OffsetX := pos;
end;

function TEceEditorWindow.GetEditorRect : TRect;
begin
	GetClientRect(Handle, Result);
    if not Assigned(FGutter) then exit;
	Result.Left := Gutter.Size;
end;

function TEceEditorWindow.GetCharsInHeight : integer;
begin
	Result := EditorRect.Bottom div CharHeight;
end;

function TEceEditorWindow.GetCharsInWidth : integer;
begin
	with EditorRect do
	Result := (Right - left) div CharWidth;
end;

procedure TEceEditorWindow.LineModification;
begin
	inc(FLineModificationChecker);
	//Изменяем и размер скролла
	VScroll.Max := FVisibleLines.Count;
end;

{ TGutter }

Constructor TGutter.Create(AEditor : TEceEditorWindow);
begin
	FEditor := AEditor;
end;


function GetDecCound(i : integer) : Cardinal;
begin
    result := 1;
    if i < 10 then exit;
    repeat
        inc(result);
        i := i div 10;
    until i < 10;
end;

Procedure TGutter.Draw(DC : HDC; Rt : TRect);
var
	Brush : HBrush;
    Pen : HPen;
    Font : HFont;
    FontColor : Cardinal;
    bkMode : Cardinal;
    i : integer;
    LineO : TLine;
    LineNum : integer;
    Text : String;
    FormatStr : String;
begin
	//Что бы не рисовать то, что не нужно
	if Rt.Left > Size Then Exit;

	Rt.Left := 0;
	Rt.Right := Size;

	Brush := GetSysColorBrush(COLOR_BTNFACE);
	 FillRect(DC, Rt, Brush);
	DeleteObject(Brush);

    Pen := SelectObject(Dc, CreatePen(PS_SOLID, 1, GetSysColor(COLOR_BTNSHADOW)));
        MoveToEx(Dc, Rt.Right-1, Rt.Top, nil);
        LineTo(dc, Rt.Right-1, Rt.Bottom);
    DeleteObject(SelectObject(dc, Pen));

    bkMode := SetBkMode(Dc, TRANSPARENT);
    SelectObject(dc, FEditor.FFonts[0]);
    FontColor := SetTextColor(Dc, GetSysColor(COLOR_BTNSHADOW));

    FormatStr := '%.' + IntToStr(GetDecCound(FEditor.FLines.Count)) + 'd';
    for i := (Rt.Top div FEditor.CharHeight) to (rt.Bottom div FEditor.CharHeight) do
    begin
        LineNum := i + FEditor.OffsetY;
        if LineNum < FEditor.FVisibleLines.Count then
        begin
            LineO := TLine(FEditor.FVisibleLines[LineNum]);
            LineNum := LineO.LineIndex
        end
        else continue;

        Text := Format(FormatStr, [LineNum+1]);
        { Тут типа +/- для сворачиваемых блоков }
        if Lineo.isRollBlock then
        begin
            if LineO.isRollUp then
                text := Text + '+'
                else
                text := Text + '-'
        end;
        TextOut(Dc, Rt.Left, i * FEditor.CharHeight, Pchar(text), Length(text));
    end;
    //Если блок свернут - рисуем линию

    SetTextColor(Dc, FontColor);
    SetBkMode(Dc, bkMode);
    SelectObject(dc, Font);
end;

Function TGutter.GetSize : Cardinal;
begin
	{todo: ширина гуттера - все символы + 2 на бордюр + 1 смвол на кнопку "Свернуть"}
    Result := 2 + FEditor.CharWidth + (GetDecCound(FEditor.FLines.Count)) * FEditor.CharWidth;
    if FSize <> Result then
    begin
        FSIze := Result;
        FEditor.Invalidate;
    end;
end;

{ TLine }

Constructor TLine.Create(AEditor : TEceEditorWindow);
begin
    inherited Create;
	FEditor := AEditor;
	FVisible := True;
	FLineIndex := -1;
	//потом создадим
    	//FRollUpLines := TList.Create;
end;

Destructor TLine.Destroy;
begin
    if Assigned(FRollUpLines) then
    begin
        { todo: Развернуть что завернули }
        FRollUpLines.free;
    end;
    inherited;
end;

procedure TLine.Draw(dc : HDC; cx, cy, StartChar : Integer);
var
  i, Count : Integer;
  Pen : HPEN;
  Brush : HBRUSH;
  Char : PChar;
  ChWidth : Integer;
  bid : TLogBrush;
begin
  Count := Length - StartChar;
  if Count < 0 then exit;
  Char := PChar(FText) + StartChar;
  ChWidth := FEditor.CharWidth;
  for i := 0 to Count - 1 do
  begin
    SelectObject(DC, FEditor.FFonts[0]);
    SetBkMode(DC, TRANSPARENT);
    if Char^ in [',', ';', ':', '=', '>', '<'] then
        SetTextColor(dc, $ff0000) else
    if Char^ in ['[', ']', '(', ')'] then
        SetTextColor(dc, $ff00ff) else
    if Char^ in ['@', '$', '&'] then
        SetTextColor(dc, $0000ff) else
    if Char^ in ['0'..'9'] then
        SetTextColor(dc, $008000) else
        SetTextColor(dc, $000000);

    TextOut(dc, cx, cy, Char, 1);
    inc(char);
    Inc(cx, ChWidth);
  end;

  //Если блок свернут
  if isRollUp then
  begin
    inc(Count, 3);
    if Count < 4 then Count := 4;
    
    Pen := SelectObject(dc, CreatePen(PS_DOT, 1, 0));
    bid.lbStyle := BS_HOLLOW;
    bid.lbColor := 0;
    bid.lbHatch := 0;
    Brush := SelectObject(dc, CreateBrushIndirect(bid));
    Rectangle(dc, FEditor.Gutter.Size,
                  cy,
                  FEditor.Gutter.Size + Count * ChWidth,
                  cy + FEditor.CharHeight);
    DeleteObject(SelectObject(dc, pen));
  end;
end;

Function TLine.GetText : String;
begin
	Result := FText;
end;

Procedure TLine.SetText(const value : String);
begin
	FText := value;
	{todo: Известить об изменении}
end;

procedure TLine.SetVisible(const value : boolean);
begin
	FVisible := value;
	{todo: Известить об изменении}
end;

function TLine._GetIndex: Integer;
begin
  Result := LineIndex;
end;

function TLine._GetText: string;
begin
  Result := Text;
end;

function TLine._SetText(Text: string): Integer;
begin
  Self.Text := Text;
end;

function TLine.GetLineIndex : integer;
begin
	{todo: безобразие.
	Эта функция будет проверять - соответствует ли ее последний индекс времени
	изменения числа строк текущему времени последнего изменения числа строк
	редактора, и если они не совпадают, то будет получать индекс по-новой,
	после чего обновит свой индекс последнего изменения числа строк}
	if FLineModificationChecker <> FEditor.FLineModificationChecker then
	begin
		FLineIndex := FEditor.FLines.IndexOf(Self);
		FLineModificationChecker := FEditor.FLineModificationChecker;
	end;
	Result := FLineIndex;
end;

function TLine.GetLength : integer;
begin
    Result := System.Length(FText);
end;

Function TLine.GetisRollBlock : boolean;
begin
    Result := FRolllUpFor <> nil;
end;

Procedure TLine.SetIsRollUp(const value : boolean);
var
    index : integer;
    i : integer;
    EndIndex : integer;
    LineO : TLine;
begin
    if FisRollUp = value then exit;
    if isRollBlock then
    begin
        FisRollUp := value;
        if isRollUp then
        begin
            { Сворачиваем }
            Index := FEditor.FVisibleLines.IndexOf(self)+1;
            EndIndex := FEditor.FLines.IndexOf(FRolllUpFor);
	    if not Assigned(FRollUpLines) then FRollUpLines := TList.Create;
            repeat
                LineO := TLine(FEditor.FVisibleLines[Index]);
                if (LineO.LineIndex <= EndIndex) then
                begin
                    LineO.FRollUpIn := self;
                    FRollUpLines.Add(LineO);
                    FEditor.FVisibleLines.Delete(Index);
                end;
            until (LineO = nil)or(LineO.LineIndex > EndIndex);
        end
            else
        begin
            { Разворачиваем }
            Index := FEditor.FVisibleLines.IndexOf(self)+1;
            for i := FRollUpLines.Count - 1 downto 0 do
            begin
                LineO := TLine(FRollUpLines[i]);
                LineO.FRollUpIn := nil;
                FEditor.FVisibleLines.Insert(index, LineO);
            end;
            FRollUpLines.Clear;
	        if Assigned(FRollUpLines) then FRollUpLines.Free;
            FRollUpLines := nil;
        end;
        FEditor.Invalidate;
        FEditor.LineModification;
    end
    else
         FisRollUp := false;
end;

Procedure TLine.Insert(const AString : String; AChar : integer);
begin
   { Добавление пробелов, если выходим за границы строкДобавление пробелов, если выходим за границы строДобавление пробелов, если выходим за границы строкДобавление пробелов, если выходим за границы строкиикии }
    while (System.Length(FText) < AChar-1) do FText := FText + ' ';

    System.Insert(AString, FText, AChar);
    Invalidate;
end;

procedure TLine.Delete(AChar, ACount :integer);
begin
    System.Delete(FText, Achar, ACount);
    Invalidate;
end;

Procedure TLine.BreakLine(AChar: integer);
var
    LineAfter : string;
begin
    LineAfter := Copy(FText, AChar, Length);
    System.Delete(FText, AChar, Length);
    FEditor.InsertString(LineAfter, LineIndex + 1);
    { Invalidate; }
    FEditor.Invalidate;
end;

Procedure TLine.Invalidate;
var
    rt : TRect;
begin
    if FEditor.FUpdateLockCount <> 0 then exit;
    GetClientRect(FEditor.Handle, rt);
    rt.Left := FEditor.FGutter.Size;
    rt.Top := (LineIndex - FEditor.Offsety) * FEditor.CharHeight;
    rt.Bottom := rt.Top + FEditor.CharHeight;
    InvalidateRect(FEditor.Handle, @rt, false);
end;

{ TCaret }

Constructor TCaret.Create(AEditor : TEceEditorWindow);
begin
	FEditor := AEditor;
	Update;
end;

procedure TCaret.SetX(const value : integer);
begin
	FX := value;
	Update;
end;

procedure TCaret.SetY(const value : integer);
begin
	FY := value;
	Update;
end;

procedure TCaret.SetStyle(Const value : TCaretStyle);
begin
    FStyle := value;
    FEditor.Invalidate;
    Update;
    Hide;
    Show;
end;

procedure TCaret.Show;
begin
    case Style of
    csNormal:
        CreateCaret(FEditor.Handle,0, 2, FEditor.CharHeight);
    csClassic:
        CreateCaret(FEditor.Handle,0, FEditor.CharWidth, 2);
    end;
    ShowCaret(FEditor.Handle);
end;

procedure TCaret.Hide;
begin
    HideCaret(FEditor.Handle);
    DestroyCaret;
end;

procedure TCaret.Update;
var
    cx, cy : integer;
begin
	if Fx < 0 then Fx := 0;
	if Fy < 0 then Fy := 0;
	if Fy > FEditor.FVisibleLines.Count - 1 then Fy := FEditor.Count - 1;

	with FEditor do
	begin
		if Fx < OffsetX then OffsetX := Fx;
		if Fy < OffsetY then Offsety := Fy;

		if Fx > HScroll.Max then HScroll.Max := Fx;
		if Fx - CharsInWidth > OffsetX - 1  then OffsetX := fX - CharsInWidth + 1;
		if Fy - CharsInHeight > OffsetY - 1  then OffsetY := fy - CharsInHeight + 1;
	end;
    case Style of
    csNormal :
        begin
            cx := FEditor.FGutter.Size + (Fx - FEditor.OffsetX) * FEditor.CharWidth;
            cy := (Fy - FEditor.OffsetY) * FEditor.CharHeight;
        end;
    csClassic:
        begin
            cx := FEditor.FGutter.Size + (Fx - FEditor.OffsetX) * FEditor.CharWidth;
            cy := (Fy - FEditor.OffsetY + 1) * FEditor.CharHeight - 2;
        end;
    end;
	SetCaretPos(cx, cy);
end;

function TCaret._GetX: Integer;
begin
  Result := X;
end;

function TCaret._GetY: Integer;
begin
  Result := Y;
end;

function TCaret._SetX(value: Integer): Integer;
begin
  Result := X;
  X := value;
end;

function TCaret._SetY(value: Integer): Integer;
begin
  Result := Y;
  Y := value;
end;

end.
