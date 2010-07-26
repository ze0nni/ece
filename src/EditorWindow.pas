unit EditorWindow;
//test comments line
{$IFDEF fpc}{$MODE delphi}{$ENDIF}

interface

uses
  Windows,
  Messages,
  Classes,
  SysUtils,
  iece,
  zeWndControls,
  DocumentWindow,
  math;

type
  TGutter = class;

  TLine = class;

  TTokenClass = class;

  TTokenClassList = class;

  TToken = class;

  TCaret = class;

  TEceEditorWindow = class(TEceDocumentWindow, IDispatch, IEceDocument,
    IEceEditor)
  private
    FFileName: string;
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
    FOffsetX: integer;
    FOffsetY: integer;

    FTokens: TTokenClassList;

    FPlugins: TInterfaceList;
    FTextColor: Integer;
    FBackgroundColor: Integer;

    Function GetCount: integer;
    Function GetStrings(const Index: integer): string;
    procedure SetStrings(const Index: integer; const value: string);
    function GetLines(const index: integer): TLine;
    function GetVisibleLines(const index: integer): TLine;
    procedure SetOffsetX(const value: integer);
    procedure SetOffsetY(const value: integer);
    function GetEditorRect: TRect;
    function GetCharsInHeight: integer;
    function GetCharsInWidth: integer;
    procedure LineModification;
    procedure SetBackgroundColor(const Value: Integer);
    procedure SetTextColor(const Value: Integer);
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
    procedure wmLbuttonUp(var msg: TWMLButtonUp);
    message WM_LBUTTONUP;
    procedure wmMouseMove(var msg: TWMMouseMove);
    message WM_MOUSEMOVE;

    procedure wmMouseWheel(var msg: TWMMouseWheel);
    message WM_MOUSEWHEEL;

    procedure onVscroll(pos: integer; EndScroll: boolean); override;
    procedure onHscroll(pos: integer; EndScroll: boolean); override;

    function GetDocumentFileName: string; override;
    function GetDocumentTitle: string; override;
  protected
    procedure _BeginUpdate; safecall;
    procedure _EndUpdate; safecall;
    function _GetLinesCount: integer; safecall;
    function _GetLines(AIndex: integer): IEceLine; safecall;
    function _GetGutter: IGutter; safecall;
    function _GetCaret: ICaret; safecall;
    function _AddLine: IEceLine; safecall;
    function _InsertLine(Index: integer): IEceLine; safecall;
    procedure _DeleteLine(Index: integer); safecall;
  public
    Constructor Create(Parent: Cardinal; AApplication: IEceApplication);
    Destructor Destroy; override;

    procedure SetFont(AFont: String; Size: integer);

    procedure Invalidate;

    procedure SetFocus; override;
    procedure KillFocus; override;

    procedure SaveToFile(AFileName: string); override;
    procedure LoadFromFile(AFileName: string); override;

    property Caret: TCaret Read FCaret;
    property Gutter: TGutter read FGutter;

    property Tokens: TTokenClassList Read FTokens;
    Property Count: integer Read GetCount;
    property Strings[const index: integer]
      : string Read GetStrings Write SetStrings;
    property Lines[const index: integer]: TLine read GetLines;
    property VisibleLines[const index: integer]: TLine read GetVisibleLines;
    procedure Clear;
    function AddString(const ANewString: string): integer;
    function InsertString(Const ANewString: string; AIndex: integer): integer;

    function AddLine: TLine;
    function InsertLine(AIndex: integer): TLine;
    procedure DeleteLine(const index: integer);

    Procedure BeginUpdate;
    Procedure EndUpdate;

    property BackgroundColor : Integer read FBackgroundColor write SetBackgroundColor;
    property TextColor : Integer read FTextColor write SetTextColor;

    property CharWidth: integer read FCharSize.Cx;
    property CharHeight: integer read FCharSize.Cy;
    property OffsetX: integer Read FOffsetX Write SetOffsetX;
    property OffsetY: integer Read FOffsetY Write SetOffsetY;

    property EditorRect: TRect Read GetEditorRect;
    property CharsInHeight: integer Read GetCharsInHeight;
    property CharsInWidth: integer Read GetCharsInWidth;

    procedure LoadPlugin(AFileName: string);
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

  //���������� ����, ��� �������� ������
  TLineSelectionMode = (selNone, //�� ��������
                        selFull,  //����������
                        selStart, //�������
                        selEnd,   //� �����
                        selMiddle //� ��������
                        );

  TLine = class(TEceInterfacedObject, IDispatch, IEceLine)
  private
    FText: String;
    FVisible: boolean;
    { ��������� �� ������, �� �������, ���� ������� }
    FRolllUpFor: TLine;
    { ������ �������� ����� }
    FRollUpLines: TList;
    { ������, ������� ������� �������� }
    FRollUpIn: TLine;
    { ������ ������� }
    FTokens: TList;
    FLineIndex: integer;
    FVisibleIndex: integer;
    FLineModificationChecker: integer;
    FEditor: TEceEditorWindow;
    FisRollUp: boolean;
    FLevel: integer;
    Function GetText: String;
    Procedure SetText(const value: String);
    procedure SetVisible(const value: boolean);
    function GetLineIndex: integer;
    function GetLength: integer;
    Function GetisRollBlock: boolean;
    Procedure SetIsRollUp(const value: boolean);
    function GetVisibleIndex: integer;
    procedure UpdateLinesIndex;
    function GetisEndInLevel: boolean;
    procedure RestorStyle(DC: HDC);
  protected
    function _GetText: string; safecall;
    function _SetText(Text: string): integer; safecall;
    function _GetIndex: integer; safecall;
    procedure UpdateSyn;
  public
    Constructor Create(AEditor: TEceEditorWindow);
    Destructor Destroy; override;
    Procedure Insert(const AString: String; AChar: integer);
    procedure Delete(AChar, ACount: integer);
    Procedure BreakLine(AChar: integer);
    procedure Draw(DC: HDC; Cx, Cy, StartChar: integer);
    Procedure Invalidate;
    property isRollBlock: boolean read GetisRollBlock;
    property isRollUp: boolean read FisRollUp write SetIsRollUp;
    property Text: String Read GetText write SetText;
    property Visible: boolean read FVisible Write SetVisible;
    property LineIndex: integer read GetLineIndex;
    property VisibleIndex: integer read GetVisibleIndex;
    property Length: integer read GetLength;
    property isEndInLevel: boolean read GetisEndInLevel;
    property Level: integer read FLevel;
  end;

  // ����� ������
  TTokenClassType = (ttSeparator, // �����������, ����� ���� ������ ������
    ttWord, // ������ �����
    ttBeginRegion, // ������ �������
    ttEndRegion, // ����� �������
    // ����, � ������� �� ������� �� �����
    // ���������� ���������
    ttBeginBlock, // ������ �����
    ttEndBlock); // ����� �����

  //�����, ��� ������� �����
  TTokenClass = class
  private
    FEditor: TEceEditorWindow;
    FName: string;
    FTokenType: TTokenClassType;
    FTextColor: integer;
    FFontStyle: integer;
    FUnderline: boolean;
    FBkColor: integer;
    FStrick: boolean;

    // ������ ������� ������ ������� ����� ���������� ����
    FInclueIn: TList;
    // ����� �������, ������ ������� �� ����� ����������
    FExceptOut: TList;

    procedure SetBkColor(const value: integer);
    procedure SetFontStyle(const value: integer);
    procedure SetStrick(const value: boolean);
    procedure SetTextColor(const value: integer);
    procedure SetUnderline(const value: boolean);
  public
    constructor Create(AEditor: TEceEditorWindow);
    property Name: string read FName Write FName;
    property TokenType: TTokenClassType read FTokenType;
    property Editor: TEceEditorWindow read FEditor;
    // ���������� ����������
    property FontStyle: integer read FFontStyle write SetFontStyle;
    property Underline: boolean read FUnderline write SetUnderline;
    property Strick: boolean read FStrick write SetStrick;
    property TextColor: integer read FTextColor write SetTextColor;
    property BkColor: integer read FBkColor write SetBkColor;
  end;

  // ������ ���� ������� ������� ���������
  TTokenClassList = class
  private
    FEditor : TEceEditorWindow;
    FTokens: TStringList;
    function GetTokens(const ATokenKey: string): TTokenClass;
  public
    constructor Create(AEditor : TEceEditorWindow);
    destructor Destroy; override;

    function NewToken(AName: string; AType: TTokenClassType): TTokenClass;
    property Tokens[const ATokenKey : string] : TTokenClass read GetTokens; default;
  end;

  // ��� �����
  TToken = class
  private
    FTokenClass: TTokenClass;
    FLength: Cardinal;
    FFirstChar: Cardinal;
    procedure SetFirstChar(const value: Cardinal);
    procedure SetLength(const value: Cardinal);

  public
    constructor Create(ATokenClass: TTokenClass);
    property TokenClass: TTokenClass read FTokenClass;
    property FirstChar: Cardinal read FFirstChar write SetFirstChar;
    property Length: Cardinal read FLength write SetLength;
    procedure ApplyStyle(DC: HDC);
  end;

  TCaretStyle = (csNormal, csClassic);

  TSelectionRange = record
    SelStart : TPoint;
    SelEnd : TPoint;
  end;

  TCaret = class(TEceInterfacedObject, IDispatch, ICaret)
  private
    FEditor: TEceEditorWindow;
    Fx, Fy: integer;
    FStyle: TCaretStyle;
    FSelection: boolean;
    FSelStartX: integer;
    FSelStartY: integer;
    Procedure SetX(Const value: integer);
    Procedure SetY(Const value: integer);
    procedure SetStyle(Const value: TCaretStyle);
    function GetLine: integer;
    function GetSelectionMode: Boolean;
    function GetHaveSelection: Boolean;
    function GetSelectionRange: TSelectionRange;
  protected
    function _GetX: integer; safecall;
    function _GetY: integer; safecall;
    function _SetX(value: integer): integer; safecall;
    function _SetY(value: integer): integer; safecall;
  public
    Constructor Create(AEditor: TEceEditorWindow);
    property X: integer read Fx Write SetX;
    property Y: integer read Fy write SetY;
    // ����������� �������� �������
    property SelStartX: integer read FSelStartX;
    property SelStartY: integer read FSelStartY;
    property SelectionMode : Boolean read GetSelectionMode;
    property HaveSelection : Boolean read GetHaveSelection;
    property Line: integer Read GetLine;
    property Style: TCaretStyle read FStyle Write SetStyle;
    property SelectionRange : TSelectionRange read GetSelectionRange;
    procedure Show;
    procedure Hide;
    procedure Update;
  end;

implementation

function isKeyDown(key: integer): boolean;
begin
  Result := (GetKeyState(key) and 128) = 128
end;

{ TEceEditorWindow }

procedure TEceEditorWindow.CreateParams(var Param: CreateStruct);
begin
  inherited;
  Param.Style := Param.Style or WS_VSCROLL or WS_HSCROLL;
  Param.dwExStyle := WS_EX_CLIENTEDGE;
end;

procedure TEceEditorWindow.wmPaint(var msg: TMessage);
var
  Ps: TPaintStruct;
  Rt: TRect;
  CDC: HDC;
  LineStart, LineEnd, i, j, lleft: integer;
  LineTop, LineLen: integer;
  LineO: TLine;
  Line: Pchar;
  Brush: HBrush;
begin
  BeginPaint(Handle, Ps);
  CDC := CreateCompatibleDC(Ps.HDC);
  SelectObject(CDC, FBackBuffer);

  {TODO -oOnni -cGeneral : ��������� ��������� �����}
  Brush := CreateSolidBrush(BackgroundColor);
  FillRect(CDC, EditorRect, Brush);
  DeleteObject(Brush);

  FGutter.Draw(CDC, Ps.rcPaint);

  LineStart := OffsetY;
  LineEnd := LineStart + (EditorRect.Bottom div CharHeight) + 1;
  if LineEnd > FVisibleLines.Count - 1 then
    LineEnd := Count - 1;

  LineTop := 0;
  Rt := EditorRect;
  Rt.Bottom := CharHeight;
  for i := LineStart to LineEnd do
  begin
    if i > FVisibleLines.Count - 1 then
      continue;
    LineO := TLine(FVisibleLines[i]);
    if LineO = nil then
      continue;
    lleft := Rt.Left;
    LineO.Draw(CDC, Rt.Left, Rt.Top, OffsetX);
    OffsetRect(Rt, 0, CharHeight);
  end;

  BitBlt(Ps.HDC, Ps.rcPaint.Left, Ps.rcPaint.Top,
    Ps.rcPaint.Right - Ps.rcPaint.Left, Ps.rcPaint.Bottom - Ps.rcPaint.Top,
    CDC, Ps.rcPaint.Left, Ps.rcPaint.Top, SRCCOPY);

  DeleteDc(CDC);
  EndPaint(Handle, Ps);
end;

procedure TEceEditorWindow.wmSize(var msg: TWMSize);
var
  Rt: TRect;
begin
  if FUpdateLockCount <> 0 then
    exit;

  // ������� ������ �����
  DeleteObject(FBackBuffer);

  GetWindowRect(Handle, Rt);
  FBackBuffer := CreateBitmap(Rt.Right, Rt.Bottom, 1, 32, nil);

  // �������� ������ �������� �������

  VScroll.Page := EditorRect.Bottom div CharHeight;
  HScroll.Page := (EditorRect.Right - EditorRect.Left) div CharWidth;

  onVscroll(VScroll.pos, true);
  onHscroll(HScroll.pos, true);

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

procedure TEceEditorWindow._DeleteLine(Index: integer);
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

function TEceEditorWindow._GetLines(AIndex: integer): IEceLine;
begin
  Result := Lines[AIndex];
end;

function TEceEditorWindow._GetLinesCount: integer;
begin
  Result := FLines.Count
end;

function TEceEditorWindow._InsertLine(Index: integer): IEceLine;
begin
  Result := InsertLine(Index);
end;

procedure TEceEditorWindow.wmGetDlgCode(var msg: TWmGetDlgCode);
begin
  msg.Result :=
    DLGC_HASSETSEL or DLGC_WANTALLKEYS or DLGC_WANTARROWS or DLGC_WANTCHARS or
    DLGC_WANTMESSAGE or DLGC_WANTTAB;
end;

procedure TEceEditorWindow.wmKeyDown(var msg: TWmKeyDown);
begin
  case msg.CharCode of
    VK_UP:
      Caret.Y := Caret.Y - 1;
    VK_DOWN:
      Caret.Y := Caret.Y + 1;
    VK_LEFT:
      Caret.X := Caret.X - 1;
    VK_RIGHT:
      Caret.X := Caret.X + 1;
    VK_HOME:
      begin
        Caret.X := 0;
      end;
    VK_END:
      begin
        Caret.X := Lines[Caret.Line].Length;
      end;
    VK_DELETE:
      begin
        Lines[Caret.Line].Delete(Caret.X + 1, 1);
      end;
  end;
end;

procedure TEceEditorWindow.wmChar(var msg: TWmChar);
var
  LIndex: integer;
begin
  inherited;
  try
    case msg.CharCode of
      VK_RETURN:
        begin
          { ��� ���� ��� ������� ����������� � ��� ����� }
          Lines[Caret.Line].BreakLine(Caret.X + 1);
          Caret.X := 0;
          Caret.Y := Caret.Y + 1;
        end;
      VK_BACK:
        begin
          if Caret.X > 0 then
          begin
            // ������� ������
            Lines[Caret.Line].Delete(Caret.X, 1);
            Caret.X := Caret.X - 1;
          end
          else
          begin
            if Caret.Y > 0 then
            begin
              LIndex := Caret.Line;
              Caret.X := Lines[LIndex - 1].Length;
              Caret.Y := Caret.Y - 1;
              // todo: ��� ��������� � ��������� ������ ��� ������ ������������
              Lines[LIndex - 1].Text := Lines[LIndex - 1].Text + Lines[LIndex]
                .Text;
              DeleteLine(LIndex);
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
          Lines[Caret.Line].Insert('  ', Caret.X + 1);
          Caret.X := Caret.X + 2;
        end;
    else
      begin
        Lines[Caret.Line].Insert(Char(msg.CharCode), Caret.X + 1);
        Caret.X := Caret.X + 1;
      end;
    end;
  except
  end;
end;

procedure TEceEditorWindow.wmSetCursor(var msg: TWmSetCursor);
var
  pt: TPoint;
begin
  GetCursorPos(pt);
  ScreenToClient(Handle, pt);

  if msg.Hittest = 1 then
  begin
    if pt.X < Gutter.Size then
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

procedure TEceEditorWindow.wmLbuttonDown(var msg: TWmLbuttonDown);
var
  LineO: TLine;
begin
  if (msg.xPos < Gutter.Size) and (msg.xPos > Gutter.Size - CharHeight) then
  { ������ �� ������ ������������ ����� }
  begin
    try
      LineO := TLine(FVisibleLines[(msg.yPos div CharHeight) + OffsetY]);
      if LineO.isRollBlock then
        LineO.isRollUp := not LineO.isRollUp;
    except

    end;
  end
  else if (msg.xPos > Gutter.Size) then
  { ������ �� ���� }
  begin
    SetCapture(Handle);
    Caret.Y := (msg.yPos div CharHeight) + OffsetY;
    Caret.X := ((msg.xPos + (CharWidth div 2) - Gutter.Size) div CharWidth)
      + OffsetX;
  end;
end;

procedure TEceEditorWindow.wmLbuttonUp(var msg: TWMLButtonUp);
begin
  ReleaseCapture;
end;

procedure TEceEditorWindow.wmMouseMove(var msg: TWMMouseMove);
begin
  if GetCapture = Handle then
  begin
    Caret.X := ((msg.xPos + (CharWidth div 2) - Gutter.Size) div CharWidth)
      + OffsetX;
    Caret.Y := (msg.yPos div CharHeight) + OffsetY;
  end;
end;

procedure TEceEditorWindow.wmMouseWheel(var msg: TWMMouseWheel);
begin
  if msg.Keys = 0 then
  begin
    if msg.WheelDelta < 0 then
      OffsetY := OffsetY + 3
    else
      OffsetY := OffsetY - 3
  end
  else if (msg.Keys and MK_CONTROL) = MK_CONTROL then
  begin
    if msg.WheelDelta < 0 then
      OffsetY := OffsetY + VScroll.Page
    else
      OffsetY := OffsetY - VScroll.Page
  end
  else if (msg.Keys and MK_SHIFT) = MK_SHIFT then
  begin
    if msg.WheelDelta < 0 then
      Caret.Y := Caret.Y + 1
    else
      Caret.Y := Caret.Y - 1;
  end;
end;

procedure TEceEditorWindow.SetBackgroundColor(const Value: Integer);
begin
  FBackgroundColor := Value;
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

procedure TEceEditorWindow.SaveToFile(AFileName: string);
var
  i: integer;
  f: TextFile;
begin
  AssignFile(f, AFileName);
  Rewrite(f);
  for i := 0 to Count - 1 do
    WriteLn(f, Strings[i]);
  CloseFile(f);
end;

procedure TEceEditorWindow.LoadFromFile(AFileName: string);
var
  f: TextFile;
  ln: String;
  Len, MaxLen: integer;
  index: Integer;
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
      ln := StringReplace(ln, #9, #32#32, [rfReplaceAll]);
      index := AddString(ln);
      Lines[index].UpdateSyn;

      Len := Length(ln);
      if Len > MaxLen then
        MaxLen := Len;
    end;
    CloseFile(f);

    { ����������� ���� ������ ����� }
    // Lines[4].FRolllUpFor := Lines[12];
    // Lines[5].FLevel := 1;
    // Lines[6].FLevel := 1;
    // Lines[7].FLevel := 1;
    // Lines[8].FLevel := 1;
    // Lines[9].FLevel := 1;
    // Lines[10].FLevel := 1;
    // Lines[11].FLevel := 1;
    // Lines[12].FLevel := 1;
    // Lines[14].FRolllUpFor := Lines[225];

    Lines[2].FRolllUpFor := Lines[4];
    Lines[3].FLevel := 1;
    Lines[4].FLevel := 1;

    Lines[6].FRolllUpFor := Lines[8];
    Lines[7].FLevel := 1;
    Lines[8].FLevel := 1;

    HScroll.Max := MaxLen;
  finally
    DocumentState := DsReady;
    EndUpdate;
  end;

  FFileName := AFileName;
  Application._UpdateCaption;
end;

procedure TEceEditorWindow.LoadPlugin(AFileName: string);
type
  PGetPlugin = function: IEceEditorPlugin; safecall;
var
  hPlugin: HMODULE;
  LoadProc: PGetPlugin;
  Plugin: IEceEditorPlugin;
begin
  hPlugin := LoadLibrary(Pchar(AFileName));
  if hPlugin = 0 then
    raise Exception.Create('�� ������� ��������� ������ ' + AFileName);

  LoadProc := GetProcAddress(hPlugin, 'GetPlugin');
  if @LoadProc = nil then
  begin
    FreeLibrary(hPlugin);
    raise Exception.Create('GetPlugin �� ������ � ������� �������� ������ ' +
        AFileName);
  end;

  Plugin := LoadProc;
  Plugin.Load(Self);

  FPlugins.Add(Plugin);
end;

Constructor TEceEditorWindow.Create(Parent: Cardinal;
  AApplication: IEceApplication);
begin
  Inherited;
  // ������������� �����
  SetFont('Lucida Console', 16);
  // ������� ������
  FLines := TList.Create;
  FVisibleLines := TList.Create;
  AddLine;

  FGutter := TGutter.Create(Self);
  FCaret := TCaret.Create(Self);

  FTokens := TTokenClassList.Create(Self);

  // � ����� ������������
  SendMessage(Handle, WM_SIZE, 0, 0);

  FPlugins := TInterfaceList.Create;
end;

Destructor TEceEditorWindow.Destroy;
var
  i: Integer;
begin
  if Assigned(FCaret) then
    FCaret.free;
  if Assigned(FGutter) then
    FGutter.free;
  if Assigned(FVisibleLines) then
    FVisibleLines.free;

  if Assigned(FLines) then
  begin
    for i := 0 to FLines.Count - 1 do
      TLine(FLines[i]).Free;
    FLines.free;
  end;

  if Assigned(FPlugins) then
  begin
    FPlugins.Clear;
    FPlugins.free;
  end;

  if Assigned(FTokens) then
  begin
    FTokens.free;
  end;

  inherited;
end;

procedure TEceEditorWindow.Invalidate;
begin
  InvalidateRect(Handle, Nil, false);
end;

Function TEceEditorWindow.GetCount: integer;
begin
  Result := FLines.Count;
end;

function TEceEditorWindow.GetDocumentFileName: string;
begin
  Result := FFileName;
end;

function TEceEditorWindow.GetDocumentTitle: string;
begin
  Result := ExtractFileName(FFileName);
end;

Function TEceEditorWindow.GetStrings(const Index: integer): string;
begin
  Result := Lines[index].Text;
end;

function TEceEditorWindow.GetVisibleLines(const index: integer): TLine;
begin
  if (index < 0) or (index > FVisibleLines.Count - 1) then
    raise Exception.Create('�������� ������ ������');
  Result := TLine(FVisibleLines[index]);
end;

procedure TEceEditorWindow.SetStrings(const Index: integer;
  const value: string);
begin
  Lines[index].Text := value;
  { todo: ������ ���� �������� ��� � ���, ��� ���������� }
end;

procedure TEceEditorWindow.SetTextColor(const Value: Integer);
begin
  FTextColor := Value;
end;

function TEceEditorWindow.GetLines(const index: integer): TLine;
begin
  if (index < 0) or (index > Count - 1) then
    raise Exception.Create('�������� ������ ������');
  Result := TLine(FLines[index]);
end;

procedure TEceEditorWindow.Clear;
begin
  while Count <> 0 do
    DeleteLine(0);
end;

function TEceEditorWindow.AddString(const ANewString: String): integer;
begin
  Result := InsertString(ANewString, FLines.Count);
end;

function TEceEditorWindow.InsertString(Const ANewString: string;
  AIndex: integer): integer;
begin
  with InsertLine(AIndex) do
    Text := ANewString;
  Result := AIndex;
end;

function TEceEditorWindow.AddLine: TLine;
var
  index: integer;
begin
  Result := InsertLine(FLines.Count);
end;

function TEceEditorWindow.InsertLine(AIndex: integer): TLine;
var
  vIndex: integer;
begin
  Result := TLine.Create(Self);
  try
    FLines.Insert(AIndex, Result);
    { ��� �� �� ������ ���������, � ��� � ������ ��������
      - ����� �� ������ ��� � ��� ��� }
    FVisibleLines.Insert(AIndex, Result);
  except
    Result.free;
    raise Exception.Create('�������� ������ ������');
  end;

  if FUpdateLockCount > 0 Then
    exit;
  LineModification;
  SendMessage(Handle, WM_SIZE, 0, 0);
end;

procedure TEceEditorWindow.DeleteLine(const index: integer);
var
  Line: TLine;
  LIndex: integer;
begin
  Line := Lines[index]; // �� ����� ��������. � ������� ����-��� ����������

  LIndex := FVisibleLines.IndexOf(Line);
  if LIndex <> -1 then
  begin
    FVisibleLines.Delete(LIndex);
  end;

  FLines.Delete(index);
  Line.free;
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
  else if FUpdateLockCount = 0 then
  begin
    LineModification;
    SendMessage(Handle, WM_SIZE, 0, 0);
    Caret.Update;
    Invalidate;
  end;
end;

procedure TEceEditorWindow.SetFont(AFont: String; Size: integer);
var
  BoldVal: integer;
  DC: HDC;
  i: integer;
  Sz: TSize;
begin
  BoldVal := 600;

  for i := 0 to 3 do
    DeleteObject(FFonts[i]);

  // ����������, ������,������, � ������ ������. �� �������
  FFonts[0] := CreateFont(Size, 0, 0, 0, 0, 0, 0, 0, DEFAULT_CHARSET, 0, 0, 0,
    0, Pchar(AFont));
  FFonts[1] := CreateFont(Size, 0, 0, 0, BoldVal, 0, 0, 0, DEFAULT_CHARSET, 0,
    0, 0, 0, Pchar(AFont));
  FFonts[2] := CreateFont(Size, 0, 0, 0, 0, 1, 0, 0, DEFAULT_CHARSET, 0, 0, 0,
    0, Pchar(AFont));
  FFonts[3] := CreateFont(Size, 0, 0, 0, BoldVal, 1, 0, 0, DEFAULT_CHARSET, 0,
    0, 0, 0, Pchar(AFont));

  DC := CreateCompatibleDC(0);

  SelectObject(DC, FFonts[0]);
  GetTextExtentPoint32(DC, '#', 1, FCharSize);

  FFontExtraSpace[0] := 0;

  // ���������� ����� ��� �������� ������� �� ����������
  for i := 1 to 3 do
  begin
    SelectObject(DC, FFonts[i]);
    GetTextExtentPoint32(DC, '#', 1, Sz);
    FFontExtraSpace[i] := FCharSize.Cx - Sz.Cx;
  end;

  DeleteDc(DC);
end;

procedure TEceEditorWindow.SetOffsetX(const value: integer);
var
  OffS: integer;
  Rt: TRect;
  CDC: HDC;
begin
  if FOffsetX = value then
    exit;

  OffS := FOffsetX - value;
  Rt := EditorRect;
  ScrollWindow(Handle, OffS * CharWidth, 0, nil, @Rt);

  FOffsetX := value;
  HScroll.pos := FOffsetX;

  // ��� �� �� ������� �� ������� � ��� ����������������
  if HScroll.pos <> FOffsetX then
  begin
    HScroll.pos := FOffsetX;
    OffsetX := HScroll.pos;
  end;
end;

procedure TEceEditorWindow.SetOffsetY(const value: integer);
var
  OffS: integer;
  Rt: TRect;
begin
  if FOffsetY = value then
    exit;

  OffS := FOffsetY - value;
  Rt := EditorRect;
  Rt.Left := 0;
  ScrollWindow(Handle, 0, OffS * CharHeight, nil, nil);
  FOffsetY := value;
  VScroll.pos := FOffsetY;
  // ��� �� �� ������� �� ������� � ��� ����������������
  if VScroll.pos <> FOffsetY then
  begin
    VScroll.pos := FOffsetY;
    OffsetY := VScroll.pos;
  end;
end;

procedure TEceEditorWindow.onVscroll(pos: integer; EndScroll: boolean);
begin
  OffsetY := pos;
end;

procedure TEceEditorWindow.onHscroll(pos: integer; EndScroll: boolean);
begin
  OffsetX := pos;
end;

function TEceEditorWindow.GetEditorRect: TRect;
begin
  GetClientRect(Handle, Result);
  if not Assigned(FGutter) then
    exit;
  Result.Left := Gutter.Size;
end;

function TEceEditorWindow.GetCharsInHeight: integer;
begin
  Result := EditorRect.Bottom div CharHeight;
end;

function TEceEditorWindow.GetCharsInWidth: integer;
begin
  with EditorRect do
    Result := (Right - Left) div CharWidth;
end;

procedure TEceEditorWindow.LineModification;
begin
  inc(FLineModificationChecker);
  // �������� � ������ �������
  VScroll.Max := FVisibleLines.Count;
end;

{ TGutter }

Constructor TGutter.Create(AEditor: TEceEditorWindow);
begin
  FEditor := AEditor;
end;

function GetDecCound(i: integer): Cardinal;
begin
  Result := 1;
  if i < 10 then
    exit;
  repeat
    inc(Result);
    i := i div 10;
  until i < 10;
end;

Procedure TGutter.Draw(DC: HDC; Rt: TRect);
var
  Brush: HBrush;
  Pen: HPen;

  Font: HFont;
  FontColor: Cardinal;
  bkMode: Cardinal;
  i: integer;
  LineO: TLine;
  LineNum: integer;
  Text: String;
  FormatStr: String;
  ert: TRect;
  Cx, Cy: integer;
begin
  // ��� �� �� �������� ��, ��� �� �����
  if Rt.Left > Size Then
    exit;

  Rt.Left := 0;
  Rt.Right := Size;

  Brush := GetSysColorBrush(COLOR_BTNFACE);
  FillRect(DC, Rt, Brush);
  DeleteObject(Brush);

  bkMode := GetBkMode(DC);
  SelectObject(DC, FEditor.FFonts[0]);
  FontColor := GetTextColor(DC);

  Pen := SelectObject(DC, CreatePen(PS_SOLID, 1, GetSysColor(COLOR_BTNSHADOW)));
  MoveToEx(DC, Rt.Right - 1, Rt.Top, nil);
  LineTo(DC, Rt.Right - 1, Rt.Bottom);

  FormatStr := '%0' + IntToStr(GetDecCound(FEditor.FLines.Count)) + 'd';
  for i := (Rt.Top div FEditor.CharHeight) to
    (Rt.Bottom div FEditor.CharHeight) do
  begin
    SetTextColor(DC, GetSysColor(COLOR_BTNSHADOW));
    SetBkMode(DC, TRANSPARENT);

    LineNum := i + FEditor.OffsetY;
    if LineNum < FEditor.FVisibleLines.Count then
    begin
      LineO := TLine(FEditor.FVisibleLines[LineNum]);
      LineNum := LineO.LineIndex
    end
    else
      continue;

    Text := Format(FormatStr, [LineNum + 1]);
    TextOut(DC, Rt.Left, i * FEditor.CharHeight, Pchar(Text), Length(Text));

    { ��� ���� +/- ��� ������������� ������ }
    if LineO.isRollBlock then
    begin
      if LineO.isRollUp then
        Text := '+'
      else
        Text := '-';
      SetTextColor(DC, FEditor.TextColor);
      // SetBkColor(dc, GetSysColor(COLOR_WINDOW));
      // SetBkMode(Dc, OPAQUE);
      Cx := Size - FEditor.CharWidth - 2;
      Cy := i * FEditor.CharHeight;
      ert := Rect(Cx, Cy, Cx + FEditor.CharWidth + 2,
        Cy + FEditor.CharHeight - 1);

      Brush := CreateSolidBrush(FEditor.BackgroundColor);
      FillRect(DC, ert, Brush);
      DeleteObject(Brush);

      MoveToEx(DC, Cx + FEditor.CharWidth + 1, Cy, nil);
      LineTo(DC, Cx, Cy);
      LineTo(DC, Cx, Cy + FEditor.CharHeight - 1);
      LineTo(DC, Cx + FEditor.CharWidth + 1, Cy + FEditor.CharHeight - 1);
      // DrawEdge(DC, ert,
      // BDR_RAISEDOUTER,
      // BF_RECT and not BF_RIGHT or BF_MIDDLE or BF_MONO);
      TextOut(DC, Cx, Cy, Pchar(Text), 1)
    end
    else
    { ����� ������ ������������ ����� }
    begin
      if LineO.Level > 0 then
      begin
        // Pen := SelectObject(Dc, CreatePen(PS_DOT, 1, GetSysColor(COLOR_BTNSHADOW)));
        Cx := (Size - FEditor.CharWidth - 2) + (FEditor.CharWidth div 2);
        Cy := i * FEditor.CharHeight;
        MoveToEx(DC, Cx, Cy, nil);
        LineTo(DC, Cx, Cy + FEditor.CharHeight);

        if LineO.isEndInLevel then
        begin
          { todo: ������� �� ������� ���� }
          MoveToEx(DC, Cx, Cy + (FEditor.CharHeight div 2), nil);
          LineTo(DC, Cx + (FEditor.CharWidth div 2) + 1, Cy +
              (FEditor.CharHeight div 2))
        end;

        // DeleteObject(SelectObject(dc, Pen));
      end;
    end;
  end;

  DeleteObject(SelectObject(DC, Pen));
  SetTextColor(DC, FontColor);
  SetBkMode(DC, bkMode);
  SelectObject(DC, Font);
end;

Function TGutter.GetSize: Cardinal;
begin
  { todo: ������ ������� - ��� ������� + 2 �� ������ + 1 ����� �� ������ "��������" }
  Result := 2 + FEditor.CharWidth + (GetDecCound(FEditor.FLines.Count))
    * FEditor.CharWidth;
  if FSize <> Result then
  begin
    FSize := Result;
    FEditor.Invalidate;
  end;
end;

{ TLine }

Constructor TLine.Create(AEditor: TEceEditorWindow);
begin
  inherited Create;
  FEditor := AEditor;
  FVisible := true;
  FLineIndex := -1;
  // ����� ��������
  // FRollUpLines := TList.Create;
  FTokens := TList.Create;
end;

Destructor TLine.Destroy;
var
  i: Integer;
begin
  if Assigned(FRollUpLines) then
  begin
    { todo: ���������� ��� ��������� }
    FRollUpLines.free;
  end;
  if Assigned(FTokens) then
  begin
    for i := 0 to FTokens.Count - 1 do
      TToken(FTokens[i]).Free;
    FTokens.free;
  end;
  inherited;
end;

procedure TLine.Draw(DC: HDC; Cx, Cy, StartChar: integer);
var
  i, Count: integer;
  Pen: HPen;
  // Brush : HBRUSH;
  Char: Pchar;
  ChWidth: integer;
  tk: TToken;
  // bid : TLogBrush;
begin
  Count := Length - StartChar;
  if Count <> 0 then
  begin
    Char := Pchar(FText) + StartChar;
    ChWidth := FEditor.CharWidth;
    // ������� ��� �������
    if FTokens.Count = 0 then
    begin
{$REGION '����� ��� ����� ���������'}
      for i := 0 to Count - 1 do
      begin
        SelectObject(DC, FEditor.FFonts[0]);
        SetBkMode(DC, TRANSPARENT);
        SetTextColor(DC, FEditor.TextColor);

        TextOut(DC, Cx, Cy, Char, 1);
        inc(Char);
        inc(Cx, ChWidth);
      end;
{$ENDREGION}
    end
    else
    begin
{$REGION '�����, �������� ������ ������'}
      for i := 0 to FTokens.Count - 1 do
      begin
        tk := TToken(FTokens[i]);
//        if StartChar > tk.FirstChar + tk.Length  then
//          continue;
        {TODO -oOnni -cDraw : ��������������}
        tk.ApplyStyle(DC);
        Char := @FText[tk.FirstChar + 1];
        TextOut(DC, Cx - FEditor.OffsetX*FEditor.CharWidth, Cy, Char, tk.Length);
        inc(Cx, tk.Length * FEditor.CharWidth);
      end;
      RestorStyle(DC);
{$ENDREGION}
    end;
  end;

  // ���� ���� �������
  if isRollUp then
  begin
    inc(Count, 3);
    if Count < 4 then
      Count := 4;
    Pen := SelectObject(DC, CreatePen(PS_SOLID, 1, GetSysColor(COLOR_BTNSHADOW))
      );
    MoveToEx(DC, FEditor.Gutter.Size, Cy + FEditor.CharHeight - 1, nil);
    LineTo(DC, FEditor.Gutter.Size + 1 + (FEditor.CharsInWidth + 1) * ChWidth,
      Cy + FEditor.CharHeight - 1);
  end;
end;

procedure TLine.RestorStyle(DC : HDC);
begin
  SetTextCharacterExtra(DC, 0);
  SetBkMode(DC, TRANSPARENT);
  SetTextColor(DC, 0);
end;

Function TLine.GetText: String;
begin
  Result := FText;
end;

function TLine.GetVisibleIndex: integer;
begin
  if FLineModificationChecker <> FEditor.FLineModificationChecker then
    UpdateLinesIndex;
  Result := FVisibleIndex;
end;

Procedure TLine.SetText(const value: String);
begin
  FText := value;
  UpdateSyn;
  { todo: ��������� �� ��������� }
end;

procedure TLine.SetVisible(const value: boolean);
begin
  FVisible := value;
  { todo: ��������� �� ��������� }
end;

function TLine._GetIndex: integer;
begin
  Result := LineIndex;
end;

function TLine._GetText: string;
begin
  Result := Text;
end;

function TLine._SetText(Text: string): integer;
begin
  Self.Text := Text;
end;

function TLine.GetLineIndex: integer;
begin
  if FLineModificationChecker <> FEditor.FLineModificationChecker then
    UpdateLinesIndex;
  Result := FLineIndex;
end;

function TLine.GetLength: integer;
begin
  Result := System.Length(FText);
end;

function TLine.GetisEndInLevel: boolean;
var
  Index: integer;
begin
  Index := LineIndex;
  if Index > FEditor.Count - 2 then
    exit(false);
  Result := (FEditor.Lines[index + 1].Level < Level);
end;

Function TLine.GetisRollBlock: boolean;
begin
  Result := FRolllUpFor <> nil;
end;

Procedure TLine.SetIsRollUp(const value: boolean);
var
  index: integer;
  i: integer;
  EndIndex: integer;
  LineO: TLine;
begin
  if FisRollUp = value then
    exit;
  if isRollBlock then
  begin
    FisRollUp := value;
    if isRollUp then
    begin
      { ����������� }
      Index := FEditor.FVisibleLines.IndexOf(Self) + 1;
      EndIndex := FEditor.FLines.IndexOf(FRolllUpFor);
      if not Assigned(FRollUpLines) then
        FRollUpLines := TList.Create;
      repeat
        LineO := TLine(FEditor.FVisibleLines[Index]);
        if (LineO.LineIndex <= EndIndex) then
        begin
          LineO.FRollUpIn := Self;
          FRollUpLines.Add(LineO);
          FEditor.FVisibleLines.Delete(Index);
        end;
      until (LineO = nil) or (LineO.LineIndex > EndIndex);
    end
    else
    begin
      { ������������� }
      Index := FEditor.FVisibleLines.IndexOf(Self) + 1;
      for i := FRollUpLines.Count - 1 downto 0 do
      begin
        LineO := TLine(FRollUpLines[i]);
        LineO.FRollUpIn := nil;
        FEditor.FVisibleLines.Insert(index, LineO);
      end;
      FRollUpLines.Clear;
      if Assigned(FRollUpLines) then
        FRollUpLines.free;
      FRollUpLines := nil;
    end;
    FEditor.Invalidate;
    FEditor.LineModification;
  end
  else
    FisRollUp := false;
end;

Procedure TLine.Insert(const AString: String; AChar: integer);
begin
  { ���������� ��������, ���� ������� �� ������� ��������������� ��������, ���� ������� �� ������� �������������� ��������, ���� ������� �� ������� ��������������� ��������, ���� ������� �� ������� ���������� }
  while (System.Length(FText) < AChar - 1) do
    FText := FText + ' ';

  System.Insert(AString, FText, AChar);
  Invalidate;
end;

procedure TLine.Delete(AChar, ACount: integer);
begin
  System.Delete(FText, AChar, ACount);
  Invalidate;
end;

Procedure TLine.BreakLine(AChar: integer);
var
  LineAfter: string;
begin
  LineAfter := Copy(FText, AChar, Length);
  System.Delete(FText, AChar, Length);
  FEditor.InsertString(LineAfter, LineIndex + 1);
  Invalidate;
  { Invalidate; }
  FEditor.Invalidate;
end;

Procedure TLine.Invalidate;
var
  Rt: TRect;
begin
  UpdateSyn;
  if FEditor.FUpdateLockCount <> 0 then
    exit;
  GetClientRect(FEditor.Handle, Rt);
  Rt.Left := FEditor.FGutter.Size;
  Rt.Top := (VisibleIndex - FEditor.OffsetY) * FEditor.CharHeight;
  Rt.Bottom := Rt.Top + FEditor.CharHeight;
  InvalidateRect(FEditor.Handle, @Rt, false);
end;

procedure TLine.UpdateLinesIndex;
begin
  { DONE: ���������� }
  { ��� ������� ����� ��������� - ������������� �� �� ��������� ������ �������
    ��������� ����� ����� �������� ������� ���������� ��������� ����� �����
    ���������, � ���� ��� �� ���������, �� ����� �������� ������ ��-�����,
    ����� ���� ������� ���� ������ ���������� ��������� ����� ����� }
  FLineIndex := FEditor.FLines.IndexOf(Self);
  FVisibleIndex := FEditor.FVisibleLines.IndexOf(Self);
  FLineModificationChecker := FEditor.FLineModificationChecker;
end;

procedure TLine.UpdateSyn;
var
  index: Integer;
  Tk: TToken;
  i: Integer;
begin
  for i := 0 to FTokens.Count - 1 do
    TToken(FTokens[i]).Free;
  FTokens.Clear;

  index := pos('//', Text);
  if index = 0 then exit;
  tk := TToken.Create(FEditor.FTokens['normal']);
  FTokens.Add(tk);
  tk.FFirstChar := 0;
  tk.Length := index - 1;

  tk := TToken.Create(FEditor.FTokens['comments']);
  FTokens.Add(tk);
  tk.FFirstChar := index-1;
  tk.Length := Length - index + 1;
end;

{ TCaret }

Constructor TCaret.Create(AEditor: TEceEditorWindow);
begin
  FEditor := AEditor;
  Update;
end;

procedure TCaret.SetX(const value: integer);
begin
  Fx := value;
  Update;
end;

procedure TCaret.SetY(const value: integer);
begin
  Fy := value;
  if Fy < 0 then
    Fy := 0;
  if Fy > FEditor.FVisibleLines.Count - 1 then
    Fy := FEditor.FVisibleLines.Count - 1;

  Update;
end;

procedure TCaret.SetStyle(Const value: TCaretStyle);
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
      CreateCaret(FEditor.Handle, 0, 2, FEditor.CharHeight);
    csClassic:
      CreateCaret(FEditor.Handle, 0, FEditor.CharWidth, 2);
  end;
  ShowCaret(FEditor.Handle);
end;

function TCaret.GetHaveSelection: Boolean;
begin
  Result :=
  {TODO -oOnni -cBug : �������� ������ ��� ��������� ������}
    (SelStartX <> X) or
    (SelStartY <> Y);
end;

function TCaret.GetLine: integer;
begin
  try
    Result := TLine(FEditor.FVisibleLines[Y]).LineIndex;
  except
    Result := -1;
  end;
end;

function TCaret.GetSelectionMode: Boolean;
begin
  Result :=
    isKeyDown(VK_SHIFT) or
    isKeyDown(VK_LBUTTON);
end;

function TCaret.GetSelectionRange: TSelectionRange;
begin
  Result.SelStart.X := Min(X, SelStartX);
  Result.SelStart.Y := Min(Y, SelStartY);
  Result.SelEnd.X := Max(X, SelStartX);
  Result.SelEnd.Y := Max(Y, SelStartY);
end;

procedure TCaret.Hide;
begin
  HideCaret(FEditor.Handle);
  DestroyCaret;
end;

procedure TCaret.Update;
var
  Cx, Cy: integer;
begin
  if Fx < 0 then
    Fx := 0;
  if Fy < 0 then
    Fy := 0;
  if Fy > FEditor.FVisibleLines.Count - 1 then
    Fy := FEditor.Count - 1;
  //���� ������� ������� �� ������� ���� - ������������
  with FEditor do
  begin
    if Fx < OffsetX then
      OffsetX := Fx;
    if Fy < OffsetY then
      OffsetY := Fy;

    if Fx > HScroll.Max then
      HScroll.Max := Fx;
    if Fx - CharsInWidth > OffsetX - 1 then
      OffsetX := Fx - CharsInWidth + 1;
    if Fy - CharsInHeight > OffsetY - 1 then
      OffsetY := Fy - CharsInHeight + 1;
  end;
  //�������� ��� ���
  if not SelectionMode then
  begin
    FSelStartX := Fx;
    FSelStartY := Fy;
  end;
  //��������� ��������� �������
  case Style of
    csNormal:
      begin
        Cx := FEditor.FGutter.Size + (Fx - FEditor.OffsetX) * FEditor.CharWidth;
        Cy := (Fy - FEditor.OffsetY) * FEditor.CharHeight;
      end;
    csClassic:
      begin
        Cx := FEditor.FGutter.Size + (Fx - FEditor.OffsetX) * FEditor.CharWidth;
        Cy := (Fy - FEditor.OffsetY + 1) * FEditor.CharHeight - 2;
      end;
  end;
  SetCaretPos(Cx, Cy);
end;

function TCaret._GetX: integer;
begin
  Result := X;
end;

function TCaret._GetY: integer;
begin
  Result := Y;
end;

function TCaret._SetX(value: integer): integer;
begin
  Result := X;
  X := value;
end;

function TCaret._SetY(value: integer): integer;
begin
  Result := Y;
  Y := value;
end;

{ TTokenClass }

constructor TTokenClass.Create(AEditor: TEceEditorWindow);
begin
  inherited Create;
  FEditor := AEditor;
end;

procedure TTokenClass.SetBkColor(const value: integer);
begin
  FBkColor := value;
end;

procedure TTokenClass.SetFontStyle(const value: integer);
begin
  FFontStyle := value;
end;

procedure TTokenClass.SetStrick(const value: boolean);
begin
  FStrick := value;
end;

procedure TTokenClass.SetTextColor(const value: integer);
begin
  FTextColor := value;
end;

procedure TTokenClass.SetUnderline(const value: boolean);
begin
  FUnderline := value;
end;

{ TTokenClassList }

constructor TTokenClassList.Create(AEditor : TEceEditorWindow);
begin
  inherited Create;
  FEditor := AEditor;
  FTokens := TStringList.Create;
end;

destructor TTokenClassList.Destroy;
var
  i: Integer;
begin
  if Assigned(FTokens) then
  begin
    for i := 0 to FTokens.Count - 1 do
      FTokens.Objects[i].Free;
    FTokens.free;
  end;
  inherited;
end;

function TTokenClassList.GetTokens(const ATokenKey: string): TTokenClass;
var
  Index: Integer;
begin
  index := FTokens.IndexOf(ATokenKey);
  if Index = -1 then
    raise Exception.Create(format('����� "%s" �� ������.', [ATokenKey]));
  Result := TTokenClass(FTokens.Objects[index]);
end;

function TTokenClassList.NewToken(AName: string; AType: TTokenClassType)
  : TTokenClass;
begin
  Result := TTokenClass.Create(FEditor);
  Result.Name := AName;
  Result.FTokenType := AType;

  FTokens.AddObject(AName, Result);
  FTokens.Sort;
  FTokens.Sorted := true;
end;

{ TToken }

procedure TToken.ApplyStyle(DC: HDC);
begin
  if TokenClass.BkColor <> -1 then
  begin
    SetBkMode(DC, OPAQUE);
    SetBkColor(DC, TokenClass.BkColor)
  end
  else
    SetBkMode(DC, TRANSPARENT);

  SelectObject(DC, TokenClass.Editor.FFonts[TokenClass.FontStyle]);
  SetTextCharacterExtra(DC, TokenClass.Editor.FFontExtraSpace
      [TokenClass.FontStyle]);
  SetTextColor(DC, TokenClass.TextColor);
end;

constructor TToken.Create(ATokenClass: TTokenClass);
begin
  inherited Create;
  FTokenClass := ATokenClass;
end;

procedure TToken.SetFirstChar(const value: Cardinal);
begin
  FFirstChar := value;
end;

procedure TToken.SetLength(const value: Cardinal);
begin
  FLength := value;
end;

end.