unit eceConsoleWindow;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Messages,
  EditorWindow,
  IEce,
  MsAsKernel;

type
  TEceConsoleCaret = class;

  TEceConsoleWindow = class(TEceEditorWindow)
  private
    FIfopKernel: TKernel;
    FScriptSource: string;
    FHistory : TStringList;
    FHistoryIndex : Integer;
  protected
    function CreateCaret: TCaret; override;
    function CreateLine: TLine; override;
    procedure wmChar(var msg: TWmChar);
    message WM_CHAR;
    procedure wmKeyDown(var msg : TWMKeyDown);
    message WM_KEYDOWN;
  protected
    procedure LoadStdScript;
  public
    Constructor Create(Parent: Cardinal; AApplication: IEceApplication);
    Destructor Destroy; override;

    property Kernal: TKernel read FIfopKernel;
  end;

  TEceConsoleCaret = class(TCaret)
  private
    procedure SetXY(const Ax, Ay: integer); override;
    Procedure SetX(Const value: integer); override;
    Procedure SetY(Const value: integer); override;
  end;

  TLineType = (ltIn, ltOut, ltErr);

  TConsoleLine = class(TLine)
  private
    FLineType: TLineType;
    procedure SetLineType(const value: TLineType);

  protected
    procedure UpdateSyn; override;
  public
    property LineType: TLineType read FLineType write SetLineType;
  end;

  // procedure StdInProc(con: TEceConsoleWindow; AText: string; AReturn: Boolean);
procedure StdOutProc(con: TEceConsoleWindow; AText: string; AReturn: Boolean);
procedure StdErrProc(con: TEceConsoleWindow; AText: string; AReturn: Boolean);

implementation

{ TEceConsoleWindow }

procedure StdOutProc(con: TEceConsoleWindow; AText: string; AReturn: Boolean);
var
  l: TConsoleLine;
  lns: TStringList;
  i: integer;
begin
  lns := TStringList.Create;
  lns.Text := AText;

  for i := 0 to lns.Count - 1 do
  begin
    AText := lns[i];
    AText := StringReplace(AText, #9, #32#32#32#32, [rfReplaceAll]);
    l := TConsoleLine(con.Lines[con.Count - 1]);
    if l.LineType = ltErr then
    begin
      if (not AReturn) and (l.Length + Length(AText) <= l.Editor.CharsInWidth)
        then
      begin
        l.Text := l.Text + AText;
        exit;
      end;
    end;

    with TConsoleLine(con.AddLine) do
    begin
      LineType := ltOut;
      Text := AText;
      Invalidate;
    end;
  end;
  lns.Free;
end;

procedure StdErrProc(con: TEceConsoleWindow; AText: string; AReturn: Boolean);
var
  l: TConsoleLine;
  lns: TStringList;
  i: integer;
begin
  lns := TStringList.Create;
  lns.Text := AText;

  for i := 0 to lns.Count - 1 do
  begin
    AText := lns[i];
    AText := StringReplace(AText, #9, #32#32#32#32, [rfReplaceAll]);
    l := TConsoleLine(con.Lines[con.Count - 1]);
    if l.LineType = ltErr then
    begin
      if (not AReturn) and (l.Length + Length(AText) <= l.Editor.CharsInWidth)
        then
      begin
        l.Text := l.Text + AText;
        exit;
      end;
    end;

    with TConsoleLine(con.AddLine) do
    begin
      LineType := ltErr;
      Text := AText;
      Invalidate;
    end;
  end;
  lns.Free;
end;

constructor TEceConsoleWindow.Create(Parent: Cardinal;
  AApplication: IEceApplication);
begin
  inherited;
  FIfopKernel := TKernel.Create;
  FHistory := TStringList.Create;
  LoadStdScript;
  // FIfopKernel.SetStdOut(Self, @StdOutProc);
  // FIfopKernel.SetStdErr(Self, @StdErrProc);
end;

function TEceConsoleWindow.CreateCaret: TCaret;
begin
  Result := TEceConsoleCaret.Create(Self);
end;

function TEceConsoleWindow.CreateLine: TLine;
begin
  Result := TConsoleLine.Create(Self);
end;

destructor TEceConsoleWindow.Destroy;
begin
  FIfopKernel.Free;
  FHistory.Free;
  inherited;
end;

procedure TEceConsoleWindow.LoadStdScript;
var
  l: TStringList;
begin
  try
    l := TStringList.Create;
    try
      FScriptSource := ExtractFilePath(ParamStr(0)) + 'script\main.vbs';
      l.LoadFromFile(FScriptSource);
      FIfopKernel.AddCode(l.Text);
    finally
      l.Free;
    end;
  except
    on e:EXception do
      MessageBox(0, Pchar(FScriptSource + #13#10#13#10 + e.Message), nil, MB_ICONERROR);
  end;
end;

procedure TEceConsoleWindow.wmChar(var msg: TWmChar);
var
  str: String;
  index: Integer;
begin
  FHistoryIndex := -1;
  case msg.CharCode of
    VK_RETURN:
      begin
        try
          try
            BeginUpdate;
            str := Strings[Count - 1];
            FIfopKernel.AddCode(str);
{$REGION 'История'}
              index := FHistory.IndexOf(str);
              if index <> -1 then
                FHistory.Delete(index);
              FHistory.Insert(0, str);
{$ENDREGION}
          finally
            EndUpdate;
          end;
        except
          on e: Exception do
            // FIfopKernel.stderr(e.ClassName + ': ' + e.Message);
            StdErrProc(Self, e.Message, true);
        end;
        AddLine;
        Caret.Y := 0;
        Caret.X := 0;
      end;
    VK_ESCAPE:
      begin
        if (Strings[Count - 1] <> '') and (Caret.X <> 0) then
        begin
          // Очищаем строку
          Strings[Count - 1] := '';
          Lines[Count - 1].Invalidate;
          Caret.X := 0;
        end
        else
        begin
          // Возвращаемся в режим ввода
          Application._FocusToActiveDocument;
        end;
      end;
    VK_BACK:
      if Caret.X > 0 then
        inherited;
  else
    begin
      inherited;
    end;
  end;
end;

procedure TEceConsoleWindow.wmKeyDown(var msg: TWMKeyDown);
begin
  case msg.CharCode of
    VK_UP:
      begin
        Inc(FHistoryIndex);
        if FHistoryIndex >= FHistory.Count then
          FHistoryIndex := -1;
        with Lines[Count-1] do
        begin
          if FHistoryIndex <> -1 then
            Text := FHistory[FHistoryIndex]
            else
            Text := '';
          Invalidate;
          Caret.X := Length;
        end;
      end;
    VK_DOWN:
      begin
        Dec(FHistoryIndex);
        if FHistoryIndex < -1  then
          FHistoryIndex := FHistory.Count-1;
        with Lines[Count-1] do
        begin
          if FHistoryIndex <> -1 then
            Text := FHistory[FHistoryIndex]
            else
            Text := '';
          Invalidate;
          Caret.X := Length;
        end;
      end;
    else
      begin
        FHistoryIndex := -1;
        inherited;
      end;
  end;
end;

{ TEceConsoleCaret }

procedure TEceConsoleCaret.SetX(const value: integer);
begin
  inherited;

end;

procedure TEceConsoleCaret.SetXY(const Ax, Ay: integer);
begin
  inherited SetXY(AX, Editor.Count-1);
end;

procedure TEceConsoleCaret.SetY(const value: integer);
begin
  // Всегда выделяем последнюю строчку
  inherited SetY(Editor.Count - 1);
end;

{ TConsoleLine }

procedure TConsoleLine.SetLineType(const value: TLineType);
begin
  FLineType := value;
end;

procedure TConsoleLine.UpdateSyn;
var
  i: integer;
  index: integer;
  tk: TToken;
begin
  for i := 0 to FTokens.Count - 1 do
    TToken(FTokens[i]).Free;
  FTokens.Clear;

  case LineType of
    ltIn:
      tk := TToken.Create(Editor.Tokens['stdin']);
    ltOut:
      tk := TToken.Create(Editor.Tokens['stdout']);
    ltErr:
      tk := TToken.Create(Editor.Tokens['stderr']);
  end;
  begin
    FTokens.Add(tk);
    tk.FirstChar := 0;
    tk.Length := Length;
  end;
end;

end.
