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
  protected
    function CreateCaret: TCaret; override;
    function CreateLine: TLine; override;
    procedure wmChar(var msg: TWmChar);
    message WM_CHAR;
  protected
    procedure LoadStdScript;
  public
    Constructor Create(Parent: Cardinal; AApplication: IEceApplication);
    Destructor Destroy; override;
  end;

  TEceConsoleCaret = class(TCaret)
  private
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

implementation

{ TEceConsoleWindow }

procedure StdOutProc(con: TEceConsoleWindow; AText: string; AReturn: Boolean);
var
  l: TConsoleLine;
begin
  AText := StringReplace(AText, #9, #32#32#32#32, [rfReplaceAll]);
  l := TConsoleLine(con.Lines[con.Count - 1]);
  if l.LineType = ltOut then
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

procedure StdErrProc(con: TEceConsoleWindow; AText: string; AReturn: Boolean);
var
  l: TConsoleLine;
begin
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

constructor TEceConsoleWindow.Create(Parent: Cardinal;
  AApplication: IEceApplication);
begin
  inherited;
  FIfopKernel := TKernel.Create;
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
  inherited;
end;

procedure TEceConsoleWindow.LoadStdScript;
var
  l: TStringList;
begin
  l := TStringList.Create;
  try
    l.LoadFromFile(ExtractFilePath(ParamStr(0)) + '\script\main.vbs');
    FIfopKernel.AddCode(l.Text);
  finally
    l.Free;
  end;
end;

procedure TEceConsoleWindow.wmChar(var msg: TWmChar);
begin
  case msg.CharCode of
    VK_RETURN:
      begin
        try
          try
            BeginUpdate;
            FIfopKernel.AddCode(Strings[Count - 1]);
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

{ TEceConsoleCaret }

procedure TEceConsoleCaret.SetX(const value: integer);
begin
  inherited;

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
