unit eceConsoleWindow;

interface

{$I EceLanguage.inc}

uses
  Windows,
  SysUtils,
  Classes,
  Messages,
  EditorWindow,
  IEce,
{$IFDEF forth}
  VForth,
  VForthMachine,
  VForthVariants,
  VForthVariantInteger,
  VForthVariantFloat,
  VForthVariantNatural,
  VForthVariantComplex,
  VForthVariantString,
  VForthModuleSystem,
  VForthAthom,
  VForthModuleIo,
  VForthModuleMath,
  VForthModuleLogic,
  VForthModule,
  VForthModuleDateTime,
  VForthVariantArray,
  VForthModuleWin32;
{$ELSE}
MsAsKernel;
{$ENDIF}

type
  TEceConsoleCaret = class;

  TEceConsoleWindow = class(TEceEditorWindow {$IFDEF forth}, IVForthIO {$ENDIF})
  private
    FApplication: IEceApplication;
{$IFDEF forth}
    FVForthMachine: IVForthMachine;
{$ELSE}
    FIfopKernel: TKernel;
{$ENDIF}
    FScriptSource: string;
    FHistory: TStringList;
    FHistoryIndex: Integer;
  protected
{$IFDEF forth}
    function StdIn: string; stdcall;
    procedure StdOut(str: string); stdcall;
    procedure StdErr(str: string); stdcall;
{$ENDIF}
    function CreateCaret: TCaret; override;
    function CreateLine: TLine; override;
    procedure wmChar(var msg: TWmChar);
    message WM_CHAR;
    procedure wmKeyDown(var msg: TWMKeyDown);
    message WM_KEYDOWN;
  protected
    procedure LoadStdScript;
  public
    Constructor Create(Parent: Cardinal; AApplication: IEceApplication);
    Destructor Destroy; override;
{$IFDEF forth}
    property Machine: IVForthMachine read FVForthMachine;
{$ELSE}
    property Kernal: TKernel read FIfopKernel;
{$ENDIF}
  end;

  TEceConsoleCaret = class(TCaret)
  private
    procedure SetXY(const Ax, Ay: Integer); override;
    Procedure SetX(Const value: Integer); override;
    Procedure SetY(Const value: Integer); override;
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

procedure StdProc(con: TEceConsoleWindow; AText: string; AReturn: Boolean;
  t: TLineType);
  procedure OutputLine(l: string);
  var
    Line: TConsoleLine;
    copyC: Integer;
  begin
    Line := TConsoleLine(con.Lines[con.Count - 1]);
    if (Line.FLineType <> t) or
      (Line.Length + Length(l) > con.CharsInWidth - 1) then
    begin
      Line := TConsoleLine(con.AddLine);
      Line.LineType := t;
    end;
    Line.LineType := t;
    Line.Text := Line.Text + l + #32#32#32#32;
    Line.UpdateSyn;
  end;

var
  Ls: TStringList;
  i: Integer;

  Line: TConsoleLine;
begin
  try
    { TODO -oOnni -cGeneral : Разбивать на строки }
    Ls := TStringList.Create;
    Ls.Text := AText;
    for i := 0 to Ls.Count - 1 do
    begin
{$IFNDEF forth}
      Line := TConsoleLine(con.AddLine);
      Line.LineType := t;
{$ENDIF}
      OutputLine(Ls[i]);
    end;
  finally
    Ls.Free;
  end;
end;

procedure StdOutProc(con: TEceConsoleWindow; AText: string; AReturn: Boolean);
begin
  StdProc(con, AText, AReturn, ltOut);
end;

procedure StdErrProc(con: TEceConsoleWindow; AText: string; AReturn: Boolean);
begin
  StdProc(con, AText, AReturn, ltErr)
end;

constructor TEceConsoleWindow.Create(Parent: Cardinal;
  AApplication: IEceApplication);
{$IFDEF forth}
var
  AppModule: IVForthModule;
{$ENDIF}
begin
  inherited;
  FApplication := AApplication;
{$IFDEF forth}
  FVForthMachine := CreateVForthMachine;
  FVForthMachine.SetIo(Self);
  FVForthMachine.LoadModule(TVForthModuleSystem.Create);
  FVForthMachine.LoadModule(TVForthModuleIo.Create);
  FVForthMachine.LoadModule(TVForthModuleMath.Create);
  FVForthMachine.LoadModule(TVForthModuleLogic.Create);
  FVForthMachine.LoadModule(TVForthModuleDateTIme.Create);
  FVForthMachine.LoadModule(TVForthModuleWin32.Create);
  FVForthMachine.LoadModule(AApplication.GetModule);
{$ELSE}
  FIfopKernel := TKernel.Create;
{$ENDIF}
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
{$IFDEF forth}
  FVForthMachine := nil;
{$ELSE}
  FIfopKernel.Free;
{$ENDIF}
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
{$IFDEF forth}
      FScriptSource := ExtractFilePath(ParamStr(0)) + 'script\main.f';
{$ELSE}
      FScriptSource := ExtractFilePath(ParamStr(0)) + 'script\main.vbs';
{$ENDIF}
      l.LoadFromFile(FScriptSource);
{$IFDEF forth}
      { TODO -oOnni -cGeneral : После модификации перестал работать с могосрочным текстом? }
      FVForthMachine.AddCode(StringReplace(l.Text, #13#10, #32, [rfReplaceAll])
        );
{$ELSE}
      FIfopKernel.AddCode(l.Text);
{$ENDIF}
    finally
      l.Free;
    end;
  except
    on e: EXception do
      MessageBox(0, Pchar(FScriptSource + #13#10#13#10 + e.Message), nil,
        MB_ICONERROR);
  end;
end;
{$IFDEF forth}

procedure TEceConsoleWindow.StdErr(str: string);
begin
  StdErrProc(Self, str, false);
end;

function TEceConsoleWindow.StdIn: string;
begin

end;

procedure TEceConsoleWindow.StdOut(str: string);
begin
  StdOutProc(Self, str, false);
end;
{$ENDIF}

procedure TEceConsoleWindow.wmChar(var msg: TWmChar);
var
  str: String;
  index: Integer;
  i: Integer;
  doc: IEceDocument;
begin
  FHistoryIndex := -1;
  case msg.CharCode of
    VK_RETURN:
      begin
        try
          try
            BeginUpdate;
            for i := 0 to Application._GetDocumentsCount - 1 do
            begin
              Application._GetDocuments(i, doc);
              doc._BeginUpdate;
            end;

            str := Strings[Count - 1];
{$IFDEF forth}
            FVForthMachine.AddCode(str);
{$ELSE}
            FIfopKernel.AddCode(str);
{$ENDIF}
{$REGION 'История'}
            index := FHistory.IndexOf(str);
            if index <> -1 then
              FHistory.Delete(index);
            FHistory.Insert(0, str);
{$ENDREGION}
          finally
            for i := 0 to Application._GetDocumentsCount - 1 do
            begin
              Application._GetDocuments(i, doc);
              doc._EndUpdate;
            end;
            EndUpdate;
          end;
        except
          on e: EXception do
            // FIfopKernel.stderr(e.ClassName + ': ' + e.Message);
            StdErrProc(Self, e.Message, true);
        end;
        AddLine;
        Caret.Y := 0;
        Caret.X := 0;
      end;
    VK_ESCAPE:
      begin
        if (Strings[Count - 1] <> '') or (Caret.X <> 0) then
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
        with Lines[Count - 1] do
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
        if FHistoryIndex < -1 then
          FHistoryIndex := FHistory.Count - 1;
        with Lines[Count - 1] do
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

procedure TEceConsoleCaret.SetX(const value: Integer);
begin
  inherited;

end;

procedure TEceConsoleCaret.SetXY(const Ax, Ay: Integer);
begin
  inherited SetXY(Ax, Editor.Count - 1);
end;

procedure TEceConsoleCaret.SetY(const value: Integer);
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
  i: Integer;
  index: Integer;
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
