program IfopCmd;
{$APPTYPE CONSOLE}

{$R *.dres}

uses
  Windows,
  Classes,
  SysUtils,
  IfopKernel in 'Ifop\IfopKernel.pas',
  IfopVariant in 'Ifop\IfopVariant.pas',
  ifopMatch in 'Ifop\std\ifopMatch.pas',
  ifopInformation in 'Ifop\std\ifopInformation.pas',
  ifopIo in 'Ifop\std\ifopIo.pas',
  ifopStack in 'Ifop\std\ifopStack.pas',
  ifopSystem in 'Ifop\std\ifopSystem.pas',
  ifopOle in 'Ifop\std\ifopOle.pas',
  ifopRegExp in 'Ifop\std\ifopRegExp.pas',
  ifopLogic in 'Ifop\std\ifopLogic.pas';

var
  Kernel: TIfopKernel;
  Cmd: string;

procedure LoadStl;
var
  stl, lib : TStringList;
  i: Integer;
  Dir : string;
begin
  try
    stl := TStringList.Create;
    lib := TStringList.Create;
    Dir := ExtractFilePath(ParamStr(0));
    stl.LoadFromFile(dir + 'stl\order.stl');
    for i := 0 to stl.Count - 1 do
    begin
      lib.LoadFromFile(dir + stl[i]);
      Kernel.AddCode(lib.Text);
    end;
  finally
    stl.Free;
    lib.Free;
  end;
end;

function ConProc(CtrlType: DWord): Bool; stdcall; far;
begin
  case CtrlType of
    CTRL_C_EVENT:
      begin
        Kernel.Abort;
      end
      else
      Exit(false)
  end;
  Result := True;
end;

begin
  SetConsoleCP(1251);
  SetConsoleOutputCP(1251);
  ReportMemoryLeaksOnShutdown := true;
  SetConsoleCtrlHandler(@ConProc, true);
  try
    SetConsoleTitle('IForth processor');
    Kernel := TIfopKernel.Create;
    LoadStl;
    repeat
      try
        writeln;
        Cmd := '> ';
        Kernel.stdin(Cmd); {TODO -oOnni -cGeneral : Если в это момент нажать ctrl+c, будет глюк}
        Kernel.AddCode(Cmd);
      except
        on E: Exception do
          Kernel.stderr(E.ClassName + ': ' + E.Message);
      end;
    until Kernel.isScriptEnd;
  finally
    Kernel.Free;
  end;
end.
