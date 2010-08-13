program IfopCmd;
{$APPTYPE CONSOLE}

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
  ifopRegExp in 'Ifop\std\ifopRegExp.pas';

var
  Kernel: TIfopKernel;
  Cmd: string;
begin
  SetConsoleCP(1251);
  SetConsoleOutputCP(1251);
  ReportMemoryLeaksOnShutdown := true;
  try
    SetConsoleTitle('IForth processor');
    Kernel := TIfopKernel.Create;
    repeat
      try
        Cmd := '> ';
        writeln;
        Kernel.stdin(Cmd);
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
