program VForthCmd;
{$APPTYPE CONSOLE}
{$IFDEF fpc}{$MODE delphi}{$ENDIF}
{$R *.dres}

uses
  Windows,
  SysUtils,
  VForth in 'VForth.pas',
  VForthMachine in 'VForthMachine.pas',
  VForthVariants in 'VForthVariants.pas',
  VForthVariantInteger in 'VForthVariantInteger.pas',
  VForthVariantFloat in 'VForthVariantFloat.pas',
  VForthVariantNatural in 'VForthVariantNatural.pas',
  VForthVariantComplex in 'VForthVariantComplex.pas',
  VForthVariantString in 'VForthVariantString.pas',
  VForthModuleSystem in 'VForthModuleSystem.pas',
  VForthAthom in 'VForthAthom.pas',
  VForthModuleIo in 'VForthModuleIo.pas',
  VForthModuleMath in 'VForthModuleMath.pas',
  VForthModuleLogic in 'VForthModuleLogic.pas',
  VForthModule in 'VForthModule.pas',
  VForthModuleDateTime in 'VForthModuleDateTime.pas',
  VForthVariantArray in 'VForthVariantArray.pas',
  VForthModuleWin32 in 'VForthModuleWin32.pas';
// VForthModuleOle in 'VForthModuleOle.pas';

type
  TIO = class(TInterfacedObject, IVForthIO)
    function StdIn: string; stdcall;
    procedure StdOut(str: string); stdcall;
    procedure StdErr(str: string); stdcall;
  end;

var
  VForth: IVForthMachine;
  cmd: string;

  { TIO }

procedure TIO.StdErr(str: string);
begin
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
    FOREGROUND_RED or FOREGROUND_INTENSITY);
  Write(str);
end;

function TIO.StdIn: string;
begin
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
    FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_RED or
      FOREGROUND_INTENSITY);
  write('> ');
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
    FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_RED);
  Readln(Result);
end;

procedure TIO.StdOut(str: string);
begin
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
    FOREGROUND_GREEN or FOREGROUND_INTENSITY);
  Write(str);
end;

begin
{$IFNDEF fpc}
{$IFDEF debug}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}
{$ENDIF}
  SetConsoleCP(1251);
  SetConsoleOutputCP(1251);
  try
    VForth := CreateVForthMachine;
    VForth.SetIo(TIO.Create);
    // Загрузка библиотек
    VForth.LoadModule(TVForthModuleSystem.Create);
    VForth.LoadModule(TVForthModuleIo.Create);
    VForth.LoadModule(TVForthModuleMath.Create);
    VForth.LoadModule(TVForthModuleLogic.Create);
    // VForth.LoadModule(TVForthModuleOle.Create);
    VForth.LoadModule(TVForthModuleDateTime.Create);
    VForth.LoadModule(TVForthModuleWin32.Create);
    //
    VForth.AddCode('help');
    repeat
      try
        VForth.StdOut(#13#10);
        VForth.AddCode(VForth.StdIn);
      except
        on E: Exception do
        begin
          VForth.StdErr(E.Message);
        end;
      end;
    until false;
  except
    on E: Exception do
    begin
      writeln(E.ClassName, ': ', E.Message);
      Readln;
    end;
  end;

end.
