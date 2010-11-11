program VForthCmd;
{$IFDEF fpc}{$MODE delphi}{$ENDIF}
{$R *.dres}

uses
  Windows,
  SysUtils,
  Classes,
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
  if GetStdHandle(STD_ERROR_HANDLE) = 0 then AllocConsole;
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
    FOREGROUND_RED or FOREGROUND_INTENSITY);
  Write(str);
end;

function TIO.StdIn: string;
begin
  if GetStdHandle(STD_INPUT_HANDLE) = 0 then AllocConsole;
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
  if GetStdHandle(STD_OUTPUT_HANDLE) = 0 then AllocConsole;
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
    FOREGROUND_GREEN or FOREGROUND_INTENSITY);
  Write(str);
end;

function getvar(varname: string): string;
var
  buffer: array [0 .. 1024] of char;
  size: integer;
begin
  size := GetEnvironmentVariable(PChar(varname), buffer, sizeof(buffer));
  if size = 0 then
    getvar := ''
  else
    getvar := String(buffer);
end;

procedure ExecParams;
var
  i: Integer;
  ln: String;
  sl: TStringList;
  FileName: String;
begin
  i := 1;
  if ParamCount <> 0 then
  begin
{$REGION 'Параметры командной строки'}
    repeat
      ln := ParamStr(i);
      if FileExists(ln) then
      begin
        sl := TStringList.Create;
        try
          sl.LoadFromFile(ln);
          VForth.AddCode(sl.Text);
        finally
          sl.Free
        end;
      end
        else
        raise Exception.CreateFmt('Unknown cmd argiment "%s".', [ln]);

      inc(i);
    until i > ParamCount;
{$ENDREGION}
  end
    else
  begin
{$REGION 'Работаем как CGI'}
      sl := TStringList.Create;
      try
        FileName := getvar('PATH_TRANSLATED');
        sl.LoadFromFile(FileName);
        VForth.AddCode(sl.Text);
      finally
        sl.Free
      end;
{$ENDREGION}
  end;
end;

begin
{$IFNDEF fpc}
{$IFDEF debug}
  //ReportMemoryLeaksOnShutdown := true;
{$ENDIF}
{$ENDIF}
//  SetConsoleCP(1251);
//  SetConsoleOutputCP(1251);
  try
    // VForth := CreateVForthMachine;
    VForth := TVForthMachine.Create;
    VForth._AddRef;
    VForth.SetIo(TIO.Create);
    // Загрузка библиотек
    VForth.LoadModule(TVForthModuleSystem.Create);
    VForth.LoadModule(TVForthModuleIo.Create);
    VForth.LoadModule(TVForthModuleMath.Create);
    VForth.LoadModule(TVForthModuleLogic.Create);
    // VForth.LoadModule(TVForthModuleOle.Create);
    VForth.LoadModule(TVForthModuleDateTime.Create);
    VForth.LoadModule(TVForthModuleWin32.Create);
    if (ParamCount = 0)and(getvar('SCRIPT_NAME')='') then
    begin
{$REGION 'Работаем в командном режиме'}
        VForth.AddCode('Help');
        repeat
          try
            VForth.StdOut(#13#10);
            VForth.AddCode(VForth.StdIn);
          except
            on E: Exception do
            begin
              VForth.StdErr(E.Message);
            end;
          end
        until false;
{$ENDREGION}
    end
    else
    begin
{$REGION 'Работаем с файлом'}
      ExecParams;
{$ENDREGION}
    end;
  except
    on E: Exception do
    begin
      AllocConsole;
      writeln(E.ClassName, ': ', E.Message);
      //Readln;
    end;
  end;

end.
