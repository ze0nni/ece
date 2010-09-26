program AsDemo;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  HelloWorldServer in 'HelloWorldServer.pas',
  MsAsKernel in '..\src\MsAsKernel.pas';

var
  Kernel : TKernel;

begin
  try
    Kernel := TKernel.Create;
    Kernel.AddObject('App', THelloWorld.Create);
    Kernel.AddCode('App.Hello()');
    Kernel.AddCode('App.Print "Input string"');
    Kernel.AddCode('MsgBox App.GetStr');
  except
    on E: Exception do
      begin
        Writeln(E.ClassName, ': ', E.Message);
        readln;
      end;
  end;
end.
