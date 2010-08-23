// ************************************************************
//
// Easy Code editor
// Copyright (c) 2010  zeDevel
//
// Разработчик: Благодарев Евгений  ze0nni@gmail.com
//
// ************************************************************

Program Ece;
{$APPTYPE GUI}
{$IFDEF fpc}{$MODE delphi}{$ENDIF}
// {$R _source\ece.res}
// {$R _source\Dialogs.res}
{$R *.dres}

uses
  Windows,
  Messages,
  SysUtils,
  MsAsKernel in 'MsAsKernal\src\MsAsKernel.pas',
  zeWndControls in 'zeWndControls.pas',
  AppWindow in 'AppWindow.pas',
  EditorWindow in 'EditorWindow.pas',
  eceCmdMenu in 'eceCmdMenu.pas',
  iece in 'iece.pas',
  Classes,
  ConExec in 'ConExec.pas',
  eceFindDialog in 'eceFindDialog.pas',
  eceSynParser in 'eceSynParser.pas',
  eceConsoleWindow in 'eceConsoleWindow.pas';

var
  App: TEceAppWindow;
  msg: tmsg;

begin
{$IFNDEF FPC}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}
  try
    App := TEceAppWindow.Create(0);
    // todo: Кривая иконка
    SendMessage(App.Handle, WM_SETICON, ICON_SMALL, LoadIcon
        (HInstance, 'appicon'));

    App.NewDocument('');
    App.ActiveDocument := 0;

    App.Documents[0].SetFocus;
    // TEceEditorWindow(App.Documents[0]).Caret.Style := csClassic;
    // TEceEditorWindow(App.Documents[0]).LoadPlugin
    // ('EditorModules\autospace.dll');
    with TEceEditorWindow(App.Documents[0]) do
    begin
      // SetFont('Lucida console', 17);
      SetFont('Consolas', 22);
      LoadColorTheme('color\default.txt');
      Caret.Style := csClassic;
    end;

    if ParamCount <> 0 then
      App.Documents[0].LoadFromFile(ParamStr(1));

    // App.LoadPlugin('modules\autospace.dll');

    { TODO -oOnni -cПочти готово : Диалог поиска }
    //ShowFindDialog(app, TEceEditorWindow(App.Documents[0]));

    while GetMessage(msg, 0, 0, 0) do
    begin
      if (msg.message = WM_KEYDOWN) and (msg.wParam = VK_RETURN) and
        (GetKeyState(VK_CONTROL) and 128 <> 0) then
      begin
        App.Console.SetFocus;
      end
      else
      begin
        TranslateMessage(msg);
        DispatchMessage(msg);
      end;
    end;
  except
    on E: Exception do
      MessageBox(App.Handle, Pchar(E.ClassName + ': ' + E.Message), nil,
        MB_ICONERROR);
  end;

end.
