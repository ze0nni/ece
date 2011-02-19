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
  zeWndControls in 'zeWndControls.pas',
  AppWindow in 'AppWindow.pas',
  EditorWindow in 'EditorWindow.pas',
  eceCmdMenu in 'eceCmdMenu.pas',
  iece in 'iece.pas',
  Classes,
  ConExec in 'ConExec.pas',
  eceFindDialog in 'eceFindDialog.pas',
  eceSynParser in 'eceSynParser.pas',
  eceConsoleWindow in 'eceConsoleWindow.pas',
  IeceObj in 'IeceObj.pas',
  EceAppUI in 'EceAppUI.pas',
  eceConfig in 'eceConfig.pas',
  eceFiniteAutomatons in 'eceFiniteAutomatons.pas';

// {$IFNDEF FPC},
// RegisterContextMenu in 'RegisterContextMenu.pas'{$ENDIF};

var
  App: TEceAppWindow;
  m: HMENU;
  msg: tmsg;

begin
{$IFNDEF FPC}
  // ReportMemoryLeaksOnShutdown := true;
{$ENDIF}
  // RegisterShellMenu;
  App := TEceAppWindow.Create(0);
  try
    // todo: Кривая иконка
    SendMessage(App.Handle, WM_SETICON, ICON_SMALL, LoadIcon
        (HInstance, 'appicon'));
    SendMessage(App.Handle, WM_SETICON, ICON_SMALL2, LoadIcon
        (HInstance, 'appicon'));
    SendMessage(App.Handle, WM_SETICON, ICON_BIG, LoadIcon(HInstance, 'appicon')
      );

    // Грузим модули
    // App.LoadPlugin('modules\startpage.dll');  //Стартовая страница
    App.RegisterDocument(TEceEditorLoader.Create); // Окно редактора
{$IFNDEF FPC}
    // App.LoadPlugin(ExtractFilePath(ParamStr(0)) + 'modules\hexview.dll');
    // // Модуль просмотра HEX
    // App.LoadPlugin(ExtractFilePath(ParamStr(0)) + 'modules\pdfview.dll');
    // // Модуль просмотра PDF
{$ENDIF}
    App.NewDocument(ParamStr(1));
    App.ActiveDocument := 0;

    App.Documents[0]._SetFocus;

    // if ParamCount <> 0 then
    // App.Documents[0]._LoadFromFile(ParamStr(1));
    App.ActiveDocument := 0;

    { TODO -oOnni -cПочти готово : Диалог поиска }
    // ShowFindDialog(app, TEceEditorWindow(App.Documents[0]));

    m := GetMenu(App.Handle);
    SetMenu(App.Handle, 0);
    SetMenu(App.Handle, m);

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
