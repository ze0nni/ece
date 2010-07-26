Program Ece;
{$APPTYPE GUI}
{$IFDEF fpc}{$MODE delphi}{$ENDIF}
// {$R _source\ece.res}
// {$R _source\Dialogs.res}
{$R _source\ece.res}
{$R _source\Dialogs.res}

uses
  Windows,
  Messages,
  SysUtils,
  zeWndControls,
  AppWindow,
  EditorWindow,
  eceCmdMenu,
  iece,
  Classes,
  ConExec,
  ZeError,
  eceFindDialog in 'eceFindDialog.pas';

var
  App: TEceAppWindow;
  msg: tmsg;

begin
{$IFNDEF FPC}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}
  try
    App := TEceAppWindow.Create(0);
    ErrMainWidnow := App.Handle;

    App.NewDocument('');
    App.ActiveDocument := 0;

    App.Documents[0].SetFocus;
    // TEceEditorWindow(App.Documents[0]).Caret.Style := csClassic;
    // TEceEditorWindow(App.Documents[0]).LoadPlugin
    // ('EditorModules\autospace.dll');
    with TEceEditorWindow(App.Documents[0]) do
    begin
      SetFont('Lucida console', 17);
      Caret.Style := csClassic;
      BackgroundColor := $404040;
      TextColor := $A0C0C0;
      with Tokens.NewToken('normal', ttWord) do
      begin
        TextColor := $A0C0C0;
        BkColor := $404040;
      end;
      with Tokens.NewToken('comments', ttEndRegion) do
      begin
        TextColor := $e0c080;
        BkColor := $404040;
        FontStyle := 2;
      end;
    end;

    if ParamCount <> 0 then
      App.Documents[0].LoadFromFile(ParamStr(1));

    // App.LoadPlugin('modules\autospace.dll');

    { TODO -oOnni -cПочти готово : Диалог поиска }
    // ShowFindDialog(app, TEceEditorWindow(App.Documents[0]));

    while GetMessage(msg, 0, 0, 0) do
    begin

      // if (msg.message = WM_KEYDOWN) then
      // case msg.Wparam of
      // VK_ESCAPE :
      // begin
      // if ShowEceCmdMenu(App, App.Documents[0]) <> 0 then
      // begin
      // TranslateMessage(msg);
      // DispatchMessage(msg);
      // end;
      // end;
      // VK_CONTROL:
      // begin
      // ShowFindDialog(app, TEceEditorWindow(App.Documents[0]));
      // end;
      // end
      // else
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
