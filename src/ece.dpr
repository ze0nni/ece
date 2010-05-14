Program Ece;

{$APPTYPE GUI}
{$ifdef fpc}{$mode delphi}{$endif}

//{$R _source\ece.res}
//{$R _source\Dialogs.res}

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
  eceFindDialog in 'eceFindDialog.pas';

var
	App : TEceAppWindow;
	msg : tmsg;
begin

	try
    App := TEceAppWindow.Create(0);
    App.LoadPlugin('modules\autospace.dll');

    App.NewDocument('');
    App.ActiveDocument := 0;

    App.Documents[0].SetFocus;
    if ParamCount <> 0 then
      App.Documents[0].LoadFromFile(ParamStr(1));
    // TEceEditorWindow(App.Documents[0]).Caret.Style := csClassic;
    TEceEditorWindow(App.Documents[0]).LoadPlugin
      ('EditorModules\autospace.dll');

    {DONE -o0nni -cПочти готово : Диалог поиска}
    //ShowFindDialog(app, TEceEditorWindow(App.Documents[0]));

    while GetMessage(msg, 0, 0, 0) do
    begin
//       if (msg.message = WM_KEYDOWN) then
//       case msg.Wparam of
//       VK_ESCAPE :
//         begin
//           if ShowEceCmdMenu(App, App.Documents[0]) <> 0 then
//           begin
//             TranslateMessage(msg);
//             DispatchMessage(msg);
//           end;
//         end;
//       VK_CONTROL:
//          begin
//            ShowFindDialog(app, TEceEditorWindow(App.Documents[0]));
//          end;
//       end
//       else
       begin
         TranslateMessage(msg);
        DispatchMessage(msg);
      end;
    end;
	except
		halt;
	end;
end.
