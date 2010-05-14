unit eceCmdMenu;
{$ifdef fpc}{$mode delphi}{$endif}
{$R _source\ece.res}
interface

uses
  Windows,
  Iece,
  Messages,
  zeWndControls,
  Classes;

function ShowEceCmdMenu(App : IEceApplication; Editor : IEceDocument) : integer;

implementation

var
  Dlg : HWND;

function CmdProc(wnd : HWND; msg : integer; wParam : wParam; lParam : lParam) : bool; stdcall;
begin
    case msg of
        WM_INITDIALOG:
            begin
                SetWindowLong(wnd, GWL_STYLE, WS_VISIBLE or WS_POPUP or WS_BORDER or WS_CHILD);
                SetWindowLong(wnd, GWL_EXSTYLE, 0);
                SetWIndowPos(wnd, 0, 0, 0, 320, 25, SWP_NOMOVE);
                InvalidateRect(wnd, nil, true);
            end;
        WM_DESTROY :
            begin
              Dlg := 0;
            end;
        WM_KILLFOCUS : EndDialog(wnd, 1);
        WM_KEYDOWN:
            begin
                case wparam of
                    VK_ESCAPE : EndDialog(wnd, 1);
                    VK_RETURN : EndDialog(wnd, 0);
                end;
            end;
        else
            result := false;
    end;
end;

function ShowEceCmdMenu(App : IEceApplication; Editor : IEceDocument) : integer;
begin
    if IsWindowVisible(dlg) then
    begin
      exit(1);
    end;

    Dlg := CreateDialogParam(hInstance, 'CUSTOMDLG', App._GetHandle, @CmdProc, 0);
    ShowWindow(Dlg, SW_SHOWNORMAL);
    SetActiveWindow(Dlg);
    SetFocus(Dlg);
    Result := 0;
end;

initialization

end.

