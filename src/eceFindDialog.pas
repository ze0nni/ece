unit eceFindDialog;

interface

{$IFNDEF FPC}
  {$DEFINE UseCo}
{$ENDIF}

uses
  Windows,
  Messages,
  {$IFDEF UseCo}
  ComObj,
  ActiveX,
  {$ENDIF}
  IEce;

procedure ShowFindDialog(AApp : IEceApplication; AEdit : IEceEditor);

implementation

var
  App : IEceApplication;
  Editor : IEceEditor;
  {$IFDEF UseCo}
  RegExp : OleVariant;
  {$ENDIF}

function GetWndText(wnd : HWND) : string;
var
  buff : PChar;
  len : Integer;
begin
  len := GetWindowTextLength(wnd);
  if len = 0 then exit;

  buff := GetMemory((len + 2) * SizeOf(char));
  GetWindowText(wnd, buff, len + 1);
  buff[len] := #0;
  Result := buff;
  FreeMemory(buff);
end;

function FindDlgProc(wnd : HWND; msg : Integer; WParam : WPARAM; LParam : LPARAM) : BOOL; stdcall;
var
  i : Integer;
  line : string;
begin
  case msg of
    WM_INITDIALOG:
      begin

      end;
    WM_COMMAND:
      begin
        case WParam of
          103: {NEXT}
            begin
              {$IFDEF UseCo}
                try
                {TODO: сделать нормальный поиск с регекспами}
                  RegExp.Pattern := GetWndText(GetDlgItem(wnd, 101));
                  for i := Editor._GetCaret._GetY() + 1 to Editor._GetLinesCount - 1 do
                  begin
                    if RegExp.Test(Editor._GetLines(i)._GetText) then
                    begin
                      Editor._GetCaret._SetY(i);
                      exit;
                    end;
                    //тада
                    MessageBeep(MB_OK);
                  end;
                except
                  MessageBeep(MB_ICONASTERISK);
                end;
              {$ENDIF}
            end;
          104: {PREV}
            begin

            end;
          102: {check}
            begin

            end;
        end;
      end
    else
      Result := false;
  end;
end;

procedure ShowFindDialog(AApp : IEceApplication; AEdit : IEceEditor);
begin
  App := AApp;
  Editor := AEdit;
  ShowWindow(CreateDialog(hInstance, 'DLGFIND', App._GetHandle, @FindDlgProc), SW_SHOW);
end;

initialization
  {$IFDEF UseCo}
  CoInitializeEx(nil, 0);
  RegExp := CreateOleObject('VBScript.RegExp');
  {$ENDIF}
finalization
  {$IFDEF UseCo}
  RegExp := 0;
  CoUninitialize;
  {$ENDIF}
end.
