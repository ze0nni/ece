unit AppWindow;
{$IFDEF fpc}{$MODE delphi}{$ENDIF}

interface

uses
  Windows,
  Messages,
  Classes,
  SysUtils,
  ActiveX,
  // zeError,
  zeWndControls,
  zePages,
  iece,
  DocumentWindow,
  EditorWindow,
  eceConsoleWindow;

type
  PGetPlugin = function: IEcePlugin; safecall;

  TEceAppWindow = class(TzeWndControl, IEceApplication, IDispatch)
  protected
    FConsole: TEceConsoleWindow;
    FDocuments: TList;
    FPages: TPages;
    FActiveDocument: Integer;
    procedure CreateParams(var Param: CreateStruct); override;
    procedure wmSize(var msg: TWMSize);
    message WM_SIZE;
    procedure wmSetFocus(var msg: TWmSetFocus);
    message WM_SETFOCUS;

    procedure wmDestroy(var msg: TWMDestroy);
    message WM_DESTROY;

    function GetDocumentsCount: Integer;
    function GetDocuments(const index: Integer): TEceDocumentWindow;
    procedure SetActiveDocument(const value: Integer);
    function GetActiveDocumentWindow: TEceDocumentWindow;
  protected
    function _GetHandle: HWND; safecall;
    function _GetDocumentsCount: Integer; safecall;
    function _GetDocuments(AIndex: Integer; var ADocument: IEceDocument)
      : Integer; safecall;
    procedure _UpdateCaption; safecall;
  protected
    function InvokeName(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; Params: TPropArr; VarResult, ExcepInfo, ArgErr: Pointer)
      : HResult; override;
  public
    procedure UpdateCaption;
    Constructor Create(AParent: Cardinal);
    Destructor Destroy; override;

    Procedure NewDocument(AFileName: String);
    function CloseDocument(const index: Integer): boolean;
    function CloseAllDocuments: boolean;

    function LoadPlugin(AFileName: string): boolean;

    property DocumentsCount: Integer read GetDocumentsCount;
    property Documents[const index: Integer]
      : TEceDocumentWindow read GetDocuments;
    property ActiveDocument
      : Integer read FActiveDocument write SetActiveDocument;
    property ActiveDocumentWindow
      : TEceDocumentWindow read GetActiveDocumentWindow;
    property Console: TEceConsoleWindow read FConsole;
  end;

implementation

const
  PROP_TITLE = 0;
  PROP_LEFT = 1;
  PROP_TOP = 2;
  PROP_WIDTH = 3;
  PROP_HEIGHT = 4;

  PROP_STDIN = 5;
  PROP_STDOUT = 6;
  PROP_STDERR = 7;

function TEceAppWindow._GetDocuments(AIndex: Integer;
  var ADocument: IEceDocument): Integer;
begin
  try
    ADocument := Documents[AIndex];
    Result := S_OK;
  except
    Result := S_FALSE;
  end;
end;

function TEceAppWindow._GetDocumentsCount: Integer;
begin
  Result := FDocuments.Count;
end;

function TEceAppWindow._GetHandle: HWND; safecall;
begin
  Result := handle;
end;

procedure TEceAppWindow._UpdateCaption;
begin
  UpdateCaption;
end;

procedure TEceAppWindow.CreateParams(var Param: CreateStruct);
begin
  inherited;
  Param.Style := Param.Style or WS_CLIPCHILDREN;
end;

procedure TEceAppWindow.wmSize(var msg: TWMSize);
var
  rt: Trect;
begin
  inherited;
  if ActiveDocumentWindow = nil then
    exit;
  GetClientRect(handle, rt);
  SetWindowPos(FPages.handle, 0, 0, 0, rt.Right, 24, 0);
  SetWindowPos(ActiveDocumentWindow.handle, 0, 0, 24, rt.Right,
    rt.Bottom - 24 - 172, 0);
  SetWindowPos(FConsole.handle, 0, 0, rt.Bottom - 172, rt.Right, 172, 0)
end;

procedure TEceAppWindow.wmSetFocus(var msg: TWmSetFocus);
begin
  inherited;
  if ActiveDocumentWindow = nil then
    exit;
  ActiveDocumentWindow.SetFocus;
end;

procedure TEceAppWindow.wmDestroy(var msg: TWMDestroy);
begin
  inherited;
  PostQuitMessage(0);
end;

Constructor TEceAppWindow.Create(AParent: Cardinal);
begin
  inherited;
  FDocuments := TList.Create;
  FPages := TPages.Create(handle);
  ShowWindow(FPages.handle, SW_SHOW);
  FConsole := TEceConsoleWindow.Create(handle, Self);
  FConsole.LoadColorTheme('color\console.txt');
  FConsole.SetFont('Consolas', 14);
  FConsole.Caret.Style := csClassic;

  FConsole.Kernal.AddObject('App', Self);
  RegisterName('Title', PROP_TITLE);
  RegisterName('Left', PROP_LEFT);
  RegisterName('Top', PROP_TOP);
  RegisterName('Width', PROP_WIDTH);
  RegisterName('Height', PROP_HEIGHT);

  RegisterName('StdIn', PROP_STDIN);
  RegisterName('StdOut', PROP_STDOUT);
  RegisterName('StdErr', PROP_STDERR);

  UpdateCaption;
end;

Destructor TEceAppWindow.Destroy;
begin
  if Assigned(FDocuments) then
  begin
    CloseAllDocuments;
    FDocuments.Free;
  end;
  if Assigned(FPages) then
  begin
    FPages.Free;
  end;
  if Assigned(FConsole) then
  begin
    FConsole.Free;
  end;

  inherited;
end;

procedure TEceAppWindow.NewDocument(AFileName: String);
var
  NewDocument: TEceDocumentWindow;
begin
  NewDocument := TEceEditorWindow.Create(handle, Self);
  FDocuments.Add(NewDocument);
  SendMessage(handle, WM_SIZE, 0, 0);
end;

function TEceAppWindow.CloseDocument(const index: Integer): boolean;
begin
  Documents[index].Free; // тут вылезет эксепшн приневерном индексе
  FDocuments.Delete(index);
  { todo: Нужно еще изменить текущий документ }
end;

function TEceAppWindow.CloseAllDocuments: boolean;
begin
  while DocumentsCount <> 0 do
    CloseDocument(0);
end;

function TEceAppWindow.GetDocumentsCount: Integer;
begin
  Result := FDocuments.Count;
end;

function TEceAppWindow.InvokeName(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; Params: TPropArr; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
begin
  case DispID of
{$REGION 'Title'}
    PROP_TITLE:
      begin
        case Flags of
          DISPATCH_GET:
            ;
          DISPATCH_SET:
            begin
              SetWindowText(handle, Params[0]);
            end
        else
          exit(DISP_E_MEMBERNOTFOUND)
        end;
      end;
{$ENDREGION}
{$REGION 'SIZE POS'}
    PROP_LEFT, PROP_TOP, PROP_WIDTH, PROP_HEIGHT:
      begin
        case Flags of
          DISPATCH_GET:
            ;
          DISPATCH_SET:
            case DispID of
              PROP_LEFT:
                Left := Params[0];
              PROP_TOP:
                Top := Params[0];
              PROP_WIDTH:
                Width := Params[0];
              PROP_HEIGHT:
                Height := Params[0];
            end;
        else
          exit(DISP_E_MEMBERNOTFOUND)
        end;
      end;
{$ENDREGION}
    PROP_STDIN, PROP_STDOUT, PROP_STDERR:
      case Flags of
        DISPATCH_SUB:
          case DispID of
            {TODO -oOnni -cGeneral : StdIn}
            PROP_STDIN: Exit(DISP_E_MEMBERNOTFOUND);
            PROP_STDOUT : StdOutProc(FConsole, Params[0], true);
            PROP_STDERR : StdErrProc(FConsole, Params[0], true);
          end;
      else
        exit(DISP_E_MEMBERNOTFOUND)
      end;
  end;
  Result := S_OK;
end;

function TEceAppWindow.LoadPlugin(AFileName: string): boolean;
var
  hPlugin: HMODULE;
  LoadProc: PGetPlugin;
  Plugin: IEcePlugin;
begin
  hPlugin := LoadLibrary(PChar(AFileName));
  if hPlugin = 0 then
    raise Exception.Create('Не удалось загрузить модуль ' + AFileName);

  LoadProc := GetProcAddress(hPlugin, 'GetPlugin');
  if @LoadProc = nil then
  begin
    FreeLibrary(hPlugin);
    raise Exception.Create('GetPlugin не найден в таблице экспорта модуля ' +
        AFileName);
  end;

  Plugin := LoadProc;
  Plugin.Load(Self);
end;

function TEceAppWindow.GetDocuments(const index: Integer): TEceDocumentWindow;
begin
  if (index < 0) or (index > DocumentsCount - 1) then
    raise Exception.Create('Неверный индекс документа');
  Result := TEceDocumentWindow(FDocuments[index]);
end;

procedure TEceAppWindow.SetActiveDocument(const value: Integer);
begin
  if (FActiveDocument < 0) or (FActiveDocument > DocumentsCount - 1) then
    Documents[FActiveDocument].KillFocus;

  FActiveDocument := value;
  if (FActiveDocument < 0) or (FActiveDocument > DocumentsCount - 1) then
    Documents[FActiveDocument].SetFocus;

  SendMessage(handle, WM_SIZE, 0, 0);
  UpdateCaption;
end;

procedure TEceAppWindow.UpdateCaption;
var
  Caption: string;
  Title: string;
begin
  Caption := 'Easy code editor';
  if DocumentsCount <> 0 then
  begin
    Title := ActiveDocumentWindow.DocumentTitle;
    if Title = '' then
      Title := 'New *';
    Caption := Title + ' - ' + Caption;
    FPages.pages[ActiveDocument].Title := Title;
  end;
  SetWindowText(handle, PChar(Caption))
end;

function TEceAppWindow.GetActiveDocumentWindow: TEceDocumentWindow;
begin
  if (ActiveDocument < 0) or (ActiveDocument > DocumentsCount - 1) then
    Result := nil
  else
    Result := Documents[ActiveDocument];
end;

end.
