unit AppWindow;
{$IFDEF fpc}{$MODE delphi}{$ENDIF}

interface

uses
  Windows,
  Messages,
  Classes,
  SysUtils,
  ActiveX,
  CommCtrl,
  Variants,
  // zeError,
  zeWndControls,
  zePages,
  iece,
  iEceObj,
  eceConsoleWindow;

type
  PGetPlugin = function: IEcePlugin; safecall;

  PMenuItem = ^TMenuItem;

  TMenuItem = record
    Handle: HMENU;
    ID: Word;
    Action: string;
    EnableTest: string;
    VisibleTest: string;
  end;

  EEceApplicationException = class(Exception)

  end;

  TEceAction = class;

  TEceAppWindow = class(TzeWndControl, IEceApplication, IDispatch)
  protected
    FConsole: TEceConsoleWindow;
    FActions: TInterfaceList;
    FDocuments: TInterfaceList;
    FPages: TPages;
    FActiveDocument: Integer;
    FMenus: TStringList;
    FMenuID: Word;
    FToolBar: IEceUiConteiner;
    FImgList: HIMAGELIST;
    // Модули
    FDocTypes: TInterfaceList;
    FDocTypesEx: TStringList;
    procedure CreateParams(var Param: CreateStruct); override;
    procedure wmSize(var msg: TWMSize);
    message WM_SIZE;
    procedure wmActivate(var msg: TWMActivate);
    message WM_ACTIVATE;

    procedure wmDestroy(var msg: TWMDestroy);
    message WM_DESTROY;

    procedure wmDrawItem(var msg: TWMDrawItem);
    message WM_DRAWITEM;
    procedure wmMeasureItem(var msg: TWMMeasureItem);
    message WM_MEASUREITEM;
    procedure wmCommand(var msg: TWMCommand);
    message WM_COMMAND;

    function GetDocumentsCount: Integer;
    function GetDocuments(const index: Integer): IEceDocument;
    procedure SetActiveDocument(const value: Integer);
    function GetActiveDocumentWindow: IEceDocument;
  protected
    function _GetHandle: HWND; safecall;
    function _GetDocumentsCount: Integer; safecall;
    function _GetDocuments(AIndex: Integer; var ADocument: IEceDocument)
      : Integer; safecall;
    procedure _FocusToActiveDocument; safecall;
    procedure _UpdateCaption; safecall;
    procedure _About; safecall;
  private
    function GethMenu(APath: string; var MenuName: string): HMENU;
    function GetDocTypeIndex(AFileName: string): Integer;
    function GetDocLoaderIndexByName(Name: string): Integer;
  protected
    function InvokeName(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; Params: TPropArr; var VarResult, ExcepInfo, ArgErr: TPropArr)
      : HResult; override;
  public
    procedure UpdateCaption;
    Constructor Create(AParent: Cardinal);
    Destructor Destroy; override;

    Procedure NewDocument(AFileName: String);
    function CloseDocument(const index: Integer): boolean;
    function CloseAllDocuments: boolean;

    function LoadPlugin(AFileName: string): boolean;
    constructor InitActions;
    constructor InitToolBar;

    property DocumentsCount: Integer read GetDocumentsCount;
    property Documents[const index: Integer]: IEceDocument read GetDocuments;
    property ActiveDocument
      : Integer read FActiveDocument write SetActiveDocument;
    property ActiveDocumentWindow: IEceDocument read GetActiveDocumentWindow;
    property Console: TEceConsoleWindow read FConsole;
    // Модули
    function CreateDocument(AFileName: string): IEceDocument;
    procedure RegisterDocument(Doc: IEceDocumentLoader); safecall;
    procedure RegisterDocumentEx(Doc: IEceDocumentLoader; ext: string);
      safecall;
  end;

  TEceAction = class(TEceInterfacedObject, IDispatch, IEceAction)
  private
    FApplication: TEceAppWindow;
    FItems: TInterfaceList;
    FHint: string;
    FText: string;
    FName: string;
    procedure SetHint(const value: string);
    procedure SetText(const value: string);
    procedure SetName(const value: string);

    procedure UpdateItems;
  protected
    function InvokeName(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; Params: TPropArr; var VarResult, ExcepInfo, ArgErr: TPropArr)
      : HResult; override;

    procedure AddLink(Ui: IEceUiItem); safecall;
    procedure RemoveLink(Ui: IEceUiItem); safecall;
  public
    Constructor Create(App: TEceAppWindow; AName, AText, AHint: string);
    Destructor Destroy; override;

    property Name: string read FName write SetName;
    property Text: string read FText write SetText;
    property Hint: string read FHint write SetHint;
  end;

var
  GlApp: TEceAppWindow;

implementation

uses
  EditorWindow,
  EceAppUI;

Function AboutDlgProc(wnd: HWND; msg: LRESULT; WParam: WParam; LParam: LParam)
  : boolean; stdcall;
var
  ctrl: HWND;
  ps: TPaintStruct;
  rgn: HRGN;
begin
  case msg of
    WM_INITDIALOG:
      begin
        SetWindowPos(wnd, 0, (GetSystemMetrics(SM_CXSCREEN) - 320) div 2,
          (GetSystemMetrics(SM_CYSCREEN) - 200) div 2, 320, 200, 0);
        CreateWindow('static', 'ECELOGO', WS_VISIBLE or WS_CHILD or SS_BITMAP,
          0, 0, 320, 200, wnd, 0, HInstance, nil);
        exit(true);
      end;
    WM_LBUTTONUP, WM_RBUTTONUP, WM_MBUTTONUP:
      begin
        EndDialog(wnd, 0)
      end
    else
      exit(false)
  end;
end;

procedure TEceAppWindow._About;
begin
  DialogBox(HInstance, 'ECEABOUTDIALOG', Handle, @AboutDlgProc);
end;

procedure TEceAppWindow._FocusToActiveDocument;
begin
  ActiveDocumentWindow._SetFocus;
end;

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
  Result := Handle;
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
  tbRect: Trect;
begin
  inherited;
  if ActiveDocumentWindow = nil then
    exit;
  GetClientRect(Handle, rt);
  tbRect := Rect(0, 0, FToolBar.GetWidth, FToolBar.GetHeight);
  // SetWindowPos(FToolBar, 0, 0, 0, tbRect.Bottom, rt.Right,
  // SWP_NOMOVE or SWP_NOACTIVATE);
  FToolBar.ParentResize;
  SetWindowPos(FPages.Handle, 0, 0, tbRect.Bottom, rt.Right, 24,
    SWP_NOACTIVATE);
  // SetWindowPos(ActiveDocumentWindow._GetHandle, 0, 0, tbRect.Bottom + 24,
  // rt.Right, rt.Bottom - 24 - 105 - tbRect.Bottom, SWP_NOACTIVATE);
  ActiveDocumentWindow._SetViewRect(0, tbRect.Bottom + 24, rt.Right,
    rt.Bottom - 24 - 105 - tbRect.Bottom);
  SetWindowPos(FConsole.Handle, 0, 0, rt.Bottom - 105, rt.Right, 105,
    SWP_NOACTIVATE)
end;

procedure TEceAppWindow.wmActivate(var msg: TWMActivate);
begin
  inherited;
  case msg.Active of
    0:
      begin
        if ActiveDocumentWindow <> nil then
          ActiveDocumentWindow._KillFocus;
      end
    else
    begin
      if ActiveDocumentWindow <> nil then
        ActiveDocumentWindow._SetFocus;
    end;
  end;
end;

procedure TEceAppWindow.wmCommand(var msg: TWMCommand);
var
  mi: PMenuItem;
  mif: TMenuItemInfo;
  Menu: HMENU;
begin
  Menu := GetMenu(Handle);
  ZeroMemory(@mif, SizeOf(mif));
  mif.cbSize := SizeOf(mif);
  mif.fMask := MIIM_DATA;
  if not GetMenuItemInfo(Menu, msg.ItemID, false, mif) then
  begin
    inherited;
    exit;
  end;
  mi := Pointer(mif.dwItemData);
  if mi <> nil then
    try
      // FConsole.Machine.AddCode(mi^.Action);
    except
      on e: Exception do
        MessageBox(Handle, PChar(format('%s: %s', [e.ClassName, e.Message])),
          nil, MB_ICONERROR);
    end;
end;

procedure TEceAppWindow.wmDestroy(var msg: TWMDestroy);
begin
  inherited;
  PostQuitMessage(0);
end;

procedure TEceAppWindow.wmMeasureItem(var msg: TWMMeasureItem);
var
  dc: hdc;
begin
  { TODO -oOnni -cGeneral : WM_MEASUREITEM не проходит }
  if msg.MeasureItemStruct^.itemHeight < 18 then
    msg.MeasureItemStruct^.itemHeight := 18;
  msg.MeasureItemStruct^.itemWidth := 120;
end;

procedure TEceAppWindow.wmDrawItem(var msg: TWMDrawItem);
var
  dc: hdc;
begin

end;

Constructor TEceAppWindow.InitActions;
var
  act: TEceAction;
begin
  // File.New
  act := TEceAction.Create(Self, 'Ece.File.New', 'New...', 'New');
  FActions.Add(act);
  // File.Open
  act := TEceAction.Create(Self, 'Ece.File.Open', 'Open...', 'Open');
  FActions.Add(act);
  // File.Save
  act := TEceAction.Create(Self, 'Ece.File.Save', 'Save', 'Save');
  FActions.Add(act);
  // File.SaveAs
  act := TEceAction.Create(Self, 'Ece.File.SaveAs', 'Save as...', 'Save as');
  FActions.Add(act);
  // File.Close
  act := TEceAction.Create(Self, 'Ece.File.Close', 'Close', 'Close');
  FActions.Add(act);
  // File.Print
  act := TEceAction.Create(Self, 'Ece.File.Print', 'Print...', 'Print');
  FActions.Add(act);
  // File.Exit
  act := TEceAction.Create(Self, 'Ece.File.Exit', 'Exit', 'Exit');
  FActions.Add(act);
  //=================================================================
  // File.Exit
  act := TEceAction.Create(Self, 'Ece.File.Exit', 'Exit', 'Exit');
  FActions.Add(act);
  // File.Exit
  act := TEceAction.Create(Self, 'Ece.File.Exit', 'Exit', 'Exit');
  FActions.Add(act);
end;

Constructor TEceAppWindow.InitToolBar;
var
  i: Integer;
begin
  for i := 0 to FActions.Count - 1 do
  FToolBar.AddActionItem(IEceAction(FActions[i]), 0);
end;

const
  PROP_TITLE = 0;
  PROP_LEFT = 1;
  PROP_TOP = 2;
  PROP_WIDTH = 3;
  PROP_HEIGHT = 4;

  PROP_STDIN = 5;
  PROP_STDOUT = 6;
  PROP_STDERR = 7;

  PROP_QUIT = 8;

  PROP_DOCOMENTSCOUNT = 9;
  PROP_DOCUMENTS = 10;
  PROP_ACTIVEDOCUMENTINDEX = 11;
  PROP_ACTIVEDOCUMENT = 12;

  PROP_ACTIONS = 13;
  PROP_ACTIONS_COUNT = 14;
  PROP_NEWACTION = 15;
  PROP_DELETE_ACTION = 16;

  PROP_MENU = 17;
  PROP_TOOLBAR = 18;

  PROP_ABOUT = 19;

Constructor TEceAppWindow.Create(AParent: Cardinal);
var
  Menu: HMENU;
  TooBmp: HBitmap;
begin
  inherited;

  // Глобальная переменная
  GlApp := Self;
  FActions := TInterfaceList.Create;

  Menu := CreateMenu;

  SetMenu(Handle, Menu);
  AppendMenu(Menu, MF_STRING, 0, 'File');
  AppendMenu(Menu, MF_STRING, 0, 'Edit');
  AppendMenu(Menu, MF_STRING, 0, 'View');
  AppendMenu(Menu, MF_STRING, 0, 'Project');
  AppendMenu(Menu, MF_STRING, 0, 'Run');
  AppendMenu(Menu, MF_STRING, 0, 'Tools');
  AppendMenu(Menu, MF_STRING, 0, 'Help');

  FMenus := TStringList.Create;

  // ToolBAr
  FImgList := ImageList_Create(16, 16, ILC_COLOR24 or ILC_MASK, 0, 0);
  TooBmp := LoadBitmap(HInstance, 'ECETOOLBAR');
  ImageList_AddMasked(FImgList, TooBmp, $000000);
  DeleteObject(TooBmp);

  FToolBar := TEceToolBar.Create(Handle);

  FToolBar.SetImageList(FImgList);

  FDocuments := TInterfaceList.Create;
  FPages := TPages.Create(Handle);
  ShowWindow(FPages.Handle, SW_SHOW);
  FConsole := TEceConsoleWindow.Create(Handle, Self);
  FConsole.LoadColorTheme(ExtractFilePath(ParamStr(0)) + 'color\console.txt');
  try
    FConsole.SetFont('Consolas', 14);
  except

  end;
  FConsole.Caret.Style := csClassic;

  FConsole.Kernal.AddObject('Application', Self);

  RegisterName('Title', PROP_TITLE);
  RegisterName('Left', PROP_LEFT);
  RegisterName('Top', PROP_TOP);
  RegisterName('Width', PROP_WIDTH);
  RegisterName('Height', PROP_HEIGHT);

  RegisterName('StdIn', PROP_STDIN);
  RegisterName('StdOut', PROP_STDOUT);
  RegisterName('StdErr', PROP_STDERR);

  RegisterName('Quit', PROP_QUIT);

  RegisterName('DocumentsCount', PROP_DOCOMENTSCOUNT);
  RegisterName('Documents', PROP_DOCUMENTS);

  RegisterName('ActiveDocumentIndex', PROP_ACTIVEDOCUMENTINDEX);
  RegisterName('ActiveDocument', PROP_ACTIVEDOCUMENT);

  RegisterName('Actions', PROP_ACTIONS);
  RegisterName('ActionsCount', PROP_ACTIONS_COUNT);
  RegisterName('NewAction', PROP_NEWACTION);
  RegisterName('DeleteAction', PROP_DELETE_ACTION);

  RegisterName('MainMenu', PROP_MENU);
  RegisterName('ToolBar', PROP_TOOLBAR);

  RegisterName('About', PROP_ABOUT);

  FDocTypes := TInterfaceList.Create;
  FDocTypesEx := TStringList.Create;

  UpdateCaption;

  // Создаем базовые события
  InitActions;
  // Создаем кнопки на панели и меню
  InitToolBar;
  // Запускаем скрипты
  FConsole.LoadStdScript;
end;

function TEceAppWindow.GetDocLoaderIndexByName(Name: string): Integer;
var
  i: Integer;
  Doc: IEceDocumentLoader;
begin
  for i := 0 to FDocTypes.Count - 1 do
  begin
    Doc := IEceDocumentLoader(FDocTypes[i]);
    if AnsiLowerCase(Name) = AnsiLowerCase(Doc.GetName) then
      exit(i);
  end;
  Result := -1;
end;

function TEceAppWindow.GetDocTypeIndex(AFileName: string): Integer;
var
  ext: String;
  index: Integer;

begin
  // Вначале пробегаем по расширениям
  ext := ExtractFileExt(AFileName);
  index := FDocTypesEx.IndexOfName(ext);
  if index <> -1 then
  begin
    Result := GetDocLoaderIndexByName(FDocTypesEx.ValueFromIndex[index]);
    if Result <> -1 then
      exit;
  end
  else
  begin
    Result := 0;
  end;
end;

function TEceAppWindow.CreateDocument(AFileName: string): IEceDocument;
var
  Loader: IEceDocumentLoader;
  err: string;
begin
  Loader := IEceDocumentLoader(FDocTypes[GetDocTypeIndex(AFileName)]);
  Loader.CreateDocument(Self, AFileName, Result, err);
end;

Destructor TEceAppWindow.Destroy;
var
  pi: ^IInterface;
  i: Integer;
begin
  ImageList_Destroy(FImgList);

  if Assigned(FDocuments) then
  begin
    CloseAllDocuments;
    FDocuments.Clear;
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
  if Assigned(FMenus) then
  begin
    FMenus.Free;
  end;
  if Assigned(FDocTypes) then
  begin
    FDocTypes.Free;
  end;
  if Assigned(FDocTypesEx) then
  begin
    FDocTypesEx.Free;
  end;
  if Assigned(FActions) then
  begin
    FActions.Free;
  end;
  inherited;
end;

procedure TEceAppWindow.NewDocument(AFileName: String);
var
  NewDocument: IEceDocument;
begin
  // NewDocument := IEceDocument.Create(Handle, Self);
  NewDocument := CreateDocument(AFileName);
  FDocuments.Add(NewDocument);
  FPages.AddPage(NewDocument.GetFileName, nil);
  SendMessage(Handle, WM_SIZE, 0, 0);
end;

procedure TEceAppWindow.RegisterDocument(Doc: IEceDocumentLoader);
begin
  if FDocTypes.IndexOf(Doc) = -1 then
    FDocTypes.Add(Doc);
end;

procedure TEceAppWindow.RegisterDocumentEx(Doc: IEceDocumentLoader;
  ext: string);
begin
  FDocTypesEx.Insert(0, format('%s=%s', [ext, Doc.GetName]));
end;

function TEceAppWindow.CloseDocument(const index: Integer): boolean;
begin
  // Documents[index].Free; // тут вылезет эксепшн приневерном индексе
  FDocuments.Delete(index);
  { todo: Нужно еще изменить текущий документ }
end;

function TEceAppWindow.GethMenu(APath: string; var MenuName: string): HMENU;
var
  sl: TStringList;
  i: Integer;
  Menu, PopUp: HMENU;
  MenuPath: string;
  index: Integer;
  mi: PMenuItem;
begin
  sl := TStringList.Create;
  try
    Menu := GetMenu(Handle);
    sl.Text := StringReplace(APath, '/', #13#10, [rfReplaceAll]);
    for i := 0 to sl.Count - 2 do
    begin
      MenuPath := MenuPath + sl[i] + '\';
      index := FMenus.IndexOf(MenuPath);
      if index = -1 then
      begin
        new(mi);

        PopUp := CreatePopupMenu;
        mi^.Handle := PopUp;
        FMenus.AddObject(MenuPath, Tobject(mi));

        Windows.AppendMenu(Menu, MF_POPUP, PopUp, PChar(sl[i]));
      end
      else
      begin
        mi := Pointer(FMenus.Objects[index]);
        PopUp := mi^.Handle;
      end;
      Menu := PopUp;
    end;
  finally
    MenuName := sl[sl.Count - 1];
    sl.Free;
    Result := PopUp;
  end;
end;

procedure ClearBitmap(hbmp: HBitmap);
var
  cdc: hdc;
  i: Integer;
  j: Integer;
  c: Integer;
  cc: Integer;
begin
  cdc := CreateCompatibleDC(0);
  SelectObject(cdc, hbmp);
  c := GetPixel(cdc, 0, 15);
  cc := GetSysColor(COLOR_BTNFACE);
  for i := 0 to 32 - 1 do
    for j := 0 to 16 - 1 do
    begin
      if GetPixel(cdc, i, j) = c then
        SetPixel(cdc, i, j, cc);
    end;
  DeleteDC(cdc);
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
  LocaleID: Integer; Flags: Word; Params: TPropArr; var VarResult, ExcepInfo,
  ArgErr: TPropArr): HResult;
var
  n: Integer;
begin
{$IFNDEF fpc}
  case DispID of
{$REGION 'Title'}
    PROP_TITLE:
      begin
        case Flags of
          DISPATCH_GET:
            VarResult[0] := Title;
          DISPATCH_SET:
            begin
              SetWindowText(Handle, Params[0]);
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
            case DispID of
              PROP_LEFT:
                VarResult[0] := Left;
              PROP_TOP:
                VarResult[0] := Top;
              PROP_WIDTH:
                VarResult[0] := Width;
              PROP_HEIGHT:
                VarResult[0] := Height;
            end;
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
{$REGION 'IN/OUT'}
    PROP_STDIN, PROP_STDOUT, PROP_STDERR:
      case Flags of
        DISPATCH_SUB:
          case DispID of
            { TODO -oOnni -cGeneral : StdIn }
            PROP_STDIN:
              exit(DISP_E_MEMBERNOTFOUND);
            PROP_STDOUT:
              StdOutProc(FConsole, Params[0], true);
            PROP_STDERR:
              StdErrProc(FConsole, Params[0], true);
          end;
      else
        exit(DISP_E_MEMBERNOTFOUND)
      end;
{$ENDREGION}
{$REGION 'Quit'}
    PROP_QUIT:
      case Flags of
        DISPATCH_SUB:
          DestroyWindow(Handle);
      else
        exit(DISP_E_MEMBERNOTFOUND)

      end;
{$ENDREGION}
{$REGION 'Documents count'}
    PROP_DOCOMENTSCOUNT:
      case Flags of
        DISPATCH_GET:
          VarResult[0] := DocumentsCount;
      else
        exit(DISP_E_MEMBERNOTFOUND)

      end;
{$ENDREGION}
{$REGION 'Documents'}
    PROP_DOCUMENTS:
      begin
        case Flags of
          DISPATCH_GET:
            begin
              n := Params[0];
              VarResult[0] := Documents[n] as IDispatch;
            end
          else
            exit(DISP_E_MEMBERNOTFOUND)
        end;
      end;
{$ENDREGION}
{$REGION 'ActiveDocumentIndex'}
    PROP_ACTIVEDOCUMENTINDEX:
      case Flags of
        DISPATCH_GET:
          begin
            VarResult[0] := ActiveDocument;
          end
        else
          exit(DISP_E_MEMBERNOTFOUND)
      end;
{$ENDREGION}
{$REGION 'ActiveDocument'}
    PROP_ACTIVEDOCUMENT:
      case Flags of
        DISPATCH_GET:
          begin
            VarResult[0] := Documents[ActiveDocument] as IDispatch;
          end
        else
          exit(DISP_E_MEMBERNOTFOUND)
      end;
{$ENDREGION}
{$REGION 'Actions'}
    PROP_ACTIONS:
      begin
        case Flags of
          DISPATCH_GET:
            begin
              { TODO -oOnni -cGeneral : Сделать вохможность доступа по имени,
                а не только по индексу }
              n := Params[0];
              VarResult[0] := FActions[n] as IDispatch;
            end
          else
            exit(DISP_E_MEMBERNOTFOUND)
        end;
        Result := S_OK;
      end;
{$ENDREGION}
{$REGION 'ActionsCount'}
    PROP_ACTIONS_COUNT:
      begin
        case Flags of
          DISPATCH_GET:
            begin
              VarResult[0] := FActions.Count;
            end
          else
            exit(DISP_E_MEMBERNOTFOUND)
        end;
        Result := S_OK;
      end;
{$ENDREGION}
{$REGION 'ActionsCount'}
    PROP_NEWACTION:
      begin
        case Flags of
          DISPATCH_GET:
            begin
              n := FActions.Add
                (TEceAction.Create(Self, Params[0], Params[1], Params[2]));
              VarResult[0] := FActions[n] as IDispatch
            end
          else
            exit(DISP_E_MEMBERNOTFOUND)
        end;
        Result := S_OK;
      end;
{$ENDREGION}
{$REGION 'DeleteAction'}
    PROP_DELETE_ACTION:
      begin
        case Flags of
          DISPATCH_METHOD:
            begin
              { TODO -oOnni -cGeneral : Сделать вохможность доступа по имени,
                а не только по индексу }
              n := Params[0];
              FActions.Delete(n);
            end
          else
            exit(DISP_E_MEMBERNOTFOUND)
        end;
        Result := S_OK;
      end;
{$ENDREGION}
{$REGION 'Main menu'}
    PROP_MENU:
      begin
        case Flags of
          DISPATCH_GET:
            begin

            end
          else
            exit(DISP_E_MEMBERNOTFOUND)
        end;
        Result := S_OK;
      end;
{$ENDREGION}
{$REGION 'ToolBar'}
    PROP_TOOLBAR:
      begin
        case Flags of
          DISPATCH_GET:
            begin
              // todo
            end
          else
            exit(DISP_E_MEMBERNOTFOUND)
        end;
        Result := S_OK;
      end;
{$ENDREGION}
{$REGION 'About'}
    PROP_ABOUT:
      begin
        case Flags of
          DISPATCH_GET:
            VarResult[0] := Title;
          DISPATCH_SUB:
            begin
              _About;
            end
          else
            exit(DISP_E_MEMBERNOTFOUND)
        end;
      end;
{$ENDREGION}
  else
    Result := DISP_E_MEMBERNOTFOUND;
  end;
{$ENDIF}
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

  try
    LoadProc := GetProcAddress(hPlugin, 'GetPlugin');
    if @LoadProc = nil then
    begin
      FreeLibrary(hPlugin);
      raise Exception.CreateFmt(
        'Метод GetPlugin не найден в таблице экспорта модуля %s', [AFileName]);
    end;

    Plugin := LoadProc;
    Plugin.Load(Self);
  except
    on e: Exception do
      raise Exception.CreateFmt('Ошибка "%s: %s" при загрузке модуля "%s"',
        [e.ToString, e.Message, AFileName]);
  end;
end;

function TEceAppWindow.GetDocuments(const index: Integer): IEceDocument;
begin
  if (index < 0) or (index > DocumentsCount - 1) then
    raise Exception.Create('Неверный индекс документа');
  Result := IEceDocument(FDocuments[index]);
end;

procedure TEceAppWindow.SetActiveDocument(const value: Integer);
begin
  if (FActiveDocument < 0) or (FActiveDocument > DocumentsCount - 1) then
    Documents[FActiveDocument]._KillFocus;

  FActiveDocument := value;
  if (FActiveDocument < 0) or (FActiveDocument > DocumentsCount - 1) then
    Documents[FActiveDocument]._SetFocus;

  SendMessage(Handle, WM_SIZE, 0, 0);
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
    Title := ExtractFileName(ActiveDocumentWindow.GetFileName);
    if Title = '' then
      Title := 'New *';
    Caption := Title + ' - ' + Caption;
    FPages.pages[ActiveDocument].Title := Title;
  end;
  SetWindowText(Handle, PChar(Caption))
end;

function TEceAppWindow.GetActiveDocumentWindow: IEceDocument;
begin
  if (ActiveDocument < 0) or (ActiveDocument > DocumentsCount - 1) then
    Result := nil
  else
    Result := Documents[ActiveDocument];
end;

{ TEceAction }

const
  PROP_ACTION_NAME = 1;
  PROP_ACTION_TEXT = 2;
  PROP_ACTION_HINT = 3;
  // PROP_ACTION_IMAGE = 4;
  PROP_ACTION_ENABLE = 5;
  PROP_ACTION_VISIBLE = 6;
  PROP_ACTION_CHECKED = 7;

constructor TEceAction.Create(App: TEceAppWindow; AName, AText, AHint: string);
begin
  FApplication := App;
  FItems := TInterfaceList.Create;

  FName := AName;
  Text := AText;
  Hint := AHint;

  RegisterName('Name', PROP_ACTION_NAME);
  RegisterName('Text', PROP_ACTION_TEXT);
  RegisterName('Hint', PROP_ACTION_HINT);
  // RegisterName('Image', PROP_ACTION_IMAGE);
  RegisterName('Enable', PROP_ACTION_ENABLE);
  RegisterName('Visible', PROP_ACTION_VISIBLE);
  RegisterName('Checked', PROP_ACTION_CHECKED);
end;

destructor TEceAction.Destroy;
begin
  if Assigned(FItems) then
  begin
    FItems.Free;
  end;
  inherited;
end;

function TEceAction.InvokeName(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; Params: TPropArr; var VarResult, ExcepInfo,
  ArgErr: TPropArr): HResult;
begin
  case DispID of
{$REGION 'Name'}
    PROP_ACTION_NAME:
      begin
        case Flags of
          DISPATCH_GET:
            VarResult[0] := Name;
          DISPATCH_SET:
            Name := Params[0];
        else
          exit(DISP_E_MEMBERNOTFOUND)
        end;
      end;
{$ENDREGION}
{$REGION 'Text'}
    PROP_ACTION_TEXT:
      begin
        case Flags of
          DISPATCH_GET:
            VarResult[0] := Text;
          DISPATCH_SET:
            Text := Params[0];
        else
          exit(DISP_E_MEMBERNOTFOUND)
        end;
      end;
{$ENDREGION}
{$REGION 'Hint'}
    PROP_ACTION_HINT:
      begin
        case Flags of
          DISPATCH_GET:
            VarResult[0] := Hint;
          DISPATCH_SET:
            Hint := Params[0];
        else
          exit(DISP_E_MEMBERNOTFOUND)
        end;
      end;
{$ENDREGION}
  else
    Result := DISP_E_MEMBERNOTFOUND;
  end;
end;

procedure TEceAction.AddLink(Ui: IEceUiItem);
var
  Index: Integer;
begin
  Index := FItems.IndexOf(Ui as IInterface);
  if Index = -1 then
    FItems.Add(Ui)
end;

procedure TEceAction.RemoveLink(Ui: IEceUiItem);
var
  Index: Integer;
begin
  Index := FItems.IndexOf(Ui as IInterface);
  if Index <> -1 then
    FItems.Delete(Index);
end;

procedure TEceAction.SetHint(const value: string);
begin
  FHint := value;
  UpdateItems;
end;

procedure TEceAction.SetName(const value: string);
begin
  FName := value;
end;

procedure TEceAction.SetText(const value: string);
begin
  FText := value;
  UpdateItems;
end;

procedure TEceAction.UpdateItems;
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
  begin
    IEceUiItem(FItems[i]).UpdateState;
  end;
end;

initialization

InitCommonControls;

finalization

end.
