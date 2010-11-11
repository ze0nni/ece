unit AppWindow;
{$IFDEF fpc}{$MODE delphi}{$ENDIF}

interface

{$I EceLanguage.inc}

uses
{$IFDEF forth}
  VForth,
{$ENDIF}
  Windows,
  Messages,
  Classes,
  SysUtils,
  ActiveX,
  CommCtrl,
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

  TEceAppWindow = class(TzeWndControl, IEceApplication, IDispatch
{$IFDEF forth}, IVForthModule {$ENDIF})
  protected
    FConsole: TEceConsoleWindow;
    FDocuments: TInterfaceList;
    FPages: TPages;
    FActiveDocument: Integer;
    FMenus: TStringList;
    FMenuID: Word;
    FImageId: Word;
    FToolBar: HWND;
    FImgList: HIMAGELIST;
    // Модули
    FDocTypes: TInterfaceList;
    FDocTypesEx: TStringList;
{$IFDEF forth}
    FModuleProp: TStringList;
{$ENDIF}
    procedure CreateParams(var Param: CreateStruct); override;
    procedure wmSize(var msg: TWMSize);
    message WM_SIZE;
    procedure wmSetFocus(var msg: TWmSetFocus);
    message WM_SETFOCUS;

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
    procedure AppendMenu(AName, AAction, EnableTest, VisibleTest,
      Image: string);
{$IFDEF forth}
    function GetModule: IVForthModule; stdcall;
    function GetMachine: IVForthMachine; stdcall;
    procedure SetProp(AProp, AValue: string); stdcall;
    function GetProp(AProp: string): string; stdcall;
    procedure Register(AMachine: IVForthMachine); stdcall;
    // Функция в зависимости от того, есть звездочка на конце слова или нет
    // Возвращает активный документ или документ, номер которого в вершине стека
    function GetDoc(AMachine: IVForthMachine; const Word: string): IEceDocument;
    function GetEditor(AMachine: IVForthMachine; const Word: string)
      : IEceEditor;
{$ENDIF}
  protected
    function _GetHandle: HWND; safecall;
    function _GetDocumentsCount: Integer; safecall;
    function _GetDocuments(AIndex: Integer; var ADocument: IEceDocument)
      : Integer; safecall;
    procedure _FocusToActiveDocument; stdcall;
    procedure _UpdateCaption; safecall;
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

    property DocumentsCount: Integer read GetDocumentsCount;
    property Documents[const index: Integer]: IEceDocument read GetDocuments;
    property ActiveDocument
      : Integer read FActiveDocument write SetActiveDocument;
    property ActiveDocumentWindow: IEceDocument read GetActiveDocumentWindow;
    property Console: TEceConsoleWindow read FConsole;
    // Модули
    function CreateDocument(AFileName: string): IEceDocument;
    procedure RegisterDocument(Doc: IEceDocumentLoader); stdcall;
    procedure RegisterDocumentEx(Doc: IEceDocumentLoader; ext: string); stdcall;
  end;

var
  GlApp: TEceAppWindow;

implementation

{$IFDEF forth}

uses
  EditorWindow,
  VForthAthom;
{$ELSE}

uses
  EditorWindow;
{$ENDIF}

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
  GetClientRect(FToolBar, tbRect);
  // SetWindowPos(FToolBar, 0, 0, 0, tbRect.Bottom, rt.Right,
  // SWP_NOMOVE or SWP_NOACTIVATE);
  SendMessage(FToolBar, TB_AUTOSIZE, 0, 0);

  SetWindowPos(FPages.Handle, 0, 0, tbRect.Bottom, rt.Right, 24,
    SWP_NOACTIVATE);
  // SetWindowPos(ActiveDocumentWindow._GetHandle, 0, 0, tbRect.Bottom + 24,
  // rt.Right, rt.Bottom - 24 - 105 - tbRect.Bottom, SWP_NOACTIVATE);
  ActiveDocumentWindow._SetViewRect(0, tbRect.Bottom + 24, rt.Right,
    rt.Bottom - 24 - 105 - tbRect.Bottom);
  SetWindowPos(FConsole.Handle, 0, 0, rt.Bottom - 105, rt.Right, 105,
    SWP_NOACTIVATE)
end;

procedure TEceAppWindow.wmSetFocus(var msg: TWmSetFocus);
begin
  inherited;
  if ActiveDocumentWindow = nil then
    exit;
  ActiveDocumentWindow._SetFocus;
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
      FConsole.Machine.AddCode(mi^.Action);
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
  dc: HDC;
begin
  { TODO -oOnni -cGeneral : WM_MEASUREITEM не проходит }
  if msg.MeasureItemStruct^.itemHeight < 18 then
    msg.MeasureItemStruct^.itemHeight := 18;
  msg.MeasureItemStruct^.itemWidth := 120;
end;

procedure TEceAppWindow.wmDrawItem(var msg: TWMDrawItem);
var
  dc: HDC;
begin

end;

Constructor TEceAppWindow.Create(AParent: Cardinal);
var
  Menu: HMENU;
begin
  inherited;
{$IFDEF forth}
  GlApp := Self;
  Menu := CreateMenu;
  SetMenu(Handle, Menu);
{$ENDIF}
  FMenus := TStringList.Create;

  // ToolBAr
  FToolBar := CreateWindowEx(TBSTYLE_EX_DOUBLEBUFFER, TOOLBARCLASSNAME,
    'ToolBar', WS_VISIBLE or WS_CHILD or TBSTYLE_TOOLTIPS OR TBSTYLE_FLAT, 0,
    0, 0, 24, Handle, 0, HInstance, nil);
  FImgList := ImageList_Create(16, 16, ILC_COLOR24, 0, 0);
  SendMessage(FToolBar, TB_SETIMAGELIST, 0, FImgList);

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
{$IFDEF forth}
  FModuleProp := TStringList.Create;
{$ELSE}
  FConsole.Kernal.AddObject('Application', Self);
{$ENDIF}
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

  FDocTypes := TInterfaceList.Create;
  FDocTypesEx := TStringList.Create;

  UpdateCaption;
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
{$IFDEF forth}
  if Assigned(FModuleProp) then
    FModuleProp.Free;
{$ENDIF}
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

procedure ClearBitmap(hbmp: HBITMAP);
var
  cdc: HDC;
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

procedure TEceAppWindow.AppendMenu(AName, AAction, EnableTest, VisibleTest,
  Image: string);
var
  hbmp: HBITMAP;
  Menu: HMENU;
  MenuName: string;
  m: HMENU;
  mif: TMenuItemInfo;
  mi: PMenuItem;
  btn: TTBButton;
begin
  Menu := GethMenu(AName, MenuName);

  inc(FMenuID); // Новый индекс

  if MenuName <> '-' then
  begin
    Windows.AppendMenu(Menu, MF_STRING, FMenuID, PChar(MenuName));
    hbmp := LoadImage(HInstance, PChar(Image), IMAGE_BITMAP, 0, 0,
      LR_LOADFROMFILE or LR_LOADTRANSPARENT or LR_COLOR);

    ClearBitmap(hbmp);

    SetMenuItemBitmaps(Menu, FMenuID, MF_BYCOMMAND, hbmp, hbmp);

    new(mi);
    mi^.Handle := Menu;
    mi^.ID := FMenuID;
    mi^.Action := AAction;
    mi^.EnableTest := EnableTest;
    mi^.VisibleTest := VisibleTest;

    ZeroMemory(@mif, SizeOf(mif));
    mif.cbSize := SizeOf(mif);
    mif.fMask := MIIM_DATA;
    mif.dwItemData := Integer(mi);

    SetMenuItemInfo(Menu, FMenuID, false, mif);

    SendMessage(FToolBar, TB_BUTTONSTRUCTSIZE, SizeOf(btn), 0);
    ZeroMemory(@btn, SizeOf(btn));

    if hbmp <> 0 then
    begin
      ImageList_Add(FImgList, hbmp, 0);
      btn.fsStyle := TBSTYLE_BUTTON;
      btn.fsState := TBSTATE_ENABLED;
      btn.idCommand := FMenuID;
      btn.iBitmap := FImageId * 2;
      SendMessage(FToolBar, TB_ADDBUTTONS, 1, Integer(@btn));
      inc(FImageId);
    end;
  end
  else
  begin
    Windows.AppendMenu(Menu, MF_SEPARATOR, FMenuID, nil);

    SendMessage(FToolBar, TB_BUTTONSTRUCTSIZE, SizeOf(btn), 0);
    ZeroMemory(@btn, SizeOf(btn));
    btn.fsStyle := TBSTYLE_SEP;
    SendMessage(FToolBar, TB_ADDBUTTONS, 1, Integer(@btn));
  end;
  m := GetMenu(Handle);

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
{$IFDEF forth}

function TEceAppWindow.GetModule: IVForthModule;
begin
  Result := Self;
end;

function TEceAppWindow.GetMachine: IVForthMachine;
begin
  Result := FConsole.Machine;
end;
{$ENDIF}

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
      case Flags of
        DISPATCH_GET:
          begin
            n := Params[0];
            VarResult[0] := Documents[n] as IDispatch;
          end
        else
          exit(DISP_E_MEMBERNOTFOUND)
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
  end;
  Result := S_OK;
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
{$IFDEF forth}

function TEceAppWindow.GetDoc(AMachine: IVForthMachine; const Word: string)
  : IEceDocument;
begin
  if Word[Length(Word)] <> '*' then
    Result := ActiveDocumentWindow
  else
    Result := Documents[AMachine.PopInt];
end;

function TEceAppWindow.GetEditor(AMachine: IVForthMachine; const Word: string)
  : IEceEditor;
var
  Doc: IEceDocument;
begin
  if Word[Length(Word)] <> '*' then
    Doc := ActiveDocumentWindow
  else
    Doc := Documents[AMachine.PopInt];
  if Doc.QueryInterface(IIdEceEditor, Result) <> S_OK then
    raise EEceApplicationException.CreateFmt('Не удалось преобразовать "%s" к %s.', [Doc.GetFileName, GUIDToString(IIdEceEditor)]);
end;
{$ENDIF}

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
{$IFDEF forth}

procedure TEceAppWindow.SetProp(AProp, AValue: string);
begin
  FModuleProp.Values[AProp] := AValue;
end;

function TEceAppWindow.GetProp(AProp: string): string;
begin
  Result := FModuleProp.Values[AProp]
end;
{$ENDIF}
{$REGION 'APP'}
{$IFDEF forth}

procedure efGetAppTitle(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  AMachine.PushString(GlApp.Title);
end;

procedure efSetAppTitle(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  GlApp.Title := AMachine.PopString;
end;

procedure efAppClose(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  SendMessage(GlApp.Handle, WM_SYSCOMMAND, SC_CLOSE, 0)
end;

procedure efAppQuit(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  PostQuitMessage(0);
end;

procedure efAppNewDocument(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  GlApp.NewDocument(AMachine.PopString);
  GlApp.ActiveDocument := GlApp.DocumentsCount - 1;
  //GlApp.Documents[GlApp.ActiveDocument]._SetFocus;
end;
{$ENDIF}
{$ENDREGION}
{$REGION 'Doc'}
{$IFDEF forth}

procedure efGetDocsCount(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  AMachine.PushInt(GlApp.DocumentsCount);
end;

procedure efGetDocFileName(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  AMachine.PushString(GlApp.GetDoc(AMachine, PAthomStr).GetFileName);
end;

procedure efGetDocTitle(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  { TODO -oOnni -cGeneral : TITLE }
  AMachine.PushString(GlApp.GetDoc(AMachine, PAthomStr).GetFileName);
end;

procedure efSetDocTitle(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  { TODO -oOnni -cGeneral : TITLE }
  // GlApp.GetDoc(AMachine, PAthomStr).Title := AMachine.PopString;
end;
{$ENDIF}
{$ENDREGION}
{$REGION 'Editor'}
{$IFDEF forth}
{ TODO -oOnni -cGeneral : переделать все что касается работы с редактором }

procedure efGetEditorLinesCount(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  AMachine.PushInt(GlApp.GetEditor(AMachine, PAthomStr)._GetLinesCount);
end;

procedure efGetEditorLineText(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  AMachine.PushString(GlApp.GetEditor(AMachine, PAthomStr)._GetLines
      (AMachine.PopInt)._GetText);
end;

procedure efSetEditorLineText(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  s: String;
  l: Integer;
begin
  l := AMachine.PopInt;
  s := AMachine.PopString;
  GlApp.GetEditor(AMachine, PAthomStr)._GetLines(l)._SetText(s);
end;

procedure efInvalidateEditor(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  GlApp.GetEditor(AMachine, PAthomStr)._Invalidate;
end;

procedure efInvalidateEditorLine(AMachine: IVForthMachine;
  AAthom: IVForthAthom; PAthomStr: PWideChar); stdcall;
begin
  GlApp.GetEditor(AMachine, PAthomStr)._InvalidateLine(AMachine.PopInt);
end;

procedure efAddEditorLine(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  AMachine.PushInt(GlApp.GetEditor(AMachine, PAthomStr)._AddLine._GetIndex);
end;

procedure efAddEditorLineText(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  with GlApp.GetEditor(AMachine, PAthomStr)._AddLine do
  begin
    _SetText(AMachine.PopString);
    AMachine.PushInt(_GetIndex);
  end;
end;

procedure efInsertEditorLine(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  GlApp.GetEditor(AMachine, PAthomStr)._InsertLine(AMachine.PopInt)
end;

procedure efInsertEditorLineText(AMachine: IVForthMachine;
  AAthom: IVForthAthom; PAthomStr: PWideChar); stdcall;
begin
  GlApp.GetEditor(AMachine, PAthomStr)._InsertLine(AMachine.PopInt)._SetText
    (AMachine.PopString)
  // GlApp.GetEditor(AMachine, PAthomStr).InsertLine(AMachine.PopInt)
  // .Text := AMachine.PopString
end;

procedure efDeleteEditorLine(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  e: IEceDocument;
begin
  // e := GlApp.GetEditor(AMachine, PAthomStr);
  // if e.Count = 1 then
  // // Первую строчку удалять ну ни как нельзя
  // e.Strings[0] := ''
  // else
  //
  // e.DeleteLine(AMachine.PopInt);
  GlApp.GetEditor(AMachine, PAthomStr)._DeleteLine(AMachine.PopInt);
end;

// caret

procedure efGetEditorCaretX(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  AMachine.PushInt(GlApp.GetEditor(AMachine, PAthomStr)._GetCaret._GetX);
end;

procedure efGetEditorCaretY(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  AMachine.PushInt(GlApp.GetEditor(AMachine, PAthomStr)._GetCaret._GetY);
end;

procedure efGetEditorCaretLine(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  AMachine.PushInt(GlApp.GetEditor(AMachine, PAthomStr)._GetCaret._GetLine);
end;

procedure efSetEditorCaretX(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  GlApp.GetEditor(AMachine, PAthomStr)._GetCaret._SetX(AMachine.PopInt);
end;

procedure efSetEditorCaretY(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  GlApp.GetEditor(AMachine, PAthomStr)._GetCaret._SetY(AMachine.PopInt);
end;

procedure efSetEditorCaretLine(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  { TODO -oOnni -cGeneral : Line только для чтения }
  // GlApp.GetEditor(AMachine, PAthomStr).Caret.Line := AMachine.PopInt;
  AMachine.StdErr('Readonly property');
end;

procedure efEditorInsert(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  e: IEceEditor;
  c: ICaret;
  s: string;
begin
  e := GlApp.GetEditor(AMachine, PAthomStr);
  c := e._GetCaret;
  s := AMachine.PopString;
  e._GetLines(c._GetLine)._Insert(s, c._GetX + 1);
  c._SetX(c._GetX + Length(s));
end;

procedure efEditorInsertXY(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  e: IEceEditor;
  l, c: Integer;
begin
   e := GlApp.GetEditor(AMachine, PAthomStr);
   l := AMachine.PopInt;
   c := AMachine.PopInt;
   e._GetLines(l)._Insert(AMachine.PopString, c + 1);
end;

procedure efEditorOwerwrite(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  { TODO -oOnni -cGeneral : Owerwrite }
end;

procedure efEditorOwerwriteXY(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  { TODO -oOnni -cGeneral : OwerwriteXY }
end;
{$ENDIF}
{$ENDREGION}
{$REGION 'Menu'}

procedure efAddEceMenuItem(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  vName, vAction, vEnable, vVisible, vImage: IVForthVariant;
begin
  vName := AMachine.Pop;
  vAction := AMachine.Pop;
  vEnable := AMachine.Pop;
  vVisible := AMachine.Pop;
  vImage := AMachine.Pop;
  GlApp.AppendMenu(vName.StringValue, vAction.StringValue, vEnable.StringValue,
    vVisible.StringValue, vImage.StringValue);
end;
{$ENDREGION}
{$IFDEF forth}

procedure TEceAppWindow.Register(AMachine: IVForthMachine);
  procedure AddAthom(name: string; proc: PVForthSystemAthomProc; Ex: boolean);
  begin
    AMachine.AddAthom(CreateVForthSystemAthom(name, Self, proc));
    if Ex then
      AMachine.AddAthom(CreateVForthSystemAthom(name + '*', Self, proc));
  end;

begin
  // APP
  AddAthom('GetAppTitle', efGetAppTitle, false);
  AddAthom('SetAppTitle', efSetAppTitle, false);
  AddAthom('AppClose', efAppClose, false);
  AddAthom('AppQuit', efAppQuit, false);
  AddAthom('AppNewDocument', efAppNewDocument, false);
  // DOC
  AddAthom('GetDocsCount', efGetDocsCount, false);
  AddAthom('GetDocFileName', efGetDocFileName, true);
  AddAthom('GetDocTitle', efGetDocTitle, true);
  AddAthom('SetDocTitle', efSetDocTitle, true);
  // Editor
  AddAthom('GetEditorLinesCount', efGetEditorLinesCount, true);
  AddAthom('GetEditorLine', efGetEditorLineText, true);
  AddAthom('SetEditorLine', efSetEditorLineText, true);
  AddAthom('InvalidateEditor', efInvalidateEditor, true);
  AddAthom('InvalidateEditorLine', efInvalidateEditorLine, true);

  AddAthom('AddEditorLine', efAddEditorLine, true);
  AddAthom('AddEditorLineText', efAddEditorLineText, true);
  AddAthom('InsertEditorLine', efInsertEditorLine, true);
  AddAthom('InsertEditorLineText', efInsertEditorLineText, true);
  AddAthom('DeleteEditorLine', efDeleteEditorLine, true);

  AddAthom('GetEditorCaretX', efGetEditorCaretX, true);
  AddAthom('GetEditorCaretY', efGetEditorCaretY, true);
  AddAthom('GetEditorCaretLine', efGetEditorCaretLine, true);
  AddAthom('SetEditorCaretX', efSetEditorCaretX, true);
  AddAthom('SetEditorCaretY', efSetEditorCaretY, true);
  AddAthom('SetEditorCaretLine', efSetEditorCaretLine, true);

  AddAthom('EditorInsert', efEditorInsert, true);
  AddAthom('EditorInsertXY', efEditorInsertXY, true);
  AddAthom('EditorOwerwrite', efEditorOwerwrite, true);
  AddAthom('EditorOwerwriteXY', efEditorOwerwriteXY, true);

  AddAthom('AddEceMenuItem', efAddEceMenuItem, true);
end;
{$ENDIF}

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

initialization

InitCommonControls;

finalization

end.
