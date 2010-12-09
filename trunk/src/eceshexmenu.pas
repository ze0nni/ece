unit eceshexmenu;

{$ifdef fpc }{$mode delphi}{$ENDIF}

interface

uses
  Windows, ActiveX, ComObj, ShlObj;

const
  Class_EceShEx :TGUID  = '{522AAF53-DEF9-4876-AEF6-FF7041AD28B2}';

type
  TContextMenu = class(TComObject, IShellExtInit, IContextMenu)
  private
    FFileName: array[0..MAX_PATH] of Char;
    TmpFileNames:String;
    FBitmap : HBitmap;
  protected
    constructor Create;
    destructor Destroy; override;
    { IShellExtInit }
    function IShellExtInit.Initialize = SEIInitialize;
    function SEIInitialize(pidlFolder: PItemIDList; lpdobj: IDataObject;
      hKeyProgID: HKEY): HResult; stdcall;
    { IContextMenu }
    function QueryContextMenu(Menu: HMENU; indexMenu, idCmdFirst, idCmdLast,
      uFlags: UINT): HResult; stdcall;
    function InvokeCommand(var lpici: TCMInvokeCommandInfo): HResult; stdcall;
    function GetCommandString(idCmd, uType: UINT; pwReserved: PUINT;
      pszName: LPSTR; cchMax: UINT): HResult; stdcall;
  end;

implementation

uses ComServ, SysUtils, ShellApi, Registry;

type
  TContextMenuFactory = class(TComObjectFactory)
  public
    procedure UpdateRegistry(Register: Boolean); override;
  end;

{ TContextMenu }

constructor TContextMenu.Create;
begin
  inherited;
  FBitmap := LoadBitmap(HInstance, 'Bitmap_1');
end;

destructor TContextMenu.Destroy;
begin
  DeleteObject(FBitmap);
  inherited;
end;

function TContextMenu.GetCommandString(idCmd, uType: UINT; pwReserved: PUINT;
  pszName: LPSTR; cchMax: UINT): HResult;
begin
    Result := S_OK;
  if uType = GCS_HELPTEXT then
    case idCmd of
      0:
      begin
        StrCopy(pszName, '—правочна€ информаци€ по первому пункту меню');
      end
      else
        Result := E_INVALIDARG
    end
end;

function TContextMenu.InvokeCommand(var lpici: TCMInvokeCommandInfo): HResult;
begin
    Result := E_FAIL;
  if (HiWord(Integer(lpici.lpVerb)) <> 0) then Exit;
  Result := NOERROR;
  // ¬ыбор элементов меню идет по возрастающей в том пор€дке
  // в каком они были добавлены
  case LoWord(lpici.lpVerb) of
  0: // первый элемент меню
     // тут собственно и нужно делать реакцию на нажатие ;)
    //MessageBox(lpici.hWnd, PChar(TmpFileNames), 'Pressed', MB_OK);
    ShellExecute(0, 'open', 'ece', PChar(TmpFileNames), nil, SW_NORMAL);
  else
    Result := E_INVALIDARG;
  end;
end;

function TContextMenu.QueryContextMenu(Menu: HMENU; indexMenu, idCmdFirst,
  idCmdLast, uFlags: UINT): HResult;
var
  mif : TMenuItemInfo;
begin
    Result := MakeResult(SEVERITY_SUCCESS, FACILITY_NULL, 0);

  if ((uFlags and $0000000F) = CMF_NORMAL) or
     ((uFlags and CMF_EXPLORE) <> 0) then
  begin
    // –азделитель
    InsertMenu(Menu, indexMenu, MF_SEPARATOR or MF_BYPOSITION, 0, nil);
    // первый пункт меню
    InsertMenu(Menu, indexMenu + 1, MF_STRING or MF_BYPOSITION, idCmdFirst,
      PChar('Open with Ece'));
    ZeroMemory(@mif, Sizeof(mif));
    mif.cbSize := sizeof(mif);
    mif.fMask := MIIM_BITMAP;
    mif.hbmpItem := FBitmap;
    SetMenuItemInfo(menu, indexMenu + 1, false, mif);
    // второй пункт меню
//    InsertMenu(Menu, indexMenu + 2, MF_STRING or MF_BYPOSITION, idCmdFirst + 1,
//      PChar(IDC_TEST2));
    // разделитель
    InsertMenu(Menu, indexMenu + 3, MF_SEPARATOR or MF_BYPOSITION, 0, nil);
    // указываем сколько пунктов меню мы добавили
    // 2 пункта - т.к. разделители не считаютс€
    Result := MakeResult(SEVERITY_SUCCESS, FACILITY_NULL, 2);
  end;
end;

function TContextMenu.SEIInitialize(pidlFolder: PItemIDList;
  lpdobj: IDataObject; hKeyProgID: HKEY): HResult;
var
  StgMedium: TStgMedium;
  FormatEtc: TFormatEtc;
  FilesCount,I:Integer;
begin

  if (lpdobj = nil) then
  begin
    Result := E_INVALIDARG;
    Exit;
  end;

  with FormatEtc do begin
    cfFormat := CF_HDROP;
    ptd      := nil;
    dwAspect := DVASPECT_CONTENT;
    lindex   := -1;
    tymed    := TYMED_HGLOBAL;
  end;

  Result := lpdobj.GetData(FormatEtc, StgMedium);
  if Failed(Result) then Exit;

  TmpFileNames := '';
  FilesCount := DragQueryFile(StgMedium.hGlobal, $FFFFFFFF, nil, 0);
  for I:= 0 to FilesCount - 1 do
  begin
    DragQueryFile(StgMedium.hGlobal, I, FFileName, SizeOf(FFileName));
    TmpFileNames := TmpFileNames + '"'+FFileName+'" ';
  end;
  Result := NOERROR;
  ReleaseStgMedium(StgMedium);
end;

{ TContextMenuFactory }

procedure TContextMenuFactory.UpdateRegistry(Register: Boolean);
var
  ClassID: string;
begin
  if Register then
  begin
    inherited UpdateRegistry(Register);

    ClassID := GUIDToString(Class_EceShEx);
    CreateRegKey('Ece\shellex', '', '');
    CreateRegKey('Ece\shellex\ContextMenuHandlers', '', '');
    CreateRegKey('Ece\shellex\ContextMenuHandlers\ContMenu', '', ClassID);

    if (Win32Platform = VER_PLATFORM_WIN32_NT) then
      with TRegistry.Create do
      try
        RootKey := HKEY_LOCAL_MACHINE;
        OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Shell Extensions', True);
        OpenKey('Approved', True);
        WriteString(ClassID, 'Ece Context Menu Shell Extension');
      finally
        Free;
      end;
  end
  else
  begin
    DeleteRegKey('Ece\shellex\ContextMenuHandlers\ContMenu');
    DeleteRegKey('Ece\shellex\ContextMenuHandlers');
    DeleteRegKey('Ece\shellex');
    inherited UpdateRegistry(Register);
  end;

end;

initialization

TContextMenuFactory.Create(ComServer, TContextMenu, Class_EceShEx,
    '', 'Ece ShellEx', ciMultiInstance,
    tmApartment);

end.
