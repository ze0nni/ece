unit VForthModuleDialogs;
{$IFDEF fpc}{$MODE delphi}{$ENDIF}

interface

uses
  VForthModule,
  Windows,
  SysUtils,
  ShlObj,
  ComObj,
  ActiveX,
  ShellAPi,
  VForth,
  CommDlg;

type

  EVForthModuleDialogsError = class(Exception)

  end;

  TVForthModuleDialogs = class(TVForthModule, IVForthModule)
  private

  protected

  public
    procedure Register(AMachine: IVForthMachine); stdcall;
  end;

implementation

{$IFDEF fpc}
{$ENDIF}

uses
  AppWindow,
  VForthAthom;

{ TVForthModuleDialogs }

procedure VfdlgMsgBox(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  MessageBox(glApp.Handle, Pchar(AMachine.PopString), Pchar('Easy Code Editor')
      , 0);
end;

procedure VfdlgMsgBoxEx(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  title, msg, flags, res: string;
  sType: Integer;
begin
  msg := AMachine.PopString;
  title := AMachine.PopString;
  flags := AMachine.PopString;

  sType := 0;

  if pos('OkCancel', flags) <> 0 then
    sType := sType or MB_OKCANCEL
  else if pos('Ok', flags) <> 0 then
    sType := sType or MB_OK
  else if pos('AbortRetryIgnore', flags) <> 0 then
    sType := sType or MB_ABORTRETRYIGNORE
  else if pos('YesNoCancel', flags) <> 0 then
    sType := sType or MB_YESNOCANCEL
  else if pos('YesNo', flags) <> 0 then
    sType := sType or MB_YESNO
  else if pos('RetryCancel', flags) <> 0 then
    sType := sType or MB_RETRYCANCEL;

  if pos('Hand', flags) <> 0 then
    sType := sType or MB_ICONHAND
  else if pos('Question', flags) <> 0 then
    sType := sType or MB_ICONQUESTION
  else if pos('Clamation', flags) <> 0 then
    sType := sType or MB_ICONEXCLAMATION
  else if pos('Asterisk', flags) <> 0 then
    sType := sType or MB_ICONASTERISK
  else if pos('Warning', flags) <> 0 then
    sType := sType or MB_ICONWARNING
  else if pos('Error', flags) <> 0 then
    sType := sType or MB_ICONERROR
  else if pos('Information', flags) <> 0 then
    sType := sType or MB_ICONINFORMATION
  else if pos('Stop', flags) <> 0 then
    sType := sType or MB_ICONSTOP;

  case MessageBox(glApp.Handle, Pchar(msg), Pchar(title), sType) of
    IDOK:
      res := 'ok';
    IDCANCEL:
      res := 'cancel';
    IDABORT:
      res := 'abort';
    IDRETRY:
      res := 'retry';
    IDIGNORE:
      res := 'ignore';
    IDYES:
      res := 'yes';
    IDNO:
      res := 'no';
    IDCLOSE:
      res := 'close';
    IDHELP:
      res := 'help';
  end;
  AMachine.PushString(res);
end;

procedure VfdlgInputBox(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin

end;

procedure BrowseCallbackProc(wnd: hWnd; uMsg: UINT; lParam, lpData: lParam)
  stdcall;
begin
  SendMessage(wnd, BFFM_ENABLEOK, 0, 1);
end;

function SelectDirectory(const Caption: string; const Root: WideString;
  var Directory: string): Boolean;
var
  WindowList: Pointer;
  BrowseInfo: TBrowseInfo;
  Buffer: Pchar;
  RootItemIDList, ItemIDList: PItemIDList;
  ShellMalloc: IMalloc;
  IDesktopFolder: IShellFolder;
  Eaten, flags: LongWord;
begin
{$IFNDEF fpc}
  // myDir := Directory;
  Result := False;
  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  if (ShGetMalloc(ShellMalloc) = S_OK) and (ShellMalloc <> nil) then
  begin
    Buffer := ShellMalloc.Alloc(MAX_PATH);
    try
      RootItemIDList := nil;
      if Root <> '' then
      begin
        SHGetDesktopFolder(IDesktopFolder);
        IDesktopFolder.ParseDisplayName(glApp.Handle, nil, POleStr(Root),
          Eaten, RootItemIDList, flags);
      end;
      with BrowseInfo do
      begin
        hwndOwner := glApp.Handle;
        pidlRoot := RootItemIDList;
        pszDisplayName := Buffer;
        lpfn := @BrowseCallbackProc;
        lParam := Integer(Pchar(Directory));
        lpszTitle := Pchar(Caption);
        ulFlags :=
          BIF_RETURNONLYFSDIRS or $0040 or BIF_EDITBOX or BIF_STATUSTEXT;
      end;
      // WindowList := DisableTaskWindows(0);
      try
        ItemIDList := ShBrowseForFolder(BrowseInfo);
      finally
        // EnableTaskWindows(WindowList);

      end;
      Result := ItemIDList <> nil;
      if Result then
      begin
        ShGetPathFromIDList(ItemIDList, Buffer);
        ShellMalloc.Free(ItemIDList);
        Directory := Buffer;
      end;
    finally
      ShellMalloc.Free(Buffer);
    end;
  end;
{$ENDIF}
end;

function GetSpecialFolderPath(folder: Integer): string;
const
  SHGFP_TYPE_CURRENT = 0;
var
  path: array [0 .. MAX_PATH] of char;
begin
  if SUCCEEDED(SHGetFolderPath(0, folder, 0, SHGFP_TYPE_CURRENT, @path[0])) then
    Result := path
  else
    Result := '';
end;

procedure VfdlgShFolder(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  sRoot: string;
  Root: Integer;
begin
  sRoot := LowerCase(AMachine.PopString);
{$REGION 'CSIDL_'}
  if sRoot = 'desktop' then
    Root := CSIDL_DESKTOP
  else if sRoot = 'programs' then
    Root := CSIDL_PROGRAMS
  else if sRoot = 'personal' then
    Root := CSIDL_PERSONAL
  else if sRoot = 'favorites' then
    Root := CSIDL_FAVORITES
  else if sRoot = 'sturtup' then
    Root := CSIDL_STARTUP
  else if sRoot = 'resent' then
    Root := CSIDL_RECENT
  else if sRoot = 'bitbucket' then
    Root := CSIDL_BITBUCKET
  else if sRoot = 'startmenu' then
    Root := CSIDL_STARTMENU
  else if sRoot = 'mydocuments' then
    Root := CSIDL_MYDOCUMENTS
  else if sRoot = 'mymusic' then
    Root := CSIDL_MYMUSIC
  else if sRoot = 'myvideo' then
    Root := CSIDL_MYVIDEO
  else if sRoot = 'desktopdirectory' then
    Root := CSIDL_DESKTOPDIRECTORY
  else if sRoot = 'drivers' then
    Root := CSIDL_DRIVES
  else if sRoot = 'templates' then
    Root := CSIDL_TEMPLATES
  else if sRoot = 'common_startmenu' then
    Root := CSIDL_COMMON_STARTMENU
  else if sRoot = 'common_programs' then
    Root := CSIDL_COMMON_PROGRAMS
  else if sRoot = 'common_startup' then
    Root := CSIDL_COMMON_STARTUP
  else if sRoot = 'common_desktopdirectory' then
    Root := CSIDL_COMMON_DESKTOPDIRECTORY
  else if sRoot = 'appdata' then
    Root := CSIDL_APPDATA
  else if sRoot = 'local_appdata' then
    Root := CSIDL_LOCAL_APPDATA
  else if sRoot = 'allstartup' then
    Root := CSIDL_ALTSTARTUP
  else if sRoot = 'common_allstartup' then
    Root := CSIDL_COMMON_ALTSTARTUP
  else if sRoot = 'common_favorites' then
    Root := CSIDL_COMMON_FAVORITES
  else if sRoot = 'internet_cache' then
    Root := CSIDL_INTERNET_CACHE
  else if sRoot = 'cookies' then
    Root := CSIDL_COOKIES
  else if sRoot = 'history' then
    Root := CSIDL_HISTORY
  else if sRoot = 'common_appdata' then
    Root := CSIDL_COMMON_APPDATA
  else if sRoot = 'windows' then
    Root := CSIDL_WINDOWS
  else if sRoot = 'system' then
    Root := CSIDL_SYSTEM
  else if sRoot = 'program_files' then
    Root := CSIDL_PROGRAM_FILES
  else if sRoot = 'mypictures' then
    Root := CSIDL_MYPICTURES
  else if sRoot = 'profile' then
    Root := CSIDL_PROFILE
  else if sRoot = 'systemx86' then
    Root := CSIDL_SYSTEMX86
  else if sRoot = 'program_filesx86' then
    Root := CSIDL_PROGRAM_FILESX86
  else if sRoot = 'program_files_common' then
    Root := CSIDL_PROGRAM_FILES_COMMON
  else if sRoot = 'program_files_commonx86' then
    Root := CSIDL_PROGRAM_FILES_COMMONX86
  else if sRoot = 'common_templates' then
    Root := CSIDL_COMMON_TEMPLATES
  else if sRoot = 'commond_documents' then
    Root := CSIDL_COMMON_DOCUMENTS
  else if sRoot = 'common_admintools' then
    Root := CSIDL_COMMON_ADMINTOOLS
  else if sRoot = 'admintools' then
    Root := CSIDL_ADMINTOOLS
  else if sRoot = 'common_music' then
    Root := CSIDL_COMMON_MUSIC
  else if sRoot = 'common_puctures' then
    Root := CSIDL_COMMON_PICTURES
  else if sRoot = 'common_video' then
    Root := CSIDL_COMMON_VIDEO
  else if sRoot = 'resources' then
    Root := CSIDL_RESOURCES
  else if sRoot = 'resources_localized' then
    Root := CSIDL_RESOURCES_LOCALIZED;
{$ENDREGION}
  AMachine.PushString(GetSpecialFolderPath(Root));
end;

procedure VfdlgBrowseFolder(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  bi: TBrowseInfo;
  sFolder: String;
  PIDL, ResPIDL: PItemIDList;
  sRoot: string;
  Root: Integer;
begin
  sRoot := AMachine.PopString;
  sFolder := AMachine.PopString;

  SelectDirectory('', sRoot, sFolder);

  AMachine.PushString(sFolder);
end;

procedure VfdlgFileOpen(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  ofn: {$IFDEF fpc} LPOPENFILENAME {$ELSE} TOpenFilename {$ENDIF};
  sFile: array [0 .. MAX_PATH] of char;
begin
  ZeroMemory(@ofn, SizeOf(ofn));
  ZeroMemory(@sFile, SizeOf(sFile));
  ofn.lStructSize := SizeOf(ofn);

  ofn.hwndOwner := glApp.Handle;
  ofn.hInstance := hInstance;
  ofn.lpstrFilter := '*.*'#0'*.*'#0#0;
  ofn.lpstrTitle := 'Open';
  ofn.lpstrFile := sFile;
  ofn.nMaxFile := SizeOf(sFile);
  ofn.flags := OFN_EXPLORER or OFN_CREATEPROMPT or OFN_FILEMUSTEXIST;

  if GetOpenFileName(ofn) then
    AMachine.PushString(sFile)
  else
    AMachine.PushString('');
end;

procedure TVForthModuleDialogs.Register(AMachine: IVForthMachine);
begin
  AMachine.AddAthom(CreateVForthSystemAthom('MsgBox', Self, VfdlgMsgBox));
  AMachine.AddAthom(CreateVForthSystemAthom('MsgBoxEx', Self, VfdlgMsgBoxEx));
  AMachine.AddAthom(CreateVForthSystemAthom('InputBox', Self, VfdlgInputBox));
  AMachine.AddAthom(CreateVForthSystemAthom('ShFolder', Self, VfdlgShFolder));
  AMachine.AddAthom(CreateVForthSystemAthom('BrowseFolder', Self,
      VfdlgBrowseFolder));
  AMachine.AddAthom(CreateVForthSystemAthom('FileOpenDlg', Self, VfdlgFileOpen)
    );
end;

end.
