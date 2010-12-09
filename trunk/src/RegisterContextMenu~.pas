unit RegisterContextMenu;

interface

uses
  Windows, Registry;

procedure RegisterShellMenu;

implementation

uses
  SysUtils, eceshexmenu;

procedure RegisterShellMenu;
var
  dllpath : string;
begin
  dllpath := ExtractFilePath(ParamStr(0)) + 'eceshex.dll';
  try
  if not FileExists(dllpath) then exit;

  with TRegistry.Create do
  try
    RootKey := HKEY_CLASSES_ROOT;
    OpenKey(Format('CLSID\%s\InprocServer32', [GuidToString(Class_EceShEx)]), True);
    WriteString('', dllpath);
    WriteString('ThreadingModel','Apartment');
    CloseKey;
  finally
    Free;
  end;

  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    OpenKey(Format('SOFTWARE\Classes\CLSID\%s\InprocServer32', [GuidToString(Class_EceShEx)]), True);
    WriteString('', dllpath);
    WriteString('ThreadingModel','Apartment');
    CloseKey;
  finally
    Free;
  end;

  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Shell Extensions\Approved', True);
    WriteString(GUIDToString(Class_EceShEx), 'Ece Context Menu Shell Extension');
    CloseKey;
  finally
    Free;
  end;

  with TRegistry.Create do
  try
    RootKey := HKEY_CLASSES_ROOT;
    OpenKey('*\shellex\ContextMenuHandlers\Ece', True);
    WriteString('',GUIDToString(Class_EceShEx));
    CloseKey;
  finally
    Free;
  end;

  with TRegistry.Create do
  try
    RootKey := HKEY_CLASSES_ROOT;
    OpenKey('Folder\shellex\ContextMenuHandlers\Ece', True);
    WriteString('',GUIDToString(Class_EceShEx));
    CloseKey;
  finally
    Free;
  end;
  except

  end;
end;

end.
