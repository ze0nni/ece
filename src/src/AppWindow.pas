unit AppWindow;
{$ifdef fpc}{$mode delphi}{$endif}
interface

uses
	Windows,
	Messages,
	Classes,
	zeError,
	zeWndControls,
  zePages,
	iece,
	DocumentWindow,
	EditorWindow;

type
  PGetPlugin = function : IEcePlugin; safecall;

  TEceAppWindow = class(TzeWndControl, IEceApplication, IDispatch)
  protected
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
    function _GetDocumentsCount : integer; safecall;
    function _GetDocuments(AIndex: Integer; var ADocument: IEceDocument)
      : Integer; safecall;
    procedure _UpdateCaption; safecall;
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
  end;

implementation

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

function TEceAppWindow._GetDocumentsCount: integer;
begin
  Result := FDocuments.Count;
end;

function TEceAppWindow._GetHandle : HWND; safecall;
begin
    result := handle;
end;

procedure TEceAppWindow._UpdateCaption;
begin
  UpdateCaption;
end;

procedure TEceAppWindow.CreateParams(var Param : CreateStruct);
begin
	inherited;
	Param.Style := Param.Style or WS_CLIPCHILDREN;
end;

procedure TEceAppWindow.wmSize(var msg : TWMSize);
var
	rt : Trect;
begin
	inherited;
	if ActiveDocumentWindow = nil then exit;
	GetClientRect(handle, rt);
    SetWindowPos(FPages.Handle, 0, 0, 0, Rt.Right, 24, 0);
	SetWindowPos(ActiveDocumentWindow.handle, 0, 0, 24, rt.Right, rt.Bottom-24, 0);
end;

procedure TEceAppWindow.wmSetFocus(var msg : TWmSetFocus);
begin
	inherited;
	if ActiveDocumentWindow = nil then exit;
	ActiveDocumentWindow.SetFocus;
end;

procedure TEceAppWindow.wmDestroy(var msg : TWMDestroy);
begin
	inherited;
	PostQuitMessage(0);
end;

Constructor TEceAppWindow.Create(AParent : Cardinal);
begin
	inherited;
	FDocuments := TList.Create;
    FPages := TPages.Create(Handle);
    ShowWindow(FPages.Handle, SW_SHOW);
  UpdateCaption;
end;

Destructor TEceAppWindow.Destroy;
begin
    if Assigned(FDocuments) then
    begin
        CloseAllDocuments;
        FDocuments.Free;
    end;
	inherited;
end;

procedure TEceAppWindow.NewDocument(AFileName : String);
var
	NewDocument : TEceDocumentWindow;
begin
	NewDocument := TEceEditorWindow.Create(Handle,  Self);
	FDocuments.Add(NewDocument);
	SendMessage(handle, WM_SIZE, 0, 0);
end;

function TEceAppWindow.CloseDocument(const index : integer) : boolean;
begin
	Documents[index].Free; //тут вылезет эксепшн приневерном индексе
	FDocuments.Delete(index);
	{todo: Нужно еще изменить текущий документ}
end;

function TEceAppWindow.CloseAllDocuments : boolean;
begin
	while DocumentsCount <> 0 do
		CloseDocument(0);
end;

function TEceAppWindow.GetDocumentsCount : integer;
begin
	result := FDocuments.Count;
end;

function TEceAppWindow.LoadPlugin(AFileName: string): boolean;
var
  hPlugin : HMODULE;
  LoadProc : PGetPlugin;
  Plugin : IEcePlugin;
begin
  hPlugin := LoadLibrary(PChar(AFileName));
  if hPlugin = 0 then raise EErr.Create('Не удалось загрузить модуль '+AFileName);

  LoadProc := GetProcAddress(hPlugin, 'GetPlugin');
  if @LoadProc = nil then
  begin
    FreeLibrary(hPlugin);
    raise EErr.Create('GetPlugin не найден в таблице экспорта модуля '+AFileName);
  end;

  Plugin := LoadProc;
  Plugin.Load(Self);
end;

function TEceAppWindow.GetDocuments(const index : integer) : TEceDocumentWindow;
begin
	if (index < 0) or (index > DocumentsCount - 1) then
		raise EErr.Create('Неверный индекс документа');
	result := TEceDocumentWindow(FDocuments[index]);
end;

procedure TEceAppWindow.SetActiveDocument(const value : integer);
begin
	if (FActiveDocument < 0) or (FActiveDocument > DocumentsCount - 1) then
		Documents[FActiveDocument].KillFocus;

	FActiveDocument := Value;
	if (FActiveDocument < 0) or (FActiveDocument > DocumentsCount - 1) then
		Documents[FActiveDocument].SetFocus;

	SendMessage(handle, WM_SIZE, 0, 0);
  UpdateCaption;
end;

procedure TEceAppWindow.UpdateCaption;
var
  Caption : string;
  Title : string;
begin
  Caption := 'Easy code editor';
  if DocumentsCount <> 0 then
  begin
    Title := ActiveDocumentWindow.DocumentTitle;
    if Title = '' then Title := 'New *';
    Caption := Title + ' - ' + Caption;
    FPages.pages[ActiveDocument].title := Title;
  end;
  SetWindowText(Handle, Pchar(Caption))
end;

function TEceAppWindow.GetActiveDocumentWindow : TEceDocumentWindow;
begin
	if (ActiveDocument < 0) or (ActiveDocument > DocumentsCount - 1) then result := nil
	else
	result := Documents[ActiveDocument];
end;

end.


