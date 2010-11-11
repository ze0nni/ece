unit DocumentWindow;
{$IFDEF fpc}{$MODE delphi}{$ENDIF}

interface

uses
  Windows,
  Messages,
  IEce,
  Classes,
  ShlObj, ActiveX,
  zeWndControls;

type
  TEceDocumentState = (dsReady, dsLoading, dsSaving);

  TEceDocumentWindow = class(TzeWndControl, IEceDocument, IDispatch)
  private
    FCsChangeState: TRTLCriticalSection;
    FState: TEceDocumentState;
    FFileName: string;
    FApplication: IEceApplication;
    procedure SetDocumentState(const value: TEceDocumentState);
    function GetDocumentState: TEceDocumentState;
  protected
    function _GetHandle: HWND; safecall;
  protected
    procedure _BeginUpdate; virtual; safecall;
    procedure _EndUpdate; virtual; safecall;
    procedure _SetFocus; virtual; stdcall;
    procedure _KillFocus; virtual; stdcall;
    function GetDocumentFileName: string; virtual;
    function GetDocumentTitle: string; virtual; stdcall;
    function GetFileName : string; virtual; stdcall;
    procedure _LoadFromFile(Const filename : string);virtual; stdcall;
    procedure _Show;virtual; stdcall;
    procedure _Hide;virtual; stdcall;
    procedure _SetViewRect(left, top, right, bottom : Integer);virtual; stdcall;
    procedure _SetParent(Parent : HWND);virtual; stdcall;
  public
    Constructor Create(Parent: Cardinal; AApplication: IEceApplication);
    Destructor Destroy; override;

    function UseHotkey(ctrl, shift, alt: BOOL; key: Word): BOOL; virtual;
      stdcall;
    property DocumentTitle: string read GetDocumentTitle;
    property DocumentFileName: string read GetDocumentFileName;
    procedure LoadFromFile(AFileName: String); virtual;
    procedure SaveToFile(AFileName: string); virtual;
    function Close: boolean; virtual;
    property DocumentState: TEceDocumentState read GetDocumentState write
      SetDocumentState;
    property Vscroll;
    property HScroll;
    property Application: IEceApplication read FApplication;
    property FileName: string read FFileName;
  end;

implementation

Constructor TEceDocumentWindow.Create(Parent: Cardinal;
  AApplication: IEceApplication);
begin
  inherited Create(Parent);
  FApplication := AApplication;
  InitializeCriticalSection(FCsChangeState);
end;

Destructor TEceDocumentWindow.Destroy;
begin
  DeleteCriticalSection(FCsChangeState);
  inherited;
end;

function TEceDocumentWindow.Close: boolean;
begin
  result := true;
end;

procedure TEceDocumentWindow.LoadFromFile(AFileName: String);
begin
  FFileName := AFileName;
end;

procedure TEceDocumentWindow.SaveToFile(AFileName: string);
begin
  FFileName := AFileName;
end;

function TEceDocumentWindow.GetDocumentFileName: string;
begin

end;

function TEceDocumentWindow.GetDocumentState: TEceDocumentState;
begin
  EnterCriticalSection(FCsChangeState);
  result := FState;
  LeaveCriticalSection(FCsChangeState)
end;

function TEceDocumentWindow.GetDocumentTitle: string;
begin

end;

function TEceDocumentWindow.GetFileName: string;
begin
  Result := '';
end;

procedure TEceDocumentWindow.SetDocumentState(const value: TEceDocumentState);
begin
  EnterCriticalSection(FCsChangeState);
  FState := value;
  LeaveCriticalSection(FCsChangeState)
end;

function TEceDocumentWindow.UseHotkey(ctrl, shift, alt: BOOL; key: Word): BOOL;
begin
  result := False;
end;

procedure TEceDocumentWindow._BeginUpdate;
begin

end;

procedure TEceDocumentWindow._EndUpdate;
begin

end;

function TEceDocumentWindow._GetHandle: HWND;
begin
  result := Handle;
end;

procedure TEceDocumentWindow._Hide;
begin
  ShowWindow(Handle, SW_HIDE)
end;

procedure TEceDocumentWindow._KillFocus;
begin

end;

procedure TEceDocumentWindow._LoadFromFile(const filename: string);
begin

end;

procedure TEceDocumentWindow._SetFocus;
begin

end;

procedure TEceDocumentWindow._SetParent(Parent: HWND);
begin
  SetParent(Handle, Parent);
end;

procedure TEceDocumentWindow._SetViewRect(left, top, right, bottom: Integer);
begin
  SetWindowPos(Handle, 0, left, top, right - left, bottom - left, 0);
end;

procedure TEceDocumentWindow._Show;
begin
  ShowWindow(Handle, SW_SHOW)
end;

end.
