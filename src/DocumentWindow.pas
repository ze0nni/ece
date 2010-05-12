unit DocumentWindow;
{$ifdef fpc}{$mode delphi}{$endif}
interface

uses
	Windows,
	Messages,
    IEce,
	Classes,
	zeWndControls;

type
    TEceDocumentState = (dsReady, dsLoading, dsSaving);

    TEceDocumentWindow = class(TzeWndControl, IEceDocument)
    private
      FCsChangeState: TRTLCriticalSection;
      FState: TEceDocumentState;
      procedure SetDocumentState(const value: TEceDocumentState);
      function GetDocumentState: TEceDocumentState;
    protected
      function _GetHandle: HWND; safecall;
    public
      Constructor Create(Parent: Cardinal);
      Destructor Destroy; override;

      procedure LoadFromFile(AFileName: String); virtual;
      procedure SaveToFile(AFileName: string); virtual;
      function Close: boolean; virtual;
      property DocumentState
        : TEceDocumentState read GetDocumentState write SetDocumentState;
      property Vscroll;
      property HScroll;
    end;

implementation

Constructor TEceDocumentWindow.Create(Parent : Cardinal);
begin
    inherited;
    InitializeCriticalSection(FCsChangeState);
end;

Destructor TEceDocumentWindow.Destroy;
begin
    DeleteCriticalSection(FCsChangeState);
    inherited;
end;

function TEceDocumentWindow.Close : boolean;
begin
	result := true;
end;

procedure TEceDocumentWindow.LoadFromFile(AFileName : String);
begin

end;

procedure TEceDocumentWindow.SaveToFile(AFileName : string);
begin

end;

function TEceDocumentWindow.GetDocumentState : TEceDocumentState;
begin
    EnterCriticalSection(FCsChangeState);
    result := FState;
    LeaveCriticalSection(FCsChangeState)
end;

procedure TEceDocumentWindow.SetDocumentState(const value : TEceDocumentState);
begin
    EnterCriticalSection(FCsChangeState);
    FState := value;
    LeaveCriticalSection(FCsChangeState)
end;

function TEceDocumentWindow._GetHandle: HWND;
begin
  Result := Handle;
end;

end.

