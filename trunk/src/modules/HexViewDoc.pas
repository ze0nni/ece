unit HexViewDoc;

interface

uses
  windows,
  Iece;

type
  THexViewDoc = class(TInterfacedObject, IEceDocument)
  private
    FHandle : HWND;
  protected
    function UseHotkey(ctrl, shift, alt: BOOL; key: Word): BOOL; safecall;
    function _GetHandle: HWND; safecall;
    procedure _BeginUpdate; safecall;
    procedure _EndUpdate; safecall;
    procedure _SetFocus; safecall;
    procedure _KillFocus; safecall;
    function GetFileName : string; safecall;

    procedure _LoadFromFile(Const filename : string); safecall;

    procedure _Show; safecall;
    procedure _Hide; safecall;
    procedure _SetViewRect(left, top, right, bottom : Integer); safecall;
    procedure _SetParent(Parent : HWND); safecall;
  public
    constructor Create(parent : HWND);
    destructor Destroy; override;
  end;

implementation

{ THexViewDoc }

constructor THexViewDoc.Create(parent : HWND);
begin
  inherited Create;
  FHandle := CreateWindowEx(WS_EX_CLIENTEDGE, 'Edit', '', WS_VISIBLE or WS_CHILD or ES_MULTILINE or WS_VSCROLL,
    0, 0, 0, 0, Parent, 0, HInstance, nil);
end;

destructor THexViewDoc.Destroy;
begin
  DestroyWindow(FHandle);
  FHandle := 0;
  inherited;
end;

function THexViewDoc.GetFileName: string;
begin

end;

function THexViewDoc.UseHotkey(ctrl, shift, alt: BOOL; key: Word): BOOL;
begin
  Result := False;
end;

procedure THexViewDoc._BeginUpdate;
begin

end;

procedure THexViewDoc._EndUpdate;
begin

end;

function THexViewDoc._GetHandle: HWND;
begin
  Result := FHandle;
end;

procedure THexViewDoc._Hide;
begin
  ShowWindow(FHandle, SW_HIDE)
end;

procedure THexViewDoc._KillFocus;
begin

end;

procedure THexViewDoc._LoadFromFile(const filename: string);
begin

end;

procedure THexViewDoc._SetFocus;
begin
  SetFocus(FHandle)
end;

procedure THexViewDoc._SetParent(Parent: HWND);
begin
  SetParent(FHandle, Parent)
end;

procedure THexViewDoc._SetViewRect(left, top, right, bottom: Integer);
begin
  SetWindowPos(FHandle, 0, left, top, right - left, bottom - top, 0)
end;

procedure THexViewDoc._Show;
begin
  ShowWindow(FHandle, SW_SHOW)
end;

end.
