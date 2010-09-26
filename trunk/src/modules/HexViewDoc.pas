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
    function UseHotkey(ctrl, shift, alt: BOOL; key: Word): BOOL; stdcall;
    function _GetHandle: HWND; safecall;
    procedure _BeginUpdate; safecall;
    procedure _EndUpdate; safecall;
    procedure _SetFocus; stdcall;
    procedure _KillFocus; stdcall;
    function GetFileName : string; stdcall;

    procedure _LoadFromFile(Const filename : string); stdcall;
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

end.
