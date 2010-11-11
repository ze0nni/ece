unit StartPageDoc;

interface

uses
  windows,
  ComObj,
  ActiveX,
  Iece;

const
  CLASS_InternetExplorer: TGUID = '{0002DF01-0000-0000-C000-000000000046}';

type
  TStartPageDoc = class(TInterfacedObject, IEceDocument)
  private
    FIE : OleVariant;
  protected
    function UseHotkey(ctrl, shift, alt: BOOL; key: Word): BOOL; stdcall;
    function _GetHandle: HWND; safecall;
    procedure _BeginUpdate; safecall;
    procedure _EndUpdate; safecall;
    procedure _SetFocus; stdcall;
    procedure _KillFocus; stdcall;
    function GetFileName: string; stdcall;

    procedure _LoadFromFile(Const filename: string); stdcall;

    procedure _Show; stdcall;
    procedure _Hide; stdcall;
    procedure _SetViewRect(left, top, right, bottom : Integer); stdcall;
    procedure _SetParent(Parent : HWND); stdcall;
  public
    constructor Create(Parent : HWND);
    destructor Destroy; override;
  end;

implementation

{ TStartPageDoc }

constructor TStartPageDoc.Create(Parent : HWND);
begin
  inherited Create;
  FIE := CreateOleObject('InternetExplorer.Application');
  FIE.Visible := True;
  SetParent(FIE.HWND, Parent);
end;

destructor TStartPageDoc.Destroy;
begin
  FIE.quit;
  FIE  := 0;
  inherited;
end;

function TStartPageDoc.GetFileName: string;
begin

end;

function TStartPageDoc.UseHotkey(ctrl, shift, alt: BOOL; key: Word): BOOL;
begin

end;

procedure TStartPageDoc._BeginUpdate;
begin

end;

procedure TStartPageDoc._EndUpdate;
begin

end;

function TStartPageDoc._GetHandle: HWND;
begin
  Result := FIE.HWnd;
end;

procedure TStartPageDoc._Hide;
begin

end;

procedure TStartPageDoc._KillFocus;
begin

end;

procedure TStartPageDoc._LoadFromFile(const filename: string);
begin

end;

procedure TStartPageDoc._SetFocus;
begin

end;

procedure TStartPageDoc._SetParent(Parent: HWND);
begin

end;

procedure TStartPageDoc._SetViewRect(left, top, right, bottom: Integer);
begin

end;

procedure TStartPageDoc._Show;
begin

end;

initialization

CoInitializeEx(nil, 0);

finalization

CoUninitialize;

end.
