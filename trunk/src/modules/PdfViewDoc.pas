unit PdfViewDoc;

interface

uses
  windows,
  Iece,
  AcroPDFLib_TLB;

type
  TPdfViewDoc = class(TInterfacedObject, IEceDocument)
  private
    FHandle : HWND;
    FPdf : TAcroPDF;
    FFileName : string;
  protected
    function UseHotkey(ctrl, shift, alt: BOOL; key: Word): BOOL; safecall;
    function _GetHandle: HWND; safecall;
    procedure _BeginUpdate; safecall;
    procedure _EndUpdate; safecall;
    procedure _SetFocus; safecall;
    procedure _KillFocus; safecall;
    function GetFileName: string; safecall;

    procedure _LoadFromFile(Const filename: string); safecall;

    procedure _Show; safecall;
    procedure _Hide; safecall;
    procedure _SetViewRect(left, top, right, bottom : Integer); safecall;
    procedure _SetParent(Parent : HWND); safecall;
  public
    constructor Create(parent: HWND);
    destructor Destroy; override;
  end;

implementation

{ TPdfViewDoc }

constructor TPdfViewDoc.Create(parent: HWND);
var
  rt: Trect;
begin
  FPdf := TAcroPDF.Create(nil);
  FPdf.Visible := false;
  FPdf.Parent := nil;
  FPdf.setShowToolbar(false);
  FPdf.ParentWindow := parent;
  GetClientRect(parent, rt);
  FPdf.SetBounds(rt.left, rt.top, rt.right-rt.left, rt.bottom-1);
  FPdf.Visible := True;
end;

destructor TPdfViewDoc.Destroy;
begin
   FPdf.Free;
  inherited;
end;

function TPdfViewDoc.GetFileName: string;
begin
  Result := FFileName;
end;

function TPdfViewDoc.UseHotkey(ctrl, shift, alt: BOOL; key: Word): BOOL;
begin

end;

procedure TPdfViewDoc._BeginUpdate;
begin

end;

procedure TPdfViewDoc._EndUpdate;
begin

end;

function TPdfViewDoc._GetHandle: HWND;
begin
  FPdf.Handle;
end;

procedure TPdfViewDoc._Hide;
begin
  FPdf.Hide;
end;

procedure TPdfViewDoc._KillFocus;
begin

end;

procedure TPdfViewDoc._LoadFromFile(const filename: string);
begin
  FPdf.LoadFile(filename);
  FFileName := filename;
end;

procedure TPdfViewDoc._SetFocus;
begin
  FPdf.SetFocus;
end;

procedure TPdfViewDoc._SetParent(Parent: HWND);
begin
  FPdf.Parent := nil;
  FPdf.ParentWindow := Parent;
end;

procedure TPdfViewDoc._SetViewRect(left, top, right, bottom: Integer);
begin
  SetWindowPos(FPdf.Handle, 0, left, top, right, bottom, 0);
//  FPdf.Left := left;
//  FPdf.Top := top;
//  FPdf.Width := right - left;
//  FPdf.Height := bottom;
  FPdf.SetBounds(left, top, right-left, bottom-1);
  //FPdf.setViewRect(left, top, right - left, bottom - top);
  FPdf.UpdateControlState;
end;

procedure TPdfViewDoc._Show;
begin
  FPdf.Show;
end;

end.
