unit EceAppUI;

interface

uses
  Windows,
  Messages,
  Classes,
  SysUtils,
  ActiveX,
  CommCtrl,
  Variants,
  iece,
  iEceObj;

type
  TEceUiContainer = class(TEceInterfacedObject, IDispatch, IEceUiConteiner)
  private
    FItems: TInterfaceList;
    FImageList : HIMAGELIST;
  protected
    function GetWidth: integer; virtual; safecall; abstract;
    function GetHeight: integer; virtual; safecall; abstract;
    procedure ParentResize; virtual; safecall; abstract;

    procedure SetImageList(AImageList : Cardinal); virtual; safecall;
    procedure AddActionItem(AItem: IEceAction; Flags: Word); safecall;
    procedure InsertActionItem(Index: integer; AItem: IEceAction; Flags: Word);
      safecall;
    procedure InsertItemAct(Index: integer; AItem: IEceAction; Flags: Word);
      virtual; abstract;
  public
    Constructor Create(ParentWindow: hwnd);
    Destructor Destroy; override;
  end;

  TEceUIItem = class(TEceInterfacedObject, IDispatch, IEceUiItem)
  private
    FAction: IEceAction;
    function GetAction: IEceAction; safecall;
    procedure SetAction(const Value: IEceAction); safecall;
  protected
    procedure UpdateState; virtual; safecall;
  public
    property Action: IEceAction read GetAction write SetAction;
  end;

  TEceToolMenu = class(TEceUiContainer, IDispatch, IEceUiConteiner)
  private
  protected
    procedure SetImageList(AImageList : Cardinal); override; safecall;
  public
    Constructor Create(ParentWindow: hwnd);
  end;

  TEceToolBar = class(TEceUiContainer, IDispatch, IEceUiConteiner)
  private
    FToolBarWnd: hwnd;
  protected
    function GetWidth: integer; override; safecall;
    function GetHeight: integer; override; safecall;
    procedure ParentResize; override; safecall;
    procedure SetImageList(AImageList : Cardinal); override; safecall;
    procedure InsertItemAct(Index: integer; AItem: IEceAction; Flags: Word);
      override;
  public
    Constructor Create(ParentWindow: hwnd);
  end;

implementation

{ TEceUiContainer }

procedure TEceUiContainer.AddActionItem(AItem: IEceAction; Flags: Word);
begin
  InsertActionItem(FItems.Count, AItem, Flags);
end;

constructor TEceUiContainer.Create(ParentWindow: hwnd);
begin
  inherited Create;
  FItems := TInterfaceList.Create;
end;

destructor TEceUiContainer.Destroy;
begin
  if Assigned(FItems) then
  begin
    FItems.Free;
  end;
  inherited;
end;

procedure TEceUiContainer.InsertActionItem(Index: integer; AItem: IEceAction;
  Flags: Word);
var
  btn: TTBButton;
begin
  FItems.Insert(Index, AItem);
  InsertItemAct(Index, AItem, Flags);
end;

procedure TEceUiContainer.SetImageList(AImageList: Cardinal);
begin
  FImageList := AImageList;
end;

{ TEceUIItem }

function TEceUIItem.GetAction: IEceAction;
begin
  result := FAction;
end;

procedure TEceUIItem.SetAction(const Value: IEceAction);
begin
  try
    if FAction <> nil then
      FAction.RemoveLink(Self);
  except

  end;
  FAction := Value;
  try
    if FAction <> nil then
      FAction.AddLink(Self);
  except

  end;
end;

procedure TEceUIItem.UpdateState;
begin

end;

{ TEceToolMenu }

constructor TEceToolMenu.Create(ParentWindow: hwnd);
begin
  inherited Create(ParentWindow);
end;

procedure TEceToolMenu.SetImageList(AImageList: Cardinal);
begin
  inherited;

end;

{ TEceToolBar }

constructor TEceToolBar.Create(ParentWindow: hwnd);
begin
  inherited Create(ParentWindow);
  FToolBarWnd := CreateWindowEx(TBSTYLE_EX_DOUBLEBUFFER, TOOLBARCLASSNAME,
    'ToolBar', WS_VISIBLE or WS_CHILD or TBSTYLE_TOOLTIPS OR TBSTYLE_FLAT, 0,
    0, 0, 24, ParentWindow, 0, HInstance, nil);
end;

// procedure TEceAppWindow.AppendMenu(AName, AAction, EnableTest, VisibleTest,
// Image: string);
// var
// hbmp: HBITMAP;
// Menu: HMENU;
// MenuName: string;
// m: HMENU;
// mif: TMenuItemInfo;
// mi: PMenuItem;
// btn: TTBButton;
// begin
// Menu := GethMenu(AName, MenuName);
//
// inc(FMenuID); // Новый индекс
//
// if MenuName <> '-' then
// begin
// Windows.AppendMenu(Menu, MF_STRING, FMenuID, PChar(MenuName));
// hbmp := LoadImage(HInstance, PChar(Image), IMAGE_BITMAP, 0, 0,
// LR_LOADFROMFILE or LR_LOADTRANSPARENT or LR_COLOR);
//
// ClearBitmap(hbmp);
//
// SetMenuItemBitmaps(Menu, FMenuID, MF_BYCOMMAND, hbmp, hbmp);
//
// new(mi);
// mi^.Handle := Menu;
// mi^.ID := FMenuID;
// mi^.Action := AAction;
// mi^.EnableTest := EnableTest;
// mi^.VisibleTest := VisibleTest;
//
// ZeroMemory(@mif, SizeOf(mif));
// mif.cbSize := SizeOf(mif);
// mif.fMask := MIIM_DATA;
// mif.dwItemData := Integer(mi);
//
// SetMenuItemInfo(Menu, FMenuID, false, mif);
//
// SendMessage(FToolBar, TB_BUTTONSTRUCTSIZE, SizeOf(btn), 0);
// ZeroMemory(@btn, SizeOf(btn));
//
// if hbmp <> 0 then
// begin
// ImageList_Add(FImgList, hbmp, 0);
// btn.fsStyle := TBSTYLE_BUTTON;
// btn.fsState := TBSTATE_ENABLED;
// btn.idCommand := FMenuID;
// btn.iBitmap := FImageId * 2;
// SendMessage(FToolBar, TB_ADDBUTTONS, 1, Integer(@btn));
// inc(FImageId);
// end;
// end
// else
// begin
// Windows.AppendMenu(Menu, MF_SEPARATOR, FMenuID, nil);
//
// SendMessage(FToolBar, TB_BUTTONSTRUCTSIZE, SizeOf(btn), 0);
// ZeroMemory(@btn, SizeOf(btn));
// btn.fsStyle := TBSTYLE_SEP;
// SendMessage(FToolBar, TB_ADDBUTTONS, 1, Integer(@btn));
// end;
// m := GetMenu(Handle);
// end;

function TEceToolBar.GetHeight: integer;
var
  rt: Trect;
begin
  GetClientRect(FToolBarWnd, rt);
  result := rt.Bottom;
end;

function TEceToolBar.GetWidth: integer;
var
  rt: Trect;
begin
  GetClientRect(FToolBarWnd, rt);
  result := rt.Right;
end;

procedure TEceToolBar.InsertItemAct(Index: integer; AItem: IEceAction;
  Flags: Word);
var
  btn: TTBButton;
begin
  SendMessage(FToolBarWnd, TB_BUTTONSTRUCTSIZE, SizeOf(btn), 0);

  ZeroMemory(@btn, SizeOf(btn));
  btn.fsStyle := TBSTYLE_BUTTON;
  btn.fsState := TBSTATE_ENABLED;
  btn.idCommand := Integer(Self);
  btn.iBitmap := FItems.Count - 1;
  SendMessage(FToolBarWnd, TB_ADDBUTTONS, 1, integer(@btn));
end;

procedure TEceToolBar.ParentResize;
begin
  SendMessage(FToolBarWnd, TB_AUTOSIZE, 0, 0);
end;

procedure TEceToolBar.SetImageList(AImageList: Cardinal);
begin
  inherited;
    SendMessage(FToolBarWnd, TB_SETIMAGELIST, 0, AImageList);
end;

end.
