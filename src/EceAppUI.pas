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
    FApplication : IEceApplication;
    FItems: TInterfaceList;
    FImageList: HIMAGELIST;
  protected
    function GetWidth: integer; virtual; safecall; abstract;
    function GetHeight: integer; virtual; safecall; abstract;
    procedure ParentResize; virtual; safecall; abstract;

    procedure SetImageList(AImageList: Cardinal); virtual; safecall;
    procedure AddActionItem(AItem: IEceAction; Flags: Word); safecall;
    procedure InsertActionItem(Index: integer; AItem: IEceAction; Flags: Word);
      safecall;
    procedure _InsertItemAct(Index: integer; AItem: IEceAction; Flags: Word);
      virtual; abstract;
  public
    Constructor Create(AApplication : IEceApplication; ParentWindow: hwnd);
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
    FHandle: HMENU;
    FMenus: TStringList;
    function GethMenu(APath: string; var MenuName: string): HMENU;
  protected
    procedure SetImageList(AImageList: Cardinal); override; safecall;
        procedure _InsertItemAct(Index: integer; AItem: IEceAction; Flags: Word); override;
  public
    Constructor Create(AApplication : IEceApplication;ParentWindow: hwnd);
    destructor Destroy; override;
  end;

  TEceToolButton = class;

  TEceToolBar = class(TEceUiContainer, IDispatch, IEceUiConteiner)
  private
    FToolBarWnd: hwnd;
  protected
    function GetWidth: integer; override; safecall;
    function GetHeight: integer; override; safecall;
    procedure ParentResize; override; safecall;
    procedure SetImageList(AImageList: Cardinal); override; safecall;
    procedure _InsertItemAct(Index: integer; AItem: IEceAction; Flags: Word);
      override;
  public
    Constructor Create(AApplication : IEceApplication; ParentWindow: hwnd);
  end;

  TEceToolButton = class(TEceUIItem, IDispatch, IEceUiItem)
  protected
    procedure UpdateState; override; safecall;
  end;

implementation

type
  PMenuItem = ^TMenuItem;

  TMenuItem = record
    Handle: HMENU;
    ID: Word;
    Action: string;
    EnableTest: string;
    VisibleTest: string;
  end;

  { TEceUiContainer }

procedure TEceUiContainer.AddActionItem(AItem: IEceAction; Flags: Word);
begin
  InsertActionItem(FItems.Count, AItem, Flags);
end;

constructor TEceUiContainer.Create(AApplication : IEceApplication; ParentWindow: hwnd);
begin
  inherited Create;
  FApplication := AApplication;
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
  _InsertItemAct(Index, AItem, Flags);
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

constructor TEceToolMenu.Create(AApplication : IEceApplication; ParentWindow: hwnd);
begin
  inherited;
  FHandle := CreateMenu;
  SetMenu(ParentWindow, FHandle);
  FMenus := TStringList.Create;
end;

procedure TEceToolMenu.SetImageList(AImageList: Cardinal);
begin
  inherited;

end;

procedure TEceToolMenu._InsertItemAct(Index: integer; AItem: IEceAction;
  Flags: Word);
var
  MenuAddr: String;
  MenuN : string;
  NewMenu : HMENU;
begin
  MenuAddr := StringReplace(AItem.GetName, '.', '/', [rfReplaceAll]);
  NewMenu := GethMenu(MenuAddr, MenuN);
  AppendMenu(NewMenu, MF_STRING, AItem.GetID, Pchar(AItem.GetText));
end;

destructor TEceToolMenu.Destroy;
begin
  if Assigned(FMenus) then
    FMenus.Free;
  DestroyMenu(FHandle);
  inherited;
end;

function TEceToolMenu.GethMenu(APath: string; var MenuName: string): HMENU;
var
  sl: TStringList;
  i: integer;
  Menu, PopUp: HMENU;
  MenuPath: string;
  index: integer;
  mi: PMenuItem;
  PLocalPath, PLocalString : String;
begin
  sl := TStringList.Create;
  try
    Menu := FHandle;
    sl.Text := StringReplace(APath, '/', #13#10, [rfReplaceAll]);
    PLocalPath := 'Ece.';
    //done: Начинаем со "втророго" слеша
    for i := 1 to sl.Count - 2 do
    begin
      MenuPath := MenuPath + sl[i] + '\';
      PLocalPath := PLocalPath + sl[i];

      index := FMenus.IndexOf(MenuPath);
      if index = -1 then
      begin
        new(mi);

        PopUp := CreatePopupMenu;
        mi^.Handle := PopUp;
        FMenus.AddObject(MenuPath, Tobject(mi));

        PLocalString := FApplication._GetLocalisationString(PLocalPath + '.Text');
        if PLocalString = '' then
          PLocalString := sl[i];
        Windows.AppendMenu(Menu, MF_POPUP, PopUp, PChar(PLocalString));
      end
      else
      begin
        mi := Pointer(FMenus.Objects[index]);
        PopUp := mi^.Handle;
      end;
      Menu := PopUp;
    end;
  finally
    MenuName := sl[sl.Count - 1];
    sl.Free;
    result := PopUp;
  end;
end;

{ TEceToolBar }

constructor TEceToolBar.Create(AApplication : IEceApplication; ParentWindow: hwnd);
begin
  inherited;
  FToolBarWnd := CreateWindowEx(TBSTYLE_EX_DOUBLEBUFFER, TOOLBARCLASSNAME,
    'ToolBar', WS_VISIBLE or WS_CHILD or TBSTYLE_TOOLTIPS OR TBSTYLE_FLAT, 0,
    0, 0, 24, ParentWindow, 0, HInstance, nil);
end;

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

procedure TEceToolBar._InsertItemAct(Index: integer; AItem: IEceAction;
  Flags: Word);
var
  btn: TTBButton;
  Bif: TBBUTTONINFO;
begin
  SendMessage(FToolBarWnd, TB_BUTTONSTRUCTSIZE, SizeOf(btn), 0);
  ZeroMemory(@btn, SizeOf(btn));
  btn.fsStyle := TBSTYLE_BUTTON;
  btn.fsState := TBSTATE_ENABLED;
  btn.idCommand := AItem.GetID;
  btn.iBitmap := AItem.GetImageIndex;

  SendMessage(FToolBarWnd, TB_ADDBUTTONS, 1, integer(@btn));
//  if AItem.GetImageIndex = -1 then
//  SendMessage(FToolBarWnd, TB_ENABLEBUTTON, AItem.GetID, 0);
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

{ TEceToolButton }

procedure TEceToolButton.UpdateState;
begin
  inherited;
  // todo:
end;

end.
