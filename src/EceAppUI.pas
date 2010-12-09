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
  protected
  public
    Constructor Create;
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
  public
  end;

  TEceToolBar = class(TEceUiContainer, IDispatch, IEceUiConteiner)

  end;

implementation

{ TEceUiContainer }

constructor TEceUiContainer.Create;
begin
  inherited;
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

end.
