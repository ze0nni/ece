library autospace;

uses
    windows,
    IEce in '.\..\Iece.pas';

type
  TPlugin = Class(TInterfacedObject, IEcePlugin)
  private
    FMsSc : OleVariant;
  public
    constructor Create;
    Destructor Destroy; override;
    function Load(App : IEceApplication) : boolean; safecall;
  End;

function GetPlugin : IEcePlugin; stdcall;
begin
  Result := TPlugin.Create;
end;

exports
	GetPlugin;

{ TPlugin }

constructor TPlugin.Create;
begin
  inherited;
end;

destructor TPlugin.Destroy;
begin

  inherited;
end;

function TPlugin.Load(App: IEceApplication): boolean;
begin
  //��� ������� ������� �� ��������� �������
end;

begin

end.
