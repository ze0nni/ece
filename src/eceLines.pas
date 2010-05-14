unit eceLines;

interface

uses
  Windows,
  Messages,

  iece;

type
  TEceLine = Class(TInterfacedObject, IEceLine)
  private
    Iself : IEceLine;
    FText: string;
    function GetText : string; stdcall;
    procedure SetText(value : string); stdcall;
  public
    constructor Create(ADocument : IEceDocument);
    destructor Destroy; override;
    property Text : string read FText write SetText;
  end;

  TEceLines = class
  private
    function GetCount: integer;

  public
    property Count : integer read GetCount;
  end;
implementation

{ TEceLine }

constructor TEceLine.Create(ADocument: IEceDocument);
begin
  inherited Create;
  ISelf := Self;
end;

destructor TEceLine.Destroy;
begin

  inherited;
end;

function TEceLine.GetText: string;
begin
  Result := FText;
end;

procedure TEceLine.SetText(value: string);
begin
  {TODO :  }
  FText := value;
end;

{ TEceLines }

function TEceLines.GetCount: integer;
begin
  {TODO :  }
end;

end.
