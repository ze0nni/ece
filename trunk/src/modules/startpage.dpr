library startpage;

uses
  windows,
  Iece in '..\Iece.pas',
  StartPageDoc in 'StartPageDoc.pas';

type
  TPlugin = class(TInterfacedObject, IEcePlugin)
  private
    FMsSc: OleVariant;
  public
    constructor Create;
    Destructor Destroy; override;
    function Load(App: IEceApplication): boolean; safecall;
    procedure UnLoad(App: IEceApplication); safecall;
  End;

  TDocumentLoader = class(TInterfacedObject, IEceDocumentLoader)
  public
    function GetName: string; safecall;
    function GetTitle: string; safecall;
    function CreateDocument(App: IEceApplication; AFileName: string;
      var IDoc: IEceDocument; var ErrResult: string): boolean; safecall;
        function CheckDocument(AApp : IEceApplication; AFileName : string) : Boolean; safecall;
  end;

function GetPlugin: IEcePlugin; safecall;
begin
  Result := TPlugin.Create;
end;

exports GetPlugin;

{ TPlugin }

constructor TPlugin.Create;
begin

end;

destructor TPlugin.Destroy;
begin

  inherited;
end;

function TPlugin.Load(App: IEceApplication): boolean;
begin
  App.RegisterDocument(TDocumentLoader.Create);
end;

procedure TPlugin.UnLoad(App: IEceApplication);
begin

end;

{ TDocumentLoader }

function TDocumentLoader.CheckDocument(AApp: IEceApplication;
  AFileName: string): Boolean;
begin
  result := AFileName = 'startpage:start'
end;

function TDocumentLoader.CreateDocument(App: IEceApplication;
  AFileName: string; var IDoc: IEceDocument; var ErrResult: string): boolean;
begin
  IDoc := TStartPageDoc.Create(App._GetHandle);
  Result := true;
end;

function TDocumentLoader.GetName: string;
begin
  Result := 'StartPage';
end;

function TDocumentLoader.GetTitle: string;
begin
  Result := 'Стартовая страница.'
end;

begin

end.
