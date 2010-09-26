library hexview;

uses
  windows,
  Classes,
  SysUtils,
  Iece in '..\Iece.pas',
  HexViewDoc in 'HexViewDoc.pas';

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
    function GetName: string; stdcall;
    function GetTitle: string; stdcall;
    function CreateDocument(App: IEceApplication; AFileName: string;
      var IDoc: IEceDocument; var ErrResult: string): boolean; stdcall;
  end;

function GetPlugin: IEcePlugin; stdcall;
begin
  Result := TPlugin.Create;
end;

exports GetPlugin;

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
var
  doc: IEceDocument;
  editor: IEceEditor;
begin
  // Тут повесим события на некоторые объекты
  App.RegisterDocument(TDocumentLoader.Create);
end;

procedure TPlugin.UnLoad(App: IEceApplication);
begin
  //
end;

{ TDocumentLoader }

function TDocumentLoader.CreateDocument(App: IEceApplication;
  AFileName: string; var IDoc: IEceDocument; var ErrResult: string): boolean;
var
  l: TStringList;
begin
  IDoc := THexViewDoc.Create(App._GetHandle);
  IDoc._LoadFromFile(AFileName);
  Result := True;
end;

function TDocumentLoader.GetName: string;
begin
  Result := 'HexView';
end;

function TDocumentLoader.GetTitle: string;
begin
  Result := 'Просмотр содержимого в шеснадцатиричном виде.';
end;

begin

end.
