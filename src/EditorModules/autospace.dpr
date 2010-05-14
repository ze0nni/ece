library autospace;

uses
    windows,
    IEce in '.\..\Iece.pas';

type
  TPlugin = Class(TInterfacedObject, IEceEditorPlugin)
  private
    FMsSc : OleVariant;
    FEditor : IEceEditor;
  public
    constructor Create;
    Destructor Destroy; override;
    function Load(Editor :IEceEditor) : boolean; safecall;
  End;

function GetPlugin : IEceEditorPlugin; stdcall;
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

function  TPlugin.Load(Editor : IEceEditor) : boolean; safecall;
var i : integer;
    text : string;
begin
  //Тут повесим события на некоторые объекты
  FEditor := Editor;

//  for i := FEditor._GetLinesCount - 1 downto 1 do
//  with FEditor._GetLines(i) do
//  begin
//    text := _GetText;
//    if text = '' then
//      FEditor._DeleteLine(i);
//  end;
end;

begin

end.
