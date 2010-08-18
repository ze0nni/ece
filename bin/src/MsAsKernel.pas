unit MsAsKernel;

interface

uses
  Windows,
  ActiveX,
  ComObj,
  MSScriptControl_TLB;

type
  TKernel = class(TInterfacedObject)
  private
    FKernal : IScriptControl;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddObject(AName : string; IObject : IDispatch);
    procedure AddCode(const code : string);
  end;

implementation

{ TKernel }

procedure TKernel.AddCode(const code: string);
begin
  FKernal.AddCode(code);
end;

procedure TKernel.AddObject(AName: string; IObject: IDispatch);
begin
  FKernal.AddObject(AName, IObject, true);
end;

constructor TKernel.Create;
begin
  inherited;
  FKernal := CreateComObject(CLASS_ScriptControl) as IScriptControl;
  FKernal.Language := 'VBScript';
end;

destructor TKernel.Destroy;
begin
  FKernal := nil;
  inherited;
end;

initialization
  CoInitialize(0)
finalization
  CoUninitialize;
end.
