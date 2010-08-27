unit MsAsKernel;

interface

uses
  Windows,
  SysUtils,
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
  try
  FKernal.AddCode(code);
  except
    on E : Exception do
    if FKernal.Error.Number <> 0 then
     raise Exception.Create(Format('Ошибка %d (строка %d символ %d)'#13#10'%s - %s', [
      FKernal.Error.Number,
      FKernal.Error.Line,
      FKernal.Error.Column,
      FKernal.Error.Source,
      FKernal.Error.Description]))
    else
      raise Exception.Create(e.Message);
  end;
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
