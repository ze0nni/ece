unit VForthMachineApi;

interface

uses
  Windows,
  SysUtils,
  VForthModule,
  VForth;

type
  EVForthModuleApiError = class(Exception)

  end;

  TVForthModuleApi = class(TVForthModule, IVForthModule)
  private

  protected

  public
    procedure Register(AMachine: IVForthMachine); stdcall;
  end;

implementation

uses
  Classes,
  VForthAthom;

const
  pVoid = 0;
  pInt32 = 1;
  pCharA = 2;
  pCharW = 3;

type
  PVForthApiAthomProc = procedure(AMachine: IVForthMachine;
    AAthom: IVForthAthom); stdcall;

  TVForthApiAthom = class(TInterfacedObject, IVForthAthom)
  private
    FParams : TList;
    FName: string;
    FModule: IVForthModule;
    FProcedure: Pointer;
    HModule : HMODULE;
    function GetName: String; stdcall;
    function GetModule: IVForthModule; stdcall;
  public
    property Name: string read GetName;
    property Module: IVForthModule read GetModule;
    procedure Execute(AMachine: IVForthMachine); stdcall;

    constructor Create;
    destructor Destroy; override;
  end;

{ TVForthModuleApi }

//declare user32.dll ShowWindow void i32 i32

procedure VfDeclare (AMachine: IVForthMachine; AAthom: IVForthAthom); stdcall;
var
  i: Integer;
  a : TVForthApiAthom;
  SModule : string;
  SFunction : string;
  tk: string;

begin
  a := TVForthApiAthom.Create;
  i := AMachine.CourientTkIndex;
  inc(i);
  SModule := AMachine.GetTk(i);
  a.HModule := LoadLibrary(Pchar(SModule));
  if a.HModule = 0 then
    begin
      a.Free;
      raise EVForthModuleApiError.CreateFmt('Can''t load libraty "%s"', [SModule]);
    end;
  inc(i);
  SFunction := AMachine.GetTk(i);
  a.fName := SFunction;
  a.FProcedure := GetProcAddress(a.HModule, Pchar(SFunction));
  if a.FProcedure = nil then
    begin
      a.Free;
      raise EVForthModuleApiError.CreateFmt('Can''t get function %s from "%s"', [SFunction, SModule]);
    end;
  //»щем параметры
    while i < AMachine.TkCount - 1 do
    begin
      inc(i);
      tk := AMachine.GetTk(i);
      if tk = 'void' then
        a.FParams.Add(TObject(pVoid)) else
      if tk = 'in32' then
        a.FParams.Add(TObject(pInt32)) else
      if tk = 'chara' then
        a.FParams.Add(TObject(pCharA)) else
      if tk = 'charw' then
        a.FParams.Add(TObject(pCharW)) else
      raise EVForthModuleApiError.CreateFmt('Bad api type %s', [tk]);
    end;
    if a.FParams.Count = 0 then
      a.FParams.Add(TObject(pVoid));
    AMachine.AddAthom(a);
end;

procedure TVForthModuleApi.Register(AMachine: IVForthMachine);
begin
  AMachine.AddAthom(CreateVForthSystemAthom('Declare', Self, VfDeclare));
end;

{ TVForthApiAthom }

constructor TVForthApiAthom.Create;
begin
  inherited;
  FParams := TList.Create;
end;

destructor TVForthApiAthom.Destroy;
begin
  if Assigned(FParams) then
    FParams.Free;
  FreeLibrary(HModule);
  inherited;
end;

procedure TVForthApiAthom.Execute(AMachine: IVForthMachine);
begin

end;

function TVForthApiAthom.GetModule: IVForthModule;
begin
  Result := FModule;
end;

function TVForthApiAthom.GetName: String;
begin

end;

end.
