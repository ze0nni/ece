library VForthObject;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  Windows,
  SysUtils,
  // ComServ,
  ComObj,
  ActiveX,
  VForth in 'VForth.pas',
  VForthMachine in 'VForthMachine.pas',
  VForthVariants in 'VForthVariants.pas',
  VForthVariantInteger in 'VForthVariantInteger.pas',
  VForthVariantFloat in 'VForthVariantFloat.pas',
  VForthVariantNatural in 'VForthVariantNatural.pas',
  VForthVariantComplex in 'VForthVariantComplex.pas',
  VForthVariantString in 'VForthVariantString.pas',
  VForthModuleSystem in 'VForthModuleSystem.pas',
  VForthAthom in 'VForthAthom.pas',
  VForthModuleIo in 'VForthModuleIo.pas',
  VForthModuleMath in 'VForthModuleMath.pas',
  VForthModuleLogic in 'VForthModuleLogic.pas',
  VForthModule in 'VForthModule.pas',
  VForthModuleDateTime in 'VForthModuleDateTime.pas',
  AutoObject in 'AutoObject.pas';

function DllRegisterServer: HResult;
begin
  Result := S_OK;
end;

function DllUnregisterServer: HResult;
begin
  Result := S_OK;
end;

function DllGetClassObject(const CLSID, IID: TGUID; var Obj): HResult;
var
  Factory: TComObjectFactory;
begin
  Factory := ComClassManager.GetFactoryFromClassID(CLSID);
  if Factory <> nil then
    if Factory.GetInterface(IID, Obj) then
      Result := S_OK
    else
      Result := E_NOINTERFACE
    else
    begin
      Pointer(Obj) := nil;
      Result := CLASS_E_CLASSNOTAVAILABLE;
    end;
end;

function DllCanUnloadNow: HResult;
begin
  Result := S_OK;
end;

function CreateVForthMachine : IVForthMachine; stdcall;
begin
  result := TVForthMachine.Create;
end;

exports DllRegisterServer, DllUnregisterServer, DllGetClassObject,
  DllCanUnloadNow, CreateVForthMachine;

{ TSortAutoObjectFactory }

begin

  // TAutoObjectFactory.Create(ComServer, TVForthMachine, IID_VForthMachine,
  // ciMultiInstance, tmBoth);
  // TAutoObjectFactory.Create(ComServer, TVForthModule, IID_VForthModule,
  // ciMultiInstance, tmBoth);
  // TAutoObjectFactory.Create(ComServer, TVForthSystemAthom, IID_VForthAthom,
  // ciMultiInstance, tmBoth);
  // TAutoObjectFactory.Create(ComServer, TCustomVForthVariant, IID_VForthVariant,
  // ciMultiInstance, tmBoth);
end.
