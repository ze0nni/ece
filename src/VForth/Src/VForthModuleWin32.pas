unit VForthModuleWin32;

interface

uses
  SysUtils,
  VForthModule,
  VForth;

type
  EVForthModuleWin32Error = class(Exception)

  end;

  TVForthModuleWin32 = class(TVForthModule, IVForthModule)
  private

  protected

  public
    procedure Register(AMachine: IVForthMachine); stdcall;
  end;

implementation

{ TVForthModuleWin32 }

procedure TVForthModuleWin32.Register(AMachine: IVForthMachine);
begin

end;

end.
