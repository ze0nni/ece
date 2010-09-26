unit VForthAthom;
{$IFDEF fpc}{$MODE delphi}{$ENDIF}
interface

uses
  ComObj,
  VForth;

type
  PVForthSystemAthomProc = procedure(AMachine: IVForthMachine;
    AAthom: IVForthAthom; PAthomStr: PWideChar); stdcall;

  TVForthSystemAthom = class(TInterfacedObject, IVForthAthom)
  private
    FNextAthom : IVForthAthom;
    FName: string;
    FModule: IVForthModule;
    FProcedure: PVForthSystemAthomProc;
    function GetName: String; stdcall;
    function GetModule: IVForthModule; stdcall;
  public
    destructor Destroy; override;
    property Name: string read GetName;
    property Module: IVForthModule read GetModule;
    procedure Execute(AMachine: IVForthMachine; PAthomStr: PWideChar); stdcall;
  end;

function CreateVForthSystemAthom(AName: string; AModule: IVForthModule;
  PProc: PVForthSystemAthomProc): IVForthAthom;

implementation

function CreateVForthSystemAthom(AName: string; AModule: IVForthModule;
  PProc: PVForthSystemAthomProc): IVForthAthom;
var
  NewAthom: TVForthSystemAthom;
begin
  NewAthom := TVForthSystemAthom.Create;
  NewAthom.FName := AName;
  NewAthom.FModule := AModule;
  NewAthom.FProcedure := PProc;
  Result := NewAthom;
end;

{ TVForthSystemAthom }

destructor TVForthSystemAthom.Destroy;
begin
  inherited;
end;

procedure TVForthSystemAthom.Execute(AMachine: IVForthMachine;
  PAthomStr: PWideChar);
begin
  FProcedure(AMachine, Self, PAthomStr);
end;

function TVForthSystemAthom.GetModule: IVForthModule;
begin
  Result := FModule;
end;

function TVForthSystemAthom.GetName: String;
begin
  Result := FName;
end;

end.
