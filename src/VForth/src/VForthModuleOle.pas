unit VForthModuleOle;

interface

uses
  VForthModule,
  VForth;

type
  TVForthModuleOle = class(TVForthModule, IVForthModule)
  private

  protected

  public
    procedure Register(AMachine: IVForthMachine); stdcall;
  end;

implementation

uses
  ComObj,
  ActiveX,
  Classes,
  VForthAthom,
  VForthVariants;

type
  TOleVariant = class(TCustomVForthVariant, IVForthVariant)
  private
    FOleName : string;
    FOleObj : OleVariant;
    FMethodIds : TStringList;
    function GetVariantType: TVariantType; override; stdcall;
    function GetIntValue: Integer; override; stdcall;
  public
    function Convert(AVt: TVariantType): IVForthVariant; override; stdcall;
    function GetStringValue: string; override; stdcall;
  end;

procedure VfNewOleObj(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
var
  oo : OleVariant;
  oos : string;
  NewVar : TOleVariant;
begin
  oos := AMachine.PopString;
  oo := CreateOleObject(oos);
  NewVar := TOleVariant.Create;
  NewVar.FOleName := oos;
  NewVar.FOleObj := oo;

  AMachine.Push(NewVar);
end;

procedure VfCallOleMethod(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
begin
//  DispInvoke(pointer(AMachine.PopInt),
end;

{ TVForthModuleOle }

procedure TVForthModuleOle.Register(AMachine: IVForthMachine);
begin
  AMachine.AddAthom(CreateVForthSystemAthom('NewOleObj', Self, VfNewOleObj));
  AMachine.AddAthom(CreateVForthSystemAthom('CallOleMethod', Self, VfCallOleMethod));
end;

{ TOleVariant }

function TOleVariant.Convert(AVt: TVariantType): IVForthVariant;
var
  NewVar : TOleVariant;
begin
  case AVt of
    vtString: Result := CreateStringVariant(FOleName) ;
    vtOle:
      begin
        NewVar := TOleVariant.Create;
        NewVar.FOleName := FOleName;
        NewVar.FOleObj := FOleObj;
        Result := NewVar;
      end;
    else
      inherited;
  end;
end;

function TOleVariant.GetIntValue: Integer;
begin
  Result := Integer(FOleObj);
end;

function TOleVariant.GetStringValue: string;
begin
  Result := FOleName;
end;

function TOleVariant.GetVariantType: TVariantType;
begin
  Result := vtOle;
end;

initialization
  CoInitializeEx(nil, 0);
finalization
  CoUninitialize;
end.
