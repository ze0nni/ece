unit VForthModuleOle;
{$IFDEF fpc}{$MODE delphi}{$ENDIF}
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
    FOleDisp : IDispatch;
    FMethodIds : TStringList;
    FIDs : TStringList;
    function GetVariantType: TVariantType; override; stdcall;
    function GetIntValue: Integer; override; stdcall;
  public
    constructor Create;
    destructor Destroy; override;
    function Convert(AVt: TVariantType): IVForthVariant; override; stdcall;
    function GetStringValue: string; override; stdcall;

    function GetDispId(const Name : string) : Integer;
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
  NewVar.FOleDisp := oo;

  AMachine.Push(NewVar);
end;

procedure VfOleFunction(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
var
  v1 : TOleVariant;
  tkid : Integer;
  Name : string;
begin
  v1 := pointer(AMachine.PopInt);

  tkid := AMachine.CourientTkIndex;
  inc(tkid);
  AMachine.CourientTkIndex := tkid;
  Name :=AMachine.GetTk(tkid);
  v1.FOleObj.pattern := '123';
  //v1.GetDispId(Name)
end;

procedure VfOleSub(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
var
  v1 : TOleVariant;
begin
  v1 := pointer(AMachine.PopInt);
end;

procedure VfOleGetProp(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
var
  v1 : TOleVariant;
begin
  v1 := pointer(AMachine.PopInt);
end;

procedure VfOleSetProp(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
var
  v1 : TOleVariant;
begin
  v1 := pointer(AMachine.PopInt);
end;

{ TVForthModuleOle }

procedure TVForthModuleOle.Register(AMachine: IVForthMachine);
begin
  AMachine.AddAthom(CreateVForthSystemAthom('NewOleObj', Self, VfNewOleObj));
  AMachine.AddAthom(CreateVForthSystemAthom('OleFunction', Self, VfOleFunction));
  AMachine.AddAthom(CreateVForthSystemAthom('OleSub', Self, VfOleSub));
  AMachine.AddAthom(CreateVForthSystemAthom('OleGetProp', Self, VfOleGetProp));
  AMachine.AddAthom(CreateVForthSystemAthom('OleSetProp', Self, VfOleSetProp));
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

constructor TOleVariant.Create;
begin
  inherited;
  FIDs := TStringList.Create;
end;

destructor TOleVariant.Destroy;
begin
  FIDs.Free;
  inherited;
end;

function TOleVariant.GetDispId(const Name: string): Integer;
type
 TStringsArr = Array of PWideChar;
 TDispArr = Array of integer;
var
  index: Integer;
  Names : TStringsArr;
  Ids : TDispArr;
begin
  index := FIDs.IndexOf(Name);
  if index = -1 then
  begin
    SetLength(Names, 1);
    Names[0] := Pchar(Name);
    SetLength(Ids, 1);
    FOleDisp.GetIDsOfNames(GUID_NULL, @names, 1, 0, @Ids);
    FIDs.AddObject(Name, TObject(Ids));
    FIDs.Sort;
    FIDs.Sorted := true;
    Result := Ids[0];
    Writeln(name, #9, Result);

    SetLength(Names, 0);
    SetLength(Ids, 0);
  end
  else
  begin
    Result := Integer(FIds.Objects[index]);
  end;
end;

function TOleVariant.GetIntValue: Integer;
begin
  Result := Integer(self);
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
