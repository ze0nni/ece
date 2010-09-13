unit VForthVariants;

interface

uses
  Windows,
  VForth,
  ComObj,
  SysUtils,
  Classes,
  Contnrs;

resourcestring
  VariantsStrConvertErrorTo = 'Convert error: "%s" to "%s".';

type
  EVForthVariantError = class(Exception)

  end;

  TCustomVForthVariant = class(TInterfacedObject, IVForthVariant)
  private
    FName: string;
    FWin32Type: TWin32Type;
    function GetName: string; stdcall;
    procedure SetName(const Value: string); stdcall;
    function GetWin32Type: TWin32Type; stdcall;
    procedure SetWin32Type(const Value: TWin32Type); stdcall;
  protected
    function GetSize: Integer; virtual; stdcall;
    procedure SetSize(const Value: Integer); virtual; stdcall;
    function GetItems(const index: Integer): IVForthVariant; virtual; stdcall;
    procedure SetItems(const index: Integer; const Value: IVForthVariant);
      virtual; stdcall;
    function GetVariantType: TVariantType; virtual; stdcall;
    procedure SetFloatValue(const Value: Double); virtual; stdcall;
    procedure SetIntValue(const Value: Integer); virtual; stdcall;
    procedure SetStringValue(const Value: string); virtual; stdcall;
    function GetFloatValue: Double; virtual; stdcall;
    function GetIntValue: Integer; virtual; stdcall;
    function GetStringValue: string; virtual; stdcall;
    function GetBoolValue: Boolean; stdcall;
    procedure SetBoolValue(const Value: Boolean); stdcall;
  protected

  public
    constructor Create;
    destructor Destroy; override;
    property Name: string read GetName write SetName;
    property VariantType: TVariantType read GetVariantType;
    function Convert(AVt: TVariantType): IVForthVariant; virtual; stdcall;
    property IntValue: Integer read GetIntValue write SetIntValue;
    property BoolValue: Boolean read GetBoolValue write SetBoolValue;
    property FloatValue: Double read GetFloatValue write SetFloatValue;
    property StringValue: string read GetStringValue write SetStringValue;
    property Size: Integer read GetSize write SetSize;
    property Items[const index: Integer]
      : IVForthVariant read GetItems write SetItems;
    property Win32Type: TWin32Type read GetWin32Type write SetWin32Type;
  end;

function VariantTypeToString(vt: TVariantType): string;
function GetPriorityVariantType(const vt1, vt2: TVariantType): TVariantType;

function CreateIntegerVariant(Value: Integer): IVForthVariant;
function CreateFloatVariant(Value: Double): IVForthVariant;
function CreateNaturalVariant(value1, value2: Integer): IVForthVariant;
function CreateComplexVariant(value1, value2: Double): IVForthVariant;
function CreateStringVariant(Value: string): IVForthVariant;

implementation

uses
  VForthVariantInteger,
  VForthVariantFloat,
  VForthVariantNatural, { TODO -oOnni -cGeneral : VForthVariantNatural }
  VForthVariantComplex, { TODO -oOnni -cGeneral : VForthVariantComplex }
  VForthVariantString;

var
  VarCount: Integer = 0;

function VariantTypeToString(vt: TVariantType): string;
begin
  case vt of
    vtNull:
      Result := 'Null';
    vtInteger:
      Result := 'Integer';
    vtFloat:
      Result := 'Float';
    vtNatural:
      Result := 'Natural';
    vtComplex:
      Result := 'Complex';
    vtString:
      Result := 'String';
    vtArray:
      Result := 'Array';
    vtObject:
      Result := 'Object';
  else
    raise EVForthVariantError.Create('Uncnown type');
  end;
end;
{$REGION 'ќпределение приоритетного типа среди двух'}

var
  VtPriorityArray: array [TVariantType, TVariantType] of TVariantType;

procedure SetVariantPriority(const vt1, vt2, vtr: TVariantType);
begin
  VtPriorityArray[vt1, vt2] := vtr;
  VtPriorityArray[vt2, vt1] := vtr;
end;

function GetPriorityVariantType(const vt1, vt2: TVariantType): TVariantType;
begin
  Result := VtPriorityArray[vt1, vt2]
end;
{$ENDREGION}
{$REGION '—оздание типизированных переменных'}

function CreateIntegerVariant(Value: Integer): IVForthVariant;
begin
  Result := TIntegerVariant.Create;
  Result.IntValue := Value;
end;

function CreateFloatVariant(Value: Double): IVForthVariant;
begin
  Result := TFloatVariant.Create;
  Result.FloatValue := Value;
end;

function CreateNaturalVariant(value1, value2: Integer): IVForthVariant;
begin
  Result := TNaturalVariant.Create;
  { TODO -oOnni -cGeneral : Result.Value = }
end;

function CreateComplexVariant(value1, value2: Double): IVForthVariant;
begin
  Result := TComplexVariant.Create;
  { TODO -oOnni -cGeneral : Result.Value = }
end;

function CreateStringVariant(Value: string): IVForthVariant;
begin
  Result := TStringVariant.Create;
  Result.StringValue := Value;
end;
{$ENDREGION}
{ TCustomVForthVariant }

function TCustomVForthVariant.Convert(AVt: TVariantType): IVForthVariant;
begin
  // if AVt = VariantType then
  // exit;
  raise EVForthVariantError.CreateFmt(VariantsStrConvertErrorTo,
    [VariantTypeToString(VariantType), VariantTypeToString(AVt)]);
end;

constructor TCustomVForthVariant.Create;
begin
  inherited;

end;

destructor TCustomVForthVariant.Destroy;
begin

  inherited;
end;

function TCustomVForthVariant.GetBoolValue: Boolean;
begin
  Result := IntValue <> 0;
end;

function TCustomVForthVariant.GetFloatValue: Double;
begin
  raise EVForthVariantError.CreateFmt(VariantsStrConvertErrorTo,
    [VariantTypeToString(VariantType), VariantTypeToString(vtFloat)]);
end;

function TCustomVForthVariant.GetIntValue: Integer;
begin
  raise EVForthVariantError.CreateFmt(VariantsStrConvertErrorTo,
    [VariantTypeToString(VariantType), VariantTypeToString(vtInteger)]);
end;

function TCustomVForthVariant.GetItems(const index: Integer): IVForthVariant;
begin
  if index = 0 then
    Result := Self
  else
    raise EVForthVariantError.CreateFmt('Variant out of range (%s)', [index]);
end;

function TCustomVForthVariant.GetName: string;
begin
  Result := FName;
end;

function TCustomVForthVariant.GetSize: Integer;
begin
  Result := 1;
end;

function TCustomVForthVariant.GetStringValue: string;
begin
  raise EVForthVariantError.CreateFmt(VariantsStrConvertErrorTo,
    [VariantTypeToString(VariantType), VariantTypeToString(vtString)]);
end;

function TCustomVForthVariant.GetVariantType: TVariantType;
begin
  Result := vtNull;
end;

function TCustomVForthVariant.GetWin32Type: TWin32Type;
begin
  Result := FWin32Type;
end;

procedure TCustomVForthVariant.SetBoolValue(const Value: Boolean);
begin
  IntValue := Integer(Value);
end;

procedure TCustomVForthVariant.SetFloatValue(const Value: Double);
begin
  raise EVForthVariantError.CreateFmt(VariantsStrConvertErrorTo,
    [VariantTypeToString(vtFloat), VariantTypeToString(VariantType)]);
end;

procedure TCustomVForthVariant.SetIntValue(const Value: Integer);
begin
  raise EVForthVariantError.CreateFmt(VariantsStrConvertErrorTo,
    [VariantTypeToString(vtInteger), VariantTypeToString(VariantType)]);
end;

procedure TCustomVForthVariant.SetItems(const index: Integer;
  const Value: IVForthVariant);
begin
  raise EVForthVariantError.Create('Variant is not array');
end;

procedure TCustomVForthVariant.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TCustomVForthVariant.SetSize(const Value: Integer);
begin
  raise EVForthVariantError.Create('Variant is not array');
end;

procedure TCustomVForthVariant.SetStringValue(const Value: string);
begin
  raise EVForthVariantError.CreateFmt(VariantsStrConvertErrorTo,
    [VariantTypeToString(vtString), VariantTypeToString(VariantType)]);
end;

procedure TCustomVForthVariant.SetWin32Type(const Value: TWin32Type);
begin
  FWin32Type := Value;
end;

var
  Iv: TVariantType;

initialization

{$REGION '¬ыставл€ем приоритет типов по умолчанию'}
for Iv := low(Iv) to High(Iv) do
  SetVariantPriority(Iv, Iv, Iv);
// Integer
SetVariantPriority(vtInteger, vtFloat, vtFloat);
SetVariantPriority(vtInteger, vtNatural, vtNatural);
SetVariantPriority(vtInteger, vtComplex, vtComplex);
SetVariantPriority(vtInteger, vtString, vtString);
// Float
SetVariantPriority(vtFloat, vtNatural, vtNatural);
SetVariantPriority(vtFloat, vtComplex, vtComplex);
SetVariantPriority(vtFloat, vtString, vtString);
{$ENDREGION}

finalization

end.
