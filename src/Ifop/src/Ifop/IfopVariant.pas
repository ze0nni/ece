unit IfopVariant;

interface

uses
  SysUtils,
  ComObj,
  ActiveX;

type
  // Переменная, которая будет храниться в стеке
  // Может быть:
  TIfoVariantType = (vtEmpty, // Не определена
    vtInteger, // Целое число
    vbPointer, // Указатель
    vtFloat, // Вещественное число
    vtString, // Строка
    vtObject, // Объект
    vtInterface); // Интерфейс

  TIfopVariant = class abstract
  private
    FName: string;
    function GetIntValue: Integer; virtual;
    procedure SetIntValue(const Value: Integer); virtual;
    function GetFloatValue: Double; virtual;
    procedure SetFloatValue(const Value: Double); virtual;
    function GetStrValue: string; virtual;
    procedure SetStrValue(const Value: string); virtual;

  protected
    function GetVariantType: TIfoVariantType; virtual; abstract;
  public
    function GetClone : TIfopVariant; virtual;
    property VariantType: TIfoVariantType read GetVariantType;
    property IntValue: Integer read GetIntValue write SetIntValue;
    property FloatValue: Double read GetFloatValue write SetFloatValue;
    property StrValue: string read GetStrValue write SetStrValue;
    property Name: string read FName;
  end;

  TifopIntegerVariant = class(TIfopVariant)
  private
    FValue: Integer;
    function GetIntValue: Integer; override;
    procedure SetIntValue(const Value: Integer); override;
    function GetFloatValue: Double; override;
    procedure SetFloatValue(const Value: Double); override;
    function GetStrValue: string; override;
    procedure SetStrValue(const Value: string); override;
  protected
    function GetVariantType: TIfoVariantType; override;
  public
    function GetClone : TIfopVariant; override;
  end;

  TifopFloatVariant = class(TIfopVariant)
  private
    FValue: Double;
    function GetIntValue: Integer; override;
    procedure SetIntValue(const Value: Integer); override;
    function GetFloatValue: Double; override;
    procedure SetFloatValue(const Value: Double); override;
    function GetStrValue: string; override;
    procedure SetStrValue(const Value: string); override;
  protected
    function GetVariantType: TIfoVariantType; override;
  public
    function GetClone : TIfopVariant; override;
  end;

  TifopStringVariant = class(TIfopVariant)
  private
    FValue: String;
    function GetIntValue: Integer; override;
    procedure SetIntValue(const Value: Integer); override;
    function GetFloatValue: Double; override;
    procedure SetFloatValue(const Value: Double); override;
    function GetStrValue: string; override;
    procedure SetStrValue(const Value: string); override;
  protected
    function GetVariantType: TIfoVariantType; override;
  public
    function GetClone : TIfopVariant; override;
  end;

  TifopInterfaceVariant = class(TIfopVariant)
  private
    FValue: OleVariant;
    FOleName : string;
    function GetStrValue: string; override;
  protected
    function GetVariantType: TIfoVariantType; override;
  public
    constructor Create; overload;
    constructor Create(AComObjName : string);overload;
    function GetClone : TIfopVariant; override;
    property OleObjectName : string read FOleName;
    property OleObj : OleVariant read FValue;
  end;

  procedure RegisterDictionary(AKernel : TObject);

implementation

uses
  IfopKernel;

function TypeToString(aType: TIfoVariantType): string;
begin
  case aType of
    vtEmpty:
      Result := 'Unassigned';
    vtInteger:
      Result := 'Integer';
    vbPointer:
      Result := 'Pointer';
    vtFloat:
      Result := 'Float';
    vtString:
      Result := 'String';
    vtObject:
      Result := 'Object';
    vtInterface:
      Result := 'Interface';
  else
    Result := 'Error';
  end;
end;

{ TIfopVariant }

function TIfopVariant.GetClone: TIfopVariant;
begin
  raise Exception.Create(format('Can''t create clone of "%s as %s"',
      [Name, TypeToString(VariantType)]));
end;

function TIfopVariant.GetFloatValue: Double;
begin
  raise Exception.Create(format('Can''t convert "%s as %s" to float',
      [Name, TypeToString(VariantType)]));
end;

function TIfopVariant.GetIntValue: Integer;
begin
  raise Exception.Create(format('Can''t convert "%s as %s" to integer',
      [Name, TypeToString(VariantType)]));
end;

function TIfopVariant.GetStrValue: string;
begin
  raise Exception.Create(format('Can''t convert "%s as %s" to string',
      [Name, TypeToString(VariantType)]));
end;

procedure TIfopVariant.SetFloatValue(const Value: Double);
begin
  raise Exception.Create(format('Can''t convert "%s as %s" to float',
      [Name, TypeToString(VariantType)]));
end;

procedure TIfopVariant.SetIntValue(const Value: Integer);
begin
  raise Exception.Create(format('Can''t convert "%s as %s" to integer',
      [Name, TypeToString(VariantType)]));
end;

procedure TIfopVariant.SetStrValue(const Value: string);
begin
  raise Exception.Create(format('Can''t convert "%s as %s" to string',
      [Name, TypeToString(VariantType)]));
end;

{ TifopIntegerVariant }

function TifopIntegerVariant.GetClone: TIfopVariant;
begin
  Result := TifopIntegerVariant.Create;
  Result.IntValue := IntValue;
end;

function TifopIntegerVariant.GetFloatValue: Double;
begin
  Result := FValue;
end;

function TifopIntegerVariant.GetIntValue: Integer;
begin
  Result := FValue;
end;

function TifopIntegerVariant.GetStrValue: string;
begin
  Result := IntToStr(FValue)
end;

function TifopIntegerVariant.GetVariantType: TIfoVariantType;
begin
  Result := vtInteger;
end;

procedure TifopIntegerVariant.SetFloatValue(const Value: Double);
begin
  // inherited;
  FValue := Trunc(Value);
end;

procedure TifopIntegerVariant.SetIntValue(const Value: Integer);
begin
  // inherited;
  FValue := Value;
end;

procedure TifopIntegerVariant.SetStrValue(const Value: string);
begin
  // inherited;
  FValue := StrToInt(Value);
end;

{ TifopFloatVariant }

function TifopFloatVariant.GetClone: TIfopVariant;
begin
  Result := TifopFloatVariant.Create;
  Result.FloatValue := FloatValue;
end;

function TifopFloatVariant.GetFloatValue: Double;
begin
  Result := FValue;
end;

function TifopFloatVariant.GetIntValue: Integer;
begin
  Result := Trunc(FValue);
end;

function TifopFloatVariant.GetStrValue: string;
begin
  Result := FloatToStr(FValue);
end;

function TifopFloatVariant.GetVariantType: TIfoVariantType;
begin
  Result := vtFloat;
end;

procedure TifopFloatVariant.SetFloatValue(const Value: Double);
begin
  //inherited;
  FValue := Value;
end;

procedure TifopFloatVariant.SetIntValue(const Value: Integer);
begin
 // inherited;
  FValue := Value;
end;

procedure TifopFloatVariant.SetStrValue(const Value: string);
begin
  //inherited;
  FValue := StrToFloat(Value)
end;

{ TifopStringVariant }

function TifopStringVariant.GetClone: TIfopVariant;
begin
  Result := TifopStringVariant.Create;
  Result.StrValue := StrValue;
end;

function TifopStringVariant.GetFloatValue: Double;
begin
  Result := StrToFloat(FValue)
end;

function TifopStringVariant.GetIntValue: Integer;
begin
  Result :=StrToInt(FValue)
end;

function TifopStringVariant.GetStrValue: string;
begin
  Result := FValue;
end;

function TifopStringVariant.GetVariantType: TIfoVariantType;
begin
  Result := vtString;
end;

procedure TifopStringVariant.SetFloatValue(const Value: Double);
begin
  //inherited;
  FValue := FloatToStr(Value)
end;

procedure TifopStringVariant.SetIntValue(const Value: Integer);
begin
  //inherited;
  FValue := IntToStr(Value)
end;

procedure TifopStringVariant.SetStrValue(const Value: string);
begin
  //inherited;
  FValue := Value;
end;

{ TifopInterfaceVariant }

constructor TifopInterfaceVariant.Create(AComObjName: string);
begin
  inherited create;
  FValue := CreateOleObject(AComObjName);
  FOleName := AComObjName;
end;

constructor TifopInterfaceVariant.Create;
begin

end;

function TifopInterfaceVariant.GetClone: TIfopVariant;
begin
  Result := TifopInterfaceVariant.Create;
  TifopInterfaceVariant(Result).FValue := FValue;
  TifopInterfaceVariant(Result).FOleName := FOleName;
end;

function TifopInterfaceVariant.GetStrValue: string;
begin
  Result := OleObjectName + ' : ' + TypeToString(VariantType);
end;

function TifopInterfaceVariant.GetVariantType: TIfoVariantType;
begin
  Result := vtInterface;
end;

////////////////////////////////////////////////////////////////////////////////

procedure ifopCInt(Kernel : TIfopKernel);
begin
  Kernel.PushInt(Kernel.PopInt);
end;

procedure ifopCFloat(Kernel : TIfopKernel);
begin
  Kernel.PushFloat(Kernel.PopFloat);
end;

procedure ifopCString(Kernel : TIfopKernel);
begin
  Kernel.PushStr(Kernel.PopStr);
end;

procedure RegisterDictionary(AKernel : TObject);
var
  Kernel : TIfopKernel;
begin
  Kernel := TIfopKernel(AKernel);

  Kernel.AddKeyword('CInt', @IfopCInt);
  Kernel.AddKeyword('CFloat', @IfopCFloat);
  Kernel.AddKeyword('CStr', @IfopCString);
end;

initialization
  CoInitialize(0)
finalization
  CoUninitialize;
end.
