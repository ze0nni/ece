unit VForth;

interface

const
  IID_VForthMachine: TGUID = '{047176F4-4F1F-45E0-888C-4247119DF8C2}';
  IID_VForthVariant: TGUID = '{EE03CE6B-9E8C-4ECB-AFAD-FA4168E9B1D1}';
  IID_VForthAthom: TGUID = '{38A44447-7150-43A8-AAF0-9421ABA964A4}';
  IID_VForthModule: TGUID = '{1AE24A86-E107-4F79-B6D9-F5EC88EF3C45}';

type
  IVForthVariant = interface;
  IVForthAthom = interface;
  IVForthModule = interface;
  IVForthIO = interface;

  TForthStack = (fsUser, fsSystem, fsException);

  IVForthMachine = interface
    [IID_VForthMachine]
    // private
    function GetAthom(const AAthom: String): IVForthAthom; stdcall;
    function GetDataStack(const index: Integer): IVForthVariant; stdcall;
    procedure SetDataStack(const index: Integer; const Value: IVForthVariant);
      stdcall;
    function GetDataStackSize: Integer; stdcall;
    function GetAthomsCount: Integer; stdcall;
    function GetAthomByIndex(const AAthom: Integer): IVForthAthom; stdcall;
    function GetVarible(AVaribleName: string): IVForthVariant; stdcall;
    function GetStack: TForthStack; stdcall;
    procedure SetStack(const Value: TForthStack); stdcall;
    // public
    procedure SetIo(AIO: IVForthIO); stdcall;
    function StdIn: string; stdcall;
    procedure StdOut(str: string); stdcall;
    procedure StdErr(str: string); stdcall;
    procedure LoadModule(AModule: IVForthModule); stdcall;
    procedure AddAthom(AAthom: IVForthAthom); stdcall;
    procedure AddCode(ACode: string); stdcall;

    property Stack: TForthStack read GetStack write SetStack;
    property DataStack[const index: Integer]
      : IVForthVariant read GetDataStack write SetDataStack;
    property DataStackSize: Integer read GetDataStackSize;
    property Athom[const AAthom: String]: IVForthAthom read GetAthom;
    property AthomByIndex[const AAthom: Integer]
      : IVForthAthom read GetAthomByIndex;
    property AthomsCount: Integer read GetAthomsCount;
    procedure Forget(AAthom: string); stdcall;

    property Varible[AVaribleName: string]: IVForthVariant read GetVarible;

    function GetCourientTkIndex: Integer; stdcall;
    procedure SetCourientTkIndex(const Value: Integer); stdcall;
    function GetTkCount: Integer; stdcall;
    // Гибкая работа со стеком
    procedure Push(AVariant: IVForthVariant); stdcall;
    function Pop: IVForthVariant; stdcall;
    procedure PushEx(index: Integer; AVariant: IVForthVariant); stdcall;
    function PopEx(index: Integer): IVForthVariant; stdcall;
    procedure PushInt(AVariant: Integer); stdcall;
    function PopInt: Integer; stdcall;
    procedure PushFloat(AVariant: Double); stdcall;
    function PopFloat: Double; stdcall;
    procedure PushString(AVariant: string); stdcall;
    function PopString: string; stdcall;
    procedure PushNatural(AVariant1, AVariant2: Integer); stdcall;
    procedure PushComplex(AVariant1, AVariant2: Double); stdcall;
    // Стек адресов
    procedure PushAddr(AValue: Integer); stdcall;
    function ReturnAddr: Integer; stdcall;
    function PopAddr: Integer; stdcall;
    // Ветвление и циклы
    property CourientTkIndex: Integer read GetCourientTkIndex write
      SetCourientTkIndex;
    property TkCount: Integer read GetTkCount;
    function GetTk(index: Integer): string; stdcall;
  end;

  TVariantType = (vtNull, vtInteger, vtFloat, vtNatural, vtComplex, vtString,
    vtArray, vtObject);

  TWin32Type = (wtNone, wtBool, wtByte, wtWord, wtInt, wtCharA, wtCharW,
    wtPCharA, wtPCharW, wtPointer);

  IVForthVariant = interface
    [IID_VForthVariant]
    // private
    function GetVariantType: TVariantType; stdcall;
    function GetName: string; stdcall;
    procedure SetName(const Value: string); stdcall;
    function GetSize: Integer; stdcall;
    procedure SetSize(const Value: Integer); stdcall;
    function GetItems(const index: Integer): IVForthVariant; stdcall;
    procedure SetItems(const index: Integer; const Value: IVForthVariant);
      stdcall;
    function GetWin32Type: TWin32Type; stdcall;
    procedure SetWin32Type(const Value: TWin32Type); stdcall;
    // protected
    procedure SetFloatValue(const Value: Double); stdcall;
    procedure SetIntValue(const Value: Integer); stdcall;
    procedure SetStringValue(const Value: string); stdcall;
    function GetFloatValue: Double; stdcall;
    function GetIntValue: Integer; stdcall;
    function GetStringValue: string; stdcall;
    function GetBoolValue: Boolean; stdcall;
    procedure SetBoolValue(const Value: Boolean); stdcall;
    // public
    property Name: string read GetName write SetName;
    property VariantType: TVariantType read GetVariantType;
    function Convert(AVt: TVariantType): IVForthVariant; stdcall;

    property IntValue: Integer read GetIntValue write SetIntValue;
    property BoolValue: Boolean read GetBoolValue write SetBoolValue;
    property FloatValue: Double read GetFloatValue write SetFloatValue;
    property StringValue: string read GetStringValue write SetStringValue;

    property Size: Integer read GetSize write SetSize;
    property Items[const index: Integer]
      : IVForthVariant read GetItems write SetItems;
    property Win32Type: TWin32Type read GetWin32Type write SetWin32Type;
  end;

  IVForthAthom = interface
    [IID_VForthAthom]
    // private
    function GetName: String; stdcall;
    function GetModule: IVForthModule; stdcall;
    // public

    property Name: string read GetName;
    property Module: IVForthModule read GetModule;
    procedure Execute(AMachine: IVForthMachine; PAthomStr: PWideChar); stdcall;
  end;

  IVForthModule = interface
    [IID_VForthModule]

    procedure SetProp(AProp, AValue: string); stdcall;
    function GetProp(AProp: string): string; stdcall;
    procedure Register(AMachine: IVForthMachine); stdcall;
  end;

  IVForthIO = interface
    function StdIn: string; stdcall;
    procedure StdOut(str: string); stdcall;
    procedure StdErr(str: string); stdcall;
  end;

function CreateVForthMachine: IVForthMachine; stdcall;

implementation

uses VForthMachine;

function CreateVForthMachine: IVForthMachine; stdcall;
begin
  Result := TVForthMachine.Create;
end;

exports CreateVForthMachine;

end.
