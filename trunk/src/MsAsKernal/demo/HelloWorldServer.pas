unit HelloWorldServer;

interface

type
  THelloWorld = class(TInterfacedObject, IDispatch)
  protected
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
  public
    procedure Hello;
    procedure Print(Str : string);
    function GetStr : string;
  end;

implementation

{ THelloWorld }

function THelloWorld.GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount,
  LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := 0;
  if PChar(Names^) = 'Hello' then
  PInteger(DispIDs)^ := 0 else
  if PChar(Names^) = 'Print' then
  PInteger(DispIDs)^ := 1 else
  if PChar(Names^) = 'GetStr' then
  PInteger(DispIDs)^ := 2
  else
  Result := -1;
end;

function THelloWorld.GetStr: string;
begin
  Writeln('Hello world!');
end;

function THelloWorld.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  writeln('GetTypeInfo');
  Result := 0;
end;

function THelloWorld.GetTypeInfoCount(out Count: Integer): HResult;
begin
  writeln('GetTypeInfoCount');
  Result := 0;
end;

procedure THelloWorld.Hello;
begin
  Writeln('Hello world!');
end;

function THelloWorld.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
var
  PVar: POleVariant;
begin
  Result := 0;
  case DispID of
    0:Hello;
    1:
      begin
        PVar := @VarResult;
        Print(Pvar^);
      end;
    2:;
  end;
end;

procedure THelloWorld.Print(Str: string);
begin
  Writeln(Str);
end;

end.
