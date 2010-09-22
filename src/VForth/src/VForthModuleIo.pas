unit VForthModuleIo;
{$IFDEF fpc}{$MODE delphi}{$ENDIF}
interface

uses
  SysUtils,
  VForthModule,
  VForth;

type
  EVForthModuleIoError = class(Exception)

  end;

  TVForthModuleIo = class(TVForthModule, IVForthModule)
  private

  protected

  public
    procedure Register(AMachine: IVForthMachine); stdcall;
  end;

implementation

uses
  windows,
  Win32Error,
  Classes,
  VForthVariants,
  VForthAthom;

const
  FILETYPE_BIN = 0;
  FILETYPE_CONFIG = 1;

type
  TFileVariant = class(TCustomVForthVariant, IVForthVariant)
  private
    FList: TStringList;
    FFileType: Integer;
    HFile: Integer;
    FFileName: string;
  protected
    function GetVariantType: TVariantType; override; stdcall;
    function GetIntValue: Integer; override; stdcall;
    function GetFloatValue: Double; override; stdcall;
    function GetStringValue: string; override; stdcall;
    destructor Destroy; override;
  end;

  { TVForthModuleIo }

procedure VfStdIn(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  AMachine.PushString(AMachine.StdIn);
end;

procedure VfStdOut(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  AMachine.PushString(AMachine.PopString);
end;

procedure VfStdErr(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  AMachine.StdErr(AMachine.PopString);
end;

procedure VfPrint(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  AMachine.stdout(AMachine.PopString);
end;

procedure VfCr(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  AMachine.stdout(#13#10);
end;

procedure VfSpaces(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  i: Integer;
begin
  for i := 0 to AMachine.PopInt - 1 do
    Write(#32);
end;

procedure VfSpace(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  AMachine.stdout(#32);
end;

procedure VfEmit(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  AMachine.stdout(Char(AMachine.PopInt));
end;

function GetFileMode(fm: string): Cardinal;
begin
  { TODO -oOnni -cGeneral : Неясности с FileMode }
  fm := LowerCase(fm);
  if fm = 'r' then
    Result := fmOpenRead
  else if fm = 'w' then
    Result := fmOpenWrite
  else if fm = 'a' then
    Result := fmOpenReadWrite
  else if fm = 'r+' then
    Result := fmOpenReadWrite
  else if fm = 'w+' then
    Result := fmOpenReadWrite
  else if fm = 'a+' then
    Result := fmOpenReadWrite
  else
    raise EVForthModuleIoError.CreateFmt('Uncnown file mode "%s"', [fm]);
end;

procedure VfFOpen(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  FileV: TFileVariant;
  HFile: Cardinal;
  FileName: string;
begin
  FileName := AMachine.PopString;
  HFile := FileOpen(FileName, GetFileMode(AMachine.PopString));
  RaiseLastWin32Error([ERROR_SUCCESS]);
  FileV := TFileVariant.Create;
  FileV.HFile := HFile;
  FileV.FFileName := FileName;
  FileV.FFileType := FILETYPE_BIN;

  AMachine.Push(FileV);
end;

procedure VfFOpenConfig(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  FileV: TFileVariant;
  HFile: Cardinal;
  FileName: string;
begin
  FileName := AMachine.PopString;
  FileV := TFileVariant.Create;
  AMachine.Push(FileV);

  FileV.FList := TStringList.Create;
  if FileExists(FileName) then
    FileV.FList.LoadFromFile(FileName);
  FileV.HFile := Integer(FileV.FList);
  FileV.FFileName := FileName;
  FileV.FFileType := FILETYPE_CONFIG;
end;

procedure vfFClose(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin
  { TODO -oOnni -cGeneral : Закрыть без сохранения }
end;

procedure VfFRead(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  f: IVForthVariant;
  v0, v1: IVForthVariant;
  n: Integer;
  nsize: Integer;
  w32t: TWin32Type;
  AStr: AnsiString;
  WStr: WideString;
  rRes: Integer;
  l: TStringList;
begin
  f := AMachine.Pop;
{$REGION 'Двоичный файл'}
  if f.FloatValue = FILETYPE_BIN then
  begin
    v0 := AMachine.DataStack[0];
    case v0.Win32Type of
      // без какого либо типа
      wtNone:
        raise Exception.Create('Need win32 type');
      // Число или символ
      wtBool, wtByte, wtWord, wtInt, wtCharA, wtCharW:
        begin
          case v0.Win32Type of
            wtBool:
              nsize := 1;
            wtByte:
              nsize := 1;
            wtWord:
              nsize := 2;
            wtInt:
              nsize := 4;
            wtCharA:
              nsize := 1;
            wtCharW:
              nsize := 2;
          end;
          FileRead(f.IntValue, n, nsize);
          RaiseLastWin32Error([ERROR_SUCCESS]);
          v0.IntValue := n;
        end;
      // строки читаются пока не встетится символ #0 или конец файла
      wtPCharA, wtPCharW:
        begin
          w32t := v0.Win32Type;
          case w32t of
            wtPCharA:
              nsize := 1;
            wtPCharW:
              nsize := 2;
          end;
          repeat
            rRes := FileRead(f.IntValue, n, nsize);
            if (rRes <> 0) and (n <> 0) then
              case w32t of
                wtPCharA:
                  AStr := AStr + AnsiChar(n);
                wtPCharW:
                  WStr := WStr + WideChar(n);
              end;
          until (n = 0) or (rRes = 0);
          case w32t of
            wtPCharA:
              v0.StringValue := AStr;
            wtPCharW:
              v0.StringValue := WStr;
          end;
          RaiseLastWin32Error([ERROR_SUCCESS]);
        end;
      wtPointer:
        { TODO -oOnni -cGeneral : Вот только что TODO? } ;
    end;
  end
{$ENDREGION}
  else if f.FloatValue = FILETYPE_CONFIG then
  begin
    l := Pointer(f.IntValue);
    v0 := AMachine.Pop;
    v1 := AMachine.DataStack[0];
    v1.StringValue := l.Values[v0.StringValue];
  end
  else
    raise EVForthModuleIoError.Create('Uncnown file type');
end;

procedure VfFWrite(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
var
  f: IVForthVariant;
  v0, v1: IVForthVariant;
  n: Integer;
  nsize: Integer;
  w32t: TWin32Type;
  AStr: AnsiString;
  WStr: WideString;
  l: TStringList;
  Sval: String;
begin
  f := AMachine.Pop;
{$REGION 'Двоичный файл'}
  if f.FloatValue = FILETYPE_BIN then
  begin
    v0 := AMachine.Pop;
    case v0.Win32Type of
      // без какого либо типа
      wtNone:
        raise Exception.Create('Need win32 type');
      // Число или символ
      wtBool, wtByte, wtWord, wtInt, wtCharA, wtCharW:
        begin
          case v0.Win32Type of
            wtBool:
              nsize := 1;
            wtByte:
              nsize := 1;
            wtWord:
              nsize := 2;
            wtInt:
              nsize := 4;
            wtCharA:
              nsize := 1;
            wtCharW:
              nsize := 2;
          end;
          n := v0.IntValue;
          FileWrite(f.IntValue, n, nsize);
          RaiseLastWin32Error([ERROR_SUCCESS]);
        end;
      // строки пищутся с завершающимся нулем
      wtPCharA, wtPCharW:
        begin
          w32t := v0.Win32Type;
          n := 0;
          case w32t of
            wtPCharA:
              begin
                AStr := v0.StringValue;
                nsize := Length(AStr) * SizeOf(AnsiChar);
                FileWrite(f.IntValue, AStr[1], nsize);
                FileWrite(f.IntValue, n, SizeOf(AnsiChar));
              end;
            wtPCharW:
              begin
                WStr := v0.StringValue;
                nsize := Length(WStr) * SizeOf(WideChar);
                FileWrite(f.IntValue, WStr[1], nsize);
                FileWrite(f.IntValue, n, SizeOf(WideChar));
              end;
          end;
          RaiseLastWin32Error([ERROR_SUCCESS]);
        end;
      wtPointer:
        { TODO -oOnni -cGeneral : Вот только что TODO? } ;
    end;
  end
{$ENDREGION}
  else if f.FloatValue = FILETYPE_CONFIG then
  begin
    l := Pointer(f.IntValue);
    v0 := AMachine.Pop;
    v1 := AMachine.Pop;
    Sval := v1.StringValue;
    if Sval = '' then
    begin
      // При присвоении пустой строки - удаляем значение
      n := l.IndexOfName(v0.StringValue);
      if n <> -1 then
        l.Delete(n);
    end
    else
      // Иначе устанавливаем
      l.Values[v0.StringValue] := Sval;
  end
  else
    raise EVForthModuleIoError.Create('Uncnown file type');
end;

procedure VfFSeek(AMachine: IVForthMachine; AAthom: IVForthAthom;
  PAthomStr: PWideChar); stdcall;
begin

end;

procedure TVForthModuleIo.Register(AMachine: IVForthMachine);
begin
  AMachine.AddAthom(CreateVForthSystemAthom('.in', Self, VfStdIn));
  AMachine.AddAthom(CreateVForthSystemAthom('.out', Self, VfStdOut));
  AMachine.AddAthom(CreateVForthSystemAthom('.err', Self, VfStdErr));
  AMachine.AddAthom(CreateVForthSystemAthom('.', Self, VfPrint));
  AMachine.AddAthom(CreateVForthSystemAthom('cr', Self, VfCr));
  AMachine.AddAthom(CreateVForthSystemAthom('spaces', Self, VfSpaces));
  AMachine.AddAthom(CreateVForthSystemAthom('space', Self, VfSpace));
  AMachine.AddAthom(CreateVForthSystemAthom('emit', Self, VfEmit));

  AMachine.AddAthom(CreateVForthSystemAthom('fopen', Self, VfFOpen));
  AMachine.AddAthom(CreateVForthSystemAthom('fopencfg', Self, VfFOpenConfig));
  AMachine.AddAthom(CreateVForthSystemAthom('fclose', Self, vfFClose));
  AMachine.AddAthom(CreateVForthSystemAthom('fread', Self, VfFRead));
  AMachine.AddAthom(CreateVForthSystemAthom('fwrite', Self, VfFWrite));
  AMachine.AddAthom(CreateVForthSystemAthom('fseek', Self, VfFSeek));
end;

{ TFileVariant }

destructor TFileVariant.Destroy;
begin
  inherited;
  if FFileType = FILETYPE_BIN then
    FileClose(HFile)
  else
  begin
    FList.SaveToFile(FFileName);
    FList.Free;
  end;
end;

function TFileVariant.GetFloatValue: Double;
begin
  Result := FFileType;
end;

function TFileVariant.GetIntValue: Integer;
begin
  Result := HFile;
end;

function TFileVariant.GetStringValue: string;
begin
  Result := FFileName;
end;

function TFileVariant.GetVariantType: TVariantType;
begin
  Result := vtObject;
end;

end.
