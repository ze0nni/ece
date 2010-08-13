unit BaseFile;

interface

//!!! ¬ качесте разделител€ дробной частичисла используетс€ “ќ„ ј

uses
  Windows,
  Classes,
  SysUtils;

type
  TBaseFile = class
  private
    FBaseFile: TStringList;
    FBaseFileName: string;
  public
    constructor Create;
    destructor Destroy; override;

    property FileName: string read FBaseFileName;
    procedure LoadFromFile(AFileName : string);

    function IntValue(AKey: string): Integer; overload;
    function IntValue(AKey: string; DefValue: Integer): Integer; overload;
    function FloatValue(AKey: string): Double; overload;
    function FloatValue(AKey: string; DefValue: Double): Double; overload;
    function StrValue(AKey: String): string; overload;
    function StrValue(AKey: string; DefValue: string): string; overload;
  end;

implementation
{$ifdef fpc}
const
{$else}
resourcestring
{$endif}
  Strќшибка¬‘айлеS÷елое«начениеЌеЌайдено = 'ќшибка в файле "%s". ÷елое значение "%s" не найдено.';
  Strќшибка¬‘айлеSЌеявл€етс€÷елым„ислом = 'ќшибка в файле "%s" строке "%d". ' +
    '«начение "%s" не €вл€етс€ целым числом.';
  Strќшибка¬‘айлеS¬ещественное«начениеЌеЌайдено =
    'ќшибка в файле "%s". ¬ещественное значение "%s" не найдено.';
  Strќшибка¬‘айлеSЌеявл€етс€¬ещественным„ислом =
    'ќшибка в файле "%s" строке "%d". ' +
    '«начение "%s" не €вл€етс€ вещественным числом.';
  Strќшибка¬‘айлеS—троковое«начениеЌеЌайдено =
    'ќшибка в файле "%s". —троковое значение "%s" не найдено.';
  Strќшибка‘айлSЌеЌайден = '‘айл "%s" не найден.';
  { TErr }

{ TBaseFile }

constructor TBaseFile.Create;
begin
  inherited Create;
  FBaseFile := TStringList.Create;
end;

destructor TBaseFile.Destroy;
begin
  FBaseFile.Free;
  inherited;
end;

function TBaseFile.FloatValue(AKey: string): Double;
var
  index: Integer;
  n: Double;
begin
  index := FBaseFile.IndexOfName(AKey);
  if index = -1 then
    // «начение не найдено
    raise Exception.Create(Format(Strќшибка¬‘айлеS¬ещественное«начениеЌеЌайдено,
        [FileName, AKey]));
  if TryStrToFloat(StringReplace(FBaseFile.ValueFromIndex[index], '.', DecimalSeparator, []), n) then
  begin
    // ¬се ок
    Exit(n)
  end
  else
  begin
    // «начение не €вл€етс€ числом
    raise Exception.Create(Format(Strќшибка¬‘айлеSЌеявл€етс€¬ещественным„ислом,
        [FileName, Index + 1, FBaseFile.ValueFromIndex[index]]));
  end;
end;

function TBaseFile.FloatValue(AKey: string; DefValue: Double): Double;
var
  index: Integer;
  n: Double;
begin
  index := FBaseFile.IndexOfName(AKey);
  if index = -1 then
    // «начение по умолчанию
    Exit(DefValue);
  if TryStrToFloat(StringReplace(FBaseFile.ValueFromIndex[index], '.', DecimalSeparator, []), n) then
  begin
    // ¬се ок
    Exit(n)
  end
  else
  begin
    // «начение не €вл€етс€ числом
    raise Exception.Create(Format(Strќшибка¬‘айлеSЌеявл€етс€÷елым„ислом,
        [FileName, Index + 1, FBaseFile.ValueFromIndex[index]]));
  end;
end;

function TBaseFile.IntValue(AKey: string): Integer;
var
  index: Integer;
  n: Integer;
begin
  index := FBaseFile.IndexOfName(AKey);
  if index = -1 then
    // «начение не найдено
    raise Exception.Create(Format(Strќшибка¬‘айлеS÷елое«начениеЌеЌайдено,
        [FileName, AKey]));
  if TryStrToInt(FBaseFile.ValueFromIndex[index], n) then
  begin
    // ¬се ок
    Exit(n)
  end
  else
  begin
    // «начение не €вл€етс€ числом
    raise Exception.Create(Format(Strќшибка¬‘айлеSЌеявл€етс€÷елым„ислом,
        [FileName, Index + 1, FBaseFile.ValueFromIndex[index]]));
  end;
end;

function TBaseFile.IntValue(AKey: string; DefValue: Integer): Integer;
var
  index: Integer;
  n: Integer;
begin
  index := FBaseFile.IndexOfName(AKey);
  if index = -1 then
    // «начение по умолчанию
    Exit(DefValue);
  if TryStrToInt(FBaseFile.ValueFromIndex[index], n) then
  begin
    // ¬се ок
    Exit(n)
  end
  else
  begin
    // «начение не €вл€етс€ числом
    raise Exception.Create(Format(Strќшибка¬‘айлеSЌеявл€етс€÷елым„ислом,
        [FileName, Index + 1, FBaseFile.ValueFromIndex[index]]));
  end;
end;

procedure TBaseFile.LoadFromFile(AFileName: string);
begin
  if not FileExists(AFileName) then
    raise Exception.Create(Format(Strќшибка‘айлSЌеЌайден, [AFileName]));
  FBaseFile.LoadFromFile(AFileName);
  FBaseFileName := AFileName;
end;

function TBaseFile.StrValue(AKey: String): string;
var
  index: Integer;
begin
  index := FBaseFile.IndexOfName(AKey);
  if index = -1 then
    // «начение не найдено
    raise Exception.Create(Format(Strќшибка¬‘айлеS—троковое«начениеЌеЌайдено,
        [FileName, AKey]));
  Result := FBaseFile[index];
end;

function TBaseFile.StrValue(AKey, DefValue: string): string;
var
  index: Integer;
begin
  index := FBaseFile.IndexOfName(AKey);
  if index = -1 then
    Exit(DefValue);
  Result := FBaseFile[index];
end;

end.
