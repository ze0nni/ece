unit eceSynParser;
{$IFDEF fpc}{$MODE delphi}{$ENDIF}

interface

uses
  SysUtils,
  Classes;

type
  TEceSynLineState = class;

  PEceSynParserCallBack = procedure(AObj: TObject; const AClassID,
    Length: Integer);

  //
  TEceSynParser = class
  private
    FKeyWords: TStringList;

    function isRegionBegin(c: PChar; var RegID: Integer): boolean;
  public
    Constructor Create;
    Destructor Destroy; override;
    // будут функции для проверки на
    function isSpace(c: PChar): boolean; inline;
    function isSymbol(c: PChar): boolean; inline;
    function isKeyword(c: PChar; var Len: Integer): boolean;
    // Процедуре передается строка, ее состояние и Объект и процедура возврата
    // процедура разбивает строку и передает по кускам стили
    function ParseLine(const ALine: string; var State: TEceSynLineState;
      AObj: TObject; CallBack: PEceSynParserCallBack): Integer;
  end;

  TEceSynClass = class
  private
    FName: string;
  public
    property Name: string read FName;
  end;

  TEceSynLineState = class

  end;

implementation

{ TEceSynParser }

function TEceSynParser.isSpace(c: PChar): boolean;
begin
  Result := c^ in [#9, #32, #0];
end;

function TEceSynParser.isSymbol(c: PChar): boolean;
begin
  Result := c^ in ['.', ',', ':', ';', '(', ')', '[', ']', '+', '-', '*', '/',
    '\', '=', '@', '$', '&', '^', '{', '}', '<', '>'];
end;

constructor TEceSynParser.Create;
begin
  FKeyWords := TStringList.Create;
  FKeyWords.Add('as');
  FKeyWords.Add('if');
  FKeyWords.Add('then');
  FKeyWords.Add('else');
  FKeyWords.Add('for');
  FKeyWords.Add('do');
  FKeyWords.Add('next');
  FKeyWords.Add('while');
  FKeyWords.Add('loop');
  FKeyWords.Add('end');
  FKeyWords.Add('dim');

  FKeyWords.Add('sub');
  FKeyWords.Add('function');
  FKeyWords.Add('class');
  FKeyWords.Add('namespace');
  FKeyWords.Add('property');
  FKeyWords.Add('private');
  FKeyWords.Add('public');
  FKeyWords.Add('shared');
  FKeyWords.Add('imports');

  FKeyWords.Sort;
  FKeyWords.Sorted := True;
end;

destructor TEceSynParser.Destroy;
begin
  if Assigned(FKeyWords) then
    FKeyWords.Free;
  inherited;
end;

function TEceSynParser.isKeyword(c: PChar; var Len: Integer): boolean;
var
  ki: Integer;
  KW: String;
  KwLen: Integer;
begin
  for ki := 0 to FKeyWords.Count - 1 do
  begin
    Kw := FKeyWords[ki];
    KwLen := Length(Kw);
    {TODO -oOnni -cGeneral : Ой вот тут мб ошибка доступа к памяти}
    try
    if (c[0] = kw[1])and(c[KwLen-1] = kw[KwLen])and(isSpace(@c[KwLen])or isSymbol(@c[KwLen])) then
    begin
      Len := KwLen;
      Exit(True);
    end;
    except
      Assert(false, 'Ну что я говорил?');
    end;
  end;
  len := 0;
  Exit(False);
end;

function TEceSynParser.isRegionBegin(c: PChar; var RegID: Integer): boolean;
begin
  Result := c^ in ['"'];
end;

function TEceSynParser.ParseLine(const ALine: string;
  var State: TEceSynLineState; AObj: TObject; CallBack: PEceSynParserCallBack)
  : Integer;
var
  SChar, CChar, EChar: PChar;
  Len: Integer;
begin
  SChar := PChar(ALine);
  CChar := SChar;
  EChar := SChar + Length(ALine);
  repeat
    if CChar^ = '"' then
    begin
      SChar := CChar;
      repeat
        inc(CChar)
      until (CChar^ = '"') or (CChar = EChar);
      CallBack(AObj, 4 { space } , CChar - SChar + 1);
      inc(CChar);
      continue;
    end
    else if isKeyword(CChar, Len) then
    begin
      CallBack(AObj, 6, Len);
      inc(CChar, Len);
      continue;
    end;
    if CChar^ = '''' then
    begin
      SChar := CChar;
      repeat
        inc(CChar)
      until (CChar = EChar);
      CallBack(AObj, 5 { space } , CChar - SChar);
      inc(CChar);
      continue;
    end
    else if isSpace(CChar) then
    begin
      SChar := CChar;
      repeat
        inc(CChar)
      until (not isSpace(CChar)) or (CChar = EChar);
      CallBack(AObj, 0, CChar - SChar);
      continue;
    end
    else if isSymbol(CChar) then
    begin
      CallBack(AObj, 2, 1);
      inc(CChar);
      continue;
    end;
    begin
      SChar := CChar;
      repeat
        inc(CChar)
      until (isSpace(CChar)) or isSymbol(CChar) or (CChar^ = '''') or
        (CChar^ = '"') or (CChar = EChar);
      CallBack(AObj, 1 { space } , CChar - SChar);
      continue;
    end;

  until CChar >= EChar;

  Result := 0;
end;

end.
