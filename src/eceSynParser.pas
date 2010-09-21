unit eceSynParser;

interface

type
  TEceSynLineState = class;

  PEceSynParserCallBack = procedure(AObj: TObject; const AClassID,
    Length: Integer);

  //
  TEceSynParser = class
  private
    class function isRegionBegin(c: PChar; var RegID: Integer): boolean; static;

  public
    // будут функции для проверки на

    class function isSpace(c: PChar): boolean; inline;
    class function isSymbol(c: PChar): boolean; inline;
    class function isKeyword(c: PChar; var Len: Integer): boolean;
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

class function TEceSynParser.isSpace(c: PChar): boolean;
begin
  Result := c^ in [#9, #32, #0];
end;

class function TEceSynParser.isSymbol(c: PChar): boolean;
begin
  Result := c^ in ['.', ',', ':', ';', '(', ')', '[', ']', '+', '-', '*', '/',
    '\', '=', '@', '$', '&', '^', '{', '}', '<', '>'];
end;

class function TEceSynParser.isKeyword(c: PChar; var Len: Integer): boolean;
begin
  if (c[0] = 'a') and (c[1] = 's') and (isSpace(@c[2]) or isSymbol(@c[2])) then
  begin
    Len := 2;
    exit(true);
  end
  else if (c[0] = 'i') and (c[1] = 'f') and (isSpace(@c[2]) or isSymbol(@c[2]))
    then
  begin
    Len := 2;
    exit(true);
  end
  else if (c[0] = 's') and (c[1] = 'u') and (c[2] = 'b') and
    (isSpace(@c[3]) or isSymbol(@c[3])) then
  begin
    Len := 3;
    exit(true);
  end else
  if (c[0] = 'e') and (c[1] = 'n') and (c[2] = 'd') and
    (isSpace(@c[3]) or isSymbol(@c[3])) then
  begin
    Len := 3;
    exit(true);
  end else
  if (c[0] = 'd') and (c[1] = 'i') and (c[2] = 'm') and
    (isSpace(@c[3]) or isSymbol(@c[3])) then
  begin
    Len := 3;
    exit(true);
  end else
  if (c[0] = 'e') and (c[1] = 'l') and (c[2] = 's') and  (c[3] = 'e') and
    (isSpace(@c[4]) or isSymbol(@c[4])) then
  begin
    Len := 4;
    exit(true);
  end else
  if (c[0] = 't') and (c[1] = 'h') and (c[2] = 'e') and  (c[3] = 'n') and
    (isSpace(@c[4]) or isSymbol(@c[4])) then
  begin
    Len := 4;
    exit(true);
  end;
  Result := false;
end;

class function TEceSynParser.isRegionBegin(c: PChar; var RegID: Integer)
  : boolean;
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
