unit eceSynParser;

interface

type
  TEceSynLineState = class;

  PEceSynParserCallBack = procedure(AObj: TObject; const AClassID,
    Length: Integer);

  //
  TEceSynParser = class
  private

  public
    class function isSpace(c: PChar): boolean; inline;
    class function isSymbol(c: PChar): boolean; inline;
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
  Result := c^ in [#9, #32];
end;

class function TEceSynParser.isSymbol(c: PChar): boolean;
begin
  Result := c^ in ['.', ',', ':', ';', '(', ')', '[', ']', '+', '-', '*', '/',
    '\', '=', '@', '$', '^', '{', '}', '<', '>'];
end;

function TEceSynParser.ParseLine(const ALine: string;
  var State: TEceSynLineState; AObj: TObject; CallBack: PEceSynParserCallBack)
  : Integer;
var
  SChar, CChar, EChar: PChar;
begin
  SChar := PChar(ALine);
  CChar := SChar;
  EChar := SChar + Length(ALine);

  repeat
    if isSymbol(CChar) then
    begin
      inc(CChar);
      CallBack(AObj, 2, 1);
    end
    else
    begin
      inc(CChar);
      CallBack(AObj, 1, 1);
    end;
  until CChar >= EChar;

  Result := 0;
end;

end.
