unit ifopInformation;

interface

uses
  Classes,
  SysUtils;

procedure RegisterDictionary(AKernel: TObject);

implementation

uses
  IfopKernel;

procedure ifopVersion(Kernel: TIfopKernel);
begin
  Kernel.stdout(format('I Forth Processor v.%d.%d.%d.%d', [IfopVersionMajor,
      IfopVersionMinor, IfopVersionRelease, IfopVersionBuild]));
end;

procedure ifopHelp(Kernel: TIfopKernel);
begin
  Kernel.stdout('Input <dictionary> to view all register keywords.');
  Kernel.stdout('Input <"%anyword%" help> to get keyword discription.');
  Kernel.stdout('Input <topic> or <"%topicid%" topic> to view examples.');
end;

function FindId(ls : TStringList; id : string) : Integer;
var
  i: Integer;
begin
  for i := 0 to ls.Count - 1 do
  begin
    if Pos(id + #9, ls[i]) = 1 then
      exit(i);
  end;
  Result := -1;
end;

procedure ifopTopic(Kernel: TIfopKernel);
var
  TopicId : string;
  sr : TSearchRec;
  ls : TStringList;
  fRes : Integer;
  SRes : Boolean;
  SpPos: Integer;
begin
  TopicId := Kernel.PopStr;
  //Перебираем все txt файлы в папке help
  ls := TStringList.Create;
  SRes := false;
  try
  if FindFirst('.\help\*.txt', faAnyFile, sr) = 0 then
  begin
    repeat
      ls.LoadFromFile('.\help\' + sr.Name);
      fRes := FindId(ls, TopicId);
      if fRes <> -1 then
      begin
        SRes := true;
        SpPos := pos(#9, ls[fRes]);
        Kernel.stdout(StringReplace(Copy(ls[fRes], SpPos+1),'\n',#13#10,[rfReplaceAll]));
      end;
    until (FindNext(sr) <> 0);
    FindClose(sr);
    if not SRes then
      raise Exception.Create('Topic "' + TopicId + '" not found');
  end;
  finally
    ls.Free;
  end;
end;

procedure ifopDictionary(Kernel: TIfopKernel);
var
  i : Integer;
begin
  for i := 0 to Kernel.DictionarySize - 1 do
    Kernel.stdout(Kernel.Dictionary[i].KeywordName + #9, false);
  Kernel.stdout(format('%d items', [Kernel.DictionarySize]), true);
end;

procedure RegisterDictionary(AKernel: TObject);
var
  Kernel: TIfopKernel;
  i: Integer;
begin
  Kernel := TIfopKernel(AKernel);
  // Регистрация функций
  Kernel.AddKeyword('version', @ifopVersion);
  Kernel.AddKeyword('help', @ifopHelp);
  Kernel.AddKeyword('topic', @ifopTopic);
  Kernel.AddKeyword('dictionary', @ifopDictionary);
end;

end.
