unit eceFiniteAutomatons;

interface

type
  IFinateAutomaton = interface
    ['{450219EE-FFF0-40F3-AB60-1F8BBB1014FF}']
  end;

  IFinateAutomatonSet = interface
    ['{F5E270C0-866D-421F-A0EB-79EEB1D3C99D}']
  end;

  IFinateAutomatonState = interface
    ['{6EE4BE56-E333-46C9-A3E1-F1A73BF1BB6A}']
  end;

function CreateFinateAutomaton : IFinateAutomaton; overload;
function CreateFinateAutomaton(FileName : string) : IFinateAutomaton; overload;
function CreateFinateAutomatonSet : IFinateAutomatonSet;
function CreateFinateAutomatonState : IFinateAutomatonState;

implementation

uses
  eceFinateAutomatonStateClass,
  eceFinateAutomatonSetClass,
  eceFinateAutomatonClass;

function CreateFinateAutomaton : IFinateAutomaton;
begin
  Result := TFinateAutomaton.Create;
end;

function CreateFinateAutomaton(FileName : string) : IFinateAutomaton;
begin
  {TODO -oOnni -cGeneral : LoadFromFile}
  Result := TFinateAutomaton.Create;
end;

function CreateFinateAutomatonSet : IFinateAutomatonSet;
begin
  Result := TFinateAutomatonSet.Create;
end;

function CreateFinateAutomatonState : IFinateAutomatonState;
begin
  result := TFinateAutomatonState.Create;
end;

end.
