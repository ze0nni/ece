unit eceFiniteAutomatons;

interface

type
  IFiniteAutomaton = interface
    ['{450219EE-FFF0-40F3-AB60-1F8BBB1014FF}']
  end;

  IFiniteAutomatonSet = interface
    ['{F5E270C0-866D-421F-A0EB-79EEB1D3C99D}']
  end;

  IFiniteAutomatonState = interface
    ['{6EE4BE56-E333-46C9-A3E1-F1A73BF1BB6A}']
  end;

function CreateFiniteAutomaton : IFiniteAutomaton; overload;
function CreateFiniteAutomaton(FileName : string) : IFiniteAutomaton; overload;
function CreateFiniteAutomatonSet : IFiniteAutomatonSet;
function CreateFiniteAutomatonState : IFiniteAutomatonState;

implementation

uses
  eceFiniteAutomatonStateClass,
  eceFiniteAutomatonSetClass,
  eceFiniteAutomatonClass;

function CreateFiniteAutomaton : IFiniteAutomaton;
begin
  Result := TFiniteAutomaton.Create;
end;

function CreateFiniteAutomaton(FileName : string) : IFiniteAutomaton;
begin
  {TODO -oOnni -cGeneral : LoadFromFile}
  Result := TFiniteAutomaton.Create;
end;

function CreateFiniteAutomatonSet : IFiniteAutomatonSet;
begin
  Result := TFiniteAutomatonSet.Create;
end;

function CreateFiniteAutomatonState : IFiniteAutomatonState;
begin
  result := TFiniteAutomatonState.Create;
end;

end.
