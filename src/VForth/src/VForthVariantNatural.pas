unit VForthVariantNatural;
{$IFDEF fpc}{$MODE delphi}{$ENDIF}
interface

uses
  VForth,
  SysUtils,
  Classes,
  Contnrs,
  VForthVariants;

type
  //записывается как "x/y" без пробела
  //пример "4/13"
  TNaturalVariant = class(TCustomVForthVariant, IVForthVariant)

  end;


implementation

end.
