unit VForthVariantComplex;

interface

uses
  VForth,
  SysUtils,
  Classes,
  Contnrs,
  VForthVariants;

type
  //записывается как "x+iy" без пробела
  //пример "4,2+6i"
  TComplexVariant = class(TCustomVForthVariant, IVForthVariant)

  end;

implementation

end.
