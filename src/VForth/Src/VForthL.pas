unit VForthL;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, VForth, TeEngine, ExtCtrls, TeeProcs, Chart, StdCtrls, ComCtrls;

type
  TVForthLiveForm = class(TForm)
    RichEdit1: TRichEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  VForthLiveForm: TVForthLiveForm;

implementation

{$R *.dfm}

end.
