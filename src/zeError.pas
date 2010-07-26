unit zeError;
{$ifdef fpc}{$mode delphi}{$endif}
interface

uses
	Windows;

type
	EErr = class
	public
		Constructor Create(Text : String); overload;
		Constructor Create(Text : String;  args : Array of PChar); overload;
		Constructor Create(TextId : Integer;  args : Array of PChar); overload;
	end;

var
  ErrMainWidnow : HWND;

implementation

Constructor EErr.Create(Text : String);
begin
	MessageBox(ErrMainWidnow, Pchar(Text), Nil, MB_ICONERROR);
end;

Constructor EErr.Create(Text : String; args : Array of PChar);
begin

end;

Constructor EErr.Create(TextId : integer; args : Array of PChar);
begin

end;

end.