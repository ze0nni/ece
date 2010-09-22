unit Win32Error;
{$IFDEF fpc}{$MODE delphi}{$ENDIF}
interface

uses
  Windows,
  SysUtils;

type
  EWin32Error = class(Exception)
  public
    constructor Create(ErrorCode: Cardinal);
  end;

procedure RaiseLastWin32Error(ExceptErrors: array of Cardinal);

implementation

procedure RaiseLastWin32Error(ExceptErrors: array of Cardinal);
var
  I: Integer;
  ErrorCode: Integer;
begin
  ErrorCode := GetLastError;
  for I := Low(ExceptErrors) to High(ExceptErrors) do
    if GetLastError = ExceptErrors[I] then
      exit;
  raise EWin32Error.Create(ErrorCode);
end;

{ EWin32Error }

constructor EWin32Error.Create(ErrorCode: Cardinal);
var
  msg: String;
  buff: Cardinal;
begin
  FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_FROM_SYSTEM,
    nil, ErrorCode, 0, @buff, 0, nil);
  msg := PChar(buff);
  LocalFree(buff);
  inherited Create(msg);
end;

end.
