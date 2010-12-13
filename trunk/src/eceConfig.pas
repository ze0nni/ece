unit eceConfig;

interface

uses
  Windows, Registry, SysUtils;

type
  TEceConfigType = set of (ctRegistry, ctIniFile, ctXmlFile, ctPortable,
    ctManualFlush);

  TEceConfigItem = class;

  TEceConfig = class
  private
    FAllUsers: TEceConfigItem;
    FCourientUser: TEceConfigItem;
  protected
  public
    constructor Create(AConfigType: TEceConfigType;
      ProgramName, CompanyName: String);
    destructor Destroy; override;
    property CourientUser: TEceConfigItem read FCourientUser;
    property AllUsers: TEceConfigItem read FAllUsers;
    procedure Flush;
  end;

  TEceConfigItem = class
  private
    FRegistry: TRegistry;
  protected
  public
    Constructor Create(AConfigType: TEceConfigType;
      ProgramName, CompanyName: String; AAllUsers: Boolean);
    Destructor Destroy; override;
    procedure Flush;
  end;

implementation

{ TEceConfig }

constructor TEceConfig.Create(AConfigType: TEceConfigType;
  ProgramName, CompanyName: String);
begin
  FAllUsers := TEceConfigItem.Create(AConfigType, ProgramName, CompanyName,
    False);
  FCourientUser := TEceConfigItem.Create(AConfigType, ProgramName, CompanyName,
    True)
end;

destructor TEceConfig.Destroy;
begin
  if Assigned(FAllUsers) then
    FAllUsers.Free;
  if Assigned(FAllUsers) then
    FCourientUser.Free;
  inherited;
end;

procedure TEceConfig.Flush;
begin
  FCourientUser.Flush;
  FAllUsers.Flush;
end;

{ TEceConfigItem }

constructor TEceConfigItem.Create(AConfigType: TEceConfigType;
  ProgramName, CompanyName: String; AAllUsers: Boolean);
begin
  if ctRegistry in AConfigType then
  begin
{$REGION 'Registry'}
    FRegistry := TRegistry.Create;
    if AAllUsers then
      FRegistry.RootKey := HKEY_LOCAL_MACHINE
    else
      FRegistry.RootKey := HKEY_CURRENT_USER;
    FRegistry.OpenKey(Format('Software\%s\%s', [CompanyName, ProgramName]), True);
{$ENDREGION}
  end;
end;

destructor TEceConfigItem.Destroy;
begin
  if Assigned(FRegistry) then
    FRegistry.Free;
  inherited;
end;

procedure TEceConfigItem.Flush;
begin

end;

end.
