unit ScriptKernal;

interface

uses
    Windows,
    ActiveX,
    ComObj,
    Scripts,
    activescp;

type
    TScriptKernal = class(TInterfacedObject,
                          IActiveScriptSite,
                          IActiveScriptSiteWindow)
    private
      FEngine: IActiveScript;
      FParser: IActiveScriptParse;
    FWindow: HWND;
    procedure SetWindow(const Value: HWND);
    protected
    {IActiveScriptSite}
      function GetLCID(out plcid: LCID): HResult; stdcall;
      function GetItemInfo(pstrName: LPCOLESTR; dwReturnMask: DWORD;
        out ppiunkItem: IUnknown; out ppti: ITypeInfo): HResult; stdcall;
      function GetDocVersionString(
        out pbstrVersion: WideString): HResult; stdcall;
      function OnScriptTerminate(var pvarResult: OleVariant;
        var pexcepinfo: EXCEPINFO): HResult; stdcall;
      function OnStateChange(
        ssScriptState: SCRIPTSTATE): HResult; stdcall;
      function OnScriptError(
        const pscripterror: IActiveScriptError): HResult; stdcall;
      function OnEnterScript: HResult; stdcall;
      function OnLeaveScript: HResult; stdcall;
    {IActiveScriptSiteWindow}
      function GetWindow(out phwnd: HWND): HResult; stdcall;
      function EnableModeless(fEnable: BOOL): HResult; stdcall;
    public
      Constructor Create(Language: TScriptLanguage);
      Destructor Destroy; override;

      Property Engine: IActiveScript read FEngine;
      Property Parser: IActiveScriptParse read FParser;
      property Window : HWND read FWindow write SetWindow;

      function Execute(code : string) : OleVariant;
    end;

implementation

{ TScriptKernal }

constructor TScriptKernal.Create(Language: TScriptLanguage);
begin
 inherited Create;
 FEngine := CreateComObject(ScriptCLSIDs[Language]) as IActiveScript;
 FParser := FEngine as IActiveScriptParse;
 FEngine.SetScriptSite(Self);
 FParser.InitNew;
end;

destructor TScriptKernal.Destroy;
begin
  FParser := nil;
  if FEngine <> nil then FEngine.Close;
  FEngine := nil;
  inherited;
end;

function TScriptKernal.EnableModeless(fEnable: BOOL): HResult;
begin

end;

function TScriptKernal.Execute(code: string): OleVariant;
var
  exc : EXCEPINFO;
begin
  Parser.ParseScriptText(PChar(code), nil, nil, nil, 0, 0, 0, Result, exc);
end;

function TScriptKernal.GetDocVersionString(
  out pbstrVersion: WideString): HResult;
begin
  Result := E_NOTIMPL;
end;

function TScriptKernal.GetItemInfo(pstrName: LPCOLESTR; dwReturnMask: DWORD;
  out ppiunkItem: IInterface; out ppti: ITypeInfo): HResult;
begin
  Result := E_NOTIMPL;
end;

function TScriptKernal.GetLCID(out plcid: LCID): HResult;
begin
  plcid := GetSystemDefaultLCID;
  Result := S_OK;
end;

function TScriptKernal.GetWindow(out phwnd: HWND): HResult;
begin
     phwnd := FWindow;
end;

function TScriptKernal.OnEnterScript: HResult;
begin

end;

function TScriptKernal.OnLeaveScript: HResult;
begin

end;

function TScriptKernal.OnScriptError(
  const pscripterror: IActiveScriptError): HResult;
begin
  MessageBeep(MB_ICONERROR)
end;

function TScriptKernal.OnScriptTerminate(var pvarResult: OleVariant;
  var pexcepinfo: EXCEPINFO): HResult;
begin

end;

function TScriptKernal.OnStateChange(ssScriptState: SCRIPTSTATE): HResult;
begin

end;

procedure TScriptKernal.SetWindow(const Value: HWND);
begin
  FWindow := Value;
end;

initialization
  OleInitialize(nil);
finalization
  OleUninitialize();
end.
