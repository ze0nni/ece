unit iece;
{$IFDEF fpc}{$MODE delphi}{$ENDIF}
{$I EceLanguage.inc}

interface

uses
{$IFDEF forth}
  VForth,
{$ENDIF}
  Windows;

const
  DISPATCH_SUB = 1;
  DISPATCH_FUNCTION = 2;
  DISPATCH_GET = 3;
  DISPATCH_SET = 4;

type
  IEceDocument = interface;
  IEceDocumentLoader = interface;

  IEceApplication = interface
    function _GetHandle: HWND; safecall;
    function _GetDocumentsCount: integer; safecall;
    function _GetDocuments(AIndex: integer; var ADocument: IEceDocument)
      : integer; safecall;
    procedure _UpdateCaption; safecall;
{$IFDEF forth}
    function GetMachine: IVForthMachine; stdcall;
    function GetModule: IVForthModule; stdcall;
{$ENDIF}
    procedure _FocusToActiveDocument; stdcall;
    // Для работы с плагинами
    // Регистрация нового "просмотрщика" документов
    procedure RegisterDocument(Doc: IEceDocumentLoader); stdcall;
  end;

  IEceDocument = interface
    function UseHotkey(ctrl, shift, alt: BOOL; key: Word): BOOL; stdcall;
    function _GetHandle: HWND; safecall;
    procedure _BeginUpdate; safecall;
    procedure _EndUpdate; safecall;
    procedure _SetFocus; stdcall;
    procedure _KillFocus; stdcall;
    function GetFileName : string; stdcall;

    procedure _LoadFromFile(Const filename : string); stdcall;
  end;

  IEceDocumentLoader = interface
    function GetName: string; stdcall;
    function GetTitle: string; stdcall;
    function CreateDocument(AApp : IEceApplication; AFileName: string; var IDoc: IEceDocument;
      var ErrResult: string): Boolean; stdcall;
  end;

  IEceLine = interface;
  IGutter = interface;
  ICaret = interface;

  IEceEditor = interface
    function _GetHandle: HWND; safecall;
    function _GetLinesCount: integer; safecall;
    function _GetLines(AIndex: integer): IEceLine; safecall;
    function _GetGutter: IGutter; safecall;
    function _GetCaret: ICaret; safecall;
    function _AddLine: IEceLine; safecall;
    function _InsertLine(Index: integer): IEceLine; safecall;
    procedure _DeleteLine(Index: integer); safecall;
  end;

  IEceLine = interface
    function _GetText: string; safecall;
    function _SetText(Text: string): integer; safecall;
    function _GetIndex: integer; safecall;
  end;

  IGutter = interface

  end;

  ICaret = interface
    function _GetX: integer; safecall;
    function _GetY: integer; safecall;
    function _SetX(value: integer): integer; safecall;
    function _SetY(value: integer): integer; safecall;
  end;

  IEcePlugin = interface
    function Load(App: IEceApplication): Boolean; safecall;
    procedure UnLoad(App: IEceApplication); safecall;
  end;

  IEceEditorPlugin = interface
    function Load(Editor: IEceEditor): Boolean; safecall;
  end;

implementation

initialization

{ InitializeCriticalSection(SyncObject); }
finalization

end.
