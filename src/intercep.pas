unit intercep;

interface
uses windows, dialogs, SysUtils;
type

  PImageDosHeader = ^TImageDosHeader;
  {EXTERNALSYM _IMAGE_DOS_HEADER}
  _IMAGE_DOS_HEADER = packed record { DOS .EXE header                  }
    e_magic: Word; { Magic number                     }
    e_cblp: Word; { Bytes on last page of file       }
    e_cp: Word; { Pages in file                    }
    e_crlc: Word; { Relocations                      }
    e_cparhdr: Word; { Size of header in paragraphs     }
    e_minalloc: Word; { Minimum extra paragraphs needed  }
    e_maxalloc: Word; { Maximum extra paragraphs needed  }
    e_ss: Word; { Initial (relative) SS value      }
    e_sp: Word; { Initial SP value                 }
    e_csum: Word; { Checksum                         }
    e_ip: Word; { Initial IP value                 }
    e_cs: Word; { Initial (relative) CS value      }
    e_lfarlc: Word; { File address of relocation table }
    e_ovno: Word; { Overlay number                   }
    e_res: array[0..3] of Word; { Reserved words                   }
    e_oemid: Word; { OEM identifier (for e_oeminfo)   }
    e_oeminfo: Word; { OEM information; e_oemid specific}
    e_res2: array[0..9] of Word; { Reserved words                   }
    e_lfanew: LongInt; { File address of new exe header   }
  end;
  TImageDosHeader = _IMAGE_DOS_HEADER;
  //  {$EXTERNALSYM IMAGE_DOS_HEADER}

  IMAGE_DOS_HEADER = _IMAGE_DOS_HEADER;

  //***************************************
  PImageDataDirectory = ^TImageDataDirectory;
  _IMAGE_DATA_DIRECTORY = record
    VirtualAddress: DWORD;
    Size: DWORD;
  end;
  //  {$EXTERNALSYM _IMAGE_DATA_DIRECTORY}
  TImageDataDirectory = _IMAGE_DATA_DIRECTORY;
  IMAGE_DATA_DIRECTORY = _IMAGE_DATA_DIRECTORY;
  //  {$EXTERNALSYM IMAGE_DATA_DIRECTORY}

  //*************
  PImageOptionalHeader = ^TImageOptionalHeader;
  _IMAGE_OPTIONAL_HEADER = packed record
    { Standard fields. }
    Magic: Word;
    MajorLinkerVersion: Byte;
    MinorLinkerVersion: Byte;
    SizeOfCode: DWORD;
    SizeOfInitializedData: DWORD;
    SizeOfUninitializedData: DWORD;
    AddressOfEntryPoint: DWORD;
    BaseOfCode: DWORD;
    BaseOfData: DWORD;
    { NT additional fields. }
    ImageBase: DWORD;
    SectionAlignment: DWORD;
    FileAlignment: DWORD;
    MajorOperatingSystemVersion: Word;
    MinorOperatingSystemVersion: Word;
    MajorImageVersion: Word;
    MinorImageVersion: Word;
    MajorSubsystemVersion: Word;
    MinorSubsystemVersion: Word;
    Win32VersionValue: DWORD;
    SizeOfImage: DWORD;
    SizeOfHeaders: DWORD;
    CheckSum: DWORD;
    Subsystem: Word;
    DllCharacteristics: Word;
    SizeOfStackReserve: DWORD;
    SizeOfStackCommit: DWORD;
    SizeOfHeapReserve: DWORD;
    SizeOfHeapCommit: DWORD;
    LoaderFlags: DWORD;
    NumberOfRvaAndSizes: DWORD;
    DataDirectory: packed array[0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES - 1] of
      TImageDataDirectory;
  end;
  // {$EXTERNALSYM _IMAGE_OPTIONAL_HEADER}
  TImageOptionalHeader = _IMAGE_OPTIONAL_HEADER;
  IMAGE_OPTIONAL_HEADER = _IMAGE_OPTIONAL_HEADER;
  // {$EXTERNALSYM IMAGE_OPTIONAL_HEADER}

  PImageNtHeaders = ^TImageNtHeaders;
  _IMAGE_NT_HEADERS = packed record
    Signature: DWORD;
    FileHeader: TImageFileHeader;
    OptionalHeader: TImageOptionalHeader;
  end;
  // {$EXTERNALSYM _IMAGE_NT_HEADERS}
  TImageNtHeaders = _IMAGE_NT_HEADERS;
  IMAGE_NT_HEADERS = _IMAGE_NT_HEADERS;
  // {$EXTERNALSYM IMAGE_NT_HEADERS}

  PImage_import_by_name = ^TImage_import_by_mame;
  _IMAGE_IMPORT_BY_NAME = packed record

    Hint: Word;
    Name: Byte;
  end;
  TImage_import_by_mame = _IMAGE_IMPORT_BY_NAME;

  _u1 = packed record
    case Integer of

      0: (ForwarderString: PByte);
      1: (Functionn: PDWORD);
      2: (Ordinal: DWORD);
      3: (AddressOfData: PImage_import_by_name);
  end;
  PImageThunkData = ^TImageThunkData;
  _IMAGE_THUNK_DATA = packed record

    u1: _u1;
  end;
  TImageThunkData = _IMAGE_THUNK_DATA;
  IMAGE_THUNK_DATA = _IMAGE_THUNK_DATA;

  _temp_charcteristics = record
    case Integer of
      0: (Characteristics: DWORD); // 0 for terminating null import descriptor
      1: (OriginalFirstThunk: PImageThunkData); // RVA to original unbound IAT
  end;

  PImageImportDescriptor = ^TImageImportDescriptor;
  _IMAGE_IMPORT_DESCRIPTOR = packed record

    t: _temp_charcteristics;
    TimeDateStamp: DWord; // 0 if not bound,
    // -1 if bound, and real date\time stamp
    //     in IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT (new BIND)
    // O.W. date/time stamp of DLL bound to (Old BIND)

    ForwarderChain: DWORD; // -1 if no forwarders
    Name: DWORD;
    FirstThunk: PImageThunkData;
      // RVA to IAT (if bound this IAT has actual addresses)
  end;
  TImageImportDescriptor = _IMAGE_IMPORT_DESCRIPTOR;
  IMAGE_IMPORT_DESCRIPTOR = _IMAGE_IMPORT_DESCRIPTOR;
  PPointer = ^Pointer;
function InterceptDllCall(

  hLocalModule: HModule;
  c_szDllName: Pchar;
  c_szApiName: PChar;
  pApiNew: Pointer;
  p_pApiOrg: PPointer;
  pApiToChange: Pointer): Boolean;
implementation

function MakePtr(base: Dword; Offset: DWORD): Pointer;
begin
  Result := Pointer(Base + Offset);
end;

function InterceptDllCall(

  hLocalModule: HModule;
  c_szDllName: Pchar;
  c_szApiName: PChar;
  pApiNew: Pointer;
  p_pApiOrg: PPointer;
  pApiToChange: Pointer): Boolean;

var
  pDosHeader: PImageDosHeader;

  pNtHeader: PImageNtHeaders;
  PImportDesc: PImageImportDescriptor;
  dwProtect: DWORD;
  dwNewProtect: DWORD;
  dwAddressToInterCept: DWORD;
  pThunk: PImageThunkData;
begin
  pDosHeader := PImageDosHeader(hLocalModule);
  Result := False;
  if (pApiToChange <> nil) then
    dwAddressToIntercept := DWORD(pApiToChange)
  else
    dwAddressToIntercept := Dword(GetProcAddress(GetModuleHandle(c_szDllName),
      c_szApiName));

  if IsBadReadPtr(Pointer(hLocalModule), sizeof(PImageNtHeaders)) then
    Exit;

  if pDosHeader.e_magic <> IMAGE_DOS_SIGNATURE then
    exit;
  pNtHeader := PImageNtHeaders(MakePtr(DWord(pDOSHeader),
    DWord(pDOSHeader.e_lfanew)));
  if pNTHeader.signature <> IMAGE_NT_SIGNATURE then
    exit;
  pImportDesc := PImageImportDescriptor(

    MakePtr(hLocalModule,
    pNtHeader.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT].VirtualAddress));

  if (PImportDesc = PImageImportDescriptor(pNtHeader)) then
    exit;

  while (pImportDesc.Name > 0) do
  begin

    pThunk := PImageThunkData(MakePtr(DWord(hLocalModule),
      Dword(pImportDesc.FirstThunk)));
    while (pThunk.u1.Functionn <> nil) do
    begin
      if DWord(pThunk.u1.Functionn) = dwAddressToIntercept then
      begin
        if not IsBadWritePtr(Pointer(@pThunk.u1.Functionn), sizeof(DWORD)) then
        begin
          if (p_pApiOrg <> nil) then
            p_pApiOrg^ := Pointer(pThunk.u1.Functionn);
          pThunk.u1.Functionn := pApiNew;
          Result := True;
        end
        else
        begin
          if VirtualProtect(Pointer(@pThunk.u1.Functionn), sizeof(DWORD),
            PAGE_EXECUTE_READWRITE, @dwProtect) then
          begin
            if (p_pApiOrg <> nil) then
              p_pApiOrg^ := Pointer(pThunk.u1.Functionn);
            pThunk.u1.Functionn := PDWORD(pApiNew);
            Result := True;
            dwNewProtect := dwProtect;
            VirtualProtect(Pointer(@pThunk.u1.Functionn), sizeof(DWORD),
              dwNewProtect, @dwProtect);
          end;
        end;
      end;
      Inc(PThunk);
    end;
    Inc(pImportDEsc);
  end;
end;

end.

// Project1.dpr

program Project1;

uses
  Forms,
  Unit1 in '..\..\Work\Temp\4\Unit1.pas' {Form1};

{$R *.RES}

begin

  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

// Unit1.dfm

object Form1: TForm1

  Left = 192
    Top = 107
    Width = 435
    Height = 300
    Caption = 'Form1'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    OldCreateOrder = False
    PixelsPerInch = 96
    TextHeight = 13
    object Button1: TButton
    Left = 72
      Top = 176
      Width = 273
      Height = 65
      Caption = 'Begin'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -24
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = Button1Click
  end
  object Button2: TButton
    Left = 304
      Top = 16
      Width = 105
      Height = 49
      Caption = 'MessageBox'
      TabOrder = 1
      OnClick = Button2Click
  end
end

// Unit1.pas

unit Unit1;

interface

uses

  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, intercep;

type

  TMyProc = function(hWnd: HWND; lpText, lpCaption: PAnsiChar; uType: UINT):
    Integer; stdcall;
  PTMyProc = ^TMyProc;

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var

  Form1: TForm1;
  myProc: PTMyProc;
implementation

function MyMessageBox(hWnd: HWND; lpText, lpCaption: PAnsiChar; uType: UINT):
  Integer; stdcall;
begin
  ShowMessage('Message intercepted');

  result := IDOK;
end;
{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);

begin
  myProc := nil;
  MessageBox(0, 'Hello', 'Message 1', MB_OK);
  InterceptDllCall(hInstance, 'user32.dll', 'MessageBoxA',
    Pointer(@MyMessageBox), PPointer(@myProc), nil); //then ShowMessage('Ok');
  MessageBox(0, 'Hello', 'Message 2', MB_OK);
  InterceptDllCall(hInstance, 'user32.dll', 'MessageBoxA',
    Pointer(myProc), nil, Pointer(@MyMessageBox));
  MessageBox(0, 'Hello', 'Message 3 ', MB_OK);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  MessageBox(0, 'Hello', 'Message 4 ', MB_OK);
end;

end.

