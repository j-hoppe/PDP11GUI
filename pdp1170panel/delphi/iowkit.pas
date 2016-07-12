//
// IO-Warrior kit library V1.5 include file
//

unit iowkit;

// activate this if you want to explicitly load the DLL
{$DEFINE LINK_ON_REQUEST}

interface

uses
  Windows;

const
  // IoWarrior vendor & product IDs
  IOWKIT_VENDOR_ID        = $07c0;
  IOWKIT_VID              = IOWKIT_VENDOR_ID;

  // IO-Warrior 40
  IOWKIT_PRODUCT_ID_IOW40 = $1500;
  IOWKIT_PID_IOW40        = IOWKIT_PRODUCT_ID_IOW40;

  // IO-Warrior 24
  IOWKIT_PRODUCT_ID_IOW24 = $1501;
  IOWKIT_PID_IOW24        = IOWKIT_PRODUCT_ID_IOW24;

  // IO-Warrior PowerVampire
  IOWKIT_PRODUCT_ID_IOWPV1 = $1511;
  IOWKIT_PID_IOWPV1        = IOWKIT_PRODUCT_ID_IOWPV1;
  IOWKIT_PRODUCT_ID_IOWPV2 = $1512;
  IOWKIT_PID_IOWPV2        = IOWKIT_PRODUCT_ID_IOWPV2;

  // IO-Warrior 56
  IOWKIT_PRODUCT_ID_IOW56  = $1503;
  IOWKIT_PID_IOW56         = IOWKIT_PRODUCT_ID_IOW56;

  // Max number of pipes per IOW device
  IOWKIT_MAX_PIPES   = 2;

  // pipe names
  IOW_PIPE_IO_PINS      = 0;
  IOW_PIPE_SPECIAL_MODE = 1;

  // Max number of IOW devices in system
  IOWKIT_MAX_DEVICES = 16;

  // IOW Legacy devices open modes
  IOW_OPEN_SIMPLE    = 1;
  IOW_OPEN_COMPLEX   = 2;

  // first IO-Warrior revision with serial numbers
  IOW_NON_LEGACY_REVISION = $1010;

type
  PIOWKIT_REPORT = ^IOWKIT_REPORT;
  IOWKIT_REPORT = packed record
    ReportID: Byte;
  case Boolean of
    False: (Value: DWORD;);
    True: (Bytes: array [0..3] of Byte;);
  end;

  PIOWKIT40_IO_REPORT = ^IOWKIT40_IO_REPORT;
  IOWKIT40_IO_REPORT = packed record
    ReportID: Byte;
  case Boolean of
    False: (Value: DWORD;);
    True: (Bytes: array [0..3] of Byte;);
  end;

  PIOWKIT24_IO_REPORT = ^IOWKIT24_IO_REPORT;
  IOWKIT24_IO_REPORT = packed record
    ReportID: Byte;
  case Boolean of
    False: (Value: WORD;);
    True: (Bytes: array [0..1] of Byte;);
  end;

  PIOWKIT_SPECIAL_REPORT = ^IOWKIT_SPECIAL_REPORT;
  IOWKIT_SPECIAL_REPORT = packed record
    ReportID: Byte;
    Bytes: array [0..6] of Byte;
  end;

  PIOWKIT56_IO_REPORT = ^IOWKIT56_IO_REPORT;
  IOWKIT56_IO_REPORT = packed record
    ReportID: Byte;
    Bytes: array [0..6] of Byte;
  end;

  PIOWKIT56_SPECIAL_REPORT = ^IOWKIT56_SPECIAL_REPORT;
  IOWKIT56_SPECIAL_REPORT = packed record
    ReportID: Byte;
    Bytes: array [0..62] of Byte;
  end;

const
  IOWKIT_REPORT_SIZE = SizeOf(IOWKIT_REPORT);
  IOWKIT40_IO_REPORT_SIZE = SizeOf(IOWKIT40_IO_REPORT);
  IOWKIT24_IO_REPORT_SIZE = SizeOf(IOWKIT24_IO_REPORT);
  IOWKIT_SPECIAL_REPORT_SIZE = SizeOf(IOWKIT_SPECIAL_REPORT);
  IOWKIT56_IO_REPORT_SIZE = SizeOf(IOWKIT56_IO_REPORT);
  IOWKIT56_SPECIAL_REPORT_SIZE = SizeOf(IOWKIT56_SPECIAL_REPORT);

type
  // Opaque IO-Warrior handle
  IOWKIT_HANDLE = Pointer;

{$IFDEF LINK_ON_REQUEST}

type
  TIowKitOpenDevice = function: IOWKIT_HANDLE; stdcall;
  TIowKitCloseDevice = procedure(devHandle: IOWKIT_HANDLE); stdcall;
  TIowKitWrite = function(devHandle: IOWKIT_HANDLE; numPipe: ULONG;
    buffer: PChar; length: ULONG): ULONG; stdcall;
  TIowKitRead = function(devHandle: IOWKIT_HANDLE; numPipe: ULONG;
    buffer: PChar; length: ULONG): ULONG; stdcall;
  TIowKitReadNonBlocking = function(devHandle: IOWKIT_HANDLE; numPipe: ULONG;
    buffer: PChar; length: ULONG): ULONG; stdcall;
  TIowKitReadImmediate = function(devHandle: IOWKIT_HANDLE; var value: DWORD): BOOL; stdcall;
  TIowKitGetNumDevs = function: ULONG; stdcall;
  TIowKitGetDeviceHandle = function(numDevice: ULONG): IOWKIT_HANDLE; stdcall;
  TIowKitSetLegacyOpenMode = function(legacyOpenMode: ULONG): BOOL; stdcall;
  TIowKitGetProductId = function(devHandle: IOWKIT_HANDLE): ULONG; stdcall;
  TIowKitGetRevision = function(devHandle: IOWKIT_HANDLE): ULONG; stdcall;
  TIowKitGetThreadHandle = function(devHandle: IOWKIT_HANDLE): THandle; stdcall;
  TIowKitGetSerialNumber = function(devHandle: IOWKIT_HANDLE; serialNumber: PWideChar): BOOL; stdcall;
  TIowKitSetTimeout = function(devHandle: IOWKIT_HANDLE; timeout: ULONG): BOOL; stdcall;
  TIowKitSetWriteTimeout = function(devHandle: IOWKIT_HANDLE; timeout: ULONG): BOOL; stdcall;
  TIowKitCancelIo = function(devHandle: IOWKIT_HANDLE; numPipe: ULONG): BOOL; stdcall;
  TIowKitVersion = function: PChar; stdcall;

var
  IowKitOpenDevice: TIowKitOpenDevice;
  IowKitCloseDevice: TIowKitCloseDevice;
  IowKitWrite: TIowKitWrite;
  IowKitRead: TIowKitRead;
  IowKitReadNonBlocking: TIowKitReadNonBlocking;
  IowKitReadImmediate: TIowKitReadImmediate;
  IowKitGetNumDevs: TIowKitGetNumDevs;
  IowKitGetDeviceHandle: TIowKitGetDeviceHandle;
  IowKitSetLegacyOpenMode: TIowKitSetLegacyOpenMode;
  IowKitGetProductId: TIowKitGetProductId;
  IowKitGetRevision: TIowKitGetRevision;
  IowKitGetThreadHandle: TIowKitGetThreadHandle;
  IowKitGetSerialNumber: TIowKitGetSerialNumber;
  IowKitSetTimeout: TIowKitSetTimeout;
  IowKitSetWriteTimeout: TIowKitSetWriteTimeout;
  IowKitCancelIo: TIowKitCancelIo;
  IowKitVersion: TIowKitVersion;

{$ELSE}

function IowKitOpenDevice: IOWKIT_HANDLE; stdcall;
procedure IowKitCloseDevice(devHandle: IOWKIT_HANDLE); stdcall;
function IowKitWrite(devHandle: IOWKIT_HANDLE; numPipe: ULONG;
  buffer: PChar; length: ULONG): ULONG; stdcall;
function IowKitRead(devHandle: IOWKIT_HANDLE; numPipe: ULONG;
  buffer: PChar; length: ULONG): ULONG; stdcall;
function IowKitReadNonBlocking(devHandle: IOWKIT_HANDLE; numPipe: ULONG;
  buffer: PChar; length: ULONG): ULONG; stdcall;
function IowKitReadImmediate(devHandle: IOWKIT_HANDLE; var value: DWORD): BOOL; stdcall;
function IowKitGetNumDevs: ULONG; stdcall;
function IowKitGetDeviceHandle(numDevice: ULONG): IOWKIT_HANDLE; stdcall;
function IowKitSetLegacyOpenMode(legacyOpenMode: ULONG): BOOL; stdcall;
function IowKitGetProductId(devHandle: IOWKIT_HANDLE): ULONG; stdcall;
function IowKitGetRevision(devHandle: IOWKIT_HANDLE): ULONG; stdcall;
function IowKitGetThreadHandle(devHandle: IOWKIT_HANDLE): THandle; stdcall;
function IowKitGetSerialNumber(devHandle: IOWKIT_HANDLE; serialNumber: PWideChar): BOOL; stdcall;
function IowKitSetTimeout(devHandle: IOWKIT_HANDLE; timeout: ULONG): BOOL; stdcall;
function IowKitSetWriteTimeout(devHandle: IOWKIT_HANDLE; timeout: ULONG): BOOL; stdcall;
function IowKitCancelIo(devHandle: IOWKIT_HANDLE; numPipe: ULONG): BOOL; stdcall;
function IowKitVersion: PChar; stdcall;

{$ENDIF LINK_ON_REQUEST}

function LoadIowKitAPI: Boolean;
procedure UnloadIowKitAPI;

implementation

const
  IOWKITDllName = 'iowkit.dll';

{$IFDEF LINK_ON_REQUEST}

var
  IowKitDLLHandle: THandle = 0;

function LoadIowKitAPI: Boolean;
begin
  if IowKitDLLHandle = 0 then
  begin
    IowKitDLLHandle := LoadLibrary(IOWKITDllName);
    Result := IowKitDLLHandle <> 0;
    if Result then
    begin
      IowKitOpenDevice := GetProcAddress(IowKitDllHandle, 'IowKitOpenDevice');
      IowKitCloseDevice := GetProcAddress(IowKitDllHandle, 'IowKitCloseDevice');
      IowKitWrite := GetProcAddress(IowKitDllHandle, 'IowKitWrite');
      IowKitRead := GetProcAddress(IowKitDllHandle, 'IowKitRead');
      IowKitReadNonBlocking := GetProcAddress(IowKitDllHandle, 'IowKitReadNonBlocking');
      IowKitReadImmediate := GetProcAddress(IowKitDllHandle, 'IowKitReadImmediate');
      IowKitGetNumDevs := GetProcAddress(IowKitDllHandle, 'IowKitGetNumDevs');
      IowKitGetDeviceHandle := GetProcAddress(IowKitDllHandle, 'IowKitGetDeviceHandle');
      IowKitSetLegacyOpenMode := GetProcAddress(IowKitDllHandle, 'IowKitSetLegacyOpenMode');
      IowKitGetProductId := GetProcAddress(IowKitDllHandle, 'IowKitGetProductId');
      IowKitGetRevision := GetProcAddress(IowKitDllHandle, 'IowKitGetRevision');
      IowKitGetThreadHandle := GetProcAddress(IowKitDllHandle, 'IowKitGetThreadHandle');
      IowKitGetSerialNumber := GetProcAddress(IowKitDllHandle, 'IowKitGetSerialNumber');
      IowKitSetTimeout := GetProcAddress(IowKitDllHandle, 'IowKitSetTimeout');
      IowKitSetWriteTimeout := GetProcAddress(IowKitDllHandle, 'IowKitSetWriteTimeout');
      IowKitCancelIo := GetProcAddress(IowKitDllHandle, 'IowKitCancelIo');
      IowKitVersion := GetProcAddress(IowKitDllHandle, 'IowKitVersion');
    end;
  end
  else
    Result := True;
end;

procedure UnloadIowKitAPI;
begin
  if IowKitDLLHandle <> 0 then
  begin
    FreeLibrary(IowKitDLLHandle);
    IowKitDLLHandle := 0;
    IowKitOpenDevice := nil;
    IowKitCloseDevice := nil;
    IowKitWrite := nil;
    IowKitRead := nil;
    IowKitReadNonBlocking := nil;
    IowKitReadImmediate := nil;
    IowKitGetNumDevs := nil;
    IowKitGetDeviceHandle := nil;
    IowKitSetLegacyOpenMode := nil;
    IowKitGetProductId := nil;
    IowKitGetRevision := nil;
    IowKitGetThreadHandle := nil;
    IowKitGetSerialNumber := nil;
    IowKitSetTimeout := nil;
    IowKitSetWriteTimeout := nil;
    IowKitCancelIo := nil;
    IowKitVersion := nil;
  end;
end;

{$ELSE}

function LoadIowKitAPI: Boolean;
begin
  Result := True;
end;

procedure UnloadIowKitAPI;
begin
end;

function IowKitOpenDevice; external IOWKITDllName name 'IowKitOpenDevice';
procedure IowKitCloseDevice; external IOWKITDllName name 'IowKitCloseDevice';
function IowKitWrite; external IOWKITDllName name 'IowKitWrite';
function IowKitRead; external IOWKITDllName name 'IowKitRead';
function IowKitReadNonBlocking; external IOWKITDllName name 'IowKitReadNonBlocking';
function IowKitReadImmediate; external IOWKITDllName name 'IowKitReadImmediate';
function IowKitGetNumDevs; external IOWKITDllName name 'IowKitGetNumDevs';
function IowKitGetDeviceHandle; external IOWKITDllName name 'IowKitGetDeviceHandle';
function IowKitSetLegacyOpenMode; external IOWKITDllName name 'IowKitSetLegacyOpenMode';
function IowKitGetProductId; external IOWKITDllName name 'IowKitGetProductId';
function IowKitGetRevision; external IOWKITDllName name 'IowKitGetRevision';
function IowKitGetThreadHandle; external IOWKITDllName name 'IowKitGetThreadHandle';
function IowKitGetSerialNumber; external IOWKITDllName name 'IowKitGetSerialNumber';
function IowKitSetTimeout; external IOWKITDllName name 'IowKitSetTimeout';
function IowKitSetWriteTimeout; external IOWKITDllName name 'IowKitSetWriteTimeout';
function IowKitCancelIo; external IOWKITDllName name 'IowKitCancelIo';
function IowKitVersion; external IOWKITDllName name 'IowKitVersion';

{$ENDIF LINK_ON_REQUEST}

end.
