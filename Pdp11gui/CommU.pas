unit CommU;
{
   Copyright (c) 2016, Joerg Hoppe
   j_hoppe@t-online.de, www.retrocmp.com

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the "Software"),
   to deal in the Software without restriction, including without limitation
   the rights to use, copy, modify, merge, publish, distribute, sublicense,
   and/or sell copies of the Software, and to permit persons to whom the
   Software is furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
   JOERG HOPPE BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
   IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}

// Einfachst moegliche Ansteuerung des COM-Ports

interface

uses
  Windows,
  Classes,
  ExtCtrls,
  SysUtils,
  Forms,
  Dialogs,
  Graphics,
  Controls,
  Buttons,
  StdCtrls;

type
  TComm = class(TComponent)

    private
      { Private declarations }
      fHandle: THandle; {handle from OpenComm }
      fPort: Integer; { port #, 1-based }
      fBaud: LongInt; { baud rate }
      fDataBits: Byte ; { one of 5,6,7,8 }
      fParity: Byte ; { one of NOPARITY, ODD..., EVEN... MARK..., SPACE... }
      fStopBits: Byte; { one of ONESTOPBIT, ONE5STOPBITS, TWOSTOPBITS }

      fRtsOn: boolean;
      fDtrOn: boolean;

      function isOpen: boolean;
      procedure setBaud(BaudToSet: LongInt);
      procedure setDataBits(DatabitsToSet: Byte);
      procedure setParity(ParityToSet: Byte);
      procedure setStopBits(StopBitsToSet: Byte);
      procedure setPort(PortToSet: Integer);
      procedure setRtsOn(OnOff: boolean);
      procedure setDtrOn(OnOff: boolean);
      function getInCount: LongInt;
      function getOutCount: LongInt;

    protected
      { Protected declarations }

    public
      { Public declarations }
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      function Open: boolean;
      function WriteByte(ch: byte): boolean;
      function WriteData(var data; size: Integer): boolean;
      function ReadByte(var b: byte): boolean ;
      function ReadData(var data; size: Integer): Integer;
      procedure Flush;
      procedure Close;

    published
      { Published declarations }
      property Port: Integer read fPort write setPort;
      property Baud: LongInt read fBaud write setBaud;
      property DataBits: Byte read fDataBits write setDataBits;
      property Parity: Byte read fParity write setParity;
      property StopBits: Byte read fStopBits write setStopBits;
      property InCount: LongInt read getInCount;                              // number of characters received
      property OutCount: LongInt read getOutCount;                    // number of characters pending on transmit
      property Active: boolean read isOpen;                                                   // is port open
      property RtsOn: boolean read fRtsOn write setRtsOn;
      property DtrOn: boolean read fDtrOn write setDtrOn;
    end{ "TYPE TComm = class(TComponent)" } ;

// procedure Register;

function MsecTime: LongInt;
procedure Delay(msec: LongInt);

implementation

// Register the component with the Delphi IDE
// procedure Register;
//   begin
//    RegisterComponents('SUP', [TComm]);
//  end;

function MsecTime: LongInt;
  var
    Present: TDateTime;
    Hour, Min, Sec, msec: Word;
  begin
    Present := Now;
    DecodeTime(Present, Hour, Min, Sec, msec);
    Result := ((((Hour * 60) + Min) * 60) + Sec) * 1000 + msec;
  end;

procedure Delay(msec: LongInt);
  var
    nTimeOut: LongInt;
  begin
    nTimeOut := MsecTime + msec;
    while MsecTime < nTimeOut do
      Application.ProcessMessages();
  end;

// Component constructor
constructor TComm.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    // set default property values
    fHandle := INVALID_HANDLE_VALUE;
    fPort := 1;
    fBaud := 9600;
    fDataBits := 8 ;
    fParity := NOPARITY ;
    fStopBits := ONESTOPBIT ;
    fRtsOn  := false;
    fDtrOn := false;
  end;

// Component destructor
destructor TComm.Destroy;
  begin
    // close the com port (if open)
    Close;
    inherited Destroy;
  end;

// Return True if port is open
function TComm.isOpen: boolean;
  begin
    Result := (fHandle <> INVALID_HANDLE_VALUE);
  end;

// Set the baud rate property
procedure TComm.setBaud(BaudToSet: LongInt);
  begin
    if BaudToSet <> fBaud then begin
      fBaud := BaudToSet;
      // if port is open, then close it and then reopen it
      // to reset the baud rate
      if isOpen then begin
        Close;
        Open;
      end;
    end;
  end;

  // Set the DataBits property
procedure TComm.setDataBits(DatabitsToSet: Byte);
  begin
    if Databits <> fDataBits then begin
      fDataBits := Databits;
      // if port is open, then close it and then reopen it
      // to reset the baud rate
      if isOpen then begin
        Close;
        Open;
      end;
    end;
  end;


  // Set the Parity property
procedure TComm.setParity(ParityToSet: Byte);
  begin
    if ParityToSet <> fParity then begin
      fParity := ParityToSet;
      // if port is open, then close it and then reopen it
      // to reset the baud rate
      if isOpen then begin
        Close;
        Open;
      end;
    end;
  end;

  // Set the StopBits property
procedure TComm.setStopBits(StopBitsToSet: Byte);
  begin
    if StopBitsToSet <> fStopBits then begin
      fStopBits := StopBitsToSet;
      // if port is open, then close it and then reopen it
      // to reset the baud rate
      if isOpen then begin
        Close;
        Open;
      end;
    end;
  end;

// Set the Port property
procedure TComm.setPort(PortToSet: Integer);
  begin
    if PortToSet <> fPort then begin
      fPort := PortToSet;
      // if port was open, then close and reopen it
      if isOpen then begin
        Close;
        Open;
      end;
    end;
  end;

procedure TComm.setRtsOn(OnOff: boolean);
  begin
    fRtsOn := OnOff;
    if isOpen then begin
        if OnOff then
          EscapeCommFunction(fHandle, SETRTS)
        else EscapeCommFunction(fHandle, CLRRTS);
      end;
  end;

procedure TComm.setDtrOn(OnOff: boolean);
  begin
    fDtrOn := OnOff;
    if isOpen then begin
      if OnOff then
        EscapeCommFunction(fHandle, SETDTR)
      else EscapeCommFunction(fHandle, CLRDTR);
    end;
  end;

// Opens the COM port, returns True if ok
function TComm.Open: boolean;
  var
    sCom: String;
    dcbPort: TDCB;  // device control block
  begin

    // close port if open already
    if isOpen then Close;

    // try to open the port
    sCom := '\\.\COM' + IntToStr(fPort);
    fHandle := CreateFile(PChar(sCom), GENERIC_READ or GENERIC_WRITE, 0,
            nil,
            OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, LongInt(0));

    // set the baud rate and other parameters
    if fHandle <> INVALID_HANDLE_VALUE then begin
      if GetCommState(fHandle, dcbPort) then begin
        // fill in the fields of the structure
        dcbPort.BaudRate := fBaud;
        dcbPort.ByteSize := fDataBits;
        dcbPort.Parity := fParity;
        dcbPort.StopBits := fStopBits;
        dcbPort.Flags := 0;
        { flag bit fields:
        dcb_Binary, dcb_Parity, dcb_OutxCtsFlow, dcb_fOutxDsrFlow,
        dcb_fOutX, dcb_fInX, dcb_DtrFlow, dcb_RtsFlow
        }
        SetCommState(fHandle, dcbPort);
      end{ "if GetCommState(fHandle, dcbPort)" } ;
      setRtsOn(fRtsOn);
      setDtrOn(fDtrOn);
    end{ "if fHandle <> INVALID_HANDLE_VALUE" } ;

    // return True if port opened
    Result := isOpen;
  end{ "function TComm.Open" } ;

// Close the COM port
procedure TComm.Close;
  begin
    if isOpen then begin
      CloseHandle(fHandle);
      fHandle := INVALID_HANDLE_VALUE;
    end;
  end;

// Write a char out the COM port
function TComm.WriteByte(ch: byte): boolean;
  var
    dwCharsWritten: DWord;
  begin
    dwCharsWritten := 0;
    if isOpen then begin
      WriteFile(fHandle, ch, sizeof(ch), dwCharsWritten, nil);
    end;
    Result := dwCharsWritten = sizeof(ch);
  end;

function TComm.WriteData(var data; size: Integer): boolean;
  var
    dwCharsWritten: DWord;
  begin
    dwCharsWritten := 0;
    if isOpen then begin
      WriteFile(fHandle, data, size, dwCharsWritten, nil);
    end;
    Result := dwCharsWritten = size;
  end;

  (*
// Reads a character from the port
function TComm.ReadByte: byte;
  var
    cbCharsAvailable, cbCharsRead: DWord;
    ch: byte;
  begin
    ch := Ord(' ');
    if isOpen then begin
      cbCharsAvailable := getInCount;
      if cbCharsAvailable > 0 then begin
        ReadFile(fHandle, ch, sizeof(ch), cbCharsRead, nil);
      end;
    end;
    Result := ch;
  end{ "function TComm.ReadByte" } ;
*)
// Reads a character from the port
function TComm.ReadByte(var b: byte): boolean ;
  var
    cbCharsAvailable, cbCharsRead: DWord;
  begin
  result := false ;
    if isOpen then begin
      cbCharsAvailable := getInCount;
      if cbCharsAvailable > 0 then begin
        ReadFile(fHandle, b, 1, cbCharsRead, nil);
        result := true ;
      end;
    end;
  end{ "function TComm.ReadByte" } ;

function TComm.ReadData(var data; size: Integer): Integer;
  var
    cbCharsAvailable, cbCharsRead: DWord;
  begin
    cbCharsRead := 0;
    if isOpen then begin
      cbCharsAvailable := getInCount;
      if cbCharsAvailable > 0 then begin
        if cbCharsAvailable < size then
          size := cbCharsAvailable;
        ReadFile(fHandle, data, size, cbCharsRead, nil);
      end;
    end;
    Result := cbCharsRead;
  end{ "function TComm.ReadData" } ;

// Return the number of bytes waiting in the input queue
function TComm.getInCount: LongInt;
  var
    statPort: TCOMSTAT;
    dwErrorCode: DWord;
  begin
    Result := 0;
    if isOpen then begin
      ClearCommError(fHandle, dwErrorCode, @statPort);
      Result := statPort.cbInQue;
    end;
  end;

// Return the number of bytes waiting in the output queue
function TComm.getOutCount: LongInt;
  var
    statPort: TCOMSTAT;
    dwErrorCode: DWord;
  begin
    Result := 0;
    if isOpen then begin
      ClearCommError(fHandle, dwErrorCode, @statPort);
      Result := statPort.cbOutQue;
    end;
  end;

// Flush the port by reading any characters in the queue
procedure TComm.Flush;
  begin
    if fHandle <> INVALID_HANDLE_VALUE then begin
      FlushFileBuffers(fHandle);
      //PurgeComm(FHandle, PURGE_TXABORT or PURGE_RXABORT or PURGE_TXCLEAR or PURGE_RXCLEAR);
    end;
  end;

end{ "unit CommU" } .
