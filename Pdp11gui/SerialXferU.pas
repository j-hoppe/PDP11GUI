unit SerialXferU;
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


{
  Klasse, um Daten zwischen PDP11 und PDP11GUI über ein
  serielles Protokoll zu übertragen.
  Ein spezieller Treiber muss in die PDP-11 geladen wurden sein.

  Layout der Datenblocks siehe pdp11gui_main.mac:
  <len> <checksum> <opcode> [<block0> [<block1>]]
  mit <block> = <len> <data words,...>
}
interface

uses
  Windows,
  SysUtils,
  Forms,
  AuxU,
  AddressU,
  MemoryCellU ;

const
  max_charbuffer_len = $10000 ; // 64K ist genug für eine 16 bit machine
  max_xmitbuffer_len = $8000 ; // halb so gross, immer noch viel zu viel

  // die Opcodes für den Treiber, wie in pdp11gui_main
  // see OpcodeAsText
  SerialTransferDriver_opecho  = 1 ; // echo block, with inverted data.
  SerialTransferDriver_opinit  = 2 ; // init disk controller
  SerialTransferDriver_opread  = 3 ; // read multiple sectors in one track
  SerialTransferDriver_opchek  = 4 ; // check      multiple sectors in one track
  SerialTransferDriver_opwrite = 5 ; // write       "  "   "
  SerialTransferDriver_ophalt  = 6 ; // halt the driver
  SerialTransferDriver_oprest  = 7 ; // execute a RESET

  // parameter word offsets in request block0, wie in pdp11gui_main
  // Same for all disk operations and disk types!
  SerialTransferDriver_prcba      = 0     ; // controller base address
  SerialTransferDriver_prunit     = 1     ; // unit number of disc: 0 or 1
  SerialTransferDriver_prflags = 2 ; // flags, drive dependent
  SerialTransferDriver_prwlen     = 3     ; // count of word to read/write
  SerialTransferDriver_prcyl      = 4     ; // cylinder, always starting with 0
  SerialTransferDriver_prhead     = 5     ; // head, always starting with 0
  SerialTransferDriver_prsect     = 6     ; // sector, always starting with 0
  // MSCP is different: starting block instead of cyl/head/sector
  SerialTransferDriver_prstb0  = 4 ;      // start block number lo word
  SerialTransferDriver_prstb1  = 5 ;      // start block number hi word

  // argument for ophalt
  SerialTransferDriver_prmnea      = 0     ; // monotr entry address

type

  // Fehler in der Übertragung
  ESerialTransferHardError = class(Exception) ;

  // Vom Driver gemeldeter Fehler. msg = "error xxxx, parm=yyy zzzz"
  ESerialTransferAppError = class(Exception) ;

  TSerialTransferStatusChangeEvent = procedure(info: string) of object ;

  TSerialTransfer = class(TObject)
    private

      // Charbuffer: speichert den Zeichenstrom, der XmitBuffer entspricht
      CharBufferLen: integer ; // soviele Zeichen sind zur Übertragung des Buffers nötig (inkl patch)
      CharBuffer: array[0..max_charbuffer_len+1] of char ;

      XmitBufferLen: integer ; //  buffer länge in word. Ohne Aufrundung auf Vielfaches von 3
      XmitBufferLenRoundedUp: integer ; // bei Senden: aufgerundet auf vielfaches von 3
      // Word buffer: speichert die übertragenen Words aus dem
      XmitBuffer: TArrayOfWord ;

      class procedure WordTriplet2CharOctet(aXmitBuffer: TArrayOfWord ; var WordReadPos: integer ;
              var aCharBuffer: array of char ; var CharWritePos: integer) ;

      class procedure CharOctet2WordTriplet(aCharBuffer: array of char ; var CharReadPos: integer ;
              var aXmitBuffer: TArrayOfWord ; var WordWritePos: integer) ;


      procedure EncodeWordBuffer2CharBuffer ; // WordBuffer -> CharBuffer, mit RLE encoding
      procedure DecodeCharBuffer2WordBuffer ; // CharBuffer -> WordBuffer
    public

      EnableRLECompression: boolean ; // wirkt auf EncodeWordBuffer2CharBuffer und DecodeCharBuffer2WordBuffer

      OnStatusChange: TSerialTransferStatusChangeEvent ;

      Aborted: boolean ; // kann vom caller auf true gesetz twerden, um sofort zu stoppen

      // die Datenblöcke 0 und 1. Längen dynamisch
      // Block 0: request = parameter
      //    response: error data, oder disk data
      // Block1 : bei opcode opwrite: disk data
      // reponse: never used
      // Anwendung setzt Länge (SetLength(), die Länge wird direkt übertragen.
      // in reponse wird wieder Länge gesetzt
      XmitBlock0Data: TArrayOfWord ;
      XmitBlock1Data: TArrayOfWord ;


      constructor Create ;
      procedure Execute (
              pdp11ProcTimeout_ms: dword ;
              var opcode: word // Funktion, die ausgeführt werden soll/ Status für Rückgabe
              ) ;// throws ESerialTransferHardError

      procedure ReceiveCharBuffer(timeout_ms: dword) ; // throws ESerialTransferHardError
      procedure TransmitCharBuffer ;// throws ESerialTransferHardError

      function DataBlockText(aBlock: TArrayOfWord): string ;

      procedure SelfTest(wordcount: integer; testpattern: integer) ;


      function OpcodeAsText(opcode: word):string ;
    end{ "TYPE TSerialTransfer = class(TObject)" } ;


implementation

uses
  OctalConst,
  SerialIoHUbU,
  FormMainU,
  FormTerminalU,
  FormExecuteU,
  ConsolePDP1144U ;


constructor TSerialTransfer.Create ;
  begin
    inherited ;
    OnStatusChange := nil ;
    EnableRLECompression := true ;
  end ;


function TSerialTransfer.OpcodeAsText(opcode: word):string ;
  begin
    case opcode of
      SerialTransferDriver_opecho: result := 'opecho' ;
      SerialTransferDriver_opinit: result := 'opinit' ;
      SerialTransferDriver_opread: result := 'opread' ;
      SerialTransferDriver_opchek: result := 'opchek' ;
      SerialTransferDriver_opwrite: result := 'opwrite' ;
      SerialTransferDriver_ophalt: result := 'ophalt' ;
      SerialTransferDriver_oprest: result := 'oprest' ;
      else
        result := Format('Illegal opcode %d', [opcode]) ;
    end;
  end{ "function TSerialTransfer.OpcodeAsText" } ;

// words des datablock in string
// idx = 0 oder 1
function  TSerialTransfer.DataBlockText(aBlock: TArrayOfWord): string ;
  var i, len: integer ;
  begin
    result := '' ;
    // block 0 als Textdarstellung
    len := length(aBlock) ;
//    result := Format('#=%d:', [len]) ;
    for i := 0 to len-1 do
      result := result + ' ' + Dword2OctalStr( aBlock[i], 16) ;
  end;

// 8 chars in 3 words wandeln
// chars werden ab charreadpos gelesen,
// words werden ab wordwritepos geschrieben
class procedure TSerialTransfer.WordTriplet2CharOctet(aXmitBuffer: TArrayOfWord ; var WordReadPos: integer ;
        var aCharBuffer: array of char ; var CharWritePos: integer) ;
// get word from XmitBuffer, or 0, if i > XmitBufferLen
  function getW(i:integer):word ;
    begin
      if i >= length(aXmitBuffer) then
        result := 0
      else result := aXmitBuffer[i] ;
    end;

  var
    tmp: int64 ;
    i: integer ;
  begin
    tmp := (int64(getW(WordReadPos+0)) shl 32)
            or (int64(getW(WordReadPos+1)) shl 16)
            or getW(WordReadPos+2) ;

    // 48bit -> 8 chars. char[0] ist das MSB
    for i := 7 downto 0 do begin
      aCharBuffer[CharWritePos+i] := char((tmp and $3f) + $20) ;
      tmp := tmp shr 6 ;
    end ;
    WordReadPos := WordReadPos + 3 ;
    CharWritePos := CharWritePos + 8 ;
  end { "procedure TSerialTransfer.WordTriplet2CharOctet" } ;


// 8 chars in 3 words wandeln
// chars werden ab charreadpos gelesen,
// words werden ab wordwritepos geschrieben
class procedure TSerialTransfer.CharOctet2WordTriplet(aCharBuffer: array of char ; var CharReadPos: integer ;
        var aXmitBuffer: TArrayOfWord ; var WordWritePos: integer) ;

  var
    tmp: int64 ; // 48 bits
    i: integer ;
    b: byte ;
  begin

    tmp := 0 ;
    // 8 chars -> 48bit. char [0] ist das MSB
    for i := 0 to 7 do begin
      if (CharReadPos + i) >= length(aCharBuffer) then
        raise Exception.CreateFmt('Internal error in CharOctet2WordTriplet(): CharBuffer[%d] addressed, length=%d',
                [CharReadPos +i, length(aCharBuffer)]) ;
      b := (ord(aCharBuffer[CharReadPos+i]) - $20) and $3f ;
      tmp := (tmp shl 6) or b ;
    end ;

    // 48bit -> 3 words. word[0] ist MSB
    for i := 2 downto 0 do begin
      if (WordWritePos+i) >= length(aXmitBuffer) then
        raise Exception.CreateFmt('Internal error in CharOctet2WordTriplet(): XmitBuffer[%d] addressed, length=%d',
                [WordWritePos+i, length(aXmitBuffer)]) ;
      aXmitBuffer[WordWritePos+i] := tmp and $ffff ;
      tmp := tmp shr 16 ;
    end;

    WordWritePos := WordWritePos + 3 ; // next word triplett
    CharReadPos := CharReadPos + 8 ; // next char octett
  end{ "procedure TSerialTransfer.CharOctet2WordTriplet" } ;



// mit RLE encoding
//
procedure TSerialTransfer.EncodeWordBuffer2CharBuffer ; // WordBuffer -> CharBuffer

  const
    // max_rle_blocklen = 64 ; // see calculation in pdp11gui_serialxfer.mac
    max_rle_blocklen = 256 ; // see calculation in pdp11gui_serialxfer.mac
  var
    i_w: integer ; // index im word buffer
    i_c: integer ; // index im char buffer
    rle_count, rle_val: word ; // rle = run length encoding
   char_delay_us, rle_delay_us : integer ;
  begin
    // CharBuffer hat feste Länge, ist nicht dynamisch
    i_w := 0 ;
    i_c := 0 ;
    while i_w < XmitBufferLen do begin
      // encode a word triplett to a byte octet
      // RLE encoding möglich?
      // finde block len gleicher werte ab i_w
      rle_val := XmitBuffer[i_w] ;
      rle_count := 0 ;
      while (rle_count < max_rle_blocklen)
              and ((i_w+rle_count) < XmitBufferLen)
              and (XmitBuffer[i_w+rle_count] = rle_val) do
        inc(rle_count) ;
      if EnableRLECompression and (rle_count > 3) then begin
        // RLE sequenz wird kodiert als
        //  '|' <count word> <value word> <dummy>
        // count and value as 3 words -> 48 bit. words[0] ist MSB
        CharBuffer[i_c] := '|' ; // output RLE marker before char octett
        inc(i_c) ;
        // rle daten in buffer rückschreiben
        i_w := i_w + rle_count - 3; // skip to last 3 words of rle sequence
        // after regular 3->8 conversion, i_w+=3 is on next unprocessed data word
        XmitBuffer[i_w] := rle_count ;
        XmitBuffer[i_w+1] := rle_val ;
        XmitBuffer[i_w+2] := 0 ;
      end{ "if EnableRLECompression and (rle_count > 3)" } ;
      // die nächsten 3 worte sind entweder RLE sequenz oder daten worte
      // kodiere sie und gib sie in den Charbuffer aus.
      // i_w+=3, i_c +=8
      WordTriplet2CharOctet(XmitBuffer, i_w, CharBuffer, i_c) ;
      { PDP-11 needs time to expand RLE sequence.
       Code:  $1: mov r0,(r1)+,
                  sob r2,1$
       Conservative estimation: 500kHz machine. fix 100 cycles for triplett evaluation
       then 3 cycles per word transfer.
       Example: 64 words, 38400 baud
       -> processing needs (100 +3*64) *2 = 584 usecs. 1 char = 260 usec
       => 3 wait chars need -> 2 additional '~' delay chars
       }
       char_delay_us := FormMain.SerialIoHub.getXmtCharTransmissionMicros ;
       rle_delay_us :=  (100 + (rle_count * 3)) * 2 { usecs/cycle} ;
       // send delay chars, until pdp-11 has processed the RLE block
       // the next regular (non-delay) char accounts also to the delay
       while rle_delay_us > char_delay_us do begin
          CharBuffer[i_c] := '~' ; // output delay char
          inc(i_c) ;
         rle_delay_us := rle_delay_us - char_delay_us ;
       end;
    end{ "while i_w < XmitBufferLen" } ;
    CharBufferLen := i_c ;
  end{ "procedure TSerialTransfer.EncodeWordBuffer2CharBuffer" } ;


procedure TSerialTransfer.DecodeCharBuffer2WordBuffer ; // CharBuffer -> WordBuffer
  var
    i_w: integer ; // index im word buffer
    i_c: integer ; // index im char buffer
    i: integer ;
    rle_triplett: TArrayOfWord ;
    rle_repeat: integer ;
    rle_value: word ;
  begin
    SetLength(rle_triplett, 3) ;
    SetLength(XmitBuffer, max_xmitbuffer_len) ;

    // XmitBuffer auf die richtige Länge bringen
    // charbufflen vielfaches von 8?
//    if (CharBufferLen mod 8) <> 0 then
//      raise Exception.CreateFmt('Internal error in DecodeCharBuffer2WordBuffer(): CharBufferlen =%d .. not a multiple of 8!', [CharBufferLen]) ;
    // Gültigkeitsheck in CharOctet2WordTriplet()

//    XmitBufferLenRoundedUp := (CharBufferLen * 6) div 16 ;
//    SetLength(XmitBuffer,XmitBufferLenRoundedUp) ;
    i_w := 0 ;
    i_c := 0 ;
    while i_c < CharBufferLen do begin
      if CharBuffer[i_c] = '|' then begin
        // Decode RLE sequenz: | <repeat> <value> <dummy>
        inc(i_c) ; // skip '|'
        i := 0 ; // Ziel pos in rle_triplett[]: dummy
        CharOctet2WordTriplet(CharBuffer, i_c, rle_triplett, i) ;
        rle_repeat := rle_triplett[0] ;
        rle_value := rle_triplett[1] ;
        Log(' DecodeCharBuffer2WordBuffer(): RLE sequence, repeat=%d, value=0x%x. charpos=%d, word pos=%d',
                [rle_repeat, rle_value, i_c-9, i_w]) ;
        if (i_w+rle_repeat) >= length(XmitBuffer) then
          raise Exception.Create('DecodeCharBuffer2WordBuffer(): RLE repeat overflow') ;
        for i := 1 to rle_repeat do begin
          XmitBuffer[i_w] := rle_value ;
          inc(i_w) ;
        end ;
      end { "if CharBuffer[i_c] = '|'" } else
        CharOctet2WordTriplet(CharBuffer, i_c, XmitBuffer, i_w) ;
    end{ "while i_c < CharBufferLen" } ;

    // ich kann hier nicht rauskriegen, wieviele Daten sinnvoll sind.
    // Das muss sich aus Registerwerten oder der Anwendung ergeben.
//    XMitBufferLen := XmitBufferLenRoundedUp ;
    XmitBufferLen := i_w ;
    SetLength(XmitBuffer, XmitBufferLen) ;
  end{ "procedure TSerialTransfer.DecodeCharBuffer2WordBuffer" } ;


// Block empfangen.
// buffer muss allokiert sein, wird aber in der länge verändert
// NB: alle Zeichen in CharBuff, RLE Sequenz noch unverarbeitet
// throws ESerialTransferHardError
procedure TSerialTransfer.ReceiveCharBuffer(timeout_ms: dword) ;
  type
    TReceiveCharBufferState = (state0,state1,state2) ;
  var
    b: byte ;
    timeout_ticks: dword ;
    state : TReceiveCharBufferState ;
    buff: string ;
  begin
    Aborted := false ;

    // characters empfangen.
    CharBufferLen := 0 ;

    // Abbruch, wenn solange Blockübertragung nicht beginnt
    timeout_ticks := GetTickCount + timeout_ms ;

    assert(FormMain.SerialIoHub.Physical_Poll_Disable > 0) ; // caller muss sperren

    // Character stream empfangen
    state := state0 ; // 0 = warte auf start char, 1 = receiving, 2= fertig
    // high speed!
    while (CharBufferLen < max_charbuffer_len) and (GetTickCount < timeout_ticks) and (state < state2) do begin
      if Aborted then Exit ;

      // Application.ProcessMessages ;

      // 100% busy loop:
      // kleinste moegliche Wartepause wäre 1ms, das ist für 38400 baud zu lange!

      // Bsp für Laufdauer: 512 byte -> 258 words -> 688 chars
      // bei 9600 -> 960 char/sec -> 688/960= 0.72 sec
      if not FormMain.SerialIoHub.Physical_ReadByte(b,'SerTransferRcvBlock.'+IntToStr(ord(state))) then
        sleep(100) // kann lange sein, serial port und telnet puffern
      else begin
//      if FormMain.SerialIoHub.Physical_ReadByte(b) then begin
        case state of
          state0: begin
            // ignore chars, until '{' was send
//Log(' TSerialTransfer.ReceiveCharBuffer,state0: char="%s"', [char(b)]);
            buff := char(b) ;
            if char(b) = '{' then begin
              // jetzt kann in
              FormMain.SerialIoHub.DataToTerminal('<START OF BLOCK>', tosPDP);
              state := state1 ;
              if assigned(OnStatusChange) then OnStatusChange('Receiving data') ;
            end else // echo on terminal
              FormMain.SerialIoHub.DataToTerminal(buff, tosPDP);
          end ;

          state1: begin
            // Beginn der Übertragung: ab jetzt nur 1 Sek zwischen Zeichen erlaubt
            timeout_ticks := GetTickCount + 1000 ;
//            if assigned(OnStatusChange) then OnStatusChange('Receiving data') ;
// Log(' TSerialTransfer.ReceiveCharBuffer,state1: char="%s", bits=0x%x', [char(b), b-$20]);
            if b = ord('}') then
              state := state2 // Exit
            else if (b > $5f) and (b <> ord('|')) then
              raise ESerialTransferHardError.CreateFmt('illegal Char 0x%0.x received', [b])
            else if b >= $20 then begin // $20..$5f, oder '|'
              // Steuerzeichen ignorieren
              CharBuffer[CharBufferLen] := char(b) ;
              inc(CharBufferLen) ;
            end;
          end{ "case state of state1:" } ;
        end{ "case state" } ;
      end{ "if not FormMain.SerialIoHub.Physical_ReadByte(b,'SerTransferRcvBlock.'+IntToS...... ELSE" } ;
    end{ "while (CharBufferLen < max_charbuffer_len) and (GetTickCount < timeout_ticks)..." } ;
    if (GetTickCount >= timeout_ticks) then begin
      Log(' TSerialTransfer.ReceiveCharBuffer ended in state %d with timeout error', [ord(state)]) ;
      raise ESerialTransferHardError.CreateFmt('receive timeout in state %d', [ord(state)]) ;
    end else
      Log(' TSerialTransfer.ReceiveCharBuffer ended by "}"') ;

  end{ "procedure TSerialTransfer.ReceiveCharBuffer" } ;


procedure TSerialTransfer.TransmitCharBuffer ;
  var
    b: byte ;
    i: integer ;
  begin
    Aborted := false ;
    assert(FormMain.SerialIoHub.Physical_Poll_Disable > 0) ; // caller muss sperren

    if assigned(OnStatusChange) then OnStatusChange('Transmitting data') ;
    // mystery sleep intervall.
    // IF using SimH over telnet AND just called ReceiveCharBuffer()
    //  THEN wait a bit // else mytserious endless "WaitForSingleObject() in telnet stack

    // NOETIG FUER COM-PORT!
    while FormMain.SerialIoHub.Physical_ReadByte(b, 'SerTransferTransmitBlock.B') do
      sleep(10) ;

//  sleep(1000) ; // OK
    sleep(100) ; // NICHT OK
//  sleep(250) ; // NICHT OK nach 90 minuten
//  sleep(500) ; // ?
    // sende '{' als Block start
    FormMain.SerialIoHub.Physical_WriteByte(ord('{'), 'SerTransferTransmitBlock.C') ;
    Log(' TSerialTransfer.TransmitCharBuffer: char="{"') ;

    for i := 0 to CharBufferLen-1 do begin
      b := ord(CharBuffer[i]) ;
      FormMain.SerialIoHub.Physical_WriteByte(b,'SerTransferTransmitBlock.D') ;
//      Log(' TSerialTransfer.TransmitCharBuffer: char="%s", bits=0x%x', [char(b),b-$20]);
      if Aborted then Exit ;
    end;
//      FormMain.SerialIoHub.Physical_WriteByte($0d) ;
    // sende '}' als Block Ende
    FormMain.SerialIoHub.Physical_WriteByte(ord('}'),'SerTransferTransmitBlock.E') ;
    Log(' TSerialTransfer.TransmitCharBuffer: char="}"') ;
  end{ "procedure TSerialTransfer.TransmitCharBuffer" } ;



// Execute()
// Lauf einer procedure auf der PDP11.
// 1. Sendet request: opcode und daten blocks
// 2. PDP11 führt operation aus
// 3. PDP11 sendet response zurück, Ergebnis oder Fehler in block 0
//  von "XmitBuffer"
// Benutzt FormExecute zum Code ausführen.
// benutzt und ändert XmtBlock*Data[]

procedure TSerialTransfer.Execute (
        pdp11ProcTimeout_ms: dword ;
        var opcode: word // Funktion, die ausgeführt werden soll/ Status für Rückgabe
        ) ;// throws ESerialTransferHardError

// alles ab XmitBuffer[2] aufsummieren: 0 = len, 1 = checksum
  function calcChecksum: word ;
    var i: integer ;
    begin
      result := 0 ;
      for i := 2 to length(XmitBuffer) - 1 do
        result := (result + XmitBuffer[i]) and $ffff ;
    end;

//  const
//    wordbuffer_firstdataidx = 7 ; // offset des ersten datanbytes im Wordbuffer
  // vor data: 0 = checksum, 6 register r0..r5
  var
    //pdp11ProcAddr: TMemoryAddress;
    checksum, checksum1: word ;
    i, idx, n: integer ;
    timeout_ms: dword ;
    timeout_ticks: dword ;
    s: string ;
  begin { "procedure TSerialTransfer.Execute" }
    Aborted := false ;

    // block 0 als Textdarstellung
    Log('SerialXfer.Execute(%s): timeout=%d, block0=%s',
            [OpcodeAsText(opcode),
            pdp11ProcTimeout_ms,
            DataBlockText(XmitBlock0Data)]) ;

(*
    // einziger Einsprung in den Treiber
    pdp11ProcAddr.mat := matVirtual ;
    pdp11ProcAddr.val := _10000 ;
  *)
    // im puffer stehen die checksum, opcode, block0 und ggf block1
    // <len> <checksum> <opcode> [<block0> [<block1>]]
    // mit : <block> <len> <data, data,...>

    // Word transmssion montieren
    // block0 in request immer übertragen, auch, wenn len=0
    // block1 übertragen, wenn len=0!

    // checksum und register davor schreiben
    XmitBufferLen := 1 + 1 + 1 // totallen, checksum, opcode
            + (1 + length(XmitBlock0Data)) ;  // block 0
    if length(XmitBlock1Data) > 0 then
      XmitBufferLen := XmitBufferLen + (1 + length(XmitBlock1Data)) ;  // block 1

    // nötiger Platz im charbuffer:
    // word zahl auf vielfaches von drei bringen
    XmitBufferLenRoundedUp := XmitBufferLen ;
    while (XmitBufferLenRoundedUp mod 3) <> 0 do inc(XmitBufferLenRoundedUp) ;
    SetLength(XmitBuffer, XmitBufferLenRoundedUp) ;
    for i := 0 to XmitBufferLenRoundedUp - 1 do // init buffer
      XmitBuffer[i] := 0 ;

    // Daten in den Transmissionbuffer montieren
    XmitBuffer[0] := XmitBufferLen - 1 ; // len itself at [0] does not count
    XmitBuffer[1] := 0 ; // checksum, kommt später
    XmitBuffer[2] := opcode ;
    // Block 0
    n := length(XmitBlock0Data) ; // blocklen
    idx := 3 ; // start word von block 0
    XmitBuffer[idx] := n ; // <len>
    for i := 0 to n-1 do
      XmitBuffer[idx+i+1] := XmitBlock0Data[i] ;

    // Block 1
    idx := idx + 1 + n ; // start word von block 1
    n := length(XmitBlock1Data) ; // blocklen
    if n > 0 then begin // block 1 ist optional
      XmitBuffer[idx] := n ; // <len>
      for i := 0 to n-1 do
        XmitBuffer[idx+i+1] := XmitBlock1Data[i] ;
      idx := idx + 1 + n ; // start word von block 0
    end ;
    assert(idx = XmitBufferLen) ; // Buffer vollständig gefüllt?

    checksum := calcChecksum ;

    // Checksum in den Buffer
    XmitBuffer[1] := checksum ;

    EncodeWordBuffer2CharBuffer ; // encode. CharBuffLen wird neu gesetzt.

    Log('TSerialTransfer sends now len+chksum+opcode+block0(%d words) [+block1(%d words)] = %d words as %d chars',
            [length(XmitBlock0Data), length(XmitBlock1Data),
            XmitBufferLen, CharBufferLen]) ;

    Log('  Checksum=%s', [Dword2OctalStr(checksum,16)]) ;
(* NO! Driver assuemd to be running
    // Zielprocedur starten
    // Startadresse des Drivers in der ExecuteForm eintragen
    FormMain.FormExecute.StartPCEdit.Text := Addr2OctalStr(pdp11ProcAddr) ;
    // starten
    Application.ProcessMessages ;

    if resetMachine then
      FormMain.FormExecute.doResetMachineAndSetPCandStart
    else begin
      // START CODE WITHOUT HARD RESET
      // Reset may wipe out any previous device initialization!
      FormMain.FormExecute.doSetPCandContinue(pdp11ProcAddr) ;

//      if FormMain.PDP11Console is TConsolePDP1144 then begin
//        // Nach Continue: Wartepause nötig für 11/44: Console processor hang up?
//        // sleep(250) ist nicht immer OK, sleep(300) scheint OK
//        sleep(350) ;
//      end;
    end ;
    // "350" nötig für 11/44
    // "250" nötig für 11/23: Sichtbarkeit der Startanweisung in ODT
    WaitAndProcessMessages(350) ;

    *)

    // Send, wait and receive sequence nicht unterbrechbar
    inc(FormMain.SerialIoHub.Physical_Poll_Disable) ; // sperre andere Zugriffe of serial
    try
      Log('TSerialTransfer.Execute: start TransmitCharBuffer()');
      // senden.
      TransmitCharBuffer ;
      if Aborted then Exit ;

      if assigned(OnStatusChange) then OnStatusChange('Processing') ;
      // max. Wartezeit bis Beginn der Rückübertragung berechnen.
      // Dabei unbekannt: die erwartete Länge der Antwort.

      // Telnet: Sendedaten können noch in Telnet/Netzwerkstack-Puffern stehen
      // D.h., senden läuft in high-speed, die PDP-11 empfängt es viel langsamer.
      // => die PDP-11 ist JETZT noch nicht mit dem Empfangen fertig!
      // Die Wartepause für ReceiveCharBuffer berücksichtigt das.
      if FormMain.SerialIoHub.connectionType = connectionTelnet then
        timeout_ms := (XmitBufferLen * FormMain.SerialIoHub.getXmtCharTransmissionMicros) div 1000
      else timeout_ms := 0 ;
      timeout_ms := timeout_ms + 1000 ; // 1 Sek reserve
      // Zeit für die Abarbeitung des Codes mit einplanen
      // retries des I/O subsystems im worst case!
      timeout_ms := timeout_ms + pdp11ProcTimeout_ms ;
      Log('TSerialTransfer.Execute: start ReceiveCharBuffer(). Timeout=%d+%d+%d=%d ms',
              [timeout_ms-1000-pdp11ProcTimeout_ms, 1000, pdp11ProcTimeout_ms, timeout_ms]);
    finally
      dec(FormMain.SerialIoHub.Physical_Poll_Disable) ;
    end{ "try" } ;

    // Sonderlogic für HALT:
    if opcode = SerialTransferDriver_ophalt then begin
      // keine Rückgabe, aber check console HALT
      Log('TSerialTransfer.Execute: waiting for CPU halt.');

      // jetzt soll die PDP11 auf ein HALT gerannt sein
      timeout_ticks := GetTickCount + 5000 ; // 5 sek.
      while (FormMain.FormExecute.TheState = esRunning) and (GetTickCount < timeout_ticks) do begin
        Application.ProcessMessages ;
      end ;
      if GetTickCount >= timeout_ticks then begin
        Log('TSerialTransfer.Execute: CPU not halted?! Stop it!');
        // Fehler im Driver: Stop, und Error
        FormMain.FormExecute.doHalt;
        raise ESerialTransferHardError.CreateFmt('Execute(%s): PDP-11 CPU not halted',
                [OpcodeAsText(opcode)]);
      end ;
      Exit ;
    end { "if opcode = SerialTransferDriver_ophalt" } ;

    try
      inc(FormMain.SerialIoHub.Physical_Poll_Disable) ;

      // read answer
      ReceiveCharBuffer(timeout_ms) ;
      if Aborted then Exit ;
//      result := ReceiveCharBuffer(60000) ; // 10 Sekunden laufzeit timeout OK?

      // Serial-I/O wieder auf normalen Betrieb schalten
    finally
      dec(FormMain.SerialIoHub.Physical_Poll_Disable) ;
    end;

    (*
    Log('TSerialTransfer.Execute: waiting for CPU halt.');

    // jetzt soll die PDP11 auf ein HALT gerannt sein
    timeout_ticks := GetTickCount + 5000 ; // 5 sek.
    while (FormMain.FormExecute.TheState = esRunning) and (GetTickCount < timeout_ticks) do begin
      Application.ProcessMessages ;
    end ;
    if GetTickCount >= timeout_ticks then begin
      Log('TSerialTransfer.Execute: CPU not halted?! Stop it!');
      // Fehler im Driver: Stop, und Error
      FormMain.FormExecute.doHalt;
      raise ESerialTransferHardError.Create('Execute(): PDP-11 CPU not halted');
    end ;
*)
    DecodeCharBuffer2WordBuffer ; // decode
    // WordBuffer contains now outdata

    // 1st word = len
    SetLength(XmitBuffer, XmitBuffer[0]+1) ;

    // return results: opcode and block 0
    opcode := XmitBuffer[2] ;
    checksum := XmitBuffer[1] ; // empfangen
    checksum1 := calcChecksum ; // berechnet
    if checksum <> checksum1 then begin
      raise ESerialTransferHardError.CreateFmt('Execute(%s): rcv checksum error. Received=0x%0.4x, calculated=0x%0.4x',
              [OpcodeAsText(opcode), checksum, checksum1]);
    end ;
    // block 0 beginnt bei index 3
    idx := 3 ;
    n := XmitBuffer[idx] ; // block len
    SetLength(XmitBlock0Data, n) ;
    SetLength(XmitBlock1Data, 0) ; // Block 1 is invalid
    for i := 0 to n-1 do
      XmitBlock0Data[i] := XmitBuffer[idx+i+1] ;
    idx := idx + 1 +n ;
    assert (idx = length(XmitBuffer)) ; // alles gelesen ?

    Log('TSerialTransfer received %d data words as %d chars with correct checksum',
            [XmitBufferLen, CharBufferLen, checksum]) ;

    if opcode <> 0 then begin
      // Driver meldet Fehler: verpacke sie in Exception
      s :=  Format('error = %s, error info = %s!',
              [Dword2OctalStr(opcode,0),
              DataBlockText(XmitBlock0Data)]) ;
      ESerialTransferAppError.Create(s);
    end ;
//    Log('  Checksum=%s, Registers: R0=%s, R1=%s, R2=%s, R3=%s, R4=%s, R5=%s',
//            [Dword2OctalStr(checksum,16),
//            Dword2OctalStr(r0val,16),Dword2OctalStr(r1val,16),Dword2OctalStr(r2val,16),
//            Dword2OctalStr(r3val,16),Dword2OctalStr(r4val,16),Dword2OctalStr(r5val,16)]) ;
    //
    // verify: programm muss jetzt HALT sein
  end{ "procedure TSerialTransfer.Execute" } ;



{
  Überträgt einen Block zur PDP11,
  ide Function "doecho" jedes word
  und sendet es zurück
  testpatterns:
  0 = random data
  1 = random with constant block
  2 = constant data, max block len (tests RLE timeout)
  3 = count up
  4 = count down from 0xffff
}
procedure TSerialTransfer.SelfTest(wordcount: integer; testpattern: integer) ;

  var
    pattern: TArrayOfWord ; // muster, mit dem getestet wird.

  function xgetTestVal(i:word): word ;
    begin
      result := (i shl 6) + i ;
    end ;

  var
    i, i0, n: integer ;
    w_ist, w_soll: word ;
    opcode: word ;

  begin { "procedure TSerialTransfer.SelfTest" }

    // muster erzeugen: Zufällig, mit einem block gleicher words
    // zum testen der RLE kodierung
    SetLength(pattern, wordcount) ;

    case testpattern of
      0: begin
        // alle Werte zufällig
        for i := 0 to wordcount-1 do
          pattern[i] := Random($10000) ; // zufällige 16 bit Werte
        Log('TSerialTransfer.Selftest with random pattern: %d data words',
                [wordcount]) ;
      end;
      1: begin
        // alle Werte zufällig, mit grossem block aus constante Daten
        for i := 0 to wordcount-1 do
//      pattern[i] := i ;
          pattern[i] := Random($10000) ; // zufällige 16 bit Werte
        // zufälliger Block mit konstanten Werten
        i0 := Random(wordcount) ; // zufällige Lage des Blocks
        n := Random(wordcount) ;  // zufällige Länge des Blocks

        if i0+n >= wordcount then // Der Block soll oft am End des patterns sitzen, realismus!
          n := wordcount-i0 ;
        for i := i0 to i0+n-1 do
          pattern[i] := pattern[i0] ;
        Log('TSerialTransfer.Selftest with mixed random and constant pattern: %d data words, %d constant words from %d to %d',
                [wordcount, n, i0, i0+n-1]) ;
      end{ "case testpattern of 1:" } ;
      2: begin
        // nur ein zufälliges Zecihen, maximale Blocklen
        pattern[0] := Random($10000) ; // zufälliger 16 bit Wert
        for i := 0 to wordcount-1 do
          pattern[i] := pattern[0] ;
        Log('TSerialTransfer.Selftest with constant data: %d data words',
                [wordcount]) ;
      end ;
      3: begin
        // count up
        for i := 0 to wordcount-1 do
          pattern[i] := i ;
        Log('TSerialTransfer.Selftest with count-up pattern: %d data words',
                [wordcount]) ;
      end;
      4: begin
        // count down from 0xffff
        for i := 0 to wordcount-1 do
          pattern[i] := $ffff - i ;
        Log('TSerialTransfer.Selftest with count-down pattern: %d data words',
                [wordcount]) ;
      end;
    end { "case testpattern" } ;

    opcode := SerialTransferDriver_opecho ;

    // request: block 0 = pattern
    SetLength(XmitBlock0Data, wordcount);
    SetLength(XmitBlock1Data, 0);

    for i := 0 to wordcount-1 do
      XmitBlock0Data[i] := pattern[i] ;

    // Testprozedur aufrufen
    // BUS reset, wg. 11/23
    Execute(
            1000, // Zeit für block daten ändern in ms.
            opcode
            ) ; // throws ESerialTransferAppError


    // Verify: von jeder Zelle wurde das complement gebildet
    // in delphi: com(x) = x XOR $ffff (0er Bits -> 1, 1er Bits->0)
    // bei receive wird der buffer auf triplets verlängert -> auf patternlen testen
    if length(XmitBlock0Data) < length(pattern)  then
      raise Exception.CreateFmt('Error: pattern is %d words, received only %d words!',
              [length(pattern), length(XmitBlock0Data)]) ;
    for i := 0 to length(pattern)-1 do begin
      w_ist := XmitBlock0Data[i] ;
      w_soll := pattern[i] xor $ffff ;
      if w_ist <> w_soll then
        raise Exception.CreateFmt('Error comparing word[%d]: expected 0x%x, found 0x%x',
                [i, w_soll, w_ist]) ;
    end ;

  end { "procedure TSerialTransfer.SelfTest" } ;


end{ "unit SerialXferU" } .
