
unit SerialIoHubU; 

{
  Handling des seriellen Datenstroms zwischen
  internem Terminal, interner Consol-Logik,
  externem COM-port bzw externem Telnet.
          TConnection

 +-------------------------+
 |            TConsole     |
 | API                     |
 |            <-RcvData--  |   Console_WriteData                                   +---------------+
 |<--->                    |                                                       |  physical     |
 |            --XmtData->  |   Console_OnReadData                                  |  Interface    |
 |                         |                                                       |  serial/telnet|
 | <.......busy ...........|                                                       |               |
 +-------------------------+                                                       |               |
 +-------------------------+                                  Physical_Writebyte   | ---------->   |
 |         Terminal        |                                                       |               |
 |         display "grey"  |                                  Physical_ReadByte    |<----------    |
 |Display  <-------RcvData |   Terminal_WriteData                                  | (polled)      |
 |           display       |                                                       +---------------+
 |         <-------------- |
 |                         |
 |         manual typing   |
 |         -----XmtData>   |   Terminal_OnReadByte
 +-------------------------+

// Physikalische Verbindung zu einer PDP-11
// wird von Console* zur Ansteuerung benutzt
// Serials IO über COM oder über den PDP-11/44-Simulator, oder Telnet
}
interface 

uses 
  Windows, Classes, 
  SysUtils, 
  ExtCtrls, 
  CommU, 
  FakePDP11GenericU, 
  FormTerminalU, 
  //IdTelnet, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  OverbyteIcsWndControl, OverbyteIcsTnCnx, 
  AddressU, 
  FormLogU 
  ; 



type 

// see SerialFormatAsText
  TSerialFormat = ( 
      serformat8N1, 
      serformat87N1, // physical 8 bits, but MSB always 0
      serformat7N1, 
      serformat7O1, 
      serformat7E1 
    ) ; 
function SerialFormatAsText(serformat: TSerialFormat): string ; 

type 
  // eine der
  TConnectionSettings = class(TObject) 
    end; 

  // connection to a Fake PDP11
  TConnectionSettingsFake = class(TConnectionSettings) 
      FakePDP11Class: TClass ; // welche Klasse der FKA PDP11 ist zu instanziieren?
      physicaladdresswidth: TMemoryAddressType ; // ODT: 16/18/22 ?
    end ; 

  (*
// connection to a Serial COM Port
TConnectionSettingsSerial = class(TConnectionSettings)
    serialComport: integer ; // 0 = none
    serialBaudrate: integer ;
    serialFormat: TSerialFormat ;
  end ;
  *)

  // connection to a Telnet Port
  TConnectionSettingsTelnet = class(TConnectionSettings) 
      telnetHostname : string ; // '' = none
      telnetPort : integer ; 
    end ; 




  // und über welches Medium?
  TSerialIoHubPhysicalConnectionType = (connectionNone, connectionInternal, connectionSerial, connectionTelnet) ; 


  TSerialIoHub = class(TObject) 
    private 
      Comm : TComm ; // der COM-Port

      Physical_PollTimer: TTimer ; 
      // es darf nicht gepollt werden, während Zeichen von Console/terminal verarbeitet werden:
      // dauert die Verarbeitung zu lange, wird sonst neu gepollt, und neuere Zeichen werden
      // vor älteren verarbeitet!
      Transmission_TotalChars: longint ; // soviele Zeichen wurden insgesamt übertragen
      Transmission_TotalWait_us: int64 ; // soviele microsecs wurde insgesamt gewartet

      Telnet_connected : boolean ; 
      Telnet_InputBuffer: string ; 
      // Telnet Daten da: sammle sie im InputBuffer

      //procedure TelnetDataAvailable(Sender: TIdTelnet; const Buffer: TBytes) ;
      // = TTnDataAvailable
      procedure TelnetDataAvailable(Sender: TTnCnx; Buffer : Pointer; Len : integer) ; 
      procedure TelnetDisplay(Sender: TTnCnx; Str : string) ; 

      // procedure TelnetConnect(Sender: TObject) ;
      // = TTnSessionConnected
      procedure TelnetConnect(Sender: TTnCnx; Error: word) ; 
      procedure Physical_Poll(Sender:TObject) ; 

    public 
      connectionType: TSerialIoHubPhysicalConnectionType ; // Mit was verbinden?
      // wenn intern: IMMER gefakte PDP-11/44!

      // IdTelnet: TIdTelnet ;
      TelnetConnection: TTnCnx; // Overbyte
      // see telnet/overbyte/delphi/vc32/OverbyteIcsTnCnx.pas
      // set .host, .port
      // .isconencted
      // .Send, .Sendstr
      // .OnDataAvailable
      isLocalTelnet: boolean ; // true, wenn telnet nach localhost ... fuer locale SimH-Connection

      Console: TObject ; // TConsoleGeneric ;
      Terminal: TformTerminal ; 

      FakePDP11: TFakePDP11Generic; // kann 11/44 oder 11/ODT sein

      Physical_Poll_Disable: integer ; // 0 = callback läuft, sonst nicht.

      // Baudraten, werden für jede Verbindung defineirt, für timeouts
      RcvBaudrate: integer ; 
      XmtBaudrate: integer ; 
      serialFormat: TSerialFormat ; 

      constructor Create ; 
      destructor Destroy ; override ; 

      // Wartepause für eine Anzahl von Zeichen, gemäss Baudrate
      procedure TransmissionWait(charcount: integer) ; 
      // Zeit im Millisekuden, die die angegeben Zeichen zahl in Abh
      // von Rcv oder Xmtbaudrate benötigt
      function getXmtCharTransmissionMicros: integer ; 
      function getRcvCharTransmissionMicros: integer ; 

      ////// Schnittstelle zur Aussenwelt
      // Wahl der Init-Routine bestinmmt Ein/Ausgabeziel

      procedure Physical_InitForCOM(comport: integer ; aBaudrate: integer; aSerialFormat: TSerialFormat) ; 
      procedure Physical_InitForFakePDP11( 
              newFakePDP11: TFakePDP11Generic ; // link to instantiated object
              baud: integer) ; 
      procedure Physical_InitForFakePDP11M9312(baud: integer) ; 
      procedure Physical_InitForFakePDP11M9301(baud: integer) ; 
      procedure Physical_InitForFakePDP1144(baud:integer) ; // baud zum Warten
      procedure Physical_InitForFakePDP1144v340c(baud:integer) ; 
      procedure Physical_InitForFakePDP11ODT(baud: integer; physicaladdresswidth: TMemoryAddressType) ; 
      procedure Physical_InitForFakePDP11ODTK1630(baud: integer) ; 
      procedure Physical_InitForTelnet(aHost:string ; aPort: integer) ; 

      // für Anzeigen: "COM1 @ 9600 baud" oder "localhost:9922"
      function Physical_getInfoString: string ; 

      // Interface zur Conole-Logic:
      procedure DataToConsole(curdata:string) ; // daten an console
      procedure DataFromConsole(curdata:string) ; // Event: Daten von Consoloe

      // Interface zum Terminal:
      procedure DataToTerminal(curdata:string; style: TTerminalOutputStyle) ; // daten an Terminal
      procedure DataFromTerminal(curdata:string) ; // Event: Daten von Consoloe

      function Physical_ReadByte(var curbyte: byte ; dbglocation: string): boolean ; 
      function Physical_WriteByte(curbyte: byte ; dbglocation: string): boolean ; 

    end{ "TYPE TSerialIoHub = class(TObject)" } ; 


implementation 

uses 
  JH_Utilities, 
  AuxU, 
  Forms, 
  ConsoleGenericU, 
  FakePDP11M9312U, 
  FakePDP11M9301U, 
  FakePDP1144U, 
  FakePDP1144v340cU, 
  FakePDP11ODTU, 
  FakePDP11ODTK1630U, 
  FormSettingsU, 
  FormMainU ; 

//  var dbgsim : TPDP1144Sim ;
var 
  loglastlocation:string ; 


procedure LogChar(colidx: TLogColumnIndex ; c:char ; location:string) ; 
  var s: string ; 
  begin 
    if not Connection_LogIoStream then Exit ; 
    s := Character2Text_Full(c, true) ; 

    if location = loglastlocation then // kurzform
      LogStrCol(colidx, Format('%s   "',[s])) 
    else 
      LogStrCol(colidx, Format('%s %s', [s, location])) ; 
//    if c = #$0d then
//      Flush(logf) ;
    loglastlocation := location; 
  end{ "procedure LogChar" } ; 


function SerialFormatAsText(serformat: TSerialFormat): string ; 
  begin 
    case serformat of 
      serformat8N1: result := '8N1' ; 
      serformat87N1: result := '8N1, MSB cleared' ; 
      serformat7N1: result := '7N1' ; 
      serformat7E1: result := '7E1' ; 
      serformat7O1: result := '7O1' ; 
      else result := 'Illegal serial format' end; 
  end; 


constructor TSerialIoHub.Create ; 
  begin 
    inherited ; 
    Comm := TComm.Create(nil); 
    FakePDP11 := nil ; // wird erst in Init instanziiert.

    //IdTelnet := TIdTelnet.Create ;
    //IdTelnet.Name := 'IdTelnet' ;
    //IdTelnet.Terminal := 'dumb' ;
    //IdTelnet.ThreadedEvent := false ;
    // IdTelnet.Host := ;
    // IdTelnet.Port := ;
    TelnetConnection := TTnCnx.Create(nil); 
    TelnetConnection.Name := 'TnCnxn' ; 
    TelnetConnection.TermType := 'dumb' ; // später VT100
    //TelnetConnection.Host :=
    //TelnetConnection.Port :=
    isLocalTelnet := false ; 

    Physical_PollTimer := TTimer.Create(nil) ; 
    Physical_PollTimer.Interval := 10 ; 
    Physical_Poll_Disable := 1 ; // callback abschalten, wird nach Physical_Init...() aktiv

    Physical_PollTimer.Enabled := true ; 
    Physical_PollTimer.OnTimer := Physical_Poll ; 

  end{ "constructor TSerialIoHub.Create" } ; 

destructor TSerialIoHub.Destroy ; 
  begin 
    Physical_PollTimer.Free ; 
    Comm.Free ; 
    if FakePDP11 <> nil then FakePDP11.Free ; 
    FakePDP11 := nil ; 
    //IdTelnet.Free ;
    TelnetConnection.Free ; 
    inherited ; 
  end; 

procedure TSerialIoHub.Physical_InitForCOM(comport: integer ; aBaudrate: integer ; aSerialFormat: TSerialFormat) ; 
  begin 
    Physical_Poll_Disable := 1 ; 
    connectionType := connectionSerial ; 

    Comm.Close ; 
    Comm.port := comport ; 
    Comm.baud := aBaudrate ; 
    case aSerialFormat of 
      serformat8N1, serformat87N1: begin // physical 8 bits, but MSB always 0
        Comm.DataBits := 8 ; 
        Comm.Parity := NOPARITY ; 
        Comm.StopBits := ONESTOPBIT ; 
      end ; 
      serformat7N1 : begin 
        Comm.DataBits := 7 ; 
        Comm.Parity := NOPARITY ; 
        Comm.StopBits := ONESTOPBIT ; 
      end; 
      serformat7O1: begin 
        Comm.DataBits := 7 ; 
        Comm.Parity := ODDPARITY ; 
        Comm.StopBits := ONESTOPBIT ; 
      end; 
      serformat7E1: begin 
        Comm.DataBits := 7 ; 
        Comm.Parity := EVENPARITY ; 
        Comm.StopBits := ONESTOPBIT ; 
      end; 

    end{ "case aSerialFormat" } ; 

    RcvBaudrate := aBaudrate ; 
    XmtBaudrate := aBaudrate ; 
    serialFormat := aSerialFormat ; 

    Log('Trying to open COM%d with %d baud ...', [comport, aBaudrate]) ; 
    Comm.Open ; 
    Log('... OK') ; 

    Transmission_TotalChars := 0 ; 
    Transmission_TotalWait_us:= 0 ; 
    Physical_Poll_Disable := 0 ; 
  end{ "procedure TSerialIoHub.Physical_InitForCOM" } ; 


// instanziere eine der "FakePDP11" Simulatoren
procedure TSerialIoHub.Physical_InitForFakePDP11( 
        newFakePDP11: TFakePDP11Generic ; // link to instantiated object
        baud: integer) ; 
  begin 
    Physical_Poll_Disable := 1 ; 
    connectionType := connectionInternal ; 
    RcvBaudrate := baud ; 
    XmtBaudrate := baud ; 
    // nur freigeben, wenn Änderung, damit die Maschine ihren Zustand möglichst behält
    if (FakePDP11 = nil) or (FakePDP11 <> newFakePDP11) then begin 
//      if FakePDP11 <> nil then FakePDP11.Free ;
//      FakePDP11 := FakePDP11Class.Create ;
      FakePDP11 := newFakePDP11 ; 
      FakePDP11.PowerOn ; 
    end; 
    Log('Simulated %s powered ON!', [FakePDP11.Name]) ; 
    Physical_Poll_Disable := 0 ; 
  end{ "procedure TSerialIoHub.Physical_InitForFakePDP11" } ; 

// baudrate für simuliertes Warten
procedure TSerialIoHub.Physical_InitForFakePDP11M9312(baud: integer) ; 
  begin 
    Physical_Poll_Disable := 1 ; 
    connectionType := connectionInternal ; 
    RcvBaudrate := baud ; 
    XmtBaudrate := baud ; 
    // nur freigeben, wenn Änderung, damit die Maschine ihren Zustand möglichst behält
    if (FakePDP11 = nil) or (FakePDP11.ClassType <> TFakePDP11M9312) then begin 
      if FakePDP11 <> nil then FakePDP11.Free ; 
      FakePDP11 := TFakePDP11M9312.Create ; 
      FakePDP11.PowerOn ; 
    end; 
    Log('Simulated PDP-11 with M9312 console emulator powered ON!') ; 
    Physical_Poll_Disable := 0 ; 
  end{ "procedure TSerialIoHub.Physical_InitForFakePDP11M9312" } ; 


// baudrate für simuliertes Warten
procedure TSerialIoHub.Physical_InitForFakePDP11M9301(baud: integer) ; 
  begin 
    Physical_Poll_Disable := 1 ; 
    connectionType := connectionInternal ; 
    RcvBaudrate := baud ; 
    XmtBaudrate := baud ; 
    // nur freigeben, wenn Änderung, damit die Maschine ihren Zustand möglichst behält
    if (FakePDP11 = nil) or (FakePDP11.ClassType <> TFakePDP11M9301) then begin 
      if FakePDP11 <> nil then FakePDP11.Free ; 
      FakePDP11 := TFakePDP11M9301.Create ; 
      FakePDP11.PowerOn ; 
    end; 
    Log('Simulated PDP-11 with M9301 console emulator powered ON!') ; 
    Physical_Poll_Disable := 0 ; 
  end{ "procedure TSerialIoHub.Physical_InitForFakePDP11M9301" } ; 


// baudrate für simuliertes Warten
procedure TSerialIoHub.Physical_InitForFakePDP1144(baud: integer) ; 
  begin 
    Physical_Poll_Disable := 1 ; 
    connectionType := connectionInternal ; 
    RcvBaudrate := baud ; 
    XmtBaudrate := baud ; 
    // nur freigeben, wenn Änderng, damit die Maschine ihren Zustand möglichst behält
    if (FakePDP11 = nil) or (FakePDP11.ClassType <> TFakePDP1144) then begin 
      if FakePDP11 <> nil then FakePDP11.Free ; 
      FakePDP11 := TFakePDP1144.Create ; 
      FakePDP11.PowerOn ; 
    end; 
    Log('Simulated PDP-11/44 powered ON!') ; 
    Physical_Poll_Disable := 0 ; 
  end{ "procedure TSerialIoHub.Physical_InitForFakePDP1144" } ; 

procedure TSerialIoHub.Physical_InitForFakePDP1144v340c(baud: integer) ; 
  begin 
    Physical_Poll_Disable := 1 ; 
    connectionType := connectionInternal ; 
    RcvBaudrate := baud ; 
    XmtBaudrate := baud ; 
    // nur freigeben, wenn Änderng, damit die Maschine ihren Zustand möglichst behält
    if (FakePDP11 = nil) or (FakePDP11.ClassType <> TFakePDP1144v340c) then begin 
      if FakePDP11 <> nil then FakePDP11.Free ; 
      FakePDP11 := TFakePDP1144v340c.Create ; 
      FakePDP11.PowerOn ; 
    end; 
    Log('Simulated PDP-11/44 v 3.40c powered ON!') ; 
    Physical_Poll_Disable := 0 ; 
  end{ "procedure TSerialIoHub.Physical_InitForFakePDP1144v340c" } ; 


// baudrate für simuliertes Warten
procedure TSerialIoHub.Physical_InitForFakePDP11ODT(baud: integer; physicaladdresswidth: TMemoryAddressType) ; 
  begin 
    Physical_Poll_Disable := 1 ; 
    connectionType := connectionInternal ; 
    RcvBaudrate := baud ; 
    XmtBaudrate := baud ; 
    // nur freigeben, wenn Änderung, damit die Maschine ihren Zustand möglichst behält
    if (FakePDP11 = nil) 
            or (FakePDP11.ClassType <> TFakePDP11ODT) 
            or (FakePDP11.mat <> physicaladdresswidth) 
      then begin 
        if FakePDP11 <> nil then FakePDP11.Free ; 
        FakePDP11 := TFakePDP11ODT.Create(physicaladdresswidth) ; 
        FakePDP11.PowerOn ; 
      end; 
    Log('Simulated PDP-11/ODT %d bit powered ON!', [ 
            AddrType2Bitswidth(physicaladdresswidth)]) ; 
    Physical_Poll_Disable := 0 ; 
  end{ "procedure TSerialIoHub.Physical_InitForFakePDP11ODT" } ; 

procedure TSerialIoHub.Physical_InitForFakePDP11ODTK1630(baud: integer) ; 
  begin 
    Physical_Poll_Disable := 1 ; 
    connectionType := connectionInternal ; 
    RcvBaudrate := baud ; 
    XmtBaudrate := baud ; 
    // nur freigeben, wenn Änderng, damit die Maschine ihren Zustand möglichst behält
    if (FakePDP11 = nil) or (FakePDP11.ClassType <> TFakePDP11ODTK1630) then begin 
      if FakePDP11 <> nil then FakePDP11.Free ; 
      FakePDP11 := TFakePDP11ODTK1630.Create ; 
      FakePDP11.PowerOn ; 
    end; 
    Log('Simulated PDP-11 ODT K1630 powered ON!') ; 
    Physical_Poll_Disable := 0 ; 
  end{ "procedure TSerialIoHub.Physical_InitForFakePDP11ODTK1630" } ; 


// verbinde über Telnet .. damit automatisch SimH ... nicht gerade logisch
procedure TSerialIoHub.Physical_InitForTelnet(aHost:string ; aPort: integer) ; 
  begin 
    Physical_Poll_Disable := 1 ; 
    connectionType := connectionTelnet ; 

    RcvBaudrate := 9600 ; // das ist minimale Speed von telnet
    XmtBaudrate := 9600 ; 

    Telnet_InputBuffer := '' ; 
    try 
      //IdTelnet.Disconnect(true);
      TelnetConnection.Close ; 
    except 
      on e: Exception do 
    end; 
    Telnet_connected := false ; 
    //IdTelnet.host := host ;
    //IdTelnet.port := port ;
    //IdTelnet.OnDataAvailable := TelnetDataAvailable ; // callback
    //IdTelnet.OnConnected := TelnetConnect ; // callback
    TelnetConnection.host := aHost ; 
    TelnetConnection.port := IntToStr(aPort) ; 
    TelnetConnection.OnDataAvailable := TelnetDataAvailable ; // callback
    TelnetConnection.OnSessionConnected := TelnetConnect ; // callback
    TelnetConnection.OnDisplay := TelnetDisplay ; 
    try 
      //IdTelnet.Connect;
      TelnetConnection.Connect ; 

      // ist host der localhost?
      isLocalTelnet := GetIPAddress('localhost') = GetIPAddress(aHost) ; 

    except 
      Log('Telnet to "%s" over port %d FAILED!', [aHost, aPort]) ; 
    end; 

    Physical_Poll_Disable := 0 ; 
  end{ "procedure TSerialIoHub.Physical_InitForTelnet" } ; 


// Zeit im Millisekunden, die die angegebene Zeichenzahl in Abh
// von Rcv oder Xmtbaudrate benötigt
function TSerialIoHub.getXmtCharTransmissionMicros: integer ; 
  begin 
    result := ({bits/char} 10 * {us/sec} 1000000) div XmtBaudrate; 
  end; 

function TSerialIoHub.getRcvCharTransmissionMicros: integer ; 
  begin 
    result := ({bits/char} 10 * {us/sec} 1000000) div RcvBaudrate ; 
  end; 


// wartet solange, wie die Übertragung von "charcount" Zeichen dauert
// warte NICHT "per char", sondern wartet,so
// das immer die "total transmission time" für alle Zeichen bisher
//   stimmt
procedure TSerialIoHub.TransmissionWait(charcount: integer) ; 
  var 
    planned_Transmission_TotalWait_us: int64 ; 
    wait_period_us : int64 ; // solange diesmal warten
    wait_starttime_us: int64 ; 
    wait_endtime_us: int64 ; 
  begin 
    if XmtBaudrate = 0 then // nicht warten
      Exit ; 

    Transmission_TotalChars := Transmission_TotalChars + charcount ; 
    // Überlauf erst nach 10 Mrd Zeichen
    planned_Transmission_TotalWait_us := 
            int64(1000000) * (Transmission_TotalChars * {bit sper char}10) div XmtBaudrate ; 
    // also zu warten?
    wait_period_us := planned_Transmission_TotalWait_us - Transmission_TotalWait_us ; 

    // warten mit dem unpräzisen "sleep(). Messen, wie lange es wirklich dauerte
    QueryPerformanceCounter(wait_starttime_us) ; 
    wait_endtime_us := wait_starttime_us ; 
    while wait_endtime_us < (wait_starttime_us + wait_period_us) do begin 
      Sleep(1) ; 
      QueryPerformanceCounter(wait_endtime_us) ; 
    end; 

    // Tatsächlich insgesamt gewartete Zeit speichern
    Transmission_TotalWait_us := Transmission_TotalWait_us + 
            (wait_endtime_us - wait_starttime_us) ; 
  end{ "procedure TSerialIoHub.TransmissionWait" } ; 


//procedure TSerialIoHub.TelnetConnect(Sender: TObject) ;
procedure TSerialIoHub.TelnetConnect(Sender: TTnCnx; Error: word) ; 
  begin 
    Telnet_connected := true ; 
    Log('Telnet to "%s" over port %s OK!', [TelnetConnection.host, TelnetConnection.port]) ; 
  end; 

// Telnet Daten da: sammle sie im InputBuffer
//procedure TelnetDataAvailable(Sender: TIdTelnet; const Buffer: TBytes);
procedure TSerialIoHub.TelnetDataAvailable(Sender: TTnCnx; Buffer : Pointer; Len : integer) ; 
  var i: integer ; 
    pCharBuffer: PAnsiChar ; 
  begin 
    pCharBuffer := PAnsiChar(Buffer) ; 
    for i := 0 to Len - 1 do 
      Telnet_InputBuffer := Telnet_InputBuffer + pCharBuffer[i] ; 
  end; 

// Fehler anzeigen, direkt in ausgabe
procedure TSerialIoHub.TelnetDisplay(Sender: TTnCnx; Str : string) ; 
  begin 
    Telnet_InputBuffer := Telnet_InputBuffer + Str ; 
  end; 

function TSerialIoHub.Physical_ReadByte(var curbyte: byte ; dbglocation: string): boolean ; 
  begin 
    inc(Physical_Poll_Disable) ; // kein parallellauf
    try 
      result := false ; 
      case connectionType of 
        connectionInternal: begin 
          result := FakePDP11.SerialReadbyte(curbyte) ; 
          if result then TransmissionWait(1) ; 
        end; 
        connectionSerial: begin 
          result := Comm.ReadByte(curbyte) ; 
          if serialFormat <> serformat8N1 then 
            curbyte := curbyte and $7f ; // mask off MSB
        end; 
        connectionTelnet: begin 
          Application.ProcessMessages ; // bediene den Empfangs-Event
//      if not Telnet_Connected then raise Exception.Create('Telnet not connected') ;

          if length(Telnet_InputBuffer) > 0 then begin 
            // nimm nächstes gepuffertes Zeichen
            curbyte := byte(Telnet_InputBuffer[1]) ; 
            Telnet_InputBuffer := Copy(Telnet_InputBuffer, 2, maxint) ; 
            result := true ; 
          end else 
            result := false ; 
        end; 
      end{ "case connectionType" } ; 

      // Bis auf weiteres: parity wegschneiden, alle PDP-11 liefern 7 bit chars.
      curbyte := curbyte and $7f ; 

      if Connection_LogIoStream and result then 
        LogChar(LogCol_PhysicalReadByte, char(curbyte), 'Read.'+dbglocation); 
//        Log('Physical_ReadByte:"%s"', [CharVisible(char(curbyte))]) ;

    finally 
      dec(Physical_Poll_Disable) ; 
    end{ "try" } ; 
  end{ "function TSerialIoHub.Physical_ReadByte" } ; 


function TSerialIoHub.Physical_WriteByte(curbyte: byte; dbglocation:string): boolean ; 
  var s: string ; 
  begin 
    inc(Physical_Poll_Disable) ; // kein parallellauf
    try 
      if Connection_LogIoStream then 
        LogChar(LogCol_PhysicalWriteByte, char(curbyte), 'Write.'+dbglocation); 
//        Log('Physical_WriteByte:"%s"', [CharVisible(char(curbyte))]) ;
      result := false ; 
      case connectionType of 
        connectionInternal: begin 
          result := FakePDP11.SerialWriteByte(curbyte) ; 
          if result then TransmissionWait(1) ; 
        end ; 
        connectionSerial: begin 
          result := Comm.WriteByte(curbyte) ; 
        end; 
        connectionTelnet: begin 
          // if not Telnet_Connected then  raise Exception.Create('Telnet not connected') ;
          //IdTelnet.SendCh(char(curbyte)) ;
          s := char(curbyte) ; 
          TelnetConnection.SendStr(s) ; // 1 char as string
          result := true ; 
        end; 
      end{ "case connectionType" } ; 
    finally 
      dec(Physical_Poll_Disable) ; 
    end{ "try" } ; 
  end{ "function TSerialIoHub.Physical_WriteByte" } ; 


// wird periodisch aufgerufen, fragt Daten aus seriellem Port ab
// leitet sie an Terminal weiter
procedure TSerialIoHub.Physical_Poll(Sender:TObject) ; 
  var buff:string ; 
    curbyte: byte ; 
  begin 
//logstr('TSerialIoHub.Physical_Poll(): Poll_Disable = ' + inttostr(Physical_Poll_Disable)) ;
    if Physical_Poll_Disable > 0 then 
      Exit ; // no-op
    buff := '' ; 
    while Physical_ReadByte(curbyte, 'IoHub.Poll') do 
      buff := buff + char(curbyte) ; 
    if buff <> '' then begin 
//Log('TSerialIoHub.Physical_Poll: %s', [buff]) ;
      // weiterleiten
      DataToTerminal(buff, tosPDP); 
      DataToConsole(buff); 
    end; 
  end{ "procedure TSerialIoHub.Physical_Poll" } ; 


// für Anzeigen: "COM1 @ 9600 baud" oder "localhost:9922"
function TSerialIoHub.Physical_getInfoString: string ; 
  begin 
    result := 'unknown Connection' ; 
    case connectionType of 
      connectionInternal: 
        result := 'internal connection' ; 
      connectionSerial: 
        result := Format('COM%d @ %d baud', [Comm.port, Comm.baud]) ; 
      connectionTelnet: 
//        result := Format('%s:%d', [IdTelnet.host, IdTelnet.port]) ;
        result := Format('%s:%s', [TelnetConnection.host, TelnetConnection.port]) ; 
    end; 
  end; 


// daten an console weiterleiten
procedure TSerialIoHub.DataToConsole(curdata:string) ; 
  begin 
    // nicht pollen, wenn Daten gerade von Console/Terminal verarbeitet werden.
    inc(Physical_Poll_Disable) ; // kein Parallellauf
    try 
      assert(Console <> nil) ; 
      (Console as TConsoleGeneric).OnSerialRcv(curdata) ; 
    finally 
      dec(Physical_Poll_Disable); 
    end; 
  end; 

// wird von Console aufgerufen, wenn die Daten schreiben will
procedure TSerialIoHub.DataFromConsole(curdata:string) ; // Event: Daten von Consoloe
  var i: integer ; 
  begin 
    // nicht pollen, wenn Daten gerade von Console/Terminal verarbeitet werden.
    inc(Physical_Poll_Disable) ; // kein Parallellauf
    try 
      for i := 1 to length(curdata) do 
        Physical_WriteByte(byte(curdata[i]), 'IoHubDataFromConsole') ; 
//    DataToTerminal(curdata, tosPDP) ; // daten auch auf Terminal anzeigen in adnerer Farbe
    finally 
      dec(Physical_Poll_Disable); 
    end; 
  end; 


// Interface zum Terminal:
procedure TSerialIoHub.DataToTerminal(curdata:string; style: TTerminalOutputStyle) ; // daten an Terminal
  begin 
    // nicht pollen, wenn Daten gerade von Console/Terminal verarbeitet werden.
    inc(Physical_Poll_Disable) ; // kein Parallellauf
    try 
      assert(Terminal <> nil) ; 
//if pos('{', curdata) > 0 then
// curdata := curdata +'#' ; // break here
      Terminal.OnSerialRcvData(curdata) ; 
    finally 
      dec(Physical_Poll_Disable); 
    end; 
  end; 


// wird von Terminal aufgerufen, wenn es Daten schreiben will
// (= wenn der User was getippt hat)
procedure TSerialIoHub.DataFromTerminal(curdata:string) ; // Event: Daten von Consoloe
  var i: integer ; 
  begin 
    // nicht pollen, wenn Daten gerade von Console/Terminal verarbeitet werden.
    inc(Physical_Poll_Disable) ; // kein Parallellauf
    try 
      assert(Console <> nil) ; 
      // nur an den physical port ausgeben, wenn der nicht von der Console belegt ist.
      if not (Console as TConsoleGeneric).InCriticalSection then 
        for i := 1 to length(curdata) do 
          Physical_WriteByte(byte(curdata[i]), 'IoHubDataFromTerminal') ; 
    finally 
      dec(Physical_Poll_Disable); 
    end; 
  end{ "procedure TSerialIoHub.DataFromTerminal" } ; 


initialization 
  loglastlocation := '' ; 



end{ "unit SerialIoHubU" } . 
