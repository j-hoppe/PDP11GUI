unit MediaImageDevicesU;
{
  Objecte für alle alle Controller und Devices.

   Datenstruktur:
   -------------
   Globale Variable "TheControllerList"

    Abstrakt:
      AbstractController 1:n AbstractDevices
    Konkret:
      DiscController 1:n DiscDevice
      TapeController 1:n TapeDevice

      "1:n" bedeutet: Controller kann device betrieben.

  Klassenstruktur:
  ----------------
    AbstractController ---> DiscController_RL11
                       +--> DiscController_MSCP
                       +--> DiscController_RH11
                       +--> DiscController_RX11
                       +--> DiscController_RX211
                       ~--> DiscController_RK11
                       ~--> DiscController_RK611
                       '--> TapeController_PC11


    AbstractDevice -+-> DiscDevice -+-> DiscDevice_Std -+-> DiscDevice_RL01
                    |               |                   +-> DiscDevice_RL02
                    |               |                   +-> DiscDevice_RX01
                    |               |                   +-> DiscDevice_RX02_SD
                    |               |                   +-> DiscDevice_RX02_DD
                    |               |                   +-> DiscDevice_RM02
                    |               |                   +-> DiscDevice_RK05
                    |               `-> DiscDevice_MSCP
                    |
                    `-> TapeDevice ---> TapeDevice_PC05


  - Treestruktur für devices und subdevices wird statisch angelegt.
  - jedes device/subdevices hat statische Stammdaten
    (für die Definition: Namen, driver, Diskgeometrien, etc.)
    und dynamische Laufdaten (während es in Bearbeitung ist)

    - Der Buffer für die Iamge daten ist eine separate Klasse,
      ein Singleton
}

interface 

uses 
  Classes, 
  Windows, 
  SysUtils, 
  AuxU, 
  SerialXferU ; 


type 

  TMediaImage_AbstractDevice = class ; 
  TMediaImage_DeviceList = class ; 

  TMediaImage_AbstractController = class(TObject) 
    public 
(*
            // sichtbare controls in der readWrite Groupbox
            // werden eingeblednet nach device type auswahl
            // alle ausgeschaltet werden mit ReadWriteControlsHideAll()
      ReadWriteControls: TList ;
      *)

      Name: string ; // as in DeviceDriverTypeComboBox, "RL01/02"
      DriverSourceFilename: string ; // 'driver\pdp11gui_rl11.mac' ;

      // Name of smalles trasnferrable unit:
      // classic disks (RK11,RL11,RX11,RH11: "sector"
      // MSCP: "block"
      // Papertape: "byte"
      // Tapes: "file"
      dataBlockName: string ; 

      defaultBaseAddress: dword ; // "1775400"
      recognizesDevice: boolean ; // MSCP: DeviceSubType automatisch klar nach "init"?
      Devices: TMediaImage_DeviceList ; // of TMediaImage_Device

      { -- Laufdaten --- }
      curBaseAddress: dword ; // "1775400"
      curUnitNumber: integer ; 
      curDevice : TMediaImage_AbstractDevice ; // ausgewählt und in Form aktiv
      constructor Create  ; 
      destructor Destroy ; override ; 
      procedure AddDevice(aDevice: TMediaImage_AbstractDevice) ; 

    end{ "TYPE TMediaImage_AbstractController = class(TObject)" } ; 


  // konkrete TMediaImage_s
  TMediaImage_DiscController_RL11 = class(TMediaImage_AbstractController) 
      constructor Create ; 
    end; 

  TMediaImage_DiscController_MSCP = class(TMediaImage_AbstractController) 
      constructor Create ; 
    end; 

  // RM02
  TMediaImage_DiscController_RH11 = class(TMediaImage_AbstractController) 
      constructor Create ; 
    end; 

  // RX01 and RX02 floppy
  TMediaImage_DiscController_RX11 = class(TMediaImage_AbstractController) 
      constructor Create ; 
    end; 
  TMediaImage_DiscController_RX211 = class(TMediaImage_AbstractController) 
      constructor Create ; 
    end; 

// RK05F and J
  TMediaImage_DiscController_RK11 = class(TMediaImage_AbstractController) 
      constructor Create ; 
    end; 

// RK06/07
  TMediaImage_DiscController_RK611 = class(TMediaImage_AbstractController) 
      constructor Create ; 
    end; 

// high speed papertape
  TMediaImage_TapeController_PC11 = class(TMediaImage_AbstractController) 
      constructor Create ; 
    end; 

  TMediaImage_ControllerList = class(TList) 
      constructor Create ; 
      destructor Destroy ; override ; 
      function get(i: integer): TMediaImage_AbstractController ; 
    end; 

{
  Device: eines der Devices, die am selben Controllertyp hängen
  Verschiedene devices (disk, MSCP, tape, papertape) beinflussen die
   Benutzeroberfläche.
   Dieser Parametersatz beschreibt einen Devicetyp
}

  TMediaImage_AbstractDevice = class(TObject) 
    public 
      {  -- Stammdaten --- }
      Name: string ; // as in DeviceDriverTypeComboBox, "RL01", "RL02"

      controller: TMediaImage_AbstractController ; // uplink to parent
      DriverDeviceType: integer ; // Untertyp, wird direkt an PDP-11 driver übergeben

      // Device als stream of blocks
      // Blöcke/Sectoren auf der Disc. cylinders*heads*sectorcount.
      BlockCount: integer ; // blocknr = 0 .. blockcount
      BlockSize: integer ; // Grösse eines sectors in byte
      MultiBlockCount: integer ; // Anzahl der Blöcke für Multi Block Read/Write

      DataBytesAreWordLSBs: boolean ; // if true, the device receives or transmits
      // data bytes as words with bits 15:8 = 0. (papertape)

      fixCapacity : boolean ; // false: Größe ist unbeschränkt (papertape)


      { --- Laufdaten --- }
      // aktuelle discaddr
      curBlockNr: integer ; // logische Blocknr, start mit 0, Block=Sector

      readErrorRetryCount: integer ; // Wiederholungen bei Lesefehler

      function getBlockRangeDisplay(aStartBlockNr, aCount: integer): string ; virtual ; abstract ; 
      procedure getGeometryDisplay(lines: TStrings) ; virtual ; abstract ; 
      procedure SetupTransmissionParams(var serialxfer_wordbuffer: TArrayOfWord ; 
              transmittedBlockCount: integer ; var blockinfo: string) ; virtual ; abstract ; 

    end{ "TYPE TMediaImage_AbstractDevice = class(TObject)" } ; 

  // all disk devices: eversthing with cylinder, head, sector
  TMediaImage_DiscDevice = class(TMediaImage_AbstractDevice) 
    public 
      { --- Stammdaten ---}
      CylinderCount: integer ; 
      HeadCount: integer ; 
      SectorCount: integer ; 
      // Warning: DEC uses "cylinder/track/sector" instead of "track/head/sector"

      { --- Laufdaten --- }
      curCylinder: integer ; // immer ab 0, ggf im Treiber korrigieren
      curHead: integer ;   // immer ab 0, ggf im Treiber korrigieren
      curSector: integer ; // immer ab 0, ggf im Treiber korrigieren

      function DiscAddr2Blocknr(aCylinder, aHead, aSector: integer): integer ;// track,head,sector -> blocknr

      procedure BlockNr2DiscAddr ; overload ;// blocknr -> cylinder,head,sector
      procedure BlockNr2DiscAddr(aBlocknr: integer ; 
              var aCylinder,aHead, aSector: integer) ; overload ; 
    end{ "TYPE TMediaImage_DiscDevice = class(TMediaImage_AbstractDevice)" } ; 


  // all discs without MSCP
  TMediaImage_DiscDevice_Std = class(TMediaImage_DiscDevice) 
    public 
      { --- Stammdaten ---}

      { --- Laufdaten --- }

      // geschützter Bereich mit Vendor data
      VendorAreaStartBlock: integer ; // start block
      VendorAreaBlockCount: integer ; // länge

      function getBlockRangeDisplay(aStartBlockNr, aCount: integer): string ; override ; 
      procedure getGeometryDisplay(lines: TStrings) ; override ; 
      procedure SetupTransmissionParams( 
              var serialxfer_wordbuffer: TArrayOfWord ; 
              transmittedBlockCount: integer ; 
              var blockinfo: string) ; override ; 
    end{ "TYPE TMediaImage_DiscDevice_Std = class(TMediaImage_DiscDevice)" } ; 

  TMediaImage_DiscDevice_RL01 = class(TMediaImage_DiscDevice_Std) 
      constructor Create ; 
    end; 

  TMediaImage_DiscDevice_RL02 = class(TMediaImage_DiscDevice_Std) 
      constructor Create ; 
    end; 

  TMediaImage_DiscDevice_RX01 = class(TMediaImage_DiscDevice_Std) 
      constructor Create ; 
    end; 

  TMediaImage_DiscDevice_RX02_SD = class(TMediaImage_DiscDevice_Std) 
      constructor Create ; 
    end; 

  TMediaImage_DiscDevice_RX02_DD = class(TMediaImage_DiscDevice_Std) 
      constructor Create ; 
    end; 

  TMediaImage_DiscDevice_RM02 = class(TMediaImage_DiscDevice_Std) 
      constructor Create ; 
    end; 

  TMediaImage_DiscDevice_RK05 = class(TMediaImage_DiscDevice_Std) 
      constructor Create ; 
    end; 

  TMediaImage_DiscDevice_RK06 = class(TMediaImage_DiscDevice_Std)
      constructor Create ; 
    end; 

  TMediaImage_DiscDevice_RK07 = class(TMediaImage_DiscDevice_Std)
      constructor Create ; 
    end; 

  TMediaImage_DiscDevice_Robotron_K5501 = class(TMediaImage_DiscDevice_RK06)
      constructor Create ;
    end;

  TMediaImage_DiscDevice_Robotron_K5502 = class(TMediaImage_DiscDevice_RK07)
      constructor Create ;
    end;

  // MSCP disk are slightly different
  TMediaImage_DiscDevice_MSCP = class(TMediaImage_DiscDevice) 
      constructor Create ; 
      procedure EvalDriveInfo(serialxfer_wordbuffer: TArrayOfWord) ; 
      function getBlockRangeDisplay(aStartBlockNr, aCount: integer): string ; override ; 
      procedure getGeometryDisplay(lines: TStrings) ; override ; 
      procedure SetupTransmissionParams( 
              var serialxfer_wordbuffer: TArrayOfWord ; 
              transmittedBlockCount: integer ; 
              var blockinfo: string) ; override ; 
    end; 


  TMediaImage_TapeDevice = class(TMediaImage_AbstractDevice) 
      constructor Create ; 
      function getBlockRangeDisplay(aStartBlockNr, aCount: integer): string ; override ; 
    end; 

  TMediaImage_TapeDevice_PC05 = class(TMediaImage_TapeDevice) 
      constructor Create ; 
      procedure getGeometryDisplay(lines: TStrings) ; override ; 
      procedure SetupTransmissionParams( 
              var serialxfer_wordbuffer: TArrayOfWord ; 
              transmittedBlockCount: integer ; 
              var blockinfo: string) ; override ; 
    end; 

  // a list of subdevices
  TMediaImage_DeviceList = class(TList) 
      constructor Create ; 
      destructor Destroy ; 
      function get(i: integer): TMediaImage_AbstractDevice ; 
    end; 


var 
  TheControllerList:  TMediaImage_ControllerList ; 

implementation 


uses 
  Dialogs, 
  OctalConst, 
  FormMainU 
  ; 



{ ------------ controllers ---------------- }


constructor TMediaImage_AbstractController.Create  ; 
  begin 
    inherited ; 
    Devices := TMediaImage_DeviceList.Create ; 
    curBaseAddress := 0 ; 
    curDevice := nil ; 
  end ; 

destructor TMediaImage_AbstractController.Destroy ; 
  begin 
    inherited ; 
    // free all devices assigned to the controller
    Devices.Free ; 
  end ; 

procedure TMediaImage_AbstractController.AddDevice(aDevice: TMediaImage_AbstractDevice) ; 
  begin 
    Devices.Add(aDevice) ; 
    aDevice.controller := self ; // back link
  end; 


// konkrete Controller
constructor TMediaImage_DiscController_RL11.Create ; 
  begin 
    inherited ; 
    Name := 'RL11' ; 
    DriverSourceFilename := 'driver\pdp11gui_rl11.mac' ; 
    dataBlockName := 'sector' ; 
    defaultBaseAddress := _174400 ; 
    recognizesDevice := false ; // only MSCP controller recognize Devices
    AddDevice(TMediaImage_DiscDevice_RL01.Create) ; 
    AddDevice(TMediaImage_DiscDevice_RL02.Create) ; 
  end; 


constructor TMediaImage_DiscController_MSCP.Create ; 
  begin 
    inherited ; 
    // UDA50, RQDX1,2,3
    Name := 'MSCP' ; 
    DriverSourceFilename := 'driver\pdp11gui_mscp.mac' ; 
    dataBlockName := 'block' ; 
    defaultBaseAddress:= _172150 ; 
    recognizesDevice := true ; // only MSCP controller recognize Devices
    // only one generic MSCP device is assigned.
    // after "open" the acutal parmaters are changed there
    AddDevice(TMediaImage_DiscDevice_MSCP.Create) ; 
  end; 


constructor TMediaImage_DiscController_RH11.Create ; 
  begin 
    inherited ; 
    // RM02/3
    Name := 'RH11' ; 
    DriverSourceFilename := 'driver\pdp11gui_rh11.mac' ; 
    dataBlockName := 'sector' ; 
    defaultBaseAddress := _176700 ; 
    recognizesDevice := false ; // only MSCP controller recognize Devices
    AddDevice(TMediaImage_DiscDevice_RM02.Create) ; 
//  RM03 nicht als eigenen Typ aufnehmen: gibt nur Verwirrung bei file extensions
  end; 


constructor TMediaImage_DiscController_RX11.Create ; 
  begin 
    // RX01 floppy
    inherited ; 
    Name := 'RX11' ; 
    DriverSourceFilename := 'driver\pdp11gui_rx11.mac' ; 
    dataBlockName := 'sector' ; 
    defaultBaseAddress := _177170 ; 
    recognizesDevice := false ; // only MSCP controller recognize Devices
    AddDevice(TMediaImage_DiscDevice_RX01.Create) ; 
  end; 

constructor TMediaImage_DiscController_RX211.Create ; 
  begin 
    // RX02 floppy
    inherited ; 
    Name := 'RX211' ; 
    dataBlockName := 'sector' ; 
    DriverSourceFilename := 'driver\pdp11gui_rx211.mac' ; 
    defaultBaseAddress := _177170 ; 
    recognizesDevice := false ; // only MSCP controller recognize Devices

    // RX211 kann nur RX02 ... see RX211 FPMS: CS RX02 bit is igored
    // and always read as "!"
    //    AddDevice(TMediaImage_DiscDevice_RX01.Create) ;
    AddDevice(TMediaImage_DiscDevice_RX02_SD.Create) ; 
    AddDevice(TMediaImage_DiscDevice_RX02_DD.Create) ; 
  end{ "constructor TMediaImage_DiscController_RX211.Create" } ; 



constructor TMediaImage_DiscController_RK11.Create ; 
  begin 
    // RK05F and J
    inherited ; 
    Name := 'RK11' ; 
    dataBlockName := 'sector' ; 
    DriverSourceFilename := 'driver\pdp11gui_rk11.mac' ; 
    defaultBaseAddress := _177400 ; 
    recognizesDevice := false ; // only MSCP controller recognize Devices
    // at least one subtype
    AddDevice(TMediaImage_DiscDevice_RK05.Create) ; 
  end; 

constructor TMediaImage_DiscController_RK611.Create ; 
  begin 
    // RK06 and RK07
    inherited ; 
    Name := 'RK611' ; 
    dataBlockName := 'sector' ; 
    DriverSourceFilename := 'driver\pdp11gui_rk611.mac' ; 
    defaultBaseAddress := _177440 ; 
    recognizesDevice := false ; // only MSCP controller recognize Devices
    // at least one subtype
    AddDevice(TMediaImage_DiscDevice_RK06.Create) ; 
    AddDevice(TMediaImage_DiscDevice_RK07.Create) ; 
    AddDevice(TMediaImage_DiscDevice_Robotron_K5501.Create) ;
    AddDevice(TMediaImage_DiscDevice_Robotron_K5502.Create) ;
  end;

constructor TMediaImage_TapeController_PC11.Create ; 
  begin 
    // high speed papertape
    inherited ; 
    Name := 'PC11' ; 
    dataBlockName := 'byte' ; 
    DriverSourceFilename := 'driver\pdp11gui_pc11.mac' ; 
    defaultBaseAddress := _177550 ; 
    recognizesDevice := false ; // only MSCP controller recognize Devices
    // at least one subtype
    AddDevice(TMediaImage_TapeDevice_PC05.Create) ; 
  end; 


(*
constructor TMediaImage_TapeController_TM11.Create ;
  begin
    inherited ;
    Name := 'RM11' ;
    dataUnitName := 'file' ;
    DriverSourceFilename := 'driver\pdp11gui_tm11.mac' ;
    defaultBaseAddress := _ ;
    recognizesDevice := false ; // only MSCP controller recognize Devices
    // at least one subtype
    AddDevice(TMediaImage_DiscDevice_??.Create) ;
  end;
*)

// erzeuge Liste mit allen bekannten Controllern.
// jeder Controller erzeugt selbst die devices, die er bedient.
constructor TMediaImage_ControllerList.Create ; 
  begin 
    inherited ; 
  end; 

// free all devices assigned to the controller
destructor TMediaImage_ControllerList.Destroy ; 
  var i: integer ; 
  begin 
    for i := 0 to Count - 1 do 
      TMediaImage_AbstractDevice(Items[i]).Free ; 
    inherited ; 
  end; 


function TMediaImage_ControllerList.get(i: integer): TMediaImage_AbstractController ; 
  begin 
    result := TMediaImage_AbstractController(Items[i]) ; 
  end; 



{ ----------- devices ---------------- }
constructor TMediaImage_DeviceList.Create ; 
  begin 
    inherited ; 
  end; 

// free all devices assigned to the controller
destructor TMediaImage_DeviceList.Destroy ; 
  var i: integer ; 
  begin 
    for i := 0 to Count - 1 do 
      TMediaImage_AbstractDevice(Items[i]).Free ; 
    inherited ; 
  end; 

function TMediaImage_DeviceList.get(i: integer): TMediaImage_AbstractDevice ; 
  begin 
    result := TMediaImage_AbstractDevice(Items[i]) ; 
  end; 

// cylinder,head,sector -> blocknr
function TMediaImage_DiscDevice.DiscAddr2Blocknr(aCylinder, aHead, aSector: integer): integer ; 
  begin 
    result := HeadCount * SectorCount * aCylinder 
            + SectorCount * aHead 
            + aSector ; 
  end; 

procedure TMediaImage_DiscDevice.BlockNr2DiscAddr(aBlocknr: integer ; 
        var aCylinder,aHead, aSector: integer) ; 
  var n: integer ; 
  begin 
    n := aBlocknr ; 
    aSector := n mod SectorCount ; 
    n := aBlocknr div SectorCount ; 
    aHead := n mod HeadCount ; 
    n := n div HeadCount ; 
    aCylinder := n; 
  end ; 

// arbeitet auf den eingebauten blocknr, head,cylinder,sector
procedure TMediaImage_DiscDevice.BlockNr2DiscAddr ; // blocknr -> cylinder,head,sector
  begin 
    BlockNr2DiscAddr(curBlockNr, curCylinder, curHead, curSector) ; 
  end; 



{ show a range of disc blocks, in cyl/head/sector }
function TMediaImage_DiscDevice_Std.getBlockRangeDisplay(aStartBlockNr, aCount: integer): string ; 
  begin 
    result := '' ; 
    // hier die Parameterprüfung ... unlogisch
    assert(aStartBlockNr >= 0) ; 
    assert((aStartBlockNr + aCount) <= BlockCount) ; 

    // Block in cylinder/head/sector umrechnen
    curBlockNr := aStartBlockNr ; 
    BlockNr2DiscAddr ; 
    // normale cyl/head/sector drives
    // Check: nicht über track ende hinaus schreiben
    assert(curSector + aCount <= SectorCount) ; 

    if aCount = 1 then 
      result := Format('block %d / %d = %d%%, cylinder.head.sector=%d.%d.%d', 
              [aStartBlockNr, BlockCount, 
              100 * aStartBlockNr div BlockCount, 
              curCylinder, curHead, curSector]) 
    else 
      // ZUS: Multiblock nur innerhalb einer Track
      result := Format('block %d-%d / %d = %d%%, cylinder.head=%d.%d', 
              [aStartBlockNr,aStartBlockNr+aCount-1, 
              BlockCount, 
              100 * aStartBlockNr div BlockCount, 
              curCylinder, curHead]) ; 
  end{ "function TMediaImage_DiscDevice_Std.getBlockRangeDisplay" } ; 

procedure TMediaImage_DiscDevice_Std.getGeometryDisplay(lines: TStrings) ; 
  begin 
    lines.Add(Format('Block count = %d, block size=%d bytes', 
            [BlockCount,BlockSize])) ; 
    lines.Add(Format('cylinders/heads/sectors= %d/%d/%d', 
            [ CylinderCount, HeadCount, SectorCount])) ; 
  end; 

{ prepare multiblock read or write:
read fill the parameters block for serial transmission to the driver }
procedure TMediaImage_DiscDevice_Std.SetupTransmissionParams( 
        var serialxfer_wordbuffer: TArrayOfWord ; 
        transmittedBlockCount: integer ; 
        var blockinfo: string) ; 
  var flags: word ; 
  begin 
    // flags: bit 0 = suppress read/checkonly
    flags := 0 ; 
    // bits 3:1 = subtype/format (zB: 3 = RX02 double density)
    flags := flags or ((DriverDeviceType and 7) shl 1) ; 
    BlockNr2DiscAddr ; // update cyl, head, sector


    if HeadCount = 1 then 
      blockinfo := Format('cylinder=%d, sector=%d', 
              [curCylinder, curSector]) 
    else 
      blockinfo := Format('cylinder=%d, head=%d, sector=%d', 
              [curCylinder, curHead, curSector]) ; 
    SetLength(serialxfer_wordbuffer, 7) ; 
    serialxfer_wordbuffer[SerialTransferDriver_prcba] := controller.curBaseAddress ; 
    serialxfer_wordbuffer[SerialTransferDriver_prunit] := controller.curUnitNumber ; 
    serialxfer_wordbuffer[SerialTransferDriver_prflags] := flags ; 
    serialxfer_wordbuffer[SerialTransferDriver_prwlen] := transmittedBlockCount  * BlockSize div 2 ; // word count
    serialxfer_wordbuffer[SerialTransferDriver_prcyl] := curCylinder ; 
    serialxfer_wordbuffer[SerialTransferDriver_prhead] := curHead ; 
    serialxfer_wordbuffer[SerialTransferDriver_prsect] := curSector ; 
  end{ "procedure TMediaImage_DiscDevice_Std.SetupTransmissionParams" } ; 

constructor TMediaImage_DiscDevice_RL01.Create ; 
  begin 
    inherited ; 
    Name := 'RL01' ; 
    // RL01 und RL02 unterscheiden sich nur in der cylinder zahl
    DriverDeviceType := 0 ; // flags for PDP-11 driver
    CylinderCount := 256 ; 
    // Geometry: cylinder, head, sector
    HeadCount := 2 ; 
    SectorCount := 40 ; 

    BlockCount := CylinderCount * HeadCount * SectorCount ; 
    BlockSize := 256 ; // in byte

    // Cylinder 511, head 1 ist reserviert
    VendorAreaStartBlock := (CylinderCount-1) * 2 + SectorCount ; 
    VendorAreaBlockCount := SectorCount ; // länge

    readErrorRetryCount := 16 ; 
    MultiBlockCount := SectorCount ; // ein Multiblock = 1 Track
    DataBytesAreWordLSBs := false ; 
    fixCapacity := true ; 

    // 32K ist die grösse des Buffers im treiber
    // See drivers\pdp11gui_serialxfer.mac, label rxbfdt:
    assert(MultiBlockCount * BlockSize <= 32768) ; 
  end { "constructor TMediaImage_DiscDevice_RL01.Create" } ; 

constructor TMediaImage_DiscDevice_RL02.Create ; 
  begin 
    inherited ; 
    Name := 'RL02' ; 
    DriverDeviceType := 1 ; // flags for PDP-11 driver
    CylinderCount := 512 ; 
    // Geometry: cylinder, head, sector
    HeadCount := 2 ; 
    SectorCount := 40 ; 

    BlockCount := CylinderCount * HeadCount * SectorCount ; 
    BlockSize := 256 ; // in byte

    // Cylinder 511, head 1 ist reserviert
    VendorAreaStartBlock := (CylinderCount-1) * 2 + SectorCount ; 
    VendorAreaBlockCount := SectorCount ; // länge

    readErrorRetryCount := 16 ; 
    MultiBlockCount := SectorCount ; // ein Multiblock = 1 Track
    DataBytesAreWordLSBs := false ; 
    fixCapacity := true ; 

    // 32K ist die grösse des Buffers im treiber
    // See drivers\pdp11gui_serialxfer.mac, label rxbfdt:
    assert(MultiBlockCount * BlockSize <= 32768) ; 
  end{ "constructor TMediaImage_DiscDevice_RL02.Create" } ; 

constructor TMediaImage_DiscDevice_RX01.Create ; 
// is connected to RX11 and RX211 controller!
  begin 
    inherited ; 
    Name := 'RX01' ; 
    CylinderCount := 77 ; 
    HeadCount := 1 ; 
    SectorCount := 26 ; 

    //DiscDriverSubType:  bit 0x02 = "RX02", bit 0x01 = "double density"
    BlockSize := 128 ; 
    DriverDeviceType := 0 ; // ignored on RX11, important on RX211

    BlockCount := CylinderCount * HeadCount * SectorCount ; 

    // Keine reservierten tracks
    VendorAreaStartBlock := 0 ; 
    VendorAreaBlockCount := 0 ; 

    readErrorRetryCount := 16 ; 
    MultiBlockCount := SectorCount ; // ein Multiblock = 1 Track
    DataBytesAreWordLSBs := false ; 
    fixCapacity := true ; 

    // 32K ist die grösse des Buffers im treiber
    // See drivers\pdp11gui_serialxfer.mac, label rxbfdt:
    assert(MultiBlockCount * BlockSize <= 32768) ; 
  end{ "constructor TMediaImage_DiscDevice_RX01.Create" } ; 

constructor TMediaImage_DiscDevice_RX02_SD.Create ; 
  begin 
    inherited ; 
    Name := 'RX02 SD' ; 
    CylinderCount := 77 ; 
    HeadCount := 1 ; 
    SectorCount := 26 ; 

    //DiscDriverSubType:  bit 0x02 = "RX02", bit 0x01 = "double density"
    BlockSize := 128 ; 
    DriverDeviceType := 2 ; 

    BlockCount := CylinderCount * HeadCount * SectorCount ; 

    // Keine reservierten tracks
    VendorAreaStartBlock := 0 ; 
    VendorAreaBlockCount := 0 ; 

    readErrorRetryCount := 16 ; 
    MultiBlockCount := SectorCount ; // ein Multiblock = 1 Track
    DataBytesAreWordLSBs := false ; 
    fixCapacity := true ; 

    // 32K ist die grösse des Buffers im treiber
    // See drivers\pdp11gui_serialxfer.mac, label rxbfdt:
    assert(MultiBlockCount * BlockSize <= 32768) ; 
  end{ "constructor TMediaImage_DiscDevice_RX02_SD.Create" } ; 

constructor TMediaImage_DiscDevice_RX02_DD.Create ; 
  begin 
    inherited ; 
    Name := 'RX02 DD' ; 
    CylinderCount := 77 ; 
    HeadCount := 1 ; 
    SectorCount := 26 ; 

    //DiscDriverSubType:  bit 0x02 = "RX02", bit 0x01 = "double density"
    BlockSize := 256 ; // double density, in byte
    DriverDeviceType := 3 ; 

    BlockCount := CylinderCount * HeadCount * SectorCount ; 

    // Keine reservierten tracks
    VendorAreaStartBlock := 0 ; 
    VendorAreaBlockCount := 0 ; 

    readErrorRetryCount := 16 ; 
    MultiBlockCount := SectorCount ; // ein Multiblock = 1 Track
    DataBytesAreWordLSBs := false ; 
    fixCapacity := true ; 

    // 32K ist die grösse des Buffers im treiber
    // See drivers\pdp11gui_serialxfer.mac, label rxbfdt:
    assert(MultiBlockCount * BlockSize <= 32768) ; 
  end{ "constructor TMediaImage_DiscDevice_RX02_DD.Create" } ; 

constructor TMediaImage_DiscDevice_RM02.Create ; 
  begin 
    inherited ; 
    Name := 'RM02/03' ; 
    // RM02 und RM03 unterscheiden sich nur in der Rotationsgeschwindigkeit
    // Die Geometrie 823/5/32 wurde aus dem Holdingregister des Emulex SC31 gelesen.
    // Geometry: cylinder, head, sector
    CylinderCount := 823 ; 
    HeadCount := 5 ; 
    SectorCount := 32 ; 
    DriverDeviceType := 0 ; // don't care

    BlockCount := CylinderCount * HeadCount * SectorCount ; 
    BlockSize := 512 ; // in byte

    VendorAreaStartBlock := maxint ; // not defined
    VendorAreaBlockCount := 0 ; 

    readErrorRetryCount := 16 ; 
    MultiBlockCount := SectorCount ; // ein Multiblock = 1 Track =
    DataBytesAreWordLSBs := false ; 
    fixCapacity := true ; 

    // 32K ist die grösse des Buffers im treiber
    // See drivers\pdp11gui_serialxfer.mac, label rxbfdt:
    assert(MultiBlockCount * BlockSize <= 32768) ; 
  end{ "constructor TMediaImage_DiscDevice_RM02.Create" } ; 

constructor TMediaImage_DiscDevice_RK05.Create ; 
  begin 
    inherited ; 
    Name := 'RK05' ; 
    // RK05F (fixed disk) has 5MB, and is accessed as two logical RK05
    // so no subtypes here!
    DriverDeviceType := 0 ; 
    // Geometry: cylinder, head, sector
    CylinderCount := 203 ; 
    HeadCount := 2 ; 
    SectorCount := 12 ; 
    BlockCount := CylinderCount * HeadCount * SectorCount ; 
    BlockSize := 512 ; // in byte

    // There are also 16 sector/track cartridges?
    // SimH implements the 12 variant, so do I

    VendorAreaStartBlock := maxint ; // not defined
    VendorAreaBlockCount := 0 ; 

    readErrorRetryCount := 16 ; 
    MultiBlockCount := SectorCount ; // ein Multiblock = 1 Track
    DataBytesAreWordLSBs := false ; 
    fixCapacity := true ; 

    // 32K ist die grösse des Buffers im treiber
    // See drivers\pdp11gui_serialxfer.mac, label rxbfdt:
    assert(MultiBlockCount * BlockSize <= 32768) ; 
  end{ "constructor TMediaImage_DiscDevice_RK05.Create" } ; 


constructor TMediaImage_DiscDevice_RK06.Create ; 
  begin 
    inherited ; 
    Name := 'RK06' ; 
    DriverDeviceType := 0 ; 
    // Geometry: cylinder, head, sector
    CylinderCount := 411 ; 
    HeadCount := 3 ; 
    SectorCount := 22 ; 
    BlockCount := CylinderCount * HeadCount * SectorCount ; 
    BlockSize := 512 ; // 256 words
    VendorAreaStartBlock := maxint ; // not defined
    VendorAreaBlockCount := 0 ; 

    readErrorRetryCount := 16 ; 
    MultiBlockCount := SectorCount ; // ein Multiblock = 1 Track
    DataBytesAreWordLSBs := false ; 
    fixCapacity := true ; 

    // 32K ist die grösse des Buffers im treiber
    // See drivers\pdp11gui_serialxfer.mac, label rxbfdt:
    assert(MultiBlockCount * BlockSize <= 32768) ; 
  end{ "constructor TMediaImage_DiscDevice_RK06.Create" } ; 

constructor TMediaImage_DiscDevice_RK07.Create ; 
  begin 
    inherited ; 
    Name := 'RK07' ;
    DriverDeviceType := 1 ;
    // Geometry: cylinder, head, sector
    CylinderCount := 815 ;
    HeadCount := 3 ;
    SectorCount := 22 ;
    BlockCount := CylinderCount * HeadCount * SectorCount ;
    BlockSize := 512 ; // in byte
    VendorAreaStartBlock := maxint ; // not defined
    VendorAreaBlockCount := 0 ;

    readErrorRetryCount := 16 ;
    MultiBlockCount := SectorCount ; // ein Multiblock = 1 Track
    DataBytesAreWordLSBs := false ;
    fixCapacity := true ;

    // 32K ist die grösse des Buffers im treiber
    // See drivers\pdp11gui_serialxfer.mac, label rxbfdt:
    assert(MultiBlockCount * BlockSize <= 32768) ;
  end{ "constructor TMediaImage_DiscDevice_RK07.Create" } ;


  // is like RK06, but different geometry
  // limits see page 16 in "AFP K 5165" Controller manual
  // There are variants with 6, 10 and 14 heads !
constructor TMediaImage_DiscDevice_Robotron_K5501.Create ;
  begin
    inherited ;
    Name := 'K5501_14' ;
    CylinderCount := 210 ; // 200+10
    HeadCount := 14 ;
    SectorCount := 24 ;
    BlockCount := CylinderCount * HeadCount * SectorCount ;
  end ;

  // is like RK07, but different geometry
  // limits see page 16 in "AFP K 5165" Controller manual
  // There are variants with 6, 10 and 14 heads !
constructor TMediaImage_DiscDevice_Robotron_K5502.Create ;
  begin
    inherited ;
    Name := 'K5502_14' ;
    CylinderCount := 588 ; // 561+27
    HeadCount := 14 ;
    SectorCount := 34 ; // 32+2
    BlockCount := CylinderCount * HeadCount * SectorCount ;
  end ;


// MSCP disk are slightly differnet
constructor TMediaImage_DiscDevice_MSCP.Create ; 
  begin 
    inherited ; 
    Name := 'MSCP' ; 
    // alle anderen Angaben kommen aus dem Controller
    BlockSize := 512 ; // in byte
    // Geometrie wird vom Controller gekapselt
//        HeadCount := -1 ;
//        SectorCount := -1 ;
//        CylinderCount := -1 ;
    DriverDeviceType := 0 ; // don't care

    readErrorRetryCount := 16 ; 

    // die Multiblock grösse ist frei wählbar und nur durch
    // den buffer "rxbuff" im driver beschränkt
//      MultiBlockCount := 16384 div BlockSize ; // immer 16K übertragen
    // immer 32K übertragen. Ist maximum im macro-11 treiber,
    // bringt performance, wenn RLE comprimierung greift.
    MultiBlockCount := 32768 div BlockSize ; 
    DataBytesAreWordLSBs := false ; 
    fixCapacity := true ; 

  end{ "constructor TMediaImage_DiscDevice_MSCP.Create" } ; 

// get device type and geometry from an MSCP controller.
// SerialXfer.XmitBlock0Data must contain the MSCP driver response
// to a SerialTransferDriver_opInit  command.
procedure TMediaImage_DiscDevice_MSCP.EvalDriveInfo(serialxfer_wordbuffer: TArrayOfWord) ; 


  function getMSCPMediaTypeId(code:dword):string ; 
(* from OpenBSD   mscp.h:
 * Macros to break up and build media IDs.  An ID encodes the port
 * type in the top 10 bits, and the drive type in the remaining 22.
 * The 10 bits, and 15 of the 22, are in groups of 5, with the value
 * 0 representing space and values 1..26 representing A..Z.  The low
 * 7 bits represent a number in 0..127.  Hence an RA81 on a UDA50
 * is <D><U><R><A>< >81, or 0x25641051.  This encoding scheme is known
 * in part in uda.c.
 *
 * The casts below are just to make pcc generate better code.
 *)
    function getch(code:dword ; bitpos:integer):Char ; 
      begin 
        code := (code shr bitpos) and $1f ; // 5 bit character code
        if code = 0 then 
          result := '_' // give space as _
        else result := Char(64 + code) ; 
      end ; 

    begin 
      result := getch(code, 27) + getch(code, 22) // port
              + getch(code, 17) + getch(code, 12) + getch(code, 7) 
              + IntToStr(code and $7f) ; 
    end { "function getMSCPMediaTypeId" } ; 

  var 
    mediatypeid: dword ; 

  begin { "procedure TMediaImage_DiscDevice_MSCP.EvalDriveInfo" } 
    // Rückgabe parameter: siehe
    //      result buffer ([word index dec]
    //      [0] unit identifer 1st lo    (from ONLINE)
    //      [1] unit identifer 1st hi    (from ONLINE)
    //      [2] unit identifer 2nd lo    (from ONLINE)
    //      [3] unit identifer 2nd hi    (from ONLINE)
    //      [4] media type identifier lo (from ONLINE)
    //      [5] media type identifier hi (from ONLINE)
    //      [6] reserved                 (from ONLINE)
    //      [7] reserved                 (from ONLINE)
    //      [8] blockcount lo           (from ONLINE)
    //      [9] blockcount hi           (from ONLINE)
    //      [10] volume serial num lo     (from ONLINE)
    //      [11] volume serial num hi     (from ONLINE)
    //      [12] cylinder size            (from GET UNIT STATUS)
    //      [13] group size               (from GET UNIT STATUS)
    //      [14] cylinder size            (from GET UNIT STATUS)
    //      [15] reserved            (from GET UNIT STATUS)
    //      [16] RCT size                 (from GET UNIT STATUS)
    //      [17] RBNS/copies             (from GET UNIT STATUS)
    if Length(serialxfer_wordbuffer) < 14 then begin 
      // error
      BlockCount  := 0 ; 
      SectorCount := 0 ; 
      HeadCount := 0 ; 
      CylinderCount := 0 ; 
      mediatypeid := 0 ; 
      Name := '' ; 
    end else begin 
      // mediatypeid ist ein komprimierter ASCII-Text
      mediatypeid := serialxfer_wordbuffer[4] 
              + integer($10000) * serialxfer_wordbuffer[5] ; 
      Name := getMSCPMediaTypeId(mediatypeid) ; 

      // geometry is now cleared, because setDiscSubType() knows nothing about MSCP
      BlockCount  := serialxfer_wordbuffer[8] 
              + integer($10000) * serialxfer_wordbuffer[9] ; 
      SectorCount := serialxfer_wordbuffer[12] ; 
      HeadCount := serialxfer_wordbuffer[13] ; // heads = groups
      CylinderCount := BlockCount 
              div (SectorCount * HeadCount) ; 
    end{ "if Length(serialxfer_wordbuffer) < 14 ... ELSE" } ; 
  end{ "procedure TMediaImage_DiscDevice_MSCP.EvalDriveInfo" } ; 



function TMediaImage_DiscDevice_MSCP.getBlockRangeDisplay(aStartBlockNr, aCount: integer): string ; 
  begin 
    result := '' ; 

    // hier die Parameterprüfung ... unlogisch

    // Check: block gültig
    assert(aStartBlockNr >= 0) ; 
    assert((aStartBlockNr + aCount) <= BlockCount) ; 

    // Block in cylinder/head/sector umrechnen
    curBlockNr := aStartBlockNr ; 
    //    BlockNr2DiscAddr ;

    if aCount = 1 then 
      result := Format('block %d / %d = %d%%', 
              [aStartBlockNr, BlockCount, 
              100 * aStartBlockNr div BlockCount]) 
    else 
      result := Format('block %d-%d / %d = %d%%', 
              [aStartBlockNr,aStartBlockNr+aCount-1, 
              BlockCount, 
              100 * aStartBlockNr div BlockCount]) ; 
  end { "function TMediaImage_DiscDevice_MSCP.getBlockRangeDisplay" } ; 

procedure TMediaImage_DiscDevice_MSCP.getGeometryDisplay(lines: TStrings) ; 
  begin 
    lines.Add(Format('Block count = %d, block size=%d bytes', 
            [BlockCount,BlockSize])) ; 
    lines.Add('disc geometry unknown') ; 
  end; 


procedure TMediaImage_DiscDevice_MSCP.SetupTransmissionParams( 
        var serialxfer_wordbuffer: TArrayOfWord ; 
        transmittedBlockCount: integer ; 
        var blockinfo: string) ; 
  var flags: word ; 
  begin 
    // flags: bit 0 = suppress read/checkonly
    flags := 0 ; 
    // bits 3:1 = subtype/format (zB: 3 = RX02 double density)
    flags := flags or ((DriverDeviceType and 7) shl 1) ; 
    BlockNr2DiscAddr ; 

    // verify Driver Sourcefilename!
    blockinfo := Format('logical block number = %d', [curBlockNr]) ; 
    blockinfo := Format('cylinder=%d, head=%d, sector=%d', 
            [curCylinder, curHead, curSector]) ; 
    SetLength(serialxfer_wordbuffer, 7) ; 
    serialxfer_wordbuffer[SerialTransferDriver_prcba] := controller.curBaseAddress ; 
    serialxfer_wordbuffer[SerialTransferDriver_prunit] := controller.curUnitNumber ; 
    serialxfer_wordbuffer[SerialTransferDriver_prflags] := flags ; 
    serialxfer_wordbuffer[SerialTransferDriver_prwlen] := transmittedBlockCount  * BlockSize div 2 ; // word count

    serialxfer_wordbuffer[SerialTransferDriver_prstb0] := 
            curBlockNr and $ffff ; // R1 = logical block number low word
    serialxfer_wordbuffer[SerialTransferDriver_prstb1] := 
            curBlockNr shr 16 ;  // R2 = logical block number hi word
  end{ "procedure TMediaImage_DiscDevice_MSCP.SetupTransmissionParams" } ; 


constructor TMediaImage_TapeDevice.Create ; 
  begin 
    inherited ; 
    // todo !
  end; 

function TMediaImage_TapeDevice.getBlockRangeDisplay(aStartBlockNr, aCount: integer): string ; 
  begin 
    // keine % angabe ... unklar, wie lang das tape ist
    if aCount = 1 then 
      result := Format('block %d', [aStartBlockNr]) 
    else 
      result := Format('block %d-%d', [aStartBlockNr, aStartBlockNr + aCount-1]) ; 
  end; 


constructor TMediaImage_TapeDevice_PC05.Create ; 
  begin 
    inherited ; 
    Name := 'PC05' ; 
    BlockCount := 0 ; // no fixCapacity

    BlockSize := 1 ; // single byte read/write
    DriverDeviceType := 0 ; // don't care
    readErrorRetryCount := 0 ; // no chance to step back

    // die Multiblock grösse ist frei wählbar und nur durch
    // den buffer "rxbuff" im driver beschränkt
//      MultiBlockCount := 16384 div BlockSize ; // immer 16K übertragen
    // immer 32K übertragen. Ist maximum im macro-11 treiber,
    // bringt performance, wenn RLE comprimierung greift.
    MultiBlockCount := 256 ;// max 256 bytes in one transmission. 32768 div BlockSize ;

    DataBytesAreWordLSBs := true ; // only the lower 8 bits of each word
    fixCapacity := false ; 

    // contain a data byte

  end{ "constructor TMediaImage_TapeDevice_PC05.Create" } ; 

procedure TMediaImage_TapeDevice_PC05.getGeometryDisplay(lines: TStrings) ; 
  begin 
    lines.Add('Papertape is endless stream of bytes, position cannot be changed') ; 
  end; 


procedure TMediaImage_TapeDevice_PC05.SetupTransmissionParams( 
        var serialxfer_wordbuffer: TArrayOfWord ; 
        transmittedBlockCount: integer ; 
        var blockinfo: string) ; 
  var flags: word ; 
  begin 
    // flags: bit 0 = suppress read/checkonly
    flags := 0 ; 
    // bits 3:1 = subtype/format (zB: 3 = RX02 double density)
    flags := flags or ((DriverDeviceType and 7) shl 1) ; 

    // verify Driver Sourcefilename!
    blockinfo := Format('byte ount = %d', [curBlockNr]) ; 
    SetLength(serialxfer_wordbuffer, 4) ; 
    serialxfer_wordbuffer[SerialTransferDriver_prcba] := controller.curBaseAddress ; 
    serialxfer_wordbuffer[SerialTransferDriver_prunit] := controller.curUnitNumber ; 
    serialxfer_wordbuffer[SerialTransferDriver_prflags] := flags ; 
    // papertape deliver words, but only lower half contains data.
    // byte count = word count!
    serialxfer_wordbuffer[SerialTransferDriver_prwlen] := transmittedBlockCount  * BlockSize ; 
  end{ "procedure TMediaImage_TapeDevice_PC05.SetupTransmissionParams" } ; 


initialization 

  // globale Liste mit allen Controllern und Devices
  TheControllerList := TMediaImage_ControllerList.Create ; 
  TheControllerList.Add(TMediaImage_DiscController_RK11.Create) ; 
  TheControllerList.Add(TMediaImage_DiscController_RK611.Create) ;
  TheControllerList.Add(TMediaImage_DiscController_RL11.Create) ;
  TheControllerList.Add(TMediaImage_DiscController_RH11.Create) ; 
  TheControllerList.Add(TMediaImage_DiscController_RX11.Create) ; 
  TheControllerList.Add(TMediaImage_DiscController_RX211.Create) ; 
  TheControllerList.Add(TMediaImage_DiscController_MSCP.Create) ; 
  TheControllerList.Add(TMediaImage_TapeController_PC11.Create) ; 



end{ "unit MediaImageDevicesU" } . 
