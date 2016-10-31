
unit FormDiscImageU; 
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

Disc-Images lesen/schreiben

  Verfahren: ein Treiberprogramm wird in die cpu geladen.
  Es liest/schreibt pro Aufruf einen bestimmten Sector = Block
  aus einem Sectorpuffer.
  Der Sector puffer wird mit Examine/Deposits übertragen
  und von PDP11GUI in einen image file geschrieben.

  Es werden die MACRO-Forms und die FormExecute benutzt, der User
  sieht das.

Imagefile = list of sectors.

BadBlockList wird immer parallel zum ImageBuffer mitgeführt

}

interface 

uses 
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, ComCtrls, Grids, Buttons, 
  FormChildU, 
  AuxU, 
  AddressU, 
  MemoryCellU, 
  DiscImageBadBlockU, 
  SerialXferU, 
  MediaImageDevicesU, 
  MediaImageBufferU, 
  FormDiscImageExitQueryU 
  ; 

type 
  // Drei States für die Subsystem Disc, Driver und File.
  TDiscImage_DeviceState = ( 
      diDevice_SelectController, // alles unbekannt
      // controller ist bekannt, einstellen von addresse, unit und device.
      diDevice_ReadyToOpenWithoutDevice, // Disktyp ist bekannt: Driver kann geladen werden, und setzt dann selber den subtype
      diDevice_ReadyToOpenWithDevice, //
      diDevice_Open // Treiber geladen, Geometrie bekannt
    ) ; 


  TDiscImage_DriverState = ( 
      diDriver_Init, // nix
      diDriver_Compiling, 
      diDriver_Loading, 
      // Ab hier: Treiber ist geladen. Disk info kann abgefragt oder gesetzt werden
      diDriver_Loaded,  // Treiber geladen, aber gestoppt
      diDriver_Execute, // Treiber läuft, kann nächsten Befehl abarbeiten
      diDriver_Execute_Selftest, 
      diDriver_Execute_GetDriveInfo, 
      diDriver_Execute_Read, 
      diDriver_Execute_Check, 
      diDriver_Execute_Write, 
      diDriver_UserStop, // Treiber wurde durch User gestopped
      diDriver_Error 
//      diDriver_Ready
    ) { "TYPE TDiscImage_DriverState" } ; 

  TDiscImage_ImageFileState = ( 
      diImageFile_Init, 
      diImageFile_Cleared, // Datei geladen/cleared
      diImageFile_Loaded, // Datei geladen/cleared
      diImageFile_Changed, // Datei durch auslesen geändert
      diImageFile_Saved 
    ) ; 


  TFormDiscImage = class(TFormChild) 
      OpenDiskImageDialog: TOpenDialog; 
      GroupBox1: TGroupBox; 
      SaveImageButton: TButton; 
      LoadImageFileButton: TButton; 
      ClearImageButton: TButton; 
      ReadWriteGroupBox: TGroupBox; 
      BlockNrLabel: TLabel; 
      StopOnErrorCheckBox: TCheckBox; 
      ProtectVendorAreaCheckBox: TCheckBox; 
      ReadImageButton: TButton; 
      StopButton: TButton; 
      BlockNrEdit: TEdit; 
      WriteImageButton: TButton; 
      SaveDiskImageDialog: TSaveDialog; 
      OverlayFileButton: TButton; 
      OpenDiskImageOverlayDialog: TOpenDialog; 
      GroupBox3: TGroupBox; 
      SelftestWordcountEdit: TEdit; 
      Label6: TLabel; 
      SelftestButton: TButton; 
      LoadDriverButton: TButton; 
      Label7: TLabel; 
      SelfTestRepeatCountEdit: TEdit; 
      SelftestRLECompressionCheckBox: TCheckBox; 
      GroupBox4: TGroupBox; 
      DriveInfoMemo: TMemo; 
      DriveInfoLabel: TLabel; 
      OpenDeviceButton: TButton; 
      Label8: TLabel; 
      ControllerComboBox: TComboBox; 
      Label3: TLabel; 
      UnitNumberEdit: TEdit; 
      UnitNrUpDown: TUpDown; 
      Label4: TLabel; 
      DeviceComboBox: TComboBox; 
      ReadSingleBlockButton: TButton; 
      WriteSingleBlockButton: TButton; 
      Label10: TLabel; 
      DriverInfoLabel: TLabel; 
      ImageFileInfoLabel: TLabel; 
      DiscInfoLabel: TLabel; 
      SelftestPatternComboBox: TComboBox; 
      Label5: TLabel; 
      InfoLabel: TLabel; 
      CheckSingleBlockButton: TButton; 
      CheckImageButton: TButton; 
      MetainformationGroupBox: TGroupBox; 
      BadBlockListStringGrid: TStringGrid; 
      ReadBadBlocksFromBadSectorFileButton: TButton; 
      WriteBadBlocksToBadSectorFileButton: TButton; 
      MediaSerialNumberEdit: TEdit; 
      Label9: TLabel; 
      UpdateCurBlockEditCheckBox: TCheckBox; 
      BlocksPerTransferLabel: TLabel; 
      BlockPerTransferEdit: TEdit; 
      Label11: TLabel; 
      ControllerBaseAddressEdit: TEdit; 
      DriverStopButton: TButton; 
      DriverInvalidateButton: TButton; 
      SaveMetaInfoButton: TButton; 
      SaveDiskMetaInfoDialog: TSaveDialog; 
      SelftestRandomWordCountCheckBox: TCheckBox; 
      procedure ClearImageButtonClick(Sender: TObject); 
      procedure LoadImageFileButtonClick(Sender: TObject); 
      procedure SaveImageButtonClick(Sender: TObject); 
      procedure ReadImageButtonClick(Sender: TObject); 
      procedure WriteImageButtonClick(Sender: TObject); 
      procedure StopButtonClick(Sender: TObject); 
      procedure DeviceComboBoxChange(Sender: TObject); 
      procedure UnitNumberEditChange(Sender: TObject); 
      procedure OpenDeviceButtonClick(Sender: TObject); 
      procedure ReadSingleBlockButtonClick(Sender: TObject); 
      procedure WriteSingleBlockButtonClick(Sender: TObject); 
      procedure SelftestButtonClick(Sender: TObject); 
      procedure OverlayFileButtonClick(Sender: TObject); 
      procedure LoadDriverButtonClick(Sender: TObject); 
      procedure FormCreate(Sender: TObject); 
      procedure FormDestroy(Sender: TObject); 
      procedure ControllerComboBoxChange(Sender: TObject); 
      procedure ReadBadBlocksFromBadSectorFileButtonClick(Sender: TObject); 
      procedure WriteBadBlocksToBadSectorFileButtonClick(Sender: TObject); 
      procedure MediaSerialNumberEditExit(Sender: TObject); 
      procedure MediaSerialNumberEditChange(Sender: TObject); 
      procedure StopOnErrorCheckBoxClick(Sender: TObject); 
      procedure ProtectVendorAreaCheckBoxClick(Sender: TObject); 
      procedure ControllerBaseAddressEditChange(Sender: TObject); 
      procedure ControllerBaseAddressEditKeyPress(Sender: TObject; var Key: Char); 
      procedure FormDeactivate(Sender: TObject); 
      procedure DriverStopButtonClick(Sender: TObject); 
      procedure DriverInvalidateButtonClick(Sender: TObject); 
      procedure SaveMetaInfoButtonClick(Sender: TObject); 
      procedure UpdateCurBlockEditCheckBoxClick(Sender: TObject);
    private 
      { Private-Deklarationen }
      procedure FormAfterShow(Sender: TObject); 

      function CheckRunMode: boolean ; 

      function getDeviceStateAsText(state: TDiscImage_DeviceState): string ; 
      function getDriverStateAsText(state: TDiscImage_DriverState): string ; 
      function getImageFileStateAsText(state: TDiscImage_ImageFileState): string ; 

      procedure setDeviceState(state: TDiscImage_DeviceState ; msg: string = '') ; 
      procedure setDriverState(state: TDiscImage_DriverState ; msg: string = '') ; 
      procedure setImageFileState(state: TDiscImage_ImageFileState ; msg: string = '') ; 

      procedure setController(aController: TMediaImage_AbstractController) ; 
      procedure controllerParametersChanged ; 

      procedure SetupDriverDiskParams(aBlocknr, BlockOrSectorCount: integer; 
              var blockinfo:string) ; 

      procedure SerialXferStatusChange(info: string) ; 

      procedure LoadDriver ; 
      procedure StartDriver ; 
      procedure StopDriver ; 

      procedure registerBadBlock(badblocknr: integer; source: TBadBlockSource ; info:string); 

    public 
      { Public-Deklarationen }
      MediaImageBuffer: TMediaImage_Buffer ; 

      selectedController: TMediaImage_AbstractController ; 
      selectedDevice : TMediaImage_AbstractDevice ; 

      RestZeit : TRestZeit ; 

      DeviceState: TDiscImage_DeviceState ; 
      DriverState:TDiscImage_DriverState ; 
      ImageFileState:TDiscImage_ImageFileState ; 
      DeviceStateInfo: string ; 
      DriverStateInfo: string ; 
      ImageFileStateInfo: string ; 

      RegistryKeyDiskImageFilename: string ; 
      RegistryKeyDiskMetaInfoFilename: string ; 


//      DriverSourceFilename: string ;

//      DeviceSubTypeName:string ;

      BadBlockList: TBadBlockList ; // index = blocknr

      SerialXfer: TSerialTransfer ; // Objekt zur Datenübertragung

      procedure UpdateDisplay ; 

      procedure Abort ; 

      procedure setDeviceForSelectedController(aDeviceName:string) ; 

      procedure Pdp11ResetAndOpenDisc ;  // Treiber laden, Geometrie einstellen

      // Controller reset und Media-Info aus drive abfragen
      procedure ResetUnibusAndDrive ; 

      // Mehrere blöcke lesen. Assert: startblock muss Sectornr 0 ergeben
      function ReadMultiBlock(aStartBlockNr: integer; blockcount: integer; check_only: boolean): integer ; 
      function WriteMultiBlock(aStartBlockNr: integer; blocksToWrite: integer): integer ; 

      procedure Selftest(wordcount: integer; randomWordcount: boolean ; testpattern:integer) ; 

      // das Image aus dem RL02 lesen nach "ImageBuffer"
      // ab bestimmtem block
      procedure ReadImage(startblocknr: integer ;check_only: boolean) ; 
      // das Image aus dem Imagebuffer in das RL02 schreiben.
      procedure WriteImage(startblocknr: integer) ; 

      // lesen/schreiben, bis Daten zuende oder Error
      procedure ReadImageStream(startblocknr: integer ; check_only: boolean) ; 
      procedure WriteImageStream(startblocknr: integer) ; 


      procedure ClearImageBuffer ; 
      // das Image aus dem RL02 lesen nach "ImageBuffer" lesen
      procedure SaveImageBuffer(filename: string) ; 
      // das Image aus dem RL02 lesen nach "ImageBuffer" lesen
      // overlay: nur Blöcke, die nicht 0 sind
      procedure LoadImageBuffer(filename: string; overlay: boolean = false) ; 

    end{ "TYPE TFormDiscImage = class(TFormChild)" } ; 


implementation 

{$R *.dfm}

uses 
  JH_Utilities, 
  RegistryU, 
  OctalConst, 
  FormMainU, 
  FormExecuteU, 
  ConsoleGenericU; 


procedure TFormDiscImage.FormCreate(Sender: TObject); 
  var i: integer ; 
  begin 
    OnAfterShow := FormAfterShow ; 

    MediaImageBuffer := TMediaImage_Buffer.Create ; 
    BadBlockList := TBadBlockList.Create ; 
    SerialXfer := TSerialTransfer.Create ; 
    RestZeit := TRestZeit.Create ; 
    SerialXfer.OnStatusChange := SerialXferStatusChange ; 

    // Auswahlliste aus Definition
    ControllerComboBox.Clear ; 
    for i := 0 to TheControllerList.Count - 1 do 
      ControllerComboBox.Items.Add(TheControllerList.get(i).name) ; 
    selectedController := nil ; 
    selectedDevice := nil ; 

    setDeviceState(diDevice_SelectController) ; 
    setDriverState(diDriver_Init) ; 
    setImageFileState(diImageFile_Init) ; 

    UpdateDisplay ; 

    TheRegistry.Load(UpdateCurBlockEditCheckBox) ; 
    TheRegistry.Load(StopOnErrorCheckBox) ; 
    TheRegistry.Load(ProtectVendorAreaCheckBox) ; 

  end{ "procedure TFormDiscImage.FormCreate" } ; 




procedure TFormDiscImage.FormDeactivate(Sender: TObject); 
  var 
    mr: TModalResult ; 
  begin 
    if DriverState >= diDriver_Execute then begin 

      // Form darf eigentlich nicht verlassen werden.
      // 3 optionen:
      // - stay in FormDiskImage window
      // - stop driver, dann change window
      // - do nothing
      if FormDiscImageExitQueryForm.DoNotShowAgain then 
        mr := mrOk 
      else 
        mr := FormDiscImageExitQueryForm.ShowModal ; 
      case mr of 
        mrOk: // change window with running driver
          ; 
        mrAbort: begin 
          // Stop driverm then change window
          StopDriver ; 
        end; 
        mrCancel: begin 
          // can not deactivate, if still executing
          BringToFront ; 
        end ; 
      end; 
    end{ "if DriverState >= diDriver_Execute" } ; 
  end{ "procedure TFormDiscImage.FormDeactivate" } ; 


procedure TFormDiscImage.FormDestroy(Sender: TObject); 
  begin 
    SerialXfer.Free ; 
    MediaImageBuffer.Free ; 
    BadBlockList.Free ; 
  end; 

procedure TFormDiscImage.FormAfterShow(Sender: TObject); 
  begin 
    // die gelöschte Warnmeldung wieder anzeigen
    FormDiscImageExitQueryForm.DoNotShowAgain := false ; 
  end; 


function TFormDiscImage.CheckRunMode: boolean ; 
  begin 
    result := true ; 
    try 
      if not (cfNonFatalHalt in FormMain.PDP11Console.getFeatures) 
              and (FormMain.PDP11Console.getMonitorEntryAddress.val = MEMORYCELL_ILLEGALVAL) then 
        MessageDlg('To execute the disc drivers, your PDP-11 must allow'+#13 
                +'continuation after the driver is stopped.'+#13 
                +'This machine stops completely on a HALT,'+#13 
                +'and no "monitor entry address" is defined.', 
                mtError, [mbOk], 0) ; 

      // true, false or exception
      if not FormMain.FormExecute.CheckRunMode then begin 
        MessageDlg('To execute the disc drivers, your PDP-11 must be in "RUN"-mode.'+#13 
                +'At the moment, it is set to "HALT" in the Execution Control Window.'+#13 
                +'Set "RUN" on the PDP-11 and on the Execution Control Window.', mtWarning, 
                [mbOk], 0) ; 
        result := false ; 
      end; 
    except 
      MessageDlg('To execute the disc drivers, your PDP-11 must be in "RUN"-mode.'+#13 
              +'At the moment, it is undefined in the Execution Control Window.'+#13 
              +'Set "RUN" on the PDP-11 and on the Execution Control Window.', mtWarning, 
              [mbOk], 0) ; 
      result := false ; 
    end{ "try" } ; 
  end{ "function TFormDiscImage.CheckRunMode" } ; 

function TFormDiscImage.getDeviceStateAsText(state: TDiscImage_DeviceState): string ; 
  begin 
    case state of 
      diDevice_SelectController: result := 'Device: select controller' ; 
      // diDevice_SelectBaseaddrAndUnitnr: result := 'Device: select address and unit' ;
      diDevice_ReadyToOpenWithoutDevice: result := 'Device: ready to open and query device' ; 
      //diDevice_SelectDevice: result := 'Device: select device type' ;
      diDevice_ReadyToOpenWithDevice: result := 'Device: ready to open' ; 
      diDevice_Open: result := 'Device: open' ; 
      else raise Exception.Create('unknown DeviceState') ; 
    end; 
  end ; 

function TFormDiscImage.getDriverStateAsText(state: TDiscImage_DriverState): string ; 
  begin 
    case state of 
      diDriver_Init: result := 'Driver: not loaded' ; 
      diDriver_Compiling: result := 'Driver: compiling' ; 
      diDriver_Loading: result := 'Driver: loading' ; 
      diDriver_Loaded: result := 'Driver: loaded and stopped' ; 
      diDriver_Execute: result := 'Driver: running and idle' ; 
      diDriver_Execute_Selftest: result := 'Driver: executing selftest' ; 
      diDriver_Execute_GetDriveInfo: result := 'Driver: executing drive info'  ; 
      diDriver_Execute_Read: result := 'Driver: executing read' ; 
      diDriver_Execute_Check: result := 'Driver: executing check'  ; 
      diDriver_Execute_Write: result := 'Driver: executing write'  ; 
      diDriver_UserStop: result := 'Driver: stopped by user' ; 
      diDriver_Error: result := 'Driver: stopped by error' ; 
      else raise Exception.Create('unknown DriverState') ; 
    end{ "case state" } ; 
  end { "function TFormDiscImage.getDriverStateAsText" } ; 

function TFormDiscImage.getImageFileStateAsText(state: TDiscImage_ImageFileState): string ; 
  begin 
    case state of 
      diImageFile_Init: result := 'File image: init, size unknown' ; 
      diImageFile_Cleared: result := 'File image: cleared' ; 
      diImageFile_Loaded: result := 'File image: loaded' ; 
      diImageFile_Changed: result := 'File image: changed' ; 
      diImageFile_Saved: result := 'File image: saved' ; 
      else raise Exception.Create('unknown ImageFileState') ; 
    end; 
  end ; 


// base address or unitnumber changed.
procedure TFormDiscImage.controllerParametersChanged ; 
  begin 
    if selectedController.recognizesDevice then 
      // device ist automatisch klar nach Driver Open
      setDeviceState(diDevice_ReadyToOpenWithoutDevice) 
    else 
      setDeviceState(diDevice_ReadyToOpenWithDevice) ; 
  end; 

// Driverparameter und memoryCellgroups aufsetzen
procedure TFormDiscImage.setController(aController: TMediaImage_AbstractController) ; 
  var i: integer ; 
  begin 
    if aController = selectedController then Exit ; 
    selectedController := aController ; 

    setImageFileState(diImageFile_Init); // Image size ist ungültig

    // fill device selection combobox
    if selectedController.curBaseAddress = 0 then 
      selectedController.curBaseAddress := selectedController.defaultBaseAddress ; 

    DeviceComboBox.Text := '' ; 
    DeviceComboBox.Clear ; 
    DeviceComboBox.Items.Clear ; 
    for i := 0 to selectedController.Devices.Count - 1 do 
      DeviceComboBox.Items.Add(selectedController.Devices.get(i).name) ; 

    // combobox shows only possible or last selected device
    if (DeviceComboBox.ItemIndex < 0) or (DeviceComboBox.Items.Count = 1) then 
      DeviceComboBox.ItemIndex := 0 // force first item
    else if selectedController.curDevice <> nil then 
      DeviceComboBox.ItemIndex := DeviceComboBox.Items.IndexOf(selectedController.curDevice.name) ; 

    // sicherstellen ,das immer ein Device ausgewählt ist.
    ControllerBaseAddressEdit.Text := Dword2OctalStr(selectedController.curBaseAddress,16) ; 
    UnitNumberEdit.Text := IntToStr(selectedController.curUnitNumber) ; 
    controllerParametersChanged ; 
    setDeviceForSelectedController(DeviceComboBox.Text) ; 

    // if the controller type has changed, the currently running driver gets invalid.
    if DriverState >= diDriver_Execute then 
      StopDriver ; 
    setDriverState(diDriver_Init); 

    UpdateDisplay ; 
  end{ "procedure TFormDiscImage.setController" } ; 


// Oberfläche und Buffer für gewähltes device einstellen
// durch manuelle Auswahl oder als Ergebnis von OpenDisc
// "selectedController" muss gültig sein
procedure TFormDiscImage.setDeviceForSelectedController(aDeviceName:string) ; 
  var ext, s: string ; 
    i: integer ; 
    dev:  TMediaImage_AbstractDevice ; 
  begin 
    assert(selectedController <> nil) ; 
    aDeviceName := Uppercase(aDeviceName) ; 

    // search in all controllers for a device named "aDevicename"
    selectedDevice := nil ; 

    for i := 0 to selectedController.Devices.Count - 1 do begin 
      dev := selectedController.Devices.get(i) ; 
      if Uppercase(dev.name) = Uppercase(aDeviceName) then begin 
        selectedDevice := dev ; 
        selectedController.curDevice := dev ; 
      end; 
    end; 
    if selectedDevice = nil then 
      raise Exception.CreateFmt('Device "%s" not found!', [aDeviceName]) ; 

    // setup file dialogs:
    // Disc subtype is file extension. replace space with '_'
    ext := LowerCase(aDeviceName) ; 
    for i := 1 to length(ext) do 
      if ext[i] = ' ' then ext[i] := '_' ; 

    RegistryKeyDiskImageFilename := 'DiskImageFilename_' + ext ; 
    OpenDiskImageDialog.DefaultExt := ext ; 
    SaveDiskImageDialog.DefaultExt := ext ; 
    s := Format('%s disc images|*.%s|All files|*.*', 
            [Uppercase(aDeviceName), ext]) ; 
    OpenDiskImageDialog.Filter := s ; 
    SaveDiskImageDialog.Filter := s ; 

    RegistryKeyDiskMetaInfoFilename := 'DiskMetaInfoFilename_' + ext ; 
    ext := ext + '_meta' ; 
    SaveDiskMetaInfoDialog.DefaultExt := ext ; 
    s := Format('%s disc meta info|*.%s|All files|*.*', 
            [Uppercase(aDeviceName), ext]) ; 
    SaveDiskMetaInfoDialog.Filter := s ; 

    MediaImageBuffer.LinkToDevice(selectedDevice); 
    // bei MSCP kommt das nochmal mit anderen Grössendaten!
    ClearImageBuffer ; 

    BlockPerTransferEdit.Text := IntToStr(selectedDevice.MultiBlockCount) ; 
    BlockNrEdit.Text := '0' ; 

    BadBlockList.Init(selectedDevice.blockcount, BadBlockListStringGrid) ; 
  end{ "procedure TFormDiscImage.setDeviceForSelectedController" } ; 


procedure TFormDiscImage.DeviceComboBoxChange(Sender: TObject); 
  begin 
    // manuell Device gewählt
    setDeviceForSelectedController(DeviceComboBox.Text) ; 
  end; 



procedure TFormDiscImage.ControllerComboBoxChange(Sender: TObject); 
  begin 
    if not CheckRunMode then begin 
      ControllerComboBox.ItemIndex := -1 ; // Auswahl ungültig
      Exit ; 
    end; 

    if ControllerComboBox.ItemIndex < 0 then Exit ; 
    // combobox index = Index in controller list
    setController(TheControllerList.get(ControllerComboBox.ItemIndex)) ; 
  end; 



procedure TFormDiscImage.ControllerBaseAddressEditKeyPress(Sender: TObject; 
        var Key: Char); 
  begin 
    // nur Octalziffern als Eingabe. Auch das backspace #8 erlauben
    if not CharInSet(Key, [#8, '0'..'7']) then 
      Key := #0 ; 
  end; 


procedure TFormDiscImage.ControllerBaseAddressEditChange(Sender: TObject); 
  var val: dword ; 
  begin 
    if ControllerBaseAddressEdit.Enabled then begin 
      val := AuxU.OctalStr2Dword(ControllerBaseAddressEdit.Text, 16) ; 
      if val <> MEMORYCELL_ILLEGALVAL then begin 
        selectedController.curBaseAddress := val ; 
        controllerParametersChanged ; 
      end; 
    end; 
  end; 



procedure TFormDiscImage.UnitNumberEditChange(Sender: TObject); 
  begin 
    if UnitNumberEdit.Enabled then begin 
      selectedController.curUnitNumber := StrToInt(UnitNumberEdit.Text) ; 
      controllerParametersChanged ; 
    end; 
  end; 


procedure TFormDiscImage.Abort ; 
  begin 
    setDriverState(diDriver_UserStop); 
    SerialXfer.Aborted := true ; // transfermodul auch stoppen
  end; 


// für einen logischen Block die Parameter in block 0 setzen
// hängt vom DiscTyp und vom Assembler-Driver-programm ab
// In "blockinfo" wird ein anzeigbarer String über cylinder, head, sector gebildet
procedure TFormDiscImage.SetupDriverDiskParams(aBlocknr, BlockOrSectorCount: integer; 
        var blockinfo:string) ; 
  var flags: word ; 
  begin 
    blockinfo := '' ; 

    selectedDevice.curBlockNr := aBlocknr ; 
    assert(selectedController.curUnitNumber < 8) ; 
    assert(selectedController.curBaseAddress >= _160000) ; // must be iopage

    // let selectedDevice do it.  it delegatest most to controller
    selectedDevice.SetupTransmissionParams(SerialXfer.XmitBlock0Data, BlockOrSectorCount, blockinfo) ; 
    // blockinfo now contains a info string like 'cylinder=2, head=1, sector=7'

  end{ "procedure TFormDiscImage.SetupDriverDiskParams" } ; 


procedure TFormDiscImage.SerialXferStatusChange(info: string) ; 
  begin 
    DriverStateInfo := info ; 
    UpdateDisplay ; 
  end; 


procedure TFormDiscImage.UpdateDisplay ; 

  function assembleCaption(a, b: string): string ; 
    begin 
      if b = '' then 
        result := a + '.' 
      else result := a + '; ' + b + '.' ; 
    end ; 

  var 
    captionmsg: string ; 
    s: string ; 
    i: integer ; 
  begin 
    DriveInfoMemo.Clear ; 
    if DeviceState = diDevice_Open then begin 
      // show name in combobox, wichtig für MSCP
      // combobx is now disabled, but must have style "dropDown"
      DeviceComboBox.Text := selectedDevice.name ; //
      selectedDevice.getGeometryDisplay(DriveInfoMemo.Lines) ; 
    end else 
      DriveInfoMemo.Lines.Add('Disc not open') ; 


    // Controls nur für Disc
    captionmsg := '' ; 
    case DeviceState of 
      diDevice_SelectController: begin 

        UnitNrUpDown.Enabled := false ; 
        ControllerBaseAddressEdit.Enabled := false ; 
        ControllerBaseAddressEdit.Text := '' ; 
        UnitNumberEdit.Enabled := false ; 
        DeviceComboBox.Clear ; 
        DeviceComboBox.Enabled := false ; 
        OpenDeviceButton.Enabled := false ; 
        ReadWriteGroupBox.Visible := false ; 
        DriverState := diDriver_Init ; // Treiber ungültig
        LoadDriverButton.Enabled := false ; 
        BlockPerTransferEdit.Enabled := false ; 
        captionmsg := getDeviceStateAsText(DeviceState) ; 
      end{ "case DeviceState of diDevice_SelectController:" } ; 
      diDevice_ReadyToOpenWithoutDevice: begin 
        UnitNrUpDown.Enabled := true ; 
        ControllerBaseAddressEdit.Enabled := true ; 
        UnitNumberEdit.Enabled := true ; 
        DeviceComboBox.Enabled := false ; 
        DeviceComboBox.Visible := false ; 
        OpenDeviceButton.Enabled := true ; 
        LoadDriverButton.Enabled := true ; 
        ReadWriteGroupBox.Visible := false ; 
        BlockPerTransferEdit.Enabled := false ; 
        captionmsg := getDeviceStateAsText(DeviceState) ; 
      end; 
      diDevice_ReadyToOpenWithDevice: begin 
        UnitNrUpDown.Enabled := true ; 
        ControllerBaseAddressEdit.Enabled := true ; 
        UnitNumberEdit.Enabled := true ; 
        DeviceComboBox.Enabled := true ; 
        DeviceComboBox.Visible := true ; 
        OpenDeviceButton.Enabled := true ; 
        LoadDriverButton.Enabled := true ; 
        ReadWriteGroupBox.Visible := false ; 
        BlockPerTransferEdit.Enabled := true ; 
        captionmsg := getDeviceStateAsText(DeviceState) ; 
      end; 
      diDevice_Open : begin 
        ControllerBaseAddressEdit.Enabled := true ; 
        UnitNumberEdit.Enabled := true ; 
        // DeviceComboBox is left disabled for MSCP and enabled for other devices
        // DeviceComboBox.Enabled := true ;
        DeviceComboBox.Visible := true ; 
        OpenDeviceButton.Enabled := true ; 
        LoadDriverButton.Enabled := true ; 
        ReadWriteGroupBox.Visible := true ; 
        BlockPerTransferEdit.Enabled := true ; 
        captionmsg := getDeviceStateAsText(DeviceState) ; 
      end; 
    end{ "case DeviceState" } ; 
    DiscInfoLabel.Caption := assembleCaption(captionmsg, DeviceStateInfo) ; 

    // Controls nur für Driver
    captionmsg := '' ; 
    case DriverState of 
      diDriver_Init: begin 
        DriverInvalidateButton.Enabled := false ;
        DriverStopButton.Enabled := false ;
        SelftestButton.Enabled := false ;
        SelftestWordcountEdit.Enabled := false ;
        SelfTestRepeatCountEdit.Enabled := false ;
        SelftestRLECompressionCheckBox.Enabled := false ;
        SelftestRandomWordCountCheckBox.Enabled := false ;
        SelftestPatternComboBox.Enabled := false ;
        captionmsg := getDriverStateAsText(DriverState) ;
      end ;
      diDriver_Compiling: begin
        SelftestButton.Enabled := false ;
        SelftestWordcountEdit.Enabled := false ;
        SelfTestRepeatCountEdit.Enabled := false ;
        SelftestRLECompressionCheckBox.Enabled := false ;
        SelftestRandomWordCountCheckBox.Enabled := false ;
        SelftestPatternComboBox.Enabled := false ;
        captionmsg := getDriverStateAsText(DriverState) ;
      end ;
      diDriver_Loading:begin //driver loaded, but not started
        captionmsg := getDriverStateAsText(DriverState) ;
        SelftestButton.Enabled := false ;
        SelftestWordcountEdit.Enabled := false ;
        SelfTestRepeatCountEdit.Enabled := false ;
        SelftestRLECompressionCheckBox.Enabled := false ;
        SelftestRandomWordCountCheckBox.Enabled := false ;
        SelftestPatternComboBox.Enabled := false ;
        captionmsg := getDriverStateAsText(DriverState) ;
      end ;
      diDriver_Loaded: begin
        DriverStopButton.Enabled := false ;
        DriverInvalidateButton.Enabled := true ;
        SelftestWordcountEdit.Enabled := false ;
        SelfTestRepeatCountEdit.Enabled := false ;
        SelftestRLECompressionCheckBox.Enabled := false ;
        SelftestRandomWordCountCheckBox.Enabled := false ;
        SelftestPatternComboBox.Enabled := false ;
        captionmsg := getDriverStateAsText(DriverState) ;
      end ;
      diDriver_Execute,
      diDriver_UserStop,
      diDriver_Error:
        begin
          DriverStopButton.Enabled := true ;
          DriverInvalidateButton.Enabled := false ;
          SelftestButton.Enabled := true ;
          SelftestWordcountEdit.Enabled := true ;
          SelfTestRepeatCountEdit.Enabled := true ;
          SelftestRLECompressionCheckBox.Enabled := true ;
          SelftestRandomWordCountCheckBox.Enabled := true ;
        SelftestPatternComboBox.Enabled := true ;
          captionmsg := getDriverStateAsText(DriverState) ;
        end ;
      diDriver_Execute_Selftest,
      diDriver_Execute_GetDriveInfo,
      diDriver_Execute_Read,
      diDriver_Execute_Check,
      diDriver_Execute_Write: begin
        SelftestButton.Enabled := false ;
        DriverInvalidateButton.Enabled := false ;
        SelftestWordcountEdit.Enabled := false ;
        SelfTestRepeatCountEdit.Enabled := false ;
        SelftestRLECompressionCheckBox.Enabled := false ;
        SelftestRandomWordCountCheckBox.Enabled := false ;
        SelftestPatternComboBox.Enabled := false ;
        captionmsg := getDriverStateAsText(DriverState) ;
      end;
    end { "case DriverState" } ;
    DriverInfoLabel.Caption := assembleCaption(captionmsg, DriverStateInfo) ;

    // Controls nur für File
    captionmsg := '' ;
    case ImageFileState of
      diImageFile_Init: begin // Grösse unklar
        ClearImageButton.Enabled := false ;
        LoadImageFileButton.Enabled := false ;
        SaveImageButton.Enabled := false ;
        SaveMetaInfoButton.Enabled := false ;
        OverlayFileButton.Enabled := false ;
        captionmsg := getImageFileStateAsText(ImageFileState) ;
        MetainformationGroupBox.Visible := false ;
      end;
      diImageFile_Cleared: begin
        // image leer: nur laden + overlay erlauben
        ClearImageButton.Enabled := false ;
        LoadImageFileButton.Enabled := true ;
        SaveImageButton.Enabled := true ;
        SaveMetaInfoButton.Enabled := true ;
        OverlayFileButton.Enabled := true ;
        captionmsg := getImageFileStateAsText(ImageFileState) ;
        MetainformationGroupBox.Visible := true ;
        ReadBadBlocksFromBadSectorFileButton.Enabled := false ;
        WriteBadBlocksToBadSectorFileButton.Enabled := false ;
        MediaSerialNumberEdit.Enabled := false ;
      end{ "case ImageFileState of diImageFile_Cleared:" } ;
      diImageFile_Loaded: begin
        // wenn File geladen: Löschen und Ändern erlauben.
        ClearImageButton.Enabled := true ;
        LoadImageFileButton.Enabled := true ;
        SaveImageButton.Enabled := true  ;
        SaveMetaInfoButton.Enabled := true  ;
        OverlayFileButton.Enabled := true ;
        captionmsg := getImageFileStateAsText(ImageFileState) ;
        MetainformationGroupBox.Visible := true ; 
        ReadBadBlocksFromBadSectorFileButton.Enabled := true ; 
        WriteBadBlocksToBadSectorFileButton.Enabled := true ; 
        MediaSerialNumberEdit.Enabled := true ; 
      end{ "case ImageFileState of diImageFile_Loaded:" } ; 
      diImageFile_Changed: begin 
        // wenn Daten von Disk oder Overlay: nur Löschen oder Speichern oder Ändern erlauben
        ClearImageButton.Enabled := true ; 
        LoadImageFileButton.Enabled := false ; 
        SaveImageButton.Enabled := true ; 
        SaveMetaInfoButton.Enabled := true  ; 
        OverlayFileButton.Enabled := true ; 
        captionmsg := getImageFileStateAsText(ImageFileState) ; 
        MetainformationGroupBox.Visible := true ; 
        ReadBadBlocksFromBadSectorFileButton.Enabled := true ; 
        WriteBadBlocksToBadSectorFileButton.Enabled := true ; 
        MediaSerialNumberEdit.Enabled := true ; 
      end{ "case ImageFileState of diImageFile_Changed:" } ; 
      diImageFile_Saved: begin 
        // wenn Daten save: kein weiteres Save erlauben
        ClearImageButton.Enabled := true ; 
        LoadImageFileButton.Enabled := true ; 
        SaveImageButton.Enabled := true ; 
        SaveMetaInfoButton.Enabled := true  ; 
        OverlayFileButton.Enabled := true ; 
        captionmsg := getImageFileStateAsText(ImageFileState) ; 
        MetainformationGroupBox.Visible := true ; 
        ReadBadBlocksFromBadSectorFileButton.Enabled := true ; 
        WriteBadBlocksToBadSectorFileButton.Enabled := true ; 
        MediaSerialNumberEdit.Enabled := true ; 
      end{ "case ImageFileState of diImageFile_Saved:" } ; 
    end{ "case ImageFileState" } ; 

    // Edit normieren
    MediaSerialNumberEdit.Text := Format('%0.8x', [BadBlockList.CartridgeSerialNumber]) ; 


    ImageFileInfoLabel.Caption := assembleCaption(captionmsg, ImageFileStateInfo) ; 

    // Schreiben möglich, wenn DiscOpen, Driver geladen, Image gültig
    if (DeviceState = diDevice_Open) 
            and (DriverState >= diDriver_Loaded) 
            and (ImageFileState >= diImageFile_Cleared) then begin 
      WriteImageButton.Enabled := true ; 
      WriteSingleBlockButton.Enabled := true ; 
    end 
    else begin // auch, wenn driver busy
      WriteImageButton.Enabled := false ; 
      WriteSingleBlockButton.Enabled := false ; 
    end; 

    // Lesen möglich, wenn DiscOpen, Driver geladen
    if (DeviceState = diDevice_Open) 
            and (DriverState >= diDriver_Loaded) then begin 
      BlockNrLabel.Caption := Format('Next %s:', [selectedController.dataBlockName]) ; 
      UpdateCurBlockEditCheckBox.Caption := Format('Track "next %s"', [selectedController.dataBlockName]) ; 
      BlocksPerTransferLabel.Caption := Format('%ss/transfer:', [selectedController.dataBlockName]) ; 
      ReadImageButton.Caption := Format('Read all from ''%s nr'' to end', [selectedController.dataBlockName]) ; 
      ReadSingleBlockButton.Caption := Format('Read single %s ''%s nr''', [selectedController.dataBlockName, selectedController.dataBlockName]) ; 
      WriteImageButton.Caption := Format('Write all from ''%s nr'' to end', [selectedController.dataBlockName]) ; 
      WriteSingleBlockButton.Caption := Format('Write single %s ''%s nr''', [selectedController.dataBlockName, selectedController.dataBlockName]) ; 
      CheckImageButton.Caption := Format('Check all from ''%s nr'' to end', [selectedController.dataBlockName]) ; 
      CheckSingleBlockButton.Caption := Format('Check single %s ''%s nr''', [selectedController.dataBlockName, selectedController.dataBlockName]) ; 

      ReadImageButton.Enabled := true ; 
      ReadSingleBlockButton.Enabled := true ; 
      CheckImageButton.Enabled := true ; 
      CheckSingleBlockButton.Enabled := true ; 
      StopButton.Enabled := true ; // hängt auch nur von DeviceState und DriverState ab
    end { "if (DeviceState = diDevice_Open) and (DriverState >= diDriver_Loaded)" } else begin 
      ReadImageButton.Enabled := false ; 
      ReadSingleBlockButton.Enabled := false ; 
      CheckImageButton.Enabled := false ; 
      CheckSingleBlockButton.Enabled := false ; 
      StopButton.Enabled := false ; 
    end; 

    // Restzeit anzeigen, wenn driver aktiv
    s := '' ; 
    if (DriverState in [diDriver_Execute_Read, diDriver_Execute_Check, diDriver_Execute_Write]) 
            and RestZeit.valid then 
      s := RestZeit.getRestZeit ; 
    if  s = '' then 
      InfoLabel.Caption := '' 
    else 
      InfoLabel.Caption := 'Remaining time: ' + s  ; 

    // state kurz in Form caption anzeigen
    if parent <> nil then with parent as TForm do begin 
        Caption := setFormCaptionInfoField(Caption, captionmsg) ; 
        BringToFront ; 
      end; 

    // Eingaben korrigieren
    // auf Bereich 1.. DiscImage.MultiBlockCount einschränken
    if BlockPerTransferEdit.Visible then 

      if selectedDevice <> nil then 
        if not TryStrToInt(BlockPerTransferEdit.Text, i) then 
          BlockPerTransferEdit.Text := IntToStr(selectedDevice.MultiBlockCount) 
        else begin 
          if i < 1 then i := 1 ; 
          if i > selectedDevice.MultiBlockCount then 
            i := selectedDevice.MultiBlockCount ; 
          BlockPerTransferEdit.Text := IntToStr(i) ; 
        end ; 

(*
    // langer Text in InfoLabel
    if (captionmsg <> '') and (StateInfo <> '') then
      captionmsg := captionmsg + ': ' + StateInfo ;

    PhaseInfoLabel.Caption := captionmsg ;
*)
    Application.ProcessMessages ; 
  end{ "procedure TFormDiscImage.UpdateDisplay" } ; 


procedure TFormDiscImage.setDeviceState(state: TDiscImage_DeviceState ; msg: string) ; 
  begin 
    Log('new disc state = "%s"', [getDeviceStateAsText(state)]) ; 
    DeviceState := state ; 
    DeviceStateInfo := msg ; 
    UpdateDisplay ; 
  end; 

procedure TFormDiscImage.setDriverState(state: TDiscImage_DriverState ; msg: string) ; 
  begin 
    Log('new driver state = "%s"', [getDriverStateAsText(state)]) ; 
    DriverState := state ; 
    DriverStateInfo := msg ; 
    UpdateDisplay ; 
  end; 

procedure TFormDiscImage.setImageFileState(state: TDiscImage_ImageFileState ; msg: string) ; 
  begin 
    Log('new imagefile state = "%s"', [getImageFileStateAsText(state)]) ; 
    ImageFileState := state ; 
    ImageFileStateInfo := msg ; 
    UpdateDisplay ; 
  end; 


// Die Source des Driver Programs laden, übersetzen und den Code schreiben
procedure TFormDiscImage.LoadDriver ; 
  var 
    filename: string ; 
    fullfname1, fullfname2: string ; 
    fullfname: string ; 
  begin 
    try 
      filename := selectedController.DriverSourceFilename ; 
      // suche relativ zum Verzeichnis  [Personalfolder]
      // "filename" enthält schon das prefix '\driver\' !
      fullfname1 := FormMain.DefaultDataDirectory + '\' + filename ; 
      fullfname2 := ExtractFilePath(Application.ExeName)  + '\' + filename ; // wenn ich entwickle
      if FileExists(fullfname1) then 
        fullfname := fullfname1 
      else if FileExists(fullfname2) then 
        // fall back: im Verzeichnis von PDP11GUI.EXE ?
        fullfname := fullfname2 
      else begin 
        setDriverState(diDriver_Init, 'Neither file "' + fullfname1 + '" nor '+ fullfname2+' found') ; 
        Exit ; 
      end ; 
      if FormMain.FormMacro11Source.Changed then begin 
        setDriverState(diDriver_Init, 'Unsaved changes in MACRO-11 source code window, can not use it.') ; 
        Exit ; 
      end; 
      // load readonly, so no "Save" is attempted, and *driver.mac can
      // resist in protected directory
      FormMain.FormMacro11Source.LoadFile(fullfname); 

      setDriverState(diDriver_Compiling, '"' + filename + '"') ; 
      FormMain.FormMacro11Source.Compile ; 
      if not FormMain.FormMacro11Source.Translated then begin 
        setDriverState(diDriver_Init, 'Compilation error in file "' + filename + '"') ; 
        Exit ; 
      end; 

      setDriverState(diDriver_Loading, '"' + filename + '"') ; 
      FormMain.FormMacro11Listing.Deposit ; 
      if not FormMain.FormMacro11Listing.DepositSuccess then begin 
        setDriverState(diDriver_Init, 'Error: Code not loaded') ; 
        Exit ; 
      end; 

      setDriverState(diDriver_Loaded, '"' + filename + '"') ; 
    finally 
    end{ "try" } ; 

  end{ "procedure TFormDiscImage.LoadDriver" } ; 


// start the driver, by executing "GO <start address>"
// then discard any serial output ("(Program)" message on 11/44 v3.40)
procedure TFormDiscImage.StartDriver ;
  var
    pdp11ProcAddr: TMemoryAddress;
  begin
    // einziger Einsprung in den Treiber
    pdp11ProcAddr.mat := matVirtual ;
    pdp11ProcAddr.val := _10000 ;

    // Zielprocedur starten
    // Startadresse des Drivers in der ExecuteForm eintragen
    FormMain.FormExecute.StartPCEdit.Text := Addr2OctalStr(pdp11ProcAddr) ;
    // starten
    Application.ProcessMessages ;

    //  cfActionResetMaschineAndStartCpu wird von allen Maschinen unterstützt
    FormMain.FormExecute.doResetMachineAndSetPCandStart ;
//    else begin
    // START CODE WITHOUT HARD RESET
    // Reset may wipe out any previous device initialization!
//      FormMain.FormExecute.doSetPCandContinue(pdp11ProcAddr) ;

//      if FormMain.PDP11Console is TConsolePDP1144 then begin
//        // Nach Continue: Wartepause nötig für 11/44: Console processor hang up?
//        // sleep(250) ist nicht immer OK, sleep(300) scheint OK
//        sleep(350) ;
//      end;
    //  end ;
    // "350" nötig für 11/44, "documented" firmware
    // "250" nötig für 11/23: Sichtbarkeit der Startanweisung in ODT

//    WaitAndProcessMessages(350) ;
    // 11/44, v.340: prints ("Program")
    FormMain.SerialIoHub.DiscardPhysicalInput(500) ;


    // Driver now running, and Bus reset done

    setDriverState(diDriver_Execute, '') ;
  end{ "procedure TFormDiscImage.StartDriver" } ;

// stop the driver, by sending him a STOP opcode
// later to be replaced by a "Jump to Monitor" (165020 for M9312 driver
procedure TFormDiscImage.StopDriver ;
  var
    opcode: word ;
    monitor_entryaddress_val: dword ;
  begin
    opcode := SerialTransferDriver_ophalt ; 
    // HALT has one argument.
    // if 0: execute HALT
    // else it is interpreted as a monitor entry address (M9312 console emulator),
    //  then driver exits with jump to monitor.
    SetLength(SerialXfer.XmitBlock0Data, 1); 
    monitor_entryaddress_val := FormMain.PDP11Console.getMonitorEntryAddress.val ; 
    if monitor_entryaddress_val = MEMORYCELL_ILLEGALVAL then 
      monitor_entryaddress_val := 0 ; // 0 = invalid, use HALT
    SerialXfer.XmitBlock0Data[SerialTransferDriver_prmnea] := monitor_entryaddress_val ; 

    SetLength(SerialXfer.XmitBlock1Data, 0); 

    SerialXfer.Execute( 
            0, // es kommt keine Antwort
            opcode 
            ) ; // throws ESerialTransferAppError

    // M9312 console emulator: CPU stops, no answer: here Exception ESerialTransferHardError

    setDriverState(diDriver_Loaded, '') ; 
  end{ "procedure TFormDiscImage.StopDriver" } ; 


procedure TFormDiscImage.LoadDriverButtonClick(Sender: TObject); 
  begin 
    LoadDriver ; 
  end; 

// driver will be reloaded on next operation
procedure TFormDiscImage.DriverInvalidateButtonClick(Sender: TObject); 
  begin 
    setDeviceState(diDevice_ReadyToOpenWithDevice) ; 
    setDriverState(diDriver_Init) ; 
    setImageFileState(diImageFile_Init) ; 
    // same as after fresh start, but controller/drive select remains

    UpdateDisplay ; 
  end; 


procedure TFormDiscImage.DriverStopButtonClick(Sender: TObject); 
  begin 
    StopDriver ; 
  end; 


procedure TFormDiscImage.OpenDeviceButtonClick(Sender: TObject); 
  begin 
    Pdp11ResetAndOpenDisc ;
  end; 

// Drive reset.
// liefert ggf Drive SubType zurück in SerialXFER.XmitBlock0Data
procedure TFormDiscImage.ResetUnibusAndDrive ; 
  var 
    //s: string ;
    opcode: word ; 
    errorloc: dword ; 
  begin 
    if DriverState = diDriver_UserStop then Exit ; 

    try 
      try 
        assert(selectedController.curUnitNumber < 8) ; 

        setDriverState(diDriver_Execute_GetDriveInfo) ; 

        // Reset UNIBUS for all drivers
        opcode := SerialTransferDriver_oprest ; 
        SetLength(SerialXfer.XmitBlock0Data, 0) ; 
        SetLength(SerialXfer.XmitBlock1Data, 0) ; 
        try 
          SerialXfer.Execute( 
                  1000, // BUS reset dauert 70ms
                  opcode) ; 
        except 
          // hat der Driver einen Fehler gemeldet? Zeige ihn an
          on E: ESerialTransferAppError do begin 
            setDriverState(diDriver_Error, E.message) ; 
            Exit ; 
          end; 
        end; 

        opcode := SerialTransferDriver_opInit ; 
        SetLength(SerialXfer.XmitBlock0Data, 4) ; 
        SetLength(SerialXfer.XmitBlock1Data, 0) ; 
        SerialXfer.XmitBlock0Data[SerialTransferDriver_prcba] := 
                selectedController.curBaseAddress ; 
        SerialXfer.XmitBlock0Data[SerialTransferDriver_prunit] := 
                selectedController.curUnitNumber ; 
        SerialXfer.XmitBlock0Data[SerialTransferDriver_prflags] := 0 ; 
        SerialXfer.XmitBlock0Data[SerialTransferDriver_prwlen] := 0 ; 
        // other parameters are not used

        try 
          SerialXfer.Execute( 
                  60000, // worst time für einen sector zugriff in ms: 60 sec!!!
                  opcode) ; // Reset Controller
          // opcode muss 0 sein, wenn OK
          errorloc := opcode ; 
          if errorloc <> 0 then begin // dump error words
            setDriverState(diDriver_Error, Format('error location = %s, error info = %s!', 
                    [Dword2OctalStr(errorloc,0), 
                    SerialXfer.WordArrayAsText(SerialXfer.XmitBlock0Data)])) ; 
            // Fehlertext landet in DriverStateInfo
            Exit ; 
          end; 
        except 
          // hat der Driver einen Fehler gemeldet? Zeige ihn an
          on E: ESerialTransferAppError do begin 
            setDriverState(diDriver_Error, E.message) ; 
            Exit ; 
          end; 
        end{ "try" } ; 
        // UnibusResetDone := true ;
        if DriverState = diDriver_UserStop then Exit ; 

      except // eat exception and display
        on E: Exception do begin 
          setDriverState(diDriver_Error, E.message) ; 
          raise ; 
        end; 
      end{ "try" } ; 

    finally 
      if DriverState in [diDriver_Error, diDriver_UserStop] then 
        Log('ReadDriveInfo() aborted') 
      else 
        setDriverState(diDriver_Execute) ; 
    end { "try" } ; 
  end{ "procedure TFormDiscImage.ResetUnibusAndDrive" } ; 

//resetdrive für alle drivetypes
//auswertung der rückgabedaten abh von disk type

//das hier nur für MSCP!!!


// Daten über den seriellen loader übertragen:
// Format: 8 characters definieren jeweils 6 bits
// 8 chars <=> 3 words
// Code in serialxfer.mac, das von den disc loadern .INCLUded wird.


// started das driver-Program mit Parametern in R0..R5
// überträgt das Ergebnis aus dem Sectorbuffer in den ImageBuffer
//
// der RL02 driver kann mehrere Blöcke auf derselben Track lesen.
// check_only: wenn true, werden die gelesenen Daten nicht übertragen,
//  dient zum bad sector scan.
//result: anzahl der gelesenen Blöcke
function TFormDiscImage.ReadMultiBlock(aStartBlockNr: integer ; blockcount: integer ; 
        check_only: boolean): integer ; 
  var 
    pdp11proctimeout_ms: integer ; // maximale laufzeit des Zugriffs in ms
    i, n: integer ; 
    errorloc: dword ; 
    blocksRead: integer ; 
    //s: string ;
    info: string ; 
    opcode: word ; 
  begin 
//OutputDebugString('ReadMultiBlock(): startblock=%d, blockcount=%d', [aStartBlockNr, blockcount]) ;
    blocksRead := 0 ; 
    if DriverState = diDriver_UserStop then Exit ; 

    try 
      if check_only then begin 
        setDriverState(diDriver_Execute_Check) ; 
        opcode := SerialTransferDriver_opchek 
      end else begin 
        setDriverState(diDriver_Execute_Read) ; 
        opcode := SerialTransferDriver_opread ; 
      end; 
      try 
        // Checke und baue Infostring der form "block i/n, cyl,head,sector...= t/h/n"
        info := selectedDevice.getBlockRangeDisplay(aStartBlockNr, blockcount) ; 
        if check_only then 
          DeviceStateInfo := Format('checking %s', [info]) 
        else DeviceStateInfo := Format('reading %s', [info]) ; 

        // Register als Inputparameter aufsetzen
        SetupDriverDiskParams(aStartBlockNr, blockcount, info) ; 

        // Laufzeit der Prozedur: max 100ms pro block
        // langsamstes bisher: RX=2: 360rpm => 167ms/rotation
        //  mit retry 3 Umdrehungen/sector =  500ms !
        pdp11proctimeout_ms := 500 * blockcount + selectedDevice.maxBlockAccessTime_ms; 

        SetLength(SerialXfer.XmitBlock1Data, 0) ; // keine DiskDaten übertragen, nur register

        RestZeit.setCur(aStartBlockNr) ; 
        UpdateDisplay ; 

        SerialXfer.Execute( 
                pdp11proctimeout_ms, // worst Zeit für alle sector zugriffe in ms,
                opcode) ; // Controller nicht reset
        if DriverState = diDriver_UserStop then Exit ; 

        // opcode muss 0 sein, wenn OK
        errorloc := opcode ; 

{$ifdef DEBUG}
        // Simulate read error: octal 666
//        if aStartBlockNr >= 100 then begin errorloc := 438 ;
//           SetLength(SerialXfer.XmitBlock0Data, 4) ;
//         end ;
{$endif}

        if errorloc <> 0 then begin // dump error words
          setDriverState(diDriver_Error, Format('error location = %s, error info = %s!', 
                  [Dword2OctalStr(errorloc,0), 
                  SerialXfer.WordArrayAsText(SerialXfer.XmitBlock0Data)])) ; 
          // Fehlertext landet in DriverStateInfo
          Exit ; 
        end; 

        if not check_only then begin 
          // Daten aus response Block0 in das DiskImage kopieren
          // Länge von Block 0 ist angepasst
          n := length(SerialXfer.XmitBlock0Data) ; // m = bytes
//      assert(n * 2 >= DiscGeometry.Blocksize) ;

          if selectedDevice.DataBytesAreWordLSBs then begin 
            for i := 0 to n-1 do 
              MediaImageBuffer.setImageBufferBlockByte(aStartBlockNr, i, 
                      SerialXfer.XmitBlock0Data[i] and $ff) ; 
            blocksRead := n div selectedDevice.BlockSize ; 
          end else begin 
            for i := 0 to n-1 do 
              MediaImageBuffer.setImageBufferBlockWord(aStartBlockNr, i, 
                      SerialXfer.XmitBlock0Data[i]) ; 
            blocksRead := (2 * n) div selectedDevice.BlockSize ;  // n words = 2n bytes
          end; 
//OutputDebugString('transferred bytes=%d, blocksread = %d, blocksize=%d', [n, blocksRead, selectedDevice.BlockSize]) ;
        end { "if not check_only" } ; 
      except // eat exception and display
        on E: Exception do begin 
          setDriverState(diDriver_Error, E.message) ; 
          raise ; 
        end; 
      end{ "try" } ; 

    finally 
      if UpdateCurBlockEditCheckBox.Checked then begin 
        BlockNrEdit.Text := IntToStr(aStartBlockNr + blocksRead) ; // next unread block
//OutputDebugString('BlockNrEdit.Text := %d + %d = %s', [aStartBlockNr, blocksRead,BlockNrEdit.Text]) ;
      end; 

      if DriverState in [diDriver_Error, diDriver_UserStop] then 
        Log('ReadMultiBlock() aborted') 
      else 
        setDriverState(diDriver_Execute) ; 
    end { "try" } ; 
    if not check_only then 
      setImageFileState(diImageFile_Changed); 
    result := blocksRead ; 
  end{ "function TFormDiscImage.ReadMultiBlock" } ; 


procedure TFormDiscImage.ReadSingleBlockButtonClick(Sender: TObject); 
  var BlockNr: integer ; 
  begin 
//    setUnitNr(StrToInt(UnitNumberEdit.Text)) ;
    BlockNr:= StrToInt(BlockNrEdit.Text) ; 
    if Sender = ReadSingleBlockButton then begin 
      setDriverState(diDriver_Execute_Read) ; 
      ReadMultiBlock(BlockNr, 1, false) ; 
    end else begin 
      setDriverState(diDriver_Execute_Check) ; 
      ReadMultiBlock(BlockNr, 1, true); // check_only
    end ; 
  end; 


///////////////////////////////////////////////////
// Open Disc: alle Vorbereitungen zum Lesen/Schreiben treffen.
// VB: DiscType, DiscSubType, UnitNr sind gültig!
// - ggf Treiber laden
// - ggf. aus Controller Geometrie abfragen,
// - oder Geometry selber setzen
// da Geometry klar: Buffergrössen setzen

procedure TFormDiscImage.Pdp11ResetAndOpenDisc ;
  begin 
    // abgeleiteten Namen verwenden

    if DriverState < diDriver_Loaded then // alte logik
      LoadDriver ; 

    // wurde nicht geladen: anzeige und schluss
    if DriverState < diDriver_Loaded then begin 
      UpdateDisplay ; 
      Exit ; 
    end; 

    // starte driver
    if DriverState < diDriver_Execute then begin 
      StartDriver ; 
      setDriverState(diDriver_Execute); 
    end; 

    // Bei RL01/02, RM02 etc. ist die Geometrie hier bekannt
    // bei MSCP wird Cylinder/Head/Sectors und Blockzahl
    // aus dem device ausgelesen.

    ResetUnibusAndDrive; // execute "doinit". optional drive data in SerialXfer.buffer
    // Includes "reset unibus"

    if selectedDevice is TMediaImage_DiscDevice_MSCP then begin 
      // MSCP "doinit" leaves Disk data in SerialXfer.XmitBlock0Data
      // Input: gefüllter SerialXFER.XmitBlock0Data, der von ResetDrive() geliefert wurde
      // SerialXfer.XmitBlock0Data must contain the MSCP driver response
      // to a SerialTransferDriver_opInit  command.
      (selectedDevice as TMediaImage_DiscDevice_MSCP).EvalDriveInfo(SerialXfer.XmitBlock0Data) ; 
      // re-select the device, so we work with the updated geometry now
      setDeviceForSelectedController(selectedDevice.name); 
    end ; 

    MediaImageBuffer.LinkToDevice(selectedDevice) ; 
//    DriverStateInfoLabel.Caption := Format('%s: Init', [DeviceSubTypeName]) ;

    // always sync with file clear/load
    // BadBlockList.Init(selectedDevice.blockcount, BadBlockListStringGrid) ;


    BlockNrEdit.Text := '0' ; 

    setDeviceState(diDevice_Open) ; 

  end{ "procedure TFormDiscImage.UnibusResetAndOpenDisc" } ; 




// das Image aus dem RL02 lesen nach "ImageBuffer" lesen
// protectVendorArea: wenn true, nicht über bad sector files oder
// Herstellerkennungen schreiben
procedure TFormDiscImage.ReadImage(startblocknr: integer; check_only: boolean) ; 

// liest einzeln und unoptimiert alle Sectoren des aktuellen Cylinder/Head.
// dient dazu, den bad block in der track zu finden
// DriverErrorTxt: Info des Drivers über Fehlerposition.
  procedure ReadTrackBlocks(startblocknr: integer ; SectorCount: integer; errorStop: boolean ;  var DriverErrorTxt:string); 
    var BlockNr: integer ; 
      retryCount: integer ; 
      lastDriverErrorTxt: string ; 
    begin 
      DriverErrorTxt := '' ; 
      BlockNr := startblocknr ; 
      while BlockNr < startblocknr + SectorCount do begin 

        retryCount := 0 ; // wird bis ReadErrorRetryCount gezählt
        repeat 

          Application.ProcessMessages ; 
          if DriverState = diDriver_UserStop then begin 
            Log('ReadTrackBlocks() aborted'); 
            Exit ; 
          end; 
          ReadMultiBlock(BlockNr, 1, check_only); // immer nur einen sector lesen
          if DriverState = diDriver_UserStop then begin 
            Log('ReadTrackBlocks() aborted'); 
            Exit ; 
          end; 

          if DriverState = diDriver_Error then // merke letzten Fehlercode des Treibers
            lastDriverErrorTxt := DriverStateInfo ; 

          inc(retryCount) ; 
        until (DriverState <> diDriver_Error) 
                or (retryCount >= selectedDevice.readErrorRetryCount) ; 
        if DriverState = diDriver_Error then begin 
          registerBadBlock(BlockNr, bbsScan, Format('FAILURE reading sector after %d retries: %s', 
                  [retryCount, lastDriverErrorTxt])) ; 
          DriverErrorTxt := lastDriverErrorTxt ; // Fehlerrückgabe
          if errorStop then 
            Exit ; 
        end else if retryCount >  1 then 
          registerBadBlock(BlockNr, bbsScan, Format('OK reading sector after %d retries: %s', 
                  [retryCount, lastDriverErrorTxt])) ; 

        // trotz fehler weiter lesen
        inc(BlockNr) ; 
      end{ "while BlockNr < startblocknr + SectorCount" } ; 
    end{ "procedure ReadTrackBlocks" } ; 

  var BlockNr: integer ; 
    n, m: integer ; 
    DriverErrorTxt: string ; //
  begin { "procedure TFormDiscImage.ReadImage" } 

    if check_only then 
      setDriverState(diDriver_Execute_Check) 
    else setDriverState(diDriver_Execute_Read) ; 

//      n := 160 ; // nur ersten zwei tracks
//      n := 400 ; // die ersten 5 tracks
    n := selectedDevice.blockcount ; 

    // es werden immer max. ganze Tracks auf einmal gelesen.


//    // dazu muss startblocknr der erste Sector pro track sein,
//    // der blockcount ist dann die Sectoranzahl pro Track.
//    // startblocknr auf vielfaches von track sector abrunden
//    startblocknr := startblocknr - startblocknr mod DiscImage.MultiBlockCount ;
    BlockNr := startblocknr ; 
    while BlockNr < n do begin 
      Application.ProcessMessages ; 
      if DriverState = diDriver_UserStop then begin 
        Log('ReadImage() aborted'); 
        Exit ; 
      end; 
      // Anzahl der Blocks, die jetzt gelesen werden. Nicht über Trackende hinaus!
      // Hier ggf kleinere Blockzahl benutzen, und nicht
      // ganze Tracks. Nötig bei langsamen Baudraten und controller timeout
      // während der seriellen Übertragung
      // Nötig für MSCP RX33 diskette bei 9600 baud? Controller timeout!

      // diese Logik hier auch nach WriteImage() !

      m := StrToInt(BlockPerTransferEdit.Text) ; // .Update() garantiert immer gültigen Inhalt
      // m := DiscImage.MultiBlockCount ;

      // Nicht über disk ende hinauslesen
      while (BlockNr + m) > n do   // ORIGINAL, aber falsch?
//      while (BlockNr + m - 1) > n do
        dec(m) ; 
      // nicht über Trackende hinauslesen
      while  ((BlockNr+m-1) div selectedDevice.MultiBlockCount) > (BlockNr div selectedDevice.MultiBlockCount) do 
        dec(m) ; 

      ReadMultiBlock(BlockNr, m, check_only); // nicht immer ganze tracks lesen
      if DriverState = diDriver_UserStop then begin 
        Log('ReadImage() aborted'); 
        Exit ; 
      end; 
      if DriverState = diDriver_Error then begin 
        // Beim Lesen des Tracks wurde ein Fehler gesehen,
        // lies den track jetzt sectorweise, um den bad block zu finden.
        // DiscGeometry ist gültig.
        ReadTrackBlocks(BlockNr, m, StopOnErrorCheckBox.Checked, DriverErrorTxt) ; 

        // Unrecoverable error? TehndiDriver_Error set again.
        if (DriverState = diDriver_Error) and StopOnErrorCheckBox.Checked then begin 
          Log('Error occured: ReadImage() terminated'); 
          // durch repeat kann Fehlerstatus weg sein, reproduziere ihn,
          setDriverState(diDriver_Error, DriverErrorTxt) ; 
          Exit ; 
        end; 
      end{ "if DriverState = diDriver_Error" } ; 
      BlockNr := BlockNr + m ; // next track
    end { "while BlockNr < n" } ; 

  end{ "procedure TFormDiscImage.ReadImage" } ; 


// Leses bis Fehler, oder bis weniger zurückkommt als angefordert
procedure TFormDiscImage.ReadImageStream(startblocknr: integer ; check_only: boolean) ; 
  var 
    BlockNr: integer ; 
    blocksToRead, blocksRead: integer ; 
//    DriverErrorTxt: string ; //
  begin 
    if check_only then 
      setDriverState(diDriver_Execute_Check) 
    else setDriverState(diDriver_Execute_Read) ; 

    BlockNr := startblocknr ; 
    repeat 
      Application.ProcessMessages ; 
      if DriverState = diDriver_UserStop then begin 
        Log('ReadImage() aborted'); 
        Exit ; 
      end; 

      blocksToRead := StrToInt(BlockPerTransferEdit.Text) ; // .Update() garantiert immer gültigen Inhalt

      blocksRead := ReadMultiBlock(BlockNr, blocksToRead, check_only); // nicht immer ganze tracks lesen
      if DriverState = diDriver_UserStop then begin 
        Log('ReadImage() aborted'); 
        Exit ; 
      end; 
      if (DriverState = diDriver_Error) and StopOnErrorCheckBox.Checked then begin 
        Log('Error occured: ReadImageStream() terminated'); 
        // durch repeat kann Fehlerstatus weg sein, reproduziere ihn,
        setDriverState(diDriver_Error, DriverStateInfo) ; 
        Exit ; 
      end; 
      BlockNr := BlockNr + blocksRead ; 
      // Log('After read: blocksToRead=%d, blocksread=%d,blocknr=%d', [blocksToRead,blocksread,blocknr]);
    until (blocksRead < blocksToRead) or (DriverState = diDriver_Error) ; 

  end{ "procedure TFormDiscImage.ReadImageStream" } ; 



// reagiert of protectVendorArea
// result: number of block written
function TFormDiscImage.WriteMultiBlock(aStartBlockNr: integer; blocksToWrite: integer): integer ; 
  var 
    pdp11proctimeout_ms: integer ; // siehe ReadMultiBlock()
    blocksWritten: integer ; 
    n: integer ; 
    i: integer ; 
    errorloc: dword ; 
    info: string ; 
    opcode : word ; 
    discdev:  TMediaImage_DiscDevice_Std ; 
  begin 
    n := 0 ; 
    if DriverState = diDriver_UserStop then Exit ; 

    // nicht schreiben, wenn
    /// FALSCHE Arithmetik! Funktioniert nur für infoArea=  ganzer Track!
    if selectedDevice is TMediaImage_DiscDevice_Std 
            and ProtectVendorAreaCheckBox.Checked then begin 
      discdev := selectedDevice as TMediaImage_DiscDevice_Std ; 
      if (aStartBlockNr >= discdev.VendorAreaStartBlock) 
              and ((aStartBlockNr + blocksToWrite) <= (discdev.VendorAreaStartBlock + discdev.VendorAreaBlockCount))then 
        Exit ; 

    end; 

    try 
      setDriverState(diDriver_Execute_Write) ; 
      opcode := SerialTransferDriver_opwrite ; 
      try 
        // Baue Infostring der form "block i/n, cylinder,head,sector...= t/h/n"
        DeviceStateInfo := Format('writing %s', 
                [selectedDevice.getBlockRangeDisplay(aStartBlockNr, blocksToWrite)]) ; 


        // may be there less data in the MediaImageBuffer ?
        // in case of stream mode
        n := MediaImageBuffer.getblockcount - aStartBlockNr ; // remaining blocks
        if n < blocksToWrite then 
          blocksWritten := n 
        else blocksWritten := blocksToWrite ; 

        // Register als Inputparameter aufsetzen
        SetupDriverDiskParams(aStartBlockNr, blocksWritten, info) ; 

        pdp11proctimeout_ms := 500 * blocksToWrite + selectedDevice.maxBlockAccessTime_ms; 

        // Daten aus DiskImage in Block1 kopieren
        if selectedDevice.DataBytesAreWordLSBs then begin 
          n := blocksWritten * selectedDevice.BlockSize ; // size in words
          SetLength(SerialXfer.XmitBlock1Data, n) ; 
          for i := 0 to n-1 do 
            SerialXfer.XmitBlock1Data[i] := MediaImageBuffer.getImageBufferBlockByte(aStartBlockNr, i) ; 
        end else begin 
          n := blocksWritten * selectedDevice.BlockSize div 2 ; // n = size in words
          SetLength(SerialXfer.XmitBlock1Data, n) ; 
          for i := 0 to n-1 do 
            SerialXfer.XmitBlock1Data[i] := MediaImageBuffer.getImageBufferBlockWord(aStartBlockNr, i) ; 
        end ; 
        RestZeit.setCur(aStartBlockNr) ; 
        UpdateDisplay ; 

        SerialXfer.Execute( 
                pdp11proctimeout_ms, // worst Zeit für alle sector zugriffe in ms,
                opcode) ; // Controller nicht reset

        // opcode muss 0 sein, wenn OK
        errorloc := opcode ; 
        if errorloc <> 0 then begin // dump error words
          setDriverState(diDriver_Error, Format('error location = %s, error info = %s!', 
                  [Dword2OctalStr(errorloc,0), 
                  SerialXfer.WordArrayAsText(SerialXfer.XmitBlock0Data)])) ; 
          // Fehlertext landet in DriverStateInfo
          Exit ; 
        end; 

      except // eat exception and display
        on E: Exception do begin 
          setDriverState(diDriver_Error, E.message) ; 
          raise ; 
        end; 
      end{ "try" } ; 

    finally 
      if UpdateCurBlockEditCheckBox.Checked then 
        BlockNrEdit.Text := IntToStr(aStartBlockNr+ blocksWritten) ; // next unwritten block

      if DriverState in [diDriver_Error, diDriver_UserStop] then 
        Log('WriteMultiBlock() aborted') 
      else 
        setDriverState(diDriver_Execute) ; 
    end { "try" } ; 
    result := blocksWritten ; 
  end{ "function TFormDiscImage.WriteMultiBlock" } ; 



procedure TFormDiscImage.WriteSingleBlockButtonClick(Sender: TObject); 
  var BlockNr: integer ; 
  begin 
//    setUnitNr(StrToInt(UnitNumberEdit.Text)) ;
    setDriverState(diDriver_Execute_Write) ; 
    BlockNr:= StrToInt(BlockNrEdit.Text) ; 
    WriteMultiBlock(BlockNr,1); 
  end; 


// das Image aus dem Imagebuffer in das RL02 schreiben.
// protectVendorArea: wenn true, nicht über bad sector files oder
// Herstellerkennungen schreiben
procedure TFormDiscImage.WriteImage(startblocknr: integer) ; 

// schreibt einzeln und unoptimiert alle Sectoren des aktuellen Cylinder/Head.
// dient dazu, den bad block in der track zu finden
  procedure WriteTrackBlocks(startblocknr: integer ; errorStop: boolean ;  SectorCount: integer); 
    var BlockNr: integer ; 
      lastErrorTxt: string ; 
    begin 
      BlockNr := startblocknr ; 
      while BlockNr < startblocknr + SectorCount do begin 
        Application.ProcessMessages ; 
        if DriverState = diDriver_UserStop then begin 
          Log('WriteTrackBlocks() aborted'); 
          Exit ; 
        end; 
        WriteMultiBlock(BlockNr, 1); // immer nur einen sector lesen
        if DriverState = diDriver_UserStop then begin 
          Log('WriteTrackBlocks() aborted'); 
          Exit ; 
        end; 
        if DriverState = diDriver_Error then begin 
          lastErrorTxt := DriverStateInfo ; // merke letzten Fehlercode des Treibers
          registerBadBlock(BlockNr, bbsScan, Format('FAILURE writing disc: %s', [lastErrorTxt])); 
          if errorStop then 
            Exit ; 
          // trotz fehler weiter lesen
        end; 
        inc(BlockNr) ; 
      end{ "while BlockNr < startblocknr + SectorCount" } ; 
    end{ "procedure WriteTrackBlocks" } ; 

  var BlockNr: integer ; 
    n, m: integer ; 
  begin { "procedure TFormDiscImage.WriteImage" } 
    setDriverState(diDriver_Execute_Write) ; 

    n := selectedDevice.blockcount ; 

    // es werden immer ganze Tracks auf einmal geschrieben.
    // dazu muss startblocknr der erste Sector pro track sein,
    // der blockcount ist dann die Sectoranzahl pro Track.
    // startblocknr auf vielfaches von track sector abrunden
    startblocknr := startblocknr - startblocknr mod selectedDevice.MultiBlockCount ; 
    BlockNr := startblocknr ; 
    while BlockNr < n do begin 
      Application.ProcessMessages ; 
      if DriverState = diDriver_UserStop then begin 
        Log('WriteImage() aborted'); 
        Exit ; 
      end; 

      // Anzahl der Blocks, die jetzt geschrieben werden. Nicht über Diskende hinaus!

      m := StrToInt(BlockPerTransferEdit.Text) ; // .Update() garantiert immer gültigen Inhalt
      // m := DiscImage.MultiBlockCount ;

      // Nicht über disk ende hinauslesen
//      while (BlockNr + m) > n do   // ORIGINAL, aber falsch?
      while (BlockNr + m - 1) > n do 
        dec(m) ; 
      // nicht über Trackende hinauslesen
      while  ((BlockNr+m-1) div selectedDevice.MultiBlockCount) > (BlockNr div selectedDevice.MultiBlockCount) do 
        dec(m) ; 

      m := selectedDevice.MultiBlockCount ; 
      while (BlockNr + m) > n do dec(m) ; 
      WriteMultiBlock(BlockNr, m); // immer ganze tracks schreiben
      if DriverState = diDriver_UserStop then begin 
        Log('WriteImage() aborted'); 
        Exit ; 
      end; 
      if DriverState = diDriver_Error then begin 
        // Beim Lesen des tracks wurde ein Fehler gesehen,
        // lies den track jetzt sectorweise, um den bad block zu finden.
        // DiscGeometry ist gültig.
        WriteTrackBlocks(BlockNr, StopOnErrorCheckBox.Checked, m) ; 
        if StopOnErrorCheckBox.Checked then begin 
          Log('Error occured: WriteImage() terminated'); 
          // durch repeat kann Fehlerstatus weg sein, reproduziere ihn,
          setDriverState(diDriver_Error) ; 
          Exit ; 
        end; 
      end; 
      BlockNr := BlockNr + m ; // next track
    end { "while BlockNr < n" } ; 
  end { "procedure TFormDiscImage.WriteImage" } ; 



// schreiben , bis Fehler
procedure TFormDiscImage.WriteImageStream(startblocknr: integer) ; 
  var 
    BlockNr: integer ; 
    blocksToWrite, blocksWritten: integer ; 
  begin 
    BlockNr := startblocknr ; 
    repeat 
      Application.ProcessMessages ; 
      if DriverState = diDriver_UserStop then begin 
        Log('WriteImageStream() aborted'); 
        Exit ; 
      end; 

      blocksToWrite := StrToInt(BlockPerTransferEdit.Text) ; // .Update() garantiert immer gültigen Inhalt

      blocksWritten := WriteMultiBlock(BlockNr, blocksToWrite); // nicht immer ganze tracks lesen
      if DriverState = diDriver_UserStop then begin 
        Log('WriteImageStream() aborted'); 
        Exit ; 
      end; 
      if (DriverState = diDriver_Error) and StopOnErrorCheckBox.Checked then begin 
        Log('Error occured: WriteImageStream() terminated'); 
        // durch repeat kann Fehlerstatus weg sein, reproduziere ihn,
        setDriverState(diDriver_Error, DriverStateInfo) ; 
        Exit ; 
      end; 
      BlockNr := BlockNr + blocksToWrite ; 
      // Log('After read: blocksToRead=%d, blocksread=%d,blocknr=%d', [blocksToRead,blocksread,blocknr]);
    until (blocksWritten < blocksToWrite) or (DriverState = diDriver_Error) ; 
  end{ "procedure TFormDiscImage.WriteImageStream" } ; 


// wordcount: Datenmenge, die zum Test übertragen wird
// testpattern: enum
procedure TFormDiscImage.Selftest(wordcount: integer; randomWordcount: boolean ; testpattern:integer) ; 
  begin 
    try 
      setDriverState(diDriver_Execute_Selftest) ; 
      if randomWordcount then 
        wordcount := Random(wordcount) ; 
      SerialXfer.Selftest(wordcount, testpattern) ; 
      setDriverState(diDriver_Execute, 'Selftest OK') ; 
    except on E: Exception do begin 
        setDriverState(diDriver_Error, 'Selftest FAILED') ; 
        raise ; 
      end; 
    end; 
  end{ "procedure TFormDiscImage.Selftest" } ; 


procedure TFormDiscImage.SelftestButtonClick(Sender: TObject); 
  var i, n: integer ; 
  begin 
    SerialXfer.EnableRLECompression := SelftestRLECompressionCheckBox.Checked ; 
    n := StrToInt(SelfTestRepeatCountEdit.Text); 
    // testpattern: siehe SerialXfer.Selftest()
    if SelftestPatternComboBox.ItemIndex < 0 then 
      SelftestPatternComboBox.ItemIndex := 0 ; // wähle ersten Eintrag
    // macht UNIBUS reset. Daher muss das Disc System neu initialisiert werden.
    // (MSCP braucht nach Reset neues UnibusResetAndOpenDisc)
    // jetzt neues "ResetAndOpen ermöglichen

//    setController(selectedController) ;
//    setDeviceState(diDevice_SelectDevice);

    for i := 0 to n-1 do 
      Selftest(StrToInt(SelftestWordcountEdit.Text), SelftestRandomWordCountCheckBox.Checked, SelftestPatternComboBox.ItemIndex) ; 
  end{ "procedure TFormDiscImage.SelftestButtonClick" } ; 


procedure TFormDiscImage.ClearImageBuffer ; 
  begin 
    MediaImageBuffer.ClearBuffer ; 
    BadBlockList.Clear ; 
    setImageFileState(diImageFile_Cleared); 
  end; 


procedure TFormDiscImage.ClearImageButtonClick(Sender: TObject); 
  begin 
    ClearImageBuffer ; 
  end; 


// das Image aus dem RL02 in Datei schreiben
// gleichzeitig BadBlockList als <filename>.meta schreiben
procedure TFormDiscImage.SaveImageBuffer(filename: string) ; 
  begin 
    MediaImageBuffer.SaveBuffer(filename) ;
    BadBlockList.NextBlockToProcess := StrToInt(BlockNrEdit.Text) ;
    BadBlockList.SaveToFile(filename+'_meta') ;

    setImageFileState(diImageFile_Saved, '"'+filename+'"'); 
  end; 

procedure TFormDiscImage.LoadImageBuffer(filename: string; overlay: boolean = false) ; 
  var metafilename: string ; 
  begin 
    MediaImageBuffer.LoadBuffer(filename, overlay) ; 
    // load bad block file, if it exists
    BadBlockList.Clear ; 
    metafilename := filename+ '_meta' ; 
    if FileExists(metafilename) then begin 
      BadBlockList.LoadFromFile(metafilename);
      if UpdateCurBlockEditCheckBox.Checked then
        BlockNrEdit.Text := IntToStr(BadBlockList.NextBlockToProcess) ;
    end;

    if overlay then 
      setImageFileState(diImageFile_Changed, 'overlayed with "' + filename + '"') 
    else 
      setImageFileState(diImageFile_Loaded, '"'+filename+'"') ; 
  end{ "procedure TFormDiscImage.LoadImageBuffer" } ; 


procedure TFormDiscImage.LoadImageFileButtonClick(Sender: TObject); 
  var fname: string ; 
  begin 
    // definiert den Filenamen
    fname := TheRegistry.Load(RegistryKeyDiskImageFilename, '') ; 
    OpenDiskImageDialog.filename := fname ; 
    OpenDiskImageDialog.InitialDir := ExtractFileDir(fname) ; 
    if OpenDiskImageDialog.Execute then begin 
      if not FileExists(OpenDiskImageDialog.filename) then begin 
        // erzeugen durch schreiben
        ClearImageBuffer ; 
        // extension: BMP -> save as Bitmap
        SaveImageBuffer(OpenDiskImageDialog.filename) 
      end else 
        LoadImageBuffer(OpenDiskImageDialog.filename) ; 

      TheRegistry.Save(RegistryKeyDiskImageFilename, OpenDiskImageDialog.filename) ; 
    end; 
  end{ "procedure TFormDiscImage.LoadImageFileButtonClick" } ; 


procedure TFormDiscImage.MediaSerialNumberEditChange(Sender: TObject); 
  begin 
    TryStrToDword('$'+MediaSerialNumberEdit.Text, BadBlockList.CartridgeSerialNumber) ; 
  end; 

procedure TFormDiscImage.MediaSerialNumberEditExit(Sender: TObject); 
  begin 
    UpdateDisplay ; 
  end; 

procedure TFormDiscImage.OverlayFileButtonClick(Sender: TObject); 
  var fname: string ; 
  begin 
    // overlay übernimmt path und extension vom image file
    fname := TheRegistry.Load(RegistryKeyDiskImageFilename, '') ; 
    OpenDiskImageOverlayDialog.InitialDir := ExtractFileDir(fname) ; 
    OpenDiskImageOverlayDialog.filename := '' ; 
    OpenDiskImageOverlayDialog.DefaultExt := ExtractFileExt(fname) ; 
    if OpenDiskImageOverlayDialog.Execute then begin 
      LoadImageBuffer(OpenDiskImageOverlayDialog.filename, {overlay=} true) ; 
    end; 
  end; 


procedure TFormDiscImage.ProtectVendorAreaCheckBoxClick(Sender: TObject); 
  begin 
    TheRegistry.Save(ProtectVendorAreaCheckBox) ; 
  end;


procedure TFormDiscImage.SaveImageButtonClick(Sender: TObject); 
  var fname: string ; 
  begin 
    fname := TheRegistry.Load(RegistryKeyDiskImageFilename, '') ; 
    SaveDiskImageDialog.InitialDir := ExtractFileDir(fname) ; 
    SaveDiskImageDialog.filename := fname ; 

    if SaveDiskImageDialog.Execute then begin 
      SaveImageBuffer(SaveDiskImageDialog.filename) ; 
      TheRegistry.Save(RegistryKeyDiskImageFilename, SaveDiskImageDialog.filename) ; 
    end; 
  end; 


procedure TFormDiscImage.SaveMetaInfoButtonClick(Sender: TObject); 
  var fname: string ; 
  begin 
    fname := TheRegistry.Load(RegistryKeyDiskMetaInfoFilename, '') ; 
    SaveDiskMetaInfoDialog.InitialDir := ExtractFilePath(fname) ; 
    SaveDiskMetaInfoDialog.filename := fname ; 

    if SaveDiskMetaInfoDialog.Execute then begin
      BadBlockList.NextBlockToProcess := StrToInt(BlockNrEdit.Text) ;
      BadBlockList.SaveToFile(SaveDiskMetaInfoDialog.filename) ;
      TheRegistry.Save(RegistryKeyDiskMetaInfoFilename, SaveDiskMetaInfoDialog.filename) ; 
    end; 
  end; 


procedure TFormDiscImage.ReadImageButtonClick(Sender: TObject); 
  var 
    check_only: boolean ; 
    startblocknr: integer ; 
  begin 
    check_only := (Sender <> ReadImageButton) ; 
    try 
      // UnibusResetAndOpenDisc ; NEIN! KEIN Reset zwischen DiskOpen und Read ... MSCP!
      startblocknr:= StrToInt(BlockNrEdit.Text) ; 
      if selectedDevice.fixCapacity then begin 
        RestZeit.setStart(startblocknr, selectedDevice.blockcount) ; 
        ReadImage(startblocknr, check_only) ; 
      end else begin 
        // device hat keine feste Grösse, stream of data
        RestZeit.valid := false ; // keine Zeitschätzung möglich
        ReadImageStream(startblocknr, check_only) ; 
      end; 
    finally 
      RestZeit.valid := false ; 
    end{ "try" } ; 
  end{ "procedure TFormDiscImage.ReadImageButtonClick" } ; 


procedure TFormDiscImage.WriteImageButtonClick(Sender: TObject); 
  var startblocknr: integer ; 
  begin 
    try 
      // UnibusResetAndOpenDisc ; NEIN! KEIN Reset zwischen DiskOpen und Read ... MSCP!
      startblocknr:= StrToInt(BlockNrEdit.Text) ; 

      if selectedDevice.fixCapacity then begin 
        RestZeit.setStart(startblocknr, selectedDevice.blockcount) ; 
        WriteImage(startblocknr) ; 
      end else begin 
        // device hat keine feste Grösse, stream of data.
        RestZeit.setStart(startblocknr, MediaImageBuffer.getblockcount) ; 
        WriteImageStream(startblocknr) ; 
      end; 

    finally 
      RestZeit.valid := false ; 
    end{ "try" } ; 
  end{ "procedure TFormDiscImage.WriteImageButtonClick" } ; 


procedure TFormDiscImage.StopButtonClick(Sender: TObject); 
  begin 
    Abort ; 
  end; 


procedure TFormDiscImage.StopOnErrorCheckBoxClick(Sender: TObject); 
  begin 
    TheRegistry.Save(StopOnErrorCheckBox) ; 
  end; 

procedure TFormDiscImage.UpdateCurBlockEditCheckBoxClick(Sender: TObject); 
  begin 
    TheRegistry.Save(UpdateCurBlockEditCheckBox) ; 
  end; 


//
procedure TFormDiscImage.registerBadBlock(badblocknr: integer; source: TBadBlockSource ; info:string); 
  var 
    discdev:  TMediaImage_DiscDevice_Std ; 
    Cylinder,Head,Sector: integer ; 

  begin 
    // ergänze cylinder,head,sector
    if selectedDevice is TMediaImage_DiscDevice_Std then begin 
      discdev := selectedDevice as TMediaImage_DiscDevice_Std ; 
      discdev.BlockNr2DiscAddr(badblocknr, Cylinder, Head, Sector) ; 
      BadBlockList.registerBadBlock(badblocknr, Cylinder, Head, Sector, source, info) ; 
      //     OptimizeAnyGridColWidths(Grid);

    end else begin 
      // tape or MSCP: register only bad block
      BadBlockList.registerBadBlock(badblocknr, -1, -1, -1, source, info) ; 
    end; 
  end{ "procedure TFormDiscImage.registerBadBlock" } ; 


procedure TFormDiscImage.ReadBadBlocksFromBadSectorFileButtonClick( 
        Sender: TObject); 
  begin 
    BadBlockList.Std144_LoadFromImage(MediaImageBuffer); 
    UpdateDisplay ; 
  end; 


procedure TFormDiscImage.WriteBadBlocksToBadSectorFileButtonClick( 
        Sender: TObject); 
  begin 
    TryStrToDword('$'+MediaSerialNumberEdit.Text, BadBlockList.CartridgeSerialNumber) ; 
    BadBlockList.Std144_WriteToImage(MediaImageBuffer); 
    setImageFileState(diImageFile_Changed, 'DEC bad sector file written'); 
    UpdateDisplay ; 
  end; 


end{ "unit FormDiscImageU" } . 
