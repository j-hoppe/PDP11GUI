unit FormExecuteU;
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
  Form for Run Control.
  also auxilary services to execute code, based on Console* methods.
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls,
  JH_Utilities,
  FormChildU,
  AddressU,
  MemoryCellU,
  RegistryU,
  ConsoleGenericU ;

type

  TExecuteState = (esUnknown, esCompiling, esStopped, esRunning) ;

  TFormExecute = class(TFormChild)
      Panel1: TPanel;
      ResetButton: TButton;
      ResetAndStartButton: TButton;
      HaltButton: TButton;
      SingleStepButton: TButton;
      StartPCEdit: TEdit;
      Label1: TLabel;
      Label2: TLabel;
      CurPCEdit: TEdit;
      NewPgmButton: TButton;
      ContinueButton: TButton;
      RunModeGroupBox: TGroupBox;
      RunRadioButton: TRadioButton;
      HaltRadioButton: TRadioButton;
      RunHaltInfoMemo: TMemo;
      ShowPCButton: TButton;
      procedure StartPCEditChange(Sender: TObject);
      procedure ResetButtonClick(Sender: TObject);
      procedure ResetAndStartButtonClick(Sender: TObject);
      procedure SingleStepButtonClick(Sender: TObject);
      procedure HaltButtonClick(Sender: TObject);
      procedure FormShow(Sender: TObject);
      procedure SetPcButtonClick(Sender: TObject);
      procedure CurPCEditKeyPress(Sender: TObject; var Key: char);
      procedure NewPgmButtonClick(Sender: TObject);
      procedure ContinueButtonClick(Sender: TObject);
      procedure RunRadioButtonClick(Sender: TObject);
      procedure HaltRadioButtonClick(Sender: TObject);
    private
      { Private-Deklarationen }
      consoleFeatures: TConsoleFeatureSet ;
      procedure updateConsoleFeatures ;

      procedure curPcMemoryCellChange(Sender { = memorycellgroup}: TObject; memorycell: TMemoryCell) ;
      // PC setzen
      procedure SetAndShowPc(newpc_v: TMemoryAddress) ;

    public
      { Public-Deklarationen }
      TheState: TExecuteState ;
      StartPc_v: TMemoryAddress ; // Program counter at start, virtuelle Adresse

      CurPcMemorycellgroup : TMemoryCellGroup ;
      CurPc: TMemoryCell ; // Current PC, virtulle Adresse

      constructor Create(AOwner: TComponent) ; override ;
      procedure ConnectToMemoryCells(mcg: TMemoryCellGroup) ;

      function CheckRunMode: boolean ;

      // wird von Console aufgerufen, wenn Halt, Stop, oder Singlestep
      procedure SimulationStopped(Sender: TObject; newpc_v: TMemoryAddress) ;
      procedure UpdateDisplay ;

      // Implementierung der Steuerbefehle mit den Fähigkeiten der Console.
      // Werden auch von anderen Modulen aufgerufen!
      procedure doResetMachineAndSetPC ;
      procedure doResetMachineAndSetPCandStart;
      procedure doSetPCandContinue(newpc_v:TMemoryAddress)  ;
      procedure doContinue ;
      procedure doSingleStep ;
      procedure doHalt ;


    end{ "TYPE TFormExecute = class(TFormChild)" } ;


implementation

uses
  AuxU,
  OctalConst,
  FormLogU, FormMainU;//, SerialIoHubU;

{$R *.dfm}


procedure TFormExecute.ContinueButtonClick(Sender: TObject);
  begin
    doContinue ;
  end;

constructor TFormExecute.Create(AOwner: TComponent) ;
  begin
    inherited ;
    TheRegistry.Load(StartPCEdit, '0') ;
    TheState := esUnknown ;
  end;


procedure TFormExecute.CurPCEditKeyPress(Sender: TObject; var Key: char);
  begin
    // nur Octalziffern als Eingabe. auch das backspace #8 erlauben
    if not CharInSet(Key, [#8, '0'..'7']) then
      Key := #0 ;
  end;

procedure TFormExecute.FormShow(Sender: TObject);
  begin
    UpdateDisplay ;
  end;



procedure TFormExecute.updateConsoleFeatures ;
  begin
    if FormMain.PDP11Console = nil then
      consoleFeatures := []
    else consoleFeatures := FormMain.PDP11Console.getFeatures ;
  end;


// wird von der memorycellgroup aufgerufen, wenn sich eine Zelle = der PC spontan ändert
procedure TFormExecute.curPcMemoryCellChange(Sender { = memorycellgroup}: TObject; memorycell: TMemoryCell) ;
  begin
    memorycell.edit_value := memorycell.pdp_value ;
    UpdateDisplay ;
  end ;


procedure TFormExecute.NewPgmButtonClick(Sender: TObject);
  begin
    //
    TheState := esCompiling ;
    try
      UpdateDisplay ;
      // wenn kein gültiger File da: User zum Wählen auffordern!
      FormMain.FormMacro11Source.Show ; // immer aufpoppen
      if not FormMain.FormMacro11Source.CanTranslate then begin
        FormMain.FormMacro11Source.LoadButtonClick(nil) ;
      end;
      if not FormMain.FormMacro11Source.CanTranslate then
        MessageDlg('No valid source file selected!', mtError, [mbOk], 0)
      else begin
        FormMain.FormMacro11Source.CompileButtonClick(nil);
        if FormMain.FormMacro11Source.Translated then // nicht laden, wenn error
          FormMain.FormMacro11Listing.DepositAllButtonClick(nil) ;
      end ;
    finally
      BringToFront ;
      doResetMachineAndSetPC;
    end{ "try" } ;
  end{ "procedure TFormExecute.NewPgmButtonClick" } ;




// die erste Cell der Group ist der PC
procedure TFormExecute.ConnectToMemoryCells(mcg: TMemoryCellGroup) ;
  begin
    CurPcMemorycellgroup := mcg ;
    CurPc := mcg.Cell(0) ;
    // callback bei Zellenänderung
    mcg.OnMemoryCellChange := curPcMemoryCellChange ;
    UpdateDisplay ;
  end;


// PC setzen,
// Disassembler-Anzeige aktualisieren
procedure TFormExecute.SetAndShowPc(newpc_v:TMemoryAddress) ;
  begin
    assert(newpc_v.mat = matVirtual) ;
    CurPc.pdp_value := newpc_v.val ;
    curPcMemoryCellChange(self, CurPc) ;
    LogStrCol(LogCol_Other, 'TFormExecute.SetAndShowPc(): TheState := esStopped') ;
    TheState := esStopped ;
    UpdateDisplay ;
    LogStrCol(LogCol_Other, 'TFormExecute.SetAndShowPc(): A') ;

    // den PC im Disassemblerfenster anzeigen
    FormMain.FormDisas.ShowNewPcAddr(newpc_v) ;
    LogStrCol(LogCol_Other, 'TFormExecute.SetAndShowPc(): B') ;

    // Darstellung des PC auf anderen Forms aktualisieren
    FormMain.MemoryCellGroups.SyncMemoryCells(CurPc) ;
    LogStrCol(LogCol_Other, 'TFormExecute.SetAndShowPc(): C') ;

    // Bei Fehler die Anzeige des Listings nicht stören, Error marker!
    if FormMain.FormMacro11Source.Translated then begin
      // im Assemblerlisting die Zeile des PC anzeigen
      FormMain.FormMacro11Listing.setPcMark(newpc_v);
    end;
    LogStrCol(LogCol_Other, 'TFormExecute.SetAndShowPc(): D') ;

    // Vom pdp1170panel wird jetzt das DisplayRegister abgefragt.
    FormMain.FormPdp1170Panel.OnCpuHalt(nil) ;
    LogStrCol(LogCol_Other, 'TFormExecute.SetAndShowPc(): E') ;

  end{ "procedure TFormExecute.SetAndShowPc" } ;



procedure TFormExecute.SetPcButtonClick(Sender: TObject);
  var newpc_v: TMemoryAddress ;
  begin
    newpc_v := OctalStr2Addr(CurPCEdit.Text, matVirtual) ;
    // der User hat den PC geändert: Update PDP-11
    CurPc.addr.val := PhysicalIopageBaseAddr(CurPc.addr.mat) + _17707 ; // R7/PC
    CurPc.edit_value := newpc_v.val ;
    CurPc.Deposit ;
    SetAndShowPc(newpc_v) ;
  end;


// wird von Console aufgerufen, wenn Halt, Stop, oder Singlestep
// signalisiert Änderung der Memorycell "PC", dadurch update aller
// anderen Forms
procedure TFormExecute.SimulationStopped(Sender: TObject; newpc_v: TMemoryAddress) ;
  begin
    // PDP-11 stoppte und meldet neuen PC
    // Das kann die Folge eines beendeten Single-steps, ein HALT,
    // oder eine folge irgend einer Konsol-aktion eines
    // ganz anderes Modus ein. Also erstmal filtern!

    // melde, dass der Stop-Pc kam
//    if TheState = esRunning then begin
//      TheState := esStopped ; //
    SetAndShowPc(newpc_v);
    // dieses Event wird von ConsoleGeneric est aufgerufen,
    // wenn eine Critical-Section -Sequenz fertig ist.
    // Daher sollte die Console bereit sein für
    // neue Aktivitäten
    // die Console ist möglicherweise noch nicht bereit für
    // die Examines des Disas-Windows!!!
//    end ;
  end{ "procedure TFormExecute.SimulationStopped" } ;


procedure TFormExecute.StartPCEditChange(Sender: TObject);
  begin
    try
      StartPc_v := OctalStr2Addr(StartPCEdit.Text, matVirtual) ;
//    UpdateProperties ;
      TheRegistry.Save(StartPCEdit) ;
    except
    end;
  end;

function TFormExecute.CheckRunMode: boolean ;
  begin
    result := true ;
    if RunModeGroupBox.Visible and not RunRadioButton.Checked and not HaltRadioButton.Checked then
      raise Exception.Create('Go to the Execution Control Window and Set RUN or HALT according to your current PDP-11 console switch!') ;
//    MessageDlg('Set RUN or HALT according to your current PDP-11 console switch',
//    mtWarning, [mbOk], 0) ;
//    result := false ;
//  end;
  end;



procedure TFormExecute.UpdateDisplay ;
  begin
    updateConsoleFeatures ;

    StartPCEdit.Text := Addr2OctalStr(StartPc_v) ;
    CurPCEdit.Text := Dword2OctalStr(CurPc.edit_value, 16) ;

    // Das Enable der Controls hängt ab von den
    // - features der Console (und damit von Run/Halt mode)
    // - vom State der Execution form
    // Es sind auf jeden Fall nur die Controls aktiv, die features der console entsprechen
    case TheState of
      esUnknown: begin // alle controls aktiv ... damit der User alles machen kann
        ResetButton.Enabled := (cfActionResetMaschineAndStartCpu in consoleFeatures) or (cfActionResetMachine in consoleFeatures) ;
        NewPgmButton.Enabled := true ;

        RunModeGroupBox.Visible := cfSwitchEnableOrHalt in consoleFeatures ;
        ResetButton.Enabled := cfActionResetMachine in consoleFeatures ;
        ResetAndStartButton.Enabled := cfActionResetMaschineAndStartCpu in consoleFeatures ;
        ContinueButton.Enabled := cfActionContinueCpu in consoleFeatures ;
        SingleStepButton.Enabled := cfActionSingleStep in consoleFeatures ;
        HaltButton.Enabled := true ;
        Caption := setFormCaptionInfoField(Caption, 'state unknown') ;
      end;
      esCompiling: begin
        NewPgmButton.Enabled := false ;

        RunModeGroupBox.Visible := cfSwitchEnableOrHalt in consoleFeatures ;
        RunModeGroupBox.Enabled := false ;
        ResetButton.Enabled := false ;
        ResetAndStartButton.Enabled := false ;
        ContinueButton.Enabled := false ;
        HaltButton.Enabled := true ; // Immer verfügbar. doHalt() improvisiert!
        SingleStepButton.Enabled := false ;

        Caption := setFormCaptionInfoField(Caption, 'Compiling ...') ;
      end{ "case TheState of esCompiling:" } ;
      esStopped: begin
        NewPgmButton.Enabled := true ;

        RunModeGroupBox.Visible := cfSwitchEnableOrHalt in consoleFeatures ;
        RunModeGroupBox.Enabled := true;

        ResetButton.Enabled := cfActionResetMachine in consoleFeatures ;
        ResetAndStartButton.Enabled := cfActionResetMaschineAndStartCpu in consoleFeatures ;
        ContinueButton.Enabled := cfActionContinueCpu in consoleFeatures ;
        HaltButton.Enabled := true ; // Immer verfügbar. doHalt() improvisiert!
        SingleStepButton.Enabled := cfActionSingleStep in consoleFeatures ;

        Caption := setFormCaptionInfoField(Caption, 'stopped') ;

      end{ "case TheState of esStopped:" } ;
      esRunning: begin
        NewPgmButton.Enabled := false  ;

        RunModeGroupBox.Visible := cfSwitchEnableOrHalt in consoleFeatures ;
        RunModeGroupBox.Enabled := true ;
        ResetButton.Enabled := false ; // reset macht nicht auch Stop
        ResetAndStartButton.Enabled := false ;
        ContinueButton.Enabled := false ;
        HaltButton.Enabled := true ; // doHalt sagt ggf: "Press Reset"
        SingleStepButton.Enabled := false ;

        Caption := setFormCaptionInfoField(Caption, 'running!') ;
      end{ "case TheState of esRunning:" } ;
    end{ "case TheState" } ;

    // Runmode aktualisieren:
    // Wurde über "Connection settings" der Console typ gewechselt, ist der neue
    // Console runmode = unknown.
    if RunModeGroupBox.Visible then begin
      if FormMain.PDP11Console.RunMode = crmHalt then begin
        HaltRadioButton.Checked := true ;
        RunHaltInfoMemo.Font.Color := clGray
      end else if FormMain.PDP11Console.RunMode = crmRun then begin
        RunRadioButton.Checked := true ;
        RunHaltInfoMemo.Font.Color := clGray
      end else begin // unknown
        RunRadioButton.Checked := false ;
        HaltRadioButton.Checked := false ;
        RunHaltInfoMemo.Font.Color := clRed ;
      end ;
    end{ "if RunModeGroupBox.Visible" } ;

    // diese Flag steuert auch den Mode der Fakes !
    if FormMain.SerialIoHub.FakePDP11 <> nil then
      FormMain.SerialIoHub.FakePDP11.RunMode := false ;

  end{ "procedure TFormExecute.UpdateDisplay" } ;


// macht reset + Set PC
procedure TFormExecute.doResetMachineAndSetPC ;
//  var tmp_pc_v: TMemoryAddress ; // Inhalt des  PC ist eine virtual addr
  begin
    if not CheckRunMode then Exit ;
    updateConsoleFeatures ;
    FormMain.FormPdp1170Panel.OnCpuStart(nil) ; // switchregister von console update?
    try
      //den aktuellen PC auf den StartPC setzen
      CurPc.addr.val := PhysicalIopageBaseAddr(CurPc.addr.mat) + _17707 ; // R7/PC
      CurPc.edit_value := StartPc_v.val ;

      if cfActionResetMachine in FormMain.PDP11Console.getFeatures then begin
//        tmp_pc_v.mat := matVirtual ;
//        tmp_pc_v.val := CurPc.pdp_value ;
        FormMain.PDP11Console.ResetMachine(StartPc_v) ;  // feed back kann fehlschlagen
      end ;

      CurPc.Deposit ; // damit alle memorycells updaten. PC in target machine
      // ist eigentlich schon gesetzt
      SetAndShowPc(StartPc_v) ;

    finally
      TheState := esStopped ;
      UpdateDisplay ;
    end{ "try" } ;
  end{ "procedure TFormExecute.doResetMachineAndSetPC" } ;


procedure TFormExecute.doResetMachineAndSetPCandStart ;
//  var tmp_pc_v: TMemoryAddress ; // Inhalt des  PC ist eine virtual addr
  begin
    if not CheckRunMode then Exit ;
    updateConsoleFeatures ;
    FormMain.FormPdp1170Panel.OnCpuStart(nil) ; // switchregister von console update?
//    tmp_pc_v.mat := matVirtual ;
//    tmp_pc_v.val := CurPc.pdp_value ;
    try
      TheState := esRunning ;
      // wenn möglich, Init benutzen
      with FormMain.PDP11Console do begin
        if not (cfActionResetMaschineAndStartCpu in consoleFeatures) then
          raise Exception.Create('cfActionResetCpuAndStart not implemented!') ;
        ResetMachineAndStartCpu(StartPc_v) // feed back kann fehlschlagen
      end ;
//        else
//          Run(tmp_pc_v) ; // feed back kann fehlschlagen
    finally
      UpdateDisplay ;
    end{ "try" } ;
  end{ "procedure TFormExecute.doResetMachineAndSetPCandStart" } ;


// wird nicht an die Oberfläche geleitet
// Start OHNE Reset!
procedure TFormExecute.doSetPCandContinue(newpc_v: TMemoryAddress) ;
  begin
    SetAndShowPc(newpc_v) ;
    CurPc.Deposit ;
    doContinue ;
  end;


procedure TFormExecute.doContinue ;
  begin
    if not CheckRunMode then Exit ;
    updateConsoleFeatures ;
    FormMain.FormPdp1170Panel.OnCpuStart(nil) ; // switchregister von console update?
    try
      TheState := esRunning ;
      with FormMain.PDP11Console do begin
        if not (cfActionContinueCpu in consoleFeatures) then
          raise Exception.Create('cfActionContinueCpu not implemented!') ;
        ContinueCpu ;
      end ;
    finally
      UpdateDisplay ;
    end;
  end{ "procedure TFormExecute.doContinue" } ;


procedure TFormExecute.doSingleStep ;
  var tmp_pc_v: TMemoryAddress ; // Inhalt des  PC ist eine virtual addr
  begin
    if not CheckRunMode then Exit ;
    updateConsoleFeatures ;
    FormMain.FormPdp1170Panel.OnCpuStart(nil) ; // switchregister von console update?
    try
      TheState := esRunning ;
      UpdateDisplay ;
      tmp_pc_v.mat := matVirtual ;
      tmp_pc_v.val := CurPc.pdp_value ;
      with FormMain.PDP11Console do begin
        if not (cfActionSingleStep in consoleFeatures) then
          raise Exception.Create('cfActionSingleStep not implemented!') ;
        SingleStep;//(tmp_pc_v) ;  // feed back kann fehlschlagen
      end;
      CurPc.pdp_value := tmp_pc_v.val ;
      // update des PC und der Anzeige durch den OnStop-Handler
    finally
//      TheState := esStopped ;
      // tmp_pc = stop position, wird ignoriert. SimulationStopped macht aber die Arbeit!
      // es erkennt uach esStopped
      UpdateDisplay ;
    end{ "try" } ;
  end{ "procedure TFormExecute.doSingleStep" } ;



// CPU anhalten, irgendwie
procedure TFormExecute.doHalt ;
  var dummypc: TMemoryAddress ;
  begin
    if not CheckRunMode then Exit ;
    updateConsoleFeatures ;
    try
      if not (cfActionHaltCpu in consoleFeatures) then begin
        // HALT wird nicht unterstützt
        if cfSwitchEnableOrHalt in consoleFeatures then
          case FormMain.PDP11Console.RunMode of
            crmHalt:
              MessageDlg('RUN/HALT switch is set to HALT already ... processor should be stopped!', mtInformation, [mbOk], 0) ;
            crmRun:
              MessageDlg('HALT CPU by toggling physical RUN/HALT switch!', mtInformation, [mbOk], 0)
          end
        else
          MessageDlg('HALT CPU somehow over console panel', mtInformation, [mbOk], 0)
//          raise Exception.Create('Console has neither RUN/HALT switch nor HALT command!?') ;
      end { "if not (cfActionHaltCpu in consoleFeatures)" } else
        FormMain.PDP11Console.HaltCpu(dummypc) ;
      // dummypc = stop position. SimulationStopped macht aber die Arbeit!
    finally
      TheState := esStopped ;
      UpdateDisplay ;
    end{ "try" } ;
  end{ "procedure TFormExecute.doHalt" } ;






procedure TFormExecute.ResetButtonClick(Sender: TObject);
  begin
    doResetMachineAndSetPC ;
  end;


procedure TFormExecute.HaltButtonClick(Sender: TObject);
  begin
    doHalt ;
  end;


procedure TFormExecute.HaltRadioButtonClick(Sender: TObject);
  begin
    // Der Console die neue Schalterstellung an der Maschine mitteilen
    if FormMain.PDP11Console <> nil then
      if HaltRadioButton.Checked then begin
        FormMain.PDP11Console.RunMode := crmHalt ;
        // diese Flag steuert auch den Mode der Fakes !
        if FormMain.SerialIoHub.FakePDP11 <> nil then
          FormMain.SerialIoHub.FakePDP11.RunMode := false ;
      end ;
    UpdateDisplay ;
  end;


procedure TFormExecute.RunRadioButtonClick(Sender: TObject);
  begin
    // Der Console die neue Schalterstellung an der Maschine mitteilen
    if FormMain.PDP11Console <> nil then
      if RunRadioButton.Checked then begin
        FormMain.PDP11Console.RunMode := crmRun ;
        // diese Flag steuert auch den Mode der Fakes !
        if FormMain.SerialIoHub.FakePDP11 <> nil then
          FormMain.SerialIoHub.FakePDP11.RunMode := true ;
      end;
    UpdateDisplay ;
  end;

procedure TFormExecute.ResetAndStartButtonClick(Sender: TObject);
  begin
    doResetMachineAndSetPCandStart ;
  end;


procedure TFormExecute.SingleStepButtonClick(Sender: TObject);
  begin
    doSingleStep ;
  end;




end{ "unit FormExecuteU" } .
