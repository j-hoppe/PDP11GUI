unit FormMainU;
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


TODO

"Autorefresh" von mem und registern nach CPU halt/single step

"Micro step", mit update der microIsntruction

Panel mit internen "Machine" registern, auch auto update





- Console:
  interface =
    Examine, Deposit, set PC, run, single step,
    OnHaltEvent

- eigenes "Execution" Fenster
  - editbox, wo man das "start" label definiert
  - Knöpfe
   reset - pc auf start
  start - "s value" an pdp11/44,   "ru adress" auf SimH
  single step: ab pc einen schritt ausführen
  show current instruction
  Editbox: Anzeige des PC
  Status: "Running/Halted"

 der aktuelle PC soll im Listingfenster rot eingemalt werden
 man soll mit singelstep laufen


- Settings: 3 x Auswahl "echte PD-P11 an COM-Port,simulierte 11/44, SimH"

 Listing : die Suffixe' G C sind Fehler!

 PDP-11/44-Console: schneller ZUgriff auf Register über "E/G n" befehle
 Machine registers: oberhalb 2^22 ansiedeln, in E/G n umsetzen


 Neue "SimH"-Console:
 Steuert SimH über einen Telnet-Clienten an.
 benutzt die SimH-COmmandos für examine/deposit

 Basisklasse für COnsole abspalten!




  Child-Forms:

  Sind mit MenuItems verknüpft:
  - MenuItems werden mit derselben Caption angelegt.
  - MenuItems und Forms können über Caption gesucht werden.

  ChildForms sind MDIChilds.
  Besonderheit: "Hide" musste ich selber bauen

  Form Create -> Create aller Childs
  Form Show und MenuItems -> ChildFormShow, style = MDIChild
  FormClose-> Style = normale, Hide

  Eine Speicherzelle kann in verschiednen ChildForms angezeigt werden.
  Damit nicht zu oft "Examine" gemacht werden muss, wird bei Änderung
  einer Zelle die Adresse in allen Formularen aktualisiert
  MemoryCellgroups.SyncMemoryCells() macht das update,
  Anzeige in den Forms über ein "OnmemoryCellChanged" callback.

  Revisionhistory:
  ----------------
  See "history.txt" !


}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, Menus, CheckLst, ComCtrls,
  FormChildU,
  RegistryU,
  AddressU,
  MemoryCellU,
  ConsoleGenericU,
  SerialIoHubU,
  FormSettingsU,
  FormTerminalU,
  FormMacro11SourceU,
  FormMacro11ListingU,
  FormMacro11CodeU,
  FormLogU,
  FormMemoryListU,
  FormMemoryTableU,
  FormMemoryLoaderU,
  FormMemoryDumperU,
  FormMemoryTestU,
  FormDiscImageU,
  FormBitfieldsU,
  FormMmuU,
  FormExecuteU,
  FormExecuteBlinkenlightU,
  FormDisasU,
  FormMicroCodeU,
  Pdp1144MicroCodeU,
  FormIoPageScannerU,
  FormAboutU,
  FormPdp1170PanelU,
  FormNumberconverterU
  ;

type
  TFormMain = class(TForm)
      UpdateTimer: TTimer;
      MainMenu1: TMainMenu;
      File1: TMenuItem;
      Exit1: TMenuItem;
      View1: TMenuItem;
      Terminal1: TMenuItem;
      Macro11Source1: TMenuItem;
      Macro11Listing1: TMenuItem;
      Log1: TMenuItem;
      Mem11: TMenuItem;
      Mem21: TMenuItem;
      MemoryMenuItem: TMenuItem;
      Image1: TImage;
      Windows1: TMenuItem;
      Cascade1: TMenuItem;
      Execute1: TMenuItem;
      Connection1: TMenuItem;
      Settings2: TMenuItem;
      Reconnect1: TMenuItem;
      Arangeicons1: TMenuItem;
      N4: TMenuItem;
      Bitfieldshelper1: TMenuItem;
      MMU1: TMenuItem;
      Code1: TMenuItem;
      Disassembly1: TMenuItem;
      Help1: TMenuItem;
      Onlinedocumentation1: TMenuItem;
      About1: TMenuItem;
      Loadmachinedescription1: TMenuItem;
      OpenDialog1: TOpenDialog;
      N1: TMenuItem;
      N6: TMenuItem;
      MemoryLoader1: TMenuItem;
      MinimizeAll1: TMenuItem;
      Minimizeallbutactive1: TMenuItem;
      RestoreAll1: TMenuItem;
      IOpagescanner1: TMenuItem;
      PDP1170panel1: TMenuItem;
      StartupTimer: TTimer;
      N7: TMenuItem;
      IOpageMenuItem: TMenuItem;
      N2: TMenuItem;
      N8: TMenuItem;
      Programming1: TMenuItem;
      RL02image1: TMenuItem;
      N3: TMenuItem;
      Onlinetutorial1: TMenuItem;
      MemoryDumper1: TMenuItem;
      Mem31: TMenuItem;
      Mem41: TMenuItem;
      N5: TMenuItem;
      N9: TMenuItem;
      Blinkenlightinstructions1: TMenuItem;
      N10: TMenuItem;
      MemoryTest1: TMenuItem;
      Numberconverter1: TMenuItem;
      N11: TMenuItem;
      procedure FormShow(Sender: TObject);
      procedure UpdateTimerTimer(Sender: TObject);
      procedure ReconnectButtonClick(Sender: TObject);
      procedure FormEnableMenuItemClick(Sender: TObject);
      procedure Exit1Click(Sender: TObject);
      procedure FormCreate(Sender: TObject);
      procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
      procedure Cascade1Click(Sender: TObject);
      procedure Settings1Click(Sender: TObject);
      procedure Arangeicons1Click(Sender: TObject);
      procedure Onlinedocumentation1Click(Sender: TObject);
      procedure About1Click(Sender: TObject);
      procedure Loadmachinedescription1Click(Sender: TObject);
      procedure MinimizeAll1Click(Sender: TObject);
      procedure Minimizeallbutactive1Click(Sender: TObject);
      procedure RestoreAll1Click(Sender: TObject);
      procedure StartupTimerTimer(Sender: TObject);
      procedure Onlinetutorial1Click(Sender: TObject);
      procedure Programming1Click(Sender: TObject);
    private
      { Private-Deklarationen }
      procedure RegistrySaveControls ;
      function isMDIChildForm(childform: TFormChild): boolean ;
      function ChildFormByCaption(aCaption: string): TFormChild ;
      function ChildFormMenuItemByCaption(aCaption: string): TMenuItem ;
      procedure ChildFormClose(Sender: TObject; var Action: TCloseAction);
      procedure AppException(Sender: TObject; E: Exception);
    public
      { Public-Deklarationen }
      DefaultDataDirectory: string ;
      // im inifile definierte Speicherbereiche (Registerliste)
      // und alle freie Memorybereiche (tabellarisch)
      MemoryCellGroups: TMemoryCellGroups ;

      // PDP-11, mit der kommuniziert wird
      PDP11Console: TConsoleGeneric ;

      SerialIoHub: TSerialIoHub ;

      // alle Forms werden der MainForm untergeordnet.
      // FormSettings : TFormSettings ; // nein! ist modal
      FormTerminal: TFormTerminal ;
      FormMem1, FormMem2,FormMem3, FormMem4 : TFormMemoryTable ;
      FormMemoryLoader : TFormMemoryLoader ;
      FormMemoryDumper : TFormMemoryDumper ;
      FormMemoryTest : TFormMemoryTest ;
      FormDiscImage: TFormDiscImage ;
      FormBitfields : TFormBitfields ;
      FormMicroCode : TFormMicroCode ;
      FormMMU : TFormMMU ;
      FormMacro11Source : TFormMacro11Source ;
      FormMacro11Listing : TFormMacro11Listing ;
      FormMacro11Code : TFormMacro11Code ;
      FormLog : TFormLog ;
      FormNumberConverter : TFormNumberConverter ;
      FormExecute : TFormExecute ;
      FormExecuteBlinkenlight : TFormExecuteBlinkenlight ;
      FormDisas : TFormDisas ;
      FormIoPageScanner : TFormIoPageScanner ;
      FormPdp1170Panel : TFormPdp1170Panel ;


      procedure RenewPDP11ConsoleAndConnection(settings: TFormSettingsConfiguration) ;
      procedure ApplySettings ;
      procedure UpdateGUI ;
      procedure LoadMachineDescription(fname:string) ;
      procedure UnloadMachineDescription ;
      procedure setChildFormVisibility(childform: TFormChild; isVisible: boolean) ;
      procedure SyncBitfieldForm(mc: TMemoryCell) ;

    end{ "TYPE TFormMain = class(TForm)" } ;


  // Logginginterface hier
procedure Log(s: string) ; overload ;
procedure Log(fmt: string ; args: array of const) ; overload ;
procedure LogStrCol(aColidx: TLogColumnIndex ; s: string) ; overload ;
procedure LogStrCol(aColidx: TLogColumnIndex ; fmt: string ; args: array of const) ; overload ;
procedure Log2Buffer(s: string) ; overload ;
procedure Log2Buffer(fmt: string ; args: array of const) ; overload ;


var
  FormMain: TFormMain;


implementation

uses
  AuxU,
  OCtalConst,
  ShellAPI, // fuer URL-Start
  ShFolder,
  JH_Utilities,
  ConsolePDP11M9312FakeU,
  ConsolePDP11M9312U,
  ConsolePDP11M9301FakeU,
  ConsolePDP11M9301U,
  ConsolePDP1144U,
  ConsolePDP1144FakeU,
  ConsolePDP1144v340cU,
  ConsolePDP1144v340cFakeU,
  ConsolePDP11ODTU,
  ConsolePDP11ODTFakeU,
  ConsolePDP11ODTK1630U,
  ConsolePDP11ODTK1630FakeU,
  ConsolePDP11SimHU ;

{$R *.dfm}

/// Globales Interface zur Logging-Komponente
procedure Log(s: string) ;
  begin
    // OutputDebugString(s);
    FormMain.FormLog.Log(s);
  end;

procedure Log(fmt: string ; args: array of const) ;
  begin
    FormMain.FormLog.Log(Format(fmt, args)) ;
  end;

// fuer zeitkritische Sachen, LogBuffer muss in Breakpoint selber gecheckt werden
procedure Log2Buffer(s: string) ;
  begin
    FormMain.FormLog.Log2Buffer(s);
  end;

procedure Log2Buffer(fmt: string ; args: array of const) ;
  begin
    FormMain.FormLog.Log2Buffer(Format(fmt, args)) ;
  end;


procedure LogStrCol(aColidx: TLogColumnIndex ; s: string) ;
  begin
    FormMain.FormLog.LogStrCol(aColidx, s) ;
  end ;

procedure LogStrCol(aColidx: TLogColumnIndex ; fmt: string ; args: array of const) ;
  begin
    FormMain.FormLog.LogStrCol(aColidx, Format(fmt, args)) ;
  end ;



procedure TFormMain.FormCreate(Sender: TObject);
  var
    machinedescriptionfname: string ;
    mcg: TMemoryCellGroup ;
  begin
    StartupTimer.Enabled := false ; // erst nach OnShow feuern
    UpdateTimer.Enabled := false ; // erst nach Startup feuern

    // CSIDL_LOCAL_APPDATA: Vista UAC comaptible
    // Siehe “Designed for Microsoft Windows XP” Application Specification.
    // data is "per-user, not-roaming"
    // zB: "C:\Dokumente und Einstellungen\Joerg\PDP11GUI\"
    // DefaultDataPath := SHGetFolderPath(CSIDL_PERSONAL) + '\PDP11GUI' ;

    // Die DEBUG-Versionen haben c:\progs\pdp11gui als "defaultdatadirectory"
    {$ifdef DEBUG}
    DefaultDataDirectory := ExtractFilePath(Application.Exename) ;
    MessageDlg('PDP11GUI Debug version: datadir= ' + DefaultDataDirectory,
            mtInformation, [mbOk], 0) ;
    {$else}
    DefaultDataDirectory := SHGetFolderPath(CSIDL_COMMON_APPDATA) + '\PDP11GUI' ;
    {$endif}

    ForceDirectories(DefaultDataDirectory) ;

    // alle Speicherzellen
    MemoryCellGroups := TMemoryCellGroups.Create ;

    // Console, mit der kommuniziert wird. wird später nach Einstellung
    // auf FormSettings erzeugt.
    SerialIoHub := TSerialIoHub.Create ;

    PDP11Console := nil ;

    FormAbout := TFormAbout.Create(self); // zuerst, wegen VersionStr

    FormLog := TFormLog.Create(self) ;
    FormLog.Clear ;
    Log('*** PDP11GUI '+ FormAbout.VersionStr +' started ***') ;
    Log('') ;
    Application.OnException := AppException ;
    // ab hier kann geloggt werden

    FormNumberConverter := TFormNumberConverter.Create(self) ;

    // 2.a feste Forms als Componente der Mainform erzeugen
    // noch nicht visible oder RegistryLoad
    // FormSettings := TFormSettings.Create(self) ;
    // alle dynamischen Forms anlegen. siehe "isChildForm()"
    FormTerminal := TFormTerminal.Create(self) ; // wird in Componetes[] gespeiehrt
    SerialIoHub.Terminal := FormTerminal ; // connect Terminal an Datenstrom

    FormMem1 := TFormMemoryTable.Create(self) ;
    FormMem1.Caption := 'Mem1' ;
    FormMem2 := TFormMemoryTable.Create(self) ;
    FormMem2.Caption := 'Mem2' ;
    FormMem3 := TFormMemoryTable.Create(self) ;
    FormMem3.Caption := 'Mem3' ;
    FormMem4 := TFormMemoryTable.Create(self) ;
    FormMem4.Caption := 'Mem4' ;
    FormMemoryLoader := TFormMemoryLoader.Create(self) ;
    FormMemoryLoader.Caption := 'Memory Loader' ;
    FormMemoryDumper := TFormMemoryDumper.Create(self) ;
    FormMemoryDumper.Caption := 'Memory Dumper' ;
    FormMemoryTest := TFormMemoryTest.Create(self) ;
    FormMemoryTest.Caption := 'Memory Test' ;
    FormDiscImage := TFormDiscImage.Create(self) ;
    FormDiscImage.Caption := 'Read/write disc images' ;
    FormBitfields := TFormBitfields.Create(self) ;
    FormBitfields.Caption := 'Bitfields' ;
    FormMMU := TFormMMU.Create(self) ;
    FormMMU.Caption := 'MMU' ;

    FormMicroCode := TFormMicroCode.Create(self) ;
    FormMicroCode.Caption := 'µ Code' ;

    FormMacro11Source := TFormMacro11Source.Create(self) ;
    FormMacro11Listing := TFormMacro11Listing.Create(self) ;
    FormMacro11Code := TFormMacro11Code.Create(self) ; // erzeugt memorycellgroups
    FormMacro11Code.Caption := 'MACRO11 Code' ;

    FormExecute := TFormExecute.Create(self) ;
    FormExecute.Caption := 'Execution Control' ;

    FormExecuteBlinkenlight := TFormExecuteBlinkenlight.Create(self) ;
    FormExecuteBlinkenlight.Caption := 'Blinkenlight instructions' ;

    FormDisas := TFormDisas.Create(self) ;
    FormDisas.Caption := 'Disassembly' ;

    FormIoPageScanner := TFormIoPageScanner.Create(self) ;
    FormIoPageScanner.Caption := 'I/O page scanner' ;

    FormPdp1170Panel := TFormPdp1170Panel.Create(self) ;
    FormPdp1170Panel.Caption := 'PDP-11/70 panel' ;

    // 21.b) für die beiden Memoryfenster CellGroups anlegen
    // zwei 64 byte lange Speicherblöcke anlegen
    // Startadresse wird vom User in der TableForm gesetzt
    // feste Memorytables Mem1 bis Mem4 erzeugen
    mcg := MemoryCellGroups.AddSequentialMemoryCellsAsGroup(matPhysical22, 'memtable', 'Mem1',{startaddr}0, 64) ;
    FormMem1.MemoryGrid.ConnectToMemoryCellGroup(mcg) ;
    mcg := MemoryCellGroups.AddSequentialMemoryCellsAsGroup(matPhysical22, 'memtable', 'Mem2',{startaddr}0, 64) ;
    FormMem2.MemoryGrid.ConnectToMemoryCellGroup(mcg) ;
    mcg := MemoryCellGroups.AddSequentialMemoryCellsAsGroup(matPhysical22, 'memtable', 'Mem3',{startaddr}0, 64) ;
    FormMem3.MemoryGrid.ConnectToMemoryCellGroup(mcg) ;
    mcg := MemoryCellGroups.AddSequentialMemoryCellsAsGroup(matPhysical22, 'memtable', 'Mem4',{startaddr}0, 64) ;
    FormMem4.MemoryGrid.ConnectToMemoryCellGroup(mcg) ;

    // für den Memory loader eine leere memorycellgroup
    mcg := MemoryCellGroups.AddSequentialMemoryCellsAsGroup(matPhysical22, 'memloader', 'Memory Loader',{startaddr}0, 1) ;
    FormMemoryLoader.MemoryGrid.ConnectToMemoryCellGroup(mcg) ;

    // für den Memory Dumper eine leere memorycellgroup
    mcg := MemoryCellGroups.AddSequentialMemoryCellsAsGroup(matPhysical22, 'memdumper', 'Memory Dumper',{startaddr}0, 1) ;
    FormMemoryDumper.MemoryGrid.ConnectToMemoryCellGroup(mcg) ;

    // für den Memory test eine leere memorycellgroup
    mcg := MemoryCellGroups.AddSequentialMemoryCellsAsGroup(matPhysical22, 'memtest', 'Memory Test',{startaddr}0, 1) ;
    FormMemoryTest.MemoryGrid.ConnectToMemoryCellGroup(mcg) ;

    // für alle Disc image loader dieselbe leere memorycellgroup
    mcg := MemoryCellGroups.AddSequentialMemoryCellsAsGroup(matPhysical22, 'discloader', 'Disc Loader',{startaddr}0, 1) ;
    //FormMemoryLoader.MemoryGrid.ConnectToMemoryCellGroup(mcg) ;

    // für die Bitfield-Form eine Group, die nur aus einer Zelle besteht
    mcg := MemoryCellGroups.AddSequentialMemoryCellsAsGroup(matPhysical22, 'bitfield', 'Bitfields',{startaddr}0, 1) ;
    FormBitfields.ConnectToMemoryCell(mcg) ;
    FormBitfields.ShowNewAddr(mcg.Cell(0));

    // für die Execute-Form eine Gruppe, die nur den PC enthält
    // wert egal, wid in FormExecute neu gesetzt
    mcg := MemoryCellGroups.AddSequentialMemoryCellsAsGroup(matPhysical22, 'ExcutePC', 'Execute',{startaddr}_17777707, 1) ;
    FormExecute.ConnectToMemoryCells(mcg) ;


    // für die Disassembly-Form eine Gruppe, die vorerst nur eine Zelle enthält
    // virtuelle adresse!
    mcg := MemoryCellGroups.AddSequentialMemoryCellsAsGroup(matVirtual, 'ExcuteDisas', 'Disassembly',{startaddr}0, 1) ;
    FormDisas.ConnectToMemoryCells(mcg) ;

    // für die IoPageScanner-Form eine Gruppe, die vorerst nur eine Zelle enthält
    mcg := MemoryCellGroups.AddSequentialMemoryCellsAsGroup(matPhysical22, 'IoPageScanner', 'I/O page scanner',{startaddr}0, 1) ;
    FormIoPageScanner.ConnectToMemoryCells(mcg) ;

    // für das PDP1170panel eine Gruppe, die vorerst nur zwei Zellen enthält
    mcg := MemoryCellGroups.AddSequentialMemoryCellsAsGroup(matPhysical22, 'Pdp1170Panel', 'PDP-11/70 console',{startaddr}0, 2) ;
    FormPdp1170Panel.ConnectToMemoryCells(mcg) ;

    Caption := 'PDP11GUI ' + FormAbout.VersionStr ;

    UnloadMachineDescription ; // um an die caption zu kommen
    machinedescriptionfname := paramstr(1) ;  // param hat vorrang vor registry
    if machinedescriptionfname = '' then
      machinedescriptionfname := TheRegistry.Load('MachineDescriptionFile', DefaultDataDirectory+'\machines\pdp11.ini') ;
    if ExtractFilePath(machinedescriptionfname) = '' then
      machinedescriptionfname := DefaultDataDirectory + '\machines\' + machinedescriptionfname ;
    if not FileExists(machinedescriptionfname) then
      Log('%s: machine description file "%s" could not be opened', [Application.Exename, machinedescriptionfname])
    else begin
      LoadMachineDescription(machinedescriptionfname);
      TheRegistry.Save('MachineDescriptionFile', machinedescriptionfname) ;
    end;

  end{ "procedure TFormMain.FormCreate" } ;



procedure TFormMain.FormShow(Sender: TObject);
  begin
    StartupTimer.Enabled := true ;
  end;


// Feuert einmal, nach Start
procedure TFormMain.StartupTimerTimer(Sender: TObject);
  var i: integer ;
    childform: TFormChild ;
  begin
    StartupTimer.Enabled := false ;
    begin
      for i := 0 to ComponentCount-1 do
        if Components[i] is TFormChild then begin
          // nur die dynamischen Forms so behandeln, nicht formsettings
          childform := Components[i] as TFormChild ;
          if isMDIChildForm(childform) then begin
//          if frm = FormPdp1170Panel then begin
            // Sonder rolle für das pdp1170-Panel: ist kein MDI Child
//          end else begin

            // registry load setzt visible, wenn formstyle nicht MDIChild
            childform.FormStyle := fsNormal ;
            TheRegistry.Load(childform) ;
            // mache eine offenen oder geschlossene Childform draus
            setChildFormVisibility(childform, childform.visible);
            childform.OnClose := ChildFormClose ; // Close für alle Childs gleich behandeln
            //      end ;
          end{ "if isMDIChildForm(childform)" } ;
        end{ "if Components[i] is TFormChild" } ;
      TheRegistry.Load(self) ;
      visible := true ; // es kam vor, dass in der Registry FormMain.Visible := 0 steht!
      TheRegistry.Save(self) ;

      FormTerminal.Hello ;
      // Settings übernehmen. Vielleicht ist in settings noch keine
      // ausgewählt: zwinge user dazu!
      try
        RenewPDP11ConsoleAndConnection(FormSettings.SelectedConfiguration) ;
      except
        FormSettings.ShowModal ;
        ApplySettings ;
      end ;
    end;
    UpdateTimer.Enabled := true ; // erst nach OnSHow feuern
  end{ "procedure TFormMain.StartupTimerTimer" } ;



procedure TFormMain.RegistrySaveControls ;
  var
    i: integer ;
    childform: TFormChild ;
  begin
    TheRegistry.Save(self) ;
    for i := 0 to ComponentCount - 1 do
      if Components[i] is TFormChild then begin
        childform := Components[i] as TFormChild ;
        TheRegistry.Save(childform) ; // .name muss eindeutig sein!
      end;
  end;


// neue machinendefinition laden, Menus und Forms erzeugen
// VB: UnloadMachineDescription ist gelaufen
// NB: Register MemoryCellgroups sind als Physical16 geladen
procedure TFormMain.LoadMachineDescription(fname:string) ;
  var
    i : integer ;
    mi: TMenuItem ;
    mcg: TMemoryCellGroup ;
    frm_mcglist: TFormMemoryList ;
  begin
    // alle cell groups als "maschinedescription" markieren
    // die memorycells sind alle matPhysical16!
    MemoryCellGroups.AddGroupsFromIniFile('machinedescription', fname) ;

    // 1.b) dynamische IO-page-Forms bilden, und mit Speicherbereichen verbinden
    for i := 0 to MemoryCellGroups.Count - 1 do  begin
      mcg := MemoryCellGroups.Items[i] as TMemoryCellGroup ;
      if mcg.usagetag = 'machinedescription' then begin

        frm_mcglist := TFormMemoryList.Create(self) ;
        frm_mcglist.name := String2ID('FormMemoryCellGroup_' +  mcg.groupname) ;
        frm_mcglist.Caption := mcg.groupname ; // + ' registers' ; // Hier identifizierend!
        frm_mcglist.ConnectToMemoryCellGroup(mcg) ;
        mcg.associate := frm_mcglist ; // form mit cellgroup verbinden, s.u.

        // 1.c) dynamische Menuitems für IOpage-form anlegen
        mi := ChildFormMenuItemByCaption(frm_mcglist.Caption) ;
        if mi = nil then begin
          // menuitem für neue From/neue memorycellgroup im Menuitem
          // "iopage1" anlegen
          mi := TMenuItem.Create(self) ;
          mi.Caption := frm_mcglist.Caption ;
          mi.name := String2ID(mi.Caption + 'MenuItem') ;
          mi.OnClick := FormEnableMenuItemClick ;
          IOpageMenuItem.Add(mi);
        end ;
      end { "if mcg.usagetag = 'machinedescription'" } ;
    end{ "for i" } ;
    // file im header anzeigen
    Caption := setFormCaptionInfoField(Caption, ExtractFilename(fname)) ;
  end{ "procedure TFormMain.LoadMachineDescription" } ;

// Alles löschen, was aus der MachineDescription kommt
procedure TFormMain.UnloadMachineDescription ;
  var
    i : integer ;
    mi: TMenuItem ;
    mcg: TMemoryCellGroup ;
    frm_mcglist: TFormMemoryList ;
  begin
    i := 0 ;
    while i < MemoryCellGroups.Count do begin
      mcg := MemoryCellGroups.Items[i] as TMemoryCellGroup ;
      if mcg.usagetag = 'machinedescription' then begin
        // es müssen memorycellgroups, forms und menuitems gelöscht werden.

        frm_mcglist :=  mcg.associate as TFormMemoryList ;
        mi := ChildFormMenuItemByCaption(frm_mcglist.Caption) ;

        if mi <> nil then mi.Free ;
        if frm_mcglist <> nil then frm_mcglist.Free ;
        mcg.Free ;
      end else
        inc(i) ;
    end{ "while i < MemoryCellGroups.Count" } ;
    MemoryCellGroups.bitfieldsdefs.UnLoad ;
    // reconnect Bitfileds, damit alter Inhalt gelöscht wird
    FormBitfields.ConnectToMemoryCell(FormBitfields.memorycellgroup);

    Caption := setFormCaptionInfoField(Caption, 'no machine description') ;
  end{ "procedure TFormMain.UnloadMachineDescription" } ;


// welche Forms werden dynamisch als MDICHilds abgezeigt,
// und hängen über die Caption an Menuitems?
// Ist nötig, um zu entscheiden, welche Forms MDIChildren werden sollen
// nach FormShow/FormCreate kann die Collection "MDICHildren[]" benutzt werden.
function  TFormMain.isMDIChildForm(childform: TFormChild): boolean ;
  begin
    result := true ;
    // Modale Forms: Settings, About, help und solche Forms
    if childform = FormSettings then
      result := false ;
  end ;


procedure TFormMain.Loadmachinedescription1Click(Sender: TObject);
  var fname:string ;
  begin
    fname := TheRegistry.Load('MachineDescriptionFile', DefaultDataDirectory+'\pdp11.ini') ;
    OpenDialog1.InitialDir := ExtractFilePath(fname) ;
    OpenDialog1.Title := 'Choose a machine description file' ;
    if OpenDialog1.Execute then begin
      UnloadMachineDescription ;
      LoadMachineDescription(OpenDialog1.Filename);
      TheRegistry.Save('MachineDescriptionFile', OpenDialog1.Filename) ;
      Caption := setFormCaptionInfoField(Caption, ExtractFilename(OpenDialog1.Filename)) ;
      if MessageDlg('A restart of PDP11GUI is necessary now! Close program?', mtConfirmation,  [mbYes, mbNo], 0) = mrYes then
        Application.Terminate ;
    end;
  end;

procedure TFormMain.Onlinedocumentation1Click(Sender: TObject);
  begin
    ShellExecute(Handle, nil, HOME_URL, nil, nil, SW_SHOW) ;
  end;


procedure TFormMain.Onlinetutorial1Click(Sender: TObject);
  begin
    ShellExecute(Handle, nil, TUTORIAL_URL, nil, nil, SW_SHOW) ;
  end;

procedure TFormMain.Programming1Click(Sender: TObject);
  begin

  end;

// jede Exception ins Logfile
procedure TFormMain.About1Click(Sender: TObject);
  begin
    FormAbout.ShowModal ;
  end;


procedure TFormMain.AppException(Sender: TObject; E: Exception);
  begin
    FormLog.Log(E.Message) ;
    Application.ShowException(E);
  end;


// erzeugt die richtige Subklasse einer PDP11-Console neu
procedure TFormMain.RenewPDP11ConsoleAndConnection(settings: TFormSettingsConfiguration) ;
  var s: string ;
  begin
    // Umrechnung der Adressbreiten (16, 18, 22) ist kitzelig:
    // - Die Fakes brauchen die MemoryCells aus der machine description in
    //   der neuen breite
    // - Für non-Fakes ist die Adressbreite erst nach erzeugen der neuen Console klar

    // Connection einstellen
    case settings.ConnectionType of
      connectionInternal:
        case settings.ConsoleType of
          consoleSelftest11M9312: begin
            SerialIoHub.Physical_InitForFakePDP11M9312(settings.serialBaudrate) ;
            // alle MemoryCells auf die neue Adressbreite umrechnen
            MemoryCellGroups.ChangeAdddressWidth(SerialIoHub.FakePDP11.mat) ;
            // der fake implementiert die register, die im machine description file stehen
            SerialIoHub.FakePDP11.CalcIoPageValidMap(MemoryCellGroups, 'machinedescription');
          end;
          consoleSelftest11M9301: begin
            SerialIoHub.Physical_InitForFakePDP11M9301(settings.serialBaudrate) ;
            // alle MemoryCells auf die neue Adressbreite umrechnen
            MemoryCellGroups.ChangeAdddressWidth(SerialIoHub.FakePDP11.mat) ;
            // der fake implementiert die register, die im machine description file stehen
            SerialIoHub.FakePDP11.CalcIoPageValidMap(MemoryCellGroups, 'machinedescription');
          end;
          consoleSelftest1144: begin
            SerialIoHub.Physical_InitForFakePDP1144(settings.serialBaudrate) ;
            // alle MemoryCells auf die neue Adressbreite umrechnen
            MemoryCellGroups.ChangeAdddressWidth(SerialIoHub.FakePDP11.mat) ;
            // der fake implementiert die register, die im machine description file stehen
            SerialIoHub.FakePDP11.CalcIoPageValidMap(MemoryCellGroups, 'machinedescription');
          end;
          consoleSelftest1144v340c: begin
            SerialIoHub.Physical_InitForFakePDP1144v340c(settings.serialBaudrate) ;
            // alle MemoryCells auf die neue Adressbreite umrechnen
            MemoryCellGroups.ChangeAdddressWidth(SerialIoHub.FakePDP11.mat) ;
            // der fake implementiert die register, die im machine description file stehen
            SerialIoHub.FakePDP11.CalcIoPageValidMap(MemoryCellGroups, 'machinedescription');
          end;
          consoleSelftest11ODT16 : begin
            SerialIoHub.Physical_InitForFakePDP11ODT(settings.serialBaudrate, matPhysical16) ;
            // alle MemoryCells auf die neue Adressbreite umrechnen
            MemoryCellGroups.ChangeAdddressWidth(SerialIoHub.FakePDP11.mat) ;
            // der fake implementiert die register, die im machine description file stehen
            SerialIoHub.FakePDP11.CalcIoPageValidMap(MemoryCellGroups, 'machinedescription');
          end;
          consoleSelftest11ODT18 : begin
            SerialIoHub.Physical_InitForFakePDP11ODT(settings.serialBaudrate, matPhysical18) ;
            // alle MemoryCells auf die neue Adressbreite umrechnen
            MemoryCellGroups.ChangeAdddressWidth(SerialIoHub.FakePDP11.mat) ;
            // der fake implementiert die register, die im machine description file stehen
            SerialIoHub.FakePDP11.CalcIoPageValidMap(MemoryCellGroups, 'machinedescription');
          end;
          consoleSelftest11ODT22 : begin
            SerialIoHub.Physical_InitForFakePDP11ODT(settings.serialBaudrate, matPhysical22) ;
            // alle MemoryCells auf die neue Adressbreite umrechnen
            MemoryCellGroups.ChangeAdddressWidth(SerialIoHub.FakePDP11.mat) ;
            // der fake implementiert die register, die im machine description file stehen
            SerialIoHub.FakePDP11.CalcIoPageValidMap(MemoryCellGroups, 'machinedescription');
          end;
          consoleSelftest11ODTK1630 : begin
            SerialIoHub.Physical_InitForFakePDP11ODTK1630(settings.serialBaudrate) ;
            // alle MemoryCells auf die neue Adressbreite umrechnen
            MemoryCellGroups.ChangeAdddressWidth(SerialIoHub.FakePDP11.mat) ;
            // der fake implementiert die register, die im machine description file stehen
            SerialIoHub.FakePDP11.CalcIoPageValidMap(MemoryCellGroups, 'machinedescription');
          end;
        end{ "case settings.ConsoleType" } ;
      connectionSerial:
        SerialIoHub.Physical_InitForCOM(settings.serialComport, settings.serialBaudrate, settings.serialFormat) ;
      connectionTelnet:
        SerialIoHub.Physical_InitForTelnet(settings.telnetHostname, settings.telnetPort) ; // SimH nur über telnet
      else
        raise Exception.CreateFmt('Can not generate PDP-11 connection of type %d', [ord(settings.ConnectionType )]) ;
    end{ "case settings.ConnectionType" } ;

    if PDP11Console <> nil then
      PDP11Console.Free ; // bisherige Console in die Tonne

    case settings.ConsoleType of
      consoleSelftest11M9301:
        PDP11Console := TConsolePDP11M9301fake.Create(MemoryCellGroups,
                {don't care} settings.monitorentryaddress) ;
      consoleSelftest11M9312:
        PDP11Console := TConsolePDP11M9312fake.Create(MemoryCellGroups,
                {don't care} settings.monitorentryaddress) ;
      consoleSelftest1144:
        PDP11Console := TConsolePDP1144fake.Create(MemoryCellGroups) ;
      consoleSelftest1144v340c:
        PDP11Console := TConsolePDP1144v340Cfake.Create(MemoryCellGroups) ;
      consoleSelftest11ODT16:
        PDP11Console := TConsolePDP11ODTfake.Create(MemoryCellGroups, matPhysical16) ;
      consoleSelftest11ODT18:
        PDP11Console := TConsolePDP11ODTfake.Create(MemoryCellGroups, matPhysical18) ;
      consoleSelftest11ODT22:
        PDP11Console := TConsolePDP11ODTfake.Create(MemoryCellGroups, matPhysical22) ;
      consoleSelftest11ODTK1630:
        PDP11Console := TConsolePDP11ODTK1630Fake.Create(MemoryCellGroups) ;
      consolePDP11M9301:
        PDP11Console := TConsolePDP11M9301.Create(MemoryCellGroups, settings.monitorentryaddress) ;
      consolePDP11M9312:
        PDP11Console := TConsolePDP11M9312.Create(MemoryCellGroups, settings.monitorentryaddress) ;
      consolePDP1144:
        PDP11Console := TConsolePDP1144.Create(MemoryCellGroups) ;
      consolePDP1144v340c:
        PDP11Console := TConsolePDP1144v340c.Create(MemoryCellGroups) ;
      consolePDP11ODT16:
        PDP11Console := TConsolePDP11ODT.Create(MemoryCellGroups, matPhysical16) ;
      consolePDP11ODT18:
        PDP11Console := TConsolePDP11ODT.Create(MemoryCellGroups, matPhysical18) ;
      consolePDP11ODT22:
        PDP11Console := TConsolePDP11ODT.Create(MemoryCellGroups, matPhysical22) ;
      consolePDP11ODTK1630:
        PDP11Console := TConsolePDP11ODTK1630.Create(MemoryCellGroups) ;
      consoleSimH: begin
        PDP11Console := TConsolePDP11SimH.Create(MemoryCellGroups) ;
        // SimH hat 50Hz input-polling -> PDP11GUI kann nur mit ca. 600 baud senden!
//        SerialIoHub.XmtBaudrate := 500 ; // non std, wird nur für timing benutzt
        SerialIoHub.XmtBaudrate := 9600 ; // oder schneller.
        // Muss langsamer als tatsächliche Übertragung sein, wegen
        // berechnung von delays
      end
      else
        raise Exception.CreateFmt('Can not generate PDP11 console of type %d', [ord(settings.ConsoleType )]) ;
    end{ "case settings.ConsoleType" } ;

    // das ganze PDP11GUI auf eine bestimmte Adressbreite einstellen
//    ThePhysicalAddressWidth := PDP11Console.getPhysicalMemoryAddressType ;
    MemoryCellGroups.ChangeAdddressWidth(PDP11Console.getPhysicalMemoryAddressType) ;

    // ODT: reproduktion von 18/22, wie im Construktor angegeben.

    SerialIoHub.Console := PDP11Console ; // connect Console an Datenstrom

    // Das Terminal zeigt die Connectionsettings an
    s := Format('%s to %s', [SerialIoHub.Physical_getInfoString, PDP11Console.getName]) ;
    FormTerminal.Caption := setFormCaptionInfoField(FormTerminal.Caption, s) ;

    // Dem Terminal die Einstellungen der neuen Console verraten
    FormTerminal.TerminalSettings := PDP11Console.getTerminalSettings ;

    // Die ExecuteForm an die neue Console anschliessen
    PDP11Console.OnExecutionStop := FormExecute.SimulationStopped;
    FormExecute.UpdateDisplay ; // buttons neu enablen

    PDP11Console.Init(SerialIoHub) ;

    // Form mit anzuzeigender MMU verbinden
    FormMMU.MMU := PDP11Console.MMU ; // MMU gehört der Console
    PDP11Console.MMU.OnMMuChanged := FormMMU.MMUChanged ; // Form update, wenn MMU touched

    // alle offenen fenster refreshen, damit Wechsel 18 <-> 22 optisch sichtbar wird.
    // ???
    // prototypisch:
    FormMemoryTest.OnConsoleChanged ;

  end{ "procedure TFormMain.RenewPDP11ConsoleAndConnection" } ;


// Position und Sichtbarkeit aller Forms speichern
// in FormClose sind die Unter-Fenster schon alle geschlossen!
procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
  begin
    RegistrySaveControls ;
    CanClose := true ;
  end;


// Alle Menuitems, die Forms an- und abschalten
// lösen "FormEnableMenuItemClick" aus
// Startup: liste der sichtabren Forms
// Liste der bekannten Forms: FormMain.Compnentes
procedure TFormMain.Exit1Click(Sender: TObject);
  begin
    Close ;
  end;


procedure TFormMain.Arangeicons1Click(Sender: TObject);
  begin
    ArrangeIcons ;
  end;

procedure TFormMain.Cascade1Click(Sender: TObject);
  var i: integer ;
    orgsize: array[0..100] of TPoint ; // original Width/Heigth
  begin
    // besonderes Cascade: Fenstergrössen NICHT verändern!

    // Grössen merken
    for i := 0 to mdichildcount - 1 do begin
      orgsize[i].x := MDIChildren[i].Width ;
      orgsize[i].y := MDIChildren[i].Height ;
    end;

    Cascade ;

    // Grössen restaurieren
    for i := 0 to mdichildcount - 1 do begin
      MDIChildren[i].Width := orgsize[i].x ;
      MDIChildren[i].Height := orgsize[i].y ;
    end;
  end{ "procedure TFormMain.Cascade1Click" } ;

procedure TFormMain.MinimizeAll1Click(Sender: TObject);
  var i: integer ;
  begin
    for i := mdichildcount-1 downto 0 do
      MDIChildren[i].Windowstate := wsMinimized ;
  end;

procedure TFormMain.Minimizeallbutactive1Click(Sender: TObject);
  var i: integer ;
  begin
    for i := mdichildcount-1 downto 0 do
      if MDIChildren[i] <> ActiveMDIChild then
        MDIChildren[i].Windowstate := wsMinimized ;
  end;


procedure TFormMain.RestoreAll1Click(Sender: TObject);
  var i: integer ;
    active: TForm ;
  begin
    active := ActiveMDIChild ;
    for i := 0 to mdichildcount-1 do
      MDIChildren[i].Windowstate := wsNormal ;
    active.SetFocus ;
    active.BringToFront ;
  end;


// sucht die childform, die mit "caption" ANFÄNGT
// Sie kann also eine Caption wie "MACRO11 source - filename" haben
function TFormMain.ChildFormByCaption(aCaption: string): TFormChild ;
  var i: integer ;
    childform: TFormChild ;
  begin
    result := nil ;
    aCaption := String2ID(getFormCaptionFixedField(aCaption)) ; // igor einfo part of caption
    for i := 0 to ComponentCount - 1 do
      if Components[i] is TFormChild then begin
        childform := Components[i] as TFormChild ;
        if aCaption = String2ID(getFormCaptionFixedField(childform.Caption)) then
          result := childform ;
      end;
  end;


function TFormMain.ChildFormMenuItemByCaption(aCaption: string): TMenuItem;

  function _FormMenuItemByCaption(Items: TMenuItem; aCaption: string): TMenuItem;
    var i: integer ;
    begin
      result := nil ;
      aCaption := String2ID(getFormCaptionFixedField(aCaption)) ;
      if String2ID(getFormCaptionFixedField(Items.Caption)) = aCaption then begin
        result := Items ;
      end else
        for i := 0 to Items.Count - 1 do begin
          // rekursion in unterliste
          result := _FormMenuItemByCaption(Items[i], aCaption) ;
          if result <> nil then break ;
        end ;
    end{ "function _FormMenuItemByCaption" } ;

  begin { "function TFormMain.ChildFormMenuItemByCaption" }
    result := _FormMenuItemByCaption(MainMenu1.Items, aCaption) ;
  end ;

// Steuert Visible, und FormStyle
procedure TFormMain.setChildFormVisibility(childform: TFormChild; isVisible: boolean) ;
  begin
    if (childform.visible = isVisible) and (childform.FormStyle = fsMDIChild) then
      Exit ; // MDIForm ist schon so, wie gewünscht

    if isVisible then begin
      childform.FormStyle := fsMDIChild ;
      childform.Show ;
      childform.BringToFront ;
    end else begin
//@        childform.FormStyle := fsNormal ; // MDICHilds können nicht unsichtbar werden
      childform.Hide ;
    end;
  end{ "procedure TFormMain.setChildFormVisibility" } ;

// Einstellungen auf der Settings.Form anwenden
procedure TFormMain.ApplySettings ;
  begin
//    with FormSettings do begin
    // ggf. geändertes consolobject neu erzeugen
    RenewPDP11ConsoleAndConnection(FormSettings.SelectedConfiguration);
//    end;
  end;


procedure TFormMain.Settings1Click(Sender: TObject);
  begin
    FormSettings.ShowModal ;
    ApplySettings ;
  end;


// läuft, wenn der User das [X] zum schliessen klickt
procedure TFormMain.ChildFormClose(Sender: TObject; var Action: TCloseAction);
  var childform: TFormChild ;
  begin
    childform := Sender as TFormChild ;
    childform.Hide ;
    Action := caNone ; // caHide löst childform.Hide nicht aus!
  end;


// Zuordnung von Forms zu Menuitems
procedure TFormMain.FormEnableMenuItemClick(Sender: TObject);
  var
    mi : TMenuItem ;
    childform: TFormChild ;
  begin
    // die passende Form finden und anzeigen/verstecken
    mi := Sender as TMenuItem ;
    // finde die Form, die genauso heisst wie der Menuitem
    childform := ChildFormByCaption(mi.Caption) ;
    if childform = nil then
      raise Exception.CreateFmt('Program error: form for menu item "%s" not found!', [mi.Caption]) ;

    setChildFormVisibility(childform, not childform.visible) ;
  end{ "procedure TFormMain.FormEnableMenuItemClick" } ;


procedure TFormMain.UpdateGUI ;
  var
    i: integer ;
    childform: TFormChild ;
    mi: TMenuItem ;
  begin
    // die check-states der Menuitems an die sichtbarkeit der Forms anpassen
    for i := 0 to ComponentCount-1 do
      if Components[i] is TFormChild then begin
        childform := Components[i] as TFormChild ;
        mi := ChildFormMenuItemByCaption(childform.Caption) ;
        if mi <> nil then
          mi.checked := childform.visible ;
      end;

  end{ "procedure TFormMain.UpdateGUI" } ;

procedure TFormMain.UpdateTimerTimer(Sender: TObject);
  begin
    UpdateGUI ;
    // regelmässig Stand der Forms speichern ... damit sie nicht dauernd verloren gehen!
    RegistrySaveControls ;
  end;

procedure TFormMain.ReconnectButtonClick(Sender: TObject);
  begin
    FormLog.Log('*** Reset ***') ;
    PDP11Console.Resync ;
//    PDP11Console.Init(FormSettings.SelectedTargetParams) ;
  end;


// Bitfield-Form auf eine bestimmte Adresse einstellen
procedure TFormMain.SyncBitfieldForm(mc: TMemoryCell) ;
  begin
    FormBitfields.ShowNewAddr(mc) ;
  end;


end{ "unit FormMainU" } .
