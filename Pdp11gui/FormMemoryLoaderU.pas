unit FormMemoryLoaderU;

{
Speicher als editierbare Tabelle
Es werden immer 'memcol' Spalten nebeneinander angezeigt
Kopf "+0, +2, +4, ..."
Reihen

Die Anzahl der Spalten wird im Constructor festgelegt ('MemoryColumns').

}

interface 

uses 
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, Grids, StdCtrls, ExtCtrls, 
  FormChildU, 
  JH_Utilities, 
  AddressU, 
  MemoryCellU, Menus, 
  MemoryLoaderU, 
  FrameMemoryCellGroupGridU ; 


type 
  TFormMemoryLoader = class(TFormChild) 
      PanelT: TPanel; 
      VerifyAllButton: TButton; 
      StartAddrLabel: TLabel; 
      StartAddrEdit: TEdit; 
      MemoryGrid: TFrameMemoryCellGroupGrid; 
      LoadFileButton: TButton; 
      Label2: TLabel; 
      LoaderFileFormatComboBox: TComboBox; 
      File1Label: TLabel; 
      File2Label: TLabel; 
      Filename2Edit: TEdit; 
      Filename1Edit: TEdit; 
      BrowseFile1Button: TButton; 
      BrowseFile2Button: TButton; 
      OpenDialog1: TOpenDialog; 
      Label1: TLabel; 
      EndAddrEdit: TEdit; 
      EntryAddrLabel: TLabel; 
      EntryAddrEdit: TEdit; 
      DepositAllButton: TButton; 

      procedure EntryAddrEditKeyPress(Sender: TObject; var Key: Char);
//      procedure SetStartAddrButtonClick(Sender: TObject);
//      procedure DecAddrButtonClick(Sender: TObject);
//      procedure IncAddrButtonClick(Sender: TObject);
      procedure ExamineCurrentButtonClick(Sender: TObject); 
      procedure ExamineAllButtonClick(Sender: TObject); 
      procedure DepositChangedButtonClick(Sender: TObject); 
      procedure DepositAlllButtonClick(Sender: TObject); 
      procedure BrowseFile1ButtonClick(Sender: TObject); 
      procedure LoaderFileFormatComboBoxChange(Sender: TObject); 
      procedure LoadFileButtonClick(Sender: TObject); 
      procedure VerifyAllButtonClick(Sender: TObject); 
    private 
      { Private-Deklarationen }
      // jeden möglichen Loader instanziieren
      Loader_ByteStreamLH: TMemoryLoader_BytestreamLH ; 
      Loader_LowByteFileHighByteFile: TMemoryLoader_LowByteFileHighByteFile ; 
      Loader_TextfileOneAddrPerLine: TMemoryLoader_TextfileOneAddrPerLine; 
      Loader_StandardAbsolutePapertape : TMemoryLoader_StandardAbsolutePapertape ; 
    public 
      { Public-Deklarationen }
      curLoader: TMemoryLoader ; // wie durch Combobox ausgewählt
      constructor Create(AOwner: TComponent) ; 
      destructor Destroy ; override ; 

      procedure UpdateDisplay(Sender: TObject); 
      procedure UpdateExecuteAddress; 
    end{ "TYPE TFormMemoryLoader = class(TFormChild)" } ; 


implementation 

{$R *.dfm}

uses 
  AuxU, 
  RegistryU, 
  FormMainU; 

constructor TFormMemoryLoader.Create(AOwner: TComponent) ; 
  begin 
    inherited Create(AOwner) ; 

    // jeden möglichen Loader instanziieren und an Controls anschliessen
    Loader_ByteStreamLH := TMemoryLoader_BytestreamLH.Create ; 
    with Loader_ByteStreamLH.getFile(0) do begin 
      control_filenameprompt := File1Label ; 
      control_filenameedit := Filename1Edit ; 
      control_filenamebrowse := BrowseFile1Button ; 
    end; 

    Loader_LowByteFileHighByteFile := TMemoryLoader_LowByteFileHighByteFile.Create ; 
    // hat zwei Files
    with Loader_LowByteFileHighByteFile.getFile(0) do begin 
      control_filenameprompt := File1Label ; 
      control_filenameedit := Filename1Edit ; 
      control_filenamebrowse := BrowseFile1Button ; 
    end; 
    with Loader_LowByteFileHighByteFile.getFile(1) do begin 
      control_filenameprompt := File2Label ; 
      control_filenameedit := Filename2Edit ; 
      control_filenamebrowse := BrowseFile2Button ; 
    end; 


    Loader_TextfileOneAddrPerLine := TMemoryLoader_TextfileOneAddrPerLine.Create ; 
    with Loader_TextfileOneAddrPerLine.getFile(0) do begin 
      control_filenameprompt := File1Label ; 
      control_filenameedit := Filename1Edit ; 
      control_filenamebrowse := BrowseFile1Button ; 
    end; 

    Loader_StandardAbsolutePapertape := TMemoryLoader_StandardAbsolutePapertape.Create ; 
    with Loader_StandardAbsolutePapertape.getFile(0) do begin 
      control_filenameprompt := File1Label ; 
      control_filenameedit := Filename1Edit ; 
      control_filenamebrowse := BrowseFile1Button ; 
    end; 

    MemoryGrid.OnUpdate := UpdateDisplay ; // wenn sich das grid ändert, muss diese Form reagieren
    TheRegistry.Load(LoaderFileFormatComboBox) ; 

  end{ "constructor TFormMemoryLoader.Create" } ; 

destructor TFormMemoryLoader.Destroy ; 
  begin 
    inherited ; 
  end; 


// neue Oberfläche malen,
// ggf ExecuteWindow Startaddress updaten
procedure TFormMemoryLoader.UpdateDisplay(Sender: TObject); 
  var 
    loaderfile: TMemoryloaderFile ; 
    mc0, mc1: TMemoryCell ; 
    h, w: integer ; 
    i: integer ; 
    s: string ; 
  begin 
    if Sender <> MemoryGrid then begin // hat der Frame das Update ausgelöst?
      MemoryGrid.UpdateDisplay  // nein: update frame, er updated wieder die Form
    end else begin 
      // Editierte Memoryinhalte behalten, auch wenn Pdp durch callbacks neu abgefragt wird.
      with MemoryGrid.memorycellgroup do begin 
        PdpOverwritesEdit := false ; // statische initialisierung
        if Count = 0 then begin 
          StartAddrEdit.Visible := false ; 
          EndAddrEdit.Visible := false ; 
          Caption := setFormCaptionInfoField(Caption, 'no data') ; 
        end else begin 
          mc0 := Cell(0) ; 
          mc1 := Cell(Count-1) ; // letzte Zelle
          StartAddrEdit.Visible := true ; 
          EndAddrEdit.Visible := true ; 
          StartAddrEdit.Text := Addr2OctalStr(mc0.addr) ; // 1. Zelle = Startaddr
          EndAddrEdit.Text := Addr2OctalStr(mc1.addr) ; // 1. Zelle = Startaddr
          s := Format('%s-%s', [Addr2OctalStr(mc0.addr),Addr2OctalStr(mc1.addr)]) ; 
          Caption := setFormCaptionInfoField(Caption, s) ; 
        end; 
      end { "with MemoryGrid.memorycellgroup" } ; 
      // ausgewählten Loader merken
      case LoaderFileFormatComboBox.ItemIndex of 
        0: curLoader := Loader_ByteStreamLH ; 
        1: curLoader := Loader_LowByteFileHighByteFile ; 
        2: curLoader := Loader_TextfileOneAddrPerLine ; 
        3: curLoader := Loader_StandardAbsolutePapertape ; 
        else curLoader := nil ; 
      end; 

      if curLoader = nil then Exit ; // nur bei erstem Start nach Installation

      // Startaddresse nicht eingebbar, wenn vom Fileformat definiert.
      // Dann Schrift etwas matter
      if curLoader.StartAddrDefined then begin 
        // Startadresse vorgegeben
        StartAddrEdit.ReadOnly := true ; 
        StartAddrEdit.Font.Color := clDkGray ; 
      end else begin 
        // Startadresse eingebbar
        StartAddrEdit.ReadOnly := false ; 
        StartAddrEdit.Font.Color := clBlack ; 
      end; 
      EndAddrEdit.ReadOnly := false ; 
      EndAddrEdit.Font.Color := clDkGray ; 

      Caption := setFormCaptionInfoField(Caption, 
              Format('%s: "%s"', [curLoader.loadername, curLoader.getFile(0).filename])) ; 

      // a) alle Controls invisible
      File1Label.Visible := false ; 
      Filename1Edit.Visible := false ; 
      BrowseFile1Button.Visible := false ; 
      File2Label.Visible := false ; 
      Filename2Edit.Visible := false ; 
      BrowseFile2Button.Visible := false ; 
      EntryAddrLabel.Visible := false ; 
      EntryAddrEdit.Visible := false ; 

      // b) controls für den aktiven Loader visible ;
      for i := 0 to curLoader.Files.Count - 1 do begin 
        loaderfile := curLoader.getFile(i) ; 
        loaderfile.control_filenameprompt.Caption := loaderfile.prompt + ' :' ; 
        loaderfile.control_filenameprompt.Visible := true ; 
        loaderfile.control_filenameedit.Text := loaderfile.filename ; 
        loaderfile.control_filenameedit.Visible := true ; 
        loaderfile.control_filenamebrowse.Visible := true ; 
      end; 

      // definiert das Format eine Einsprungaddresse?
      if curLoader.hasEntryAddress then begin 
        EntryAddrLabel.Visible := true ; 
        EntryAddrEdit.Visible := true ; 
        EntryAddrEdit.Text := Addr2OctalStr(curLoader.EntryAddress) ; 
        EntryAddrEdit.ReadOnly := true ; 
        EntryAddrEdit.Font.Color := clDkGray ; 
      end; 

      // Das MemoryGrid ist alClient und möchte in einer bestimmten Grösse angezeigt werden,
      // tue ihm den Gefallen.
      // Wilde ad hoc Logik: das Codewindow kann extrem hoch werden, dann kürzer anzeigen
      h := MemoryGrid.optimal_height + PanelT.Height ;
      w := MemoryGrid.optimal_width ; 
      if h > (FormMain.ClientHeight-100) then 
        h := FormMain.ClientHeight-100  ; 
      if h < 200 then h := 200 ; // falls mainform klein ist
      if h - PanelT.Height < MemoryGrid.optimal_height then 
        w := w + 20 ; // grid zeigt vertical scrollbar

      ClientHeight := h ; 
      ClientWidth := w ; 
    end{ "if Sender <> MemoryGrid ... ELSE" } ;
  end{ "procedure TFormMemoryLoader.UpdateDisplay" } ; 


// nach Laden eines files ggf im ExecuteWindow die Startadrresse ändern
// nur EINMAL nach Laden, nicht jedesmal in UpdateDisplay()
procedure TFormMemoryLoader.UpdateExecuteAddress;
  begin 
    if curLoader.hasEntryAddress then begin 
      FormMain.FormExecute.StartPCEdit.Text := Addr2OctalStr(curLoader.EntryAddress) ;
      FormMain.FormExecute.StartPCEditChange(nil); 
    end; 
  end; 


procedure TFormMemoryLoader.VerifyAllButtonClick(Sender: TObject); 
  begin 
    MemoryGrid.Verify1Click(Sender); 
  end; 

procedure TFormMemoryLoader.ExamineAllButtonClick(Sender: TObject); 
  begin 
    MemoryGrid.ExamineAllButtonClick(Sender); 
  end; 

procedure TFormMemoryLoader.ExamineCurrentButtonClick(Sender: TObject); 
  begin 
    MemoryGrid.ExamineCurrentButtonClick(Sender); 
  end; 

procedure TFormMemoryLoader.LoaderFileFormatComboBoxChange(Sender: TObject); 
  begin 
    UpdateDisplay(self); 
    TheRegistry.Save(LoaderFileFormatComboBox) ; 
  end; 


procedure TFormMemoryLoader.BrowseFile1ButtonClick(Sender: TObject); 
  var 
    i: integer ; 
    loaderfile : TMemoryloaderFile ; 
  begin 
    // wird für alle Browsebuttons aufgerufen
    // VB: 'Loader" schon instanziiert.
    // finde den file, für den das browsen gilt
    // Sender als browsebutton eingetragen
    for i := 0 to curLoader.Files.Count - 1 do begin 
      loaderfile := curLoader.getFile(i) ; 
      if loaderfile.control_filenamebrowse = Sender then begin 
        OpenDialog1.Title := 'Select a ' + loaderfile.prompt ; 
        OpenDialog1.InitialDir := ExtractFilePath(loaderfile.filename) ; 
        if not DirectoryExists(OpenDialog1.InitialDir) then 
          OpenDialog1.InitialDir := FormMain.DefaultDataDirectory ; 

        if OpenDialog1.Execute then begin 
          loaderfile.filename := OpenDialog1.filename ; 
          TheRegistry.Save(loaderfile.regkey, loaderfile.filename) ; 
        end; 
      end; 
    end{ "for i" } ; 
    UpdateDisplay(self); 
  end{ "procedure TFormMemoryLoader.BrowseFile1ButtonClick" } ; 



procedure TFormMemoryLoader.LoadFileButtonClick(Sender: TObject); 
  var 
    startaddr: dword ; 
//    n: dword ; 
  begin 
    startaddr := OctalStr2Dword(StartAddrEdit.Text, 0) ; 
    if curLoader = nil then Exit ; 
    // grid-memory neu laden
    curLoader.Load(MemoryGrid.memorycellgroup, startaddr) ; 
//    if MemoryGrid.memorycellgroup.Count = 0 then
//      raise Exception.Create('No data read!');

    // neue mcg: neu mit grid verbinden
    MemoryGrid.ConnectToMemoryCellGroup(MemoryGrid.memorycellgroup) ; 
//    n := MemoryGrid.memorycellgroup.Cell(0).edit_value ;
    UpdateDisplay(self); 
    UpdateExecuteAddress ;
  end{ "procedure TFormMemoryLoader.LoadFileButtonClick" } ; 


procedure TFormMemoryLoader.EntryAddrEditKeyPress(Sender: TObject;
        var Key: Char);
  begin 
    // nur Octalziffern als Eingabe. auch das backspace #8 erlauben
    if not CharInSet(Key, [#8, '0'..'7']) then 
      Key := #0 ; 
  end; 

(*
procedure TFormMemoryLoader.SetStartAddrButtonClick(Sender: TObject);
  var
    startaddr: TMemoryAddress ;
  begin
    startaddr := OctalStr2Addr(StartAddrEdit.Text, MemoryGrid.memorycellgroup.mat) ;
    if startaddr.val <> MEMORYCELL_ILLEGALVAL then begin
      MemoryGrid.memorycellgroup.ExamineNewStartAddr(startaddr, {optimize=}true);
    end;

    // neue mcg: neu mit grid verbinden
    MemoryGrid.ConnectToMemoryCellGroup(MemoryGrid.memorycellgroup) ;
    // examine nur die zellen mit edit_value =ILLEGAL
    MemoryGrid.ExamineCells({unknown_only}true) ;
    UpdateDisplay(self) ;
  end{ "procedure TFormMemoryLoader.SetStartAddrButtonClick" } ;
 *)

(*
procedure TFormMemoryLoader.DecAddrButtonClick(Sender: TObject);
var startaddr: TMemoryAddress ;
  deltaaddr : dword ; // decrement = 1 Grid Zeile
begin
  deltaaddr := (MemoryGrid.MemoryCellsStringGrid.ColCount -1) * 2 ;
  // Adresse um eine Gridzeile verringern
  startaddr := OctalStr2Addr(StartAddrEdit.Text, MemoryGrid.memorycellgroup.mat) ;
  if startaddr.val <> MEMORYCELL_ILLEGALVAL then
    if startaddr.val < deltaaddr then
      startaddr.val := 0
    else
      startaddr.val := startaddr.val - deltaaddr ;
  StartAddrEdit.Text := Addr2OctalStr(startaddr) ;
  // Anzeige
  SetStartAddrButtonClick(Sender) ;
end{ "procedure TFormMemoryLoader.DecAddrButtonClick" } ;
*)
procedure TFormMemoryLoader.DepositAlllButtonClick(Sender: TObject); 
  begin 
    MemoryGrid.DepositAllButtonClick(Sender); 
  end; 

procedure TFormMemoryLoader.DepositChangedButtonClick(Sender: TObject); 
  begin 
    MemoryGrid.DepositChangedButtonClick(Sender); 
  end; 

(*
procedure TFormMemoryLoader.IncAddrButtonClick(Sender: TObject);
var startaddr: TMemoryAddress ;
  deltaaddr : dword ; // Increment = 1 Grid Zeile
begin
  deltaaddr := (MemoryGrid.MemoryCellsStringGrid.ColCount -1) * 2 ;
  // Adresse um eine Gridzeile verringern
  startaddr :=  OctalStr2Addr(StartAddrEdit.Text, MemoryGrid.memorycellgroup.mat) ;
  if startaddr.val <> MEMORYCELL_ILLEGALVAL then
    startaddr.val := startaddr.val + deltaaddr ;
  StartAddrEdit.Text := Addr2OctalStr(startaddr) ;
  // Anzeige
  SetStartAddrButtonClick(Sender) ;
end;
*)

end{ "unit FormMemoryLoaderU" } . 


