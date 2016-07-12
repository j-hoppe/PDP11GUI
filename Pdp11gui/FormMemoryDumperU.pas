unit FormMemoryDumperU;
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
  TFormMemoryDumper = class(TFormChild)
      PanelT: TPanel;
      ExamineAllButton: TButton;
      StartAddrLabel: TLabel;
      StartAddrEdit: TEdit;
      MemoryGrid: TFrameMemoryCellGroupGrid;
      DumpFileButton: TButton;
      Label2: TLabel;
      DumperFileFormatComboBox: TComboBox;
      File1Label: TLabel;
      File2Label: TLabel;
      Filename2Edit: TEdit;
      Filename1Edit: TEdit;
      BrowseFile1Button: TButton;
      BrowseFile2Button: TButton;
      Label1: TLabel;
      EndAddrEdit: TEdit;
      SaveDialog1: TSaveDialog;
      EntryAddrLabel: TLabel;
      EntryAddrEdit: TEdit;

      procedure AddrEditKeyPress(Sender: TObject; var Key: Char);
      //procedure SetStartAddrButtonClick(Sender: TObject);
      procedure ExamineCurrentButtonClick(Sender: TObject);
      procedure ExamineAllButtonClick(Sender: TObject);
      procedure DepositChangedButtonClick(Sender: TObject);
      procedure DepositAllButtonClick(Sender: TObject);
      procedure BrowseFile1ButtonClick(Sender: TObject);
      procedure DumperFileFormatComboBoxChange(Sender: TObject);
      procedure DumpFileButtonClick(Sender: TObject);
      procedure EntryAddrEditKeyPress(Sender: TObject; var Key: Char);
    private
      { Private-Deklarationen }
      // jeden möglichen Loader instanziieren
      Loader_ByteStreamLH: TMemoryLoader_BytestreamLH ;
      Loader_LowByteFileHighByteFile: TMemoryLoader_LowByteFileHighByteFile ;
      Loader_TextfileOneAddrPerLine: TMemoryLoader_TextfileOneAddrPerLine;
      Loader_BlinkenlightInstructions: TMemoryLoader_BlinkenlightInstructions;
      Loader_StandardAbsolutePapertape: TMemoryLoader_StandardAbsolutePapertape;
    public
      { Public-Deklarationen }
      curLoader: TMemoryLoader ; // wie durch Combobox ausgewählt
      constructor Create(AOwner: TComponent) ;
      destructor Destroy ; override ;

      procedure UpdateDisplay(Sender: TObject);
    end{ "TYPE TFormMemoryDumper = class(TFormChild)" } ;


implementation

{$R *.dfm}

uses
  AuxU,
  RegistryU,
  FormMainU;

constructor TFormMemoryDumper.Create(AOwner: TComponent) ;
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

    Loader_BlinkenlightInstructions := TMemoryLoader_BlinkenlightInstructions.Create ;
    with Loader_BlinkenlightInstructions.getFile(0) do begin
      control_filenameprompt := File1Label ;
      control_filenameedit := Filename1Edit ;
      control_filenamebrowse := BrowseFile1Button ;
    end;

    Loader_StandardAbsolutePapertape:= TMemoryLoader_StandardAbsolutePapertape.Create;
    with Loader_StandardAbsolutePapertape.getFile(0) do begin
      control_filenameprompt := File1Label ;
      control_filenameedit := Filename1Edit ;
      control_filenamebrowse := BrowseFile1Button ;
    end;

    MemoryGrid.OnUpdate := UpdateDisplay ; // wenn sich das grid ändert, muss diese Form reagieren
    TheRegistry.Load(DumperFileFormatComboBox) ;

  end{ "constructor TFormMemoryDumper.Create" } ;

destructor TFormMemoryDumper.Destroy ;
  begin
    inherited ;
  end;


// neue Oberfläche malen
procedure TFormMemoryDumper.UpdateDisplay(Sender: TObject);
  var
    dumperfile: TMemoryloaderFile ;
    mc: TMemoryCell ;
    h, w: integer ;
    i: integer ;
  begin
    if Sender <> MemoryGrid then // hat der Frame das Update ausgelöst?
      MemoryGrid.UpdateDisplay  // nein: update frame, er updated wieder die Form
    else begin
      // Editierte Memoryinhalte behalten, auch wenn Pdp durch callbacks neu abgefragt wird.
      MemoryGrid.memorycellgroup.PdpOverwritesEdit := false ; // statische initialisierung


      mc := MemoryGrid.memorycellgroup.Cell(0) ;
      StartAddrEdit.Text := Addr2OctalStr(mc.addr) ; // 1. Zelle = Startaddr
      Caption := setFormCaptionInfoField(Caption, Addr2OctalStr(mc.addr)) ;

      // ausgewählten Loader merken
      case DumperFileFormatComboBox.ItemIndex of
        0: curLoader := Loader_ByteStreamLH ;
        1: curLoader := Loader_LowByteFileHighByteFile ;
        2: curLoader := Loader_TextfileOneAddrPerLine ;
        3: curLoader := Loader_BlinkenlightInstructions ;
        4: curLoader := Loader_StandardAbsolutePapertape ;
        else curLoader := nil ;
      end;


      if curLoader = nil then Exit ; // nur bei erstem Start nach Installation

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

      // definiert das Format eine Einsprungaddresse?
      if curLoader.hasEntryAddress then begin
        EntryAddrLabel.Visible := true ;
        EntryAddrEdit.Visible := true ;
//        EntryAddrEdit.Text := Addr2OctalStr(curLoader.EntryAddress) ;
        //EntryAddrEdit.ReadOnly := false ;
        //EntryAddrEdit.Font.Color := clBlack ;
      end;

      // b) controls für den aktiven Loader visible ;
      for i := 0 to curLoader.Files.Count - 1 do begin
        dumperfile := curLoader.getFile(i) ;
        dumperfile.control_filenameprompt.Caption := dumperfile.prompt + ' :' ;
        dumperfile.control_filenameprompt.Visible := true ;
        dumperfile.control_filenameedit.Text := dumperfile.filename ;
        dumperfile.control_filenameedit.Visible := true ;
        dumperfile.control_filenamebrowse.Visible := true ;
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
  end{ "procedure TFormMemoryDumper.UpdateDisplay" } ;


procedure TFormMemoryDumper.EntryAddrEditKeyPress(Sender: TObject;
        var Key: Char);
  begin
    // nur Octalziffern als Eingabe. auch das backspace #8 erlauben
    if not CharInSet(Key, [#8, '0'..'7']) then
      Key := #0 ;
  end;


procedure TFormMemoryDumper.ExamineAllButtonClick(Sender: TObject);
  var startaddr, endaddr: dword ;
    wordcount: integer ;
  begin
    // MemoryCellgroups erzeugen.
    startaddr := OctalStr2Dword(StartAddrEdit.Text, 0) ;
    endaddr := OctalStr2Dword(EndAddrEdit.Text, 0) ;
    wordcount := endaddr div 2 - startaddr div 2 + 1 ; // wordcount
    if wordcount < 1 then wordcount := 1 ; // silent stability

    MemoryGrid.memorycellgroup.Clear ;
    MemoryGrid.memorycellgroup.Add(startaddr, wordcount) ;

    // neue mcg: neu mit grid verbinden
    MemoryGrid.ConnectToMemoryCellGroup(MemoryGrid.memorycellgroup) ;
    MemoryGrid.ExamineAllButtonClick(Sender);

//    n := MemoryGrid.memorycellgroup.Cell(0).edit_value ;
    UpdateDisplay(self);
  end{ "procedure TFormMemoryDumper.ExamineAllButtonClick" } ;

procedure TFormMemoryDumper.ExamineCurrentButtonClick(Sender: TObject);
  begin
    MemoryGrid.ExamineCurrentButtonClick(Sender);
  end;


procedure TFormMemoryDumper.DumperFileFormatComboBoxChange(Sender: TObject);
  begin
    UpdateDisplay(self);
    TheRegistry.Save(DumperFileFormatComboBox) ;
  end;


procedure TFormMemoryDumper.BrowseFile1ButtonClick(Sender: TObject);
  var
    i: integer ;
    dumperfile : TMemoryloaderFile ;
  begin
    // wird für alle Browsebuttons aufgerufen
    // VB: 'Loader" schon instanziiert.
    // finde den file, für den das browsen gilt
    // Sender als browsebutton eingetragen
    for i := 0 to curLoader.Files.Count - 1 do begin
      dumperfile := curLoader.getFile(i) ;
      if dumperfile.control_filenamebrowse = Sender then begin
        SaveDialog1.Title := 'Select a ' + dumperfile.prompt ;
        SaveDialog1.InitialDir := ExtractFilePath(dumperfile.filename) ;
        if not DirectoryExists(SaveDialog1.InitialDir) then
          SaveDialog1.InitialDir := FormMain.DefaultDataDirectory ;

        if SaveDialog1.Execute then begin
          dumperfile.filename := SaveDialog1.filename ;
          TheRegistry.Save(dumperfile.regkey, dumperfile.filename) ;
        end;
      end;
    end{ "for i" } ;
    UpdateDisplay(self);
  end{ "procedure TFormMemoryDumper.BrowseFile1ButtonClick" } ;



procedure TFormMemoryDumper.DumpFileButtonClick(Sender: TObject);
  var entryaddr: dword ;
  begin
    if curLoader = nil then Exit ;
    // grid-memory neu laden
    if curLoader.hasEntryAddress then begin
      if Trim(EntryAddrEdit.Text) <> '' then
        curLoader.EntryAddress.val := OctalStr2Dword(EntryAddrEdit.Text)
      else
        curLoader.EntryAddress.val :=   OctalStr2Dword(StartAddrEdit.Text) ;
      curLoader.Save(MemoryGrid.memorycellgroup) ;
    end else
      curLoader.Save(MemoryGrid.memorycellgroup) ;
    // if Textfile: ask, wether to open in notepad
    if curLoader.isText
            and (MessageDlg('Show text file in notepad?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      OpenTextFileInNotepad(curLoader.getFile(0).filename) ;

    UpdateDisplay(self);
  end{ "procedure TFormMemoryDumper.DumpFileButtonClick" } ;


procedure TFormMemoryDumper.AddrEditKeyPress(Sender: TObject;
        var Key: Char);
  begin
    // nur Octalziffern als Eingabe. auch das backspace #8 erlauben
    if not CharInSet(Key, [#8, '0'..'7']) then
      Key := #0 ;
  end;

(*
procedure TFormMemoryDumper.SetStartAddrButtonClick(Sender: TObject);
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
  end{ "procedure TFormMemoryDumper.SetStartAddrButtonClick" } ;
*)

procedure TFormMemoryDumper.DepositAllButtonClick(Sender: TObject);
  begin
    MemoryGrid.DepositAllButtonClick(Sender);
  end;

procedure TFormMemoryDumper.DepositChangedButtonClick(Sender: TObject);
  begin
    MemoryGrid.DepositChangedButtonClick(Sender);
  end;


end{ "unit FormMemoryDumperU" } .


