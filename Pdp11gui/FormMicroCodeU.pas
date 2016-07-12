unit FormMicroCodeU;
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

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  FormChildU,
  JvExGrids, JvStringGrid,
  JH_Utilities,
  PDP1144MicroCodeU, Grids;

type
  TFormMicroCode = class(TFormChild)
      Panel1: TPanel;
      MicroInstructionStringGrid: TJvStringGrid;
      MicroCodeSearchComboBox: TComboBox;
      Label1: TLabel;
      NextMicroInstructionButton: TButton;
      Label2: TLabel;
      MicroCodeSearchModeComboBox: TComboBox;
      MicroCodeLoadButton: TButton;
      OpenDialog1: TOpenDialog;
      procedure MicroCodeSearchComboBoxChange(Sender: TObject);
      procedure FormResize(Sender: TObject);
      procedure MicroInstructionStringGridDrawCell(Sender: TObject; aCol,
              aRow: integer; Rect: TRect; State: TGridDrawState);
      procedure NextMicroInstructionButtonClick(Sender: TObject);
      procedure MicroInstructionStringGridMouseUp(Sender: TObject;
              Button: TMouseButton; Shift: TShiftState; X, Y: integer);
      procedure MicroCodeSearchModeComboBoxChange(Sender: TObject);
      procedure MicroCodeLoadButtonClick(Sender: TObject);
      procedure FormShow(Sender: TObject);
    private
      { Private-Deklarationen }
      searchMode: integer ; // 0 = addr, 1 = tag
      inMicroInstructionAddressComboBoxChange: boolean ;
      procedure setSearchMode(mode:integer; force:boolean = false) ;
    public
      { Public-Deklarationen }
      Loaded: boolean ; // micro code aus listing files geladen?
      CurMicroInstruction: TPDP1144MicroInstruction ; // aktuell angezeigter Schritt
      Pdp1144MicroCode : TPdp1144MicroCode ;
      constructor Create(AOwner: TComponent); override ;
      destructor Destroy ; override ;
      procedure UpdateDisplay ;
      procedure Load(filepattern:string) ;
      procedure setMicroPc(newpc: dword) ;
    end{ "TYPE TFormMicroCode = class(TFormChild)" } ;



implementation

{$R *.dfm}

uses
  AuxU,
  RegistryU,
  FormMainU ;

const
  MicroCodeDefaultFilename = 'EY-C3012-RB-001 PDP-1144 Processor Maintenance Supplementary Listings (microcode) Apr-81.txt' ;


constructor TFormMicroCode.Create(AOwner: TComponent);
  var s: string ;
  begin
    inherited ;
    Loaded := false ;
    CurMicroInstruction := nil ;
    Pdp1144MicroCode := TPdp1144MicroCode.Create ;
    MicroCodeSearchComboBox.Clear ;
    inMicroInstructionAddressComboBoxChange := false ;
    TheRegistry.Load(MicroCodeSearchModeComboBox) ;
    setSearchMode(MicroCodeSearchModeComboBox.ItemIndex) ;
    // load standard microcode file
    s := TheRegistry.Load('MicroCodeListingFilePattern', FormMain.DefaultDataDirectory + '\' + MicroCodeDefaultFilename);
    Log('Loading micro code file "%s"', [s]) ;
    if s <> '' then
      try
        Load(s) ;
      except on e:Exception do  // ignoriere alle Fehler, aber logge sie
          Log(e.Message) ;
      end;
  end{ "constructor TFormMicroCode.Create" } ;


destructor TFormMicroCode.Destroy ;
  begin
    Pdp1144MicroCode.Free ;
    inherited ;
  end;


procedure TFormMicroCode.FormShow(Sender: TObject);
  begin
    UpdateDisplay ;
  end;


procedure TFormMicroCode.MicroCodeLoadButtonClick(Sender: TObject);
  var curpc: dword ;
    fpath, fname, fext: string ;
  begin
    OpenDialog1.InitialDir := ExtractFilePath(Pdp1144MicroCode.listingfilenamepattern) ;
    if OpenDialog1.Execute then begin
      // Zahlen hinten am Dateinamen durch Wildcard ersetzen
      fpath := ExtractFilePath(OpenDialog1.Filename) ;
      fname := ExtractFileName(OpenDialog1.Filename) ;
      fext := ExtractFileExt(OpenDialog1.Filename) ;
      fname := ChangeFileExt(fname, '') ;
      while (fname <> '') and CharInSet(fname[length(fname)], ['0'..'9']) do
        fname := Copy(fname, 1, length(fname)-1) ;
      fname := fpath + '\' + fname + '*' + fext ;
      Load(fname) ;
    end;
    // Files (neu) einlesen, aber position erhalten
    if CurMicroInstruction <> nil then
      curpc := CurMicroInstruction.addr
    else curpc := 0 ;
    Load(Pdp1144MicroCode.listingfilenamepattern) ;

    setMicroPc(curpc) ;
//    MicroCodeSearchComboBoxChange(nil);
  end{ "procedure TFormMicroCode.MicroCodeLoadButtonClick" } ;


// Spaltenbreiten sichern
procedure TFormMicroCode.FormResize(Sender: TObject);
  var i, n: integer ;
  begin
    n := 0 ;
    with MicroInstructionStringGrid do begin
      for i := 0 to 1 do
        n := n + 1 + ColWidths[i] ;
      ColWidths[2] := Clientwidth- n ;
    end;
  end ;


procedure TFormMicroCode.UpdateDisplay ;

function textextent(s:string): TSize ;
begin
MicroInstructionStringGrid.Canvas.Font.Assign(MicroInstructionStringGrid.Font) ;
result := MicroInstructionStringGrid.Canvas.TextExtent(s) ;
end;

  var
    i, n: integer ;
    r: integer ;
    s: string ;
  begin
    if Loaded then begin
      MicroCodeSearchModeComboBox.Enabled := true ;
      MicroCodeSearchComboBox.Enabled := true ;
      NextMicroInstructionButton.Enabled := true ;
    end else begin
      MicroCodeSearchModeComboBox.Enabled := false ;
      MicroCodeSearchComboBox.Enabled := false ;
      NextMicroInstructionButton.Enabled := false ;
    end ;

    // Zeilen ausfüllen
    if not Loaded or (CurMicroInstruction = nil) then Exit ; // es gibt noch nix anzuzeigen

    // ComboBox update
    if not inMicroInstructionAddressComboBoxChange then
      case searchMode of
        0:  MicroCodeSearchComboBox.Text := Dword2Octalstr(CurMicroInstruction.addr, 12) ;
        1:  MicroCodeSearchComboBox.Text := CurMicroInstruction.symbolictag ;
        2:  MicroCodeSearchComboBox.Text := IntToStr(CurMicroInstruction.linenr) ;
      end;

    with MicroInstructionStringGrid do begin
  DefaultRowHeight := textextent('Xg').Height ;
      ColCount := 3 ; // name, bits, value
      // title + tag + addr + alle felder + opcodes, file path, filename, linenr
      RowCount := 1 + 2 + PDP1144MicroInstructionFields_Count + 5 ;
      FixedRows := 1 ;
      FixedCols := 0 ;
      Cells[0, 0] := ' Field' ;
      Cells[1, 0] := ' Bits' ;
      Cells[2, 0] := ' Info' ;
      ColWidths[0] := textextent('X ABCDEFGH').Width; // 100 ; // override by registry settings
      ColWidths[1] := textextent('X 100:999').Width ;
      ColWidths[2] := textextent('X 12 = irgendwas').Width ;

      FormGridLoadColWidths(self.name, MicroInstructionStringGrid) ; //

      n := 0 ; // die letzte spalte auf jeden Fall bis an den Gridrand
      for i := 0 to 1 do
        n := n + 1 + ColWidths[i] ;
      ColWidths[2] := Clientwidth - n ;


      r := 1 ;
      Cells[0, r] := ' Symbolic tag' ;
      Cells[2, r] := ' ' + CurMicroInstruction.symbolictag ;
      inc(r) ;
      Cells[0, r] := ' Address' ;
      Cells[2, r] := ' ' + Dword2Octalstr(CurMicroInstruction.addr, 12) ;
      inc(r) ;
      for i := 0 to PDP1144MicroInstructionFields_Count-1 do begin
        Cells[0, r] := ' ' + CurMicroInstruction.fields[i].name ;
        with PDP1144MicroInstructionFieldDefs[i] do
          if len = 1 then
            Cells[1, r] := ' ' + IntToStr(lsb)
          else Cells[1, r] := ' ' + Format('%d:%d', [lsb+len-1, lsb]) ;
        // Anzeige: num oder num = <text>
        s := Dword2Octalstr(CurMicroInstruction.fields[i].value,
                PDP1144MicroInstructionFieldDefs[i].len) ;

        if (CurMicroInstruction.fields[i].Text <> '?')
                and (CurMicroInstruction.fields[i].Text <> '') then
          s := s + ' = ' + CurMicroInstruction.fields[i].Text ;
        Cells[2, r] := ' ' + s ;
        inc(r) ;
      end{ "for i" } ;
      s := '' ;
      for i := 0 to CurMicroInstruction.opcodes.Count-1 do begin
        if s <> '' then s := s + '  |  ' ;
        s := s + CurMicroInstruction.opcodes[i] ;
      end;
      Cells[0, r] := ' Source code' ;
      Cells[2, r] := ' ' + s ;
      inc(r) ;
      Cells[0, r] := ' Listing file path' ;
      Cells[2, r] := ' ' + ExtractFilePath(CurMicroInstruction.Filename) ;
      inc(r) ;
      Cells[0, r] := ' Listing file name' ;
      Cells[2, r] := ' ' + ExtractFileName(CurMicroInstruction.Filename) ;
      inc(r) ;
      Cells[0, r] := ' Listing line#' ;
      Cells[2, r] := ' ' + IntToStr(CurMicroInstruction.linenr) ;
      // inc(r) ;

    end { "with MicroInstructionStringGrid" } ;

    if Loaded then
      Caption := setFormCaptionInfoField(Caption, 'µPC = ' + Dword2Octalstr(CurMicroInstruction.addr, 12))
    else
      Caption := setFormCaptionInfoField(Caption, 'code not loaded') ;
  end{ "procedure TFormMicroCode.UpdateDisplay" } ;


procedure TFormMicroCode.Load(filepattern:string) ;
  begin
    CurMicroInstruction := nil ; // ist jetzt ungültig
    Loaded := false ;
    Pdp1144MicroCode.LoadListingPages(filepattern);
    // Code geladen: Combobox neu aufbauen. Force change!
    setSearchMode(MicroCodeSearchModeComboBox.ItemIndex, true) ;

    setMicroPc(0);
    Loaded := true ; // keine Exception
    TheRegistry.Save('MicroCodeListingFilePattern', Pdp1144MicroCode.listingfilenamepattern);
  end;


procedure TFormMicroCode.setMicroPc(newpc: dword) ;
  var microInst: TPDP1144MicroInstruction ;
  begin
    microInst := Pdp1144MicroCode.InstructionByAddr(newpc) ;
    if microInst = nil then Exit ; // illegal address: do nothing
    CurMicroInstruction := microInst ;
    UpdateDisplay ;
  end;

// suche nach addresse (0) oder tag(1)
// optimerung kann mit "force" abgeschaltet werden
procedure TFormMicroCode.setSearchMode(mode:integer; force: boolean) ;
  var i: integer ;
  begin
    if not force and (mode = searchMode) then Exit ; // nothing to do
    MicroCodeSearchComboBox.Clear ;
    case mode of
      0: begin
        Pdp1144MicroCode.Sort(TPDP1144MicroInstruction_ListSortCompare_Addr);
        for i := 0 to Pdp1144MicroCode.Count-1 do
          MicroCodeSearchComboBox.Items.Add(Dword2Octalstr(Pdp1144MicroCode.Instruction(i).addr, 12)) ;
      end;
      1: begin
        Pdp1144MicroCode.Sort(TPDP1144MicroInstruction_ListSortCompare_Tag);
        for i := 0 to Pdp1144MicroCode.Count-1 do
          MicroCodeSearchComboBox.Items.Add(Pdp1144MicroCode.Instruction(i).symbolictag) ;
      end;
      2: begin
        Pdp1144MicroCode.Sort(TPDP1144MicroInstruction_ListSortCompare_LineNr);
        for i := 0 to Pdp1144MicroCode.Count-1 do
          MicroCodeSearchComboBox.Items.Add(IntToStr(Pdp1144MicroCode.Instruction(i).linenr)) ;
      end;
    end{ "case mode" } ;
    searchMode := mode ;
    UpdateDisplay ; // Combobox.Text auf aktuelle Istruction setzen
  end{ "procedure TFormMicroCode.setSearchMode" } ;

procedure TFormMicroCode.MicroCodeSearchModeComboBoxChange(Sender: TObject);
  begin
    setSearchMode(MicroCodeSearchModeComboBox.ItemIndex) ;
    TheRegistry.Save(MicroCodeSearchModeComboBox) ;
  end;


procedure TFormMicroCode.MicroCodeSearchComboBoxChange(Sender: TObject);
  var microInst: TPDP1144MicroInstruction ;
    newpc: dword ;
  begin
    // verhindert Update der ComboBox
    inMicroInstructionAddressComboBoxChange := true ;
    case searchMode of
      0:
        newpc := OctalStr2Dword(MicroCodeSearchComboBox.Text,0) ;
      1: begin
        microInst := Pdp1144MicroCode.InstructionByTag(MicroCodeSearchComboBox.Text) ;
        if microInst = nil then Exit ; // illegal tag: do nothing
        newpc := microInst.addr ;
      end;
      2: begin
        microInst := Pdp1144MicroCode.InstructionByLineNr(StrToInt(MicroCodeSearchComboBox.Text)) ;
        if microInst = nil then Exit ; // illegal tag: do nothing
        newpc := microInst.addr ;
      end;
    end{ "case searchMode" } ;
    setMicroPc(newpc);
    inMicroInstructionAddressComboBoxChange := false ;
  end{ "procedure TFormMicroCode.MicroCodeSearchComboBoxChange" } ;


// Werte, die nicht default sind, gelb malen
procedure TFormMicroCode.MicroInstructionStringGridDrawCell(Sender: TObject;
        aCol, aRow: integer; Rect: TRect; State: TGridDrawState);
  var
    s: string ;
    newcolor: boolean  ;
    fieldidx: integer ;
    //bitfield: TPDP1144MicroInstructionField ;
    bitfield_changed, specialfield: boolean ;
  begin
    if CurMicroInstruction = nil then Exit ;

    with MicroInstructionStringGrid do begin
      s := Cells[aCol, aRow];
      newcolor := false ;

      // ab zeile 2 die Feldwerte der MicroInstruction
      fieldidx := aRow-3 ;
      bitfield_changed := (aCol = 2)
              and (fieldidx >= 0)
              and (fieldidx < PDP1144MicroInstructionFields_Count)
              and (CurMicroInstruction.fields[fieldidx].value <> PDP1144MicroInstructionFieldDefs[fieldidx].def) ;

      specialfield := (aCol = 2)
              and (Cells[0,aRow] = ' Source code') ;

      if bitfield_changed or specialfield then begin
        newcolor := true ; // geänderte Felder mit gelbem Hintergrund
        Canvas.Brush.Color := ColorGridCellChangedBkGnd ;
        Canvas.Font.Color := ColorGridCellChangedText ;
      end;
      if newcolor then begin
        Canvas.FillRect(Rect);
        DrawText(Canvas.Handle, PChar(s), length(s), Rect, DT_LEFT);
      end;
    end{ "with MicroInstructionStringGrid" } ;
  end{ "procedure TFormMicroCode.MicroInstructionStringGridDrawCell" } ;


// Geänderte Spaltenbreiten sichern
procedure TFormMicroCode.MicroInstructionStringGridMouseUp(Sender: TObject;
        Button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    FormGridSaveColWidths(self.name, MicroInstructionStringGrid) ;
  end;


procedure TFormMicroCode.NextMicroInstructionButtonClick(Sender: TObject);
  begin
    setMicroPc(CurMicroInstruction.nextaddr);
  end;

end{ "unit FormMicroCodeU" } .
