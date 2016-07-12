unit FormMacro11ListingU;
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
  Zeigt den output einer MACRO-11 Übersetzung an,
  analysiert ihn und erzeugt eine MemoryCellgroup für den Code.

  Findet auch MACRO-11 Fehler-meldungen,
  sowie ungebundene globale Symbole (Postfix 'G' und ')


  Es besteht folgende Relation zwischen Source, Listing und Code:
  Sourcezeile 1:n ListingZeile 1:n memorycell

  "memorycell.listinglinenr" ist der foreign key auf listing
  "formlisting.sourcelinenr[i]" enthält die Nr der Sourcezeile,
  die listingzeile i fabriziert hat

  Der aktuelle PC kann im Gutter mit eienr Marke versehen werden.

  Adressen sind virtual
}
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  FormChildU,
  Dialogs, StdCtrls, ExtCtrls, JvExControls, JvEditorCommon, JvEditor,
  JH_Utilities,
  AddressU,
  MemoryCellU;

type
  TFormMacro11Listing = class(TFormChild)
      Editor: TJvEditor;
      PanelT: TPanel;
      LoadButton: TButton;
      OpenDialog1: TOpenDialog;
      ShowCodeMemFormButton: TButton;
      DepositAllButton: TButton;
      procedure LoadButtonClick(Sender: TObject);
      procedure ShowCodeMemFormButtonClick(Sender: TObject);
      procedure DepositAllButtonClick(Sender: TObject);
      procedure EditorPaintGutter(Sender: TObject; Canvas: TCanvas);
      procedure EditorResize(Sender: TObject);
    private
      { Private-Deklarationen }

      CodeParsed: boolean ; // keine doppelten Läufe von "ParseCode()"

      procedure FormAfterShow(Sender: TObject);
      procedure FormBeforeHide(Sender: TObject);

    public
      { Public-Deklarationen }
      ListingFilename: string ;
      memorycellgroup: TMemoryCellGroup ; // geparstes Listing
      sourcelinenr: array of integer ;  // Listing->Source, Index: ListingZeilennumer, first = 1

      DepositSuccess: boolean ; // true, wenn erfolgriech in memory geschrieben

      FirstErrorMsg : string ; // Fehlermeldung bei Übersetzung. '' = kein Fehler
      FirstErrorFilename : string ; // Fehlerfile bei Übersetzung
      FirstErrorLineNr : integer ; // Zeile der Fehlermeldung in der Source

      PCMarkerRow: integer ; // Zeilennumer mit dem PC

      constructor Create(aOwner: TComponent) ; override ;
      destructor Destroy ; override ;

      procedure LoadFile(fname: string) ;
      procedure ParseCode;
      procedure Deposit ;

      procedure setPCMark(addr: TMemoryAddress);
      procedure setErrorMark(aSourcelinenr: integer);

      procedure ShowCodeForm;

    end{ "TYPE TFormMacro11Listing = class(TFormChild)" } ;


implementation

{$R *.dfm}
uses
  AuxU,
  RegistryU,
  FormMainU ; // wg. FormMacro11Code

constructor TFormMacro11Listing.Create(aOwner: TComponent) ;
  begin
    inherited Create(aOwner) ;

    // private events
    // die MDI-Show/Hide logik in TFormChild verursacht Windowsgehler,
    // wenn JVEditor eine lange Source geladen hat.
    OnAfterShow := FormAfterShow ; // lädt den letzten File
    OnBeforeHide := FormBeforeHide ;

    // Farbe für Ausführungsposition
    Editor.LineInformations.DebugPointColor := ColorCodeExecutionPositionBkGnd ;
    Editor.LineInformations.DebugPointTextColor:= ColorCodeExecutionPositionText ;

    Editor.LineInformations.ErrorPointColor:= ColorCodeErrorBkGnd ;
    Editor.LineInformations.ErrorPointTextColor:= ColorCodeErrorText ;

    // muss den anderen memorygroups beigeordnet sein
    memorycellgroup := TMemoryCellGroup.Create(FormMain.MemoryCellGroups) ;
    memorycellgroup.mat := matVirtual ;

    DepositSuccess := false ;
    CodeParsed := false ;

  end{ "constructor TFormMacro11Listing.Create" } ;


destructor TFormMacro11Listing.Destroy ;
  begin
    memorycellgroup.Free ;
    inherited ;
  end;

// prorammcode laden
procedure TFormMacro11Listing.Deposit ;
  begin
    DepositSuccess := false ;
    ParseCode ;
    memorycellgroup.Deposit({optimize}false, true) ; // long list
//    UpdateDisplay ;
    DepositSuccess := true ;
  end;


procedure TFormMacro11Listing.DepositAllButtonClick(Sender: TObject);
  begin
    Deposit ;
  end;

// Gutter malen:
// die Zeile mit der Asuführungsporition mitenem Roten Pfel markieren
procedure TFormMacro11Listing.EditorPaintGutter(Sender: TObject;
        Canvas: TCanvas);
  var
    i: integer;
    Rect: TRect;
    oldFont: TFont;
  begin
    oldFont := TFont.Create;
    try
      oldFont.Assign(Canvas.Font);
      Canvas.Font := oldFont ; // GutterFont.Font;
      Canvas.Font.Color := ColorCodeExecutionPositionText ;
      with Editor do
        for i := TopRow to TopRow + VisibleRowCount do
          if PCMarkerRow = i then begin
            Rect := Bounds(2, (i - TopRow) * CellRect.Height, GutterWidth - 2 - 5, CellRect.Height);
            // Zelennr =PChar(IntToStr(i + 1))
            DrawText(Canvas.Handle, 'PC>' , -1, Rect, DT_RIGHT or DT_VCENTER or DT_SINGLELINE);
          end;
    finally
      Canvas.Font := oldFont;
      oldFont.Free;
    end{ "try" } ;
  end{ "procedure TFormMacro11Listing.EditorPaintGutter" } ;


procedure TFormMacro11Listing.EditorResize(Sender: TObject);
  begin
    Invalidate ; // zeichnet sich sonst nicht richtig neu
  end ;

procedure TFormMacro11Listing.FormBeforeHide(Sender: TObject);
  begin
    // source aus editor löschen, sonst
    // gibt es einen Fehler beim Load, wenn formstyle auf fsMDICild geht
    Editor.Lines.Clear ;
  end;

procedure TFormMacro11Listing.FormAfterShow(Sender: TObject);
  begin
    // letzten File automatisch laden
    ListingFilename := TheRegistry.Load('ListingFilename', '') ;
    if ListingFilename <> '' then begin
      LoadFile(ListingFilename);
    end;
  end;


procedure TFormMacro11Listing.LoadFile(fname: string) ;
  var
    tmpLines: TStringList ;
    i: integer ;
  begin
    CodeParsed := false ; // letztes Parsing wird ungültig
    PCMarkerRow := -1 ;
    FirstErrorFilename := '' ;
    FirstErrorMsg := '' ;
    FirstErrorLineNr := -1 ;

    Caption := setFormCaptionInfoField(Caption, '') ;
    if not FileExists(fname) then
      Exit ;

    tmpLines:= TStringList.Create ;
    try
      memorycellgroup.Clear ;
      Editor.Lines.Clear ;
      Editor.LineInformations.Clear ;
      // diese bescheuerte MDI-Child-visiblity-Problematik:
      // bei geladenen (grossem) File kann der Übergang visible->invisible nicht stattfinden
      // LoadFromFile darf nur ausgeführt werden, wenn "Visible = true'
      // Wenn invisible: das Laden wird dann verzögert im OnAfterShow() durchgeführt.
      if visible then begin
        Editor.BeginUpdate ;
        Log('LoadFromFile(%s)', [fname]) ;
        // do not use "Editor.Lines.LoadFromFile(fname, TEncoding.ASCII)"
        // JVeditor is total UNICODE and loads no ASCII
        try
          tmpLines.LoadFromFile(fname); // load as ASCII
        except on E: Exception do
            raise Exception.CreateFmt('Error in TFormMacro11Listing.Loadfile(): can not read file %s', [fname]);
        end ;
        // laden mit detab: TJvEditor does not display Tabs?
        for i := 0 to tmpLines.Count - 1 do
          tmpLines[i] := detab(tmpLines[i], 8) ;

        Editor.Lines.Assign(tmpLines); // tmplines is now Unicode with correct converted ASCII
        Editor.EndUpdate ;
      end { "if visible" } ;
      Caption := setFormCaptionInfoField(Caption, fname) ;

      // Filename merken, auch für OnAfterShow
      TheRegistry.Save('ListingFilename', fname);
    finally
      tmpLines.Free ;
    end{ "try" } ;

  end{ "procedure TFormMacro11Listing.LoadFile" } ;


{
  die address und Wertefelder parsen und
  in memorycellgroup speichern

  Das Listing sieht so aus
      37 000006                         ddxbuf        =+6                                ; transmit data
    38
    39 165564                         diags        =165564                         ; console diags phase2 entry
    40
    41                                        .asect
    42 173000                                 .=173000
    43
    44                                        ; --------------------------------------------------
    45
    46 173000    104     104          start:        .ascii        "DD"                        ; device code (reversed)
    47
    48 173002 000176                          .word        last-.                        ; offset to next boot header
    49
    50 173004 000261                  dd0n:        sec                                ; boot std csr, unit zero, no diags
    51
    52                                aproc:
D:\pdp11\pdp 11-44\progs\memoryaddress.mac:11: ***ERROR Illegal addressing mode
    53 000740 000200                          rts
    54 000742 000000                          halt

}
procedure TFormMacro11Listing.ParseCode;


// einen byte oder word-Value in die aktuelel/näcshte
// adresse füllen. addr inc
// result: false, wenn format-fehler

// an manchen Values hängen hinten noch das Suffixe
//  '
//  G: Ungebundenes Global (sehr häufig!), Zeichen für Tippfehler,
//      oder unvollständige Source
//  C: ?
// manche values sind 8 bit = 3 zeichen lang.
// dann kombiniere ZWEI zu einer memorycell
{
     114 173200    110     145     154  tst0:  .ascii  "Hello"                    ; ungerade, bytes
         173203    154     157
     115 173205    054     040          tst1:  .ascii  ", "
}
// Fehlermeldungen in 'LastCompileErrorMsg' und 'LastCompileErrorLine' ablegen


// von einem octalword '123G' hinten die non-octal-Ziffern abtrennen
  procedure split_octalsuffix(s_in:string; var octal_out, suffix_out:string) ;
    var
      i: integer ;
    begin
      i := 1 ;
      octal_out := '' ;
      suffix_out := '' ;
      while (i <= length(s_in)) and CharInSet(s_in[i], ['0'..'7']) do  begin
        octal_out := octal_out + s_in[i] ;
        inc(i) ;
      end;

      while i <= length(s_in) do begin
        suffix_out := suffix_out + s_in[i] ;
        inc(i) ;
      end;
    end { "procedure split_octalsuffix" } ;

// octal wert in den memory buffer
  procedure FillVal2MemoryCell(var addr:TMemoryAddress; s_value:string; linenr: integer) ;
//  function FillVal2MemoryCell(var addr:dword; s_value:string; linenr: integer ): boolean  ;
    var
      mc: tmemorycell ;
      idx: integer ;
      value_byte_count: integer ; // aus wieviel byte besteht ein value?
      value: dword ;
      tmpaddr: TMemoryAddress ;
    begin
      // steht da ein byte oder ein word?
      if length(s_value) <= 3 then
        value_byte_count := 1 // byte
      else value_byte_count := 2 ; // word

      value := OctalStr2Dword(s_value, 16) ;

      // wenn byte, und ungerade adresse: wert als msb der
      // vorhergehenden geraden adresse definieren.
      // a) Memorycell finden/erzeugen
      mc := nil ;
      case value_byte_count of
        1: if odd(addr.val) then begin
            tmpaddr := addr ;
            tmpaddr.val := addr.val-1 ;
            idx := memorycellgroup.CellIndexByAddr(tmpaddr) ;
            if idx >= 0 then // nimm vorherige gerade adresse
              mc := memorycellgroup.Cell(idx) ;
          end;
      end;
      if mc = nil then
        mc := memorycellgroup.Add(addr.val and $fffffffe) ; // neue Cell immer mit gerader adresse            // weiteren 16 bit wert in der Zeile gefunden
      // b) wert in memorycell füllen
      case value_byte_count of
        1: if not odd(addr.val) then
            mc.edit_value := value
          else mc.edit_value := mc.edit_value or (value shl 8) ;
        2: mc.edit_value := value ;
      end;
      mc.listinglinenr := linenr ; // Verweis Code->listing
      mc.pdp_value := MEMORYCELL_ILLEGALVAL ;
//      mc.edit_value := mc.pdp_value ; // damit man es auch sieht
      addr.val := addr.val + value_byte_count ; // auto inc adress for next value in line
//      end{ "if result" } ;
    end{ "procedure FillVal2MemoryCell" } ;

  var
    i, j: integer ;
    line, line1: string;
    cursourcelinenr: integer ;
    s_lineno: string ;
    s_addr: string ;
    s_value, suffix: string ;
    addr: TMemoryAddress ;
    code_field_ready: boolean ;
  begin { "procedure TFormMacro11Listing.ParseCode" }
    if CodeParsed then
      Exit ; // noting todo

    memorycellgroup.Clear ;
    SetLength(sourcelinenr, Editor.Lines.Count+1) ;
    cursourcelinenr:= 0 ;
    FirstErrorFilename := '' ;
    FirstErrorMsg := '' ;
    FirstErrorLineNr := -1 ;

    for i := 0 to Editor.Lines.Count - 1 do begin
      line := Editor.Lines[i];

      // Fehlerzeile?
      if (length(line) > 10) and (line[1] <> ' ') then begin
        // schwierig: der Filename kann auch einen ':' enthalten
// D:\pdp11\pdp 11-44\progs\memoryaddress.mac:11: ***ERROR Illegal addressing mode
        // den ':' vom Laufwerksbuchstebn des Filename  garnaetiert wegkriegen,
        // der filename interessiert auch nicht
        if line[2] = ':' then begin
          j := 2 + pos(':', Copy(line, 3, maxint)) ; // hinter dem drive ':' suchen
          if j = 2 then j := 0 ;
        end else
          j := pos(':', line) ;
        if (FirstErrorMsg = '') and (j > 0) then begin
          // ":" gefunden: Annahme, es ist eine Fehlerzeile
          // nur parsen, wenn es die erste Fehlerzeile ist
          FirstErrorFilename := Copy(line, 1, j-1) ;
          line := Copy(line, j+1, maxint) ;
          j := pos(':', line) ;
          if j > 0 then begin // zweites ':' gefunden
            try
              FirstErrorLineNr := StrToInt(Copy(line, 1, j-1)) ;
              FirstErrorMsg := Copy(line, j+1, maxint) ; // marker: fehler da!
            except
            end ;
          end ;
        end{ "if (FirstErrorMsg = '') and (j > 0)" } ;

      end { "if (length(line) > 10) and (line[1] <> ' ')" } else begin
        // das erste Wort muss eine dezimalzahl sein.
        // danach kommt optional eine octal-adresse
        // danach optional Werte.
        // Nur die Werte zählen

        // Spalten: 1..8: Zeilennummer (optional)
        //            10-16 adresse
        // Ansatz: Zeilennummer weg, dann frei Format parsen, keine festen Spalten
        // (ich bin zu faul zum auszählen)
        s_lineno := Trim(Copy(line, 1, 8)) ;
        s_addr := Trim(Copy(line, 10,6)) ;
        line1 := Copy(line, 17, maxint) ;

        // Zeile der Source aktualisieren
        if TryStrToInt(s_lineno, j) then
          cursourcelinenr := j ; // neuer wert

//        sourcelinenr[i+1] := cursourcelinenr ;// Verweis Listing->Source eintragen
        sourcelinenr[i] := cursourcelinenr ;// Verweis Listing->Source eintragen

        if s_addr = '' then Continue ;
        try
          addr := OctalStr2Addr(s_addr, matVirtual) ;
          code_field_ready := (addr.val = MEMORYCELL_ILLEGALVAL) ;
        except
          code_field_ready := true ;
        end;
        if code_field_ready then Continue ; // nächste Zeile

        // line1: Sourcezeile ohne Zeilennummer und addresse
        // values sind jetzt das 1, 2., 3. , ... Wort in der verkürzten Zeile
        // read all octal data words bnehind address, until label or opcode
        j := 1 ;
        code_field_ready := false ;
        while not code_field_ready do begin
          s_value := ExtractWord(j, line1, [' ', #9]) ;
          if s_value = '' then begin
            code_field_ready := true ;
            Break ;
          end ;
          if (s_value <> '') and (s_value[length(s_value)] = ':') then begin
            code_field_ready := true ; // Es ist ein Label, vielleicht der Form "2$:"
            Break ;
          end ;
          split_octalsuffix(s_value, s_value, suffix) ;
          // Zus: s_value jetzt leer, oder wandelbare octalzahl
          if s_value = '' then begin
            code_field_ready := true ; // keine octalzahl mehr da
            Break ;
          end ;
          FillVal2MemoryCell(addr, s_value, i) ;
          if suffix = 'G' then begin
            if FirstErrorMsg = '' then begin
              FirstErrorMsg := Format('Unresolved global symbol', [0]) ;
              FirstErrorFilename := '' ;
              FirstErrorLineNr := cursourcelinenr ;
            end;
          end else if suffix = '''' then begin
            // in den macro11-Sources als PC-relative "displaced" bezeichnet?
          end else if suffix <> '' then
            raise Exception.CreateFmt('Unknown suffix "%s" in value "%s%s"', [suffix, s_value,suffix]) ;
          inc(j) ;  // check next word in line1
        end{ "while not code_field_ready" } ;
      end{ "if (length(line) > 10) and (line[1] <> ' ') ... ELSE" } ;
    end { "for i" } ;
    setErrorMark(FirstErrorLineNr) ;
    CodeParsed := true ;
  end { "procedure TFormMacro11Listing.ParseCode" } ;


// zeige den gelisteten Code an
procedure TFormMacro11Listing.ShowCodeForm;
  begin
    ParseCode ;

    with FormMain do begin
      FormMacro11Code.MemoryGrid.ConnectToMemoryCellGroup(FormMacro11Listing.memorycellgroup);
      setChildFormVisibility(FormMacro11Code, true);
    end ;
  end;

// zeige die zeile einer Maschinen-addresse markiert an.
procedure TFormMacro11Listing.setPCMark(addr: TMemoryAddress);
  var i, idx: integer ;
  begin
    ParseCode ;
    // finde die Memorycell mit der angebenen Adresse
    idx := memorycellgroup.CellIndexByAddr(addr) ;
    if idx >= 0 then begin
      // die Zeilennumer in der Source zu dieser Adresse wurde schon in ParseCode() berechent
      PCMarkerRow := memorycellgroup.Cell(idx).listinglinenr ;
      Editor.MakeRowVisible(PCMarkerRow);  // scrolle sichtbar
    end else
      PCMarkerRow := -1 ;

    // Marker-Zeile neu setzen
    with Editor do
      for i := 0 to Lines.Count-1 do
        if i = PCMarkerRow then
          LineInformations.SelectStyle[i] := lssDebugPoint
        else LineInformations.SelectStyle[i] := lssUnselected ;

    Editor.Invalidate ; // repaint, insbesondere Gutter
  end{ "procedure TFormMacro11Listing.setPCMark" } ;


// Fehler marke auf die Ziele mit der angegebenen SourceLineNr
procedure TFormMacro11Listing.setErrorMark(aSourcelinenr: integer);
  var i: integer ;
  begin
    with Editor do begin
      for i := Lines.Count-1 downto 0 do
//      for i := 0 to Lines.Count-1 do
        if sourcelinenr[i] = aSourcelinenr then begin
          LineInformations.SelectStyle[i] := lssErrorPoint ;
          MakeRowVisible(i);
        end else
          LineInformations.SelectStyle[i] := lssUnselected ;
    end;
  end;


procedure TFormMacro11Listing.ShowCodeMemFormButtonClick(Sender: TObject);
  begin
    ShowCodeForm ;
  end;


procedure TFormMacro11Listing.LoadButtonClick(Sender: TObject);
  begin
    if ListingFilename = '' then
      OpenDialog1.InitialDir := FormMain.DefaultDataDirectory
    else OpenDialog1.InitialDir := ExtractFilePath(ListingFilename) ;

    if OpenDialog1.Execute then begin
      ListingFilename := OpenDialog1.FileName ;

      //Editor.BeginUpdate ; // supress events while loading
      //Editor.Lines.Assign(Lines) ;
      //Editor.EndUpdate ;

      LoadFile(ListingFilename) ;
    end;
  end{ "procedure TFormMacro11Listing.LoadButtonClick" } ;

end{ "unit FormMacro11ListingU" } .
