unit FrameMemoryCellGroupGridU;
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

  Ereignisse für DepositAll, DepoitChanged, ExamineCur, ExamineAll
  sind schon in diesen Frame eingebaut.

  Zum sync der memorycells:
  pdp_values können immer vonanderen events (Disassembyl window)
  geändert werden. Das darf aber nicht mmer auf die edit_values duchschalgen!
  Bsp: Mem1 ist komplett "gelb", und fängt mit  Deposit All an
  Dann kann sich Disaselbly updaten.
  Disassembly fragt neue Werte ab, vertielt diese an alle Fenster ...
  auch an Mem1 ! Die Mem1.edit_values werden korrigiert und gelten als "aktuell,
  der USer verliert Werte!
  PdpOverwritesEdit steuert, ob aktualisierte PDp11-Werte IMMER in den Edit
  übernommen werden. default ist true

}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, StdCtrls, ExtCtrls, Menus,
  JH_Utilities,
  AddressU,
  MemoryCellU, JvExGrids, JvStringGrid ;

type
  TFrameMemoryCellGroupGrid = class(TFrame)
      MemoryCellsStringGrid: TJvStringGrid;
      PopupMenu1: TPopupMenu;
      Filldatawithaddr1: TMenuItem;
      Cleardata1: TMenuItem;
      Verify1: TMenuItem;
      N1: TMenuItem;
      ExportasSimHDOscript1: TMenuItem;
      ExportSaveDialog: TSaveDialog;

      procedure MemoryCellsStringGridSetEditText(Sender: TObject; aCol,
              aRow: integer; const Value: string);
      procedure MemoryCellsStringGridDrawCell(Sender: TObject; aCol,
              aRow: integer; Rect: TRect; State: TGridDrawState);
      procedure MemoryCellsStringGridSelectCell(Sender: TObject; aCol,
              aRow: integer; var CanSelect: Boolean);
      procedure ExamineAllButtonClick(Sender: TObject);
      procedure DepositChangedButtonClick(Sender: TObject);
      procedure DepositAllButtonClick(Sender: TObject);
      procedure MemoryCellsStringGridKeyPress(Sender: TObject; var Key: Char);
      procedure MemoryCellsStringGridGetEditText(Sender: TObject; aCol,
              aRow: integer; var Value: string);
      procedure ExamineCurrentButtonClick(Sender: TObject);
      procedure Cleardata1Click(Sender: TObject);
      procedure Filldatawithaddr1Click(Sender: TObject);
      procedure Verify1Click(Sender: TObject);
      procedure AnyControlMouseDown(Sender: TObject;
              Button: TMouseButton; Shift: TShiftState; x, y: integer);
      procedure ExportasSimHDOscript1Click(Sender: TObject);
    private
      { Private-Deklarationen }
      cur_row, cur_col : integer ;
      // wird von der memorycellgroup aufgerufen, wenn sich eine Zelle spontan ändert
      procedure memoryCellChange(Sender { = memorycellgroup}: TObject; memorycell: TMemoryCell) ;
      procedure syncBitfieldForm(aCol, aRow: integer) ;
    public
      { Public-Deklarationen }
      memorycellgroup: TMemoryCellGroup ;
      MemoryColumns: integer ;

      optimal_width, optimal_height: integer ; // Info an die Form, wie gross das grid werden möchte.

      OnUpdate: TNotifyEvent ; // Eine xternes Ereignis änderte die Darstellung oder Inhalt des Grid
      constructor Create(AOwner: TComponent) ; override ;
      destructor Destroy ; override ;

      procedure UpdateDisplay;

      procedure ClipboardCopy;
      procedure ClipboardPaste;

      procedure ConnectToMemoryCellGroup(mcg: TMemoryCellGroup) ;
      procedure ExamineCells(unknown_only: Boolean) ;
      procedure ShowMemoryCell(mc: TMemoryCell) ;
      procedure WriteCodeAsSimHScript(fname:string);
    end{ "TYPE TFrameMemoryCellGroupGrid = class(TFrame)" } ;


implementation

{$R *.dfm}

uses
  AuxU,
  ClipBrd,
  ConsoleGenericU, FormMainU;

constructor TFrameMemoryCellGroupGrid.Create(AOwner: TComponent) ;
  begin
    inherited Create(AOwner) ;
    OnUpdate := nil ;
    MemoryColumns := 8 ;
  end;

destructor TFrameMemoryCellGroupGrid.Destroy ;
  begin
    inherited ;
  end;


// neue malen
procedure TFrameMemoryCellGroupGrid.UpdateDisplay;
  var i: integer ;
    mc: TMemoryCell ;
  begin
    // alle anzeigen
    for i := 0 to memorycellgroup.Count-1 do begin
      mc := memorycellgroup.Cell(i) ;
      ShowMemoryCell(mc) ;
    end;

// parent form möcht auch updaten.
    if assigned(OnUpdate) then
      OnUpdate(self) ;
  end{ "procedure TFrameMemoryCellGroupGrid.UpdateDisplay" } ;


procedure TFrameMemoryCellGroupGrid.ClipboardCopy;
  begin
    ClipBoard.Astext := MemoryCellsStringGrid.InplaceEditor.SelText ;
  end;

procedure TFrameMemoryCellGroupGrid.ClipboardPaste;
  begin
    if ClipBoard <> nil then // passiert, wenn die cell nicht im editmdoe ist
      MemoryCellsStringGrid.InplaceEditor.SelText := ClipBoard.Astext ;
  end;


procedure TFrameMemoryCellGroupGrid.syncBitfieldForm(aCol, aRow: integer) ;
  begin
    // Bitfields-Form soll auch diese Zelle anzeigen
    with MemoryCellsStringGrid do
      if Objects[aCol,aRow] as TMemoryCell <> nil then
        FormMain.syncBitfieldForm((Objects[aCol,aRow] as TMemoryCell)) ;
  end;


// memorycell der group in der Form aktualisieren
procedure TFrameMemoryCellGroupGrid.DepositAllButtonClick(Sender: TObject);
  begin
    memorycellgroup.Deposit({optimize}false, true) ; // long list
    UpdateDisplay ;
    with MemoryCellsStringGrid do
      syncBitfieldForm(Col, Row);
  end;


procedure TFrameMemoryCellGroupGrid.DepositChangedButtonClick(Sender: TObject);
  begin
    memorycellgroup.Deposit({optimize}true, true) ; // long list

    UpdateDisplay ;

    with MemoryCellsStringGrid do
      syncBitfieldForm(Col, Row);
  end;


// Zelen der Liste vonder PDP abfragen.
// unknown_only = true: nur die mit pdp_value = MEMORYCELL_ILLEGALVAL
procedure TFrameMemoryCellGroupGrid.ExamineCells(unknown_only: Boolean) ;
  var i: integer ;
    mc: TMemoryCell ;
  begin
    // alle Zellen laden, nach pdp_value
    memorycellgroup.Examine(unknown_only, true) ; // long list

    for i := 0 to memorycellgroup.Count-1 do begin
      mc := memorycellgroup.Cell(i) ;
      mc.edit_value := mc.pdp_value ;
    end;
  end;

procedure TFrameMemoryCellGroupGrid.ExamineCurrentButtonClick(Sender: TObject);
  var mc: TMemoryCell ;
  begin
    with MemoryCellsStringGrid do
      if Objects[Col,Row] as TMemoryCell <> nil then begin
        mc := Objects[Col,Row] as TMemoryCell ;
        mc.Examine ;
        mc.edit_value := mc.pdp_value ;
        UpdateDisplay ;
        syncBitfieldForm(Col, Row);
      end ;
  end;



procedure TFrameMemoryCellGroupGrid.ExamineAllButtonClick(Sender: TObject);
  begin
    ExamineCells({unknown_only}false) ;

    UpdateDisplay ;
    with MemoryCellsStringGrid do
      syncBitfieldForm(Col, Row);
  end;


// readonly-spalten nicht weiss,
// geänderte Werte rosa
procedure TFrameMemoryCellGroupGrid.MemoryCellsStringGridDrawCell(Sender: TObject;
        aCol, aRow: integer; Rect: TRect; State: TGridDrawState);
  var
    s: string ;
    newcolor: Boolean  ;
    mc: TMemoryCell ;
  begin
    with MemoryCellsStringGrid do begin

      if gdFocused in State then begin
        // hat sich die Fokuszelle geändert?
        // Wenn ja: neue Position merken, und nochmal alles neu malen
        // keine Rekursion: da beim zweiten Malen die Fokuszelle gleich bleibt
        if (cur_row <> aRow) or (cur_col <> aCol) then begin
          cur_row := aRow ;
          cur_col := aCol ;
          Invalidate ;
        end ;
      end;

      s := Cells[aCol, aRow];
      newcolor := false ;
      if (aRow = 0) or (aCol = 0) then begin
        // Randspalte/Kopfzeile
        Canvas.Brush.Color := clBtnFace ;
        newcolor := true ;
        if (aRow = 0) and (aCol = cur_col) and (aCol <> 0) then begin
          // offset und startaddr der aktuellen zelle rot malen, aber nicht 0/0
          newcolor := true ;
//          Canvas.Brush.Color := TColor(#$E0E0E0) ; // readonly Felder in grauer schrift
          Canvas.Font.Color := clred ;
        end else if (aRow = cur_row) and (aCol = 0) and (aRow <> 0) then begin
          newcolor := true ;
//          Canvas.Brush.Color := TColor(#$E0E0E0) ; // readonly Felder in grauer schrift
          Canvas.Font.Color := clred ;
        end else begin
        end;
      end { "if (aRow = 0) or (aCol = 0)" } else begin
        mc := Objects[aCol, aRow] as TMemoryCell ;
        // Gridzellen ohne memorycell dahinter grau malen
        if mc = nil then begin
//          Canvas.Brush.Color := clSilver ;
//          newcolor := true ;
        end else if mc.edit_value <> mc.pdp_value then begin
          newcolor := true ;
          Canvas.Brush.Color := ColorGridCellChangedBkGnd ;
          Canvas.Font.Color := ColorGridCellChangedText ;
        end ;
      end;

      if newcolor then begin
        Canvas.FillRect(Rect);
        Canvas.TextOut(Rect.left+2, Rect.Top+1, s) ;
//        DrawText(Canvas.Handle, PChar(s), Length(s), Rect, DT_LEFT + DT_BOTTOM + DT_SINGLELINE);
      end;
    end{ "with MemoryCellsStringGrid" } ;
  end{ "procedure TFrameMemoryCellGroupGrid.MemoryCellsStringGridDrawCell" } ;



procedure TFrameMemoryCellGroupGrid.MemoryCellsStringGridGetEditText(
        Sender: TObject; aCol, aRow: integer; var Value: string);
  begin
    // neue Zelle ist angesteuert: neu malen!
//            MemoryCellsStringGrid.Repaint ;

  end;

procedure TFrameMemoryCellGroupGrid.MemoryCellsStringGridKeyPress(Sender: TObject;
        var Key: Char);
  begin
    // nur Octalziffern als Eingabe. auch das backspace #8 erlauben
//    if not CharInSet(Key, [#8, '0'..'7']) then
//      Key := #0 ;
    // nur Octalziffern als Eingabe. auch das backspace #8 erlauben
//    if not CharInSet(Key, [#8, '0'..'7']) then
//      Key := #0 ;
    case Key of
      #3: begin
        Key := #0 ;
        ClipboardCopy ; // ^C
      end ;
      #22: begin
        Key := #0 ;
        ClipboardPaste ; // ^V
      end ;
      // nur Octalziffern als Eingabe. auch das backspace #8 erlauben
      #8, '0'..'7': ;
      else
        Key := #0 ;
    end{ "case Key" } ;

  end{ "procedure TFrameMemoryCellGroupGrid.MemoryCellsStringGridKeyPress" } ;


procedure TFrameMemoryCellGroupGrid.AnyControlMouseDown(
        Sender: TObject; Button: TMouseButton; Shift: TShiftState; x, y: integer);
  var p: TPoint ;
  begin
    // Form ist nicht visible, rightclick auf grid
    p.x := x ; p.y := y ;
    p := (Sender as TControl).ClientToScreen(p) ;
    if ssRight in Shift then
      PopupMenu1.Popup(p.x,p.y) ;
  end;

// nur Werte in Spalte 2 editierbar
procedure TFrameMemoryCellGroupGrid.MemoryCellsStringGridSelectCell(Sender: TObject;
        aCol, aRow: integer; var CanSelect: Boolean);
  begin
    with MemoryCellsStringGrid do begin
      // falls eine neue Zelle gewählt wird,
      // wird eine editierte verlassen: alles neu malen!
      UpdateDisplay ;
      syncBitfieldForm(aCol, aRow);
    end;
  end;


procedure TFrameMemoryCellGroupGrid.MemoryCellsStringGridSetEditText(Sender: TObject;
        aCol, aRow: integer; const Value: string);
  var mc: TMemoryCell ;
    s: string ;
    i: integer ;
  begin
    // eingegebenen Value an die memorycell weiterleiten
    // nur gültige Octalziffern durchlassen
    // '?' erlauben, als ungültige Zahl (macht das Grid intern)
    s := '' ;
    for i := 1 to Length(Value) do
      if CharInSet(Value[i], ['0'..'7']) then
        s := s + Value[i]
      else if Value[i] = '?' then begin
        s := '?' ;
        break ;
      end;

    mc := MemoryCellsStringGrid.Objects[aCol, aRow] as TMemoryCell ;
    if mc <> nil then
      if s = '' then
        mc.edit_value := MEMORYCELL_ILLEGALVAL // kann während EEdit vorkommen
      else
        mc.edit_value := OctalStr2Dword(s, 16) ;
  end{ "procedure TFrameMemoryCellGroupGrid.MemoryCellsStringGridSetEditText" } ;


procedure TFrameMemoryCellGroupGrid.ShowMemoryCell(mc: TMemoryCell) ;
  begin
    // die memorycell kennt die Gridrow, in der sie angezeigt wird.
    MemoryCellsStringGrid.Cells[mc.grid_c,mc.grid_r] := ' ' + Dword2OctalStr(mc.edit_value, 16) ;
  end;


// Die MemoryCells laden und im Grid anzeigen
// Achtung: adressen der memorycellgroup können Löcher haben,
//(wenn sie von MACRO11 erzeugt wurden)
procedure TFrameMemoryCellGroupGrid.ConnectToMemoryCellGroup(mcg: TMemoryCellGroup) ;

function textextent(s:string): TSize ;
begin
MemoryCellsStringGrid.Canvas.Font.Assign(MemoryCellsStringGrid.Font) ;
result := MemoryCellsStringGrid.Canvas.TextExtent(s) ;
end;

  var i, n: integer ;
    mc: TMemoryCell ;
    startaddr, curaddr: TMemoryAddress ;
    displaycellcount: dword ; // anzuzeigende GridZellen: max_addr-min_addr+1
    c,r: integer ;
    addridx: integer ;
  begin
    memorycellgroup := mcg ;

    // soviele zellen, wie 16bit worte
    displaycellcount := (mcg.max_addr.val - mcg.min_addr.val) div 2  + 1 ;
    with MemoryCellsStringGrid do begin
      // tabelle aufbauen
      ColCount := 1 + MemoryColumns ;
      RowCount := 1 + (displaycellcount-1) div MemoryColumns + 1; // 1,2,3,4 -> 1, 5,6,7,8 -> 2, ...
      FixedRows := 1 ;
      FixedCols := 1 ;

      // Zellen löschen
      for c:= 0 to ColCount-1 do
        for r := 0 to RowCount - 1 do begin
          Cells[c, r] := '' ;
          Objects[c, r] := nil ;
        end;

      Cells[0,0] := 'start \ offset' ;
      if mcg.Count > 0 then begin

      canvas.Font.Assign(font) ;

        startaddr := mcg.Cell(0).addr ;

        // offsets horizontal = +0, +2, +4 , ...
        MemoryCellsStringGrid.ColWidths[0] := textextent('Xstart \ offset').width ; // 70 ; // 22 bit addr

        for i := 0 to MemoryColumns-1 do begin
          MemoryCellsStringGrid.ColWidths[i+1] := textextent('X 000000').Width ; // ; // 16 bit val
          Cells[i+1, 0] := ' + ' + Dword2OctalStr(i*2) ;
        end;
        // startadressen vertikal
        curaddr := startaddr ;
        for i := 0 to RowCount-1 do begin
          Cells[0, i+1] := ' ' + Addr2OctalStr(curaddr) ;
          curaddr.val := curaddr.val + 2*MemoryColumns ;
        end;
      end { "if mcg.Count > 0" } ;
    end{ "with MemoryCellsStringGrid" } ;

    // Zellen auf GridPlätze verteilen. Lücken in adressen beachten!
    for i := 0 to mcg.Count-1 do begin
      mc := mcg.Cell(i) ;
      mc.grid := MemoryCellsStringGrid ;
      addridx := (mc.addr.val - mcg.min_addr.val) div 2 ;
      mc.grid_c := 1 + addridx mod MemoryColumns ;
      mc.grid_r := 1 + addridx div MemoryColumns ;
      // memory cells an gridzellen binden
      MemoryCellsStringGrid.Objects[mc.grid_c, mc.grid_r] := mc ;
      ShowMemoryCell(mc) ;
    end ;

    // callback bei Zellenänderung
    mcg.OnMemoryCellChange := memoryCellChange ;

    // optimal_width, optimal_height:
    // Info an die Form, wie gross das grid werden möchte.
    // Formhoehe/breite so einstellen, dass Grid genau alle Zeilen anzeigt
    with MemoryCellsStringGrid do begin
      optimal_height := RowCount * (1+DefaultRowHeight) + 3 ;
      n := 0 ;
      for i := 0 to ColCount-1 do n := n + ColWidths[i] + 1 ;
      optimal_width := n + 3 ;
    end ;
    UpdateDisplay ;
  end{ "procedure TFrameMemoryCellGroupGrid.ConnectToMemoryCellGroup" } ;


// wird von der memorycellgroup aufgerufen, wenn sich eine Zelle spontan ändert
procedure TFrameMemoryCellGroupGrid.memoryCellChange(Sender { = memorycellgroup}: TObject; memorycell: TMemoryCell) ;
  begin
    memorycell.edit_value := memorycell.pdp_value ;
    ShowMemoryCell(memorycell) ;
  end ;


procedure TFrameMemoryCellGroupGrid.Cleardata1Click(Sender: TObject);
  var
    i: integer ;
    mc: TMemoryCell ;
  begin
    // Alle Zellen löschen
    for i:= 0 to memorycellgroup.Count - 1 do begin
      mc := memorycellgroup.Cell(i) ;
      mc.edit_value := 0 ;
    end ;
    UpdateDisplay ;
  end;

procedure TFrameMemoryCellGroupGrid.Filldatawithaddr1Click(Sender: TObject);
// Jede Zelle mit ihrer WORD(!)-Adresse füllen
  var
    i: integer ;
    mc: TMemoryCell ;
  begin
    // Alle Zellen löschen
    for i:= 0 to memorycellgroup.Count - 1 do begin
      mc := memorycellgroup.Cell(i) ;
      mc.edit_value := (mc.addr.val shr 1) and  $ffff ;
    end ;
    UpdateDisplay ;
  end;


procedure TFrameMemoryCellGroupGrid.Verify1Click(Sender: TObject);
  begin
    // Neu laden, gelbe Zellen, wenn PDP von Edit abweicht
    // das sit das einfahcste von der Welt
    memorycellgroup.Examine({unknown_only}false, true) ; // long list
    UpdateDisplay ;
  end;

// Erzeuge die Deposit-Commandos für SimH
procedure TFrameMemoryCellGroupGrid.WriteCodeAsSimHScript(fname:string);
  var i: integer ;
    sl: TStringList ;
  begin
    sl := TStringList.Create ;
    try
      for i := 0 to memorycellgroup.Count - 1 do
        sl.Add(Format('d %s %s', [
                Addr2OctalStr(memorycellgroup.Cell(i).addr),
                Dword2OctalStr(memorycellgroup.Cell(i).edit_value, 16)
                ])) ;
      Log('SaveToFile(%s)', [fname]);
      sl.SaveToFile(fname);
    finally
      sl.Free ;
    end;
  end{ "procedure TFrameMemoryCellGroupGrid.WriteCodeAsSimHScript" } ;



procedure TFrameMemoryCellGroupGrid.ExportasSimHDOscript1Click(Sender: TObject);
  begin
    ExportSaveDialog.DefaultExt := 'sim' ;
    if ExportSaveDialog.Execute then
      WriteCodeAsSimHScript(ExportSaveDialog.FileName) ;
  end ;

end{ "unit FrameMemoryCellGroupGridU" } .


