unit FrameMemoryCellGroupListU;
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
  Dialogs, Grids, ExtCtrls,
  StdCtrls,
  JH_Utilities,
  AddressU,
  MemoryCellU, JvExGrids, JvStringGrid;

type
  TFrameMemoryCellGroupList = class(TFrame)
      MemoryCellsStringGrid: TJvStringGrid;
      procedure MemoryCellsStringGridSetEditText(Sender: TObject; aCol,
              aRow: integer; const Value: string);
      procedure MemoryCellsStringGridDrawCell(Sender: TObject; aCol,
              aRow: integer; Rect: TRect; State: TGridDrawState);
      procedure MemoryCellsStringGridSelectCell(Sender: TObject; aCol,
              aRow: integer; var CanSelect: Boolean);
      procedure MemoryCellsStringGridKeyPress(Sender: TObject; var Key: Char);
      procedure MemoryCellsStringGridMouseUp(Sender: TObject;
              Button: TMouseButton; Shift: TShiftState; X, Y: integer);
      procedure FrameResize(Sender: TObject);

    private
      { Private-Deklarationen }
      memorycellgroup: TMemoryCellGroup ;
      // wird von der memorycellgroup aufgerufen, wenn sich eine zelle spontan ändert
      procedure MemoryCellChange(Sender { = memorycellgroup}: TObject; memorycell: TMemoryCell) ;
      procedure SyncBitfieldForm(aRow: integer) ;


    public
      { Public-Deklarationen }
      procedure UpdateDisplay;
      procedure ClipboardCopy;
      procedure ClipboardPaste;

      procedure ConnectToMemoryCellGroup(mcg: TMemoryCellGroup) ;
      procedure ShowMemoryCell(mc: TMemoryCell) ;

      procedure ExamineCurrentButtonClick(Sender: TObject);
      procedure ExamineAllButtonClick(Sender: TObject);
      procedure DepositChangedButtonClick(Sender: TObject);
      procedure DepositAllButtonClick(Sender: TObject);

    end{ "TYPE TFrameMemoryCellGroupList = class(TFrame)" } ;

implementation

uses
  ClipBrd,
  AuxU,
  FormMainU;


{$R *.dfm}


// neue malen
procedure TFrameMemoryCellGroupList.UpdateDisplay;
  var i: integer ;
    mc: TMemoryCell ;
  begin
    // alle anzeigen
    if memorycellgroup <> nil then
      for i := 0 to memorycellgroup.Count-1 do begin
        mc := memorycellgroup.Cell(i) ;
        ShowMemoryCell(mc) ;
      end;
  end;


procedure TFrameMemoryCellGroupList.ClipboardCopy;
  begin
    ClipBoard.Astext := MemoryCellsStringGrid.InplaceEditor.SelText ;
  end;

procedure TFrameMemoryCellGroupList.ClipboardPaste;
  begin
    if ClipBoard <> nil then // passiert, wenn die cell nicht im editmdoe ist
      MemoryCellsStringGrid.InplaceEditor.SelText := ClipBoard.Astext ;
  end;

procedure TFrameMemoryCellGroupList.SyncBitfieldForm(aRow: integer) ;
  begin
    // Bitfields-Form soll auch diese Zelle anzeigen
    with MemoryCellsStringGrid do
      if Objects[2,aRow] as TMemoryCell <> nil then
        FormMain.SyncBitfieldForm((Objects[2,aRow] as TMemoryCell)) ;
  end;

procedure TFrameMemoryCellGroupList.FrameResize(Sender: TObject);
  var i, n: integer ;
  begin
    n := 0 ;
    with MemoryCellsStringGrid do begin
      for i := 0 to 2 do
        n := n + 1 + ColWidths[i] ;
      ColWidths[3] := Clientwidth- n ;
    end;
  end ;


// readonly-Spalten nicht weiss,
// geänderte Werte gelb
procedure TFrameMemoryCellGroupList.MemoryCellsStringGridDrawCell(Sender: TObject;
        aCol, aRow: integer; Rect: TRect; State: TGridDrawState);
  var
    s: string ;
    newcolor: Boolean  ;
    mc: TMemoryCell ;
  begin
    with MemoryCellsStringGrid do begin
      s := Cells[aCol, aRow];
      newcolor := false ;
      if aRow >= 1 then begin
        newcolor := true ;
        if aCol in [0,1,3] then begin
          Canvas.Brush.Color := ColorGridCellReadOnlyBkGnd ;
          Canvas.Font.Color := ColorGridCellReadOnlyText ;
        end;
        if aCol = 2 then begin // editierbarer Wert
          mc := Objects[aCol,aRow] as TMemoryCell ;
          if (mc <> nil) and (mc.edit_value <> mc.pdp_value) then begin
            newcolor := true ; // geänderte Felder mit gelbem Hintergrund
            Canvas.Brush.Color := ColorGridCellChangedBkGnd ;
            Canvas.Font.Color := ColorGridCellChangedText ;
          end;
        end;
      end{ "if aRow >= 1" } ;
      if newcolor then begin
        Canvas.FillRect(Rect);
        Canvas.TextOut(Rect.Left+2,Rect.Top, s);
//        DrawText(Canvas.Handle, PChar(s), Length(s), Rect, DT_LEFT);
      end;
    end{ "with MemoryCellsStringGrid" } ;
  end{ "procedure TFrameMemoryCellGroupList.MemoryCellsStringGridDrawCell" } ;





procedure TFrameMemoryCellGroupList.MemoryCellsStringGridKeyPress(Sender: TObject;
        var Key: Char);
  begin
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
  end{ "procedure TFrameMemoryCellGroupList.MemoryCellsStringGridKeyPress" } ;


procedure TFrameMemoryCellGroupList.MemoryCellsStringGridMouseUp(Sender: TObject;
        Button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin

    // die neuen ColWidhts sichern.
    FormGridSaveColWidths(self.name, MemoryCellsStringGrid) ;
  end;

// nur Werte in Spalte 2 editierbar
procedure TFrameMemoryCellGroupList.MemoryCellsStringGridSelectCell(Sender: TObject;
        aCol, aRow: integer; var CanSelect: Boolean);
//  var mc: TMemoryCell ;
  begin
    with MemoryCellsStringGrid do begin
      if (aRow < 1) or ((aCol=0) or (aCol = 1) or (aCol=3)) then
        options := options - [goEditing]
      else begin
        // falls eine neue Zelle gewählt wird,
        // wird eine editierte verlassen: alles neu malen!
        UpdateDisplay ;
        options := options + [goEditing] ;
      end;

      SyncBitfieldForm(aRow);
    end;
  end{ "procedure TFrameMemoryCellGroupList.MemoryCellsStringGridSelectCell" } ;


procedure TFrameMemoryCellGroupList.MemoryCellsStringGridSetEditText(Sender: TObject;
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
      if isOctalDigit(Value[i]) then
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
  end{ "procedure TFrameMemoryCellGroupList.MemoryCellsStringGridSetEditText" } ;

procedure TFrameMemoryCellGroupList.ShowMemoryCell(mc: TMemoryCell) ;
  begin
    // die memorycell kennt die Gridrow, in der sie angezeigt wird.
    MemoryCellsStringGrid.Cells[0,mc.grid_r] := ' ' + mc.name ;
    MemoryCellsStringGrid.Cells[1,mc.grid_r] := ' ' + Addr2OctalStr(mc.addr) ;
    MemoryCellsStringGrid.Cells[2,mc.grid_r] := ' ' + Dword2OctalStr(mc.edit_value, 16) ;
    MemoryCellsStringGrid.Cells[3,mc.grid_r] := ' ' + mc.info ;
//    mc.edit_value := mc.pdp_value ; // es wird jetzt angezeigt, was in der PDP-11 steht
  end;


// Die MemoryCells im Grid anzeigen
procedure TFrameMemoryCellGroupList.ConnectToMemoryCellGroup(mcg: TMemoryCellGroup) ;

function textextent(s:string): TSize ;
begin
MemoryCellsStringGrid.Canvas.Font.Assign(MemoryCellsStringGrid.Font) ;
result := MemoryCellsStringGrid.Canvas.TextExtent(s) ;
end;

  var i: integer ;
    mc: TMemoryCell ;
  begin
    memorycellgroup := mcg ;

    MemoryCellsStringGrid.Cells[0,0] := 'Register' ;
    MemoryCellsStringGrid.Cells[1,0] := 'Addr' ;
    MemoryCellsStringGrid.Cells[2,0] := 'Value' ;
    MemoryCellsStringGrid.Cells[3,0] := 'Info' ;
    MemoryCellsStringGrid.RowCount := 1 + mcg.Count ;

    MemoryCellsStringGrid.DefaultRowHeight := textextent('Xg').Height ; // ;

    MemoryCellsStringGrid.ColWidths[0] := textextent('XRegister').Width ; // ;
    MemoryCellsStringGrid.ColWidths[1] := textextent('X00000000').Width ; // 60 22 bit addr
    MemoryCellsStringGrid.ColWidths[2] := textextent('X000000').Width ; // 45 16 bit val ;

    // die letzte Spalte geht bis ans Formende
    FrameResize(nil) ;

    // callback bei Zellenänderung
    mcg.OnMemoryCellChange := MemoryCellChange ;

    // Alle Zellen anzeigen
    for i := 0 to mcg.Count - 1 do begin
      mc := mcg.Cell(i) ;
      mc.grid := MemoryCellsStringGrid ;
      mc.grid_c := 2 ; // value immer in Spalte 2
      mc.grid_r := i+1 ; // row
      // Verbidnung grid->memorycell
      MemoryCellsStringGrid.Objects[mc.grid_c, mc.grid_r] := mc ;

      ShowMemoryCell(mc) ;
    end;

    FormGridLoadColWidths(self.name, MemoryCellsStringGrid) ; //
    // OptimizeAnyGridColWidths(MemoryCellsStringGrid) ;

  end{ "procedure TFrameMemoryCellGroupList.ConnectToMemoryCellGroup" } ;


// wird von der memorycellgroup aufgerufen, wenn sich eine zelle spontan ändert
procedure TFrameMemoryCellGroupList.MemoryCellChange(Sender { = memorycellgroup}: TObject; memorycell: TMemoryCell) ;
  begin
    memorycell.edit_value := memorycell.pdp_value ;
    ShowMemoryCell(memorycell) ;
  end ;

// memorycell der group ind er Form aktualisieren
procedure TFrameMemoryCellGroupList.DepositAllButtonClick(Sender: TObject);
  begin
    memorycellgroup.Deposit({optimize}false, true) ; // long list
    UpdateDisplay ;
    SyncBitfieldForm(MemoryCellsStringGrid.row);
  end;


procedure TFrameMemoryCellGroupList.DepositChangedButtonClick(Sender: TObject);
  begin
    memorycellgroup.Deposit({optimize}true, true) ; // long list
    UpdateDisplay ;
    with MemoryCellsStringGrid do
      SyncBitfieldForm(row);

  end;


procedure TFrameMemoryCellGroupList.ExamineCurrentButtonClick(Sender: TObject);
  var mc: TMemoryCell ;
  begin
    with MemoryCellsStringGrid do
      if Objects[2,row] as TMemoryCell <> nil then begin
        mc := Objects[2,row] as TMemoryCell ;
        mc.Examine ;
        mc.edit_value := mc.pdp_value ;
        UpdateDisplay ;
        SyncBitfieldForm(row);
      end;
  end;

procedure TFrameMemoryCellGroupList.ExamineAllButtonClick(Sender: TObject);
  var i: integer ;
    mc: TMemoryCell ;
  begin
    // alle Zellen laden, nach pdp_value
    memorycellgroup.Examine({unknown_only} false, true) ; // long list

    for i := 0 to memorycellgroup.Count-1 do begin
      mc := memorycellgroup.Cell(i) ;
      mc.edit_value := mc.pdp_value ;
    end;
    UpdateDisplay ;

    with MemoryCellsStringGrid do
      SyncBitfieldForm(row);
  end{ "procedure TFrameMemoryCellGroupList.ExamineAllButtonClick" } ;



end{ "unit FrameMemoryCellGroupListU" } .
