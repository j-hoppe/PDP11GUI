unit FormBitfieldsU;

{

Ein Bitfeldeditor für eine MemoryCell
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  JH_Utilities,
  FormChildU,
  MemoryCellU,
  JvExGrids, JvStringGrid,
  BitFieldU, Grids ; 

type 
  TFormBitfields = class(TFormChild) 
      PanelT: TPanel; 
      AddrLabel: TLabel; 
      Label3: TLabel; 
      InfoLabel: TLabel; 
      AddrEdit: TEdit; 
      ValueEdit: TEdit; 
      BitfieldsStringGrid: TJvStringGrid; 
      DepositButton: TButton; 
      ExamineButton: TButton; 
      PanelBackground: TPanel; 
      procedure AddrEditKeyPress(Sender: TObject; var Key: char); 
      procedure ValueEditKeyPress(Sender: TObject; var Key: char); 
      procedure BitfieldsStringGridKeyPress(Sender: TObject; var Key: char); 
      procedure ValueEditChange(Sender: TObject); 
      procedure ExamineButtonClick(Sender: TObject); 
      procedure DepositButtonClick(Sender: TObject); 
      procedure BitfieldsStringGridDrawCell(Sender: TObject; ACol, ARow: integer; 
              Rect: TRect; State: TGridDrawState); 
      procedure BitfieldsStringGridSelectCell(Sender: TObject; ACol, 
              ARow: integer; var CanSelect: Boolean); 
      procedure BitfieldsStringGridSetEditText(Sender: TObject; ACol, 
              ARow: integer; const Value: string); 
      procedure FormCreate(Sender: TObject); 
      procedure FormResize(Sender: TObject); 
      procedure BitfieldsStringGridMouseUp(Sender: TObject; Button: TMouseButton; 
              Shift: TShiftState; X, Y: integer); 
    private 
      { Private-Deklarationen }
      bitfieldsdef: TBitFieldsDef ; // Liste der Bitfler im der aktuellen memroycell

      editChangeEventsEnabled: Boolean ; // OnChange-Event von Grid und Edit zeitweise abschalten

      function BitFieldDefForRow(ARow: integer): TBitFieldDef ; 
      // welcher Wert muss in der iten Gridzeile stehen?
      function getBitFieldValueForRow(ARow: integer; Value: dword): dword ; 
      procedure setBitFieldValueForRow(ARow: integer; fieldvalue: dword) ; 
      procedure UpdateBitfieldsText ; 
      procedure UpdateValueEditText ; 

    public 
      { Public-Deklarationen }
      memorycellgroup : TMemoryCellGroup ; 
      memorycell: TMemoryCell ; 
      procedure ConnectToMemoryCell(mcg: TMemoryCellGroup) ; 

      procedure UpdateDisplay ; 
      // wird von der memorycellgroup aufgerufen, wenn sich eine zelle spontan ändert
      procedure MemoryCellChange(Sender { = memorycellgroup}: TObject; memorycell: TMemoryCell) ; 
      procedure ShowNewAddr(mc: TMemoryCell) ; 
    end{ "TYPE TFormBitfields = class(TFormChild)" } ; 


implementation 

{$R *.dfm}

uses 
  AuxU, 
  AddressU ; 

procedure TFormBitfields.FormCreate(Sender: TObject); 
  begin 
    editChangeEventsEnabled := true ; 
  end; 


procedure TFormBitfields.FormResize(Sender: TObject); 
  var i, n: integer ; 
  begin 
    // die letzte Spalte des grids bis an den Rand ausdehnen
    with BitfieldsStringGrid do begin 
      n := 0 ; 
      for i := 0 to 4 do 
        n := n + 1 + ColWidths[i] ; 
      ColWidths[5] := Clientwidth- n ; 
    end ; 
  end; 


// welches Bitfield steht in welcher Grid-Row?
function TFormBitfields.BitFieldDefForRow(ARow: integer): TBitFieldDef ; 
  begin 
    if ARow = 0 then 
      result := nil 
    else 
      result := bitfieldsdef.bitfields.Items[ARow-1] 
              as TBitFieldDef ; 
  end ; 

// welcher Wert muss in der iten Gridzeile stehen?
// value: pdp_value oder edit_value
function TFormBitfields.getBitFieldValueForRow(ARow: integer; Value: dword): dword ; 
  var bfd: TBitFieldDef ; 
  begin 
    bfd := BitFieldDefForRow(ARow) ; 
    assert(bfd <> nil) ; 
    result := bfd.getFieldInValue(Value) ; 
  end; 


// welcher Wert muss in der iten Gridzeile stehen?
procedure TFormBitfields.setBitFieldValueForRow(ARow: integer; fieldvalue: dword) ; 
  var bfd: TBitFieldDef ; 
  begin 
    bfd := BitFieldDefForRow(ARow) ; 
    assert(bfd <> nil) ; 
    memorycell.edit_value := bfd.setFieldInValue(memorycell.edit_value, fieldvalue) ; 
  end; 


procedure TFormBitfields.AddrEditKeyPress(Sender: TObject; var Key: char); 
  begin 
    // nur Octalziffern als Eingabe. auch das backspace #8 erlauben
    if not CharInSet(Key, [#8, '0'..'7']) then
      Key := #0 ; 
  end; 


procedure TFormBitfields.DepositButtonClick(Sender: TObject); 
  begin 
    memorycell.Deposit ;
    UpdateDisplay ; 
  end; 

procedure TFormBitfields.ExamineButtonClick(Sender: TObject); 
  begin 
    memorycell.Examine ;
    memorycell.edit_value := memorycell.pdp_value ; 
    UpdateDisplay ; 
  end; 


// readonly-Spalten nicht weiss,
// geänderte Werte gelb
procedure TFormBitfields.BitfieldsStringGridDrawCell(Sender: TObject; ACol, 
        ARow: integer; Rect: TRect; State: TGridDrawState); 
  var 
    s: string ; 
    newcolor: Boolean  ; 
    pdp_fieldvalue, edit_fieldvalue: dword ; 
  begin 
    with BitfieldsStringGrid do begin 
      s := Cells[ACol, ARow]; 
      newcolor := false ; 
      if ARow >= 1 then begin 
        newcolor := true ; 
        if ACol in [0,1,2,4,5] then begin
          Canvas.Brush.Color := ColorGridCellReadOnlyBkGnd ; // readonly Felder in grauer schrift
          Canvas.Font.Color := ColorGridCellReadOnlyText ; 
        end; 
        if ACol = 3 then begin // editierbarer Wert
          pdp_fieldvalue := getBitFieldValueForRow(ARow, memorycell.pdp_value) ; 
          edit_fieldvalue := getBitFieldValueForRow(ARow, memorycell.edit_value) ; 
          if pdp_fieldvalue <> edit_fieldvalue then begin 
            newcolor := true ; // geänderte Felder mit gelbem Hintergrund
            Canvas.Brush.Color := ColorGridCellChangedBkGnd ; 
            Canvas.Font.Color := ColorGridCellChangedText ; 
          end; 
        end; 
      end{ "if ARow >= 1" } ; 
      if newcolor then begin 
        Canvas.FillRect(Rect); 
        DrawText(Canvas.Handle, PChar(s), Length(s), Rect, DT_LEFT); 
      end; 
    end{ "with BitfieldsStringGrid" } ; 

  end{ "procedure TFormBitfields.BitfieldsStringGridDrawCell" } ; 

procedure TFormBitfields.BitfieldsStringGridKeyPress(Sender: TObject; var Key: char); 
  begin 
    // nur Octalziffern als Eingabe. auch das backspace #8 erlauben
    if not CharInSet(Key, [#8, '0'..'7']) then
      Key := #0 ; 
  end; 


procedure TFormBitfields.BitfieldsStringGridMouseUp(Sender: TObject; 
        Button: TMouseButton; Shift: TShiftState; X, Y: integer); 
  begin 
    // die neuen ColWidhts sichern.
    FormGridSaveColWidths(self.name, BitfieldsStringGrid) ; 
  end; 


procedure TFormBitfields.BitfieldsStringGridSelectCell(Sender: TObject; ACol, 
        ARow: integer; var CanSelect: Boolean); 
  begin 
    with BitfieldsStringGrid do begin 
      if (ARow < 1) or (ACol in [0,1,2,4]) then 
        options := options - [goEditing] 
      else begin 
        // falls eine neue Zelle gewählt wird,
        // wird eine editierte verlassen: alles neu malen!
        UpdateDisplay ; 
        options := options + [goEditing] ; 
      end; 
    end; 
  end; 

// wird während des Tippens in den bitfield-values aufgerufen
procedure TFormBitfields.BitfieldsStringGridSetEditText(Sender: TObject; ACol, 
        ARow: integer; const Value: string); 
  begin 
    if editChangeEventsEnabled then begin 
      if (ARow > 0) and (ACol = 3) then begin 
        if Value = '' then 
          setBitFieldValueForRow(ARow, 0) 
        else 
          setBitFieldValueForRow(ARow, OctalStr2Dword(Value,16)) ; // edit_value ist aktualisiert
        UpdateValueEditText ; // das TEdit aktualsieren
      end ; 
    end; 
  end; 

// Wird während des Eintippens aufgerufen
procedure TFormBitfields.ValueEditChange(Sender: TObject); 
  var 
    Value, s: string ; 
    i: integer ; 
  begin 
    if editChangeEventsEnabled then begin 

      // eingegebenen Value an die memorycell weiterleiten
      // nur gültige Octalziffern durchlassen
      // '?' erlauben, als ungültige Zahl (macht das Grid intern)
      Value := ValueEdit.Text ; 
      s := '' ; 
      for i := 1 to Length(Value) do 
        if CharInSet(Value[i], ['0'..'7']) then
          s := s + Value[i] 
        else if Value[i] = '?' then begin 
          s := '?' ; 
          break ; 
        end; 

      if s = '' then 
        memorycell.edit_value := MEMORYCELL_ILLEGALVAL // kann während EEdit vorkommen
      else 
        memorycell.edit_value := OctalStr2Dword(s, 16) ; 

      // Wertänderung durch gelbes Fenster
      if memorycell.edit_value <> memorycell.pdp_value then 
        ValueEdit.Color := ColorGridCellChangedBkGnd 
      else 
        ValueEdit.Color := clWindow ; 

      UpdateBitfieldsText ; // die Bitfield aktualisieren
    end { "if editChangeEventsEnabled" } ; 
  end{ "procedure TFormBitfields.ValueEditChange" } ; 



procedure TFormBitfields.ValueEditKeyPress(Sender: TObject; var Key: char); 
  begin 
    // nur Octalziffern als Eingabe. auch das backspace #8 erlauben
    if not CharInSet(Key, [#8, '0'..'7']) then 
      Key := #0 ; 
  end; 


// wird von der memorycellgroup aufgerufen, wenn sich eine zelle spontan ändert
procedure TFormBitfields.MemoryCellChange(Sender { = memorycellgroup}: TObject; memorycell: TMemoryCell) ; 
  begin 
    memorycell.edit_value := memorycell.pdp_value ; 
    UpdateDisplay ; 
  end ; 


// Bitfields im grid aktualisieren
procedure TFormBitfields.UpdateBitfieldsText ; 
  var i: integer ; 
    bfd : TBitFieldDef ; 
  begin 
    editChangeEventsEnabled := false ; // keine Rekursion ValueEdit->BitfieldEdits->ValueEdit ...

    if bitfieldsdef <> nil then begin 
      // Definitionen da: Grid füllen
      for i := 0 to bitfieldsdef.bitfields.Count - 1 do begin 
        bfd := bitfieldsdef.bitfields.Items[i] as TBitFieldDef ; 
        BitfieldsStringGrid.Cells[0, i+1] := ' ' + bfd.name ; 
        if bfd.bit_lo = bfd.bit_hi then 
          BitfieldsStringGrid.Cells[1, i+1] := Format(' %d', [bfd.bit_hi]) 
        else 
          BitfieldsStringGrid.Cells[1, i+1] := Format(' %d:%d', [bfd.bit_hi,bfd.bit_lo]) ; 

        BitfieldsStringGrid.Cells[2, i+1] := ' ' + Dword2OctalStr( 
                bfd.getMask(true, bfd.bit_hi, bfd.bit_lo), 16) ; 
        BitfieldsStringGrid.Cells[3, i+1] := ' ' + Dword2OctalStr( 
                bfd.getFieldInValue(memorycell.edit_value), 
                bfd.bit_hi - bfd.bit_lo + 1 
                ) ; 
        BitfieldsStringGrid.Cells[4, i+1] := ' ' + 
                Dword2OctalStr((bfd.getMask(false, bfd.bit_hi, bfd.bit_lo)), 
                bfd.bit_hi - bfd.bit_lo+1) ; 
        BitfieldsStringGrid.Cells[5, i+1] := ' ' + bfd.info ; 
      end{ "for i" } ; 
    end{ "if bitfieldsdef <> nil" } ; 
    editChangeEventsEnabled := true ; 
  end{ "procedure TFormBitfields.UpdateBitfieldsText" } ; 


// das ValueEdit aktualsieren
procedure TFormBitfields.UpdateValueEditText ; 
  begin 
    editChangeEventsEnabled := false ; // keine Rekursion ValueEdit->BitfieldEdits->ValueEdit ...

    // Wertänderung durch gelbes Fenster
    if memorycell.edit_value <> memorycell.pdp_value then 
      ValueEdit.Color := ColorGridCellChangedBkGnd 
    else 
      ValueEdit.Color := clWindow ; 
    ValueEdit.Text := Dword2OctalStr(memorycell.edit_value, 16) ; 
    editChangeEventsEnabled := true ; 
  end; 

// alles aktualsieren
procedure TFormBitfields.UpdateDisplay ; 
  begin 
    AddrEdit.Text := Addr2OctalStr(memorycell.addr) ; 

    UpdateValueEditText ; 

    if bitfieldsdef <> nil then 
      UpdateBitfieldsText ; 

  end; 


// nur die erste Cell der Group wird angezeigt
procedure TFormBitfields.ConnectToMemoryCell(mcg: TMemoryCellGroup) ; 
  begin 
    memorycellgroup := mcg ; 
    memorycell := mcg.Cell(0) ; 
    // callback bei Zellenänderung
    mcg.OnMemoryCellChange := MemoryCellChange ; 

    // angezeigte Bits sind ungültig, grid weg.
    // durch "ShowNewAddr()" kommt das Display wieder
    bitfieldsdef := nil ; 
    BitfieldsStringGrid.Hide ; 

    UpdateDisplay ; 
  end{ "procedure TFormBitfields.ConnectToMemoryCell" } ; 


procedure TFormBitfields.ShowNewAddr(mc: TMemoryCell) ;

function textextent(s:string): TSize ;
begin
BitfieldsStringGrid.Canvas.Font.Assign(BitfieldsStringGrid.Font) ;
result := BitfieldsStringGrid.Canvas.TextExtent(s) ;
end;

  var 
    bitfieldsdefs : TBitfieldsDefs ; 
    s : string ; 
  begin 

    if mc = memorycell then // First initalization after create
      Caption := setFormCaptionInfoField(Caption, '') 
    else begin 
      // Caption ist "Group.name, also "CPU.PSW" oder "RL11.CSW"
      s := '' ; 
      if (mc.memorycellgroup <> nil) and (mc.memorycellgroup.groupname <> '') then
        s := mc.memorycellgroup.groupname + ' . ' ; 
      if mc.name = '' then 
        s := s + Addr2OctalStr(mc.addr) // kein Name da: adresse anzeigen
      else s := s + mc.name ; 
      Caption := setFormCaptionInfoField(Caption, s) ; 
    end; 

    memorycell.Assign(mc); 

    AddrEdit.Text := Addr2OctalStr(memorycell.addr) ; 
    // NameLabel.Caption := '"' + memorycell.name + '"' ;
    InfoLabel.Caption := 'Info : ' + memorycell.info ; 
    ValueEdit.Text := Dword2OctalStr(memorycell.pdp_value, 16) ; 

    // die Bitfeld-Definitionen werden von den Groups verwaltet
    bitfieldsdefs := (memorycellgroup.collection as TMemoryCellGroups).bitfieldsdefs ; 
    bitfieldsdef := bitfieldsdefs.BitFieldsDefByAddr(memorycell.addr) ; 
    if bitfieldsdef = nil then begin 
      // keine Definitionen da: Hide Grid
      BitfieldsStringGrid.Hide ;
      // jetzt ist der Hintergrund sichtbar: "no bit field definitions loaded"
      ClientHeight := PanelT.Height + 50 ; // etwas Platz für Background
    end else begin 
      // hat die cell keine Infom wird der bitdef-name genommen
      if memorycell.info = '' then 
        InfoLabel.Caption := 'Info : ' + bitfieldsdef.name ; 
      with BitfieldsStringGrid do begin
        DefaultRowHeight := textextent('Xg').Height ;
        RowCount := 1 + bitfieldsdef.bitfields.Count ;
        ColCount := 6 ;
        Cells[0,0] := 'Name' ;
        ColWidths[0] := textextent('X ABCDEFGH').Width ; // 150 ; deafult: 10 chars
        Cells[1,0] := 'Bits' ;
        ColWidths[1] := textextent('X 15:10').Width ;// 35 ;
        Cells[2,0] := 'Mask' ;
        ColWidths[2] := textextent('X 000000').Width ;// 45 ; 16bit val
        Cells[3,0] := 'Value' ;
        ColWidths[3] := textextent('X 000000').Width ;// 45 ; 16bit val
        Cells[4,0] := 'Max' ;
        ColWidths[3] := textextent('X 000000').Width ;// 45 ; 16bit val
        Cells[5,0] := 'Info' ;
        ColWidths[4] := 100 ;

        FixedRows := 1; 
        FixedCols := 0; 

        BitfieldsStringGrid.Show ; 
        // die letzte Spalte geht bis ans Formende
        FormResize(nil); 

        // Formhoehe so einstellen, dass Grid genau alle Zeilen anzeigt
        self.ClientHeight := PanelT.Height + RowCount * (1+DefaultRowHeight) + 3 ; 

      end { "with BitfieldsStringGrid" } ; 
    end{ "if bitfieldsdef = nil ... ELSE" } ; 

    // die neuen ColWidhts laden.
    FormGridLoadColWidths(self.name, BitfieldsStringGrid) ; 

    UpdateDisplay ; 
  end{ "procedure TFormBitfields.ShowNewAddr" } ; 


end{ "unit FormBitfieldsU" } . 
