unit FormDisasU;
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
In der Maske: Start und len frei wählbar

Wenn Aufruf durch Form Execution  über showExecutionLine():
len auf 10, Start so setzen, dass PC mittig im listing

}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  FormChildU,
  JvExControls, JvEditorCommon, JvEditor, ExtCtrls, StdCtrls,
  JH_Utilities,
  AddressU,
  MemoryCellU ;

type
  TFormDisas = class(TFormChild)
      Panel1: TPanel;
      DisasSourceEditor: TJvEditor;
      SetAddressButton: TButton;
      StartAddrEdit: TEdit;
      Label1: TLabel;
      DecAddrButton: TButton;
      IncAddrButton: TButton;
      UseCacheCheckBox: TCheckBox;
      Label2: TLabel;
      EndAddrEdit: TEdit;
      procedure SetAddressButtonClick(Sender: TObject);
      procedure AnyAddrEditKeyPress(Sender: TObject; var Key: Char);
      procedure DisasSourceEditorResize(Sender: TObject);
      procedure FormShow(Sender: TObject);
      procedure DecAddrButtonClick(Sender: TObject);
      procedure IncAddrButtonClick(Sender: TObject);
      procedure UseCacheCheckBoxClick(Sender: TObject);
    private
      { Private-Deklarationen }
      disas_pcaddr_window_size: dword ; // soviele adressen vor und nach dem PC anzeigen
      procedure Disassemble(aStartaddr, aEndaddr, aPcaddr: TMemoryAddress ;
              useCodeCache: boolean ;
              var sourcelines: TStringList ; var pcline: integer) ;
      procedure showExecutionLine(n:integer) ;

      procedure CheckInput ;
      procedure UpdateDisplay ;
      procedure ExamineAll(useCodeCache: boolean) ;
      procedure memoryCellChange(Sender { = memorycellgroup}: TObject; memorycell: TMemoryCell) ;

    public
      { Public-Deklarationen }

      StartAddr: TMemoryAddress ; // Startadresse des Listings
      EndAddr: TMemoryAddress ; // Endadresse des Listings
      CodeAddr: TMemoryAddress ; // virtuelle codeadresse  im Listingbereiech

      // Codeauszug um den PC herum
      Memorycellgroup : TMemoryCellGroup ;

      constructor Create(AOwner: TComponent) ; override ;
      destructor Destroy ; override ;

      procedure ConnectToMemoryCells(mcg: TMemoryCellGroup) ;
      procedure ShowNewPcAddr(newaddr_v: TMemoryAddress) ;
    end{ "TYPE TFormDisas = class(TFormChild)" } ;


implementation

{$R *.dfm}

uses
  AuxU,
  RegistryU
  ;

// disassembler
(**)
procedure Disas11 (
        srcbuff: PAnsiChar;        // der Rückgabepuffer
        srcbuff_size: integer ;
        coremem: PAnsiChar ; // array of byte, 65K
        coremem_valid: PAnsiChar; // array of flags. 1 for valid memory cells
        coremem_size: integer
        )
        cdecl ; // if dll function uses "stdcall", it gets a decoreated name
//        stdcall ;
        external 'PDP11DISAS.DLL' ;
(**)

(** )
// disable for aqtime analysis
procedure Disas11 (
        srcbuff: PChar;        // der Rückgabepuffer
        srcbuff_size: integer ;
        coremem: PChar ; // array of byte, 65K
        coremem_valid: PChar; // array of flags. 1 for valid memory cells
        coremem_size: integer
        ) ;
        begin
        end ;
(**)


constructor TFormDisas.Create(AOwner: TComponent) ;
  begin
    inherited ;
    disas_pcaddr_window_size := 10 ;
    StartAddr.mat := matVirtual ;
    StartAddr.val := 0 ;
    EndAddr.mat := matVirtual ;
    EndAddr.val := 0 ;
    CodeAddr.mat := matVirtual ;
    CodeAddr.val := MEMORYCELL_ILLEGALVAL ;

    TheRegistry.Load(UseCacheCheckBox) ;
    TheRegistry.Load(StartAddrEdit) ;
    TheRegistry.Load(EndAddrEdit) ;
    try
      StartAddr := OctalStr2Addr(StartAddrEdit.Text,matVirtual) ;
    except
      StartAddr := OctalStr2Addr('0',matVirtual) ;
    end;
    try
      EndAddr := OctalStr2Addr(EndAddrEdit.Text,matVirtual) ;
    except
      EndAddr := OctalStr2Addr('0',matVirtual) ;
    end;
    CheckInput ;

    // Farben für die markierten Zeilen
    DisasSourceEditor.LineInformations.DebugPointColor := ColorCodeExecutionPositionBkGnd ;
    DisasSourceEditor.LineInformations.DebugPointTextColor:= ColorCodeExecutionPositionText ;
  end{ "constructor TFormDisas.Create" } ;


destructor TFormDisas.Destroy ;
  begin
    inherited ;
  end;


procedure TFormDisas.FormShow(Sender: TObject);
  begin
    // beim aufpoppen: update
    ExamineAll(UseCacheCheckBox.Checked) ; // immer mit optimierung
    UpdateDisplay ;
  end;


procedure TFormDisas.AnyAddrEditKeyPress(Sender: TObject; var Key: Char);
  begin
    // nur Octalziffern als Eingabe. auch das backspace #8 erlauben
    if not CharInSet(Key, [#8, '0'..'7']) then
      Key := #0 ;
  end;


// Die gerade ausgeführte Zeile markieren
procedure TFormDisas.showExecutionLine(n:integer) ;
  var i: integer ;
  begin
    dec(n) ; // Lines[] ab 0 !
    with DisasSourceEditor do
      for i := 0 to Lines.Count-1 do
        if i = n then
          LineInformations.SelectStyle[i] := lssDebugPoint
        else LineInformations.SelectStyle[i] := lssUnselected ;
  end;


// Speicherbereich in Source umwandeln
// input: startaddr: Anfang  des zu disassemblenden Speicherbereich.
//    Länge wird durch MemoryCellgroup.Count vorgegeben
// useCodeCache: wenn möglich, schon bekannten code nicht neu aus maschine in die memortycellgroup laden.
// aStartAddr, aEndAddr: Einschränkung des disassembierten Bereichs (fine tuning)
// aPcaddr: Position des Program counters
// Rückgabe: in sourcelines: assembler code
// pcline: zeile mit program counter in sourcelines
//    -1, wenn not found
procedure TFormDisas.Disassemble(aStartaddr, aEndaddr, aPcaddr: TMemoryAddress ;
        useCodeCache: boolean ;
        var sourcelines: TStringList ; var pcline: integer) ;
  const
    srcbuff_size = 100000 ; // max 100k listing
    coremem_size = $10000 ; // 64k
  var
    i, j: integer ;
    srcbuff: array[0..srcbuff_size] of AnsiChar ;
    coremem: array[0..coremem_size] of AnsiChar ;  // 64k virtueller Speicher
    coremem_valid: array[0..coremem_size] of AnsiChar ;
    mc: TMemoryCell ;
    s: string ;
  begin
    assert(aStartaddr.mat = matVirtual) ;
    assert(aEndaddr.mat = matVirtual) ;
    assert(aPcaddr.mat = matVirtual) ;

    // Memoryzellen in Coremem-Array schreiben
    for i := 0 to coremem_size - 1 do // erstmal allen Speicher als ungültig markieren
      coremem_valid[i] := #0 ;

    for i := 0 to Memorycellgroup.Count - 1 do begin
      mc := Memorycellgroup.Cell(i) ;
      if (mc.addr.val >= aStartaddr.val) and (mc.addr.val <= aEndaddr.val) then begin

        // hier virtual to physical wandeln
        // pdp ist hat lsb auf kleinerer Adresse
        coremem[mc.addr.val] :=  AnsiChar(mc.pdp_value and $ff) ;
        coremem[mc.addr.val+1] := AnsiChar((mc.pdp_value shr 8) and $ff) ;
        coremem_valid[mc.addr.val] := #1 ;
        coremem_valid[mc.addr.val+1] := #1 ;
      end;
    end;

    // Umwandlung aufrufen
    ZeroMemory(@srcbuff, srcbuff_size);

    Disas11 (srcbuff, srcbuff_size, coremem, coremem_valid, coremem_size) ;
    sourcelines.Text := srcbuff ;
//    sourcelines.Text := 'Sorry, Disas11 disabled!' ;

    // PC-Position finden. format: "<addr>:"
    pcline := -1 ;
    if aPcaddr.val <> MEMORYCELL_ILLEGALVAL then
      for i := 0 to  sourcelines.Count - 1 do begin
        j := pos(':', sourcelines[i]) ;
        if (j > 0) and (j < 10)  then begin // das ':' muss ganz links stehen
          s := Copy(sourcelines[i], 1, j-1) ; // extract addr
          try
            if aPcaddr.val = OctalStr2Addr(s,matVirtual).val then
              pcline := i+1 ; // save linenumer
          except
          end;
        end;
      end;
  end{ "procedure TFormDisas.Disassemble" } ;


procedure TFormDisas.DisasSourceEditorResize(Sender: TObject);
  begin
    Invalidate ; // zeichnet sich sonst nicht richtig neu
  end;


procedure TFormDisas.CheckInput ;
  begin
    if StartAddr.val > EndAddr.val then EndAddr.val := StartAddr.val ;
  end;


// Diassembler-Anzeige updaten
procedure TFormDisas.UpdateDisplay ;
  var
    sourcelines: TStringList ;
    codeline: integer ; // Zeile im Listing, das "codeaddr" enthält. -1, wenn not found
    visibleStartAddr: TMemoryAddress ; // Startadresse des sichtabren Listings
  begin
    sourcelines := TStringList.Create ;
    try

      // Immer Neuanzeige, aber nicht immer neu "Examine"!

      // direkt nach .Create ist der State esUnknown.

      // CodeAddr kann INVALID sein: ZB bei M9312 console emualtor
      visibleStartAddr := StartAddr ;
      Disassemble(visibleStartAddr, EndAddr, CodeAddr, UseCacheCheckBox.Checked, sourcelines, codeline) ;
      // wenn der PC (CodeAddr) im Listing nicht aufzufinden ist:
      // (zB weil Binärdaten davor als langer Befehl interpretiert werden)
      // start adresse schrittweise um zwei erhöhen, nochmal probieren
      if CodeAddr.val <> MEMORYCELL_ILLEGALVAL then
        while (codeline < 0) and (visibleStartAddr.val < CodeAddr.val) do begin
          visibleStartAddr.val := visibleStartAddr.val+2 ;
          Disassemble(visibleStartAddr, EndAddr, CodeAddr, UseCacheCheckBox.Checked, sourcelines, codeline) ;
        end;

      DisasSourceEditor.Lines.Assign(sourcelines) ;

      // PC-Marker setzen
      showExecutionLine(codeline);

      StartAddrEdit.Text := Addr2OctalStr(StartAddr) ;
      TheRegistry.Save(StartAddrEdit); // hier geprüfte Werte
      EndAddrEdit.Text := Addr2OctalStr(EndAddr) ;
      TheRegistry.Save(EndAddrEdit);

      // Caption := setFormCaptionInfoField(Caption, 'addr = ' + CodeAddrEdit.Text) ;

    finally
      sourcelines.Free ;
    end{ "try" } ;
  end{ "procedure TFormDisas.UpdateDisplay" } ;


// Speicher ggf neu abfragen und Disassembly anzeigen.
procedure TFormDisas.ExamineAll(useCodeCache: boolean) ;
  var
    n: integer ;
  begin
    assert(StartAddr.mat = matVirtual) ;
    assert(EndAddr.mat = matVirtual) ;

    // hier optimierung: memorycellgroup verschieben,
    // un überlappende Addesse nicht neu laden
    // ODER: alles neu laden

    n := (EndAddr.val - StartAddr.val) div 2 + 1 ; // calc new address count
    Memorycellgroup.ShiftRange(StartAddr, n, {optimize=}useCodeCache) ;
    Memorycellgroup.Examine({optimize=}useCodeCache, true) ; // long list
  end{ "procedure TFormDisas.ExamineAll" } ;



procedure TFormDisas.UseCacheCheckBoxClick(Sender: TObject);
  begin
    TheRegistry.Save(UseCacheCheckBox) ;
  end;


// für externen Zugriff: Anzeige für bestimmte Zieladresse aufbauen
// newaddr: virtuelle Adresse
procedure TFormDisas.ShowNewPcAddr(newaddr_v: TMemoryAddress) ;
  begin
    // Memoryzellen um den PC herum laden.
    CodeAddr := newaddr_v ;
    if CodeAddr.val = MEMORYCELL_ILLEGALVAL then
      Exit ;  // zB M9312 console emulator kennt den PC nicht: dann zeige ihn auch nicht an,
    // und verändere den angezeigten Adressbereich nicht

    // Dargestellten Bereich an Fenster um PC herum anpassen

    // Beginn des Adressbereichs kann an 0 anstossen
    StartAddr.mat := matVirtual ;
    if CodeAddr.val < (2 * disas_pcaddr_window_size) div 2  then
      StartAddr.val := 0
    else
      StartAddr.val := CodeAddr.val - (2 * disas_pcaddr_window_size) div 2 ;
    EndAddr.val := StartAddr.val + (2 * disas_pcaddr_window_size) ;

    // Fenster wird bei jedem Stop aktualisiert. Man muss es aber abschalten können
    if Visible then begin
      ExamineAll(UseCacheCheckBox.Checked) ;
      UpdateDisplay ;
    end;
  end{ "procedure TFormDisas.ShowNewPcAddr" } ;


procedure TFormDisas.DecAddrButtonClick(Sender: TObject);
  begin
    if StartAddr.val > 0 then begin
      StartAddr.val := StartAddr.val - 2 ;
      EndAddr.val := EndAddr.val - 2 ;
      CodeAddr.val := MEMORYCELL_ILLEGALVAL ; // PC nicht anzeigen, greift in Addressbereich ein
      CheckInput ;
      ExamineAll(true) ; // immer mit optimierung
      UpdateDisplay ;
    end;
  end;


procedure TFormDisas.IncAddrButtonClick(Sender: TObject);
  begin
    if EndAddr.val < $fffe then begin
      StartAddr.val := StartAddr.val + 2 ;
      EndAddr.val := EndAddr.val + 2 ;
      CodeAddr.val := MEMORYCELL_ILLEGALVAL ; // PC nicht anzeigen, greift in Addressbereich ein
      CheckInput ;
      ExamineAll(true) ; // immer mit optimierung
      UpdateDisplay ;
    end;
  end;


procedure TFormDisas.SetAddressButtonClick(Sender: TObject);
  begin
    StartAddr := OctalStr2Addr(StartAddrEdit.Text,matVirtual) ;
    EndAddr := OctalStr2Addr(EndAddrEdit.Text,matVirtual) ;
    CodeAddr.val := MEMORYCELL_ILLEGALVAL ; // PC nicht anzeigen, greift in Addressbereich ein
    CheckInput ;
    ExamineAll(UseCacheCheckBox.Checked) ;
    UpdateDisplay ;
  end;


// die erste Cell der Group ist der PC
procedure TFormDisas.ConnectToMemoryCells(mcg: TMemoryCellGroup) ;
  var n: integer ;
  begin
    assert(mcg.mat = matVirtual) ;

    Memorycellgroup := mcg ;
    // es werden 5 Adressen davor und 5 danach angezeigt
    n := 2 * disas_pcaddr_window_size + 1 ;
    while Memorycellgroup.Count < n do
      Memorycellgroup.Add(0) ; // adressen werden erst nach PC-Stop festgelegt

    // callback bei Zellenänderung
    mcg.OnMemoryCellChange := memoryCellChange ;

  end{ "procedure TFormDisas.ConnectToMemoryCells" } ;


// wird von der memorycellgroup aufgerufen, wenn sich eine Zelle spontan ändert
procedure TFormDisas.memoryCellChange(Sender { = memorycellgroup}: TObject; memorycell: TMemoryCell) ;
  begin
    memorycell.edit_value := memorycell.pdp_value ;
    if Visible then
      // Achtung: wenn von einem anderen Fenster ein ganzer programblock geladen wird
      // wird dieses callback für jede einzelne Zelle aufgerufen.
      // daher nicht "UpdateDisplay" (was wiederum alle Zellen abfragen würde).
      // Aber: neues Disassembly!
      UpdateDisplay ;
  end ;



end{ "unit FormDisasU" } .
