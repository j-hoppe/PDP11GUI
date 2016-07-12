unit FormIoPageScannerU;
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
  Dialogs, FrameMemoryCellGroupListU, ExtCtrls, StdCtrls,
  FormChildU,
  MemoryCellU,
  FormBusyU ;

type

  TIOPageScannerState =  (
      iopsEmpty, // noch nie ein scan
      iopsScanning,
      iopsReady, // Ergebnisse da
      iopsUserStop // User will stop
    ) ;


  TFormIopageScanner = class(TFormChild)
      PanelT: TPanel;
      MemoryList: TFrameMemoryCellGroupList;
      StartScanButton: TButton;
      Label1: TLabel;
      Splitter1: TSplitter;
      IoPageMachineDescriptionMemo: TMemo;
      ExamineCurrentButton: TButton;
      ExamineAllButton: TButton;
      DepositChangedButton: TButton;
      procedure StartScanButtonClick(Sender: TObject);
      procedure UserStopButtonClick(Sender: TObject);
      procedure FormCreate(Sender: TObject);
      procedure ExamineCurrentButtonClick(Sender: TObject);
      procedure ExamineAllButtonClick(Sender: TObject);
      procedure DepositChangedButtonClick(Sender: TObject);
      procedure DepositAllButtonClick(Sender: TObject);
    private
      { Private-Deklarationen }
    public
      { Public-Deklarationen }
      mcg: TMemoryCellGroup; // wird zum scannnen verwendet

      TheState : TIOPageScannerState ;

      procedure UpdateDisplay ;
      procedure ConnectToMemoryCells(mcg: TMemoryCellGroup);

    end{ "TYPE TFormIopageScanner = class(TFormChild)" } ;


implementation

{$R *.dfm}

uses
  AuxU,
  AddressU,
  OctalConst,
  ConsoleGenericU,
  FormMainU ;


procedure TFormIopageScanner.FormCreate(Sender: TObject);
  begin
    with IoPageMachineDescriptionMemo.Lines do begin
      Clear ;
      Add('; After scan, this memo will show the new I/O page addresses.') ;
      Add('; You can mark parts of this output with the mouse') ;
      Add('; and Copy/Paste it to your machine description file.') ;
    end;

    TheState := iopsEmpty ;
    UpdateDisplay ;
  end;


procedure TFormIopageScanner.UpdateDisplay ;
  begin
    if (TheState = iopsScanning) and BusyForm.Aborted then begin
      TheState := iopsUserStop ;
      BusyForm.Close ;
    end;

    case TheState of
      iopsEmpty: begin
        StartScanButton.Enabled := true ;
        ExamineCurrentButton.Enabled := false ;
        ExamineAllButton.Enabled := false ;
        DepositChangedButton.Enabled := false ;
      end ;
      iopsScanning: begin
        StartScanButton.Enabled := false ;
        ExamineCurrentButton.Enabled := false ;
        ExamineAllButton.Enabled := false ;
        DepositChangedButton.Enabled := false ;
      end ;
      iopsReady:begin
        StartScanButton.Enabled := true ;
        ExamineCurrentButton.Enabled := true ;
        ExamineAllButton.Enabled := true ;
        DepositChangedButton.Enabled := true ;
      end ;
      iopsUserStop:begin //bewirkt kein Display
      end ;
    end{ "case TheState" } ;
    MemoryList.UpdateDisplay ;
  end{ "procedure TFormIopageScanner.UpdateDisplay" } ;


procedure TFormIopageScanner.StartScanButtonClick(Sender: TObject);
  const
//    iopageStartAddr = _17770000 ;
//    iopageStartAddr = _17760000 ;
    iopageSize = 8192 ;// _20000000 - iopageStartAddr ;
  var
//    mat: TMemoryAddressType ;
    iopagebase: dword ;
    i, j: dword ;
    n: dword ;
    mc: TMemoryCell ;
    symbol_mc: TMemoryCell ;
    i_blockstart: integer ;
    mc_blockstart: TMemoryCell ;
    block_size_words: integer ;
    block_startaddr_s: string ;
    block_endaddr_s: string ;
  begin { "procedure TFormIopageScanner.StartScanButtonClick" }
    IoPageMachineDescriptionMemo.Lines.Clear ;

    if not (cfNonFatalUNIBUStimeout in FormMain.PDP11Console.getFeatures) then begin
      MessageDlg('To execute the I/O page scanner, your PDP-11 must continue'+#13
              +'after an invalid bus address is accessed (UNIBUS timeout).'+#13
              +'This machine stops on UNIBUS timeout, the I/O page scanner can not run!', mtError,
              [mbOk], 0) ;
      Exit ;
    end;

    iopagebase := PhysicalIopageBaseAddr(mcg.mat) ;

//      assert(iopageSize = 8192) ;
    TheState := iopsScanning ;
    UpdateDisplay ;

    // 1) memory cells für alle io-adressen anlegen
    n := iopageSize div 2; // nur gerade adressen

    mcg.Clear ; // alle adressen löschen
    for i := 0 to n-1 do
      mcg.Add(iopagebase + 2*i) ;
    mcg.calcAddrRange ;
    mcg.Invalidate ;

    BusyForm.Start('Scanning IO range ...', mcg.Count, true) ;
    try
      Application.ProcessMessages ;

      // 2) examine all, aber nicht list-optimiert, sondern einzel-Zugriffe
      // grund: sehr viele Adressen, sehr viele ungültige Adressen
      for i := 0 to n-1 do begin
        if TheState = iopsScanning then begin
          mc := mcg.Cell(i) ;
          Caption := setFormCaptionInfoField(Caption, Format('scanning %d%%', [100 * i div (n-1)])) ;
          Application.ProcessMessages ;
          mc.Examine;
          BusyForm.StepIt ; if BusyForm.Aborted then Break ;

          mc.edit_value := mc.pdp_value ; // anzeigen
        end;
      end;

      // 3) undefinierte adressen löschen
      i := 0 ;
      while i < mcg.Count do begin
        if mcg.Cell(i).pdp_value = MEMORYCELL_ILLEGALVAL then
          mcg.Delete(i)
        else inc(i) ;
      end;

      // 4) gefundene Adressen mit Symbolinfo versehen
      // 4.1 check, ob in machine definition file beschrieben
      for i := 0 to mcg.Count - 1 do begin
        mc := mcg.Cell(i) ;
        // gibt es die cell mit symbolinfo in irgendeiner anderen cellgroup?
        symbol_mc := (mcg.Collection as TMemoryCellGroups).getSymbolInfoCell(mc) ;
        if symbol_mc <> nil then begin
          // register namen mit device (= memorycellgroup) prefixen
          mc.name := symbol_mc.memorycellgroup.groupname + '.' + symbol_mc.name ;
          mc.info := symbol_mc.info ;
        end;
      end;

      // 4.2 wenn nicht: zusammenhängende Blöcke erkennen, so tun, als ob
      // es devices wären.
      // dabei alle schon bekannten memory cells ignorieren!
      i := 0 ;
      while (i < mcg.Count ) do begin
        // nächster Block ab mc[i]
        i_blockstart := i ; // _blockend+1 ;
        mc_blockstart := mcg.Cell(i_blockstart) ;
        if mc_blockstart.name <> '' then begin
          inc(i) ; // Skip known Memorycells
          Continue ;
        end;

        // Ende des Blocks finden
        while (i < mcg.Count )
                and (mcg.Cell(i).name = '') // block end at known memory cell
                and (mcg.Cell(i).addr.val = (mc_blockstart.addr.val + 2*(i-i_blockstart)))
                do begin
          inc(i) ;
        end;
        // Garantie: im Block sind jetzt nur Memorycells, die noch nicht im
        // Maschine desription file vorkommen.

        // Zellen im Block kommentieren
        block_startaddr_s := Dword2OctalStr( // auf 16 bit physical umrechnen
                mc_blockstart.addr.val - iopagebase + _160000,
                16) ;
        block_size_words := i - i_blockstart ;
        block_endaddr_s := Dword2OctalStr( // auf 16 bit physical umrechnen
                mc_blockstart.addr.val + 2* (block_size_words-1) - iopagebase + _160000,
                16) ;
        for j := 0 to block_size_words - 1 do begin
          mc := mcg.Cell(i_blockstart + j) ;
          mc.name := Format('device_%s.reg_%s', [block_startaddr_s,
                  Dword2OctalStr(2*j, 0)]) ;
          mc.info := Format('device base at %s, word #%d, octal offset +%s',
                  [block_startaddr_s, j, Dword2OctalStr(2*j, 0)]) ;
        end ;

        // 4.3) Block beschreibung in das result Memo
        with IoPageMachineDescriptionMemo.Lines do begin
          Add('') ;
          Add(Format('[Device_%s]', [block_startaddr_s])) ;
          Add(Format('Info=register block at %s with %d words len.',
                  [block_startaddr_s, block_size_words])) ;
          // Add('; enable this entry with "Enabled=1" or "Enabled=true".') ;
          Add('Enabled=true') ;

          if block_size_words <= 16 then begin
            // wenn es wenige Register sind: alle einzeln listen
            for j := 0 to block_size_words - 1 do begin
              mc := mcg.Cell(i_blockstart + j) ;
              Add(Format('Register_%s=%s;"%s"', [
                      Dword2OctalStr(2*j, 0),
                      // addressen auf 16 bit I/O page umrechnen
                      Dword2OctalStr(mc.addr.val - iopagebase + _160000, 0),
                      mc.info])) ;
            end;
          end else begin
            // sonst ROM?: mit von-bis Syntax
            Add(Format('Registers=%s:%s;"%d consecutive words ... ROM or RAM?"', [
                    block_startaddr_s, block_endaddr_s, block_size_words])) ;
          end ;
        end{ "with IoPageMachineDescriptionMemo.Lines" } ;
        Application.ProcessMessages ;
      end{ "while (i < mcg.Count )" } ;


      // 5) Anzeigen
      MemoryList.ConnectToMemoryCellGroup(mcg);

    finally
      BusyForm.Close ;
      TheState := iopsReady ;
      UpdateDisplay ; // may set state to aborted

      // ruhig den Scanzustand zusammen mit dem Ergebnis stehen lassen
      // Caption := setFormCaptionInfoField(Caption, '') ;
    end{ "try" } ;

  end{ "procedure TFormIopageScanner.StartScanButtonClick" } ;


procedure TFormIopageScanner.UserStopButtonClick(Sender: TObject);
  begin
    TheState := iopsUserStop ;
  end;


procedure TFormIopageScanner.ConnectToMemoryCells(mcg: TMemoryCellGroup);
  begin
    self.mcg := mcg ; // auch in der Form merken
    MemoryList.ConnectToMemoryCellGroup(mcg);
    UpdateDisplay ;
  end;


procedure TFormIopageScanner.DepositAllButtonClick(Sender: TObject);
  begin
    MemoryList.DepositAllButtonClick(Sender);
  end;


procedure TFormIopageScanner.DepositChangedButtonClick(Sender: TObject);
  begin
    MemoryList.DepositChangedButtonClick(Sender);
  end;


procedure TFormIopageScanner.ExamineAllButtonClick(Sender: TObject);
  begin
    MemoryList.ExamineAllButtonClick(Sender);
  end;


procedure TFormIopageScanner.ExamineCurrentButtonClick(Sender: TObject);
  begin
    MemoryList.ExamineCurrentButtonClick(Sender);
  end;


end{ "unit FormIoPageScannerU" } .
