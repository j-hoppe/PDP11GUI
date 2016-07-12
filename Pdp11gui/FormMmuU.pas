unit FormMmuU;
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

{ visualiert die interne MMU der Console
  (Pdp11Console.MMU)
}
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  FormChildU,
  JvExGrids, JvStringGrid,
  MemoryCellU,
  Pdp11MMuU, Grids ;

type
  TFormMMU = class(TFormChild)
      PageControl1: TPageControl;
      DataSpaceTabSheet: TTabSheet;
      InstructionSpaceTabSheet: TTabSheet;
      Panel1: TPanel;
      RefreshButton: TButton;
      Label1: TLabel;
      CpuModeEdit: TEdit;
      SpecialInfoLabel: TLabel;
      DataSpaceStringGrid: TJvStringGrid;
      InstructionSpaceStringGrid: TJvStringGrid;
      procedure RefreshButtonClick(Sender: TObject);
    private
      { Private-Deklarationen }
    public
      { Public-Deklarationen }
      MMU: TPDP11MMU ; // Memory Managment Unit, die visualisiert werden soll
      constructor Create(AOwner: TComponent) ; override ;
      destructor Destroy ; override ;

      procedure UpdateDisplay ;
      procedure MMUChanged(Sender: TObject) ;
    end{ "TYPE TFormMMU = class(TFormChild)" } ;


implementation

{$R *.dfm}

uses
  AuxU,
  AddressU ;

constructor TFormMMU.Create(AOwner: TComponent) ;
  begin
    inherited ;
    MMU := nil ; // muss durch Anwender gefüllt werden
  end;


destructor TFormMMU.Destroy ;
  begin
    inherited ;
  end;


procedure TFormMMU.UpdateDisplay ;

  procedure UpdateMemoryMapGrid(grid: TJvStringGrid; idmode: TPdp11MmuIDMode) ;
  function textextent(s:string): TSize ;
begin
grid.Canvas.Font.Assign(grid.Font) ;
result := grid.Canvas.TextExtent(s) ;
end;

    var
      i, n: integer ;
      blocknr: integer ;
      block_start_addr_v, block_end_addr_v, addr_v: TMemoryAddress ; // virtuelle addressen
      block_start_addr_p, block_end_addr_p, addr_p: TMemoryAddress ; // physikalische addressen
    begin
    grid.DefaultRowHeight := textextent('Xg').Height ;
      grid.ColCount := 4 ;
      grid.RowCount := 1+16 ; // 8 pages, jeweils assigned+unassigned moegl.
      grid.FixedRows := 1 ;
      grid.FixedCols := 1 ;
      grid.Cells[0, 0] := ' #' ;
      grid.Cells[1, 0] := ' virtual addr' ;
      grid.Cells[2, 0] := ' physical addr' ;
      grid.Cells[3, 0] := ' info' ;
      grid.ColWidths[0] := textextent('X 123').Width ; // 16 ; // '123'
      grid.ColWidths[1] := textextent('X 023456 .. 023456   ').Width ; //
      grid.ColWidths[2] := textextent('X 01234567 .. 01234567  ').Width ; // 120 ;
      // letzte Spalte bis Gridende
      n := 0 ;
      for i := 0 to 2 do
        n := n + 1 + grid.ColWidths[i] ;
      grid.ColWidths[3] := grid.Clientwidth - n ;

      // Addressmap ausfüllen:
      // durch alle virtuellen 16-Bit addressen iterieren
      addr_v.mat := matVirtual ; addr_v.val := 0 ;
      blocknr := 0 ; // 1. ergebnis = 1-> zur Adressierung der Gridrows geeignet
      while addr_v.val < $10000 do begin

        // Block finden, wo aufeinanderfolgende virtuelle Adressen auf
        // aufeinanderfolgende physikalische Addressen abgebildet werden.
        inc(blocknr) ;
        grid.Cells[0, blocknr] := Format('%2d', [blocknr]) ;
        block_start_addr_v := addr_v ;
        block_end_addr_v := addr_v ;
        addr_p := MMU.Virtual2Physical(addr_v, MMU.curCpuMode, idmode) ;
        block_start_addr_p := addr_p ;
        block_end_addr_p := addr_p ;
        if addr_p.val = MEMORYCELL_ILLEGALVAL then begin// nicht zugewiesenen Block scannen
          // find block end
          while (addr_v.val < $10000) and (addr_p.val = MEMORYCELL_ILLEGALVAL) do begin
            block_end_addr_v := addr_v ;
            block_end_addr_p := addr_p ;
            addr_v.val := addr_v.val + 2 ;
            addr_p := MMU.Virtual2Physical(addr_v, MMU.curCpuMode, idmode) ;
          end;
          grid.Cells[2, blocknr] := ' not assigned' ;
        end else begin
          // find block end. addr_v,_p after blockend
          while (addr_v.val < $10000)
                  and ((addr_v.val - block_start_addr_v.val) = (addr_p.val - block_start_addr_p.val)) do begin
            block_end_addr_v := addr_v ;
            block_end_addr_p := addr_p ;
            addr_v.val := addr_v.val + 2 ;
            addr_p := MMU.Virtual2Physical(addr_v, MMU.curCpuMode, idmode) ;
          end;
          grid.Cells[2, blocknr] := Format(' %s .. %s',
                  [Addr2OctalStr(block_start_addr_p), Addr2OctalStr(block_end_addr_p)]) ;
        end ;
        grid.Cells[1, blocknr] := Format(' %s .. %s',
                [Addr2OctalStr(block_start_addr_v),Addr2OctalStr(block_end_addr_v)]) ;
        grid.Cells[3, blocknr] := Format(' %d bytes', [block_end_addr_v.val - block_start_addr_v.val + 2]) ;

      end { "while addr_v.val < $10000" } ;
      grid.RowCount := 1+blocknr ; // 8 pages, jeweils assigned+unassigned moegl.
    end{ "procedure UpdateMemoryMapGrid" } ;

  var
    s: string  ;
  begin { "procedure TFormMMU.UpdateDisplay" }
    case MMU.curCpuMode of
      cpumodeKernel:     s := 'Kernel' ;
      cpumodeSupervisor: s := 'Supervisor' ;
      cpumodeUser:       s := 'User' ;
    end;

    CpuModeEdit.Text := s ;
    if not MMU.EnableRelocation then
      SpecialInfoLabel.Caption := 'Relocation disabled, virtual == physical'
    else
      if MMU.EnableDSpace[MMU.curCpuMode] then
        SpecialInfoLabel.Caption := Format('%s Data Space enabled', [s])
      else SpecialInfoLabel.Caption := Format('%s Data Space disabled and identical with Instruction Space', [s]) ;

    UpdateMemoryMapGrid(DataSpaceStringGrid, idmodeData) ;
    UpdateMemoryMapGrid(InstructionSpaceStringGrid, idmodeInstruction) ;
  end{ "procedure TFormMMU.UpdateDisplay" } ;


procedure TFormMMU.RefreshButtonClick(Sender: TObject);
  begin
    // alle MMU-relevanten Register aus der angeschlossenen PDP11 abfragen
    MMU.ExamineMMU ;
    UpdateDisplay ;
    // Umsetzungs
  end;


procedure TFormMMU.MMUChanged(Sender: TObject) ;
  begin
    // hat sich was an den Einstellungen meiner MMU geändert?
    if (Sender as TPDP11MMU) = MMU then
      UpdateDisplay ;
  end;

end{ "unit FormMmuU" } .
