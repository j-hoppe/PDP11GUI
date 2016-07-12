unit Pdp11MMUU ;
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


// Simulation der Memory Manament Unit einer PDP-11
// (11/44, 11/70).

//
// ACHTUNG: TPdp11MMU muss ständig mit dem
// CPU.PWS mode, dem MMR0, MMR3 und den PAR-Table aktualisiert werden.

// Die MMU hat für die relevanten Register eine eigene MemoryCellgroup.
// Sie führt auf Befehl aktiv ein "Examine" aus.
// Die ergebnisse leitet sie per an die MemoryCells der Oberfläche weiter
//
// Werden memoryCellgroups mit gleichen Adressen ausserhalb aktualisiert,
// wird die MMU durch TMemoryCellGroups.SyncMemoryCells() ebenfalls geändert.
//
// TPdp11MMU ist unabhängig von der Definition der MMU in der *.ini-Datei!
//
// TPdp11MMU ist ein logisches member der PDP-11-Console.
// Erst wenn die PDP-11-Version bekannt ist, kann auch die MMU parametrisiert werden.
//
// Funktioneit für 18 und 22 Bit PDP-11,
// Steuerung durch ThePhysicalAddressWidth.

interface

uses
  Windows, Classes, SysUtils,
  AddressU,
  MemoryCellU ;

type

  // je nach Run-Mode gilt eine andere memorymap
  // ordinal Werte wie in PSW<15:14>
  TPdp11MmuCpuMode = (
      cpumodeKernel,  // 0
      cpumodeSupervisor, // 1
      cpumodeIllegal, // 2
      cpumodeUser // 3
    ) ;

  // je nach Instruction fetch/data access gilt eine andere memorymap
  TPdp11MmuIDMode = (
      idmodeInstruction,
      idmodeData
    ) ;


  TPdp11Mmu = class(TObject)
    private
      // Speicherzellen der simulierten PDP-11, die relevant für die MMU sind
      memorycellgroup: TMemoryCellGroup ;

      psw_memorycell: TMemoryCell ; // CPU.PSW hat eine Sonderrolle

      // memorycells auslesen und Datenstruktur aktualisieren
      procedure evalMemoryCell(mc: TMemoryCell) ;
      procedure evalMemoryCells ;

      // wird aufgerufen, wenn PSW, oder die MMU register von aussen
      // durch TMemoryCellGroups.SyncMemoryCells() geändert werden.
      procedure MemoryCellChange(Sender: TObject ; mc: TMemoryCell) ;

    public
      //////////// MMU Zustand /////////////////
      // par[mode,i/r,page ]

      // Mode der CPU
      curCpuMode: TPdp11MmuCpuMode ;

      // Bits aus MMR0:
      EnableRelocation: boolean ; // Bit mmr0:<0>

      // Bits aus MMR3:
      // wenn für einen CPU-Mode "EnableDSpace" false ist,
      // wird immer ein Zugriff auf den IsntructionSpace durchgeführt
      EnableDSpace: array [TPdp11MmuCpuMode] of boolean ; // MMR<2:0>

      Mapping22Bit: boolean ; // MMR3<4>, not used

      UnibusRelocation: boolean ; // MMR3<5>, not used

      // die Page Adress Register bilden die eigentliche MemoryPageTable in aufbereiteter Form
      PAR: array[TPdp11MmuCpuMode, TPdp11MmuIDMode, 0..7] of dword ;
      // die page descriptor register (PDR) liefern die seitenlänge
      // die schlichte Adressumsetzung.
      PDR: array[TPdp11MmuCpuMode, TPdp11MmuIDMode, 0..7] of dword ;

      OnMMUChanged: TNotifyEvent ;


      constructor Create(memorycellgroups: TMemoryCellgroups) ;
      destructor Destroy ; override ;
      // ist die MMU gerade auf 16, 18 oder 22 BIt physical eingestellt?
      function getPhysicalAddressType: TMemoryAddressType ;

      function Virtual2Physical(addr_v: TMemoryAddress; cpumode: TPdp11MmuCpuMode ; idmode: TPdp11MmuIDMode): TMemoryAddress ;
      function Virtual2PhysicalData(addr_v: TMemoryAddress): TMemoryAddress ;
      function Virtual2PhysicalInstruction(addr_v: TMemoryAddress): TMemoryAddress ;

      // das PSW auffrischen, sync mit anderen memorycells
      // eigene Function, da es sich wahrscheinlich viel öfter ändert
      // als die MMU Einstellungen
      procedure ExamineCpuMode ;
      // die MMR0, MMR3 und PAR[] auffrischen, sync mit anderen memorycelles
      procedure ExamineMMU ;


    end { "TYPE TPdp11Mmu = class(TObject)" } ;

  // siehe SimH, pdp11_cpu.crelocR()


implementation


uses
  OctalConst ;

// memorycellgroups die MMU legt eine eigene MemoryCellGroup an, die
// mit den anderen MemoryCellgroup der Application synchronisiert werden muss.
constructor TPdp11Mmu.Create(memorycellgroups: TMemoryCellgroups) ;
  var //idx: integer ;
  iopagebase: dword ;
  begin
    inherited Create ;
    iopagebase  := PhysicalIopageBaseAddr(matPhysical22) ;

    memorycellgroup:= TMemoryCellGroup.Create(memorycellgroups) ;
    memorycellgroup.OnMemoryCellChange := MemoryCellChange ;
    // MMU erstmal mit 22 bit anlegen.
    // Wird bei Wahl einer targetmachine umgerechnet
    memorycellgroup.mat := matPhysical22 ;

    // die Speicherzellen anlegen, die die MMU ausmachen.
    // in evalMemoryCell) werden sie interpretiert.
    psw_memorycell := memorycellgroup.Add(iopagebase + _17776) ; // PSW
    memorycellgroup.Add(iopagebase + _17572) ; // MMR0
    memorycellgroup.Add(iopagebase + _12516) ; // MMR3
    memorycellgroup.Add(iopagebase + _17660, 8) ; // User Data PAR
      memorycellgroup.Add(iopagebase + _17620, 8) ; // User Data PDR
      memorycellgroup.Add(iopagebase + _17640, 8) ; // User Instruction PAR
      memorycellgroup.Add(iopagebase + _17600, 8) ; // User Instruction PDR
      memorycellgroup.Add(iopagebase + _12360, 8) ; // Kernel Data PAR
      memorycellgroup.Add(iopagebase + _12320, 8) ; // Kernel Data PDR
      memorycellgroup.Add(iopagebase + _12340, 8) ; // Kernel Instruction PAR
      memorycellgroup.Add(iopagebase + _12300, 8) ; // Kernel Instruction PDR
      memorycellgroup.Add(iopagebase + _12260, 8) ; // Supervisor Data PAR
      memorycellgroup.Add(iopagebase + _12220, 8) ; // Supervisor Data PDR
      memorycellgroup.Add(iopagebase + _12240, 8) ; // Supervisor Instruction PAR
      memorycellgroup.Add(iopagebase + _12200, 8) ; // Supervisor Instruction PDR
(*
    for idx := 0 to 7 do begin
      memorycellgroup.Add(iopagebase + _17660 + 2 * idx) ; // User Data PAR
      memorycellgroup.Add(iopagebase + _17620 + 2 * idx) ; // User Data PDR
      memorycellgroup.Add(iopagebase + _17640 + 2 * idx) ; // User Instruction PAR
      memorycellgroup.Add(iopagebase + _17600 + 2 * idx) ; // User Instruction PDR
      memorycellgroup.Add(iopagebase + _12360 + 2 * idx) ; // Kernel Data PAR
      memorycellgroup.Add(iopagebase + _12320 + 2 * idx) ; // Kernel Data PDR
      memorycellgroup.Add(iopagebase + _12340 + 2 * idx) ; // Kernel Instruction PAR
      memorycellgroup.Add(iopagebase + _12300 + 2 * idx) ; // Kernel Instruction PDR
      memorycellgroup.Add(iopagebase + _12260 + 2 * idx) ; // Supervisor Data PAR
      memorycellgroup.Add(iopagebase + _12220 + 2 * idx) ; // Supervisor Data PDR
      memorycellgroup.Add(iopagebase + _12240 + 2 * idx) ; // Supervisor Instruction PAR
      memorycellgroup.Add(iopagebase + _12200 + 2 * idx) ; // Supervisor Instruction PDR
      // muss sortiert werden für optimierten Zugriff
    end{ "for idx" } ;
*)
  end { "constructor TPdp11Mmu.Create" } ;


destructor TPdp11Mmu.Destroy ;
  begin
    memorycellgroup.Free  ;
    inherited  ;

  end ;


  // ist die MMU gerade auf 16, 18 oder 22 Bit physical eingestellt?
  // ändert sichm wenn ein anderes Tagret gewählt wird!
function  TPdp11Mmu.getPhysicalAddressType: TMemoryAddressType ;
begin
result := memorycellgroup.Cell(0).addr.mat ;
end;


// low level address transformation
// Umsetzung des ganzen Memory Managment Kapitel aus dem
// "EP-17716-1879 pdp-11 processor handbook" ein.
// nur 22 bit Mapping!
//
// addr_v: virtual 16bitaddress
// result: 22bit-addr
// i_space: I/D-flag true, wenn zugriff durch PC, sonst false
function TPdp11Mmu.Virtual2Physical(addr_v: TMemoryAddress; cpumode: TPdp11MmuCpuMode ; idmode: TPdp11MmuIDMode): TMemoryAddress ;
  var
    activePageField: dword ;
    displacement: dword ;
    pageAddressField: dword ; // address field from par[..,..]
    curpdr: dword ;
    expansiondirection: boolean ;
    pagelen: dword ; // in bytes
  begin
    assert(addr_v.mat = matVirtual) ;
    result.mat := getPhysicalAddressType ;
    if  addr_v.val >= $10000 then
      result.val := MEMORYCELL_ILLEGALVAL
    else if addr_v.val >= _160000 then // iopage!
      result.val := addr_v.val - _160000 + PhysicalIopageBaseAddr(result.mat)  // upper 8K auf 22 bit addr space
    else if not EnableRelocation then
      result.val := addr_v.val
    else begin
      if not EnableDSpace[cpumode] then
        idmode := idmodeInstruction ; // Daae Space  := Instruction Space

      activePageField := (addr_v.val shr 13) and 7 ; // bits 15..13 = active page field
      displacement := addr_v.val and $1777 ; // lower 13 bit erhalten
      pageAddressField := PAR[cpumode,idmode,activePageField] ;
      curpdr := PDR[cpumode,idmode,activePageField] ;
      expansiondirection := (curpdr and 8) <> 0 ;
      if expansiondirection then
        raise Exception.CreateFmt('Virtual2Physical: Cannnot handle downward expansion in[%d,%d,%d]',
                [ord(cpumode), ord(idmode), activePageField]) ;

      pagelen := 64 * ((curpdr shr 8) and $7f ); // bit 14..8 in 32 words
      // displacement ist 13 bit, pagelen = 128*64 =13 bits
      if displacement >= pagelen then
        result.val := MEMORYCELL_ILLEGALVAL
      else
        result.val := (pageAddressField shl 6) + displacement ;
      // Addr ungültig wegen page len?
    end { "if not EnableRelocation ... ELSE" } ;
  end { "function TPdp11Mmu.Virtual2Physical" } ;



//
function TPdp11Mmu.Virtual2PhysicalData(addr_v: TMemoryAddress): TMemoryAddress ;
  begin
    result := Virtual2Physical(addr_v, curCpuMode, idmodeData) ;
  end ;


function TPdp11Mmu.Virtual2PhysicalInstruction(addr_v: TMemoryAddress): TMemoryAddress ;
// mode: kernel/user/supervisor
// access = fetch/data
  begin
    result := Virtual2Physical(addr_v, curCpuMode, idmodeInstruction) ;
  end ;



// einzelne memorycell auslesen und Datenstruktur aktualisieren
procedure TPdp11Mmu.evalMemoryCell(mc: TMemoryCell) ;
  var
     iopagebase: dword ;
    idx: integer ;
  begin
    iopagebase := PhysicalIopageBaseAddr(getPhysicalAddressType) ;
    with mc do begin
      if addr.val = (iopagebase + _17776) then begin // PSW
        curCpuMode := TPdp11MmuCpuMode(pdp_value shr 14) ; // bit 15,14 sind der Mode
      end else if addr.val = (iopagebase + _17572) then begin // MMR0
        EnableRelocation := (pdp_value and $01) <> 0 ;
      end else if addr.val = (iopagebase + _12516) then begin // mmr3
        EnableDSpace[cpumodeUser] := (pdp_value and $01) <> 0 ;
        EnableDSpace[cpumodeSupervisor] := (pdp_value and $02) <> 0 ;
        EnableDSpace[cpumodeKernel] := (pdp_value and $04) <> 0 ;
        Mapping22Bit := (pdp_value and $10) <> 0 ;
        UnibusRelocation := (pdp_value and $20) <> 0 ;
      end else begin
        idx := (addr.val div 2) and $07 ; // bits 1..3 bilden register index 0..7
        if (addr.val >= (PhysicalIopageBaseAddr(getPhysicalAddressType) + _17660))
                and (addr.val < (PhysicalIopageBaseAddr(getPhysicalAddressType) + _17660+ 16)) then // User Data PAR
          PAR[cpumodeUser, idmodeData, idx] := pdp_value
        else if (addr.val >= (iopagebase + _17620))
                and (addr.val < (iopagebase + _17620+ 16)) then // User Data PDR
          PDR[cpumodeUser, idmodeData, idx] := pdp_value
        else if (addr.val >= (iopagebase + _17640))
                and (addr.val < (iopagebase + _17640+ 16)) then  // User Instruction PAR
          PAR[cpumodeUser, idmodeInstruction, idx] := pdp_value
        else if (addr.val >= (iopagebase + _17600))
                and (addr.val < (iopagebase + _17600 + 16)) then  // User Instruction PDR
          PDR[cpumodeUser, idmodeInstruction, idx] := pdp_value

        else if (addr.val >= (iopagebase + _12360))
                and (addr.val < (iopagebase + _12360+ 16)) then // Kernel Data PAR
          PAR[cpumodeKernel, idmodeData, idx] := pdp_value
        else if (addr.val >= (iopagebase + _12320))
                and (addr.val < (iopagebase + _12320+ 16)) then // Kernel Data PDR
          PDR[cpumodeKernel, idmodeData, idx] := pdp_value
        else if (addr.val >= (iopagebase + _12340))
                and (addr.val < (iopagebase + _12340+ 16)) then // Kernel Instruction PAR
          PAR[cpumodeUser, idmodeInstruction, idx] := pdp_value
        else if (addr.val >= (iopagebase + _12340))
                and (addr.val < (iopagebase + _12340+ 16)) then // Kernel Instruction PDR
          PDR[cpumodeUser, idmodeInstruction, idx] := pdp_value

        else if (addr.val >= iopagebase + _12260)
                and (addr.val < (iopagebase + _12260+ 16)) then // Supervisor Data PAR
          PAR[cpumodeSupervisor, idmodeData, idx] := pdp_value
        else if (addr.val >= (iopagebase + _12260))
                and (addr.val < (iopagebase + _12260+ 16)) then // Supervisor Data PDR
          PDR[cpumodeSupervisor, idmodeData, idx] := pdp_value
        else if (addr.val >= (iopagebase + _12240))
                and (addr.val < (iopagebase + _12240+ 16)) then // Supervisor Instruction PAR
          PAR[cpumodeSupervisor, idmodeInstruction, idx] := pdp_value
        else if (addr.val >= (iopagebase + _12240))
                and (addr.val < (iopagebase + _12240+ 16)) then  // Supervisor Instruction PDR
          PDR[cpumodeSupervisor, idmodeInstruction, idx] := pdp_value ;

      end{ "if addr.val = (ThePhysicalAddressWidth + _12516) ... ELSE" } ;
    end{ "with mc" } ;
  end{ "procedure TPdp11Mmu.evalMemoryCell" } ;


// memorycells auslesen und Datenstruktur aktualisieren
procedure TPdp11Mmu.evalMemoryCells ;
  var i: integer ;
  begin
    // schleife über alle memorycell
    for i := 0 to memorycellgroup.Count-1 do begin
      evalMemoryCell(memorycellgroup.Cell(i)) ;
    end ;

  end ;


// das PSW auffrischen, sync mit anderen memorycells
// eigene Function, da es sich wahrscheinlich viel öfter ändert
// als die MMU Einstellungen
procedure TPdp11Mmu.ExamineCpuMode ;
  begin
    // nur PSW updaten
    psw_memorycell.Examine ;
    (memorycellgroup.Collection as TMemoryCellgroups).SyncMemoryCells(psw_memorycell);
    evalMemoryCell(psw_memorycell) ;
  end ;

// die PSW, MMR0, MMR3 und PAR[] auffrischen, sync mit anderen memorycells
procedure TPdp11Mmu.ExamineMMU ;
  begin
    memorycellgroup.Examine(false, true); // update all, long list
    // SyncMemoryCells() wird schon gemacht
    evalMemoryCells ;
  end ;

// wird aufgerufen, wenn PSW, oder die MMU register von aussen
// durch TMemoryCellGroups.SyncMemoryCells() geändert werden.
procedure TPdp11Mmu.MemoryCellChange(Sender: TObject ; mc: TMemoryCell) ;
  begin
    evalMemoryCell(mc);
    if assigned(OnMMUChanged) then
      OnMMUChanged(self) ;
  end ;



end{ "unit Pdp11MMUU" } .


