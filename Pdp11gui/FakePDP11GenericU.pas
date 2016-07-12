unit FakePDP11GenericU;
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
  Interface für die rudimentären PDP-11 consolen.
  Sie dient nur zum Test der GUI

  - Deposit, , mit /n, /g
  - Examine, /n, /g
  - ^C

}
interface

uses
  Windows,
  SysUtils,
  ExtCtrls,
  AddressU,
  MemoryCellU ;

const
  // local
//  FakePDP11_max_mem = $100000 ; // 1MB

  FakePDP11_max_addr = $400000 ; // 2^22
  FakePDP11_iopage_size = 8192 ; // 8 K Byte

//  FakePDP11_iopage_end = FakePDP11_max_addr ;
//  FakePDP11_iopage_start = FakePDP11_iopage_end - FakePDP11_iopage_size ; // top 8KB

type
  // wird ausgelöst, wenn die Console einen Fehler entdeckt
  EFakePDP11Error = class(Exception) ;


  TFakePDP11Generic = class(TObject)
    private
    public
      name: string ;

      // fiktiver RUN/HALT-Switch (für alle ausser 11/44 und SimH)
      RunMode: boolean ;

      mat: TMemoryAddressType ; // 16, 18, 22 bit

      PhysicalMemorySize: dword ; // Speicher in bytes
      PhysicalIopageBaseAddr: dword ; // beginn der IOPage

      // Zustandsvariable
      // Memory konstant und maximal für ppdp-11 architektur
      Mem: array[0..FakePDP11_max_addr-1] of word ; // 0....
      IoPage: array[0..FakePDP11_iopage_size-1] of word ;
      IoPageValidMap: array[0..FakePDP11_iopage_size-1] of boolean ;

      ProgramCounterAddr: TMemoryAddress ; // addesse von R7, braucht man dauernd

      RunToHaltTimer: TTimer ; // um ein Code-HALT nach einen RUN zu simulieren

      // gepufferte Zeichen
      SerialInBuff: string ; // eingabe durch user
      SerialOutBuff: string ; // Ausgabe

      constructor Create(mat: TMemoryAddressType) ; virtual ;
      destructor Destroy ; override ;

      // nur die IOpage-Adressen gültig machen, die CPU register oder
      // in MemoryCellgroups mit tag "iopage_usagetag" versehen sind
      procedure CalcIoPageValidMap(MemoryCellGroups: TMemoryCellGroups ; iopage_usagetag: string) ;


      procedure PowerOn ; virtual ; abstract ;
      procedure Reset ; virtual ; abstract ;

      procedure RunToHalt(startpcval: dword) ; // löst nach zufalls periode "doHalt" aus
      procedure OnRunToHaltTimer(Sender: TObject) ;
      function isRunning: boolean ; // true, wenn doHalt noch nicht kam
      procedure Halt ;

      // Interface zur serialle Console
      function SerialReadByte(var b: byte) : boolean ; virtual ; abstract ;
      function SerialWriteByte(b: byte) : boolean ; virtual ; abstract ;
    protected
      // Exception EFakePDP11Error bei illegal adress
      procedure setMem(addr: TMemoryAddress ; val: word) ; virtual ;
      function getMem(addr: TMemoryAddress): dword; virtual ;

      procedure doHalt ; virtual ; abstract ;

    end{ "TYPE TFakePDP11Generic = class(TObject)" } ;


implementation


uses
  OctalConst ;


// mat: ist der Fake eine physical16, 18 oder 22 Bit machine?
constructor TFakePDP11Generic.Create(mat: TMemoryAddressType) ;
  begin
    inherited Create;
    assert(mat > matAnyPhysical) ;
    // physicaladdresswidth: 18 oder 22
    self.mat := mat ;
    // Speicher nur halb "bestücken"
    case mat of
      matPhysical16: begin
        PhysicalIopageBaseAddr := _160000 ;
        PhysicalMemorySize := $8000 ; // 32 K byte
      end;
      matPhysical18: begin
        PhysicalIopageBaseAddr := _760000 ;
        PhysicalMemorySize := $20000 ; // 128 K byte
      end;
      matPhysical22: begin
        PhysicalIopageBaseAddr := _17760000 ;
        PhysicalMemorySize := $100000 ; // 1MB
      end;
      else raise Exception.Create('Fake PDP11: physicaladdresswidth not 16, 18 or 22') ;
    end{ "case mat" } ;

    RunMode := false ; // started in HALT mode, wenn relevant

    ProgramCounterAddr.mat := mat ;
    ProgramCounterAddr.val := PhysicalIopageBaseAddr + _17707 ;

    RunToHaltTimer := TTimer.Create(nil) ;
    RunToHaltTimer.Enabled := false ;
    CalcIoPageValidMap(nil, '') ; // erst mal nur standard CPU register implementieren

    // Caller sollte "PowerOn()" aufrufen!
  end{ "constructor TFakePDP11Generic.Create" } ;


destructor TFakePDP11Generic.Destroy ;
  begin
    RunToHaltTimer.Free ;
    inherited ;
  end;

procedure TFakePDP11Generic.setMem(addr: TMemoryAddress ; val: word) ;
  begin
    // kein test auf gerade Adressen                                                 '
    if (addr.val <= PhysicalMemorySize-2) then
      Mem[addr.val] := val
    else if (addr.val >=  PhysicalIopageBaseAddr)
            and ((addr.val - PhysicalIopageBaseAddr) < FakePDP11_iopage_size)
            and IoPageValidMap[addr.val - PhysicalIopageBaseAddr] then
      IoPage[addr.val - PhysicalIopageBaseAddr] := val
    else
      raise EFakePDP11Error.Create('') ;
  end;


function TFakePDP11Generic.getMem(addr: TMemoryAddress): dword;
// kein test auf gerade Adressen                                                 '
  begin
    // result := MEMORYCELL_ILLEGALVAL ;
    if (addr.val <= PhysicalMemorySize-2) then
      result := Mem[addr.val]
    else if (addr.val >= PhysicalIopageBaseAddr)
            and ((addr.val - PhysicalIopageBaseAddr) < FakePDP11_iopage_size)
            and IoPageValidMap[addr.val - PhysicalIopageBaseAddr] then
      result := IoPage[addr.val - PhysicalIopageBaseAddr]
    else
      raise EFakePDP11Error.Create('') ;
  end ;


// löst nach zufalls periode "doHalt" aus
procedure TFakePDP11Generic.RunToHalt(startpcval: dword) ;
  begin
    setMem(ProgramCounterAddr, startpcval); // PC setzen

    RunToHaltTimer.OnTimer := OnRunToHaltTimer ;
    RunToHaltTimer.Interval := 1000 + random(4000) ; // 1..5 Sekunden warten
    RunToHaltTimer.Enabled := true ;
  end;

function TFakePDP11Generic.isRunning: boolean ; // true, wenn doHalt noch nicht kam
  begin
    result := RunToHaltTimer.Enabled ;
  end;

  // wenn isRunning: HALT
  procedure TFakePDP11Generic.halt ;
  begin
    RunToHaltTimer.Enabled := false ;
  end;


procedure TFakePDP11Generic.OnRunToHaltTimer(Sender: TObject) ;
  var pcval: dword ;
  begin
    RunToHaltTimer.Enabled := false ; // Timer ist one-shot

    // irgendwo in den naechsten 100 bytes es PC anhalten
    pcval := getMem(ProgramCounterAddr) ;
    pcval := pcval + dword(random(_77)) + 2 ;
    if odd(pcval) then inc(pcval) ; // pc immer gerade

    // Console entry conditions
    setMem(ProgramCounterAddr, pcval) ; // Program Counter setzen

    doHalt ;
  end{ "procedure TFakePDP11Generic.OnRunToHaltTimer" } ;


// nur die IOpage-Adressen gültig machen, die CPU regsiter oder
// in MemoryCellgroups mit tag "iopagetag" versehen sind
procedure TFakePDP11Generic.CalcIoPageValidMap(MemoryCellGroups: TMemoryCellGroups ; iopage_usagetag: string) ;
  var i, j: integer ;
    mcg: TMemoryCellGroup ;
    addr: dword ;
  begin
    for i := 0 to FakePDP11_iopage_size - 1 do
      IoPageValidMap[i] := false ;

    // standard cpu register R0..R7,PSW enablen
    IoPageValidMap[_17700] := true ;
    IoPageValidMap[_17701] := true ;
    IoPageValidMap[_17702] := true ;
    IoPageValidMap[_17703] := true ;
    IoPageValidMap[_17704] := true ;
    IoPageValidMap[_17705] := true ;
    IoPageValidMap[_17706] := true ;
    IoPageValidMap[_17707] := true ;
    IoPageValidMap[_17776] := true ;

    // alle adressen implementieren, die das angegeben usagetag haben
    if MemoryCellGroups <> nil then begin
      for i := 0 to MemoryCellGroups.Count - 1 do  begin
        mcg := MemoryCellGroups.Items[i] as TMemoryCellGroup ;
        if mcg.usagetag = iopage_usagetag then
          for j := 0 to mcg.Count - 1 do begin
            assert (mcg.Cell(j).addr.mat > matAnyPhysical) ;
            addr := mcg.Cell(j).addr.val ;
            assert (addr >=  PhysicalIopageBaseAddr) ;
            assert ((addr - PhysicalIopageBaseAddr) < FakePDP11_iopage_size) ;
            IoPageValidMap[addr - PhysicalIopageBaseAddr] := true ;
          end;
      end;
    end { "if MemoryCellGroups <> nil" } ;

  end{ "procedure TFakePDP11Generic.CalcIoPageValidMap" } ;



end{ "unit FakePDP11GenericU" } .
