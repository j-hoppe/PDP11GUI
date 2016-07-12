unit AddressU;
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
  wegen Unit-Referenzen aus MmeoryCellU ausgegliedert.
}


interface

uses
  Windows,
  SysUtils,
  OctalConst ;

const
  PhysicalMaxAddr16Bit = _177776 ;
  PhysicalMaxAddr18Bit = _777776 ;
  PhysicalMaxAddr22Bit = _17777776 ;

  PhysicalIopageBaseAddr16Bit = _160000 ;
  PhysicalIopageBaseAddr18Bit = _760000 ;
  PhysicalIopageBaseAddr22Bit = _17760000 ;


type

  TMemoryAddressType = (
      matUnknown,
      matSpecialRegister, // val kodiert MEMORYCELL_SPECIALADDR_*
      matVirtual,  // 16 bit
      matAnyPhysical,  // ThePhysicalAddressWidth bit
      matPhysical16,
      matPhysical18,
      matPhysical22
    ) ;


  TMemoryAddress = record
      mat: TMemoryAddressType ;
      val: dword ; // wert virtuell oder physical
      tmpval: dword ; // eine andere Darstellung von val.
    end;


function AddrType2Bitswidth(mat: TMemoryAddressType): integer ; inline ;
function Addr2OctalStr(addr: TMemoryAddress): string ; inline ;
function OctalStr2Addr(s: string; mat: TMemoryAddressType): TMemoryAddress ; inline ;

// "Constructor"
// function MemoryAddress(mat: TMemoryAddressType ; val: dword) : TMemoryAddress ;

// Adresse in ein anderes Längenmodell umrechnen
function ChangePhysicalAddressBitWidth(
        addr: TMemoryAddress ; newMat:TMemoryAddressType): TMemoryAddress ;


function PhysicalIopageBaseAddr(mat: TMemoryAddressType): dword ; // Startadresse der I/O-page, abh von ThePhysicalAddressWidth

//var
// globale Einstellung der physikalischen Adressbreite für die
// aktuelle Maschine.
// Wird von Conosle.getÜhysicalAddressWidth definiert,
// und wird von allen Forms in PDP11GUI benutzt.

//  ThePhysicalAddressWidth: byte ;

implementation

uses
  AuxU ;

function PhysicalIopageBaseAddr(mat: TMemoryAddressType): dword ; // Startadresse der I/O-page, abh von ThePhysicalAddressWidth
  begin
    case mat of
      matVirtual: result := PhysicalIopageBaseAddr16Bit ;
      matPhysical16: result := PhysicalIopageBaseAddr16Bit ;
      matPhysical18: result := PhysicalIopageBaseAddr18Bit ;
      matPhysical22: result := PhysicalIopageBaseAddr22Bit ;
      else
        raise Exception.CreateFmt('MemoryAddressType must be virtual or physical16/18/22, is %d',[ord(mat)]) ;
    end;
  end;


function AddrType2Bitswidth(mat: TMemoryAddressType): integer ;
  begin
    case mat of
      matPhysical16: result := 16 ;
      matPhysical18: result := 18 ;
      matPhysical22: result := 22 ;
      matVirtual: result := 16 ;
      else raise Exception.CreateFmt ('illegal TMemoryAddressType %d', [ord(mat)]);
    end;
  end;

function Addr2OctalStr(addr: TMemoryAddress):string ; inline ;
  begin
    result := Dword2OctalStr(addr.val, AddrType2Bitswidth(addr.mat)) ;
  end;

function OctalStr2Addr(s: string; mat: TMemoryAddressType): TMemoryAddress ; inline ;
  begin
    result.mat := mat ;
    result.val := OctalStr2Dword(s, AddrType2Bitswidth(mat)) ;
  end;


// Adresse in ein anderes Längenmodell umrechnen
// Adressen kleiner als die IpgeBase bleiben erhalten.
// Adresse in ein anderes Längenmodell umrechnen
function ChangePhysicalAddressBitWidth(
        addr: TMemoryAddress ; newMat:TMemoryAddressType): TMemoryAddress ;
  begin
    assert(newMat > matAnyPhysical) ;

    if addr.val < PhysicalIopageBaseAddr(addr.mat) then
      result.val := addr.val
    else
      // Adresse liegt in IOPage
      result.val := addr.val
              + PhysicalIopageBaseAddr(newMat) // Start der IOPage im neuen model
              - PhysicalIopageBaseAddr(addr.mat) ; // Start der IOPage im alten model

    result.mat := newMat ;
  end;


end{ "unit AddressU" } .
