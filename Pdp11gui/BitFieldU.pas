unit BitFieldU;
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
Definitionen von Bitfelder
TBitfieldDef - ein Feld in einem dword-Wert
TBitfieldsDef - alle Felder in einem dword Wert
TBitfieldsDefs - Liste von allen dword-Definitionen
}

interface
uses
  Windows,
  Classes,
  SysUtils,
  JH_Utilities,
  AddressU,
  IniFiles;

type
// ein einzelnes Bitfeld
  TBitfieldDef = class(TCollectionItem)
    public
      // eine zusammenhängender Bitbereich
      name:string ;
      info: string ;
      bit_lo: integer ; // niedrigstes Bit
      bit_hi: integer ; // niedrigstes Bit
      // in einer MemoryCell die Bits des Feldes setzen
      class function getMask(shifted: boolean ; bit_hi, bit_lo:integer):dword ;
      function setFieldInValue(value: dword; fieldvalue: dword): dword ;
      function getFieldInValue(value: dword) : dword ;
    end;


// Definition aller Bitfelder einer MemoryCell
  TBitfieldsDef = class(TCollectionItem)
    public
      name:string ;
      bitfields: TCollection ; // of  TBitfieldDef
      // keine Felder ... ist nur eine Collection of TMemoryCellBitfieldDef
      constructor Create(Collection: TCollection); override ;
      destructor Destroy ; override ;

    end;



  // linkt eine Adresse auf eine Bitfield-Defintion
  TAddr2BitFieldsDef = class(TCollectionItem)
    public
      addr: TMemoryAddress ;
      bitfieldsdef: TBitfieldsDef ;
    end;

  //
  TBitfieldsDefs = class(TCollection)
    private
      // zuordnung adressen n:1 bitfieldsdef
      // sortierte Liste. Strings[]=addr, OBjects = TBitfieldsDef
      addr2bitfieldsdef: TCollection ; // of TAddr2BitFieldsDef ;
    public
      constructor Create;
      destructor Destroy ; override ;

      procedure Clear ;

      procedure LoadFromIniFile(inifile: TMemIniFile) ;
      procedure UnLoad ;
      function BitFieldsDefByName(aName:string):TBitfieldsDef ;

      function LinkAddr2BitfieldsDef(aAddr:TMemoryAddress ; BitfieldsdefName:string): boolean ;

      function BitFieldsDefByAddr(aAddr: TMemoryAddress): TBitfieldsDef ;
    end{ "TYPE TBitfieldsDefs = class(TCollection)" } ;

implementation

uses
  FormMainU,
  AuxU
  ;



// bits hi..lo gesetzt.
// wenn not shifted: bit_lo auf bit 0
class function TBitfieldDef.getMask(shifted: boolean ; bit_hi, bit_lo:integer):dword ;
  const mask: array[0..16] of word = ( $0000,
      $0001, $0003, $0007, $000f,
      $001f, $003f, $007f, $00ff,
      $01ff, $03ff, $07ff, $0fff,
      $1fff, $3fff, $7fff, $ffff ) ;
  begin
    assert(bit_hi >= bit_lo) ;
    assert(bit_hi >= 0) ;
    assert(bit_lo >= 0) ;
    assert(bit_hi <= 15) ;
    result := mask[bit_hi - bit_lo + 1] ;
    if shifted then
      result := result  shl bit_lo ;
  end{ "function TBitfieldDef.getMask" } ;

function TBitfieldDef.setFieldInValue(value: dword; fieldvalue: dword): dword ;
  var mask: dword ;
  begin
    // maske, ganz nach rechts geshiftet
    mask := getMask(false, bit_hi-bit_lo, 0) ;
    if (fieldvalue and not mask) <> 0 then
      raise Exception.CreateFmt('Value 0x%x does not fit in bits %d..%d', [fieldvalue, bit_hi, bit_lo]) ;
    mask := getMask(true, bit_hi, bit_lo) ; // geshiftete Maske
    value := value and not mask ; // feld := 0 setzen
    value := value or (fieldvalue shl bit_lo) ;
    result := value ;
  end ;


function TBitfieldDef.getFieldInValue(value: dword) : dword ;
  var mask: dword ;
  begin
    mask := getMask(true, bit_hi, bit_lo) ; // geshiftete Maske
    result := (value and mask) shr bit_lo ;
  end;



constructor TBitfieldsDef.Create(Collection: TCollection);
  begin
    inherited Create(Collection) ;
    bitfields := TCollection.Create(TBitfieldDef) ;
  end;

destructor TBitfieldsDef.Destroy ;
  begin
    bitfields.Free ;
    inherited ;
  end;

constructor TBitfieldsDefs.Create;
  begin
    inherited Create(TBitfieldsDef) ;
    addr2bitfieldsdef := TCollection.Create(TAddr2BitFieldsDef) ;
  end;

destructor TBitfieldsDefs.Destroy ;
  begin
    addr2bitfieldsdef.Free ; // objecte werden erst im inherited freigegeben
    inherited ;
  end;

procedure TBitfieldsDefs.Clear ;
  begin
    inherited Clear ;
    addr2bitfieldsdef.Clear ;
  end;


procedure TBitfieldsDefs.LoadFromIniFile(inifile: TMemIniFile) ;
  var
    sections, keys: TStringList ;
    i, j, k: integer ;
    keystr: string ;
    w, s: string ;
    tmp: integer ;
  begin
    Clear ;

    sections := TStringList.Create ;
    keys := TStringList.Create ;

    try
      inifile.ReadSections(sections) ; // alle [..] Abschnitte = alle Listen

      for i := 0 to sections.Count - 1 do
        // Gruppen, die mit 'Bits.' anfangen sind keine Register-Definitionen!
        if pos('BITS.', Uppercase(sections[i])) > 0 then
          with Add as TBitfieldsDef do begin
            name := sections[i] ;
            inifile.ReadSectionValues(sections[i], keys) ;

            for j := 0 to keys.Count - 1 do
              with bitfields.Add as TBitfieldDef do begin
                keystr := keys[j] ;
//Log('Bitfields: Processing section[%d], key[%d]: %s', [i, j, keystr]) ;

                // Format:
                // name = bit_hi:bit_lo;info
                k := pos('=', keystr) ; // name am ersten'=' abtrennen
                name := Trim(Copy(keystr, 1, k-1)) ;
                keystr := Copy(keystr, k+1, maxint) ; // jetzt nur noch alles nach dem '='

                w := Trim(ExtractWord(1, keystr, [';'])) ;
                // 1. Wort: "bit_hi:bit_lo"
                s := Trim(ExtractWord(1, w, [':'])) ;
                assert(s <> '') ;
                bit_hi := StrToInt(s) ;
                s := Trim(ExtractWord(2, w, [':'])) ;
                if s = '' then
                  bit_lo := bit_hi
                else
                  bit_lo := StrToInt(s) ;
                if bit_hi < bit_lo then begin // swap
                  tmp := bit_hi ; bit_hi := bit_lo ; bit_lo := tmp ;
                end;

                // 2. Wort: Info
                w := Trim(ExtractWord(2, keystr, [';'])) ;
                info := StripQuotes(w) ;
              end{ "with bitfields.Add as TBitfieldDef" } ;
          end{ "with Add as TBitfieldsDef" } ;
    finally
      sections.Free ;
      keys.Free ;
    end{ "try" } ;
  end{ "procedure TBitfieldsDefs.LoadFromIniFile" } ;

procedure TBitfieldsDefs.Unload ;
begin
Clear ;
addr2bitfieldsdef.Clear ;
end;

// suche eine Definition
function TBitfieldsDefs.BitFieldsDefByName(aName:string): TBitfieldsDef ;
  var i: integer ;
  begin
    aName := Uppercase(aName) ;
    result := nil ;
    for i :=  0 to Count - 1 do
      with Items[i] as TBitfieldsDef do
        if Uppercase(name) = aName then begin
          result := Items[i] as TBitfieldsDef ;
          Exit ;
        end;
  end;


// während die Registerdefinitionen geladen werden, entstehen
// auch die Verknüpfungen mit memoryadressen
// result: name gefunden?
function TBitfieldsDefs.LinkAddr2BitfieldsDef(aAddr:TMemoryAddress ; BitfieldsdefName:string): boolean ;
  var bfsd: TBitfieldsDef ;
  begin
    bfsd := BitFieldsDefByName(BitfieldsdefName) ;
    if bfsd = nil then
      result := false
    else
      with addr2bitfieldsdef.Add as TAddr2BitFieldsDef do begin
        addr := aAddr ;
        bitfieldsdef := bfsd ;
        result := true ;
      end;
  end;

// bitfields definition über adresse suchen
function TBitfieldsDefs.BitFieldsDefByAddr(aAddr: TMemoryAddress): TBitfieldsDef ;
  var i: integer ;
  begin
    result := nil ;
    assert (aAddr.mat > matAnyPhysical) ; // kann nur nach physikalischen Adressen suchen
    // liste der adressen durchsuchen
    for i := 0 to addr2bitfieldsdef.Count - 1 do
      with addr2bitfieldsdef.Items[i] as TAddr2BitFieldsDef do begin
        if addr.mat <> aAddr.mat then
          // Adresse in Liste in anderem Model als gesuchte adresse:
          // passe die Liste an, da sich das model der aAddr nur selten bei
          // einer neuen Zielmashcine ändert.
          addr := ChangePhysicalAddressBitWidth(addr, aAddr.mat) ;
        if addr.val = aAddr.val then begin
          result := bitfieldsdef ;
          Exit ;
        end;
      end;
  end{ "function TBitfieldsDefs.BitFieldsDefByAddr" } ;


end{ "unit BitFieldU" } .
