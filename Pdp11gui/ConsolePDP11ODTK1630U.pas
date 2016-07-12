unit ConsolePDP11ODTK1630U;
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
  Wrapper for K1630 CPU of Robotron A6402 clone.
  Contct: ruediger.kurth@freenet.de, rechenwerk.de, Halle, 2016

  Is a 18bit 11/23 with slightly different console syntax:
  some extra space separators, and absolute addresses need an "A" postfix
  }


interface

uses
  MemoryCellU,
  AddressU,
  ConsolePDP11ODTU ;

type
  TConsolePDP11ODTK1630 = class(TConsolePDP11ODT)
    public
      constructor Create(memorycellgroups: TMemoryCellGroups) ;
      function getName: string ; override ;
    end;

implementation

{ TConsolePDP11K1630 }

constructor TConsolePDP11ODTK1630.Create(memorycellgroups: TMemoryCellGroups);
  begin
    inherited Create(memorycellgroups, matPhysical18) ;
    GobbleExtraSpaceAfterPrompt := true ;
    isK1630 := true ;
  end;

function TConsolePDP11ODTK1630.getName: string ;
  begin
    result := 'Robotron CPU K1630 (ODT 18 bit 11/23)' ;
  end;

end{ "unit ConsolePDP11ODTK1630U" } .
