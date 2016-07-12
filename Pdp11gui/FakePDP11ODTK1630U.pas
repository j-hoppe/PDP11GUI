unit FakePDP11ODTK1630U;
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


// Simulierte K1630. Wie ODT18BBit, some flags are dofferent

interface

uses
  SysUtils,
  ConsoleGenericU,
  FakePDP11ODTU ;

type
  TFakePDP11ODTK1630 = class(TFakePDP11ODT)
      constructor Create ;
    end ;


implementation

uses
  AddressU ;

constructor TFakePDP11ODTK1630.Create ;
  begin
    inherited Create(matPhysical18) ;
    isK1630 := true ;
  end ;


end{ "unit FakePDP11ODTK1630U" } .
