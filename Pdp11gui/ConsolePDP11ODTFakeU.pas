unit ConsolePDP11ODTFakeU;
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

// Console-Adpater für den fake.
// wie ConsolePDP11ODT, aber nur Examine und Deposit sind implementiert

interface

uses
  SysUtils,
  ConsoleGenericU,
  ConsolePDP11ODTU ;

type
  TConsolePDP11ODTFake = class(TConsolePDP11ODT)
      function getName: string ; override ;
      function getFeatures: TConsoleFeatureSet ; override ;
    end ;


implementation

uses
  AddressU ;

function TConsolePDP11ODTFake.getName: string ;
  begin
    result := '??? ' ;
    case getPhysicalMemoryAddressType of
      matPhysical16:
        result := 'PDP-11 ODT 16 bit fake console' ;
      matPhysical18:
        result := 'PDP-11 ODT 18 bit fake console' ;
      matPhysical22:
        result := 'PDP-11 ODT 22 bit fake console' ;
    end;
  end;


function TConsolePDP11ODTFake.getFeatures: TConsoleFeatureSet ;
  begin
   result := inherited getFeatures ;
  end;

end{ "unit ConsolePDP11ODTFakeU" } .
