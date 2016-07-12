unit ConsolePDP1144v340cU;
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

// Console-Adapter für den fake.
// wie ConsolePDP1144, aber nur Examine und Deposit sind implementiert

interface

uses
 MemoryCellU,
  ConsoleGenericU,
  ConsolePDP1144U ;

type
  TConsolePDP1144v340c = class(TConsolePDP1144)
  public
      function getName: string ; override ;
      constructor Create(memorycellgroups: TMemoryCellGroups) ;
    end ;


implementation

function TConsolePDP1144v340c.getName: string ;
  begin
    result := 'PDP-11/44 v3.40c console' ;
  end;


constructor TConsolePDP1144v340c.Create(memorycellgroups: TMemoryCellGroups) ;
begin
  inherited Create(memorycellgroups) ;
  isV340c := true ;
end;


end{ "unit ConsolePDP1144FakeU" } .
