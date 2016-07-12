unit ConsolePDP11M9301U;
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
  Steuert über RS232 den console emulator auf dem
  M9301 boot terminator an. Wird zB in PDP-11/34 benutzt.
  Die Anwendung ruft nur Deposit(), Examine(), Start auf

  Identisch mit M9312, aber prompt '$' statt '@'
  }


interface

uses
  MemoryCellU,
  AddressU,
  ConsolePDP11M9312U ;

type
  TConsolePDP11M9301 = class(TConsolePDP11M9312)
    public
      constructor Create(memorycellgroups: TMemoryCellGroups;
              aMonitor_entryaddress: TMemoryAddress) ;
    end;

implementation

{ TConsolePDP11M9301 }

constructor TConsolePDP11M9301.Create(memorycellgroups: TMemoryCellGroups;
        aMonitor_entryaddress: TMemoryAddress) ;

  begin
    inherited Create(memorycellgroups, aMonitor_entryaddress) ;
    Prompt := '$' ; // M9312 default is '@'
  end;

end{ "unit ConsolePDP11M9301U" } .
