unit FakePDP11M9301U;
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
  Simuliert eine rudimentäre PDP-11 mit M9301 console emulator.
  Abgeleitet von M9312
  }

interface

uses
  FakePDP11M9312U ;

type
  TFakePDP11M9301 = class(TFakePDP11M9312)
    public
      constructor Create ;
    end;



implementation

{ FakePDP11M9301 }

constructor TFakePDP11M9301.Create;
  begin
    inherited ;
    // like M9312, but other prompt.
    // The ROMs in MP301-YA add a NUL, M9301-YF does not.
    // With NUL it is more difficult to parse!
    Prompt := '$' + #0 ;
  end;

end{ "unit FakePDP11M9301U" } .
