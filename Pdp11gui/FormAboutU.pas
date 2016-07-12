unit FormAboutU;
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

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FormChildU ;

const
  HOME_URL = 'http://www.retrocmp.com/tools/pdp11gui';
  TUTORIAL_URL = 'http://www.retrocmp.com/tools/pdp11gui/tutorial';
  CONTACT_EMAIL = 'j_hoppe@t-online.de' ;

type
  TFormAbout = class(TForm) // ist kein MDI Child, kein "TFormChild"
      Button1: TButton;
      Memo1: TMemo;
      procedure FormCreate(Sender: TObject);
    private
      { Private-Deklarationen }
    public
      { Public-Deklarationen }
      VersionStr: string ; // wird auch von anderen Modulen gebraucht
    end;

var
  FormAbout: TFormAbout;

implementation

{$R *.dfm}

uses
  JclFileUtils;


procedure TFormAbout.FormCreate(Sender: TObject);

  procedure Add(fmt: string ; args: array of const) ;
    begin
      Memo1.Lines.Add(Format(fmt, args)) ;
    end;

  var
    fvi: TJclFileVersionInfo ;
  begin
    fvi := TJclFileVersionInfo.Create(Application.ExeName) ;
    try
      Memo1.Lines.Clear ;
      VersionStr := 'v ' + fvi.FileVersion ;
      {$ifdef DEBUG}
      VersionStr := VersionStr + ' (debug)' ;
      {$endif}
      Add('* * *  %s   Version %s  * * *', [fvi.ProductName, fvi.FileVersion]);
      Add('', [0]) ;
      Add('%s', [fvi.FileDescription]);
      Add('', [0]) ;
      Add('All rights by J. Hoppe, Göttingen. ', [0]) ;
      Add('Free use granted to everbody for non-commercial usage.', [0]) ;
      Add('', [0]) ;
      Add('Updates: %s', [HOME_URL]) ;
      Add('', [0]) ;
      Add('Contact: %s', [CONTACT_EMAIL]) ;
      Add('', [0]) ;
      Add('Copyright %s', [fvi.LegalCopyright]);
      Add('', [0]) ;
      Add('MACRO11.EXE is Copyright(c) 2001 by Richard Krehbiel.', [0]) ;
      Add('', [0]) ;
      Add('M4.EXE is Copyright (c) 2010 by Free Software Foundation ', [0]) ;
      Add('', [0]) ;
      Add('SimH is Copyright (c) 1993-2008 by Robert M Supnik.', [0]) ;

    finally
      fvi.Free;
    end{ "try" } ;
  end{ "procedure TFormAbout.FormCreate" } ;

end{ "unit FormAboutU" } .
