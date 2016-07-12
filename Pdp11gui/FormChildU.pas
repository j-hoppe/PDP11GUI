unit FormChildU;
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
    Basisklasse für die PDP11GUI-MDIChilds.
    Diese sind keine normalen MDI-childs,
    sondern haben besondere show/hide logik

    Die Forms werden als TForm designed, dann
    wird
    "type TFormxxx = class(TForm)"
      in
    "type TFormxxx = class(TFormChild)"
    geändert

    Motivation:
    1. MDI childs sollen "Hide" können, d.h. ihren Zusatnd behalten, aber unsichtbar sein.
      Die Standard MDIChild kennt diesen Zusatnd nicht.
      Daher wird TFormChild "fsNormal, wenn sie Invisible ist"
    2.  MDIChilds mit gefülltem TJvEditor, der grosse Sourcen enthält,
        lassen sich nicht zwischen fsNormal und fsMDiChild umschalten.
        Dieses Problem wird auch hier behoben.

     Ausserdem is ".scaled": := false, das schaltet die "Windows Font Magnification" ab
     udn lässt Schriftgrössen wie etnworfen.
}

interface
uses
  Classes, Forms ;

type
  TFormChild = class(TForm)
    private
      mOnBeforeHide: TNotifyEvent ;
      mOnAfterShow: TNotifyEvent ;
      orgFormStyle :TFormStyle ;
    public
      constructor Create(aOwner: TComponent) ; virtual ;
      procedure Show ; virtual ;
      procedure Hide ; virtual ;

    published
      property OnBeforeHide: TNotifyEvent read mOnBeforeHide write mOnBeforeHide;
      property OnAfterShow: TNotifyEvent read mOnAfterShow write mOnAfterShow;
    end { "TYPE TFormChild = class(TForm)" } ;


implementation

uses
  RegistryU ;



constructor TFormChild.Create(aOwner: TComponent) ;
  begin
    inherited Create(aOwner) ;
    mOnBeforeHide := nil ;
    mOnAfterShow := nil ;
    orgFormStyle := fsNormal ;
  end ;

// MDIChild "unsichtbar" durch umschalten auf normale Form
procedure TFormChild.Hide ;
var orgOnFormShow: TNotifyEvent ;
  begin
    // muss sich die Form vorbereiten?
    // Form ist noch ein sichtbares MDI-Child: Form reduzieren
    if assigned(mOnBeforeHide) then mOnBeforeHide(self) ;
    orgFormStyle := FormStyle ;
    if FormStyle = fsMDIChild then begin
      //childform.PrepareForClose ;
      // "Formstyle := " bewirkt ein "FormShow": unterdrücke es!
      orgOnFormShow := self.OnShow  ; self.OnShow := nil ; // disable OnFormShow
      self.FormStyle := fsNormal ;
      self.OnShow := orgOnFormShow ; // re-enable OnFormShow
    end;
    TheRegistry.Save(self) ;
    inherited Hide ;
  end ;

// MDIChild "unsichtbar" durch umschalten auf normale Form
procedure TFormChild.Show ;
  begin
    if orgFormStyle = fsMDIChild then
      self.FormStyle := fsMDIChild ;
    inherited Show ;
    // Form ist jetzt sichtbares MDI-Child: weiter füllen
    if assigned(mOnAfterShow) then mOnAfterShow(self) ;

    TheRegistry.Load(self) ;
  end ;



//

end{ "unit FormChildU" } .
