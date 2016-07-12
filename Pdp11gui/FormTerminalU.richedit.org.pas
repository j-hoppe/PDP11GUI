unit FormTerminalU;
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
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Menus,
  FormChildU ;

type
  // Darstellung verschiedner Arten von input
  TTerminalOutputStyle = ( //
      tosPDP, // was die PDP ausgibt
      tosUser, // ws der User ins terminal tippt
      tosSystem // commandos, die automatisch vom System erzeugt erden.
    ) ;


  // Charakteristika des Terminals. Werden von jeder COnsole
  // mit "getTerminalSettings" geliefert
  TTerminalSettings = record
      CRisCRLF: boolean ; // true: Empfang von $d = #13 = CR: Zeilenumbruch
      // false: CR ignorieren
      LFisCRLF: boolean ; // true: Empfang von $a = #10 = LF: Zeilenumbruch
      // false: LF ignorieren
      Backspace: char ; // bewirkt rubout des letzten Zeichens
      TabStop: integer ; // Tabs auf diese Position expandieren. 0 = disable
    end ;


  TFormTerminal = class(TFormChild)
      StatusBar1: TStatusBar;
      RichEdit1: TRichEdit;
      TerminalPopupMenu: TPopupMenu;
      Cut1: TMenuItem;
      Paste1: TMenuItem;
      Clear1: TMenuItem;
      N1: TMenuItem;
      Selectfont1: TMenuItem;
      TerminalFontDialog: TFontDialog;
      procedure RichEdit1KeyPress(Sender: TObject; var Key: char);
      procedure RichEdit1KeyDown(Sender: TObject; var Key: Word;
              Shift: TShiftState);
      procedure FormCreate(Sender: TObject);
      procedure RichEdit1MouseUp(Sender: TObject; Button: TMouseButton;
              Shift: TShiftState; X, Y: integer);
      procedure Copy1Click(Sender: TObject);
      procedure Paste1Click(Sender: TObject);
      procedure Clear1Click(Sender: TObject);
      procedure Selectfont1Click(Sender: TObject);
      procedure FormShow(Sender: TObject);
    private
      { Private-Deklarationen }
      pending_newline: boolean ;
    public
      { Public-Deklarationen }
      TerminalSettings: TTerminalSettings ;

      procedure Clear ;

      procedure SetStyle(outputstyle: TTerminalOutputStyle) ;
      procedure Hello ;

      procedure AppendText(outputstyle: TTerminalOutputStyle ; chars:string) ;
      // wird von aussen aufgerufen, wenn das Terminal was empfängt
      procedure OnSerialRcvData(curdata: string) ;

    end{ "TYPE TFormTerminal = class(TFormChild)" } ;


implementation


uses
  ClipBrd,
  JH_Utilities,
  AuxU,
  RegistryU,
  FormLogU, FormMainU;

{$R *.dfm}

procedure TFormTerminal.FormCreate(Sender: TObject);
  begin
    // irgendwelche default Einstellungen
    // Bei Aktivierung einer Console setzt FormMain auch die TterminalSettings
    TerminalSettings.CRisCRLF := true ;
    TerminalSettings.LFisCRLF := false ;
    TerminalSettings.Backspace := #8 ;
    TerminalSettings.TabStop := 8 ;
  end;


procedure TFormTerminal.FormShow(Sender: TObject);
  var s: string ;
    font: TFont ;
  begin
    s := TheRegistry.Load('TerminalFont', '') ;
    if s <> '' then begin
      font := TFont.Create ;
      StringToFont(s, font) ;
      RichEdit1.font.Assign(font) ;
      font.Free ;
    end;
  end{ "procedure TFormTerminal.FormShow" } ;


procedure TFormTerminal.Selectfont1Click(Sender: TObject);
  var s: string ;
  begin
    TerminalFontDialog.font.Assign(RichEdit1.font) ;
    if TerminalFontDialog.Execute then begin
      RichEdit1.font.Assign(TerminalFontDialog.font) ;
      s := FontToString(TerminalFontDialog.font);
      TheRegistry.Save('TerminalFont', s) ;
    end { "if TerminalFontDialog.Execute" } ;
  end{ "procedure TFormTerminal.Selectfont1Click" } ;

procedure TFormTerminal.SetStyle(outputstyle: TTerminalOutputStyle) ;
  begin
    with RichEdit1 do
      case outputstyle of
        tosPDP:  begin
          SelAttributes.Color := clYellow ;
        end;
        tosUser: begin
          SelAttributes.Color := clWhite ;
        end;
        tosSystem: begin // automatische Ausgabe etwas gedämpfter
          SelAttributes.Color := clGray ;
        end;
      end;
  end{ "procedure TFormTerminal.SetStyle" } ;


procedure TFormTerminal.Clear ;
  begin
    pending_newline := false ;
    RichEdit1.Clear ;
  end;

procedure TFormTerminal.Clear1Click(Sender: TObject);
  begin
    RichEdit1.Lines.Clear ;
  end;

// Text an die ausgabe in einem bestimmten Stil anhängen
// #13 als EOLN benutzen!
// unsichtbare Zeichen ausser #13 in \x.. Notation wandeln

// Interpretiert TerminalSettings
procedure TFormTerminal.AppendText(outputstyle: TTerminalOutputStyle ; chars:string) ;
  var s: string ;
    c: char ;
    i: integer ;
    lastline: string ;
  begin
    with RichEdit1 do begin
      // unsichtbare Zeichen umwandeln
      s := '' ;
      for i := 1 to length(chars) do begin
        // input zeichenweise verarbeiten
        c := chars[i] ; //char(ord(chars[i]) and #$7f) ; // strip MSB
        pending_newline := false ;
        if Lines.Count = 0 then
          Lines.Add('') ;
        lastline := Lines[Lines.Count-1] ;
        s := '' ; // neuer Text

        if c = #$a then begin //LF
          if TerminalSettings.LFisCRLF then
            pending_newline := true ;
        end else if c = #$d then begin //CD
          if TerminalSettings.CRisCRLF then
            pending_newline := true ;
        end else if c = #9 then begin // TAB
          if TerminalSettings.TabStop > 0 then
            while (length(lastline) + length(s)) mod TerminalSettings.TabStop <> 0 do
              s := s + ' ' ;
        end else if c = TerminalSettings.Backspace then begin
          lastline := Copy(lastline, 1, length(lastline)-1) ;
          Lines[Lines.Count-1] := lastline ;
        end else begin
          // Ausgabe von richtigem Text
          s := CharVisible(c) ;
        end;

//    if s <> '' then
//formlog.log('appendtext: %s', [s]) ;
        SelStart := maxint ; // Cursor ans Ende
        SetStyle(outputstyle) ;
//        if pending_newline then // Zeilenumbruch verlangt?
//        Lines.Add('') ;
//          s := #13 + s ;
        SelText := s ;
        SelStart := maxint ; // Cursor ans Ende
        if pending_newline then
          Lines.Add('') ;
      end{ "for i" } ;

      // Scrolle Caret in View. (Gefummel am VertScrollbar hat nicht funktioniert)
      Perform(EM_ScrollCaret, 0, 0);

    end{ "with RichEdit1" } ;
  end{ "procedure TFormTerminal.AppendText" } ;


procedure TFormTerminal.Hello ;
  begin
    with RichEdit1 do begin
      // SelStart ist der cursor
//      AppendText(tosPDP, '*** Terminal für PDP-11/44 console***'
//              +#13 + '>>> So wird Ausgabe der PDP-11 angezeigt!' + #13
//              + '>>>') ;

//      AppendText(tosUser,'So wird User input angezeigt') ;
//      AppendText(tosUser,#13) ;
//
//      AppendText(tosPDP, '>>>') ;

//      AppendText(tosSystem,'So wird automatische Ausgabe User input angezeigt') ;

//      AppendText(tosPDP, #13+'>>>') ;

//        AppendText(tosUser, '') ;
    end{ "with RichEdit1" } ;

  end{ "procedure TFormTerminal.Hello" } ;




// Tastendrücke gehen direkt an die Konsole
procedure TFormTerminal.RichEdit1KeyDown(Sender: TObject; var Key: Word;
        Shift: TShiftState);
  begin
// das ENTF in DELETE 0x7F umwandeln!
    if Key = VK_DELETE then
      FormMain.SerialIoHub.DataFromTerminal(#$7f);

    // Control-Steuerzeichen nicht ins RTF-Memo gelangen lassen, nur an die Console.
    // "KeyPress()" liefert sie trotzdem an die COnsole, auch wenn ich sie hier wegwerfe!
    if (ssCtrl in Shift) and (Key in [ord('A') .. ord('Z')]) then begin
      // Erzeuge ASCII Ctrl-<key> code
//       FormMain.PDP11Console.TerminalCharToPDP(char(key - ord('A') + 1));
      Key := 0 ;
    end;
  end{ "procedure TFormTerminal.RichEdit1KeyDown" } ;

procedure TFormTerminal.RichEdit1KeyPress(Sender: TObject; var Key: char);
  begin
// if LocalEcho then
//        AppendText(tosUser, key) ;
    FormMain.SerialIoHub.DataFromTerminal(Key);
  end;


procedure TFormTerminal.RichEdit1MouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: integer);
  var p: TPoint ;
  begin
    if Button = mbRight then begin
      p.X := X ; p.Y := Y ;
      p := RichEdit1.ClientToscreen(p) ;
      TerminalPopupMenu.PopUp(p.X, p.Y) ;
    end;
  end;


// wird von aussen aufgerufen, wenn das Terminal was empfängt
procedure TFormTerminal.OnSerialRcvData(curdata: string) ;
  begin
//Log('TFormTerminal.OnSerialRcvData: "%s"', [curdata]);
    AppendText(tosPDP, curdata) ;
  end;


procedure TFormTerminal.Copy1Click(Sender: TObject);
  begin
    RichEdit1.CopyToClipboard ;
  end;


procedure TFormTerminal.Paste1Click(Sender: TObject);
  var i: integer ;
    s: string ;
  begin
    // Clipboard zeichenweise eintippen!!!
    s := ClipBoard.AsText ;
    for i := 1 to length(s) do
      FormMain.SerialIoHub.DataFromTerminal(s[i]);

//    RichEdit1.SetSelTextBuf(PChar(ClipBoard.AsText));
  end;
end{ "unit FormTerminalU" } .
