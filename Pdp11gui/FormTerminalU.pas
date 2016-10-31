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


  // Charakteristika des Terminals. Werden von jeder Console
  // mit "getTerminalSettings" geliefert
  TTerminalSettings = record
            // Receive_CRisCRLF, Receive_LFisCRLF
            // Anabe, welches Zeichen ein Zeileende im Terminal bewirkt.
            // Exakt 1 der beiden soll true sein!

      Receive_CRisNewline: boolean ; // true: Empfang von $d = #13 = CR: Zeilenumbruch
      // false: CR ignorieren
      Receive_LFisNewline: boolean ; // true: Empfang von $a = #10 = LF: Zeilenumbruch
      // false: LF ignorieren
      Backspace: char ; // bewirkt rubout des letzten Zeichens. #0 = disable
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
      Selectcolor1: TMenuItem;
      TerminalColorDialog: TColorDialog;
      Showcontrolchars1: TMenuItem;
      Copyall1: TMenuItem;
      N2: TMenuItem;
    Beep1: TMenuItem;
      procedure RichEdit1KeyPress(Sender: TObject; var Key: char);
      procedure RichEdit1KeyDown(Sender: TObject; var Key: Word;
              Shift: TShiftState);
      procedure FormCreate(Sender: TObject);
      procedure FormShow(Sender: TObject);
      procedure FormDestroy(Sender: TObject);

      procedure Copy1Click(Sender: TObject);
      procedure Paste1Click(Sender: TObject);
      procedure Clear1Click(Sender: TObject);
      procedure Selectfont1Click(Sender: TObject);
      procedure RichEdit1MouseUp(Sender: TObject; Button: TMouseButton;
              Shift: TShiftState; X, Y: integer);
      procedure Selectcolor1Click(Sender: TObject);
      procedure Showcontrolchars1Click(Sender: TObject);
      procedure Copyall1Click(Sender: TObject);
    procedure Beep1Click(Sender: TObject);
    private
      { Private-Deklarationen }
      pending_newline: boolean ;
      procedure setBackgroundColor(bkcolor: TColor) ;
    public
      TerminalSettings: TTerminalSettings ;
      TerminalFont: TFont ;
      TerminalBackgroundColor: TColor ;

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
    TerminalFont := TFont.Create ;

    // irgendwelche default Einstellungen
    // Bei Aktivierung einer Console setzt FormMain auch die TterminalSettings
    TerminalSettings.Receive_CRisNewline := true ;
    TerminalSettings.Receive_LFisNewline := false ;
    TerminalSettings.Backspace := #8 ;
    TerminalSettings.TabStop := 8 ;
  end;


procedure TFormTerminal.FormDestroy(Sender: TObject);
  begin
    TerminalFont.Free ;
//    EmulVT.Free ; // do not free: has form as owner
  end;

procedure TFormTerminal.FormShow(Sender: TObject);
  var s: string ;
  begin
    Showcontrolchars1.Checked := (TheRegistry.Load('TerminalShowControlChars', 0) <> 0) ;
    Beep1.Checked := (TheRegistry.Load('TerminalBeep', 0) <> 0) ;

    s := TheRegistry.Load('TerminalFont', '') ;
    if s <> '' then begin
      StringToFont(s, TerminalFont) ;
      RichEdit1.Font.Assign(TerminalFont) ;
    end else
      TerminalFont.Assign(RichEdit1.Font) ;// on start: as designed

    s := TheRegistry.Load('TerminalBackgroundColor', '') ;
    if s <> '' then
      setBackgroundColor(StringToColor(s))
    else
      setBackgroundColor(RichEdit1.Color) ; // correct default color
  end{ "procedure TFormTerminal.FormShow" } ;

procedure TFormTerminal.setBackgroundColor(bkcolor: TColor) ;
  var s: string ;
  begin
    // correct background color, so the font remains visible
    TerminalBackgroundColor := AssertDifferentColor(bkcolor, RichEdit1.Font.Color) ;
    RichEdit1.Color := TerminalBackgroundColor ;
    s := ColorToString(TerminalBackgroundColor) ;
    TheRegistry.Save('TerminalBackgroundColor', s) ;
  end;


procedure TFormTerminal.Selectcolor1Click(Sender: TObject);
  begin
    TerminalColorDialog.Color := TerminalBackgroundColor ;
    if TerminalColorDialog.Execute then
      setBackgroundColor(TerminalColorDialog.Color) ;
  end;

procedure TFormTerminal.Selectfont1Click(Sender: TObject);
  var s: string ;
  begin
    TerminalFontDialog.Font.Assign(TerminalFont) ;
    if TerminalFontDialog.Execute then begin
      TerminalFont.Assign(TerminalFontDialog.Font);
      TerminalFont.Style := [] ; // no bold, no italic (VT220!)
      RichEdit1.Font.Assign(TerminalFont);
      s := FontToString(TerminalFont);
      TheRegistry.Save('TerminalFont', s) ;
      // adjust background color
      setBackgroundColor(TerminalBackgroundColor);
    end ;
  end{ "procedure TFormTerminal.Selectfont1Click" } ;

procedure TFormTerminal.SetStyle(outputstyle: TTerminalOutputStyle) ;
  begin
    with RichEdit1 do
      case outputstyle of
        tosPDP:  begin
          SelAttributes.Color := TerminalFont.Color ;
        end;
        tosUser: begin
          SelAttributes.Color := clWhite ;
        end;
        tosSystem: begin // automatische Ausgabe etwas gedämpfter
          SelAttributes.Color := clGray ;
        end;
      end;
  end{ "procedure TFormTerminal.SetStyle" } ;


procedure TFormTerminal.Showcontrolchars1Click(Sender: TObject);
  begin
    Showcontrolchars1.Checked := not Showcontrolchars1.Checked ;
    TheRegistry.Save('TerminalShowControlChars', Showcontrolchars1.Checked) ;
  end;

procedure TFormTerminal.Beep1Click(Sender: TObject);
begin
  Beep1.Checked := not Beep1.Checked ;
    TheRegistry.Save('TerminalBeep', Beep1.Checked) ;
end;

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
procedure TFormTerminal.AppendText(outputstyle: TTerminalOutputStyle ; chars: string) ;
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
          if TerminalSettings.Receive_LFisNewline then
            pending_newline := true ;
        end else if c = #$d then begin //CD
          if TerminalSettings.Receive_CRisNewline then
            pending_newline := true ;
        end else if c = #9 then begin // TAB
          if TerminalSettings.TabStop > 0 then
            while (length(lastline) + length(s)) mod TerminalSettings.TabStop <> 0 do
              s := s + ' ' ;
        end else if c = #7 then begin // ^G is Beep
          if Beep1.Checked then
            Beep ;
        end else if (TerminalSettings.Backspace <> #0) and (c = TerminalSettings.Backspace) then begin
          lastline := Copy(lastline, 1, length(lastline)-1) ;
          Lines[Lines.Count-1] := lastline ;
        end else begin
          // Ausgabe von richtigem Text. Sonderzeichen filtern oder in "<...>" wandeln
          s := Character2PrintableText(c, Showcontrolchars1.Checked) ;
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
      Perform(EM_LINESCROLL, 0, 1);
      // Perform(EM_ScrollCaret, 0, 0);

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
    Key := 0 ;
  end{ "procedure TFormTerminal.RichEdit1KeyDown" } ;

procedure TFormTerminal.RichEdit1KeyPress(Sender: TObject; var Key: char);
  begin
// if LocalEcho then
//        AppendText(tosUser, key) ;
    FormMain.SerialIoHub.DataFromTerminal(Key);
    Key := #0 ; // mark key as "processed, do not forward to richedit
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
    Log('TFormTerminal.OnSerialRcvData: "%s"', [String2PrintableText(curdata, true)]);
    AppendText(tosPDP, curdata) ;
  end;


procedure TFormTerminal.Copy1Click(Sender: TObject);
  begin
    RichEdit1.CopyToClipboard ;
  end;


procedure TFormTerminal.Copyall1Click(Sender: TObject);
  var ss: integer ;
  begin
    ss := RichEdit1.SelStart ;
    RichEdit1.SelectAll ;
    RichEdit1.CopyToClipboard ;
    RichEdit1.SelLength := 0 ; // deselect
    RichEdit1.SelStart := ss ; // restore caret
  end;

procedure TFormTerminal.Paste1Click(Sender: TObject);
  var i: integer ;
    s: string ;
  begin
    // Clipboard zeichenweise eintippen!!!
    s := Clipboard.AsText ;
    for i := 1 to length(s) do
      FormMain.SerialIoHub.DataFromTerminal(s[i]);

//    RichEdit1.SetSelTextBuf(PChar(ClipBoard.AsText));
  end;


end{ "unit FormTerminalU" } .
