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
  FormChildU, OverbyteIcsEmulVT ;

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
      Receive_CRisCRLF: boolean ; // true: Empfang von $d = #13 = CR: Zeilenumbruch
      // false: CR ignorieren
      Receive_LFisCRLF: boolean ; // true: Empfang von $a = #10 = LF: Zeilenumbruch
      // false: LF ignorieren
      Backspace: char ; // bewirkt rubout des letzten Zeichens
      TabStop: integer ; // Tabs auf diese Position expandieren. 0 = disable
    end ;


  TFormTerminal = class(TFormChild)
      StatusBar1: TStatusBar;
      TerminalPopupMenu: TPopupMenu;
      Cut1: TMenuItem;
      Paste1: TMenuItem;
      Clear1: TMenuItem;
      N1: TMenuItem;
      Selectfont1: TMenuItem;
      TerminalFontDialog: TFontDialog;
      Label1: TLabel;
      procedure FormCreate(Sender: TObject);
      procedure FormShow(Sender: TObject);
      procedure FormDestroy(Sender: TObject);

      procedure EmulVTKeyDown (Sender: TObject; var VirtKey: integer;
              var Shift: TShiftState; var ShiftLock: boolean; var ScanCode: char;
              var Ext: boolean);
      procedure EmulVTKeyPress(Sender: TObject; var Key: char);
      procedure EmulVTKeyBuffer(Sender: TObject; Buffer: PWideChar; Len: integer);
      procedure EmulVTMouseUp(Sender: TObject; Button: TMouseButton;
              Shift: TShiftState; X, Y: integer);
      procedure Copy1Click(Sender: TObject);
      procedure Paste1Click(Sender: TObject);
      procedure Clear1Click(Sender: TObject);
      procedure Selectfont1Click(Sender: TObject);
    private
      { Private-Deklarationen }
    public
      EmulVT: TEmulVT;

      TerminalSettings: TTerminalSettings ;
      TerminalFont: TFont ;

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

    // constructed programmatically, so Overbyte package must not be installed
    EmulVT := TEmulVT.Create(self) ;
    EmulVT.Parent := self ;
    EmulVT.OnKeyDown := EmulVTKeyDown ;
    EmulVT.OnKeyPress := EmulVTKeyPress ;
    EmulVT.OnKeyBuffer := EmulVTKeyBuffer ;
    EmulVT.OnMouseUp := EmulVTMouseUp ;
    //
    EmulVT.AutoCR := false ;
    EmulVT.AutoLF := false ;
    EmulVT.autorepaint := true ;

    EmulVT.BackColor := vtsWhite ;

    // font metrics. Wird beei setzen von .font neu berechnet
//    EmulVT.CharWidth := 12 ;
//    EmulVT.LineHeight := - EmulVT.font.Size + 4 ;
//              EmulVT.LineHeight := 16 ;

    EmulVT.LocalEcho := false ;
    EmulVT.Log := false ;
    EmulVT.LogFileName := 'EMULVT.LOG' ;

    EmulVT.MonoChrome := false ;
    EmulVT.Options := [ vtoBackColor] ;

    EmulVT.Cols := 80 ;
    EmulVT.Rows := 50 ; // maximum MAX_ROWS in OverbyteIcsEmulVT.pas
//    EmulVT.Rows := 25 ;
    EmulVT.LeftMargin := 3 ; // Schwarzer Rahmen und Schriftfläche
    EmulVT.RightMargin := 3 ;

    EmulVT.TabStop := false ;
    EmulVT.Xlat := true ;


// EmulVT.SetupFont ; // char width/lineheigth aus .fon berehcnen?

    EmulVT.Align := alClient ;
    EmulVT.BringToFront ;


    // irgendwelche default Einstellungen
    // Bei Aktivierung einer Console setzt FormMain auch die TterminalSettings
    TerminalSettings.Receive_CRisCRLF := true ;
    TerminalSettings.Receive_LFisCRLF := false ;
    TerminalSettings.Backspace := #8 ;
    TerminalSettings.TabStop := 8 ;
  end{ "procedure TFormTerminal.FormCreate" } ;


procedure TFormTerminal.FormDestroy(Sender: TObject);
  begin
    TerminalFont.Free ;
//    EmulVT.Free ; // do not free: has form as owner
  end;

procedure TFormTerminal.FormShow(Sender: TObject);
  var s: string ;
  begin
    s := TheRegistry.Load('TerminalFont', '') ;
    if s <> '' then begin
      StringToFont(s, TerminalFont) ;
      EmulVT.Font := TerminalFont ; // adjust char geometry
      EmulVT.Font.Color := clYellow ;
    end;
  end;


procedure TFormTerminal.Selectfont1Click(Sender: TObject);
  var s: string ;
  begin
    TerminalFontDialog.Font.Assign(TerminalFont) ;
    if TerminalFontDialog.Execute then begin
      TerminalFont.Assign(TerminalFontDialog.Font);
      TerminalFont.Style := [] ; // no bold, no italic (VT220!)
      EmulVT.Font := TerminalFont ; // adjust char geometry
      s := FontToString(TerminalFont);
      TheRegistry.Save('TerminalFont', s) ;
    end ;
  end;

procedure TFormTerminal.SetStyle(outputstyle: TTerminalOutputStyle) ;
  begin
(*
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
*)
  end{ "procedure TFormTerminal.SetStyle" } ;


procedure TFormTerminal.Clear ;
  begin
    EmulVT.Clear ;
    EmulVT.UpdateScreen ;
  end;

procedure TFormTerminal.Clear1Click(Sender: TObject);
  begin
    EmulVT.Clear ;
    EmulVT.UpdateScreen ;
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



    // processing in OverbyteIcsEmulVT.pas, TScreen.WriteChar():
    //
    // #8: BackSpace
    // #9: TAB: fixed 8er Tab
    // #10 = Linefeed: CursorDown()
    // #13 = CR: CarriageReturn()
    for i := 1 to length(chars) do begin
      c := chars[i] ; //char(ord(chars[i]) and #$7f) ; // strip MSB
      if c = #$a then begin // LineFeed
        if TerminalSettings.Receive_LFisCRLF then
          EmulVT.WriteStr(#$d + #$a)
        else EmulVT.WriteStr(#$a) ;
      end else if c = #$d then begin // CarriageReturn
        if TerminalSettings.Receive_CRisCRLF then
          EmulVT.WriteStr(#$d + #$a)
        else EmulVT.WriteStr(#$d) ;
      end else if c = #9 then begin // TAB
        EmulVT.WriteStr(#9) ; // pass TAB: fixed 8er expansion
      end else if c = TerminalSettings.Backspace then begin
        EmulVT.WriteChar(#8) ; // cursor to left, do not erase
        //        lastline := Copy(lastline, 1, length(lastline)-1) ;
//          Lines[Lines.Count-1] := lastline ;
      end else begin
        // Ausgabe von richtigem Text
        s := CharVisible(c) ;
        EmulVT.WriteStr(s);
      end
    end{ "for i" } ;
    EmulVT.VScrollBar.Position := EmulVT.VScrollBar.Max ;

//     EmulVt.VScrollBar.ScrollBy(0, -maxint); // scroll scree ncontent up, so new text is visible


(*
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
*)
  end{ "procedure TFormTerminal.AppendText" } ;


procedure TFormTerminal.Hello ;
  begin
    with EmulVT do begin
      // SelStart ist der cursor
//      AppendText(tosPDP, '*** Terminal für PDP-11/44 console***'
//              +#13 + '>>> So wird Ausgabe der PDP-11 angezeigt!' + #13
//              + '>>>') ;
//      Application.ProcessMessages ;

//      AppendText(tosUser,'So wird User input angezeigt') ;
//      AppendText(tosUser,#13) ;
//
//      AppendText(tosPDP, '>>>') ;

//      AppendText(tosSystem,'So wird automatische Ausgabe User input angezeigt') ;

//      AppendText(tosPDP, #13+'>>>') ;

//        AppendText(tosUser, '') ;
    end{ "with EmulVT" } ;

  end{ "procedure TFormTerminal.Hello" } ;


// Tastendrücke gehen direkt an die Konsole
//procedure TFormTerminal.TerminalWindowKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

// wenn umgewandelete VT characters durc PC eingabe produziert wurder (ESC sequneces.

procedure TFormTerminal.EmulVTKeyDown(Sender: TObject; var VirtKey: integer;
        var Shift: TShiftState; var ShiftLock: boolean; var ScanCode: char;
        var Ext: boolean);
  begin
    // das ENTF in DELETE 0x7F umwandeln!
    // das macht EmulVT schon in der FKey map
//    if VirtKey = VK_DELETE then
//      FormMain.SerialIoHub.DataFromTerminal(#$7f);

    // Control-Steuerzeichen nicht ins RTF-Memo gelangen lassen, nur an die Console.
    // "KeyPress()" liefert sie trotzdem an die COnsole, auch wenn ich sie hier wegwerfe!
    if (ssCtrl in Shift) and (VirtKey in [ord('A') .. ord('Z')]) then begin
      // Erzeuge ASCII Ctrl-<key> code
//       FormMain.PDP11Console.TerminalCharToPDP(char(key - ord('A') + 1));
//      VirtKey := 0 ;
    end;
  end{ "procedure TFormTerminal.EmulVTKeyDown" } ;

procedure TFormTerminal.EmulVTKeyPress(Sender: TObject; var Key: char);
  begin
// if LocalEcho then
//        AppendText(tosUser, key) ;
    Log('TFormTerminal.EmulVTKeyPress: "%s"', [CharVisible(Key)]);

    FormMain.SerialIoHub.DataFromTerminal(Key);
  end;

//  characters,wie vom VT terminal erzeugt nach Umwandlung vom PC keypress
// ZB: Cursor-Pfeiltaste -> ESC ] sequence
procedure TFormTerminal.EmulVTKeyBuffer(Sender: TObject; Buffer: PWideChar; Len: integer);
  var i: integer ;
  begin
    // EmulVT: Der linke ENTER KEY erzeugt CR/LF, der rechte nur CR
    // (in allen FKEy mappings)
    // Bei einem echten VT100 kann dieses Verhalten programiert werden
    //Hier: IMMER nur CR ausgeben. (Das war auch in dem alten RichtEdit so.)
    if (Len = 2) and (Buffer[0] = #$d) and (Buffer[1] = #$a) then
      Len := 1 ; // strip additional LF

    Log('TFormTerminal.EmulVTKeyBuffer: "%s"', [StringVisible(Buffer)]);

    for i := 0 to Len - 1 do
      FormMain.SerialIoHub.DataFromTerminal(Buffer[i]);
  end{ "procedure TFormTerminal.EmulVTKeyBuffer" } ;

// popup menu
procedure TFormTerminal.EmulVTMouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: integer);
  var p: TPoint ;
  begin
    if Button = mbRight then begin
      p.X := X ; p.Y := Y ;
      p := EmulVT.ClientToScreen(p) ;
      TerminalPopupMenu.PopUp(p.X, p.Y) ;
    end;
  end;


// wird von aussen aufgerufen, wenn das Terminal was empfängt
procedure TFormTerminal.OnSerialRcvData(curdata: string) ;
  begin
    Log('TFormTerminal.OnSerialRcvData: "%s"', [StringVisible(curdata)]);
    AppendText(tosPDP, curdata) ;
  end;


procedure TFormTerminal.Copy1Click(Sender: TObject);
  var i: integer ;
    sl: TStringList ;
  begin
    sl := TStringList.Create ;
    try
      for i := 0 to EmulVT.Rows-1 do
        sl.Add(EmulVT.Screen.Lines[i].Txt) ;
      Clipboard.AsText := sl.Text ;
    finally
      sl.Free ;
    end;
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
