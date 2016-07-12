unit FormNumberconverterU;

{
 Schnelle Umwandlung hex / octal / dezimal / binär

 TAstten:
 'O','H', 'D'
 'ESC' : clear cur edit

 DDDD = hexdigit UND Decimal select???

}

interface 

uses 
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, ClipBrd,
  FormChildU ;

type 
  TFormNumberConverter = class(TFormChild)
      HexEdit: TEdit; 
      OctalEdit: TEdit; 
      DecimalEdit: TEdit; 
      HexAsBinaryEdit: TEdit; 
      OctalAsBinaryEdit: TEdit; 
      Label1: TLabel; 
      Label2: TLabel; 
      Label3: TLabel; 
      procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); 
      procedure NumberEditEnter(Sender: TObject); 
      procedure FormKeyPress(Sender: TObject; var Key: Char); 
      procedure NumberEditChange(Sender: TObject); 
      procedure FormCreate(Sender: TObject); 
      procedure FormShow(Sender: TObject); 
    private 
      { Private-Deklarationen }
      InternalEditChange: boolean ; 
      function getCurrentEditNumberBase: integer ; 
      procedure setCurrentEditNumberBase(base: integer) ; 
    public 
      { Public-Deklarationen }
      TheValue: Dword ; // aktueller Eingabewert
      function updateValue: boolean ; 
      procedure updateEdits(value: Dword; sourceEdit: TEdit) ; 

      property currentBase: integer read getCurrentEditNumberBase write setCurrentEditNumberBase ; 
    end{ "TYPE TMainForm = class(TForm)" } ; 

var 
  FormNumberConverter: TFormNumberConverter;

implementation 

{$R *.dfm}
// Zahl nach Binärstring wandeln.
function DwordToBinStr(val: Dword; digits: integer): string ; 
  var digit: integer ; 
  begin 
    result := '' ; 
    while (val <> 0) do begin 
      digit := val and 1 ; 
      result := Char(ord('0') + digit) + result ; 
      val := val shr 1 ; 
    end; 
    // links mit 0en auffüllen
    while length(result) < digits do 
      result := '0' + result ; 
  end; 

// Zahl nach Octalstring wandeln.
function DwordToOctalStr(val: Dword): string ; 
  var digit: integer ;
  begin
    result := '' ;
    while (val <> 0) do begin
      digit := val and 7 ;  // 3 bits
      result := Char(ord('0') + digit) + result ;
      val := val shr 3;
    end;
    if result = '' then
      result := '0' ;
  end;

// Octale Zahl nach Wert wandeln.
function OctalStr2Int64(s:string): Int64 ;
  var i: integer ;
  begin
    s := Trim(s) ;
    result := 0 ;
    for i := 1 to length(s) do begin
      if not CharInSet(s[i], ['0'..'7']) then
        raise Exception.CreateFmt('String "%s" can not be converted to octal!', [s]) ;
      result := (result shl 3) or Dword(ord(s[i]) - ord('0')) ;
    end;
  end;


              // ist die Ziffer für das Zahlensystem gültig?
function isValidDigit(numberbase: integer; digit: char): boolean ;
begin
        case numberbase of
          8:  result := CharInSet(digit, ['0'..'7']) ;
          10: result := CharInSet(digit, ['0'..'9']) ;
          16: result := CharInSet(digit, ['0'..'9', 'A'..'F', 'a'..'f']) ;
          else
          result := false ;
        end;
end ;

function stripInvalidDigits(numberbase: integer; s: string): string ;
var i: integer ;
begin
result := '' ;
  for i:= 1 to length(s)  do
  if isValidDigit(numberbase, s[i]) then
    result := result + s[i] ;
end;


// setze edit auf string.
// unpassende Zeichen werden einfach weggeworfen
procedure setEditText(e: TEdit ; s: string) ;
  begin
    e.Text := s ;
    e.SelStart := length(s) ; // caret ganz nach rechts
  end; 


procedure TFormNumberConverter.FormCreate(Sender: TObject);
  begin 
    TheValue := 0 ; 
    InternalEditChange := false ;

    // mark edits with numebr base
    HexEdit.Tag := 16 ;
    OctalEdit.Tag := 8 ;
    DecimalEdit.Tag := 10 ;
  end;


procedure TFormNumberConverter.FormShow(Sender: TObject);
  begin 
    // reset state
    OctalEdit.SetFocus ; 
    TheValue := 0 ; 
    // nötig, damit carte auch witklich von anfnag an rechts sitzt!
    setCurrentEditNumberBase(10) ; // start in hex edit
    Application.ProcessMessages ; 

    setCurrentEditNumberBase(16) ; // start in hex edit
    updateEdits(0, nil); 
  end; 


// welches Edit/Welche Zahlenbasis ist aktiv für die Eingabe?
function TFormNumberConverter.getCurrentEditNumberBase: integer ;
  begin 
    result := -1 ;
    // cycle trouhg the Edits
    if ActiveControl = HexEdit then
      result := 16 
    else if ActiveControl = OctalEdit then 
      result := 8 
    else if ActiveControl = DecimalEdit then 
      result := 10 ; 
    assert(result > 0) ; 
  end; 


{ paint the focused Edit in another colr than the depends edits }
procedure TFormNumberConverter.setCurrentEditNumberBase(base: integer) ;
  var activeEdit: TEdit ; 
  begin 
// unmark all Edits
    OctalEdit.Color := clWindow ; 
    DecimalEdit.Color := clWindow ; 
    HexEdit.Color := clWindow ; 
(*
    OctalEdit.BorderStyle := bsNone ;
    DecimalEdit.BorderStyle := bsNone ;
    HexEdit.BorderStyle := bsNone ;
*)
(*
    OctalEdit.BevelInner := bvNone ;
    OctalEdit.BevelOuter := bvNone ;
    OctalEdit.BevelKind := bkFlat ;
    OctalEdit.BevelWidth := 3 ;
    DecimalEdit.BevelInner := bvNone ;
    DecimalEdit.BevelOuter := bvNone ;
    DecimalEdit.BevelKind := bkFlat ;
   DecimalEdit.BevelWidth := 3 ;
    HexEdit.BevelInner := bvNone ;
    HexEdit.BevelOuter := bvNone ;
    HexEdit.BevelKind := bkFlat ;
    HexEdit.BevelWidth := 3 ;
  *)
    activeEdit := nil ; 
    case base  of 
      8: 
        activeEdit := OctalEdit ; 
      10: 
        activeEdit := DecimalEdit ; 
      16: 
        activeEdit := HexEdit ; 
    end; 
    if activeEdit <> nil then begin 
//      activeEdit.BevelInner := bvRaised ; // mark
//      activeEdit.BevelOuter := bvLowered ;
//          activeEdit.BorderStyle := bsSingle ;
      activeEdit.Color := clInfoBk ; 
      ActiveControl := activeEdit ; 
    end; 
  end{ "procedure TMainForm.setCurrentEditNumberBase" } ; 


// caused by mouse and by TAB
procedure TFormNumberConverter.NumberEditEnter(Sender: TObject);
  begin 
    //refresh state
    setCurrentEditNumberBase(getCurrentEditNumberBase); 
  end; 


procedure TFormNumberConverter.FormKeyDown(Sender: TObject; var Key: Word;
        Shift: TShiftState);
var s: string ;
  begin
    if (Key = ord('D')) and (ssAlt in Shift) then begin
      // Alt-D selects "Decimal"
      setCurrentEditNumberBase(10); 
      Key := 0 ; // processed
    end else if (Key = ord('H')) and (ssAlt in Shift) then begin 
      // Alt-H selects "Hex"
      setCurrentEditNumberBase(16); 
      Key := 0 ; // processed
    end else if (Key = ord('O')) and (ssAlt in Shift) then begin 
      // Alt-O selects "Octal"
      setCurrentEditNumberBase(8); 
      Key := 0 ; // processed
    end (*else if (Key = ord('C')) and (ssCtrl in Shift) then begin
    // ^C: copy to clibboard
       Clipboard.AsText := Trim((ActiveControl as TEdit).Text) ;
    end else if (Key = ord('V')) and (ssCtrl in Shift) then begin
    // ^V
     s:= Clipboard.AsText ;
     s := stripInvalidDigits(getCurrentEditNumberBase, s) ;
    setEditText(ActiveControl as TEdit, s);
    end*) ;
  end{ "procedure TMainForm.FormKeyDown" } ; 


procedure TFormNumberConverter.FormKeyPress(Sender: TObject; var Key: Char);

  begin 
    // keypreview must be true!
    // filter illegal input keys
    case Key of 
      #$1b: begin // Escape = clear
        TheValue := 0 ; 
        updateEdits(TheValue, nil); 
      end ; 

      ' '..#$FF:
        if not isValidDigit(getCurrentEditNumberBase, Key) then
              Key := #0 ;
    end{ "case Key" } ;
    // route to Edits if <> #0
  end{ "procedure TMainForm.FormKeyPress" } ;


procedure TFormNumberConverter.NumberEditChange(Sender: TObject);
  var e: TEdit ; 
    s: string ; 
  begin 
    e := Sender as TEdit ; 
    if InternalEditChange then Exit ; 
    try 
      InternalEditChange := true ; // no recursion

      //Änderung kann über clipboard paste kommen!
//            s := stripInvalidDigits(e.Tag, e.Text) ;
      s := e.Text ;

      // wenn erste Zahl nach führender 0' weg mit der 0
      if (length(s) = 2) and (s[1] = '0') then begin
        setEditText(e, s[2]) ; // nur neue ziffer stehenlassen, caret nach rechts
      end;

      // auf overflow testen. Zahl links verkürzen, bis
      if not updateValue then // höchste ziffer weg
        setEditText(e,  Copy(e.Text, 2, maxint)) ;
      // update other edits
      updateEdits(TheValue, Sender as TEdit);
    finally 
      InternalEditChange := false ; 
    end{ "try" } ; 
  end{ "procedure TMainForm.NumberEditChange" } ; 


{ aktualisiert den eingabe wert: "TheValue"
result: false = overflow
}
function TFormNumberConverter.updateValue: boolean ;
  var 
    numberbase: integer ; 
    s: string ; 
    rawvalue: Int64 ; 
  begin 
    result := false ; // default: fehler
    numberbase := getCurrentEditNumberBase ; 
    if numberbase > 0 then begin 
      // ein TEdit hat den focus!
      s := Trim((ActiveControl as TEdit).Text) ;
      if s = '' then s := '0' ; 
      case numberbase of 
        8:  rawvalue := OctalStr2Int64(s); 
        10: rawvalue := StrToInt64(s) ; 
        16: rawvalue := StrToInt64('$'+s) ; 
        else rawvalue := $100000000 ; // overflow
      end; 
      if rawvalue <= $ffffffff then begin 
        TheValue := rawvalue ; 
        result := true ; 
      end; 
    end { "if numberbase > 0" } ; 
  end{ "function TMainForm.updateValue" } ; 


{ update value in allen Edits AUSSER dem, in das es eingegeben wurde }
procedure TFormNumberConverter.updateEdits(value: Dword; sourceEdit: TEdit) ;


  var 
    s, binstr: string ; 
    i: integer ; 
  begin 
    try 
      // inhibit "OnChange" event
      InternalEditChange := true ; 

      // binär
      binstr := DwordToBinStr(TheValue, 32) ; 

      // 1. hex
      if sourceEdit <> HexEdit then begin 
        setEditText(HexEdit, Format('%x', [TheValue]));
      end;
      // binär ziffern immer setzen. spaces in 4er gruppen + header
      // '1111 0000 1111 0000 1111 0000 1111 0000'
      s := '' ; 
      for i := 0 to 31 do begin 
        if (i > 0) and ((i mod 4) = 0) then 
          s := ' ' + s ; 
        s := binstr[32-i] + s ; 
      end; 
      setEditText(HexAsBinaryEdit, s) ;

      // 2. oct
      if sourceEdit <> OctalEdit then begin 
        s := DwordToOctalStr(TheValue) ; 
        setEditText(OctalEdit, s) ; 
      end; 
      // binär ziffern immer setzen. spaces in 3er gruppen + header
      // '  00  111  000  111  000  111  000  111  000  111  000'
      s := '' ; 
      for i := 0 to 31 do begin 
        if (i > 0) and ((i mod 3) = 0) then 
          s := '  ' + s ; 
        s := binstr[32-i] + s ; 
      end ; 
      setEditText(OctalAsBinaryEdit, '  ' + s) ; 

      // 3. dez
      if sourceEdit <> DecimalEdit then begin 
        setEditText(DecimalEdit, IntToStr(TheValue)) ; 
      end; 
    finally 
      InternalEditChange := false ; 
    end{ "try" } ; 
  end{ "procedure TMainForm.updateEdits" } ; 

end{ "unit FormMainU" } . 
