{

Definition:
  http://www.bitsavers.org/pdf/dec/pdp11/1173/EK-KDJ1B-UG_KDJ11-B_Nov86.pdf
  Chapter 3, pdf-page 90

  Fragen:
LF direct nach prompt: auto inc?

LF nach error 177776000
@2000000/ -> ?

"/" nach error ?


WARNING: ODT greift auf VIRTUAL R0/R7 zu:
Kernel/Userstack Rx depend on PSW-Mode !!!


Das Flag RUNMODE steuert das verhalten von nnnG (START)und P (Proceed)
}

unit FakePDP11ODTU; 
{
  Simuliert eine rudimentäre PDP-11 ODT console ( wie 11/73).
  Sie dient nur zum Test der GUI

  - Deposit, , mit /n, /g
  - Examine, /n, /g
  - ^C


globals:
  last_addr letzte offene adresse, string


   state          keys            action                                  newstate
   prompt         0..7 $, a-Z     chg last_addr                           addr_entering
   prompt         CR              clr last_addr                           prompt
   prompt         P                                                       go

   addr_entering   0..7 $, a-Z    chg last_addr                           addr_entering
   addr_entering   /              eval last_addr                          addr_open, wenn valid addr. Sonst terror
   addr_enttering  G              pc := last_addr                         go
   addr_open      CR                                                      prompt
   addr_open      LF              last_addr+=2, prompt+print last_addr,   addr_open
   addr_open      0..7            curval := digit                              val_entering

   val_entering  0..7            chg curval                               val_entering
   val_entering  CR              last_addr := curval                      save                                         +prompt
   val_entering  LF               last_addr := curval, last_addr+=2,
                                  prompt+print  last_addr,                addr_open

  error: print ?,       prompt


   error

   go             read HALT   prompt



 Zusatz zur Documentation, ausgetestet von Steve Maddison, www.cosam.org:

 2008/9/5 Jörg Hoppe <j_hoppe@t-online.de>:
> > - odd addresses? ("1001/")

They're rejected, and your response is correct too:
@1001/?
@

> > - addresses wich cause unibus timeout? As "1777774"

For non-existent addresses, you get zero back straight away. Writing
also appears to succeed immediately but has no effect (i.e. you still
get zero if you read it back).

> > - trash as address ("123xyz"). Does is echo illegal chars? Does print a "?"
> > immediately, or after the "/"?. Does it print "?" on the same or the next line?

@123x?
@

So the "x" is echoed, then the ? on the same line. You then get a new
prompt, so if you carried on you'd get:

@y?
@z?
@

> > - trash as data

@1000/000000 a?
@

The "a" is echoed and the ? follows immediately after.

> > - is the reaction on to "/ <CR>" correct? <CR> is the RETURN key of your
> > keyboard

Looks good

> > - is the reaction on to "/ <LF>" correct? <LF> must be entered as Ctrl-J

Almost - on your simulator I get an extra "@" between the lines:

@1000/000123 <LF>
@
@1002/000456 <LF>
@
[etc...]

On the PDP I get the lines one after another, and subsequent lines
have no @ at all.

@1000/000123 <LF>
1002/000456 <LF>
1004/000701 <LF>
[etc...]

> > - is the reaction on to "/ <octal data><CR>" correct?
> > - is the reaction on to "/ <octal data><LF>" correct?

CR is correct, LF has the same problem as above.

> > - is the reaction on "@/" correct?

Yes

> > - is the behaviour of P and G correct?

P looks correct, not sure what's happening with G. On the real PDP it
works like this:

@1000G

There's no more output, not even a CR of LF after the "G".

> > - is the output of stop on HALT correct?

Looks fine.

> > - is the power-on message correct?

Format is correct, but I get 173000 instead of 000000, although I
don't think it really matters at start-up.


}
interface 

uses 
  Windows, Classes, SysUtils, 
  JH_Utilities, 
  FakePDP11GenericU, 
  AddressU, 
  MemoryCellU ; 


type 

  TFakePDP11ODTState = ( 
      odtstateInit, 
      odtstatePrompt,   // @ wird angezeigt
      odtstateEnterLocationAddr, // user hat nach @ was getippt
      odtstateOpen,  // user hat / getippt, wert von addr wurde angezeigt
      odtstateEnterLocationContents // user tippt nach val neuen val ein
    ) ; 

  TFakePDP11ODT = class(TFakePDP11Generic) 
    private 
      LastLocationExpr: string ; // letzte geöffnete Location
      NextLocationExpr: string ; // logisch "nächste" Addresse

      LastLocationAddr: TMemoryAddress ; // letzte geöffnete Addresse

      procedure print(s:string) ; 

      // Consol-Commandos verarbeiten
      procedure doPrompt(print_prompt:boolean=true) ; // Eingabe reset
      procedure doOpenLocation(addrexpr:string) ; 
      procedure doCloseLocation(valexpr:string) ; // ohne Prompt
      procedure doRun(valexpr: string) ; 
      procedure doReset(valexpr: string) ; 
      procedure doSingleStep(valexpr: string) ; 
      procedure doError ; // ohne Prompt


    public 

      TheState: TFakePDP11ODTState ; 

      // Rüdiger Kurt 2016: hat Robotron A6402 mit K1630 CPU,
      // PDP-11 Nachbau, der als ODT prompt nicht "@", sondern "@ " zeigt.
      IsK1630: boolean ; 



      constructor Create(mat: TMemoryAddressType) ; override ; 

      procedure PowerOn ; override ; 
      procedure Reset ; override ; 

      // Interface zur seriellen Console
      function SerialReadByte(var b: byte) : boolean ; override ; 
      function SerialWriteByte(b: byte) : boolean ; override ; 

      // So tun, als ob eine laufende CPU auf ein HALT gerannt waere
      procedure doHalt ; override ; 

    end{ "TYPE TFakePDP11ODT = class(TFakePDP11Generic)" } ; 


implementation 

uses 
  OctalConst, 
  AuxU 
  ; 


constructor TFakePDP11ODT.Create(mat: TMemoryAddressType) ; 
  begin 
    inherited Create(mat) ; 
    IsK1630 := false ; 
  end; 

// Speicher löschen
procedure TFakePDP11ODT.PowerOn ; 
  var i: integer ; 
  begin 
    for i := 0 to PhysicalMemorySize - 1 do 
      Mem[i] := 0 ; 

    SerialInBuff := '' ; 
    SerialOutBuff := '' ; 
    Reset ; 
  end; 

procedure TFakePDP11ODT.Reset ; 
  begin 
    TheState := odtstateInit ; 

    SerialInBuff := '' ; 

    LastLocationExpr := '' ; 

    // start PC = 173000 auf PDP-11/73
    setMem(ProgramCounterAddr, _173000) ; 

    doHalt ; // tue so, also ob ein HALT gekommen waere
  end; 



// Interface zur serialle Console
function TFakePDP11ODT.SerialReadByte(var b: byte) : boolean ; 
  begin 
    if SerialOutBuff = '' then 
      result := false // buffer leer: nix zu lesen da!
    else begin 
      // ältestes Zeichen zurückgeben
      b := byte(SerialOutBuff[1]) ; 
      SerialOutBuff := Copy(SerialOutBuff, 2, maxint) ; 
      result := true ; 
    end; 
  end; 

procedure TFakePDP11ODT.print(s:string) ; 
  begin 
    SerialOutBuff := SerialOutBuff + s ; 
  end; 


// Prompt "@" drucken und alles für neue Zeile vorbereiten.
// das "@"  kann auch unterdrückt werden, siehe <LF>-Problematik.
procedure TFakePDP11ODT.doPrompt(print_prompt:boolean=true) ; 
  begin 
    if print_prompt then begin 
      if IsK1630 then 
//        print(CHAR_CR + CHAR_LF + ' ?'+CHAR_LF + CHAR_CR + '@ ')
        print(CHAR_LF + CHAR_CR + '@ ') 
      else 
        print(CHAR_CR + CHAR_LF + '@') ; 
    end else 
      print(CHAR_CR + CHAR_LF) ; 
    SerialInBuff := '' ; 
    TheState := odtstatePrompt ; 
  end; 

// Wert einer Adresse anzeigen. addrexpr muss ein gültiger
// addressausdruck sein: octal, oder R0..R7, $0..$§7 $S,RS, oder ähnlich
//
// Ausserdem wird hier gleich "next_location" berechnet.
// das ist "+2" für octal adressen, "+! und rollaround für Regsiter,
// noop für PSW
procedure TFakePDP11ODT.doOpenLocation(addrexpr:string) ; 
  var 
    c: char ; 
    i: integer ; 
    val: dword ; 
    nextlocationaddr: TMemoryAddress ; 
  begin 
    NextLocationExpr := '???' ; // invalidate ;
    addrexpr := Uppercase(addrexpr) ; 
    LastLocationAddr.mat := mat ; 
    LastLocationAddr.val := MEMORYCELL_ILLEGALVAL ; 
    if (addrexpr = '') or not CharInSet(addrexpr[1], ['0'..'7','R','$']) then 
      raise EFakePDP11Error.Create('') ; 
    if isOctalDigit(addrexpr[1]) then begin 
      // octal address: use only last 8 digits
      addrexpr := Copy(addrexpr, length(addrexpr)-7, maxint) ; 
      LastLocationAddr := OctalStr2Addr(addrexpr, mat) ; 

      // Abfrage von R0..R7 geht nur mit "R0".."R7". Aber PSW geht mit 17777776 !
      // (Chapter 3.5.1)
      if (LastLocationAddr.val >= PhysicalIopageBaseAddr + _17700) 
              and (LastLocationAddr.val <= PhysicalIopageBaseAddr + _17707) then 
        raise EFakePDP11Error.Create('') ; 

      if odd(LastLocationAddr.val) then 
        raise EFakePDP11Error.Create('') ; 

      LastLocationExpr := Addr2OctalStr(LastLocationAddr) ; 
      nextlocationaddr := LastLocationAddr ; // init
      if LastLocationAddr.val < PhysicalIopageBaseAddr + _17776 then 
        nextlocationaddr.val := LastLocationAddr.val+2 // +=2 für octal adressen
      else 
        nextlocationaddr.val := 0 ; // roll around

      NextLocationExpr := Addr2OctalStr(nextlocationaddr) ; 
    end { "if isOctalDigit(addrexpr[1])" } else if CharInSet(addrexpr[1], ['R','$']) then begin 
      c := addrexpr[1] ; 
      addrexpr := Copy(addrexpr, 2, maxint) ; // Ziffern abschneiden
      // die letzten 3 Ziffern testen
      if length(addrexpr) >= 3 then 
        addrexpr := Copy(addrexpr, length(addrexpr)-2, maxint) ; 
      if (addrexpr = '077') or (addrexpr = '477') then begin 
        LastLocationAddr.val := PhysicalIopageBaseAddr + _17776 ; // $077, Rxxx477: alles PSW!
        addrexpr := 'RS' ; 
        LastLocationExpr := 'RS' ; 
        NextLocationExpr := 'RS' ; // keine nächste Adresse
      end else begin 
        // wie "R0", "$6", nur die letzte Ziffer beachten
        c := addrexpr[length(addrexpr)] ; 
        case c of 
          'S': begin 
            LastLocationAddr.val := PhysicalIopageBaseAddr + _17776 ;  // RS -> PSW
            LastLocationExpr := 'RS' ; 
            NextLocationExpr := 'RS' ; // keine nächste Adresse
          end ; 
          '0'..'7': begin 
            i := ord(c) - ord('0') ; 
            LastLocationAddr.val := PhysicalIopageBaseAddr + _17700 + i ; // R0..R7
            LastLocationExpr := 'R' + IntToStr(i) ; 
            i := (i + 1) mod 8 ; // roll around
            NextLocationExpr := 'R' + IntToStr(i) ; 
          end ; 
          else 
            LastLocationAddr.val := MEMORYCELL_ILLEGALVAL ; 
        end{ "case c" } ; 
      end{ "if (addrexpr = '077') or (addrexpr = '477') ... ELSE" } ; 
    end { "if CharInSet(addrexpr[1], ['R','$'])" } ; 

    if LastLocationAddr.val = MEMORYCELL_ILLEGALVAL then 
      raise EFakePDP11Error.Create('') ; 

    val := getMem(LastLocationAddr) ; 
    if val =  MEMORYCELL_ILLEGALVAL then 
      raise EFakePDP11Error.Create('') ; 

    // Ausgabe. das "/" ist schon angezeigt
    print(Dword2OctalStr(val, 16) + ' ') ; 

    SerialInBuff := '' ; 
    TheState := odtstateOpen ; 

  end{ "procedure TFakePDP11ODT.doOpenLocation" } ; 


// offene Adresse "last_location" schliessen
// wenn valexpr <> '': prüfen und neuen Wert schreiben
procedure TFakePDP11ODT.doCloseLocation(valexpr:string) ; 
  var 
    val: dword ; 
  begin 
    valexpr := Uppercase(valexpr) ; 
    if valexpr <> '' then begin 
      // nur die letzten 6 Ziffern beachten
      valexpr := Copy(valexpr, length(valexpr)-5, maxint) ; 
      val := OctalStr2Dword(valexpr, 0) ; 
      if val = MEMORYCELL_ILLEGALVAL then 
        raise EFakePDP11Error.Create('') ; 
      // !!! Die ODT-Console auf 11/73 schreibt illegale Adressen klaglos
//      try
      setMem(LastLocationAddr, val); 
//      except
//      end;
    end; 
  end{ "procedure TFakePDP11ODT.doCloseLocation" } ; 

// setze PC und starte PSeudo run, der nach Zufallszeit an Zufallsadresse endet
procedure TFakePDP11ODT.doRun(valexpr: string) ; 
  var 
    val: dword ; 
  begin 
    if valexpr <> '' then begin 
      // nur die letzten 6 Ziffern beachten
      valexpr := Copy(valexpr, length(valexpr)-5, maxint) ; 
      val := OctalStr2Dword(valexpr, 0) ; 
      if val = MEMORYCELL_ILLEGALVAL then 
        raise EFakePDP11Error.Create('') ;
      RunToHalt(val) ;
    end; 

  end{ "procedure TFakePDP11ODT.doRun" } ; 


procedure TFakePDP11ODT.doReset(valexpr: string) ; 
  var 
    val: dword ; 
  begin 
    if valexpr <> '' then begin 
      // nur die letzten 6 Ziffern beachten
      valexpr := Copy(valexpr, length(valexpr)-5, maxint) ; 
      val := OctalStr2Dword(valexpr, 0) ; 
      if val = MEMORYCELL_ILLEGALVAL then 
        raise EFakePDP11Error.Create('') ; 
      setMem(ProgramCounterAddr, val); // PC setzen
      doHalt ; // stop
    end; 
  end{ "procedure TFakePDP11ODT.doReset" } ; 


procedure TFakePDP11ODT.doSingleStep(valexpr: string) ; 
  var 
    val: dword ; 
  begin 
    if valexpr <> '' then begin 
      // nur die letzten 6 Ziffern beachten
      valexpr := Copy(valexpr, length(valexpr)-5, maxint) ; 
      val := OctalStr2Dword(valexpr, 0) ; 
      if val = MEMORYCELL_ILLEGALVAL then 
        raise EFakePDP11Error.Create('') ; 

      setMem(ProgramCounterAddr, val+2); // PC eins weiter setzen
      doHalt ; // stop
    end ; 
  end{ "procedure TFakePDP11ODT.doSingleStep" } ; 


procedure TFakePDP11ODT.doError ; 
  begin 
    // Ausgabe. das "/" ist schon angezeigt
    if IsK1630 then 
      print(' ?') 
    else 
      print('?') ; 
  end; 


// EIn Zeichen in die Zustandsmachine eingeben

function TFakePDP11ODT.SerialWriteByte(b: byte) : boolean ; 
  var 
    c: char ; 
  begin 
    // Wenn die fiktive CPU aktiv ist, gibts keine Console
    if isRunning then 
      Exit ; 

    c := char(b and $7f) ; // 7Bit
    result := true ; // write klappt immer

    try 
      // Fallunterscheidungen nicht nach  States, sondern nach Keys, denn so ist die DEC-Doku
      // EK-KDJ1B-UG_KDJ11-B_Nov86.pdf, [3.4.1]
      case UpCase(c) of 
        'R', '$', 'S': 
          // oder wird alles ASCII > #$20 geechot?
          begin // nur im Adressfeld
            print(c) ; 
            case TheState of 
              odtstatePrompt: begin 
                SerialInBuff := SerialInBuff + c ; // das wird in beinahe allen States gebraucht
                TheState := odtstateEnterLocationAddr ; 
              end ; 
              odtstateEnterLocationAddr: 
                SerialInBuff := SerialInBuff + c ; // das wird in beinahe allen States gebraucht
              else 
                raise EFakePDP11Error.Create('') ; 
            end; 
          end{ "case UpCase(c) of 'R', '$', 'S':" } ; 
        '0'..'7': begin // octalsziffern sind in location addr und value erlaubt
          print(c) ; 
          SerialInBuff := SerialInBuff + c ; 
          case TheState of 
            odtstatePrompt: 
              TheState := odtstateEnterLocationAddr ; 
            odtstateOpen: 
              TheState := odtstateEnterLocationContents ; 
          end; 
        end ; 
        '/': begin 
          print(c) ; 
          case TheState of 
            odtstatePrompt: // slash immediately after prompt
              if LastLocationExpr = '' then 
                raise EFakePDP11Error.Create('') 
              else 
                doOpenLocation(LastLocationExpr) ; 
            odtstateEnterLocationAddr: 
              doOpenLocation(SerialInBuff); 
            else 
              raise EFakePDP11Error.Create('') ; 
          end; 
        end{ "case UpCase(c) of '/':" } ; 
        CHAR_CR: begin // Close Address.
          // DO NOT echo the <CR> !
          case TheState of 
            odtstatePrompt: 
              doPrompt ; // my guess!
            odtstateOpen: begin 
              doCloseLocation('') ; // do not change
              doPrompt ; 
            end; 
            odtstateEnterLocationContents: begin 
              doCloseLocation(SerialInBuff) ; // change
              doPrompt ; 
            end else 
              raise EFakePDP11Error.Create('') ; 
          end{ "case TheState" } ; 
        end{ "case UpCase(c) of CHAR_CR:" } ; 
        CHAR_LF: begin // close and open next
          // do NOT echo <LF> !
          // ZUS: "next_location" wurde schon in "doOpenLocation() berechnet
          // wenn next_location = 0: roll around von max addr to 0:
          // LF verhält sich dann aber wie <CR>!
          case TheState of 
            odtstateOpen, odtstateEnterLocationContents: begin 
              if TheState = odtstateOpen then 
                doCloseLocation('')  // do not change.
              else doCloseLocation(SerialInBuff) ; // change
              // Not clear, wether the next line is preceeded by a @ or not:
              // documentation says YES, a real 11/73 did NOT show a prompt.
              // ODT-11 also has no @
              if (NextLocationExpr = '00000000') or (LastLocationExpr = 'RS') then begin // STOP des auto inc
                doPrompt(true) ; // normal prompt
              end else begin 
                doPrompt(false) ; // do NOT print "@"
                print(NextLocationExpr) ; print('/') ; // show next location
                doOpenLocation(NextLocationExpr); 
              end; 
            end { "case TheState of odtstateOpen, odtstateEnterLocationContents:" } else // LF an der falschen Stelle
              raise EFakePDP11Error.Create('') ; 
          end{ "case TheState" } ; 
        end{ "case UpCase(c) of CHAR_LF:" } ; 
        // go, proceed: NO-OP
        'G': begin 
          print(c) ; 
          case TheState of 
            odtstateEnterLocationAddr: // adresse muss eingeben sein
              if RunMode then
                doRun(SerialInBuff) 
              else doReset(SerialInBuff) 
            else 
              raise EFakePDP11Error.Create('') ; 
          end; 
        end; 
        'P': begin // proceed ab PC
          print(c) ; 
          case TheState of 
            odtstatePrompt: begin 
              if RunMode then 
                doRun(Dword2OctalStr(getMem(ProgramCounterAddr), 16)) 
              else doSingleStep(Dword2OctalStr(getMem(ProgramCounterAddr), 16)) ; 
            end else 
              raise EFakePDP11Error.Create('') ; 
          end; 
        end; 
        // illegale Zeichen: echo und Error
        else begin 
          // K1630 erlaubt suffix "A" nach adressen: das sind dann absolute, nicht virtual
          // ignore
          if (c = 'A') and IsK1630 and (TheState = odtstateEnterLocationAddr) then begin
            // ignore all 'A's in address, not just the last
            print(c) ;
          end else begin
            // Error
            print(c) ;
            raise EFakePDP11Error.Create('') ;
          end; 
        end; 
      end{ "case UpCase(c)" } ; 

      // Fehlermeldungen der Console
    except 
      on e: EFakePDP11Error do begin 
        doError ; 
        doPrompt ; 
      end; 
    end{ "try" } ; 

  end{ "function TFakePDP11ODT.SerialWriteByte" } ; 


procedure TFakePDP11ODT.doHalt ; 
  var pc: dword ; 
  begin 
    pc := getMem(ProgramCounterAddr) ;
    if IsK1630 then // K1630 puts an "Esc S" before cr/lf on HALT
      print(CHAR_ESC + 'S' + CHAR_CR + CHAR_LF + Dword2OctalStr(pc, 16))
    else
      print(CHAR_CR + CHAR_LF + Dword2OctalStr(pc, 16)) ;
    doPrompt ; 
  end; 


end{ "unit FakePDP11ODTU" } . 
