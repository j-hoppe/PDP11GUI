unit MemoryCellU;
{
  Ein Window (CPU, IOpage, Mem) zeigt immer eine Liste von
  memory cells an

}

interface 

uses 
  Windows, 
  Classes, 
  SysUtils, 
  StdCtrls, 
  JvExGrids, JvStringGrid,
  JH_Utilities,
  AddressU, 
  IniFiles, 
  BitFieldU 
  ; 

const 
  // illegaler Wert für PDP-11-Adressen und Werte.
  MEMORYCELL_ILLEGALVAL = $ffffffff ; 

  // Adressen für spezielle interne Register, die nicht im regulären
  // Adressraum vorkommen, aber mit besonderen Consol-Befehlen abfragbar
  // sind.
  // Die hier verwendeten Adressen werden nie an die Machinen weitergeleitet.
  // die Console-Adapter kapseln sie.
  // address.mat muss matSpecialRegister sein!
  MEMORYCELL_SPECIALADDR_DISPLAYREG = 0 ; 
  // 177570 enthält bei lesen die Switches, schreiben geht auf die Consol-LEDs.
  // SimH kann es aber trotzdem abfragen.
  MEMORYCELL_SPECIALADDR_SWITCHREG = 1 ; 


type 
  TMemoryCellGroup = class; 

// hat eine Adresse und einen 16bit wert
  TMemoryCell = class(TCollectionItem) 
    public 
      addr: TMemoryAddress ; 
      // nach "Deposit" ist edit-value und pdp_value gleich
      edit_value: dword ; // Wert, wie vom User angegeben. 16bit, oder "illegal value"
      pdp_value: dword ; // Wert, wie aus der pdp 11 gelesen
      name: string[32] ; // falls es ein bekanntes Register ist
      info: string ; // gelber Hint im Editor

      // da sie in Grid angezeigt werden:
      grid: TJvStringGrid ; 
      grid_r, grid_c: integer ; // row und column

      // wenn sie aus einem MACRO11-Listing stammen
      listinglinenr: integer ; // 1. zeile = 0

      // Nein: zur Laufzeit über addr besorgen!
//      bitfieldsdef: TBitfieldsDef ; // <> nil, wenn definition da.

      memorycellgroup: TMemoryCellGroup ; // uplink

      tag: integer ; // multi function mark

      constructor Create(Collection: TCollection); override ; 

      procedure Assign(mc: TMemoryCell) ; 

      procedure Examine ;
      procedure Deposit ;

    end{ "TYPE TMemoryCell = class(TCollectionItem)" } ; 


  // wenn eine Zelle "von alleine" = nicht durch User in über GUI, sondern über
  // Console oder weil eine andre zelle gleicher adresse geändert wurde:
  TMemoryCellChangeEvent = procedure (sender { = memorycellgroup}: TObject; memorycell: TMemoryCell) of object ; 


  // Ein zusammengehörender Satz Speicherzellen
  // zB die Register eines Controllers
  TMemoryCellGroup = class(TCollectionItem) 
    private 
      memorycells: TCollection ; // of TMemoryCell
    public 
      usagetag: string ; // freies internes tag
      groupname: string ; 
      groupinfo: string ; 

      mat: TMemoryAddressType ; // alle MemoryCell haben diesen adresstype

//      enabled: boolean ;

      min_addr, max_addr: TMemoryAddress ; // kleinste und grösste vorkommende Adresse
      // Zellen sind nicht sequentiell, aber doch dicht beieiander ...
      // dient der schnellen Filterung für CellIndex()

      associate: TObject ; // beliebiges object

      OnMemoryCellChange: TMemoryCellChangeEvent ; 

      PdpOverwritesEdit: boolean ; 

      constructor Create(Collection: TCollection) ; override ; // alle MemoryCell haben diesen adresstype

      destructor Destroy ; override ; 

      procedure Clear ; 
      procedure Invalidate ; 

      procedure Assign(mcg: TMemoryCellGroup) ; 

      function getCount: integer ; 
      function Cell(idx: integer): TMemoryCell ; inline ; 
      function Add(addrval: dword): TMemoryCell ; overload ; 
      procedure Add(startaddrval: dword; wordcount: integer) ; overload ; 
      function Insert(idx: integer ; addrval: dword): TMemoryCell ; 
      procedure Delete(idx:integer) ; 

      procedure Examine(unknown_only: boolean ; abortable:boolean) ; // alle aus PDP-11 laden, nach pdp_val
      procedure Deposit(optimize: boolean ; abortable:boolean ) ; // alle in PDP-11 schreiben. ggf  nur geänderte
      // fordert danach übergeordnetes "Grous" auf, nachbar cellroups zu aktualisieren

      procedure calcAddrRange ; 
      procedure extendAddrRange(addrval: dword) ; inline ; 

      function CellIndexByAddr(addr:TMemoryAddress): integer ; // Zelle über Adresse finden. -1: not found

      procedure ShiftRange(startaddr:TMemoryAddress;newsize: integer; optimize: boolean) ; 

      property Count: integer read  getCount ; 

      procedure Sort ; 

    end{ "TYPE TMemoryCellGroup = class(TCollectionItem)" } ; 

  // Mehrere MemoryCellGroup
  TMemoryCellGroups = class(TCollection) 
    public 
      // Bitfeld-Definitionen
      bitfieldsdefs : TBitfieldsDefs ; 

      constructor Create ; 
      destructor Destroy ; override ; 
      function PreprocessIniFile(inputfilename:string): string ; 
      procedure AddGroupsFromIniFile(aUsageTag: string ; fname:string) ; 
      function AddSequentialMemoryCellsAsGroup( 
              mat: TMemoryAddressType; 
              aUsageTag, aGroupname: string ; startaddr: dword ; cellcount: integer) : 
              TMemoryCellGroup ; 
      procedure SyncMemoryCells(memorycell: TMemoryCell) ; 
      function getSymbolInfoCell(memorycell: TMemoryCell): TMemoryCell ; 
      procedure ChangeAdddressWidth(newMat: TMemoryAddressType) ; 

    end{ "TYPE TMemoryCellGroups = class(TCollection)" } ; 

  // Sortierfunktion für Listen
function MemoryCellSortCompare(item1, item2: Pointer): integer; 


implementation 

uses 
  Forms, 
  AuxU, 
  OctalConst, 
  ConsolePDP1144U, 
  FormMainU, 
  AppControlU 
  ; 


constructor TMemoryCell.Create(Collection: TCollection); 
  begin 
    inherited Create(Collection) ; 
    addr.mat := matUnknown ; // TMemoryCellGroup.Add setzt es
    addr.val := MEMORYCELL_ILLEGALVAL ; 
    pdp_value := MEMORYCELL_ILLEGALVAL ; 
    edit_value := MEMORYCELL_ILLEGALVAL ; 
    name := '' ; 
    info := '' ; 

    grid := nil ; 
    grid_r := 0 ; 
    grid_c := 0 ; 
  end{ "constructor TMemoryCell.Create" } ; 

// adresse und werte kopieren, lsiten zugehörigkeiten unverändert lassen
procedure TMemoryCell.Assign(mc: TMemoryCell) ; 
  begin 
    addr := mc.addr ; 
    edit_value := mc.edit_value; 
    pdp_value := mc.pdp_value; 
    name := mc.name; 
    info := mc.info; 
  end; 


procedure TMemoryCell.Examine ; 
  var mcg: TMemoryCellGroup ; 
  begin 
    FormMain.PDP11Console.ClearState ; 
    pdp_value := FormMain.PDP11Console.Examine(addr) ; 
    // dieselbe Zelle in allen MemoryCellGroups aktualisieren
    // dort werden die OnMemoryCellChange-Callbacks aufgerufen
    mcg := self.memorycellgroup ; 
    (mcg.Collection as TMemoryCellGroups).SyncMemoryCells(self) ; 
  end; 


procedure TMemoryCell.Deposit ; 
  var mcg: TMemoryCellGroup ; 
  begin 
    FormMain.PDP11Console.ClearState ; 
    FormMain.PDP11Console.Deposit(addr, edit_value) ; 
    pdp_value := edit_value ; 
    // dieselbe Zelle in NachbarGroups aktualisieren,
    // dort werden die OnMemoryCellChange-Callbacks aufgerufen
    mcg := self.memorycellgroup ; 
    (mcg.Collection as TMemoryCellGroups).SyncMemoryCells(self) ; 
  end; 


constructor TMemoryCellGroup.Create(Collection: TCollection) ; 
  begin 
    inherited Create(Collection) ; 
    mat := matUnknown ; // muss von anwender gesetzt werden!
    groupname := '' ; 
    groupinfo := '' ; 
//    enabled := false ;
    associate := nil ; 
    memorycells := TCollection.Create(TMemoryCell) ; // of TMemoryCell

    PdpOverwritesEdit := true ; 

    calcAddrRange ; 
  end{ "constructor TMemoryCellGroup.Create" } ; 


destructor TMemoryCellGroup.Destroy ; 
  begin 
    memorycells.Destroy ; 
    inherited Destroy ; 
  end; 


procedure TMemoryCellGroup.Clear ; 
  begin 
    memorycells.Clear ; 
    calcAddrRange ; 
  end; 


procedure TMemoryCellGroup.Invalidate ; 
  var i: integer ; 
  begin 
    for i := 0 to Count - 1 do 
      Cell(i).pdp_value := MEMORYCELL_ILLEGALVAL ; 
  end; 


procedure TMemoryCellGroup.Assign(mcg: TMemoryCellGroup) ; 
  var i: integer ; 
    mc: TMemoryCell ; 
  begin 
    memorycells.Clear ; // alle löschen
    mat := mcg.mat ; // Typ übernehmen, für Zellkopieen
    for i := 0 to mcg.Count - 1 do begin 
      mc := Add(mcg.Cell(i).addr.val) ; 
      mc.Assign(mcg.Cell(i)); 
    end; 
    groupname := mcg.groupname ; 
    groupinfo := mcg.groupinfo ; 

    calcAddrRange ; 
  end{ "procedure TMemoryCellGroup.Assign" } ; 


// berechnet niedrigste und höchste vorkommende Adresse
procedure TMemoryCellGroup.calcAddrRange ; 
  var i: integer ; 
  begin 
    if Count = 0 then begin 
      min_addr.val := MEMORYCELL_ILLEGALVAL ; 
      max_addr.val := MEMORYCELL_ILLEGALVAL ; 
    end else begin 
      min_addr := Cell(0).addr ; 
      max_addr := Cell(0).addr ; 
      // was, wenn Cell(0)    MEMORYCELL_ILLEGALVAL hat? kommt ja nicht vor
      assert(min_addr.val <> MEMORYCELL_ILLEGALVAL) ; 

      for i := 1 to Count - 1 do 
        with Cell(i) do 
          if addr.val <> MEMORYCELL_ILLEGALVAL then begin 
            if addr.val < min_addr.val then 
              min_addr := addr ; 
            if addr.val > max_addr.val then 
              max_addr := addr ; 
          end; 
    end{ "if Count = 0 ... ELSE" } ; 

  end{ "procedure TMemoryCellGroup.calcAddrRange" } ; 

// zu gültiger Cellist kommt addrval dazu
// Schnell! Für .Add
// Achtung: sicherstellen, dass alle
// 'Clear'-Methoden  das unoptimierte calcAddrRange() aufrufen,
// um den Range gültig zu initialisieren.
procedure TMemoryCellGroup.extendAddrRange(addrval:dword) ; 
  begin 
    if min_addr.val = MEMORYCELL_ILLEGALVAL then 
      min_addr.val := addrval // untergrenze undefiniert: jetzt bekannt
    else if min_addr.val > addrval then 
      min_addr.val := addrval ; // neuer Wert unter Untergrenze
    if max_addr.val = MEMORYCELL_ILLEGALVAL then 
      max_addr.val := addrval // untergrenze undefiniert: jetzt bekannt
    else if max_addr.val < addrval then 
      max_addr.val := addrval ; // neuer Wert unter Untergrenze
  end ; 


function TMemoryCellGroup.getCount: integer ; 
  begin 
    if memorycells = nil then 
      result := 0 
    else 
      result := memorycells.Count ; 
  end; 


function TMemoryCellGroup.Cell(idx: integer): TMemoryCell ; 
  var ci: TCollectionItem ; 
  begin 
    ci := memorycells.Items[idx] ; 
    assert(ci is TMemoryCell) ; 
    result := memorycells.Items[idx] as TMemoryCell ; 
  end; 

function TMemoryCellGroup.Add(addrval: dword): TMemoryCell ; 
  begin 
    // wenn die Cells strikt aufwärtszählend geADDed werden,
    // ist die liste sorted.
    result := memorycells.Add as TMemoryCell ; 
    assert(mat <> matUnknown) ; 
    result.addr.mat := mat ; // von group übernehmen
    result.addr.val := addrval ; 
    result.memorycellgroup := self ; // uplink
    extendAddrRange(addrval) ;  // optimiert
//    calcAddrRange ;
  end; 

// Einen Bereich aufeinander folgender Cells erzeugen
procedure TMemoryCellGroup.Add(startaddrval: dword ; wordcount: integer) ; 
  var i: dword ; 
  begin 
    // MemoryCellgroup für Bytes aufbauen
//        Clear ; // alle zellen löschen
    for i := 0 to wordcount - 1 do 
      Add(startaddrval + 2 * i) ; 
//    calcAddrRange ;
    Invalidate ; 
//      assert(wordcount = Count) ;
  end ; 


procedure TMemoryCellGroup.Delete(idx:integer) ; 
  begin 
    memorycells.Delete(idx) ; 
    calcAddrRange ; 
  end; 

function TMemoryCellGroup.Insert(idx: integer; addrval: dword): TMemoryCell ; 
  begin 
    result := memorycells.Insert(idx) as TMemoryCell ; 
    result.addr.mat := mat ; // von group übernehmen
    result.addr.val := addrval ; 
    extendAddrRange(addrval) ; // optimierung
//    calcAddrRange ; 
  end; 




// Zelle über Adresse suchen. -1 = not found
function TMemoryCellGroup.CellIndexByAddr(addr:TMemoryAddress): integer ; // Zelle über Adresse finden. -1: not found
  var i: integer ; 
  begin 
    result := -1 ; 
    if addr.val = MEMORYCELL_ILLEGALVAL then 
      Exit ; 

    // schnell raus kriegen, ob die Suche lohnt
    if (min_addr.val <= addr.val) and (addr.val <= max_addr.val) then begin 
      for i := 0 to Count-1 do // Zelle
        if Cell(i).addr.val = addr.val then begin 
          result := i ; 
          Exit ; 
        end ; 
    end; 
  end{ "function TMemoryCellGroup.CellIndexByAddr" } ; 


// eine ganze Liste von Zellen auslesen
// "unknown_only": nur zellen mit pdp_value = MEMORYCELL_ILLEGALVAL
procedure TMemoryCellGroup.Examine(unknown_only: boolean; abortable:boolean) ; // aus PDP-11 laden
  var i: integer ; 
  begin 
    if FormMain.PDP11Console = nil then 
      Invalidate // alle Werte auf MEMORYCELL_ILLEGALVAL setzen
    else begin 
      FormMain.PDP11Console.ClearState ; 
      FormMain.PDP11Console.Examine(self, unknown_only, abortable) ;
    end; 
    for i := 0 to Count - 1 do begin 
      // dieselbe Zelle in NachbarGroups aktualisieren
      // dort werden die OnMemoryCellChange-Callbacks aufgerufen
      (self.Collection as TMemoryCellGroups).SyncMemoryCells(Cell(i)) ; 
    end; 
  end{ "procedure TMemoryCellGroup.Examine" } ; 


// fordert danach übergeordnetes "Groups" auf, nachbar cellgroups zu aktualisieren
// optimize: nur ausgeben, wenn edit_value <> pdp_value
procedure TMemoryCellGroup.Deposit(optimize: boolean; abortable:boolean) ; // alle in PDP-11 schreiben, ggf nur geänderte
  begin 
    FormMain.PDP11Console.ClearState ; 
    FormMain.PDP11Console.Deposit(self, optimize, abortable) ;
    // dieselbe Zelle in NachbarGroups aktualisieren,
  end; 


// setzt die addresse der ersten zelle, und inkrementiert die
// nachfolgenden in Schritten von 2.
// Verändert alle Adresse, aber nicht die Anzahl der Zellen.
// gedacht für Scrollen in einem MemoryTable, oder update
// des Disassembler-Speicherauszugs.
// Optimierung: bekannte Werte für Adressen werden soweit wie möglich erhalten
// newsize: neue Länge, ab neuem "startaddr",
// oder -1, wenn Länge unverändert bleiben soll.
// Danach muss für neue Zellen "Examine" ausgeführt werden.
procedure TMemoryCellGroup.ShiftRange(startaddr:TMemoryAddress; newsize: integer ; optimize: boolean) ; 
  var 
    tmp_mcg: TMemoryCellGroup ; // speichert die alten Werte
    i: dword ; 
    idx: integer ; 
  begin 
    // alten Zellen
    mat := startaddr.mat ; // Typ der Liste umstellen
    tmp_mcg := TMemoryCellGroup.Create(nil) ; 
    try 
      tmp_mcg.Assign(self) ; // Kopie ziehen

      // wenn vergrösserung der Liste:
      // erst verlängern, dann verschieben
      // (falls startaddr verkleinert wird und gleichzeiig size vergrössert, bleibt
      // das ende der liste gültig
      // verkleinerung: NACH verschieben 
      if newsize < 0 then newsize := Count ; 

      //  with tmp_mcg do  // Vergrössern vor shift
      while newsize > Count do begin // vergrösserung der Liste, hinten anhängen
        Insert(Count, max_addr.val+2) ; // count und max_addr wird mitgepflegt!
      end ; 


      // von allen Zellen die Adresse ändern
      // Name der zelle wird dadurch ungültig!
      for i := 0 to Count - 1 do 
        with Cell(i) do begin 
          addr.mat := startaddr.mat ; 
          addr.val := startaddr.val + 2*i ; 
          idx := tmp_mcg.CellIndexByAddr(addr) ; // suche adresse in Sicherungskopie
          if optimize and (idx >= 0) then begin 
            // die Liste enthält weiterhin Zelle(i): kopiere
            Assign(tmp_mcg.Cell(idx)) ; // aktuelle zelle aus Sicherungskopie übernehmen
          end else begin // Zelle ist jetzt ungültig: neu laden
            pdp_value := MEMORYCELL_ILLEGALVAL ; 
            edit_value := MEMORYCELL_ILLEGALVAL ; 
            name := '' ; 
          end ; 
        end{ "with Cell(i)" } ; 
      calcAddrRange ; // neues min/maxaddr

      // Verkleinern nach shift , disemal NICHT tmp_mcg
      while newsize < Count do begin // vergrösserung der Liste, hinten anhängen
        Delete(Count-1) ; // Count wird mit gepflegt!
      end ; 


//      Examine({unknown_only}true);
    finally 
      tmp_mcg.Free ; 
    end{ "try" } ; 

  end{ "procedure TMemoryCellGroup.ShiftRange" } ; 

{ sort after address }
procedure TMemoryCellGroup.Sort ; 
  var list: TList ; 
    i: integer ; 
  begin 
    list := TList.Create ; 
    list.Capacity := Count ; 
    try 
      // 1. list := collection
      for i := 0 to Count - 1 do 
        list.Add(memorycells.Items[i]) ; 

      // 2. sort
      list.Sort(MemoryCellSortCompare) ; 


      // 3. collection := list
      for i := 0 to Count - 1 do 
        TCollectionItem(list[i]).Index := i ; 
//        memorycells.items[i] := TCollectionItem(list[i]) ;

    finally 
      list.Free ; 
    end{ "try" } ; 
  end{ "procedure TMemoryCellGroup.Sort" } ; 



constructor TMemoryCellGroups.Create ; 
  begin 
    inherited Create(TMemoryCellGroup) ; 
    bitfieldsdefs := TBitfieldsDefs.Create ; 
  end; 


destructor TMemoryCellGroups.Destroy ; 
  begin 
    bitfieldsdefs.Free ; 
    inherited ; 
  end; 


// Liste aus einer Datei aufbauen
// dann hat jede Zelle einen Namen
// dient zum Aufbau der CPU-Register und der IOpage
// Format:
//  [listname]
//  name=addr;info
//
//  alle Addressen im File  sind physical 16 bit.

//  ThePhysicalAddressWidth muss bekannt sein!


// einen inifile durch den M4-preprocessor schicken.
// result: Name des temporären Ergebnisfiles
// den bacthfile 'm4-bat' ausführen
function TMemoryCellGroups.PreprocessIniFile(inputfilename:string): string ; 
  const timeout_millis = 5000 ; // M4 darf max 5 Sek laufen!
    // parameter  PDP11GUIEXEDIR und PDP11GUIAPPDATADIR setzen

  var 
    m4_path: string ; 
    outputfilename: string ; 

    args: string ; 
    workdir: string ; 
    starttime: dword ; 
    timeout: boolean ; 

    buff: array[0..1023] of AnsiChar ; 
    m4_appcontrol: TAppControl ; 

    errormsg: string ; 
  begin { "function TMemoryCellGroups.PreprocessIniFile" } 
    result := '' ; 
    errormsg := '' ; 
    // M4.BAT ausführen
    // macro11.bat <sourcefilename> <listingfilename>

    // macro11 im Verzeichnis der GUI finden
    m4_path := ExtractFilePath(Application.ExeName) + '\m4.bat' ; 
    if not FileExists(m4_path) then 
      raise Exception.CreateFmt('M4.bat not found, must be "%s"', [m4_path]) ; 

    // Variable PDP11GUIEXEDIR setzen, damit in M4.BAT der
    // Installationspath und das Includeverzeichnis bekannt ist.
    SetEnvironmentVariableA('PDP11GUIEXEDIR', strpcopy(buff, ExtractFileDir(Application.ExeName))) ; 
    SetEnvironmentVariableA('PDP11GUIAPPDATADIR', strpcopy(buff, FormMain.DefaultDataDirectory)) ; 

    // temporärer Zielfile erzeugen
    GetEnvironmentVariableA('TEMP', buff, sizeof(buff)) ; 
    outputfilename := Format('%s\tmp_pdp11gui_m4_out.ini', [buff]) ; 
    DeleteFile(outputfilename) ; 

    workdir := ExtractFilePath(inputfilename) ; // dir der inputdatei, damit includes im gleichen Verz gefunden werden
    args := Format('"%s" "%s"', [inputfilename, outputfilename]) ; 
    Log('Starting M4:') ; 
    Log('  Path    : %s', [m4_path]) ; 
    Log('  args    : %s', [args]) ; 
    Log('  work dir: %s', [workdir]) ; 
    m4_appcontrol := TAppControl.Create ; 
    try 
      with m4_appcontrol do begin 
        starttime := GetTickCount ; 
        StartApplication(m4_path, args, workdir) ; 
        sleep(500) ; 
//      if not ApplicationContact then
//        raise Exception.Create('MACRO11 does not start!') ;
        // warte, bis timeout, oder macro11 fertig
        repeat 
          Application.ProcessMessages ; 
          sleep(500) ; 
          timeout :=  GetTickCount > (starttime+timeout_millis) ; 
        until timeout or not ApplicationContact ; 
        if timeout then 
          raise Exception.CreateFmt('M4 timeout: running longer then %d secs', [timeout_millis div 1000]) ; 
      end{ "with m4_appcontrol" } ; 

    finally 
      m4_appcontrol.Free ; 
    end{ "try" } ; 

    // network delay
    starttime := GetTickCount ;
    repeat 
      Application.ProcessMessages ; 
      sleep(100) ;
      timeout :=  GetTickCount > (starttime+timeout_millis) ; 
    until timeout or FileExists(outputfilename) ; 

    if not FileExists(outputfilename) then begin 
      errormsg := Format('M4 failure: output file %s not found', [outputfilename]) ; 
      // Wenn eine Fehlerzeile da ist:
      Log(errormsg) ; 
      raise Exception.Create(errormsg) ; 
    end; 
    result := outputfilename ; // alles OK
  end{ "function TMemoryCellGroups.PreprocessIniFile" } ; 



procedure TMemoryCellGroups.AddGroupsFromIniFile(aUsageTag: string ; fname:string) ; 
  var 
    preprocessed_inifilename: string ; 
    inifile: TMemIniFile ; 
    sections, keys: TStringList ; 
    words: TStringList ; 
    i, j, k: integer ; 
//    k: dword ;
    keystr, regname: string ; 
    s_addr_from, s_addr_to: string ; 
    addr_from, addr_to: TMemoryAddress ; 
    s: string ; 
    enabled : boolean ; 
  begin { "procedure TMemoryCellGroups.AddGroupsFromIniFile" } 
    sections := TStringList.Create ; 
    keys := TStringList.Create ; 
    words := TStringList.Create ; 

    if not FileExists(fname) then 
      raise Exception.CreateFmt('Machine description file "%S" not found', [fname]) ; 

    // Zuerst inifile durch M4 schicken ...
    preprocessed_inifilename := PreprocessIniFile(fname) ; 

    inifile := TMemIniFile.Create(preprocessed_inifilename) ; 
    try 

      // Zuerst die globalen Bitfeld-Definitionen neu laden
      bitfieldsdefs.LoadFromIniFile(inifile) ; 

      inifile.ReadSections(sections) ; // alle [..] Abschnitte = alle Listen

      for i := 0 to sections.Count - 1 do 
        // Gruppen, die mit 'Bits.' anfangen sind keine Register-Definitionen!
        if pos('BITS.', Uppercase(sections[i])) <= 0 then 
          with Add as TMemoryCellGroup do begin 
            mat := matPhysical16 ; //
            usagetag := aUsageTag ; 
            groupname := sections[i] ; 
            enabled := true ; // default

            inifile.ReadSectionValues(sections[i], keys) ; 
            for j := 0 to keys.Count - 1 do begin 
              keystr := keys[j] ; 
//Log('register: Processing section[%d], key[%d]: %s', [i, j, keystr]) ;
              k := pos('=', keystr) ; 
              if k < 0 then 
                s := '' ; // break here
              s := Trim(StripQuotes(Copy(keystr,1, k-1))) ; // "INFO=", oder "<register>="
              keystr := Copy(keystr, k+1, maxint) ; // keystr jetzt alles hinter '='
              if Uppercase(s)='INFO' then 
                groupinfo := Trim(StripQuotes(keystr)) 
              else if Uppercase(s)='ENABLED' then begin 
                s := Trim(Uppercase(StripQuotes(keystr))) ; 
                if (s = '0') or (s = 'FALSE') then 
                  enabled := false ; 
              end else begin // "<register>="
                regname := s ; 
                words.Delimiter := ';' ; 
                words.DelimitedText := keystr +';;' ; // hat jetzt immer 3 words
                // Adresse: "addr" , oder "addr_von:addr_bis"
                s_addr_from := ExtractWord(1, words[0], [' ', ':']) ; 
                s_addr_to := ExtractWord(2, words[0], [' ', ':']) ; // maybe ''
                addr_from := OctalStr2Addr(s_addr_from, matPhysical16) ; 
                if s_addr_to = '' then // nur eine Adresse anlegen
                  addr_to := addr_from 
                else addr_to := OctalStr2Addr(s_addr_to, matPhysical16) ; 

                for k := 0 to (addr_to.val - addr_from.val) div 2 do // k: über alle adressen
                  with Add( addr_from.val + 2 * k ) do begin 
                    if addr_from.val = addr_to.val then begin 
                      name := regname ; 
                      info := Trim(StripQuotes(words[1])) ; 
                    end else begin // Word index mit in name und info
                      name := regname + Format('[%d]', [k]); 
                      info := Trim(StripQuotes(words[1])) 
                              + Format(' Word #%d, offset +%s', [k, Dword2OctalStr(2*k, 0)]) ; 
                    end; 
                    // suche die Liste mit den Bitfelder für dieses Register
                    s := Trim(words[2]) ; 
                    if s <> ''  then // Bitfield-Definition mit Memoryadresse verknüpfen
                      if not bitfieldsdefs.LinkAddr2BitfieldsDef(addr, s) then 
                        raise Exception.CreateFmt('Loading "%s": bitfields definition "%s" not found for "%s"', 
                                [fname, s, regname]) ; 
                    // memorycell anlegen
                  end { "with Add( addr_from.val + 2 * k )" } ; 
              end{ "if Uppercase(s)='ENABLED' ... ELSE" } ; 
            end{ "for j" } ; 

            // abgschaltete Definitionen gar nicht speichern
            if not enabled then 
              Free ; 
          end { "with Add as TMemoryCellGroup" } ; 

    finally 
      inifile.Free ; 
      words.Free ; 
      sections.Free ; 
      keys.Free ; 
//      if preprocessed_inifilename <> '' then
//      Deletefile(preprocessed_inifilename) ;

    end{ "try" } ; 
  end{ "procedure TMemoryCellGroups.AddGroupsFromIniFile" } ; 


// eine Gruppe aus einer aufeinanderfolgenden Liste von
// Zellen erzeugen. Adresssprung immer +2
function TMemoryCellGroups.AddSequentialMemoryCellsAsGroup( 
        mat: TMemoryAddressType; 
        aUsageTag, aGroupname: string ; startaddr: dword ; cellcount: integer) 
        : TMemoryCellGroup ; 
  var 
    i: dword ; 
    mcg: TMemoryCellGroup ; 
  begin 
    mcg := Add as TMemoryCellGroup ; 
    mcg.mat := mat ; 
    mcg.usagetag := aUsageTag ; 
    mcg.groupname := aGroupname ; 
    for i := 0 to cellcount - 1 do 
      mcg.Add(startaddr + 2*i) ; 
    mcg.Invalidate ; 
    result := mcg ; 
  end{ "function TMemoryCellGroups.AddSequentialMemoryCellsAsGroup" } ; 


// die angegebene memoryCell wurde verändert.
// checke, wo überall diese Adresse noch vorkommt
// und aktualisiere. Rufe bei Änderung das callback
// MemoryCellChange() auf, damit die Forms die Änderungen auch
// anzeigen.
// Wenn keine Editwerte verändert werden sollen,
// wird der event nicht ausgelöst
procedure TMemoryCellGroups.SyncMemoryCells(memorycell: TMemoryCell) ; 
  var i, idx: integer ; 
    mcg: TMemoryCellGroup ; 
    mc: TMemoryCell ; 
  begin 
    for i := 0 to Count - 1 do begin 
      mcg := Items[i] as TMemoryCellGroup ; 
      idx := mcg.CellIndexByAddr(memorycell.addr) ; // suche zelle mit der selben adresse in jeder Gruppe
      if mcg.PdpOverwritesEdit and (idx >= 0) then begin 
        mc := mcg.Cell(idx) ; 
        if (mc <> memorycell) and (mc.pdp_value <> memorycell.pdp_value) then begin 
          // Update and Notify
          mc.pdp_value := memorycell.pdp_value ; 
          if assigned(mcg.OnMemoryCellChange) then 
            mcg.OnMemoryCellChange( mcg, mc) ; 
        end; 
      end; 
    end{ "for i" } ; 

  end { "procedure TMemoryCellGroups.SyncMemoryCells" } ; 


// checkt alle Cells aller Cellgroups durch, ob eine mit gleicher
// adresse dabei ist, die SymbolInfo () hat.
// Es wird eine memorycell mit gleicheradresse und Symbolinfo
// zurückgegeben!

function TMemoryCellGroups.getSymbolInfoCell(memorycell: TMemoryCell): TMemoryCell ; 
  var i, idx: integer ; 
    mcg: TMemoryCellGroup ; 
    mc: TMemoryCell ; 
  begin 
    result := nil ; 
    for i := 0 to Count - 1 do begin 
      mcg := Items[i] as TMemoryCellGroup ; 
      idx := mcg.CellIndexByAddr(memorycell.addr) ; // suche zelle mit der selben adresse in jeder Gruppe
      if (idx >= 0) then begin 
        mc := mcg.Cell(idx) ; 
        if (mc <> memorycell) and (mc.name <> '') then begin 
          result := mc ; 
          Exit ; 
        end; 
      end; 
    end; 
  end{ "function TMemoryCellGroups.getSymbolInfoCell" } ; 


// alle Adressen von einer Bitlaenge auf eine andere umrechnen
procedure TMemoryCellGroups.ChangeAdddressWidth(newMat: TMemoryAddressType) ; 
  var 
    i, j: integer ; 
    mcg: TMemoryCellGroup ; 
    mc: TMemoryCell ; 
  begin 
    for i := 0 to Count - 1 do begin 
      mcg := Items[i] as TMemoryCellGroup ; 
      for j := 0 to mcg.Count - 1 do begin 
        mc := mcg.Cell(j) ; 
        if mc.addr.mat <> matVirtual then 
          mc.addr := ChangePhysicalAddressBitWidth(mc.addr, newMat) ; 
      end; 
      mcg.mat := newMat ; 
      mcg.calcAddrRange ; 
    end; 
  end{ "procedure TMemoryCellGroups.ChangeAdddressWidth" } ; 



// Sortierfunktion für Listen
function MemoryCellSortCompare(item1, item2: Pointer): integer; 
  var addr1val, addr2val: dword ; 
  begin 
    addr1val := TMemoryCell(item1).addr.val ; 
    addr2val := TMemoryCell(item2).addr.val ; 
    if addr1val < addr2val then 
      result := -1 
    else if addr1val > addr2val then 
      result := 1 
    else result := 0 ; 
  end ; 



end{ "unit MemoryCellU" } . 
