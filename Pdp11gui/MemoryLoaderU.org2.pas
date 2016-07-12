unit MemoryLoaderU;
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

//
// Algorithmen zum Laden UND zum Speichern
// Alle adressen sind physical
interface

uses
  Windows,
  Classes,
  Sysutils,
  StdCtrls,
  AddressU,
  MemoryCellU,
  BlinkenlightInstructionsU ;

type
  TMemoryLoaderFormat = (
      mlfmtByteStreamLH,
      mlfmtLowByteFileHighByteFile,
      mlfmtTextfileOneAddrPerLine,
      mlfmtBlinkenlightInstructions, // only .Save
      mlfmtStandardAbsolutePapertape
    ) ;


// Beschreibung der verschiedenen Inputfiles
  TMemoryloaderFile = class(TCollectionItem)
    public
      regkey: string ; // suffix für Registry
      prompt: string ; // für caption
      loaderfmtidx: integer ; // für multi-file formats: 0=low byte, 1 = high byte
      filename: string ; // letzter bekannter Filename, registry
      control_filenameprompt : TLabel ;
      control_filenameedit : TEdit ;
      control_filenamebrowse : TButton ;
    end ;

  // allgemeiner loader
  TMemoryLoader = class(TObject)
    public
      loaderfmt: TMemoryLoaderFormat ; // Typ nochmal als enum
      isText: boolean ; // true, wenn dump nach Textformat
      hasEntryAddress: boolean ; // true, wenn Format eine Einsprungaddresse definiert
      EntryAddress: TMemoryAddress ; // wert, wenn ja
      loadername: string ;
      StartAddrDefined: boolean ; // true, wenn das Fileformat die Adressen fix definiert
      Files: TCollection ; // of TMemoryloaderFile
      function getFile(idx: integer): TMemoryloaderFile ;
      procedure Load(memorycellgroup: TMemoryCellgroup; startaddr:dword) ; virtual ; abstract ;
      procedure Save(memorycellgroup: TMemoryCellgroup) ; virtual ; abstract ;
//    function getWordCount: integer ; abstract ; virtual ;
// more to come
      constructor Create; virtual;
      destructor Destroy ; override ;
    end{ "TYPE TMemoryLoader = class(TObject)" } ;

  // liest einen bytstrom, nach wordstrom
  TMemoryLoader_BytestreamLH = class(TMemoryLoader)
    public
      constructor Create; override;
      destructor Destroy ; override ;
      procedure Load(memorycellgroup: TMemoryCellgroup ; startaddr: dword) ; override ;
      procedure Save(memorycellgroup: TMemoryCellgroup) ;  override ;
    end;

// liest l-bytes und h-Bytes aus zwei files
  TMemoryLoader_LowByteFileHighByteFile = class(TMemoryLoader)
    public
      constructor Create ; virtual;
      destructor Destroy ;
      procedure Load(memorycellgroup: TMemoryCellgroup ; startaddr: dword) ; override ;
      procedure Save(memorycellgroup: TMemoryCellgroup) ;  override ;
    end;

  // list einen Textfile mit einer "address value [value ...]" Liste pro Textzeile
  // Streuzeichen werden ausgefiltert.
  // Data line must begin with octal number (=address)
  TMemoryLoader_TextfileOneAddrPerLine = class(TMemoryLoader)
    public
      constructor Create ; virtual;
      destructor Destroy ;
      procedure Load(memorycellgroup: TMemoryCellgroup ; startaddr: dword) ; override ;
      procedure Save(memorycellgroup: TMemoryCellgroup) ;  override ;
    end;

  TMemoryLoader_BlinkenlightInstructions = class(TMemoryLoader)
    public
      constructor Create ; virtual;
      destructor Destroy ;
      // procedure Load(memorycellgroup: TMemoryCellgroup ; startaddr: dword) ; override ;
      procedure Save(memorycellgroup: TMemoryCellgroup) ;  override ;
    end;

  TMemoryLoader_StandardAbsolutePapertape = class(TMemoryLoader)
    private
      // buffer to read data into
      buffer_size: integer ;
      buffer_data: array of byte ;
      buffer_valid: array of boolean ;

    public
      diaglog: TstringList ;
      constructor Create ; virtual;
      destructor Destroy ;
      procedure Load(memorycellgroup: TMemoryCellgroup ; startaddr: dword) ; override ;
      procedure Save(memorycellgroup: TMemoryCellgroup) ;  override ;
    end{ "TYPE TMemoryLoader_StandardAbsolutePapertape = class(TMemoryLoader)" } ;



implementation

uses
  AuxU,
  OpHHMIsc,
  RegistryU,
  FormMainU,
  FormAboutU ;


constructor TMemoryLoader.Create ;
  begin
    inherited Create ;
    Files := TCollection.Create(TMemoryloaderFile) ;
    hasEntryAddress := false ;
  end;

destructor TMemoryLoader.Destroy ;
  begin
    Files.Free ;
    inherited ;
  end;

function TMemoryLoader.getFile(idx: integer): TMemoryloaderFile ;
  begin
    assert(Files <> nil) ;
    assert(idx < Files.Count) ;
    result := Files.items[idx] as TMemoryloaderFile ;
  end;


constructor TMemoryLoader_BytestreamLH.Create ;
  begin
    inherited Create ;
    loaderfmt:= mlfmtByteStreamLH ;
    loadername := 'binary byte stream' ;
    isText := false ;

    StartAddrDefined := false ;
// hat nur 1 File
    with Files.Add as TMemoryloaderFile do begin
      regkey := 'MemoryLoader_FilenameByteStream' ;
      prompt := 'Byte stream file';
      loaderfmtidx := 0 ;
      control_filenameprompt := nil ; // wird von Form angeschlossen
      control_filenameedit := nil ;
      control_filenamebrowse := nil ;
      filename := TheRegistry.Load(regkey, '') ;
    end;

  end { "constructor TMemoryLoader_BytestreamLH.Create" } ;

destructor TMemoryLoader_BytestreamLH.Destroy ;
  begin
    inherited ;
  end;


// alle bytes der binärdate in die memorycellgroup.
procedure TMemoryLoader_BytestreamLH.Load(memorycellgroup: TMemoryCellgroup ; startaddr: dword) ;
  var
    stream: TFileStream ;
    fname: string ;
    wordcount: integer ;
    i: integer ;
    byte_l, byte_h: byte ;
    w: word ;
  begin
    assert(memorycellgroup.mat > matAnyPhysical) ;
    fname := getFile(0).filename ;
    Log('TMemoryLoader_BytestreamLH.Load(): reading file %s', [fname]);
    stream := TFileStream.Create(fname, fmOpenRead);
    try
      wordcount := stream.Size div 2 ;

      memorycellgroup.Clear ;
      memorycellgroup.Add(startaddr, wordcount) ;

      // byte-weise lesen ... wie cool
      for i := 0 to wordcount - 1 do begin
        stream.Read(byte_l, 1) ; stream.Read(byte_h, 1) ;
        w := word(byte_h) shl 8 + byte_l ;
        memorycellgroup.Cell(i).edit_value := w ;
      end;

    finally
      stream.Free ;
    end{ "try" } ;
  end{ "procedure TMemoryLoader_BytestreamLH.Load" } ;

// alle bytes der binärdate in die memorycellgroup.
procedure TMemoryLoader_BytestreamLH.Save(memorycellgroup: TMemoryCellgroup) ;
  var
    stream: TFileStream ;
    fname: string ;
    wordcount: integer ;
    i: integer ;
    byte_l, byte_h: byte ;
    w: word ;
  begin
    assert(memorycellgroup.mat > matAnyPhysical) ;
    fname := getFile(0).filename ;
    Log('TMemoryLoader_BytestreamLH.Save(): writing file %s', [fname]);
    stream := TFileStream.Create(fname, fmCreate);
    try
      wordcount := memorycellgroup.Count;

      // byte-weise schreiben ...
      for i := 0 to wordcount - 1 do begin
        w := memorycellgroup.Cell(i).edit_value ;
        byte_l := w and $ff ;
        byte_h := w shr 8 ;
        stream.Write(byte_l, 1) ; stream.Write(byte_h, 1) ;
      end;

    finally
      stream.Free ;
    end{ "try" } ;
  end{ "procedure TMemoryLoader_BytestreamLH.Save" } ;


constructor TMemoryLoader_LowByteFileHighByteFile.Create ;
  begin
    inherited Create ;
    loaderfmt := mlfmtLowByteFileHighByteFile ;
    loadername := 'separate low byte/high byte binary files' ;
    isText := false ;

    StartAddrDefined := false ;
    // hat zwei Files
    with Files.Add as TMemoryloaderFile do begin
      regkey := 'MemoryLoader_FilenameLowBytes' ;
      prompt := 'LOW bytes file';
      loaderfmtidx := 0 ;
      control_filenameprompt := nil ; // wird von Form angeschlossen
      control_filenameedit := nil ;
      control_filenamebrowse := nil ;
      filename := TheRegistry.Load(regkey, '') ;
    end;
    with Files.Add as TMemoryloaderFile do begin
      regkey := 'MemoryLoader_FilenameHighBytes' ;
      prompt := 'HIGH bytes file';
      loaderfmtidx := 1 ;
      control_filenameprompt := nil ; // wird von Form angeschlossen
      control_filenameedit := nil ;
      control_filenamebrowse := nil ;
      filename := TheRegistry.Load(regkey, '') ;
    end;
  end { "constructor TMemoryLoader_LowByteFileHighByteFile.Create" } ;

destructor TMemoryLoader_LowByteFileHighByteFile.Destroy ;
  begin
    inherited ;
  end;


procedure TMemoryLoader_LowByteFileHighByteFile.Load(memorycellgroup: TMemoryCellgroup ; startaddr: dword) ;
  var
    stream_l, stream_h: TFileStream ;
    fname: string ;
    wordcount: integer ;
    i: integer ;
    byte_l, byte_h: byte ;
    w: word ;
  begin
    assert(memorycellgroup.mat > matAnyPhysical) ;

    fname := getFile(0).filename ;
    Log('TMemoryLoader_LowByteFileHighByteFile.Load(): reading file %s', [fname]);
    stream_l := TFileStream.Create(fname, fmOpenRead);
    try
      fname := getFile(1).filename ;
      stream_h := TFileStream.Create(fname, fmOpenRead);
      try
        wordcount := stream_l.Size div 2 ;

        // MemoryCellgroup für Bytes aufbauen
        memorycellgroup.Clear ;
        memorycellgroup.Add(startaddr, wordcount) ;
        assert(wordcount = memorycellgroup.Count) ;

        // byte-weise lesen ... wie cool
        for i := 0 to wordcount - 1 do begin
          stream_l.Read(byte_l, 1) ; stream_h.Read(byte_h, 1) ;
          w := word(byte_h) shl 8 + byte_l ;
          memorycellgroup.Cell(i).edit_value := w ;
        end;
      finally
        stream_h.Free ;
      end { "try" } ;
    finally
      stream_l.Free ;
    end{ "try" } ;
  end{ "procedure TMemoryLoader_LowByteFileHighByteFile.Load" } ;

procedure TMemoryLoader_LowByteFileHighByteFile.Save(memorycellgroup: TMemoryCellgroup) ;
  var
    stream_l, stream_h: TFileStream ;
    fname: string ;
    wordcount: integer ;
    i: integer ;
    byte_l, byte_h: byte ;
    w: word ;
  begin
    assert(memorycellgroup.mat > matAnyPhysical) ;

    fname := getFile(0).filename ;
    Log('TMemoryLoader_LowByteFileHighByteFile.Save(): writing file %s', [fname]);
    stream_l := TFileStream.Create(fname, fmCreate);
    try
      fname := getFile(1).filename ;
      stream_h := TFileStream.Create(fname, fmCreate);
      try
        if stream_l.Size <> stream_h.Size then
          raise Exception.CreateFmt('Both files must have same size!', [0]) ;

        wordcount := memorycellgroup.Count;

        // byte-weise schreiben ... wie cool
        for i := 0 to wordcount - 1 do begin
          w := memorycellgroup.Cell(i).edit_value ;
          byte_l := w and $ff ;
          byte_h := w shl 8 ;
          stream_l.Write(byte_l, 1) ; stream_h.Write(byte_h, 1) ;
        end;
      finally
        stream_h.Free ;
      end { "try" } ;
    finally
      stream_l.Free ;
    end{ "try" } ;
  end{ "procedure TMemoryLoader_LowByteFileHighByteFile.Save" } ;


constructor TMemoryLoader_TextfileOneAddrPerLine.Create ;
  begin
    inherited Create ;
    loaderfmt := mlfmtTextfileOneAddrPerLine ;
    loadername := 'text file with one octal addr,value[,value...] list per line' ;
    isText := true ;

    StartAddrDefined := true ;
    // hat einen Files
    with Files.Add as TMemoryloaderFile do begin
      regkey := 'MemoryLoader_FilenameTextfile' ;
      prompt := 'Text file';
      loaderfmtidx := 0 ;
      control_filenameprompt := nil ; // wird von Form angeschlossen
      control_filenameedit := nil ;
      control_filenamebrowse := nil ;
      filename := TheRegistry.Load(regkey, '') ;
    end;
  end { "constructor TMemoryLoader_TextfileOneAddrPerLine.Create" } ;

destructor TMemoryLoader_TextfileOneAddrPerLine.Destroy ;
  begin
    inherited ;
  end;


procedure TMemoryLoader_TextfileOneAddrPerLine.Load(memorycellgroup: TMemoryCellgroup ; startaddr: dword) ;
  var
    f: System.Text ;
    fname: string ;
    line: string ;
    s: string ;
    i, n: integer ;
    addr: dword ;
    value: dword ;
  begin
    assert(memorycellgroup.mat > matAnyPhysical) ;
    memorycellgroup.Clear ;

    fname := getFile(0).filename ;

    Log('TMemoryLoader_TextfileOneAddrPerLine.Load(): reading file %s', [fname]) ;
    AssignFile(f, fname) ;

    try
      reset(f) ;

      while not Eof(f) do begin
        readln(f, line) ;
        // Verify: Line must begin with 0..7
        line := trim(line) ;
        if (line = '') or not isOctalDigit(line[1]) then
          Continue ; // ignore line

        // line begins with octal digit: clear out non-digit chars
        for i := 1 to length(line) do
          if not isOctalDigit(line[i]) then
            line[i] := ' ' ;
        n := wordcount(line, [' ']) ; // Anzahl der octal values
        if n >= 2 then begin
          // word 1 = addresse
          s := ExtractWord(1, line, [' ']) ;
          addr := OctalStr2Dword(s, 0) ;
          // word 2.. n = Werte für diese und die folgenden Adressen
          for i := 2 to n do begin
            s := ExtractWord(i, line, [' ']) ;
            value := OctalStr2Dword(s, 0) ;

            with memorycellgroup.Add(addr) do
              edit_value := value ;
            addr := addr+ 2 ;
          end;
        end{ "if n >= 2" } ;
      end { "while not Eof(f)" } ;

    finally
      CloseFile(f) ;
      memorycellgroup.calcAddrRange ;
      memorycellgroup.Invalidate ;

      // Ausgabe: "addr: value"
    end{ "try" } ;
  end{ "procedure TMemoryLoader_TextfileOneAddrPerLine.Load" } ;


procedure TMemoryLoader_TextfileOneAddrPerLine.Save(memorycellgroup: TMemoryCellgroup) ;
  const
    max_vals_per_line: integer = 8 ;

  var
    line: string ;
    f: System.Text ;

  procedure puts(s:string) ;
    begin
      line := line + s ;
    end ;

  procedure putln ;
    begin
// produce no empty lines
      if line <> '' then
        writeln(f, line) ;
      line := '' ;
    end ;

  var
    fname: string ;
    wordcount: integer ;
    i: integer ;
    lastaddr, curaddr: dword ;
    val: dword ;
  begin { "procedure TMemoryLoader_TextfileOneAddrPerLine.Save" }
    assert(memorycellgroup.mat > matAnyPhysical) ;

    fname := getFile(0).filename ;
    Log('TMemoryLoader_TextfileOneAddrPerLine.Save(): writing file %s', [fname]) ;

    AssignFile(f, fname) ;

    try
      Rewrite(f) ;
      wordcount := memorycellgroup.Count;

      // Ausgabe: "addr: value0 value1 .. value7"
      // new line if addr mod 8 = 0
      line := '' ;
      lastaddr :=  MEMORYCELL_ILLEGALVAL ;
      for i := 0 to wordcount - 1 do begin
        curaddr := memorycellgroup.Cell(i).addr.val ;
        val :=  memorycellgroup.Cell(i).edit_value ;
        if val <> MEMORYCELL_ILLEGALVAL then begin
          // dump only valid addresses
          if (line = '')  // new line at file start
                  or (curaddr <> (lastaddr+2) ) // newline on addr gap
                  or ((curaddr mod (max_vals_per_line *2)) = 0) // newline if line full
            then begin
              // start new line with address at begin
              putln ;
              puts(Format('%s:', [Dword2OctalStr(curaddr, 16)])) ;
            end ;
          puts(Format(' %s', [Dword2OctalStr(val, 16)])) ;
          lastaddr := curaddr ;
        end{ "if val <> MEMORYCELL_ILLEGALVAL" } ;
      end { "for i" } ;
      putln ; // flush last line

    finally
      CloseFile(f) ;
    end{ "try" } ;
  end{ "procedure TMemoryLoader_TextfileOneAddrPerLine.Save" } ;



constructor TMemoryLoader_BlinkenlightInstructions.Create ;
  begin
    inherited Create ;
    loaderfmt := mlfmtBlinkenlightInstructions ;
    loadername := 'Instructions, how to operate a DEC blinken light console' ;
    isText := true ;
    StartAddrDefined := true ;
    // hat einen Files
    with Files.Add as TMemoryloaderFile do begin
      regkey := 'MemoryLoader_FilenameTextfile' ;
      prompt := 'Text file';
      loaderfmtidx := 0 ;
      control_filenameprompt := nil ; // wird von Form angeschlossen
      control_filenameedit := nil ;
      control_filenamebrowse := nil ;
      filename := TheRegistry.Load(regkey, '') ;
    end;
  end{ "constructor TMemoryLoader_BlinkenlightInstructions.Create" } ;

destructor TMemoryLoader_BlinkenlightInstructions.Destroy ;
  begin
    inherited ;
  end;

procedure TMemoryLoader_BlinkenlightInstructions.Save(memorycellgroup: TMemoryCellgroup) ;
  var
    bi: TBlinkenLightInstructions ;
    fname: string ;
  begin
    bi := TBlinkenLightInstructions.Create ;
    try
      fname := getFile(0).filename ;
      Log('TMemoryLoader_BlinkenlightInstructions.Save(): writing file %s', [fname]) ;
      bi.Generate(memorycellgroup) ;
      bi.OutputLines.SaveToFile(fname) ;
    finally
      bi.Free ;
    end;
  end{ "procedure TMemoryLoader_BlinkenlightInstructions.Save" } ;

{
   MAINDEC paper tape image =
   "DEC Standard Absolute Papertape Format"
   See email from Mattis Lind, Sept 2013. mattis@mattisborgen.se
   see "maindec.c"
}
constructor TMemoryLoader_StandardAbsolutePapertape.Create;
  begin
    inherited Create;
    diaglog := TstringList.Create ;
    loaderfmt := mlfmtStandardAbsolutePapertape ;
    loadername := 'DEC Standard Absolute Paper Tape Format' ;
    isText := false ;
    StartAddrDefined := true ;
    // hat einen File
    with Files.Add as TMemoryloaderFile do begin
      regkey := 'MemoryLoader_FilenameStandardAbsolutePapertape' ;
      prompt := 'Image file';
      loaderfmtidx := 0 ;
      control_filenameprompt := nil ; // wird von Form angeschlossen
      control_filenameedit := nil ;
      control_filenamebrowse := nil ;
      filename := TheRegistry.Load(regkey, '') ;
    end;
    hasEntryAddress := true ;
    EntryAddress.mat := matVirtual;
    EntryAddress.val := 0 ;

    buffer_size := $10000 ; // can read 64K bytes 0..0xffff
    SetLength(buffer_data, buffer_size) ;
    SetLength(buffer_valid, buffer_size) ;
  end { "constructor TMemoryLoader_StandardAbsolutePapertape.Create" } ;

destructor TMemoryLoader_StandardAbsolutePapertape.Destroy ;
  begin
    diaglog.Free ;
    inherited ;
  end ;

procedure TMemoryLoader_StandardAbsolutePapertape.Load(memorycellgroup: TMemoryCellgroup ; startaddr: dword) ;

  procedure diaglog_put(field:string ; value: uint32 ; bytecount: integer);
    var s: string ;
    begin
      s :=  '<' + field + '=' ;
      case bytecount of
        1: s := s + Format('x%0.2x=o%s', [value, Dword2OctalStr(value, 8)]) ;
        2: s := s + Format('x%0.4x=o%s', [value, Dword2OctalStr(value, 16)]) ;
        4: s := s + Format('x%0.8x=o%s', [value, Dword2OctalStr(value, 32)]) ;
        else s := s + '???' ;
      end;
      s := s + '>';// newline
      diaglog.Add(s) ;
    end;

  procedure diaglog_putdata(addr: uint32 ; data: byte) ;
    var s: string ;
    begin
      s := Format('<addr=x%0.2x=o%s, data=x%0.2x=o%s>',
              [addr, Dword2OctalStr(addr, 0),
              data, Dword2OctalStr(data, 8)
              ]) ;
      diaglog.Add(s) ;
    end;

// read in a byte buffer first, then convert to memorycellgroups
// (memory content in words).
// Missing bytes are assumed to be "0".

  procedure BufferClear;
    var i: integer ;
    begin
      for i := 0 to buffer_size-1 do begin
        buffer_data[i] := 0 ;
        buffer_valid[i] := false ;
      end;
    end;

  // read paper tape records, and fill buffer.
  // result: Entry Adress filled

  //   see "maindec.c"
  //    format is : <block> <block> <block> ...
  //  block := <stuff bytes> <0x01> <0x00> <blocksize_low> <blocksize_high>
  //          <startaddr_low> <startaddr_high>
  //        ( <blocksize> - 6) / 2 x <data_low> <data_high>
  //         <chksum>
  //   <blocksize> includes everything from <0x01> to last data byte
  //   everything from 0x01 to <cksum> goes into checksum, result must be 0
  //
  // After C-Code from Mattis Lind, www.datormuseum.se. mattis@mattisborgen.se


  procedure BufferLoad(stream: TFileStream) ;
    var
      stream_byte_idx: integer ; // index of current byte in stream
      state: integer ;
      b: byte ;
      sum: integer ;
      block_byte_idx: integer ; // idx of byte inside of block. start with 1
      block_byte_size: integer ; // size of a block in bytes
      block_dataword_count: integer ;
      //block_dataword_idx : integer ; // # of data word in block
      address: dword ;
    begin
      state := 0 ;
      EntryAddress.val := 1 ; // invalid
      stream_byte_idx:= 0 ;
      block_byte_idx := 0 ;
      sum := 0 ;
      try
        //block_dataword_idx := 0 ;
        while stream_byte_idx < stream.Size do begin
          stream.Read(b, 1) ;
//        Log('[0x%0.4x] state=%d ch=0x%0.2x sum=0x%0.2x block_byte_idx=%d',
//                [stream_byte_idx, state, b, sum, block_byte_idx]);
          inc(stream_byte_idx) ;
          case (state) of
            0: begin // skip bytes until 0x01 is found
              sum := 0 ;
              if b = 1 then begin
                state := 1; // valid header: initialize counter and checksum
                block_byte_idx := 1;
                sum := sum + b;
              end ;
            end ;
            1: // 0x01 is found, check for following 0x00
              if b <> 0 then
                state := 0 // invalid start, skip bytes
              else begin
                state := 2;
                diaglog_put('header', 1, 2) ;
                inc(block_byte_idx) ;
                sum := sum + b ;
              end ;
            2: begin  // read low count byte
              block_byte_size := b;
              state := 3;
              sum := sum + b ;
              inc(block_byte_idx);
            end ;
            3: begin // read high count byte
              block_byte_size := block_byte_size or (b shl 8);
              diaglog_put('blocksize_byte', block_byte_size, 2) ;
              // calculate data word count
              block_dataword_count := (block_byte_size - 6) div 2 ;
              state := 4;
              sum := sum + b ;
              inc(block_byte_idx);
            end ;
            4: begin // read address low
              address := b;
              sum := sum + b ;
              state := 5;
              inc(block_byte_idx);
            end ;
            5: begin // read address high
              address := address or (b shl 8);
              diaglog_put('address', address, 2) ;
              state := 6;
              sum := sum + b ;
              inc(block_byte_idx);
              if (block_byte_idx > block_byte_size) then begin
                Log('Skipping mis-sized block with addr = %s, size=%d', [
                        Dword2OctalStr(address, 26), block_byte_size]);
                state := 0 ;
              end else begin
                // block range now known
                // if block size = 0: entry address
                if block_dataword_count = 0 then begin
                  EntryAddress.val := address ;
                  Log('Empty block with addr = %s, size=%d', [
                          Dword2OctalStr(address, 16), block_byte_size]);
                  state := 0 ; // no chksum ?
                end else begin
                  // MemoryCellgroup für Bytes aufbauen
                  Log('Starting data block with addr = %s, block_byte_size=%d, datawordcount=%d', [
                          Dword2OctalStr(address,16), block_byte_size, block_dataword_count]);
                  //assert(block_dataword_count = memorycellgroup.Count) ;
                  //block_dataword_idx := 0 ;
                end;
              end{ "if (block_byte_idx > block_byte_size) ... ELSE" } ;
            end { "case (state) of 5:" } ;
            6: begin // read data byte
              if address >= buffer_size then
                raise Exception.CreateFmt('Address overflow: Max = 0x%x = 0%s',
                        [buffer_size, Dword2OctalStr(buffer_size, 0)]) ;

              diaglog_putdata(address,b) ;
              sum := sum + b ;
              buffer_data[address] := b ;
              buffer_valid[address] := true ;
              address := address + 1 ;
              inc(block_byte_idx);

              if block_byte_idx >= block_byte_size then
                state := 7 ;// block end
            end { "case (state) of 6:" } ;
            7: begin // all words of block read, eval checksum
              sum := sum + b ;
              sum := sum and $ff;
              if sum <> 0 then begin
                diaglog_put('chksum error', b, 1) ;
                raise Exception.CreateFmt('Check sum error. byte_idx=%d, sum=0x%x',
                        [stream_byte_idx, sum])
              end else
                diaglog_put('chksum ok', b, 1) ;
              Log('Block end with correct checksum');
              sum := 0;
              state := 0;
            end{ "case (state) of 7:" } ;
          end{ "case (state)" } ; // case
        end{ "while stream_byte_idx < stream.Size" } ;
      finally
        // diaglog.SaveToFile('e:\temp\diaglog.txt') ;
      end{ "try" } ;
    end{ "procedure BufferLoad" } ;

  // create memory cellgroups for the (fragmeneted) data in the buffer
  procedure Buffer2MemoryCellgroup ;
    var
      address: TMemoryAddress ;
      i: integer ;
      w: word ;
    begin
      // buffer contains pattern of bytes
      // scan as pattern of words!
      address.mat := matVirtual ;
      address.val := 0 ;
      while address.val < buffer_size do begin
        // at least one byte set in the word at "address?"
        if buffer_valid[address.val] or buffer_valid[address.val+1] then begin
          // build word from bytes. low byte first!
          // invalid bytes, which are not read from file, are initialized to '0'
          w := buffer_data[address.val] + word(buffer_data[address.val+1]) shl 8 ;
          // save word. allocate memory cell, if not written before
          i := memorycellgroup.CellIndexByAddr(address) ;
          if i < 0 then begin
            memorycellgroup.Add(address.val) ;
            i := memorycellgroup.CellIndexByAddr(address) ;
          end;
          assert(i >= 0) ;
          memorycellgroup.Cell(i).edit_value := w ;
        end{ "if buffer_valid[address.val] or buffer_valid[address.val+1]" } ;
        address.val := address.val + 2 ;
      end{ "while address.val < buffer_size" } ;
    end { "procedure Buffer2MemoryCellgroup" } ;


  var
    fname: string ;
    stream: TFileStream ;
  begin { "procedure TMemoryLoader_StandardAbsolutePapertape.Load" }
    diaglog.Clear ;
    assert(memorycellgroup.mat > matAnyPhysical) ;
    memorycellgroup.Clear ;

    EntryAddress.mat := matVirtual;
    EntryAddress.val := 0 ;

    fname := getFile(0).filename ;
    Log('TMemoryLoader_StandardAbsolutePapertape.Load(): reading file %s', [fname]);

    stream := TFileStream.Create(fname, fmOpenRead);
    try
      BufferClear ;
      BufferLoad(stream) ;
      Buffer2MemoryCellgroup ;
    finally
      stream.Free ;
    end;
  end { "procedure TMemoryLoader_StandardAbsolutePapertape.Load" } ;


procedure TMemoryLoader_StandardAbsolutePapertape.Save(memorycellgroup: TMemoryCellgroup) ; begin
end ;

end{ "unit MemoryLoaderU" } .
