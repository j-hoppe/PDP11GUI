unit MediaImageBufferU;

interface 

uses 
  Classes, 
  SysUtils, 
  JH_Utilities, 
  Dialogs, 
  MediaImageDevicesU ; 


type 
  TMediaImage_Buffer = class(TObject) 
    public 
      // which device ?
      Device: TMediaImage_AbstractDevice ; 
//    Controller: TMediaImage_AbstractController ;
      //DiscDriverType: TDiscDriverType ;
      //DiscDriverSubType: integer ; // Untertyp, wird direkt an driver übergeben
      ImageBuffer: array of byte ; // der Image buffer

//      defaultControllerBaseAddress: word ; // virtual 16 bit
//      controllerBaseAddress: word ; // virtual 16 bit

      constructor Create ; 

      procedure LinkToDevice(aDevice: TMediaImage_AbstractDevice) ; 

      function getBlockCount: integer ; 

      procedure setByte(i: integer; val : byte) ; 

      // Imagebuffer über block und byte in block zugreifen
      procedure setImageBufferBlockByte(BlockNr, bytenr:integer ; val: byte) ; 
      function getImageBufferBlockByte(BlockNr, bytenr:integer): byte ; 
      // Imagebuffer über block und word in block zugreifen
      procedure setImageBufferBlockWord(BlockNr, wordnr:integer ; val: word) ; 
      function getImageBufferBlockWord(BlockNr, wordnr:integer): word ; 

      procedure ClearBuffer ; 
      procedure SaveBuffer(filename: string) ; 
      procedure LoadBuffer(filename: string; overlay: boolean = false) ; 

      function IsBitmap(filename: string): boolean ; 
      procedure Bitmap2Bytestream(bitmapfilename, bytestreamfilename: string) ; 
      procedure Bytestream2Bitmap(bytestreamfilename, bitmapfilename: string) ; 
    end { "TYPE TMediaImage_Buffer = class(TObject)" } ; 


implementation 

uses 
  FormMainU, 
  Graphics ; // Bitmap


constructor TMediaImage_Buffer.Create; 
  begin 
    inherited ; 
    Device := nil ; 
  end; 

procedure TMediaImage_Buffer.LinkToDevice(aDevice: TMediaImage_AbstractDevice) ; 
  begin 
    Device := aDevice ; 
    // Disc grösse ist definitiv bekannt
    // Buffer áuf die richtige Grösse einstellen
    // blockcount < 0: dynamische Länge (papertape).
    SetLength(ImageBuffer, Device.BlockCount * Device.BlockSize) ; 
  end; 

// query fill
function TMediaImage_Buffer.getBlockCount: integer ; 
  begin 
    result := Length(ImageBuffer) div Device.BlockSize ; 
  end; 


// set a byte, with autoextend
procedure TMediaImage_Buffer.setByte(i: integer; val : byte) ; 
  begin 
    if i >= Length(ImageBuffer) then 
      if not Device.fixCapacity then 
        SetLength(ImageBuffer, i+1) // buffer vergrössern
      else raise Exception.CreateFmt('Imagebuffer overflow: size = %d, trying to write %d', 
                [Length(ImageBuffer), i]) ; 
    ImageBuffer[i] := val ; 
  end; 

procedure TMediaImage_Buffer.setImageBufferBlockByte(BlockNr, bytenr:integer ; val: byte) ; 
  var i: integer ; 
  begin 
    // i auf byte basis
    i := Device.curBlockNr * Device.BlockSize  + bytenr ; 
    setByte(i, val) ; 
  end ; 

function TMediaImage_Buffer.getImageBufferBlockByte(BlockNr, bytenr:integer): byte ; 
  var i: integer ; 
  begin 
    i := BlockNr * Device.BlockSize + bytenr ; 
    result := ImageBuffer[i] ; 
  end; 

// Imagebuffer auf Wordbasis zugreifen
procedure TMediaImage_Buffer.setImageBufferBlockWord(BlockNr, wordnr:integer ; val: word) ; 
  var i: integer ; 
  begin 
    // i auf byte basis
    i := Device.curBlockNr * Device.BlockSize + wordnr * 2 ; 
    setByte(i, val and $ff) ;  // low byte first
    setByte(i + 1, (val shr 8)and $ff) ;  // hi byte last
  end ; 


function TMediaImage_Buffer.getImageBufferBlockWord(BlockNr, wordnr: integer): word ; 
  var i: integer ; 
  begin 
    // i auf byte basis
    i := BlockNr * Device.BlockSize + wordnr * 2 ; 
    result := ImageBuffer[i] 
            +  word(ImageBuffer[i+1]) shl 8 ; // hi byte last
  end; 

procedure TMediaImage_Buffer.ClearBuffer ; 
  var i: integer ; 
  begin 
    for i := 0 to Length(ImageBuffer) - 1 do 
      ImageBuffer[i] := 0 ; 
  end; 


// Is the file a valid Bitmap picture?
function TMediaImage_Buffer.IsBitmap(filename: string): boolean ;
  var ext: string ;
    bmp: TBitmap ;
  begin
    result := false ;
    ext := AnsiUppercase(ExtractFileExt(filename)) ;
    if ext <> '.BMP' then
      Exit ;
    try
      try
        bmp := TBitmap.Create ;
        bmp.LoadFromFile(filename);
        result := true ; // can read: OK
      except on e: Exception do ; // all Exceptions silent
      end ;
    finally
      if bmp <> nil then bmp.Free ;
    end;
  end { "function TMediaImage_Buffer.IsBitmap" } ;

// copy content of bitmap picture file toi stream of words.
// Only used for papertape, so
// Bitmap must be 8 rows heigth, length arbitrary
// Top line = bit7, line 7 = bit 0
procedure TMediaImage_Buffer.Bitmap2Bytestream(bitmapfilename, bytestreamfilename: string) ; 
  type 
    TByteArray = array[word] of byte ; 
    pByteArray = ^TByteArray ; 
  var 
    bmp: TBitmap ; 
    b: byte ; 
    i, n: integer ; 
    x, y: integer ; 
    fs: TFileStream ; 
    p: pByteArray ; // pointer auf 1 byte
    bytes: array of byte ; 
  begin 
    bmp := nil ; 
    try 
      try 
        bmp := TBitmap.Create ; 
        // 1. read bitmap
        bmp.LoadFromFile(bitmapfilename); 
        bmp.pixelformat := pf8bit ; // 1 byte gray per pixel

        if bmp.Height <> 8 then 
          raise Exception.CreateFmt('Bitmap must be 8 pixels heigth, %d!', [bmp.Height]); 
        // setup byte buffer
        n := bmp.Width ; // Anzahl der Bytes
        SetLength(bytes, n) ; //
        for i := 0 to n-1 do 
          bytes[i] := 0 ; 

        // 2. Write bytes
        fs := TFileStream.Create(bytestreamfilename, fmCreate)  ; 
        // die obersten 8 rows in je ein Byte wandeln
        // 1. byte Reihenfolge: links nach rechts
        for y := 0 to bmp.Height-1 do begin 
          p := bmp.ScanLine[y]; 
          for x := 0 to n-1 do  begin// one row of each byte
            b := p[x]; // read pixel
            // top row => y = 0 => MSB im byte
            if b < 127 then // dark pixel: set "1" bit in output byte
               bytes[x] := bytes[x] or ($80 shr y) ;
          end ; 
        end; 
        // write the whole byte array
        for i := 0 to n-1 do 
          fs.Write(bytes[i], 1) ; 
        fs.Free ; fs := nil ; 
      except on e: Exception do ; // all Exceptions silent
      end { "try" } ; 
    finally 
      if bmp <> nil then bmp.Free ; 
      if fs <> nil then fs.Free ; 
    end{ "try" } ; 
  end { "procedure TMediaImage_Buffer.Bitmap2Bytestream" } ; 


procedure TMediaImage_Buffer.Bytestream2Bitmap(bytestreamfilename, bitmapfilename: string) ;
  begin
  end ;


// das Image aus dem device in Datei schreiben
// gleichzeitig BadBlockList als <filename>.meta schreiben

// for some devices (papertape!), only the lower half of each word is valid (a byte)
procedure TMediaImage_Buffer.SaveBuffer(filename: string) ;
  const BlockSize = 10000 ;
  var
    f: TFileStream ;
    tmpfilename:string ;
    i, n, m : integer ;
    byteswritten: integer ;
  begin
    Log('TDiscImage.SaveBuffer(): writing file %s', [filename]);

    // if bitmap picture: convert picture content to word stream file
    tmpfilename := '' ;
    if IsBitmap(filename) then begin
      tmpfilename := TempFilename ;
      f := TFileStream.Create(tmpfilename, fmOpenRead)
    end else
      f := TFileStream.Create(filename, fmCreate) ;

    try
      n := Length(ImageBuffer) ;
      i := 0 ;
      while i < n  do begin 
        // schreibe einen Block ab [i]
        if (n - i) > BlockSize then 
          m := BlockSize 
        else m := n-i ; 
        byteswritten := f.Write( ImageBuffer[i], m) ; 
        assert(byteswritten = m) ; 
        i := i + byteswritten ; // vorwärts schieben
      end ; 
      if tmpfilename <> '' then begin 
        DeleteFile(tmpfilename) ; 
      end ; 
    finally
      f.Free ; 
    end{ "try" } ; 
  end{ "procedure TMediaImage_Buffer.SaveBuffer" } ; 


// das Image aus einer Datei nach "ImageBuffer" lesen
// overlay: nur Blöcke, die im Image nicht 0 sind, aus Datei dazu laden
//  DiscGeometry gültig!
procedure TMediaImage_Buffer.LoadBuffer(filename: string; overlay: boolean = false) ; 

  function isImageBlockEmpty(aBlocknr: integer): boolean ; 
    var start, i: integer ; 
    begin 
      assert(aBlocknr >= 0) ; 
      assert(aBlocknr < Device.BlockCount) ; 
      start := Device.BlockSize * aBlocknr ;
      assert(start + Device.BlockSize <= Length(ImageBuffer)) ; 
      result := true ; 
      for i := 0 to Device.BlockSize-1 do
        if ImageBuffer[start+i] <> 0 then 
          result := false ; // not empty
    end; 

  var 
    f: TFileStream ; 
    tmpfilename: string ; 
    i, n, m: integer ; 
    bytesread: integer ; 
    dummy: array[0..10000] of byte ; 
  begin { "procedure TMediaImage_Buffer.LoadBuffer" } 
    Log('TDiscImage.LoadBuffer(): reading file %s', [filename]); 

    // if bitmap picture: convert picture content to word stream file
    tmpfilename := '' ;
    if IsBitmap(filename) then begin
      tmpfilename := TempFilename ;
      Bitmap2Bytestream(filename, tmpfilename) ;
      f := TFileStream.Create(tmpfilename, fmOpenRead) ;
    end else
      f := TFileStream.Create(filename, fmOpenRead) ;

//    f := TFileStream.Create(filename, fmOpenread) ;
    try
      n := Length(ImageBuffer) ;
      if not overlay then begin
        if Device.fixCapacity then begin
          if f.Size > n then
            raise Exception.CreateFmt('Image file "%s" contains %d bytes, but disc has only %d bytes',
                    [filename, f.Size, n]) ;
          if f.Size < n then
            MessageDlg(Format('Image file "%s" contains only %d bytes, but disc has %d bytes',
                    [filename, f.Size, n]),
                    mtInformation, [mbOk], 0) ; 
        end else begin 
          SetLength(ImageBuffer, f.Size) ; // image buffer as large as file
        end ; 
      end{ "if not overlay" } ; 
      // ZUS: file enthält ganze Anzahl von BlockSize
      assert((n mod Device.BlockSize) = 0) ; 
      n := Length(ImageBuffer) div Device.BlockSize ; // n = Blockzahl
      i := 0 ; // blockcount
      m := Device.BlockSize ; 

//wen not fixCapacity: länge von iamgebuffer??ß

      while i < n do begin 
//        if (n - i) > ReadSize then
//          m := ReadSize
//        else m := n-i ;
        if overlay then begin 
          // overlay: keine Prüfung auf Dateiende,
          // keine Prüfung auf vollständigen Block. Lade einfach drüber, was da ist.
          if isImageBlockEmpty(i) then 
            f.read(ImageBuffer[i*m], m) // overlay data
          else 
            f.read(dummy, m) ;  // skip data. ignore bytes read, maybe even 0.
        end else begin 
          bytesread := f.read(ImageBuffer[i*m], m) ; // read next block
          if bytesread = 0 then 
            Break ; // Datei ist vorzeitig zu Ende: verlasse Schleife
          assert(bytesread = m) ; 
        end; 
        inc(i) ; 
      end{ "while i < n" } ; 
      // Bitmap erzeugen
      if tmpfilename <> '' then begin 
        Bytestream2Bitmap(tmpfilename, filename) ; 
        DeleteFile(tmpfilename) ; 
      end ; 
    finally 
      f.Free ; 
    end{ "try" } ; 
  end{ "procedure TMediaImage_Buffer.LoadBuffer" } ; 


end{ "unit MediaImageBufferU" } . 
