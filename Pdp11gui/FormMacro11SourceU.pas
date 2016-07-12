unit FormMacro11SourceU; 

interface 

uses 
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, JvExControls, JvEditorCommon, JvEditor, ExtCtrls, 
  FormChildU, 
  AppControlU, 
  JH_Utilities; 

type 
  TFormMacro11Source = class(TFormChild) 
      PanelT: TPanel; 
      Editor: TJvEditor; 
      LoadButton: TButton; 
      SaveAsButton: TButton; 
      CompileButton: TButton; 
      OpenDialog1: TOpenDialog; 
      SaveDialog1: TSaveDialog; 
      SaveButton: TButton; 
      NewButton: TButton; 
      procedure LoadButtonClick(Sender: TObject); 
      procedure SaveAsButtonClick(Sender: TObject); 
      procedure CompileButtonClick(Sender: TObject); 
      procedure EditorPaintGutter(Sender: TObject; Canvas: TCanvas); 
      procedure SaveButtonClick(Sender: TObject); 
      procedure EditorResize(Sender: TObject); 
      procedure EditorChange(Sender: TObject); 
      procedure FormShow(Sender: TObject); 
      procedure NewButtonClick(Sender: TObject); 
    private 
      { Private-Deklarationen }
      originalFileContent: TStringList ; // disk mirror to calculate "Changed()"
      procedure FormAfterShow(Sender: TObject); 
      procedure FormBeforeHide(Sender: TObject); 
    public 
      { Public-Deklarationen }
      macro11_appcontrol: TAppControl ; 
      SourceFilename: string ; 
      CanTranslate: boolean ; // true, wenn gültiger File geladen
      Translated: boolean ; // true, wenn MACRO-11 Lauf erfolgreich
      constructor Create(aOwner: TComponent) ; override ; 
      destructor Destroy ; override ; 
      procedure UpdateDisplay ; 
      procedure LoadFile(fname:string); 
      procedure SaveFile(fname:string); 
      function Changed: boolean ; // true, if unsaved changes in Editor
      procedure Compile ; 
      procedure setErrorLine(n:integer) ; 
      procedure setExecutionLine(n:integer) ; 

    end{ "TYPE TFormMacro11Source = class(TFormChild)" } ; 

implementation 


uses 
  RegistryU, 
  AuxU, 
  FormMainU // wg. zugriff auf Listing Window
          , FormMacro11ListingU; 

{$R *.dfm}

constructor TFormMacro11Source.Create(aOwner: TComponent) ; 
  begin 
    inherited Create(aOwner) ; 
    originalFileContent := TStringList.Create ; 

    // private events
    // die MDI-Show/Hide logik in TFormChild verursacht Windowsgehler,
    // wenn JVEditor eine lange Source geladen hat.
    OnAfterShow := FormAfterShow ; // lädt den letzten File 
    OnBeforeHide := FormBeforeHide ; 

    CanTranslate := false ; 
    Translated := false ; 

    // Farben für die markierten zeilen
    Editor.LineInformations.DebugPointColor := ColorCodeExecutionPositionBkGnd ; 
    Editor.LineInformations.DebugPointTextColor:= ColorCodeExecutionPositionText ; 
    (*
      property BreakpointColor: TColor read FBreakpointColor write SetBreakpointColor;
      property BreakpointTextColor: TColor read FBreakpointTextColor write SetBreakpointTextColor;
      *)
    Editor.LineInformations.ErrorPointColor:= ColorCodeErrorBkGnd ; 
    Editor.LineInformations.ErrorPointTextColor:= ColorCodeErrorText ; 


    macro11_appcontrol := TAppControl.Create ; 
  end{ "constructor TFormMacro11Source.Create" } ; 


destructor TFormMacro11Source.Destroy ; 
  begin 
    macro11_appcontrol.Free ; 
    originalFileContent.Free ; 
    inherited ; 
  end; 

procedure TFormMacro11Source.UpdateDisplay ; 
  var i: integer ; 
  begin 
    // bei Änderung:
    if Changed then 
      Caption := setFormCaptionInfoField(Caption, ' * ' + SourceFilename) 
    else 
      Caption := setFormCaptionInfoField(Caption, SourceFilename) ; 

    if Trim(SourceFilename) = '' then 
      SaveButton.Enabled := false // kein Name bekannt
    else SaveButton.Enabled := true ; 

    // wenn irgendwas nicht leeres drin steht: Compile-knopf an.
    CanTranslate := false ; 
    for i := 0 to Editor.Lines.Count - 1 do 
      if Trim(Editor.Lines[i]) <> '' then 
        CanTranslate := true ; 

    // noch ein Check: gültige Datei da?
    if not FileExists(SourceFilename) then 
      CanTranslate := false ; 

    CompileButton.Enabled := CanTranslate ; 

    // Der CompileButton ist auch disabled, wenn neue Source eingeben wurde,
    // aber noch kein filenamen bekannt ist!
    // (Da dann nicht fürs compilieren gespeichert werden kann)

  end{ "procedure TFormMacro11Source.UpdateDisplay" } ; 

procedure TFormMacro11Source.EditorChange(Sender: TObject); 
  begin 
    Log('TFormMacro11Source.Changed := true') ; 
    UpdateDisplay ; 
  end; 

procedure TFormMacro11Source.EditorPaintGutter(Sender: TObject; 
        Canvas: TCanvas); 
  var 
    i: integer; 
    Rect: TRect; 
    oldFont: TFont; 
    s: string ; 
  begin 
    oldFont := TFont.Create; 
    try 
      oldFont.Assign(Canvas.Font); 
      Canvas.Font := oldFont ; // GutterFont.Font;
//      Canvas.Font.Size := oldFont.Size-1 ;
      Canvas.Font.Color := TColor($808080) ; // grau
      with Editor do 
        for i := TopRow to TopRow + VisibleRowCount do begin 
          Rect := Bounds(2, (i - TopRow) * CellRect.Height, GutterWidth - 2 - 5, CellRect.Height); 
          s := Format('%3d', [i+1]) ; // die Zeilennummer
          DrawText(Canvas.Handle, PChar(s), -1, Rect, DT_RIGHT or DT_VCENTER or DT_SINGLELINE); 
        end; 
    finally 
      Canvas.Font := oldFont; 
      oldFont.Free; 
    end{ "try" } ; 
  end{ "procedure TFormMacro11Source.EditorPaintGutter" } ; 


procedure TFormMacro11Source.EditorResize(Sender: TObject); 
  begin 
    Invalidate ; // zeichnet sich sonst nicht richtig neu
  end; 

procedure TFormMacro11Source.FormBeforeHide(Sender: TObject); 
  begin 
    // source aus editor löschen
    Editor.Lines.Clear ; 
    originalFileContent.Clear ; // supress "Changed = true"
  end; 

procedure TFormMacro11Source.FormShow(Sender: TObject); 
  begin 
    UpdateDisplay ; 
  end; 

procedure TFormMacro11Source.FormAfterShow(Sender: TObject); 
  begin 
    // source neu in editor laden
    // letzten File automatisch laden
    SourceFilename := TheRegistry.Load('SourceFilename', '') ; 
    if SourceFilename <> '' then 
      LoadFile(SourceFilename); 
  end; 


// Die Zeile mit einem Fehler drin markieren
procedure TFormMacro11Source.setErrorLine(n:integer) ; 
  var i: integer ; 
  begin 
    dec(n) ; // Lines[] ab 0 !
    with Editor do 
      for i := 0 to Lines.Count-1 do 
        if i = n then 
          LineInformations.SelectStyle[i] := lssErrorPoint 
                  // langsam bei grossen Files!!
        else LineInformations.SelectStyle[i] := lssUnselected ; 
    Editor.MakeRowVisible(n);  // scrolle sichtbar
    Editor.SetCaret(1,n); // Zeile anfahren
  end; 

// Die gerade ausgeführte Zeile markieren
procedure TFormMacro11Source.setExecutionLine(n:integer) ; 
  var i: integer ; 
  begin 
    with Editor do 
      for i := 0 to Lines.Count-1 do 
        if i = n then 
          LineInformations.SelectStyle[i] := lssErrorPoint 
        else LineInformations.SelectStyle[i] := lssUnselected ; 
  end; 



// fname = '': new emtpy file
procedure TFormMacro11Source.LoadFile(fname:string); 
  var 
    tmpLines: TStringList ; 
    i: integer ; 
  begin 
    originalFileContent.Clear ; 
    Caption := setFormCaptionInfoField(Caption, '') ; 
    CanTranslate := false ; 

    if (fname <> '') and not FileExists(fname) then 
      Exit ; 

    // 1. clear editor

    setErrorLine(-1); // marker löschen
    setExecutionLine(-1); 
    Editor.Clear ; 

    if fname <> '' then begin 
      // 2. load file
      tmpLines:= TStringList.Create ; 
      try 
        Log('LoadFromFile(%s)', [fname]) ; 
        try 
          tmpLines.LoadFromFile(fname); 
        except on E: Exception do 
            raise Exception.CreateFmt('Error in Macro11Source.Loadfile(): can not read file %s', [fname]); 
        end; 
        // laden mit detab: TJvEditor does not display Tabs?
        for i := 0 to tmpLines.Count - 1 do 
          tmpLines[i] := detab(tmpLines[i], 8) ; 
        Editor.BeginUpdate ; // supress events while loading
        Editor.Lines.Assign(tmpLines) ; 
        originalFileContent.Assign(tmpLines) ; 
        Editor.EndUpdate ; 

        Log('TFormMacro11Source.Loadfile(): Changed := false') ; 

        TheRegistry.Save('SourceFilename', fname); 
      finally 
        tmpLines.Free ; 
      end{ "try" } ; 

    end { "if fname <> ''" } ; 
    // Editor.LineInformations.SelectStyle[6] := lssUnselected ;
    // Editor.LineInformations.SelectStyle[8] := lssBreakPoint ;
    // Editor.LineInformations.SelectStyle[10] := lssDebugPoint ;
    // Editor.LineInformations.SelectStyle[12] := lssErrorPoint ;
    CanTranslate := true ; 
    SourceFilename := fname ; // '', if "New"
    UpdateDisplay ; 
  end{ "procedure TFormMacro11Source.LoadFile" } ; 


procedure TFormMacro11Source.SaveFile(fname:string); 
  var f: System.Text ; 
    i: integer ; 
    s: string ; 
  begin 
    Log('TFormMacro11Source.SaveFile(%s)', [fname]) ; 
    try 
      AssignFile(f, fname) ; 
      try 
        Rewrite(f) ; 
        for i := 0 to Editor.Lines.Count - 1 do begin 
          s := Editor.Lines[i] ; 
          s := entab(s, 8) ; // unnötig, und kaputt?
          writeln(f, s) ; 
        end; 
        originalFileContent.Assign(Editor.Lines) ; 
        Log('TFormMacro11Source.Savefile(): Changed := false') ; 
      finally 
        CloseFile(f) ; 
      end; 
    except on E: Exception do 
        raise Exception.CreateFmt('Error in Macro11Source.SaveFile(): can not save to file %s', [fname]); 
    end{ "try" } ; 
    TheRegistry.Save('SourceFilename', fname); 
    Caption := setFormCaptionInfoField(Caption, fname) ; 
    UpdateDisplay ; 
  end{ "procedure TFormMacro11Source.SaveFile" } ; 

// compare Editor content with fileContent
function TFormMacro11Source.Changed: boolean ; 
  begin 
    // Editor.Lines.SaveToFile('e:\temp\editorslines.txt') ;
    // originalFileContent.SaveToFile('e:\temp\originalFileContent.txt');
    result := not Editor.Lines.Equals(originalFileContent) ; 
  end; 

// Source Im Editor mit MACRO11 übersetzten
// Listing automatisch ins Listingfenster laden
//
procedure TFormMacro11Source.Compile; 
  const timeout_millis = 5000 ; // macro11 darf max 5 Sek laufen!
  var 
    macro11_path: string ; 
    listfilename: string ; 
    args: string ; 
    workdir: string ; 
    starttime: dword ; 
    timeout: boolean ; 
    errormsg: string ; 
    errorline: integer ; 
    buff: array[0..1023] of AnsiChar ; 
  begin 
    // Marken löschen
    setExecutionLine(-1) ; 
    setErrorLine(-1); 
    errormsg := '' ; 
    errorline := -1 ; 
    Translated := false ; 

    // MACRO11.BAT ausführen
    // macro11.bat <sourcefilename> <listingfilename>

    // macro11 im Verzeichnis der GUI finden
    macro11_path := ExtractFilePath(Application.ExeName) + '\macro11.bat' ; 
    if not FileExists(macro11_path) then 
      raise Exception.CreateFmt('MACRO11.bat not found, must be "%s"', [macro11_path]) ; 

    // Variable PDP11GUIEXEDIR setzen, damit in MACRO11.BAT der
    // Installationspath bekannt ist.
    SetEnvironmentVariableA('PDP11GUIEXEDIR', strpcopy(buff, ExtractFileDir(Application.ExeName))) ; 
    SetEnvironmentVariableA('PDP11GUIAPPDATADIR', strpcopy(buff, FormMain.DefaultDataDirectory)) ; 

    if not IsDirectoryWriteable(FormMain.DefaultDataDirectory) then 
      raise Exception.CreateFmt('Can not write to directory "%s". Perhaps it''s read-only flag or you must "Run as Admin".', 
              [FormMain.DefaultDataDirectory]) ; 

    workdir := ExtractFileDir(SourceFilename) ; 
    if not IsDirectoryWriteable(workdir) then 
      raise Exception.CreateFmt('Can not write to directory "%s". Perhaps it''s read-only flag or you must "Run as Admin".', 
              [workdir]) ; 

    if Changed then 
      // auto speichern. Disk driver are readonly and write protected.
      SaveFile(SourceFilename) ; 

    listfilename := ChangeFileExt(SourceFilename, '.lst') ; // same directory
    DeleteFile(listfilename) ; 


    args := Format('"%s" "%s"', [SourceFilename, listfilename]) ; 
    Log('Starting MACRO11:') ; 
    Log('  Path    : %s', [macro11_path]) ; 
    Log('  args    : %s', [args]) ; 
    Log('  work dir: %s', [workdir]) ; 
    with macro11_appcontrol do begin 
      starttime := GetTickCount ; 
      StartApplication(macro11_path, args,workdir) ; 
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
        raise Exception.CreateFmt('MACRO11 timeout: running longer then %d secs', [timeout_millis div 1000]) ; 
    end{ "with macro11_appcontrol" } ; 

    ///// Mögliche Fehlermeldungen erkennen
    ///   in der Source und im Listing rot markieren,
    ///   und im Logfenster anzeigen
    if not FileExists(listfilename) then begin 
      errormsg := Format('MACRO11 failure: list file %s not found', [listfilename]) ; 
    end else begin 
      // Codeform füllen und anzeigen. Fehler im Listing finden und anzeigen
      with FormMain do begin 
        // listingform füllen und anzeigen

        TheRegistry.Save(FormMacro11Listing) ; // jetzige position sichern
        setChildFormVisibility(FormMacro11Listing, true); 

        FormMacro11Listing.LoadFile(listfilename) ; // echtes load erst in OnAfterShow() 
        // Nur die Listingform kann auch die Fehlermeldungen finden
        FormMacro11Listing.ParseCode ; 
        errormsg  := FormMacro11Listing.FirstErrorMsg ; 
        errorline := FormMacro11Listing.FirstErrorLineNr ; 

        // nicht automatisch den hex dump anzeigen.
        // FormMacro11Listing.ShowCodeForm ;
      end{ "with FormMain" } ; 

      // Wenn eine Fehlerzeile da ist:
      if errormsg <> '' then begin 
        setErrorLine(errorline) ; 
        errormsg := Format('MACRO-11 Error in line %d: "%s"', [errorline, errormsg]) ; 

        Log(errormsg) ; 
        UpdateDisplay ; 
        BringToFront ; 

        MessageDlg(errormsg, mtError, [mbOk], 0) ; // aufpoppen!
      end else begin 
        Translated := true ; 
      end; 
    end{ "if not FileExists(listfilename) ... ELSE" } ; 

  end{ "procedure TFormMacro11Source.Compile" } ; 


procedure TFormMacro11Source.CompileButtonClick(Sender: TObject); 
  begin 
    Compile ; 
  end; 


procedure TFormMacro11Source.NewButtonClick(Sender: TObject); 
  begin 
    LoadFile('') ; // "new"
  end; 


procedure TFormMacro11Source.LoadButtonClick(Sender: TObject); 
  begin 
    if SourceFilename = '' then 
      OpenDialog1.InitialDir := FormMain.DefaultDataDirectory 
    else OpenDialog1.InitialDir := ExtractFilePath(SourceFilename) ; 
    if OpenDialog1.Execute then begin 
      LoadFile(OpenDialog1.FileName) ; 
    end; 
  end; 


procedure TFormMacro11Source.SaveAsButtonClick(Sender: TObject); 
  begin 
    if SaveDialog1.InitialDir = '' then 
      SaveDialog1.InitialDir := FormMain.DefaultDataDirectory ; 
    SaveDialog1.FileName := SourceFilename ; 
    if SaveDialog1.Execute then begin 
      SourceFilename := SaveDialog1.FileName ; 
      SaveFile(SourceFilename); 
    end; 
    UpdateDisplay ; 
  end; 

procedure TFormMacro11Source.SaveButtonClick(Sender: TObject); 
  begin 
    if not FileExists(SourceFilename) then 
      SaveAsButtonClick(Sender) // fall back to "Save As"
    else begin 
      SaveFile(SourceFilename); 
      UpdateDisplay ; 
    end; 
  end; 


end{ "unit FormMacro11SourceU" } . 
