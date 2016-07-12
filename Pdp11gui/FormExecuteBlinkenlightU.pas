unit FormExecuteBlinkenlightU;
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
  Form for Run Control.
  also auxilary services to execute code, based on Console* methods.
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls,
  JH_Utilities,
  FormChildU,
  AddressU,
  MemoryCellU,
  RegistryU,
  BlinkenlightInstructionsU ;

type

  TExecuteState = (esUnknown,
      esCompiling, // programm wird übersetzt
      esShow // instructions werden angezeigt
    ) ;

  TFormExecuteBlinkenlight = class(TFormChild)
      Panel1: TPanel;
      StartPCEdit: TEdit;
      Label1: TLabel;
      NewPgmButton: TButton;
      BlinkenLightInstructionMemo: TMemo;
    SaveButton: TButton;
    SaveDialog1: TSaveDialog;
      procedure StartPCEditChange(Sender: TObject);
      procedure FormShow(Sender: TObject);
      procedure CurPCEditKeyPress(Sender: TObject; var Key: char);
      procedure NewPgmButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    private
      { Private-Deklarationen }
    public
      { Public-Deklarationen }
      TheState: TExecuteState ;
      StartPc_v: TMemoryAddress ; // Program counter at start, virtuelle Adresse

      constructor Create(AOwner: TComponent) ; override ;

      procedure UpdateDisplay ;
    end{ "TYPE TFormExecuteBlinkenlight = class(TFormChild)" } ;


implementation

uses
  AuxU,
  OctalConst,
  FormLogU, FormMainU;//, SerialIoHubU;

{$R *.dfm}


constructor TFormExecuteBlinkenlight.Create(AOwner: TComponent) ;
  begin
    inherited ;
    TheRegistry.Load(StartPCEdit, '0') ;
    TheState := esUnknown ;
  end;


procedure TFormExecuteBlinkenlight.CurPCEditKeyPress(Sender: TObject; var Key: char);
  begin
    // nur Octalziffern als Eingabe. auch das backspace #8 erlauben
    if not CharInSet(Key, [#8, '0'..'7']) then
      Key := #0 ;
  end;

procedure TFormExecuteBlinkenlight.FormShow(Sender: TObject);
  begin
    UpdateDisplay ;
  end;


procedure TFormExecuteBlinkenlight.NewPgmButtonClick(Sender: TObject);
  var
    bi: TBlinkenlightInstructions ;
  begin
    bi := TBlinkenlightInstructions.Create ;
    TheState := esCompiling ;

    try
      UpdateDisplay ;
      // wenn kein gültiger File da: User zum Wählen auffordern!
      FormMain.FormMacro11Source.Show ; // immer aufpoppen
      if not FormMain.FormMacro11Source.CanTranslate then begin
        FormMain.FormMacro11Source.LoadButtonClick(nil) ;
      end;
      if not FormMain.FormMacro11Source.CanTranslate then
        MessageDlg('No valid source file selected!', mtError, [mbOk], 0)
      else begin
        FormMain.FormMacro11Source.CompileButtonClick(nil);
        if FormMain.FormMacro11Source.Translated then begin // nicht laden, wenn error
          // Binärcode jetzt in FormMain.FormMacro11Listing.memorycellgroup
          TheState := esShow ;
          // Anzeige mit Listing und Anleitung zum Programmstart
          bi.Generate(
            FormMain.FormMacro11Listing.memorycellgroup,
              FormMain.FormMacro11Listing.Editor.Lines,
            StartPc_v.val) ;
          BlinkenLightInstructionMemo.Lines.Assign(bi.OutputLines);
          UpdateDisplay ;
        end;
      end ;
    finally
      bi.Free ;
      BringToFront ;
    end{ "try" } ;
  end{ "procedure TFormExecuteBlinkenlight.NewPgmButtonClick" } ;


procedure TFormExecuteBlinkenlight.SaveButtonClick(Sender: TObject);
begin
 if SaveDialog1.Execute then
    BlinkenLightInstructionMemo.Lines.SaveToFile(SaveDialog1.FileName);
end;

procedure TFormExecuteBlinkenlight.StartPCEditChange(Sender: TObject);
  begin
    try
      StartPc_v := OctalStr2Addr(StartPCEdit.Text, matVirtual) ;
//    UpdateProperties ;
      TheRegistry.Save(StartPCEdit) ;
    except
    end;
  end;


procedure TFormExecuteBlinkenlight.UpdateDisplay ;
  begin

    StartPCEdit.Text := Addr2OctalStr(StartPc_v) ;

    // Das Enable der Controls hängt ab von den
    // - features der Console (und damit von Run/Halt mode)
    // - vom State der Execution form
    // Es sind auf jeden Fall nur die Controls aktiv, die features der console entsprechen
    case TheState of
      esUnknown: begin // alle controls aktiv ... damit der User alles machen kann
        NewPgmButton.Enabled := true ;
        Caption := setFormCaptionInfoField(Caption, 'state unknown') ;
        BlinkenLightInstructionMemo.Clear ;
        BlinkenLightInstructionMemo.Enabled := false ;
      end;
      esCompiling: begin
        NewPgmButton.Enabled := false ;
        BlinkenLightInstructionMemo.Clear ;
        BlinkenLightInstructionMemo.Enabled := false ;

        Caption := setFormCaptionInfoField(Caption, 'Compiling ...') ;
      end;
      esShow: begin
        NewPgmButton.Enabled := true ;
        BlinkenLightInstructionMemo.Enabled := true ;

        Caption := setFormCaptionInfoField(Caption, 'Ready') ;

      end;
    end{ "case TheState" } ;


  end{ "procedure TFormExecuteBlinkenlight.UpdateDisplay" } ;




end{ "unit FormExecuteBlinkenlightU" } .
