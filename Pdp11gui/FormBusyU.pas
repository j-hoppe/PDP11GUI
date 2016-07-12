unit FormBusyU;
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
  Form mit progressbar und ABORT Button.
  Erscheint erst nach Verzögerung auf, wenn lange Laufdauer zu erwarten ist
}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls;


type
  TBusyForm = class(TForm)
      ProgressBar1: TProgressBar;
      InfoLabel: TLabel;
      AbortButton: TButton;
      procedure AbortButtonClick(Sender: TObject);
    private
      { Private-Deklarationen }
      fEnabled: boolean ;
      fAborted: boolean ;
      fSteps: integer ;
      fStartticks: dword ;

      function getRunningTime_ms: dword ;
      function getExpectedEndTime_ms: dword ;
    public
      { Public-Deklarationen }
      procedure Start(info:string; total: integer; enabled: boolean) ;
//      function Progress(current, total: integer): boolean ;
      procedure StepIt ; overload ;
      procedure StepIt(n: integer) ; overload ;
      function Aborted: boolean ;
      procedure Close ;
    end{ "TYPE TBusyForm = class(TForm)" } ;

var
  BusyForm: TBusyForm;

implementation

{$R *.dfm}


const
  sampletime_ms = 1000 ; // sample tocks 1 sekunde, bevor über pop-up entschieden wird
  displayThreshold_ms = 5000 ; // pop up, wenn vorgang mindestens solange dauert




{
  enabled: false = keine functions, ist fuer caller bequemer so
}
procedure TBusyForm.Start(info:string; total: integer; enabled: boolean) ;
  begin
    fEnabled := enabled ;
    fAborted := false ;
    InfoLabel.Caption := info ;
    ProgressBar1.Min := 0 ;
    ProgressBar1.Max := total ;
    ProgressBar1.Position := 0 ;
    fSteps := 0 ;
    fStartticks := GetTickCount ;
    // show erst nach Anfangswartepause, wenn Vorgaang länger dauern wird
  end;

function TBusyForm.Aborted: boolean ;
  begin
    if not fEnabled then
      result := false // nie user abbruch
    else begin
      Application.ProcessMessages ;
      result := fAborted ;
    end;
  end;

procedure TBusyForm.StepIt(n: integer) ;
  begin
    if not fEnabled then
      Exit ;
    fSteps := fSteps +n ;
    ProgressBar1.Position := fSteps ;
    ProgressBar1.Update ;
    Application.ProcessMessages ;
    if Visible then
      BringToFront
    else begin
      // show erst nach Anfangswartepause, wenn Vorgang länger dauern wird
      if (getRunningTime_ms > sampletime_ms)
//      and ( getExpectedEndTime_ms > (fStartticks + displayThreshold_ms)) then
        then
          inherited Show ;
    end ;

  end{ "procedure TBusyForm.StepIt" } ;

  procedure TBusyForm.StepIt ;
  begin
    StepIt(1) ;
  end;



procedure TBusyForm.AbortButtonClick(Sender: TObject);
  begin
    fAborted := true ;
  end;

procedure TBusyForm.Close ;
  begin
    inherited Close ;
  end;


function TBusyForm.getRunningTime_ms: dword ;
  begin
    result := GetTickCount - fStartticks ;
  end;

// Hochrechnung: wann werden 100% erriecht?
// 0: nicht möglich
function TBusyForm.getExpectedEndTime_ms: dword ;
  begin
    if fSteps = 0 then
      result := 0
    else
      result := round(ProgressBar1.Max * getRunningTime_ms / fSteps) ;
  end;


end{ "unit FormBusyU" } .
