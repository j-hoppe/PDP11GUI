unit FormDiscImageExitQueryU;
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
mrOk = ExitWIndow
mrCancel = stayinWindow
mrABort = stop driver
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormDiscImageExitQueryForm = class(TForm)
      Label1: TLabel;
      ExitWindowButton: TButton;
      StayInWindowButton: TButton;
      StopDriverButton: TButton;
      DoNotShowAgainCheckBox: TCheckBox;
      procedure StayInWindowButtonClick(Sender: TObject);
      procedure StopDriverButtonClick(Sender: TObject);
      procedure ExitWindowButtonClick(Sender: TObject);
      procedure DoNotShowAgainCheckBoxClick(Sender: TObject);
      procedure FormShow(Sender: TObject);
    private
      { Private-Deklarationen }
    public
      { Public-Deklarationen }
      DoNotShowAgain : boolean ;
    end{ "TYPE TFormDiscImageExitQueryForm = class(TForm)" } ;

var
  FormDiscImageExitQueryForm: TFormDiscImageExitQueryForm;

implementation

{$R *.dfm}

procedure TFormDiscImageExitQueryForm.DoNotShowAgainCheckBoxClick(
        Sender: TObject);
  begin
    DoNotShowAgain := DoNotShowAgainCheckBox.Checked ;
  end;

procedure TFormDiscImageExitQueryForm.ExitWindowButtonClick(Sender: TObject);
  begin
    ModalResult := mrOk ;
  end;

procedure TFormDiscImageExitQueryForm.FormShow(Sender: TObject);
  begin
    DoNotShowAgainCheckBox.Checked := DoNotShowAgain ;
  end;

procedure TFormDiscImageExitQueryForm.StayInWindowButtonClick(Sender: TObject);
  begin
    ModalResult := mrCancel ; //
  end;

procedure TFormDiscImageExitQueryForm.StopDriverButtonClick(Sender: TObject);
  begin
    ModalResult := mrAbort ;
  end;

end{ "unit FormDiscImageExitQueryU" } .
