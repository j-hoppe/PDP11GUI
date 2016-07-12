unit FormDiscImageExitQueryU; 

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
