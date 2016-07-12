unit FormNoConsolePromptU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  FormChildU;

type
  TFormNoConsolePrompt = class(TFormChild)
    Memo1: TMemo;
    OkButton: TButton;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FormNoConsolePrompt: TFormNoConsolePrompt;

implementation

{$R *.dfm}

end.
