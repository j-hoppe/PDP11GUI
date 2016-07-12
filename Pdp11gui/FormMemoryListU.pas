unit FormMemoryListU; 

{
  Eine Reihe Speicherzellen als ausführliche Liste
}

interface 

uses 
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, Grids, ExtCtrls, StdCtrls, 
  FormChildU, 
  MemoryCellU, FrameMemoryCellGroupListU; 

type 
  TFormMemoryList = class(TFormChild) 
      PanelT: TPanel; 
      DepositChangedButton: TButton; 
      DepositAllButton: TButton; 
      ExamineAllButton: TButton; 
      InfoLabel: TLabel; 
      ExamineCurrentButton: TButton; 
      MemoryList: TFrameMemoryCellGroupList; 
      procedure ExamineCurrentButtonClick(Sender: TObject); 
      procedure ExamineAllButtonClick(Sender: TObject); 
      procedure DepositChangedButtonClick(Sender: TObject); 
      procedure DepositAllButtonClick(Sender: TObject); 
    private 
      { Private-Deklarationen }
    public 
      { Public-Deklarationen }
      procedure ConnectToMemoryCellGroup(mcg: TMemoryCellGroup) ; 

    end{ "TYPE TFormMemoryList = class(TFormChild)" } ; 


implementation 


{$R *.dfm}
procedure TFormMemoryList.ConnectToMemoryCellGroup(mcg: TMemoryCellGroup) ; 
  begin 
    InfoLabel.Caption := mcg.groupinfo ; 

    MemoryList.ConnectToMemoryCellGroup(mcg); 

    // Formhoehe so einstellen, dass das Grid genau alle Zeilen anzeigt
    with MemoryList.MemoryCellsStringGrid do 
      self.ClientHeight := PanelT.Height + RowCount * (1+DefaultRowHeight) + 3 ; 
  end; 


procedure TFormMemoryList.DepositAllButtonClick(Sender: TObject); 
  begin 
    MemoryList.DepositAllButtonClick(Sender); 
  end; 

procedure TFormMemoryList.DepositChangedButtonClick(Sender: TObject); 
  begin 
    MemoryList.DepositChangedButtonClick(Sender); 
  end; 

procedure TFormMemoryList.ExamineAllButtonClick(Sender: TObject); 
  begin 
    MemoryList.ExamineAllButtonClick(Sender); 
  end; 

procedure TFormMemoryList.ExamineCurrentButtonClick(Sender: TObject); 
  begin 
    MemoryList.ExamineCurrentButtonClick(Sender); 
  end; 

end{ "unit FormMemoryListU" } . 
