unit FormMemoryListU;
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
