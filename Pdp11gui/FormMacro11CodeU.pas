unit FormMacro11CodeU;
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
Speicher als editierbare Tabelle
Es werden immer 'memcol' Spalten nebeneinander angezeigt
Kopf "+0, +2, +4, ..."
Reihen

Die Anzahl der Spalten wird im Constructor festgelegt ('MemoryColumns').

}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, StdCtrls, ExtCtrls,
  FormChildU,
  JH_Utilities,
  AddressU,
  MemoryCellU,
  Menus,
  FrameMemoryCellGroupGridU ;

type
  TFormMacro11Code = class(TFormChild)
      PanelT: TPanel;
      DepositAllButton: TButton;
      Label1: TLabel;
      StartAddrEdit: TEdit;
      MemoryGrid: TFrameMemoryCellGroupGrid;

      procedure DepositAllButtonClick(Sender: TObject);
    private
      { Private-Deklarationen }
    public
      { Public-Deklarationen }
      constructor Create(AOwner: TComponent) ;
      destructor Destroy ; override ;

      procedure UpdateDisplay(Sender: TObject);
    end{ "TYPE TFormMacro11Code = class(TFormChild)" } ;

//var
//  FormMacro11Code: TFormMacro11Code;

implementation

{$R *.dfm}

uses
  AuxU, FormMainU;


constructor TFormMacro11Code.Create(AOwner: TComponent) ;
  begin
    inherited Create(AOwner) ;
    MemoryGrid.OnUpdate := UpdateDisplay ; // wenn sich das grid ändert, muss diese Form reagieren
    StartAddrEdit.ReadOnly := true ;

  end;

destructor TFormMacro11Code.Destroy ;
  begin
    inherited ;
  end;



// neue malen
procedure TFormMacro11Code.UpdateDisplay(Sender: TObject);
  var
    mc: TMemoryCell ;
    h, w: integer ;
  begin
    if Sender <> MemoryGrid then // hat der Frame das Update ausgelöst?
      MemoryGrid.UpdateDisplay  // nein: update frame, er updated wieder die Form
    else begin
      // Editierte Memoryinhalte behalten, auch wenn Pdp durch callbacks neu abgefragt wird.
      MemoryGrid.memorycellgroup.PdpOverwritesEdit := false ; // statische initialisierung

      mc := MemoryGrid.memorycellgroup.Cell(0) ;
      StartAddrEdit.Text := Addr2OctalStr(mc.addr) ; // 1. Zelle = Startaddr
      Caption := setFormCaptionInfoField(Caption, Addr2OctalStr(mc.addr)) ;

      // Das MemoryGrid ist alClient und möchte in einer bestimmten Grösse angezeigt werden,
      // tue ihm den Gefallen.
      // Wilde ad hoc Logik: das Codewindow kann extrem hoch werden, dann kürzer anzeigen
      h := MemoryGrid.optimal_height + PanelT.Height ;
      w := MemoryGrid.optimal_width ;
      if h > (FormMain.ClientHeight-100) then
        h := FormMain.ClientHeight-100  ;
      if h < 200 then h := 200 ; // falls mainform klein ist
      if h - PanelT.Height < MemoryGrid.optimal_height then
        w := w + 20 ; // grid zeigt vertical scrollbar

      ClientHeight := h ;
      ClientWidth := w ;
    end{ "if Sender <> MemoryGrid ... ELSE" } ;
  end{ "procedure TFormMacro11Code.UpdateDisplay" } ;


procedure TFormMacro11Code.DepositAllButtonClick(Sender: TObject);
  begin
    MemoryGrid.DepositAllButtonClick(Sender);
  end;

end{ "unit FormMacro11CodeU" } .


