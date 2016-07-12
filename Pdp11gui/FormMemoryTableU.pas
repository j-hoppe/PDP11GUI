unit FormMemoryTableU;

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
  MemoryCellU, Menus, FrameMemoryCellGroupGridU ;

const
  max_memoryblocksize = 256 ; // nur max soviele Speicherzellen anzeigen

type
  TFormMemoryTable = class(TFormChild)
      PanelT: TPanel;
      ExamineAllButton: TButton;
      DepositChangedButton: TButton;
      DepositAllButton: TButton;
      Label1: TLabel;
      StartAddrEdit: TEdit;
      SetStartAddrButton: TButton;
      DecAddrButton: TButton;
      IncAddrButton: TButton;
      ExamineCurrentButton: TButton;
      MemoryGrid: TFrameMemoryCellGroupGrid;
      Label2: TLabel;
      MemoryBlockSizeEdit: TEdit;

      procedure StartAddrEditKeyPress(Sender: TObject; var Key: Char);
      procedure SetStartAddrButtonClick(Sender: TObject);
      procedure IncDecAddrButtonClick(Sender: TObject);
      procedure ExamineCurrentButtonClick(Sender: TObject);
      procedure ExamineAllButtonClick(Sender: TObject);
      procedure DepositChangedButtonClick(Sender: TObject);
      procedure DepositAllButtonClick(Sender: TObject);
      procedure PanelTClick(Sender: TObject);
      procedure MemoryBlockSizeEditKeyPress(Sender: TObject; var Key: Char);
    procedure MemoryGridVerify1Click(Sender: TObject);
    private
      { Private-Deklarationen }
    public
      { Public-Deklarationen }
      constructor Create(AOwner: TComponent) ;
      destructor Destroy ; override ;

      procedure UpdateDisplay(Sender: TObject);
    end{ "TYPE TFormMemoryTable = class(TFormChild)" } ;


implementation

{$R *.dfm}

uses
  AuxU,
  AddressU ;

constructor TFormMemoryTable.Create(AOwner: TComponent) ;
  begin
    inherited Create(AOwner) ;
    MemoryGrid.OnUpdate := UpdateDisplay ; // wenn sich das grid ändert, muss diese Form reagieren
  end;

destructor TFormMemoryTable.Destroy ;
  begin
    inherited ;
  end;

procedure TFormMemoryTable.ExamineAllButtonClick(Sender: TObject);
  begin
    MemoryGrid.ExamineAllButtonClick(Sender);
  end;

procedure TFormMemoryTable.ExamineCurrentButtonClick(Sender: TObject);
  begin
    MemoryGrid.ExamineCurrentButtonClick(Sender);
  end;

// neue malen
procedure TFormMemoryTable.UpdateDisplay(Sender: TObject);
  var
    mc: TMemoryCell ;
  begin
    if Sender <> MemoryGrid then // hat der Frame das Update ausgelöst?
      MemoryGrid.UpdateDisplay  // nein: update frame, er updated wieder die Form
    else begin
      mc := MemoryGrid.memorycellgroup.Cell(0) ;
      StartAddrEdit.Text := Addr2OctalStr(mc.addr) ; // 1. Zelle = Startaddr
      MemoryBlockSizeEdit.Text := Dword2OctalStr(MemoryGrid.memorycellgroup.getCount) ;

      Caption := setFormCaptionInfoField(Caption, Addr2OctalStr(mc.addr)) ;

      // Das MemoryGrid ist alClient und möchte in einer bestimmten Grösse angezeigt werden,
      // tue ihm den Gefallen.
      ClientHeight :=  MemoryGrid.optimal_height + PanelT.Height ;
      // nur vergrössern
      // normal ist width statisch, can aber zu klein sein, wenn
      // "Windows font magnification" auf > 100% ist
      if MemoryGrid.optimal_width > CLientWidth then
          ClientWidth := MemoryGrid.optimal_width ;
    end;
  end{ "procedure TFormMemoryTable.UpdateDisplay" } ;


procedure TFormMemoryTable.StartAddrEditKeyPress(Sender: TObject;
        var Key: Char);
  begin
    // nur Octalziffern als Eingabe. auch das backspace #8 erlauben
    if not CharInSet(Key, [#8, '0'..'7']) then
      Key := #0 ;
  end;


procedure TFormMemoryTable.MemoryBlockSizeEditKeyPress(Sender: TObject;
        var Key: Char);
  begin
    // nur Octalziffern als Eingabe. auch das backspace #8 erlauben
    if not CharInSet(Key, [#8, '0'..'7']) then
      Key := #0 ;
  end;


procedure TFormMemoryTable.MemoryGridVerify1Click(Sender: TObject);
begin
  MemoryGrid.Verify1Click(Sender);
end;

procedure TFormMemoryTable.SetStartAddrButtonClick(Sender: TObject);
  var
    startaddr: TMemoryAddress ;
    memblocksize: integer ;
  begin
    startaddr := OctalStr2Addr(StartAddrEdit.Text, MemoryGrid.memorycellgroup.mat) ;
    memblocksize := OctalStr2Dword(MemoryBlockSizeEdit.Text) ;
    if memblocksize < 1 then
      memblocksize := 1
    else if memblocksize > max_memoryblocksize then
      memblocksize := max_memoryblocksize ;

    if startaddr.val <> MEMORYCELL_ILLEGALVAL then begin
      MemoryGrid.memorycellgroup.ShiftRange(startaddr, memblocksize, {optimize=}true) ;
//      MemoryGrid.memorycellgroup.ShiftRange(startaddr, -1, {optimize=}true) ;
//      MemoryGrid.memorycellgroup.Examine({optimize=}true) ;
    end;

    // neue mcg: neu mit grid verbinden
    MemoryGrid.ConnectToMemoryCellGroup(MemoryGrid.memorycellgroup) ;
    // examine nur die zellen mit edit_value =ILLEGAL
    // nein! auf M9312 console emulator führt jede nicht vorhandene Adresse zum stop
//    MemoryGrid.ExamineCells({unknown_only}true) ;
    UpdateDisplay(self) ;
  end{ "procedure TFormMemoryTable.SetStartAddrButtonClick" } ;



procedure TFormMemoryTable.IncDecAddrButtonClick(Sender: TObject);
  var startaddr: TMemoryAddress;
    deltaaddr : dword ; // decrement = 1 Grid Zeile, oder blocksize
  begin
    // Adresse um eine Gridzeile verringern, oder um memoryblocksize, wenn das weniger ist
    deltaaddr := (MemoryGrid.MemoryCellsStringGrid.ColCount -1) * 2 ;
      if deltaaddr > MemoryGrid.memorycellgroup.getCount then
       deltaaddr:= MemoryGrid.memorycellgroup.getCount ;

    startaddr := OctalStr2Addr(StartAddrEdit.Text, MemoryGrid.memorycellgroup.mat) ;

    if Sender = DecAddrButton then begin
      // decrement
      if startaddr.val <> MEMORYCELL_ILLEGALVAL then
        if startaddr.val < deltaaddr then
          startaddr.val := 0
        else
          startaddr.val := startaddr.val - deltaaddr ;
    end else begin
      // increment
      if startaddr.val <> MEMORYCELL_ILLEGALVAL then
        startaddr.val := startaddr.val + deltaaddr ;
    end ;
    StartAddrEdit.Text := Addr2OctalStr(startaddr) ;
    // Anzeige
    SetStartAddrButtonClick(Sender) ;
    MemoryGrid.ExamineCells({unknown_only}true) ; // neue Zeile gleich abfragen
    UpdateDisplay(self) ; // nochmal
  end{ "procedure TFormMemoryTable.IncDecAddrButtonClick" } ;

procedure TFormMemoryTable.DepositAllButtonClick(Sender: TObject);
  begin
    MemoryGrid.DepositAllButtonClick(Sender);
  end;

procedure TFormMemoryTable.DepositChangedButtonClick(Sender: TObject);
  begin
    MemoryGrid.DepositChangedButtonClick(Sender);
  end;


procedure TFormMemoryTable.PanelTClick(Sender: TObject);
  begin
    UpdateDisplay(nil);
  end;

end{ "unit FormMemoryTableU" } .


