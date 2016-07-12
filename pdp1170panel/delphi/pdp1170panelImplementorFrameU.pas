unit pdp1170panelImplementorFrameU;
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
    pdp1170panelframe

    zeigt das Bild eine sPDP11/70 Consolepanels an
    Die LEDs und Switches werden über ein untergeordnetes
    Tpdp1170panel verwaltet. Die Anwendung arbeitet nur mit member
    "pdp1170panel", nie mit dem Frame

    LEDS und Switches sind durch Images dargestellt.
    EIn LED oder Switch hat mehrere "States"
    LED: immer 2 States (0=OFF, 1=ON)
    Switches haben meist 2 States, manche haben auch mehr (Drehknöpfe haben 8).

    Die Images sind den States zugeordnet.
    Für den Normalsatte 0 zeigt das Hintergrund bild den State schon an.

    - Klicken auf einen Switch:
    - a) ein Visible Image wird angeclickt:
      der Switch ist im Satte > 0 und wird über ein übergegblendetes Image dargestellt.
     b) Das Hintergrundbild kriegt den Click:
       Der Switch ist im State 0, kein besonderes Iamge wird angezeigt.
       Es wird ermittelt, welche unsichtbaren Images am Mausklickpunkt liegen,
       über diese wird dann der angclickte Switch gefunden.

}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls,
  pdp1170panelU ;

type

  // Ist ein Implementor für pdp1170panel
  Tpdp1170panelImplementorFrame = class(TFrame)
      img_panel_background: TImage;
      img_sw_21: TImage;
      img_sw_17: TImage;
      img_sw_16: TImage;
      img_sw_15: TImage;
      img_sw_11: TImage;
      img_sw_10: TImage;
      img_sw_09: TImage;
      img_sw_05: TImage;
      img_sw_04: TImage;
      img_sw_03: TImage;
      img_sw_load_adrs: TImage;
      img_sw_dep: TImage;
      img_sw_halt: TImage;
      img_sw_start: TImage;
      img_sw_20: TImage;
      img_sw_19: TImage;
      img_sw_18: TImage;
      img_sw_14: TImage;
      img_sw_13: TImage;
      img_sw_12: TImage;
      img_sw_08: TImage;
      img_sw_07: TImage;
      img_sw_06: TImage;
      img_sw_02: TImage;
      img_sw_01: TImage;
      img_sw_00: TImage;
      img_sw_exam: TImage;
      img_sw_cont: TImage;
      img_sw_sbus_cycle: TImage;
      img_sw_lamptest: TImage;
      img_led_addr_21: TImage;
      img_led_addr_20: TImage;
      img_led_addr_15: TImage;
      img_led_addr_19: TImage;
      img_led_addr_18: TImage;
      img_led_addr_17: TImage;
      img_led_addr_16: TImage;
      img_led_data_14: TImage;
      img_led_data_13: TImage;
      img_led_data_12: TImage;
      img_led_data_11: TImage;
      img_led_data_10: TImage;
      img_led_data_09: TImage;
      img_led_data_08: TImage;
      img_led_par_err: TImage;
      img_led_adrs_err: TImage;
      img_led_run: TImage;
      img_led_pause: TImage;
      img_led_master: TImage;
      img_led_user: TImage;
      img_led_super: TImage;
      img_led_kernel: TImage;
      img_led_data: TImage;
      img_led_addressing_16: TImage;
      img_led_addressing_18: TImage;
      img_led_addressing_22: TImage;
      img_led_addr_07: TImage;
      img_led_addr_06: TImage;
      img_led_addr_05: TImage;
      img_led_addr_04: TImage;
      img_led_addr_03: TImage;
      img_led_addr_02: TImage;
      img_led_data_00: TImage;
      img_led_data_01: TImage;
      img_led_data_03: TImage;
      img_led_data_04: TImage;
      img_led_data_07: TImage;
      img_led_data_06: TImage;
      img_led_data_05: TImage;
      img_led_data_15: TImage;
      img_led_parity_low: TImage;
      img_led_parity_high: TImage;
      img_led_addr_14: TImage;
      img_led_addr_13: TImage;
      img_led_addr_12: TImage;
      img_led_addr_11: TImage;
      img_led_addr_10: TImage;
      img_led_addr_09: TImage;
      img_led_addr_08: TImage;
      img_led_user_d: TImage;
      img_led_super_d: TImage;
      img_led_kernel_d: TImage;
      img_led_cons_phy: TImage;
      img_led_user_i: TImage;
      img_led_super_i: TImage;
      img_led_kernel_i: TImage;
      img_led_prog_phy: TImage;
      img_led_data_paths: TImage;
      img_led_bus_reg: TImage;
      img_led_uadrs_fpp_cpu: TImage;
      img_led_display_reg: TImage;
      img_led_data_02: TImage;
      img_led_addr_01: TImage;
      img_led_addr_00: TImage;
      img_sw_addrselect_pos0: TImage;
      img_sw_addrselect_pos1: TImage;
      img_sw_addrselect_pos2: TImage;
      img_sw_addrselect_pos3: TImage;
      img_sw_addrselect_pos4: TImage;
      img_sw_addrselect_pos5: TImage;
      img_sw_addrselect_pos6: TImage;
      img_sw_addrselect_pos7: TImage;
      img_sw_dataselect_pos0: TImage;
      img_sw_dataselect_pos1: TImage;
      img_sw_dataselect_pos2: TImage;
      img_sw_dataselect_pos3: TImage;
      img_sw_dataselect_pos4: TImage;
      img_sw_dataselect_pos5: TImage;
      img_sw_dataselect_pos6: TImage;
      img_sw_dataselect_pos7: TImage;

      procedure ImagesMouseDown(Sender: TObject; Button: TMouseButton;
              Shift: TShiftState; x, y: integer);
      procedure ImagesMouseUp(Sender: TObject; Button: TMouseButton;
              Shift: TShiftState; x, y: integer);
      procedure img_panel_backgroundMouseMove(Sender: TObject; Shift: TShiftState;
              x, y: integer);
    private
      { Private-Deklarationen }
      // Welches Image liegt an den Mauskoordinaten?
      function ControlImageAtPos(pos: TPoint; var ctrl: Tpdp1170PanelControl): TImage ;

      // alle Images so anzeigen, wie es der State der pdp1170panels erfordert
      procedure UpdateImages ;

    public
      { Public-Deklarationen }
      pdp1170panel: Tpdp1170Panel ; // BasisPanel, unabhängig von Darstellung

      constructor Create(Owner: TComponent); override ;
      // destructor Destroy ;

      // gleicht das pdp1170panel-Object mit dem Frame ab.
      // schreibt die gelesenen neuen Control-States nach new_active_state.
      procedure SyncLedsFromBasePanel(Sender: TObject) ;
      procedure SyncSwitchesToBasePanel(Sender: TObject) ;

    end{ "TYPE Tpdp1170panelImplementorFrame = class(TFrame)" } ;


implementation


{$R *.DFM}


constructor Tpdp1170panelImplementorFrame.Create(Owner: TComponent) ;
  var
    ctrl: Tpdp1170PanelControl ;
    state: Tpdp1170PanelControlState ;
    i, j: integer ;
  begin
    inherited Create(Owner) ;
    pdp1170panel :=  Tpdp1170Panel.Create ;

    pdp1170panel.OnSyncSwitchesFromImplementor := SyncSwitchesToBasePanel ;
    pdp1170panel.OnSyncLedsToImplementor := SyncLedsFromBasePanel ;

    DoubleBuffered := true ;

    ////////////////// die Images an die LEDs binden ////////////////////////////
    ctrl := pdp1170panel.getControlByName(_Led, pdp1170Led_ADDRESS_SELECT, 'KERNEL-D') ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_kernel_d ; // state[0] := OFF: im Hintergrundsbild!
    ctrl := pdp1170panel.getControlByName(_Led, pdp1170Led_ADDRESS_SELECT, 'SUPER-D') ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_super_d ;
    ctrl := pdp1170panel.getControlByName(_Led, pdp1170Led_ADDRESS_SELECT, 'USER-D') ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_user_d ;
    ctrl := pdp1170panel.getControlByName(_Led, pdp1170Led_ADDRESS_SELECT, 'KERNEL-I') ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_kernel_i ;
    ctrl := pdp1170panel.getControlByName(_Led, pdp1170Led_ADDRESS_SELECT, 'SUPER-I') ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_super_i ;
    ctrl := pdp1170panel.getControlByName(_Led, pdp1170Led_ADDRESS_SELECT, 'USER-I') ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_user_i ;
    ctrl := pdp1170panel.getControlByName(_Led, pdp1170Led_ADDRESS_SELECT, 'CONS PHY') ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_cons_phy ;
    ctrl := pdp1170panel.getControlByName(_Led, pdp1170Led_ADDRESS_SELECT, 'PROG PHY') ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_prog_phy ;

    ctrl := pdp1170panel.getControlByName(_Led, pdp1170Led_DATA_SELECT, 'DATA PATHS') ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_data_paths ;
    ctrl := pdp1170panel.getControlByName(_Led, pdp1170Led_DATA_SELECT, 'BUS REG') ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_bus_reg ;
    ctrl := pdp1170panel.getControlByName(_Led, pdp1170Led_DATA_SELECT, 'µADRS FPP/CPU') ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_uadrs_fpp_cpu ;
    ctrl := pdp1170panel.getControlByName(_Led, pdp1170Led_DATA_SELECT, 'DISPLAY REGISTER') ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_display_reg ;

    // Adress-Leds: über Index finden
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 0) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_addr_00 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 1) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_addr_01 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 2) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_addr_02 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 3) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_addr_03 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 4) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_addr_04 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 5) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_addr_05 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 6) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_addr_06 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 7) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_addr_07 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 8) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_addr_08 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 9) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_addr_09 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 10) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_addr_10 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 11) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_addr_11 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 12) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_addr_12 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 13) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_addr_13 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 14) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_addr_14 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 15) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_addr_15 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 16) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_addr_16 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 17) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_addr_17 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 18) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_addr_18 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 19) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_addr_19 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 20) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_addr_20 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 21) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_addr_21 ;

    // DATA-Leds: über Index finden
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_DATA, 0) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_data_00 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_DATA, 1) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_data_01 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_DATA, 2) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_data_02 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_DATA, 3) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_data_03 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_DATA, 4) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_data_04 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_DATA, 5) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_data_05 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_DATA, 6) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_data_06 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_DATA, 7) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_data_07 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_DATA, 8) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_data_08 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_DATA, 9) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_data_09 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_DATA, 10) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_data_10 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_DATA, 11) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_data_11 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_DATA, 12) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_data_12 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_DATA, 13) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_data_13 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_DATA, 14) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_data_14 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_DATA, 15) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_data_15 ;

    ctrl := pdp1170panel.getControlByName(_Led, pdp1170Led_PARITY, 'PARITY LOW') ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_parity_low ;
    ctrl := pdp1170panel.getControlByName(_Led, pdp1170Led_PARITY, 'PARITY HIGH') ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_parity_high ;

    // Diese LEDS sind einzeln in ihrer Gruppe, kein Name nötig
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_PAR_ERR, 0) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_par_err ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADRS_ERR, 0) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_adrs_err ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_RUN, 0) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_run ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_PAUSE, 0) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_pause ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_MASTER, 0) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_master ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_USER_SUPER_KERNEL, 0) ; assert(ctrl <> nil) ;
    state := ctrl.getStateByName('USER') ; assert(state <> nil) ;
    state.image := img_led_user ;
    state := ctrl.getStateByName('SUPER') ; assert(state <> nil) ;
    state.image := img_led_super ;
    state := ctrl.getStateByName('KERNEL') ; assert(state <> nil) ;
    state.image := img_led_kernel ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESSING_16, 0) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_addressing_16 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESSING_18, 0) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_addressing_18 ;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESSING_22, 0) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_addressing_22;
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_DATA_SPACE, 0) ; assert(ctrl <> nil) ;
    ctrl.States[1].image := img_led_data ;


    ////////////////// die Images an die Switches binden ////////////////////////////
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_Power, 0) ; assert(ctrl <> nil) ;
    state := ctrl.getStateByName('OFF') ; assert(state <> nil) ;
    state.image := nil ; // sw_power_off
    state := ctrl.getStateByName('POWER') ; assert(state <> nil) ;
    state.image := nil ; // sw_power_power
    state := ctrl.getStateByName('LOCK') ; assert(state <> nil) ;
    state.image := nil ; // sw_power_lock

    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_LampTest, 0) ; assert(ctrl <> nil) ;
    state := ctrl.getStateByName('OFF') ; assert(state <> nil) ;
    state.image := nil ; // Hintergrundsbild
    state := ctrl.getStateByName('LAMP TEST') ; assert(state <> nil) ;
    state.image := img_sw_lamptest ;

    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_LOAD_ADRS, 0) ; assert(ctrl <> nil) ;
    state := ctrl.getStateByName('OFF') ; assert(state <> nil) ;
    state.image := nil ; // Hintergrundsbild
    state := ctrl.getStateByName('LOAD ADRS') ; assert(state <> nil) ;
    state.image := img_sw_load_adrs ;

    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_EXAM, 0) ; assert(ctrl <> nil) ;
    state := ctrl.getStateByName('OFF') ; assert(state <> nil) ;
    state.image := nil ; // Hintergrundsbild
    state := ctrl.getStateByName('EXAM') ; assert(state <> nil) ;
    state.image := img_sw_exam ;

    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DEP, 0) ; assert(ctrl <> nil) ;
    state := ctrl.getStateByName('OFF') ; assert(state <> nil) ;
    state.image := nil ; // Hintergrundsbild
    state := ctrl.getStateByName('DEP') ; assert(state <> nil) ;
    state.image := img_sw_dep ;

    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_CONT, 0) ; assert(ctrl <> nil) ;
    state := ctrl.getStateByName('OFF') ; assert(state <> nil) ;
    state.image := nil ; // Hintergrundsbild
    state := ctrl.getStateByName('CONT') ; assert(state <> nil) ;
    state.image := img_sw_cont ;

    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_ENABLE_HALT, 0) ; assert(ctrl <> nil) ;
    state := ctrl.getStateByName('ENABLE') ; assert(state <> nil) ;
    state.image := nil ; // Hintergrundsbild
    state := ctrl.getStateByName('HALT') ; assert(state <> nil) ;
    state.image := img_sw_halt ;

    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_SINST_SBUS_CYCLE, 0) ; assert(ctrl <> nil) ;
    state := ctrl.getStateByName('S INST') ; assert(state <> nil) ;
    state.image := nil ; // Hintergrundsbild
    state := ctrl.getStateByName('S BUS CYCLE') ; assert(state <> nil) ;
    state.image := img_sw_sbus_cycle ;

    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_START, 0) ; assert(ctrl <> nil) ;
    state := ctrl.getStateByName('OFF') ; assert(state <> nil) ;
    state.image := nil ; // Hintergrundsbild
    state := ctrl.getStateByName('START') ; assert(state <> nil) ;
    state.image := img_sw_start ;

    // ADDRESS_SELECT-Drehschalter
    //   Codierung der values     Codierung der images
    //     4      0                 7      0
    //       \   /                    \   /
    //   3 -. \ / .- 5            6 -. \ / .- 1
    //       `- -'                    `- -'
    //       .- -.                    .- -.
    //   7 -' / \ ^- 1            5 -' / \ ^- 2
    //       /   \                    /   \
    //      6     2                  4     3
    //
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_ADDRESS_SELECT, 0) ; assert(ctrl <> nil) ;
    state := ctrl.getStateByValue(0) ; assert(state <> nil) ;
    state.image := img_sw_addrselect_pos0 ;
    state := ctrl.getStateByValue(1) ; assert(state <> nil) ;
    state.image := img_sw_addrselect_pos2 ;
    state := ctrl.getStateByValue(2) ; assert(state <> nil) ;
    state.image := img_sw_addrselect_pos3 ;
    state := ctrl.getStateByValue(3) ; assert(state <> nil) ;
    state.image := img_sw_addrselect_pos6 ;
    state := ctrl.getStateByValue(4) ; assert(state <> nil) ;
    state.image := img_sw_addrselect_pos7 ;
    state := ctrl.getStateByValue(5) ; assert(state <> nil) ;
    state.image := img_sw_addrselect_pos1 ;
    state := ctrl.getStateByValue(6) ; assert(state <> nil) ;
    state.image := img_sw_addrselect_pos4 ;
    state := ctrl.getStateByValue(7) ; assert(state <> nil) ;
    state.image := img_sw_addrselect_pos5 ;

    // DATA_SELECT-Drehschalter
    // Hier: immer zwei Schalterstellungen für einen State:
    // "getStateByName" findet den ersten State, der nächste kommt 4 states später
    //   Codierung der indexe   Codierung der images
    //   (pdp1170panel.Create)
    //     3      0                 7      0
    //       \   /                    \   /
    //   2 -. \ / .- 1            6 -. \ / .- 1
    //       `- -'                    `- -'
    //       .- -.                    .- -.
    //   1 -' / \ ^- 2            5 -' / \ ^- 2
    //       /   \                    /   \
    //      0     3                  4     3
    //
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA_SELECT, 0) ; assert(ctrl <> nil) ;
    // reihenfolge muss mit values übereinstimmen!
    state := ctrl.States[0] ; assert(state.value = 0) ;
    state.image := img_sw_dataselect_pos0;
    state := ctrl.States[4] ; assert(state.value = 0) ;
    state.image := img_sw_dataselect_pos4;
    state := ctrl.States[1] ; assert(state.value = 1) ;
    state.image := img_sw_dataselect_pos1;
    state := ctrl.States[5] ; assert(state.value = 1) ;
    state.image := img_sw_dataselect_pos5;
    state := ctrl.States[2] ; assert(state.value = 3) ;
    state.image := img_sw_dataselect_pos2;
    state := ctrl.States[6] ; assert(state.value = 3) ;
    state.image := img_sw_dataselect_pos6;
    state := ctrl.States[3] ; assert(state.value = 2) ;
    state.image := img_sw_dataselect_pos3;
    state := ctrl.States[7] ; assert(state.value = 2) ;
    state.image := img_sw_dataselect_pos7;

    // die 22 Data-Schalter
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 0) ; assert(ctrl <> nil) ;
    state := ctrl.getStateByName('OFF') ; assert(state <> nil) ;
    state.image := nil ; // Hintergrundsbild
    state := ctrl.getStateByName('ON') ; assert(state <> nil) ;
    state.image := img_sw_00 ;

    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 1) ; assert(ctrl <> nil) ;
    state := ctrl.getStateByName('OFF') ; assert(state <> nil) ;
    state.image := nil ; // Hintergrundsbild
    state := ctrl.getStateByName('ON') ; assert(state <> nil) ;
    state.image := img_sw_01 ;

    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 2) ; assert(ctrl <> nil) ;
    state := ctrl.getStateByName('OFF') ; assert(state <> nil) ;
    state.image := nil ; // Hintergrundsbild
    state := ctrl.getStateByName('ON') ; assert(state <> nil) ;
    state.image := img_sw_02 ;

    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 3) ; assert(ctrl <> nil) ;
    state := ctrl.getStateByName('OFF') ; assert(state <> nil) ;
    state.image := nil ; // Hintergrundsbild
    state := ctrl.getStateByName('ON') ; assert(state <> nil) ;
    state.image := img_sw_03 ;

    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 4) ; assert(ctrl <> nil) ;
    state := ctrl.getStateByName('OFF') ; assert(state <> nil) ;
    state.image := nil ; // Hintergrundsbild
    state := ctrl.getStateByName('ON') ; assert(state <> nil) ;
    state.image := img_sw_04 ;

    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 5) ; assert(ctrl <> nil) ;
    state := ctrl.getStateByName('OFF') ; assert(state <> nil) ;
    state.image := nil ; // Hintergrundsbild
    state := ctrl.getStateByName('ON') ; assert(state <> nil) ;
    state.image := img_sw_05 ;

    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 6) ; assert(ctrl <> nil) ;
    state := ctrl.getStateByName('OFF') ; assert(state <> nil) ;
    state.image := nil ; // Hintergrundsbild
    state := ctrl.getStateByName('ON') ; assert(state <> nil) ;
    state.image := img_sw_06 ;

    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 7) ; assert(ctrl <> nil) ;
    state := ctrl.getStateByName('OFF') ; assert(state <> nil) ;
    state.image := nil ; // Hintergrundsbild
    state := ctrl.getStateByName('ON') ; assert(state <> nil) ;
    state.image := img_sw_07 ;

    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 8) ; assert(ctrl <> nil) ;
    state := ctrl.getStateByName('OFF') ; assert(state <> nil) ;
    state.image := nil ; // Hintergrundsbild
    state := ctrl.getStateByName('ON') ; assert(state <> nil) ;
    state.image := img_sw_08 ;

    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 9) ; assert(ctrl <> nil) ;
    state := ctrl.getStateByName('OFF') ; assert(state <> nil) ;
    state.image := nil ; // Hintergrundsbild
    state := ctrl.getStateByName('ON') ; assert(state <> nil) ;
    state.image := img_sw_09 ;

    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 10) ; assert(ctrl <> nil) ;
    state := ctrl.getStateByName('OFF') ; assert(state <> nil) ;
    state.image := nil ; // Hintergrundsbild
    state := ctrl.getStateByName('ON') ; assert(state <> nil) ;
    state.image := img_sw_10 ;

    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 11) ; assert(ctrl <> nil) ;
    state := ctrl.getStateByName('OFF') ; assert(state <> nil) ;
    state.image := nil ; // Hintergrundsbild
    state := ctrl.getStateByName('ON') ; assert(state <> nil) ;
    state.image := img_sw_11 ;

    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 12) ; assert(ctrl <> nil) ;
    state := ctrl.getStateByName('OFF') ; assert(state <> nil) ;
    state.image := nil ; // Hintergrundsbild
    state := ctrl.getStateByName('ON') ; assert(state <> nil) ;
    state.image := img_sw_12 ;

    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 13) ; assert(ctrl <> nil) ;
    state := ctrl.getStateByName('OFF') ; assert(state <> nil) ;
    state.image := nil ; // Hintergrundsbild
    state := ctrl.getStateByName('ON') ; assert(state <> nil) ;
    state.image := img_sw_13 ;

    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 14) ; assert(ctrl <> nil) ;
    state := ctrl.getStateByName('OFF') ; assert(state <> nil) ;
    state.image := nil ; // Hintergrundsbild
    state := ctrl.getStateByName('ON') ; assert(state <> nil) ;
    state.image := img_sw_14 ;

    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 15) ; assert(ctrl <> nil) ;
    state := ctrl.getStateByName('OFF') ; assert(state <> nil) ;
    state.image := nil ; // Hintergrundsbild
    state := ctrl.getStateByName('ON') ; assert(state <> nil) ;
    state.image := img_sw_15 ;

    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 16) ; assert(ctrl <> nil) ;
    state := ctrl.getStateByName('OFF') ; assert(state <> nil) ;
    state.image := nil ; // Hintergrundsbild
    state := ctrl.getStateByName('ON') ; assert(state <> nil) ;
    state.image := img_sw_16 ;

    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 17) ; assert(ctrl <> nil) ;
    state := ctrl.getStateByName('OFF') ; assert(state <> nil) ;
    state.image := nil ; // Hintergrundsbild
    state := ctrl.getStateByName('ON') ; assert(state <> nil) ;
    state.image := img_sw_17 ;

    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 18) ; assert(ctrl <> nil) ;
    state := ctrl.getStateByName('OFF') ; assert(state <> nil) ;
    state.image := nil ; // Hintergrundsbild
    state := ctrl.getStateByName('ON') ; assert(state <> nil) ;
    state.image := img_sw_18 ;

    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 19) ; assert(ctrl <> nil) ;
    state := ctrl.getStateByName('OFF') ; assert(state <> nil) ;
    state.image := nil ; // Hintergrundsbild
    state := ctrl.getStateByName('ON') ; assert(state <> nil) ;
    state.image := img_sw_19 ;

    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 20) ; assert(ctrl <> nil) ;
    state := ctrl.getStateByName('OFF') ; assert(state <> nil) ;
    state.image := nil ; // Hintergrundsbild
    state := ctrl.getStateByName('ON') ; assert(state <> nil) ;
    state.image := img_sw_20 ;

    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 21) ; assert(ctrl <> nil) ;
    state := ctrl.getStateByName('OFF') ; assert(state <> nil) ;
    state.image := nil ; // Hintergrundsbild
    state := ctrl.getStateByName('ON') ; assert(state <> nil) ;
    state.image := img_sw_21 ;

    // MouseEvents jedes Control-Image (Switch or Led) setzen
    // Hints setzen
    for i := 0 to pdp1170panel.ControlCount-1 do begin
      ctrl := pdp1170panel.Controls[i] ;
      for j := 0 to ctrl.StateCount-1 do
        if ctrl.States[j].image <> nil then begin

          ctrl.States[j].image.OnMouseDown := ImagesMouseDown ;
          ctrl.States[j].image.OnMouseUp := ImagesMouseUp ;

          ctrl.States[j].image.Hint := Format('%s "%s"', [ctrl.ctltype_str, ctrl.name]) ;
          ctrl.States[j].image.ShowHint := true ;
        end ;
    end ;
    img_panel_background.ShowHint := true ;

    assert(assigned(img_sw_addrselect_pos7.OnMouseDown)) ;
    assert(assigned(img_sw_dataselect_pos7.OnMouseDown)) ;
  end { "constructor Tpdp1170panelImplementorFrame.Create" } ;


// Welches Switch/Led-Image liegt an den Mauskoordinaten?
// 'ctrl' ist das zugehörige control zurückgegeben werden.
function Tpdp1170panelImplementorFrame.ControlImageAtPos(pos: TPoint; var ctrl: Tpdp1170PanelControl): TImage ;
  var
    i, j: integer ;
    image: TImage ;
  begin
    result := nil ; // != NULL, wenn control ein TImage ist.
    // alle Switch Images durchgehen
    for i := 0 to pdp1170panel.ControlCount-1 do begin
      ctrl := pdp1170panel.Controls[i] ;
      for j := 0 to ctrl.StateCount-1 do
        if ctrl.States[j].image <> nil then begin
          image := ctrl.States[j].image ;
          if (image.Left <= pos.x)
                  and (image.Left + image.Width > pos.x)
                  and (image.Top <= pos.y)
                  and (image.Top + image.Height > pos.y) then begin
            result := image ; Exit ;
          end;
        end;
    end{ "for i" } ;
    ctrl := nil ;
  end { "function Tpdp1170panelImplementorFrame.ControlImageAtPos" } ;


// gleicht das pdp1170panel-Object mit dem Frame ab.
// schreibt die gelesenen neuen Control-States nach new_active_state.
procedure Tpdp1170panelImplementorFrame.SyncLedsFromBasePanel ;
  begin
    // nur LED-Images updaten. die Switches sind schon aktuell, wegen der Events
    UpdateImages ;
  end;


procedure Tpdp1170panelImplementorFrame.SyncSwitchesToBasePanel(Sender: TObject) ;
  begin
    // no-op - Switches werden schon bei OnMouse-Events gesetzt
  end;


// setzt den Hint des Backgroudn-images, damit er den Controls entspricht,.
// die ohne actives images an der Mausposition liegen.
procedure Tpdp1170panelImplementorFrame.img_panel_backgroundMouseMove(Sender: TObject;
        Shift: TShiftState; x, y: integer);
  var
    ctrl: Tpdp1170PanelControl ;
    pos: TPoint  ;
    cur_hint: string ;
  begin
    pos.x := x ; pos.y := y ;
    // finde control zur mouseposition.
    pos := self.ScreenToClient(img_panel_background.ClientToScreen(pos)) ;
    ControlImageAtPos(pos, ctrl) ;

    if ctrl = nil then // kein ctrl an dieser Stelle: kein Hint
      cur_hint := ''
    else cur_hint := Format('%s "%s"', [ctrl.ctltype_str, ctrl.name]) ;

    // wenn sich der Hint ändert: refresh des Hint fensters.
    if img_panel_background.Hint <> cur_hint then begin
      Application.CancelHint ;
      img_panel_background.Hint := cur_hint ;
    end;

    //formmain.logmemo.Lines.Add(Format('background mouse %d/%d', [x, y])) ;
  end{ "procedure Tpdp1170panelImplementorFrame.img_panel_backgroundMouseMove" } ;


procedure Tpdp1170panelImplementorFrame.ImagesMouseDown(Sender: TObject;
        Button: TMouseButton; Shift: TShiftState; x, y: integer);
  var
    image: TImage ;
    ctrl: Tpdp1170PanelControl ;
    pos: TPoint  ;
  begin
    ctrl := nil ;
    image := nil ;

    if Sender = img_panel_background then begin
      pos.x := x ; pos.y := y ;

      // Click auf Hintergrund: welches Image könnte am Clickpunkt liegen?
      // wenn eins gefunden: Das PanelControl ist im Zustand 0 und zeigt kein Image an
      pos := self.ScreenToClient(img_panel_background.ClientToScreen(pos)) ;
      image := ControlImageAtPos(pos, ctrl) ;
      // das ctrl ist jetzt auch klar
    end else if Sender is TImage then begin
      image := Sender as TImage ;
      // Finde control zum image
      ctrl := pdp1170panel.getControlByImage(image) ;
    end;
    if ctrl <> nil then
      case ctrl.ctltype of
        _Led: begin
          // Sonderfunktion: Anklicken der Led ändert ihren Zustand,
          // wird erst von pdp1170panel an die Application signalisiert
          ctrl.implementor_active_state_index := (ctrl.getActiveState.index + 1) mod ctrl.StateCount;
          UpdateImages ;
        end ;
        _Switch: begin
          // Maus ist down: Drehswitch auf die nächste Position
          // Bidi-Switches flippen
          // Taster auf "activ"
          if (ctrl.flags and PDP1170PANEL_FLAG_SWITCH_LAMPTEST) <> 0 then
            pdp1170panel.lamptest_active := true ;

          // Switch auf den zyklisch nächsten value setzen
          // Dabei nicht an "values" orientieren, sodnenr an
          // der Defintionsreihenfolge
          // Bsp "11/70 rotationsknöpfe": codewerte sind nicht sequntiell,
          // wenn man den Schalter sequentuiell dreht!
          // wird erst von pdp1170panel an die Application signalisiert

          ctrl.implementor_active_state_index  := (ctrl.getActiveState.index + 1) mod ctrl.StateCount ;
          UpdateImages ;
        end { "case ctrl.ctltype of _Switch:" } ;
      end{ "case ctrl.ctltype" } ;
  end{ "procedure Tpdp1170panelImplementorFrame.ImagesMouseDown" } ;


procedure Tpdp1170panelImplementorFrame.ImagesMouseUp(Sender: TObject;
        Button: TMouseButton; Shift: TShiftState; x, y: integer);
  var
    ctrl: Tpdp1170PanelControl ;
    i: integer ;
  begin
    // alle Switches loslassen
    for i := 0 to pdp1170panel.ControlCount-1 do begin
      ctrl := pdp1170panel.Controls[i] ;
      if (ctrl.ctltype = _Switch)
              and (ctrl.active_state_index <> 0)
              and ((ctrl.flags and PDP1170PANEL_FLAG_SWITCH_ACTION) <> 0) then begin
        // "Action"-Switch loslassen
        // wird erst von pdp1170panel an die Application signalisiert
        ctrl.implementor_active_state_index := 0;
        if (ctrl.flags and PDP1170PANEL_FLAG_SWITCH_LAMPTEST) <> 0 then
          pdp1170panel.lamptest_active := false ;
        UpdateImages ;
      end ;
    end { "for i" } ;
  end { "procedure Tpdp1170panelImplementorFrame.ImagesMouseUp" } ;



// alle Images so anzeigen, wie es der State der panelcontrols erfordert
procedure Tpdp1170panelImplementorFrame.UpdateImages ;
  var
    ctrl: Tpdp1170PanelControl ;
    image: TImage ;
    i, j: integer ;
  begin
    // Alle LEDs, alle Switches
    for i := 0 to pdp1170panel.ControlCount-1 do begin
      ctrl := pdp1170panel.Controls[i] ;
      for j := 0 to ctrl.StateCount-1 do begin
        image := ctrl.States[j].image ;
        if image <> nil then
          // Sonderfunktion: Lamptest für LEDs: alle images zeigen
          if (ctrl.ctltype = _Led) and pdp1170panel.lamptest_active and (image <> nil) then begin
            image.Visible := true ;
            image.BringToFront ;
          end else if ctrl.implementor_active_state_index = j then begin
            // Image für diesen State anzeigen
            image.Visible := true ;
            image.BringToFront ;
          end else // images für die nichtactiven States ausblenden
            image.Visible := false ;
      end { "for j" } ;
    end { "for i" } ;
  end{ "procedure Tpdp1170panelImplementorFrame.UpdateImages" } ;



end{ "unit pdp1170panelImplementorFrameU" } .
