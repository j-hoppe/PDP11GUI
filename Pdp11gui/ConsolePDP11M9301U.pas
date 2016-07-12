unit ConsolePDP11M9301U; 
{
  Steuert über RS232 den console emulator auf dem
  M9301 boot terminator an. Wird zB in PDP-11/34 benutzt.
  Die Anwendung ruft nur Deposit(), Examine(), Start auf

  Identisch mit M9312, aber prompt '$' statt '@'
  }


interface 

uses 
  MemoryCellU, 
  AddressU, 
  ConsolePDP11M9312U ; 

type 
  TConsolePDP11M9301 = class(TConsolePDP11M9312) 
    public 
      constructor Create(memorycellgroups: TMemoryCellGroups; 
              aMonitor_entryaddress: TMemoryAddress) ; 
    end; 

implementation 

{ TConsolePDP11M9301 }

constructor TConsolePDP11M9301.Create(memorycellgroups: TMemoryCellGroups; 
        aMonitor_entryaddress: TMemoryAddress) ; 

  begin 
    inherited Create(memorycellgroups, aMonitor_entryaddress) ; 
    Prompt := '$' ; // M9312 default is '@'
  end; 

end{ "unit ConsolePDP11M9301U" } . 
