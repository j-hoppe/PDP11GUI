
unit ConsolePDP11M9301FakeU; 

// Console-Adpater für den fake.
// wie ConsolePDP11M9301.

interface 

uses 
  ConsoleGenericU, 
  ConsolePDP11M9301U, 
  FormTerminalU ; 

type 
  TConsolePDP11M9301Fake = class(TConsolePDP11M9301) 
      function getName: string ; override ; 
    end ; 


implementation 

function TConsolePDP11M9301Fake.getName: string ; 
  begin 
    result := 'PDP-11 M9301 fake console' ; 
  end; 


end{ "unit ConsolePDP11M9301FakeU" } .