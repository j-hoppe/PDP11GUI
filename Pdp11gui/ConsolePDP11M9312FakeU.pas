unit ConsolePDP11M9312FakeU; 

// Console-Adpater für den fake.
// wie ConsolePDP11M9312.
// Noch mehr kann man ja nicht weglassen!

interface 

uses 
  ConsoleGenericU, 
  ConsolePDP11M9312U, 
  FormTerminalU ; 

type 
  TConsolePDP11M9312Fake = class(TConsolePDP11M9312) 
      function getName: string ; override ; 
    end ; 


implementation 

function TConsolePDP11M9312Fake.getName: string ; 
  begin 
    result := 'PDP-11 M9312 fake console' ; 
  end; 


end{ "unit ConsolePDP11M9312FakeU" } . 
