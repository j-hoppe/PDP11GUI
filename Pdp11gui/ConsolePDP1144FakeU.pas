unit ConsolePDP1144FakeU; 

// Console-Adpater für den fake.
// wie ConsolePDP1144, aber nur Examine und Deposit sind implementiert

interface 

uses 
  ConsoleGenericU, 
  ConsolePDP1144U ; 

type 
  TConsolePDP1144Fake = class(TConsolePDP1144) 
      function getName: string ; override ; 
      function getFeatures: TConsoleFeatureSet ; override ; 
    end ; 


implementation 

function TConsolePDP1144Fake.getName: string ; 
  begin 
    result := 'PDP-11/44 fake console' ; 
  end; 


function TConsolePDP1144Fake.getFeatures: TConsoleFeatureSet ;
  begin
    result := [
    cfNonFatalUNIBUStimeout,
                cfActionResetMachine, // reset unabhängig von run: INITIALIZE
    cfActionResetMaschineAndStartCpu] ; // kann auch Run!
  end; 

end{ "unit ConsolePDP1144FakeU" } . 
