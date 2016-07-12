unit ConsolePDP1144v340cFakeU;

// Console-Adpater für den fake.
// wie ConsolePDP1144, aber nur Examine und Deposit sind implementiert

interface

uses
  ConsoleGenericU,
  ConsolePDP1144v340cU ;

type
  TConsolePDP1144v340cFake = class(TConsolePDP1144v340c)
      function getName: string ; override ;
      function getFeatures: TConsoleFeatureSet ; override ;
    end ;


implementation

function TConsolePDP1144v340cFake.getName: string ;
  begin
    result := 'PDP-11/44 v3.40c fake console' ;
  end;


function TConsolePDP1144v340cFake.getFeatures: TConsoleFeatureSet ;
  begin
    result := [
    cfNonFatalUNIBUStimeout,
                cfActionResetMachine, // reset unabhängig von run: INITIALIZE
    cfActionResetMaschineAndStartCpu] ; // kann auch Run!
  end;

end{ "unit ConsolePDP1144FakeU" } .
