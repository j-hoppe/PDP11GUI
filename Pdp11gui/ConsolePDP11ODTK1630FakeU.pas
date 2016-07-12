unit ConsolePDP11ODTK1630FakeU; 

// Console-Adpater für den fake.
// wie ConsolePDP11ODT, aber nur Examine und Deposit sind implementiert

interface 

uses 
  SysUtils, 
  ConsoleGenericU, 
  ConsolePDP11ODTK1630U ; 

type 
  TConsolePDP11ODTK1630Fake = class(TConsolePDP11ODTK1630) 
      function getName: string ; override ; 
      function getFeatures: TConsoleFeatureSet ; override ; 
    end ; 


implementation 

function TConsolePDP11ODTK1630Fake.getName: string ; 
  begin 
    result := 'Robotron CPU K1630 (ODT 18 bit 11/23) fake console' ; 
  end; 


function TConsolePDP11ODTK1630Fake.getFeatures: TConsoleFeatureSet ; 
  begin 
    result := inherited getFeatures ; 
  end; 

end{ "unit ConsolePDP11ODTK1630FakeU" } . 
