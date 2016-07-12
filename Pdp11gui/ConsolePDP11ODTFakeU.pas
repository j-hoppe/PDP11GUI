unit ConsolePDP11ODTFakeU; 

// Console-Adpater für den fake.
// wie ConsolePDP11ODT, aber nur Examine und Deposit sind implementiert

interface 

uses 
  SysUtils, 
  ConsoleGenericU, 
  ConsolePDP11ODTU ; 

type 
  TConsolePDP11ODTFake = class(TConsolePDP11ODT) 
      function getName: string ; override ; 
      function getFeatures: TConsoleFeatureSet ; override ; 
    end ; 


implementation 

uses 
  AddressU ; 

function TConsolePDP11ODTFake.getName: string ; 
  begin 
    result := '??? ' ; 
    case getPhysicalMemoryAddressType of 
      matPhysical16:
        result := 'PDP-11 ODT 16 bit fake console' ;
      matPhysical18:
        result := 'PDP-11 ODT 18 bit fake console' ; 
      matPhysical22: 
        result := 'PDP-11 ODT 22 bit fake console' ; 
    end; 
  end; 


function TConsolePDP11ODTFake.getFeatures: TConsoleFeatureSet ; 
  begin
   result := inherited getFeatures ;
  end;

end{ "unit ConsolePDP11ODTFakeU" } . 
