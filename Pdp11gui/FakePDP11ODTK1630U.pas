unit FakePDP11ODTK1630U; 

// Simulierte K1630. Wie ODT18BBit, some flags are dofferent

interface 

uses 
  SysUtils, 
  ConsoleGenericU, 
  FakePDP11ODTU ; 

type 
  TFakePDP11ODTK1630 = class(TFakePDP11ODT) 
      constructor Create ; 
    end ; 


implementation 

uses 
  AddressU ; 

constructor TFakePDP11ODTK1630.Create ; 
  begin 
    inherited Create(matPhysical18) ; 
    isK1630 := true ; 
  end ; 


end{ "unit FakePDP11ODTK1630U" } . 
