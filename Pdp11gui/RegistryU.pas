unit RegistryU; 


interface 

uses JH_Utilities ; 

var 
  TheRegistry : TJH_Registry ; 

implementation 


initialization 
  TheRegistry := TJH_Registry.Create ; 
  TheRegistry.Key := '\Software\Joerg Hoppe\PDP11GUI\' 

end{ "unit RegistryU" } . 
