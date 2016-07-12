unit ConsolePDP11ODTK1630U; 
{
  Wrapper for K1630 CPU of Robotron A6402 clone.
  Contct: ruediger.kurth@freenet.de, rechenwerk.de, Halle, 2016

  Is a 18bit 11/23 with slightly different console syntax:
  some extra space separators, and absolute addresses need an "A" postfix
  }


interface 

uses 
  MemoryCellU, 
  AddressU, 
  ConsolePDP11ODTU ; 

type 
  TConsolePDP11ODTK1630 = class(TConsolePDP11ODT) 
    public 
      constructor Create(memorycellgroups: TMemoryCellGroups) ; 
      function getName: string ; override ; 
    end; 

implementation 

{ TConsolePDP11K1630 }

constructor TConsolePDP11ODTK1630.Create(memorycellgroups: TMemoryCellGroups); 
  begin 
    inherited Create(memorycellgroups, matPhysical18) ; 
    GobbleExtraSpaceAfterPrompt := true ; 
    isK1630 := true ; 
  end; 

function TConsolePDP11ODTK1630.getName: string ; 
  begin 
    result := 'Robotron CPU K1630 (ODT 18 bit 11/23)' ; 
  end; 

end{ "unit ConsolePDP11ODTK1630U" } . 
