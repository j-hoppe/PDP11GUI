unit ConsolePDP1144v340cU;

// Console-Adpater für den fake.
// wie ConsolePDP1144, aber nur Examine und Deposit sind implementiert

interface

uses
 MemoryCellU,
  ConsoleGenericU,
  ConsolePDP1144U ;

type
  TConsolePDP1144v340c = class(TConsolePDP1144)
  public
      function getName: string ; override ;
      constructor Create(memorycellgroups: TMemoryCellGroups) ;
    end ;


implementation

function TConsolePDP1144v340c.getName: string ;
  begin
    result := 'PDP-11/44 v3.40c console' ;
  end;


constructor TConsolePDP1144v340c.Create(memorycellgroups: TMemoryCellGroups) ;
begin
  inherited Create(memorycellgroups) ;
  isV340c := true ;
end;


end{ "unit ConsolePDP1144FakeU" } .
