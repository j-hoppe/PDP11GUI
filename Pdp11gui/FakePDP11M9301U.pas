unit FakePDP11M9301U; 
{
  Simuliert eine rudimentäre PDP-11 mit M9301 console emulator.
  Abgeleitet von M9312
  }

interface 

uses 
  FakePDP11M9312U ; 

type 
  TFakePDP11M9301 = class(TFakePDP11M9312) 
    public 
      constructor Create ; 
    end; 



implementation 

{ FakePDP11M9301 }

constructor TFakePDP11M9301.Create; 
  begin 
    inherited ; 
    // like M9312, but other prompt.
    // The ROMs in MP301-YA add a NUL, M9301-YF does not.
    // With NUL it is more difficult to parse!
    Prompt := '$' + #0 ;
  end; 

end{ "unit FakePDP11M9301U" } . 
