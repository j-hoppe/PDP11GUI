program PDP11GUI;



uses
  Forms,
  FormMainU in 'FormMainU.pas' {FormMain},
  FormSettingsU in 'FormSettingsU.pas' {FormSettings},
  ConsolePDP1144U in 'ConsolePDP1144U.pas',
  FormTerminalU in 'FormTerminalU.pas' {FormTerminal},
  FormMacro11SourceU in 'FormMacro11SourceU.pas' {FormMacro11Source},
  FormMacro11ListingU in 'FormMacro11ListingU.pas' {FormMacro11Listing},
  FormLogU in 'FormLogU.pas' {FormLog},
  MemoryCellU in 'MemoryCellU.pas',
  FormMemoryListU in 'FormMemoryListU.pas' {FormMemoryList},
  RegistryU in 'RegistryU.pas',
  AuxU in 'AuxU.pas',
  SerialIoHubU in 'SerialIoHubU.pas',
  FormMemoryTableU in 'FormMemoryTableU.pas' {FormMemoryTable},
  FormBitfieldsU in 'FormBitfieldsU.pas' {FormBitfields},
  BitFieldU in 'BitFieldU.pas',
  ConsoleGenericU in 'ConsoleGenericU.pas',
  ConsolePDP11SimHU in 'ConsolePDP11SimHU.pas',
  FormExecuteU in 'FormExecuteU.pas' {FormExecute},
  Pdp11MmuU in 'Pdp11MmuU.pas',
  OctalConst in 'OctalConst.pas',
  FormMmuU in 'FormMmuU.pas' {FormMMU},
  Pdp1144MicroCodeU in 'Pdp1144MicroCodeU.pas',
  FormMicroCodeU in 'FormMicroCodeU.pas' {FormMicroCode},
  FormDisasU in 'FormDisasU.pas' {FormDisas},
  FormAboutU in 'FormAboutU.pas' {FormAbout},
  ConsolePDP1144FakeU in 'ConsolePDP1144FakeU.pas',
  FrameMemoryCellGroupGridU in 'FrameMemoryCellGroupGridU.pas' {FrameMemoryCellGroupGrid: TFrame},
  FormMacro11CodeU in 'FormMacro11CodeU.pas' {FormMacro11Code},
  FormMemoryLoaderU in 'FormMemoryLoaderU.pas' {FormMemoryLoader},
  MemoryLoaderU in 'MemoryLoaderU.pas',
  FakePDP1144U in 'FakePDP1144U.pas',
  FakePDP11ODTU in 'FakePDP11ODTU.pas',
  FakePDP11GenericU in 'FakePDP11GenericU.pas',
  ConsolePDP11ODTU in 'ConsolePDP11ODTU.pas',
  ConsolePDP11ODTFakeU in 'ConsolePDP11ODTFakeU.pas',
  AddressU in 'AddressU.pas',
  FrameMemoryCellGroupListU in 'FrameMemoryCellGroupListU.pas' {FrameMemoryCellGroupList: TFrame},
  FormIoPageScannerU in 'FormIoPageScannerU.pas' {FormIopageScanner},
  FormPdp1170PanelU in 'FormPdp1170PanelU.pas' {Formpdp1170Panel},
  pdp1170panelImplementorFrameU in '..\PDP1170Panel\delphi\pdp1170panelImplementorFrameU.pas' {pdp1170panelImplementorFrame: TFrame},
  iowkit in '..\PDP1170Panel\delphi\iowkit.pas',
  pdp1170panelImplementorPhysicalU in '..\PDP1170Panel\delphi\pdp1170panelImplementorPhysicalU.pas',
  pdp1170panelU in '..\PDP1170Panel\delphi\pdp1170panelU.pas',
  CommU in 'CommU.pas',
  FormNoConsolePromptU in 'FormNoConsolePromptU.pas' {FormNoConsolePrompt},
  SerialXferU in 'SerialXferU.pas',
  FormMemoryDumperU in 'FormMemoryDumperU.pas' {FormMemoryDumper},
  FormDiscImageU in 'FormDiscImageU.pas' {FormDiscImage},
  FormChildU in 'FormChildU.pas',
  DiscImageBadBlockU in 'DiscImageBadBlockU.pas',
  FakePDP11M9312U in 'FakePDP11M9312U.pas',
  ConsolePDP11M9312FakeU in 'ConsolePDP11M9312FakeU.pas',
  ConsolePDP11M9312U in 'ConsolePDP11M9312U.pas',
  FormExecuteBlinkenlightU in 'FormExecuteBlinkenlightU.pas' {FormExecuteBlinkenlight},
  BlinkenlightInstructionsU in 'BlinkenlightInstructionsU.pas',
  FormMemoryTestU in 'FormMemoryTestU.pas' {FormMemoryTest},
  FormDiscImageExitQueryU in 'FormDiscImageExitQueryU.pas' {FormDiscImageExitQueryForm},
  MediaImageDevicesU in 'MediaImageDevicesU.pas',
  MediaImageBufferU in 'MediaImageBufferU.pas',
  FormNumberconverterU in 'FormNumberconverterU.pas' {FormNumberConverter},
  FakePDP11M9301U in 'FakePDP11M9301U.pas',
  ConsolePDP11M9301U in 'ConsolePDP11M9301U.pas',
  ConsolePDP11M9301FakeU in 'ConsolePDP11M9301FakeU.pas',
  ConsolePDP1144v340cFakeU in 'ConsolePDP1144v340cFakeU.pas',
  FakePDP1144v340cU in 'FakePDP1144v340cU.pas',
  FormBusyU in 'FormBusyU.pas' {BusyForm},
  ConsolePDP1144v340cU in 'ConsolePDP1144v340cU.pas',
  ConsolePDP11ODTK1630U in 'ConsolePDP11ODTK1630U.pas',
  ConsolePDP11ODTK1630FakeU in 'ConsolePDP11ODTK1630FakeU.pas',
  FakePDP11ODTK1630U in 'FakePDP11ODTK1630U.pas';

{$R *.res}

begin
  Randomize ;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormSettings, FormSettings);
  Application.CreateForm(TFormNoConsolePrompt, FormNoConsolePrompt);
  Application.CreateForm(TFormDiscImageExitQueryForm, FormDiscImageExitQueryForm);
  Application.CreateForm(TBusyForm, BusyForm);
  Application.Run;
end.
