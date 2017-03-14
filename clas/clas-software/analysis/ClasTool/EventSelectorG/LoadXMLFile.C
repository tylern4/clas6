{
  gSystem->Load("libGui.so");
  gSystem->Load("libTree.so");
  gSystem->Load("libXMLParser.so");
  gSystem->Load("libXMLIO.so");
  
  gSystem->Load("libClasBanks.so");
  gSystem->Load("libVirtualReader.so");
  gSystem->Load("libDSTReader.so");
  gSystem->Load("libCTEventSelectorG.so");

  TCTContainer fCont;

  fCont.LoadXMLConfig("ParticleConfiguration.xml");

  fCont.Print("PCVD");
}
