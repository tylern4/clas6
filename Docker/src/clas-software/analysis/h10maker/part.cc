#include "part.hh"

struct partnt_ partnt_;

void part_branches(TTree* tree)
{
  tree->Branch("nprt",&partnt_.nprt,"nprt/I");
  tree->Branch("pidpart",&partnt_.pidpart,"pidpart[nprt]/I");
  tree->Branch("xpart",&partnt_.xpart,"xpart[nprt]/F");
  tree->Branch("ypart",&partnt_.ypart,"ypart[nprt]/F");
  tree->Branch("zpart",&partnt_.zpart,"zpart[nprt]/F");
  tree->Branch("epart",&partnt_.epart,"epart[nprt]/F");
  tree->Branch("pxpart",&partnt_.pxpart,"pxpart[nprt]/F");
  tree->Branch("pypart",&partnt_.pypart,"pypart[nprt]/F");
  tree->Branch("pzpart",&partnt_.pzpart,"pzpart[nprt]/F");
  tree->Branch("qpart",&partnt_.qpart,"qpart[nprt]/F");
  tree->Branch("flagspart",&partnt_.flagspart,"flagspart[nprt]/I");
  return;
}
