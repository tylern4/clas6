#include "mvrt.hh"

struct mvrtnt_ mvrtnt_;

void mvrt_branches(TTree* tree)
{
  tree->Branch("ntrk_mvrt",&mvrtnt_.ntrk_mvrt,"ntrk_mvrt/I");
  tree->Branch("x_mvrt",&mvrtnt_.x_mvrt,"x_mvrt/F");
  tree->Branch("y_mvrt",&mvrtnt_.y_mvrt,"y_mvrt/F");
  tree->Branch("z_mvrt",&mvrtnt_.z_mvrt,"z_mvrt/F");
}
