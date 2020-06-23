#include "mctk.hh"

struct mc_nt_ mc_nt_;

void mctk_branches(TTree* tree)
{
  tree->Branch("mcnentr",&mc_nt_.mcnentr,"mcnentr/I");
  tree->Branch("mcnpart",&mc_nt_.mcnpart,"mcnpart/I");
  tree->Branch("mcst",&mc_nt_.mcst,"mcst[mcnentr]/I");
  tree->Branch("mcid",&mc_nt_.mcid,"mcid[mcnentr]/I");
  tree->Branch("mcpid",&mc_nt_.mcpid,"mcpid[mcnentr]/I");
  tree->Branch("mctheta",&mc_nt_.mctheta,"mctheta[mcnentr]/F");
  tree->Branch("mcphi",&mc_nt_.mcphi,"mcphi[mcnentr]/F");
  tree->Branch("mcp",&mc_nt_.mcp,"mcp[mcnentr]/F");
  tree->Branch("mcm",&mc_nt_.mcm,"mcm[mcnentr]/F");
  //See my comment in the header for why this shouldn't have been here
  //in the first place, but has to be because everyone's code
  //potentially uses this now.
  tree->Branch("mcvx_x_el",&mc_nt_.mcvx_x_el,"mcvx_x_el/F");
  tree->Branch("mcvx_y_el",&mc_nt_.mcvx_y_el,"mcvx_y_el/F");
  tree->Branch("mcvx_z_el",&mc_nt_.mcvx_z_el,"mcvx_z_el/F");
}
