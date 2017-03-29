#ifndef MCTK_HH
#define MCTK_HH

#include <TTree.h>

extern "C" {

  const int max_part = 20;
  
  struct mc_nt_
  {
    int mcnentr;
    int mcnpart;
    int mcst[max_part];
    int mcid[max_part];
    int mcpid[max_part];
    float mctheta[max_part];
    float mcphi[max_part];
    float mcp[max_part];
    float mcm[max_part];
    //These are the vertex fields, and for some reason nt10maker_part
    //doesn't use the existing MVRT code and instead adds more fields
    //to the MC_NT common.  So as not to break everyone else's code,
    //I'll do the same here, but not without grumbling about it.
    float mcvx_x_el;
    float mcvx_y_el;
    float mcvx_z_el;
  };

  extern struct mc_nt_ mc_nt_;
  
  extern void fill_mc_nt_();
  
}

void mctk_branches(TTree* tree);

#endif
