#include<pdgutil.h>

/* convert geant id's into mass, width, etc...
   why re-invent the wheel? */

float gmass(int geant_id){
  int pdg_id = geant_to_pdg_(&geant_id);
  return(pmass_(&pdg_id));
}

int gchrg(int geant_id){
  int pdg_id = geant_to_pdg_(&geant_id);
  return(pchrg_(&pdg_id));
}

float gtlif(int geant_id){
  int pdg_id = geant_to_pdg_(&geant_id);
  return(ptlif_(&pdg_id));
}

float gwidth(int geant_id){
  int pdg_id = geant_to_pdg_(&geant_id);
  return(pwidth_(&pdg_id));
}
