// g1c2.4pcor package header file -*- C++ -*-
#ifndef _g1c2445pcor_H
#define _g1c2445pcor_H

// For more detailed descriptions of the functions, see g1c2445pcor.cc

extern "C" { // 'dumb it down' for c and fortran
//_____________________________________________________________________________
  // Tagger Correction:
  /* C/C++ Function:
   * Parameters: eg         Measured photon energy
   *             e_beam     Beam energy
   *             eid        E-paddle id in the tagger
   *             runNumber  CLAS run number
   *
   * Returns: Corrected photon energy
   */
  float g1c2445TaggerCor(float __eg,float __e_beam,int __eid,int __runNumber);

  /* FORTRAN Subroutine:
   * Parameters: eg         Measured photon energy
   *             e_beam     Beam energy
   *             eid        E-paddle id in the tagger
   *             runNumber  CLAS run number
   *             egCor      Corrected photon energy
   */
  void g1c2445TaggerCor_(float *__eg,float *__e_beam,int *__eid,int *__runNumber,
		     float *__egCor);
//_____________________________________________________________________________
  // Momentum Corrections:
  /* C/C++ Function:
   * Parameters: p3[3]    Measured 3-momentum [x,y,z]
   *             q        Charge (ex. -1 for pi-)
   *             p3cor[3] Corrected 3-momentum [x,y,z]
   */
  void g1c2445MomCor(float __p3[3],int __q,float __p3cor[3]);

  /* FORTRAN Subroutine:
   * Parameters: p3[3]    Measured 3-momentum [x,y,z]
   *             q        Charge (ex. -1 for pi-)
   *             p3cor[3] Corrected 3-momentum [x,y,z]
   */
  void g1c2445MomCor_(float __p3[3],int *__q,float __p3cor[3]);
//_____________________________________________________________________________
  // Fiducial Cuts:
  /* C/C++ Function:
   * Parameters: p3[3]  Measured 3-momentum [x,y,z]
   *             q      Charge (ex 1 for proton)
   *
   * Returns: true = good fiducial region, false otherwise
   */
  bool g1c2445FiducialCut(float __p3[3],int __q);

  /* FORTRAN Subroutine:
   * Parameters: p3[3]   Measured 3-momentum [x,y,z]
   *             q       Charge (ex 1 for pi+)
   *             cut     Set to 1 if good fiducial region, 0 otherwise
   */
  void g1c2445FiducialCut_(float __p3[3],int *__q,int *__cut);
//_____________________________________________________________________________
}

#endif /* _g1c2445pcor_H */
