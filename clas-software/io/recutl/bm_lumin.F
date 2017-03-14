      SUBROUTINE BM_LUMIN(Tg_mol_Mass, Tg_Thick, Tg_Dens, r_Lumin)
c
c_begin_doc
c  RCS ID string
c  $Id: bm_lumin.F,v 1.2 1997/10/23 21:54:35 hovanes Exp $
c
c  Documentation for subroutine BM_LUMIN
c
c  Purpose:  Calculate current Luminisity in 1/( cm^2 * sec )
c  --------  for target with given Molar mass , thicknes and
c            density ( units are gramms and cm )         
c
c  Input Parameters: Tg_mol_Mass, Tg_Thick, Tg_Dens
c  ----------------
c
c  Output Parameters: r_Lumin
c  -----------------
c
c  Called from:
c  ------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Hovanes Egiyan      Created:  Thu Oct 16 14:01:23 EST 1997
c  -------
c
c  Major revisions:
c  ----------------
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID 
      PARAMETER ( CFILE = '$RCSfile: bm_lumin.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.2 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1997/10/23 21:54:35 $')
      PARAMETER ( CAUTHO = '$Author: hovanes $')
      DATA CRCSID/
     1'$Id: bm_lumin.F,v 1.2 1997/10/23 21:54:35 hovanes Exp $
     3'/
c  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      PARAMETER (CRNAME='BM_LUMIN')
      PARAMETER (CRAUTH='Hovanes Egiyan')
c
c  Local User defined variables:
c  -----------------------------
C-
C          Molar Mass    Thicknes  Density
      real Tg_mol_Mass, Tg_Thick,  Tg_Dens, r_Lumin 
C-
      real ELEC_CHARGE
      parameter ( ELEC_CHARGE = 1.60217733E-19 )
C-
      real N_Avgd
      parameter ( N_Avgd = 6.02213674E+23 )      
C-
      character*100 CRMESS      
C-
      real Q_TOT, CURRENT, TL_TU 
C-
c_end_var
c
c  executable code for routine BM_LUMIN:
c  -------------------------------------
c
C-
        if ( Abs( Tg_mol_Mass ) .LT. 0.2 ) then
	 CRMESS = ' Too small value for molar mass: Skipping'
	 call RECMES( CRNAME, 'W', CRMESS )
	 r_Lumin = 0.
	 return
        endif
C-
	call bm_get_trig( Q_TOT, CURRENT, TL_TU, 0 )
C-	
	r_Lumin = Abs ( CURRENT / ELEC_CHARGE ) * 
     *        Tg_Dens * Tg_Thick / ( Tg_mol_Mass / N_Avgd )	
C-
       RETURN
       END
c
c----------------------------------------------------------------------
