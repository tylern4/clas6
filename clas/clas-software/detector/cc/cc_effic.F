      real function cc_effic(theta, phy)
c     ==============================
c
c_begin_doc
c  RCS ID string
c  $Id: cc_effic.F,v 1.2 1999/03/11 18:41:34 vlassov Exp $
c
c  Documentation for subroutine CC_EFFIC
c
c  Purpose:   returnes the mean Cerenkov detector efficiency as a 
c  --------   function of projective angles theta, phy (in degrees)
c
c
c
c  Author:   Alexander Vlassov    Created:  May 10 18:16:11 EDT 1998
c  -------
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: cc_effic.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1999/03/11 18:41:34 $')
      PARAMETER (CAUTHO = '$Author: vlassov $')
      DATA CRCSID/   
     1'$Id: cc_effic.F,v 1.2 1999/03/11 18:41:34 vlassov Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*200  CRMESS
      PARAMETER (CRNAME='CC_EFFIC')
      PARAMETER (CRAUTH='Alexander Vlassov')
c
c  Local User defined variables:
c  -----------------------------
       real*4 theta, phy, cc_nphe, s
c
       s = cc_nphe(theta,phy)
       cc_effic = 1. - exp(0.2828 - 0.8719*s)
       if(cc_effic.le.0.) cc_effic = 0.
c       
       return
       end






