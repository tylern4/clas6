      SUBROUTINE DC_TCL_INIT
c
c_begin_doc
c  RCS ID string
c  $Id: dc_tcl_init.F,v 1.16 2001/08/28 19:50:19 feuerbac Exp $
c
c  Documentation for subroutine DC_TCL_INIT
c
c  Purpose:
c  --------
c
c  Define variables that have to be accessible to the user via the
c  TCL interface.
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c   None
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author: Gilfoyle     Created:  Fri Nov 15 09:48:36 EST 1996
c  -------
c
c  Major revisions:  Nov.1997  F.Klein : some more tracking related tcl params.
c  ----------------  Jan.1998  J.Manak : moved defaults into routine dc_set_def.F
c     
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
#include "dc_tcl.inc"
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
      PARAMETER (CFILE = '$RCSfile: dc_tcl_init.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.16 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2001/08/28 19:50:19 $')
      PARAMETER (CAUTHO = '$Author: feuerbac $')
      DATA CRCSID/   
     1'$Id: dc_tcl_init.F,v 1.16 2001/08/28 19:50:19 feuerbac Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='DC_TCL_INIT')
      PARAMETER (CRAUTH=' ')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER ITCL_INT, ITCL_DOUBLE, ITCL_STRING, ITCL_INT_READ
      DATA ITCL_INT, ITCL_DOUBLE, ITCL_STRING, ITCL_INT_READ/1,2,4,129/
      CHARACTER*32  tstring
      INTEGER  j, i
c_end_var
c
c  executable code for routine DC_TCL_INIT:
c----6----------------------------------------------------------------72
c
c set tcl defaults
      call dc_set_def()

      do j=1,6
        write(tstring,'(A,I1,A)') 'dc_Sigma_doca(',j,')'
        call tcl_linkvar(tstring(1:16),dc_Sigma_doca(j),itcl_double)
      enddo

      call tcl_linkvar('dc_efficiency',dc_efficiency,itcl_double)

c adjustment of time delays for common stop [nsec]  (i.e. betw.DC & SC/Trig)

      write(tstring,'(A)') 'dc_trig_time'
      call tcl_linkvar(tstring(1:12),dc_trig_time,itcl_double)

c adjustment of pulser delays for three regions [nsec]  
c   "        of propagation velocity along the wire

      do i=1,3
        write(tstring,'(A,I1,A)') 'dc_reg_pulser_time(',i,')'
        call tcl_linkvar(tstring(1:21),dc_reg_pulser_time(i),itcl_double)
      enddo 
      call tcl_linkvar('dc_vel_prop',dc_vel_prop,itcl_double)

c choose a function for the T-->D and D-->T algorithms
      call tcl_linkvar('dc_xvst_choice',dc_xvst_choice,itcl_int)

c choose the slope if choice = 0
      call tcl_linkvar('dc_xvst_default_slope',dc_xvst_default_slope,itcl_double)

c choose the power of t/tmax if choice = 1 (curve used)
      call tcl_linkvar('dc_xvst_n_power',dc_xvst_n_power,itcl_double)
      call tcl_linkvar('dc_xvst_m_power',dc_xvst_m_power,itcl_double)

      do j=1,6
        write(tstring,'(A,I1,A)') 'dc_mult_Sigma(',j,')'
        call tcl_linkvar(tstring(1:16),dc_mult_Sigma(j),itcl_double)
      enddo

      RETURN
      END
c
c------------------------------------------------------------------------------




