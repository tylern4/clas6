      SUBROUTINE DASPE_TAB(Ierr)
c
c_begin_doc
c  RCS ID string
c  $Id$
c
c  Documentation for subroutine DASPE_TAB
c
c  Purpose:
c  --------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c
c  Other routines:
c  -------------
c
c  Notes:
c  ------
c
c  Author:  S.Stepanyan   Created:  Wed Jun 23 17:16:39 EST 1999
c  -------
c
c  Major revisions:
c  ----------------
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
c BOS common block  uncomment the next line for BOS include file
#include "cross_sec.inc"
#include "daspetab.inc"
#include "fit_par.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      Integer Ierr
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile$')
      PARAMETER (CREVIS = '$Revision$')
      PARAMETER (CSTATE = '$State$')
      PARAMETER (CDATE = '$Date$')
      PARAMETER (CAUTHO = '$Author$')
      DATA CRCSID/   
     1'$Id$'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      PARAMETER (CRNAME='DASPE_TAB')
      PARAMETER (CRAUTH='Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
c
      INTEGER iw,iq2,icos,iphi,i
      INTEGER Nw,Nq2,Ncos,Nphi
      REAL Q2, W, CosTh, Phi, Epsilon, nu, ep, mp,theta
      DATA mp/0.938/
c_end_var
c
c  executable code for routine DASPE_RUN:
c----6----------------------------------------------------------------72
c Get the number of data points in the table
      Ierr = 0
         cM=start_value(1)
         cE=start_value(2)
         cS=start_value(3)
         ap11=start_value(4)
         sp11=start_value(5)
         as11=start_value(6)
         ss11=start_value(7)
         a1d13=start_value(8)
         a3d13=start_value(9)
         sd13=start_value(10)
         cas11=start_value(11)
         css11=start_value(12)
         cas31=start_value(13)
         css31=start_value(14)
         a1f15=start_value(15)
         a3f15=start_value(16)
         sf15=start_value(17)
         ca1d33=start_value(18)
         ca3d33=start_value(19)
         csd33=start_value(20)
         ca1d13=start_value(21)
         ca3d13=start_value(22)
         csd13=start_value(23)
         cap11=start_value(24)
         csp11=start_value(25)
         a1d15=start_value(26)
         a3d15=start_value(27)
         sd15=start_value(28)
         a1p13=start_value(29)
         a3p13=start_value(30)
         sp13=start_value(31)
         a1p33=start_value(32)
         a3p33=start_value(33)
         sp33=start_value(34)
         cw1=start_value(35)
         cw2=start_value(36)
         cw3=start_value(37)
         cw4=start_value(38)
         cw5=start_value(39)
         cw6=start_value(40)
         cw7=start_value(41)
         cw8=start_value(42)
         cw9=start_value(43)
         cw10=start_value(44)
         cw11=start_value(45)
         cw12=start_value(46)
         cw13=start_value(47)
         cw14=start_value(48)
         cw15=start_value(49)
C
         write(6,111)(start_value(i),i=1,49)
111     format(25x,'Value of parameters => 1-49 ',/,
     +'   cM =',f9.5,'   cE =',f9.5,'   cS =',f9.5,/,
     +'   ap11 =',f9.5,' sp11 =',f9.5,'  as11 =',f9.5,' ss11 =',f9.5,/,
     +'   a1d13 =',f9.5,'   a3d13 =',f9.5,'   sd13 =',f9.5,/,
     +'   cas11 =',f9.5,' css11 =',f9.5,' cas31 =',f9.5,' css31 =',f9.5,/,
     +'   a1f15 =',f9.5,'   a3f15 =',f9.5,'   sf15 =',f9.5,/,
     +'   ca1d33 =',f9.5,'   ca3d33 =',f9.5,'   csd33 =',f9.5,/,
     +'   ca1d13 =',f9.5,'   ca3d13 =',f9.5,'   csd13 =',f9.5,/,
     +'   cap11 =',f9.5,'   csp11 =',f9.5,/,
     +'   a1d15 =',f9.5,'   a3d15 =',f9.5,'   sd15 =',f9.5,/,
     +'   a1p13 =',f9.5,'   a3p13 =',f9.5,'   sp13 =',f9.5,/,
     +'   a1p33 =',f9.5,'   a3p33 =',f9.5,'   sp33 =',f9.5,/,
     +     '   cw1 =',f9.5,'   cw2 =',f9.5,'   cw3 =',f9.5,/,
     +     '   cw4 =',f9.5,'   cw5 =',f9.5,'   cw6 =',f9.5,/,
     +     '   cw7 =',f9.5,'   cw8 =',f9.5,'   cw9 =',f9.5,/,
     +     '   cw10 =',f9.5,'   cw11 =',f9.5,'   cw12 =',f9.5,/,
     +     '   cw13 =',f9.5,'   cw14 =',f9.5,'   cw15 =',f9.5,/)
C
      IF ( Wstep.GT.0 .AND. Wmin+Wstep.LT.Wmax )THEN
         Nw = INT( (Wmax - Wmin) / Wstep ) + 1
      ELSE
         If( Wmin.Eq.Wmax )Then
            Nw=1
         EndIf
      ENDIF
      IF ( Q2step.GT.0 .AND. Q2min+Q2step.LT.Q2max )THEN
         Nq2 = INT( (Q2max - Q2min) / Q2step ) + 1
      ELSE
         If( Q2min.Eq.Q2max )Then
            Nq2=1
         EndIf
      ENDIF
      IF ( cosstep.GT.0 .AND. cosmin+cosstep.LT.cosmax )THEN
         Ncos = INT( (cosmax - cosmin) / cosstep ) + 1
      ELSE
         If( cosmin.Eq.cosmax )Then
            Ncos=1
         EndIf
      ENDIF
      IF ( phistep.GT.0 .AND. phimin+phistep.LT.phimax )THEN
         Nphi = INT( (phimax - phimin) / phistep ) + 1
      ELSE
         If( phimin.Eq.phimax )Then
            Nphi=1
         EndIf
      ENDIF
      OPEN (UNIT=57,FILE=tabfile,STATUS='unknown')
      write(6,*)nw,nq2,ncos,nphi
c
      DO iq2=1,Nq2
c Loop over Q2
          Q2 = Q2min + float(iq2-1)*Q2step
          Do iw=1,Nw
c Loop over W
             W = Wmin + float(iw-1)*Wstep
c Get the epsilon
             nu=(w**2+q2-mp**2)/2./mp
             ep=ebeam-nu
             epsilon=1./(1.+2.*(q2+nu**2)/(4.*ebeam*ep-q2))
             do icos=1,Ncos
c Loop over cos(Theta*)
                costh=cosmin + float(icos-1)*cosstep
                dO iphi=1,Nphi
c Loop over phi*
                   phi=phimin + float(iphi-1)*phistep
c Calculate cross section
                   CALL daspe_RUN(W,Q2,epsilon,costh,phi,ierr)
	pi_type=pi_type2fit                   
                   cs_tab(iq2,iw,icos,iphi) = sigma(pi_type)
         theta=acos(costh)*180./acos(-1.)          
c write(57,*)q2,w,epsilon,costh,phi,cs_tab(iq2,iw,icos,iphi)

c       write(57,201)w,costh,phi,cs_tab(iq2,iw,icos,iphi)
       write(57,201)w,q2,costh,
     +  sigma_tpl(pi_type2fit),sigma_tt(pi_type2fit)
c     +  sigma_tl(pi_type2fit),sigma_tlp(pi_type2fit)
                enDdO
             enddo
          EndDo
       ENDDO
       close(57)
c 201  format(2x,3F7.3,2x,1F12.3)
 201  format(2x,3F7.2,2x,4F9.4)
       RETURN
 999  continue
       write(6,*)'Error while reading the data file'
      RETURN
      END
c
c------------------------------------------------------------------------------




