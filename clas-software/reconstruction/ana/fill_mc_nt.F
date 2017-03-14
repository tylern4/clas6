      SUBROUTINE FILL_MC_NT(ierr)
c     
c     _begin_doc
c     RCS ID string
c     $Id: fill_mc_nt.F,v 1.8 2009/03/14 17:31:56 fklein Exp $
c     
c     Documentation for subroutine FILL_MC_NT
c     
c     Purpose:
c     --------
c     
c     Input Parameters:  NON
c     ----------------
c     
c     Output Parameters:  NON
c     -----------------
c     
c     Other routines:
c     ---------------
c     
c     Notes:
c     ------
c     
c     Author:    Stepan     Created:  Tue Mar  4 16:49:27 EST 1997
c     -------
c     
c     Major revisions:
c     ----------------
c     
c     
c     _end_doc
c     
      IMPLICIT NONE
      SAVE
c     
c     _begin_inc
c     include files :
c     ---------------------
c     BOS common block  uncomment the next line for BOS include file
#include "bcs.inc"
#include "mc_nt.inc"
c     _end_inc
c     
c     _begin_var
c     input/output variables:
c     -----------------------
c     
c     Local pre-defined variables:
c     ---------------------------
c     RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: fill_mc_nt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.8 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2009/03/14 17:31:56 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
c 'think before changing!'   ^^^^^^^^^^^^^^^^^^who has changed the routine
      DATA CRCSID/   
     1     '$Id: fill_mc_nt.F,v 1.8 2009/03/14 17:31:56 fklein Exp $'   
     2     /   
c     Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='FILL_MC_NT')
      PARAMETER (CRAUTH='whoistheauthor')
c     
c     Local User defined variables:
c     -----------------------------
      INTEGER NWRITE
      INTEGER MAMIND,MLINK,ierr
      INTEGER nameindex
      DATA nameindex /0/
      INTEGER nwords, indvx(0:1),nvx,nrec
      INTEGER ind, indx, j, ind1,indx1
      REAL cx,cy,cz,charge
      real pi
      DATA pi/3.141592654/
c     _end_var
c     
c     executable code for routine FILL_MC_NT:
c---- 6----------------------------------------------------------------72
c     
      IF (NWRITE .LT. 1) THEN
         NWRITE = NWRITE + 1
         CRMESS='This is a DUMMY routine, this message written once'
         CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      if(nameindex.eq.0)nameindex=MAMIND(IW,'MCTK')
      indvx(0)=MLINK(IW,'MCVX',0)
      indvx(1)=MLINK(IW,'MCVX',1)
      mcnentr=0
      mcnpart=0
      ind=nameindex+1
      do while (iw(ind-1).gt.0)
         ind=iw(ind-1)
         nrec=iw(ind-2)
         if(nrec.eq.0 .or. nrec.eq.1) then
            nwords=IW(ind)-1
            do j=0,nwords,11
               if(mcnentr.ge.max_mcpart) goto 999
               mcnentr=mcnentr+1
               cx = rw(ind+j+1) 
               cy = rw(ind+j+2) 
               cz = rw(ind+j+3)
               if(abs(cz).gt.1) cz=sign(1.,cz)
               mcp(mcnentr)   = rw(ind+j+4)
               mcm(mcnentr)   = rw(ind+j+5)
               charge=rw(ind+j+6)
               mcid(mcnentr)  = iw(ind+j+7)
               mcst(mcnentr)  = iw(ind+j+8)
               nvx=iw(ind+j+9)
               mcpid(mcnentr) = iw(ind+j+11)
               if(mcst(mcnentr).lt.10)then
                  mcnpart=mcnpart+1
               endif
               mctheta(mcnentr) = acos(cz)*180./pi
               mcphi(mcnentr)   = atan2(cy,cx)*180./pi
               if(mcphi(mcnentr).lt.0.)mcphi(mcnentr)=360+mcphi(mcnentr) 
c MCVX corresponding hits
c NOTE: there is no one-to-one correlation!
               mcvx(mcnentr) = 0.
               mcvy(mcnentr) = 0.
               mcvz(mcnentr) = 0.
               if(nvx.eq.0) then
                  mcvz(mcnentr) = -1000.
               else
                  if(nvx.lt.0) then
                     nrec=0
                     nvx=-nvx
                  endif
                  if( indvx(nrec).gt.0) then
                     indx1=indvx(nrec)
                     if(iw(indx1).ge.(nvx-1)*5) then
                        mcvx(mcnentr) = rw(indx1+(nvx-1)*5+1) 
                        mcvy(mcnentr) = rw(indx1+(nvx-1)*5+2)
                        mcvz(mcnentr) = rw(indx1+(nvx-1)*5+3)
                        mctof(mcnentr)= rw(indx1+(nvx-1)*5+4)
                     endif
                  endif
               endif
            enddo
         endif
      enddo

 999  continue
      ierr=mcnentr   
c
      RETURN
      END
c
c------------------------------------------------------------------------------




