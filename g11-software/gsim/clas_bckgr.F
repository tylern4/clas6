      SUBROUTINE CLAS_BACKGR

C /* Turn on debug features. */
C /* #define DEBUG  */
C /* #undef DEBUG */
c
c_begin_doc
c  RCS ID string
c  $Id: clas_bckgr.F,v 1.4 1999/07/07 16:07:23 feuerbac Exp $
c
c  Documentation for subroutine CLAS_BACKGR
c
c  Purpose:
c  --------
c
c  Reads background events generated with EGS for a specific target material
c  and target thickness. This routine generates background electron, photon
c  and positron tracks for the CLAS detector, corresponding to a given 
c  luminosity, and weighted in energy and angle according to the EGS results.
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
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
c  The program expects a PAW histogram file as input for the EGS weighting info. 
c
c
c  Author:   V. Burkert  Nov 1987, May 1988
c  -------
c
c  Major revisions:
c  ----------------
c     Maurik Holtrop      Created GSIM version :  Mon Jan  6 17:44:21 EST 1997
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
c#include "bcs.inc"

#include "ffpar_bckgr.inc"
#include "gckine.inc"
#include "gsimpar.inc"
#include "gconsp.inc"
#include "gccuts.inc"

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
      PARAMETER (CFILE = '$RCSfile: clas_bckgr.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.4 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1999/07/07 16:07:23 $')
      PARAMETER (CAUTHO = '$Author: feuerbac $')
      DATA CRCSID/   
     1'$Id: clas_bckgr.F,v 1.4 1999/07/07 16:07:23 feuerbac Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='CLAS_BCKGR')
      PARAMETER (CRAUTH='Maurik Holtrop')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER lunfet
      INTEGER nbrems,ierr,i,j,klm,numtrack
      INTEGER nx,ny,nwt,iad,nvtx,ntrans
      REAL  xmi,xma,ymi,yma
      REAL  brems,fis,olmc,fback,ebrems
      REAL  thetbr,ptot,phi,theta,energy,pgamma,egamma
      real hsum,hij
      real tmpf
      external hsum
      external hij

      character*120 filename

C
      REAL PLAB(3), FRA(3), ATRANS(3), ESHELL(3), ZSHELL(3),
     + PBREM(3)
      REAL RNDM(3),RNDMG(3)
      CHARACTER*80 TIT
      LOGICAL LFIRST, HEXIST
      DATA    LFIRST /.TRUE./
      DATA  ZSHELL   /     1. ,     0. ,     0./
      REAL E0,EBRMIN,THBMIN
      DATA         E0,  EBRMIN,  THBMIN
     1     /       3.,   1.E-6,     .1/

      INTEGER MXTRAK
      DATA MXTRAK/20000/

c_end_var
c
c  executable code for routine CLAS_BACKGR:
c----6----------------------------------------------------------------72
c
C
C     ------------------------------------------------------------------
C
      IF (LFIRST) THEN
         LFIRST = .FALSE.

         THBMIN = THBMIN*DEGRAD

         if( (ntarget.gt.0).AND.(ntarget.le.4))then

C We're getting a standard background file.
C Get full path name to $CLAS_PARMS/file.hbook

            call revinm(clas_parms,HBOOKNAM(ntarget),filename)
            zelem=xzelem(ntarget)
            atom= xatom(ntarget)
            rho = xrho(ntarget)
            tleng= xtleng(ntarget)
            nincel=xnincel(ntarget)
                        
         else
            write(crmess,'('' No standard input histogram file for target num ''
     +           ,I3)')ntarget
            call recmes(crname,'I',crmess)
            call recmes(crname,'I',' Looking for bckgr.hbook in local directory.')
            filename = 'bckgr.hbook'
         endif

         write(crmess,'('' Target type:'',I3)')ntarget
         call recmes(crname,'I',crmess)
         write(crmess,'('' Luminocity = '',E8.2,''  Target (A,Z)='',
     +        F6.2,'','',F6.2)')olumen,atom,zelem
         call recmes(crname,'I',crmess)
         write(crmess,'('' Density = '',E8.2,'' Target lenght = '',
     +        E6.2)')rho,tleng
         call recmes(crname,'I',crmess)

C
C Get a logical unit.
C
         call reslun(crname,lunfet,ierr)
         if(ierr.ne.1)then
            call recmes(crname,'E',' Can not reserve logical unit.')
            return
         endif

	
C
C--  Use IKINE as error flag. The calling routine clas_kine reset the IKINE
C    variable to not use clas_backgr (subtracting 10). If all goes well, we add
C    10 back in to continue callinhclas_backgr.
C
C     Generate # photons from large angle bremsstrahlung within
C     the sensitive time interval. A simple formula is used
C     which has no screening (L. Maximom). The cross section
C     has been normalized at 1 MeV and 35 degrees using the
C     results of S. Williamsons numerical calculation, which
C     is based on Maximoms theory.
C
         BREMS = 2.46E-33*ZELEM**2/ATOM/E0**2*ALOG(E0/EBRMIN)
     1            * OLUMEN*TIMEDC*(1./(SIN(0.5*THBMIN))**2-1.)
         CALL POISSN(BREMS, NBREMS, IERR)

c	print*,NBREMS
         IF (NBREMS.GT.MXTRAK) THEN
            NBREMS = MXTRAK
            write(crmess,'('' Too many bremstrahlung tracks'')')
            call recmes(crname,'E',crmess)
         END IF
C
C      Now do the atomic transitions
C      Calculate the K-L-M shell energies according to
C      Moseley's law.
C
         ESHELL(1) = 0.
         ESHELL(2) = 0.
         ESHELL(3) = 0.
         IF (ZELEM.GE.2.) THEN
            ESHELL(1) = 13.6E-9*(ZELEM-1.)**2*(1./1.-1./2.**2)
         END IF
         IF (ZELEM.GE.8.) THEN
            ESHELL(2) = 13.6E-9*(ZELEM-7.4)**2*(1./2.**2-1./3.**2)
         END IF
         IF (ZELEM.GE.18.) THEN
            ESHELL(3) = 13.6E-9*(ZELEM-17.)**2*(1./3.**2-1./4.**2)
         END IF
C
C      Generate the # photons from atomic transitions (KLM-shell)
C      within the sensitive interval. For now only photons with the
C      the respective shell energy are generated;this should be
C      modified to generate the full K-, L-, M- series with correct
C      weights.
C
         DO 10 KLM=1,3
           ATRANS(KLM) = 0.
C
C      Consider only shell energies greater than 1 KeV
C
           IF (ESHELL(KLM).GE.1.E-6) THEN
              ATRANS(KLM) = 2.5494E-28/ESHELL(KLM)/ATOM*ZSHELL(KLM)*
     1             OLUMEN*TIMEDC
C
C      Correct for the low value obtained with the above formula
C      The measured cross sections indicate a factor of 6 higher
C      photon yield (Middleman, Ford, Hofstadter;
C      Phys.Rev. A2,1429 (1970)). The fit shown in this reference
C      should be used in the future!!!!!!!!!
C
              ATRANS(KLM) = 6.*ATRANS(KLM)
           END IF
 10     CONTINUE
C
C     Read booked histograms
C
#ifdef DEBUG
        write(crmess,'('' Opening debug file back.out '')')
        call recmes(crname,'I',crmess)
        
        open(unit=83,file='back.out',status='unknown')
        write(83,*) 'Background Debug File.'
        write(83,*) ' L=',olumen,' A,Z',atom,zelem
        write(83,*) ' N=',nincel,' rho',rho
        write(83,*) ' tl=',tleng,' timedc:',timedc
        write(83,*) ' Bckfile:',filename
        write(83,*) ' Expecting ',NBREMS,' Brem events, brems=',BREMS
        write(83,*) ' Atrans:',ATRANS(1),ATRANS(2),ATRANS(3)
#endif
        call recmes(crname,'I','Opening File:')
        write(crmess,'(A100)')filename
        call recmes(crname,'I',crmess)

	call HROPEN(lunfet,'HISTO',filename,' ',1024,IERR)

        write(crmess,'('' Read hist from logical unit:'', I3)')LUNFET
        call recmes(crname,'I',crmess)
        
        DO 90 IPART=1,3
C*
           CALL HRIN(IPART, 9999, 0)
           IF (.NOT.HEXIST(IPART)) THEN
              write(crmess,'(''  Histogram'', I3, '' not found; '',
     +             ''IKINE reset to no-background value.'')')IPART
              call recmes(crname,'E',crmess)
              RETURN
           END IF
           CALL HGIVE(IPART,TIT,NX,XMI,XMA,NY,YMI,YMA,NWT,IAD)
           write(crmess,'('' Histogram'', I2, '': '',5A4)')IPART, TIT
           call recmes(crname,'I',crmess)
           FIS = HSUM(IPART)
           tmpf = FIS
           DO 80 J=1,NY
             FIS = FIS-HIJ(IPART,1,J)
 80        CONTINUE

#ifdef DEBUG
         write(83,*)' Ipart: ',IPART,' HSUM:',tmpf,' Fis:',FIS
#endif
           FRA(IPART) = FIS/NINCEL
           tmpf = FRA(IPART)
           OLMC = NINCEL*RHO*6.E23*TLENG
           FBACK = OLUMEN/OLMC*NINCEL*TIMEDC
           FRA(IPART) = FBACK*FRA(IPART)
#ifdef DEBUG
           write(83,*)' F/N:',tmpf,' OLMC:',OLMC,
     +          ' FBACK:',FBACK,'FRA:',FRA(IPART)
#endif           
 90      CONTINUE

          CALL HREND('HISTO')
	close (lunfet)
        call reslun(crname,-lunfet,ierr)

      END IF
C
C       Here do the bremsstrahlung contribution
C
      DO 300 I=1,NBREMS
C
C      Sample the photon energy and angle
C
        CALL GRNDM(RNDM,3)
        EBREMS = EBRMIN*EXP(ALOG(E0/EBRMIN)*RNDM(1))
        IF (EBREMS.LE.CUTGAM) GO TO 300
        THETBR = 4.*ASIN(SIN(0.5*THBMIN)/SQRT(1.-RNDM(2)*
     1           (COS(0.5*THBMIN))**2))
        PTOT  = EBREMS
        PHI = RNDM(3)*TWOPI
        PLAB(1) = PTOT * SIN(THETBR)*SIN(PHI)
        PLAB(2) = PTOT * SIN(THETBR)*COS(PHI)
        PLAB(3) = PTOT * COS(THETBR)
        CALL KIVERT(NVTX)
        IF (NVTX.LE.0) THEN
           write(crmess,'(''  Vertex could not be stored'')')
           call recmes(crname,'E',crmess)
           GO TO 99
        END IF
#ifdef DEBUG
        write(83,*)' Brem e: ',PTOT,PLAB(1),PLAB(2),PLAB(3)
#endif
        CALL GSKINE(PLAB,1,NVTX,0,0,numtrack)
        IF (numtrack.LE.0) THEN
           write(crmess,'('' Track could not be stored'')')
           call recmes(crname,'E',crmess)
           GO TO 99
        END IF
 300  CONTINUE
C
C       Here do the atomic transitions
C
      DO 20 KLM=1,3
        IF (ESHELL(KLM).LE.CUTGAM) GO TO 20
        CALL POISSN(ATRANS(KLM),NTRANS,IERR)
        IF (NTRANS.GE.MXTRAK) THEN
           NTRANS = MXTRAK
           write(crmess,'('' Too many tracks'')')
           call recmes(crname,'E',crmess)
        END IF

        DO 30 I=1,NTRANS
          CALL KIVERT(NVTX)
          IF (NVTX.LE.0) THEN
             write(crmess,'(''  Vertex could not be stored'')')
             call recmes(crname,'E',crmess)
             GO TO 99
          END IF
C
C       Sample the uniform angular distribution
C
          CALL GRNDM(RNDM,2)
          PHI = RNDM(1)*TWOPI
          THETA = ACOS(1.-2.*RNDM(2))
          PTOT = ESHELL(KLM)
          PLAB(1) = PTOT*SIN(THETA)*SIN(PHI)
          PLAB(2) = PTOT*SIN(THETA)*COS(PHI)
          PLAB(3) = PTOT*COS(THETA)
#ifdef DEBUG
          write(83,*)' Shel e: ',PTOT,PLAB(1),PLAB(2),PLAB(3)
#endif
          CALL GSKINE(PLAB,1,NVTX,0,0,numtrack)
          IF (numtrack.LE.0) THEN
             write(crmess,'('' Track could not be stored'')')
             call recmes(crname,'E',crmess)
             GO TO 99
          END IF
  30    CONTINUE
  20  CONTINUE
C
C      Generate random numbers according to Poisson distribution
C      for the # of EGS4 background electrons, photons, positrons
C
      
      DO 100 IPART=1,3
         CALL POISSN(FRA(IPART),numtrack,IERR)
         IF (numtrack.GT.MXTRAK) THEN
            numtrack = MXTRAK
            write(crmess,'('' Too many tracks'')')
            call recmes(crname,'E',crmess)
         END IF
#ifdef DEBUG
        write(83,*)' Ntrack PI,# : ',IPART,FRA(IPART)
#endif
         DO 200 I=1,numtrack
            
C      Generate the energy and angles weighted with the histogram
C      contents
C
            CALL GRNDM(RNDM,1)
            CALL HRNDM2(IPART,THETA,ENERGY)
            ENERGY = 1.E-3 * EXP((ENERGY-1.)/10.+0.1-6.9)
            IF (IPART.EQ.1) THEN
               IF (ENERGY.LE.CUTGAM) GO TO 200
            ELSE
               IF (ENERGY.LE.CUTELE) GO TO 200
            END IF
            PTOT  = SQRT(ENERGY**2 + 2*ENERGY*EMASS)
            THETA = (1.8*THETA-0.9) * DEGRAD
            PHI = RNDM(1)*TWOPI
            PLAB(1) = PTOT * SIN(THETA)*SIN(PHI)
            PLAB(2) = PTOT * SIN(THETA)*COS(PHI)
            PLAB(3) = PTOT * COS(THETA)
            CALL KIVERT(NVTX)
            IF (NVTX.LE.0) THEN
               write(crmess,'(''  Vertex could not be stored'')')
               call recmes(crname,'E',crmess)
               GO TO 99
            END IF
            if (ipart.eq.3) then
               Pgamma=1/(137.*pi)*log(4.*(ENERGY+EMASS)**2/EMASS**2-1.)
     +              *log(real((ENERGY+EMASS)/CUTELE))
               call grndm(rndmg,1)
               if (pgamma.gt.rndmg(1)) then
                  egamma=cutele*exp(rndm(1)*alog(energy/cutgam))
                  IF (EGAMMA.LE.CUTGAM) GO TO 200
                  PBREM(1) = EGAMMA * SIN(THETA)*SIN(PHI)
                  PBREM(2) = EGAMMA * SIN(THETA)*COS(PHI)
                  PBREM(3) = EGAMMA * COS(THETA)
#ifdef DEBUG
                  write(83,*)' Brem g: ',egamma,PBREM(1),PBREM(2),PBREM(3)
#endif
                  call gskine(pbrem,1,NVTX,0,0,numtrack)
               endif
            endif
#ifdef DEBUG
            if(IPART.eq.1)then
               write(83,*)' Photon: ',PTOT,PLAB(1),PLAB(2),PLAB(3)
            elseif(IPART.eq.2)then
               write(83,*)' Positr: ',PTOT,PLAB(1),PLAB(2),PLAB(3)
            elseif(IPART.eq.3)then
               write(83,*)' Electr: ',PTOT,PLAB(1),PLAB(2),PLAB(3)
            else
               write(83,*)' Unknown:',IPART,PTOT,PLAB(1),PLAB(2),PLAB(3)
            endif
#endif
            CALL GSKINE(PLAB,IPART,NVTX,0,0,numtrack)
            IF (numtrack.LE.0) THEN
               write(crmess,'('' Track could not be stored'')')
               call recmes(crname,'E',crmess)
               GO TO 99
            END IF
 200     CONTINUE
 100  CONTINUE
 99   CONTINUE
C
C
C--  Since everything seems to be ok reset IKINE again.
        if(ikine.le.0)then
           IKINE = ikine-10
        else
           ikine = ikine+10
        endif

      RETURN
      END

c
c------------------------------------------------------------------------------

      SUBROUTINE KIVERT(NVTX)

      implicit none
C
C  Vertex origin is located randomly in a cube of 1mm to a side.
C
C
      integer nvtx
      real vertex(3),uran(3)

      CALL GRNDM(URAN,3)
      VERTEX(1) = 0.1*URAN(1)
      VERTEX(2) = 0.1*URAN(2)
      VERTEX(3) = 0.1*URAN(3)

      CALL GSVERT(VERTEX, 0, 0, 0, 0, NVTX)
C
      RETURN
      END
