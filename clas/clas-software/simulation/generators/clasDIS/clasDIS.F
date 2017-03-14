c
        PROGRAM CLASDIS
c
        IMPLICIT NONE
c
C******************************************************************
C
C       Main program for PEPSI simulations for CLAS kinematics
C
C***************************************************************** 
#include "names.inc"
#include "claslepto.inc"
C.. {\bf LFN} Number of the status files for random numbers 
      INTEGER       LFN,I,NEVENT
c
      call CL_OPTIONS          ! command line processing
      call INIT_PAW            ! command line processing
c
C..   number of events   
c
       NEVENT=cl_triggers


C.. Constats and Parameter

C.. Number of the  RNDMSTATUS FILE
      LFN = 10
C.. Read in of the random number status file 
      CALL RNDMSTART(LFN)

C.. Initialization of the lepto program 
      CALL DISINIT
      if(cl_verblev.gt.0) WRITE(*,*) 'NEVENT=',NEVENT

C.. Event generation: 
       DO I = 1,NEVENT
       
C.. Output of the number of generated events 
        IF (MOD(I,cl_nprint).EQ.0.and.cl_verblev.gt.0)
     6 WRITE(*,*) I,'  from ',NEVENT,PARL(23),PARL(24)
         
C.. Generation of an event in {\sc Lepto-6.5} 
 10     CALL LEPTO
       if(cl_verblev.gt.100.and.cl_verblev.lt.104) 
     $         call lulist(cl_verblev-100)
C.. Message in case of an error in the event generation 
        IF ((LST(21).NE.0)) THEN
          WRITE(*,*) 'event-error at',I,LST(21)  
          GOTO 10
        ENDIF
       CALL CLASFILL(I)
c
       ENDDO
c
       CALL CLOSEPAW()
       if(datfileOK) CLOSE(41) ! close the data file
c
       WRITE(*,*) 'Total ',NEVENT,' in pb ',PARL(23),PARL(24)
C.. Saving the last random number configuration 
 999  if(cl_rand.eq.1) CALL RNDMSAVE(LFN)
 
       END      
c
       subroutine closepaw()
       implicit none
       INTEGER ICYCLE
         call HCDIR('//ZXC',' ')
         call HROUT(0,icycle,'')
         call HREND('ZXC')
         close(50)
         return
       end
c
      subroutine get_datfilename()
      implicit none
#include "names.inc"
#include "claslepto.inc"
      character*4  cpol,cdis
      character*10  cxsec
      character*21  ccuts
c
c
      if(cl_beam_energy.lt.10.0) then
      write(ccuts,'(A,F5.3,A,F3.2,A,F3.2)') 'e',cl_beam_energy,
     6'.emin',cl_emin,'tmin',cl_tmin 
      else
      write(ccuts,'(A,F6.3,A,F4.2,A,F3.2)') 'e',cl_beam_energy,
     6'.emn',cl_emin,'tmn',cl_tmin 
      endif
      write(cxsec,'(A,F5.2,A)') '.xs',PARL(23)/1000.0,'nb'
      print *,'**',ccuts,'++',cxsec
c
      if(cl_pol.eq.1) then
       cpol='.p1.'
      else if (cl_pol.eq.-1) then
       cpol='.m1.'
      else
       cpol='.00.'
      endif
c
      if (cl_pid.eq.0) then 
       cdis='.dis'
      else
       write (cdis,'(A,F3.1)') '.',cl_zmin
      endif
      print *,'PARL23,24',PARL(23),PARL(24)
      write(datfilename,'(A)') cl_path(1:17)//'clasdis'//
     6cl_target(1:2)//cpol//ccuts//cxsec//cdis
      print *,datfilename
      end
c
       SUBROUTINE INIT_PAW
       implicit none
       integer h,nwpawc
       parameter (nwpawc=5000000)
       integer  NTID_OUT

       common /PAWC/ h(nwpawc)
       call HLIMIT(nwpawc)
       NTID_OUT=22
       call hini(NTID_OUT)
       return
       end
 
      subroutine hini(NTID_OUT)
      implicit none
#include "ntupgkoko.inc"
#include "names.inc"
c
      integer NTID_OUT,IQUEST
      COMMON/QUEST/IQUEST(100) 
      integer lrec,istat
      lrec=4096
c     open koko
c
       IQUEST(10)=65000
       CALL HROPEN(50,'ZXC','clasDIS.hbook','NQE',LREC,ISTAT)
       call hbset('BSIZE',8176,istat)
      if (istat.ne.0) stop
      call HBNT(NTID_OUT,'semiout',' ')
      call HBNAME(NTID_OUT,'elegen',gnele,
     +'gnele:I,gelebits:I,ghpar:I,gproh:I,geleh:I,'//
     +'gelee:R,gelet:R,'//
     +'gelef:R,gelex:R,geley:R,'//
     +'gelez:R,gelewgt:R,gxb:R,gq2:R,gw:R')
c234567890123456789012345678901234567890123456789012345678901234567890123456
      call HBNAME(NTID_OUT,'hadgen',gnpi0,
     +'gnpi0:I,gnpip:I,gnpim:I,gnpro:I,gngam:I,'//
     +'gnneu:I,gpi0e:R,gpi0t:R,gpi0f:R,gpipe:R,'//
     +'gpipt:R,gpipf:R,gpime:R,'//
     +'gpimt:R,gpimf:R,gproe:R,gprot:R,gprof:R,ggame:R,'//
     +'ggamt:R,ggamf:R,'//
     +'ggam2t:R,ggam2e:R,ggam2f:R,gneue:R,gneut:R,gneuf:R,gpipx:R,'//
     +'gpipy:R,gpipz:R,gpimx:R,gpimy:R,gpimz:R,gprox:R,gproy:R,gproz:R')
c
      if (cl_radgen) then    
       call HBNAME(NTID_OUT,'radgn',iproctpe,
     +'iproctpe:I,radq2:R,radnu:R,radweight:R,'//
     +'grade:R,gradt:R,gradf:R')
      endif
      end
c
       SUBROUTINE CL_OPTIONS
       implicit none
#include "names.inc"
       integer i
       character*20 cnumber
       INTEGER  numopts,NCDECI
       real valnum
c
c    command line processing
c
c     defaults
c
      cl_mstu=1              ! default mstu(16)
      cl_zpos=0              ! z-position
      cl_zwidth=0            ! z-width ( zpos+/-zwidth)
      cl_pol=0               ! unpolarized target
      cl_beam_energy=5.754   ! def eg1-2000 (e16-5.754)
      cl_emin=0.75           ! def e'min
      cl_emax=0.0  
      cl_tmin=0.24           ! e16->0.3           ! def e'tmin
      cl_tmax=1.57           ! def e'tmax
      cl_target='proton'     ! (def proton target)
c
      cl_xmin=0.05
      cl_xmax=0.95
      cl_qmin=0.85
      cl_qmax=20.0
      cl_wmin=4.0
      cl_wmax=50.0
c
      cl_zmin=0.3           ! def z-min
c
      cl_verblev=0
      cl_nprint=1000         ! print every cl_nprint event
      cl_rand=1              ! update the random number in end
      cl_triggers = 10  
      cl_ht=0
      cl_pid = 0               ! take all  
      ntOK=.TRUE.
      acce16=.FALSE.
      acceg1=.FALSE.
      datfileOK=.FALSE.               ! don't write a data file
      cl_path='/home/avakian/w6/'     ! path for the datafile
      cl_outform=1                    ! All events (2-LUND-GSIM)
      numopts  = IARGC()
c      
      if(numopts.gt.0 ) then
        i=0
      else
        goto 2
      endif
 1    i=i+1
        CALL GETARG(i,cnumber)
        if(cnumber.eq.'--trig'.and.i.lt.numopts) then
          i=i+1
          CALL GETARG(i,cnumber)
          cl_triggers=NCDECI(cnumber)
        elseif(cnumber.eq.'--v'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_verblev=NCDECI(cnumber)
        elseif(cnumber.eq.'--print'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_nprint=NCDECI(cnumber)
        elseif(cnumber.eq.'--pol'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_pol=NCDECI(cnumber)
        elseif(cnumber.eq.'--outform'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_outform=NCDECI(cnumber)
        elseif(cnumber.eq.'--beam'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_beam_energy=NCDECI(cnumber)*0.001           
        elseif(cnumber.eq.'--pid'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_pid=NCDECI(cnumber)
        elseif(cnumber.eq.'--z'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_zmin=valnum(cnumber)           
        elseif(cnumber.eq.'--t'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_tmin=valnum(cnumber)*3.1415926/180.0
           i=i+1
           CALL GETARG(i,cnumber)
           cl_tmax=valnum(cnumber)*3.1415926/180.0
        elseif(cnumber.eq.'--e'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_emin=NCDECI(cnumber)*0.001
           i=i+1
           CALL GETARG(i,cnumber)
           cl_emax=NCDECI(cnumber)*0.001
        elseif(cnumber.eq.'--x'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_xmin=valnum(cnumber)
           i=i+1
           CALL GETARG(i,cnumber)
           cl_xmax=valnum(cnumber)
        elseif(cnumber.eq.'--q'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_qmin=valnum(cnumber)
           i=i+1
           CALL GETARG(i,cnumber)
           cl_qmax=valnum(cnumber)
        elseif(cnumber.eq.'--w'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_wmin=valnum(cnumber)
           i=i+1
           CALL GETARG(i,cnumber)
           cl_wmax=valnum(cnumber)
        elseif(cnumber.eq.'--zpos'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_zpos=NCDECI(cnumber)*0.1           
        elseif(cnumber.eq.'--zwidth'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_zwidth=NCDECI(cnumber)*0.1           
        elseif(cnumber.eq.'--targ'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cl_target)
        elseif(cnumber.eq.'--path'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cl_path)
         elseif(cnumber.eq.'--nont') then
           ntOK=.FALSE.
        elseif(cnumber.eq.'--norand') then
          cl_rand=0
        elseif(cnumber.eq.'--radgen') then
           cl_radgen=.TRUE.
        elseif(cnumber.eq.'--HT'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_ht=NCDECI(cnumber)
        elseif(cnumber.eq.'--mstu') then
          cl_mstu=2
        elseif(cnumber.eq.'--datf') then
           datfileOK=.TRUE.
        elseif(cnumber.eq.'--acce16') then
           acce16=.TRUE.
        elseif(cnumber.eq.'--acceg1') then
           acceg1=.TRUE.
        else
          call printusage
          stop
        endif
        if(i.lt. numopts) goto 1
 2      continue
        if(cl_emax.eq.0.0) then
         cl_emax=0.95*cl_beam_energy            ! put some limit
        endif
        call printvalues
        return 
        end
        subroutine printusage
        print *,'clasDIS [options]            '
        print *,'      option  value      default  comment  '
        print *,'     --v    verbos_level    0     additional printout '
        print *,'     --trig nevents         10     number of triggers '
        print *,'     --beam beam_energy     5700   e- momentum in MeV '
        print *,'     --e e-min e-max               min[750] max[0.95*e] Eprime in MeV '
        print *,'     --t tmin tmax         14.0    theta min/max for \theta in degree'
        print *,'     --x xmin xmax        0.05 0.95    x_b min/max'
        print *,'     --q Q2min Q2max        0.85 20.0    Q2 min max in GeV^2'
        print *,'     --w W2min W2max        4.0  50      W^2 min max GeV^2'
        print *,'     --z                 0.3       z=E_pi/nu min value '
        print *,'     --zpos z-position      0    target z position in mm '
        print *,'     --zwidth z-width       0    width in z in mm (zpos+/-zwidth) '
        print *,'     --nont               do not write out the ntuple '
        print *,'     --targ target       proton   deuteron/neutron possible '
        print *,'     --pol  polarization    0%    -100 - 100% target polarization '
        print *,'     --print nprint       1000   print every nprint event '
        print *,'     --acceg1                    acceptance for eg1 ON '
        print *,'     --acce16                    acceptance for e16 ON '
        print *,'     --pid  LUND_ID       0   0-all,211-pip,3122-Lambda..'
        print *,'     --norand                   keep the  old random number'
        print *,'     --mstu                     set mstu(16)=2'
        print *,'     --HT    iht           0   higher twists on (LST(11)=iht)'
        print *,'     --outform       1         output filr with all events 2-GSIM'
        print *,'     --datf       .FALSE.      write the output file'
        return
        end
c
        subroutine printvalues
        implicit none
#include "names.inc"
        print *,'clasDIS input values:'    
        print *,'NEVENTS:             ',cl_triggers    
        print *,'BEAM ENERGY:         ',cl_beam_energy  
        print *,'Eprime   min/max:         ',cl_emin,cl_emax  
        print *,'Eprime tmin/tmax:         ',cl_tmin,cl_tmax  
        print *,'TARGET:              ', cl_target 
        print *,'TARGET z-position:   ', cl_zpos 
        print *,'TARGET 1/2 width :   ', cl_zwidth 
        print *,'polarization     :   ', cl_pol 
        print *,'acceptance       :   ',acceg1,acce16 
        print *,'pid to keep:         ',cl_pid  
        if(cl_ht.gt.0) print *,'Higher twists ON: LST(11)=',cl_ht 
        print *,'__________________________________'
        print *,'verbosity level',  cl_verblev
        return
        end

C******************************************************************


        SUBROUTINE DISINIT
        IMPLICIT NONE

C*******************************************************************
#include "names.inc"
#include "claslepto.inc"

C.. Set up cuts and parameters which are different from the default
C.. valules 
c
       REAL PLZ,PPZ
c
c$$$      lst(1)=2 ! x-y greed
c$$$      lst(3)=1 ! output
c$$$      lst(5)=3 ! lab system
c$$$      lst(7)=0 ! kinem. and parton level event generated
c$$$      lst(12)=2 ! max flavour in sea
c$$$      lst(18)=1 ! alfa_em=1/137
C.. $x$ min cut  
       CUT(1)=cl_xmin
C.. $x$ max cut  
       CUT(2)=cl_xmax
C.. $y$ min cut 
       CUT(3)=0.05
C.. $y$ max cut  
       CUT(4)=0.95
C.. $Q^2$ min cut
       CUT(5)=cl_qmin
C.. $Q^2$ max cut
       CUT(6)=cl_qmax
C.. $W^2$ min cut    
       CUT(7)= cl_wmin
C.. $W^2$ max cut   
       CUT(8)= cl_wmax
C.. lower limit scat. lep. energy
       CUT(11)=cl_emin !0.6 
       print *,'e_min,cut(11)',cl_emin,CUT(9),CUT(10),CUT(11),CUT(12)
C..  upper limit scat. lep. energy
       CUT(12)=cl_emax !1.65
C.. $\nu$ min cut
       CUT(9)=0.1
C.. $\nu$ max cut
       CUT(10)= cl_beam_energy-cl_emin
C.. lower limit scat lep. angle
       CUT(13)=cl_tmin !0.52-0.08          ! 29.75 degree+/-4
C..  upper limit scat lep. angle
       CUT(14)=cl_tmax !0.52+0.08

C.. Cuts from LEPTOU applied
c       LST(2) = 2
C.. Prints detailed output and MINUIT-output 
       LST(3)= 5
C.. With matrix elements 
       LST(8)= 1
C.. With matrix elements 
       LST(11)= cl_ht
C.. Number of intrinsic sea-quark flavours 
       LST(12) = 3
C.. Parton distribution:  145 grsv2000 hep-ph/0011215 LO valence scenario
C.. Parton distribution:  147 grsv2000 hep-ph/0011215 NLO valence scenario
C.. Parton distribution:  110 Gehrmann  et al hep-ph/9512406 Gluon A (LO)
       LST(15) = 145
C.. choice of the x W grid
C       LST(19) = 10
C.. Only $\gamma$ exchange in the simulation. 
       LST(23) = 1
C.. Only valence-isopin roation
       LST(36) = 0
C.. Multiplication factor for the polarized parton distributions,
C.. This is designed in oder to shorten the CPU time necessary  
C.. considerably. For example, a multiplication factor of 100 yields 
C.. a shortening of the running time of a factor 10000, i.e in order  
C.. to generate the same result in a run without this multiplication  
C.. factor one has to produce 10000 times more events. 
       LST(38) = 1
C.. Rotation is happening in the program automatically
       LST(39) = 0
C.. Set up of the polarization configuration
       LST(40) = cl_pol        ! 0 unpolarized run
C.. set mstu
       MSTU(16)=cl_mstu
C.. Momentum of incoming lepton (in GeV)
       PLZ = cl_beam_energy  

C.. Momentum of incoming nucleon  (in GeV)
       PPZ   = 0.0


C.. Simulation for a proton
      if(cl_target.eq.'deuteron') then
        PARL(1) = 2
        PARL(2) = 1 
        FMAXFH  = 1.7
      elseif(cl_target.eq.'neutron') then
        PARL(1) = 1
        PARL(2) = 0
        FMAXFH  = 1.5
      elseif(cl_target.eq.'proton') then
        PARL(1) = 1
        PARL(2) = 1
        FMAXFH  = 1.9
      else               ! proton
          call printusage
          stop       
      endif
C..
C..  used in PEPSI
C..  IF(PARI(LST(23)).EQ.0.0) PARI(LST(23))=FMAXFH

C.. Call of the {\sc Pepsi}/{\sc Lepto} initialization routine 
C..       CALL POLLINIT(0,11,PLZ,PPZ,1)
          CALL LINIT(0,11,PLZ,PPZ,1)


        RETURN
        END
      


C**********************************************************************
C      random number generator
C**********************************************************************
         real function random_num()
         random_num=rndm(-1)
         return
         end

C**********************************************************************
C
C       Subroutine for saving satus of random number generator
C
C**********************************************************************


       SUBROUTINE RNDMSAVE(LFN)
C.. Saving of the satuts of the randum number generator 

       COMMON/LUDATR/MRLU(6),RRLU(100)
       INTEGER  MRLU
       REAL  RRLU
       SAVE /LUDATR/
       INTEGER       LFN,I1,I2    
c
       OPEN(LFN,FILE="RNDMSTATUS",status='UNKNOWN')
       WRITE(LFN,*) (MRLU(I1),I1=1,5),
     & (RRLU(I2),I2=1,100)
       CLOSE(LFN)

       RETURN
       END       
      
C***********************************************************************
C
C         Subroutine for reading satus of random number generator
C
C**********************************************************************


        SUBROUTINE RNDMSTART(LFN)
C.. Read in the current status of the random number generator 

        COMMON/LUDATR/MRLU(6),RRLU(100)
        INTEGER  MRLU
        REAL  RRLU
        SAVE /LUDATR/

        INTEGER       LFN,I1,I2    
        LOGICAL       EX
       
       INQUIRE(FILE="RNDMSTATUS",EXIST = EX)
       IF (EX) THEN

       OPEN(LFN,FILE="RNDMSTATUS",status='UNKNOWN')
       READ(LFN,*) (MRLU(I1),I1=1,5),
     & (RRLU(I2),I2=1,100)
       CLOSE(LFN)

       ENDIF

       RETURN
       END       
       

C***********************************************************************
      SUBROUTINE CLASFILL(IEVENT)
C***********************************************************************
       implicit none
#include "names.inc"
#include "ntupgkoko.inc"
#include "claslepto.inc"
c
c
c
          integer ievent,ierr,lunout
	  parameter (lunout=33)
          integer ind,indx,mctk,mcvx,mbank,nbank,part
          integer j,jj,lund2geantid
          real pmom,plu,random_num
C ********************************************************
C       Write to the output event file
C ********************************************************
c
c
       call VZERO(gnele,12)
       call VZERO(gnpi0,35)
        jj=0
c
         do 999 j=1,N
c
       if(k(j,1).gt.10) goto 999
c       if(k(j,2).eq.22.and.  k(k(j,3),2).eq.111) goto 999
         jj=jj+1
c
         call fillevent(j)

 999    CONTINUE
c 
          call goodevent(IEVENT)
          if(clasdisOK.and.ntOK) then
            call HFNT(22)
          endif
c
          if(clasdisOK.and.datfileOK) then
                 call write2file(jj,cl_outform)  ! 1-all 2-GSIM-lund
          endif               ! if(clasdisOK.and.datfileOK)
c
      RETURN
      END     

C***********************************************************************
          subroutine write2file(jj,il)
C***********************************************************************
c write the LUND event:
c
c the header:
c N number of particles in the event
c PARL(1),PARL(2): number of nucleons and protons of target
c cl_pol,elpol: polarization of proton and electron
c LEX,LEYY,LEW2,LEQ2,LEU: x,y,W^2,Q^2, nu
c
c the body:
c i particle number
c plu(i,6) : particle charge
c
c K(I,1) = 3, 13} or \ttt{14}, instead, it  contains special
c K(I,2) : particle code
c K(I,3) : line number of parent particle, where known,otherwise 0. 
c K(I,4) : normally the line number of the first daughter and
c          0 for an undecayed particle or unfragmented parton.
c
c P(I,1) $p_x$, momentum in the $x$ direction, in GeV/$c$.
c P(I,2) $p_y$, momentum in the $y$ direction, in GeV/$c$.
c P(I,3) $p_z$, momentum in the $z$ direction, in GeV/$c$.
c P(I,4) $E$, energy, in GeV.
c P(I,5) $m$, mass, in GeV/$c^2$. 
c       In parton showers, with  space-like virtualities, 
c       i.e.\ where $Q^2 = - m^2 > 0$, one puts \ttt{P(I,5)}$ = -Q$.
c
c V(I,1) $x$ position of production vertex, in cm.
c V(I,2) $y$ position of production vertex, in cm.
c V(I,3) $z$ position of production vertex, in cm.
c V(I,4) time of production, in mm/$c$ ($\approx 3.33 \times 10^{-12}$ s).
c V(I,5) proper lifetime of particle, in mm/$c$
c      ($\approx 3.33 \times 10^{-12}$ s). 
c      If the particle is not expected to decay V(I,5)=0}.
c  A line with K(I,1)=4, i.e.\ a  particle that could have decayed,
c   but did not within the allowed region, has the proper non-zero V(I,5).
cIn the absence of electric or magnetic fields, or other
cdisturbances, the decay vertex \ttt{VP} of an unstable particle
cmay be calculated as VP(j) = V(I,j) + V(I,5)*P(I,j)/P(I,5)},
c
c
c
c
          implicit none
#include "names.inc"
#include "claslepto.inc"
          integer ifile,itotal,iparl23
          DATA ifile /0/
          DATA itotal /0/
          character*100  fname
c
          real plu
          integer jj,il,i,j,istat,elpol
          logical lFirst
          DATA lFirst /.TRUE./
          DATA elpol /1/
c
          iparl23=PARL(23)
          if(itotal.ge.IPARL23) then   ! each file correspond to number ~ xsection
           close (41)
           lFirst=.TRUE.
           ifile=ifile+1
           itotal=0
          endif

          if (lFirst) then   ! open the file
             call get_datfilename()
           if(ifile.le.9) then
             write(fname,'(A,i1,A)') datfilename(1:65),ifile,'.dat'
             write(6,'(A,i1,A)') datfilename(1:65),ifile,'.dat'
           else if(ifile.ge.10.and.ifile.le.99) then
            write(fname,'(A,i2,A)') datfilename(1:65),ifile,'.dat'
            write(6,'(A,i2,A)') datfilename(1:65),ifile,'.dat'
           else if(ifile.ge.100.and.ifile.le.999) then
            write(fname,'(A,i3,A)') datfilename(1:65),ifile,'.dat'
            write(6,'(A,i3,A)') datfilename(1:65),ifile,'.dat'
           else
            write(fname,'(A,i4,A)') datfilename(1:65),ifile,'.dat'
            write(6,'(A,i4,A)') datfilename(1:65),ifile,'.dat'
           endif 
             OPEN(unit=41,file=fname,status='unknown')
             lFirst=.FALSE.
          endif
c
                itotal=itotal+1


            if (lFirst) then   ! open the file
             OPEN(unit=41,file=datfilename,status='unknown')
             lFirst=.FALSE.
            endif
c
               if(il.eq.2) then
                 j=0
                 do i=1,N
                  if(k(i,1).lt.11) j=j+1   ! save space
                 enddo
                 write(41,111) j,PARL(1),LEQ2,LEW2,1.0*cl_pol,1.0*elpol
               else
                write(41,101) N,PARL(1),PARL(2),cl_pol,elpol,
     6                   LEX,LEYY,LEW2,LEQ2,LEU
               endif
c
 111           FORMAT(2x,I5,10F10.4)
 110           FORMAT(2x,10I10)
 112           FORMAT(2x,10F10.4)
c 101      FORMAT(2x,I10,2F4.0,2I3,5F6.3)
 101      FORMAT(2x,I10,2F4.0,2I3,5(F6.3,2x))
 102      FORMAT(2x,I3,F4.0,2I5,2I3,5F10.4,2X,5F10.4)
              do i=1,N
               if(il.eq.2) then
                 if(k(i,1).lt.11) then   ! save space
                   write(41,110) (k(i,j),j=1,2),k(k(i,3),2),i,k(i,3)
                   write(41,112) (p(i,j),j=1,5) 
                   write(41,112) 0.0,0.0,cl_zpos,0.0,0.0
                 endif 
               else
                 write (41,102) i,plu(i,6),(k(i,j),j=1,4),(P(i,j),j=1,5)
     6                                                   ,(V(i,j)*0.1,j=1,3)
               endif
              enddo
          return
          end
c
c
c
c
c
C***********************************************************************
      SUBROUTINE goodevent(j)
C***********************************************************************
       implicit none
#include "ntupgkoko.inc"
#include "names.inc"
       integer j,iaccept,eg1amcaccept
       real pi4(4),qiu4(4),el04(4),elf4(4),tnorm(4)
       real pi,anu,pien,amp,ebeam,lambda
       real pro4(4),tnorm2(4)
       real vmass,vangle,vdotm,zpi,zka
       real gt,gmismasrho,gphistar,pimis
c
       clasdisOK=.FALSE.
       pi=acos(-1.0)
       amp=0.93827
       ebeam=cl_beam_energy
       anu=ebeam-gelee
c
        iaccept=eg1amcaccept(cl_beam_energy,gelee,gelet,gelef)
           if(acceg1.and.iaccept.eq.0) then
            return
           endif

          SELECT CASE (cl_pid)  
           CASE (0)   ! write all events
            clasdisOK=.TRUE.
           CASE (211) ! pi+
            zpi=gpipe/(ebeam-gelee)
            if(zpi.gt.cl_zmin) clasdisOK=.TRUE.
           CASE (-211) ! pi+
            zpi=gpime/(ebeam-gelee)
            if(zpi.gt.cl_zmin) clasdisOK=.TRUE.
           CASE (111) ! pi0
            if(gnpi0.gt.0) clasdisOK=.TRUE.
           CASE (3122) ! Lambda
          if(gnpim.gt.0.and.gnpro.gt.0.and.lambda(1,ebeam).lt.1.2) then
             clasdisOK=.TRUE.
          endif
           CASE (9211) ! Exclusive pi+
          if(gnpip.eq.1.and.gmismasrho(1).lt.0.95) then
             clasdisOK=.TRUE.
          endif
          CASE DEFAULT
          END SELECT       

         return
         end

c
        real function  PolarTheta(vx,vy,vz)
       implicit none
        real vx,vy,vz,pmod,theta
        pmod=vx*vx+vy*vy+vz*vz
        if(pmod .gt. 0 ) then
         theta=acos(vz/sqrt(pmod))
        else
         theta=-100
        endif
        PolarTheta=theta
        return
        end

        real function Azimuthalphi(vx,vy )
      implicit none
        real pi
        parameter ( PI=3.1415926)
        real vx,vy,pmod,phi,cosf
        pmod=vx*vx+vy*vy
        if(pmod .gt. 0 ) then
         pmod=sqrt(pmod)
         cosf=vx/pmod
        else
         cosf=1.0
        endif
        if(abs(cosf) .le. 1.0) phi=acos(cosf);
        if(vy .lt. 0.0) phi= 2*PI-phi;
        Azimuthalphi=phi
        return
        end
c
       subroutine fillevent(j)
       implicit none
#include "names.inc"
#include "ntupgkoko.inc"
#include "claslepto.inc"
c
       real amp,el04(4),elf4(4),pro4(4),qiu4(4)
       parameter (amp=0.938)
       integer part,part_id,parent_id
       real cx,cy,cz,theta,phi,pmom,px,py,pz,pener
       real Azimuthalphi,PolarTheta
       real anu,gw2,gpi0u,pi0x,pi0y,pi0z
       integer j,lund2geantid
       part_id=lund2geantid(k(j,2))
       parent_id=k(k(j,3),2)
       px=p(j,1)
       py=p(j,2)
       pz=p(j,3)
       pmom=sqrt(px*px+py*py+pz*pz)
       pener=sqrt(p(j,5)*p(j,5)+pmom*pmom)
       geleh=cl_pol
c
       if(pmom.gt.0) then
        cz=pz/pmom
        cx=px/pmom
        cy=py/pmom
       else
        cx=0
        cy=0
        cz=0
       endif

        if(cz.le.1.0) then
           theta=acos(cz)
        else
           theta=0
        endif
        phi=Azimuthalphi(px,py) !atan2(cy,cx)
c        print *,PolarTheta(px,py,pz),theta,Azimuthalphi(px,py),phi,gnele
       
       SELECT CASE (part_id)  
        CASE (3) ! electron
         gnele=gnele+1
         if(gnele.eq.1 .or. gelee.lt.pener ) then
          gelee=pener
          gelet=theta
          gelef=phi
          gelex=gelee
          geley=gelet
          gelez=gelef
          if(cl_smear.eq.1) call smear(part_id,gelee,gelet,gelef)  ! smear 
          anu=cl_beam_energy-gelee
          gq2=2.*cl_beam_energy*gelee*(1.0-cos(gelet))
          gw2=amp*amp+2.0*amp*anu-gq2
          if(gw2.gt.0) then
            gw=sqrt(gw2)
          else
            gw=0
          endif
        el04(1)=0
        el04(2)=0
        el04(3)=cl_beam_energy
        el04(4)=cl_beam_energy
c
        elf4(1)=gelee*sin(gelet)*cos(gelef)
        elf4(2)=gelee*sin(gelet)*sin(gelef)
        elf4(3)=gelee*cos(gelet)
        elf4(4)=gelee
         call vdifm(el04,elf4,qiu4,4)
c        print *,'q2',gq2,vdotm(qiu4,qiu4,4)          
          gxb=gq2/2.0/amp/anu

         endif 
        CASE (9) ! pi-
         gnpim=gnpim+1
         if(gnpim.eq.1 .or. gpime.lt.pener) then
          gpime=pener
          gpimt=theta
          gpimf=phi
          gelewgt=parent_id
          if(cl_smear.eq.1) call smear(part_id,gpime,gpimt,gpimf)  ! smear
         endif 

        CASE (14) ! proton
         gnpro=gnpro+1
         if(gnpro.eq.1 .or. gproe.lt.pener) then
          gproe=pener
          gprot=theta
          gprof=phi
          if(cl_smear.eq.1) call smear(part_id,gproe,gprot,gprof)  ! smear           
         endif 

        CASE (8) ! pi+
         gnpip=gnpip+1
         if(gnpip.eq.1 .or. gpipe.lt.pener) then
          gpipe=pener
          gpipt=theta
          gpipf=phi
          gpipx=gpipe
          gpipy=gpipt
          gpipz=gpipf
          if(cl_smear.eq.1) call smear(part_id,gpipe,gpipt,gpipf)  ! smear 
          gelebits=parent_id
c          if(gelebits.eq.213) call lulist(2)
         endif 

        CASE (11) ! K+
         gnneu=gnneu+1
         if(gnneu.eq.1 .or. gneue.lt.pener ) then
          gneue=pener
          gneut=theta
          gneuf=phi
          if(cl_smear.eq.1) call smear(part_id,gneue,gneut,gneuf)  ! smear 
          ghpar=parent_id
c          if(geleh.eq.313) call lulist(2)
         endif 

        CASE (7) ! pi0
         gpi0u=gpi0u+1
         if(gnpi0.eq.1 .or. gpi0e.lt.pener ) then
          gpi0e=pener
          gpi0t=theta
          gpi0f=phi
         endif 

        CASE (1) ! gamma
         gngam=gngam+1
         if(gngam.eq.1 .or. ggame.lt.pener) then
           if(gngam.gt.1) then
            ggam2e=ggame
            ggam2t=ggamt
            ggam2f=ggamf
           endif
          ggame=pener
          ggamt=theta
          ggamf=phi
         endif
         if(gngam.gt.1) then
          gnpi0=1
          gpi0e=ggam2e+ggame
          pi0x=game*sin(gamt)*cos(gamf)+gam2e*sin(gam2t)*cos(gam2f)
          pi0y=game*sin(gamt)*sin(gamf)+gam2e*sin(gam2t)*sin(gam2f)
          pi0z=game*cos(gamt)+gam2e*cos(gam2t)
          pi0f=Azimuthalphi(pi0x,pi0y)
          pi0t=PolarTheta(pi0x,pi0y,pi0z)
         endif

        CASE DEFAULT

       END SELECT       
         return
         end

          subroutine smear(pid,e,t,f)
          IMPLICIT NONE
#include "names.inc"
          integer pid
          real p,e,t,f,rn1,rn2,rn3,rn4,sin6
          real dts,ps,es,ts,fs
           es=e
           ts=t
           fs=f
           sin6=9.6                  ! sin6^o
           call rannor(rn1,rn2)
           call rannor(rn3,rn4)
          SELECT CASE (pid)  
          CASE (3) ! electron
           p=e
          CASE (14) ! proton
           p=e*e-0.83827**2
           if(p.gt.0) then 
            p=sqrt(p)
           else
            p=0
           endif
          CASE (8) ! pi+
           p=e*e-0.139**2
           if(p.gt.0) then 
            p=sqrt(p)
           else
            p=0
           endif
          CASE (9) ! pi-
           p=e*e-0.139**2
           if(p.gt.0) then 
            p=sqrt(p)
           else
            p=0
           endif
          CASE (11) ! K+
           p=e*e-0.494**2
           if(p.gt.0) then 
            p=sqrt(p)
           else
            p=0
           endif

          CASE DEFAULT
          END SELECT       

           ps=p+(cl_sma+cl_smb*p)*p*rn1
           dts=(cl_smd+cl_smc/p)*rn2
           ts=t+dts
           fs=f+dts/sin6*rn3
           
          SELECT CASE (pid)  
          CASE (3) ! electron
           es=ps
          CASE (14) ! proton
           es=sqrt(ps*ps+0.83827**2)
          CASE (8) ! pi+
           es=sqrt(ps*ps+0.139**2)
          CASE (9) ! pi-
           es=sqrt(ps*ps+0.139**2)
          CASE (11) ! K+
           es=sqrt(ps*ps+0.494**2)
          CASE DEFAULT
          END SELECT       
           e=es
           f=fs
           t=ts
          return
          end


      INTEGER FUNCTION LUND2GEANTID(I)
      PARAMETER (NSEL=44)
      INTEGER IPLUND(NSEL),IGE
      DATA IPLUND/
     +    22,   -11,    11,    12,   -13,    13,   111,   211,
     +  -211,   130,   321,  -321,  2112,  2212, -2212,   310,
     +   221,  3122,  3222,  3212,  3112,  3322,  3312,  3334,
     + -2112, -3122, -3112, -3212, -3222, -3322, -3312, -3334,
     +   -15,    15,   411,  -411,   421,  -421,   431,  -431,
     +  4122,    24,   -24,    23/

       DO IGE=1,NSEL
        IF(I.EQ.IPLUND(IGE)) THEN
         LUND2GEANTID=IGE
         RETURN
        ENDIF
       ENDDO
         LUND2GEANTID=0 
       RETURN
       END
c
      INTEGER FUNCTION GEANT2LUNDID(I)
      PARAMETER (NSEL=44)
      INTEGER IPLUND(NSEL),I
      DATA IPLUND/
     +    22,   -11,    11,    12,   -13,    13,   111,   211,
     +  -211,   130,   321,  -321,  2112,  2212, -2212,   310,
     +   221,  3122,  3222,  3212,  3112,  3322,  3312,  3334,
     + -2112, -3122, -3112, -3212, -3222, -3322, -3312, -3334,
     +   -15,    15,   411,  -411,   421,  -421,   431,  -431,
     +  4122,    24,   -24,    23/

      IF(I.GT.0.AND.I.LE.NSEL) THEN
       GEANT2LUNDID=IPLUND(I)
      ELSE
       GEANT2LUNDID=0
      ENDIF 
       RETURN
      END

C********************************************************************* 

       real function gmismasrho(ip)
       implicit none
#include "ntupgkoko.inc"
#include "names.inc"
       integer ip
c
c      calculating azimuthal angle from lab variables
c      of hadron  
c
c      input variables: pie,pit,pif-momentum,theta,phi of hadron
c                       posq2,pose,posf-Q^2,momentum,phi of positron                 
c
c     CALL CROSS(A,B,C) C=[AxB]
c     VDOT(A,B,N), CALL VMUL(A,B,X,N) X_i=A_i.B_i  N=3  reals
c     VDOTN(A,B,N)=ab/|a||b|
c     VMOD (A,N)  =|a|
       real pi,tetgam,anu,pien,beam
       real eleq,pitg1,cospff1
       real xm,amp,tetgam1,elety,ebeam
       real pi4(4),qiu4(4),el04(4),elf4(4),tnorm(4)
       real pro4(4),tnorm2(4)
       real vmass,vangle,vdotm,phigstar
       pi=acos(-1.0)

        amp=0.93827
        ebeam=cl_beam_energy

c
c     define all 4momenta
c
       if(ip.eq.1) then
c
c      pi+
c
        if(gnpip.eq.0) print *,'Bad pip'
         pien=sqrt(gpipe*gpipe-0.139*0.139)
         pi4(4)=gpipe
         pi4(1)=pien*cos(gpipf)*sin(gpipt) 
         pi4(2)=pien*sin(gpipf)*sin(gpipt)
         pi4(3)=pien*cos(gpipt)
c
       elseif(ip.eq.-1) then
c      pi-
c
        if(gnpim.eq.0) print *,'Bad pim'
        pien=sqrt(pien*pien-0.139*0.139)
        pi4(4)=gpime
        pi4(1)=pien*cos(gpimf)*sin(gpimt) 
        pi4(2)=pien*sin(gpimf)*sin(gpimt)
        pi4(3)=pien*cos(gpimt)
c
       elseif(ip.eq.0) then
c      pi0
c
        if(gnpi0.eq.0) print *,'Bad pi0'
        pien=sqrt(gpi0e*gpi0e-0.135*0.135)
        pi4(4)=gpi0e
        pi4(1)=pien*cos(gpi0f)*sin(gpi0t) 
        pi4(2)=pien*sin(gpi0f)*sin(gpi0t)
        pi4(3)=pien*cos(gpi0t)
c
       elseif(ip.eq.2) then
c      real gamma
c
        if(gngam.eq.0) print *,'Bad gamma'
        pien=ggame
        pi4(4)=pien
        pi4(1)=pien*cos(ggamf)*sin(ggamt) 
        pi4(2)=pien*sin(ggamf)*sin(ggamt)
        pi4(3)=pien*cos(ggamt)
c

       elseif(ip.eq.4) then
c      proton
c
        if(gnpro.eq.0) print *,'Bad pro'
        pien=sqrt(pien*pien-amp*amp)
        pi4(4)=gproe
        pi4(1)=pien*cos(gprof)*sin(gprot) 
        pi4(2)=pien*sin(gprof)*sin(gprot)
        pi4(3)=pien*cos(gprot)
c
c
       elseif(ip.eq.11) then
c      K+ 
c
        if(gnneu.eq.0) print *,'Bad K+',gnneu
        pien=sqrt(gneue*gneue-0.244)
        pi4(4)=gneue
        pi4(1)=pien*cos(gneuf)*sin(gneut) 
        pi4(2)=pien*sin(gneuf)*sin(gneut)
        pi4(3)=pien*cos(gneut)
       endif

c       Now el and gamma
c
c
c       el0
c
        el04(1)=0
        el04(2)=0
        el04(3)=ebeam
        el04(4)=ebeam
        elf4(1)=gelee*sin(gelet)*cos(gelef)
        elf4(2)=gelee*sin(gelet)*sin(gelef)
        elf4(3)=gelee*cos(gelet)
        elf4(4)=gelee
c
c     gamma*
c
c proton
c
       pro4(4)=amp
       pro4(3)=0
       pro4(2)=0
       pro4(1)=0
c
c
         call vdifm(el04,elf4,qiu4,4)
c
         call vsumm(qiu4,pro4,tnorm,4)
         call vdifm(tnorm,pi4,tnorm2,4)
         gmismasrho=vmass(tnorm2)
         return
         end


c
       subroutine vsumm(a,b,c,n)
       real a(n),b(n),c(n)
       integer i,n
        do i=1,n
          c(i)=a(i)+b(i)
        enddo
       return
       end
c
       subroutine vdifm(a,b,c,n)
       real a(n),b(n),c(n)
       integer i,n
        do i=1,n
          c(i)=a(i)-b(i)
        enddo
       return
       end
c
c
       real function vdotm(a,b,n)
       real a(n),b(n),s
       integer i,n
       s=0.0
       do i=1,3
         s=s+a(i)*b(i)
       enddo
       if(n.eq.4) s=s-a(n)*b(n)
       vdotm=s
       return
       end
c   
       real function vangle(a,b,c,d)
       real a(3),b(3),c(3),d(3),xm,ym,vcos
       real x(3),y(3),pi
       pi=acos(-1.0)
       call crossm(a,b,x)
       call crossm(c,d,y)
       xm=vdotm(x,x,3)
       ym=vdotm(y,y,3)
       if(xm.gt.0.0 .and. ym.gt.0.0) then
         vcos=vdotm(x,y,3)/sqrt(xm)/sqrt(ym)
         if(abs(vcos).lt.1.0) then
            vangle=acos(vcos)
         else
            if(vcos.ge.1.0)  vangle=0
            if(vcos.le.-1.0)  vangle=pi
         endif 
       else
         vangle=0
       endif
       return
       end
c
       subroutine crossm(a,b,c)
       real a(3),b(3),c(3)
       c(1)=a(2)*b(3)-a(3)*b(2)
       c(2)=a(3)*b(1)-a(1)*b(3)
       c(3)=a(1)*b(2)-a(2)*b(1)
       return
       end
c
c
       real function vmass(a)
       real vm
        vm= vdotm(a,a,4)
        if (vm.lt.0.0) then
          vmass=sqrt(-vm)
        else
          vmass=-1.0
        endif 
       return
       end



      integer function length(str)
c     return the string length without the blanks characters

      implicit integer (k-l)
      character *(*) str    
      
      lmax=len(str)      
       
c     search the last non blank character
      do i=lmax,1,-1
      if(str(i:i).ne.' ')then
      length=i
      return
      end if
      end do

      length=lmax

      return
      end     

      real function valnum(str)
c     return the real value contained into a string

      implicit integer (k-l)
      character *(*) str
      logical segno
      

      segno=.false.
      valnum=0.00
      lu=length(str)
      
c     check the number sign
      if(str(1:1).eq.'-')then
      segno=.true.
      str=str(2:lu)
      lu=lu-1
      end if

c     check if number is float or integer
      if(index(str,'.').ne.0)then
      iin=index(str,'.')-1
      else
      iin=lu
      end if

      ifr=lu-(iin+1)

c     translate the integer portion
      doi=1,iin  
      k=ichar(str(i:i))-48
      valnum=valnum+float(k)*10.00**float(iin-i)
      end do         

      if(iin.eq.lu)goto 10
      str=str(iin+2:lu)

c     translate the decimal portion
      doi=1,ifr  
      k=ichar(str(i:i))-48
      valnum=valnum+float(k)/10.00**float(i)
      end do

10    if(segno)valnum=-valnum

      return
      end     





