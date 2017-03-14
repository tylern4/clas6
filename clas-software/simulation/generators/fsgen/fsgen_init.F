      SUBROUTINE FSGEN_INIT(ierr)
c     ---------------
c     
c     Notes:
c     ------
c     
c     Author:    S.Stepanyan     Created:  Tue Mar 25 14:32:25 EST 1997
c     -------
c     
c     Major revisions:
c     ----------------
c     
      IMPLICIT NONE
C     
#include "leptou.inc"
#include "ludat.inc"
#include "lujet.inc"
#include "parm1.inc"
#include "parm2.inc"
#include "parm3.inc"
#include "pawc.inc"
#include "bcs.inc"
#include "gbd.inc"
C     
      INTEGER i,j,IDtargetA
      INTEGER mi,di,kc,lucomp,kk,kpc,ndec,kd(5)
      INTEGER ierr,length(20), lengthT, itagr
      REAL ULMASS
C     
      CHARACTER*10 PARTNAME(18) 
      CHARACTER*16 Tname

      CHARACTER*10 LSTEVNT,EVEDIT,DECAYEV
      CHARACTER*60 ListName
C     
      integer ijklin, ntotin, nto2in
      integer tmp1, jj
      INTEGER IOSTATUS
      INTEGER lenocc
      real ftmp1,ftmp2,ftmp3,Tcharg,charg,minm
      real r2d
      data r2d/57.29578/
c     

c     ierr=0
      CALL HLIMIT (LIMHST)
C     
c     - Read new data format
c     
c      call lulist(12)
      OPEN (unit=77,file='lund_upd.dat',status='OLD')
      call luupda(2,77)
      close(77)
c      call lulist(12)
C     
      OPEN (UNIT=LINPUT,FILE='fsgen_input',STATUS='OLD')
C     
C     read input file
C     
      INAM='NO'
      LNAM='NO'
      HNAM='NO'
      READ (LINPUT,*) MAXEVENTS
      READ (LINPUT,*) MAXFILES,STFILE
      READ (LINPUT,*) MAXITER
      READ (LINPUT,*) Ebeam
      READ (LINPUT,*) WMIN,WMAX
      READ (LINPUT,*) Q2MIN,Q2MAX
      READ (LINPUT,*) XMIN,XMAX
      READ (LINPUT,*) IDtargetA, IDtargetN, IDspect
C     get the mass
      Tmass_all=Pmas(lucomp(IDtargetA),1)
      if(IDtargetN.eq.0)then
         call luname(IDtargetA,Tname)
         lengthT=lenocc(Tname)
         IDtarget=IDtargetA
         Tmass=Tmass_all
         Tcharg=kchg(lucomp(IDtarget),1)/3.
         ptarget=0.
         etarget=tmass
         do i=1,3
            ptarg(i)=0.
         enddo
      else
         IDtarget=IDtargetN
         call luname(IDtarget,Tname)
         lengthT=lenocc(Tname)
         Tmass=Pmas(lucomp(IDtargetN),1)
         Tcharg=kchg(lucomp(IDtargetN),1)/3.
      endif
      MAXFILES=MAXFILES+STFILE-1
      READ (LINPUT,*) 
 54   FORMAT(A60)
      READ (LINPUT,54) ListName
C     
C     open output inf file
      OPEN (UNIT=LOUT,FILE=ListName,STATUS='NEW')
C     
      READ (LINPUT,*) 
 55   FORMAT(i10,A60)
      READ (LINPUT,55) bin,INAM
      READ (LINPUT,55) bout,LNAM
      WRITE (LOUT,'(a/)') '  Input File Specifications'
c     
      IF (bin.EQ.0.or.bout.eq.0) THEN
C     ********************************************************
C     Open BOS input & output files
C     ********************************************************
         CALL bos(iw,nbcs)
         print *,'Initializing BOS'
         CALL fsgen_ini_bos(ierr,STFILE)
         if(ierr.ne.0)then
            Write(LOUT,*)'Error opening BOS files'
            return
         endif
         WRITE (LOUT,*) ' Output BOS File is ',LNAM
      ENDIF
      READ (LINPUT,55) hout,HNAM
      IF ( hout.EQ.0 ) THEN
C     ********************************************************
C     Open Hbook output file and book histograms
C     ********************************************************
         CALL fsgen_hist(1,0)
         WRITE (LOUT,*) ' Output N-tuple file is',HNAM
      ENDIF
c     
      list=.false.
      decay=.false.
      edit=.false.
c    
      READ (LINPUT,*) 
      READ (LINPUT,*) NQ2
 131  format(i7)
      READ (LINPUT,*) T_SLOP, t_min, t_max
      READ (LINPUT,*) kgenev
      write(lout,*)NQ2, T_SLOP, t_min, t_max, kgenev
c     
      READ (LINPUT,*) LSTEVNT
      IF(LSTEVNT.eq.'LIST'.or.LSTEVNT.eq.'list')list=.true.
      READ (LINPUT,*) DECAYEV
      IF(DECAYEV.eq.'DECAY'.or.DECAYEV.eq.'decay')decay=.true.
      READ (LINPUT,*) EVEDIT
      IF(EVEDIT.eq.'EDIT'.or.EVEDIT.eq.'edit')edit=.true.
      READ (LINPUT,*) 
      READ (LINPUT,*) NPART
      write(lout,*)npart
c     
      charg=0.
      DO I=1,NPART
         READ (LINPUT,*) KP(I),(kd(j),j=1,5),tdep_slop(i)
         write(lout,*) KP(I),(kd(j),j=1,5),tdep_slop(i)
c     IF(KP(I).GT.100)THEN
         KC=LUCOMP(KP(I))
         charg=charg+kp(i)*kchg(kc,1)/3./abs(kp(i))
         print *,kp(i),mdcy(kc,2),mdcy(kc,2)+mdcy(kc,3)-1,mdcy(kc,1)
         minmass(i)=0.
         Do di=mdcy(kc,2),mdcy(kc,2)+mdcy(kc,3)-1
            mdme(di,1)=0
            minm=0
            do mi=1,5
               if(di.eq.kd(mi))then
                  mdme(di,1)=1
                  do jj=1,5
                    if(kfdp(di,jj).ne.0)then
                       minm=minm+pmas(lucomp(KfdP(dI,jj)),1)
                    endif
                 enddo
              endif
           enddo
           if(minm.gt.minmass(i))minmass(i)=minm
        EndDo
c     ENDIF
      ENDDO
c     
C     
C     
      DO J=1,NPART
         CALL LUNAME (KP(J),PARTNAME(J))
         length(J)=lenocc(PARTNAME(J))
      ENDDO
c
      IF(Q2MAX.eq.0..and.Q2Min.eq.0.)then
         CALL fsgen_init_tagger(Ebeam,ierr)
         if(ierr.ne.0)Write(LOUT,*)'Error initializing tagger'
      endif
c     
      WRITE (LOUT,*) ' Event Limit      = ',MAXEVENTS
      WRITE (LOUT,*) ' Incident Electron Energy    = ',Ebeam,'GeV'
      IF(Q2MAX.EQ.0.)THEN
         WRITE (LOUT,*) ' Photon Energy Range   = ',WMIN,' - ',WMAX,
     &        'GeV'
         WRITE (LOUT,*) 
         WRITE (LOUT,*) ' Reaction '
         WRITE (LOUT,*) '                   g + ',Tname(1:lengthT),
     &       ' ==> ',PARTNAME(1)(1:length(1)),(' + ',
     &       PARTNAME(KK)(1:length(kk)),KK=2,NPART)
         if(charg.ne.tcharg)then
            write(LOUT,*)'  !!=== Charge is not consrved: before =',
     &          tcharg,' after = ',charg,' ===!!'
            ierr=1
            return
         endif
      ELSE
         WRITE (LOUT,*) ' Produced Hadronic Mass Range  = ',WMIN,' - ',
     &        WMAX,'GeV'
         WRITE (LOUT,*) ' Virtual Photon Mass Range  = ',Q2MIN,' - ',
     &        Q2MAX,'GeV/c**2'
         WRITE (LOUT,*) 
         WRITE (LOUT,*) ' Reaction '
         WRITE (LOUT,*) '                   e + ',Tname(1:lengthT),
     &       ' ==> e1',(' + ',PARTNAME(KK)(1:length(kk)),KK=1,NPART)
         if(charg.ne.tcharg)then
            write(LOUT,*)'  !!=== Charge is not consrved: before =',
     &          tcharg-1.,' after = ',charg-1.,' ===!!'
            ierr=1
            return
         endif
      ENDIF
      WRITE (LOUT,*) 
      WRITE (LOUT,*) ' Number of Generated Hadronic Final State'
      WRITE (LOUT,*) ' per (Beam+Target) Interaction   = ',MAXITER
      if(list)WRITE (LOUT,*) ' Will Print List of the First 15 Events '
C'     
      READ (LINPUT,*) 
      READ (LINPUT,*) NDEC
      DO I=1,Ndec
         READ (LINPUT,*) kpc,(kd(j),j=1,5)
         KC=LUCOMP(KPc)
c         if(kd(1).gt.0)then
            print *,KPc,mdcy(kc,2),mdcy(kc,2)+mdcy(kc,3)-1
            Do di=mdcy(kc,2),mdcy(kc,2)+mdcy(kc,3)-1
               mdme(di,1)=0
               do mi=1,5
                  if(di.eq.kd(mi))mdme(di,1)=1
               enddo
            EndDo
c         endif
      ENDDO
      mstu(14)=5
      
c     check to see if there is seed information for the random number
c     generator
      ijklin=17
c     Next line expected to be blank or non-existent
c     if there are no next lines, just do the default initialization
      READ (LINPUT,*,IOSTAT=IOSTATUS,END=220)
      READ (LINPUT,*,IOSTAT=IOSTATUS,END=220)tmp1
      
      if (IOSTATUS .EQ. 0) then
         ijklin=tmp1
      endif
      
 200  call rmarin(ijklin,ntotin,nto2in)
      
      WRITE (LOUT,*) 
      WRITE (LOUT,*) ' RANMAR seeded with ',ijklin,' starting at '
     $     ,ntotin,' ',nto2in
      
c     Get the target start/stop position if it is here
      tzmin = 0.
      tzmax = 0.
c     Next line expected to be blank or non-existent
c     if there are no next lines, just do the default initialization
      READ (LINPUT,*,IOSTAT=IOSTATUS,END=220)
      READ (LINPUT,*,IOSTAT=IOSTATUS,END=220)RunNumber
c     Next line expected to be blank or non-existent
c     if there are no next lines, just do the default initialization
      READ (LINPUT,*,IOSTAT=IOSTATUS,END=220)
      READ (LINPUT,*,IOSTAT=IOSTATUS,END=220)ftmp1,ftmp2,ftmp3
      print *,'IOSTATUS ',iostatus
      if (IOSTATUS .EQ. 0) then
         tzmin = ftmp1
         tzmax = ftmp2
         txy=ftmp3
      endif
      
      WRITE (LOUT,*) 
 210  WRITE (LOUT,*) ' Target z goes from ',tzmin,' to ',tzmax,' cm.'
      WRITE (LOUT,*) 
      READ (LINPUT,*,IOSTAT=IOSTATUS,END=220)
      READ (LINPUT,*,IOSTAT=IOSTATUS,END=220)itagr
      ltagr=.false.
      if(itagr.eq.0)ltagr=.true.
      print *,'Write TAGR bank ',ltagr      
      WRITE (LOUT,*) 'Fiducal acceptance for generating particles '
c Read angular limits for charged particles, fiducial region   
      READ (LINPUT,*,IOSTAT=IOSTATUS,END=220)
      READ (LINPUT,*,IOSTAT=IOSTATUS,END=220)NFC
      do i=1,nfc
         READ (LINPUT,*,IOSTAT=IOSTATUS,END=220)pidfc(i),pfcm(i),
     &       pfcx(i),thfcm(i),thfcx(i),phfcm(i),phfcx(i),ffc(i)
         if(ffc(i).gt.0)then
            write(Lout,*)'Fiducial region for particle ',pidfc(i),' is ',pfcm(i),pfcx(i),thfcm(i),thfcx(i),phfcm(i),phfcx(i)
         elseif(ffc(i).eq.0)then
            write(Lout,*)'No fiducial region for particle ',pidfc(i)
         elseif(ffc(i).lt.0)then
            write(Lout,*)'Fiducial region for particle ',pidfc(i),
     &          'must be given in user_ffc.F'
         endif
      enddo
      READ (LINPUT,*,IOSTAT=IOSTATUS,END=220)
      READ (LINPUT,*,IOSTAT=IOSTATUS,END=220)
      READ (LINPUT,*,IOSTAT=IOSTATUS,END=220)
      READ (LINPUT,55) txout,txnam
      if(txout.eq.0)then
      OPEN (UNIT=51,FILE=txnam,STATUS='NEW')
      endif
 220  continue
c 
      RETURN
      END
C     
      
      
      
