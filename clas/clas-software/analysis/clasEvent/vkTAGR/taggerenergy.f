      SUBROUTINE taggerenergy(RUN,Energy0, Ecorr)
      IMPLICIT NONE

      REAL    Ecorr
      REAL    Energy0
      INTEGER RUN
      INTEGER RUNNUMBER
      REAL    XBEAM, YBEAM, ZBEAM
      REAL    EBEAM0, EBEAM1, EBEAM2

      REAL KAVG(767), ELECTRON_ENERGY

      COMMON/BEAMPOSITION/ RUNNUMBER,XBEAM, YBEAM, ZBEAM
     *    , KAVG, ELECTRON_ENERGY
     *    , EBEAM0, EBEAM1, EBEAM2

C #include "beamposition.inc"
      
      REAL    RUNLIST(1000),RATIO(1000)
      REAL    EB(1000), EB_to_EB0(1000)
      REAL    DUMMY(3),MPROTON,RAT,DM,DE
      REAL E_ID
      INTEGER  id_tagger
      EXTERNAL id_tagger
      INTEGER I,J,NUM_OF_RUNS,II, NUM_OF_EB
      LOGICAL FIRST
      CHARACTER*132 FILE,line
      REAL XX,X1,X2,Y1,Y2,E0
      DATA FIRST /.TRUE./
      DATA MPROTON / 0.938272/

      IF    (RUN.LT.44108) THEN
          ELECTRON_ENERGY=4.01860
      ELSE
          ELECTRON_ENERGY=5.02114
      ENDIF

      IF(FIRST) THEN
         FIRST=.FALSE.
C
C  READ ELECTRON BEAMENERGY CORRECTIONS AS A FUNCTION OF RUNNUMBER
C  THE CORRECTION IS THE RATIO OF EBAM1/EBEAM0, WHERE EBEAM0 - IS THE COOKING
C  ENERGY AND EBEAM1 IS THE CURRENT BEAM ENERGY FOR EVERY RUN.
C  THE CORRCTION BASED ON THE MISSING MASS SPECTRUM 
C  GP--> PI+ PI- (PROTON)
C
 
C         OPEN(UNIT=20,FILE=
C     *    '/home/bellis/clas/builds/packages/
C     *    utilities/clasEvent/vkTAGR/r_vs_runnumber.vec',
         call revinm ('CLAS_PACK','/utilities/
     *clasEvent/vkTAGR/r_vs_runnumber.vec',FILE)
         OPEN(UNIT=20,FILE=FILE ,STATUS='old',ERR=992)
C         OPEN(UNIT=20,FILE='r_vs_runnumber.vec',
         DO I=1,1000
            READ(20,*,END=990) RUNLIST(I),DUMMY,RATIO(I)
            NUM_OF_RUNS=I
         ENDDO
 990     CONTINUE
         CLOSE(20)


C
C  READ THE PHOTON ENERGY CORRECTIONS AS A FUNCTION OF THE TAGGER_ID
C  CORRECTED RUN BY RUN BASIS (SEE THE PREVIOUS CORRECTION)
C 
C         OPEN(UNIT=20,FILE=
C     *     '/home/bellis/clas/builds/packages/
C     *     utilities/clasEvent/vkTAGR/r_vs_taggerid.vec',
         call revinm ('CLAS_PACK','/utilities/
     *clasEvent/vkTAGR/r_vs_taggerid.vec',FILE)
         OPEN(UNIT=20,FILE=FILE ,STATUS='old',ERR=992)
C         OPEN(UNIT=20,FILE= 'r_vs_taggerid.vec',
C     *        STATUS='old',ERR=992)
         DO I=1,1000
            READ(20,*,END=991) EB(I), EB_TO_EB0(I)
            NUM_OF_EB=I
         ENDDO
 991     CONTINUE
         CLOSE(20)

      ENDIF
C------------------------------------------------------------------------
      EBEAM0=Energy0
      IF    (RUN.LE.RUNLIST(1))            THEN
         RAT=RATIO(1)
      ELSEIF(RUN.GE.RUNLIST(NUM_OF_RUNS)) THEN
         RAT=RATIO(NUM_OF_RUNS)
      ELSE
         DO I=2,NUM_OF_RUNS
            IF(RUN.GE.RUNLIST(I-1).AND.RUN.LT.RUNLIST(I)) THEN
               Y1=RATIO(I-1)
               Y2=RATIO(I)
               X1=RUNLIST(I-1)
               X2=RUNLIST(I)
               XX=RUN
               RAT=y1+(y2-y1)/(x2-x1)*(xx-x1)
               GOTO 1
            ENDIF
         ENDDO
      ENDIF

 1    CONTINUE

c       EBEAM1=EBEAM*1.0039*RAT
       EBEAM1=EBEAM0*RAT
 
c      CALL INTERPOLATION(EBEAM1, NUM_OF_EB, EB, EB_TO_EB0, EBEAM2)
      E_ID=REAL(id_tagger(EBEAM0/ELECTRON_ENERGY))
      CALL INTERPOLATION(E_ID, NUM_OF_EB, EB, EB_TO_EB0, RAT)
      EBEAM2=EBEAM1*RAT
*      print *, num_of_eb,rat,ratio(1),runlist(1),ebeam0,ebeam1, ebeam2
c      taggerenergy=Ebeam2
      Ecorr=Ebeam2
      RETURN

 992  CONTINUE
      PRINT *, '1 ERROR WHEN OPENING FILE ', FILE
      STOP
      END

C-------------------------------------------------------

      SUBROUTINE INTERPOLATION(E1, N, E, D, E2)
      IMPLICIT NONE
      REAL E1, E(*), D(*), E2, DM, EM, DE
      INTEGER N, I, KE

      REAL XX,X1,X2,Y1,Y2

      IF    (E1.LE.E(1)) THEN
        DM=D(1)
      ELSEIF(E1.GE.E(N)) THEN
        DM=D(N)
      ELSE
        DO I=2,N
          IF(E1.GE.E(I-1).AND.E1.LT.E(I)) THEN
             Y1=D(I-1)
             Y2=D(I)
             X1=E(I-1)
             X2=E(I)
             XX=E1
             DM=y1+(y2-y1)/(x2-x1)*(xx-x1)
             GOTO 1
          ENDIF
        ENDDO
      ENDIF

 1    CONTINUE
      E2=DM
c      print *, e1,e2,de,dm,em,d(1),e(1),d(n),e(n)
      RETURN
      END

C------------------------------------------------------------

      integer function id_tagger(R)
C
C     R=PHOTON_ENERGY/ELECTRON_ENERGY
C     PHOTON_ENERGY IS THE ENERGY WITHOUT ANY CORRECTION  
C     ELECTRON_ENERGY IS THE COOKING ELECTRON ENERGY
C

      implicit none
C      SAVE
      INTEGER RUNNUMBER
      REAL    XBEAM, YBEAM, ZBEAM
      REAL    EBEAM0, EBEAM1, EBEAM2

      REAL KAVG(767), ELECTRON_ENERGY

      COMMON/BEAMPOSITION/ RUNNUMBER,XBEAM, YBEAM, ZBEAM
     *    , KAVG, ELECTRON_ENERGY
     *    , EBEAM0, EBEAM1, EBEAM2
C #include "beamposition.inc"      
      real R
      integer I,J
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      REAL E0
      CHARACTER*132 FILE,line

      IF(FIRST) THEN
         FIRST=.FALSE.
C
C  READ THE TABLE THAT WAS USED DURING THE COOKING FOR THE CALCULATION OF THE
C  PHOTON BEAM ENERGY. THE PHOTON ENERGY IS
C      E_PHOTON= R(E_ID) * ELECTRON_BEAM_ENERGY
C  THE COEFFICIENTS R(E_ID) ARE THE FUNCTION OF E_ID AND 
C  ELECTRON ENERGY DUE TO THE NONLINEAR EFFECTS,
C  THESE TABLE WILL BE USED FOR THE INVERT CALCULATIONS OF THE
C  E_ID FOR THE IMPLEMENTATION OF THE PHOTON ENERGY CORRECTIONS.
C  The file tage-boundaries was copied from the /group/clas/PARMS dir

C      OPEN(UNIT=20,FILE=
C     *    '/home/bellis/clas/builds/packages/
C     *    utilities/clasEvent/vkTAGR/tageboundariesall.dat',
         call revinm ('CLAS_PACK','/utilities/
     *clasEvent/vkTAGR/tageboundariesall.dat',FILE)
         OPEN(UNIT=20,FILE=FILE ,STATUS='old',ERR=992)
C      OPEN(UNIT=20,FILE='tageboundariesall.dat',
C     *  STATUS='old',ERR=992)
         DO I=1,11
            READ(20,100) line
 100        FORMAT(a80)
c            PRINT 100, line
         ENDDO
 
         DO I=1,1000000
            READ(20,100) line
c            print *,line
            READ(line,101,END=992) E0
 101        FORMAT(7X,F8.4)
            READ(20,100) line
            READ(20,100) line
            READ(20,100) line
            READ(20,100) line
            READ(20,100) line
c            PRINT *,'E0= ',E0, ELECTRON_ENERGY
               DO J=1,767
                  READ(20,100) line
c                  print *,line
                 READ(line,102,END=992) KAVG(J)
 102             FORMAT(28X,F7.5)
c                IF(E0.GE.ELECTRON_ENERGY) THEN
c                   PRINT *,j,kavg(J)
c                ENDIF
               ENDDO
               IF(E0.GE.ELECTRON_ENERGY) GOTO 103
         ENDDO
 103     CONTINUE
         CLOSE(20)
      ENDIF
c-------------------------------------------------------------------c

      Do i=1,767
         if(R.GE.kavg(I)) THEN
            id_tagger=I
            return
         endif
      enddo
      return
 992  CONTINUE
      PRINT *, '2 ERROR WHEN OPENING FILE ', FILE
      STOP
      end

            
