      SUBROUTINE RECSIS_MAIN
c
c  Purpose:   Provide framework for REConstruction and analySIS code on CLAS
c  --------   No bells and No whistles
c
c  Author:   Arne Freyberger      Created:  Wed Feb  8 13:20:54 EST 1995
c  -------
c
c  Major revisions:  Modified to read data from the DD ring in
c  ----------------  addition to reading from a file.
c                                          gilfoyle 7/97
c
c                    Modified the online portion to automatically
c                    reset upon detecting a change in the run
c                    number.
c                                          gilfoyle 11/97
c
      IMPLICIT NONE
      SAVE
c
c     local parameters
c
      LOGICAL LINFO, LWARN, LDEBUG, LERROR, LFATAL
      PARAMETER  (LINFO = .FALSE., LWARN = .TRUE., LDEBUG = .FALSE.,
     &     LERROR = .TRUE., LFATAL = .TRUE.)
c
c  include files :
c  ---------------------
c BOS common block  
#include "bcsparm.inc"     
c CLAS control module
#include "clasmdl.inc"     
c RUN control module (TCL variables)
#include "run_control.inc"
#include "re_units.inc"
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132 CRCSID
      DATA CRCSID/
     &'$Id: recsis_main.F,v 1.24 2008/09/09 21:22:26 turonski Exp $
     &'/
c
c  Local User defined variables:
c  -----------------------------
      INTEGER  LENOCC, ATEXIT, RFATAL, recrash 
      EXTERNAL LENOCC, ATEXIT, RFATAL, RECRASH
c
      INTEGER ISTAT, NRECIN, ITCL_ERR
      LOGICAL LFF_LOOP ! process another fpack file set
      LOGICAL LEVENT_LOOP ! loop over events in current file set
      LOGICAL LSKIP_TO_SOCKET ! skip to socket checking code
c
c  executable code for routine RECSIS_MAIN:
c  ----------------------------------------
c
c     reserve temporary unit number
c
      CALL RESLUN('recsis_main', LTMP, ISTAT)
      IF (ISTAT .NE. 1)
     &     call recmes('recsis_main','f','Could not get temporary unit number')
c
c  call f_init_tcl routine to get the pointer to the tcl interperter
c
      CALL F_INIT_TCL
      CALL SCAT_INIT_TCL
c
c     set the message level
c
      CALL SET_LEVEL(LINFO,LWARN,LDEBUG,LERROR,LFATAL)


#ifdef DEBUG
cfk      call clas_enable_fp_trap
#endif
c
c deal with command line arguments
c
      CALL RECARG
c
c  initialize some of the job control variables
c
      CALL REJOB_INFO
c
c  initialize ALL variables under control of tcl
c
      CALL RETCL_INIT(ITCL_ERR)
c
c now that hbook variables are initialized you can perform the hbook 
c initialization. for the online version do this in rebgrn se we can
c stop and start runs on the fly. gilfoyle 5/15/98
c
      if (.not. read_et) then
         CALL HBOOK_OPEN
      end if
c
c     open the log file
c
      CALL RECMOP
c
      CALL RECPU(NTRIG+1)
c
c  Initialize the BOS bank, read in DATA card....
c
      CALL RECINI
      CALL RETCL_YIELD(ITCL_ERR)

C     Comment out the following line, stop recsis from running on
C     ifarml's, with this happening only for the non-debug version.
C      call clas_enable_fp_trap
c
c  Begin the loop
c
      NRECIN = 0
      BEGJOB = .TRUE.
      LFF_LOOP = .TRUE.
      DO WHILE (LFF_LOOP)
c
c  Check to see if an input/output file has been opened.
c
         CALL RECKIN
         CALL FPACK_MESS
         CALL RECKOUT
         CALL FPACK_MESS

         CALL RECPU(NTRIG+1)

c
c get data and begin event loop after reading data from
c file (FSEQR) or from event transfer (ET).
c
         ISTAT=-1
         LEVENT_LOOP = .TRUE.
         DO WHILE (LEVENT_LOOP)
            LSKIP_TO_SOCKET = .FALSE.
            IF (READ_ET) THEN
               CALL RE_ET_GET(ISTAT, LSKIP_TO_SOCKET)
            ELSEIF( nevt_to_process.EQ.-1) THEN
               LPAUSE=.TRUE.
            ELSE
               CALL FSEQR(FCLASIN,ISTAT)
            END IF
            IF (.NOT. LSKIP_TO_SOCKET) THEN
c
c bail out if we fail to read the data.
c
               IF (ISTAT .LT. 0) THEN
                  CALL RENDRN
                  IF (LEXIT_PEND.OR. .NOT. LPAUSE_EXIT) THEN
                     LEXIT = .TRUE.
                  ELSE
                     LPAUSE = .TRUE.
                  ENDIF
                  LCLOSE = .FALSE.
                  LEVENT_LOOP = .FALSE.
               ELSE IF (ISTAT .GT. 0) THEN
                  CALL RE_RECORD(NRECIN) ! process a record.
               END IF
            ENDIF

c  check the socket every now and then

            IF (MOD(NRECIN-1,MAX(1,NTCL_CHECK)) .EQ. 0 
     1           .OR. LPAUSE .OR. LEXIT .OR. LQUIT 
     1           .OR. NRECIN .LT. 1000/NTCL_CHECK)  THEN
               
               CALL RETCL_YIELD(ITCL_ERR)
               CALL FPACK_MESS
            ENDIF

         ENDDO
c
c  End of input reached should we ask for more?
c
         LFF_LOOP = .FALSE.       ! assume that we do not want to process
                                ! another set of files
         IF (LPAUSE_EXIT) THEN
            CALL RENDDA
            LEOF = .TRUE.
            CALL RETCL_YIELD(ITCL_ERR)
            IF (.NOT. (LQUIT .OR. LEXIT)) LFF_LOOP = .TRUE.
         ENDIF
      ENDDO
c
c  Exit routines
c
      CALL RECPU(0)
      CALL RECPU(20)
      CALL RENJOB
      
c     
      RETURN
      END
