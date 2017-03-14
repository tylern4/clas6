*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Ralf Gerhards   16/12/94
      SUBROUTINE TIMCNV(IMODE,IDATE,ITIME,ITIMU)
*
************************************************************************
*                                                                      *
*     Subroutine to find current UNIX time and to convert UNIX time    *
*     to local time and vice versa                                     *
*                                                                      *
*     Set IOFFSET below according to the local time zone!!!            *
*                                                                      *
*     IMODE= 1 :   Return UNIX time (seconds since 1.1.1970 00:00:00   *
*                  GMT) in ITIMU and GMT plus IOFFSET sec in the form  *
*                  IDATE=YYMMDD and ITIME=HHMMSS                       *
*            2 :   Convert UNIX time ITIMU into GMT plus IOFFSET sec   *
*                  (local time without taking summer/winter shifts     *
*                  into account)                                       *
*            3 :   Convert IDATE and ITIME into UNIX time              *
*                  (this part is taken from the original TIMUYH        *
*                  routine by V.Blobel and A.Dirner)                   *
************************************************************************
*
      INTEGER UTIME, UCTIMD, UCTIMT
*
      INTEGER     NDY(12)
      DATA    NDY /00,31,59,90,120,151,181,212,243,273,304,334/
      DATA    IXYQ28 /31536000/, IXYQ29 /31622400/
      SAVE NDY,IXYQ28,IXYQ29
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
* Correction for the local time zone. Change if necessary.
      PARAMETER(IOFFSET=3600)
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*
      MODE = IMODE
      IF( MODE.GT.3 ) RETURN
*
      IF(MODE.EQ.1) THEN
         ITIMU = UTIME()
      END IF
*
      IF(MODE.EQ.1 .OR. MODE.EQ.2) THEN
         ITIM  = ITIMU + IOFFSET
         IDATE = UCTIMD(ITIM)
         ITIME = UCTIMT(ITIM)
      END IF
*
      IF(MODE.EQ.3) THEN
         NYEARS  = IDATE/10000
         NHOURS  = ITIME/10000
         NMONTHS = (IDATE-NYEARS*10000)/100
         NMINS   = (ITIME-NHOURS*10000)/100
         NDAYS   = MOD(IDATE,100)
         NSECS   = MOD(ITIME,100)
* Get current years
         NYEAR = 1900 + NYEARS
         IF (NYEARS.LT.70)    NYEAR=NYEAR+100
* Some tests for correct input data
         ITIMU = -1
         IF (NMONTHS.LT.1.OR.NMONTHS.GT.12) RETURN
         IF (NDAYS.LT.1.OR.NDAYS.GT.31)     RETURN
         IF (NHOURS.LT.0.OR.NHOURS.GT.24)   RETURN
         IF (NMINS.LT.0.OR.NMINS.GT.60)     RETURN
         IF (NSECS.LT.0.OR.NSECS.GT.60)     RETURN
         MODE = 100
      ENDIF
      IF( MODE.EQ.100 ) THEN
* First second on day
         ITIMU = NHOURS*3600 + NMINS*60 + NSECS
* Day of years
         NYDAY = NDY(NMONTHS) + NDAYS
* 29. February ?
         IF(MOD(NYEAR,4).EQ.0 .AND. NMONTHS.GT.2) NYDAY=NYDAY+1
* Seconds since 1. January this year
         ITIMU = (NYDAY-1)*86400 + ITIMU
* Add seconds from 1. January 1970 to 1. January of current year
         DO 70 NYRS=1970,NYEAR-1
            IF(MOD(NYRS,4).EQ.0) THEN
               ITIMU = ITIMU + IXYQ29
            ELSE
               ITIMU = ITIMU + IXYQ28
            ENDIF
70       CONTINUE
*
* correct time  for greenwich offset
         ITIMU = ITIMU - IOFFSET
      ENDIF
      RETURN
      END
