*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Pavel Binko   16/12/94
      SUBROUTINE TIMUYH (MODE,IDATE,ITIME,ITIMU)
*
************************************************************************
*                                                                      *
* Purpose: 1. Returns current decimal integer date, time and Unix time *
*          2. Returns decimal integer date and time from UNIX time     *
*          3. Reconstruction UNIX time from ITIME and IDATE            *
*                                                                      *
* Called by :  separately                                              *
*                                                                      *
*     Explanation of arguments:                                        *
*                                                                      *
*               MODE  = input argument with values 1,2 or 3            *
*               ITIMU = UNIX time            (9 digits)                *
*                     = seconds since 1.1.1970                         *
*               IDATE = corresponding date   (6 digits)                *
*                     = YYMMDD                                         *
*               ITIME = corresponding time   (6 digits)                *
*                     = HHMMSS                                         *
*                                                                      *
*     MODE =1:  return actual time in all three arguments              *
*                                                                      *
*     MODE =2:  translate ITIMU -> IDATE,ITIME                         *
*                         input       result                           *
*                                                                      *
*     MODE =3:  translate IDATE,ITIME -> ITIMU                         *
*                            input       result                        *
*                                                                      *
* Note:   if this routine is expanded from CMZ to call a real UNIX     *
*         time routine which needs to be provided during loading       *
*         Sofar this is foreseen for the SGI, MIPS, APOLLO and HPUX.   *
*         In all other cases the UNIX time is calculated from the      *
*         actual date (CERN routine DATIMH).                           *
*                                                                      *
* Author :  Volker Blobel          at : 05/02/92 for 1992 year         *
*           Alexander Dirner       at : 16/02/92 for period 1970-2069  *
*                                                                      *
* Source :  this procedure use algorithm from FBTIME timing routine    *
*           by author Volker Blobel, which has to be improved for      *
*           comming years after 1st jan 1970                           *
*                                                                      *
* Remark :  it should work correct for period                          *
*           from 1st jan 1979 to 31th dec 2069 only !!!                *
*
************************************************************************
*
      CHARACTER*8 NC
      INTEGER ND(2), NT(2)
      INTEGER     NDY(12)
*     Day of year from month
      DATA    NDY /00,31,59,90,120,151,181,212,243,273,304,334/
*     UNIX time on 1. january 1992
      DATA JANFIR /694224000/, IXYQ28 /31536000/, IXYQ29 /31622400/
      DATA NYEARS/0/,NMONTHS/0/,NHOURS/0/,NDAYS/0/,NSECS/0/,NMINS/0/
*
      IMODE=MODE
      ISAVE=MODE
*
    1 CONTINUE
      IF(IMODE.EQ.1) THEN
*
* Get DATE and TIME from DATIMH
*
* Get Hollerith date and time
*
          CALL DATIMH(ND,NT)
          CALL UHTOC(NT,4,NC,8)
          READ(NC(1:2),'(I2)') NHOURS
          READ(NC(4:5),'(I2)') NMINS
          READ(NC(7:8),'(I2)') NSECS
          ITIME=10000*NHOURS + 100*NMINS + NSECS
 
          CALL UHTOC(ND,4,NC,8)
          READ(NC(1:2),'(I2)') NDAYS
          READ(NC(4:5),'(I2)') NMONTHS
          READ(NC(7:8),'(I2)') NYEARS
          IDATE=10000*NYEARS + 100*NMONTHS + NDAYS
*
* Translate DATE and TIME to corresponding UNIX time
*
          ISAVE = IMODE
          IMODE = 3
*==>      IF(IMODE.EQ.3) GOTO 1
          GOTO 1
      ELSE IF(IMODE.EQ.2) THEN
*
* Translate UNIX time to corresponding DATE and TIME
*
* Returns DATE and TIME as integers from UNIX time
* It should work from 1st jan 1970 to 28. february 2100 only!!!
*
          IXTIME = ITIMU
          IF(IXTIME.LT.0) GOTO 100
* Get current year
          NYEARS = 1970
          IF(IXTIME.GE.IXYQ28) THEN
             DO 30 NYRS=1971,2070
                IF(MOD(NYRS-1,4).EQ.0) THEN
                   IF(IXTIME.LE.IXYQ29) GOTO 35
                   IXTIME = IXTIME - IXYQ29
                   IF(IXTIME.LE.IXYQ29) GOTO 40
                ELSE
                   IF(IXTIME.LE.IXYQ28) GOTO 35
                   IXTIME = IXTIME - IXYQ28
                   IF(IXTIME.LE.IXYQ28) GOTO 40
                END IF
   30        CONTINUE
   35        NYRS = NYRS - 1
   40        NYEARS = NYRS
          END IF
* Date and Time in readable form from UNIX time
          JXTIME = IXTIME
* Day of the year
          NYDAY = JXTIME/86400
          JXTIME = JXTIME - 86400*NYDAY
          NYDAY = NYDAY + 1
* Hour of day
          NDHOUR = JXTIME/3600
          JXTIME = JXTIME - 3600*NDHOUR
* Minute
          NHMIN = JXTIME/60
          JXTIME = JXTIME -60*NHMIN
* Second
          NMSEC = JXTIME
* Time in readable form
          ITIME = (NDHOUR*100+NHMIN)*100 + NMSEC
* 29. FEBRUARY not treated !!!!!
          NYDAYFX = NYDAY
          IF(MOD(NYEARS,4).EQ.0) THEN
              IF(NYDAYFX.GE.60) NYDAYFX=NYDAYFX-1
          END IF
          DO 50 NM=2,12
          IF(NYDAY.LE.NDY(NM)) GOTO 60
   50     CONTINUE
          NM = 13
   60     NYMONTH = NM - 1
          NMDAY = NYDAYFX - NDY(NYMONTH)
          IF(MOD(NYEARS,4).EQ.0) THEN
              IF(NYDAY.EQ.60) THEN
* 29. February
                  NMDAY = 29
                  NYMONTH = 2
              END IF
          END IF
* Date in readable form
          NYEAR = NYEARS - (NYEARS/100)*100
          IDATE = (NYEAR*100+NYMONTH)*100 + NMDAY
*
      ELSE IF(IMODE.EQ.3) THEN
*
* Translate DATE and TIME to corresponding UNIX time
*
* Reconstruct UNIX time from ITIME and IDATE
* It should work from 1st Jan 1970 to 31. Decembry 2069 only!!!
*
          IF(ISAVE.NE.1) THEN
              NYEARS  = IDATE/10000
              NHOURS  = ITIME/10000
              NMONTHS = (IDATE-NYEARS*10000)/100
              NMINS   = (ITIME-NHOURS*10000)/100
              NDAYS   = MOD(IDATE,100)
              NSECS   = MOD(ITIME,100)
          END IF
* Get current years
          NYEAR = 1900 + NYEARS
          IF(NYEARS.LT.70)    NYEAR=NYEAR+100
* Some tests for correct input data
          ITIMU = -1
          IF(NMONTHS.LT.1.OR.NMONTHS.GT.12) GOTO 100
          IF(NDAYS.LT.1.OR.NDAYS.GT.31)     GOTO 100
          IF(NHOURS.LT.0.OR.NHOURS.GT.24)   GOTO 100
          IF(NMINS.LT.0.OR.NMINS.GT.60)     GOTO 100
          IF(NSECS.LT.0.OR.NSECS.GT.60)     GOTO 100
* First second on day
          ITIMU = NHOURS*3600 + NMINS*60 + NSECS
* Day of years
          NYDAY = NDY(NMONTHS) + NDAYS
* 29. February ?
          IF(MOD(NYEARS,4).eq.0) THEN
             IF(NMONTHS.GT.2)   NYDAY=NYDAY+1
          END IF
* Seconds since 1. January this years
          ITIMU = (NYDAY-1)*86400 + ITIMU
* Add seconds until 1. January 1992
*         ITIMU = ITIMU + JANFIR
* Add seconds from 1. January 1992 to 1. January of current year
          IF(NYEAR.GT.1970) THEN
              DO 70 NYRS=1971,2070
                IF(MOD(NYRS-1,4).EQ.0) THEN
                    ITIMU = ITIMU + IXYQ29
                    IF(NYEAR.EQ.NYRS) GOTO 80
                ELSE
                    ITIMU = ITIMU + IXYQ28
                    IF(NYEAR.EQ.NYRS) GOTO 80
                END IF
   70        CONTINUE
          END IF
   80     CONTINUE
*
* correct time  for greenwich offset (ONLY MEZ!)
          ITIMU = ITIMU - 3600
      END IF
*
  100 RETURN
      END
