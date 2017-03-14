#ifndef Linux
/*CMZ :  1.00/14 02/03/96  12.04.34  by  Wiesiek Dlugosz*/
#endif /* not Linux */
/*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko*/
/*-- Author :    Ralf Gerhards   16/12/94*/
 
/*************************************************************
 *                                                           *
 *    UNIX time for UNIX machines                            *
 *                                                           *
 *************************************************************/
 
#define MAC_OFFSET 0
 
#define GMT_OFFSET 0
 
#include <time.h>
#include <stdio.h>
 
int utime_()
{
#ifndef SommerTime
    return(time(NULL) - MAC_OFFSET - GMT_OFFSET);
#else
    return(time(NULL) - MAC_OFFSET - GMT_OFFSET - 3600);
#endif
}
 
 
int uctimd_(ltime)
int *ltime;
{
int iday, imonth, iyear, yymmdd;
struct tm *timecode;
time_t modtime;
 
/* Structure tm contains:
   tm_sec     seconds (0-61)
   tm_min     minutes (0-59)
   tm_hour    hours (0-23)
   tm_mday    day of month (1-31)
   tm_mon     month (0-11; january = 0 !!)
   tm_year    year (current year minus 1900)
   tm_wday    day of week (0-6; sunday = 0)
   tm_yday    day of year (0-365; january 1 = 0)
   tm_isdst   zero if daylight saving time is not in effect; else positive */
 
    modtime = *ltime + MAC_OFFSET;
    timecode = gmtime(&modtime);
    iday = timecode->tm_mday;
    imonth = timecode->tm_mon;
    iyear = timecode->tm_year;
    yymmdd = iday + 100 * (imonth+1) + 10000 * iyear;
    return(yymmdd);
}
 
int uctimt_(ltime)
int *ltime;
{
int isec, imin, ihour, hhmmss;
struct tm *timecode;
time_t modtime;
 
    modtime = *ltime + MAC_OFFSET;
    timecode = gmtime(&modtime);
    isec = timecode->tm_sec;
    imin = timecode->tm_min;
    ihour = timecode->tm_hour;
    hhmmss = isec + 100 * imin + 10000 * ihour;
    return(hhmmss);
}
 
