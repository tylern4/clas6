/*      subroutine TRIP */

/*_begin_doc
 *  Documentation for subroutine TRIP
 *
 *  Purpose:
 *  --------
 *
 *  Calling Sequence:
 *  ----------------
 *
 *  Input Parameters:  (Name - Type - Meaning)
 *  ----------------
 *
 *  Output Parameters:  (Name - Type - Meaning)
 *  -----------------
 *
 *  Called from:
 *  ------------
 *
 *  Other routines:
 *  ---------------
 *
 *  Notes:
 *  ------
 *
 *  Author:   Eugene Pasyuk      Created:  Wed Aug 15 11:27:42 EDT 2001
 *  -------
 *
 *  Major revisions:
 *  ----------------
 *
 *
 *_end_doc
 */


/*_begin_inc
 *  include files :
 *  ---------------------
 */

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <utility.h>
#include <trip.h>
#include <tripGlobal.h>

/* _end_inc */




/* local static variables */

static int first_time = 0;
static int read_counter = 0;
static int last_read = 0; /* 0=phys event. 1=scaler event */
static int previous_trip = 0;
static int end_tripfile = 0;
FILE *fp_trip = NULL;


/*   Module information: */
#define USE(var) static void * use_##var =  (void *) &var
static char *crcsid = "$Id: ftrip.c,v 1.7 2002/02/12 22:12:31 pasyuk Exp $";
USE(crcsid);


/*
 *  executable code for TRIP:
 *  -------------------------------------
*/


/*------------------------------------*/
void open_trip_file(char *file_trip)
{
    /* this function opens file with trip information */
    int nread;
    fp_trip = fopen(file_trip, "r");

    if(fp_trip) {
        fprintf(stderr,"\n Trip input file = %s \n",file_trip);
        /* get first record */
        nread=fscanf(fp_trip,"%d %d %d %d %d %d %d %f %f %u %u %f", \
                     &trip_data_.scaler_event_counter, \
                     &trip_data_.scaler_current, \
                     &trip_data_.trip, \
                     &trip_data_.first_in_scaler_interval, \
                     &trip_data_.last_in_scaler_interval, \
                     &trip_data_.physics_event_counter, \
                     &trip_data_.scaler_interval, \
                     &trip_data_.diff_average, \
                     &trip_data_.diff_scaler_based, \
                     &trip_data_.last_event_timer, \
                     &trip_data_.current_scaler_timer, \
                     &trip_data_.live_time);
        first_time = 1;

        if(read_counter != trip_data_.scaler_event_counter) {
            fprintf(stderr, " Interval number mismatch %d %d\n Exit now. \n", \
                    read_counter, trip_data_.scaler_event_counter);
            fflush(stderr);
            exit(0);
        }
        else {
            if(trip_data_.trip != 0)
                fprintf(stderr, " trip = %d Events from %d to %d are to be ignored \n",\
                        trip_data_.trip, trip_data_.first_in_scaler_interval,\
                        trip_data_.last_in_scaler_interval);

            return;
        }
    }
    else {
        fprintf(stderr,"\n Unable to open trip file: %s\n", file_trip);
        fflush(stderr);
        exit(0);
    }
}

/*--------------------------------------*/

int trip_()
{
    return(TRIP()); /* fortran wrapper */
}

/*--------------------------------------*/

int TRIP()
{
    int nread;
    clasHEAD_t *HEAD = NULL;

    if(!fp_trip) return(0); /* return "notrip" if no trip file specified */

    if(HEAD = getBank(&bcs_, "HEAD")) {
        if(HEAD->head[0].type >0 && HEAD->head[0].type <10) {
            /* got physics event */
            if(HEAD->head[0].nevent <= trip_data_.last_in_scaler_interval \
                    && HEAD->head[0].nevent >= trip_data_.first_in_scaler_interval)
                return(trip_data_.trip);
            else {
                fprintf(stderr," Something is wrong! \n");
                fprintf(stderr, "Current event number %d \n is out of current interval %d %d \n",\
                        HEAD->head[0].nevent,trip_data_.first_in_scaler_interval,\
                        trip_data_.last_in_scaler_interval);
                fflush(stderr);
                exit(0);
            }
        }


        else if( HEAD->head[0].type == 10) { /* scaler event */
            if(trip_data_.scaler_current != HEAD->head[0].nevent) {
                fprintf(stderr, "Scaler event number mismatch %d %d\n Exit now. \n", \
                        trip_data_.scaler_current, HEAD->head[0].nevent);
                fflush(stderr);
                exit(0);
            }
            else { /* need to read new record */
                if(end_tripfile) {
                    fprintf(stderr,"\n Sorry, I cannot read beyound EOF \n");
                    fflush(stderr);
                    exit(0);
                }
                else {
                    previous_trip = trip_data_.trip;
                    /* read next record from trip file */
                    nread = fscanf(fp_trip,"%d%d%d%d%d%d%d%f%f%u%u%f", \
                                   &trip_data_.scaler_event_counter, \
                                   &trip_data_.scaler_current, \
                                   &trip_data_.trip, \
                                   &trip_data_.first_in_scaler_interval, \
                                   &trip_data_.last_in_scaler_interval, \
                                   &trip_data_.physics_event_counter, \
                                   &trip_data_.scaler_interval, \
                                   &trip_data_.diff_average, \
                                   &trip_data_.diff_scaler_based, \
                                   &trip_data_.last_event_timer, \
                                   &trip_data_.current_scaler_timer, \
                                   &trip_data_.live_time);

                    if(nread != EOF) {
                        read_counter++;
                        if(read_counter != trip_data_.scaler_event_counter) {
                            fprintf(stderr, " Interval number mismatch %d %d\n Exit now. \n", \
                                    read_counter, trip_data_.scaler_event_counter);
                            fflush(stderr);
                            exit(0);
                        }
                        else {
                            if(trip_data_.trip != 0)
                                fprintf(stderr, " trip = %d Events from %d to %d are to be ignored \n",\
                                        trip_data_.trip, trip_data_.first_in_scaler_interval,\
                                        trip_data_.last_in_scaler_interval);
                            return(previous_trip);
                        }
                    }
                    else {
                        fprintf(stderr," EOF encountred fot trip file \n");
                        return(previous_trip);
                    }
                }
            }
        }
    }
}

/*--------------------------------------------------------*/

