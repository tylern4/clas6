/* gflux_util.c */
/*  $Id: gflux_util.c,v 1.13 2006/07/20 13:45:28 pasyuk Exp $    */

#ifndef lint
static char vcid[] = "$Id: gflux_util.c,v 1.13 2006/07/20 13:45:28 pasyuk Exp $";
#endif /* lint */

#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <makebanks.h>
#include <call.h>
#include <pid.h>
#include <particleType.h>
#include <math.h>
#include <bosddl.h>
#include <clas_cern.h>
#include <map_manager.h>
#include <kinematics.h>
#include <trip.h>
#include <tripGlobal.h>
#include <tag_param.h>
#include <tagtnorm.h>
#include <tagruncontrol.h>
#include <gflux.h>

extern float beginWindow;
extern float runGatedClockFreq;
extern float endWindow;
extern float tdcWindow;

extern float clock;
extern float runTime;
extern float runTimeFU;

extern int Ncharged;
extern int Npim;
extern int Npip;
extern int Npro;
extern int Nunknown;

extern int NchargedG;
extern int NpimG;
extern int NpipG;
extern int NproG;
extern int NunknownG;

extern int NnonTrip;
extern int Ns;
extern int Nstr;
extern int Ntagr;
extern int NtphoBins;
extern int Nevents;
extern int NeventsPHY;
extern int NeventsTRK;
extern int clocklt;
extern int doExclusive;
extern int doParticles;
extern int doRawTagger;
extern int fileNo;
extern int firsttime;
extern int max;
extern int normRun;
extern int normRunNo;
extern int quest_[100];
extern int runNo;
extern int startCounter;
extern int syncCor;
extern int timeline;
extern int trip;

extern uint32 syncIntervalCumulative;

extern float stolenWindow[NUM_TC];
extern float tagRatio[NUM_TC];
extern float tagRatioU[NUM_TC];

/*--------------------------------------------------------------------------*/

float binomialU(float p, float n) {

    // This function returns the error associated with a binomial variable
    // p=k/n; p: probability of sucess; k # of successes; n # of trials

    return(sqrt(p * (1. - p) / n));

}
/*--------------------------------------------------------------------------*/

void bookHistos() {

    char gpidMes[10];
    char pidMes[10];
    char normRunMes[50];
    char partMes[10];
    char runMes[50];
    char title[100];

    int hIndex;
    int i;

    sprintf(gpidMes, "GPID");
    sprintf(normRunMes, "Normalization Run %d", normRunNo);
    sprintf(partMes, "PART");
    sprintf(runMes, "Run %d", runNo);

    if(doRawTagger) {

        sprintf(title, "TAGE Raw TDC, %s", runMes);
        hbook2(1, title, NUM_EC, 0.5, NUM_EC+0.5, 512, -0.5, EC_CHANNELS-0.5, 0);

        sprintf(title, "TAGT Left Raw TDC, %s", runMes);
        hbook2(2, title, NUM_TC, 0.5, NUM_TC+0.5, 512, -0.5, TC_CHANNELS-0.5, 0);

        sprintf(title, "TAGT Right Raw TDC, %s", runMes);
        hbook2(3, title, NUM_TC, 0.5, NUM_TC+0.5, 512, -0.5, TC_CHANNELS-0.5, 0);

        sprintf(title, "Ttime-Etime vs TC, status 7, %s", runMes);
        hbook2(10, title, NUM_TC, 0.5, NUM_TC+0.5, NUM_ET_T_DIFF, ET_T_DIFF_MIN, ET_T_DIFF_MAX, 0);

        sprintf(title, "Ttime-Etime vs E-Bin, status 7, %s", runMes);
        hbook2(11, title, NUM_EB, 0.5, NUM_EB+0.5, NUM_ET_T_DIFF, ET_T_DIFF_MIN, ET_T_DIFF_MAX, 0);

        if(doParticles) {

            sprintf(title, "Ttime-Etime from good [g] vs TC, status 7, %s", runMes);
            hbook2(12, title, NUM_TC, 0.5, NUM_TC+0.5, NUM_ET_T_DIFF, ET_T_DIFF_MIN, ET_T_DIFF_MAX, 0);

            sprintf(title, "Ttime-Etime from good [g] vs E-Bin, status 7, %s", runMes);
            hbook2(13, title, NUM_EB, 0.5, NUM_EB+0.5, NUM_ET_T_DIFF, ET_T_DIFF_MIN, ET_T_DIFF_MAX, 0);
        }

        sprintf(title, "Ttime-Etime vs TC, status 15, %s", runMes);
        hbook2(20, title, NUM_TC, 0.5, NUM_TC+0.5, NUM_ET_T_DIFF, ET_T_DIFF_MIN, ET_T_DIFF_MAX, 0);

        sprintf(title, "Ttime-Etime vs E-Bin, status 15, %s", runMes);
        hbook2(21, title, NUM_EB, 0.5, NUM_EB+0.5, NUM_ET_T_DIFF, ET_T_DIFF_MIN, ET_T_DIFF_MAX, 0);

        if(doParticles) {

            sprintf(title, "Ttime-Etime from good [g] vs TC, status 15, %s", runMes);
            hbook2(22, title, NUM_TC, 0.5, NUM_TC+0.5, NUM_ET_T_DIFF, ET_T_DIFF_MIN, ET_T_DIFF_MAX, 0);

            sprintf(title, "Ttime-Etime from good [g] vs E-Bin, status 15, %s", runMes);
            hbook2(23, title, NUM_EB, 0.5, NUM_EB+0.5, NUM_ET_T_DIFF, ET_T_DIFF_MIN, ET_T_DIFF_MAX, 0);
        }

        sprintf(title, "Min TC TDC Sum Occupancy, %s", runMes);
        hbook1(30, title, NUM_TC, 0.5, NUM_TC+0.5, 0);
    } // if(doRawTagger)

    sprintf(title, "Tagger Status vs TC, %s", runMes);
    hbook2(40, title, NUM_TC, 0.5, NUM_TC+0.5, 256, 0.5, 256.5, 0);

    //-----> ET Matrix

    sprintf(title, "ET Matrix, Out of Time, %s", runMes);
    hbook2(50, title, NUM_EB, 0.5, NUM_EB+0.5, NUM_TC, 0.5, NUM_TC+0.5, 0);

    sprintf(title, "ET Matrix, status 7, %s", runMes);
    hbook2(51, title, NUM_EB, 0.5, NUM_EB+0.5, NUM_TC, 0.5, NUM_TC+0.5, 0);

    sprintf(title, "ET Matrix, status 15, %s", runMes);
    hbook2(52, title, NUM_EB, 0.5, NUM_EB+0.5, NUM_TC, 0.5, NUM_TC+0.5, 0);

    if(doParticles) {

        sprintf(title, "ET Matrix from good [g], status 7, %s", runMes);
        hbook2(53, title, NUM_EB, 0.5, NUM_EB+0.5, NUM_TC, 0.5, NUM_TC+0.5, 0);

        sprintf(title, "ET Matrix from good [g], status 15, %s", runMes);
        hbook2(54, title, NUM_EB, 0.5, NUM_EB+0.5, NUM_TC, 0.5, NUM_TC+0.5, 0);
    }

    sprintf(title, "ET Matrix (Energy Bins), Out of Time, %s", runMes);
    hbook2(60, title, NUM_ERG, ERG_MIN, ERG_MAX, NUM_TC, 0.5, NUM_TC+0.5, 0);

    sprintf(title, "ET Matrix (Energy Bins), status 7, %s", runMes);
    hbook2(61, title, NUM_ERG, ERG_MIN, ERG_MAX, NUM_TC, 0.5, NUM_TC+0.5, 0);

    sprintf(title, "ET Matrix (Energy Bins), status 15, %s", runMes);
    hbook2(62, title, NUM_ERG, ERG_MIN, ERG_MAX, NUM_TC, 0.5, NUM_TC+0.5, 0);

    sprintf(title, "ET Matrix (Energy Bins2), status 7, %s", runMes);
    hbook2(161, title, NUM_ERG2, ERG2_MIN, ERG2_MAX, NUM_TC, 0.5, NUM_TC+0.5, 0);

    sprintf(title, "ET Matrix (Energy Bins2), status 15, %s", runMes);
    hbook2(162, title, NUM_ERG2, ERG2_MIN, ERG2_MAX, NUM_TC, 0.5, NUM_TC+0.5, 0);

    if(doParticles) {

        sprintf(title, "ET Matrix (Energy Bins) from good [g], status 7, %s", runMes);
        hbook2(63, title, NUM_ERG, ERG_MIN, ERG_MAX, NUM_TC, 0.5, NUM_TC+0.5, 0);

        sprintf(title, "ET Matrix (Energy Bins) from good [g], status 15, %s", runMes);
        hbook2(64, title, NUM_ERG, ERG_MIN, ERG_MAX, NUM_TC, 0.5, NUM_TC+0.5, 0);
    }

    //-----> T counter timing spectrums

    sprintf(title, "TC vs Tpho, Out of Time, %s", runMes);
    hbook2(70, title, NtphoBins, beginWindow, endWindow, NUM_TC, 0.5, NUM_TC+0.5, 0);

    sprintf(title, "TC vs Tpho, status 7, %s", runMes);
    hbook2(71, title, NUM_TDC, TDC_MIN, TDC_MAX, NUM_TC, 0.5, NUM_TC+0.5, 0);

    sprintf(title, "TC vs Tpho, status 15,  %s", runMes);
    hbook2(72, title, NUM_TDC, TDC_MIN, TDC_MAX, NUM_TC, 0.5, NUM_TC+0.5, 0);

    sprintf(title, "TC vs Tpho, Independent of Ecounters, %s", runMes);
    hbook2(73, title, NUM_TDC, TDC_MIN, TDC_MAX, NUM_TC, 0.5, NUM_TC+0.5, 0);

    sprintf(title, "Ne^-! (Status 7 or 15) per TC, %s", runMes);
    hbook1(80, title, NUM_TC, 0.5, NUM_TC+0.5, 0);

    sprintf(title, "Ne^-! (Status 7 or 15) corrected for stolen hits per TC, %s", runMes);
    hbook1(81, title, NUM_TC, 0.5, NUM_TC+0.5, 0);

    sprintf(title, "NoutOfTime Hits per TC, First Non-Trip Scaler Interval, %s", runMes);
    hbook1(90, title, NUM_TC, 0.5, NUM_TC+0.5, 0);

    sprintf(title, "Nearly [g] per TC, First Non-Trip Scaler Interval, %s", runMes);
    hbook1(91, title, NUM_TC, 0.5, NUM_TC+0.5, 0);


    //-----> Temporary Histograms

    sprintf(title, "NoutOfTime Hits per ST, Temp, %s", runMes);
    hbook1(6666, title, NUM_ST, 0.5, NUM_ST+0.5, 0);

    sprintf(title, "NoutOfTime Hits per TC NoE, Temp, %s", runMes);
    hbook1(7777, title, NUM_TC, 0.5, NUM_TC+0.5, 0);

    sprintf(title, "NoutOfTime Hits per TC, Temp, %s", runMes);
    hbook1(8888, title, NUM_TC, 0.5, NUM_TC+0.5, 0);

    sprintf(title, "Nearly [g] per TC, Temp, %s", runMes);
    hbook1(9999, title, NUM_TC, 0.5, NUM_TC+0.5, 0);

    //-----> Timeline Histograms

    sprintf(title, "TC e^-! Rate (Status 7 or 15) per Scaler Interval, %s", runMes);
    hbook2(100, title, timeline, 0.5, timeline+0.5, NUM_TC, 0.5, NUM_TC+0.5, 0);

    sprintf(title, "Ne^-! (Status 7 or 15) per TC per Scaler Interval, %s", runMes);
    hbook2(101, title, timeline, 0.5, timeline+0.5, NUM_TC, 0.5, NUM_TC+0.5, 0);

    sprintf(title, "TC e^-! Rate (EC coincidence not necessary) per Scaler Interval, %s", runMes);
    hbook2(102, title, timeline, 0.5, timeline+0.5, NUM_TC, 0.5, NUM_TC+0.5, 0);

    sprintf(title, "TC e^-! Rate Uncertainty (EC coincidence not necessary) per Scaler Interval, %s", runMes);
    hbook2(103, title, timeline, 0.5, timeline+0.5, NUM_TC, 0.5, NUM_TC+0.5, 0);

    sprintf(title, "Nevents(Physics) per Scaler Interval, %s", runMes);
    hbook1(110, title, timeline, 0.5, timeline+0.5, 0);

    sprintf(title, "Nevents with tracks per Scaler Interval, %s", runMes);
    hbook1(111, title, timeline, 0.5, timeline+0.5, 0);

    sprintf(title, "Nevents with TAGR bank per Scaler Interval, %s", runMes);
    hbook1(112, title, timeline, 0.5, timeline+0.5, 0);

    sprintf(title, "Event Livetime per Scaler Interval, %s", runMes);
    hbook1(120, title, timeline, 0.5, timeline+0.5, 0);

    sprintf(title, "Time of Scaler Interval, %s", runMes);
    hbook1(121, title, timeline, 0.5, timeline+0.5, 0);

    sprintf(title, "Run Time of Scaler Interval, %s", runMes);
    hbook1(122, title, timeline, 0.5, timeline+0.5, 0);

    sprintf(title, "Non-Tripped Scaler Intervals, %s", runMes);
    hbook1(123, title, timeline, 0.5, timeline+0.5, 0);

    //-----> Tagging Ratio per T counter

    if(normRun) {

        sprintf(title, "TC Tagging Ratio, %s", runMes);
        hbook1(200, title, NUM_TC, 0.5, NUM_TC+0.5, 0);

        sprintf(title, "TC MOR Triggers, %s", runMes);
        hbook1(201, title, NUM_TC, 0.5, NUM_TC+0.5, 0);

        sprintf(title, "TC MOR*TAC Coincidences, %s", runMes);
        hbook1(202, title, NUM_TC, 0.5, NUM_TC+0.5, 0);

        sprintf(title, "TC Tagging Ratio, tagtnorm eq 1, %s", runMes);
        hbook1(210, title, NUM_TC, 0.5, NUM_TC+0.5, 0);

        sprintf(title, "TC MOR Triggers, tagtnorm eq 1, %s", runMes);
        hbook1(211, title, NUM_TC, 0.5, NUM_TC+0.5, 0);

        sprintf(title, "TC MOR*TAC Coincidences tagtnorm eq 1, %s", runMes);
        hbook1(212, title, NUM_TC, 0.5, NUM_TC+0.5, 0);

        sprintf(title, "tpho - tacotime vs tpho, %s", runMes);
        hbook2(231, title, NUM_TDC, TDC_MIN, TDC_MAX, 100, -50., 50., 0);

        sprintf(title, "tpho - tacotime vs tpho, tagtnorm eq 1, %s", runMes);
        hbook2(232, title, NUM_TDC, TDC_MIN, TDC_MAX, 80, -50., 50., 0);

        sprintf(title, "tagtnorm, %", runMes);
        hbook1(251, title, 20, 0., 20., 0);

        sprintf(title, "tagtnorm, tac match, %", runMes);
        hbook1(252, title, 20, 0., 20., 0);

    }
    else {

        sprintf(title, "TC Tagging Ratio, %s", normRunMes);
        hbook1(203, title, NUM_TC, 0.5, NUM_TC+0.5, 0);
    }

    sprintf(title, "Probability for a stolen Hit per TC, %s", runMes);
    hbook1(300, title, NUM_TC, 0.5, NUM_TC+0.5, 0);

    sprintf(title, "Stolen window time interval per TC, %s", runMes);
    hbook1(301, title, NUM_TC, 0.5, NUM_TC+0.5, 0);

    sprintf(title, "Average rate without E coincidence per TC, %s", runMes);
    hbook1(302, title, NUM_TC, 0.5, NUM_TC+0.5, 0);

    //-----> Start Counter

    sprintf(title, "ST pair vs ST rate from scalers per Scaler Interval, %s", runMes);
    hbook2(400, title, timeline, 0.5, timeline+0.5, 3, 0.5, 3.5, 0);

    if(startCounter) {

        sprintf(title, "ST pair vs STR rate per Scaler Interval, %s", runMes);
        hbook2(401, title, timeline, 0.5, timeline+0.5, 3, 0.5, 3.5, 0);

        sprintf(title, "ST pair vs ST1 average time, %s", runMes);
        hbook2(410, title, 260, -80., 180., 3, 0.5, 3.5, 0);

        sprintf(title, "ST pair vs STR time, %s", runMes);
        hbook2(411, title, 260, -80., 180., 3, 0.5, 3.5, 0);
    }

    //-----> Photon Flux

    sprintf(title, "N[g] per TC, %s", runMes);
    hbook1(1000, title, NUM_TC, 0.5, NUM_TC+0.5, 0);

    sprintf(title, "N[g] per E-Bin, %s", runMes);
    hbook1(1001, title, NUM_EB, 0.5, NUM_EB+0.5, 0);

    sprintf(title, "N[g] per Energy Bin, %s", runMes);
    hbook1(1002, title, NUM_ERG, ERG_MIN, ERG_MAX, 0);

    sprintf(title, "N[g] per Energy Bin2, %s", runMes);
    hbook1(1003, title, NUM_ERG2, ERG2_MIN, ERG2_MAX, 0);

    //-----> Particle based histograms

    if(doParticles) {

        for(i = 0; i < 2 ; i++) {

            if(i == 0) {
                hIndex = 0;
                strcpy(pidMes, partMes);
            }
            else {
                hIndex = 10000;
                strcpy(pidMes, gpidMes);
            }

            //-----> T counter binning

            sprintf(title, "Np vs TC, %s, %s", pidMes, runMes);
            hbook1(2000+hIndex, title, NUM_TC, 0.5, NUM_TC+0.5, 0);

            sprintf(title, "N[p]^+! vs TC, %s, %s", pidMes, runMes);
            hbook1(2001+hIndex, title, NUM_TC, 0.5, NUM_TC+0.5, 0);

            sprintf(title, "N[p]^-! vs TC, %s, %s", pidMes, runMes);
            hbook1(2002+hIndex, title, NUM_TC, 0.5, NUM_TC+0.5, 0);

            sprintf(title, "Ncharged vs TC, %s, %s", pidMes, runMes);
            hbook1(2003+hIndex, title, NUM_TC, 0.5, NUM_TC+0.5, 0);

            sprintf(title, "Nunknown vs TC, %s, %s", pidMes, runMes);
            hbook1(2004+hIndex, title, NUM_TC, 0.5, NUM_TC+0.5, 0);

            sprintf(title, "Normalized p Yield per TC, %s, %s", pidMes, runMes);
            hbook1(2010+hIndex, title, NUM_TC, 0.5, NUM_TC+0.5, 0);

            sprintf(title, "Normalized [p]^+! Yield per TC, %s, %s", pidMes, runMes);
            hbook1(2011+hIndex, title, NUM_TC, 0.5, NUM_TC+0.5, 0);

            sprintf(title, "Normalized [p]^-! Yield per TC, %s, %s", pidMes, runMes);
            hbook1(2012+hIndex, title, NUM_TC, 0.5, NUM_TC+0.5, 0);

            sprintf(title, "Normalized Charged Particle Yield per TC, %s, %s", pidMes, runMes);
            hbook1(2013+hIndex, title, NUM_TC, 0.5, NUM_TC+0.5, 0);

            sprintf(title, "Normalized Unknown Yield per TC, %s, %s", pidMes, runMes);
            hbook1(2014+hIndex, title, NUM_TC, 0.5, NUM_TC+0.5, 0);

            sprintf(title, "Ratio of p to [p]^+! Yields per TC, %s, %s", pidMes, runMes);
            hbook1(2020+hIndex, title, NUM_TC, 0.5, NUM_TC+0.5, 0);

            sprintf(title, "Ratio of [p]^-! to [p]^+! Yields per TC, %s, %s", pidMes, runMes);
            hbook1(2021+hIndex, title, NUM_TC, 0.5, NUM_TC+0.5, 0);

            sprintf(title, "Ratio of Unknown to [p]^+! Yields per TC, %s, %s", pidMes, runMes);
            hbook1(2022+hIndex, title, NUM_TC, 0.5, NUM_TC+0.5, 0);

            sprintf(title, "Mass of Charged Particles vs TC, %s, %s", pidMes, runMes);
            hbook2(2030+hIndex, title, 40, 0, 2, NUM_TC, 0.5, NUM_TC+0.5, 0);

            // Exclusive reactions only done with GPID

            if(doExclusive && i == 1) {

                sprintf(title, "Yield of [g]p ""5# [p]^+!n per TC, %s, %s", pidMes, runMes);
                hbook1(12100, title, NUM_TC, 0.5, NUM_TC+0.5, 0);

                sprintf(title, "Yield of [g]p ""5# p[p]^+![p]^-! per TC, %s, %s", pidMes, runMes);
                hbook1(12101, title, NUM_TC, 0.5, NUM_TC+0.5, 0);

                sprintf(title, "Normalized Yield of [g]p ""5# [p]^+!n per TC, %s, %s", pidMes, runMes);
                hbook1(12110, title, NUM_TC, 0.5, NUM_TC+0.5, 0);

                sprintf(title, "Normalized Yield of [g]p ""5# p[p]^+![p]^-! per TC, %s, %s", pidMes, runMes);
                hbook1(12111, title, NUM_TC, 0.5, NUM_TC+0.5, 0);
            }

            //-----> E counter binning

            sprintf(title, "Np vs Ebin, %s, %s", pidMes, runMes);
            hbook1(3000+hIndex, title, NUM_EB, 0.5, NUM_EB+0.5, 0);

            sprintf(title, "N[p]^+! vs Ebin, %s, %s", pidMes, runMes);
            hbook1(3001+hIndex, title, NUM_EB, 0.5, NUM_EB+0.5, 0);

            sprintf(title, "N[p]^-! vs Ebin, %s, %s", pidMes, runMes);
            hbook1(3002+hIndex, title, NUM_EB, 0.5, NUM_EB+0.5, 0);

            sprintf(title, "Ncharged vs Ebin, %s, %s", pidMes, runMes);
            hbook1(3003+hIndex, title, NUM_EB, 0.5, NUM_EB+0.5, 0);

            sprintf(title, "Nunknown vs Ebin, %s, %s", pidMes, runMes);
            hbook1(3004+hIndex, title, NUM_EB, 0.5, NUM_EB+0.5, 0);

            sprintf(title, "Normalized p Yield per Ebin, %s, %s", pidMes, runMes);
            hbook1(3010+hIndex, title, NUM_EB, 0.5, NUM_EB+0.5, 0);

            sprintf(title, "Normalized [p]^+! Yield per Ebin, %s, %s", pidMes, runMes);
            hbook1(3011+hIndex, title, NUM_EB, 0.5, NUM_EB+0.5, 0);

            sprintf(title, "Normalized [p]^-! Yield per Ebin, %s, %s", pidMes, runMes);
            hbook1(3012+hIndex, title, NUM_EB, 0.5, NUM_EB+0.5, 0);

            sprintf(title, "Normalized Charged Particle Yield per Ebin, %s, %s", pidMes, runMes);
            hbook1(3013+hIndex, title, NUM_EB, 0.5, NUM_EB+0.5, 0);

            sprintf(title, "Normalized Unknown Yield per Ebin, %s, %s", pidMes, runMes);
            hbook1(3014+hIndex, title, NUM_EB, 0.5, NUM_EB+0.5, 0);

            sprintf(title, "Ratio of p to [p]^+! Yields per Ebin, %s, %s", pidMes, runMes);
            hbook1(3020+hIndex, title, NUM_EB, 0.5, NUM_EB+0.5, 0);

            sprintf(title, "Ratio of [p]^-! to [p]^+! Yields per Ebin, %s, %s", pidMes, runMes);
            hbook1(3021+hIndex, title, NUM_EB, 0.5, NUM_EB+0.5, 0);

            sprintf(title, "Ratio of Unknown to [p]^+! Yields per Ebin, %s, %s", pidMes, runMes);
            hbook1(3022+hIndex, title, NUM_EB, 0.5, NUM_EB+0.5, 0);

            // Exclusive reactions only done with GPID

            if(doExclusive && i == 1) {

                sprintf(title, "Yield [g]p ""5# [p]^+!n per E-bin , %s, %s", pidMes, runMes);
                hbook1(13100, title, NUM_EB, 0.5, NUM_EB+0.5, 0);

                sprintf(title, "Yield of [g]p ""5# p[p]^+![p]^-! E-bin, %s, %s", pidMes, runMes);
                hbook1(13101, title, NUM_EB, 0.5, NUM_EB+0.5, 0);

                sprintf(title, "Normalized Yield [g]p ""5# [p]^+!n per E-bin, %s, %s", pidMes, runMes);
                hbook1(13110, title, NUM_EB, 0.5, NUM_EB+0.5, 0);

                sprintf(title, "Normalized Yield of [g]p ""5# p[p]^+![p]^-! per E-bin, %s, %s", pidMes, runMes);
                hbook1(13111, title, NUM_EB, 0.5, NUM_EB+0.5, 0);

            }

            //-----> Energy binning

            sprintf(title, "Np vs Energy Bin, %s, %s", pidMes, runMes);
            hbook1(4000+hIndex, title, NUM_ERG, ERG_MIN, ERG_MAX, 0);

            sprintf(title, "N[p]^+! vs Energy Bin, %s, %s", pidMes, runMes);
            hbook1(4001+hIndex, title, NUM_ERG, ERG_MIN, ERG_MAX, 0);

            sprintf(title, "N[p]^-! vs Energy Bin, %s, %s", pidMes, runMes);
            hbook1(4002+hIndex, title, NUM_ERG, ERG_MIN, ERG_MAX, 0);

            sprintf(title, "Ncharged vs Energy Bin, %s, %s", pidMes, runMes);
            hbook1(4003+hIndex, title, NUM_ERG, ERG_MIN, ERG_MAX, 0);

            sprintf(title, "Nunknown vs Energy Bin, %s, %s", pidMes, runMes);
            hbook1(4004+hIndex, title, NUM_ERG, ERG_MIN, ERG_MAX, 0);

            sprintf(title, "Normalized p Yield per Energy Bin, %s, %s", pidMes, runMes);
            hbook1(4010+hIndex, title, NUM_ERG, ERG_MIN, ERG_MAX, 0);

            sprintf(title, "Normalized [p]^+! Yield per Energy Bin, %s, %s", pidMes, runMes);
            hbook1(4011+hIndex, title, NUM_ERG, ERG_MIN, ERG_MAX, 0);

            sprintf(title, "Normalized [p]^-! Yield per Energy Bin, %s, %s", pidMes, runMes);
            hbook1(4012+hIndex, title, NUM_ERG, ERG_MIN, ERG_MAX, 0);

            sprintf(title, "Normalized Charged Particle Yield per Energy Bin, %s, %s", pidMes, runMes);
            hbook1(4013+hIndex, title, NUM_ERG, ERG_MIN, ERG_MAX, 0);

            sprintf(title, "Normalized Unknown Yield per Energy Bin, %s, %s", pidMes, runMes);
            hbook1(4014+hIndex, title, NUM_ERG, ERG_MIN, ERG_MAX, 0);

            sprintf(title, "Ratio of p to [p]^+! Yields per Energy Bin, %s, %s", pidMes, runMes);
            hbook1(4020+hIndex, title, NUM_ERG, ERG_MIN, ERG_MAX, 0);

            sprintf(title, "Ratio of [p]^-! to [p]^+! Yields per Energy Bin, %s, %s", pidMes, runMes);
            hbook1(4021+hIndex, title, NUM_ERG, ERG_MIN, ERG_MAX, 0);

            sprintf(title, "Ratio of Unknown to [p]^+! Yields per Energy Bin, %s, %s", pidMes, runMes);
            hbook1(4022+hIndex, title, NUM_ERG, ERG_MIN, ERG_MAX, 0);

            // Exclusive reactions only done with GPID

            if(doExclusive && i == 1) {

                sprintf(title, "Yield [g]p ""5# [p]^+!n per Energy Bin , %s", runMes);
                hbook1(14100, title, NUM_ERG, ERG_MIN, ERG_MAX, 0);

                sprintf(title, "Yield of [g]p ""5# p[p]^+![p]^-! per Energy Bin , %s", runMes);
                hbook1(14101, title, NUM_ERG, ERG_MIN, ERG_MAX, 0);

                sprintf(title, "Normalized Yield [g]p ""5# [p]^+!n  per Energy Bin , %s", runMes);
                hbook1(14110, title, NUM_ERG, ERG_MIN, ERG_MAX, 0);

                sprintf(title, "Normalized Yield of [g]p ""5# p[p]^+![p]^-! per Energy Bin , %s", runMes);
                hbook1(14111, title, NUM_ERG, ERG_MIN, ERG_MAX, 0);

                sprintf(title, "MM for [g]p ""5# [p]^+!n, %s, %s", pidMes, runMes);
                hbook1(14200, title, 1000, 0.0, 2.0, 0);

                sprintf(title, "MM^2! for [g]p ""5# p[p]^+![p]^-!, %s, %s", pidMes, runMes);
                hbook1(14201, title, 2000, -0.5, 1.5, 0);
            }

            //----->  Scaler Interval Histograms

            sprintf(title, "Np between Scaler Events, %s, %s", pidMes, runMes);
            hbook1(5000+hIndex, title, timeline, 0.5, timeline+0.5, 0);

            sprintf(title, "N[p]^+! between Scaler Events, %s, %s", pidMes, runMes);
            hbook1(5001+hIndex, title, timeline, 0.5, timeline+0.5, 0);

            sprintf(title, "N[p]^-! between Scaler Events, %s, %s", pidMes, runMes);
            hbook1(5002+hIndex, title, timeline, 0.5, timeline+0.5, 0);

            sprintf(title, "Ncharged between Scaler Events, %s, %s", pidMes, runMes);
            hbook1(5003+hIndex, title, timeline, 0.5, timeline+0.5, 0);

            sprintf(title, "Nunknown between Scaler Events, %s, %s", pidMes, runMes);
            hbook1(5004+hIndex, title, timeline, 0.5, timeline+0.5, 0);

        } // loop over pid schemes PART and GPID

    } // if(doParticles)

    return;

} // void bookHistos

/*--------------------------------------------------------------------------*/

void cleanUp() {

    char mess[100];
    int icycle;

    sprintf(mess, "CLOSE BOSOUTPUT UNIT=7");
    fparm_c(mess);

    hdelet(6666);
    hdelet(7777);
    hdelet(8888);
    hdelet(9999);
    hrout(0, icycle, "T");
    hrend("esr");

    return;

} // cleanUp

/*--------------------------------------------------------------------------*/

void ctrlCHandle(int x) {

    signal(SIGINT, ctrlCHandle);
    signal(SIGHUP, ctrlCHandle);
    fprintf(stderr, "\n\n\t\t\t***  INTERRUPTED!!!  ***\n\n");
    max = 1;

    return;

} // ctrlCHandle

/*--------------------------------------------------------------------------*/

void fillHistos() {

    clasBID_t  *TBID = getBank(&bcs_, "TBID");
    clasGPID_t *GPID = getBank(&bcs_, "GPID");
    clasMVRT_t *MVRT = getBank(&bcs_, "MVRT");
    clasPART_t *PART = getBank(&bcs_, "PART");
    clasRUNC_t *RUNC = getBank(&wcs_, "RUNC");
    clasST1_t  *ST1  = getBank(&bcs_, "ST1 ");
    clasSTR_t  *STR  = getBank(&bcs_, "STR ");
    clasTACO_t *TACO = getBank(&bcs_, "TACO");
    clasTAGE_t *TAGE = getBank(&bcs_, "TAGE");
    clasTAGI_t *TAGI = getBank(&bcs_, "TAGI");
    clasTAGR_t *TAGR = getBank(&bcs_, "TAGR");
    clasTAGT_t *TAGT = getBank(&bcs_, "TAGT");
    clasTGTL_t *TGTL = getBank(&bcs_, "TGTL");
    clasTGTR_t *TGTR = getBank(&bcs_, "TGTR");

    float beta;
    float gamma;
    float mass;
    float p;
    float timeAve;
    float timeDif;

    int ST;
    int firstHitTC;
    int hid;
    int i, j;
    int minID;
    int sum;
    int sumMin;
    int tbidIndex;

    int TCnoE_used[61];
    int TC_used[61];

    int eidG;
    int tidG;
    float ephoG;

    int nprotons = 0;
    int npiplus = 0;
    int npiminus = 0;
    int nkplus = 0;
    int protonIndex = -1;
    int piplusIndex = -1;
    int piminusIndex = -1;
    int kplusIndex = -1;

    vector4_t photon4v = {0.0, 0.0, 0.0, 0.0};
    vector4_t finalState4v;
    vector4_t undetected4v;
    vector4_t initalState4v;

    float undetectedM = 0;
    float undetectedMassSq = 0;

    if(doRawTagger && TAGE)
        for(i = 0; i < TAGE->bank.nrow; i++)

            if(TAGE->tage[i].id > 0 &&
                    TAGE->tage[i].id <= NUM_EC &&
                    TAGE->tage[i].tdc > 0 &&
                    TAGE->tage[i].tdc < EC_CHANNELS)
                hf2(1, TAGE->tage[i].id, TAGE->tage[i].tdc, 1.);

    if(doRawTagger && TAGT) {

        minID = 0;
        sumMin = 2 * TC_CHANNELS;

        for(i = 0; i < TAGT->bank.nrow; i++) {

            if(TAGT->tagt[i].id > 0 &&
                    TAGT->tagt[i].id <= NUM_TC) {

                if(TAGT->tagt[i].tdcl > 0 &&
                        TAGT->tagt[i].tdcl < TC_CHANNELS)
                    hf2(2, TAGT->tagt[i].id, TAGT->tagt[i].tdcl, 1.);

                if(TAGT->tagt[i].tdcr > 0 &&
                        TAGT->tagt[i].tdcr < TC_CHANNELS)
                    hf2(3, TAGT->tagt[i].id, TAGT->tagt[i].tdcr, 1.);

                if(TAGT->tagt[i].tdcl > 0 &&
                        TAGT->tagt[i].tdcl < TC_CHANNELS &&
                        TAGT->tagt[i].tdcr > 0 &&
                        TAGT->tagt[i].tdcr < TC_CHANNELS) {

                    sum = TAGT->tagt[i].tdcl + TAGT->tagt[i].tdcr;

                    if(sum < sumMin && sum != 0)
                        minID = TAGT->tagt[i].id;
                }
            }
        } // for(i = 0; i < TAGT->bank.nrow; i++)

        hf1(30, minID, 1.);

    } // if(doRawTagger && TAGT)

    // reset used arrays
    memset(&TCnoE_used[0], 0, sizeof(TCnoE_used  ));
    memset(&TC_used[0], 0, sizeof(TC_used  ));
    //these arrays emulate single hit TDCs

    if(TAGR) {

        Ntagr++;


        // Counting early hits originally out of TAGR if statement

        for(i = 0; i < tagtnorm_.Nb_Tn; i++) {

            if(tagtnorm_.Tn_ID[i] % 2)
                firstHitTC = (tagtnorm_.Tn_ID[i] + 1) / 2;
            else
                firstHitTC = tagtnorm_.Tn_ID[i] / 2;

            hf2(73, tagtnorm_.Tn_time[0][i], firstHitTC, 1.);
            if (tagruncontrol_.use_tgtlr == 0) {
                // ount early hits only for single hit TDCs
                if(tagtnorm_.Tn_time[0][i] <= beginWindow)

                    if((tagtnorm_.Tn_ID[i]) % 2) {

                        hf1(9999, firstHitTC, 1.);

                        if(NnonTrip == 1)
                            hf1(91, firstHitTC, 1.);
                    }
                    else {

                        hf1(9999, firstHitTC, 1.);
                        hf1(9999, firstHitTC + 1, 1.);

                        if(NnonTrip == 1) {
                            hf1(91, firstHitTC, 1.);
                            hf1(91, firstHitTC + 1, 1.);
                        }
                    }
            }
            if(tagtnorm_.Tn_time[0][i] > beginWindow &&
                    tagtnorm_.Tn_time[0][i] < endWindow)
                if((tagtnorm_.Tn_ID[i]) % 2)
                    if(TCnoE_used[firstHitTC-1] == 0) {
                        TCnoE_used[firstHitTC-1] = 1;
                        hf1(7777, firstHitTC, 1.);
                    }
                    else {
                        if(TCnoE_used[firstHitTC-1] == 0 &&
                                TCnoE_used[firstHitTC] == 0 ) {
                            TCnoE_used[firstHitTC-1] = 1;
                            TCnoE_used[firstHitTC] = 1;
                            hf1(7777, firstHitTC, 1.);
                            hf1(7777, firstHitTC + 1, 1.);
                        }
                    }

        } // for(i = 0; i < tagtnorm_.Nb_Tn; i++)

        for(i = 0; i < TAGR->bank.nrow; i++) {

            if(doRawTagger && TAGI &&
                    (TAGR->tagr[i].stat == 7 || TAGR->tagr[i].stat == 15)) {

                for(j = 0; j < TAGI->bank.nrow; j++) {

                    timeDif = TAGI->tagi[j].timemean - TAGI->tagi[j].timee;

                    if(TAGI->tagi[j].idt % 2)
                        firstHitTC = (TAGI->tagi[j].idt + 1) / 2;
                    else
                        firstHitTC = TAGI->tagi[j].idt / 2;

                    if(TAGI->tagi[j].idt > 0 &&
                            TAGI->tagi[j].idt <= NUM_TB)

                        if(TAGR->tagr[i].stat == 7)
                            hf2(10, firstHitTC, timeDif, 1.);
                        else // status 15
                            hf2(20, firstHitTC, timeDif, 1.);

                    if(TAGI->tagi[j].ide > 0 &&
                            TAGI->tagi[j].ide <= NUM_EB)

                        if(TAGR->tagr[i].stat == 7)
                            hf2(11, TAGI->tagi[j].ide, timeDif, 1.);
                        else // status 15
                            hf2(21, TAGI->tagi[j].ide, timeDif, 1.);

                } // for(j = 0; j < TAGI->bank.nrow; j++)
            } // if(doRawTagger && TAGI ...

            if(TAGR->tagr[i].t_id % 2)
                firstHitTC = (TAGR->tagr[i].t_id + 1) / 2;
            else
                firstHitTC = TAGR->tagr[i].t_id / 2;

            hf2(40, firstHitTC, TAGR->tagr[i].stat, 1.);

            // Out of time hits

            if(TAGR->tagr[i].stat == 15 &&
                    TAGR->tagr[i].tpho > beginWindow &&
                    TAGR->tagr[i].tpho < endWindow) {

                hf2(50, TAGR->tagr[i].e_id, firstHitTC, 1.);
                hf2(60, TAGR->tagr[i].erg, firstHitTC, 1.);
                hf2(70, TAGR->tagr[i].tpho, firstHitTC, 1.);
                if(TC_used[firstHitTC-1] == 0) {
                    TC_used[firstHitTC-1] = 1;
                    hf1(8888, firstHitTC, 1.);
                }

                if(NnonTrip == 1)
                    hf1(90, firstHitTC, 1.);
            }

            // Single "good" tagger hits

            if(TAGR->tagr[i].stat == 7) {

                hf2(51, TAGR->tagr[i].e_id, firstHitTC, 1.);
                hf2(61, TAGR->tagr[i].erg, firstHitTC, 1.);
                hf2(71, TAGR->tagr[i].tpho, firstHitTC, 1.);
                hf2(161, TAGR->tagr[i].erg, firstHitTC, 1.);
            }

            // Multiple "good" tagger hits

            if(TAGR->tagr[i].stat == 15) {

                hf2(52, TAGR->tagr[i].e_id, firstHitTC, 1.);
                hf2(62, TAGR->tagr[i].erg, firstHitTC, 1.);
                hf2(72, TAGR->tagr[i].tpho, firstHitTC, 1.);
                hf2(162, TAGR->tagr[i].erg, firstHitTC, 1.);
            }

            // When calculating the tagging ratio only using single hits

            if(normRun && TAGR->tagr[i].stat == 7) {

                hf1(201, firstHitTC, 1.);

                if(tagtnorm_.Nb_Tn == 1)
                    hf1(211, firstHitTC, 1.);
                if(TACO)
                    hf2(231, TAGR->tagr[i].tpho, TAGR->tagr[i].tpho - TACO->taco[0].time, 1.);

                hf1(251, tagtnorm_.Nb_Tn, 1.);

                if(TACO && fabs(TAGR->tagr[i].tpho - TACO->taco[0].time)<tagparam_.TAG_DSD_window) {
                    hf1(202, firstHitTC, 1.);

                    if(tagtnorm_.Nb_Tn == 1)
                        hf1(212, firstHitTC, 1.);

                    hf2(232, TAGR->tagr[i].tpho, TAGR->tagr[i].tpho - TACO->taco[0].time, 1.);
                    hf1(252, tagtnorm_.Nb_Tn, 1.);

                }

            }

        } // for(i = 0; i < TAGR->bank.nrow; i++)

        if(doParticles && PART && TBID) {

            tagr_t *tagr = get_photon_tagr(TAGR, TBID, TIME_BASED);

            if(tagr) { // Found the best photon for the event

                if(tagr->stat == 7 || tagr->stat == 15) {

                    if(doRawTagger && TAGI)

                        for(i = 0; i < TAGI->bank.nrow; i++) {

                            if(TAGI->tagi[i].idt == tagr->t_id) {

                                timeDif = TAGI->tagi[i].timemean - TAGI->tagi[i].timee;

                                if(TAGI->tagi[i].idt % 2)
                                    firstHitTC = (TAGI->tagi[i].idt + 1) / 2;
                                else // status 15
                                    firstHitTC = TAGI->tagi[i].idt / 2;

                                if(TAGI->tagi[i].idt > 0 &&
                                        TAGI->tagi[i].idt <= NUM_TB)

                                    if(tagr->stat == 7)
                                        hf2(12, firstHitTC, timeDif, 1.);
                                    else // status 15
                                        hf2(22, firstHitTC, timeDif, 1.);

                                if(TAGI->tagi[i].ide > 0 &&
                                        TAGI->tagi[i].ide <= NUM_EB)

                                    if(tagr->stat == 7)
                                        hf2(13, TAGI->tagi[i].ide, timeDif, 1.);
                                    else // status 15
                                        hf2(23, TAGI->tagi[i].ide, timeDif, 1.);
                            }
                        } // for(i = 0; i < TAGI->bank.nrow; i++)

                    if(tagr->stat == 7) { /* ET matrix */

                        hf2(53, tagr->e_id, firstHitTC, 1.);
                        hf2(63, tagr->erg, firstHitTC, 1.);
                    }
                    else { // status 15

                        hf2(54, tagr->e_id, firstHitTC, 1.);
                        hf2(64, tagr->erg, firstHitTC, 1.);
                    }

                } // if(tagr->stat == 7 || tagr->stat == 15)

                if(tagr->t_id % 2)
                    firstHitTC = (tagr->t_id + 1) / 2;
                else
                    firstHitTC = tagr->t_id / 2;

                for(i = 0; i < PART->bank.nrow; i++) {

                    hid = -1;

                    if(PART->part[i].q != 0) { /*All charged particles*/

                        Ncharged++;

                        p = v3mag(PART->part[i].p.space);
                        tbidIndex = PART->part[i].trkid - 1;
                        beta = TBID->bid[tbidIndex].beta;

                        if(beta > 0. && beta < 1.5) {

                            gamma = beta2gamma(beta);

                            mass = p / gamma / beta;

                            hf1(2003, firstHitTC, 1.);
                            hf1(3003, tagr->e_id, 1.);
                            hf1(4003, tagr->erg, 1.);

                            hf2(2030, mass, firstHitTC, 1.);

                        }

                        if(PART->part[i].pid == Proton) { /*Proton*/
                            hid = 2000;
                            Npro++;
                        }

                        else if(PART->part[i].pid == PiPlus) { /*Pi+*/
                            hid = 2001;
                            Npip++;
                        }

                        else if(PART->part[i].pid == PiMinus) { /*Pi-*/
                            hid = 2002;
                            Npim++;
                        }

                        else if(PART->part[i].pid == Unknown) { /*Unknown*/
                            hid = 2004;
                            Nunknown++;
                        }

                        if(hid > 0) { // filling PART particle histograms
                            hf1(hid, firstHitTC, 1.);
                            hf1(hid+1000, tagr->e_id, 1.);
                            hf1(hid+2000, tagr->erg, 1.);
                        }

                    } // if(PART->part[i].q != 0)
                } /* done with PART */
            } // if(tagr)
        } // if(doParticles && PART && TBID)

        if(GPID)
            for(i = 0; i < GPID->bank.nrow; i++) {

                hid = -1;

                if(GPID->gpid[i].ngrf &&
                        GPID->gpid[i].q != 0) {

                    NchargedG++;

                    eidG = TAGR->tagr[GPID->gpid[i].tagrid - 1].e_id;
                    tidG = TAGR->tagr[GPID->gpid[i].tagrid - 1].t_id;

                    if(tidG % 2)
                        firstHitTC = (tidG + 1) / 2;
                    else
                        firstHitTC = tidG / 2;

                    ephoG = GPID->gpid[i].epho;

                    /* all charged */
                    hf1(12003, firstHitTC, 1.);
                    hf1(13003, eidG, 1.);
                    hf1(14003, ephoG, 1.);

                    hf2(12030, GPID->gpid[i].mass, firstHitTC, 1.);

                    if(GPID->gpid[i].pid == Proton) {
                        hid = 12000;
                        nprotons++;
                        protonIndex = i;
                        NproG++;
                    }
                    else if(GPID->gpid[i].pid == PiPlus) {
                        hid = 12001;
                        npiplus++;
                        piplusIndex = i;
                        NpipG++;
                    }
                    else if(GPID->gpid[i].pid == PiMinus) {
                        hid = 12002;
                        npiminus++;
                        piminusIndex = i;
                        NpimG++;
                    }
                    else if(GPID->gpid[i].pid == Unknown) {
                        hid = 12004;
                        NunknownG++;
                    }

                    if(hid > 0) {
                        hf1(hid, firstHitTC, 1.);
                        hf1(hid+1000, eidG, 1.);
                        hf1(hid+2000, ephoG, 1.);
                    }

                    // fill histograms for exclusive reactions

                    if(doExclusive) {

                        // Reaction:  gamma p -> pi+ n
                        if(npiplus == 1 &&
                                fabs(MVRT->mvrt[0].vert.z) <= TARGET_Z_MAX) {

                            photon4v.t = photon4v.space.z = GPID->gpid[piplusIndex].epho;

                            initalState4v = v4add(photon4v, RUNC->runc.target);

                            undetected4v = v4sub(initalState4v, GPID->gpid[piplusIndex].p);
                            undetectedM = v4mass(undetected4v);

                            hf1(14200, undetectedM, 1.);

                            if(fabs(undetectedM < 1.)) {

                                eidG = TAGR->tagr[GPID->gpid[piplusIndex].tagrid - 1].e_id;
                                tidG = TAGR->tagr[GPID->gpid[piplusIndex].tagrid - 1].t_id;

                                if(tidG % 2)
                                    firstHitTC = (tidG + 1) / 2;
                                else
                                    firstHitTC = tidG / 2;

                                ephoG = GPID->gpid[piplusIndex].epho;

                                hf1(12100, firstHitTC, 1.);
                                hf1(13100, eidG, 1.);
                                hf1(14100, ephoG, 1.);
                            }
                        }

                        // Reaction gamma p -> pi+ pi- p

                        if(nprotons == 1 &&
                                npiplus  == 1 &&
                                npiminus == 1 &&
                                fabs(MVRT->mvrt[0].vert.z) <= TARGET_Z_MAX) {

                            if(GPID->gpid[protonIndex].tagrid ==
                                    GPID->gpid[piplusIndex].tagrid &&
                                    GPID->gpid[protonIndex].tagrid ==
                                    GPID->gpid[piminusIndex].tagrid) {

                                // all three particles from the same photon

                                photon4v.t = photon4v.space.z = GPID->gpid[protonIndex].epho;

                                initalState4v = v4add(photon4v, RUNC->runc.target);

                                finalState4v = v4add(GPID->gpid[protonIndex].p,
                                                     v4add(GPID->gpid[piplusIndex].p,
                                                           GPID->gpid[piminusIndex].p));

                                undetected4v = v4sub(initalState4v, finalState4v);
                                undetectedMassSq = v4dot(undetected4v, undetected4v);

                                hf1(14201, undetectedMassSq, 1.);

                                if(fabs(undetectedMassSq < 0.005)) {

                                    eidG = TAGR->tagr[GPID->gpid[protonIndex].tagrid - 1].e_id;
                                    tidG = TAGR->tagr[GPID->gpid[protonIndex].tagrid - 1].t_id;

                                    if(tidG % 2)
                                        firstHitTC = (tidG + 1) / 2;
                                    else
                                        firstHitTC = tidG / 2;

                                    ephoG = GPID->gpid[protonIndex].epho;

                                    hf1(12101, firstHitTC, 1.);
                                    hf1(13101, eidG, 1.);
                                    hf1(14101, ephoG, 1.);

                                }
                            }
                        }
                    } // if(doExclusive)
                }
            } // for(i = 0; i < GPID->bank.nrow; i++)
    } // if(TAGR)

    // Start counter histograms

    if(startCounter && ST1)
        for(ST = 0; ST < ST1->bank.nrow; ST++) {

            timeAve = (ST1->st1[ST].time_1 + ST1->st1[ST].time_2) / 2.;

            if(ST1->st1[ST].time_1 && ST1->st1[ST].time_2)
                hf2(410, timeAve, ST1->st1[ST].id, 1.);
        }

    if(startCounter && STR) {

        Nstr++;

        for(ST = 0; ST < STR->bank.nrow; ST++) {

            if(STR->str[ST].id == 1 || STR->str[ST].id == 2)
                hf2(411, STR->str[ST].st_time, 1., 1.);
            else if(STR->str[ST].id == 3 || STR->str[ST].id == 4)
                hf2(411, STR->str[ST].st_time, 2., 1.);
            else if(STR->str[ST].id == 5 || STR->str[ST].id == 6)
                hf2(411, STR->str[ST].st_time, 3., 1.);

            if(STR->str[ST].st_time > beginWindow
                    && STR->str[ST].st_time < endWindow)
                if(STR->str[ST].id == 1 || STR->str[ST].id == 2)
                    hf1(6666, 1., 1.);
                else if(STR->str[ST].id == 3 || STR->str[ST].id == 4)
                    hf1(6666, 2., 1.);
                else if(STR->str[ST].id == 5 || STR->str[ST].id == 6)
                    hf1(6666, 3., 1.);
        }
    } // if(startCounter && STR)

    return;

} // void fillHistos

/*--------------------------------------------------------------------------*/

void fillScalerHistos() {

    hf1(110, Ns, NeventsPHY);
    hf1(111, Ns, NeventsTRK);
    hf1(112, Ns, Ntagr);

    hf1(123, Ns, 1.);

    if(doParticles) {

        hf1(5000, Ns, Npro);
        hf1(5001, Ns, Npip);
        hf1(5002, Ns, Npim);
        hf1(5003, Ns, Ncharged);
        hf1(5004, Ns, Nunknown);

        hf1(15000, Ns, NproG);
        hf1(15001, Ns, NpipG);
        hf1(15002, Ns, NpimG);
        hf1(15003, Ns, NchargedG);
        hf1(15004, Ns, NunknownG);

        Npro = 0;
        Npip = 0;
        Npim = 0;
        Ncharged = 0;
        Nunknown = 0;

        NproG = 0;
        NpipG = 0;
        NpimG = 0;
        NchargedG = 0;
        NunknownG = 0;

    } // if(doParticles)

    return;

} // void fillScalerHistos

/*--------------------------------------------------------------------------*/

void finalCal() {

    FILE *fp = NULL;

    char *dir=getenv("CLAS_PARMS");
    char *fileName = NULL;
    char map[100];
    char name[100];

    float Ne[NUM_TC];
    float NeU[NUM_TC];
    float NeFU[NUM_TC];
    float NphoTC[NUM_TC];
    float NphoTCU[NUM_TC];
    float NphoTCFU[NUM_TC];
    float NphoEB[NUM_EB];
    float NphoEBU[NUM_EB];
    float NphoERG[NUM_ERG];
    float NphoERGU[NUM_ERG];
    float NphoERG2[NUM_ERG2];
    float NphoERG2U[NUM_ERG2];
    float PstolenHit[NUM_TC];
    float PstolenHitU[NUM_TC];
    float inverseVarianceSum = 0.;
    float phoAtten = 0.;
    float phoAttenU = 0.;
    float phoAttenTermFU = 0.;
    float rateAve[NUM_TC];
    float rateAveU[NUM_TC];
    float rateNoE[NUM_TC*timeline];
    float rateNoEU[NUM_TC*timeline];
    float tagRatioFU[NUM_TC];
    float weightedSum = 0.;

    int EB, TC;
    int j;
    int numScalerIntervals;
    int index;

    memset(&Ne[0], 0, sizeof(Ne));
    memset(&NeU[0], 0, sizeof(NeU));
    memset(&NeFU[0], 0, sizeof(NeFU));
    memset(&NphoTC[0], 0, sizeof(NphoTC));
    memset(&NphoTCU[0], 0, sizeof(NphoTCU));
    memset(&NphoTCFU[0], 0, sizeof(NphoTCFU));
    memset(&NphoEB[0], 0, sizeof(NphoEB));
    memset(&NphoEBU[0], 0, sizeof(NphoEBU));
    memset(&NphoERG[0], 0, sizeof(NphoERG));
    memset(&NphoERGU[0], 0, sizeof(NphoERGU));
    memset(&NphoERG2[0], 0, sizeof(NphoERG2));
    memset(&NphoERG2U[0], 0, sizeof(NphoERG2U));
    memset(&PstolenHit[0], 0, sizeof(PstolenHit));
    memset(&PstolenHitU[0], 0, sizeof(PstolenHitU));
    memset(&rateAve[0], 0, sizeof(rateAve));
    memset(&rateAveU[0], 0, sizeof(rateAveU));
    memset(&rateNoE[0], 0, sizeof(rateNoE));
    memset(&rateNoEU[0], 0, sizeof(rateNoEU));
    memset(&tagRatioFU[0], 0, sizeof(tagRatioFU));

    hunpak(80, Ne, "HIST", 1);
    hunpke(80, NeU, "HIST", 1);

    hunpak(102, rateNoE, "HIST", 1);
    hunpak(103, rateNoEU, "HIST", 1);


    // The photon attenuation from the CLAS target to downstream TAC

    //map_get_float(map, "photon_atten", "value",
    //      1, &phoAtten, normRunNo, &firsttime);

    //map_get_float(map, "photon_atten", "error",
    //      1, &phoAttenU, normRunNo, &firsttime);

    phoAtten = 0.05;
    phoAttenU = 0.002;

    // The correction to the number of photons due to attenuation is
    // 1 / (1 - phoAtten) and has been determined to be essentially energy
    // independent in our region of intrest.

    if(phoAtten)
        phoAttenTermFU = phoAttenU / (1. - phoAtten); /* Term is 1/(1-phoAtten) */

    // Calculates the weighted average and error of the TC rate with out
    // E counter coincidence which is used for the stolen T counter probability

    for(TC = 0; TC < NUM_TC; TC++) {

        weightedSum = 0.;
        inverseVarianceSum = 0.;

        for(j = 0; j < timeline; j++) {

            index = timeline * TC + j;

            // only considering intervals with a non-zero rate

            if(rateNoE[index] && rateNoEU[index]) {
                weightedSum += rateNoE[index] / sq(rateNoEU[index]);
                inverseVarianceSum += 1. / sq(rateNoEU[index]);
            }
        }

        if(inverseVarianceSum) {
            rateAve[TC] = weightedSum / inverseVarianceSum;
            rateAveU[TC] = 1. / sqrt(inverseVarianceSum);
        }
    } // for(TC = 0; TC < NUM_TC; TC++)

    hpak(302, rateAve);
    hpake(302, rateAveU);

    // Correcting the number of "good" electrons measured per TC due to stolen
    // hits by calculating the probabilty of a stolen hit using the average
    // rate on the T counters without E counter coincidence being necessary

    for(TC = 0; TC < NUM_TC; TC++)
        if(rateAve[TC] && stolenWindow[TC]) {

            PstolenHit[TC] = 1. - exp(-rateAve[TC] * stolenWindow[TC] * 1.e-9);

            PstolenHitU[TC] = exp(-rateAve[TC] * stolenWindow[TC] * 1.e-9) *
                              stolenWindow[TC] * 1.e-9 * rateAveU[TC];

            if(Ne[TC])
                NeFU[TC] = NeU[TC] / Ne[TC]; // Used in error calculation below

            Ne[TC] *= 1. - PstolenHit[TC];

            NeU[TC] = Ne[TC] *
                      sqrt(sq(NeFU[TC]) + sq(PstolenHitU[TC] / (1. - PstolenHit[TC])));
        }

    hpak(81, Ne);
    hpake(81, NeU);

    hpak(300, PstolenHit);
    hpake(300, PstolenHitU);

    for(TC = 0; TC < NUM_TC; TC++) { // Number of photons per T counter

        NphoTC[TC] = Ne[TC] * tagRatio[TC] / (1. - phoAtten);

        if(Ne[TC] && tagRatio[TC] && NphoTC[TC]) {

            NeFU[TC] = NeU[TC] / Ne[TC];

            tagRatioFU[TC] = tagRatioU[TC] / tagRatio[TC];

            NphoTCU[TC] = NphoTC[TC] *
                          sqrt(sq(NeFU[TC]) + sq(tagRatioFU[TC]) + sq(phoAttenTermFU));

            NphoTCFU[TC] = NphoTCU[TC] / NphoTC[TC];
        }

    } // for(TC = 0; TC < NUM_TC; TC++)

    // Rebinning the number of photons from T counters to E counter bins
    // or energy bins

    photonsTC2EB(&NphoTC[0], &NphoTCFU[0], &NphoEB[0], &NphoEBU[0], NUM_EB, 50);

    photonsTC2EB(&NphoTC[0], &NphoTCFU[0], &NphoERG[0], &NphoERGU[0], NUM_ERG, 60);

    photonsTC2EB(&NphoTC[0], &NphoTCFU[0], &NphoERG2[0], &NphoERG2U[0], NUM_ERG2, 160);

    hpak(1000, NphoTC);
    hpake(1000, NphoTCU);

    hpak(1001, NphoEB);
    hpake(1001, NphoEBU);

    hpak(1002, NphoERG);
    hpake(1002, NphoERGU);

    hpak(1003, NphoERG2);
    hpake(1003, NphoERG2U);

    // Here I think it would be best to strip the file name off and add it to
    // the string, otherwise not using the -f option will cause these files
    // to be overwriten for multiple files in a run

    if(fileNo == -1)
        sprintf(name, "gflux%d_tc.dat", runNo);
    else if(fileNo < 10)
        sprintf(name, "gflux%d_tc.a0%d.dat", runNo, fileNo);
    else
        sprintf(name, "gflux%d_tc.a%d.dat", runNo, fileNo);

    fileName = &name[0];

    fp = fopen(fileName, "w");

    for(TC = 0; TC < NUM_TC; TC++)
        fprintf(fp, "%f %f\n", NphoTC[TC], NphoTCU[TC]);

    fclose(fp);

    if(fileNo == -1)
        sprintf(name, "gflux%d_eb.dat", runNo);
    else if(fileNo < 10)
        sprintf(name, "gflux%d_eb.a0%d.dat", runNo, fileNo);
    else
        sprintf(name, "gflux%d_eb.a%d.dat", runNo, fileNo);

    fileName = &name[0];

    fp = fopen(fileName, "w");

    for(EB = 0; EB < NUM_EB; EB++)
        fprintf(fp, "%f %f\n", NphoEB[EB], NphoEBU[EB]);

    fclose(fp);

    if(fileNo == -1)
        sprintf(name, "gflux%d_erg.dat", runNo);
    else if(fileNo < 10)
        sprintf(name, "gflux%d_erg.a0%d.dat", runNo, fileNo);
    else
        sprintf(name, "gflux%d_erg.a%d.dat", runNo, fileNo);

    fileName = &name[0];

    fp = fopen(fileName, "w");

    for(EB = 0; EB < NUM_ERG; EB++)
        fprintf(fp, "%f %f\n", NphoERG[EB], NphoERGU[EB]);

    fclose(fp);

    if(fileNo == -1)
        sprintf(name, "gflux%d_erg2.dat", runNo);
    else if(fileNo < 10)
        sprintf(name, "gflux%d_erg2.a0%d.dat", runNo, fileNo);
    else
        sprintf(name, "gflux%d_erg2.a%d.dat", runNo, fileNo);

    fileName = &name[0];

    fp = fopen(fileName, "w");

    for(EB = 0; EB < NUM_ERG2; EB++)
        fprintf(fp, "%f %f\n", NphoERG2[EB], NphoERG2U[EB]);

    fclose(fp);

    return;

} // void finalCal

/*---------------------------------------------------------------------------*/

void fluxCal() {

    float Ne[NUM_TC];
    float NeU[NUM_TC];
    float NeSum[NUM_TC];
    float NeSumU[NUM_TC];
    float Nearly[NUM_TC];
    float NoutOfTime[NUM_TC];
    float NoutOfTimeNoE[NUM_TC];
    float NoutOfTimeST[NUM_ST];
    float Ntrials[NUM_TC];
    float NtrialsST[NUM_ST];
    float rate[NUM_TC];
    float rateFU[NUM_TC];
    float rateNoE[NUM_TC];
    float rateNoEFU[NUM_TC];
    float rateNoEU[NUM_TC];
    float rateST[NUM_ST];
    float rateSTFU[NUM_ST];

    int ST, TC;

    memset(&Ne[0], 0, sizeof(Ne));
    memset(&NeU[0], 0, sizeof(NeU));
    memset(&NeSum[0], 0, sizeof(NeSum));
    memset(&NeSumU[0], 0, sizeof(NeSumU));
    memset(&Nearly[0], 0, sizeof(Nearly));
    memset(&NoutOfTime[0], 0, sizeof(NoutOfTime));
    memset(&NoutOfTimeNoE[0], 0, sizeof(NoutOfTimeNoE));
    memset(&NoutOfTimeST[0], 0, sizeof(NoutOfTimeST));
    memset(&Ntrials[0], 0, sizeof(Ntrials));
    memset(&NtrialsST[0], 0, sizeof(NtrialsST));
    memset(&rate[0], 0, sizeof(rate));
    memset(&rateFU[0], 0, sizeof(rateFU));
    memset(&rateNoE[0], 0, sizeof(rateNoE));
    memset(&rateNoEFU[0], 0, sizeof(rateNoEFU));
    memset(&rateNoEU[0], 0, sizeof(rateNoEU));
    memset(&rateST[0], 0, sizeof(rateST));
    memset(&rateSTFU[0], 0, sizeof(rateSTFU));

    /* Contents of tdc temp occupancy */

    if(startCounter)
        hunpak(6666, NoutOfTimeST, "HIST", 1);

    hunpak(7777, NoutOfTimeNoE, "HIST", 1);
    hunpak(8888, NoutOfTime, "HIST", 1);
    hunpak(9999, Nearly, "HIST", 1);

    //-----> T counter Rates

    for(TC = 0; TC < NUM_TC; TC++)
        Ntrials[TC] = (float)Ntagr - Nearly[TC];

    // Rate of "good" status 7 and 15 electrons per T counter

    getRate(&rate[0], &rateFU[0], &NoutOfTime[0], &Ntrials[0], NUM_TC);

    // Rate of electrons per T counter, E counter coincidence not necessary

    getRate(&rateNoE[0], &rateNoEFU[0], &NoutOfTimeNoE[0], &Ntrials[0], NUM_TC);

    for(TC = 0; TC < NUM_TC; TC++) {

        Ne[TC] = rate[TC] * runTime;

        NeU[TC] = Ne[TC] * sqrt(sq(rateFU[TC]) + sq(runTimeFU));

        rateNoEU[TC] = rateNoE[TC] * rateNoEFU[TC];

        if(Ns < timeline) {
            hf2(100, Ns, TC+1, rate[TC]);
            hf2(101, Ns, TC+1, Ne[TC]);
            hf2(102, Ns, TC+1, rateNoE[TC]);
            hf2(103, Ns, TC+1, rateNoEU[TC]);
        }

    } // for(TC = 0; TC < NUM_TC; TC++)

    if(NnonTrip > 1) {
        hunpak(80, NeSum, "HIST", 1); /* Running total of e- per TC */
        hunpke(80, NeSumU, "HIST", 1);
    }

    for(TC = 0; TC < NUM_TC; TC++) { /* Add current # of e- to the sum */

        NeSum[TC] += Ne[TC];
        NeSumU[TC] = sqrt(sq(NeSumU[TC]) + sq(NeU[TC]));
    }

    /* Sum of e- per TC in a histogram */

    hpak(80, NeSum);
    hpake(80, NeSumU);

    /* Reset histograms that count between scaler events */

    hreset(7777, " ");
    hreset(8888, " ");
    hreset(9999, " ");

    //-----> Start counter STR rates

    // Measuring the startcounter rate with the ST1 bank has proven to be faulty
    // I assume this is from the bends in the scinalator causing the effective
    // velocity to be position dependent.  I can think of two other ways
    // to do this, first calculate the rate from the scalers or use the STR bank
    // but since this bank uses tracks we need to scale the result so that it
    // is track independent, hence we can take the ratio of occupancies between
    // the ST1 bank and STR bank to scale the rates

    if(startCounter) {

        for(ST = 0; ST < NUM_ST; ST++)
            NtrialsST[ST] = (float)Nstr;

        getRate(&rateST[0], &rateSTFU[0], &NoutOfTimeST[0], &NtrialsST[0], NUM_ST);

        for(ST = 0; ST < NUM_ST; ST++)
            if(Ns < timeline)
                hf2(401, Ns, ST+1, rateST[ST]);

        hreset(6666, " ");
    }

    return;

} // void fluxCal

/*--------------------------------------------------------------------------*/

void getMapInfo() {

    char *dir = getenv("CLAS_PARMS");
    char map[100];

    memset(&stolenWindow[0], 0, sizeof(stolenWindow));
    memset(&tagRatio[0], 0, sizeof(tagRatio));
    memset(&tagRatioU[0], 0, sizeof(tagRatioU));

    sprintf(map, "%s/Maps/NORM.map", dir);

    if(!normRunNo && !normRun)
        map_get_int(map, "gflux", "norm_run",
                    1, &normRunNo, runNo, &firsttime);

    if(!normRun)
        fprintf(stderr, "Using normalization run %d\n", normRunNo);

    map_get_float(map, "gflux", "begin_t_window",
                  1, &beginWindow, runNo, &firsttime);

    map_get_float(map, "gflux", "end_t_window",
                  1, &endWindow, runNo, &firsttime);

    fprintf(stderr, "Out of time window ranges from %f to %f ns\n",
            beginWindow, endWindow);

    tdcWindow = endWindow - beginWindow;
    NtphoBins = (int)tdcWindow;


    /// begin code moved from finalCal() function
    sprintf(map, "%s/Maps/NORM.map", dir);

    // Getting the tagging ratio from the map

    map_get_float(map, "gflux", "tag_ratio",
                  NUM_TC, &tagRatio[0], normRunNo, &firsttime);

    map_get_float(map, "gflux", "tag_ratio_u",
                  NUM_TC, &tagRatioU[0], normRunNo, &firsttime);

    hpak(203, tagRatio);
    hpake(203, tagRatioU);

    // Length of the stolen Window per T counter, this is defined as the time
    // interval between the start of the TDC to the dip before the trigger peak

    map_get_float(map, "gflux", "stolen_window",
                  NUM_TC, &stolenWindow[0], runNo, &firsttime);

    hpak(301, stolenWindow);
    /// end code moved from finalCal() function


    return;

} // void getMapInfo

/*--------------------------------------------------------------------------*/

void getRate(float *rate, float *rateFU, float *NoutOfTime, float *Ntrials,
             int numBins) {

    // calculates the rate on a detector through it's out of time occupancy
    // for a given out of time interval through poisson statictics

    // PnoHits is the probability of having no hits during the out of time
    // window, and from this the rate is determined

    float PnoHits[numBins];

    int i;

    memset(&PnoHits[0], 0, sizeof(PnoHits));

    for(i = 0; i < numBins; i++) {

        if(Ntrials[i]) {

            PnoHits[i] = 1. - NoutOfTime[i] / Ntrials[i];

            if(PnoHits[i]) {

                rate[i] = -log(PnoHits[i]) / (tdcWindow * 1.e-9);

                if(NoutOfTime[i] && rate[i]) {

                    rateFU[i] = sqrt(1. / NoutOfTime[i] + 1. / Ntrials[i]) *
                                NoutOfTime[i] / Ntrials[i] / PnoHits[i] /
                                (tdcWindow * 1.e-9) / rate[i];
                }
            }
        } // if(Ntrials[i])
    } // for(i = 0; i < numBins; i++)

    //if(NnonTrip == 1)
    //hpak(92, PnoHits);

    return;

} // void getRate

/*--------------------------------------------------------------------------*/

void getRateST() {

    // calculates the rate of each start counter pair using scalers

    clasSTS_t *STS = getBank(&bcs_, "STS ");

    float rate[NUM_ST];

    int i;

    memset(&rate[0], 0, sizeof(rate));

    if(STS && clock) {
        rate[0] = STS->sts[0].sts1 / clock;
        rate[1] = STS->sts[0].sts2 / clock;
        rate[2] = STS->sts[0].sts3 / clock;
    }

    for(i = 0; i < NUM_ST; i++)
        hf2(400, Ns, i+1, rate[i]);

    return;

} // void getRateST

/*--------------------------------------------------------------------------*/

void getTime() {

    // Caculates the run time of each scaler interval, this is the time of the
    // scaler interval multiplied by the livetime for this interval

    clasS1ST_t *S1ST = getBank(&bcs_, "S1ST");
    clasTRGS_t *TRGS = getBank(&bcs_, "TRGS");

    float livetime = 1.;
    float ltFU = 0.;
    float syncClock = 0.;

    static uint32 AtrgsPrev;
    static uint32 TtrgsPrev;
    static uint32 beginClock;

    uint32 Atrgs = 0;
    uint32 AtrgsS = 0;
    uint32 Ttrgs = 0;
    uint32 TtrgsS = 0;
    uint32 clockInterval = 0;
    uint32 finalClock;

    if(Ns == 1) {

        AtrgsPrev = 0;
        TtrgsPrev = 0;
        beginClock = 0;
    }

    //-----> Livetime calcuation

    if(!clocklt) { // event livetime calculation (default)

        if(S1ST)
            Atrgs = S1ST->s1st[0].event_count; // Accepted triggers

        if(TRGS)
            Ttrgs = TRGS->trgs[0].trig_or_ug; // Total triggers
    }
    else // clock livetime calculation can be choosen with -c

        if(TRGS) {

            Atrgs = TRGS->trgs[0].clock_g2; // busy gated clock
            Ttrgs = TRGS->trgs[0].clock_g1; // run gated clock
        }


    if(Atrgs >= AtrgsPrev)
        AtrgsS = Atrgs - AtrgsPrev;
    else
        AtrgsS = OVERFLOW_32BIT - AtrgsPrev + Atrgs;

    if(Ttrgs >= TtrgsPrev)
        TtrgsS = Ttrgs - TtrgsPrev;
    else
        TtrgsS = OVERFLOW_32BIT - TtrgsPrev + Ttrgs;

    AtrgsPrev = Atrgs;
    TtrgsPrev = Ttrgs;

    if(AtrgsS && TtrgsS) {

        livetime = (float)AtrgsS / (float)TtrgsS;

        if(livetime)
            ltFU = binomialU(livetime, (float)TtrgsS) / livetime;
    }

    //-----> clock calculation

    if(TRGS)
        finalClock = TRGS->trgs[0].clock_g1; // run gated clock

    if(finalClock >= beginClock)
        clockInterval = finalClock - beginClock;
    else
        clockInterval = OVERFLOW_32BIT - beginClock + finalClock;

    beginClock = finalClock;

    clock = (float)clockInterval / runGatedClockFreq;

    // correct time to account for the lost sync intervals

    if(syncCor) {
        syncClock = (float)syncIntervalCumulative / INTERRUPT_TIME_FREQ;

        clock -= syncClock;

    }

    if(clock < 0.) {

        fprintf(stderr,"The time for this scaler interval is less than zero\n");
        fprintf(stderr,"Did you run the sync utility with the -c flag?\n");
        fprintf(stderr,"Exiting gflux now\n");

        exit(0);
    }

    //-----> Runtime calculation

    runTime = clock * livetime;
    runTimeFU = ltFU; // assuming the error is dominated by the lt calculation

    if(Ns < timeline) {

        hf1(120, Ns, livetime);
        hf1(121, Ns, clock);
        hf1(122, Ns, runTime);
    }

    return;

} // void getTime

/*---------------------------------------------------------------------------*/

void hini(char *outfile) {

    char hbook[100];

    /* hbook init */
    quest_[9] = 65000;
    hlimit(MEMH);

    // should this be named after the file ie
    // gflux_run21785_.A00.00.hbook or gflux_clas_021785.A00.hbook ?
    // this way it would be easy to tell what file was processed

    if(outfile == NULL) {

        if(fileNo == -1)
            sprintf(hbook, "gflux%d.hbk", runNo);
        else if(fileNo < 10)
            sprintf(hbook, "gflux%d.a0%d.hbk", runNo, fileNo);
        else
            sprintf(hbook, "gflux%d.a%d.hbk", runNo, fileNo);

        outfile = &hbook[0];
    }

    hropen(LUN, "esr", outfile, "N", LREC, 0);

    bookHistos();

    return;

} // void hini

/*---------------------------------------------------------------------------*/

void normYield(int numBins, int hIndex, int pidType) {

    // Called when the -p or -P options are used, and produces normalized yields
    // of various channels from both PART and GPID banks

    // hIndex is the number common number between the yield histograms

    // pidType 1: PART, 2: GPID

    float Ng[numBins];
    float NgU[numBins];
    float NgFU[numBins];

    float Nchr[numBins];
    float Npim[numBins];
    float Npip[numBins];
    float Npro[numBins];
    float Nunk[numBins];

    float NYchr[numBins];
    float NYpim[numBins];
    float NYpip[numBins];
    float NYpro[numBins];
    float NYunk[numBins];

    float NYchrU[numBins];
    float NYpimU[numBins];
    float NYpipU[numBins];
    float NYproU[numBins];
    float NYunkU[numBins];

    float pim2pipRatio[numBins];
    float pro2pipRatio[numBins];
    float unk2pipRatio[numBins];

    float pim2pipRatioU[numBins];
    float pro2pipRatioU[numBins];
    float unk2pipRatioU[numBins];

    //if(doExclusive){

    float Npipn[numBins];
    float Npippimp[numBins];

    float NYpipn[numBins];
    float NYpippimp[numBins];

    float NYpipnU[numBins];
    float NYpippimpU[numBins];

    //}

    int i;

    int binType;

    memset(&Ng[0], 0, sizeof(Ng));
    memset(&NgU[0], 0, sizeof(NgU));
    memset(&NgFU[0], 0, sizeof(NgFU));

    memset(&Nchr[0], 0, sizeof(Nchr));
    memset(&Npim[0], 0, sizeof(Npim));
    memset(&Npip[0], 0, sizeof(Npip));
    memset(&Npro[0], 0, sizeof(Npro));
    memset(&Nunk[0], 0, sizeof(Nunk));

    memset(&NYchr[0], 0, sizeof(NYchr));
    memset(&NYpim[0], 0, sizeof(NYpim));
    memset(&NYpip[0], 0, sizeof(NYpip));
    memset(&NYpro[0], 0, sizeof(NYpro));
    memset(&NYunk[0], 0, sizeof(NYunk));

    memset(&NYchrU[0], 0, sizeof(NYchrU));
    memset(&NYpimU[0], 0, sizeof(NYpimU));
    memset(&NYpipU[0], 0, sizeof(NYpipU));
    memset(&NYproU[0], 0, sizeof(NYproU));
    memset(&NYunkU[0], 0, sizeof(NYunkU));

    memset(&pim2pipRatio[0], 0, sizeof(pim2pipRatio));
    memset(&pro2pipRatio[0], 0, sizeof(pro2pipRatio));
    memset(&unk2pipRatio[0], 0, sizeof(unk2pipRatio));

    memset(&pim2pipRatioU[0], 0, sizeof(pim2pipRatioU));
    memset(&pro2pipRatioU[0], 0, sizeof(pro2pipRatioU));
    memset(&unk2pipRatioU[0], 0, sizeof(unk2pipRatioU));

    if(doExclusive && pidType == 2) {

        memset(&Npipn[0], 0, sizeof(Npipn));
        memset(&Npippimp[0], 0, sizeof(Npippimp));

        memset(&NYpipn[0], 0, sizeof(NYpipn));
        memset(&NYpippimp[0], 0, sizeof(NYpippimp));

        memset(&NYpipnU[0], 0, sizeof(NYpipnU));
        memset(&NYpippimpU[0], 0, sizeof(NYpippimpU));
    }

    if(numBins == NUM_TC)
        binType = 0;
    else if(numBins == NUM_EB)
        binType = 1;
    else if(numBins == NUM_ERG)
        binType = 2;

    hunpak(1000+binType, Ng, "HIST", 1);
    hunpke(1000+binType, NgU, "HIST", 1);

    hunpak(hIndex, Npro, "HIST", 1);
    hunpak(hIndex+1, Npip, "HIST", 1);
    hunpak(hIndex+2, Npim, "HIST", 1);
    hunpak(hIndex+3, Nchr, "HIST", 1);
    hunpak(hIndex+4, Nunk, "HIST", 1);


    if(doExclusive && pidType == 2) {

        hunpak(hIndex+100, Npipn, "HIST", 1);
        hunpak(hIndex+101, Npippimp, "HIST", 1);
    }

    for(i = 0; i < numBins; i++) {

        if(Ng[i]) {

            NgFU[i] = NgU[i] / Ng[i];

            NYpro[i] = Npro[i] / Ng[i];

            if(Npro[i])
                NYproU[i] = NYpro[i] * sqrt(1. / Npro[i] + sq(NgFU[i]));

            NYpip[i] = Npip[i] / Ng[i];

            if(Npip[i])
                NYpipU[i] = NYpip[i] * sqrt(1. / Npip[i] + sq(NgFU[i]));

            NYpim[i] = Npim[i] / Ng[i];

            if(Npim[i])
                NYpimU[i] = NYpim[i] * sqrt(1. / Npim[i] + sq(NgFU[i]));

            NYchr[i] = Nchr[i] / Ng[i];

            if(Nchr[i])
                NYchrU[i] = NYchr[i] * sqrt(1. / Nchr[i] + sq(NgFU[i]));

            NYunk[i] = Nunk[i] / Ng[i];

            if(Nunk[i])
                NYunkU[i] = NYunk[i] * sqrt(1. / Nunk[i] + sq(NgFU[i]));

            if(doExclusive) {

                NYpipn[i] = Npipn[i] / Ng[i];

                if(Npipn[i])
                    NYpipnU[i] = NYpipn[i] * sqrt(1. / Npipn[i] + sq(NgFU[i]));

                NYpippimp[i] = Npippimp[i] / Ng[i];

                if(Npippimp[i])
                    NYpippimpU[i] = NYpippimp[i] * sqrt(1. / Npippimp[i] + sq(NgFU[i]));
            }

        } // if(Ng[i])

        if(Npip[i]) {

            pro2pipRatio[i] = Npro[i] / Npip[i];

            if(Npro[i])
                pro2pipRatioU[i] = pro2pipRatio[i] * sqrt(1. / Npro[i] + 1. / Npip[i]);

            pim2pipRatio[i] = Npim[i] / Npip[i];

            if(Npim[i])
                pim2pipRatioU[i] = pim2pipRatio[i] * sqrt(1. / Npim[i] + 1. / Npip[i]);

            unk2pipRatio[i] = Nunk[i] / Npip[i];

            if(Nunk[i])
                unk2pipRatioU[i] = unk2pipRatio[i] * sqrt(1. / Nunk[i] + 1. / Npip[i]);
        }

    } // for(i = 0; i < numBins; i++)

    hpak(hIndex+10, NYpro);
    hpake(hIndex+10, NYproU);

    hpak(hIndex+11, NYpip);
    hpake(hIndex+11, NYpipU);

    hpak(hIndex+12, NYpim);
    hpake(hIndex+12, NYpimU);

    hpak(hIndex+13, NYchr);
    hpake(hIndex+13, NYchrU);

    hpak(hIndex+14, NYunk);
    hpake(hIndex+14, NYunkU);

    hpak(hIndex+20, pro2pipRatio);
    hpake(hIndex+20, pro2pipRatioU);

    hpak(hIndex+21, pim2pipRatio);
    hpake(hIndex+21, pim2pipRatioU);

    hpak(hIndex+22, unk2pipRatio);
    hpake(hIndex+22, unk2pipRatioU);

    if(doExclusive && pidType == 2) {

        hpak(hIndex+110, NYpipn);
        hpake(hIndex+110, NYpipnU);

        hpak(hIndex+111, NYpippimp);
        hpake(hIndex+111, NYpippimpU);
    }

    return;

} // void normYield

/*--------------------------------------------------------------------------*/

void photonsTC2EB(float *NphoTC, float *NphoTCFU, float *NphoEB,
                  float *NphoEBU, int numBins, int hIndex) {

    // This function partitions the Tcounter photons into the other binning
    // schemes, Ecounter and energy, the arguments are: Tcounter photons,
    // Tcounter photon error, new binned photons, new binned photon error,
    // number of bins for the new binning, and in index to the ET maps of
    // Tcounter vs the desired binning.  It is possible to add multiple binning
    // if desired, just by adding multiple ET maps.  The restrictions are that
    // status 7 electrons have a histogram that ends in 1 and status 15 electrons
    // end in 2, and both histograms have the rest of the numbers in common.
    // For example the ET matrices for Ecounters are 51 for status 7 and 52
    // for status 15 then you pass to this function the common numbers to the
    // variable hIndex which is 50 for the example

    float et[NUM_TC*numBins];
    float et_7[NUM_TC*numBins];
    float et_15[NUM_TC*numBins];
    float etFract[NUM_TC*numBins];
    float etFractFU[NUM_TC*numBins];
    float TCsum[NUM_TC];

    int EB, TC, i;
    int index;
    float newPartition;

    memset(&et[0], 0, sizeof(et));
    memset(&et_7[0], 0, sizeof(et_7));
    memset(&et_15[0], 0, sizeof(et_15));
    memset(&etFract[0], 0, sizeof(etFract));
    memset(&etFractFU[0], 0, sizeof(etFractFU));
    memset(&TCsum[0], 0, sizeof(TCsum));

    hunpak(hIndex+1, et_7, "HIST", 1);
    hunpak(hIndex+2, et_15, "HIST", 1);


    // Using all "good" tagged electrons(status 7 & 15) to compose the ET matrix
    for(i = 0; i < NUM_TC * numBins; i++)
        et[i] = et_7[i] + et_15[i];

    for(TC = 0; TC < NUM_TC; TC++) {

        // Determining the T counter occupancy
        for(EB = 0; EB < numBins; EB++) {

            index = numBins * TC + EB;
            TCsum[TC] += et[index];

        }

        if(TCsum[TC])
            for(EB = 0; EB < numBins; EB++) {

                index = numBins * TC + EB;

                if(et[index]) {

                    etFract[index] = et[index] / TCsum[TC];
                    etFractFU[index] = binomialU(etFract[index], TCsum[TC]) /
                                       etFract[index];

                }

                if(etFract[index] && NphoTC[TC]) {

                    newPartition = NphoTC[TC] * etFract[index];
                    NphoEB[EB] += newPartition;
                    NphoEBU[EB] = sqrt(sq(NphoEBU[EB]) + sq(newPartition) *
                                       (sq(NphoTCFU[TC]) + sq(etFractFU[index])));

                }
            } // for(EB = 0; EB < numBins; EB++)
    } // for(TC = 0; TC < NUM_TC; TC++)

    return;

} // void photonsTC2EB

/*---------------------------------------------------------------------------*/

float sq(float x) {

    return(x * x);

}

/*---------------------------------------------------------------------------*/

void taggingRatio() {

    FILE *fp = NULL;

    char name[100];
    char *outfile = NULL;

    float mor[NUM_TC];
    float morU[NUM_TC];
    float tac[NUM_TC];
    float tacU[NUM_TC];
    float tagRatio[NUM_TC];
    float tagRatioU[NUM_TC];

    float mor1[NUM_TC];
    float mor1U[NUM_TC];
    float tac1[NUM_TC];
    float tac1U[NUM_TC];
    float tagRatio1[NUM_TC];
    float tagRatio1U[NUM_TC];

    int TC;

    memset(&mor[0], 0, sizeof(mor));
    memset(&morU[0], 0, sizeof(morU));
    memset(&tac[0], 0, sizeof(tac));
    memset(&tacU[0], 0, sizeof(tacU));
    memset(&tagRatio[0], 0, sizeof(tagRatio));
    memset(&tagRatioU[0], 0, sizeof(tagRatioU));

    memset(&mor1[0], 0, sizeof(mor1));
    memset(&mor1U[0], 0, sizeof(mor1U));
    memset(&tac1[0], 0, sizeof(tac1));
    memset(&tac1U[0], 0, sizeof(tac1U));
    memset(&tagRatio1[0], 0, sizeof(tagRatio1));
    memset(&tagRatio1U[0], 0, sizeof(tagRatio1U));

    hunpak(201, mor, "HIST", 1);
    hunpke(201, morU, "HIST", 1);

    hunpak(202, tac, "HIST", 1);
    hunpke(202, tacU, "HIST", 1);

    hunpak(211, mor1, "HIST", 1);
    hunpke(211, mor1U, "HIST", 1);

    hunpak(212, tac1, "HIST", 1);
    hunpke(212, tac1U, "HIST", 1);


    for(TC = 0; TC < NUM_TC; TC++) {
        if(mor[TC] && tac[TC]) {

            tagRatio[TC] = tac[TC] / mor[TC];
            tagRatioU[TC] = binomialU(tagRatio[TC], mor[TC]);

        }

        if(mor1[TC] && tac1[TC]) {

            tagRatio1[TC] = tac1[TC] / mor1[TC];
            tagRatio1U[TC] = binomialU(tagRatio1[TC], mor1[TC]);

        }
    }


    hpak(200, tagRatio);
    hpake(200, tagRatioU);

    hpak(210, tagRatio1);
    hpake(210, tagRatio1U);

    sprintf(name, "gflux%d_tag_ratio.dat", runNo);

    outfile = &name[0];
    fp=fopen(outfile, "w");

    for(TC = 0; TC < NUM_TC; TC++)
        fprintf(fp, "%f %f\n", tagRatio[TC], tagRatioU[TC]);

    fclose(fp);

    return;

}

/*------------------------------End of File----------------------------------*/
