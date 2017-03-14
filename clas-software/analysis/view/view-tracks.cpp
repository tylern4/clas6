/**
 * view-tag.cpp
 *
 * for use with makent/makeht scripts.
 * first, create the ascii ntuple:
 * > view-tag in.bos | cview 'TAG ' > tag.view
 *
 * then make the root macro with makent:
 * > view-tag -l > tag.lab
 * > makent tag tag.lab > tag.C
 * > root tag.C
 *
 * the you can make the C++ analysis program with makeht:
 * > view-tag -l | makeht > tag.cpp
 *
 * compile tag.cpp as per instructions at botton of tag.cpp file
 *
 * > cat tag.view | tag outfile.root
 * > root outfile.root
 * root[0] .ls // to get a listing of histograms created
 **/
#include <cmath>
#include <map>
#include <vector>

#include "view.hpp"

bool processEvent(clasEvent &event, bool verbose, bool silent)
{
    event.eLoss();

    int track_id;
	int part_id;
    int i;
	int j;

	int pid=0;
    double p=0;

	double stbeta=0.;
	double scbeta=0.;
    double beta=0.;

	double stm=0.;
	double scm=0.;
	double m=0.;

	clasPART_t* PART;
    clasTBID_t* TBID;
    clasTBTR_t* TBTR;

    if (TBID = (clasTBID_t*) getBank(&bcs_,"TBID"))
    {
        for (i=0; i<TBID->bank.nrow; ++i)
        {
            if (TBID->tbid[i].track)
            {
                track_id = TBID->tbid[i].track - 1;
                if (TBTR = (clasTBTR_t*) getBank(&bcs_,"TBTR"))
                {
                    if (TBTR->tbtr[track_id].q)
                    {
if (PART = (clasPART_t*) getBank(&bcs_,"PART"))
{
	for (j=0; j<PART->bank.nrow; ++j)
	{
		if (PART->part[j].trkid - 1 == track_id)
		{
			part_id = j;
			break;
		}
	}
	pid = PART->part[part_id].pid;
}
p = sqrt(
	pow(TBTR->tbtr[track_id].p.x,2) +
    pow(TBTR->tbtr[track_id].p.y,2) +
    pow(TBTR->tbtr[track_id].p.z,2) );
if (TBID->tbid[i].st_stat)
{
    stbeta = TBID->tbid[i].st_beta;
	stm = (p / stbeta) * sqrt(1 - pow(stbeta,2));
}
if (TBID->tbid[i].sc_stat)
{
    scbeta = TBID->tbid[i].sc_beta;
	scm = (p / scbeta) * sqrt(1 - pow(scbeta,2));
}
beta = TBID->tbid[i].beta;
m = (p / beta) * sqrt(1 - pow(beta,2));

cout << "TRACKS"
	<< ' ' << event.run()
	<< ' ' << event.event()
	<< ' ' << pid
	<< ' ' << p
	<< ' ' << beta
	<< ' ' << scbeta
	<< ' ' << stbeta
	<< ' ' << m
	<< ' ' << scm
	<< ' ' << stm
    << endl;
return true;
                    }
                }
            }
		}
	}

    return false;
}

#define COLUMN(vec, tag, desc) vec.push_back(make_pair(tag,desc))

void printLabels() {
    string name = "TRACKS";
    string desc = "track by track information";

    /// vector< ntuple_id, description > columns
    vector<pair<string, string> > cols;
    COLUMN(cols, "run", "run number");
	COLUMN(cols, "event", "event number");
	COLUMN(cols, "pid", "geant particle id");
	COLUMN(cols, "p", "momentum of track");
	COLUMN(cols, "beta", "beta");
	COLUMN(cols, "scbeta", "TOF beta");
	COLUMN(cols, "stbeta", "ST to TOF beta");
	COLUMN(cols, "m", "mass");
	COLUMN(cols, "scm", "TOF mass");
	COLUMN(cols, "stm", "ST mass");
	
    unsigned int nlab = 0;
    vector<pair<string, string> >::iterator itr = cols.begin();
    cout << "Labels for " << name << " (" << desc << "):" << endl;
    while(itr != cols.end()) {
        cout << ++nlab
            << "\t" << itr->second
            << " {" << itr->first << "}"
            << endl;
        ++itr;
    }
}

