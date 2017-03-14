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

bool processEvent(clasEvent &event, bool verbose, bool silent) {
    event.eLoss();

    int npr = event.N(Proton);
    int npip = event.N(PiPlus);
    int npim = event.N(PiMinus);
    if(npr==1 && npip && npim) {
        fourVec beam = event.getBeam(1.0,6.0);
        fourVec target;
            target.set(0.93827,0,0,0);
        fourVec pr1 = event.cp(Proton, 1).p();
        fourVec pip1 = event.cp(PiPlus, 1).p();
        fourVec pim1 = event.cp(PiMinus, 1).p();
        fourVec ppippim_m4p = beam + target - pr1 - pip1 - pim1;
        double ppippim_mm2 = ppippim_m4p.lenSq();

        if(fabs(ppippim_mm2) < 0.5) {
            double ppippim_mpz = ppippim_m4p.z();
            double ppippim_mpt = sqrt(
                pow(ppippim_m4p.x(),2)
              + pow(ppippim_m4p.y(),2) );
            double ppippim_mp  = ppippim_m4p.r();

            clasTaggedPhoton tagged_photon = event.ntag(0);
            int tid = tagged_photon.t_id();
            int eid = tagged_photon.e_id();
            double E = tagged_photon.E();
            double ttag = tagged_photon.ttag();
            double tpho = tagged_photon.tpho();

            double target_me = (pr1 + pip1 + pim1 - target).t();

            cout << "TAG"
                << ' ' << event.run()
                << ' ' << ppippim_mm2
                << ' ' << ppippim_mpz
                << ' ' << ppippim_mpt
                << ' ' << ppippim_mp
                << ' ' << tid
                << ' ' << eid
                << ' ' << E
                << ' ' << ttag
                << ' ' << tpho
                << ' ' << target_me
                << endl;
            return true;
        }
    }
    return false;
}

#define COLUMN(vec, tag, desc) vec.push_back(make_pair(tag,desc))

void printLabels() {
    string name = "TAG";
    string desc = "tagger energy and paddles";

    /// vector< ntuple_id, description > columns
    vector<pair<string, string> > cols;
    COLUMN(cols, "run", "run number");
    COLUMN(cols, "ppippim_mm2", "MM^2(p pi+ pi-)");
    COLUMN(cols, "ppippim_mpz", "MP_{Z}(p pi+ pi-)");
    COLUMN(cols, "ppippim_mpt", "MP_{t}(p pi+ pi-)");
    COLUMN(cols, "ppippim_mp", "MP(p pi+ pi-)");
    COLUMN(cols, "tid", "tagger t ID");
    COLUMN(cols, "eid", "tagger e ID");
    COLUMN(cols, "E", "tagger energy");
    COLUMN(cols, "ttag", "tagger vtime");
    COLUMN(cols, "tpho", "tagger/RF vtime");
    COLUMN(cols, "target_me", "MissEnergy(target) or beam energy from p pi+ pi-");

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
