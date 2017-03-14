/**
 * view-st.cpp
 *
 * for use with makent/makeht scripts.
 * first, create the ascii ntuple:
 * > view-st in.bos | cview 'ST ' > st.view
 *
 * then make the root macro with makent:
 * > view-st -l > st.lab
 * > makent st st.lab > st.C
 * > root st.C
 *
 * the you can make the C++ analysis program with makeht:
 * > view-st -l | makeht > st.cpp
 *
 * compile st.cpp as per instructions at botton of st.cpp file
 *
 * > cat st.view | st outfile.root
 * > root outfile.root
 * root[0] .ls // to get a listing of histograms created
 **/
#include <cmath>
#include <map>
#include <vector>

#include "view.hpp"

bool processEvent(clasEvent &event, bool verbose, bool silent) {
    int pid;
    double rfvtime;
    double stvtime;
    int st_paddle_id;

    for(int i = 1; i <= event.Ncp(); ++i) {
        clasParticle part = event.cp(i);
        if(part.Q() && part.isSThit()) {
            clasSThit st_hit = part.SThit();
            pid = part.pid();
            rfvtime = event.vtime();
            stvtime =  part.stVtime();
            st_paddle_id = st_hit.id();

            if(
                fabs(stvtime - rfvtime) < 4
            ) {
                cout << "ST"
                    << ' ' << event.run()
                    << ' ' << pid
                    << ' ' << rfvtime
                    << ' ' << stvtime
                    << ' ' << st_paddle_id
                    << endl;
                return true;
            }
        }
    }
    return false;
}

#define COLUMN(vec, tag, desc) vec.push_back(make_pair(tag,desc));

void printLabels() {
    string name = "ST";
    string desc = "start counter";

    /// vector< ntuple_id, description > columns
    vector<pair<string, string> > cols;
    COLUMN(cols, "run",      "run number")
    COLUMN(cols, "pid",      "particle ID")
    COLUMN(cols, "rfvtime",  "RF vertex time")
    COLUMN(cols, "stvtime",  "ST vertex time")
    COLUMN(cols, "stpaddle", "ST paddle ID")

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
