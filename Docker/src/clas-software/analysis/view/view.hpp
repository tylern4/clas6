#ifndef __VIEW_HPP__
#define __VIEW_HPP__

/**
 * view.hpp
 *
 * This includes a generic main() which calls
 * two functions:
 *   void processEvent(clasEvent &event, bool verbose, bool silent)
 *   void printLabels()
 *
 * These are defined in a .cpp file which contains the line:
 *   #include "view.hpp"
 **/
#include <csignal>
#include <iostream>
#include <unistd.h>

extern "C" {
    #include <ntypes.h>
    #include <bostypes.h>
    #include <clas_cern.h>
    #include <particleType.h>
    #include <kinematics.h>
    #include <pdgutil.h>
    #include <pid.h>
    #include <dataIO.h>
    #include <itape.h>
    #include <vertex.h>
    #include <trk.h>
    #include <makebanks.h>
}

#include <Vec.h>
#include <lorentz.h>
#include <pputil.h>
#include <clasEvent.h>

/**
 * the bos common
 **/
BOSbank bcs_;
BOSbank wcs_;

int CurrentRun = 0;
int CurrentEvent = 0;

/**
 * Function prototypes
 * (defined in the individual program .cpp files)
 **/
bool processEvent(clasEvent &event, bool verbose, bool quiet);
void printLabels();

/**
 * Fault handlers
 **/
static void signalINT(int isig) {
    cerr << "Run/Event: " << CurrentRun
        << " " << CurrentEvent
        << endl;
    exit(0);
}
static void signalSEGV(int isig) {
    cerr << "signalSEGV: Caught signal " << endl;
    cerr << "Run/Event: " << CurrentRun
        << " " << CurrentEvent
        << endl;
    exit(0);
}
void installFaultHandlers() {
    signal(SIGINT,signalINT);
    signal(SIGSEGV,signalSEGV);
}

void printUsage(char *processName) {
    cerr << processName << " [options] file1 file2 ...\n"
        << "\t[-o<string>]\toutput bos file (default: no bos output)\n"
        << "\t[-m<int>]\t\tprocess only <int> number of events\n"
        << "\t[-v]\t\tverbose mode\n"
        << "\t[-q]\t\tquiet (batch) mode\n"
        << "\t[-l]\t\tprint labels\n"
        << "\t[-h]\t\tprint this message\n";
}

int StartRun(int runNo) {
    int static CurrentRun = -1;
    if(CurrentRun != runNo) {
        CurrentRun = runNo;
    }
    return 0;
}

int main(int argc,char** argv) {
    /**
     * argument handler
     **/
    char *argptr;
    clasOutput coutput;
    bool out_to_bosfile = false;
    int max = 0;
    bool quiet = false;
    bool verbose = false;

    for(int i=1; i<argc; ++i) {
        argptr = argv[i];
        if(*argptr == '-') {
            argptr++;
            switch(*argptr) {
case 'o':
    if (*(++argptr)) {
        unlink(argptr);
        int status = coutput.open(argptr, 9);
        out_to_bosfile = true;
        cerr << "output bos file: " << argptr << endl;
    }
    break;
case 'm': max = atoi(++argptr); break;
case 'q': quiet = true; break;
case 'v': verbose = true; break;
case 'l': printLabels(); exit(0); break;
case 'h': printUsage(argv[0]); exit(0); break;
                default:
                    cerr << "unrecognized argument: " << argptr << endl;
                    break;
            }
        }
    }

    installFaultHandlers();

    /// Initialize BOS
    int MaxBanks = 1000;
    bnames_(&MaxBanks);
    initbos();
    configure_banks(stderr,0);

    clasHEAD_t *HEAD;

    size_t nFiles = 0;
    size_t nEvents = 0;
    size_t nProc = 0;

    for(int i=1; i<argc; ++i) {
        argptr = argv[i];
        if(*argptr != '-') {
            cerr << "reading bos file: " << argptr << endl;
            if(!quiet) clog << "events: processed / total / files\n";
            clasEvent event(argptr, &bcs_, 1, 0);
            ++nFiles;
            if(event.status()) {
                while((max ? nEvents < max : 1) && event.read(1)) {
                    if(event.type() == 1) {
if(HEAD = (clasHEAD_t*) getBank(&bcs_, "HEAD")) {
    CurrentRun   = HEAD->head[0].nrun;
    CurrentEvent = HEAD->head[0].nevent;
    StartRun(HEAD->head[0].nrun);
    try {
        if(processEvent(event, verbose, quiet)) {
            if(out_to_bosfile) {
                coutput.write(&bcs_);
            }
            ++nProc;
        }
    } catch(exception &e) {
        cerr << "exception caught: " << e.what() << endl;
    }
}
                    }
                    event.clean();
                    if(!(++nEvents % 1000) && !quiet) {
                        clog << nProc
                            << '\t' << nEvents
                            << '\t' << nFiles
                            << '\r';
                    }
                }
                if(!quiet) {
                    clog << nProc
                        << '\t' << nEvents
                        << '\t' << nFiles
                        << endl;
                }
            } else {
                cerr << "unable to open " << argptr << endl;
            }
        }
    }
    if(!quiet) {
        clog << nProc
            << '\t' << nEvents
            << '\t' << nFiles
            << endl;
    }
    return 0;
}

#endif /** __VIEW_HPP__ **/
