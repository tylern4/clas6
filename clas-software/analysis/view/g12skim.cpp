/**
 * skim.cpp
 **/

#include <csignal>
#include <iostream>
#include <fstream>
#include <string>
#include <vector>

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

using namespace std;

/**
 * the bos common
 **/
BOSbank bcs_;
BOSbank wcs_;

/// 32bit machines don't like files larger than 2GB!
#define FILE_SIZE_MAX 2000000000

int CurrentRun = 0;
int CurrentEvent = 0;

size_t get_file_size(char* fname)
{
    ifstream ifs(fname, ifstream::binary | ifstream::ate);
    return ifs.tellg();
}

int n_positive()
{
    int n = 0;
    int track;
    int i;
    clasTBID_t* TBID;
    clasTBTR_t* TBTR;
    if (TBID = (clasTBID_t*) getBank(&bcs_,"TBID"))
    {
        for (i=0; i<TBID->bank.nrow; ++i)
        {
            if (TBID->tbid[i].track)
            {
                track = TBID->tbid[i].track - 1;
                if (TBTR = (clasTBTR_t*) getBank(&bcs_,"TBTR"))
                {
                    if (TBTR->tbtr[track].q > 0)
                    {
                        ++n;
                    }
                }
            }
        }
    }
    return n;
}

int n_negative()
{
    int n = 0;
    int track;
    int i;
    clasTBID_t* TBID;
    clasTBTR_t* TBTR;
    if (TBID = (clasTBID_t*) getBank(&bcs_,"TBID"))
    {
        for (i=0; i<TBID->bank.nrow; ++i)
        {
            if (TBID->tbid[i].track)
            {
                track = TBID->tbid[i].track - 1;
                if (TBTR = (clasTBTR_t*) getBank(&bcs_,"TBTR"))
                {
                    if (TBTR->tbtr[track].q < 0)
                    {
                        ++n;
                    }
                }
            }
        }
    }
    return n;
}

/**
 * calculates mass and determines if track looks like it might
 * be a kaon:
 *      1. momentum > 3.
 *      2. momentum > 2 and mass(beta,p) < (proton + kaon) / 2
 *      3. mass(beta,p) in range: [(pion + kaon) / 2, (proton + kaon) / 2]
 **/
bool is_possible_kaon(double p, double beta)
{
    /// min is midpoint betwee pion and kaon mass
    static double kaon_mass_min = ((0.493677 + 0.13957018) / 2.);
    /// max is midpoint between proton and kaon mass
    static double kaon_mass_max = ((0.938272 + 0.493677) / 2.);
    if (p > 3.)
    {
        return true;
    }
    double mass;
    mass = (p / beta) * sqrt(1 - pow(beta,2));
    if (p > 2.)
    {
        if (mass < kaon_mass_max)
        {
            return true;
        }
    }
    if (mass > kaon_mass_min && mass < kaon_mass_max)
    {
        return true;
    }
    return false;
}

bool has_possible_charged_kaon(clasEvent &event)
{
    #ifdef DEBUG
    clog << "looking for kaon\n";
    #endif
    int i;
    int nk;
    int npip;
    int npim;
    int npr;
    double p;
    #ifdef DEBUG
    clog << "looking for part kaon\n";
    #endif
    /// look at the PART bank first for a charged kaon
    nk = event.N(KPlus) + event.N(KMinus);
    if (nk)
    {
        #ifdef DEBUG
        clog << "found PART kaon\n";
        #endif
        return true;
    }
    #ifdef DEBUG
    clog << "looking for part pi+ that might be a kaon\n";
    #endif
    /// still using the PART bank
    /// take all events with a charged pion with monetum > 2 GeV
    npip = event.N(PiPlus);
    for (i=1; i<=npip; ++i)
    {
        p = event.cp(PiPlus,i).p().V().r();
        if (p > 2.)
        {
            #ifdef DEBUG
            clog << "found part pion that might be kaon\n";
            #endif
            return true;
        }
    }
    #ifdef DEBUG
    clog << "looking for part pi- that might be a kaon\n";
    #endif
    npim = event.N(PiMinus);
    for (i=1; i<=npim; ++i)
    {
        p = event.cp(PiMinus,i).p().V().r();
        if (p > 2.)
        {
            #ifdef DEBUG
            clog << "found part pion that might be kaon\n";
            #endif
            return true;
        }
    }
    #ifdef DEBUG
    clog << "looking for part proton that might be a kaon\n";
    #endif
    /// and take all events with a proton with momentum > 3 GeV
    npr = event.N(Proton);
    for (i=1; i<=npr; ++i)
    {
        p = event.cp(Proton,i).p().V().r();
        if (p > 3.)
        {
            #ifdef DEBUG
            clog << "found part proton that might be kaon\n";
            #endif
            return true;
        }
    }
    /**
     * event looks like it might not have a kaon
     * but just to be sure, lets take all charged
     * tracks with:
     *      1. momentum > 3.
     *      2. momentum > 2 and mass(beta,p) < (proton + kaon) / 2
     *      3. mass(beta,p) in range: [(pion + kaon) / 2, (proton + kaon) / 2]
     **/
    int track;
    double beta;
    clasTBID_t* TBID;
    clasTBTR_t* TBTR;
    #ifdef DEBUG
    clog << "part banks has no kaons. looking at individual tracks now\n";
    #endif
    if (TBID = (clasTBID_t*) getBank(&bcs_,"TBID"))
    {
        for (i=0; i<TBID->bank.nrow; ++i)
        {
            if (TBID->tbid[i].track)
            {
                #ifdef DEBUG
                clog << "good tbid track\n";
                #endif
                track = TBID->tbid[i].track - 1;
                if (TBTR = (clasTBTR_t*) getBank(&bcs_,"TBTR"))
                {
                    if (TBTR->tbtr[track].q)
                    {
                        #ifdef DEBUG
                        clog << "good tbid/tbtr charged track\n";
                        #endif
                        p = sqrt(
                            pow(TBTR->tbtr[track].p.x,2) +
                            pow(TBTR->tbtr[track].p.y,2) +
                            pow(TBTR->tbtr[track].p.z,2) );
                        if (TBID->tbid[i].st_stat)
                        {
                            #ifdef DEBUG
                            clog << "using st_beta\n";
                            #endif
                            beta = TBID->tbid[i].st_beta;
                            if (is_possible_kaon(p, beta))
                            {
                                #ifdef DEBUG
                                clog << "found other possible kaon (st to tof beta)\n";
                                #endif
                                return true;
                            }
                        }
                        if (TBID->tbid[i].sc_stat)
                        {
                            #ifdef DEBUG
                            clog << "using sc_beta\n";
                            #endif
                            beta = TBID->tbid[i].sc_beta;
                            if (is_possible_kaon(p, beta))
                            {
                                #ifdef DEBUG
                                clog << "found other possible kaon (vtx to tof beta)\n";
                                #endif
                                return true;
                            }
                        }
                        #ifdef DEBUG
                        clog << "using beta\n";
                        #endif
                        beta = TBID->tbid[i].beta;
                        if (is_possible_kaon(p, beta))
                        {
                            #ifdef DEBUG
                            clog << "found other possible kaon (tbid.beta)\n";
                            #endif
                            return true;
                        }
                    }
                }
            }
        }
    }
    #ifdef DEBUG
    clog << "did not find possible kaon\n";
    #endif
    return false;
}

bool has1ckaon1ctrk(clasEvent &event)
{
    int n = n_positive() + n_negative();
    if (n > 1)
    {
        #ifdef DEBUG
        clog << "found " << n << " tracks\n";
        #endif
        if (has_possible_charged_kaon(event))
        {
            return true;
        }
    }
    return false;
}

bool has2pos1neg(clasEvent &event){
    int np = n_positive();
    int nm = n_negative();
    if ((np > 1) && (nm > 0))
    {
        #ifdef DEBUG
        clog << "found 2pos1neg event\n";
        #endif
        return true;
    }
    #ifdef DEBUG
    clog << "not a 2pos1neg event\n";
    #endif
    return false;
}

bool has1lepton(clasEvent &event){
    if(event.nMaybeLepton() || event.nIsLepton())
    {
        #ifdef DEBUG
        clog << "found possible lepton\n";
        #endif
        return true;
    }
    #ifdef DEBUG
    clog << "no possible leptons\n";
    #endif
    return false;
}

bool has2ctrk(clasEvent &event){
    int n = n_positive() + n_negative();
    if( n > 1 )
    {
        #ifdef DEBUG
        clog << "found " << n << " tracks\n";
        #endif
        return true;
    }
    return false;
}

bool has3ctrk(clasEvent &event){
    int n = n_positive() + n_negative();
    if( n > 2 )
    {
        #ifdef DEBUG
        clog << "found " << n << " tracks\n";
        #endif
        return true;
    }
    return false;
}

bool has4ctrk(clasEvent &event){
    int n = n_positive() + n_negative();
    if( n > 3 )
    {
        #ifdef DEBUG
        clog << "found " << n << " tracks\n";
        #endif
        return true;
    }
    return false;
}

bool hasppbar(clasEvent &event){
    int np = event.N(Proton);
    int npbar = event.N(AntiProton);
    if( np > 0 && npbar > 0)
    {
        #ifdef DEBUG
        clog << "found p pbar\n";
        #endif
        return true;
    }
    #ifdef DEBUG
    clog << "no p pbar\n";
    #endif
    return false;
}

/**
 * Fault handlers
 **/
static void signalINT(int isig)
{
    cerr << "Run/Event: " << CurrentRun
        << " " << CurrentEvent
        << endl;
    exit(0);
}
static void signalSEGV(int isig)
{
    cerr << "signalSEGV: Caught signal " << endl;
    cerr << "Run/Event: " << CurrentRun
        << " " << CurrentEvent
        << endl;
    exit(0);
}
void installFaultHandlers()
{
    signal(SIGINT,signalINT);
    signal(SIGSEGV,signalSEGV);
}

void printUsage(char *processName)
{
    clog << processName << " [options] -o output-file input-files ..." << endl;
    clog << "\t[-m#]\t\tprocess only # events" << endl;
    clog << "\t[-v]\t\tverbose mode" << endl;
    clog << "\t[-q]\t\tquiet (batch) mode" << endl;
    clog << "\t== these three skims are redundant with those above ==\n";
    clog << "\t[-l]<string>\t base directory location where files will be placed.\n";
    clog << "\t[-n]<string>\t output file name for each skim when used with -l option\n";
    clog << "\t\t files will be placed here: basedir/[skim-name]/outfilename\n";
    clog << "\t\t the options below will override the ones above.\n";
    clog << "\t== the first five skims form a complete set ==\n";
    clog << "\t[-a]<string>\t 1 charged kaon & 1 charged track, inclusive.\n";
    clog <<     "\t\t(1ckaon1ctrk.bos)\n";
    clog << "\t[-b]<string>\t 2 positive tracks & 1 negative track,\n";
    clog <<     "\t\tnot 1ckaon1ctrk (2pos1neg_not_1ckaon1ctrk.bos)\n";
    clog << "\t[-c]<string>\t 2 charged tracks, not 2pos1neg or 1ckaon1ctrk\n";
    clog <<     "\t\tevents (2ctrk_not_2pos1neg.bos)\n";
    clog << "\t[-d]<string>\t not 2ctrk or 2pos1neg or 1ckaon1ctrk events (not_2ctrk.bos)\n";
    clog << "\t[-e]<string>\t 'other' events are of non-data types (database, sync, etc).\n";
    clog <<     "\t\t(other.bos)\n";
    clog << "\t== these three skims are redundant with those above ==\n";
    clog << "\t[-f]<string>\t 1 lepton, inclusive (1lepton.bos)\n";
    clog << "\t[-g]<string>\t 4 charged tracks, inclusive (4ctrk.bos)\n";
    clog << "\t[-i]<string>\t 1 proton & 1 antiproton, inclusive (ppbar.bos)\n";
    clog << "\t[-h]\t\tprint this message" << endl;
}

int StartRun(int runNo)
{
    int static CurrentRun = -1;
    if(CurrentRun != runNo)
    {
        CurrentRun = runNo;
    }
    return 0;
}

int main(int argc,char** argv)
{
    /**
     * argument handler
     **/
    char *argptr;
    int max = 0;
    bool quiet = false;
    bool verbose = false;
    clasOutput kaon_coutput;
    clasOutput posneg_coutput;
    clasOutput twoctrk_coutput;
    clasOutput ntwoctrk_coutput;
    clasOutput other_coutput;
    clasOutput lepton_coutput;
    clasOutput fctrk_coutput;
    clasOutput ppb_coutput;
    int a_status = 0;
    int b_status = 0;
    int c_status = 0;
    int d_status = 0;
    int e_status = 0;
    int f_status = 0;
    int g_status = 0;
    int i_status = 0;
    string outdir = "";

    for(int i=1; i<argc; ++i)
    {
        argptr = argv[i];
        if(*argptr == '-')
        {
            argptr++;
            switch(*argptr)
            {
case 'm': max = atoi(++argptr); break;
case 'q': quiet = true; break;
case 'v': verbose = true; break;
case 'a':
    if (*(++argptr))
    {
        unlink(argptr);
        a_status = kaon_coutput.open(argptr, 9);
        cerr << "1ckaon1ctrk output file: " << argptr << endl;
    }
    break;
case 'b':
    if(*(++argptr))
    {
        unlink(argptr);
        b_status = posneg_coutput.open(argptr, 10);
        cerr << "2pos1neg output file: " << argptr << endl;
    }
    break;
case 'c':
    if(*(++argptr))
    {
        unlink(argptr);
        c_status = twoctrk_coutput.open(argptr, 11);
        cerr << "2ctrk output file: " << argptr << endl;
    }
    break;
case 'd':
    if(*(++argptr))
    {
        unlink(argptr);
        d_status = ntwoctrk_coutput.open(argptr, 12);
        cerr << "n2ctrk output file: " << argptr << endl;
    }
    break;
case 'e':
    if(*(++argptr))
    {
        unlink(argptr);
        e_status = other_coutput.open(argptr, 14);
        cerr << "other output file: " << argptr << endl;
    }
    break;
case 'f':
    if(*(++argptr))
    {
        unlink(argptr);
        f_status = lepton_coutput.open(argptr, 15);
        cerr << "1lepton output file: " << argptr << endl;
    }
    break;
case 'g':
    if(*(++argptr))
    {
        unlink(argptr);
        g_status = fctrk_coutput.open(argptr, 16);
        cerr << "4ctrk output file: " << argptr << endl;
    }
    break;
case 'i':
    if(*(++argptr))
    {
        unlink(argptr);
        i_status = ppb_coutput.open(argptr, 17);
        cerr << "ppbar output file: " << argptr << endl;
    }
    break;
case 'h': printUsage(argv[0]); exit(0); break;
                default:
                    cerr << "unrecognized argument: " << argptr << endl;
                    break;
            }
        }
    }

if ( !a_status )
{
    a_status = kaon_coutput.open((char*)"1ckaon1ctrk.bos",9);
}
if ( !b_status )
{
    b_status = posneg_coutput.open((char*)"2pos1neg_not_1ckaon1ctrk.bos",10);
}
if ( !c_status )
{
    c_status = twoctrk_coutput.open((char*)"2ctrk_not_2pos1neg.bos",11);
}
if ( !d_status )
{
    d_status = ntwoctrk_coutput.open((char*)"not_2ctrk.bos",12);
}
if ( !e_status )
{
    e_status = other_coutput.open((char*)"other.bos",13);
}

if ( !f_status )
{
    f_status = lepton_coutput.open((char*)"1lepton.bos",14);
}
if ( !g_status )
{
    g_status = fctrk_coutput.open((char*)"4ctrk.bos",15);
}
if ( !i_status )
{
    i_status = ppb_coutput.open((char*)"ppbar.bos",16);
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
    size_t kaon_nProc = 0;
    size_t posneg_nProc = 0;
    size_t twoctrk_nProc = 0;
    size_t ntwoctrk_nProc = 0;
    size_t other_nProc = 0;

    size_t filesize = 0;
    size_t kaon_nBytes = 0;
    size_t posneg_nBytes = 0;
    size_t twoctrk_nBytes = 0;
    size_t ntwoctrk_nBytes = 0;
    size_t other_nBytes = 0;

    size_t lepton_nProc = 0;
    size_t fctrk_nProc = 0;
    size_t ppb_nProc = 0;

    for(int i=1; i<argc; ++i)
    {
        argptr = argv[i];
        if(*argptr != '-')
        {
            cerr << "reading bos file: " << argptr << endl;
            if(!quiet) clog << "events: processed / total / files\n";
            clasEvent event(argptr, &bcs_, 1, 0);
            ++nFiles;
            if(event.status())
            {
                if (!quiet)
                {
                    clog << "1ckaon1ctrk 2pos1neg_not_1ckaon1ctrk 2ctrk_not_2pos1neg not_2ctrk other 1lepton 4ctrk ppbar\n";
                }

                while((max ? nEvents < max : 1) && event.read(1))
                {
                    if(event.type() == 1)
                    {
                        #ifdef DEBUG
                        clog << "new event\n";
                        #endif
if(HEAD = (clasHEAD_t*) getBank(&bcs_, "HEAD"))
{
    CurrentRun   = HEAD->head[0].nrun;
    CurrentEvent = HEAD->head[0].nevent;
    StartRun(HEAD->head[0].nrun);
    try
    {
        if ( has1ckaon1ctrk(event)
            || (has2pos1neg(event)
                && get_file_size(posneg_coutput.name()) > FILE_SIZE_MAX)
            || (has2ctrk(event)
                && get_file_size(posneg_coutput.name()) > FILE_SIZE_MAX
                && get_file_size(twoctrk_coutput.name()) > FILE_SIZE_MAX)
            || (get_file_size(posneg_coutput.name()) > FILE_SIZE_MAX
                && get_file_size(twoctrk_coutput.name()) > FILE_SIZE_MAX
                && get_file_size(ntwoctrk_coutput.name()) > FILE_SIZE_MAX) )
        {
            kaon_coutput.write(&bcs_);
            ++kaon_nProc;
            kaon_nBytes += 1;
        }
        else if ( has2pos1neg(event)
            || (has2ctrk(event)
                && get_file_size(twoctrk_coutput.name()) > FILE_SIZE_MAX)
            || (get_file_size(twoctrk_coutput.name()) > FILE_SIZE_MAX
                && get_file_size(ntwoctrk_coutput.name()) > FILE_SIZE_MAX) )
        {
            posneg_coutput.write(&bcs_);
            ++posneg_nProc;
            posneg_nBytes += 1;
        }
        else if ( has2ctrk(event)
            || (get_file_size(ntwoctrk_coutput.name()) > FILE_SIZE_MAX) )
        {
            twoctrk_coutput.write(&bcs_);
            ++twoctrk_nProc;
            twoctrk_nBytes += 1;
        }
        else
        {
            ntwoctrk_coutput.write(&bcs_);
            ++ntwoctrk_nProc;
            ntwoctrk_nBytes += 1;
        }


        if (has1lepton(event))
        {
            lepton_coutput.write(&bcs_);
            ++lepton_nProc;
        }
        if (has4ctrk(event))
        {
            fctrk_coutput.write(&bcs_);
            ++fctrk_nProc;
        }
        if (hasppbar(event))
        {
            ppb_coutput.write(&bcs_);
            ++ppb_nProc;
        }
    }
    catch(exception &e)
    {
        cerr << "exception caught: " << e.what() << endl;
    }
}
                    }
                    else
                    {
                        /// event.type() != 1
                        other_coutput.write(&bcs_);
                        ++other_nProc;
                    }
                    event.clean();
                    if(!(++nEvents % 1000) && !quiet)
                    {
                        clog        << kaon_nProc
                            << '\t' << posneg_nProc
                            << '\t' << twoctrk_nProc
                            << '\t' << ntwoctrk_nProc
                            << '\t' << other_nProc
                            << '\t' << lepton_nProc
                            << '\t' << fctrk_nProc
                            << '\t' << ppb_nProc
                            << '\t' << nEvents
                            << '\t' << nFiles
                            << '\r';
                    }
                }
                if(!quiet)
                {
                    clog        << kaon_nProc
                        << '\t' << posneg_nProc
                        << '\t' << twoctrk_nProc
                        << '\t' << ntwoctrk_nProc
                        << '\t' << other_nProc
                        << '\t' << lepton_nProc
                        << '\t' << fctrk_nProc
                        << '\t' << ppb_nProc
                        << '\t' << nEvents
                        << '\t' << nFiles
                        << '\r';
                }
            }
            else
            {
                cerr << "unable to open " << argptr << endl;
            }
        }
    }
    if(!quiet)
    {
        clog        << kaon_nProc
            << '\t' << posneg_nProc
            << '\t' << twoctrk_nProc
            << '\t' << ntwoctrk_nProc
            << '\t' << other_nProc
            << '\t' << lepton_nProc
            << '\t' << fctrk_nProc
            << '\t' << ppb_nProc
            << '\t' << nEvents
            << '\t' << nFiles
            << '\n';
    }
    kaon_coutput.write(&bcs_, (char*)"0");
    kaon_coutput.close();
    posneg_coutput.write(&bcs_, (char*)"0");
    posneg_coutput.close();
    twoctrk_coutput.write(&bcs_, (char*)"0");
    twoctrk_coutput.close();
    ntwoctrk_coutput.write(&bcs_, (char*)"0");
    ntwoctrk_coutput.close();
    other_coutput.write(&bcs_, (char*)"0");
    other_coutput.close();

    lepton_coutput.write(&bcs_, (char*)"0");
    lepton_coutput.close();
    fctrk_coutput.write(&bcs_, (char*)"0");
    fctrk_coutput.close();
    ppb_coutput.write(&bcs_, (char*)"0");
    ppb_coutput.close();

    return 0;
}

