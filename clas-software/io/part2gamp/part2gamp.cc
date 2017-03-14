/*
  part2gamp: convert CLAS PART bank to gamp format

 */

#include <cstdlib>
#include <cstdio>
#include <string>
#include <vector>
#include <cmath>
#include <iostream>
#include <cstring>

using namespace std;

/*------example PAW Ntuple structure---------------------------*/
extern "C" {
#include <signal.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <clas_cern.h>
#include <particleType.h>
#include <kinematics.h>
#include <pdgutil.h>
#include <pid.h>
#include <scalers.h>
#include <utility.h>
#include <printBOS.h>
#include <ec.h>
#include <PartUtil.h>
#include <map_manager.h>


using namespace std;

#define MELECTRON .00051

  /* ----------- Function prototypes ---------------- */
void ctrlCHandle(int);
void PrintUsage(char *processName);
  /* declare the bos common */
BOSbank bcs_;

}
#include <tag_cpp_wrapper.h>

int maxx = 0;
#include <kinematics_o.h>
#include <clasPART_o.h>
clasPART_t *make_pizero_part_bank(int GroupNo, int newpart);
part_t makeBeam(int pid, vector4_t p );
void printgamp(part_t *part);
float beamP(int run);
double Number(double x);

/*#include <matrix.h>
#include <Vec.h>
#include <lorentz.h>*/




/* --------------------------------------------------- */

void PrintUsage(char *processName)
{
  cerr << "Usage: " << processName << " [-M] file1 [file2] etc....\n\n";
  cerr << "  Options:\n";
  cerr << "\t-M[#]      \tPrint out only # number of events\n";
  cerr << "\t-p[#]      \tPrint this number part bank out\n";
  cerr << "\t-C         \tmonteCarlo mode, generate beam from TAGR->tag[0].erg\n";
  cerr << "\t-e[#]\t\telectron beam, if # is present, set beam energy to #" << endl;
  cerr << "\t-R#\t\tUse Run number # for tagger initialization" << endl;
  cerr << "\t-l#,#,...\t\tOnly print listed pid's" << endl;
  cerr << "\t-h         \tPrint this message\n";
  exit(0);
}



main(int argc,char **argv)
{
  FILE *fp = NULL;
  char *word;
  int i;
  char *argptr, *outfile = NULL;
  int Nevents = 0;
  char mess[100];
  int ret;
  ios::sync_with_stdio();
  int partno = 0;
  int monteCarlo = 0;
  int verbose = 0;
  int Verbose = 0;
  clasHEAD_t *HEAD;
  double beamp;
  double SetBeamp = -1000.0;

  int ElectronRun = 0;
  int CurrentRun = 0;
  int UseInitRun = 0;

  int nskip = 0;

  std::vector<int> accept;
  vector<int>::iterator pids;

  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);
  for (i=1; i<argc; i++) {
    argptr = argv[i];
    if (*argptr == '-') {
      argptr++;
      switch (*argptr) {
      case 's':
    nskip = atoi(++argptr);
    break;
      case 'v':
    verbose = 1;
    break;
      case 'V':
    Verbose = 1;
    break;
      case 'h':
    PrintUsage(argv[0]);
    break;
      case 'M':
    maxx = atoi(++argptr);
    break;
      case 'p':
    partno = atoi(++argptr);
    break;
      case 'R':
    UseInitRun = atoi(++argptr);
    break;
      case 'C':
    monteCarlo = 1;
    break;
      case 'e':
    ElectronRun = 1;
    if (strlen(++argptr)) {
      SetBeamp = atof(argptr);
    }
    break;
      case 'l':
    word = strtok(++argptr,",");
    while (word) {
      int pid = atoi(word);
      accept.push_back(pid);
      word = strtok(NULL,",");
    }
    break;

      default:
    cerr << "Unrecognized argument:\t" << argptr << endl;
    PrintUsage(argv[0]);
    break;
      }
    }
  }
  cerr << accept.size() << endl;
  for(pids = accept.begin(); pids != accept.end(); pids++)
    {
      cerr << "Accept: " << *pids << endl;
    }

  /*initialize bos and hbook*/
  initbos();

  for (int iarg = 1;iarg < argc; ++iarg) {
    argptr = argv[iarg];
    if (*argptr != '-') {
      sprintf(mess,"OPEN BOSINPUT UNIT=1 FILE=\"%s\" READ", argptr);
      if (!fparm_c(mess)) {
    cerr << "Unable to open file" <<  argptr << " " << strerror(errno) << endl;
      }
      else {
    int Nwrite = 0;
    int Nfile = 0;
    cerr << "opening file: " << argptr << " " << iarg << " " << argv[iarg] << endl;
    while ((maxx ? Nevents < maxx : 1) && getBOS(&bcs_,1,"E")) {
      clasTBID_t *TBID = (clasTBID_t *)getBank(&bcs_,"TBID");
      clasTAGR_t *TAGR = (clasTAGR_t *)getBank(&bcs_, "TAGR");
      if (nskip ? (Nevents > nskip) : 1) {
        clasPART_t *PART = NULL;

        if (HEAD = (clasHEAD_t *)getBank(&bcs_,"HEAD")) {
          if ((CurrentRun != HEAD->head[0].nrun) &&  ElectronRun) {
        beamp = (SetBeamp < 0.0) ? beamP(HEAD->head[0].nrun) : SetBeamp;

          }
          else if ((CurrentRun != HEAD->head[0].nrun)) {

        tagtcl_set_def_();
        tag_init_();
        tag_brun_(UseInitRun ? &UseInitRun : &HEAD->head[0].nrun);
        make_TGEO(UseInitRun ? UseInitRun : HEAD->head[0].nrun);
          }
          CurrentRun = HEAD->head[0].nrun;
        }




        if (PART = (clasPART_t *)getGroup(&bcs_, "PART", partno)){
          int nprint = 0;
          // first compute number to print
          if (accept.size()) {
        for (int i = 0; i < PART->bank.nrow; ++i) {
          for(pids = accept.begin(); pids != accept.end(); pids++)
            {
              if (PART->part[i].pid == *pids) {
            nprint++;
              }
            }
        }
          }
          else
        nprint = PART->bank.nrow;

          int nout = (monteCarlo || ElectronRun) ? nprint + 1 : nprint + 1;
          cout << "GAMP " << nout << endl;
          if (monteCarlo){
        if (TAGR){
          cout << "GAMP 1  0  0.0 0.0 " << TAGR->tagr[0].erg << " " << TAGR->tagr[0].erg << endl;
        }
          }
          else if (ElectronRun) {
        cout << "GAMP 3 -1 0.0 0.0 " << beamp << " " << sqrt(MELECTRON * MELECTRON + beamp * beamp) << endl;
          }
          else {
        beamp = get_photon_energy(TAGR,(clasBID_t *)TBID);
        cout << "GAMP 1 0 0.0 0.0 " << beamp << " " << beamp << endl;
          }
          for (i=0; i< PART->bank.nrow; i++){
        if (!(monteCarlo && PART->part[i].flags == BEAM_FLAG))
            if (accept.size()) {
              for(pids = accept.begin(); pids != accept.end(); pids++)
            {
              if (PART->part[i].pid == *pids) {
                printgamp(&(PART->part[i])); // do not print the beam twice if mcmode
              }
            }
            }
            else
              printgamp(&(PART->part[i])); // do not print the beam twice if mcmode

          }
          if (Verbose) {
        if (HEAD = (clasHEAD_t *)getBank(&bcs_,"HEAD"))
          cerr << HEAD->head[0].nrun << " " << HEAD->head[0].nevent << " " << Nevents <<  endl;
          }
        }

        if (verbose) {
          if (! (Nevents % 100))
        cerr << Nevents << "\t\r" << flush;
        }
        dropAllBanks(&bcs_,"E");
        cleanBanks(&bcs_);
      }
      Nevents++;
      Nfile++;
    }
    cerr << "#  of events processed (" << argptr << "):\t" << Nfile << " " << Nevents << endl;
    sprintf(mess,"CLOSE BOSINPUT", argptr);
    fparm_c(mess);
      }
    }

  }
  cerr << "Total # of events processed:\t" << Nevents << endl;
}

void printgamp(part_t *part){
  cout << "GAMP " << part->pid << " " << gchrg(part->pid) << " "<< Number(part->p.space.x) << " ";
  cout << Number(part->p.space.y) << " " << Number(part->p.space.z) << " " << Number(part->p.t) << endl;
}

void ctrlCHandle(int x)
{
  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);
  maxx = 1;
  fprintf(stderr,"\n\n\t\t\t***  INTERRUPTED!!!  ***\n\n");
}

float beamP(int run)
{
  char *map = "RUN_CONTROL.map";
  char *subsystem = "beam";
  char *item = "energy";
  int length = 1;
  int firsttime;
  float p;
  char x[1000];
  float ret;


      sprintf(x,"%s/Maps/%s",getenv("CLAS_PARMS"),map);
      map_get_float(x  ,subsystem,item,length,&p,run,&firsttime);
      cerr << "BEAM MOMENTUM for Run " << run << " = " << p << " MeV " << endl;
      return(p/1000.0);
}

double Number(double x)
{

  return(isnan(x) ? 1000.0 : x);
}



/* end file */
