/**
 * skim-gflux.cpp
 *
 * This reads in a BOS files and outputs only the banks needed
 * for the flux calculation by gflux. This includes the banks:
 *
 *  physics events (HEAD.type: 1-9)
 *      HEAD
 *      CALL
 *      TGTL
 *      TGTR
 *      TAGE
 *  Scalar events (HEAD.type: 10)
 *      (complete event)
 **/

#include <iostream>

#include "args.hpp"
#include "bos_handle.h"

using namespace std;
using namespace clas::bos;

int main(int argc, char** argv)
{
    // Begin command line option handler
    bool quiet = false;
    bool verbose = false;
    int max = 0;
    string outfile = "out.bos";
    vector<string> infiles;

    po::options_description arg_specific("Program specific options");
    arg_specific.add_options()
        ("max,m",
            po::value<int>(&max)->default_value(max),
            "maximum number of events to process.")
        ;

    process_program_options(
        argc, argv,
        quiet,
        verbose,
        outfile,
        infiles,
        arg_specific );

    if (!quiet)
    {
        clog << "Max: " << max << endl;
    }
    // End command line option handler


    clasHEAD_t* HEAD;

    size_t nFiles = 0;
    size_t nEvents = 0;
    size_t nPhys = 0;
    size_t nScalar = 0;

    try
    {
        clog << "Opening output file: " << outfile << "...\n";
        openBOSOutputFile(outfile);
    }
    catch (domain_error& e)
    {
        clog << "domain_error caught: " << e.what() << endl;
    }

    for (int i=0; i<infiles.size(); i++)
    {
        try
        {
            clog << "Opening input file: " << infiles[i] << "...\n";
            openBOSInputFile(infiles[i]);
        }
        catch (domain_error& e)
        {
            clog << "domain_error caught: " << e.what() << endl;
        }
        nFiles++;

        while(getBOSEvent())
        {
            HEAD = (clasHEAD_t*) getBank("HEAD");
            int evtype = HEAD->head[0].type;

            if (0 < evtype && evtype < 10)
            {
                writeBOS("HEADCALLTGTLTGTRTAGE");
                nPhys++;
            }
            else if (evtype == 10)
            {
                writeBOS("E");
                nScalar++;
            }

            cleanBOS();

            if (!(++nEvents % 1000) && !quiet)
            {
                clog          << nPhys
                    << "    " << nScalar
                    << "    " << nEvents
                    << "    " << nFiles
                    << "    \r";
            }

        } /// end event loop

        try
        {
            closeBOSInput();
        }
        catch (domain_error& e)
        {
            clog << "domain_error caught: " << e.what() << endl;
        }

        if (!quiet)
        {
            clog          << nPhys
                << "    " << nScalar
                << "    " << nEvents
                << "    " << nFiles
                << "    \n";
        }

    } /// end infile loop

    try
    {
        closeBOSOutput();
    }
    catch (domain_error& e)
    {
        clog << "domain_error caught: " << e.what() << endl;
    }

    if (!quiet)
    {
        clog          << nPhys
            << "    " << nScalar
            << "    " << nEvents
            << "    " << nFiles
            << "    \n";
    }

    return 0;
}
