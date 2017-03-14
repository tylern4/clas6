#ifndef __ARGS_HPP__
#define __ARGS_HPP__

#include <iostream>
#include <string>
#include <vector>

#include <boost/program_options.hpp>

namespace po = boost::program_options;

/**
 available option schemes:

po::variables_map process_program_options(argc, argv, quiet, verbose, outfile, infiles, specific=none)
po::variables_map process_program_options(argc, argv, quiet, verbose, infiles, specific=none);

**/

po::variables_map process_program_options(
    int argc, char** argv,
    bool& quiet,
    bool& verbose,
    std::string& outfile,
    std::vector<std::string>& infiles,
    po::options_description specific =  po::options_description("Program Specific Options"))
{
    po::options_description generic("Generic options");
    generic.add_options()
        ("help,h",
            "produce help message.")
        ("quiet,q","quiet mode.")
        ("verbose,v","verbose mode.")
        ("outfile,o",
            po::value<std::string>(&outfile)->default_value(outfile),
            "output file name.")
    ;
    po::options_description hidden("Hidden options");
    hidden.add_options()
        ("input-files",
            po::value< std::vector<std::string> >(&infiles),
            "input file(s)")
    ;

    po::positional_options_description positional_options;
    positional_options.add("input-files",-1);

    po::options_description all_options;
    all_options.add(generic).add(hidden).add(specific);

    po::options_description visible_options;
    visible_options.add(generic).add(specific);

    po::variables_map vm;
    po::store(po::command_line_parser(argc, argv).
        options(all_options).
        positional(positional_options).
        run(), vm);
    po::notify(vm);

    if (vm.count("help") || infiles.size() == 0)
    {
        std::clog << visible_options << std::endl;
        exit(0);
    }
    if (vm.count("quiet")) { quiet = true; }
    if (vm.count("verbose")) { verbose = true; }

    if (!quiet)
    {
        int i;
        std::clog << std::boolalpha;
        std::clog << "Quiet:       " << quiet << std::endl;
        std::clog << "Verbose:     " << verbose << std::endl;
        std::clog << "Output file: " << outfile << std::endl;
        std::clog << "Intput file(s):";
        for (i=0; i<infiles.size(); i++)
        {
            std::clog << ' ' << infiles[i];
            if (i > 5)
            {
                std::clog << "...\n";
                break;
            }
        }
        std::clog << std::endl;
        std::clog << std::noboolalpha;
    }
    return vm;
}

po::variables_map process_program_options(
    int argc, char** argv,
    bool& quiet,
    bool& verbose,
    std::vector<std::string>& infiles,
    po::options_description specific =  po::options_description("Program Specific Options") )
{
    po::options_description generic("Generic options");
    generic.add_options()
        ("help,h",
            "produce help message.")
        ("quiet,q","quiet mode.")
        ("verbose,v","verbose mode.")
    ;
    po::options_description hidden("Hidden options");
    hidden.add_options()
        ("input-files",
            po::value< std::vector<std::string> >(&infiles),
            "input file(s)")
    ;

    po::positional_options_description positional_options;
    positional_options.add("input-files",-1);

    po::options_description all_options;
    all_options.add(generic).add(hidden).add(specific);

    po::options_description visible_options;
    visible_options.add(generic).add(specific);

    po::variables_map vm;
    po::store(po::command_line_parser(argc, argv).
        options(all_options).
        positional(positional_options).
        run(), vm);
    po::notify(vm);

    if (vm.count("help") || infiles.size() == 0)
    {
        std::clog << visible_options << std::endl;
        exit(0);
    }
    if (vm.count("quiet")) { quiet = true; }
    if (vm.count("verbose")) { verbose = true; }

    if (!quiet)
    {
        int i;
        std::clog << std::boolalpha;
        std::clog << "Quiet:       " << quiet << std::endl;
        std::clog << "Verbose:     " << verbose << std::endl;
        std::clog << "Intput file(s):";
        for (i=0; i<infiles.size(); i++)
        {
            std::clog << ' ' << infiles[i];
            if (i > 5)
            {
                std::clog << "...\n";
                break;
            }
        }
        std::clog << std::endl;
        std::clog << std::noboolalpha;
    }
    return vm;
}
#endif /* __ARGS_HPP__ */
