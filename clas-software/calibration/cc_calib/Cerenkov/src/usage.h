#ifndef cc_OPTIONS_H
#define cc_OPTIONS_H

#include <string>
#include <map>
#include <vector>
using namespace std;

/// \class opts
/// <b> opts </b>\n\n
/// Single Argument class.\n
/// - arg:  double assigned to argument.
/// - args: string assigned to argument.
/// - name: name to be displayed for the argument variable.
/// - help: help for the argument variable.
/// - type: 0 = number, 1 = string
class opts
{
 public:
   double  arg;  ///< double assigned to argument.
   string args;  ///< string assigned to argument.
   string name;  ///< name to be displayed for the argument variable.
   string help;  ///< help for the argument variable.
   int    type;  ///< 0 = number, 1 = string
   string ctgr;  ///< help category
};


/// \class cc_opts
/// <b> cc_opts </b>\n\n
/// This is the cc options class. It contains a map of opts which key is
/// the command line argument, the list of files, and the fit parameters limits\n
class cc_opts
{

 public:

   cc_opts();
  ~cc_opts();
   int Set(int argc, char **args); ///< Sets map from command line arguments


   map<string, opts> args;         ///< Options map
   vector<string> ifiles;

   int mean_min, mean_max, sigm_min, sigm_max, back_min, back_max;  ///< Fit Parameters Limits

};

#include <sstream>
inline string stringify(double x)
{
  ostringstream o;
  o << x;
  return o.str();
}

inline string stringify(int x)
{
  ostringstream o;
  o << x;
  return o.str();
}

#endif





