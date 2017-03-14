#ifndef __CLAS_G12_PCOR_HPP__
#define __CLAS_G12_PCOR_HPP__

#include <cmath>
#include <fstream>
#include <iostream>
#include <map>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

namespace clas
{
namespace g12 {

using std::clog;
using std::endl;
using std::ifstream;
using std::fmod;
using std::getline;
using std::string;
using std::stringstream;
using std::vector;

namespace pid
{
    typedef enum particle_enum
    {
        electron   = 2,
        positron   = 3,
        piplus     = 8,
        piminus    = 9,
        kplus      = 11,
        kminus     = 12,
        proton     = 14,
    } particle_t;
}

class MomentumCorrection
{
  private:
    std::map<int, vector<vector<float> > >_pcor_map;
    bool _verbose;

  public:
    MomentumCorrection(const string& parms_dir = "/u/home/mkunkel/g12_corrections", bool verbose=false)
    {
        _verbose = verbose;

        _pcor_map[pid::proton] = vector<vector<float> >(6);
        _pcor_map[pid::piplus] = vector<vector<float> >(6);
        _pcor_map[pid::piminus] = vector<vector<float> >(6);
        _pcor_map[pid::kplus] = vector<vector<float> >(6);
        _pcor_map[pid::kminus] = vector<vector<float> >(6);

      //  /Volumes/Mac_Storage/YIELD/SINGLE_TRACK/YIELD_DATA/PRODUCTION/RUN_56515/DATA_FOR_FIX/g12_corrections
      
        //string infile = parms_dir+"/pcor/g12/pcor.txt";

      string infile = "/u/home/mkunkel/g12_corrections/pcor/g12/pcor.txt";

        ifstream fin(infile.c_str());
        if (!fin.is_open())
        {
          cout<<"Not opened"<<infile<<endl;

            //throw std::runtime_error(string("error opening pcor file: ")+infile);
        }

        string fin_line;
        string partid;
        int sec;
        float p1;
        float p0;

      
        if (_verbose)
        {
            clog << "\nGetting momentum correction parameters:\n";
        }
        while (fin.good())
        {
            getline(fin, fin_line);
            if (fin_line[0] != '#' && fin_line.size() > 0)
            {
                stringstream ss;
                ss << fin_line;
                ss >> partid >> sec >> p1 >> p0;

                pid::particle_t id;

                if (partid == "p")
                {
                    id = pid::proton;
                }
                else if (partid == "pi+")
                {
                    id = pid::piplus;
                }
                else if (partid == "pi-")
                {
                    id = pid::piminus;
                }
                else if (partid == "K+")
                {
                    id = pid::kplus;
                }
                else if (partid == "K-")
                {
                    id = pid::kminus;
                }

                _pcor_map[id][sec-1].push_back(p1);
                _pcor_map[id][sec-1].push_back(p0);

                if (_verbose)
                {
                    clog << "    " << partid << " " << sec
                         << " " << p1 << " " << p0 << endl;
                }
            }
        }
    }

    template <typename T>
    inline
    T pcor(const T& phi, const int& part_id)
    {
        /// adjust phi so it goes from -30 to 30
        static const T rad2deg = 180.0 / 3.14159265;
        T phi_shifted = fmod((phi*rad2deg+390.),360);
        int sec = int(phi_shifted / 60.) + 1;
        phi_shifted = fmod(phi_shifted,60) - 30.;


        T pcorrection = _pcor_map[part_id][sec-1][0] * phi_shifted + _pcor_map[part_id][sec-1][1];

        if (_verbose)
        {
            clog << "\npid, phi, phi_shifted, pcor= " << part_id
                << " " << phi << " " << phi_shifted
                << " " << pcorrection;
        }

        /// returns the (additive) correction to the absolute value
        /// of the momentum of the track in GeV
        return (0.001*pcorrection);
    }
};



} /* namespace clas::g12 */
} /* namespace clas */

#endif /* __CLAS_G12_PCOR_HPP__ */
