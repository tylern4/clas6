#ifndef __BOS_HANDLE_CC__
#define __BOS_HANDLE_CC__

#include <stdexcept>
#include <sstream>

#include "bos_handle.hpp"

namespace clas {
namespace bos {

using std::domain_error;
using std::remove;
using std::stringstream;

void cleanBOS(const string& bos_string /* = default_bos_string */) {
    dropAllBanks(&bcs_,const_cast<char*>(bos_string.c_str()));
    cleanBanks(&bcs_);
}

void writeBOS(
        const string& bos_string /* = default_bos_string */,
        const int&    unit       /* = default_output_unit */
    ) {
    putBOS(&bcs_, unit, const_cast<char*>(bos_string.c_str()));
}

void openBOSInputFile(
        const string& infile,
        const int& unit /* = default_input_unit */
    ) {
    static bool bos_initialized = false;
    if(!bos_initialized) {
        initbos();
        bos_initialized = true;
    }
    stringstream ss;
    ss << "OPEN BOSINPUT UNIT=" << unit << " FILE=\"" << infile << "\" READ";
    if( !fparm_c(const_cast<char*>(ss.str().c_str())) )
        throw domain_error("Couldn't open BOS input file.");
}

void closeBOSInput(const int& unit /* = default_input_unit */) {
    stringstream ss;
    ss << "CLOSE BOSINPUT UNIT=" << unit;
    if( !fparm_c(const_cast<char*>(ss.str().c_str())) )
        throw domain_error("Couldn't close BOS file.");
}

void openBOSOutputFile(
        const string& outfile,
        const int& unit /* = default_output_unit */
    ){
    remove(outfile.c_str());
    stringstream ss;
    ss << "OPEN BOSOUTPUT UNIT=" << unit
     << " FILE=\"" << outfile
     << " WRITE STATUS=NEW RECL=3600";
    if( !fparm_c(const_cast<char*>(ss.str().c_str())) )
        throw domain_error("Couldn't open BOS output file.");
}

void closeBOSOutput(
        const int& unit /* = default_output_unit */
    ) {
    stringstream ss;
    ss << "CLOSE BOSOUTPUT UNIT=" << unit;
    if( !fparm_c(const_cast<char*>(ss.str().c_str())) )
        throw domain_error("Couldn't close BOS output file.");
}

bool getBOSEvent(const int& unit /* = default_input_unit */){
    if( !getBOS(&bcs_, unit, "E") )
        return false;
    return true;
}

const void* getBank(const string& bank) {
    string bankname = bank;
    bankname.append(4 - bank.size(), ' ');
    return getBank(&bcs_,const_cast<char*>(bankname.c_str()));
}
const void* getBank(const string& bank, const int& sector) {
    string bankname = bank;
    bankname.append(4 - bank.size(), ' ');
    return getGroup(&bcs_,const_cast<char*>(bankname.c_str()),sector);
}

} /** namespace bos **/
} /** namespace clas **/

#endif /** __BOS_HANDLE_CC__ **/
