/**
 * bos_handle C++ wrapper for c_bos_io/fpack interface
 **/
#ifndef __BOS_HANDLE_H__
#define __BOS_HANDLE_H__

#include <cstdio>
#include <string>

#include "ntypes.h"
#include "bosddl.h"

extern "C" {
#include "bostypes.h"
}

namespace clas {
namespace bos {

using std::string;

static const string default_bos_string   = "E";
static const int    default_input_unit  = 1;
static const int    default_output_unit = 7;

/**
 * openBOSInputFile ( filename, unit )
 *    open BOS input file and attach it to unit number
 * closeBOSInputFile ( unit )
 *    close BOS input file associated with unit number
 **/
void openBOSInputFile(
    const string& infile,
    const int& unit = default_input_unit
);
void closeBOSInput(const int& unit = default_input_unit);

/**
 * openBOSOutputFile ( filename, unit )
 *    open BOS output file and attach it to unit number
 * closeBOSOutputFile ( unit )
 *    close BOS output file associated with unit number
 **/
void openBOSOutputFile(
    const string& outfile,
    const int& unit = default_output_unit
);
void closeBOSOutput(const int& unit = default_output_unit);

/**
 * getBOSEvent ( unit )
 *    get next event
 *    from input file associated with the unit number
 *    calls getBOS from /packages/c_bos_io/readbos.c
 *    throws exception (type: domain_error) if no events
 * cleanBOS ( bos_string )
 *    drop banks in memory and clean them prior to getting the next event
 * writeBOS ( bos_string, unit )
 *    writes event to output bosfile associated with unit number
 *    default is to write all bos banks (bos_string == "E")
 **/
bool getBOSEvent(const int& unit = default_input_unit);
void cleanBOS(const string& bos_string = default_bos_string);
void writeBOS(
    const string& bos_string = default_bos_string,
    const int&    unit       = default_output_unit
);

/**
 * getBank ( bankstring )
 * getBank ( bankstring, sector )
 *    returns void* pointer to BOS bank
 *    returns desired sector of bank if specified
 *    otherwise returns entire bank
 **/
const void* getBank(const string& bank);
const void* getBank(const string& bank, const int& sector);

} /** namespace bos **/
} /** namespace clas **/

#endif /** __BOS_HANDLE_H__ **/
