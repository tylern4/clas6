#include "stpp_JCalibration.h"

#include <cstdio>
#include "map_manager.h"
#include <sstream>

extern "C" {
#include <string.h>
}

using namespace std;

JCalibration::JCalibration(int runno_) :
  runno(runno_) {
  memset (caldb, 0, sizeof(caldb));
  memset (type, 0, sizeof(type));
}

JCalibration::~JCalibration() {
  for (int i=0; i<MAX_CALDB_TABLE; i++) {
    if (caldb[i]) {
      switch (type[i]) {
      case ctype_float: delete [] ((float*) caldb[i]); break;
      case ctype_int:   delete [] ((int*) caldb[i]);   break;
      case ctype_char:  delete [] ((char*) caldb[i]);  break;
      }
    }
  }
}

double JCalibration::GetFloat(int caldbitem, int index) {
  float *table = (float*) caldb[caldbitem];

  if (!table) {
    if (caldb_table_descr[caldbitem].type != ctype_float) {
      ostringstream errmsg;
      errmsg << "item "
         << caldb_table_descr[caldbitem].system << "::"
         << caldb_table_descr[caldbitem].subsystem << "::"
         << caldb_table_descr[caldbitem].item <<  " is not a float";
      cerr << errmsg.str() << endl;
      throw errmsg.str().c_str();
    }

    type[caldbitem] = caldb_table_descr[caldbitem].type;
    char caldbmap[255];
    int max = caldb_table_descr[caldbitem].length;
    table = new float[max];
    int firsttime;
    sprintf(caldbmap,"%s/Maps/%s.map",getenv("CLAS_PARMS"),
        caldb_table_descr[caldbitem].system);
    int status = map_get_float(caldbmap,
                   caldb_table_descr[caldbitem].subsystem,
                   caldb_table_descr[caldbitem].item,
                   max, table, runno, &firsttime);
    if (status < 0) {
      ostringstream errmsg;
      errmsg << "get float "
         << caldb_table_descr[caldbitem].system << "::"
         << caldb_table_descr[caldbitem].subsystem << "::"
         << caldb_table_descr[caldbitem].item <<  " --error-> " << status;
      cerr << errmsg.str() << endl;
      throw errmsg.str().c_str();
    }
    caldb[caldbitem] = (void*) table;
  }

  return table[index];
}

int JCalibration::GetInt(int caldbitem, int index) {
  int *table = (int*) caldb[caldbitem];

  if (!table) {
    if (caldb_table_descr[caldbitem].type != ctype_int) {
      ostringstream errmsg;
      errmsg << "item "
         << caldb_table_descr[caldbitem].system << "::"
         << caldb_table_descr[caldbitem].subsystem << "::"
         << caldb_table_descr[caldbitem].item <<  " is not a int";
      cerr << errmsg.str() << endl;
      throw errmsg.str().c_str();
    }

    type[caldbitem] = caldb_table_descr[caldbitem].type;
    char caldbmap[255];
    int max = caldb_table_descr[caldbitem].length;
    table = new int[max];
    int firsttime;
    sprintf(caldbmap,"%s/Maps/%s.map",getenv("CLAS_PARMS"),
        caldb_table_descr[caldbitem].system);
    int status = map_get_int(caldbmap,
                 caldb_table_descr[caldbitem].subsystem,
                 caldb_table_descr[caldbitem].item,
                 max, table, runno, &firsttime);
    if (status < 0) {
      ostringstream errmsg;
      errmsg << "get int "
         << caldb_table_descr[caldbitem].system << "::"
         << caldb_table_descr[caldbitem].subsystem << "::"
         << caldb_table_descr[caldbitem].item <<  " --error-> " << status;
      cerr << errmsg.str() << endl;
      throw errmsg.str().c_str();
    }
    caldb[caldbitem] = (void*) table;
  }

  return table[index];
}

