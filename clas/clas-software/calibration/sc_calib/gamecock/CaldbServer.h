#ifndef __CALDBSERVER_H
#define __CALDBSERVER_H

#include "jdefine.h"
#include "MySQL.h"
#include <string>

using namespace std;

class CaldbServer : public MySQL {
  string tablename;
public:
  CaldbServer(const char* hostname, const char* user, 
	      const char* passwd, bool verbose_=false);

  CaldbServer(string hostname, string user, 
	      string passwd, bool verbose_=false);

  int GetItemId(const char* system, 
		const char* subsystem, const char*item);

  int GetItemId(string system, string subsystem, string item);

  TSQLResult* GetHistory(int itemId, const char* RunIndex, 
			 int run=0);

  TSQLResult* GetHistory(int itemId, string RunIndex, int run=0);

  int GetValues(int itemValueId, int len, double* values);

  int WriteValues(int itemId, const double* values, int srmin, int srmax, 
		  const char* author, const char* comment, int len=N_CHANNEL);

  int WriteRunIndex(int itemId, int itemValueId, int minRun, int maxRun,
       	    const char* RunIndex, const char* author, const char* comment);
};

#endif
