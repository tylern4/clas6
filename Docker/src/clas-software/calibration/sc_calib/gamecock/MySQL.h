#ifndef __MYSQL_H
#define __MYSQL_H

#include "ROOT.h"


class MySQL {
  TSQLServer* serv;
  char hostname [80];
  char username [80];
  char password [80];
  void Connect();
protected:
  bool verbose;
public:
  MySQL(): serv(NULL) {}
  MySQL(const char* h, const char* u, const char* p, bool verbose_=false);
  ~MySQL();
  bool IsConnected();
  TSQLResult* Query       (const char* q);
  TSQLRow*    QuerySelect1(const char* q, int* fields=NULL);
  int         QueryInsert (const char* q);
};

#endif
