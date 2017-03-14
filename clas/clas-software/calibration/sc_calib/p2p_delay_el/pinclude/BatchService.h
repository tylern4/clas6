#ifndef BATCH_SERVICE_INCLUDED_
#define BATCH_SERVICE_INCLUDED_

#include <iostream>
#include "c_stds.h"
#include "RootF.h"

#define N_SOCK_MAX 10


class BatchService
{
 public:
  BatchService();
  ~BatchService();
  int CheckRequest();
  void SendHisto1F( const TH1F *Histo );
  void SendString(const char *String);
 private:
  int nSock;
  TServerSocket *ServerSock;
  TSocket *Sock[N_SOCK_MAX];
};

#endif

