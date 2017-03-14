
#include "BatchService.h"


BatchService::BatchService()
{
 printf("Opening Server Socket \n");
 nSock = 0 ;
 ServerSock = new TServerSocket(9090, kTRUE) ;
 ServerSock->SetOption(kNoBlock,1) ;
}


BatchService::~BatchService()
{
 SendString("Finished");
 for ( int iSock = 0; iSock < nSock ; iSock++)
  (Sock[iSock])->Close();
 ServerSock->Close(); 
 printf("Closed All Sockets\n"); 
}

int BatchService::CheckRequest()
{
 if ( nSock >= N_SOCK_MAX )
  return 0;
 Sock[nSock] = ServerSock->Accept()  ;
 if (Sock[nSock] > 0)
   {
     nSock++;
     printf( "Opened socket #%d\n", nSock );
     return 1;
   }
 else
   return 0;
}

void BatchService::SendHisto1F( const TH1F *Histo )
{
 TMessage mess( kMESS_OBJECT );

 for ( int iSock = 0; iSock < nSock; iSock++)
 {
    mess.Reset();   
    mess.WriteObject( Histo );     // write object in message buffer
    Sock[iSock]->Send( mess );     // send message     
 }
}


void BatchService::SendString( const char *String )
{
 for ( int iSock = 0; iSock < nSock; iSock++)
  Sock[iSock]->Send(String);
}
