#include "MySQL.h"

using namespace std;

MySQL::MySQL(const char* h, const char* u, const char* p, bool verbose_) :
  verbose(verbose_) {
  sprintf (hostname, "mysql://%s/calib", h);
  strcpy (username, u);
  strcpy (password, p);
  Connect();
  bool success = IsConnected();
  if (verbose)
    cout << (success? "Connected to " : "Failed to connect to ")
	 << hostname << " for user " << username << endl;
}

MySQL::~MySQL() {
  if (serv) serv->Close();
  serv = NULL;
}

bool MySQL::IsConnected() {
  if (!serv) return false;
  if (!serv->IsConnected()) { serv=NULL; return false; }
  return true;
}

void MySQL::Connect() {
  serv = TSQLServer::Connect(hostname,username,password);
}

TSQLResult* MySQL::Query(const char* q) {
  if (!serv) {
    throw "MySQL::Query: not connected to server";
    return NULL;
  }
  return (TSQLResult*) serv->Query(q);
}

TSQLRow* MySQL::QuerySelect1(const char* q, int* fields) {
  if (strncmp (q,"SELECT ", 7) )
    throw "MySQL::QuerySelect1: missing SELECT command in query string";
  TSQLResult* res = Query(q);
  if (res->GetRowCount() != 1) 
    throw "MySQL::QuerySelect1: number of lines returned is not one";
  TSQLRow* row = res->Next();
  
  if (verbose) {
    cout << "MySQL::QuerySelect1: ";
    for (int i = 0; i<res->GetFieldCount(); i++) {
      cout << " <" << row->GetField(i) << ">";
    }
    cout << endl;
  }

  if (fields)  *fields = res->GetFieldCount();
  return row;
}

int MySQL::QueryInsert(const char* q) {
  if (strncmp (q,"INSERT ", 7) )
    throw "MySQL::QueryInsert: missing INSERT command in query string";
  Query(q);
  return 0;
}
