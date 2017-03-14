#include "CaldbServer.h"

using namespace std;
extern int SC_Version_Flag;

CaldbServer::CaldbServer(const char* hostname, const char* user, 
			 const char* passwd, bool verbose) :
  MySQL(hostname, user, passwd, verbose), tablename("") {
}

CaldbServer::CaldbServer(string hostname, string user, 
			 string passwd, bool verbose) :
  MySQL(hostname.c_str(), user.c_str(), passwd.c_str(), verbose),
  tablename("") {
}


int CaldbServer::GetItemId(const char* system, 
			   const char* subsystem, const char* item) {

  string strsys(system);
  string strsub(subsystem);
  string stritm(item);
  return GetItemId(strsys, strsub, stritm);
}

int CaldbServer::GetItemId(string system, string subsystem, string item) {
  ostringstream query;

  enum retval_t {i_sysId, i_ssysId, i_itemId, i_length, i_type, i_descr };
  int retval[4];  /// systemId | subsystemId | itemId | length

  tablename = system + "_" + subsystem + "_" + item;

  query << "SELECT System.systemId,Subsystem.subsystemId,itemId,length,type,Item.description "
	<< "FROM Item, Subsystem, System  WHERE itemName = '" << item << "' AND subsystemName = '" 
	<< subsystem << "' AND Subsystem.subsystemId = Item.subsystemId AND systemName = '" 
	<< system << "' AND System.systemId = Subsystem.systemId";

  int fields;
  if (verbose) cout << "<" << query.str() << ">" << endl;
  TSQLRow* row = QuerySelect1(query.str().c_str(), &fields);

  if (fields != 6) 
    throw "CaldbServer::GetItemId: resulting number of fields don't match query";

  for (int i=i_sysId; i<i_type; i++)
    if ( (retval[i] = atoi (row->GetField(i))) <= 0 )
      throw "CaldbServer::GetItemId: positive integer number expected";

  if (strncmp(row->GetField(i_type),"float",5)) 
    throw "CaldbServer::GetItemId: only float type allowed";

  int locSize = 288;
  if(SC_Version_Flag == 2)
    locSize = 342;
//  cout << "system, versionflag, length = " << system << ", " << SC_Version_Flag << ", " << retval[i_length] << endl;
  if (retval[i_length]!=locSize)
    throw "CaldbServer::GetItemId: table should have N_CHANNEL values";

  return retval[i_itemId];
}

TSQLResult* CaldbServer::GetHistory(int itemId, const char* RunIndex, 
				    int run) {
  string strRunIndex(RunIndex);
  return GetHistory(itemId, strRunIndex, run);
}

TSQLResult* CaldbServer::GetHistory(int itemId, string RunIndex, int run) {
  ostringstream query;

  query << "SELECT " << RunIndex << ".time," << tablename << ".time,minRun,maxRun," 
	<< RunIndex << ".itemValueId,officer,author," << tablename << ".comment,"
	<< RunIndex << ".comment FROM " << RunIndex << " left join " << tablename 
	<< " using(itemValueId) WHERE itemId = " << itemId;

  if (run) query << " AND minRun <= " << run << " AND maxRun >= " << run;

  query<< " ORDER BY " << RunIndex << ".time DESC, minRun";
  
  return Query(query.str().c_str());
}

int CaldbServer::GetValues(int itemValueId, int len, double* values) {
  ostringstream query;
  
  query << "SELECT ";
  if((len == N_CHANNEL) && (SC_Version_Flag == 1))
    len = 288;

  for (int i=0; i<len; i++) {
    char cf; 
    query << (i ? ", v_" : " v_");
    query.width(4); 
    cf=query.fill('0'); 
    query << i+1;   /// good old fortran counting starts at 1
    query.fill(cf);
  }
  query << " FROM " << tablename << " WHERE itemValueId = " << itemValueId;

  int fields;
  if (verbose) cout << "<" << query.str() << ">" << endl;
  TSQLRow* row = QuerySelect1(query.str().c_str(), &fields);

  if (fields != len) 
    throw "CaldbServer::GetValues: don't get expected number of values";

  int locShift = 0;
  for (int i=0; i<len; i++){
    if((len == 288) && (i%48 == 0) && (i != 0)){
      for(int j=0; j<9; j++){ //have to insert blanks
        values[i + locShift] = 0.0;
        locShift++;
       }
    }
    values[i + locShift] = atof(row->GetField(i));
  }

  return len;
}

int CaldbServer::WriteValues(int itemId, const double* values, int srmin,
			     int srmax, const char* author, 
			     const char* comment, int len) {
  ostringstream query;

  if (!tablename.length()) throw "CaldbServer::WriteValues: call GetItemId() first";

  query << "INSERT " << tablename << " ( minRunSource, maxRunSource, author, comment";

  if((len == N_CHANNEL) && (SC_Version_Flag == 1))
    len = 288;

  /// create the field list v_0001 ....
  for (int i=0; i<len; i++) {
    char cf; 
    query << ", v_";
    query.width(4); 
    cf=query.fill('0'); 
    query << i+1;   /// good old fortran counting starts at 1
    query.fill(cf);
  }
  query << ") VALUES ( " << srmin << ", " << srmax << ", '" 
	<< author << "', '" << comment << "'";

  int locShift = 0;
  for (int i=0; i<len; i++){ //gaps in data are skipped and not included in query
    if((len == 288) && (i%48 == 0) && (i != 0))
      locShift += 9;
    query << ", " << values[i + locShift];
  }
  query << ")";

  QueryInsert(query.str().c_str());

  int fields;
  if (verbose) cout << "<" << query.str() << ">" << endl;
  TSQLRow* row = QuerySelect1("SELECT LAST_INSERT_ID()", &fields);
  if (fields != 1)
    throw "CaldbServer:WriteValues: can't get LAST_INSERT_ID()";
  
  return atoi(row->GetField(0));
}

int CaldbServer::WriteRunIndex(int itemId, int itemValueId, int minRun, int maxRun,
			       const char* RunIndex, const char* author, 
			       const char* comment) {
  ostringstream query;

  query << "INSERT " << RunIndex 
	<< " ( minRun, maxRun, itemId, itemValueId, officer, comment ) VALUES ( "
	<< minRun << ", " << maxRun << ", " << itemId << ", " 
	<< itemValueId << ", '" << author << "', '" << comment << "')";

  QueryInsert(query.str().c_str());

  int fields;
  if (verbose) cout << "<" << query.str() << ">" << endl;
  TSQLRow* row = QuerySelect1("SELECT LAST_INSERT_ID()", &fields);
  if (fields != 1)
    throw "CaldbServer:WriteValues: can't get LAST_INSERT_ID()";
  
  return atoi(row->GetField(0));
}

