using namespace std;

#include <iostream>
#include <cassert>
#include <string>
#include <vector>
#include <algorithm>
#include <streambuf>
#include <complex>

#include <lorentz.h>
#include <particle.h>
#include <event.h>
#include <pputil.h>
#include <pwaData.h>

pwaData::pwaData(){
  low_range = 0.0;
  high_range = 0.0;
  likelihood = 0.0;
  num_V = 0;

}

pwaData::~pwaData(){

}


// straight from Formulas for PWA v.2 - S.U. Chung pg 15
float pwaData::yield(vector<pwa_amp> waves){
  complex<double> N = (0.0, 0.0);
  vector<pwa_amp>::iterator alpha;
  for(alpha = waves.begin(); alpha != waves.end(); alpha++){
    vector<pwa_amp>::iterator alpha_prime;
    for(alpha_prime = waves.begin(); alpha_prime != waves.end(); alpha_prime++){
      N += alpha->amp * conj(alpha_prime->amp) * raw_ni.el(alpha->name, alpha_prime->name);
    }
  }
  return(N.real());
}

/*pwaData::pwaData( float likelihood, float low_mass, float high_mass, float low_t, float high_t, vector< pwa_amp> &waves, matrix<double> &error_matrix, integral &acc_ni, integral &raw_ni){
  this->likelihood = likelihood;
  low_range = low_mass;
  high_range = high_mass;
  this->low_t = low_t;
  this->high_t = high_t;
  this->waves = waves;
  this->error_matrix = error_matrix;
  this->acc_ni = acc_ni;
  this->raw_ni = raw_ni;
  }*/

void pwaData::print(ostream *os){
  int i;
  int nwaves;

  *os << "Fit Results: " << endl << endl;
  *os << "number of events: " << n_events << endl << endl;
  *os << "mass range: " << low_range << " to " << high_range << endl << endl;
  *os << "t range: " << low_t << " to " << high_t << endl << endl;
  *os << "likelihood: " << likelihood << endl << endl;
  *os << "number of ranks: " << n_rank << endl << endl;
  *os << "Amplitudes:" << endl;
  for (int rank = 1; rank <= n_rank ; rank++){
    nwaves = 0;
    *os << "rank: " << rank << endl;
    for (int wave=0; wave < waves.size(); wave++)
      if (waves[wave].rank == rank) nwaves++;
    *os << "number of waves in rank: " << nwaves << endl;
    *os <<  "Wavename : V" << endl;
    for (int wave=0; wave < waves.size(); wave++){
      if (waves[wave].rank == rank)
    *os << waves[wave].name << " : " << waves[wave].amp << endl;
    }
    *os << endl;
  }
  *os << "Error Matrix:" << endl;
  *os << error_matrix;
  //for(int i = 0; i < 2*num_V; i++){
  //for(int j=0; j < 2*num_V; j++) *os << error_matrix[i*2*num_V + j] << " ";
  //*os << endl;
  //}
  *os << endl;
  *os << "Acceptance Normalization Integrals:" << endl;
  acc_ni.print();
  *os << endl;
  *os << "Raw Normalization Integrals:" << endl;
  raw_ni.print();

  *os << "end" << endl;

}


int pwaData::read(istream *is){
  string end_string;
  int ok = 0;
  // keep getting lines until I find the beginning of the fit
  if (!findstring(is, "Fit Results:")) return 0;
  read_val(is, "number of events:", n_events);
  read_val(is, "mass range:", low_range, high_range);
  read_val(is, "t range:", low_t, high_t);
  read_val(is, "likelihood:", likelihood);
  read_val(is, "number of ranks:", n_rank);

  findstring(is, "Amplitudes:");

  for(int rank = 0; rank < n_rank; rank++){
    pwa_amp tmp_amp;
    int waveno = 0;;
    int rank_no;
    read_val(is, "rank:", rank_no);
    read_val(is, "number of waves in rank:", num_V);
    findstring(is,  "Wavename : V");
    for(int wave=0; wave < num_V; wave++){
      string sep;
      tmp_amp.rank = rank_no;
      tmp_amp.real_index = waveno;
      tmp_amp.img_index = ++waveno;
      *is >>  tmp_amp.name >> sep >>  tmp_amp.amp;
      waves.push_back(tmp_amp);
    }
  }
  findstring(is, "Error Matrix:");
  *is >> error_matrix;

  findstring(is, "Acceptance Normalization Integrals:");
  acc_ni.scan();

  findstring(is, "Raw Normalization Integrals:");
  raw_ni.scan();

  if (*is >> end_string) ok =1;

  return ok;

}


int pwaData::findstring(istream *is, string st){
  string input_line;
  int ok = 0;
  int ret = 0;

  // exit when you find a match or get to end of file, ok=1
  while(!ok){
    if (getline(*is,input_line,'\n')){
      if( input_line.find(st, 0) != string::npos){
    ret = 1;
    ok = 1;
      }
    } else {
      cerr << "findstring: string: " << st << "not found, end of file\n";
      ok = 1;
    }
  }
  return ret;
}

int pwaData::read_val(istream *is, string st, float &low, float &high){
  string input_line;
  int ok = 0;
  string::size_type size, start, end;

  while( !ok){
    if (getline(*is,input_line,'\n')){
      if( (start = input_line.find(st, 0)) != string::npos){
    start += st.size();
    if ( end = input_line.find("to", start)){
      string tmp = input_line.substr(start, end);
      low = atof(tmp.c_str());
      tmp = input_line.substr(end+2, input_line.size());
      high = atof(tmp.c_str());
    ok = 1;
    }
      }
    } else {
      cerr << "findpair: Reached end of File" << endl;
      ok = 1;
    }
  }
  return ok;
}

int pwaData::read_val(istream *is, string st, float &val){
  string input_line;
  int ok = 1;
  string::size_type size, start;

  while( ok ){
    if (getline(*is,input_line,'\n')){
      if( (start = input_line.find(st, 0)) != string::npos){
    string tmp;
    start += st.size();
    tmp = input_line.substr(start, input_line.size());
    val = atof(tmp.c_str());
    ok = 0;
      }
    } else{
      cerr << "read_val: Reached end of File, searching for: " << st  << endl;
      ok = 0;
    }
  }
  return ok;
}

int pwaData::read_val(istream *is, string st, int &val){
  string input_line;
  int ok = 1;
  string::size_type size, start;

  while( ok ){
    if (getline(*is,input_line,'\n')){
      if( (start = input_line.find(st, 0)) != string::npos){
    string tmp;
    start += st.size();
    tmp = input_line.substr(start, input_line.size());
    val = atoi(tmp.c_str());
    ok = 0;
      }
    } else{
      cerr << "read_val: Reached end of File, searching for: " << st  << endl;
      ok = 0;
    }
  }
  return ok;
}


ostream & operator<<(ostream &os, pwaData &data){
  data.print(&os);
  return os;
}
