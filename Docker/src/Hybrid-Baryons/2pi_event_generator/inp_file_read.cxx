#include "inp_file_read.h"
using namespace std;

int inp_file_read(Float_t &E_beam) {
  string qqq;

  getline(cin, qqq);
  qqq = qqq.substr(0, qqq.find(" ", 0));
  Nevents = atoi(qqq.c_str());
  cout << "Number of events to be generated = " << Nevents << "\n";

  getline(cin, qqq);
  qqq = qqq.substr(0, qqq.find(" ", 0));
  E_beam = atof(qqq.c_str());

  cout << "beam energy is " << E_beam << " GeV"
       << "\n";

  getline(cin, qqq);
  qqq = qqq.substr(0, qqq.find(" ", 0));
  W_min = atof(qqq.c_str());

  cout << "W_min is  " << W_min << " GeV"
       << "\n";

  getline(cin, qqq);
  qqq = qqq.substr(0, qqq.find(" ", 0));
  W_max = atof(qqq.c_str());

  cout << "W_max is  " << W_max << " GeV"
       << "\n";

  getline(cin, qqq);
  qqq = qqq.substr(0, qqq.find(" ", 0));
  Q2_min = atof(qqq.c_str());

  cout << "Q2_min is  " << Q2_min << " GeV^2"
       << "\n";

  getline(cin, qqq);
  qqq = qqq.substr(0, qqq.find(" ", 0));
  Q2_max = atof(qqq.c_str());

  cout << "Q2_max is  " << Q2_max << " GeV^2"
       << "\n";

  getline(cin, qqq);
  qqq = qqq.substr(0, qqq.find(" ", 0));
  Theta_min = atof(qqq.c_str());

  cout << "Theta_min is  " << Theta_min << " deg"
       << "\n";

  getline(cin, qqq);
  qqq = qqq.substr(0, qqq.find(" ", 0));
  Theta_max = atof(qqq.c_str());

  cout << "Theta_max is  " << Theta_max << " deg"
       << "\n";

  getline(cin, qqq);
  qqq = qqq.substr(0, qqq.find(" ", 0));
  E_eprime_min = atof(qqq.c_str());

  cout << "Minimal energy of scattered electron  " << E_eprime_min << " GeV"
       << "\n";

  getline(cin, qqq);
  qqq = qqq.substr(0, qqq.find(" ", 0));
  Targ_rad = atof(qqq.c_str());

  cout << "Target radius is  " << Targ_rad << " cm"
       << "\n";

  getline(cin, qqq);
  qqq = qqq.substr(0, qqq.find(" ", 0));
  Targ_len = atof(qqq.c_str());

  cout << "Target length is  " << Targ_len << " cm"
       << "\n";

  getline(cin, qqq);
  qqq = qqq.substr(0, qqq.find(" ", 0));
  Targ_off = atof(qqq.c_str());

  cout << "Target offset in z is  " << Targ_off << " cm"
       << "\n";

  getline(cin, qqq);
  qqq = qqq.substr(0, qqq.find(" ", 0));
  Targ_dens = atof(qqq.c_str());

  cout << "Target density is  " << Targ_dens << " g/cm^3"
       << "\n";

  getline(cin, qqq);
  qqq = qqq.substr(0, qqq.find(" ", 0));
  Targ_radlen = atof(qqq.c_str());

  cout << "Target radiation length is  " << Targ_radlen << " cm"
       << "\n";

  getline(cin, qqq);
  qqq = qqq.substr(0, qqq.find(" ", 0));
  Targ_Z = atof(qqq.c_str());

  cout << "Target Z is  " << Targ_Z << "\n";

  getline(cin, qqq);
  qqq = qqq.substr(0, qqq.find(" ", 0));
  Targ_A = atof(qqq.c_str());

  cout << "Target A is  " << Targ_A << "\n";

  getline(cin, qqq);
  Twi_thick = atof(qqq.substr(0, qqq.find(",", 0)).c_str());
  Twf_thick = atof(qqq.substr(qqq.substr(0, qqq.find(",", 0)).length() + 1,
                              qqq.rfind(",", 0))
                       .c_str());
  printf("Thickness of the target windows initial, final %f,%f um\n", Twi_thick,
         Twf_thick);

  getline(cin, qqq);
  Twi_dens = atof(qqq.substr(0, qqq.find(",", 0)).c_str());
  Twf_dens = atof(qqq.substr(qqq.substr(0, qqq.find(",", 0)).length() + 1,
                             qqq.rfind(",", 0))
                      .c_str());
  printf("Density of the target windows initial, final %f,%f g/cm^3\n",
         Twi_dens, Twf_dens);

  getline(cin, qqq);
  Twi_radlen = atof(qqq.substr(0, qqq.find(",", 0)).c_str());
  Twf_radlen = atof(qqq.substr(qqq.substr(0, qqq.find(",", 0)).length() + 1,
                               qqq.rfind(",", 0))
                        .c_str());
  printf("Radiation length of the target windows initial, final %f,%f cm\n",
         Twi_radlen, Twf_radlen);

  getline(cin, qqq);
  Twi_Z = atof(qqq.substr(0, qqq.find(",", 0)).c_str());
  Twf_Z = atof(qqq.substr(qqq.substr(0, qqq.find(",", 0)).length() + 1,
                          qqq.rfind(",", 0))
                   .c_str());
  cout << "Z of the target windows initial, final  " << Twi_Z << ", " << Twf_Z
       << "\n";
  //  printf ("Z of the target windows initial, final %f,%f \n", Twi_Z,Twf_Z);

  getline(cin, qqq);
  Twi_A = atof(qqq.substr(0, qqq.find(",", 0)).c_str());
  Twf_A = atof(qqq.substr(qqq.substr(0, qqq.find(",", 0)).length() + 1,
                          qqq.rfind(",", 0))
                   .c_str());
  //  printf ("A of the target windows initial, final %f,%f \n", Twi_A,Twf_A);
  cout << "A of the target windows initial, final  " << Twi_A << ", " << Twf_A
       << "\n";

  getline(cin, qqq);
  qqq = qqq.substr(0, qqq.find(" ", 0));
  flag_bos = atof(qqq.c_str());
#ifdef BOS
  switch (flag_bos) {
    case 0:
      cout << "Ouput BOS flag  " << flag_bos << "  - no BOS output"
           << "\n";
      break;
    case 1:
      cout << "Ouput BOS flag  " << flag_bos
           << "  -  output with MCTK, MCVX banks"
           << "\n";
      break;
    case 2:
      cout << "Ouput BOS flag  " << flag_bos << "  -  output with PART bank"
           << "\n";
      break;
  }
#endif

#ifndef BOS
  if (!(flag_bos == 0)) {
    flag_bos = 0;
    cout << "Ouput BOS flag  " << flag_bos
         << "  - no BOS output, since 'make nobos' used (for BOS output "
            "compile 'make bos') "
         << "\n";
  }
#endif

  getline(cin, qqq);
  out_bos_file = qqq.substr(0, qqq.find(" ", 0));

#ifdef BOS
  if (flag_bos == 1) cout << "BOS output file name " << out_bos_file << "\n";
#endif

  getline(cin, qqq);
  qqq = qqq.substr(0, qqq.find(" ", 0));
  flag_lund = atof(qqq.c_str());
  switch (flag_lund) {
    case 0:
      cout << "Ouput LUND flag  " << flag_lund << "  - no LUND output"
           << "\n";
      break;
    case 1:
      cout << "Ouput LUND flag  " << flag_lund << "  -  output LUND file"
           << "\n";
      break;
  }

  getline(cin, qqq);
  out_lund_file = qqq.substr(0, qqq.find(" ", 0));
  cout << "LUND output file name " << out_lund_file << "\n";

  getline(cin, qqq);
  qqq = qqq.substr(0, qqq.find(" ", 0));
  flag_radmod = atof(qqq.c_str());
  switch (flag_radmod) {
    case 0:
      cout << "Radiative mode flag  " << flag_radmod << "  - no rad effects"
           << "\n";
      break;
    case 1:
      cout << "Radiative mode flag  " << flag_radmod
           << "  -  rad eff with no straggling"
           << "\n";
      break;
    case 2:
      cout << "Radiative mode flag  " << flag_radmod
           << "  -  rad eff with straggling"
           << "\n";
      break;
  }

  getline(cin, qqq);
  qqq = qqq.substr(0, qqq.find(" ", 0));
  flag_fermi = atof(qqq.c_str());
  switch (flag_fermi) {
    case 0:
      cout << "Fermi flag " << flag_fermi << "  - no fermi smearing"
           << "\n";
      break;
    case 1:
      cout << "Fermi flag " << flag_fermi << "  -  with fermi smearing"
           << "\n";
      break;
  }

  getline(cin, qqq);
  qqq = qqq.substr(0, qqq.find(" ", 0));
  flag_flux = atof(qqq.c_str());
  switch (flag_flux) {
    case 0:
      cout << "Flux flag " << flag_flux
           << "  - under influence of virtual photons (model cross section)"
           << "\n";
      break;
    case 1:
      cout << "Flux flag " << flag_flux
           << "  -  under influence of electrons (like exp data)"
           << "\n";
      break;
  }

  if ((!(flag_radmod == 0)) && (flag_fermi == 1))
    cout << "\nCAUTION! Fermi mode flag = " << flag_fermi
         << " and Rad mode flag = " << flag_radmod
         << ".\nIn this EG these two modes are combined naively. It is "
            "recommended to use separate TWOPEG-D version, where Fermi mode "
            "and Rad mode are combined properly.\n";
  // It is not recommended to combine these two modes in this EG. If you want to
  // combine Fermi mode with Rad mode properly, please, use TWOPEG-D version.
  // See TWOPEG-CLAS-NOTE for detailes.
  cout << "\n";
  cout << "----------------------------------------\n";
  cout << "\n";
}
