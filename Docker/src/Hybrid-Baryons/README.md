
# 2pi_event_generator

## Basic options

A basic configuration specifying a beam energy, W vs. Q2 region an output file name.

```sh
./2pi_event_generator --trig 10000 -E_beam 7.5 -W_min 1.0 -W_max 2.0 -Q2_min 0.5 -Q2_max 12.0 -output two_pion_7.5GeV.lund
```

## Options for online OSG submissions

The gemc OSG submission automatically adds the `--trig` and `--docker` options and becuase of this in the same configuration as abouve would be:

```sh
-E_beam 7.5 -W_min 1.0 -W_max 2.0 -Q2_min 0.5 -Q2_max 12.0
```


## All Generator Options

The generator has many options 

```sh
SYNOPSIS
        ./2pi_event_generator [-h] [--trig <Nevents>] [--docker] [-E_beam <E_beam>] [-W_min <W_min>]
                              [-W_max <W_max>] [-Q2_min <Q2_min>] [-Q2_max <Q2_max>] [-Theta_min
                              <Theta_min>] [-Theta_max <Theta_max>] [-E_eprime_min <E_eprime_min>]
                              [-Targ_rad <Targ_rad>] [-Targ_len <Targ_len>] [-Targ_off <Targ_off>]
                              [-Targ_dens <Targ_dens>] [-Targ_radlen <Targ_radlen>] [-Targ_Z
                              <Targ_Z>] [-Targ_A <Targ_A>] [-Twi_thick <Twi_thick>] [-Twf_thick
                              <Twf_thick>] [-Twi_dens <Twi_dens>] [-Twf_dens <Twf_dens>]
                              [-Twi_radlen <Twi_radlen>] [-Twf_radlen <Twf_radlen>] [-Twi_Z <Twi_Z>]
                              [-Twf_Z <Twf_Z>] [-Twi_A <Twi_A>] [-Twf_A <Twf_A>] [-flag_radmod
                              <flag_radmod>] [-flag_fermi <flag_fermi>] [-flag_flux <flag_flux>]
                              [-output <out.lund>]

OPTIONS
            -h, --help      print help
            <Nevents>       Number of events [10000]
            <E_beam>        Beam En. (GeV) [10.6]
            <W_min>         W min (GeV) [1.4]
            <W_max>         W max (GeV) [2.3]
            <Q2_min>        Q2 min (GeV^2) [0.05]
            <Q2_max>        Q2 max (GeV^2) [3.5]
            <Theta_min>     min theta scattered electron (deg) [1.0]
            <Theta_max>     max theta scattered electron (deg) [50.0]
            <E_eprime_min>  min E scattered electron (GeV) [0.01]
            <Targ_rad>      Target Radius [0.6]
            <Targ_len>      Target length [2.0]
            <Targ_off>      Target offset [-0.4]
            <Targ_dens>     Target Density [0.0708]
            <Targ_radlen>   Target Radiation Length [890.4]
            <Targ_Z>        Target Z [1]
            <Targ_A>        Target A [2]
            <Twi_thick>     Target Window Thickness [15.0]
            <Twf_thick>     Target Window Thickness [15.0]
            <Twi_dens>      Target window density [2.699]
            <Twf_dens>      Target window density [2.699]
            <Twi_radlen>    Target Radiation length [8.897]
            <Twf_radlen>    Target Radiation length [8.897]
            <Twi_Z>         Target Window Z [13]
            <Twf_Z>         Target Window Z [13]
            <Twi_A>         Target Window A [27]
            <Twf_A>         Target Window A [27]
            <flag_radmod>   Radiative effects? [2]
            <flag_fermi>    Turn on off fermi motion [0]
            <flag_flux>     Photon Flux [1]
            <output>        Output Filename [2pi_event_generator.dat] (if --docker options forced to be 2pi_event_generator.dat)
```

