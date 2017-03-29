onepigen is a single charged pion event generator which supports cross
sections generated via AO, daresbury, or MAID tables, in addition to
supporting Fermi motion in the target as well as generation of a
spectator particle.  In addition, it might be modified in the future
to support arbitrary cross sections via shaping an interpolated model
of a sampled cross section.

The model used for Fermi motion supports many settings.  First, the
energy component can be determined in a variety of ways, even though
the off-shellness in the energy component is not significant in that
many different reasonable estimates yield indistinguishable results,
at least to the level of precision currently available in cross
section measurements.  Second, the Hulthen, Paris, and CD-Bonn
momentum distributions can be used to model the Fermi momentum
distribution.  It is not clear to me whether or not the asymmetrical
angular states should be handled or if they have any meaningful
effects.

The program is controlled via the standard input, and accepts
Fortran's namelist I/O formatting.  This seems to be the most
efficient way to handle the large number of control parameters the
program accepts as opposed to flags supplied at the command line.
