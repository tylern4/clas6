It is not obvious to me how to modify aao_rad to produce the MAID
model cross section with radiative corrections in an
analytical/non-Monte Carlo way, so I'm just doing the non-radiative
cross section right now.  Kijun Park's thesis appears to do this as
well; I believe he applies bin-centering corrections after radiative
corrections.

The dsigma subroutine appears to be responsible in both aao_norad and
aao_rad for producing the non-radiative single-pion hadronic cross
section in the (W,Q^2,omega_pi) coordinate system.  It outputs the
hadronic cross section in the sigma0 argument as well as various
decomposition terms.
