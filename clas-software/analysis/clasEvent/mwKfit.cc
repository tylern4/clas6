#include <list>
#include <vector>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <map_manager.h>
#include <particleType.h>
#include <mwKfit.h>
#include <clasEvent.h>
#include <Vec.h>
#include <lorentz.h>
#include <matrix.h>

using namespace std;



#define ITMAX 100
#define EPS 3.0e-7
#define FPMIN 1.0e-30

double gammq(double a, double z);
void gser(double *gamser, double a, double x, double *gln);
void gcf(double *gammcf, double a, double x, double *gln);
double gammln(double xx);
void nrerror(char error_text[]);

clasKineFit
mwKfit(double beam, clasParticle * final, int numf)
{
  int x[10];
  return (mwKfit(beam, final, numf, 0.0, x, 0.0));
}

clasKineFit
mwKfit(double beam, clasParticle * final, int numf, double missMass, int constrain[], double conMass)
{

  clasEvent *ev = final->ev();
  int verbose = ev->verbose();
  clasKineFit Kret;
  int i, j, k;
  clasParticle *placeHolder = final;

  int isMissParticle;
  if(missMass == 0.0)
    isMissParticle = 0;
  else
    isMissParticle = 1;

  int isConstraint;
  if(conMass == 0.0)
    isConstraint = 0;
  else
    isConstraint = 1;

  int ndf = 1;
  if(!missMass && !conMass)
    ndf = 4;
  else if(missMass && !conMass)
    ndf = 1;
  else if(!missMass && conMass)
    ndf = 5;
  else if(missMass && conMass)
    ndf = 2;

  matrix < double >x(3, 1);
  matrix < double >newx(3, 1);
  matrix < double >xi(3, 1);

  matrix < double >y(1 + 3 * numf, 1);
  matrix < double >newy(1 + 3 * numf, 1);
  matrix < double >yinitial(1 + 3 * numf, 1);
  matrix < double >pulls(1 + 3 * numf, 1);

  matrix < double >epsilon(1 + 3 * numf, 1);
  matrix < double >chisq(1, 1);
  double prob;

  matrix < double >C(1 + 3 * numf, 1 + 3 * numf);
  C.setStatus(1);
  matrix < double >testC(1 + 3 * numf, 1 + 3 * numf);
  matrix < double >newC(1 + 3 * numf, 1 + 3 * numf);

  matrix < double >eta(1 + 3 * numf, 1);
  matrix < double >delta(1 + 3 * numf, 1);
  matrix < double >masses(numf, 1);
  matrix < double >alpha(numf, 1);

  matrix < double >CB(4 + isConstraint, 4 + isConstraint);
  matrix < double >CBi(4 + isConstraint, 4 + isConstraint);

  matrix < double >CBA(4 + isConstraint, 4 + isConstraint);
  matrix < double >CBAi(4 + isConstraint, 4 + isConstraint);

  matrix < double >c(4 + isConstraint, 1);
  matrix < double >B(4 + isConstraint, 1 + 3 * numf);
  matrix < double >Bt(1 + 3 * numf, 4 + isConstraint);

  matrix < double >A(4 + isConstraint, 3);
  matrix < double >At(3, 4 + isConstraint);

  matrix < double >dum(3, 3);
  double p, lambda, phi;
  fourVec P[numf];
  fourVec mm;
  double calcMass;
  int max = 10;
  double dumt, dumx, dumy, dumz;
  fourVec photon = fourVec(beam, threeVec(0.0, 0.0, beam));
  double targetMaterial;
  fourVec target;
  fourVec finalState;

  // define the final list of clasParticles
  clasParticle cpOut[numf];

  y.el(0, 0) = beam;

  double E0 = 2.445;
  if(final->ev()->runPeriod() == g1c)
  {
    if(final->ev()->run() >= 20926 && final->ev()->run() <= 21359)
      E0 = 3.115;
    else if(final->ev()->run() >= 21427 && final->ev()->run() <= 21615)
      E0 = 2.897;
    else if(final->ev()->run() >= 21763 && final->ev()->run() <= 21823)
      E0 = 2.445;
    else
      E0 = 3.115;
  }
  else if(final->ev()->runPeriod() == g11a)
  {
    E0 = 4.016;
  }
  else if(final->ev()->runPeriod() == g10)
  {
    E0 = 4.0;
  }
  else if(final->ev()->runPeriod() == g12)
  {
    E0 = 4.0;
  }
  //C.el(0, 0) = 0.000001 * ((1.0 / 6.230) * 2 * pow(E0, 3) * (1.0 / 3.0));
  C.el(0, 0) = pow(0.004016/sqrt(3.0),2);

  if(final->ev()->runPeriod() == g10)
    targetMaterial = 2 * 0.938272;
  else
    targetMaterial = 0.938272;

  target = fourVec(targetMaterial, threeVec(0.0, 0.0, 0.0));

  if(verbose)
  {
    cerr << "Run no: " << final->ev()->run() << endl;
    cerr << "Beam endpoint is set to " << E0 << endl;
    cerr << "Beam error is set to " << C.el(0, 0) << endl;
    cerr << "Target material has a mass of " << targetMaterial << endl;
  }


  if(verbose)
    cerr << "photon: " << beam << endl;
  // Initialize the matrices
  for(i = 0; i < numf; i++)
  {
    // add this particle to the input list...

    Kret.addParticleIn(*final);
    if(verbose)
      cerr << "i: " << final->p();
    alpha.el(i, 0) = (3.14159 / 3) * (final->scPaddleId_sector() - 1);
    p = final->p().r();
    lambda = final->TBERlambda();
    phi = final->TBERphi();
    y.el((3 * i) + 1, 0) = p;
    y.el((3 * i) + 2, 0) = lambda;
    y.el((3 * i) + 3, 0) = phi;
    masses.el(i, 0) = final->p().len();
    if(verbose)
      cerr << "masses: " << masses.el(i, 0) << endl;
    P[i] = fourVec(sqrt(pow(y.el((3 * i) + 1, 0), 2) + pow(masses.el(i, 0), 2)),
                   threeVec(y.el((3 * i) + 1, 0) * (cos(y.el((3 * i) + 2, 0)) * sin(y.el((3 * i) + 3, 0)) * cos(alpha.el(i, 0)) - sin(y.el((3 * i) + 2, 0)) * sin(alpha.el(i, 0))),
                            y.el((3 * i) + 1, 0) * (cos(y.el((3 * i) + 2, 0)) * sin(y.el((3 * i) + 3, 0)) * sin(alpha.el(i, 0)) + sin(y.el((3 * i) + 2, 0)) * cos(alpha.el(i, 0))),
                            y.el((3 * i) + 1, 0) * (cos(y.el((3 * i) + 2, 0)) * cos(y.el((3 * i) + 3, 0)))));
    dum = final->getCovMatrix();
    for(j = 0; j < 3; j++)
    {
      for(k = 0; k < 3; k++)
      {
        if(verbose) cerr << "dum: " << dum.el(j, k) << endl;
        if(dum.el(j, k) == dum.el(j, k))
        {
					C.el(1 + (3 * i) + j, 1 + (3 * i) + k) = dum.el(j, k);
        }
        else
				{
					cerr << "Covariance matrix is goofed....exiting fit." << endl;
					return Kret;
				}
      }
    }
    finalState += final->p();
    final++;
  }
	if(C.status()==0)
	{
		cerr << "Singular matrix encountered in kinematic fit...covariance matrix....exiting fit." << endl;
		cerr << C;
		return Kret;
	}
  yinitial = y;
  if(verbose) cerr << C;

  if(isMissParticle != 0)
    mm = photon + target - finalState;
  else
    mm = fourVec(0.0, threeVec(0.0, 0.0, 0.0));

  x.el(0, 0) = mm.x();
  x.el(1, 0) = mm.y();
  x.el(2, 0) = mm.z();
  if(verbose)
    cerr << "x: " << x << endl;

  //cerr << "Who's in the mass constraint?" << endl;
  for(i = 0; i < numf + 1; i++)
  {
    //cerr << "i: " << i << "\t" << constrain[i] << endl;
  }

  // Iterate over loop
  for(j = 0; j < max; j++)
  {
    if(verbose && j==0)
      cerr << "j ------------------ " << j << endl;
    dumt = 0.0;
    dumx = 0.0;
    dumy = 0.0;
    dumz = 0.0;
    B.el(0, 0) = 1.0;
    B.el(3, 0) = -1.0;
    for(i = 0; i < numf; i++)
    {
      final--;                  // Rewind the final particle pointer for later.
      P[i] = fourVec(sqrt(pow(y.el((3 * i) + 1, 0), 2) + pow(masses.el(i, 0), 2)),
                     threeVec(y.el((3 * i) + 1, 0) * (cos(y.el((3 * i) + 2, 0)) * sin(y.el((3 * i) + 3, 0)) * cos(alpha.el(i, 0)) - sin(y.el((3 * i) + 2, 0)) * sin(alpha.el(i, 0))),
                              y.el((3 * i) + 1, 0) * (cos(y.el((3 * i) + 2, 0)) * sin(y.el((3 * i) + 3, 0)) * sin(alpha.el(i, 0)) + sin(y.el((3 * i) + 2, 0)) * cos(alpha.el(i, 0))),
                              y.el((3 * i) + 1, 0) * (cos(y.el((3 * i) + 2, 0)) * cos(y.el((3 * i) + 3, 0)))));

      B.el(0, (3 * i + 1)) = -1.0 * y.el((3 * i) + 1, 0) / sqrt(pow(y.el((3 * i) + 1, 0), 2) + pow(masses.el(i, 0), 2));

      B.el(1, (3 * i + 1)) = cos(y.el((3 * i) + 2, 0)) * sin(y.el((3 * i) + 3, 0)) * cos(alpha.el(i, 0)) - sin(y.el((3 * i) + 2, 0)) * sin(alpha.el(i, 0));
      B.el(1, (3 * i + 1) + 1) = y.el((3 * i) + 1, 0) * (-sin(y.el((3 * i) + 2, 0)) * sin(y.el((3 * i) + 3, 0)) * cos(alpha.el(i, 0)) - cos(y.el((3 * i) + 2, 0)) * sin(alpha.el(i, 0)));
      B.el(1, (3 * i + 1) + 2) = y.el((3 * i) + 1, 0) * (cos(y.el((3 * i) + 2, 0)) * cos(y.el((3 * i) + 3, 0)) * cos(alpha.el(i, 0)));

      B.el(2, (3 * i + 1)) = cos(y.el((3 * i) + 2, 0)) * sin(y.el((3 * i) + 3, 0)) * sin(alpha.el(i, 0)) + sin(y.el((3 * i) + 2, 0)) * cos(alpha.el(i, 0));
      B.el(2, (3 * i + 1) + 1) = y.el((3 * i) + 1, 0) * (-sin(y.el((3 * i) + 2, 0)) * sin(y.el((3 * i) + 3, 0)) * sin(alpha.el(i, 0)) + cos(y.el((3 * i) + 2, 0)) * cos(alpha.el(i, 0)));
      B.el(2, (3 * i + 1) + 2) = y.el((3 * i) + 1, 0) * (cos(y.el((3 * i) + 2, 0)) * cos(y.el((3 * i) + 3, 0)) * sin(alpha.el(i, 0)));

      B.el(3, (3 * i + 1)) = cos(y.el((3 * i) + 2, 0)) * cos(y.el((3 * i) + 3, 0));
      B.el(3, (3 * i + 1) + 1) = y.el((3 * i) + 1, 0) * (-sin(y.el((3 * i) + 2, 0)) * cos(y.el((3 * i) + 3, 0)));
      B.el(3, (3 * i + 1) + 2) = y.el((3 * i) + 1, 0) * (cos(y.el((3 * i) + 2, 0)) * (-sin(y.el((3 * i) + 3, 0))));

      dumt += P[i].t();
      dumx += P[i].x();
      dumy += P[i].y();
      dumz += P[i].z();
    }
    if(verbose && j==0)
      cerr << "Zeroth break......" << endl;

    double missPsq;
    double missE;
    if(isMissParticle != 0)
    {
      missPsq = pow(x.el(0, 0), 2) + pow(x.el(1, 0), 2) + pow(x.el(2, 0), 2);
      missE = sqrt(missPsq + pow(missMass, 2));
    }
    else
    {
      missE = 0.0;
    }
    // Constraint equations 
    if(verbose && j==0)
      cerr << y.el(0, 0) << "\t" << dumt << "\t" << missE << endl;
    c.el(0, 0) = y.el(0, 0) + targetMaterial - dumt - missE;
    c.el(1, 0) = dumx + x.el(0, 0);
    c.el(2, 0) = dumy + x.el(1, 0);
    c.el(3, 0) = dumz + x.el(2, 0) - y.el(0, 0);

    if(verbose && j==0)
    {
      cerr << "missMass: " << missMass << endl;
      cerr << "missE: " << missE << endl;
    }

    if(isMissParticle != 0)
    {

      A.el(0, 0) = -x.el(0, 0) / missE;
      A.el(0, 1) = -x.el(1, 0) / missE;
      A.el(0, 2) = -x.el(2, 0) / missE;

      A.el(1, 0) = 1.0;
      A.el(2, 1) = 1.0;
      A.el(3, 2) = 1.0;

    }

    ///////////// Extra mass constraints //////////
    if(isConstraint != 0)
    {
      if(verbose && j==0)
        cerr << "We're in the constraint!!!!!!!!!!" << endl;
      dumt = 0.0;
      dumx = 0.0;
      dumy = 0.0;
      dumz = 0.0;
      if(constrain[i] != 0)
      {
        if(verbose && j==0)
          cerr << "Addding the missing particle to the constraint..." << endl;
        dumt += missE;
        dumx += x.el(0, 0);
        dumy += x.el(1, 0);
        dumz += x.el(2, 0);
      }
      for(i = 0; i < numf; i++)
      {
        if(constrain[i] != 0)
        {
          if(verbose && j==0)
            cerr << "Found a constraint!\t" << i << endl;
          dumt += P[i].t();
          dumx += P[i].x();
          dumy += P[i].y();
          dumz += P[i].z();
          if(verbose && j==0)
          {
            cerr << "Sum of dummies" << endl;
            cerr << dumt << endl;
            cerr << dumx << endl;
            cerr << dumy << endl;
            cerr << dumz << endl;
            cerr << P[i];
          }
        }
      }
      c.el(4, 0) = pow(dumt, 2) - pow(dumx, 2) - pow(dumy, 2) - pow(dumz, 2) - pow(conMass, 2);
      for(i = 0; i < numf; i++)
      {
        if(constrain[i] != 0)
        {
          B.el(4, 1 + 3 * i + 0) = 2 * dumt * y.el((3 * i) + 1, 0) / P[i].t()
            - 2 * dumx * (cos(y.el((3 * i) + 2, 0)) * sin(y.el((3 * i) + 3, 0)) * cos(alpha.el(i, 0)) - sin(y.el((3 * i) + 2, 0)) * sin(alpha.el(i, 0)))
            - 2 * dumy * (cos(y.el((3 * i) + 2, 0)) * sin(y.el((3 * i) + 3, 0)) * sin(alpha.el(i, 0)) + sin(y.el((3 * i) + 2, 0)) * cos(alpha.el(i, 0)))
            - 2 * dumz * (cos(y.el((3 * i) + 2, 0)) * cos(y.el((3 * i) + 3, 0)));

          B.el(4, 1 + 3 * i + 1) = 0.0
            - 2 * dumx * (y.el((3 * i) + 1, 0) * (-sin(y.el((3 * i) + 2, 0)) * sin(y.el((3 * i) + 3, 0)) * cos(alpha.el(i, 0)) -
                                                  cos(y.el((3 * i) + 2, 0)) * sin(alpha.el(i, 0))))
            - 2 * dumy * (y.el((3 * i) + 1, 0) * (-sin(y.el((3 * i) + 2, 0)) * sin(y.el((3 * i) + 3, 0)) * sin(alpha.el(i, 0)) +
                                                  cos(y.el((3 * i) + 2, 0)) * cos(alpha.el(i, 0)))) - 2 * dumz * (y.el((3 * i) + 1, 0) * (-sin(y.el((3 * i) + 2, 0)) * cos(y.el((3 * i) + 3, 0))));

          B.el(4, 1 + 3 * i + 2) = 0.0
            - 2 * dumx * (y.el((3 * i) + 1, 0) * (cos(y.el((3 * i) + 2, 0)) * cos(y.el((3 * i) + 3, 0)) * cos(alpha.el(i, 0))))
            - 2 * dumy * (y.el((3 * i) + 1, 0) * (cos(y.el((3 * i) + 2, 0)) * cos(y.el((3 * i) + 3, 0)) * sin(alpha.el(i, 0))))
            - 2 * dumz * (y.el((3 * i) + 1, 0) * (cos(y.el((3 * i) + 2, 0)) * (-sin(y.el((3 * i) + 3, 0)))));

        }
      }
      if(constrain[numf] != 0)
      {
        if(verbose && j==0)
          cerr << "Adding to A - - - - - - -" << endl;
        A.el(4, 0) = (2.0 * dumt * x.el(0, 0) / missE) - (2.0 * dumx);
        A.el(4, 1) = (2.0 * dumt * x.el(1, 0) / missE) - (2.0 * dumy);
        A.el(4, 2) = (2.0 * dumt * x.el(2, 0) / missE) - (2.0 * dumz);
      }
    }

    //if(A.status()==0 || B.status()==0 || C.status())
    //{
			//cerr << "Singular matrix encountered in kinematic fit....exiting fit." << endl;
      //return Kret;
    //}

    At = A.transpose();
    Bt = B.transpose();
    CB = B * C * Bt;
    if(verbose && j==0)
    {
      cerr << "c: " << c << endl;
      cerr << "x: " << x << endl;
    }

    if(CB.status() == 0)
    {
			cerr << "Singular matrix encountered in kinematic fit....exiting fit." << endl;
      return Kret;
    }

    CBi = CB.inv();
    if(isMissParticle == 0)
    {
      delta = C * Bt * CBi * c;
      if(verbose && j==0)
      {
        cerr << "A: " << A << endl;
        cerr << "At: " << At << endl;
        cerr << "CBAi: " << CBAi << endl;
        cerr << "CBi: " << CBi << endl;
        cerr << "Bt: " << Bt << endl;
        cerr << "delta: " << delta << endl;
        cerr << "xi: " << xi << endl;
      }
    }
    else
    {

      CBA = At * CBi * A;
      if(CBA.status() == 0)
      {
				cerr << "Singular matrix encountered in kinematic fit....exiting fit." << endl;
				return Kret;
      }
      CBAi = CBA.inv();

      delta = C * Bt * CBi * c - C * Bt * CBi * A * CBAi * At * CBi * c;

      xi = CBAi * At * CBi * c;
      if(verbose && j==0)
      {
        cerr << "A: " << A << endl;
        cerr << "At: " << At << endl;
        cerr << "CBAi: " << CBAi << endl;
        cerr << "CBi: " << CBi << endl;
        cerr << "Bt: " << Bt << endl;
        cerr << "delta: " << delta << endl;
        cerr << "xi: " << xi << endl;
      }

      newx = x - xi;
      x = newx;
    }

    newy = y - delta;
    y = newy;


  }
  if(isMissParticle)
    newC = C - (C * B.transpose() * CB.inv() * B * C - C * B.transpose() * CB.inv() * A * CBA.inv() * A.transpose() * CB.inv() * B * C);
  else
    newC = C - (C * B.transpose() * CB.inv() * B * C);



  epsilon = y - yinitial;

  final = placeHolder;
  for(i = 0; i < numf; i++)
  {
    P[i] = fourVec(sqrt(pow(y.el((3 * i) + 1, 0), 2) + pow(masses.el(i, 0), 2)),
                   threeVec(y.el((3 * i) + 1, 0) * (cos(y.el((3 * i) + 2, 0)) * sin(y.el((3 * i) + 3, 0)) * cos(alpha.el(i, 0)) - sin(y.el((3 * i) + 2, 0)) * sin(alpha.el(i, 0))),
                            y.el((3 * i) + 1, 0) * (cos(y.el((3 * i) + 2, 0)) * sin(y.el((3 * i) + 3, 0)) * sin(alpha.el(i, 0)) + sin(y.el((3 * i) + 2, 0)) * cos(alpha.el(i, 0))),
                            y.el((3 * i) + 1, 0) * (cos(y.el((3 * i) + 2, 0)) * cos(y.el((3 * i) + 3, 0)))));

    // fil the fit object
    cpOut[i] = *final;
    cpOut[i].p(P[i]);

    Kret.addParticleOut(cpOut[i]);

    if(verbose)
      cerr << "New momentum:\t" << P[i];
    if(verbose)
      cerr << "Old momentum:\t" << final->p();
    final++;
  }

  for(i = 0; i < 1 + 3 * numf; i++)
  {

    pulls.el(i, 0) = (y.el(i, 0) - yinitial.el(i, 0)) / (sqrt(C.el(i, i) - newC.el(i, i)));
    if(verbose)
      cerr << "pulls: " << i << "\t" << pulls.el(i, 0) << endl;
    Kret.addPull(pulls.el(i, 0));
  }

  fourVec omega;
  fourVec dummy = fourVec(y.el(0, 0), threeVec(0.0, 0.0, y.el(0, 0)));
  mm = dummy + target - P[0] - P[1] - P[2];
  omega = P[1] + P[2];
  if(verbose)
  {
    cerr << "omega: " << omega.len() << endl;
    cerr << "omega: " << omega << endl;
  }

  chisq = epsilon.transpose() * Bt * CBi * B * epsilon;
  if(verbose)
    cerr << "chi square 2 is " << chisq.el(0, 0) << endl;
  if(verbose)
    cerr << "ndf is " << ndf << endl;
  prob = gammq((double) ndf / 2.0, chisq.el(0, 0) / 2.0);
  Kret.status(1);
  Kret.prob(prob);
  Kret.ndf(ndf);
  if(verbose) cerr << "ndf is " << Kret.ndf() << endl;
  Kret.beamEnergy(y.el(0, 0));
  Kret.chisq(chisq.el(0, 0));
  if(verbose)
    cerr << "ndf: " << ndf << "\t\tchisq: " << chisq.el(0, 0) << "\t\tprob: " << prob << endl;
  return (Kret);

}

clasKineFit & clasKineFit::operator=(const clasKineFit & fit)
{
  this->_status = fit.status();
  this->_chisq = fit.chisq();
  this->_ndf = fit.ndf();
  this->_cpIn = fit.cpIn();
  this->_cpOut = fit.cpOut();
  this->_pulls = fit.pulls();
  this->_prob = fit.prob();
  this->_beamp = fit.beamEnergy();
  return (*this);

}

clasKineFit & clasKineFit::addParticleIn(const clasParticle & cp)
{
  this->_cpIn.push_back(cp);
  return *this;
}

clasKineFit & clasKineFit::addParticleOut(const clasParticle & cp)
{
  this->_cpOut.push_back(cp);
  return *this;
}

double clasKineFit::chisq2prob(const double chisq, const int ndf)
{
  double prob;
  prob = gammq((double) ndf / 2.0, chisq / 2.0);
  return prob;
}

clasParticle
clasKineFit::cpIn(int i) const
{
  clasParticle ret;
  i--;
  std::list < clasParticle >::const_iterator p = this->_cpIn.begin();
  for(int j = 0; j < i; j++, p++)
    ;
  ret = *p;


  return (ret);
}

clasParticle
clasKineFit::cpOut(int i) const
{
  clasParticle ret;
  i--;
  std::list < clasParticle >::const_iterator p = this->_cpOut.begin();
  for(int j = 0; j < i; j++, p++)
    ;
  ret = *p;


  return (ret);
}

// returns the incomplete gamma function P(a,x)
double
gammq(double a, double x)
{
  double gamser, gammcf, gln;

  if(x < 0.0 || a <= 0.0)
    nrerror("Invalid arguments in routine gammq");
  if(x < (a + 1.0))
  {
    gser(&gamser, a, x, &gln);
    return 1.0 - gamser;
  }
  else
  {
    gcf(&gammcf, a, x, &gln);
    return gammcf;
  }
}


void
gser(double *gamser, double a, double x, double *gln)
{
  int n;
  double sum, del, ap;

  *gln = gammln(a);
  if(x <= 0.0)
  {
    if(x < 0.0)
      nrerror("x less than 0 in routine gser");
    *gamser = 0.0;
    return;
  }
  else
  {
    ap = a;
    del = sum = 1.0 / a;
    for(n = 1; n <= ITMAX; n++)
    {
      ++ap;
      del *= x / ap;
      sum += del;
      if(fabs(del) < fabs(sum) * EPS)
      {
        *gamser = sum * exp(-x + a * log(x) - (*gln));
        return;
      }
    }
    nrerror("a too large, ITMAX too small in routine gser");
    return;
  }
}

void
gcf(double *gammcf, double a, double x, double *gln)
{
  int i;
  double an, b, c, d, del, h;

  *gln = gammln(a);
  b = x + 1.0 - a;
  c = 1.0 / FPMIN;
  d = 1.0 / b;
  h = d;
  for(i = 1; i <= ITMAX; i++)
  {
    an = -i * (i - a);
    b += 2.0;
    d = an * d + b;
    if(fabs(d) < FPMIN)
      d = FPMIN;
    c = b + an / c;
    if(fabs(c) < FPMIN)
      c = FPMIN;
    d = 1.0 / d;
    del = d * c;
    h *= del;
    if(fabs(del - 1.0) < EPS)
      break;
  }
  if(i > ITMAX)
    nrerror("a too large, ITMAX too small in gcf");
  *gammcf = exp(-x + a * log(x) - (*gln)) * h;
}

double
gammln(double xx)
{
  double x, y, tmp, ser;
  static double cof[6] = { 76.18009172947146, -86.50532032941677,
    24.01409824083091, -1.231739572450155,
    0.1208650973866179e-2, -0.5395239384953e-5
  };
  int j;

  y = x = xx;
  tmp = x + 5.5;
  tmp -= (x + 0.5) * log(tmp);
  ser = 1.000000000190015;
  for(j = 0; j <= 5; j++)
    ser += cof[j] / ++y;
  return -tmp + log(2.5066282746310005 * ser / x);
}

void
nrerror(char error_text[])
{

  fprintf(stderr, "numerical recipes run time error ... \n");
  fprintf(stderr, "%s\n", error_text);
  fprintf(stderr, "..now exiting to system ... \n");
  exit(1);
}
