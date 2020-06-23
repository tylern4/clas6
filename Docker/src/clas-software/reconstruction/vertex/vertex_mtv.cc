#include <iostream>
#include <cstdlib>
#include <cassert>
#include <cmath>
#include <cstring>
#include <clasbanks.h>
#include <kinematics.h>
#include "dmatrix.h"
#include "ssbdmat.h"
extern "C" {
#include <ntypes.h>
#include <bostypes.h>
}

using namespace std;

#define N_ERROR_WAIT 100000

/*_begin_doc
 *  RCS ID string
 *  $Id: vertex_mtv.cc,v 1.8 2008/09/05 18:36:59 goetz Exp $*/
#define USE(var) static void * use_##var = (void *) &var
static char crcsid[] =
    "$Id: vertex_mtv.cc,v 1.8 2008/09/05 18:36:59 goetz Exp $";
USE(crcsid);   /* make sure it is not optimized away */
/* END of RCS information*/

dMatrix inv3x3sym(dMatrix A);//inverts symmetric 3x3 dMatrix
ssbdmat invbd(ssbdmat A);
//enum coord {U,V,W};
//enum tparm {LAMBDA,PHI,D0,Z0};

//note: the coordinate system used is the lab coordinate system with U,V, &W
//   in what is normally called x,y,and z.  this is done to avoid confusion of
//   variables in the lab system with those in the tracking parameters (esp Z0)

extern "C" {
    int mtv(int *tracks, int ntrk,vector3_t *vert,float Cmat[9],float *chi2,int *iter);
    int mtv_(int *tracks, int *ntrk,vector3_t *vert,float Cmat[9],float *chi2,int *iter); /*fortran wrapper*/
}

int mtv_(int *tracks, int *ntrk,vector3_t *vert,float Cmat[9],float *chi2,int *iter)
{
    return( mtv(tracks,*ntrk,vert,Cmat,chi2,iter));
}


int mtv(int *tracks, int ntrk,vector3_t *vert,float Cmat[9],float *chi2,int *iter)
{
    // uses tber bank to create the best vertex possible fit to those tracks.
    //   tracks[]: array of track numbers in the TBER bank from 1, 0 = beam
    //   ntrk    :  number of tracks to combine (grab from tracks[])
    //   vert    : returned best fit vertex
    //   Cmat[]  : symmetric error matrix
    //   chi2    : chi2 of the fit (NOT per degree of freedom)
    //   iter    : number of iterations of the fit
    //
    //   returns:   0  -- everything okay
    //              1  --  ERROR: determinant = 0. in fit:
    //              2  --  ERROR: tried to use nonexistent track
    //             13  --  ERROR: tried to use unimplemented beamline track
    //

    try
    {
        int U=0,V=1,W=2;
        int LAMBDA=0,PHI=1,D0=2,Z0=3;
        clasTBER_t *my_tber=(clasTBER_t *)getBank(&bcs_,"TBER");        //tracking bank

        dMatrix U0(3*ntrk,1);    //starting point of each track, read in once
        dMatrix C(3*ntrk,1);     //unit vector in direction of tracks at U0
        ssbdmat bC(3,1,ntrk);   //copy of C in block diagonal form

        dMatrix vertex(3,1);     //self-explanatory
        dMatrix Uv(3*ntrk,1);    //n copies of vertex location
        dMatrix d(3*ntrk,1);     //Uv-U0
        ssbdmat bd(3,1,ntrk);   //copy of d in block diagonal form
        ssbdmat dp(3,4,ntrk);    //4 copies of d
        dMatrix t(ntrk,1);//Tr(d)*C for each track,dist along track from U0 to point closest to Uv

        dMatrix F(3*ntrk,1);   //d-C*t
        dMatrix A(3*ntrk,3);    //partial derivative of F w/respect to Uv (calc once)
        dMatrix Cv(3,3);        //covariance dMatrix of vertex position
        dMatrix delvert(3,1);   //change in vertex position between iterations
        dMatrix delparms(4*ntrk,1);
        //change in track parms that would move track to the vertex

        ssbdmat B(3,4,ntrk);    //partial of F w/ respect to track parms
        ssbdmat dd(3,4,ntrk);   //partial of d w/ respect to track parms
        ssbdmat dc(3,4,ntrk);   //partial of C w/respect to track parms
        ssbdmat tdc(3,4,ntrk);  //t*dc
        ssbdmat Ct(4,4,ntrk);   //covariance dMatrix of fitted track parameters
        ssbdmat Gb(3,3,ntrk);

        dMatrix newchi2(1,1);
        dMatrix oldchi2(1,1);
        float diffchi2;
        float Piby3=(3.141592654/3);
        int track;
        int maxtrack;

        //Initialize the Matrices
        newchi2(0)=oldchi2(0)=10000000;
        diffchi2=-1000;

        maxtrack = my_tber->bank.nrow;

        for(track=0; track<ntrk; track++)
        {
            float lambda,phi,d0,z0,alpha,cu,cv,cw;
            int layinfo2,sector;
            /* numbers in tracks are fortran counted. 0 is the beamline */
            int this_track = tracks[track]-1;

            /* Trying to get a non-existent track */
            if (this_track+1 > maxtrack)
            {
                return(2);
            }

            /* BEAMLINE NOT implemented yet!! */
            if (this_track < 0)
            {
                return(13);
            }

            //initialize covariance dMatrix of tracks
            Ct[track](LAMBDA,LAMBDA)=my_tber->tber[this_track].c22;
            Ct[track](PHI,PHI)      =my_tber->tber[this_track].c33;
            Ct[track](D0,D0)        =my_tber->tber[this_track].c44;
            Ct[track](Z0,Z0)        =my_tber->tber[this_track].c55;

            Ct[track](PHI,LAMBDA)=Ct[track](LAMBDA,PHI)=my_tber->tber[this_track].c23;
            Ct[track](D0,LAMBDA) =Ct[track](LAMBDA,D0) =my_tber->tber[this_track].c24;
            Ct[track](Z0,LAMBDA) =Ct[track](LAMBDA,Z0) =my_tber->tber[this_track].c25;
            Ct[track](D0,PHI)    =Ct[track](PHI,D0)    =my_tber->tber[this_track].c34;
            Ct[track](Z0,PHI)    =Ct[track](PHI,Z0)    =my_tber->tber[this_track].c35;
            Ct[track](Z0,D0)     =Ct[track](D0,Z0)     =my_tber->tber[this_track].c45;

            //get track info
            lambda=my_tber->tber[this_track].lambda;
            phi=my_tber->tber[this_track].phi;
            if(fabs(phi)<1.e-8) phi= phi<0.0 ? -1.e-8 : 1.e-8;
            d0 = my_tber->tber[this_track].d0;
            z0=my_tber->tber[this_track].z0;
            layinfo2=my_tber->tber[this_track].layinfo2;
            sector = layinfo2 / (256*256*256);
            alpha = Piby3*(sector -1);

            //initialize track starting point
            U0(U+3*track,0)=-z0*sin(alpha);
            U0(V+3*track,0)= z0*cos(alpha);
            U0(W+3*track,0)=-d0/sin(phi);

            //initialize track direction
            cu=cos(lambda)*sin(phi)*cos(alpha)-sin(lambda)*sin(alpha);
            cv=cos(lambda)*sin(phi)*sin(alpha)+sin(lambda)*cos(alpha);
            cw=cos(lambda)*cos(phi);
            C(U+3*track,0)=cu;
            C(V+3*track,0)=cv;
            C(W+3*track,0)=cw;
            bC(U,0,track)+=cu;
            bC(V,0,track)+=cv;
            bC(W,0,track)+=cw;

            //initialize A dMatrix (never changes, not a function of Uv)
            A(U+track*3,U)=1-cu*cu;
            A(V+track*3,V)=1-cv*cv;
            A(W+track*3,W)=1-cw*cw;
            A(U+track*3,V)=A(V+track*3,U)=-cu*cv;
            A(U+track*3,W)=A(W+track*3,U)=-cu*cw;
            A(V+track*3,W)=A(W+track*3,V)=-cv*cw;

            //initialize dc dMatrix (never changes, used to calculate B)
            dc(U,LAMBDA,track)=-sin(lambda)*sin(phi)*cos(alpha)-cos(lambda)*sin(alpha);
            dc(V,LAMBDA,track)=-sin(lambda)*sin(phi)*sin(alpha)+cos(lambda)*cos(alpha);
            dc(W,LAMBDA,track)=-sin(lambda)*cos(phi);
            dc(U,PHI,track)=cos(lambda)*cos(phi)*cos(alpha);
            dc(V,PHI,track)=cos(lambda)*cos(phi)*cos(alpha);
            dc(W,PHI,track)=-cos(lambda)*sin(phi);
            dc(U,D0,track)=0;
            dc(V,D0,track)=0;
            dc(W,D0,track)=0;
            dc(U,Z0,track)=0;
            dc(V,Z0,track)=0;
            dc(W,Z0,track)=0;

            //initialize dd dMatrix (never changes, used to calculate B)
            dd(U,LAMBDA,track)=0;
            dd(V,LAMBDA,track)=0;
            dd(W,LAMBDA,track)=0;
            dd(U,PHI,track)=0;
            dd(V,PHI,track)=0;
            dd(W,PHI,track)=-d0*cos(phi)/(sin(phi)*sin(phi));
            dd(U,D0,track)=0;
            dd(V,D0,track)=0;
            dd(W,D0,track)=1/sin(phi);
            dd(U,Z0,track)=sin(alpha);
            dd(V,Z0,track)=-cos(alpha);
            dd(W,Z0,track)=0;
        }
        //initial guess at vertex location;
        vertex(U)=0;
        vertex(V)=0;
        vertex(W)=0;

        //loop to find minimum
        for(*iter=0; (*iter)<100 && diffchi2<-0.01 ; (*iter)++)
        {
            for(track=0; track<ntrk; track++)
            {
                //copy vertex into Uv
                Uv(U+3*track,0)=vertex(U);
                Uv(V+3*track,0)=vertex(V);
                Uv(W+3*track,0)=vertex(W);
            }

            //calculate d,t,F and B
            d=Uv-U0;
            for(track=0; track<ntrk; track++)
            {
                t(track)=d(U+3*track,0)*C(U+3*track,0)+d(V+3*track,0)*C(V+3*track,0)+d(W+3*track,0)*C(W+3*track,0);
                bd(U,0,track)=d(U+3*track);
                bd(V,0,track)=d(V+3*track);
                bd(W,0,track)=d(W+3*track);
                F(U+3*track)=d(U+3*track)-C(U+3*track)*t(track);
                F(V+3*track)=d(V+3*track)-C(V+3*track)*t(track);
                F(W+3*track)=d(W+3*track)-C(W+3*track)*t(track);
            }

            for(track=0; track<ntrk; track++)
            {
                //hack to get around differing size of t and dc
                tdc[track]=t(track)*dc[track];
            }
            B=dd-tdc-bC*(Trn(bC)*dd+Trn(bd)*dc);

            //calculate other matrices
            Gb=invbd(B*Ct*Trn(B));
            Cv=inv3x3sym(Trn(A)*Gb*A);

            //calculate new vertex location as well as change in chi2
            delvert=-1*Cv*(Trn(A)*Gb*F);
            vertex=vertex+delvert;
            delparms=Ct*Trn(B)*Gb*(F-A*delvert);
            oldchi2=newchi2;
            newchi2=Trn(B*delparms)*Gb*(B*delparms);
            diffchi2=newchi2(0)-oldchi2(0);
        }

        //store results
        vert->x=vertex(U);
        vert->y=vertex(V);
        vert->z=vertex(W);
        for(int j=0; j<9; j++)
            Cmat[j]=(float) Cv(j);
        *chi2=(float)newchi2(0);
    } //end try

    catch(int err_code)
    {
        static char err_text[4][80]= {{"Unknown error in vertex_mtv: code="},
            {"Wrong dimension dMatrix in inv3x3sym"},
            {"Trying to invert non-symmetric dMatrix in inv3x3sym"},
            {"Attempted inversion of dMatrix with det=0 in inv3x3sym"}
        };

        static int n_err=0;
        static int prev_err=-1;

        if(err_code<1 || err_code>3)
            cerr << err_text[0] << err_code <<endl;
        else
        {
            if( N_ERROR_WAIT<=1 )
                cerr << err_text[err_code] <<endl;
            else
            {
                if( err_code == prev_err )
                {
                    n_err++;
                    if( n_err%N_ERROR_WAIT==0 )
                        cerr << err_text[err_code] << "  (" << n_err << " times)" <<endl;
                }
                else
                {
                    if(prev_err>=0)
                        cerr << err_text[prev_err] << "  (" << n_err << " times)" <<endl;
                    prev_err=err_code;
                    n_err=0;
                    cerr << err_text[err_code] << "  (new error)" <<endl;
                }
            }
        }
        return(1);
    }
    return(0);
}


ssbdmat invbd(ssbdmat A)
{
    ssbdmat Ainv(3,3,A.nb());
    if(A.nr()!=3||A.nc()!=3)
    {
        throw 1;
    }
    else
    {
        for(int i=0; i<A.nb(); i++)
        {
            Ainv[i]=inv3x3sym(A[i]);
        }
    }
    return(Ainv);
}

dMatrix inv3x3sym(dMatrix A)
{
    /* inverts a symmetric 3x3 dMatrix A and returns the result*/
    dMatrix Ainv(3,3);
    double denominator,temp;
    double a11,a12,a13,a22,a23,a33;
    int notsym=0;

    if(A.nr()!=3||A.nc()!=3)
    {
        throw 1;
    }
    else
    {
        for(int i=0; i<3; i++)
            for(int j=i; j<3; j++)
            {
                if((A(i,j)-A(j,i))*(A(i,j)-A(j,i))>A(i,j)*A(i,j)/(1000*1000))
                    notsym=1;
            }
    }
    if(notsym)
        throw 2;
    else
    {
        a11=A(0,0);
        a12=A(0,1);
        a13=A(0,2);
        a22=A(1,1);
        a23=A(1,2);
        a33=A(2,2);

        denominator = -1.0*(a11*a23*a23+a22*a13*a13 + a33*a12*a12) +2*a12*a13*a23 +a11*a22*a33;
        if(denominator>-1.e-15 && denominator<1.e-15)
        {
            //print(A);
            throw 3;
        }

        temp=1/denominator;
        /*diagonal elements*/
        Ainv(0,0)=temp*(-a23*a23+a22*a33);
        Ainv(1,1)=temp*(-a13*a13+a11*a33);
        Ainv(2,2)=temp*(-a12*a12+a11*a22);
        /*offdiagonal elements*/
        Ainv(0,1)=temp*(a13*a23-a12*a33);
        Ainv(0,2)=temp*(a12*a23-a13*a22);
        Ainv(1,2)=temp*(a12*a13-a23*a11);
        Ainv(1,0)=Ainv(0,1);
        Ainv(2,0)=Ainv(0,2);
        Ainv(2,1)=Ainv(1,2);
        return(Ainv);
    }
}






