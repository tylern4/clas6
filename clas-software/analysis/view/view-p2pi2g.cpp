/**
 * view-st.cpp
 *
 * for use with makent/makeht scripts.
 * first, create the ascii ntuple:
 * > view-st in.bos | cview 'ST ' > st.view
 *
 * then make the root macro with makent:
 * > view-st -l > st.lab
 * > makent st st.lab > st.C
 * > root st.C
 *
 * then you can make the C++ analysis program with makeht:
 * > view-st -l | makeht > st.cpp
 *
 * compile st.cpp as per instructions at botton of st.cpp file
 *
 * > cat st.view | st outfile.root
 * > root outfile.root
 * root[0] .ls // to get a listing of histograms created
 * 
 * /home/dschott/linux26_i686_gcc41/bin/view-ppippim-2g /work/clas/clasg12/clasg12/g6cTW/2009.06.12/bos/run56855_cooked.A00.00 | cview 'PPIPPIM2G' > ppippim2g.view
 * /home/dschott/linux26_i686_gcc41/bin/view-ppippim-2g -l > ppippim2g.lab 
 * makent ppippim2g ppippim2g.lab > ppippim2g.C 
 * root ppippim2g.C 
 **/
#include <cmath>
#include <map>
#include <vector>
#include <lorentz.h>

#include "view.hpp"
//#include "linelinecp.h"



bool processEvent(clasEvent &event, bool verbose, bool silent) {
	
  int InitEloss(event.run());
   	
   	if (event.N() == 5) {
   		
		if (event.N(Proton) == 1 && event.N(PiPlus) == 1 && event.N(PiMinus) == 1 && event.N(Gamma) == 2) {

			event.eLoss();

		   	fourVec beam = event.beam().get4P();
			fourVec target(PROTON_MASS,threeVec(0.0,0.0,0.0));
			clasParticle p = event.cp(Proton, 1);
			clasParticle pip = event.cp(PiPlus, 1);
			clasParticle pim = event.cp(PiMinus, 1);
			clasParticle g1 = event.cp(Gamma, 1);
			clasParticle g2 = event.cp(Gamma, 2);
			threeVec vert = event.V();
			float charged_pion_mass = .13457018;
			
			fourVec miss4p = beam + target - p.p() - pip.p() - pim.p();
			fourVec gammas = g1.p() + g2.p();
			fourVec totmiss4p = miss4p - gammas;
			
			lorentzTransform L;

			double ectime1 = g1.ecTime();
			double ectime2 = g2.ecTime();

			double g1theta = g1.p().theta();
			double g2theta = g2.p().theta();

			double mm2 = miss4p.lenSq();
			double mpz = miss4p.z();
			double mpt = sqrt(pow(miss4p.x(),2) + pow(miss4p.y(),2));
			double mtheta = miss4p.V().theta();
			double mphi = miss4p.V().phi();
			
			double gamm2 = gammas.lenSq();
			double gampz = gammas.z();
			double gampt = sqrt(pow(gammas.x(),2) + pow(gammas.y(),2));
			double gamtheta = gammas.V().theta();
			double gamphi = gammas.V().phi();       
			
			double tmm2 = totmiss4p.lenSq();
			double tmpz = totmiss4p.z();
			double tmpt = sqrt(pow(totmiss4p.x(),2) + pow(totmiss4p.y(),2));
			double tmtheta = totmiss4p.V().theta();
			double tmphi = totmiss4p.V().phi();        
			
			if ( fabs( event.vtime() - event.stVtime() ) < 1.002
				&& sqrt( pow(vert.x(),2) + pow(vert.y(),2) ) < 1.4
				&& vert.z() > -110 && vert.z() < -70
				&& p.beta() < 1 && p.Beta() < 1
				&& p.beta() > 0 && p.Beta() > 0
				&& pip.beta() < 1 && pip.Beta() < 1
				&& pip.beta() > 0 && pip.Beta() > 0
				&& pim.beta() < 1 && pim.Beta() < 1
				&& pim.beta() > 0 && pim.Beta() > 0
				&& mpt > 0.085 
				&& abs(p.beta() - p.Beta()) < 0.04
				&& abs(pip.beta() - pip.Beta()) < 0.04
				&& abs(pim.beta() - pim.Beta()) < 0.04
			     //&& abs(ectime1-ectime2) < 1
				) {

			  int data=0;
			//if (1){
			  ///kinfit??

			  //mwKfit(double beam, clasParticle * final, int numf, double missMass, int constrain[], double conMass);

			  ///fidcut?
			  ///FidCut();

				fourVec ppip = p.p() + pip.p();
				fourVec ppim = p.p() + pim.p();
				fourVec pippim = pip.p() + pim.p();
				fourVec mmpim = miss4p + pim.p();
				fourVec mmpip = miss4p +  pip.p();
				fourVec mmp = miss4p + p.p();
				fourVec ppippim = p.p() + pip.p() + pim.p();

				double miss4p_m = sqrt(miss4p*miss4p);
				double gammas_m = sqrt(gammas*gammas);
				double ppip_m = sqrt(ppip*ppip);
				double ppim_m = sqrt(ppim*ppim);
				double pippim_m = sqrt(pippim*pippim);
				double mmpim_m = sqrt(mmpim*mmpim);
				double mmpip_m = sqrt(mmpip*mmpip);
				double mmp_m = sqrt(mmp*mmp);
				double ppippim_m = sqrt(ppippim*ppippim);

				double t = (beam - (pim.p()+miss4p))*(beam - (pim.p()+miss4p));
				double tplus = (beam - (pip.p()+miss4p))*(beam - (pip.p()+miss4p));

				//clasECHB_t *ECHB = (clasECHB_t *)getBank(this->_bcs,"ECHB");

				//echb_t *echb;
				//echb = &ECHB->echb[g1.ec_id() - 1];

				//fourVec g1new, g2new;
				threeVec r1,r2;
				float energy1 = g1.p().t();
				float energy2 = g2.p().t();
				r1.set(g1.ecPosition().x() - vert.x(),g1.ecPosition().y() - vert.y(),g1.ecPosition().z() - vert.z());
				r2.set(g2.ecPosition().x() - vert.x(),g2.ecPosition().y() - vert.y(),g2.ecPosition().z() - vert.z());
				float mag1 = sqrt(r1*r1);
				float mag2 = sqrt(r2*r2);
				r1= r1*(energy1/mag1);
				r2= r2*(energy2/mag2);
				fourVec g1new(energy1,r1);
				fourVec g2new(energy2,r2);
				fourVec gammasnew = g1new + g2new ;

				double gammas_mN = sqrt(gammasnew*gammasnew);
				double gamm2N = gammasnew.lenSq();
				double gampzN = gammasnew.z();
				double gamptN = sqrt(pow(gammasnew.x(),2) + pow(gammasnew.y(),2));
				double gamthetaN = gammasnew.V().theta();
				double gamphiN = gammasnew.V().phi(); 

				if (ectime1>0&&ectime1<50&&ectime2>0&&ectime2<50
				    &&beam.t()>4.5&&beam.t()<5.7
				    &&abs(tmm2)<0.02
				    &&abs(gamphiN-gamphi)<0.0001&&abs(gamthetaN-gamtheta)<0.0001&&abs(gamptN-gampt)<0.0001
				    &&abs(gamtheta-mtheta-.004)<0.042&&abs(gamphi-mphi)<0.1&&abs(gampt-mpt-0.0033026)<0.2
				    &&pow(((gammas_m-0.5578)/.13),2)+pow(((miss4p_m*miss4p_m-0.296)/.08),2)<1
				    &&abs(ppip_m-1.21958)<0.08
				    &&t > -0.5 && t < 0
				    //&&t > -0.75 && t < -0.5
				    //&&t > -1 && t < -0.75
				  ){
				  /*
				TCut cectime("diffectime1>0&&diffectime1<50&&diffectime2>0&&diffectime2<50");
				TCut cmm("miss>0&&miss<3");
				TCut cbeam("beam>4.5");
				TCut ctmm("abs(tmm2)<0.02");
				TCut cgamN("abs(gamphiN-gamphi)<0.0001&&abs(gamthetaN-gamtheta)<0.0001&&abs(gamptN-gampt)<0.0001");
				TCut cut(cectime&&cmm&&cbeam&&ctmm&&cgamN);
				
				TCut cgammas("abs(gamtheta-mtheta-.004)<0.042&&abs(gamphi-mphi)<0.1&&abs(gampt-mpt-0.0033026)<0.2");
				
				//MM(eta)= 0.548206, 0.030584 GeV
				//InvM(eta)= 0.55274, 0.062552 GeV
				//InvM(Delta)= 1.21958, 0.046729 GeV
				//InvM(etapi-)= 1.29935, 0.0563242 GeV
				
				TCut ceta("((gam-0.55274)/.12)**2+((miss-0.548206)/.06)**2<1");
				TCut cdelta("abs(ppip-1.21958)<0.08");
				  */
				  // if (1){
				  
					fourVec beamp = beam;
					fourVec targetp = target;
					fourVec pp = p.p();
					fourVec pipp = pip.p();
					fourVec pimp = pim.p();
					fourVec miss4pp = miss4p;
					
					//transform to CM system
					
					L.set(beam + target);
					beamp *= L;
					targetp *= L;
					pp *= L;
					pipp *= L;
					pimp *= L;
					miss4pp *= L;
					
					//align beam along z-axis
					L.set(beamp.phi(),beamp.theta(),0.0);
					beamp *= L;
					targetp *= L;
					pp *= L;
					pipp *= L;
					pimp *= L;
					miss4pp *= L;				
					
					//define y-axis as the cross product of beam and pim eta system
					
					threeVec cross = beamp/(miss4pp + pimp);
					

					L.set(cross.phi(),cross.theta()-M_PI/2.0,-M_PI/2);
					beamp *= L;
					targetp *= L;
					pp *= L;
					pipp *= L;
					pimp *= L;
					miss4pp *= L;
	
					fourVec beam_h = beamp;
					fourVec target_h = targetp;
					fourVec p_h = pp;
					fourVec pip_h = pipp;
					fourVec pim_h = pimp;
					fourVec miss4p_h = miss4pp;


					//for helicity frame
					//align z axis to X
					L.set(0,(miss4p_h+pim_h).theta(),0.0);
					beam_h *= L;
					target_h *= L;
					p_h *= L;
					pip_h *= L;
					pim_h *= L;
					miss4p_h *= L;
					
					//boost to the X frame
					L.set(miss4p_h+pim_h);
					beam_h *= L;
					target_h *= L;
					p_h *= L;
					pip_h *= L;
					pim_h *= L;
					miss4p_h *= L;

					//J-G frame
					//boost to resonance frame
					L.set(miss4pp+pimp);
					beamp *= L;
					targetp *= L;
					pp *= L;
					pipp *= L;
					pimp *= L;
					miss4pp *= L;
					
					//align z to beam direction
					L.set(beamp.phi(),beamp.theta(),0.0);	
					beamp *= L;
					targetp *= L;
					pp *= L;
					pipp *= L;
					pimp *= L;
					miss4pp *= L;

				  
					
					
					cout << "PPIPPIM2G"
					  ///Dennis
					  /*
						<< ' ' << 3
						<< endl 
					     << "PPIPPIM2G"
						<< ' ' << 1
						<< ' ' << 0
						<< ' ' << beam.V().x()
						<< ' ' << beam.V().y()
						<< ' ' << beam.V().z()
						<< ' ' << beam.t()
						<< endl 
					     << "PPIPPIM2G"
						<< ' ' << 9
						<< ' ' << -1
					        << ' ' << pim.p().x()
						<< ' ' << pim.p().y()
						<< ' ' << pim.p().z()
						<< ' ' << pim.p().t()
						<< endl 
					     << "PPIPPIM2G"
						<< ' ' << 17
						<< ' ' << 0
						<< ' ' << miss4p.V().x()
						<< ' ' << miss4p.V().y()
						<< ' ' << miss4p.V().z()
						<< ' ' << miss4p.t()
*/
					  ///for CMU
					  /*
						<< ' ' << 4
						<< endl 
					  << "PPIPPIM2G"
						<< ' ' << 0
						<< endl 
					  << "PPIPPIM2G"
						<< "target  "
						<< ' ' << target.t()
						<< ' ' << target.V().x()
						<< ' ' << target.V().y()
						<< ' ' << target.V().z()
						<< endl
					  << "PPIPPIM2G"
						<< "incident_meson  " 
						<< ' ' << beam.t()
						<< ' ' << beam.V().x()
						<< ' ' << beam.V().y()
						<< ' ' << beam.V().z()
						<< endl 
					  << "PPIPPIM2G"
						<< "pi-  "
						<< ' ' << pim.p().t()
					        << ' ' << pim.p().x()
						<< ' ' << pim.p().y()
						<< ' ' << pim.p().z()
						<< endl 
					  << "PPIPPIM2G"
						<< "eta  "
						<< ' ' << miss4p.t()
						<< ' ' << miss4p.V().x()
						<< ' ' << miss4p.V().y()
						<< ' ' << miss4p.V().z()
						<< endl 
					  << "PPIPPIM2G"
						<< "Delta++  "
						<< ' ' << ppip.t()
						<< ' ' << ppip.V().x()
						<< ' ' << ppip.V().y()
						<< ' ' << ppip.V().z()
						/*
					  /*
						<< endl 
					     << "PPIPPIM2G"
						<< ' ' << 8
						<< ' ' << +1
						<< ' ' << pip.p().x()
						<< ' ' << pip.p().y()
						<< ' ' << pip.p().z()
						<< ' ' << pip.p().t()
						<< endl 
					     << "PPIPPIM2G"
						<< ' ' << 14
						<< ' ' << +1
						<< ' ' << p.p().x()
						<< ' ' << p.p().y()
						<< ' ' << p.p().z()
						<< ' ' << p.p().t()
					  */
					      << ' ' << event.run()
						<< ' ' << event.event()
						<< ' ' << t
						<< ' ' << tplus
						<< ' ' << beam.t()
						
						<< ' ' << miss4p_m
						<< ' ' << mpt
						<< ' ' << mtheta
						<< ' ' << mphi
						
						<< ' ' << gammas_m
						<< ' ' << gampt
						<< ' ' << gamtheta
						<< ' ' << gamphi
						
						<< ' ' << gammas_mN
						<< ' ' << gamptN
						<< ' ' << gamthetaN
						<< ' ' << gamphiN

						<< ' ' << ppip_m
						<< ' ' << ppim_m
						<< ' ' << pippim_m
						<< ' ' << mmpim_m
						<< ' ' << mmpip_m
						<< ' ' << mmp_m
						<< ' ' << ppippim_m
						
						<< ' ' << miss4pp.theta()
						<< ' ' << miss4pp.phi()

						<< ' ' << miss4p_h.theta()
						<< ' ' << miss4p_h.phi()

						<< ' ' << tmm2 
						<< ' ' << tmpz 
			 			<< ' ' << tmpt

 			 			<< ' ' << data 

// 						<< ' ' << pfid
// 						<< ' ' << pipfid 
// 			 			<< ' ' << pimfid
 						<< ' ' << ectime1-event.vtime()
 						<< ' ' << ectime2-event.vtime()
// 						<< ' ' << scpath1
// 						<< ' ' << scpath2
// 						<< ' ' << sctrack1
// 						<< ' ' << sctrack2
// 						<< ' ' << sclen1
// 						<< ' ' << sclen2
// 						<< ' ' << g1theta
// 						<< ' ' << g2theta


						<< endl; 
					return true;
					}
			}
		}
   	}
    return false;
}

#define COLUMN(vec, tag, desc) vec.push_back(make_pair(tag,desc));

void printLabels() {
    string name = "PPIPPIM2G";
    string desc = "proton pi+ pi- and 2 photons events";

    /// vector< ntuple_id, description > columns
    vector<pair<string, string> > cols;
    /*COLUMN(cols, "PID",      "PID")
    COLUMN(cols, "c",  "charge")
    COLUMN(cols, "px",  "x-momentum")
    COLUMN(cols, "py",  "y-momentum")
    COLUMN(cols, "pz",  "z-momentum")
    COLUMN(cols, "E",  "energy")
    */
    COLUMN(cols, "run",      "run number")
    COLUMN(cols, "event",  "event number")
    COLUMN(cols, "t",  "momentum transfer t")
    COLUMN(cols, "tplus",  "momentum transfer t")
    COLUMN(cols, "beam",  "beam energy")

	COLUMN(cols, "miss",  "missing-momentum inv mass")
	COLUMN(cols, "mpt",  "missing-momentum transverse")
    COLUMN(cols, "mtheta",  "missing-momentum theta")
    COLUMN(cols, "mphi",  "missing-momentum phi")
    
	COLUMN(cols, "gam",  "two photon inv mass")
	COLUMN(cols, "gampt",  "two photon momentum-transverse")
    COLUMN(cols, "gamtheta",  "two photon momentum theta")
    COLUMN(cols, "gamphi",  "two photon momentum phi")

	COLUMN(cols, "gamN",  "two photon inv mass")
	COLUMN(cols, "gamptN",  "two photon momentum-transverse")
    COLUMN(cols, "gamthetaN",  "two photon momentum theta")
    COLUMN(cols, "gamphiN",  "two photon momentum phi")
    
    COLUMN(cols, "ppip","inv mass of p pip")
    COLUMN(cols, "ppim","inv mass of p pim")   
    COLUMN(cols, "pippim","inv mass of pip pim")   
    COLUMN(cols, "mmpim","inv mass of p pim")
    COLUMN(cols, "mmpip","inv mass of p pip")
    COLUMN(cols, "mmp","inv mass of mm p")
    COLUMN(cols, "ppippim","inv mass of p pip pim")
    
    COLUMN(cols, "mtheta_jg",  "missing-momentum theta JG frame")
    COLUMN(cols, "mphip_jg",  "missing-momentum phi JG frame")    

    COLUMN(cols, "mtheta_h",  "missing-momentum theta helicity frame")
    COLUMN(cols, "mphip_h",  "missing-momentum phi helicity frame")   
    
  	COLUMN(cols, "tmm2",  "total inv mass")
	COLUMN(cols, "tmpz",  "total momentum-z") 
  	COLUMN(cols, "tmpt",  "total momentum-trans")

  	COLUMN(cols, "data",  "data type")
 
//   	COLUMN(cols, "pfid",  "total inv mass")
// 	COLUMN(cols, "pipfid",  "total momentum-z") 
//   	COLUMN(cols, "pimfid",  "total momentum-trans") 
     COLUMN(cols, "diffectime1","diff in ec and v time")
     COLUMN(cols, "diffectime2","diff in ec and v time")
      
//     COLUMN(cols, "scpath1","inv mass of pip pim")   
//     COLUMN(cols, "scpath2","inv mass of p pim")
//     COLUMN(cols, "sctrack1","inv mass of p pip")
//     COLUMN(cols, "sctrack2","inv mass of mm p")
//     COLUMN(cols, "sclen1","inv mass of p pip")
//     COLUMN(cols, "sclen2","inv mass of p pim")
//     COLUMN(cols, "g1theta","inv mass of p pip")
//     COLUMN(cols, "g2theta","inv mass of mm p")

   
    unsigned int nlab = 0;
    vector<pair<string, string> >::iterator itr = cols.begin();
    cout << "Labels for " << name << " (" << desc << "):" << endl;
    while(itr != cols.end()) {
        cout << ++nlab
            << "\t" << itr->second
            << " {" << itr->first << "}"
            << endl;
        ++itr;
    }
}
