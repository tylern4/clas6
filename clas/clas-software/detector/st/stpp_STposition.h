#ifndef __ST_POSITION_H__
#define __ST_POSITION_H__

#include <math.h>
extern "C" {
#include "ntypes.h"
}

const double DEG = 180. / M_PI;

/// should be replaced by TVector3 as soon ROOT becomes a part of the cooking
class JVector3 {
 protected:
  double x;
  double y;
  double z;
 public:
  JVector3 (): x(0), y(0), z(0) {};
  JVector3 (double x_, double y_, double z_);
  JVector3 (const vector3_t& v);
  double X() const { return x; }
  double Y() const { return y; }
  double Z() const { return z; }
  double ViehDeg() const;
  JVector3& Unit();
  double Mag() { return sqrt( (*this) * (*this) ); }
  void   RotateZ(double angle);
  void   SetMagThetaPhi(double mag, double theta, double phi);
  const JVector3& operator+=(const JVector3& v);
  friend double operator*(const JVector3& a, const JVector3& b);
  friend JVector3 operator*(const double s, const JVector3& v); 
  friend JVector3 operator-(const JVector3& a, const JVector3& b);
  friend JVector3 operator+(const JVector3& a, const JVector3& b);
};

/// ST detector plane constructed from GEOMETRY database entries
class STplane {
 protected:
  int id;          /// plane 1 or 2
  int sector;      /// sector 1..6
  JVector3 norm;   /// plane unit normal vector
  double d_origin; /// locataion is d_origin * norm
 public:
  STplane(int id_, int sector_);
  int Intersection(JVector3& pos, JVector3& dir, double& tlen);
};

/// Intersection between a track and the appropriate plane of the ST detector
/*!
  The STposition class represents the point of intersection between a track
  from either the HDPL or the TDPL bank (depending on the parameter use_tdpl_
  in the constructor) with either the leg or the nose plane of the STartcounter 
  (depending on the z-position of the point). The algorithm depends on the
  static variables x0, z0, z1, zbend, and rbend, which are initialized in the 
  routine stpp_init_geometry(). The meaning of these variables is illustrated
  in the plot below:
  \image html  stgeo.gif
  \image latex stgeo.eps "Static variables in STposition"
 */
class STposition {
  /// construct ST properties using pos, dir, and tlen
  void ConstructorGeo();
  /// get pos, dir, and tlen from [HT]DPL bank
  void ConstructorXDPL();
  int  sector;      /// sector 1..6
  int  ixdpl;       /// track no in [HT]DPL bank
  bool use_tdpl;    /// true = TDPL,  false = HDPL
  bool moretrks;    /// more than just one trk in [HT]DPL
  int  errcode;     /// problems  (missing bank, bad geom., etc)
  int  goodplane;   /// 0 = none, 3 = both planes,  1,2 = good plane   
  JVector3 pos;     /// x,y,z position at ST plane (sector coord.)
  JVector3 dir;     /// track direction at ST plane
  double tlen;      /// distance vertex - ST plane
  bool   isnose;    /// hit in nose
  double stpos;     /// distance ST hit point - ST PMT
  double geoscale;  /// correct ADC for track angle (thickness)
  double angSTplane;/// angle between ST plane and z-axis at hit point
 protected:

  /// x-pos for leg
  static double x0;

  /// min z-pos for leg
  static double z0; 

  /// max z-pos for leg
  static double z1; 

  /// bending radius
  static double rbend; 

  /// leg z-region affected by bending
  static double zbend; 
 public:
  /// empty constructor to make compiler happy
  STposition();

  /*!
    constructor for tracks given by their index in DCPB bank
    \param idcpb index to track in DCPB bank 
  */
  STposition(int idcpb);

  /*!
    constructor for tracks given by sector and index in [HT]DPL bank 
    \param sector_ sector, range [1..6]
    \param itdpl index to track in either HDPL or TDPL bank
    \param use_tdpl_  true = use TDPL bank, false = use HDPL bank
  */
  STposition(int sector_, int itdpl, bool use_tdpl_);


  /*!
    constructor for tracks given by sector, vertex; no [HT]DPL bank used
    \param sector_ sector, range [1..6]
    \param vert vertex as of EVNT bank
    \param p momentum vector as of EVNT bank
  */
  STposition(int sector_, JVector3 vert, JVector3 p);

  /// returns status for STR bank: 
  ///    1 = the only track in this sector, 
  ///    3 = several tracks in this sector
  int STRstatus() const               { return moretrks ? 3 : 1; } 

  /// returns error code  (missing bank, bad geom., etc)
  int GetError() const                { return errcode; }

  /// returns 0 = no good plane, 3 = both planes,  1,2 = good plane   
  int GoodPlane() const               { return goodplane; }

  /// returns sector (1..6)
  int GetSector() const               { return sector; }

  /// returns x-position of intersect
  double X() const                    { return pos.X(); }

  /// returns y-position of intersect
  double Y() const                    { return pos.Y(); }

  /// returns z-position of intersect
  double Z() const                    { return pos.Z(); }

  /// returns distance within ST between intersect and PMT
  double GetPosition() const          { return stpos; };

  /// returns distance within ST nose between intersect 
  /// and nose/leg conjunction
  double GetNosePosition() const      { return stpos - GetLegLength(); }

  /// returns track length from vertex to ST plane given by [TH]DPL
  double GetTrackLength() const       { return tlen; }

  /// returns index of track in [TH]DPL or RGLK  bank
  int GetTrackIndex() const           { return ixdpl; }

  /// set index of track in [TH]DPL or RGLK  bank
  void SetTrackIndex(int trkindex)    { ixdpl = trkindex; } 

  /// returns angle between ST plane and Z axis at intersection point
  double GetAngleSTplaneZaxis() const { return angSTplane; }

  /// returns true if valid intersection has been found
  bool Found() const                  { return goodplane > 0; }

  /// returns true if intersect is in the nose region of ST
  bool IsNose() const                 { return isnose;  }

  /// returns true if intersect is in the leg region of ST
  bool IsLeg() const                  { return !isnose; } 

  /// scale factor for track angle relative to ST plane
  /// to be applied on energy deposit
  double GetGeoScale() const          { return geoscale; }

  /// length of leg (extension in z-direction)
  double GetLegLength() const         { return z1 - z0; }

  /// phi in deg in sector coordinates
  double ViehDeg() const;                      

  /// ST segment ranging from 0..7. 
  /// To have 8 segments allows to find not only the best paddle but 
  /// also the next neighbour.
  /// 0 = first half of first paddle,
  /// 1 = second half of first paddle,
  /// 2 = first half of second paddle,
  /// 3 = second half of second paddle... and so on
  int GetSegment8() const;

  /// initialize ST geometry from CALDB database
  friend void stpp_init_geometry();
  friend class STplane;
};

/// initialize ST geometry from CALDB database
void stpp_init_geometry();

#endif
