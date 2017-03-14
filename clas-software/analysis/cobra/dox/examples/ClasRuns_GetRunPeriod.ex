// grab a pointer to the ClasRuns instance
ClasRuns *ptypes = ClasRuns::Instance();

// now get the torus current for run 20926
double i_torus = runs->GetRunPeriod(20926).TorusCurrent();

// or, if you only want 1 piece of info, you can skip declaring the pointer:
double e_beam = ClasRuns::Instance()->GetRunPeriod(20926).BeamEnergy();