

CREATE TABLE IF NOT EXISTS Residuals (
	cid				int 				NOT NULL,
	sector			int 				NOT NULL,
	superlayer		int				NOT NULL,
	sigma_narrow	float				NOT NULL,
	sigma_wide		float				NOT NULL,
	sigma_total		float				NOT NULL,
	mean				float				NOT NULL,
	nhits				int				NOT NULL,
	author			varchar(255)	default NULL,
	time				timestamp(14)	NOT NULL
) TYPE=MyISAM;


CREATE TABLE IF NOT EXISTS ConstantsID (
	cid						int 				PRIMARY KEY AUTO_INCREMENT,
	runnumber				int				NOT NULL,
	CLAS_CALDB_HOST		varchar(255)	default NULL,
	CLAS_CALDB_TIMESTAMP	varchar(255)	default NULL,
	CLAS_CALDB_RUNINDEX	varchar(255)	default NULL,
	author					varchar(255)	default NULL,
	time						timestamp(14)	NOT NULL
) TYPE=MyISAM;

CREATE TABLE IF NOT EXISTS TrackingStats (
	cid				int 				NOT NULL,
	sector			int 				NOT NULL,
	HitsPerTBT		float				NOT NULL,
	ChisqPerDOF		float				NOT NULL,
	ntracks			int				NOT NULL,
	author			varchar(255)	default NULL,
	time				timestamp(14)	NOT NULL
) TYPE=MyISAM;

CREATE TABLE IF NOT EXISTS LocalAngleCuts (
	cid				int 				NOT NULL,
	R1Upper			float				NOT NULL,
	R1Lower			float				NOT NULL,
	R2Upper			float				NOT NULL,
	R2Lower			float				NOT NULL,
	R3Upper			float				NOT NULL,
	R3Lower			float				NOT NULL,
	author			varchar(255)	default NULL,
	time				timestamp(14)	NOT NULL
) TYPE=MyISAM;
