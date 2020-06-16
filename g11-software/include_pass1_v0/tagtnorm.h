typedef struct {
  int Nb_Tn;
  int Tn_ID[61];
  float Tn_L_val[2][61];
  float Tn_R_val[2][61];
  float Mean_Tn_val[2][61];
  float Diff_Tn_val[2][61];
  float Tn_time[2][61];
} tagtnorm_t;

extern tagtnorm_t tagtnorm_;

