/* gtrip.h */
/* some global variables */

static int init_diff = 0;

static int scaler_last = 0;
static int scaler_current = 0;
static int first_in_scaler_interval = 0;
static int last_in_scaler_interval = 0;
static int scaler_event_counter = 0;
static int physics_event_counter = 0;
static int last_event_number = 0;
static int trip = -1;
static int last_type_read = 0; /* 0=phys event, 1=scaler event */
static int current_event_no;
static int scaler_interval = 0;
static int live_interval = 0;

static int ungated_triggers_current = 0;
static int ungated_triggers_previous = 0;
static int ungated_triggers = 0;
static int accepted_triggers = 0;
static int accepted_triggers_current = 0;
static int accepted_triggers_previous = 0;

static uint32 first_timer = 0;
static uint32 last_event_timer = 0;
static uint32 current_scaler_timer = 0;
static uint32 previous_scaler_timer = 0;
static uint32 current_timer=0;
static uint32 current_live_timer = 0;
static uint32 previous_live_timer = 0;

static float live_time = 1.;
static float average_live_time = 1.;
static float LT_diff = 0.;

static float diff_scaler_based = 0.;
static float diff_scaler_based_average =0;
static float diff_current = 0.;
static float diff_average = 0.;
static float diff_summ = 0.;

static int total_trips = 0;

