{   Program PBPSIM
*
*   Test program and simulation of the pulse-by-pulse method of controlling a
*   switching power supply.
}
program pbpsim;
%include '(cog)/lib/base.ins.pas';
%include '(cog)/lib/stuff.ins.pas';

const
  n_cmdnames_k = 20;                   {number of command names in the list}
  cmdname_maxchars_k = 7;              {max chars in any command name}
{
*   Derived constants.
}
  cmdname_len_k = cmdname_maxchars_k + 1; {number of chars to reserve per cmd name}

type
  cmdname_t =                          {one command name in the list}
    array[1..cmdname_len_k] of char;
  cmdnames_t =                         {list of all the command names}
    array[1..n_cmdnames_k] of cmdname_t;

var
  cmdnames: cmdnames_t := [            {list of all the command names}
    'HELP   ',                         {1}
    '?      ',                         {2}
    'QUIT   ',                         {3}
    'Q      ',                         {4}
    'TP     ',                         {5}
    'QP     ',                         {6}
    'L      ',                         {7}
    'I1     ',                         {8}
    'A      ',                         {9}
    'VIN    ',                         {10}
    'VOUT   ',                         {11}
    'DD     ',                         {12}
    'S      ',                         {13}
    'SAT    ',                         {14}
    'MINON  ',                         {15}
    'CAP    ',                         {16}
    'VREG   ',                         {17}
    'LOAD   ',                         {18}
    'LOG    ',                         {19}
    'UNLOG  ',                         {20}
    ];

var
{
*   Input parameters for computing pulse.
}
  l: real;                             {inductance, Henries}
  sat: real;                           {inductor saturation current, A}
  tp: real;                            {PWM period, seconds}
  minon: real;                         {min allowed Ton, seconds}
  vin, vout: real;                     {input and output voltages}
  vd: real;                            {voltage accross diode during discharge}
  i1: real;                            {initial inductor current, Amps}
  qp: real;                            {charge to deliver, C}
{
*   Parameters computed each pulse.
}
  ton: real;                           {switch on time, seconds}
  td: real;                            {inductor discharge time, non-zero curr, A}
  ip: real;                            {peak inductor current during pulse, A}
  i2: real;                            {inductor current at end of pulse, A}
  qd: real;                            {charge actually delivered during pulse, C}
{
*   Parameters to determine the requested charge (Qp) from the next pulse.
}
  cap: real;                           {output capacitance value, F}
  vreg: real;                          {output regulation level}
  vlast: real;                         {Vout before the last pulse}
  vprev: real;                         {Vout one pulse before that}
  qlast: real;                         {charge delivered previous pulse}
  load: real;                          {simulated output load, Amps}
  compq: boolean;                      {compute desired output charge next pulse}

  log: boolean;                        {data is being logged to CSV file}
  logt: double;                        {log file data time}
  procid: sys_sys_proc_id_t;           {scratch process ID}
  iounit: sys_sys_iounit_t;            {scratch system I/O unit ID}

  prompt:                              {prompt string for entering command}
    %include '(cog)lib/string4.ins.pas';
  pst: string_index_t;                 {scratch command parse index}
  r1, r2: double;                      {floating point command parameters}

  opt:                                 {upcased command}
    %include '(cog)lib/string_treename.ins.pas';
  parm:                                {command parameter string}
    %include '(cog)lib/string_treename.ins.pas';
  pick: sys_int_machine_t;             {number of token picked from list}
  stat: sys_err_t;                     {completion status code}

label
  loop_cmd, done_cmd,
  err_extra, bad_cmd, err_cmparm, cmd_nsupp, leave;

%include '(cog)lib/wout_local.ins.pas'; {define standard output writing routines}
%include '(cog)lib/nextin_local.ins.pas'; {define command reading routines}
%include '(cog)lib/csv_local.ins.pas'; {define CSV file writing routines}
{
****************************************************************************
*
*   Subroutine SHOW_STATE
*
*   Show the current input state for the next pulse.
}
procedure show_state;
  val_param; internal;

begin
  lockout;

  write ('Inductance '); weng (l, 3, 'H'); writeln;
  write ('Saturation '); weng (sat, 3, 'A'); writeln;
  write ('PWM period '); weng (tp, 3, 's');
  write (' ('); weng(1.0/tp, 3, 'Hz'); writeln (' rate)');
  write ('Min allowed on time '); weng (minon, 3, 's'); writeln;
  write ('Diode drop '); weng(vd, 3, 'V'); writeln;
  write ('Initial inductor current '); weng (i1, 3, 'A'); writeln;
  write ('Output capacitance '); weng (cap, 3, 'F'); writeln;
  write ('Regulation voltage '); weng (vreg, 3, 'V'); writeln;
  write ('Load current '); weng (load, 3, 'A'); writeln;

  write ('Vin '); weng(vin, 3, 'V');
  write (', Vout '); weng (vout, 4, 'V'); writeln;
  write ('Charge to deliver '); weng(qp, 3, 'C');
  write (' ('); weng(qp/tp, 3, 'A'); writeln (' average rate)');

  unlockout;
  end;
{
********************************************************************************
*
*   Subroutine COMPUTE_PULSE
*
*   Compute the parameters of the next pulse.  The input parameters are shown
*   and the live values updated.
*
*   The input parameters are:
*
*     L  -  Inductor value, Henries
*
*     SAT  -  Inductor saturation current, Amps.  The pulse is adjusted as
*       necessary so that the peak current during the pulse does not exceed
*       this value.
*
*     TP  -  Full pulse period, seconds.  This is the reciprocal of the PWM
*       frequency.
*
*     MINON  -  Minimum allowed on time, seconds.  If the on time is less then
*       this value then the switch is not turned on this pulse.
*
*     VIN  -  Input voltage.
*
*     VOUT  -  Output voltage.
*
*     VD  -  Voltage accross the diode during the inductor discharge phase.
*
*     I1  -  Initial inductor current, Amps.
*
*     QP  -  Desired charge to deliver this pulse, Coulombs.
*
*   This routine will set the following:
*
*     TON  -  Time to turn on the switch for at the start of this pulse,
*       seconds.
*
*     TD  -  Inductor discharge time.  This only includes the time where the
*       inductor current is non-zero.
*
*     IP  -  Peak inductor current during the pulse, Amps.
*
*     I2  -  Current that will be in the inductor at the end of the pulse, Amps.
*
*     QD  -  Charge that will actually be delivered by this pulse, Coulombs.
}
procedure compute_pulse;
  internal; val_param;

var
  xa, xb, xc: real;                    {quadratic coeficients}
  x1: real;                            {scratch floating point}

label
  nopulse, mode_discont, mode_cont;

begin
{
*   Check for the minimum allowed Ton results in more delivered charge than
*   desired or that it would saturate the inductor.  In that case just do a 0
*   Ton pulse.  Checking for this here alleviates the need to check for these
*   special cases in several other places later.  This also guarantees that the
*   full solution to Ton later won't result in a negative Ton value.
}
  ton := minon;                        {set switch on time to minimum allowed}
  ip := i1 + (vin - vout) * ton / l;   {peak current}
  if ip > sat then goto nopulse;       {would exceed inductor saturation limit ?}

  td := tp - ton;                      {init to discharge whole rest of pulse}
  i2 := ip - (vout + vd) * td / l;     {curr at end of discharge}
  if i2 >= 0.0 then begin              {min pulse is continuous ?}
    qd :=                              {find charge delivered, continuous mode}
      ((i1 + ip) * ton + (ip + i2) * td) / 2.0;
    if qd > qp then goto nopulse;      {min pulse delivers too much ?}
    goto mode_cont;                    {no, do full continuous mode solution}
    end;
  {
  *   The minimum pulse is in discontinuous mode.
  }
  i2 := 0.0;                           {discontinuous, ending current will be 0}
  td := ip * l / (vout + vd);          {compute discharge time}
  qd :=                                {find charge delivered, discontinous mode}
    ((i1 + ip) * ton + ip * td) / 2.0;

  if qd > qp then begin                {min pulse results in too much output ?}
nopulse:                               {do pulse with Ton 0}
    ton := 0;                          {inductor on time}
    ip := i1;                          {peak current is initial current}
    i2 := i1 - (vout + vd) * tp / l;   {curr at end of pulse}
    if i2 < 0.0                        {inductor fully discharges before pulse end ?}
      then begin                       {discontinuous}
        i2 := 0.0;                     {ending current is zero}
        td := ip * l / (vout + vd);    {discharge time}
        end
      else begin                       {continuous}
        td := tp;                      {discharging during the whole pulse}
        end
      ;
    qd := (ip + i2) * td / 2.0;        {charge delivered by 0 pulse}
    return;
    end;
{
*   Decide whether this pulse will be in continuous or discontinuous mode.
*   Continuous mode means there will be some (non-zero) current in the inductor
*   at the end of the pulse.  It is useful to decide this up front since different
*   equations have to be solved to find Ton depending on which mode the pulse
*   will be in.  These envolve solving quadratics with the associated square root,
*   so it is better to do a few simpler calculations up front to determine which
*   quadartic to solve, than to solve one and find the other needs to be solved.
*
*   The pulse will be in discontinuous mode if the maximum charge that can be
*   delivered that way is sufficient, or the maximum discontinuous pulse would
*   result in exceeding the inductor saturation current anyway.
*
*   To determine which mode, we first solve for the Ton value that causes the
*   inductor current to get to exactly 0 at the end of the pulse.  From that we
*   can calculate the inductor peak current, check that against the saturation
*   current, and compute the delivered charge from this maximum discontinuous mode
*   pulse.
*
*   It has already been determined that the minimum allowed Ton (other than 0)
*   results in a discontinous pulse, that this pulse would not saturate the
*   inductor, and that the resulting delivered charge is less than desired.  In
*   other words, we already know Ton will be at least MINON.
}
  ton :=                               {find Ton at cont/discont boundary}
    ((vout + vd) * tp - i1 * l) / (vin + vd);
  td := tp - ton;                      {inductor discharge time}
  ip :=                                {peak inductor current for this Ton}
    i1 + (vin - vout) * ton / l;
  if ip > sat then goto mode_discont;  {discont already saturates inductor ?}

  qd :=                                {charge delivered by max discont pulse}
    ((i1 + ip) * ton + ip * td) / 2.0;
  if qd < qp then goto mode_cont;      {not enough output, use cont mode ?}
{
*   Discontinuous mode.  The maximum continuous pulse delivers more than the
*   desired charge or would exceed the inductor saturation current.  It has
*   already been determined that the minimum size pulse does not deliver enough
*   charge but does not saturate the inductor.
}
mode_discont:
  i2 := 0.0;                           {ending inductor current will be 0}

  xa :=                                {make the quadratic coef for finding Ton}
    (vin - vout) / (2.0 * l) + sqr(vin - vout) / (2.0 * l * (vout + vd));
  xb :=
    i1 + (vin - vout) * i1 / (vout + vd);
  xc :=
    sqr(i1) * l / (2.0 * (vout + vd)) - qp;
  x1 :=                                {B^2 - 4AC}
    sqr(xb) - (4.0 * xa * xc);
  if x1 < 0.0 then goto nopulse;       {no solution ? (shouldn't happen)}
  ton := (-xb + sqrt(x1)) / (2.0 * xa); {find Ton to deliver the desired charge}

  ip :=                                {peak inductor current for this Ton}
    i1 + (vin - vout) * ton / l;
  if ip > sat then begin               {desired Ton oversaturates the inductor ?}
    ton := (sat - i1) * l / (vin - vout); {Ton to just saturate the inductor}
    ip := sat;
    end;

  td := ip * l / (vout + vd);          {inductor discharge time}
  qd :=                                {delivered charge}
    ((i1 + ip) * ton  + ip * td) / 2.0;
  return;
{
*   Continuous mode.  It has already been determined that the minimum
*   continous mode pulse does not saturate the inductor and does not deliver
*   enough charge.
}
mode_cont:
  xa :=                                {make quadratic coef for finding Ton}
    -(vin + vd) / (2.0 * l);
  xb :=
    (vin + vd) * tp / l;
  xc :=
    (i1 * tp)
    - ((vout + vd) * sqr(tp) / (2.0 * l))
    - qp;
  x1 :=                                {B^2 - 4AC}
    sqr(xb) - (4.0 * xa * xc);
  if x1 < 0.0
    then begin                         {the quadratic has no solution}
      ton := tp;                       {do max size pulse}
      end
    else begin                         {a solution exists}
      ton := (-xb + sqrt(x1)) / (2.0 * xa); {find Ton to deliver the desired charge}
      ton := min(tp, ton);             {clip to max size pulse}
      end
    ;

  ip :=                                {peak inductor current}
    i1 + (vin - vout) * ton / l;
  if ip > sat then begin               {inductor would saturate ?}
    ton :=                             {find Ton to just saturate the inductor}
     (sat - i1) * l / (vin - vout);
    ip := sat;
    end;

  td := tp - ton;                      {inductor discharge time}
  i2 :=                                {ending inductor current}
    ip - (vout + vd) * td / l;
  qd :=                                {total charge delivered}
    ((i1 + ip) * ton + (ip + i2) * td) / 2.0;
  end;                                 {end of COMPUTE_PULSE}
{
****************************************************************************
*
*   Subroutine LOG_PULSE
*
*   Write the last pulse to the log output file.  VLAST is the output
*   voltage at the start of the pulse and VOUT at the end of the pulse.
}
procedure log_pulse;                   {write pulse to log file}
  val_param; internal;

var
  v: real;                             {voltage}
  q: real;                             {charge}
{
********************
*
*   Local subroutine LOGPOINT (T, I, LOAD, VOUT)
*
*   Write one data point to the output file.  T is the time of this data point,
*   I is the inductor current in Amps, LOAD is the load current in Amps, and
*   VOUT is the output voltage.
}
procedure logpoint (                   {write data point to log file}
  in      t: real;                     {time, seconds}
  in      i: real;                     {inductor current, Amps}
  in      load: real;                  {load current, Amps}
  in      vout: real);                 {output voltage}
  val_param; internal;

begin
  wcsv_fp_fixed (t * 1.0e6, 8);        {time}
  wcsv_fp_free (i * 10.0, 5);          {inductor current}
  wcsv_fp_free (load * 10.0, 5);       {load current}
  wcsv_fp_free (vout, 5);              {output voltage}
  wcsv_fp_free ((vout - vreg) / 0.010, 5); {output voltage error}
  wcsv_line;
  end;
{
*   Start of executable code of LOG_PULSE.
}
begin
  logpoint (logt, i1, load, vlast);    {write initial point}
{
*   Write point at 1/2 Ton, if appropriate.
}
  q := (i1 * 0.75 + ip * 0.25) * ton / 2.0; {charge dumped onto output}
  q := q - load * ton / 2.0;           {minus charge drained by the load}
  v := vlast + q / cap;                {Vout updated to charge}

  if ton > (tp / 1000.0) then begin    {Ton long enough to make separate data point ?}
    logpoint (logt + ton / 2.0, (i1 + ip) / 2.0, load, v);
    end;
{
*   Write point at end of Ton, if appropriate.
}
  q := (i1 + ip) * ton / 2.0;          {charge dumped onto output}
  q := q - load * ton;                 {minus charge drained by the load}
  v := vlast + q / cap;                {Vout updated to charge}

  if ton > (tp / 1000.0) then begin    {Ton long enough to make separate data point ?}
    logpoint (logt + ton, ip, load, v);
    end;
{
*   Write point half way thru the inductor discharge phase.
}
  q := (ip * 0.75 + i2 * 0.25) * td / 2.0; {charge onto cap during Td}
  q := q - load * td / 2.0;            {minus charge drained by the load}
  logpoint (logt + ton + td / 2.0, (ip + i2) / 2.0, load, v + q / cap);
{
*   Write points to end of inductor discharge phase if this is a discontinous
*   mode pulse.
}
  q := (ip + i2) * td / 2.0;           {charge onto cap during Td}
  q := q - load * td;                  {minus charge drained by the load}
  v := v + q / cap;                    {Vout updated to charge during Td}

  if (ton + td) < (tp * 0.999) then begin {discontinuous mode ?}
    logpoint (logt + ton + td, 0.0, load, v);
    end;
{
*   Write point at the end of the pulse.
}
  logpoint (logt + tp, i2, load, vout);

  logt := logt + tp;                   {update data time for start of next pulse}
  end;
{
****************************************************************************
*
*   Subroutine DO_PULSE
*
*   Compute the desired output from the next pulse, if appropriate, and then
*   compute the resulting pulse parameters.  The overall power supply state
*   is updated from the charge delivered by the pulse.
}
procedure do_pulse;
  val_param; internal;

var
  ld: real;                            {charge comsumed by the load}
  q: real;                             {scratch charge value}

begin
  if compq then begin                  {automatically compute desired charge ?}
    q := (vlast - vprev) * cap;        {total charge added to cap last pulse}
    ld := qlast - q;                   {charge comsumed by the load}
    q := (vreg - vlast) * cap;         {charge to get cap to desired level}
    qp := q + ld;                      {total charge to get to level and supply load}
    qp := qp + ld;                     {charge assumed to be used by load last pulse}
    qp := qp - qd;                     {minus charge delivered last pulse}
    qp := max(0.0, qp);                {clip negative values to 0}
    end;
  compq := true;                       {default to auto compute Qp next time}

  show_state;                          {show state before the pulse}
  lockout;
  writeln;
  unlockout;
{
*   Compute the parameters of the pulse to do.
}
  qlast := qd;                         {save previous charge delivered}
  compute_pulse;                       {compute pulse to deliver desired charge}
{
*   Apply the computed pulse to update the output voltage.
}
  vprev := vlast;                      {promote existing voltages one pulse back}
  vlast := vout;
  q := load * tp;                      {charge comsumed by the load}
  vout := vout + (qd - q) / cap;       {update output voltage to after this pulse}
{
*   Write info about this pulse.
}
  if log then begin                    {write this pulse to log file ?}
    log_pulse;
    end;

  lockout;
  write ('Assumed load '); weng(ld / tp, 3, 'A'); writeln;
  write ('Ton '); weng(ton, 3, 's'); writeln;
  write ('Ipeak '); weng(ip, 3, 'A'); writeln;
  write ('Ind end current '); weng(i2, 3, 'A'); writeln;
  write ('Charge actually delivered '); weng(qd, 3, 'C');
  write (' ('); weng(qd/tp, 3, 'A'); writeln (' average rate)');
  write ('Output '); weng (vout, 4, 'V');
  write (' (err '); weng (vout - vreg, 3, 'V'); writeln (')');
  unlockout;
{
*   Promote ending state of this pulse to the starting state for the next.
}
  i1 := i2;                            {set starting inductor current}
  end;
{
****************************************************************************
*
*   Start of main routine.
}
begin
  wout_init;                           {init output writing routines}
{
*   Set defaults.
}
  l := 100.0e-6;                       {inductance}
  sat := 2.0;                          {inductor saturation current}
  tp := 10.0e-6;                       {10 us period, 100 kHz}
  ton := tp / 4.0;
  i1 := 0;
  qp := 0.0;
  vin := 24.0;
  vout := 0.0;
  vd := 0.5;
  minon := 500.0e-9;                   {init min allowed pulse}
  cap := 470.0e-6;                     {output capacitance}
  vreg := 5.0;                         {output regulation value}
  load := 0.0;                         {init power supply output load}
  qd := 0.0;
  qlast := 0.0;
  vlast := vout;
  vprev := vout;
  compq := true;                       {init to compute desired charge automaticaly}
  log := false;                        {init to not writing log file}
{
***************************************
*
*   Process user commands.
*
*   Initialize before command processing.
}
  string_vstring (prompt, ': '(0), -1); {set command prompt string}
  show_state;

loop_cmd:
  lockout;
  string_prompt (prompt);              {prompt the user for a command}
  newline := false;                    {indicate STDOUT not at start of new line}
  unlockout;

  string_readin (inbuf);               {get command from the user}
  string_unpad (inbuf);                {strip all trailling blanks}
  if inbuf.len <= 0 then begin         {user entered a blank line ?}
    do_pulse;
    goto loop_cmd;
    end;
  newline := true;                     {STDOUT now at start of line}
  p := 1;                              {init BUF parse index}
  while inbuf.str[p] = ' ' do begin    {scan forwards to the first non-blank}
    p := p + 1;
    end;
  pst := p;                            {save parse index at start of command}
  next_keyw (opt, stat);               {extract command name into OPT}
  if string_eos(stat) then goto loop_cmd;
  if sys_error_check (stat, '', '', nil, 0) then begin
    goto loop_cmd;
    end;
  string_tkpick_s (                    {pick command name from list}
    opt, cmdnames, sizeof(cmdnames), pick);
  case pick of                         {which command is it}
{
**********
*
*   HELP
}
1, 2: begin
  if not_eos then goto err_extra;

  lockout;                             {acquire lock for writing to output}
  writeln;
  writeln ('HELP or ?      - Show this list of commands');

  writeln ('VREG volts     - Set output regulation level (desired output)');
  writeln ('CAP uF         - Set output capacitor size');
  writeln ('L uHenry       - Set inductor value');
  writeln ('SAT amps       - Set inductor saturation current');
  writeln ('DD volts       - Set diode drop for inductor discharge');
  writeln ('VIN volts      - Set input voltage');

  writeln ('TP usec        - Set total pulse repetition period');
  writeln ('MINON usec     - Set min allowed switch on time per pulse');
  writeln ('QP charge      - Desired inductor output next pulse, Coulombs');
  writeln ('A amps         - Average inductor output next pulse, Amps');
  writeln ('I1 amps        - Inductor starting current next pulse, Amps');
  writeln ('VOUT volts [vlast] - Set current and previous output voltage');

  writeln ('LOAD amps      - Set load on power supply output');
  writeln ('S              - Show current input parameters for pulse');
  writeln ('<blank line>   - Do pulse with current parameters');
  writeln ('LOG [fnam]     - Start logging data to CSV file, "pbp.csv" default');
  writeln ('UNLOG          - Stop logging, display log data if was logging');

  writeln ('Q or QUIT      - Exit the program');
  unlockout;                           {release lock for writing to output}
  end;
{
**********
*
*   QUIT
}
3, 4: begin
  if not_eos then goto err_extra;

  goto leave;
  end;
{
**********
*
*   TP usec
}
5: begin
  r1 := next_fp (stat);
  if sys_error(stat) then goto err_cmparm;
  if not_eos then goto err_extra;

  tp := r1 * 1.0e-6;                   {save PWM period in seconds}
  show_state;
  end;
{
**********
*
*   QP charge
}
6: begin
  r1 := next_fp (stat);
  if sys_error(stat) then goto err_cmparm;
  if not_eos then goto err_extra;

  qp := r1;                            {set charge output next pulse}
  compq := false;                      {don't automatically recompute Qp}
  show_state;
  end;
{
**********
*
*   L uHenry
}
7: begin
  r1 := next_fp (stat);
  if sys_error(stat) then goto err_cmparm;
  if not_eos then goto err_extra;

  l := r1 * 1.0e-6;                    {update inductance in Henries}
  show_state;
  end;
{
**********
*
*   I1 current
}
8: begin
  r1 := next_fp (stat);
  if sys_error(stat) then goto err_cmparm;
  if not_eos then goto err_extra;

  i1 := r1;                            {set new inductor initial current}
  show_state;
  end;
{
**********
*
*   A current
}
9: begin
  r1 := next_fp (stat);
  if sys_error(stat) then goto err_cmparm;
  if not_eos then goto err_extra;

  qp := r1 * tp;                       {set charge output next pulse}
  compq := false;                      {don't automatically recompute Qp}
  show_state;
  end;
{
**********
*
*   Vin volts
}
10: begin
  r1 := next_fp (stat);
  if sys_error(stat) then goto err_cmparm;
  if not_eos then goto err_extra;

  vin := r1;
  show_state;
  end;
{
**********
*
*   Vout volts [vlast]
}
11: begin
  r1 := next_fp (stat);
  if sys_error(stat) then goto err_cmparm;
  r2 := next_fp (stat);
  if string_eos(stat)
    then begin
      r2 := r1
      end
    else begin
      if sys_error(stat) then goto err_cmparm;
      if not_eos then goto err_extra;
      end
    ;

  vout := r1;
  vlast := r1;
  vprev := r2;
  show_state;
  end;
{
**********
*
*   DD volts
}
12: begin
  r1 := next_fp (stat);
  if sys_error(stat) then goto err_cmparm;
  if not_eos then goto err_extra;

  vd := r1;
  show_state;
  end;
{
**********
*
*   S
}
13: begin
  if not_eos then goto err_extra;

  show_state;
  end;
{
**********
*
*   SAT amps
}
14: begin
  r1 := next_fp (stat);
  if sys_error(stat) then goto err_cmparm;
  if not_eos then goto err_extra;

  sat := r1;
  show_state;
  end;
{
**********
*
*   MINON usec
}
15: begin
  r1 := next_fp (stat);
  if sys_error(stat) then goto err_cmparm;
  if not_eos then goto err_extra;

  minon := r1 * 1.0e-6;
  show_state;
  end;
{
**********
*
*   CAP uF
}
16: begin
  r1 := next_fp (stat);
  if sys_error(stat) then goto err_cmparm;
  if not_eos then goto err_extra;

  cap := r1 * 1.0e-6;
  show_state;
  end;
{
**********
*
*   VREG volts
}
17: begin
  r1 := next_fp (stat);
  if sys_error(stat) then goto err_cmparm;
  if not_eos then goto err_extra;

  vreg := r1;
  show_state;
  end;
{
**********
*
*   LOAD amps
}
18: begin
  r1 := next_fp (stat);
  if sys_error(stat) then goto err_cmparm;
  if not_eos then goto err_extra;

  load := r1;
  show_state;
  end;
{
**********
*
*   LOG [fnam]
}
19: begin
  next_token (parm, stat);
  if string_eos(stat)
    then begin                         {no name supplied, set to default}
      string_vstring (parm, 'pbp'(0), -1);
      end
    else begin
      if sys_error(stat) then goto err_cmparm;
      if not_eos then goto err_extra;
      end
    ;

  if log then begin                    {previous log file currently open ?}
    wcsv_close;                        {close the old log file}
    end;

  wcsv_open (parm, stat);              {open this new log file}
  if sys_error(stat) then goto err_cmparm;

  wcsv_str ('Microseconds');           {write CSV file header}
  wcsv_str ('Ind, 100 mA');
  wcsv_str ('Load, 100 mA');
  wcsv_str ('Out volts');
  wcsv_str ('Err, 10 mV');
  wcsv_line;

  logt := 0.0;                         {init logged data time to 0}
  log := true;                         {indicate log file now open}
  end;
{
**********
*
*   UNLOG
}
20: begin
  if not_eos then goto err_extra;

  if not log then goto done_cmd;       {no log file to close and display ?}
  wcsv_close;                          {close the log file}
  log := false;                        {the log file is now closed}

  parm.len := 0;                       {init command string to run}
  string_appends (parm, 'csvplot');    {build the command string}
  string_append_token (parm, csv_out_p^.conn.tnam);
  string_appends (parm, ' -dev screen');
  iounit := 0;
  sys_run (                            {run program to display CSV file contents}
    parm,                              {command line to run}
    sys_procio_none_k,                 {no I/O connection to this process}
    iounit, iounit, iounit,            {I/O unit IDs, not used}
    procid,                            {returned ID of the new process}
    stat);
  if sys_error(stat) then begin
    lockout;
    writeln ('Error on attempt to launch program to show log file:');
    sys_error_print (stat, '', '', nil, 0);
    unlockout;
    end;
  end;
{
**********
*
*   Unrecognized command name.
}
otherwise
    goto bad_cmd;
    end;

done_cmd:                              {done processing this command}
  if sys_error(stat) then goto err_cmparm;

  if not_eos then begin                {extraneous token after command ?}
err_extra:
    lockout;
    writeln ('Too many parameters for this command.');
    unlockout;
    end;
  goto loop_cmd;                       {back to process next command}

bad_cmd:                               {unrecognized or illegal command}
  lockout;
  writeln ('Huh?');
  unlockout;
  goto loop_cmd;

err_cmparm:                            {parameter error, STAT set accordingly}
  lockout;
  sys_error_print (stat, '', '', nil, 0);
  unlockout;
  goto loop_cmd;

cmd_nsupp:
  lockout;
  writeln ('Command not supported by this firmware.');
  unlockout;
  goto loop_cmd;

leave:
  end.
