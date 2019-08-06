{   Program TEST_PBP [options]
*
*   This program provides a command line interface for the binary protocol of
*   the PBPTEST firmware, which runs the switching power supply pulse-by-pulse
*   testbed.
}
program test_pbp;
%include '(cog)lib/base.ins.pas';
%include '(cog)lib/stuff.ins.pas';
%include '(cog)lib/pic.ins.pas';

const
  vref = 2.5;                          {reference voltage for full scale}
  fcy = 69.78e6;                       {instruction cycle frequency, Hz}
  tcy = 1.0 / fcy;                     {instruction cycle time, secnds}
  fwtype_k = 43;                       {expected firmware type ID}
  baud_k = file_baud_115200_k;         {serial line baud rate}
  outbuf_size = 32;                    {size of buffer for bytes to remote unit}
  n_cmdnames_k = 13;                   {number of command names in the list}
  cmdname_maxchars_k = 7;              {max chars in any command name}
  max_msg_parms = 2;                   {max parameters we can pass to a message}
{
*   Response opcodes.
}
  rsp_nop_k = 0;                       {NOP, ignored}
  rsp_pong_k = 1;                      {response to PING command}
  rsp_fwinfo_k = 2;                    {firmware version info}
  rsp_vset_k = 3;                      {output voltage set point}
  rsp_mval_k = 4;                      {measured values}
  rsp_trace_k = 5;                     {dump of stored trace data}
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
    'PING   ',                         {5}
    'FWINFO ',                         {6}
    'V      ',                         {7}
    'GV     ',                         {8}
    'MVAL   ',                         {9}
    'ARM    ',                         {10}
    'TRIG   ',                         {11}
    'TOFF   ',                         {12}
    'BREAK  ',                         {13}
    ];

var
  sio: sys_int_machine_t;              {number of system serial line to use}
  conn: file_conn_t;                   {connection to the system serial line}
  fwtyp: sys_int_machine_t;            {firmware type ID}
  fwver: sys_int_machine_t;            {firmware version number}
  fwseq: sys_int_machine_t;            {firmware sequence number}
  thid_in: sys_sys_thread_id_t;        {ID of input thread}
  ev_fwver: sys_sys_event_id_t;        {notified on firmware version response}
  ev_pong: sys_sys_event_id_t;         {notified on PONG response}
  ii: sys_int_machine_t;               {scratch integer and loop counter}
  prompt:                              {prompt string for entering command}
    %include '(cog)lib/string4.ins.pas';
  cmdv: array[0 .. 255] of boolean;    {TRUE for valid commands}
  vreg: real;                          {output voltage regulation setpoint}
  quit: boolean;                       {TRUE when trying to exit the program}
  isopen: boolean;                     {connection to remote unit is open}
  pst: string_index_t;                 {scratch command parse index}
  show_nop: boolean;                   {show NOP responses}
  show_pong: boolean;                  {show PONG responses}
  i1: sys_int_machine_t;               {integer command parameters}
  r1: double;                          {floating point command parameters}

  opt:                                 {upcased command line option}
    %include '(cog)lib/string_treename.ins.pas';
  parm:                                {command parameter}
    %include '(cog)lib/string_treename.ins.pas';
  pick: sys_int_machine_t;             {number of token picked from list}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;
  stat: sys_err_t;                     {completion status code}

label
  next_opt, err_parm, parm_bad, done_opts, loop_cmd,
  done_cmd, err_extra, bad_cmd, err_cmparm, cmd_nsupp, leave;

%include '(cog)lib/wout_local.ins.pas'; {define std out writing routines}
%include '(cog)lib/nextin_local.ins.pas'; {define command reading routines}
%include '(cog)lib/send_local.ins.pas'; {define routines for sending to device}
%include '(cog)lib/csv_local.ins.pas'; {define routines for sending to device}
{
********************************************************************************
*
*   Function MINVER (TYP, VER)
*
*   Returns TRUE iff the remote unit firmware has the type ID TYP and its
*   version is at least VER.
}
function minver (                      {check for minimum firmware version}
  in      typ: sys_int_machine_t;      {required firmware version type ID}
  in      ver: sys_int_machine_t)      {min required firmware version within type}
  :boolean;                            {min requirements are met}
  val_param; internal;

begin
  minver := false;                     {init to command not supported}

  if                                   {sufficient firmware version ?}
      (fwtyp = typ) and                {right firmware type ?}
      (fwver >= ver)                   {minimum required version }
      then begin
    minver := true;                    {the remote firmware is sufficient}
    end;
  end;
{
********************************************************************************
*
*   Subroutine AVAILABLE_CMDS
*
*   Update the list of available commands to the currently known firmware
*   versions in the remote device.  This routine is intended to be called
*   whenever new firmware information is received.  The list of available
*   commands in CMDV must be previously initialized.
}
procedure available_cmds;              {recompute available commands}
  val_param; internal;

begin
  cmdv[0] := true;                     {NOP}
  cmdv[1] := true;                     {PING}
  cmdv[2] := true;                     {FWINFO}
  cmdv[3] := minver(fwtype_k, 1);      {VSET}
  cmdv[4] := minver(fwtype_k, 1);      {GVSET}
  cmdv[5] := minver(fwtype_k, 1);      {MVAL}
  cmdv[6] := minver(fwtype_k, 1);      {ARM}
  cmdv[7] := minver(fwtype_k, 1);      {TRIG}
  cmdv[8] := minver(fwtype_k, 1);      {TOFF}
  cmdv[9] := minver(fwtype_k, 3);      {BREAK}
  end;
{
********************************************************************************
*
*   Subroutine SENDALL
*
*   Send all buffered bytes, if any.  This routine does nothing if the output
*   buffer is empty.  The output buffer is reset to empty after its content is
*   sent.
*
*   This routine is required by the SEND_LOCAL include file.
}
procedure sendall;                     {send all buffered output bytes, if any}
  val_param; internal;

var
  stat: sys_err_t;                     {completion status}

begin
  if outbuf.len <= 0 then return;      {buffer is empty, nothing to send ?}

  send_acquire;
  file_write_sio_rec (outbuf, conn, stat); {send the bytes over the serial line}
  sys_error_abort (stat, '', '', nil, 0);
  outbuf.len := 0;                     {reset output buffer to empty}
  send_release;
  end;
{
********************************************************************************
*
*   Subroutine CMDWAIT
*
*   Wait for the remote system to finish executing all commands sent to it and
*   that are in our output buffer.
*
*   The remote system has a limited command stream input buffer.  It can get
*   overrun by sending commands faster than the remote system can process
*   them.  This routine waits for the command input buffer to be drained, and
*   should be called periodically if command bytes are sent continuously.
*
*   The output sending lock must NOT be held when this routine is called.
}
procedure cmdwait;                     {wait for remote system to finish all commands}
  val_param; internal;

var
  stat: sys_err_t;

begin
  discard( sys_event_wait_tout(ev_pong, 0.0, stat) ); {reset PONG received event}

  show_pong := false;                  {disable showing next PONG response}
  send_acquire;
  sendb (1);                           {PING opcode}
  send_release;
  sendall;                             {send any buffered commands, followed by the PING}

  if sys_event_wait_tout (ev_pong, 5.0, stat) then begin {didn't get PONG ?}
    lockout;
    sys_error_abort (stat, '', '', nil, 0); {abort on hard error}
    writeln ('Remote system failed to respond.');
    sys_bomb;
    end;
  end;
{
********************************************************************************
*
*   Subroutine THREAD_IN (ARG)
*
*   This routine is run in a separate thread.  It reads the response stream from
*   the host unit and processes the indiviual responses.
}
procedure thread_in (                  {get data bytes from serial line}
  in      arg: sys_int_adr_t);         {unused argument}
  val_param; internal;

var
  b: sys_int_machine_t;                {data byte value}
  ii: sys_int_machine_t;               {scratch integer}
  i1, i2, i3, i4:                      {integer response parameters}
    sys_int_conv32_t;
  r1, r2, r3: real;                    {floating point response parameters}
  tk: string_var32_t;                  {scratch token}
  procid: sys_sys_proc_id_t;           {scratch process ID}
  iounit: sys_sys_iounit_t;            {scratch system I/O unit ID}
  stat: sys_err_t;

label
  loop, err_locked, done_rsp;
{
******************************
*
*   Function IBYTE
*   This function is local to THREAD_IN.
*
*   Return the next byte from the remote unit.
}
function ibyte                         {return next byte from remote system}
  :sys_int_machine_t;                  {0-255 byte value}

var
  buf: string_var4_t;                  {raw bytes input buffer}
  stat: sys_err_t;                     {completion status}

begin
  buf.max := 1;                        {allow only single byte to be read at a time}
  file_read_sio_rec (conn, buf, stat); {read next byte from serial line}
  if quit then begin                   {trying to exit the program ?}
    sys_thread_exit;
    end;
  sys_error_abort (stat, '', '', nil, 0);
  ibyte := ord(buf.str[1]);
  end;
{
******************************
*
*   Function GETI16U
*   This function is local to THREAD_IN.
*
*   Returns the next two input bytes interpreted as a unsigned 16 bit integer.
}
function geti16u                       {get next 2 bytes as unsigned integer}
  :sys_int_machine_t;

var
  ii: sys_int_machine_t;

begin
  ii := lshft(ibyte, 8);               {get the high byte}
  ii := ii ! ibyte;                    {get the low byte}
  geti16u := ii;
  end;
{
******************************
*
*   Function GETI16S
*   This function is local to THREAD_IN.
*
*   Returns the next two input bytes interpreted as a signed 16 bit integer.
}
(*
function geti16s                       {get next 2 bytes as signed integer}
  :sys_int_machine_t;

var
  ii: sys_int_machine_t;

begin
  ii := lshft(ibyte, 8);               {get the high byte}
  ii := ii ! ibyte;                    {get the low byte}
  if ii >= 32768 then begin            {negative ?}
    ii := ii - 65536;
    end;
  geti16s := ii;                       {pass back the result}
  end;
*)
{
******************************
*
*   Function GETI24U
*   This function is local to THREAD_IN.
*
*   Returns the next three input bytes interpreted as a unsigned 24 bit integer.
}
(*
function geti24u                       {get next 3 bytes as unsigned integer}
  :sys_int_machine_t;

var
  ii: sys_int_machine_t;

begin
  ii := lshft(ibyte, 16);              {get the high byte}
  ii := ii ! lshft(ibyte, 8);          {get the middle byte}
  ii := ii ! ibyte;                    {get the low byte}
  geti24u := ii;
  end;
*)
{
******************************
*
*   Executable code for subroutine THREAD_IN.
}
begin
  tk.max := size_char(tk.str);         {init local var strings}

loop:                                  {back here each new response opcode}
  b := ibyte;                          {get response opcode byte}
  case b of
{
*   NOP
}
rsp_nop_k: begin
  if show_nop then begin
    lockout;
    writeln ('NOP');
    unlockout;
    end;
  end;
{
*   PONG
}
rsp_pong_k: begin
  if show_pong
    then begin
      lockout;
      writeln ('PONG');
      unlockout;
      end
    else begin
      sys_event_notify_bool (ev_pong); {indicate PONG response received}
      end;
    ;
  show_pong := true;                   {restore to showing PONG responses}
  end;
{
*   FWINFO type version sequence
}
rsp_fwinfo_k: begin
  i1 := ibyte;                         {get firmware type ID}
  i2 := ibyte;                         {get firmware version number}
  i3 := ibyte;                         {get firmware sequence number}

  fwtyp := i1;
  fwver := i2;
  fwseq := i3;

  lockout;                             {get exclusive use of standard output}
  writeln;
  write ('Firmware version is type ');
  case i1 of
fwtype_k:  begin
      write ('PBPTEST');
      end;
otherwise
    write (i1);
    end;
  writeln (' ver ', i2, ' seq ', i3);
  unlockout;                           {release lock on standard output}

  available_cmds;                      {update list of available commands}
  sys_event_notify_bool (ev_fwver);    {indicate firmware version received}
  end;
{
*   VSET mv
}
rsp_vset_k: begin
  i1 := geti16u;                       {output setpoint, mV}

  vreg := i1 / 1000.0;                 {save output setpoint, Volts}
  lockout;
  writeln ('Output setpoint ', vreg:6:3, ' V');
  unlockout;
  end;
{
*   MVAL an0 vin vout
}
rsp_mval_k: begin
  i1 := geti16u;                       {AN0 measurement, 0-65535 full scale}
  i2 := geti16u;                       {input EMF, mV}
  i3 := geti16u;                       {output EMF, mV}

  r1 := vref * (i1 / 65535.0);         {AN0 volts}
  r2 := i2 / 1000.0;                   {input volts}
  r3 := i3 / 1000.0;                   {output volts}

  lockout;
  writeln ('Measured values:');
  writeln ('  AN0     ', r1:7:4, ' V');
  writeln ('  Input   ', r2:6:3, '  V');
  writeln ('  Output  ', r3:6:3, '  V');
  unlockout;
  end;
{
*   TRACE nsamp wordps trigofs word ... word
}
rsp_trace_k: begin
  i1 := geti16u;                       {number of samples}
  i2 := ibyte;                         {number of words per sample}
  i3 := geti16u;                       {0-N sample number of the trigger}

  if i3 > i1 then begin                {no trigger ?}
    i3 := 0;                           {display data as if trigger is at first sample}
    end;

  wcsv_open (string_v('pbp'), stat);   {create and open the CSV output file}
  sys_error_abort (stat, '', '', nil, 0);

  wcsv_str ('Sample');                 {write the header line}
  wcsv_str ('OutOfs (mV)');
  wcsv_str ('Ton (100ns)');
  wcsv_str ('Ip, mA');
  wcsv_str ('I2, mA');
  wcsv_str ('I1, mA');
  wcsv_line;

  for ii := 0 to i1-1 do begin         {once for each sample}
    i4 := ii - i3;                     {make sample number relative to the trigger}
    wcsv_int (i4);                     {sample number}

    i4 := geti16u;                     {VLAST, output voltage, mV}
    r1 := i4 / 1000.0;                 {output, volts}
    r1 := r1 - vreg;                   {offset from regulation level, volts}
    r1 := r1 * 1000.0;                 {offset, units of mV}
    wcsv_fp_fixed (r1, 0);

    i4 := geti16u;                     {TON, inductor on time, instruction cycles}
    r1 := i4 * tcy;                    {seconds}
    r1 := r1 / 100.0e-9;               {units of 100 ns}
    wcsv_fp_fixed (r1, 2);

    i4 := geti16u;                     {IP, peak inductor current}
    wcsv_fp_fixed (i4, 0);

    i4 := geti16u;                     {I2, ending inductor current}
    wcsv_fp_fixed (i4, 0);

    i4 := geti16u;                     {I2, starting inductor current}
    wcsv_fp_fixed (i4, 0);

    wcsv_line;
    end;                               {back to get next sample and write it to CSV file}

  wcsv_close;                          {done writing the CSV file}
  iounit := 0;
  sys_run (                            {run program to display CSV file contents}
    string_v('csvplot pbp -dev screen -miny -5'), {command line to run}
    sys_procio_none_k,                 {no I/O connection to this process}
    iounit, iounit, iounit,            {I/O unit IDs, not used}
    procid,                            {returned ID of the new process}
    stat);
  if sys_error(stat) then begin
    lockout;
    writeln ('Error on attempt to launch program to show trace data CSV file:');
    sys_error_print (stat, '', '', nil, 0);
    unlockout;
    end;
  end;
{
*   Unrecognized response opcode.
}
otherwise
    lockout;                           {get exclusive use of standard output}
    write ('Rsp: ');
    whex (b);
    writeln ('h  ', b);
err_locked:                            {jump here on error while output locked}
    unlockout;                         {release lock on standard output}
    end;

done_rsp:                              {done processing this response}
  goto loop;                           {back to read next byte from serial line}
  end;
{
********************************************************************************
*
*   Subroutine OPEN_CONN
*
*   Open the connection to the remote unit.
*
*   Nothing is done if the connection to the remote unit is already open.
}
procedure open_conn;                   {make sure connection to remote unit is open}
  val_param; internal;

var
  tk: string_var32_t;                  {scratch token}

begin
  tk.max := size_char(tk.str);         {init local var string}

  if isopen then return;               {connection previously opened ?}

  file_open_sio (                      {open connection to the serial line}
    sio,                               {number of serial line to use}
    baud_k,                            {baud rate}
    [],                                {no flow control}
    conn,                              {returned connection to the serial line}
    stat);
  sys_error_abort (stat, '', '', nil, 0);
  file_sio_set_eor_read (conn, '', 0); {no special input end of record sequence}
  file_sio_set_eor_write (conn, '', 0); {no special output end of record sequence}
  isopen := true;                      {connection to remote unit is now open}
{
*   Perform some system initialization.
}
  sys_thread_create (                  {start thread for reading serial line input}
    addr(thread_in),                   {address of thread root routine}
    0,                                 {argument passed to thread (unused)}
    thid_in,                           {returned thread ID}
    stat);
  sys_error_abort (stat, '', '', nil, 0);

  sendb (2);                           {request firmware version}
  sendb (4);                           {request output voltage setpoint}
  sendall;                             {make sure command is physically sent}
  discard( sys_event_wait_tout(ev_fwver, 1.0, stat) ); {wait a little while for FW version}
  sys_error_abort (stat, '', '', nil, 0);
  end;
{
****************************************************************************
*
*   Start of main routine.
}
begin
  wout_init;                           {init output writing state}
  send_init;                           {init state for sending to remote unit}
  send_high_low;                       {send multi-byte data in high to low byte order}
  isopen := false;                     {indicate connection to remote unit not open}
  quit := false;                       {init to not trying to exit the program}
{
*   Initialize our state before reading the command line options.
}
  sio := 1;                            {init to default serial line number}
  sys_envvar_get (string_v('TEST_PBP_SIO'), parm, stat);
  if not sys_error(stat) then begin
    string_t_int (parm, ii, stat);
    if not sys_error(stat) then begin
      sio := ii;
      end;
    end;

  show_nop := false;                   {init to not show NOP responses}
  show_pong := true;
  string_cmline_init;                  {init for reading the command line}
{
*   Back here each new command line option.
}
next_opt:
  string_cmline_token (opt, stat);     {get next command line option name}
  if string_eos(stat) then goto done_opts; {exhausted command line ?}
  sys_error_abort (stat, 'string', 'cmline_opt_err', nil, 0);
  string_upcase (opt);                 {make upper case for matching list}
  string_tkpick80 (opt,                {pick command line option name from list}
    '-SIO',
    pick);                             {number of keyword picked from list}
  case pick of                         {do routine for specific option}
{
*   -SIO n
}
1: begin
  string_cmline_token_int (sio, stat);
  end;
{
*   Unrecognized command line option.
}
otherwise
    string_cmline_opt_bad;             {unrecognized command line option}
    end;                               {end of command line option case statement}

err_parm:                              {jump here on error with parameter}
  string_cmline_parm_check (stat, opt); {check for bad command line option parameter}
  goto next_opt;                       {back for next command line option}

parm_bad:                              {jump here on got illegal parameter}
  string_cmline_reuse;                 {re-read last command line token next time}
  string_cmline_token (parm, stat);    {re-read the token for the bad parameter}
  sys_msg_parm_vstr (msg_parm[1], parm);
  sys_msg_parm_vstr (msg_parm[2], opt);
  sys_message_bomb ('string', 'cmline_parm_bad', msg_parm, 2);

done_opts:                             {done with all the command line options}
{
*   All done reading the command line.
}
  fwtyp := 0;                          {init firmare version to unknown}
  fwver := 0;
  fwseq := 0;
  vreg := 5.0;

  for ii := 0 to 255 do begin          {init all remote commands to unimplemented}
    cmdv[ii] := false;
    end;
  available_cmds;                      {init to minimum required commands implemented}

  sys_event_create_bool (ev_fwver);    {create firmware version received event}
  sys_event_create_bool (ev_pong);     {create PONG response received event}

  open_conn;                           {open the connection to the remote unit}
{
***************************************
*
*   Process user commands.
*
*   Initialize before command processing.
}
  string_vstring (prompt, ': '(0), -1); {set command prompt string}

loop_cmd:
  sys_wait (0.100);
  show_pong := true;                   {init to show PONG responses}
  lockout;
  string_prompt (prompt);              {prompt the user for a command}
  newline := false;                    {indicate STDOUT not at start of new line}
  unlockout;

  string_readin (inbuf);               {get command from the user}
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
  writeln ('HELP or ?      - Show this list of commands.');

  if cmdv[1] then
    writeln ('PING           - Send PING command to test communication link');
  if cmdv[2] then
    writeln ('FWINFO         - Request firmware version info');
  if cmdv[3] then
    writeln ('V mv           - Set output voltage');
  if cmdv[4] then
    writeln ('GV             - Get output voltage setpoint');
  if cmdv[5] then
    writeln ('MVAL           - Get directly measured values');
  if cmdv[6] then
    writeln ('ARM            - Arm data trace for triggering');
  if cmdv[7] then
    writeln ('TRIG           - Artificially trigger data trace');
  if cmdv[8] then
    writeln ('TOFF           - Dis-arm data trace, triggers ignored');
  if cmdv[9] then
    writeln ('BREAK          - Sets breakpoint flag, for debugging');

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
*   PING
}
5: begin
  if not cmdv[1] then goto cmd_nsupp;
  if not_eos then goto err_extra;

  send_acquire;
  sendb (1);                           {send PING command}
  send_release;
  sendall;                             {send command now, output buffer will be empty}
  end;
{
**********
*
*   FWINFO
}
6: begin
  if not cmdv[2] then goto cmd_nsupp;
  if not_eos then goto err_extra;

  send_acquire;
  sendb (2);                           {request the firmware version}
  send_release;
  end;
{
**********
*
*   V volts
}
7: begin
  if not cmdv[3] then goto cmd_nsupp;
  r1 := next_fp (stat);
  if sys_error(stat) then goto err_cmparm;
  if not_eos then goto err_extra;

  r1 := max(0.0, min(65.5355, r1));    {clip to valid range}
  i1 := trunc(r1 * 1000.0);            {make integer mV}
  sendb (3);                           {VSET opcode}
  sendw (i1);                          {mV}
  sendb (4);                           {GVSET opcode}
  end;
{
**********
*
*   GV
}
8: begin
  if not cmdv[4] then goto cmd_nsupp;
  if not_eos then goto err_extra;

  send_acquire;
  sendb (4);                           {GVSET opcode}
  send_release;
  end;
{
**********
*
*   MVAL
}
9: begin
  if not cmdv[5] then goto cmd_nsupp;
  if not_eos then goto err_extra;

  send_acquire;
  sendb (5);                           {MVAL opcode}
  send_release;
  end;
{
**********
*
*   ARM
}
10: begin
  if not cmdv[6] then goto cmd_nsupp;
  if not_eos then goto err_extra;

  send_acquire;
  sendb (6);                           {ARM opcode}
  send_release;
  end;
{
**********
*
*   TRIG
}
11: begin
  if not cmdv[7] then goto cmd_nsupp;
  if not_eos then goto err_extra;

  send_acquire;
  sendb (7);                           {TRIG opcode}
  send_release;
  end;
{
**********
*
*   TOFF
}
12: begin
  if not cmdv[8] then goto cmd_nsupp;
  if not_eos then goto err_extra;

  send_acquire;
  sendb (8);                           {TOFF opcode}
  send_release;
  end;
{
**********
*
*   BREAK
}
13: begin
  if not cmdv[9] then goto cmd_nsupp;
  if not_eos then goto err_extra;

  send_acquire;
  sendb (9);                           {BREAK opcode}
  send_release;
  end;
{
**********
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

  if outbuf.len > 0 then begin         {there are unsent bytes in output buffer ?}
    cmdwait;                           {send them and wait for remote system done}
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
  quit := true;                        {tell all threads to shut down}
  file_close (conn);                   {close connection to the serial line}
  end.
