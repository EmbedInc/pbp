{   Program SHIFTVAL highw loww shift
*
*   Show the value of a 32 bit word with shift count, as used in the PULSE
*   module of the PBP firmware.
*
*   HIGHW is the value of the high 16 bit word, LOWW the value of the low 16 bit
*   word, and SHIFT the number of bits the value has been shifted left from its
*   true representation.
}
program shiftval;
%include '(cog)lib/base.ins.pas';

var
  h, l: sys_int_machine_t;             {high and low 16 bits words}
  sh: sys_int_machine_t;               {shift count}
  w: double;                           {combined word value}
  tk:                                  {scratch strings for formatting answer}
    %include '(cog)lib/string32.ins.pas';
  stat: sys_err_t;

begin
  string_cmline_init;
  string_cmline_token_int (h, stat);   {high 16 bit word}
  sys_error_abort (stat, '', '', nil, 0);
  string_cmline_token_int (l, stat);   {low 16 bit word}
  sys_error_abort (stat, '', '', nil, 0);
  string_cmline_token_int (sh, stat);  {bits the value has been shifted left}
  sys_error_abort (stat, '', '', nil, 0);
  string_cmline_end_abort;

  w := (h * 65536.0) + l;              {make combined 32 bit word value}
  if h >= 32768 then begin             {whole word is really negative ?}
    w := w - (2.0 ** 32);
    end;
  w := w / (2.0 ** sh);                {adjust for the shift count}

  string_f_fp (                        {make result value string}
    tk,                                {output string}
    w,                                 {input value}
    0, 0,                              {free form, no fixed field width}
    5,                                 {minimum significant digits to show}
    6,                                 {max allowed digits left of point}
    0,                                 {min required digits right of point}
    5,                                 {max allowed digits right of point}
    [string_ffp_exp_eng_k],            {engineering notation when exp used}
    stat);
  writeln (tk.str:tk.len);
  end.
