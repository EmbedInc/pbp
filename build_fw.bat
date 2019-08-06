@echo off
rem
rem   Build the firmware from this source directory.
rem
setlocal
call build_fwinit

call src_dspic %srcdir% %fwname%_clock
call src_dspic %srcdir% %fwname%_cmd
call src_dspic %srcdir% %fwname%_cmds
call src_dspic %srcdir% %fwname%_init
call src_dspic %srcdir% %fwname%_main
call src_dspic %srcdir% %fwname%_port
call src_dspic %srcdir% %fwname%_pulse
call src_dspic %srcdir% %fwname%_strt
call src_dspic %srcdir% %fwname%_task
call src_dspic %srcdir% %fwname%_trace
call src_dspic %srcdir% %fwname%_trap
call src_dspic %srcdir% %fwname%_uart

call src_exp30 %srcdir% %fwname%
