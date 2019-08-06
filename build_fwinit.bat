@echo off
rem
rem   Set up the environment for building firmware from this source directory.
rem
call build_vars
set using_xc16=FALSE
set heapsize=0

rem   Get assembler "library" include files.  These are left in the SRC/DSPIC
rem   directory.
rem
call src_go dspic
make_dspic_inc %pictype%
call src_get_ins_dspic dspic cmd
call src_get_ins_dspic dspic cmds
call src_get_ins_dspic dspic fwtype
call src_get_ins_dspic dspic lcase
call src_get_ins_dspic dspic port
call src_get_ins_dspic dspic std_def
call src_get_ins_dspic dspic std
call src_get_ins_dspic dspic task
call src_get_ins_dspic dspic trace
call src_get_ins_dspic dspic trace_setup
call src_get_ins_dspic dspic uart

call src_get_ins_dspic %srcdir% %fwname%_cmdrsp

call src_ins_dspic %srcdir% %fwname%lib -set make_version
call src_get_ins_dspic %srcdir% %fwname%
