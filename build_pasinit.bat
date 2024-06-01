@echo off
rem
rem   Set up for building a Pascal module.
rem
call build_vars

call src_getbase

make_debug debug_switches.ins.pas
call src_builddate "%srcdir%"
