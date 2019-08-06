@echo off
rem
rem   BUILD_PROGS [-dbg]
rem
rem   Build the executable programs from this source directory.
rem
setlocal
call build_pasinit

call src_prog %srcdir% pbpsim %1
call src_prog %srcdir% shiftval %1
call src_prog %srcdir% test_pbp %1
