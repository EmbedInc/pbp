@echo off
rem
rem   Define the variables for running builds from this source library.
rem
set srcdir=pbp
set buildname=
call treename_var "(cog)source/pbp" sourcedir
set libname=
set fwname=pbptest
set pictype=33EP32MC202
set picclass=dsPIC
set t_parms=-qv
call treename_var "(cog)src/%srcdir%/debug_%fwname%.bat" tnam
make_debug "%tnam%"
call "%tnam%"
