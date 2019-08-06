@echo off
rem
rem   Build everything from this source directory.
rem
setlocal
call godir "(cog)source/pbp"

call build_fw
call build_progs
