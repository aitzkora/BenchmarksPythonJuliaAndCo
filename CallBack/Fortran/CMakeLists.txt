cmake_minimum_required(VERSION 3.10)
project(CallBack)
enable_language(Fortran C)
add_executable(call_back call_back.f90 m_call_back.f90 utils.c m_utils.f90)
