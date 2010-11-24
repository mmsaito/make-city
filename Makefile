#(make 2>&1) > err
FC = ifort
OPT = /Qopenmp

all: unreal.obj

.SUFFIXES: 
.SUFFIXES: .f90 .obj


.f90.obj:
	$(FC) /c $(OPT)  $<

argtest:
	echoarg /C /B /D /E c:\hoge
