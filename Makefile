#(make 2>&1) > err
FC = ifort
#OPT = /Qopenmp /Zi /check /traceback /Od
OPT = /Qopenmp /check

all: jcl_sh1.exe

OBJ1 = unreal.obj model1.obj jcl_sh1.obj
jcl_sh1.exe: $(OBJ1)
	$(FC) $(OBJ1) /exe:$@

.SUFFIXES: 
.SUFFIXES: .f90 .obj


.f90.obj:
	$(FC) /c $(OPT)  $<

argtest:
	echoarg /C /B /D /E c:\hoge
clean:
	rm *.obj *.exe *.mod
