#!/bin/sh
#@$-q r256
#@$-r JOB
#@$-lP 32
#@$-lp 8
#@$-lm 21574mb
#@$-oi
#@$-eo
#@$-o output
#@$-x

#. /opt/intel/ictce/3.2.1.015/ictvars.sh
source /home/saitohm/.bashrc
export PATH=/opt/intel/impi/3.2.1/bin:$PATH

cd ${QSUB_WORKDIR}
export MPD_CON_EXT=${QSUB_REQID}
plen2c ${QSUB_NODEINF} > ./machinefile.${QSUB_REQID}
mpdboot -n ${QSUB_VNODES} -f ./machinefile.${QSUB_REQID} -r plesh

#/home/saitohm/MPICH2/bin/mpiexec -n 32 -f ./machinefile.${QSUB_REQID} ./mpi_sample_mpich32
mpiexec -n 256 sml ./go.sml 512
mpdallexit
