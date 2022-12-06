#!/bin/csh

#PBS -N A3-cal
#PBS -m bea
#PBS -M niug@email.arizona.edu
#PBS -W group_list=niug
#PBS -q standard
##PBS -q windfall
#PBS -l place=free:shared
#PBS -l select=1:ncpus=28:mem=1gb
#PBS -l walltime=40:00:00
#PBS -l cput=1480:00:00

 cd /home/u4/niug/5TB/UA_HPC/NLDAS/NASA-VG-A3-cal/Run
 /usr/bin/time ./Noah >&out
