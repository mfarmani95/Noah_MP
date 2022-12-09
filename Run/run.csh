#!/bin/csh

#PBS -N ROOT
#PBS -m bea
#PBS -M jetal164@email.arizona.edu
#PBS -W group_list=niug
#PBS -q standard
###PBS -q windfall
#PBS -l place=free:shared
#PBS -l select=1:ncpus=28:mem=1gb
#PBS -l walltime=7:00:00
#PBS -l cput=80:00:00

 cd /home/u30/jetal164/imerg_noahmp/Run
 /usr/bin/time ./Noah >&out
