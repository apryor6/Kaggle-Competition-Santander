#!/bin/csh -f
#  master-singleclass.cmd
#
#  UGE job for master-singleclass built Thu Dec 22 05:20:28 PST 2016
#
#  The following items pertain to this script
#  Use current working directory
#$ -cwd
#  input           = /dev/null
#  output          = /u/home/a/apryor/test/project/Santander/lib/master-singleclass.joblog
#$ -o /u/home/a/apryor/test/project/Santander/lib/master-singleclass.joblog.$JOB_ID
#  error           = Merged with joblog
#$ -j y
#  The following items pertain to the user program
#  user program    = /u/home/a/apryor/test/project/Santander/lib/master-singleclass.R
#  arguments       = 
#  program input   = Specified by user program
#  program output  = Specified by user program
#  Resources requested
#
#$ -l h_data=256G,h_rt=8:00:00,highp,highmem
#
#  Name of application for log
#$ -v QQAPP=R
#  Email address to notify
#$ -M apryor@mail
#  Notify at beginning and end of job
#$ -m bea
#  Job is not rerunable
#$ -r n
#
# Initialization for serial execution
#
  unalias *
  set qqversion = 
  set qqapp     = "R serial"
  set qqidir    = /u/home/a/apryor/test/project/Santander/lib
  set qqjob     = master-singleclass
  set qqodir    = /u/home/a/apryor/test/project/Santander/lib
  cd     /u/home/a/apryor/test/project/Santander/lib
  source /u/local/bin/qq.sge/qr.runtime
  if ($status != 0) exit (1)
#
  echo "UGE job for master-singleclass built Thu Dec 22 05:20:28 PST 2016"
  echo ""
  echo "  master-singleclass directory:"
  echo "    "/u/home/a/apryor/test/project/Santander/lib
  echo "  Submitted to UGE:"
  echo "    "$qqsubmit
  echo "  SCRATCH directory:"
  echo "    "$qqscratch
#
  echo ""
  echo "master-singleclass started on:   "` hostname -s `
  echo "master-singleclass started at:   "` date `
  echo ""
#
# Run the user program
#
  source /u/local/Modules/default/init/modules.csh
  module load R
  module li
#
  echo ""
  echo R CMD BATCH  master-singleclass.R master-singleclass.out.$JOB_ID
#
  /usr/bin/time -p \
  R CMD BATCH  /u/home/a/apryor/test/project/Santander/lib/master-singleclass.R  /u/home/a/apryor/test/project/Santander/lib/master-singleclass.out.$JOB_ID 
#
  echo ""
  echo "master-singleclass finished at:  "` date `
#
# Cleanup after serial execution
#
  source /u/local/bin/qq.sge/qr.runtime
#
  echo "-------- /u/home/a/apryor/test/project/Santander/lib/master-singleclass.joblog.$JOB_ID  --------" >> /u/local/apps/queue.logs/R.log.serial
  if (`wc -l /u/home/a/apryor/test/project/Santander/lib/master-singleclass.joblog.$JOB_ID  | awk '{print $1}'` >= 1000) then
        head -50 /u/home/a/apryor/test/project/Santander/lib/master-singleclass.joblog.$JOB_ID >> /u/local/apps/queue.logs/R.log.serial
        echo " "  >> /u/local/apps/queue.logs/R.log.serial
        tail -10 /u/home/a/apryor/test/project/Santander/lib/master-singleclass.joblog.$JOB_ID >> /u/local/apps/queue.logs/R.log.serial
  else
        cat /u/home/a/apryor/test/project/Santander/lib/master-singleclass.joblog.$JOB_ID >> /u/local/apps/queue.logs/R.log.serial
  endif
  exit (0)
