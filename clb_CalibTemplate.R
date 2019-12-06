
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  SUBID=6
}else{
  print(args)
  SUBID=args[1]
}

clib=c('adagio','hydroGOF', 'doParallel', 'parallel', 'xts', 'ggplot2')
x=lapply(clib, library, character.only=TRUE)
library(PIHMgisR)
# rm(list=ls())
odir = '.'
# odir ='/Users/leleshu/Dropbox/workspace/Xcode/PIHM++/Build/Products/Debug'
cdir = 'CalibFiles'
prjname=paste0('sac', SUBID)
inpath = file.path(odir, 'input', prjname)
outpath = file.path(odir, 'output', paste0(prjname, '.out') )
dir.create(inpath, showWarnings = F, recursive = T)
dir.create(outpath, showWarnings = F, recursive = T)

PIHM(prjname, inpath, outpath)
# source('StepX_ParaCalib.R')
source(file.path(cdir, 'ObjectiveFunction.R') )
fn.cmaes = file.path(cdir, paste0('sac', '.calib.cmaes'))
# write.cmaes(file= fn.cmaes, backup = TRUE)
x.cmaes = readconfig(file = fn.cmaes)
x.cmaes$PATH_OUT = file.path(odir, 'cmaes_out', paste0('sac', SUBID) )
dir.create(as.character(x.cmaes$PATH_OUT), showWarnings = FALSE, recursive = TRUE)

fn.range=file.path(cdir, paste0('sac', '.calib.range'))
calset = readconfig(file=fn.range)
calset
rownames(calset) = c('onoff', 'log', 'min', 'max')

fn.calib=PIHM.filein()['md.calib']
calib=readcalib(file=fn.calib)

para=readconfig(file = PIHM.filein()['md.para'] )
t0 = para['START'];
t1 = t0 + x.cmaes$nspinup; 
t2 = para['END']
# xdf = cbind('skip'=qo[1:t0, ], 'spinup'=qo[t0:t1,], 'calib'=qo[t1:t2,])
# debug(mycmaes)
unlink(as.character( x.cmaes$PATH_OUT ), recursive = FALSE)

qo=read.tsd(file.path(inpath, paste0(prjname,'.tsd.obs')))[[1]]
time(qo) = as.Date(time(qo))
tab.gage = read.csv('CalibFiles/gageid2019.10.csv', sep=',')
rivID = as.numeric(tab.gage$RiverID[SUBID])
# stop()
CV = list('prjname' =prjname, 'calib'=calib, 'range'=calset,
          'method' = type.convert(x.cmaes),
          'obs' = qo, 'rivID' = as.numeric(rivID), 'minLength' = 100 )
dir.create(as.character(CV$method$PATH_OUT), showWarnings = FALSE, recursive = TRUE)
saveRDS(CV, file.path(CV$method$PATH_OUT, 'cv.RDS') )
file.copy(from='../PIHM/pihm++',  to='./pihm++', overwrite=T)
file.copy(from='/Users/leleshu/Dropbox/workspace/Xcode/pihm++/build/Products/Debug/pihm++', 
          to='./pihm++', overwrite=T)
#debug(CMAES)
# debug(Call_Model)
message('rivID=', CV$rivID)
sol1 <- CMAES(CV=CV, cmd='./pihm++',
              objfunc=Obj.Func,  Call_Model=Call_Model,
              oid=rivID)
# debug(Obj.Func)
# Obj.Func(jobid=1, iGen=1, inpath='input/sac6', 
#          outpath='output/sac6.out/sac6_1', CV=CV, outdir = CV$method$PATH_OUT)
