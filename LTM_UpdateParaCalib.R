rm(list=ls())
clib=c('rgdal', 'rgeos', 'raster', 'sp', 'PIHMgisR')
x=lapply(clib, library, character.only=T)
nd=11
pjs = paste0('sac', 1:nd)
# pihmout = '/Users/leleshu/Dropbox/PIHM/Projects/SAC/GIS_SUB/PIHM'
pihmout= './'
# dirs = file.path('/Users/leleshu/Dropbox/PIHM/Projects/SAC/GIS_SUB/Subs', pjs)
# dirs


# years = 2000:2002
# nday = 365*length(years) +round(length(years)/4)
go.para<- function(file=PIHM.filein()['md.para']){
  cfg.para=pihmpara()
  print(cfg.para)
  cfg.para['INIT_MODE'] = 3
  cfg.para['START'] = 0
  cfg.para['END'] = 365*17+4+0
  cfg.para['RELTOL']=1e-4
  cfg.para['ABSTOL']=1e-4
  cfg.para['INIT_SOLVER_STEP']=1
  
  id=which(grepl('DT_', names(cfg.para)) )
  # cfg.para[id] = 0
  cfg.para$DT_QR_DOWN = 1440
  cfg.para$DT_YE_GW = 1440
  cfg.para
  # write.config(cfg.para, file=file, backup = F )
  cfg.para
}
go.calib <- function(file=PIHM.filein()['md.calib']){
  cfg.calib = pihmcalib()
  cfg.calib['GEOL_KMACSATH']=0.1
  cfg.calib$`AQ_DEPTH+`=0
  cfg.calib$`RIV_WDTH+`=50
  cfg.calib$`RIV_DPTH+`=0
  cfg.calib$RIV_SINU=1.2
  cfg.calib$`RIV_BSLOPE+`=0
  # write.config(cfg.calib, file=file, backup = F )
  cfg.calib
}
cfg.calib=go.calib()
cfg.para=go.para()

dir.calib = 'bestcalib'
nd=11
for(i in 1:nd){
  prjname = paste0('sac', i)
  outdir = file.path(pihmout, 'input')
  message('\n\n', i,'/', nd, '\t', prjname)
  
  fn=file.path(outdir, prjname, paste0(prjname, '.cfg.para'))
  write.config(cfg.para, file=fn, backup = FALSE)
  fn.new = file.path(dir.calib, paste0(prjname, '.cfg.calib'))
  fn=file.path(outdir, prjname, paste0(prjname, '.cfg.calib'))
  file.copy(from = fn.new, to=fn, overwrite = TRUE)
}
file.copy(from='../PIHM/pihm++',  to='./pihm++', overwrite=T)
file.copy(from='/Users/leleshu/Dropbox/workspace/Xcode/pihm++/build/Products/Debug/pihm++', 
          to='./pihm++', overwrite=T)
fns=file.path('Sub', paste0('Task', 1:nd, '.job'))
dir.create('Sub', showWarnings = F, recursive = T)
for(i in 1:nd){
  message(i, '/',nd, '\t', fns[i])
  str=character()
  str[1] = '#!/bin/bash -l'
  str[2] = paste('#SBATCH -J ', paste0( pjs[i]) )
  str[3] = paste('#SBATCH -o ', paste0('Job_', pjs[i], '.out') )
  str[4] = paste('#SBATCH -e ', paste0('Job_', pjs[i], '.out') )
  str[5] = paste('./pihm++ ', pjs[i])
  write.table(str, fns[i], quote=FALSE, 
              row.name=FALSE, col.names=FALSE)
}

fn='Sub.sh'
cmd=paste('sbatch -t 7200 -N 1 -n 1 ',fns)
write.table(cmd, file=fn,  quote=FALSE, 
            row.name=FALSE, col.names=FALSE)
system(paste('chmod +x ', fn) )
message('Finished. ')