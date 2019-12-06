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
  cfg.para['START'] = 180
  cfg.para['END'] = 365*3+1+180
  cfg.para['RELTOL']=1e-4
  cfg.para['ABSTOL']=1e-4
  cfg.para['INIT_SOLVER_STEP']=1
  
  id=which(grepl('DT_', names(cfg.para)) )
  cfg.para[id] = 0
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

nd=11
for(i in 1:nd){
  prjname = paste0('sac', i)
  outdir = file.path(pihmout, 'input')
  message('\n\n', i,'/', nd, '\t', prjname)
  
  fn=file.path(outdir, prjname, paste0(prjname, '.cfg.para'))
  write.config(cfg.para, file=fn, backup = FALSE)
  
  fn=file.path(outdir, prjname, paste0(prjname, '.cfg.calib'))
  write.config(cfg.calib, file=fn, backup = FALSE)
}

