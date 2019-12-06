rm(list=ls())
clib=c('rgdal', 'rgeos', 'raster', 'sp', 'PIHMgisR')
x=lapply(clib, library, character.only=T)
# debug(m.DomainDecomposition)
nd=11
pjs = paste0('sac', 1:nd)
pihmout = '/Users/leleshu/Dropbox/PIHM/Projects/SAC/GIS_SUB/PIHM'
dirs = file.path('/Users/leleshu/Dropbox/PIHM/Projects/SAC/GIS_SUB/Subs', pjs)
dirs

years = 2000:2010
nday = 365*length(years) +round(length(years)/4)
cfg.para = pihmpara(nday=nday)
cfg.calib = pihmcalib()
tsd.mf = MeltFactor(years=years)

i=9
forcdir = '/home/llshu/csv2000-2017'
# forcdir = '/Users/leleshu/Dropbox/PIHM/Projects/SAC/forcing/csv2000-2017'
for(i in 1:nd){
  cdir = dirs[i]
  prjname = basename(cdir)
  outdir = file.path(pihmout, 'input')
  message('\n\n', i,'/', nd, '\t', prjname)
  # write(forc.fns, file = file.path(cdir, paste0(prjname,'.forc.csv')))
  # saveRDS(indata, file=file.path(cdir, paste0(prjname,'.RDS') ) )
  
  forc.fns = readLines(file.path(cdir, paste0(prjname,'.forc.csv')))
  forc.fns = file.path(forcdir, basename(forc.fns))
  # forc.fns = file.path('/home/llshu/csv2000-2017', basename(forc.fns))
  indata = readRDS(file.path(cdir, paste0(prjname,'.RDS') ) )
  
  wbd = indata[['wbd']]
  a.max = round( min(max(gArea(wbd)/700, 1e6*0.2), 1e6 * 2) ) 
  message('Area.Max = ', a.max *1e-6)
  names(indata)
  plot(indata[['forc']])
  rforc = indata[['forc']]
  a =rforc@data
  pxy=coordinates(rforc)
  plot(rforc); text(pxy[,1], pxy[,2], as.character(a[,5]))
  # debug(m.DomainDecomposition)
  # undebug(autoPIHMgis)
  pm=PIHMgisR::autoPIHMgis(indata, backup=FALSE,
                 prjname = prjname,
                 forcfiles = forc.fns,
                 outdir= file.path(outdir, prjname),
                 a.max = a.max,
                 cfg.para=cfg.para,
                 cfg.calib=cfg.calib,
                 mf=tsd.mf, AqDepth = 30, 
                 rm.outlier = TRUE)
}
source('StepX_ParaCalib.R')
source('StepX_ForcOnly.R')
source('StepX_Gage_obs_fig.R')

