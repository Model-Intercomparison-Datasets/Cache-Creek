# read gage data from .RDS file and export tsd.obs file.

rm(list=ls())
clib=c('rgdal', 'rgeos', 'raster', 'sp', 'xts')
x=lapply(clib, library, character.only=T)

library(PIHMgisR)

pj = 'sac'
outdir = '/Users/leleshu/Dropbox/PIHM/Projects/SAC/GIS_SUB/obs'
pihmdir = '/Users/leleshu/Dropbox/PIHM/Projects/SAC/GIS_SUB/PIHM/input'
s=readOGR('/Users/leleshu/Dropbox/PIHM/Projects/SAC/GIS/HeadWaters/Gage_WBD.shp')
ol = readRDS('/Users/leleshu/Dropbox/PIHM/Projects/SAC/GIS/Gage/gageData.RDS')
head(s@data)
gageid = as.character(s@data[,'SOURCE_FEA'])
head(s@data)
sid = as.numeric( s@data[,'SAC'] )
usid = sort(unique( sid ))
nsid = length(usid)
i=1
t0 = as.Date('2000-01-01')
t1 = as.Date('2005-01-01')
xl=list()
for(i in 1:nsid){
  key = usid[i]
  id = which(sid %in% key)    
  gids = gageid[id]
  ng = length(gids)
  j=1
  message('\n\n', i,'/', nsid)
  xm = NULL
  for(j in 1:ng){
    cdir = file.path(outdir, paste0('sac',i))
    dir.create(cdir, showWarnings = F, recursive = TRUE)
    prefix = file.path(cdir, paste0(pj, i,'_', gids[j]) )
    fn.obs =  paste0(prefix, '.obs')
    fn.png =  paste0(prefix, '.png')
    message('\t', j,'/', ng, '\t', gids[j])
    qo = ol[[gids[j]]]
    tq = time(qo)
    head(qo)  
    x = qo[tq >= t0 & tq < t1]
    write.tsd(x*86400, file=fn.obs, backup = FALSE)
    fn.obs2 =  file.path(pihmdir, paste0('sac',i), paste0(pj, i,'.tsd.obs') )
    write.tsd(x*86400, file=fn.obs2, backup = FALSE)
    
    png.control(file = fn.png)
    plot.zoo(x, main='Q (cms) ')
    grid()
    dev.off()
    xm = cbind(xm, x)
  }
  colnames(xm) = gids
  xl[[i]] = xm;
}

png(filename =file.path(outdir, paste0('subs_Q.png')), height = 11, width = 15, units = 'in', res=96)
par(mfrow=c(3 ,4), mar=c(1,1,1,1.5)*2, omi=c(1,1,1,1)*.5)
for(i in 1:length(xl)){
  x = xl[[i]]
  plot.zoo(x, screen=1)
  mtext(side=3, paste0('sac ', i), line=-2)
}
dev.off()
