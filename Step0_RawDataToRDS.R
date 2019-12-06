rm(list=ls())
clib=c('rgdal', 'rgeos', 'raster', 'sp')
x=lapply(clib, library, character.only=T)

library(PIHMgisR)

pihmout = '/Users/leleshu/Dropbox/PIHM/Projects/SAC/GIS_SUB/PIHM'
nd = 11
dirs = file.path('/Users/leleshu/Dropbox/PIHM/Projects/SAC/GIS_SUB/Subs', paste0('sac', 1:nd) )

dem = raster('/Users/leleshu/Dropbox/PIHM/Projects/SAC/GIS/dem/dem_m.tif')
rsoil = raster('/Users/leleshu/Dropbox/PIHM/Projects/SAC/GIS/Soil/MUKEY.asc')
asoil = read.csv('/Users/leleshu/Dropbox/PIHM/Projects/SAC/GIS/Soil/mukey_Soil.txt', sep = '\t')
rlc = raster('/Users/leleshu/Dropbox/PIHM/Projects/SAC/GIS/NLCD/landcover2006.asc')
alc = read.table('/Users/leleshu/Dropbox/PIHM/Projects/SAC/GIS/NLCD/landcover.txt')
spforc = readOGR('/Users/leleshu/Dropbox/PIHM/Projects/SAC/GIS/NLDAS/NLDAS_5070.shp')
aforc = spforc@data

for(i in 1:nd){
  cdir = dirs[i]
  prjname = basename(cdir)
  outdir = file.path(pihmout, 'input')
  dir.create(outdir, showWarnings = F, recursive = T)
  ss = prjname
  message(i,'/', nd, '\t', ss)
  riv = readOGR(file.path(cdir, paste0(ss, '_strD.shp') ) )
  # riv = sp.DissolveLines(riv)
  # len = rgeos::gLength(byid=T, riv)
  # id = which(len < 100)
  # riv = riv[-1*id,]
  wbd = readOGR(file.path(cdir, paste0(ss, '_wbd.shp') ) )
  plot(wbd)
  plot(riv, add=T, col=4)
  mtext(side=3, ss)
  #========= =========
  wbbuf = rgeos::gBuffer(wbd, width = 1000)
  r.dem = raster::crop(dem, wbbuf)
  #========= =========
  lc.r = raster::crop(rlc, wbbuf)
  #========= =========
  rdata = rsoil
  rdata = raster::crop(rdata, wbbuf)
  r.id = sort(unique(rdata[]))
  rcl = cbind(r.id, 1:length(r.id))
  d.r = raster::reclassify(rdata, rcl)
  d.tb = asoil[r.id,]
  d.tb[,1] = 1:nrow(d.tb)
  id = which(d.tb[,2] <= 1)
  if(length(id)>0){
    d.tb[id, -1] = round(t(matrix( colMeans(d.tb[-id, -1]), ncol=length(id), nrow=ncol(d.tb)-1)), 2)
  }
  # plot(d.r)
  soil.r = d.r
  soil.tb = d.tb
  #======== ==========
  sp.forc = raster::crop(spforc, wbbuf)
  sp.forc@data[,'ForcingID'] = 1:nrow(sp.forc@data)
  # sp.forc@data
  #==================
  indata = list('wbd'=wbd, 
             'riv'=riv,
             'dem'=r.dem,
             'rsoil' = soil.r,
             'asoil' = soil.tb,
             'rlc' = lc.r,
             'alc' = alc,
             'forc' = sp.forc
  )
  
  sp.forc =indata[['forc']]
  forc.fns = file.path('/Users/leleshu/Dropbox/PIHM/Projects/SAC/forcing/csv2000-2017',
                       paste0(sp.forc@data[, 'NLDAS_ID'], '.csv') )
  forc.fns
  a.max=min(gArea(wbd)/800, 1e6 * 2)
  
  write(forc.fns, file = file.path(cdir, paste0(prjname,'.forc.csv')))
  saveRDS(indata, file=file.path(cdir, paste0(prjname,'.RDS') ) )
  # pm=autoPIHMgis(indata,
  #                prjname = prjname,
  #                forcfiles = forc.fns, 
  #                outdir= file.path(outdir, prjname),
  #                a.max = a.max)
}


