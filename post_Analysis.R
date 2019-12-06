# rm(list=ls())
source('post_getReady.R')
source('functions.R')
fx <- function(x){
  x.yr=xts::apply.yearly(x, FUN=mean)
  r=MeshData2Raster(colMeans(x.yr) )
}

for(i in sid){
  prjname = paste0('sac', i)
  dir.out = file.path(dir.fig, prjname)
  dir.create(dir.out, showWarnings = F, recursive = T)
  message('\n\n', i,'/', nd, '\t', prjname)
  inpath = file.path(workdir, 'input', prjname)
  outpath = file.path(workdir, 'output', paste0(prjname, '.out') )
  pp=shud.env(prjname, inpath, outpath)
  ia=getArea(); AA=sum(ia)
  spr=readOGR(file.path(inpath, 'gis', 'river.shp'))
  graphics.off()
  gage.id = which(gage.sp@data$SAC == i)
  gage.uID = as.character(gage.sp@data$SOURCE_FEA)[gage.id]
  gage.uID
  qo = gage.rds[[gage.uID]]
  rivID = as.numeric(tab.gage$RiverID[i])
  
  rdsfile=file.path(dir.rds, paste0(prjname, '.RDS'))
  xl=readRDS(rdsfile)
   qs=xl$rivqdown[, rivID]/86400
  time(qs) = as.Date(time(qs))
  tt=go.time(time(qs))
  t1=tt[[1]]; t2 = tt[[2]]; t3=tt[[3]]; tx=c(t1,t2,t3)
  t23=c(t2,t3)
  p=xl$elevprcp %*% ia/AA; p=cbind('Precipitation'=p, qs)[, 1]
  tsd0=cbind('Precipitation'=p[tx], 
             'Simulation'=qs[tx],
             'Observation'=qo[tx])
  tsd=tsd0[t23,]
  tsm = apply.monthly(tsd, mean)
  tsw = apply.weekly(tsd, mean)
  p=hydroplot(tsd, t2, t3, heights = c(3,7))
  fn=file.path(dir.out, paste0(prjname,'_hydrograph_daily.png'))
  ggsave(p, filename = fn, width=7, height= 5, dpi=300)
  # hydroplot(tsm, t2, t3,  fn=file.path(dir.out, paste0(prjname,'_hydrograph_monthly.png')) )
  # pt = readRDS(file.path(dir.rds, paste0(prjname, '.PT_avg.RDS') ) )
  go.fdc(val = coredata(tsd[,-1]), col=c('blue','red'), fn=file.path(dir.out, paste0(prjname,'_fdc.png')))
  go.linefit(tsd[, c(3, 2)], tsw[,c(3,2)], fn=file.path(dir.out, paste0(prjname,'_Obs_Sim_LineFit.png')))

  ygw=xl$eleygw[t3, ]
  yus=xl$eleyunsat[t3,]
  rzs=MeshData2Raster()
  aqd=getAquiferDepth()
  rzb=rzs - mean(aqd)
  rgw=fx(ygw)
  zgw=rgw + rzb
  # writeRaster(rgw, file.path(pp$anapath, 'rgw.tif'), overwrite=TRUE)
  go.gw()
  stop()
}

