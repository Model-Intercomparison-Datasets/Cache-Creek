

rm(list=ls())
lib=list('xts', 'raster', 'hydroTSM', 'PIHMgisR')
tmp=lapply(lib, library, character.only=T)

pj = 'sac'
inpath='../PIHM/input/sac'
outpath='../PIHM/output/sac.out/'
PIHM(prjname = pj, inpath = inpath, outpath = outpath)
ia=getArea()
aa=sum(ia)
att=readatt()
sp=sp.mesh2Shape(dbf = att)
sl=sp.riv2shp()
ka = att[, 'FORC']
plot(sp, col=ka )
plot(sl, add=T, col=2, lwd=3);plot(sl[35,], add=T, col=3, lwd=3);

readforc.fn()
forc=as.matrix(read.table('/Users/leleshu/Dropbox/PIHM/Projects/SAC/forcing/csv2000-2017/x29y110.csv', header = T, skip=1))
prcp = as.xts(forc[,'APCP'], order.by = as.POSIXct('2000-01-01') + forc[,1]*86400)
temp = as.xts(forc[,'TMP'], order.by = as.POSIXct('2000-01-01') + forc[,1]*86400)
pd = apply.daily(prcp, mean) * 24
td = apply.daily(temp, mean)
time(pd) = as.Date(time(pd))
time(td) = as.Date(time(td))

sid='11427750'
ol = readRDS('/Users/leleshu/Dropbox/PIHM/Projects/SAC/GIS/Gage/gageData.RDS')
qo=ol[[sid]]
time(qo) = as.Date(time(qo))
tx=time(pd)[1:730]
hp=cbind(pd[tx,], qo[tx,])
hydrograph(hp)

plot.zoo(hp)

stop()
for(i in 1:34 -1){
  tmp=read.table(paste0('../PIHM/output/sac.out',i,'/sac.ovs.csv'), header=T)
  matplot(type='l', tmp[-(1:600),-1], main=paste0('case', i+1))
  readline('anykey...')
}

