# rm(list=ls())
source('post_getReady.R')
source('functions.R')
fx <- function(x){
  x.yr=xts::apply.yearly(x, FUN=mean)
  r=MeshData2Raster(colMeans(x.yr[-1:-2, ]) )
}

att=data.frame('ID'=sid, 'Area'=0, 'Amean'=0,'P'=0, 'T'=0, 
               'Ncell'=0, 'Nriv'=0,
               'LenRiv'=0, 'rivorder'=0)
for(ix in 1:length(sid)){
  i = sid[ix]
  prjname = paste0('sac', i)
  dir.out = file.path(dir.fig, prjname)
  dir.create(dir.out, showWarnings = F, recursive = T)
  message('\n\n', i,'/', nd, '\t', prjname)
  inpath = file.path(pihmout, 'input', prjname)
  outpath = file.path(pihmout, 'output', paste0(prjname, '.out') )
  pp=PIHM(prjname, inpath, outpath)
  pm=readmesh()
  pr=readriv()
  spm=sp.mesh2Shape(pm)
  spr=readOGR(file.path(inpath, 'gis', 'river.shp'))
  rlen= gLength(spr)
  ncell=nrow(pm@mesh)
  nriv=nrow(pr@river)
  ia=getArea() / 1e6; AA=sum(ia)
  
  p= ggplot(data.frame(ia), aes(x=ia))+
    geom_histogram(aes(y=..density..),binwidth=10000)+
    geom_density(alpha=.2, fill="#FF6666") 
  ggsave(file.path(dir.out, paste0(prjname, '_ia.png')), p)
  fn=file.path(dir.rds, paste0(prjname, '.PT.RDS'))
  pt=readRDS(fn)
  nc=ncol(pt[[1]])
  
  pd = apply.yearly(pt[[1]], sum, na.rm=TRUE )/nc/24
  pd=pd[-length(pd)]
  py = mean(pd[-length(pd)])
  
  summary(coredata(pd))
  ty=mean(pt[[2]])
  ty
  
  att[ix, ]=c(i, AA, mean(ia), py, ty, ncell, nriv, rlen, max(pr@river$Type) )
}
print(att)
write.table(att, file = 'Information.csv')