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
   
  rdsfile=file.path(dir.rds, paste0(prjname, '.RDS'))
  xl=readRDS(rdsfile)
  # debug(wb.all)
  wb=wb.all(xl = xl, plot = F)
  time(wb)=as.Date(time(wb))
  tx=time(xl[[1]])
  tx=as.Date(tx)
  tt=go.time(tx)
  t1=tt[[1]]; t2 = tt[[2]]; t3=tt[[3]]; tx=c(t1,t2,t3)
  t23=c(t2,t3)
  wb.s=apply(wb[t23, ], 2, sum)
  rbind(wb.s, wb.s/wb.s[2] * 100)
  head(wb)
  
  xx=wb[t23,1:5]
  p=go.wb(xx, ylabs = c(bquote('Change('~mm~d^{-1}~')'), 
                        bquote('Flux ('~ mm~d^{-1}~')')),
          labs = c('Precipitation', 'Discharge', 'Actual ET', 'Potential ET'), 
          heights = c(3,8) )
  ggsave(file.path(dir.out, paste0(prjname, '_wb.png')), p,
         width=7, height=6, dpi=300)
  stop()
}

