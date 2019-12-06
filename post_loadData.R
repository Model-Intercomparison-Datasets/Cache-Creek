rm(list=ls())
source('post_getReady.R')

go.result <- function(){
  for(i in sid){
    prjname = paste0('sac', i)
    inpath = file.path(pihmout, 'input', prjname)
    outpath = file.path(pihmout, 'output', paste0(prjname, '.out') )
    pp=PIHM(prjname, inpath, outpath)
    message('\n\n', i,'/', nd, '\t', prjname)
    rdsfile=file.path(dir.rds, paste0(prjname, '.RDS'))
    if(!file.exists(rdsfile)){
      xl=BasicPlot(plot = FALSE, return = TRUE, rdsfile =rdsfile)
    }else{
      xl=readRDS(rdsfile)
    }
  }
}


go.forc <- function(){
  for(i in sid){
    prjname = paste0('sac', i)
    message('\n\n', i,'/', nd, '\t', prjname)
    inpath = file.path(pihmout, 'input', prjname)
    outpath = file.path(pihmout, 'output', paste0(prjname, '.out') )
    pp=PIHM(prjname, inpath, outpath)
    ia=getArea(); AA=sum(ia)
    fc=readforc.csv()
    Prcp = NULL
    Temp = NULL
    for(j in 1:length(fc)){
      Prcp = cbind(Prcp, as.xts(fc[[j]]$APCP))
      Temp = cbind(Temp, as.xts(fc[[j]]$TMP) )
    }
    colnames(Prcp) = names(fc)
    colnames(Temp) = names(fc)
    fn=file.path(dir.rds, paste0(prjname, '.PT.RDS'))
    saveRDS(list(Prcp, Temp), fn)
  }
}