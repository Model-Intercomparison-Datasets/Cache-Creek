rm(list=ls())
source('post_getReady.R')

for(i in 5){
  prjname = paste0('sac', i)
  dir.out = file.path(dir.fig, prjname)
  dir.create(dir.out, showWarnings = F, recursive = T)
  message('\n\n', i,'/', nd, '\t', prjname)
  inpath = file.path(workdir, 'input', prjname)
  outpath = file.path(workdir, 'output', paste0(prjname, '.out') )
  pp=shud.env(prjname, inpath, outpath)
  ia=getArea(); AA=sum(ia)
  
  fn=file.path(dir.rds, paste0(prjname, '.PT.RDS'))
  pt=readRDS(fn)
  nc=ncol(pt[[1]])
  pd = apply.monthly(apply.daily(apply.daily(pt[[1]], mean, na.rm=TRUE), sum)/nc, sum)
  td.max = apply.monthly(apply.daily(pt[[2]], max, na.rm=TRUE), max)
  td.min = apply.monthly(apply.daily(pt[[2]], min, na.rm=TRUE), min)
  td.mean = apply.monthly(apply.daily(apply.daily(pt[[2]], mean, na.rm=TRUE), sum)/nc, mean)
  
  tsd=cbind(pd, td.min, td.mean, td.max)
  colnames(tsd)=c('P', 'Tmin', 'Tmean', 'Tmax')
  time(tsd)=as.Date(time(tsd))
  saveRDS(tsd, file.path(dir.rds, paste0(prjname, '.PT_avg.RDS') ) )
  cfactor=50
  head(tsd)
  # x=apply.yearly(tsd, FUN=mean)
  # x$P=x$P * 12
  # x
  # ggplot(data=tsd)+ ylab('Monthly Tempearature (C)')+xlab('Time')+
  #   geom_col(aes(x=time, y=P*cfactor), group = 1, color='Blue')
  p=ggplot(data=tsd)+ 
    ylab('Monthly Tempearature (C)')+xlab('Time')+
    geom_col(aes(x=Index, y=P*cfactor, group=1,  fill='P')) +
    geom_line(aes(x=Index, y=Tmean,group = 1, color="T")) +
    geom_ribbon(aes(x = Index, ymax = Tmax, ymin = Tmin, fill = 'R'),
                alpha = 0.4)+
    scale_color_manual(values=c('red'), lab='Mean temperature', name='')+
    scale_fill_manual(values=c('blue', 'skyblue'), lab=c('Precipitation', 'Temperature range'),
                      name='')+
    theme(legend.position = 'top', legend.direction = 'horizontal')+
    scale_y_continuous(
      sec.axis = sec_axis(~./cfactor,
                          name = bquote('Precipitation (' ~ m ~ month^{-1} ~ ')' ) ) )
  p
  
  ggsave(p, filename = file.path(dir.out, paste0(prjname, '_PT.png')),
         height = 4, width=6)
}