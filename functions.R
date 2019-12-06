go.time <- function(tt, 
                    d1=as.Date('2001-07-01'),
                    d2=as.Date('2003-07-01'),
                    d3=as.Date('2007-07-01')){
  # d1=as.Date('2001-07-01')
  # d2=as.Date('2003-07-01')
  # d3=as.Date('2006-07-01')
  t1 = tt[tt < d1]
  t2 = tt[ tt >= d1 & tt < d2]
  t3 = tt[ tt >= d2 & tt < d3]
  range(t1)
  range(t2)
  range(t3)
  list(t1,t2,t3)
}

go.gof <- function(x){
  sim=x[,1]
  obs=x[,2]
  y=hydroGOF::gof(sim=sim, obs=obs)
  cn=c('NSE', 'KGE', 'R2')
  r=y[cn, ]
}

hydroplot <- function(tsd,  t2, t3, fn,  
                      heights = c(3, 7),
                      col=c('blue','blue', 'red')){

  md1=data.frame('Time'=time(tsd), 'Precipitation'=tsd[,1]*1000)
  
  g.top = ggplot(md1) + xlab('')+ ylab(bquote('P ('~ mm~d^{-1}~')') )+
    geom_col(aes(x=Time, y=Precipitation), color=col[1])+
    theme(axis.title.x=ggplot2::element_blank(),
          axis.text.x=ggplot2::element_blank(),
          axis.ticks.x=ggplot2::element_blank()) 
  
  md2= reshape2::melt(data.frame('Time'=time(tsd), tsd[,-1]), id='Time')
  head(md2)
  colnames(md2)=c('Time', 'Discharge', 'value')
  tab=data.frame('Calibration'=go.gof(tsd[t2,-1]),
                 'Validation'=go.gof(tsd[t3,-1]) )
  
  g.bottom<-ggplot(data=md2, aes(x = Time, y=value, alpha=.7,
                                 color=Discharge, linetype=Discharge))+ 
    ylab( bquote('Discharge ('~ m^3 ~ s^{-1}~ ')') )+ xlab('')+
    geom_line(alpha=.55)+ 
    scale_color_manual(values=col[2:3])+
    theme(legend.position=c(0.88, 0.9), #'bottom',
          # legend.direction = 'horizontal',
          legend.title = element_blank())
  ta=c(t2,t3)
  g.bottom<-g.bottom+
    geom_vline(aes(xintercept=max(t2) ), size=1.2,
               linetype=4, colour="gray")+
    annotation_custom(tableGrob(tab, theme = ttheme_minimal(base_size=10)), 
                      xmin=as.Date('2002-01-15'), 
                      xmax=as.Date('2004-05-15'), 
                      ymin=max(tsd[,2])+10, ymax=max(tsd[,2])+10 )
  # g.bottom
  gA <- ggplot2::ggplotGrob(g.top)
  gB <- ggplot2::ggplotGrob(g.bottom)
  maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
  gA$widths[2:5] <- as.list(maxWidth)
  gB$widths[2:5] <- as.list(maxWidth)
  p <- gridExtra::grid.arrange(gA, gB, ncol=1,heights = heights )
}
# hydroplot(tsd, t2, t3, fn=file.path(dir.out, paste0(prjname,'_hydrograph_daily.png')) )


go.fdc <- function(val, col,
                   xlab='Exceedance (%)',
                   ylab='Discharge (m3/d)', fn){
  p=PIHMgisR::fdc(val, xlab=xlab, ylab=ylab)+
    scale_x_log10()+
    scale_y_log10()+
    scale_color_manual(values=col)+
    theme(legend.position=c(.8, .9),
          legend.direction = 'horizontal',
          legend.title = element_blank())
  p
  ggsave(fn, p)
}
# go.fdc(val = coredata(tsd[,-1]), col=c('blue','red'), fn=file.path(dir.fig, paste0(prjname,'.fdc.png')))

go.linefit <- function(tsd, tsm, fn){
  # library(ggpmisc)
  formula = y ~ x
  p1=LineFit(tsd)+
    ggpmisc::stat_poly_eq(aes(label = paste0("atop(", ..eq.label.., ",", ..rr.label.., ")")), 
                          formula = formula,  color='blue',
                          parse = TRUE)  +
    ggtitle(' (A) Daily')
  p2=LineFit(tsm)+
    ggpmisc::stat_poly_eq(aes(label = paste0("atop(", ..eq.label.., ",", ..rr.label.., ")")), 
                          formula = formula, color='blue',
                          parse = TRUE) +
    ggtitle(' (B) Weekly')
  g=gridExtra::arrangeGrob(grobs=list(p1,p2), nrow=1, ncol=2, main='')
  ggsave(fn, g, height = 4, width = 7)
}

go.gw <- function(){
  rgw[rgw<0]=0
  dgw=30-rgw
  dgw[dgw < 0]=0
  x=0:100/100
  y=0.3 + .01 * x
  xy=cbind(x,y)
  nc=nrow(xy)
  x.z=extractRaster(rzs, xy=xy, plot=F)
  x.gw=extractRaster(rgw, xy=xy, plot = F)
  lxy=x.z[, 1:2]
  go.sgw <- function(){
    xx=c(0, cumsum(unlist(lapply(2:nc, function(i, xy) Eudist(xy[i, ], xy[i-1, ]),
                                 xy=x.z[, 1:2])) ) )  *1e-3
    md0 = data.frame('x'=xx,
                     'max'=x.z[,3],
                     'value' =x.gw[,3] + x.z[,3] - 30,
                     'min' = x.z[,3]-30,
                     'name' = 'Aquifer'
    )
    md=reshape2::melt(md0[, 1:4], id=1)
    colnames(md)=c('x','Layer', 'value')
    md$Layer=c(rep('Surface', nc), rep('Groundwater Table', nc),
               rep('Bedrock', nc))
    ylim=range(md$value, na.rm = TRUE) + c(-500, 0)
    sgw=x.gw[,3]
    sgw[sgw<0]=0
    ff=10
    md1=data.frame('x'=xx,
                   'max'=ylim[1]+sgw*ff,
                   'value'=x.gw[,3], 
                   'min'=ylim[1], 
                   'name'='Groundwater storage')
    tmp = md0
    col=c('blue', 'darkgreen')
    tmp$value[tmp$value <= tmp$min]=NA
    mdx= rbind(md0, md1)
    tmp$name='Groundwater level'
    p=ggplot(data=mdx)+ xlab('Distance (km)') + ylab('Elevation (m)')+
      ylim(ylim)+
      geom_ribbon( aes(x=x,  ymax=max,ymin=min, fill=name) )+
      geom_line(data=tmp, aes(x=x, y=value, color=name))+
      theme_bw()+
      scale_y_continuous(sec.axis = sec_axis( ~. /ff -ylim[1]/ff, name = "Groundwater storage (m)", breaks = seq(0, 50, 10)))+
      theme(legend.position = c(0.75, 0.85), 
            legend.direction = 'horizontal',
            legend.title = element_blank())+
      scale_fill_manual(values = c('gray80', 'skyblue', 'blue'))+
      scale_color_manual(values = c('blue'));p
    
    p = p + ggplot2::ggtitle('(b)')+
      theme(plot.title = element_text(hjust = 0.5)); p
    
    # p=ggplot()+ xlab('Distance (km)') + ylab('Elevation (m)')+
    #   ylim(ylim)+
    #   geom_ribbon(data=md1,  aes(x=x,  ymax=ymax,ymin=ymin),color=col[2] , 
    #               fill=col[2] )+
    #   geom_ribbon(data=md0,  aes(x=x,  
    #                              ymax=Surface,
    #                              ymin=Bedrock),
    #               color='gray50', fill='gray80')+
    #   geom_line(data=tmp, aes(x=x, y=GWT), color=col[1] )+
    #   theme_bw()+
    #   theme(legend.position = 'top', 
    #         legend.direction = 'horizontal')+
    #   theme( axis.line.y.right = element_line(color = col[2] ),
    #          axis.ticks.y.right = element_line(color = col[2] ))+
    #   scale_y_continuous(sec.axis = sec_axis( ~. /ff -ylim[1]/ff, 
    #                                           name = "Groundwater storage (m)", 
    #                                           breaks = seq(0, 50, 10)))
    ggsave(file.path(dir.out, paste0(prjname, '_sgw.png')), p,
           height = 4, width = 7)
  }
  # go.sgw()
  
  go.rgw <- function(dgw){
    col=c('darkgreen', 'darkred')
    cn=c( 'Cross-section', 'River')
    lty=c(2, 1)
    png.control(fn=paste0(prjname,'_rgw.png'), path=dir.out, 
                wd=7, ht=5)
    xlim=range(round(lxy[,1]), -3)
    ax = seq(xlim[1], xlim[2], 5000)
    labs=ax-min(ax)
    brks=seq(0, 30, 3)
    cols=colorspace::sequential_hcl(n=length(brks)+1)
    par(mar=c(3, 3, 0.2,0)+.0)
    plot(dgw, breaks=brks, col=cols, axes=F, box=F, legend=F)
    lines(lxy, lty=lty[1], col=col[1], lwd=2)
    plot(add=T, spr, col=col[2], lwd=2, lty=lty[2])
    legend('topright', cn,
           lty=lty, col=col, lwd=2)
    axis(side=1, at=ax, labels = labs/1000)
    mtext(side=1, 'x (km)', line=2)
    mtext(side=3, '(a)', line=-1)
    plot(dgw, legend.only=TRUE, breaks=brks, col=cols,
         # smallplot=c(c(0.0, .05)+.8, c(0.0, .6)+.25 ),
         smallplot=c(c(0.0, .35)+.56, c(0.0, .03)+.75 ),
         horizontal=T,
         # legend.width=5, legend.shrink=.7, cex=5,
         axis.args=list(col.axis='blue', lwd = 0,
                        font.axis=1, cex.axis=1,tck = 0, line=-.85,
                        at=brks, 
                        cex.axis=.8),
         legend.args=list(text='Depth of Groundwater Table (m)',
                          col=4, side=3, font=2, cex=1.)
    )
    dev.off()
  }
  go.sgw()
  go.rgw(dgw)
}
# go.gw()


go.wb <- function(x, legend.position='bottom', unit = rep('', ncol(x)),
                      heights = c(4,7), bg=TRUE,
                       ylabs=NULL, labs=colnames(x)[-1]
){
  zoo::index(x) = as.POSIXct(time(x) )
  Time = NULL
  rain = NULL
  varialbe = NULL
  cn=colnames(x)
  pv = as.numeric(x[,1])
  sv = rep(1,length(pv))
  sv[pv<0]=2
  dfp = data.frame('Time' = time(x), 'rain' = pv )
  # head(dfp)
  
  dfqq =  data.frame('Time' = time(x), x[,-1] )
  dfq = reshape2::melt(dfqq, id='Time')
  
  plim = range(pv, na.rm = TRUE)
  g.top <- ggplot2::ggplot()
  g.top <- g.top +
    ggplot2::coord_cartesian(ylim = plim ) +
    ggplot2::guides(fill = "none") +
    ggplot2::geom_col(data=dfp,ggplot2::aes_string(x = 'Time', y = 'rain'), fill=3) +
    ggplot2::scale_y_continuous(trans = "reverse") +
    ggplot2::theme(axis.title.x=ggplot2::element_blank(),
                   axis.text.x=ggplot2::element_blank(),
                   axis.ticks.x=ggplot2::element_blank()) +
    ggplot2::labs(y = paste( cn[1], unit[1]))
  
  plim = range(x[,-1], na.rm = TRUE)
  
  g.bottom <- ggplot2::ggplot()
  g.bottom <- g.bottom+
    ggplot2::coord_cartesian(ylim = plim ) +
    ggplot2::guides(fill = "none") +
    ggplot2::geom_line(data=dfq, ggplot2::aes_string(x = 'Time', y = 'value', 
                                                     linetype = 'variable' , color = 'variable')) +
    ggplot2::theme() + 
    scale_color_discrete(labels=labs) +
    scale_linetype_discrete(labels=labs)
  
  if(!is.null(ylabs)){
    if(is.list(ylabs)){
      g.top <- g.top + ggplot2::ylab(ylabs[[1]])
      g.bottom <- g.bottom + ggplot2::ylab(ylabs[[2]])
    }else{
      g.top <- g.top + ggplot2::ylab(ylabs[1])
      g.bottom <- g.bottom + ggplot2::ylab(ylabs[2])
    }
  }else{
  }
  if(ncol(x)>2){
    g.bottom  <- g.bottom +
      ggplot2::theme(legend.position=legend.position,
                     legend.direction = 'horizontal',
                     legend.title = ggplot2::element_blank())
  }else{
    g.bottom  <- g.bottom +
      ggplot2::theme(legend.position='none')
  }
  
  gA <- ggplot2::ggplotGrob(g.top)
  gB <- ggplot2::ggplotGrob(g.bottom)
  maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
  gA$widths[2:5] <- as.list(maxWidth)
  gB$widths[2:5] <- as.list(maxWidth)
  p <- gridExtra::grid.arrange(gA, gB, ncol=1,heights = heights )
}
