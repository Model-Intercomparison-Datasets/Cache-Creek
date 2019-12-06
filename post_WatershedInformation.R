rm(list=ls())
source('post_getReady.R')

dir.sp = '/Users/leleshu/Dropbox/PIHM/Projects/SAC/GIS_SUB/Subs'
att=NULL
for(i in sid){
  prjname = paste0('sac', i)
  dir.out = file.path(dir.fig, prjname)
  dir.create(dir.out, showWarnings = F, recursive = T)
  
  inpath = file.path( 'input', prjname)
  outpath = file.path( 'output', paste0(prjname, '.out') )
  pp=PIHM(prjname, inpath, outpath)
  spr=readriv.sp()
  ModelInfo(spr = spr)
  rivID = as.numeric(tab.gage$RiverID[i])
  
  message('\n\n', i,'/', nd, '\t', prjname)
  inpath <- file.path('input', prjname)
  
  dir.create(inpath, showWarnings = F, recursive = T)
  
  indata = readRDS(file.path(dir.sp, prjname, paste0(prjname,'.RDS') ) )
  wbd=indata$wbd
  riv=indata$riv
  dem=indata$dem
  wbd0=gUnaryUnion(wbd)
  elv=mask(dem, wbd)
  rlc=mask(indata$rlc, wbd)
  pm=readmesh()
  
  spm=sp.mesh2Shape(pm, crs=crs(wbd))
  map.us=readRDS('../CONUS.RDS')
  map.us0=gUnaryUnion(map.us)
  map.state = map.us[map.us@data$STUSPS %in% 'CA', ]
  wbd.gcs= spTransform(wbd0, raster::crs(map.state))
  
  slope=raster::terrain(elv, opt='slope', unit='tangent')
  rslope=focal(slope, w=matrix(rep(1,25), 5, 5), fun=mean)
  
  # asp=raster::terrain(elv, opt='aspect', unit='degrees')
  zlim=round(cellStats(elv, range), -2)
  brk.contour = seq(zlim[1], zlim[2], 200)
  ext=extent(elv); dx=diff(ext[1:2]); dy=diff(ext[3:4])
  map.gage = crop(gage.sp, wbd)
  
  xatt=c(i, 'elev'=summary(elv), 'slope'=summary(rslope) )
  att=rbind(att, xatt)
         
  xlim=ext[1:2]+c(-1,1)*1000
  ylim=ext[3:4]+c(-1,1)*1000
  dx.scale= 10000;  #round(diff(xlim)/10, -3)
  
  p.wr <- function(){
    # plot(add=T, wbd0, lwd=2, border='darkred')
    plot(add=T, riv, lwd=1, col='darkblue')
    plot(map.gage, pch=18, add=T, col=2, cex=2)
  }
  xloc=c(0, .45); yloc=c(0, .45)
  locs = rbind(c(xloc+.0, yloc+.5),
               c(xloc+.5, yloc+.5),
               c(xloc+.0, yloc+.0),
               c(xloc+.5, yloc+.0)
  )
  
  xloc=c(0.8, 0.83)+.1; yloc=c(0.2, 0.8) # legend;
  
  go1 <- function(ilet){
    par(fig=locs[ilet,], new=T, mar=c(0,0,0,0))
    ext.state=extent(map.state)
    plot(map.state,  xlab='', ylab='', xlim=ext.state[1:2]+c(-1,1), ylim=ext.state[3:4]+c(-1,1));
    # grid();
    # mtext(side=3, line=-2, 'Pennsylvania, USA')
    points(coordinates(wbd.gcs), pch=3, cex=5, col=2)
    points(coordinates(wbd.gcs), pch=20, cex=1, col=2)
    text(coordinates(map.state), 'California, USA', cex=1)
    # text(coordinates(wbd.gcs)+1,  'PA', cex=1)
    mtext(side=3, line=-1, cex=1.0, font=2, paste0('(', letters[ilet], ') Location') );
    #USA MAP
    # ilet=ilet+1
    # par(fig=locs[ilet,], new=T, mar=c(0,0,0,0))
    xloc = c(0.4, 0.5)-.1; yloc=c(0.7,0.8)+.1
    par(fig=c(xloc, yloc), new=T, mar=c(0,0,0,0))
    plot(map.us0); text(coordinates(map.us0), 'USA'); 
    plot(map.state, add=T)
    points(coordinates(wbd.gcs), pch=17, cex=1, col=2)
    
    # dev.off(); stop()
  }
  go2 <- function(ilet){
    #====elevation++++++++++++
    col=colorspace::diverge_hcl(45)
    
    par(fig=locs[ilet,], new=T, mar=c(0,0,0,0))
    plot(elv, axes=F, box=F, legend=F, col=col, xlim=xlim, ylim=ylim)
    par(fig=locs[ilet,], new=T, mar=c(0,0,0,0))
    scalebar(dx.scale, divs=4, type='bar', cex=.8, 
             label = c(0, dx.scale/2, dx.scale)/ 1000, below='km' )
    par(fig=locs[ilet,], new=T, mar=c(0,0,0,0))
    p.wr()
    mtext(side=3, line=-1, cex=1.0, font=2, paste0('(', letters[ilet], ') Elevation') ); 
    # plot(elv, axes=F, box=F, legend=F, col=col);
    par(fig=locs[ilet,], new=T, mar=c(0,0,0,0))
    GISTools::north.arrow(xb=ext[1]+dx*.05, yb=ext[3]+dy*.38, 
                          len=dy /30, lab='N')
    
    # par(fig=locs[ilet,], new=T, mar=c(0,0,0,0))
    # contour(elv, add=T, col='gray20', nlevels=10);
    
    par(fig=locs[ilet,], new=T, mar=c(0,0,0,0))
    plot(elv, legend.only=TRUE, col=col, 
         smallplot=c(xloc, yloc+.0),
         legend.width=5, legend.shrink=.7, cex=5, horizontal=F,
         axis.args=list(col.axis='blue', lwd = 0,
                        font.axis=4, cex.axis=1.5,tck = 0, line=-.85, cex.axis=.8),
         legend.args=list(text='Elevation (m)',col=4, side=3, font=2, cex=1) )
  }
  go3 <- function(ilet){
    #====slope++++++++++++
    col=rev(colorspace::heat_hcl(10))
    par(fig=locs[ilet,], new=T, mar=c(0,0,0,0))
    plot(rslope, axes=F, box=F, legend=F, col=col, xlim=xlim, ylim=ylim);
    par(fig=locs[ilet,], new=T, mar=c(0,0,0,0))
    p.wr()
    mtext(side=3, line=-1, cex=1.0, font=2, paste0('(', letters[ilet], ') Slope') ); 
    par(fig=locs[ilet,], new=T, mar=c(0,0,0,0))
    plot(rslope, legend.only=TRUE, col=col, 
         smallplot=c(xloc, yloc+.0),
         legend.width=5, legend.shrink=.7, cex=5, horizontal=F,
         axis.args=list(col.axis='blue', lwd = 0,
                        font.axis=4, cex.axis=1.5,tck = 0, line=-.85, cex.axis=.8),
         legend.args=list(text='Slope (m/m)',col=4, side=3, font=2, cex=1) )
  }
  go4 <- function(ilet){
    #====MESH++++++++++++
    z=spm@data$Zmax; nz=length(z)
    col=(colorspace::diverge_hcl(length(z)))
    
    par(fig=locs[ilet,], new=T, mar=c(0,0,0,0))
    plot(wbd, border='transparent', xlim=xlim, ylim=ylim)
    plot(spm[order(z), ], col=col, add=T)
    plot(add=T, riv, lwd=3, col=rgb(.7, .7, .7, .5))
    p.wr()
    mtext(side=3, line=-1, cex=1.0, font=2, paste0('(', letters[ilet], ') Unstractured domain') ); 
    
    plot(elv, legend.only=TRUE, col=col,
         smallplot=c(xloc, yloc),
         legend.width=5, legend.shrink=.7, cex=5, horizontal=F,
         axis.args=list(col.axis='blue', lwd = 0,
                        font.axis=4, cex.axis=1.5,tck = 0, line=-.85, cex.axis=.8),
         legend.args=list(text='Elevation (m)',col=4, side=3, font=2, cex=1) )
  }
  
  #==================================
  # PLOT 
  #==================================
  png.control(paste0(prjname, '_map.png'), path=dir.out, wd=9, ht=7)
  # par(mar=rep(0,4), mfrow=c(2,2))
  par(mar=c(1,1,1,2), mfrow=c(1,1))
  plot.new()
  go1(1);
  go2(2);
  go3(3);
  go4(4)
  dev.off()
  # stop()
}
print(att)
write.table(att, file='dem_att.csv')
