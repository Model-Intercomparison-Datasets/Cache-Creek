# 
# Obj.plot <- function(jobid=0, iGen=0,
#                      inpath, outpath, 
#                      CV, vlist,  outdir = CV$method$PATH_OUT){
#   pngout = file.path(outdir, 'figure')
#   rdsout = file.path(outdir, 'RDS')
#   dir.create(pngout, recursive = T, showWarnings = F)
#   dir.create(rdsout, recursive = T, showWarnings = F)
#   
#   fn.prefix = paste0('GW', '_Gen',iGen, '_Ch', jobid)
#   fn=file.path(rdsout,  paste0(fn.prefix, '.RDS' ) )
#   # dat=readRDS(fn)
#   # nt=ncol(dat$sim)
#   oid=getOutlets()
#   readout(path = outpath, keyword = 'rivqdown')
#   fn=file.path(pngout, fn.prefix )
#   xdf=data.frame(dat$sim)
#   md= reshape2::melt(xdf, id=c('x') )
#   p <-ggplot(data=md, aes(x=x, y=value)) +
#     xlab('X (m) ') + ylab('GWL')+ xlim(0, 3) + ylim(0, 2)+
#     theme_gray()+ theme_linedraw()+
#     geom_line(aes(color=variable) )  +
#     scale_colour_manual(values=heat.colors(5) ) +
#     geom_point(data=dat$obs, aes(x=x, y=z, shape=col) )  +
#     labs(shape="Vauclin (1979)", colour="Simulations")
#   # print(p)
#   fn=file.path(pngout, fn.prefix )
#   ggsave(p, filename=paste0(fn, '_plot.png'), width=10, height=5)
#   
#   xx=dat$vs
#   # p= ggplot(data=data.frame(xx), aes(x=obs, y=sim))+geom_point()+
#   #   geom_smooth(method = 'lm')
#   # tmp=file.path(pngout, 'OS'); dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
#   # ggsave(p, filename=file.path(tmp, paste0(fn.prefix, '_OS.png')), width=10, height=5)
#   
#   obs=xx[,2]; sim=xx[,3]
#   png.control(fn=paste0(fn.prefix, '_gof.png'), wd=8, ht=5, path=file.path(pngout, 'GOF') )
#   hydroGOF::ggof(obs=obs, sim=sim)
#   dev.off()
# }
Obj.Func <- function(jobid=0, iGen=0, oid,
                     inpath, outpath, 
                     CV, vlist,  outdir = CV$method$PATH_OUT){
  msg = paste0('Obj.Func(Gen:', iGen, 'Job:', jobid, ')::')
  # library(ggplot2)
  inpath=as.character(inpath)
  outpath=as.character(outpath)
  # tmp=PIHM(prjname = CV$prjname, outpath = outpath, inpath = inpath)
  pngout = file.path(outdir, 'figure')
  rdsout = file.path(outdir, 'RDS')
  dir.create(pngout, recursive = T, showWarnings = F)
  dir.create(rdsout, recursive = T, showWarnings = F)
  gof.v=gof(rnorm(100), rnorm(100)+1000)
  
  message(msg, 'OID = ', oid)
  theFile = file.path(outpath, paste0(CV$prjname, '.rivqdown.dat'))
  message(msg, "Reading File: ", theFile)
  if(file.exists(theFile)){
    f.info = file.info(theFile)
    if(f.info$size>0){
      x.obs=CV$obs
      tmp=readout(file=theFile)[, oid]; 
      time(tmp)=as.Date(time(tmp))
      x.sim=tmp[-1 * (1:CV$method$NSPINGUP), ]
      t1=time(x.obs); t2=time(x.sim)
      ct=t1[t1 %in% t2]
    }else{
      message(msg, '*******Empty File: ', theFile, '*******')
      message(msg, '*************************************************')
      return(gof.v)
    }
  }else{
    message(msg, '*******File missing:', theFile, '*******')
    message(msg, '*************************************************')
    return(gof.v)
  }
  if(length(x.sim)< CV$minLength  | length(ct) < CV$minLength ){
    message(msg, '*******Length of simulations: ', length(x.sim), '*******')
    message(msg, '*************************************************')
    return(gof.v)
  }else{
    fn=paste0('Gen_', iGen, '_Job_', jobid, '.gof.png')
    obs = x.obs[ct,]
    sim=x.sim[ct,]
    message(msg, 'Plot GOF figure ... ')
    png(filename = file.path(pngout, fn), type='cairo',
        width = 7, height=5, units = 'in', res=100)
    hydroGOF::ggof(sim = sim, obs=obs)
    dev.off()
    gof.v = hydroGOF::gof(sim = sim, obs=obs)
    message(msg, 'GOF:')
    print(gof.v)
    gof.v
  }
  return(gof.v)
} #Obj.Func 

Call_Model <- function(iGen, pop, ncores, CV, CMD.EXE, objfunc, 
                       lambda=CV$method$LAMBDA,
                       debug=FALSE, 
                       ...){
  tab.gage = read.csv('CalibFiles/gageid2019.10.csv', sep=',')
  SUBID=as.numeric(substr(CV$prjname, 4,nchar(CV$prjname)) )
  oid = tab.gage$RiverID[SUBID]
  msg = paste0('Call_Model(Gen:', iGen, ')::')
  message(msg, 'OID = ', oid)
  message(msg, 'rivID = ', CV$rivID)
  # debug(pre.files)
  vlist = pre.files(iGen = iGen, pop = pop, CV=CV);
  arfitness = numeric(lambda)
  if(ncores > 1){
    # cl = parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cores = ncores)
    tmp = foreach::foreach(i = 1:lambda) %dopar% {
      message(msg, 'Gen:', iGen, '\t Child:', i, '/', lambda, '\t', vlist$att$dir.out[i])
      if(!debug){
        EXEC(CV, CMD.EXE=CMD.EXE, calibfile = vlist$att[i, 'fn.calib'],
             outpath = vlist$att[i, 'dir.out'], fn.log = vlist$att[i, 'fn.log'] )
      }
      message(msg, 'Model finished. ', iGen, '-', i, '\n')
      gof.v= objfunc(jobid=i, iGen=iGen, CV=CV, oid=oid, 
                    inpath = vlist$att[i, 'dir.in'],
                    outpath = vlist$att[i, 'dir.out'])
      message(msg, 'GOF ', iGen, '-', i, ':')
      print(as.numeric(gof.v))
      gof.v
    } # END of foreach parallel.
    gof.mat = do.call(cbind, tmp)
  }else{
    gof.mat = NULL
    for(i in 1:lambda){
      message(msg, 'Gen:', iGen, '\t Child:', i, '/', lambda, '\t', vlist$att$dir.out[i])
      if(!debug){
        EXEC(CV, CMD.EXE=CMD.EXE, calibfile = vlist$att[i, 'fn.calib'],
             outpath = vlist$att[i, 'dir.out'], fn.log = vlist$att[i, 'fn.log'] )
      }
      message(msg, 'Model finished. ', iGen, '-', i)
      gof.v= objfunc(jobid=i, iGen=iGen, CV=CV,oid=oid,
                    inpath = vlist$att[i, 'dir.in'],
                    outpath = vlist$att[i, 'dir.out'])
      message(msg, 'GOF ', iGen, '-', i, ':')
      print(as.numeric(gof.v))
      gof.v
      gof.mat = cbind(gof.mat, gof.v)
    } # END of for. Serials
  }
  colnames(gof.mat) = paste0('X', 1:lambda)
  write.table(gof.mat, file=file.path(CV$method$PATH_OUT, 
                                      paste0('gof_G', iGen, '.csv')),
              quote = FALSE, col.names = TRUE, row.names = TRUE)
  message(msg, 'GOF matrix:')
  print(gof.mat)
  
  gof.nse = gof.mat['NSE',]
  message(msg, 'NSE:')
  print(gof.nse)
  ret <- list('CV'=CV,'varlist'=vlist, 'fitness'=gof.nse)
}
# sol1 <- CMAES(CV=CV, cmd='./pihm++', objfunc=Obj.Func,  Call_Model=Call_Model)

# for(i in 1:12){
#   message(i, '/', 12)
#   Obj.Func(jobid=i, iGen=1, inpath='input/sac6',
#            outpath=paste0('output/sac6.out/sac6_', i),
#            CV=CV, outdir = CV$method$PATH_OUT)
# }
