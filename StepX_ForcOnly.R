# write .tsd.forc file based on locations.
rm(list=ls())
clib=c('rgdal', 'rgeos', 'raster', 'sp', 'PIHMgisR')
x=lapply(clib, library, character.only=T)
nd=11
pjs = paste0('sac', 1:nd)
pihmout = '.'
dirs = file.path('/Users/leleshu/Dropbox/PIHM/Projects/SAC/GIS_SUB/Subs', pjs)
dirs

# years = 2000:2010
# nday = 365*length(years) +round(length(years)/4)
nd=11
# forcdir = '/home/llshu/csv2000-2017'
forcdir = '/Users/leleshu/Dropbox/PIHM/Projects/SAC/forcing/csv2000-2017'
for(i in 1:nd){
  # i=7
  cdir = dirs[i]
  prjname = basename(cdir)
  outdir = file.path(pihmout, 'input')
  forc.fns = readLines(file.path(cdir, paste0(prjname,'.forc.csv')))
  forc.fns = file.path(forcdir, basename(forc.fns))
  fn=file.path(outdir, prjname, paste0(prjname, '.tsd.forc'))
  message('\n\n', i,'/', nd, '\t', prjname)
  writeforc(forc.fns,file = fn, backup = FALSE)
  # forc.fns = file.path('/home/llshu/csv2000-2017', basename(forc.fns))
}

