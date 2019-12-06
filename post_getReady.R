clib=c('rgdal', 'rgeos', 'raster', 'sp', 'SHUDtoolbox', 'xts',
       'ggplot2', 'reshape2','gridExtra')
x=lapply(clib, library, character.only=T)
nd=11
pjs = paste0('sac', 1:nd)
workdir = './'
dir.rds='RDS'
dir.fig = 'Fig'
dir.create(dir.rds, showWarnings = F, recursive = T)
dir.create(dir.fig, showWarnings = F, recursive = T)
# dirs = file.path('/Users/leleshu/Dropbox/PIHM/Projects/SAC/GIS_SUB/Subs', pjs)
# dirs
sid = c(5, 7, 8, 9)


col=c('darkblue', 'blue', 'red')


gage.rds=readRDS('/Users/leleshu/Dropbox/PIHM/Projects/SAC/GIS/Gage/Gagesplot.RDS')
# gage.sp = readOGR('/Users/leleshu/Dropbox/PIHM/Projects/SAC/GIS_SUB/gisData/gages.shp')
# saveRDS(gage.sp, file='/Users/leleshu/Dropbox/PIHM/Projects/SAC/GIS_SUB/gisData/gages.RDS')
gage.sp = readRDS('/Users/leleshu/Dropbox/PIHM/Projects/SAC/GIS_SUB/gisData/gages.RDS')
tab.gage = read.csv('CalibFiles/gageid2019.10.csv', sep=',')

