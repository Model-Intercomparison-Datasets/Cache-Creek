n=11
cdir='cmaes_best'
dir.create(cdir, showWarnings = FALSE, recursive = TRUE)
pjs=paste0('sac', 1:n)
dir.all = file.path(cdir, 'all')
dir.calib = file.path(cdir, 'calib')
dir.create(dir.all, showWarnings = FALSE, recursive = TRUE)
dir.create(dir.calib, showWarnings = FALSE, recursive = TRUE)
bmat = NULL
cmat = NULL
for(i in 1:n){
  xdir = file.path('cmaes_out', pjs[i])
  odir=file.path(cdir, pjs[i])
  dir.create(odir, showWarnings = FALSE, recursive = TRUE)
  fn.gof=file.path(xdir, 'gof.csv')
  mat=as.matrix(read.table(fn.gof, header = FALSE))
  id=order(mat[,3], decreasing = FALSE)[1:3]
  bmat = rbind(bmat, cbind(i, mat[id, ]))
  cmat = rbind(cmat, c(i, mat[id[1], ]))
  igen=mat[id, 1]
  ijob=mat[id, 2]
  fns=file.path(xdir, 'figure', paste0('Gen_', igen, '_Job_', ijob, '.gof.png'))
  lapply(fns, function(x) file.copy(from = x, to=file.path(odir), overwrite = TRUE) )
  file.copy(from = fns[1], to=file.path(dir.all, paste0(pjs[i], basename(fns[1])) ), overwrite = TRUE)
  fn.calib=file.path('cmaes_out', pjs[i], 
                     paste0('calib_Gen.', igen[1], '.calib'))
  file.copy(from = fn.calib, 
            to=file.path(dir.calib, paste0(pjs[i], '.cfg.calib') ), overwrite = TRUE)
}
write.table(bmat, file.path(cdir, 'best5.csv'), row.names = F, col.names = F, quote = F)
write.table(cmat, file.path(cdir, 'best.csv'), row.names = F, col.names = F, quote = F)
