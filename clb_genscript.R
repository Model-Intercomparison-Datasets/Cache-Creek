n=11
cdir='CalibFiles'
dir.create(cdir, showWarnings = FALSE, recursive = TRUE)
fns=file.path(cdir, paste0('Task', 1:n, '.job') )
pjs=paste0('sac', 1:n)

for(i in 1:n){
  message(i, '/',n, '\t', fns[i])
  str=character()
  str[1] = '#!/bin/bash -l'
  str[2] = paste('#SBATCH -J ', paste0( pjs[i]) )
  str[3] = paste('#SBATCH -o ', paste0('Job_', pjs[i], '.out') )
  str[4] = paste('#SBATCH -e ', paste0('Job_', pjs[i], '.out') )
  str[5] = paste('Rscript J_CalibTemplate.R', i)
  write.table(str, fns[i], quote=FALSE, 
        row.name=FALSE, col.names=FALSE)
}

fn='Sub.sh'
cmd=paste('sbatch -t 7200 -N 3 -n 72 ',fns)
write.table(cmd, file=fn,  quote=FALSE, 
            row.name=FALSE, col.names=FALSE)
system(paste('chmod +x ', fn) )
message('Finished. ')
