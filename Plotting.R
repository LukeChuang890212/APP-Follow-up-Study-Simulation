plot.emr.ss = function(N,p.mr,per.m.in.ltfus,res,ltfur){
  bp = barplot(height=res$m.mrs,names.arg=N,xlab="Sample Size",ylab="Estimate",ylim=c(0.0,max(res$uplm.ci)),main=paste("Level of Association =",per.m.in.ltfus))
  lines(bp,res$downlm.ci,lty=2)
  lines(bp,res$uplm.ci,lty=2)
  abline(h=p.mr,lwd=2)
  return(bp)
}

plot.bias = function(N,pers.m.in.ltfus,reses,ltfur){
  # par(mfrow=c(1,1))
  p = NULL
  for(i in 1:length(pers.m.in.ltfus)){ # i means index of per.m.in.ltfus
    per.m.in.ltfus = pers.m.in.ltfus[i]
    p = plot(x=N,y=reses[[i]]$bias,type='l',xlab="Sample Size",ylab="Bias",main=paste("Level of Association =",per.m.in.ltfus)) # plot bias~ss
    abline(h=0.0,lwd=2)
  }
  return(p)
  #title(paste("Proportion of Loss of Follow-up =",ltfur),outer=T)
}