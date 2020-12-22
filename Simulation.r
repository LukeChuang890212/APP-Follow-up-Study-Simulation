rm(list=ls())

# setwd("C:/Users/莊明儒/Desktop/Epidemiology/Follow-up.Simulation")
# source("Calculation.r")
source("Plotting.r")
source("test.r")

# essential formula:
# proportion of total not continue to be followed = p.mr+ltfur-p.mr*per.m.in.ltfus
# n*(1-p.mr*(1-per.m.in.ltfus)-avg.ltfur)^10 = n*(1-cir*(1-per.m.in.ltfus)-ltfur)
# avg.ltfur = 1-p.mr*(1-per.m.in.ltfus)-(1-cir*(1-per.m.in.ltfus)-ltfur)^(1/10)

sim = function(p.mr,ltfur,per.m.in.ltfus){
  N = 50:200 # no. of samples 
  n.sim = 1000 # no. of simulation (fixed)
  n.followyr = 10 # no. of years to follow up (fixed)
  
  p.mrs = c(p.mr) # mortality rate in pupulation c(0.02,0.1,0.4) 
  ltfurs = c(ltfur) # rate of loss to follow up (fixed)
  pers.m.in.ltfus = c(per.m.in.ltfus) # percentages of mortality that is mistaken for loss of follow-up (association of loss-to-follow-up with mortality)
                                      # conditional prob: P(loss|mortality)
  
  n.N = length(N) # no. of differnt sample sizes
  
  for(p.mr in p.mrs){
    # save.dir = paste("bias.x.emr~ss(",p.mr,')',sep='')
    # dir.create(save.dir,showWarnings = FALSE)
    
    cir = 1-exp((-p.mr)*n.followyr) # cir according to p.mr
    # print(paste("cir:",cir))
    
    par(mfrow=c(1,2))
    
    # biases = list() # store the biases for each ltfur (proportion of loss of follow up)
    for(ltfur in ltfurs){
      # avg.ltfur = 1-(1-ltfur)^(1/n.followyr) # average loss rate given a particular n.followyr
      
      reses = list() #save each res of different per.m.in.lfus for the plotting of bias~ss into reses list 
      
      for(per.m.in.ltfus in pers.m.in.ltfus){
        avg.ltfur = 1-p.mr*(1-per.m.in.ltfus)-(1-cir*(1-per.m.in.ltfus)-ltfur)^(1/10) # average loss rate
        
        res = list(m.mrs=rep(0,n.N),downlm.ci=rep(0,n.N),uplm.ci=rep(0,n.N),bias=rep(0,n.N))
        for(i in 1:n.N){ # i means index of sample size 
          n=N[i] # sample size
            
          mrs = rep(0,n.sim)
          
          # n.total.ltfuss = rep(0,n.sim)
          for(sim in 1:n.sim){ # sim means the (sim)th time of simulation
            mrs[sim] = cal.mr(n,n.followyr,p.mr,per.m.in.ltfus,avg.ltfur) 
            cal.mean.actual.params(sim)
          }
          
          #hist(mrs,breaks="Scott")
          #Sys.sleep(60)
          
          print(round(get.mean.actual.params(),3))
          
          res = cal.m.ci(i,res,mrs) # calculate the mean and ci for our estimation of mortality rate and store them into res list at index i
          res = cal.bias(i,p.mr,res,mrs) # calculate the bias for our estimation of mortality rate and store them into res list at index i
        }
        reses[[length(reses)+1]] = res
        cat(paste("Simulation for level of association =",per.m.in.ltfus,"under \nproportion of loss of follow-up =",ltfur,'&','population inccidence rate =',p.mr,"\nhas been done"))
        
        # plot(x=N,y=res$bias,type='l',xlab="Sample Size",ylab="Bias(rmse)",main=paste("Level of Association =",per.m.in.ltfus)) # plot bias~ss
        plot.emr.ss(N,p.mr,per.m.in.ltfus,res) # plot emr~ss
      }
      #title(paste("Proportion of Loss of Follow-up =",ltfur),outer=T)
      
      plot.bias(N,pers.m.in.ltfus,reses,ltfur)
      
      # biases[[length(biases)+1]] = reses[[length(reses)]]$bias
    }
    
    # par = (mfrow=c(1,1))
    # lcols = c("darkred","darkorange","darkgreen","darkblue","purple") # lines colors
    # for(ltfur in ltfurs){
    #   bias.plot.index = match(ltfur,ltfurs)
    #   if(bias.plot.index == 1){
    #     plot(x=N,y=biases[[bias.plot.index]],type='l',ylim=c(min(sapply(biases, min)),max(sapply(biases, max))),xlab="Sample Size",ylab="Bias",main=paste("Level of Association =",per.m.in.ltfus),col=lcols[bias.plot.index]) # plot bias~ss
    #   }else{
    #     lines(x=N,y=biases[[bias.plot.index]],lty=1,col=lcols[bias.plot.index])
    #     if(bias.plot.index == length(ltfurs)){
    #       legend("topright",legend=c(paste("proportion =",ltfurs)),lty=rep(1,length(ltfurs)),col=lcols)
    #     }
    #   }
    # }
    # 
  }
}
# 隨著母體參數越大，低估效果隨樣本數增加而變大的現象越明顯。
# 低估效果只在關聯程度為0.08或0.1的時候才有。隨關聯程度越高低估效果亦越明顯。










