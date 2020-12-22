rm(list=ls())

# setwd("C:/Users/莊明儒/Desktop/Epidemiology/Follow-up.Simulation")
source("Functions.r")

# essential formula:
# proportion of total not continue to be followed = avg.p.mr+ltfur-avg.p.mr*per.m.in.ltfus
# n*(1-p.mr*(1-per.m.in.ltfus)-avg.ltfur)^10 = n*(1-cir*(1-per.m.in.ltfus)-ltfur)
# avg.ltfur = 1-p.mr*(1-per.m.in.ltfus)-(1-cir*(1-per.m.in.ltfus)-ltfur)^(1/10)

sim = function(p.mr,ltfur,per.m.in.ltfus){
  N = 50:200 # no. of samples 
  
  p.mrs = c(p.mr) # mortality rate in pupulation c(0.02,0.1,0.4) 
  pers.m.in.ltfus = c(per.m.in.ltfus) # percentages of mortality that is mistaken for loss of follow-up (association of loss-to-follow-up with mortality)
  # conditional prob: P(loss|mortality)
  n.sim = 1000 # no. of simulation (fixed)
  ltfurs = c(ltfur) # rate of loss to follow up (fixed)
  n.followyr = 10 # no. of years to follow up (fixed)
  
  n.N = length(N) # no. of differnt sample sizes
  
  for(p.mr in p.mrs){
    # save.dir = paste("bias.x.emr~ss(",p.mr,')',sep='')
    # dir.create(save.dir,showWarnings = FALSE)
    
    cir = 1-exp((-p.mr)*n.followyr) # cir according to p.mr
    # avg.p.mr = 1-(1-cir)^(1/n.followyr) # average population mortality rate given a particular n.followyr
    
    par(mfrow=c(1,2))
    
    # biases = list() # store the biases for each ltfur (proportion of loss of follow up)
    for(ltfur in ltfurs){
      #avg.ltfur = 1-(1-ltfur)^(1/n.followyr) # average loss rate given a particular n.followyr
      
      reses = list() #save each res of different per.m.in.lfus for the plotting of bias~ss into reses list 
      
      for(per.m.in.ltfus in pers.m.in.ltfus){
        # library(lpSolve)
        # A = matrix(c(10*per.m.in.ltfus-10,-10,1,0,0,1),nrow=3,byrow=T)
        # b = c(cir*per.m.in.ltfus-ltfur-cir-9,0,0)
        # ans1 = lp(objective.in=c(1,1),const.mat=A,const.rhs=b,const.dir=c("=",">=",">="),dir="min")
        # avg.pmr = ans1$solution[1]
        # avg.ltfur = ans1$solution[2]
        # 
        # avg.pmr.ltfur = solve(c(10*per.m.in.ltfus-10,-10),cir*per.m.in.ltfus-ltfur-cir-9)
        # avg.pmr = avg.pmr.ltfur[0]
        # avg.ltfur = avg.pmr.ltfur[1] 
      
        avg.ltfur = 1-p.mr*(1-per.m.in.ltfus)-(1-cir*(1-per.m.in.ltfus)-ltfur)^(1/10)
        
        print(paste("pmr =",p.mr))
        print(paste("avg.ltfur =",avg.ltfur))
        
        # res = list(m.mrs=rep(0,n.N),downlm.ci=rep(0,n.N),uplm.ci=rep(0,n.N),bias=rep(0,n.N))
        # for(i in 1:n.N){ # i means index of sample size 
        #   n=N[i] # sample size
        #   
        #   mrs = rep(0,n.sim)
        #   # n.total.ltfuss = rep(0,n.sim)
        #   for(sim in 1:n.sim){ # sim means the (sim)th time of simulation
        #     subjects = sample(n)
        #     mrs[sim] = cal.mr(subjects,avg.p.mr,per.m.in.ltfus,n.followyr,avg.ltfur) 
        #   }
        #   
        #   #hist(mrs,breaks="Scott")
        #   #Sys.sleep(60)
        #   
        #   res = cal.m.ci(i,res,mrs) # calculate the mean and ci for our estimation of mortality rate and store them into res list at index i
        #   res = cal.bias(i,p.mr,res,mrs) # calculate the bias for our estimation of mortality rate and store them into res list at index i
        # }
        # reses[[length(reses)+1]] = res
        # cat(paste("Simulation for level of association =",per.m.in.ltfus,"under \nproportion of loss of follow-up =",ltfur,'&','population inccidence rate =',p.mr,"\nhas been done"))
        # 
        # # plot(x=N,y=res$bias,type='l',xlab="Sample Size",ylab="Bias(rmse)",main=paste("Level of Association =",per.m.in.ltfus)) # plot bias~ss
        # plot.emr.ss(N,p.mr,per.m.in.ltfus,res) # plot emr~ss
      }
      #title(paste("Proportion of Loss of Follow-up =",ltfur),outer=T)
      
      # plot.bias(N,pers.m.in.ltfus,reses,ltfur)
      
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