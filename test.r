# Initialization for actual params
actual.mr = 0 # actual mr
actual.ltfur = 0 # actual proportion of loss of follow-up (actual loss rate)
actual.lv.ass = 0 # actual level of association (the proportion of mortality that mistaken for loss)

mean.actual.mr = 0 # mean of actual mr
mean.actual.ltfur = 0 # mean of actual proportion of loss of follow-up (actual loss rate)
mean.actual.lv.ass = 0 # mean of actual level of association (the proportion of mortality that mistaken for loss)

# Initialization for total amount of mos, nominal.mos, ltfus (from real follow-up)
n.total.mos = 0 # no. of total (actual) mortality 
n.total.nominal.mos = 0 # no. of total nominal mortality 
n.total.ltfus = 0 # no. of total loss
n.total.actual.mos = 0
n.total.only.ltfus = 0
# n.total.mistake = 0

cal.mr = function(n,n.followyr,p.mr,per.m.in.ltfus,avg.ltfur){
  subjects = sample(n)
  
  # Initialization for total amount of mos, nominal.mos, ltfus
  # n.total.mos = 0 # no. of total (actual) mortality 
  # n.total.nominal.mos = 0 # no. of total nominal mortality 
  # n.total.ltfus = 0 # no. of total loss
  # n.total.actual.mos = 0
  # n.total.only.ltfus = 0
  # n.total.mistake = 0
  
  ptar = 0 #population time at risk
  for(yr in 1:n.followyr){
    # print(paste("start length(subjects):",length(subjects)))
    # print(paste("p.mr:",p.mr))
    # print(paste("avg.ltfur:",avg.ltfur))
    
    # n.mos = rpois(n=1,lambda=length(subjects)*p.mr) # no. of mortality-occurence subjects
    n.mos = rbinom(n=1,size=length(subjects),prob=p.mr)
    n.ltfus = rbinom(n=1,size=length(subjects),prob=avg.ltfur) # no. of ltfus
    n.mistake = rbinom(n=1,size=n.mos,prob=per.m.in.ltfus)
    nominal.n.mos = n.mos-n.mistake
    n.only.ltfus = n.ltfus-n.mistake # no. of subjects that is loss out of reason of mortality
    
    # print(paste("n.mos:",n.mos))
    # print(paste("n.ltfus:",n.ltfus))
    # print(paste("n.mistake:",n.mistake))
    # print(paste("nominal.n.mos:",nominal.n.mos))
     
    n.total.mos = n.total.mos+n.mos
    n.total.ltfus = n.total.ltfus+n.ltfus
    n.total.nominal.mos = n.total.nominal.mos+nominal.n.mos
    # print(paste("n.total.mos:",n.total.mos))
    # print(paste("n.total.ltfus:",n.total.ltfus))
    # print(paste("n.total.nominal.mos:",n.total.nominal.mos))
    
    # print(paste("nominal.n.mos+n.ltfus:",nominal.n.mos+n.ltfus))
    not.keep.follow.s = sample(subjects,nominal.n.mos+n.ltfus)
    
    
    subjects = subjects[! subjects %in% not.keep.follow.s]
    
    ptar = ptar+n.ltfus*(yr)+nominal.n.mos*(yr-0.5)
  }
  ptar = ptar+length(subjects)*n.followyr # add the contribution of person-year from each remain subjects to ptar
  
  # print(paste("end length(subjects):",length(subjects)))
  # print(paste("ptar:",ptar))
  # print(paste("n.total.mos:",n.total.mos))
  # print(paste("n.total.nominal.mos:",n.total.nominal.mos))
  # print(paste("n.total.ltfus:",n.total.ltfus))
  
  mr = n.total.nominal.mos/ptar # (estimated mortality rate)
  # print(paste("mr:",mr))
  
  actual.cir = (n.total.mos)/n # actual cir
  actual.mr <<- -log(1-actual.cir)/n.followyr # actual mr
  # print(paste("actual.mr:",actual.mr))
  
  actual.ltfur <<- (n.total.ltfus)/n # actual proportion of loss of follow-up (actual loss rate)
  # print(paste("actual.ltfur:",actual.ltfur))
  
  actual.lv.ass <<- (n.total.mos-n.total.nominal.mos)/n.total.mos # actual level of association (the proportion of mortality that mistaken for loss)
  # print(paste("actual.lv.ass:",actual.lv.ass))
  
  return(mr)
}

cal.m.ci = function(i,res,mrs){
  m.mrs = mean(mrs)
  sd.mrs = sd(mrs)
  print(paste("mean:",m.mrs))
  
  uplm.ci = round(m.mrs+1.96*sd.mrs,4) # uplimit of ci
  downlm.ci = round(m.mrs-1.96*sd.mrs,4) # downlimit of ci
  downlm.ci = ifelse(downlm.ci<0,0,downlm.ci) # if downlimit is negative then replace it with 0 
  print(paste("CI: [",downlm.ci,",",uplm.ci,"]"))
  
  res$m.mrs[i] = m.mrs
  res$downlm.ci[i] = downlm.ci
  res$uplm.ci[i] = uplm.ci
  
  return(res)
}

cal.bias = function(i,p.mr,res,mrs){
  #bias = sum(abs(mrs-p.mr)/p.mr)/length(mrs)
  bias = sum(mrs-p.mr)/length(mrs)
  # bias = sqrt(sum((mrs-p.mr)^2)/length(mrs))
  res$bias[i] = bias
  
  return(res)
}

cal.roll.mean = function(n,mean,new_data){
  mean = mean-mean/n
  mean = mean+new_data/n
  return(mean)
}

cal.mean.actual.params = function(n){ #calculate the mean actual params under particular condition (rolling mean)
  mean.actual.mr <<- cal.roll.mean(n,mean.actual.mr,actual.mr)
  mean.actual.ltfur <<- cal.roll.mean(n,mean.actual.ltfur,actual.ltfur)
  mean.actual.lv.ass <<- cal.roll.mean(n,mean.actual.lv.ass,actual.lv.ass)
  
  # print(paste("mean.actual.mr:",mean.actual.mr))
  # print(paste("mean.actual.ltfur:",mean.actual.ltfur))
  # print(paste("mean.actual.lv.ass:",mean.actual.lv.ass))
}

get.mean.actual.params = function(){
  mean.actual.params.ls = c(mean.actual.mr,mean.actual.ltfur,mean.actual.lv.ass)
  
  #reset
  mean.actual.mr <<- 0
  mean.actual.ltfur <<- 0
  mean.actual.lv.ass <<- 0
  
  return(mean.actual.params.ls)
}
