welch = function(x,out)
{
  ab= t.test(x~out)
  
  round(ab$p.value)
  
  
  
  k=deparse(substitute(x))
  
  a= levels(out)[1]
  b=  levels(out)[2]
  
  cat('\n')
  
  cat('Since assumption of Homogeneity of Variance was violated we applied Welch T test')
  
  cat('\n')
  
  r = paste('The Mean +/- SD of  in ',a,' group  was ',round(mean(x[out==a]),2),' and  standard deviation was ',round(sd(x[out==a]),2),'.')
  
  
  r1 = paste('The Mean +/- SD of  in ',b,' group  was ',round(mean(x[out==b]),2),' and  standard deviation was ',round(sd(x[out==b]),2),'.')
  
  value = ab$p.value
  
  
  value = ifelse(value<0.0001,"<0.0001",round(value,4))
  
  
  
  
  value1 = ifelse(value<0.05,"significant","non-significant")
  
  ifelse(ab$p.value<0.0001,"<0.0001",round(ab$p.value,4))
  
  r2 = paste('On performing ',ab$method,' there was a statistically ',value1,' difference between two groups.')
  
  
  r3 = paste('The mean  difference between group ',a,' and group ',b,' was ',round( mean(x[out==a])-mean(x[out==b]),2),' .')
  
  
  r4 = paste('The 95 % confidence interval of difference was',round(ab$conf.int[1],2),' to', round(ab$conf.int[2],2),' .')                  
  
  
  r5 = paste('The t statistic was',round(ab$statistic,2),'. The p value was ',value,' .')
  
  r0 = 'On conducting levene test for Homogeneity of Variance we found out that assumption of equal variance was Violated. Hence We conducted Welch Two-sample t-test'
  
  cat(paste(r,r1,r2,r3,r4,r5,sep="\n"))
}





nowelch = function(x,out)
{
  ab= t.test(x~out,var.equal=TRUE)
  
  round(ab$p.value)
  
  
  
  k=deparse(substitute(x))
  
  a= levels(out)[1]
  b=  levels(out)[2]
  
  
  cat('\n')
  
  
  r = paste('The Mean +/- SD of ',k,' in ',a,' group  was ',round(mean(x[out==a]),2),' and  standard deviation was ',round(sd(x[out==a]),2),'.')
  
  
  r1 = paste('The Mean +/- SD of ',k,' in ',b,' group  was ',round(mean(x[out==b]),2),' and  standard deviation was ',round(sd(x[out==b]),2),'.')
  
  value = ab$p.value
  
  
  value = ifelse(value<0.0001,"<0.0001",round(value,4))
  
  
  value1 = ifelse(value<0.05,"significant","non-significant")
  
  ifelse(ab$p.value<0.0001,"<0.0001",round(ab$p.value,4))
  
  r2 = paste('On performing ',ab$method,' there was a statistically ',value1,' difference between two groups.')
  
  
  r3 = paste('The mean  difference between group ',a,' and group ',b,' was ',round( mean(x[out==a])-mean(x[out==b]),2),' .')
  
  
  r4 = paste('The 95 % confidence interval of difference was',round(ab$conf.int[1],2),' to', round(ab$conf.int[2],2),' .')                  
  
  r5 = paste('The t statistic was',round(ab$statistic,2),'. The p value was ',value,' .')
  
  r0 = 'On conducting levene test for Homogeneity of Variance we found out that assumption of equal variance was True. Hence We conducted equal variance t-test'
  
  cat(paste(r0,r,r1,r2,r3,r4,r5,sep="\n"))
}

levine = function(x,out){
  library(car)
  
  lv= leveneTest(x,out)
  
  
  cat('#########################################')
  cat('\n')
  
  cat('Levene\' test for hompgeneity of variances revealed degree of Freedom of ',lv$Df,' and  F ratio 0f ',round(lv$`F value`,2),'.')
  
  cat('\n')
  if(lv$`Pr(>F)`[1]<0.05){ 
welch(x,out)} else {
nowelch(x,out)}

}


wilcoxy = function(x,out){
  cat('\n')
  cat("The assumpton of normality of parameter  was violated, hence we conducted Mann-Whitney U test")
  
  cat('\n')
  
  a= levels(out)[1]
  b=  levels(out)[2]
  
  ww=wilcox.test(x~out)
  
  r = paste('The Median   in ',a,' group  was ',round(median(x[out==a]),2),' and  inter-quartile range was ',round(IQR(x[out==a]),2),'.')
  
  
  r1 = paste('The Median  in ',b,' group  was ',round(median(x[out==b]),2),' and  inter-quartile range was ',round(IQR(x[out==b]),2),'.')
  
  value = ww$p.value
  
  
  value = ifelse(value<0.0001,"<0.0001",round(value,4))
  
  value1 = ifelse(value<0.05,"significant","non-significant")
  
  ifelse(ww$p.value<0.0001,"<0.0001",round(ww$p.value,4))
  
  r2 = paste('On performing Mann whitney U test there was a statistically ',value1,' difference between two groups.')
  
  r5 = paste('The p value was ',value,' .')
  
  cat(paste(r,r1,r2,r5,sep="\n"))
  
  
}



finalt = function(x,out){
  k1=deparse(substitute(x))

cat('\n')
cat('#########################################')
  cat('\n')
  
  cat(' Analysis of ',k1,' .')
  
  cat('\n')
  
  
  
  cat(paste('On conducting The shapiro-wilk test of normality, the pvalue was=',shapiro.test(x)$p.value,"."))
  cat('\n')
  if(shapiro.test(x)$p.value<0.05){
    wilcoxy(x,out)}
  
  if(shapiro.test(x)$p.value>=0.05){
    cat('\n')
    cat('The assumption of normality was preserved ')
    cat('\n')
    suppressWarnings(levine(x,out))}

cat('\n')

  
}


###Examples

##abc= c(rnorm(30,10,5),rnorm(30,70,10))
##def = c(rnorm(30,30,5),rnorm(30,32,10))
##ghi = c(rnorm(60,40,20))
# god = factor(c(rep("high",30),rep("low",30)))
# levine(def,god)
# levine(abc,god)
# 
# options(scipen = 999)
# 
# 
# 
# finalt(ghi,god)
# 
# library(ggplot2)
# 
# ggplot(data=data.frame(ghi,god),aes(x=god,y=ghi))+geom_boxplot(fill="green")
