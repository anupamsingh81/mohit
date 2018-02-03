
control = rnorm(10,60,15)

stable_angina = rnorm(60,40,12)

unstable_angina= rnorm(30,28,10)



vitamin_D = c(control,stable_angina,unstable_angina)

group = c(rep("Control",10),rep("stable_angina",60), rep("unstable_angina",30))


df = data.frame(Vitamin = vitamin_D, group = group)



fit = aov(df$Vitamin~df$group)

summary(fit)

TukeyHSD(fit)

summary(df)

library(dplyr)
library(ggplot2)

df %>% ggplot(aes(y=Vitamin,x=group,fill=group))+geom_boxplot()+labs(
  title="Vitamin D is significantly higher in control than Angina Subgroup")



df %>% ggplot(aes(y=Vitamin,x=group,fill=group))+geom_bar()+labs(
  title="Vitamin D is significantly higher in control than Angina Subgroup")

# randomly simulating demographic variables

df$age= rnorm(100,50,15)

df$sex= rbinom(n=100,size=1,prob = 0.52)

df$smoker= c(rbinom(n=10,size=1,p=0.2),rbinom(n=90,size=1,p=0.8))
  
df$Hypertensive= c(rbinom(n=10,size=1,p=0.25),rbinom(n=90,size=1,p=0.72))

df$Diabetic= c(rbinom(n=10,size=1,p=0.1),rbinom(n=90,size=1,p=0.6))

df$LDL =  c(rnorm(n=10,mean=120,sd=20),rnorm(n=90,mean=180,sd=30))

df$BMI = c(rnorm(n=10,mean=23,sd=2), rnorm(n=90,mean=28,sd=3))

df$Family_history = c(rbinom(n=10,size=1,p=0.15),rbinom(n=90,size=1,p=0.35))


library(tableone)

vars = names(df)

vars = vars[-2]

tab2 <- CreateTableOne(vars = vars, strata = "group" , data = df)

df$sex= as.factor(df$sex)
df$smoker= as.factor(df$smoker)
df$Hypertensive = as.factor(df$Hypertensive)
df$Diabetic = as.factor(df$Diabetic)
df$group = as.factor(df)

df$Family_history = as.factor(df$Family_history)

print(tab2,quote = TRUE,noSpaces = TRUE)

df[is.factor(df)]

df$VitaminD

df %>% df[map(.,is.factor)]%>% names()

names(df[is.factor(df)])
library(purrr)
library(dplyr)
library(ggplot2)
plots= df%>% select_if(is.numeric) %>% names() %>% map(function(y)ggplot(df,aes(group,fill=group))+ geom_boxplot(aes_string(y=y)))
paths <- stringr::str_c("group",1:length(plots), ".png")
pwalk(list(paths, plots), ggsave, path = getwd())

fac= df %>% select_if(is.factor) %>% names()

plots2= df %>% select_if(is.factor) %>% map(~ggplot(df, aes(group, ..count..)) + geom_bar(aes(fill = .), position = "dodge")+ labs(title=.))
paths2 <- stringr::str_c("group",fac,1:length(plots2), ".png")
pwalk(list(paths2, plots2), ggsave, path = getwd())

df %>% filter(Vitamin<20)

df= df %>% mutate( vitaminstatus = case_when(
  Vitamin<20 ~"Deficiency",
  Vitamin<30~"Insufficiency",
  TRUE~ " Normal"))

  df$vitaminstatus = as.factor(df$vitaminstatus)

  plots3= df%>% select_if(is.numeric) %>% names() %>% map(function(y)ggplot(df,aes(vitaminstatus,fill=vitaminstatus))+ geom_boxplot(aes_string(y=y)))
  paths3 <- stringr::str_c("vitaminstatus",1:length(plots), ".png")
  pwalk(list(paths3, plots3), ggsave, path = getwd())
  
  
  plots4= df %>% select_if(is.factor) %>% map(~ggplot(df, aes(vitaminstatus, ..count..)) + geom_bar(aes(fill = .), position = "dodge")+ labs(title=.))
  paths4 <- stringr::str_c("vitamin",fac,1:length(plots2), ".png")
  pwalk(list(paths4, plots4), ggsave, path = getwd())
  
  
  av= function(x){
    
    f=aov(x~df$group)
    
    
    f1= summary(f)
    
    f2=TukeyHSD(f)
    
    f3=list(f2,f1)
    
    f3
  }
  
  # apply map argument
  
  
  df%>% select_if(is.numeric) %>% map(~av(.))
  
  ?chisq.test
  chisq.test(df$smoker,df$Diabetic)
  table(df$smoker,df$Diabetic)
  
  
  ?write.csv
  
  write.csv(df1,file="vitamind.csv")
  
  as.integer(df$age)
  
  df$age = as.integer(df$age)
  df$LDL = as.integer(df$LDL)
  df$BMI = as.integer(df$BMI)
  df$Vitamin = round(df$Vitamin,1)
  write.csv(df1,file="vitamin.csv")
  
  library(dplyr)
  df1= df %>% mutate(sex = case_when(
    sex==1~"Male",
    TRUE~"Female"),
    Family_history = case_when(
      Family_history== 1~"Family History",
      TRUE~"No Family History"),
    Hypertensive = case_when(
      Hypertensive== 1~"Hypertensive",
      TRUE~"Normotensive"),
    Diabetic = case_when(
     Diabetic== 1~"Diabetic",
      TRUE~"Non-Diabetic"),
    smoker = case_when(
     smoker== 1~"Diabetic",
      TRUE~"Non-Diabetic"))
    
    
  library(gmodels)
  library(tidyverse)
  
  chichi1 = df1 %>% select_if(is.factor) %>% map(~CrossTable(.,df1$group,chisq=TRUE))
  
  sink("chi1.txt")
  chichi1
  sink()
  
  str(df1)
  
  
  
  df1$smoker = as.factor(df1$smoker)
  df1$Hypertensive = as.factor(df1$Hypertensive)
  df1$Diabetic = as.factor(df1$Diabetic)
  df1$sex = as.factor(df1$sex)
  df1$Family_history = as.factor(df1$Family_history)