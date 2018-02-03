
regcoef = function(x){
  data = df1 %>% dplyr::select(x,Diabetic)
  #df1
  dd = glm(as.factor(Diabetic)~.,family=binomial(),data=data)
  l=c(as.numeric(exp(confint(dd)[-1,])[1]),as.numeric(exp(dd$coefficient[-1])),as.numeric(exp(confint(dd)[-1,])[2]))
l
}

regcoef = function(x){
  data = df1 %>% dplyr::select(x,Diabetic)
  #df1
  dd = glm(as.factor(Diabetic)~.,family=binomial(),data=data)
  l=c(exp(confint(dd)[-1,])[1],exp(dd$coefficient[-1]),exp(confint(dd)[-1,])[2])
  l
}

regcoef1 = function(x){
  df1 = hem2 %>% dplyr::select(x,REBLEED.EPISODE)
  df1
  a = glm(as.factor(REBLEED.EPISODE)~.,family=binomial(),data=df1)
  b= exp(a$coefficient)
  
  d =  exp(confint(a)) # retain intercept for factor for first level
  list(b,d)
}
library(tidyverse)

unlist(regcoef('smoker'))[[1]]

unlist(regcoef('smoker'))[[2]]

as.numeric(regcoef('smoker')[[2]][2])

dd=glm(as.factor(Diabetic)~smoker,family=binomial(),data=df1)

l=c(as.numeric(exp(confint(dd)[-1,])[1]),as.numeric(exp(dd$coefficient[-1])),as.numeric(exp(confint(dd)[-1,])[2]))
   
regcoef('smoker')
   
   df1 =df1 %>% mutate(smoker= ifelse(smoker=="Diabetic","smoker","non-smoker"))
  
   
   factors = df1 %>%  select_if(~n_distinct(.)<3) %>% select(-Diabetic) %>% names() 
   df1 %>% 
     select_if(~n_distinct(.)<3) %>% 
     select(-Diabetic) %>% 
     names() %>% 
  map(~regcoef(.)) %>% 
     enframe() %>% # list storing vectors
     unnest() %>% #unnesting
     mutate(name=as.factor(name)) %>% 
     mutate(id=1:n()) %>% # important for spread to work otherise duplicate identifier
     spread(name,value) %>% 
     select(-id) %>% 
     map_df(~na.omit(.)) %>% 
     t() %>%   # transpose %>% 
   as_data_frame() %>% 
     rename( lower= V1,mean=V2,upper=V3) %>% 
     add_column(riskfactor=factors)
   
   ttt= table(df1$Diabetic,df1$smoker)
   
   
            length(df1$smoker[df1$Diabetic=="Diabetic"])
   count(df1$smoker[df1$Diabetic=="Diabetic"])
     
  
  regcoef(smoker)
  
  factors1 =df1 %>% 
    select_if(~n_distinct(.)<3) %>% names()
  
  df1 %>% 
    select_if(~n_distinct(.)<3) %>% 
    group_by_at(factors1)%>% tally()
    #group_by(Diabetic,smoker)
    
    table()