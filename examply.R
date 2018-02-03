
source('anovat.R')

av= function(x){
  
  f=anovafinal(x,df1$group)
  
  f
  
  
}

library(tidyverse)

avv= df1%>% select_if(is.numeric) %>% names()

sink("avv.txt")
df1%>%select_if(is.numeric) %>%  map(~anovafinal(.,df1$group))
sink()

source('finalt.R')

sink('tt.txt')

#df1%>%select_if(is.numeric) %>%map(~finalt(.,df1$Diabetic))
#df1 %>% map_if(is.numeric,~finalt(.x,df1$Diabetic))

sink()

