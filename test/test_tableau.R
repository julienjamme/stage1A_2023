library(dplyr)

test_tableau<-function(data,v1,v2,res,totcode){
  tab<- select(data, v1, v2)
  tab$v3 <- paste(data[[v1]], data[[v2]], sep = "_")
  
  var1_total <- totcode[v1]
  var2_total <- totcode[v2]
  
  
  var<-paste( v1,v2, sep = "_")
  
  # On filtre 
  data_voulu2<-tab %>% 
    filter(data[[v2]] == var2_total | (data[[v1]] !=var1_total & data[[v2]] != var2_total))
  data_voulu1<-tab %>% 
    filter(data[[v1]] == var1_total | (data[[v1]] !=var1_total & data[[v2]] != var2_total))
  
  
  
  nom1<-names(res$tabs)[1]
  nom2<-names(res$tabs)[2]
  
  #Les mêmes modalités
  t<-identical(sort(unique(res$tabs[[nom1]][[var]])),sort(unique(data_voulu2$v3)))
  t1<-identical(sort(res$tabs[[nom1]][[var]]),sort(data_voulu2$v3))
  t2<-identical(sort(res$tabs[[nom2]][[var]]),sort(data_voulu1$v3))
  return (list(tab1=t1,tab2=t2))
}