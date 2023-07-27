# data <- data.frame(ID = c("A", "A", "A","A", "B", "B", "B", "C", "C", "D"),
#                    colour = c("B", "C", "D", "E", "C", "D", "E", "D", "E", "E"),
#                    age =c(7, 7, 5, 6, 3, 7, 9, 8, 3, 2))


ahp_all<-readRDS("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/ahp.rds")
ahp<-readRDS("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/ahp_test.rds")

## only one part
data<-ahp_all%>%filter(userID =="eSInDb5DdD")%>%select(comp_val,left,right)
rm(ahp_all)
# data$comp_val<-abs(data$comp_val)
for (i in 1:nrow(data)) {
  if(data$comp_val[i]<0){
  data$comp_val_rigt[i]<-abs(data$comp_val[i])
  data$comp_val[i]<-1/abs(data$comp_val[i])
}else{
  data$comp_val_rigt[i]<-1/data$comp_val[i]
}
}
  


un1 <- sort(unique(unlist(data[2:3])))

#### lower matrix with left side


m1 <- xtabs(comp_val ~ left + right, transform(data, 
                                         left = factor(left, levels = un1), right = factor(right, levels = un1)))

m2 <- xtabs(comp_val_rigt~ right + left, transform(data, 
                                               left = factor(left, levels = un1), right = factor(right, levels = un1)))


m3<-(m1+m2)
