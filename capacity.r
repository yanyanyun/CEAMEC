## cost effectiveness management methods for covariates
Capacity <- function(bestmodel, costs, rate, P_dens, ly, expect, area, time){
  coeffs <- as.data.frame(bestmodel@estimates@estimates$state@estimates)
  name_coeffs <- as.data.frame(names(bestmodel@estimates@estimates$state@estimates))
  row.names(coeffs) <- NULL
  coeffs <- cbind(name_coeffs, coeffs)
  names(coeffs) <- c("X", "coefficient")
  co <- merge(coeffs, costs, by.X = "X", by.y = "X")
  r<-rate
  p<-P_dens
  ps<-subset(p,p$layer==ly)
  ply<-ps[,c("layer","Predicted","SE","lower","upper",as.character(co[,1]))]
  N0=ply$Predicted*area
  Nt<-expect*area
  T<-time
  K<-(N0*(exp(r*T)-1))*Nt/(N0*exp(r*T)-Nt)  
  Delta<-log(N0)-log(K)
  pl<-length(co[,1])+5
  
  DFF<-data.frame("item"=co[,1], "weight"=co[,2], "unitvalue"=c(co[,3]*T+co[,4]), "fixedvalue"=c(co[,5]*T+co[,6]), "pieces"=as.numeric(ply[,6:pl]))  
  
  
  fitness= function(x= rep(1, nrow(DFF))){
    total_value= sum(DFF$unitvalue * x + DFF$fixedvalue*round(x/(x+0.00001)))
    total_weight= sum(DFF$weight * x)
    ifelse(total_weight >= Delta, total_value, Inf)
  }
  
  allowed= matrix(c(rep(0, nrow(DFF)), DFF$pieces), ncol = 2)
  set.seed(42)
  evolution= rgenoud::genoud(fn= fitness, 
                             nvars= nrow(allowed), 
                             max= FALSE,
                             pop.size= 10000,
                             data.type.int= TRUE, 
                             Domains= allowed)
  
  list_covs <- list()
  for (i in as.character(co[,1])){
    
    list_covs$X <- c(0, ply[1,i])  
    names(list_covs)[names(list_covs)=="X"] <- i
    
  }
  qq <- expand.grid(list_covs)
  
  list_names <- list()
  for (i in as.character(co[,1])){
    
    list_names$X <- c(0,i)
    names(list_names)[names(list_names)=="X"] <- i
    
  }
  qq_names <- do.call(paste, c(expand.grid(list_names), sep="+"))
  qq_names <- c(qq_names,"best")
  
  qqq <- rbind(qq, as.numeric(evolution$par))
  
  qqqq <- 0 - qqq
  
  psn <- Filter(is.numeric, ps)
  psc <- Filter(is.character, ps)
  names(qqqq) <- as.character(co[,1])
  qqn <- cbind(qqqq, psn)
  qqn <- t(rowsum(t(qqn), group = colnames(qqn), na.rm = T))
  newdatar <- cbind(qqn,psc)
  
  P_densr <- predict(bestmodel, type = "state", newdata = newdatar)
  P_densr[,2] <- P_densr[1,1]*exp(r*T)*P_densr[,1]/(P_densr[1,1]*exp(r*T)+P_densr[,1]-P_densr[1,1])
  
  q_values <- qqq
  
  for (i in as.character(co[,1])){
    
    q_values[,i] <- qqq[,i]*DFF[DFF$item==i,3] + round(qqq[,i]/(qqq[,i]+0.00001))*DFF[DFF$item==i,4]  
    
  }
  
  final <- cbind(qq_names, rowSums(q_values))
  final <- cbind(final, P_densr[1,1])
  final <- cbind(final, P_densr[,2])
  final <- cbind(final, qqq)
  final <- subset(final, final[,2]!=0)
  colnames(final)[1:4] <- c("Scenarios","Costs","Pre-management","Post-management")
  
  return(final)
}#######End function Capacity
