#TOMASZ GŁADKI, KACPER OLCZAK

r=0.02
t=1/12
sigma=0.3
u=exp(sigma*t^(1/2))
d=exp(-sigma*t^(1/2))
S0=50
K=48
Time=2
num=Time/t


syf<-function(S0, u, d, num){
  #u<-
  #d<-
  vec <- rep(S0*u^(num-1),num)
  k <- 0:num
  S0*u^(num-k)*d^(k)
  # vec2 <- d^seq(0,length.out=(num),by=2)
  # return(c(vec*vec2))
}
last<-syf(S0, u, d, num)

payoff <- function(vec,K,arg){
  if (arg=="PUT"){
    return(pmax(K-vec,0))
  }
  if (arg=="CALL"){
    return(pmax(vec-K,0))
  }
}

payoff_ec<-payoff(last, K, "CALL")
payoff_ep<-payoff(last, K, "PUT")


CE <- payoff_calc_e(S0, K, sigma, r, t, Time)
PE <- payoff_calc_e(S0, K, sigma, r, t, Time, "PUT")
PE[91,1]+S0 - (CE[91,1]+K*exp(-r*Time))
payoff_calc_e<-function(S0,K,sigma, r, t, Time,arg="CALL"){
  u <- exp(sigma*t^(1/2))
  d <- exp(-sigma*t^(1/2))
  p <- (exp(r*t)-d)/(u-d)
  num<-Time/t
  last<-syf(S0,u,d,num)
  ceny_opcji<-payoff(last,K,arg)
  num<-length(ceny_opcji) # długość ostatniego
  first<-rep(num-1,num)
  second<-seq(num-1,-num+1,-2)
  for(i in 1:(num-1)){
    len_curr_payoff <- num-i+1 #długość obecnego
    payoffs<-ceny_opcji[(length(ceny_opcji)-len_curr_payoff+1):length(ceny_opcji)] # obecny wektor payoffów (ostatnie 25-i wyrazów)
    a <- exp(-r*t)*(p*payoffs[1:(length(payoffs)-1)]+(1-p)*payoffs[2:length(payoffs)]) # wartość na poprzedni moment
    first<-append(first, rep(num-i-1,len_curr_payoff-1))
    second<-append(second, seq(len_curr_payoff-2,-len_curr_payoff+2,-2))
    ceny_opcji<-append(ceny_opcji, a)
    
    
  }
  return(data.frame(ceny_opcji,first,second))
}

tailed_payoff_calc_e<-function(S0,K,sigma, r, t, Time,arg){
  return(tail(payoff_calc_e(S0,K,sigma,r,t,Time,arg)$ceny_opcji,1))
}

tailed_payoff_calc_e<-Vectorize(tailed_payoff_calc_e)

dfce<-payoff_calc_e(S0,K,sigma,r,t,Time,arg="CALL")
dfpe<-payoff_calc_e(S0,K,sigma,r,t,Time,arg="PUT")


payoff_calc_a<-function(S0,K,sigma,r,t,Time,arg){
  u<-exp(sigma*t^(1/2))
  d<-exp(-sigma*t^(1/2))
  p <- (exp(r*t)-d)/(u-d)
  num<-Time/t
  vec_price<-syf(S0,u,d,num)
  ceny_opcji<-payoff(vec_price,K,arg)
  when <- (ceny_opcji > 0)
  num<-length(ceny_opcji) # długość ostatniego
  first<-rep(num-1,num)
  second<-seq(num-1,-num+1,-2)
  for(i in 1:(num-1)){
    vec_price<-vec_price[1:(length(vec_price)-1)]/u
    len_curr_payoff <- num-i+1 #długość obecnego
    payoffs<-ceny_opcji[(length(ceny_opcji)-len_curr_payoff+1):length(ceny_opcji)] # obecny wektor payoffów (ostatnie 25-i wyrazów)
    result <- exp(-r*t)*(p*payoffs[1:(length(payoffs)-1)]+(1-p)*payoffs[2:length(payoffs)]) # wartość na poprzedni moment
    if(arg=="CALL"){
      when<-append(when,((vec_price-K)>result))
      result <- pmax(vec_price-K,result)
    }
    if(arg=="PUT"){
      when<-append(when,(K-vec_price)>result)
      result <- pmax(K-vec_price,result)
    }
    first<-append(first, rep(num-i-1,len_curr_payoff-1))
    second<-append(second, seq(len_curr_payoff-2,-len_curr_payoff+2,-2))
    ceny_opcji<-append(ceny_opcji, result)
    
  }
  return(data.frame(ceny_opcji,first,second,when))
}

tailed_payoff_calc_a<-function(S0,K,sigma, r, t, Time,arg){
  return(tail(payoff_calc_a(S0,K,sigma,r,t,Time,arg)$ceny_opcji,1))
}


dfca<-payoff_calc_a(S0,K,sigma,r,t,Time,arg="CALL")
dfpa<-payoff_calc_a(S0,K,sigma,r,t,Time,arg="PUT")

library(ggplot2)
require(gridExtra)

dce<-ggplot(data=dfce, aes(x=first,y=second))+
  geom_point(aes(alpha=ceny_opcji), color="red") + 
  ggtitle("CALL europejski") +
  theme_void()+ theme(plot.title = element_text(hjust = 0.5))
dca<-ggplot(data=dfca, aes(x=first,y=second))+
  geom_point(aes(alpha=ceny_opcji), color="blue") + 
  ggtitle("CALL amerykański") +
  theme_void()+ theme(plot.title = element_text(hjust = 0.5))
grid.arrange(dce,dca,ncol=2)


dpe<-ggplot(data=dfpe, aes(x=first,y=second))+
  geom_point(aes(alpha=ceny_opcji), color="red") + ggtitle("PUT europejski") +
  theme_void()+ theme(plot.title = element_text(hjust = 0.5))
dpa<-ggplot(data=dfpa, aes(x=first,y=second))+
  geom_point(aes(alpha=ceny_opcji), color="blue") + ggtitle("PUT amerykański") +
  theme_void()+ theme(plot.title = element_text(hjust = 0.5))
grid.arrange(dpe,dpa,ncol=2)


mca<-ggplot(data=dfca, aes(x=first,y=second))+
  geom_point(aes(alpha=when), color="blue")+ ggtitle("CALL amerykański") +
  theme_void()+ theme(plot.title = element_text(hjust = 0.5))
mpa<-ggplot(data=dfpa, aes(x=first,y=second))+
  geom_point(aes(alpha=when), color="blue")+ ggtitle("PUT amerykański") +
  theme_void()+ theme(plot.title = element_text(hjust = 0.5))
grid.arrange(mca,mpa,ncol=2)



tresce<-tailed_payoff_calc_e(S0,K_vec,sigma,r,t,Time,arg="CALL")


trespe<-tailed_payoff_calc_e(S0,K_vec,sigma,r,t,Time,arg="PUT")


#tresca<-tailed_payoff_calc_a(S0,K_vec,sigma,r,t,Time,arg="CALL")

K_vec <- seq(0,200,0.1) #tutaj taki duży, bo widać w sumie że jak jest duży strike to put rośnie już normalnie bo od tego zależy cena puta głownie
S0_vec <-seq(0,50,0.1) #tutaj w sumie analogicznie jak wyżej, ale myśle, że trzeba zrobić też drugie wektory od powidzmy 0 do 100 jak wcześniej robiliśmy
r_vec<-seq(0,1,0.001) #to nwm chyba ok jest
sigma_vec<-seq(0,3,0.01) #to też myśle git
Time_vec<-seq(1, 50, 1) #tutaj jak dałem dużo lat to sie psuło mocno a 4 chyba wystarczą
t_vec<-1/seq(1,100,1) #te dwa są dziwne mego bo coś w tym nie działa to wgl nie robiłem,
t_vec2<-union(1/seq(100,1,-1),2/seq(100,2,-1))
#t_vec2<-c(1/52,1/30,1/20,1/12,1/6,1/4,1/3,1/2,1) #a ten działa ale ma w sumie tylko takie podstawowe wartości t takie życiowe nwm

tailed_payoff_calc_a<-Vectorize(tailed_payoff_calc_a)
tailed_payoff_calc_e<-Vectorize(tailed_payoff_calc_e)




#STRIKE - K
tresce<-tailed_payoff_calc_e(S0,K_vec,sigma,r,t,Time,arg="CALL")
tresca<-tailed_payoff_calc_a(S0,K_vec,sigma,r,t,Time,arg="CALL")

trespe<-tailed_payoff_calc_e(S0,K_vec,sigma,r,t,Time,arg="PUT")
trespa<-tailed_payoff_calc_a(S0,K_vec,sigma,r,t,Time,arg="PUT")

dfKc <- data.frame("K"=K_vec, "EUR"=tresce, "AM"=tresca)
dfKp<-data.frame("K"=K_vec, "EUR"=trespe, "AM"=trespa)

PKc <- ggplot()+
  geom_line(data=dfKc, aes(x=K, y=EUR),color="red", alpha=0.5)+
  geom_line(data=dfKc, aes(x=K, y=AM),color="blue", alpha=0.5)+
  ylab("Cena opcji")+ggtitle("CALL przy zmianie K")+ theme(plot.title = element_text(hjust = 0.5))

PKp <- ggplot()+
  geom_line(data=dfKp, aes(x=K, y=EUR),color="red", alpha=0.5)+
  geom_line(data=dfKp, aes(x=K, y=AM),color="blue", alpha=0.5)+
  ylab("Cena opcji")+ggtitle("PUT przy zmianie K")+theme(plot.title = element_text(hjust = 0.5))
grid.arrange(PKc,PKp, ncol=2)  



#CENA AKCJI - S0
tresce2<-tailed_payoff_calc_e(S0_vec,K,sigma,r,t,Time,arg="CALL")
tresca2<-tailed_payoff_calc_a(S0_vec,K,sigma,r,t,Time,arg="CALL")

trespe2<-tailed_payoff_calc_e(S0_vec,K,sigma,r,t,Time,arg="PUT")
trespa2<-tailed_payoff_calc_a(S0_vec,K,sigma,r,t,Time,arg="PUT")

dfSc<-data.frame("S_0"=S0_vec, "EUR"=tresce2, "AM"=tresca2)
dfSp<-data.frame("S_0"=S0_vec, "EUR"=trespe2, "AM"=trespa2)

PSc <- ggplot()+
  geom_line(data=dfSc, aes(x=S_0, y=EUR),color="red", alpha=0.5)+
  geom_line(data=dfSc, aes(x=S_0, y=AM),color="blue", alpha=0.5)+
  ylab("Cena opcji")+ggtitle("CALL przy zmianie S_0")+ theme(plot.title = element_text(hjust = 0.5))

PSp <- ggplot()+
  geom_line(data=dfSp, aes(x=S_0, y=EUR),color="red", alpha=0.5)+
  geom_line(data=dfSp, aes(x=S_0, y=AM),color="blue", alpha=0.5)+
  ylab("Cena opcji")+ggtitle("PUT przy zmianie S_0")+theme(plot.title = element_text(hjust = 0.5))

grid.arrange(PSc,PSp, ncol=2)  

#STOPA PROCENTOWA - r
tresce3<-tailed_payoff_calc_e(S0,K,sigma,r_vec,t,Time,arg="CALL")
tresca3<-tailed_payoff_calc_a(S0,K,sigma,r_vec,t,Time,arg="CALL")

trespe3<-tailed_payoff_calc_e(S0,K,sigma,r_vec,t,Time,arg="PUT")
trespa3<-tailed_payoff_calc_a(S0,K,sigma,r_vec,t,Time,arg="PUT")

dfrc<-data.frame("r"=r_vec, "EUR"=tresce3, "AM"=tresca3)
dfrp<-data.frame("r"=r_vec, "EUR"=trespe3, "AM"=trespa3)

Prc <- ggplot()+
  geom_line(data=dfrc, aes(x=r, y=EUR),color="red", alpha=0.5)+
  geom_line(data=dfrc, aes(x=r, y=AM),color="blue", alpha=0.5)+
  ylab("Cena opcji")+ggtitle("CALL przy zmianie r")+ theme(plot.title = element_text(hjust = 0.5))

Prp <- ggplot()+
  geom_line(data=dfrp, aes(x=r, y=EUR),color="red", alpha=0.5)+
  geom_line(data=dfrp, aes(x=r, y=AM),color="blue", alpha=0.5)+
  ylab("Cena opcji")+ggtitle("PUT przy zmianie r")+theme(plot.title = element_text(hjust = 0.5))
grid.arrange(Prc,Prp,ncol=2)

#SIGMA
tresce4<-tailed_payoff_calc_e(S0,K,sigma_vec,r,t,Time,arg="CALL")
tresca4<-tailed_payoff_calc_a(S0,K,sigma_vec,r,t,Time,arg="CALL")

trespe4<-tailed_payoff_calc_e(S0,K,sigma_vec,r,t,Time,arg="PUT")
trespa4<-tailed_payoff_calc_a(S0,K,sigma_vec,r,t,Time,arg="PUT")

dfsc<-data.frame("sigma"=sigma_vec, "EUR"=tresce4, "AM"=tresca4)
dfsp<-data.frame("sigma"=sigma_vec, "EUR"=trespe4, "AM"=trespa4)

Psc <- ggplot()+
  geom_line(data=dfsc, aes(x=sigma, y=EUR),color="red", alpha=0.5)+
  geom_line(data=dfsc, aes(x=sigma, y=AM),color="blue", alpha=0.5)+
  ylab("Cena opcji")+ggtitle("CALL przy zmianie sigma")+ theme(plot.title = element_text(hjust = 0.5))

Psp <- ggplot()+
  geom_line(data=dfsp, aes(x=sigma, y=EUR),color="red", alpha=0.5)+
  geom_line(data=dfsp, aes(x=sigma, y=AM),color="blue", alpha=0.5)+
  ylab("Cena opcji")+ggtitle("PUT przy zmianie sigma")+theme(plot.title = element_text(hjust = 0.5))
grid.arrange(Psc,Psp,ncol=2)

#CZAS WYKONANIA - Time
tresce5<-tailed_payoff_calc_e(S0,K,sigma,r,t,Time_vec,arg="CALL")
tresca5<-tailed_payoff_calc_a(S0,K,sigma,r,t,Time_vec,arg="CALL")

trespe5<-tailed_payoff_calc_e(S0,K,sigma,r,t,Time_vec,arg="PUT")
trespa5<-tailed_payoff_calc_a(S0,K,sigma,r,t,Time_vec,arg="PUT")

dfTc<-data.frame("T"=Time_vec, "EUR"=tresce5, "AM"=tresca5)
dfTp<-data.frame("T"=Time_vec, "EUR"=trespe5, "AM"=trespa5)

PTc <- ggplot()+
  geom_line(data=dfTc, aes(x=T, y=EUR),color="red", alpha=0.5)+
  geom_line(data=dfTc, aes(x=T, y=AM),color="blue", alpha=0.5)+
  ylab("Cena opcji")+ggtitle("CALL przy zmianie T")+ theme(plot.title = element_text(hjust = 0.5))

PTp <- ggplot()+
  geom_line(data=dfTp, aes(x=T, y=EUR),color="red", alpha=0.5)+
  geom_line(data=dfTp, aes(x=T, y=AM),color="blue", alpha=0.5)+
  ylab("Cena opcji")+ggtitle("PUT przy zmianie T")+theme(plot.title = element_text(hjust = 0.5))
grid.arrange(PTc,PTp,ncol=2)

#CZAS PODOKRESU - t
tresce6<-tailed_payoff_calc_e(S0,K,sigma,r,t_vec,Time,arg="CALL")
tresca6<-tailed_payoff_calc_a(S0,K,sigma,r,t_vec,Time,arg="CALL")

trespe6<-tailed_payoff_calc_e(S0,K,sigma,r,t_vec,Time,arg="PUT")
trespa6<-tailed_payoff_calc_a(S0,K,sigma,r,t_vec,Time,arg="PUT")

dftc<-data.frame("dt"=t_vec, "EUR"=tresce6, "AM"=tresca6)
dftp<-data.frame("dt"=t_vec, "EUR"=trespe6, "AM"=trespa6)

Ptc <- ggplot()+
  geom_line(data=dftc, aes(x=dt, y=EUR),color="red", alpha=0.5)+
  geom_line(data=dftc, aes(x=dt, y=AM),color="blue", alpha=0.5)+
  ylab("Cena opcji")+ggtitle("CALL przy zmianie t")+ theme(plot.title = element_text(hjust = 0.5))

Ptp <- ggplot()+
  geom_line(data=dftp, aes(x=dt, y=EUR),color="red", alpha=0.5)+
  geom_line(data=dftp, aes(x=dt, y=AM),color="blue", alpha=0.5)+
  ylab("Cena opcji")+ggtitle("PUT przy zmianie t")+theme(plot.title = element_text(hjust = 0.5))
grid.arrange(Ptc,Ptp,ncol=2)

#CZAS PODOKRESU - t2

tresce6<-tailed_payoff_calc_e(S0,K,sigma,r,t_vec2,Time,arg="CALL")
tresca6<-tailed_payoff_calc_a(S0,K,sigma,r,t_vec2,Time,arg="CALL")

trespe6<-tailed_payoff_calc_e(S0,K,sigma,r,t_vec2,Time,arg="PUT")
trespa6<-tailed_payoff_calc_a(S0,K,sigma,r,t_vec2,Time,arg="PUT")

dftc2<-data.frame("dt"=t_vec2, "EUR"=tresce6, "AM"=tresca6)
dftp2<-data.frame("dt"=t_vec2, "EUR"=trespe6, "AM"=trespa6)

Pt2c <- ggplot()+
  geom_line(data=dftc2, aes(x=dt, y=EUR),color="red", alpha=0.5)+
  geom_line(data=dftc2, aes(x=dt, y=AM),color="blue", alpha=0.5)+
  ylab("Cena opcji")+ggtitle("CALL przy zmianie t")+ theme(plot.title = element_text(hjust = 0.5))

Pt2p <- ggplot()+
  geom_line(data=dftp2, aes(x=dt, y=EUR),color="red", alpha=0.5)+
  geom_line(data=dftp2, aes(x=dt, y=AM),color="blue", alpha=0.5)+
  ylab("Cena opcji")+ggtitle("PUT przy zmianie t")+theme(plot.title = element_text(hjust = 0.5))
grid.arrange(Pt2c,Pt2p,ncol=2)


delta_a<-function(S0,K,sigma,r,t,Time,arg){
  u=exp(sigma*t^(1/2))
  d=exp(-sigma*t^(1/2))
  p <- (exp(r*t)-d)/(u-d)
  num<-Time/t+1
  vec_price<-syf(S0,u,d,num)
  vec_main<-payoff(vec_price,K,arg)
  when <- (vec_main > 0)
  num<-length(vec_main) # długość ostatniego
  first<-rep(num-1,num)
  second<-seq(num-1,-num+1,-2)
  delta<-rep(-10,times=num)
  for(i in 1:(num-1)){
    
    len_curr_payoff <- num-i+1 #długość obecnego
    payoffs<-vec_main[(length(vec_main)-len_curr_payoff+1):length(vec_main)] # obecny wektor payoffów (ostatnie 25-i wyrazów)
    licznik<-payoffs[1:(length(payoffs)-1)]-payoffs[2:length(payoffs)]
    mianownik<-vec_price[1:(length(vec_price)-1)]-vec_price[2:length(vec_price)]
    delta<-append(delta,abs(licznik/mianownik))
    result <- exp(-r*t)*(p*payoffs[1:(length(payoffs)-1)]+(1-p)*payoffs[2:length(payoffs)]) # wartość na poprzedni moment
    vec_price<-vec_price[1:(length(vec_price)-1)]/u
    if(arg=="CALL"){
      when<-append(when,((vec_price-K)>result))
      result <- pmax(vec_price-K,result)
    }
    if(arg=="PUT"){
      when<-append(when,(K-vec_price)>result)
      result <- pmax(K-vec_price,result)
    }
    first<-append(first, rep(num-i-1,len_curr_payoff-1))
    second<-append(second, seq(len_curr_payoff-2,-len_curr_payoff+2,-2))
    vec_main<-append(vec_main, result)
    
  }
  return(data.frame(delta,first,second))
}



delta_e<-function(S0,K,sigma,r,t,Time,arg){
  u=exp(sigma*t^(1/2))
  d=exp(-sigma*t^(1/2))
  p <- (exp(r*t)-d)/(u-d)
  num<-Time/t+1
  vec_price<-syf(S0,u,d,num)
  vec_main<-payoff(vec_price,K,arg)
  when <- (vec_main > 0)
  num<-length(vec_main) # długość ostatniego
  first<-rep(num-1,num)
  second<-seq(num-1,-num+1,-2)
  delta<-rep(-10,times=num)
  for(i in 1:(num-1)){
    len_curr_payoff <- num-i+1 #długość obecnego
    payoffs<-vec_main[(length(vec_main)-len_curr_payoff+1):length(vec_main)] # obecny wektor payoffów (ostatnie 25-i wyrazów)
    licznik<-payoffs[1:(length(payoffs)-1)]-payoffs[2:length(payoffs)]
    mianownik<-vec_price[1:(length(vec_price)-1)]-vec_price[2:length(vec_price)]
    delta<-append(delta,abs(licznik/mianownik))
    result <- exp(-r*t)*(p*payoffs[1:(length(payoffs)-1)]+(1-p)*payoffs[2:length(payoffs)]) # wartość na poprzedni moment
    vec_price<-vec_price[1:(length(vec_price)-1)]/u
    first<-append(first, rep(num-i-1,len_curr_payoff-1))
    second<-append(second, seq(len_curr_payoff-2,-len_curr_payoff+2,-2))
    vec_main<-append(vec_main, result)
    
  }
  return(data.frame(delta,first,second))
}

delfce<-tail(delta_e(S0,K,sigma,r,t,Time,arg="CALL"),300)
delfca<-tail(delta_a(S0,K,sigma,r,t,Time,arg="CALL"),300)

delfpe<-tail(delta_e(S0,K,sigma,r,t,Time,arg="PUT"),300)
delfpa<-tail(delta_a(S0,K,sigma,r,t,Time,arg="PUT"),300)



dce<-ggplot(data=delfce, aes(x=first,y=second))+
  geom_point(aes(alpha=delta), color="red") + ggtitle("CALL europejski") +
  theme_void()
dca<-ggplot(data=delfca, aes(x=first,y=second))+
  geom_point(aes(alpha=delta), color="blue") + ggtitle("CALL amerykański") +
  theme_void()
grid.arrange(dce,dca,ncol=2)


dpe<-ggplot(data=delfpe, aes(x=first,y=second))+
  geom_point(aes(alpha=delta), color="red") + ggtitle("PUT europejski") +
  theme_void()
dpa<-ggplot(data=delfpa, aes(x=first,y=second))+
  geom_point(aes(alpha=delta), color="blue") + ggtitle("PUT amerykański") +
  theme_void()
grid.arrange(dpe,dpa,ncol=2)

