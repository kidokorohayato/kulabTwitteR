hyou <- matrix(c(20,25,30,20,45,30,35,30,20,10,25,35,50,45,60,60,30,20,60,40,30,25,80,35,105,45,60,130),ncol=7,nrow=4)

hyou

free <- function(n){
	a <- ncol(n)-1;
	b <- nrow(n)-1;
	ans <- a*b;
	as.numeric(ans)
}

free(hyou)
t.test <- t.test(hyou,alternative="greater")

chisq.test(hyou,correct=F)
fisher.test(hyou)

t.test$p.value

t.test(hyou,var.equal=F)
sd(hyou)
mean(hyou)
sampling(hyou,10)

sample <- sample(hyou,size=10,replace=F)
sample
sd(sample)

mean(sample)

t <- (mean(sample)-mean(hyou))/(sd(hyou)/sqrt(free(hyou)))
power.t.test(delta=(mean(sample)-mean(hyou)),sd=sd(hyou),sig.level=0.95)
wilcox.test(hyou,paird=FALSE)

nit <- function(n){
	ans<- 2*n*log(n)

	if(is.nan(ans)){
		ans<-0
　	}			
	as.numeric(ans)
}
HA <- function(n){
	
	Ha <- sum(sapply(n,nit))
	Ha
}
IA <- function(n){
	
	as.numeric(nit(sum(n))-HAB(n))
}

IA(hyou)


HAB <- function(n){
	
	Hab <- sum(sapply(n,nit))
	Hab
}
 IBIA <- function(n){
 	
 	HA(n) - HAB(n)
 }
 IAlB <- function(n){
 	
 	sum(sapply(rowSums(n),nit))-HAB(n)
 }
 
 IAB <- function(n){
 	
 	Iab <- nit(sum(n)) - HAB(n)
 }
IAxB <-function(n){
	invisible(nit(sum(n))-HA(colSums(n))-IAlB(n))
	
}

 HAB(hyou)
 IAIB(hyou)
 IAxB(hyou) 
 IAlB(hyou)
 
 
 f.test <- qchisq(0.95,df=18)
 f.test
 
 hyou
 
 kentei.yui <- function(n){
 	ans1 <- IAxB (n)
 	a <- (ncol(n)-1)*(nrow(n)-1)
 	ans2 <- qchisq(0.95,df=a)
 	cat("IA×IB=" ,ans1 ,"\n")
 	cat("自由度" ,ans2 ,"\n")
 	if(ans1 > ans2){
 		cat("有意である")
 	}else if(ans1 > ans2){
 		cat("有意でない")
 	}
 	
}

kentei.yui(hyou)
source("http://aoki2.si.gunma-u.ac.jp/R/src/all.R", encoding="euc-jp")
