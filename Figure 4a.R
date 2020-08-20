#Competing-risk nomogram construction
install.packages("mstate")
install.packages("rms")

training_data <- read.table("Figure 4a-b.csv",sep = ",", head = T,row.names=1)
library(mstate)
training_data$size <- factor(training_data$size)
training_data$differentiation2 <- factor(training_data$differentiation2)
training_data$Nstage <- factor(training_data$Nstage)

Competing_nomogram <- crprep("DFS",status=training_data$group2,data=training_data,cens=0,trans=c(1,2),
                             keep=c("Collagen","size","differentiation2","Nstage"))
library(rms)
ddist<-datadist(Competing_nomogram)
options(datadist='ddist')
Model1 <- cph(Surv(Tstart,Tstop,status==1)~Collagen+size+differentiation2+Nstage,data=Competing_nomogram,
              subset=failcode==1,surv=T)
surv1 <- Survival(Model1)
nom.sur <- nomogram(Model1,fun=list(function(x)1-surv1(12,x),function(x)1-surv1(24,x),function(x)1-surv1(36,x)),
                    fun.at=list(c(0.05,0.1,0.15,seq(.2,.6,by=.1)),c(0.1,0.15,seq(.2,.9,by=.1)),c(0.1,0.15,seq(.2,.9,by=.1))),
                    funlabel=c("1-year Probability of peritoneal metastasis","2-year Probability of peritoneal metastasis",
                               "3-year Probability of peritoneal metastasis"),lp=F)
plot(nom.sur)
