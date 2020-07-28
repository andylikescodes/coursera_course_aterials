install.packages('tableone')
install.packages('Matching')
install.packages("MatchIt")

library(tableone)
library(Matching)
data(lalonde, package = 'MatchIt')

#dat = read.csv(file='ladonle.csv')


vars = c("age", "educ", "black", "hispan", "married", "nodegree", "re74", "re75")
table1<- CreateTableOne(vars=vars, strata='treat', data = lalonde, test=FALSE)

print(table1, smd=TRUE)

#propensity score model
psmodel <- glm(treat ~ age + educ + black + hispan + married + nodegree + re74 +
                 re75,
               family  = binomial(link ="logit"), data=lalonde)

set.seed(931139)
ps <-predict(psmodel, type = "response")

psmatch<-Match(Tr=lalonde$treat,M=1,X=ps,replace=FALSE, caliper=0.1)

matched<-lalonde[unlist(psmatch[c("index.treated","index.control")]), ]

table1<- CreateTableOne(vars=vars, strata='treat', data = matched, test=FALSE)
print(table1, smd=TRUE)


#outcome analysis
y_trt<-matched$re78[matched$treat==1]
y_con<-matched$re78[matched$treat==0]

#pairwise difference
diffy<-y_trt-y_con

#paired t-test
t.test(diffy)
6
