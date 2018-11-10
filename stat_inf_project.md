Statistical inference with the GSS data
---------------------------------------

### Load packages

    options(repos="https://cran.rstudio.com" )
    library(ggplot2)
    library(dplyr)
    library(statsr)
    library(magrittr)
    library(doBy)

### Load data

    load("gss.Rdata")

------------------------------------------------------------------------

Part 1: Data
------------

I think the data set is generalizable. Because the data set was sampled
from GSS Data though it was pre-processed. In the 'GSS.html'
documentation,"Unlike the full General Social Survey Cumulative File, we
have removed missing values from the responses and created factor
variables when appropriate to facilitate analysis using R.", thus, the
form of the data set was a little changed, but the contents of the data
set was not changed. In the data set, there is no causality. This data
set was collecting data by conducting survay, so there is no assignment.

------------------------------------------------------------------------

Part 2: Research question
-------------------------

Question

In the samples, if the people are satisfied their job, would they
continue their job though they become rich? one of reasons people are
working is to prepare their rest of life when they are too old to work.
I was wondering if people got enough money for their rest of life, would
they stop working although they are satisfied their job?

------------------------------------------------------------------------

Part 3: Exploratory data analysis
---------------------------------

    satjob<-gss%>%select(satjob)
    satjob%<>%filter(satjob!="NA")
    richwork<-gss%>%select(richwork)%>%na.omit
    satjobRichwork<-gss%>%select(satjob,richwork)%>%na.omit
    summary.satjobRichwork<-satjobRichwork%>%group_by(satjob,richwork)%>%summarize(count=n())
    summary.satjobRichwork%<>%group_by(satjob)%>%mutate(totalCount=sum(count))
    summary.satjobRichwork%<>%mutate(prop=round((count/totalCount)*100,2))
    summary.satjobRichwork%>%select(-prop)

    ## # A tibble: 8 x 4
    ## # Groups:   satjob [4]
    ##   satjob            richwork         count totalCount
    ##   <fct>             <fct>            <int>      <int>
    ## 1 Very Satisfied    Continue Working  7708      10245
    ## 2 Very Satisfied    Stop Working      2537      10245
    ## 3 Mod. Satisfied    Continue Working  5647       8538
    ## 4 Mod. Satisfied    Stop Working      2891       8538
    ## 5 A Little Dissat   Continue Working  1394       2163
    ## 6 A Little Dissat   Stop Working       769       2163
    ## 7 Very Dissatisfied Continue Working   544        878
    ## 8 Very Dissatisfied Stop Working       334        878

    summary.satjobRichwork%>%select(-c(count,totalCount))

    ## # A tibble: 8 x 3
    ## # Groups:   satjob [4]
    ##   satjob            richwork          prop
    ##   <fct>             <fct>            <dbl>
    ## 1 Very Satisfied    Continue Working  75.2
    ## 2 Very Satisfied    Stop Working      24.8
    ## 3 Mod. Satisfied    Continue Working  66.1
    ## 4 Mod. Satisfied    Stop Working      33.9
    ## 5 A Little Dissat   Continue Working  64.4
    ## 6 A Little Dissat   Stop Working      35.6
    ## 7 Very Dissatisfied Continue Working  62.0
    ## 8 Very Dissatisfied Stop Working      38.0

    ggplot(satjobRichwork,aes(x=satjob,fill=richwork))+geom_bar(position = "dodge")

![](stat_inf_project_files/figure-markdown_strict/unnamed-chunk-3-1.png)

Above bar plot shows counts of continue working or not of each levels in
'satjob' variable.

    ggplot(satjobRichwork,aes(x=satjob,fill=richwork))+geom_bar(position = "fill")

![](stat_inf_project_files/figure-markdown_strict/unnamed-chunk-4-1.png)

Above bar plot shows proportion of continue working or not of each
levels in 'satjob' variable.

*Interpretation and conclusion*

I used two variables, 'satjob' and 'richwork', which are categorial
values. The satjob variable describes how I satisfy my current job and
the richwork variable describes if she or he got enough money for rest
of my life, would they have stopped their work. I removed 'NA' values in
two variables. The satjob had 41288 objects and the richwork had 21948
objects. Thus, I could only use 21948 objects of two variables. I made
'count' variable that is getting Continue Working count and Stop Working
count in the richwork variable and grouped by satjob variable. I should
get ratio of the count of Coninue Working via Stop Working count of each
level in the satjob variable. Because, The number of total working(i.e
Continue Working and Stop Working) objects of each levels in the satjob
variable are different. Thus, I made 'totalCount' variable to get total
working count of the each levels in the satjob variable. Finally, I
divided 'count' to 'totalCount' to get the ratio of Continue Working
versus the ratio of Stop Working of the each levels in the 'satjob'
variable. In conclusion, people who are more satisfied their job have
higher ratio of Continue Working than ratio of Stop Working.

------------------------------------------------------------------------

Part 4: Inference
-----------------

*Hypotheses*

People who are satisfied their job want to continue their working more
than people who are not satisfied their job.

H0 : The population proportion of people who are satisfied their job
want to continue their working is equal to the population proportion of
people who are not satisfied their job want to continue their working

HA : The population proportion of people who are satisfied their job
want to continue their working is not equal to the population proportion
of people who are not satisfied their job want to continue their working

*Varaibles*

satjob: categorial variable, 4 levels

satjob.f: categorial variable, 2 levels. I reduced satjob levels. The
original level was 4,but I combined "Very Satisfied" and "Mod.
Satisfied" values as "Satisfied" and "A Little Dissat" and "Very
Dissatisfied" as "Dissatified".

    gss$satjob.f<-factor(recodeVar(gss$satjob,src = list(c("Very Satisfied","Mod. Satisfied"),c("A Little Dissat","Very Dissatisfied")),tgt = list("Satisfied","Dissatisfied")))

richwork: categorial variable, 2 levels

*Check condition*

1.Each group is a simple random sample from less than 10% of the
population, the observation are independent, both within the samples and
between the samples.

2.Success-failure condition are met Thus, the normal model can be used
for the point estimate of the difference. *Method*

I used theoretical method. Because, the variables that I used were
categorial values and the my sample was met the conditions. For testing
the hypothesis,I used the pooled proportion for p1-p2 when H0 is true.

    modiSatjob<-gss%>%select(satjob.f,richwork)%>%na.omit
    modiSatjob%<>%group_by(satjob.f,richwork)%>%summarize(count=n())
    modiSatjob%<>%group_by(satjob.f)%>%mutate(totalCount=sum(count))
    modiSatjob%<>%mutate(prop=round((count/totalCount)*100,2))
    modiSatjob

    ## # A tibble: 4 x 5
    ## # Groups:   satjob.f [2]
    ##   satjob.f     richwork         count totalCount  prop
    ##   <fct>        <fct>            <int>      <int> <dbl>
    ## 1 Dissatisfied Continue Working  1938       3041  63.7
    ## 2 Dissatisfied Stop Working      1103       3041  36.3
    ## 3 Satisfied    Continue Working 13355      18783  71.1
    ## 4 Satisfied    Stop Working      5428      18783  28.9

    pHat<-round((modiSatjob$count[1]+modiSatjob$count[3])/(modiSatjob$totalCount[1]+modiSatjob$totalCount[3]),2)
    # # of people who choose continue working/# of people in the entire study
    Sat.pHat<-round(modiSatjob$count[3]/modiSatjob$totalCount[3],4)
    #The point estimate of Satisfied people
    Dis.pHat<-round(modiSatjob$count[1]/modiSatjob$totalCount[1],4)
    #The point estimate of Dissatisfied people
    pointEstimate<-Sat.pHat-Dis.pHat
    #The point estimate of the difference of two point estimate
    standardError<-round(sqrt((pHat*(1-pHat)/modiSatjob$totalCount[1])+(pHat*(1-pHat)/modiSatjob$totalCount[3])),5)
    # Standard error is calculated using pooled proportion
    z<-round((pointEstimate-0)/standardError,4)
    # Getting test score
    result<-c("PHat"=pHat,"Sat.pHat"=Sat.pHat,"Dis.pHat"=Dis.pHat,"pointEstimate"=pointEstimate,"standardError"=standardError,"z"=z)
    result

    ##          PHat      Sat.pHat      Dis.pHat pointEstimate standardError 
    ##       0.70000       0.71100       0.63730       0.07370       0.00896 
    ##             z 
    ##       8.22540

*Inference conclusion*

The test score(Z-score) was too big not to get p-value with 'qnorm'
function or appendix sheet of z-score. The p-value was less than
significance level(0.05) thus, I rejected H0 in favor of HA.
