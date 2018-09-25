libname motiv 'C:\Users\vcole\Desktop\class_demos';

data motiv;
set motiv.motivation;
run;
proc print data=motiv (obs=20);
run;

/*we see that goalstruct varies by classid and 
sex, RelPerf, and intrinsic vary by studentid.
sex is dummy coded (1=male)*/

/*spaghetti plot to visualize variation between classrooms*/
goptions reset=all  ;
proc sgplot data=motiv;
where classid<21; /*request a subset so plot doesn't get too crowded*/
title 'Within-classroom association relperf and intrinsic';
reg x=relperf y=intrinsic/group=classid name='Classroom ID';
keylegend 'Classroom ID';
run;

/*what is the ICC of the DV?*/
proc mixed data=motiv method=reml covtest; 
/*reml is a less biased estimation method than ml*/
/*covtest provides significance tests for variance components
(variance components are the variance estimates for the random
effects -- the random intercept and (if applicable) the random slope(s)*/ 
class classid; /*put the Level 2 ID here*/
model intrinsic=/s ; /*no predictors for null model*/
/*"s" asks for fixed effects to be printed. */
random intercept/subject=classid vcorr;
/*want the intercept for intrinsic motivation to be allowed to
vary randomly across classrooms. vcorr asks for the ICC to be printed*/
run;
/*ICC=.25. 25% of the variance in a student's intrinsic motivation is explained
by classroom membership*/



/*MORE ON CENTERING: DATA MANAGEMENT*/
/*First, we will group mean-center "relperf"*/
proc sort data=motiv;
by classid;
run;
/*computing class-mean relperf and outputting into a new dataset
called "classmean". The class means for relperf are called "relperf_class"
Class means of goalstrct will have the same values but we want to keep
this variable in the class-level file that is being created so we can
calculate its grand mean*/
proc means data=motiv noprint;
by classid;
var relperf GoalStrct;
output out=classmean  mean=relperf_class goalstrct;
run;
proc print data=classmean;
run;
/*calculating the grand mean*/
proc means data=classmean;
var relperf_class GoalStrct ;
run;
data classmean;
set classmean;
drop _TYPE_ _FREQ_; /*extra variables created in last step*/
relperf_class_grdmc=relperf_class-3.1690349; /*subtracting the grand mean*/
goalstrct_grdmc=goalstrct-2.8851064; /*subtracting the grand mean*/
run;
/*merge class means with original data*/
data motiv;
merge motiv classmean;
by classid;
run;
/*group-mean-center relperf*/
data motiv;
set motiv;
relperf_grpmn=relperf-relperf_class; /*subtract the un-centered class mean*/
run;
proc print data=motiv (obs=20);
run;

/*MORE ON CENTERING: INTERPRETING THE RESULTS*/
/*adding fixed effects (simultaneous)*/
/*random intercept model*/
proc mixed data=motiv method=reml covtest;
class classid;
/*all predictors go on the same line (Level 1 & Level 2 together)*/
model intrinsic=sex relperf_grpmn relperf_class_grdmc goalstrct_grdmc/s;
random intercept/subject=classid ;
run;

/*RANDOM INTERCEPTS: FITTING THE VISUALIZING THE RESULTS*/
/*outputting random intercept estimates and fixed effect estimates*/
/*same model as above*/
/*this line saves the random intercept values into a new file called 'randomeffects'
and the fixed effect estimates into a new file called 'fixedeffects'*/
ods output SolutionR=randomeffects ;
proc mixed data=motiv method=reml covtest;
class classid;
/*all predictors go on the same line (Level 1 & Level 2 together)*/
model intrinsic=sex relperf_grpmn relperf_class_grdmc goalstrct_grdmc/s;
random intercept/subject=classid s; /*adding "s" to random line
requests estimates of each classroom's random intercept*/
run;

/*one record per classroom; contains a value of each classroom's
deviation from the grand mean of intrinsic motivation*/
proc print data=randomeffects;
run;

data randomeffects;
set randomeffects;
InterceptEstimate=Estimate+6.2560; /*adding the fixed intercept to obtain each
classroom's expected intrinsic value when all predictors are '0'*/
keep classid InterceptEstimate;
run;

proc sort data=randomeffects;
by classid;
run;
proc sort data=motiv;
by classid;
run;
/*merging classroom-specific intercept values with original data*/
data motiv;
merge motiv randomeffects;
by classid;
run;
/*creating variables equal to the fixed effect slope estimates*/
data motiv;
set motiv;
grpmc_relperf_Estimate=.218;
sex_Estimate=-0.00471;
grdmc_relperf_Estimate=-0.2328;
grdmc_goalstruct=-.8025;
run;
/*creating expected value for intrinsic motivation. I could have pasted the
fixed effect parameter estimates directly into the equation below but wanted to 
make the equation more general for later demonstratiaon of random slopes*/
data motiv;
set motiv;
exp_intrinsic=InterceptEstimate+sex_estimate*sex+grpmc_relperf_estimate*relperf_grpmn
+grdmc_relperf_estimate*relperf_class_grdmc+grdmc_goalstruct*goalstrct_grdmc;
run;

/*illustrating the within-classroom relperf effects implied by the
random intercept only model*/
goptions reset=all  ;
proc sgplot data=motiv;
where classid<21; /*request a subset so plot doesn't get too crowded*/
title 'Model-implied within-classroom association betweem
relperf and expected value of intrinsic: Random intercept only model';
reg x=relperf y=exp_intrinsic/group=classid name='Classroom ID';
keylegend 'Classroom ID';
run;
goptions reset=all;

/*We will now estimate a model with a random slope of relative performance
and output each classroom's estimate of this slopes.*/
ods output SolutionR=randomeffects_un ;
proc mixed data=motiv method=reml covtest;
class classid;
model intrinsic=sex relperf_grpmn relperf_class_grdmc goalstrct_grdmc/s;
random intercept relperf_grpmn/subject=classid type=un s;
/*specifying an unstructured covariance matrix for the random effects*/
run;

/*We will now manipulate the "randomeffects_un" dataset to get predicted values
of intrinsic motivation for each individual case under a random slopes model.*/
/*This isn't absolutely necessary for you to be able to replicate -- it's just 
good code to have if you ever want to make plots of model-implied values.*/
proc print data=randomeffects_un;
run;
data randomeffects_un;
set randomeffects_un;
if Effect="Intercept" then newEstimate=Estimate+6.2529; 
if Effect="relperf_grpmn" then newEstimate=Estimate+0.3092;
keep classid Effect newEstimate;
run;

/*flip to wide*/
proc sort data=randomeffects_un;
by classid;
run;
data randomeffects_un;
set randomeffects_un;
if Effect="Intercept" then parmnumber=1;
if Effect="relperf_grpmn" then parmnumber=2;
run;
data wide;
set randomeffects_un;
by classid;
keep classid Int_est relperfgrpmnSlope_est;
retain Int_est relperfgrpmnSlope_est;
array aest {2} Int_est relperfgrpmnSlope_est;
if first.classid then do;
do i=1 to 2;
aest(i)=.;
end; end;
aest(parmnumber)=newEstimate;
if last.classid then output;
run;
proc print data=wide;
run;

proc sort data=wide;
by classid;
run;
proc sort data=motiv;
by classid;
run;
/*merging classroom-specific intercept and slope values with original data*/
data motiv3;
merge motiv wide;
by classid;
run;
/*creating variables equal to the fixed effect slope estimates*/
data motiv3;
set motiv3;
sex_Estimate=0.001772;
grdmc_relperf_Estimate=-0.2192;
grdmc_goalstruct=-0.8420;
run;
/*creating new expected value for intrinsic motivation.*/
data motiv3;
set motiv3;
exp_intrinsic=Int_Est+sex_estimate*sex+relperfgrpmnSlope_est*relperf_grpmn
+grdmc_relperf_estimate*relperf_class_grdmc+grdmc_goalstruct*goalstrct_grdmc;
run;

/*illustrating the within-classroom relperf effects implied by the
random intercept only model*/
goptions reset=all  ;
proc sgplot data=motiv3;
where classid<21; /*request a subset so plot doesn't get too crowded*/
title 'Model-implied within-classroom association relperf and expected value of intrinsic: Random slope model';
reg x=relperf y=exp_intrinsic/group=classid name='Classroom ID';
keylegend 'Classroom ID';
run;


/*CROSS-LEVEL INTERACTIONS*/
/*Review: The model we just fit has only main effects. Let's rerun it to remind ourselves.*/
proc mixed data=motiv method=reml covtest;
class classid;
model intrinsic=sex relperf_grpmn relperf_class_grdmc goalstrct_grdmc/s;
random intercept relperf_grpmn/subject=classid type=un ;
run;

/*Now let's add a cross-level interaction between each the classroom-level variable "goalstrct" 
(the extent to which each classroom emphasizes performance) and the person-level variable "relperf"
(the extent to which each student is focused on his or her relative performance). */
proc mixed data=motiv method=reml covtest;
class classid;
model intrinsic=sex relperf_grpmn relperf_class_grdmc goalstrct_grdmc
relperf_grpmn*goalstrct_grdmc  /*add an interaction term to the model statement*/
/*SAS does not "care" whether the interaction is a cross-level interaction or 
any other type of interaction because it uses mixed notation so everything
goes on one line*/
/s;
random intercept relperf_grpmn/subject=classid type=un ;
run;

/*Probing interactions*/
/*We want to see what each individual's predicted value of "relperf" is at informative values 
of "goalstrct_grdmc". So we get the 25th, 50th, and 75th percentile values of "goalstrct_grdmc".*/
proc univariate data=motiv;
var goalstrct_grdmc;
histogram;
run;


/*Now we will rerun the model we just ran, but add in "estimate" statements to get the 
predicted simple intercepts and slopes at each value of "goalstrct_grdmc"*/
proc mixed data=motiv method=reml covtest;
class classid;
model intrinsic=sex relperf_grpmn relperf_class_grdmc goalstrct_grdmc
relperf_grpmn*goalstrct_grdmc  /s;
random intercept relperf_grpmn/subject=classid type=un ;
/*plug in moderator values to calculate simple intercepts and slopes*/
/*other variables are fixed to 0 if there are not included here*/
/*for simple intercepts, always include a '1' after intercept to add the constant in*/
/*for simple slopes, want to know the effect of a one unit increase in x
conditional on the value of the moderator, so plug in 1*moderator value = moderator value*/
estimate '25th percentile intercept' intercept 1 goalstrct_grdmc -.89; 
estimate '25th percentile slope' relperf_grpmn 1 relperf_grpmn*goalstrct_grdmc -.89;
estimate '50th percentile intercept' intercept 1 goalstrct_grdmc .11; 
estimate '50th percentile slope' relperf_grpmn 1 relperf_grpmn*goalstrct_grdmc .11;
estimate '75th percentile intercept' intercept 1 goalstrct_grdmc .51; 
estimate '75th percentile slope' relperf_grpmn 1 relperf_grpmn*goalstrct_grdmc .51;
run;

/*Now we want to probe this interaction using the online plotting utility, so we need
the asymptotic covariance matrix ("covb"). This is the matrix of the error in parameter estimates.
Optional fun fact: The square root of each diagonal element is each parameter's standard error!*/
proc mixed data=motiv method=reml covtest;
class classid;
model intrinsic=sex relperf_grpmn relperf_class_grdmc goalstrct_grdmc
relperf_grpmn*goalstrct_grdmc  /s covb; 
/*covb ouputs the asymptotic covariance matrix for the regression coefficients*/
run;

/*Since we will be plotting the effect of "relperf_grpmn" over the range of "goalstrct_grdmc"
and vice versa, we need to know what these ranges are in order to plot them informatively.*/
proc means data=motiv;
var relperf_grpmn;
run;
proc means data=motiv;
var goalstrct_grdmc;
run;
