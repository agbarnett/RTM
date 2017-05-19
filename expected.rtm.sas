* SAS code to calculate the expected RTM effect, Equations (1) and (2)
data work.rtmeffect;
* Change these parameters depending on your data;
sigma=15; * total std;
mu=60; * population mean;
cut=40; * cut-off;
* Loops to run through rho and m scenrarios;
do rho=0.0 to 1.00 by 0.1; * within-subject correlation;
sigma2_w=(1-rho)*(sigma**2); * within-subject variance;
sigma2_b=rho*(sigma**2); * between-subject variance;
do m=1 to 1; * Number of baseline measurements;
zg=(cut-mu)/sigma; * z;
zl=(mu-cut)/sigma; * z;
x1g=pdf('Normal',zg); * phi - probability density;
x2g=1-cdf('Normal',zg); * Phi - cumulative distribution function;
x1l=pdf('Normal',zl); * phi;
x2l=1-cdf('Normal',zl); * Phi;
czl=x1l/x2l; * C(z) in paper;
czg=x1g/x2g; * C(z) in paper;
Rl=(sigma2_w/m)/sqrt(sigma2_b+(sigma2_w/m))*czl; * RTM effect, Equations (1) m=1 & (2) m>1;
Rg=(sigma2_w/m)/sqrt(sigma2_b+(sigma2_w/m))*czg; * RTM effect;
output;
end;
end;
run;
proc sort data=work.rtmeffect;
by rho m;
run;
title "The expected RTM effect for a range of baseline samples sizes and rhos";
proc print data=work.rtmeffect label noobs;
var m rho sigma sigma2_b sigma2_w Rl Rg;
label sigma="std(Y)" sigma2_b="between-subject variance" sigma2_w="within-subject variance" rho="within-subject correlation" Rl="RTM effect (<cut-off)"
Rg="RTM effect (>cut-off)" m="# baseline measurements";
format Rl Rg 10.2 rho 15.3;
run;