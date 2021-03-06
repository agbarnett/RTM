* SAS code to perform an ANCOVA, using the Nambour skin cancer prevention trial as an example
* Read in the Nambour data (n=96);
* Note!: treatment codes are scrambled from those in paper and will not give the same results;
data work.nambour;
input betac_b betac_f group @@;
datalines;
0.04 1.29 0 0.44 1.36 1 2.7 1.5 0 0.34 2.62 0
0.65 0.03 0 0.7 5.6 1 1 0.43 1 2.25 4.91 0
0.22 0.2 0 0.03 0.16 0 0.49 0.42 1 0.63 0.48 0
0.34 0.24 1 1.67 3.9 1 0.23 0.22 1 0.7 0.75 0
0.16 0.32 1 0.8 0.31 1 0.22 1.6 0 0.16 0.42 1
1.08 0.72 1 0.49 0.64 1 0.73 4.8 0 1.1 0.63 1
0.86 0.87 1 1.24 0.32 1 0.4 0.3 0 1.1 4.45 0
0.36 0.32 0 0.47 2.24 0 0.92 2.72 0 0.41 0.43 0
0.13 0.13 1 0.44 1.69 1 0.12 0.1 1 0.37 0.13 1
0.44 0.26 1 0.91 1.46 0 0.67 0.45 0 1.01 0.47 1
0.91 0.36 1 0.56 1.46 0 0.82 0.62 0 0.96 0.79 1
0.18 0.61 0 0.65 0.34 0 1.8 1.2 0 1.5 1 1
0.11 0.55 0 2.41 2.4 0 1.36 2.73 1 0.57 1.62 0
0.34 0.43 1 0.47 0.1 1 0.48 6.55 0 0.78 2.1 0
0.48 0.42 1 0.51 0.65 0 0.27 0.15 1 1.13 0.94 0
0.24 0.51 0 0.6 5.5 1 0.55 0.47 0 0.72 2.63 0
1 1.77 1 0.6 3.41 1 0.24 0.17 1 0.27 0.26 0
0.33 1.82 1 0.33 0.31 1 0.49 0.58 0 0.46 0.91 1
1.17 2.04 0 0.31 2.4 1 0.17 0.32 1 0.19 1.65 1
0.32 0.51 1 0.56 0.54 1 1.31 1.13 0 0.11 1.34 1
0.42 0.78 1 1.29 3.31 1 0.22 0.36 1 0.51 0.55 0
0.17 0.16 1 0.64 0.54 1 1.02 0.02 0 0.28 2.26 1
0.22 0.16 0 0.26 1.62 0 1.04 0.98 1 0.13 1.01 1
0.14 0.17 1 1.2 4.5 0 0.08 0.13 0 1.24 11 1
;
run;
* Log-transform the betacarotene data;
data work.lognambour;
set work.nambour;
dummy=1;
l_betac_b=log(betac_b);
l_betac_f=log(betac_f);
label l_betac_b='Log-transformed baseline betacarotene'
l_betac_f='Log-transformed follow-up betacarotene'
group='Treatment group';
run;
* Calculate the baseline mean;
proc univariate data=work.lognambour noprint;
by dummy;
var l_betac_b;
output out=work.bstats mean=meanb;
run;
* Difference the baseline mean from every baseline observation;
data work.meandiff;
merge work.lognambour work.bstats;
by dummy;
adiff=l_betac_b-meanb; * Baseline-mean;
run;
* ANCOVA;
proc genmod data=work.meandiff;
class group;
model l_betac_f=adiff group; * Follow-up=(baseline-mean);
estimate 'Treatment (drug-plac)' group 1 -1;
run; quit;