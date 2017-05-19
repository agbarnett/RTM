## R code to calculate the expected RTM effect,  Equations (1) and (2)
# Change these parameters depending on your data;
sigma = 15; # total std;
mu = 60; # population mean;
cut = 40; # cut-off;
# Loops to run through rho and m scenrarios;
sigma2_w=vector(length=11, mode="numeric")
sigma2_b=vector(length=11, mode="numeric")
Rl=vector(length=11, mode="numeric")
Rg=vector(length=11, mode="numeric")
rho=vector(length=11, mode="numeric")
for (rhox in 0:10){
  rho[rhox + 1] = rhox / 10
  sigma2_w[rhox + 1] = (1-rho[rhox + 1])*(sigma^2); # within-subject variance;
  sigma2_b[rhox + 1] = rho[rhox + 1]*(sigma^2); # between-subject variance;
  for (m in 1:1){ # Number of baseline measurements;
    zg = (cut-mu) / sigma; # z;
    zl = (mu-cut) / sigma; # z;
    x1g = dnorm(x=zg); # phi - probability density;
    x2g = 1-pnorm(q=zg); # Phi - CDF
    x1l = dnorm(x=zl); # phi;
    x2l = 1-pnorm(q=zl); # Phi;
    czl = x1l / x2l; # C(z) in paper;
    czg = x1g / x2g; # C(z) in paper;
    Rl[rhox + 1] = (sigma2_w[rhox + 1] / m) / sqrt(sigma2_b[rhox + 1] + (sigma2_w[rhox + 1] / m))*czl; # RTM effect,  Equations (1) m=1 & (2) m>1;
    Rg[rhox + 1] = (sigma2_w[rhox + 1] / m) / sqrt(sigma2_b[rhox + 1] + (sigma2_w[rhox + 1] / m))*czg; # RTM effect;
  }
}
output = cbind(sigma2_b, sigma2_w, rho, Rl, Rg)
print("The expected RTM effect for a range of baseline samples sizes and rhos")
print(output)
print("sigma2_b=between-subject variance,  sigma2_w=within-subject variance")
print("rho=within-subject correlation,  Rl=RTM effect (<cut-off),  Rg=RTM effect (>cut-off)");


