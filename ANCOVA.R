## R code to perform an ANCOVA,  using the Nambour skin cancer prevention trial as an example
# read in the data
nambour = read.table("nambour.csv", sep=",", header=T)
# log-transform the data
nambour$l_betac_b = log(nambour$betac_b)
nambour$l_betac_f = log(nambour$betac_f)
# Calculate the baseline mean
meanb = mean(nambour$l_betac_b)
# Difference the baseline mean from every baseline observation
nambour$adiff = nambour$l_betac_b-meanb
#ANCOVA using lm
model = lm(formula=l_betac_f~adiff+group, data=nambour)
summary(model)
