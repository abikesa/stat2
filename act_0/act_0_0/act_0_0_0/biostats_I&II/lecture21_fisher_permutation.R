#################################
# Make the matrix
#################################
rm(list = ls())
library(pbapply)
dat <- matrix(c(4, 1, 2, 3), 2, byrow = TRUE)
colnames(dat) = c("Tumor", "None")
rownames(dat) = c("Treatment", "Control")
fisher.test(dat, 
            alternative = "greater")

real_dat = cbind(trt = rep(c("Treatment", "Control"), each = 5),
             y = c("Tumor", "Tumor", "Tumor", "Tumor", 
                   "None", "Tumor", "Tumor", "None", "None", "None"))
real_dat = data.frame(real_dat, stringsAsFactors = FALSE)

n_each_group = rowSums(dat)
n_outcome = colSums(dat)
# dhyper - hypergeometric distribution
# assuming there were going to be 6 tumors/4 none - fixing other margins
# black ball - no tumor
# white ball - tumor
this_table_prob = dhyper(x = dat["Treatment", "Tumor"], 
       m = n_outcome["Tumor"], 
       n = n_outcome["None"], 
       k = n_each_group["Control"])


# black ball - number of treatment
# white ball - number of control
dhyper(x = dat["Treatment", "Tumor"], 
       m = n_each_group["Treatment"], 
       n = n_each_group["Control"], 
       k = n_outcome["Tumor"])
dhyper(x = dat["Treatment", "Tumor"] + 1, 
       m = n_each_group["Treatment"], 
       n = n_each_group["Control"], 
       k = n_outcome["Tumor"])
# this is zero because it doesn't make sense (choosing more than exist)
dhyper(x = dat["Treatment", "Tumor"] + 2, 
       m = n_each_group["Treatment"], 
       n = n_each_group["Control"], 
       k = n_outcome["Tumor"])

pval = dhyper(x = dat["Treatment", "Tumor"], 
         m = n_each_group["Treatment"], 
         n = n_each_group["Control"], 
         k = n_outcome["Tumor"])
added_pval = 1 # just to start
i = 1
# while loop to make "more extreme tables"
while (added_pval > 0) {
  added_pval = dhyper(x = dat["Treatment", "Tumor"] + i, 
                      m = n_each_group["Treatment"], 
                      n = n_each_group["Control"], 
                      k = n_outcome["Tumor"])
  pval = pval + added_pval
  i = i + 1
}


###################################
# Simulate a bunch of data
###################################
n_permutations = 1000
sim = sapply(seq(n_permutations), function(x) {
  sample(real_dat$y)
})
dim(sim)
# new_y = sim[,1]

###################################
# Simulate a bunch of data
###################################
get_one_sided_statistic = function(new_y) {
  tab = table(real_dat$trt, new_y)
  ptab = prop.table(tab, 1)
  ptab["Treatment", "Tumor"] > ptab["Control", "Tumor"]
}

###################################
# get a p-value
###################################
stats = pbapply(sim, 2, get_one_sided_statistic)
mean(stats) # p-value
fisher.test(dat, simulate.p.value = TRUE, 
            alternative = "greater")

# From ?fisher.test 
# Two-sided tests are based on the probabilities of the tables, 
# and take as 'more extreme' all tables with probabilities less 
# than or equal to that of the observed table, the p-value 
# being the sum of such probabilities.
fisher.test(dat, alternative = "two.sided")



# How many permutations really exist?
# Tumor could be 0-5 for treatment
trt_tumor = 0:5
control_tumor = 0:5
eg = expand.grid(trt = trt_tumor,
                 control = control_tumor)
eg$n_tumor = eg$trt + eg$control
# here we have to do some conditioning
# we need 6 tumors 
# we could have conditioned also where trt = 4, but below code for dhyper 
# needs to change
eg = eg[ eg$n_tumor == 6,]
trt = eg[1,"trt"]
control = eg[1,"control"]

make_tab = function(trt, control) {
  dat <- matrix(c(trt, 5 - trt, control, 5 - control), 2, byrow = TRUE)
  colnames(dat) = c("Tumor", "None")
  rownames(dat) = c("Treatment", "Control")
  n_each_group = rowSums(dat)
  n_outcome = colSums(dat)  
  # conditioned on number of tumors
  dhyper(x = dat["Treatment", "Tumor"], 
         m = n_outcome["Tumor"], 
         n = n_outcome["None"], 
         k = n_each_group["Control"])  
}

# let's get the probabilities
eg$res = pbapply(eg, 1, function(x) {
  trt = x["trt"]
  control = x["control"]
  make_tab(trt, control)
})

# add up those that have a probability less than or equal to the observed 
# probability
eg$real_p = this_table_prob
eg$keep = eg$res <= eg$real_p
sum(eg$res[ eg$keep])

