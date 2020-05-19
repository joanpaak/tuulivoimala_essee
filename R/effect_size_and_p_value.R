#
# It is possible that inferences about there being "no differences" between groups is 
# based on p-values in the  Policy  Brief (11/2020). However, since the sample sizes 
# are rather small, 11 in one group and 16 in the other, the effect sizes should've
# been more than large (usually defined as > 0.8) in order to reach statistical sig-
# nificance on the typical level (p < 0.05).
# 
# This R script plots two-sided p-value for independent samples t-test with unequal 
# sample sizes as a function of the effect size. This is done by keeping the difference
# between the groups constant (1.0) and varying the standard deviation in each group. 
# The standard deviation is assumed to be the same in both groups.
#
# Effect size is here defined simply as Cohen's d. 
# 
# Sources:
# Independent samples t-test from section 5.2.3 of https://en.wikipedia.org/wiki/Student%27s_t-test
# Cohen's d from https://www.socscistatistics.com/effectsize/default3.aspx
#

n_1 = 11
n_2 = 16

diff_bw_groups = 1.0

sdev = seq(0.75, 10, length.out = 10000)

cohen_d = diff_bw_groups / sqrt((sdev^2 + sdev^2) / 2)

t_stat = diff_bw_groups /
  (sqrt(((n_1 - 1) * sdev^2 + (n_2 - 1) * sdev^2) / (n_1 + n_2 - 2)) * 
   sqrt(1 / n_1 + 1 / n_2))

two_sided_p_value = (1.0 - pt(t_stat, df = n_1 + n_2 - 2)) * 2

plot(cohen_d, two_sided_p_value, type = "l", 
     ylab = "p-arvo", xlab = "Efektikoko", lwd = 2, axes = F)
axis(side = 1, at = seq(0, 2, 0.2))
axis(side = 2, at = seq(0, 1, 0.2))
abline(h = seq(0, 1, 0.2), lty = 3)
abline(v = seq(0, 2, 0.2), lty = 3)

abline(h = 0.05, lty = 1, col = "red", lwd = 2)
abline(v = cohen_d[which.min((0.05 - two_sided_p_value)^2)], col = "red", lwd = 2)
