# ALL DATA IS FROM PAGE 7 of the Policy Brief report
#
#
# ANALYSIS 1:
#
# Proportions of participants reporting symptoms during the experiment.
# 6 out of 11, who had reported getting symptoms from (infra)sounds emitted from wind turbines, reported symptoms.
# 2 out of 16, who had reported NOT getting symptoms from (infra)sounds emitted from wind turbines, reported symptoms. 
#
# Thus:
# P1 = 6/11
# P2 = 2/16
#
# Prior = Beta(1, 1)
#

# 1) Plot posterior distributions:

curve(dbeta(x, 1 + 6, 1 + 11 - 6), ylim = c(0, 4.0), axes = F,
      ylab = "Tiheys", xlab = "P(Raportoi oireen)")
curve(dbeta(x, 1 + 2, 1 + 16 - 2), add = T, lty = 2)
axis(side = 1); axis(side = 2)

# 2) Use random sampling to approximate difference between distributions:

y1_1 = rbeta(1e6, 1 + 6, 1 + 11 - 6)
y2_1 = rbeta(1e6, 1 + 2, 1 + 16 - 2)

diff_1 = y1_1 - y2_1

# Plot the approximated distribution:

hist(diff_1, prob = T, ylab = "Tiheys",
     xlab = "Ero ryhmien välillä", main = "", breaks = seq(-1, 1, 0.05))
qs = quantile(diff_1, c(0.025, 0.25, 0.75, 0.975))
points(qs[c(1,4)], c(0, 0), lwd = 8, type = "l", lend = 1)
points(qs[c(2,3)], c(0, 0), lwd = 15, type = "l", lend = 1)


# ANALYSIS 2:
# Condition in which the participants were told they were exposed to infrasounds, even though
# in reality they weren't. 
# Six people in total reported symptoms. 
# Five of them had reported getting symptoms from infrasounds; one had reported not getting symptoms. 
# Thus:
#
# P1 = 5/11
# P2 = 1/16
#
# Prior is again Beta(1, 1)
#

curve(dbeta(x, 1 + 5, 1 + 11 - 5), ylim = c(0, 7.0), axes = F,
      ylab = "Tiheys", xlab = "P(Raportoi oireen)")
curve(dbeta(x, 1 + 1, 1 + 16 - 1), add = T, lty = 2)
axis(side = 1); axis(side = 2)

y1_2 = rbeta(1e6, 1 + 5, 1 + 11 - 5)
y2_2 = rbeta(1e6, 1 + 1, 1 + 16 - 1)

diff_2 = y1_2 - y2_2

hist(diff, prob = T, ylab = "Tiheys",
     xlab = "Ero ryhmien välillä", main = "", breaks = seq(-0.5, 1, 0.05))
qs = quantile(diff_2, c(0.025, 0.25, 0.75, 0.975))
points(qs[c(1,4)], c(0, 0), lwd = 8, type = "l", lend = 1)
points(qs[c(2,3)], c(0, 0), lwd = 15, type = "l", lend = 1)
