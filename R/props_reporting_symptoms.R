
# Proportions of participants reporting symptoms during the experiment.
# 6 out of 11, who reporting getting symptoms from (infra)sounds emitted from wind turbines, reported symptoms. 
# 2 out of 16, who reported NOT getting symptoms from (infra)sounds emitted from wind turbines, reported symptoms. 
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

y1_s = rbeta(1e6, 1 + 6, 1 + 11 - 6)
y2_s = rbeta(1e6, 1 + 2, 1 + 16 - 2)

diff = y1_s - y2_s

hist(diff, prob = T, ylab = "Tiheys",
     xlab = "Ero ryhmien välillä", main = "", breaks = seq(-1, 1, 0.05))
qs = quantile(y1_s - y2_s, c(0.025, 0.25, 0.75, 0.975))
points(qs[c(1,4)], c(0, 0), lwd = 8, type = "l", lend = 1)
points(qs[c(2,3)], c(0, 0), lwd = 15, type = "l", lend = 1)

