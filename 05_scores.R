# =============================================================================
# 05_scores.R — synthetic supplier scores (two-stage Beta model) and the
#               score-distribution fit check
# =============================================================================
# Workbook equivalents: Config!A8:AU47 (scores, firm means), 'Simulation
#   Parameters'!A12:D15, 'RH2 Score Distribution Fit'.
#
# Model, per tier (Tier 1 = Comp1–Comp8, Beta(5,2); Tier 2 = Comp9–Comp40,
# Beta(4,3)); one mulberry32 stream per tier, companies drawn in order:
#   Stage 1  firm mean      m_i  ~ Beta(κ·μ, κ·(1−μ)),  μ = α/(α+β), κ = 30
#   Stage 2  35 criteria    x_ij ~ Beta(7·m_i, 7·(1−m_i))
#   Score                   s_ij = min(5, 1 + floor(5·x_ij))

cat("\n== 05 Synthetic scores ==\n")

companies <- data.frame(id = xl_vec_chr("Config", "A8:A47"),
                        tier = xl_vec_chr("Config", "B8:B47"),
                        alpha = xl_vec_num("Config", "D8:D47"),
                        beta = xl_vec_num("Config", "E8:E47"),
                        stringsAsFactors = FALSE)

generate_scores <- function(companies, kappa = CFG$kappa, ab = CFG$ab_within) {
  n_how <- length(HOQ$how_id)
  S <- matrix(NA_integer_, nrow(companies), n_how,
              dimnames = list(companies$id, HOQ$how_id))
  m <- setNames(numeric(nrow(companies)), companies$id)
  streams <- list("Tier 1" = CFG$seed_tier1, "Tier 2" = CFG$seed_tier2)
  for (tier in names(streams)) {
    smp <- new_samplers(new_rng(streams[[tier]]))
    for (i in which(companies$tier == tier)) {
      mu <- companies$alpha[i] / (companies$alpha[i] + companies$beta[i])
      m[i] <- smp$beta(kappa * mu, kappa * (1 - mu))
      for (j in seq_len(n_how)) {
        x <- smp$beta(ab * m[i], ab * (1 - m[i]))
        S[i, j] <- min(5L, 1L + as.integer(floor(5 * x)))
      }
    }
  }
  list(scores = S, firm_mean = m)
}

SC <- generate_scores(companies)
SCORES <- SC$scores

# ---- score-distribution fit ('RH2 Score Distribution Fit') ----------------
# Expected share of score k integrates P(score = k | m) over the firm-mean
# distribution on a 200-point midpoint grid.
score_fit <- function(tier, a, b, grid_n = 200, kappa = CFG$kappa, ab = CFG$ab_within) {
  idx <- companies$tier == tier
  s <- as.vector(SCORES[idx, ]); n <- length(s)
  mu <- a / (a + b)
  mg <- (seq_len(grid_n) - 0.5) / grid_n
  wm <- pbeta(mg + 0.5 / grid_n, kappa * mu, kappa * (1 - mu)) -
        pbeta(mg - 0.5 / grid_n, kappa * mu, kappa * (1 - mu))
  pk <- sapply(1:5, function(k) pbeta(k / 5, ab * mg, ab * (1 - mg)) -
                                pbeta((k - 1) / 5, ab * mg, ab * (1 - mg)))
  e_m <- as.vector(pk %*% (1:5)); v_m <- as.vector(pk %*% (1:5)^2) - e_m^2
  obs <- tabulate(s, 5); exp_share <- as.vector(wm %*% pk)
  exp_cnt <- exp_share * n
  chi <- sum((obs - exp_cnt)^2 / exp_cnt)
  p1 <- pbeta((1:5) / 5, a, b) - pbeta((0:4) / 5, a, b)
  firm_avg <- rowMeans(SCORES[idx, ])
  list(table = data.frame(score = 1:5, observed = obs, obs_share = obs / n,
                          exp_share = exp_share, exp_count = exp_cnt,
                          diff_pts = (obs / n - exp_share) * 100,
                          chi_contrib = (obs - exp_cnt)^2 / exp_cnt),
       n = n, companies = sum(idx),
       obs_mean = mean(s), ex