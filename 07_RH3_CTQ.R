# =============================================================================
# 07_rq3_ctq.R — RQ3 / RH3: stability of the top-ranked alternative under
#                uncertainty in the CTQ (Quality / Time / Cost) weighting
# =============================================================================
# Workbook equivalents: 'RQ3 Tier 2 Sample' (primary), 'RQ3 Population Sample'
#   (secondary), and the five RH3 worked-example tabs.
# Design: 1,000 common Dirichlet(2,2,2) CTQ draws; a 30-triple pilot sizes the
# study, then an independent 200-triple main sample (no triple repeats the
# pilot). For each triple: baseline winner at equal CTQ shares; stability share
# = fraction of draws in which that winner keeps rank 1; near-tie share =
# fraction of draws with top − runner-up CC < 0.01.

cat("\n== 07 RQ3 CTQ-weight robustness ==\n")

# Closeness coefficients for one alternative set under many weight vectors.
# Because every weight is positive, PIS_j = w_j·max_i r_ij and NIS_j = w_j·min_i r_ij,
# so S± reduce to sqrt(Σ_j w_j² (r_ij − r_max/min,j)²) — identical to the
# long-form TOPSIS, computed for all draws at once.
cc_many <- function(alts, Wmat) {
  M <- SCORES[alts, , drop = FALSE]
  R <- sweep(M, 2, sqrt(colSums(M^2)), "/")
  dp <- (t(R) - apply(R, 2, max))^2                 # 35 x n_alt
  dm <- (t(R) - apply(R, 2, min))^2
  W2 <- Wmat^2
  sp <- sqrt(W2 %*% dp); sm <- sqrt(W2 %*% dm)
  cc <- sm / (sp + sm); colnames(cc) <- alts; cc
}
ctq_weight_matrix <- function(D) t(apply(D, 1, function(d) ctq_weights(d[1], d[2], d[3])))
w_equal_ctq <- ctq_weights(1/3, 1/3, 1/3)

evaluate_triple <- function(alts, Wd) {
  cc0 <- cc_many(alts, matrix(w_equal_ctq, 1))[1, ]
  s0 <- sort(cc0, decreasing = TRUE)
  win <- alts[which.max(cc0)]
  cc <- cc_many(alts, Wd)
  top <- max.col(cc, ties.method = "first")
  srt <- t(apply(cc, 1, sort, decreasing = TRUE))
  data.frame(alt1 = alts[1], alt2 = alts[2], alt3 = alts[3], baseline_winner = win,
             baseline_margin = s0[1] - s0[2],
             stability = mean(alts[top] == win),
             near_tie = mean(srt[, 1] - srt[, 2] < CFG$near_tie),
             stringsAsFactors = FALSE)
}

# Triple sampling: draw companies until 3 distinct, sort, reject repeats; the
# pilot and main samples share the "already used" set.
sample_triples <- function(pool, seeds, n_pilot = CFG$rq3_pilot_n, n_main = CFG$rq3_main_n) {
  num <- as.integer(sub("Comp", "", pool)); seen <- character(0); out <- list()
  for (stage in c("pilot", "main")) {
    rng <- new_rng(seeds[[stage]]); n <- if (stage == "pilot") n_pilot else n_main; k <- 0
    while (k < n) {
      st <- integer(0)
      while (length(st) < 3) {
        x <- num[floor(rng$u() * length(num)) + 1]
        if (!(x %in% st)) st <- c(st, x)
      }
      st <- sort(st); key <- paste(st, collapse = "-")
      if (key %in% seen) next
      seen <- c(seen, key); k <- k + 1
      out[[length(out) + 1]] <- data.frame(stage = if (stage == "pilot") "Pilot" else "Main",
                                           a = st[1], b = st[2], c = st[3])
    }
  }
  tt <- do.call(rbind, out)
  data.frame(stage = tt$stage, alt1 = paste0("Comp", tt$a), alt2 = paste0("Comp", tt$b),
             alt3 = paste0("Comp", tt$c), stringsAsFactors = FALSE)
}

rh3_panels <- function(R) {
  pil <- R$stability[R$stage == "Pilot"]; m <- R[R$stage == "Main", ]
  g <- m$stability; n <- length(g); sdp <- sd(pil); se <- sd(g) / sqrt(n)
  tq <- qt(0.975, n - 1); q1 <- pinc(m$baseline_margin, 0.25); q3 <- pinc(m$baseline_margin, 0.75)
  band <- cut(m$baseline_margin, c(-Inf, q1, q3, Inf), labels = c("Q1", "Q2-Q3", "Q4"))
  list(pilot_mean = mean(pil), pilot_sd = sdp,
       n_required_precision = ceiling((1.96 * sdp / 0.03)^2),
       n_required_power = ceiling(7.849 * sdp^2 / (5 / 100)^2),
       n = n, mean = mean(g), sd = sd(g), se = se,
       ci = c(mean(g) - tq * se, mean(g) + tq * se), median = median(g),
       min = min(g), max = max(g), n_fully_stable = sum(g == 1),
       n_below = sum(g < CFG$rq3_threshold), share_below = mean(g < CFG$rq3_threshold),
       t = (mean(g) - CFG$rq3_threshold) / se,
       p_one_tailed = pt((mean(g) - CFG$rq3_threshold) / se, n - 1, lower.tail = FALSE),
       verdict = if (mean(g) - tq * se >= CFG$rq3_threshold) "RH3 SUPPORTED" else "RH3 NOT SUPPORTED",
       by_margin = data.frame(band = levels(band), cutoff = c(q1, q3, max(m$baseline_margin)),
                              triples = as.vector(table(band)),
                              mean_stability = as.vector(tapply(g, band, mean)),
                              below_threshold = as.vector(tapply(g < CFG$rq3_threshold, band, sum))))
}

DRAWS <- dirichlet2_draws(CFG$rq3_draw_seed, CFG$rq3_draws)
WD <- ctq_weight_matrix(DRAWS)

run_rq3 <- function(pool, seeds) {
  tr <- sample_triples(pool, seeds)
  res <- do.call(rbind, lapply(seq_len(nrow(tr)), function(i)
    evaluate_triple(c(tr$alt1[i], tr$alt2[i], tr$alt3[i]), WD)))
  R <- cbind(stage = tr$stage, res, stringsAsFactors = FALSE)
  list(results = R, panels = rh3_panels(R))
}

RQ3 <- list(
  tier2      = run_rq3(companies$id[companies$tier == "Tier 2"], CFG$rq3_t2_seeds),
  population = run_rq3(companies$id, CFG$rq3_pop_seeds))
for (k in names(RQ3)) with(RQ3[[k]]$panels, cat(sprintf(
  "   %-10s mean stability %s (95%% CI %s to %s), %d of %d below 70%%, %d fully stable -> %s\n",
  k, fmt_pct(mean), fmt_pct(ci[1]), fmt_pct(ci[2]), n_below, n, n_fully_stable, verdict)))

# ---- Worked examples (10,000 draws each) ------------------------------------
worked_tabs <- c("RH3 T2 Mean (C14-C16-C22)", "RH3 T2 Typical (C28-C29-C37)",
                 "RH3 Best (C21-C36-C40)", "RH3 Worst (C13-C14-C26)",
                 "RH3 Flip (C27-C31-C34)")
worked_example <- function(tab) {
  alts <- xl_vec_chr(tab, "C8:E8")
  seed <- CFG$worked_seed; n <- CFG$worked_draws
  D <- dirichlet2_draws(seed, n)
  cc <- cc_many(alts, ctq_weight_matrix(D))
  winner <- alts[max.col(cc, ties.method = "first")]
  base <- cc_many(alts, matrix(w_equal_ctq, 1))[1, ]
  bw <- alts[which.max(base)]
  stab <- mean(winner == bw)
  list(tab = tab, alts = alts, draws = D, cc = cc, winner = winner,
       panel_c = data.frame(alt = alts, times_rank1 = sapply(alts, function(a) sum(winner == a)),
                            share = sapply(alts, function(a) mean(winner == a)),
                            mean_cc = colMeans(cc), sd_cc = apply(cc, 2, sd),
                            min_cc = apply(cc, 2, min)),
       baseline_cc = base, baseline_winner = bw, stability = stab,
       ci = stab + c(-1, 1) * 1.96 * sqrt(stab * (1 - stab) / n))
}
WORKED <- lapply(setNames(worked_tabs, worked_tabs), worked_example)
for (w in WORKED) cat(sprintf("   %-30s winner %s, stability %s\n", w$tab, w$baseline_winner,
                              fmt_pct(w$stability)))

# ---- reconciliation --------------------------------------------------------
check("07", "Dirichlet draws vs 'RQ3 Tier 2 Sample'!T5:V1004", DRAWS,
      xl_num("RQ3 Tier 2 Sample", "T5:V1004"), 1e-12)
rq3_tabs <- c(tier2 = "RQ3 Tier 2 Sample", population = "RQ3 Population Sample")
for (nm in names(rq3_tabs)) {
  k <- rq3_tabs[[nm]]
  R <- RQ3[[nm]]$results; ref <- xl_raw(k, "A53:H282")
  check("07", paste0(k, ": sampled triples + winners (A:E)"),
        apply(R[, 1:5], 1, paste, collapse = "|"),
        apply(sapply(ref[1:5], as.character), 1, paste, collapse = "|"))
  check("07", paste0(k, ": margin, stability, near-tie (F:H)"),
        as.matrix(R[, 6:8]), sapply(ref[6:8], as.numeric), 1e-9)
  p <- RQ3[[nm]]$panels
  check("07", paste0(k, ": mean, SD, SE, CI (B27:B31)"),
        c(p$mean, p$sd, p$se, p$ci), xl_vec_num(k, "B27:B31"), 1e-12)
  check("07", paste0(k, ": t and one-tailed p (B41, B43)"),
        c(p$t, p$p_one_tailed), xl_vec_num(k, "B41:B43")[c(1, 3)], 1e-9)
}
for (w in WORKED) {
  check("07", paste0(w$tab, ": draws"), w$draws, xl_num(w$tab, "B60:D10059"), 1e-12)
  check("07", paste0(w$tab, ": Panel C (rank-1 counts, share, CC stats)"),
        as.matrix(w$panel_c[, -1]), xl_num(w$tab, "B31:F33"), 1e-9)
  check("07", paste0(w$tab, ": stability share (B43)"), w$stability,
        xl_vec_num(w$tab, "B43"), 1e-12)
}

save_csv(RQ3$tier2$results, "07_rq3_tier2_sample.csv")
save_csv(RQ3$population$results, "07_rq3_population_sample.csv")
save_csv(data.frame(draw = seq_len(nrow(DRAWS)), DRAWS), "07_rq3_dirichlet_draws.csv")