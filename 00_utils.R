# =============================================================================
# 00_utils.R — shared settings, workbook reader, random-number generators,
#              small statistics helpers and the reconciliation (check) log
# =============================================================================
# Every other module sources this file first (run_all.R does it for you).

suppressPackageStartupMessages({
  if (!requireNamespace("readxl", quietly = TRUE))
    stop("Package 'readxl' is required: install.packages('readxl')")
})

# -----------------------------------------------------------------------------
# Settings
# -----------------------------------------------------------------------------
CFG <- list(
  workbook   = Sys.getenv("HOQ_WORKBOOK", "HOQ and TOPSIS 17.xlsm"),
  out_dir    = "outputs",
  tol        = 1e-9,     # default tolerance when comparing with the workbook
  tol_loose  = 1e-6,     # for values Excel stores to 15 significant digits
  # Simulation parameters (mirrors 'Simulation Parameters'!A4:D19)
  boot_B          = 10000L, boot_seed  = 20261124,
  seed_tier1      = 20261201, seed_tier2 = 20261202,
  kappa           = 30,       ab_within  = 7,
  rq3_draws       = 1000L,    rq3_alpha  = 2, rq3_draw_seed = 777001,
  rq3_t2_seeds    = c(pilot = 555012, main = 555013),
  rq3_pop_seeds   = c(pilot = 555002, main = 555003),
  rq3_pilot_n     = 30L,      rq3_main_n = 200L,
  rq3_threshold   = 0.70,     near_tie   = 0.01,
  worked_draws    = 10000L,   worked_seed = 20260917,
  cv_seeds        = 20261121:20261125, cv_test_n = 2260L,
  rh1_threshold   = 0.95,     rh2_threshold = 0.10,
  anchor          = "Comp22", anchor_sens = c("Comp36", "Comp6"),
  ctq_equal       = c(Q = 1/3, T = 1/3, C = 1/3)
)
dir.create(CFG$out_dir, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Workbook reader (uses the cached values Excel saved with the file)
# -----------------------------------------------------------------------------
xl_raw <- function(sheet, range) {
  # col_types = "list" keeps each cell's native type, so numbers are never
  # round-tripped through text (which would cost precision)
  d <- readxl::read_excel(CFG$workbook, sheet = sheet, range = range,
                          col_names = FALSE, col_types = "list",
                          .name_repair = "minimal")
  d <- lapply(d, function(col) {
    v <- lapply(col, function(x) if (is.null(x) || length(x) == 0) NA else x)
    isnum <- vapply(v, function(x) is.numeric(x) || is.na(x), TRUE)
    if (all(isnum)) as.numeric(unlist(v)) else
      vapply(v, function(x) if (is.na(x)[1]) "" else
        if (is.numeric(x)) format(x, digits = 15) else as.character(x), "")
  })
  as.data.frame(d, stringsAsFactors = FALSE, col.names = seq_along(d))
}
xl_num <- function(sheet, range) {
  d <- xl_raw(sheet, range)
  m <- vapply(d, function(col) suppressWarnings(as.numeric(col)), numeric(nrow(d)))
  matrix(m, nrow = nrow(d))
}
xl_chr <- function(sheet, range) {
  d <- xl_raw(sheet, range)
  m <- vapply(d, function(col) { x <- as.character(col); x[is.na(x)] <- ""; x },
              character(nrow(d)))
  matrix(m, nrow = nrow(d))
}
xl_vec_num <- function(sheet, range) as.vector(xl_num(sheet, range))
xl_vec_chr <- function(sheet, range) as.vector(xl_chr(sheet, range))

# -----------------------------------------------------------------------------
# mulberry32 random-number stream
# -----------------------------------------------------------------------------
# Uses the compiled version in src/mulberry32.cpp when Rcpp and a compiler are
# available (needed for the 384-million-draw bootstrap). Falls back to an exact
# pure-R implementation otherwise (fine for everything except the bootstrap).
.HAS_CPP <- FALSE
if (requireNamespace("Rcpp", quietly = TRUE)) {
  ok <- try(Rcpp::sourceCpp(file.path("src", "mulberry32.cpp"),
                            env = globalenv()), silent = TRUE)
  .HAS_CPP <- !inherits(ok, "try-error")
}
if (!.HAS_CPP) message("Rcpp not available: using the pure-R mulberry32 ",
                       "(exact but slow; the RQ1 bootstrap will take hours).")

# --- pure-R exact 32-bit arithmetic (doubles hold unsigned 32-bit integers) --
.U32 <- 4294967296
.to_s32 <- function(x) as.integer(ifelse(x >= 2^31, x - .U32, x))
.to_u32 <- function(i) { i <- as.numeric(i); ifelse(i < 0, i + .U32, i) }
.xor  <- function(x, y) .to_u32(bitwXor(.to_s32(x), .to_s32(y)))
.or   <- function(x, y) .to_u32(bitwOr(.to_s32(x), .to_s32(y)))
.shr  <- function(x, k) floor(x / 2^k)
.imul <- function(a, b) {             # (a * b) mod 2^32, exact in doubles
  ah <- floor(a / 65536); al <- a %% 65536
  bh <- floor(b / 65536); bl <- b %% 65536
  (al * bl + ((ah * bl + al * bh) %% 65536) * 65536) %% .U32
}
.mb32_next_R <- function(a) {
  a <- (a + 0x6D2B79F5) %% .U32
  t <- a
  t <- .imul(.xor(t, .shr(t, 15)), .or(t, 1))
  t <- .xor(t, (t + .imul(.xor(t, .shr(t, 7)), .or(t, 61))) %% .U32)
  list(a = a, u = .xor(t, .shr(t, 14)) / .U32)
}

# new_rng(seed): a stream object with $u() (one uniform) and $un(n) (n uniforms)
new_rng <- function(seed) {
  env <- new.env()
  env$state <- as.numeric(seed) %% .U32
  env$un <- function(n) {
    if (.HAS_CPP) {
      r <- mb32_draw(env$state, as.integer(n)); env$state <- r$state; r$u
    } else {
      u <- numeric(n)
      for (i in seq_len(n)) {
        r <- .mb32_next_R(env$state); env$state <- r$a; u[i] <- r$u
      }
      u
    }
  }
  env$u <- function() env$un(1L)
  env
}

# -----------------------------------------------------------------------------
# Variate generators (draw order matches the workbook runs exactly)
# -----------------------------------------------------------------------------
# Standard normal: Box–Muller with the sine value cached and returned on the
# next call (the cache lives in the sampler, so it persists across companies).
new_samplers <- function(rng) {
  env <- new.env()
  env$spare <- NULL
  env$norm <- function() {
    if (!is.null(env$spare)) { s <- env$spare; env$spare <- NULL; return(s) }
    u1 <- rng$u(); if (u1 == 0) u1 <- 1e-300
    u2 <- rng$u()
    R <- sqrt(-2 * log(u1))
    env$spare <- R * sin(2 * pi * u2)
    R * cos(2 * pi * u2)
  }
  # Gamma(a, 1): Marsaglia–Tsang; for a < 1 uses Gamma(a+1) * U^(1/a), with U
  # drawn BEFORE the Gamma(a+1) variate.
  env$gamma <- function(a) {
    if (a < 1) { u <- rng$u(); return(env$gamma(a + 1) * u^(1 / a)) }
    d <- a - 1/3; cc <- 1 / sqrt(9 * d)
    repeat {
      repeat { x <- env$norm(); v <- 1 + cc * x; if (v > 0) break }
      v <- v^3
      u <- rng$u()
      if (u < 1 - 0.0331 * x^4) return(d * v)
      if (log(u) < 0.5 * x^2 + d * (1 - v + log(v))) return(d * v)
    }
  }
  env$beta <- function(a, b) { x <- env$gamma(a); y <- env$gamma(b); x / (x + y) }
  env
}

# Dirichlet(2,2,2) used for the CTQ scenarios: Gamma(2,1) = -ln(U1 * U2)
dirichlet2_draws <- function(seed, n) {
  rng <- new_rng(seed)
  u <- rng$un(6L * n)
  m <- matrix(u, ncol = 6, byrow = TRUE)
  g <- cbind(-log(m[, 1] * m[, 2]), -log(m[, 3] * m[, 4]), -log(m[, 5] * m[, 6]))
  w <- g / rowSums(g)
  colnames(w) <- c("Q", "T", "C")
  w
}

# -----------------------------------------------------------------------------
# Excel-equivalent statistics helpers
# -----------------------------------------------------------------------------
pinc     <- function(x, p) unname(quantile(x, p, type = 7))   # PERCENTILE.INC / QUARTILE.INC
rank_eq  <- function(x) vapply(x, function(v) 1L + sum(x > v), 1L)  # RANK.EQ, descending
avedev   <- function(x) mean(abs(x - mean(x)))
excel_skew <- function(x) { n <- length(x); s <- sd(x)
  n / ((n - 1) * (n - 2)) * sum(((x - mean(x)) / s)^3) }
excel_kurt <- function(x) { n <- length(x); s <- sd(x)
  n * (n + 1) / ((n - 1) * (n - 2) * (n - 3)) * sum(((x - mean(x)) / s)^4) -
    3 * (n - 1)^2 / ((n - 2) * (n - 3)) }
modal_rank <- function(r) { tb <- table(r); as.integer(names(tb)[which.max(tb)]) }
fmt_pct  <- function(x, d = 1) sprintf(paste0("%.", d, "f%%"), 100 * x)

# -----------------------------------------------------------------------------
# Reconciliation log: every module compares its results with the workbook
# -----------------------------------------------------------------------------
.CHECKS <- data.frame(module = character(), check = character(),
                      max_abs_diff = numeric(), tol = numeric(),
                      pass = logical(), stringsAsFactors = FALSE)
check <- function(module, name, computed, reference, tol = CFG$tol) {
  if (is.character(computed) || is.character(reference)) {
    d <- as.numeric(sum(as.character(computed) != as.character(reference)))
    pass <- d == 0
  } else {
    d <- suppressWarnings(max(abs(as.numeric(computed) - as.numeric(reference)),
                              na.rm = TRUE))
    pass <- is.finite(d) && d <= tol
  }
  .CHECKS[nrow(.CHECKS) + 1, ] <<- list(module, name, d, tol, pass)
  cat(sprintf("  [%s] %-58s max diff = %.3g\n", if (pass) "PASS" else "FAIL",
              name, d))
  invisible(pass)
}
check_summary <- function() {
  cat("\n================ RECONCILIATION WITH WORKBOOK ================\n")
  cat(sprintf("%d checks: %d passed, %d failed\n", nrow(.CHECKS),
              sum(.CHECKS$pass), sum(!.CHECKS$pass)))
  if (any(!.CHECKS$pass)) print(.CHECKS[!.CHECKS$pass, ], row.names = FALSE)
  write.csv(.CHECKS, file.path(CFG$out_dir, "reconciliation_checks.csv"),
            row.names = FALSE)
  invisible(.CHECKS)
}
save_csv <- function(x, name) write.csv(x, file.path(CFG$out_dir, name),
                                        row.names = FALSE)