# ==== DOCUMENTATION ====

#' Create subgroup analysis table (subgroups)
#'
#' `subgroups()` is a function which create a dataframe, which can be copied directly into
#' word or presented in as a subgroup analysis.
#'
#' @name subgroups
#'
#' @usage subgroups(df,group,subgroups,outcome,test,time.to,
#'                   strata.fixed,strata.random,digs_p,digs_s,
#'                   conf.level, paired,markdown, caption, fig)
#'
#' @param df dataframe. (`df`)
#' @param group Column name of stratification (`string`)
#' @param subgroups column names of subgroups (`list`)
#' @param outcome column name of outcome of interest (`string`)
#' @param test list of tests carried out, currently the following works: `t.test`, `wilcox.test`, `fisher.test`, `auc`, `lm`, and `glm`. (`list`)
#' @param paired if tests should be paired (`boolean`)
#' @param digs_p digits for p-values (`numeric`)
#' @param digs_s digits for statistics (`numeric`)
#' @param strata.fixed list of columns which should be used as fixed stratification (`list`)
#' @param strata.random list of columns which should be used as random stratification (`list`)
#' @param time.to Column name of the time column for cox regression (`list`)
#' @param conf.level confidence intervals which should be presented (`numeric`).
#' @param markdown default is true and output is pander, while false output is a dataframe (`boolean`)
#' @param caption Table caption only in use when markdown is true (`string`)
#' @param fig If figure should be presented (`boolean`)
#'
#' @return Returns summarised information in dataframe.
#'
#' @examples
#' \dontrun{
#'    zubgroups <- c(`Estimated fetal weight`="est_fetal_weight_grp",
#'    `Gestational age`="Gestational Age at rand (cat)",
#'    `End diastolic flow`="end_dia_flow_grp",
#'    `Uterine artery Doppler / Notch`="dop_notch_grp",
#'    `Maternal S-Placental-GF`="maternalSplacentalGF_grp",
#'    `Hypertension or preeclampsia`="ht_preeclamps_grp",
#'    `Abdominal circumference`="abd_circum_cent_grp")
#'
#'    subgroups(df,group="group",zubgroups,
#'       outcome="Infant survival without SAE",
#'       test="glm",strata.fixed = "site")
#' }
#'
#' @importFrom stats as.formula anova setNames
#' @importFrom patchwork wrap_plots
#' @importFrom ggplot2 geom_point expand_scale geom_errorbar
#' @importFrom utils stack
#' @export
#
# ==== FUNCTION ====
#
# group = NULL;
# render.numeric = c("median [IQR]","mean (%CI)");
# render.factor = "simple"; tests = NA; test.vars = NA; paired = F;
# digs_n = 2; digs_f = 1; digs_p = 3; digs_s = 2;
# only_stats = F; strata.fixed = NA; strata.random = NA;
# time.to = NA; present.missing = "dynamic"; conf.level = 0.95;
# zeroonetoyn =T
# markdown=T; caption=""
# fig=T
#
# group="group"
# subgroups=c("Trial","Sex"," Age","GAF/SAS-SR")
# outcome="Primary outcome (short)"
# test="glm"
subgroups <- function(df, group, subgroups, outcome,
                      test = NA, time.to = NA,
                      strata.fixed = NA, strata.random = NA,
                      digs_p = 3, digs_s = 2,
                      conf.level = 0.95, paired = F,
                      markdown = T, caption = "",
                      fig = F, debug = F, xlim = NULL) {

   ####### HELPER FUNCTIONS #######
   format_p_value <- function(p_value, digs_p) {
      # Sikr at vi kun arbejder med en enkelt værdi
      p_value <- as.numeric(p_value)[1]

      # Håndter NA, NULL, NaN
      if (is.null(p_value) || is.na(p_value) || is.nan(p_value)) {
         return(NA_character_)
      }

      # Check if the p-value is less than the threshold for rounding
      if (p_value < 10^(-digs_p)) {
         return(paste0("<", format(10^(-digs_p), nsmall = digs_p)))
      } else {
         return(format(round(p_value, digs_p), nsmall = digs_p))
      }
   }

   # Beregn mean og CI for kontinuerte variable
   calc_mean_ci <- function(x, conf.level = 0.95) {
      n <- length(na.omit(x))
      if (n == 0) return(list(mean = NA, lcl = NA, ucl = NA, n = 0))

      mean_val <- mean(x, na.rm = TRUE)
      se <- sd(x, na.rm = TRUE) / sqrt(n)
      t_val <- qt((1 + conf.level) / 2, df = n - 1)

      list(
         mean = mean_val,
         lcl = mean_val - t_val * se,
         ucl = mean_val + t_val * se,
         n = n
      )
   }

   quickStat <- function(d, outcome, group, test, digs_s, digs_p,
                         paired, strata.fixed, strata.random, interactions, time.to, conf.level) {

      # Helper functions
      midP <- function(n) {
         if (n[1, 2] == n[2, 1]) {
            midP <- 1 - 0.5 * dbinom(n[1, 2], n[1, 2] + n[2, 1], 0.5)
         } else {
            P <- 2 * pbinom(min(n[1, 2], n[2, 1]), n[1, 2] + n[2, 1], 0.5)
            P <- min(P, 1)
            midP <- P - dbinom(n[1, 2], n[1, 2] + n[2, 1], 0.5)
         }
         return(midP)
      }

      format_ci <- function(est, lcl, ucl, digs_s) {
         paste0(format(round(est, digs_s), nsmall = digs_s), " (",
                format(round(lcl, digs_s), nsmall = digs_s), ";",
                format(round(ucl, digs_s), nsmall = digs_s), ")")
      }

      generate_formula <- function(outcome, group, strata.fixed, strata.random,
                                   interactions) {
         formel <- paste0("`", group, "`")
         if (any(!is.na(strata.fixed))) {
            formel <- paste0(formel, " + ", paste0("`", strata.fixed, "`", collapse = " + "))
         }
         if (!is.na(strata.random)) {
            formel <- paste0(formel, " + (1|`", strata.random, "`)", collapse = " + ")
         }
         if (all(!is.na(interactions))) {
            formel <- paste0(formel, " + ", paste0("`", interactions[1], "`*`",
                                                   interactions[2], "`"))
         }
         formula(paste0("`", outcome, "` ~ ", formel))
      }

      # Starting
      out <- NULL

      d[[group]] <- as.factor(d[[group]])
      group_list <- levels(d[[group]])
      group_list <- group_list[group_list != ""]

      formel <- generate_formula(outcome, group, strata.fixed, strata.random,
                                 interactions)
      formelf <- generate_formula(outcome, group, strata.fixed = c(strata.fixed, strata.random),
                                  strata.random = NA,
                                  interactions)

      # Continuous outcome
      if (class(d[[outcome]]) %in% c("numeric", "integer") & length(group_list) == 2) {
         y <- d[[outcome]][d[[group]] == group_list[1]]
         x <- d[[outcome]][d[[group]] == group_list[2]]

         if (test %in% c("t.test", "wilcox.test")) {
            tst <- if (test == "t.test") {
               suppressWarnings(t.test(x, y, paired = paired, conf.level = conf.level))
            } else {
               wilcox.test(x, y, paired = paired, conf.int = TRUE,
                           conf.level = conf.level)
            }
            if (length(tst$estimate) == 2) tst$estimate <-
                  tst$estimate[1] - tst$estimate[2]

            out$txt <- if (test == "t.test") {
               if (paired) "paired.t.test" else "unpaired.t.test"
            } else {
               if (paired) "paired.wilcox.test" else "unpaired.wilcox.test"
            }

            out$estci <- format_ci(tst$estimate[[1]],
                                   tst$conf.int[[1]], tst$conf.int[[2]], digs_s)
            out$est <- tst$estimate[[1]]
            out$lcl <- tst$conf.int[[1]]
            out$ucl <- tst$conf.int[[2]]
            out$pval <- format_p_value(tst$p.value, digs_p)

         } else if (test == "auc") {
            auc_values <- sapply(c("<", ">"), function(dir) {
               suppressMessages(pROC::auc(d[[group]], d[[outcome]], direction = dir))
            })
            best_auc <- max(auc_values)
            tst <- pROC::ci.auc(best_auc, conf.level = conf.level)

            out$txt <- "auroc"
            out$estci <- format_ci(tst[2], tst[1], tst[3], digs_s)
            out$est <- tst[2]
            out$lcl <- tst[1]
            out$ucl <- tst[3]
            out$pval <- NA

         } else if (test %in% c("lm")) {
            if (!is.na(strata.random)) {
               m1 <- lme4::lmer(formel, data = d)
               est <- lme4::fixef(m1)[grepl(group, names(lme4::fixef(m1)))]
               # Bedre håndtering af konfidensintervaller for lmer
               ci <- tryCatch({
                  confint(m1, parm = "beta_", method = "Wald", level = conf.level)
               }, error = function(e) {
                  # Hvis Wald fejler, prøv profile method
                  tryCatch({
                     confint(m1, parm = "beta_", level = conf.level)
                  }, error = function(e2) {
                     # Sidste udvej: manuel beregning
                     se <- sqrt(diag(vcov(m1)))
                     z <- qnorm((1 + conf.level) / 2)
                     cbind(lme4::fixef(m1) - z * se, lme4::fixef(m1) + z * se)
                  })
               })
               out$txt <- "lmer"
            } else {
               m1 <- lm(formel, data = d)
               est <- m1$coefficients[grepl(group, names(m1$coefficients))]
               ci <- confint.default(m1, level = conf.level)
               out$txt <- "lm"
            }
            out$estci <- format_ci(est, ci[grepl(group, rownames(ci)), 1],
                                   ci[grepl(group, rownames(ci)), 2], digs_s)
            out$est <- est
            out$lcl <- ci[grepl(group, rownames(ci)), 1]
            out$ucl <- ci[grepl(group, rownames(ci)), 2]
            pval <- parameters::p_value(m1)
            pval <- pval[grepl(group, pval$Parameter), "p"]
            out$pval <- format_p_value(pval, digs_p)
         }

         # Categorical outcome
      } else if (class(d[[outcome]]) %in% c("factor", "character")) {
         if (test %in% c("fisher.test")) {
            x <- d[[group]]
            y <- d[[outcome]]

            tst <- tryCatch(fisher.test(table(y, x), conf.level = conf.level),
                            error = function(e) e, warning = function(w) w)
            if (any(class(tst) %in% c("error", "try-error", "warning"))) {
               tst <- fisher.test(table(y, x), simulate.p.value = TRUE, B = 10^(digs_p + 1),
                                  conf.level = conf.level)
            }
            if (!is.null(tst$estimate)) {
               out$estci <- format_ci(tst$estimate[[1]], tst$conf.int[[1]],
                                      tst$conf.int[[2]], digs_s)
               out$est <- tst$estimate[[1]]
               out$lcl <- tst$conf.int[[1]]
               out$ucl <- tst$conf.int[[2]]
            } else {
               out$estci <- "- (-;-)"
               out$est <- NA
               out$lcl <- NA
               out$ucl <- NA
            }
            out$txt <- "fisher.test"
            out$pval <- format_p_value(tst$p.value, digs_p)

         } else if (test %in% c("glm") & length(group_list) == 2) {
            m <- d[complete.cases(d[, colnames(d) %in%
                                       c(outcome, group, strata.fixed, strata.random)]), ]

            if (!is.na(strata.random)) {
               fit_glmer_safe <- function(formel, data) {
                  attempts <- list(
                     list(args = list(family = binomial(log))),
                     list(args = list(family = binomial(log), nAGQ = 0)),
                     list(args = list(family = binomial(log),
                                      control = lme4::glmerControl(optimizer = "bobyqa")))
                  )

                  for (attempt in attempts) {
                     result <- try(lme4::glmer(formel, data = data, !!!attempt$args), silent = TRUE)
                     if (!inherits(result, "try-error")) {
                        return(result)
                     }
                  }

                  stop("Alle forsøg på at fit glmer mislykkedes.")
               }

               m1 <- tryCatch(fit_glmer_safe(formel, m),
                              error = function(e) e, warning = function(w) w)
               if (!(any(class(m1) %in% c("error", "try-error", "warning")))) {
                  out$txt <- "glmer"
               }

               if (any(class(m1) %in% c("error", "try-error", "warning"))) {
                  formel_fixed <- as.formula(
                     gsub("\\(1 \\| ([^)]+)\\)", "\\1", paste(deparse(formel), collapse = ""))
                  )
                  tmp_d <- d
                  tmp_d[[outcome]] <- as.numeric(as.factor(tmp_d[[outcome]])) - 1
                  m1 <- glm(formel_fixed, data = tmp_d, family = poisson(link = "log"))
                  out$txt <- "glm"
               }
            } else {
               tmp_d <- d
               tmp_d[[outcome]] <- as.numeric(as.factor(tmp_d[[outcome]])) - 1

               m1 <- glm(formel, data = tmp_d, family = poisson(link = "log"))
               out$txt <- "glm"
            }

            if (out$txt == "glm") {
               est <- exp(coef(m1))[grepl(group, names(coef(m1))) &
                                       !grepl(":", names(coef(m1)))]

               ci <- try(exp(confint.default(m1, level = conf.level)), silent = T)
               if ("try-error" %in% class(ci)) ci <-
                  exp(confint(m1, conf.level = conf.level))
               lcl <- ci[grepl(group, rownames(ci)) &
                            !grepl(":", names(coef(m1))), 1]
               ucl <- ci[grepl(group, rownames(ci)) &
                            !grepl(":", names(coef(m1))), 2]

               pval <- parameters::p_value(m1)
               pval <- pval[grepl(group, pval$Parameter) &
                               !grepl(":", pval$Parameter), "p"]

            } else if (out$txt == "glmer") {
               res <- exp(cbind(lme4::fixef(m1), confint.default(m1,
                                                                 level = conf.level)[-1, ]))
               est <- res[grepl(group, rownames(res)) &
                             !grepl(":", rownames(res)), 1]
               lcl <- res[grepl(group, rownames(res)) &
                             !grepl(":", rownames(res)), 2]
               ucl <- res[grepl(group, rownames(res)) &
                             !grepl(":", rownames(res)), 3]
               pval <- summary(m1)$coefficients[grepl(
                  group, rownames(summary(m1)$coefficients)) &
                     !grepl(":", rownames(summary(m1)$coefficients)), 4]
            }
            out$estci <- format_ci(est, lcl, ucl, digs_s)
            out$est <- est
            out$lcl <- lcl
            out$ucl <- ucl
            out$pval <- format_p_value(pval, digs_p)

         } else if (test %in% c("midp") & paired == T & length(group_list) == 2) {
            res_midp <- midP(table(d[d[[group]] == group_list[1], outcome],
                                   d[d[[group]] == group_list[2], outcome]))

            out$txt <- "midp"
            out$estci <- ""
            out$est <- NA
            out$lcl <- NA
            out$ucl <- NA
            out$pval <- format_p_value(res_midp, digs_p)

         } else if (test %in% c("cox")) {
            d[[outcome]] <- as.numeric(d[[outcome]]) - 1
            SurvVar <- survival::Surv(d[[time.to]], d[[outcome]])

            formel <- paste0("`", c(group, strata.fixed), "`", collapse = " + ")
            if (all(!is.na(interactions))) {
               formel <- paste(formel, "+", paste0("`", interactions[1], "`*`", interactions[2], "`"))
            }
            formel <- formula(paste0("`SurvVar`~", formel))
            m1 <- survival::coxph(formel, data = d)
            m1est <- exp(m1$coefficients)
            m1ci <- exp(confint.default(m1, level = conf.level))

            out$txt <- "cox"
            out$estci <- format_ci(m1est[[1]], m1ci[1, 1], m1ci[1, 2], digs_s)
            out$est <- m1est[[1]]
            out$lcl <- m1ci[1, 1]
            out$ucl <- m1ci[1, 2]
            out$pval <- format_p_value(parameters::p_value(m1)[1, 2], digs_p)
         }
      }

      test_names <- list(
         "paired.t.test" = "Paired t-test",
         "unpaired.t.test" = "Unpaired t-test",
         "paired.wilcox.test" = "Wilcoxon signed rank test",
         "unpaired.wilcox.test" = "Wilcoxon rank sum test",
         "fisher.test" = "Fisher's exact test",
         "midp" = "McNemar's test",
         "lm" = "Linear regression",
         "lmer" = "Mixed effects linear regression",
         "glm" = "Logistic regression",
         "glmer" = "Mixed effects logistic regression",
         "auroc" = "AUROC",
         "cox" = "Cox Proportional-Hazards Model"
      )
      out$name <- test_names[[out$txt]]
      nmzci <- round(conf.level * 100, digs_s)
      out$estci_txt <- paste0(
         switch(out$txt,
                "paired.t.test" = "MD",
                "unpaired.t.test" = "MD",
                "paired.wilcox.test" = "median diff.",
                "unpaired.wilcox.test" = "median diff.",
                "fisher.test" = "OR",
                "midp" = NA,
                "lm" = "est.",
                "lmer" = "est.",
                "glm" = "RR",
                "glmer" = "RR",
                "auroc" = "AUC",
                "cox" = "HR"),
         " (", nmzci, "%CI)")

      out <- c(out, list(model = m1))

      return(out)
   }

   ####### MAIN FUNCTION #######

   d <- df

   # NAMED VARS
   if (!is.null(names(subgroups))) {
      for (i in 1:length(subgroups)) {
         if (nchar(names(subgroups)[i]) > 0) {
            d[[names(subgroups)[i]]] <- d[[subgroups[i]]]
            subgroups[i] <- names(subgroups)[i]
         }
      }
   }

   d[[group]] <- as.factor(d[[group]])
   if (length(levels(d[[group]])) != 2) stop("Need two groups - not more, not fewer")

   # Tjek om outcome er kontinuert eller kategorisk
   is_continuous <- class(d[[outcome]]) %in% c("numeric", "integer")

   if (!is_continuous) {
      d[[outcome]] <- as.factor(d[[outcome]])
      if (length(levels(d[[outcome]])) != 2) stop("Need binomial outcome (2 levels) - not more, not fewer")
   }

   tbl <- NULL
   # SUMMARY ----
   for (i in subgroups) {

      if (debug) cat("\n=== Processing subgroup:", i, "===\n")

      # Tjek om subgruppen har mindst 2 levels
      if (length(unique(na.omit(d[[i]]))) < 2) {
         warning(paste0("Skipping subgroup '", i, "' - has less than 2 levels"))
         next
      }

      # Find levels med tilstrækkelige observationer
      valid_levels <- c()
      for (lvl in unique(na.omit(d[[i]]))) {
         # Tjek om dette level har observationer i begge grupper
         level_data <- d[!is.na(d[[i]]) & d[[i]] == lvl, ]
         level_data <- level_data[complete.cases(level_data[, na.omit(c(outcome, group, strata.fixed, strata.random))]), ]

         if (nrow(level_data) > 10 &&  # mindst 10 obs totalt
             length(unique(level_data[[group]])) == 2 &&
             all(table(level_data[[group]]) > 5)) {  # mindst 5 obs per gruppe
            valid_levels <- c(valid_levels, lvl)
         } else {
            if (debug) warning(paste0("Skipping level '", lvl, "' in subgroup '", i,
                                      "' - insufficient observations (n=", nrow(level_data), ")"))
         }
      }

      if (debug) cat("Valid levels:", paste(valid_levels, collapse=", "), "\n")

      # Spring hele subgruppen over hvis der ikke er nok valid levels
      if (length(valid_levels) < 2) {
         warning(paste0("Skipping subgroup '", i, "' - less than 2 valid levels after filtering"))
         next
      }

      # Lav en midlertidig dataset med kun valid levels
      d_temp <- d[!is.na(d[[i]]) & d[[i]] %in% valid_levels, ]
      if (debug) cat("Rows in d_temp:", nrow(d_temp), "\n")

      # VIGTIGT: Refactorize alle factor variabler for at droppe unused levels
      d_temp[[i]] <- droplevels(factor(d_temp[[i]]))
      d_temp[[group]] <- droplevels(factor(d_temp[[group]]))

      if (debug) {
         cat("Subgroup levels after droplevels:", paste(levels(d_temp[[i]]), collapse=", "), "\n")
         cat("Group levels after droplevels:", paste(levels(d_temp[[group]]), collapse=", "), "\n")
      }

      # Refactorize også alle strata variabler og tjek om de har mindst 2 levels
      strata_to_use <- c()
      for (sf in strata.fixed[!is.na(strata.fixed)]) {
         if (sf %in% colnames(d_temp)) {
            if (is.factor(d_temp[[sf]]) || is.character(d_temp[[sf]])) {
               d_temp[[sf]] <- droplevels(factor(d_temp[[sf]]))
               n_levels <- length(levels(d_temp[[sf]]))
               if (debug) cat("Strata variable", sf, "has", n_levels, "levels\n")
               # Kun inkluder hvis der er mindst 2 levels
               if (n_levels >= 2) {
                  strata_to_use <- c(strata_to_use, sf)
               } else {
                  if (debug) warning(paste0("Dropping strata variable '", sf, "' for subgroup '", i,
                                            "' - only ", n_levels, " level(s) after filtering"))
               }
            } else {
               strata_to_use <- c(strata_to_use, sf)
               if (debug) cat("Strata variable", sf, "is numeric (keeping)\n")
            }
         }
      }

      if (debug) cat("Strata to use:", paste(strata_to_use, collapse=", "), "\n")

      # Tjek random effects
      strata_random_to_use <- strata.random
      if (!is.na(strata.random) && strata.random %in% colnames(d_temp)) {
         if (is.factor(d_temp[[strata.random]]) || is.character(d_temp[[strata.random]])) {
            d_temp[[strata.random]] <- droplevels(factor(d_temp[[strata.random]]))
            n_levels <- length(levels(d_temp[[strata.random]]))
            if (debug) cat("Random effect", strata.random, "has", n_levels, "levels\n")
            if (n_levels < 2) {
               if (debug) warning(paste0("Dropping random effect '", strata.random, "' for subgroup '", i,
                                         "' - only ", n_levels, " level(s) after filtering"))
               strata_random_to_use <- NA
            }
         }
      }

      if (debug) cat("Random effect to use:", strata_random_to_use, "\n")

      # Hvis der ikke er nok strata variabler tilbage, spring over
      if (length(strata_to_use) == 0 && all(is.na(strata_random_to_use))) {
         warning(paste0("Skipping subgroup '", i, "' - no valid strata variables after filtering"))
         next
      }

      if (debug) cat("Calling quickStat for reduced model...\n")
      tmp_reduced <- tryCatch({
         quickStat(d_temp, outcome, group, test, digs_s, digs_p,
                   paired, strata.fixed = c(i, strata_to_use),
                   strata_random_to_use, interactions = NA, time.to, conf.level)
      }, error = function(e) {
         if (debug) cat("ERROR in reduced model:", e$message, "\n")
         warning(paste0("Error in reduced model for subgroup '", i, "': ", e$message))
         return(NULL)
      })

      if (is.null(tmp_reduced)) {
         if (debug) cat("Reduced model returned NULL, skipping\n")
         next
      }
      if (debug) cat("Reduced model successful\n")

      if (debug) cat("Calling quickStat for full model...\n")
      tmp_full <- tryCatch({
         quickStat(d_temp, outcome,
                   group, test, digs_s, digs_p,
                   paired, strata.fixed = c(i, strata_to_use),
                   strata_random_to_use, interactions = c(i, group),
                   time.to, conf.level)
      }, error = function(e) {
         if (debug) cat("ERROR in full model:", e$message, "\n")
         warning(paste0("Error in full model for subgroup '", i, "': ", e$message))
         return(NULL)
      })

      if (is.null(tmp_full)) {
         if (debug) cat("Full model returned NULL, skipping\n")
         next
      }
      if (debug) cat("Full model successful\n")

      if (debug) cat("Running ANOVA...\n")
      lrt <- tryCatch({
         anova(tmp_reduced$model, tmp_full$model, test = "LRT")
      }, error = function(e) {
         if (debug) cat("ERROR in ANOVA:", e$message, "\n")
         warning(paste0("Error in ANOVA for subgroup '", i, "': ", e$message))
         return(NULL)
      })

      if (is.null(lrt)) {
         if (debug) cat("ANOVA returned NULL, skipping\n")
         next
      }
      if (debug) cat("ANOVA successful\n")

      pcol <- colnames(lrt)[grepl("^Pr", colnames(lrt))]

      # Brug kun valid levels
      tbl <- rbind(tbl, cbind(i, valid_levels,
                              format_p_value(na.omit(lrt[[pcol]]), digs_p)))
      if (debug) cat("Added to results table\n")

   }

   # Tjek om der er nogen resultater
   if (is.null(tbl) || nrow(tbl) == 0) {
      stop("No valid subgroups could be analyzed. Check your data for sufficient observations in each subgroup.")
   }

   tbl <- data.frame(tbl, stringsAsFactors = FALSE)
   colnames(tbl) <- c("subgroup", "group", "p_interaction")

   # Analyses for each subgroup
   res <- NULL
   for (i in 1:nrow(tbl)) {
      if (debug) cat("\n--- Analyzing level", tbl[i, 2], "of subgroup", tbl[i, 1], "---\n")

      tmpd <- d[d[, tbl[i, 1]] == tbl[i, 2], ]

      # Tjek om der er nok data
      if (nrow(tmpd) == 0) {
         if (debug) cat("No data for this level, skipping\n")
         warning(paste0("No data for level '", tbl[i, 2], "' of subgroup '", tbl[i, 1], "'"))
         next
      }

      # Refactorize og tjek alle variabler
      tmpd[[group]] <- droplevels(factor(tmpd[[group]]))

      # Tjek om group variabel stadig har 2 levels
      if (length(levels(tmpd[[group]])) < 2) {
         if (debug) cat("Less than 2 group levels after filtering, skipping\n")
         warning(paste0("Level '", tbl[i, 2], "' of subgroup '", tbl[i, 1],
                        "' has only ", length(levels(tmpd[[group]])), " group level(s)"))
         next
      }

      # Find valid strata for dette specifikke level
      strata_valid <- c()
      for (sf in strata.fixed[!is.na(strata.fixed)]) {
         if (sf %in% colnames(tmpd) && sf != tbl[i, 1]) {  # skip subgroup variable
            if (is.factor(tmpd[[sf]]) || is.character(tmpd[[sf]])) {
               tmpd[[sf]] <- droplevels(factor(tmpd[[sf]]))
               n_lev <- length(levels(tmpd[[sf]]))
               if (n_lev >= 2) {
                  strata_valid <- c(strata_valid, sf)
               } else {
                  if (debug) cat("Dropping", sf, "- only", n_lev, "level\n")
               }
            } else {
               strata_valid <- c(strata_valid, sf)
            }
         }
      }

      # Tjek random effect
      random_valid <- NA
      if (!is.na(strata.random) && strata.random %in% colnames(tmpd)) {
         if (is.factor(tmpd[[strata.random]]) || is.character(tmpd[[strata.random]])) {
            tmpd[[strata.random]] <- droplevels(factor(tmpd[[strata.random]]))
            n_lev <- length(levels(tmpd[[strata.random]]))
            if (n_lev >= 2) {
               random_valid <- strata.random
            } else {
               if (debug) cat("Dropping random effect", strata.random, "- only", n_lev, "level\n")
            }
         } else {
            random_valid <- strata.random
         }
      }

      if (debug) {
         cat("Using strata:", paste(strata_valid, collapse=", "), "\n")
         cat("Using random:", random_valid, "\n")
      }

      tmp <- tryCatch({
         quickStat(tmpd, outcome, group, test, digs_s,
                   digs_p, paired, strata.fixed = strata_valid,
                   random_valid,
                   interactions = NA, time.to, conf.level)
      }, error = function(e) {
         if (debug) cat("ERROR:", e$message, "\n")
         warning(paste0("Error for level '", tbl[i, 2], "' of subgroup '", tbl[i, 1], "': ", e$message))
         return(NULL)
      })

      if (is.null(tmp)) {
         if (debug) cat("Analysis failed, skipping this level\n")
         next
      }

      tmpd <- tmpd[complete.cases(tmpd[, na.omit(c(outcome, group, strata_valid,
                                                   random_valid))]), ]

      # Tjek igen efter complete.cases
      if (nrow(tmpd) == 0 || length(unique(tmpd[[group]])) < 2) {
         if (debug) cat("Insufficient data after removing incomplete cases, skipping\n")
         warning(paste0("Insufficient complete data for level '", tbl[i, 2], "' of subgroup '", tbl[i, 1], "'"))
         next
      }

      if (is_continuous) {
         # For kontinuerte outcomes: beregn mean (95%CI) for hver gruppe
         grp1_stats <- calc_mean_ci(tmpd[[outcome]][tmpd[[group]] == levels(tmpd[[group]])[1]], conf.level)
         grp2_stats <- calc_mean_ci(tmpd[[outcome]][tmpd[[group]] == levels(tmpd[[group]])[2]], conf.level)

         tmp$C <- paste0(format(round(grp1_stats$mean, digs_s), nsmall = digs_s), " (",
                         format(round(grp1_stats$lcl, digs_s), nsmall = digs_s), ";",
                         format(round(grp1_stats$ucl, digs_s), nsmall = digs_s), ")\n",
                         "n=", grp1_stats$n)
         tmp$E <- paste0(format(round(grp2_stats$mean, digs_s), nsmall = digs_s), " (",
                         format(round(grp2_stats$lcl, digs_s), nsmall = digs_s), ";",
                         format(round(grp2_stats$ucl, digs_s), nsmall = digs_s), ")\n",
                         "n=", grp2_stats$n)
      } else {
         # For dikotome outcomes: behold n/N
         tmp$C <- paste0(sum(tmpd[[group]] == levels(tmpd[[group]])[1] &
                                tmpd[[outcome]] == levels(tmpd[[outcome]])[2]), "/",
                         sum(tmpd[[group]] == levels(tmpd[[group]])[1]))
         tmp$E <- paste0(sum(tmpd[[group]] == levels(tmpd[[group]])[2] &
                                tmpd[[outcome]] == levels(tmpd[[outcome]])[2]), "/",
                         sum(tmpd[[group]] == levels(tmpd[[group]])[2]))
      }

      tmp$model <- NULL

      res <- rbind(res, data.frame(tmp))
      if (debug) cat("Successfully added to results\n")
   }

   # Tjek om der er nogen resultater
   if (is.null(res) || nrow(res) == 0) {
      stop("No subgroup levels could be analyzed. All levels were filtered out due to insufficient data.")
   }

   tbl <- cbind(tbl, res)

   tbl2 <- tbl[, c("subgroup", "group", "E", "C", "estci", "est", "lcl", "ucl", "pval", "p_interaction")]

   tbl2$yaxis <- as.factor(c(1:nrow(tbl2)))
   tbl2$subgroup[duplicated(tbl2$subgroup)] <- ""
   tbl2$p_interaction[tbl2$subgroup == ""] <- ""

   # Tilføj tomme linjer til alle kolonner for at matche højden når is_continuous
   if (is_continuous) {
      tbl2$subgroup <- ifelse(tbl2$subgroup != "", paste0(tbl2$subgroup, "\n "), " \n ")
      tbl2$group <- paste0(tbl2$group, "\n ")
      tbl2$estci <- paste0(tbl2$estci, "\n ")
      tbl2$pval <- paste0(tbl2$pval, "\n ")
      tbl2$p_interaction <- paste0(tbl2$p_interaction, "\n ")
   }

   if (!fig) {
      tbl2[, c("yaxis", "est", "lcl", "ucl")] <- NULL

      # Tilpas kolonnenavne baseret på outcome type
      if (is_continuous) {
         colnames(tbl2) <- c("",
                             "**Subgroup**",
                             paste0("**", levels(d[[group]])[2], "**\\\n*mean (95%CI)*\\\n*n*"),
                             paste0("**", levels(d[[group]])[1], "**\\\n*mean (95%CI)*\\\n*n*"),
                             paste0("**", unique(tbl$name), "**\\\n*",
                                    unique(tbl$estci_txt), "*"),
                             "**p-value**",
                             "**p-value for interaction**")
      } else {
         colnames(tbl2) <- c("",
                             "**Subgroup**",
                             paste0("**", levels(d[[group]])[2], "**\\\n*n/N*"),
                             paste0("**", levels(d[[group]])[1], "**\\\n*n/N*"),
                             paste0("**", unique(tbl$name), "**\\\n*",
                                    unique(tbl$estci_txt), "*"),
                             "**p-value**",
                             "**p-value for interaction**")
      }

      if (markdown) {
         pander::pander(tbl2, keep.line.breaks = TRUE, split.tables = Inf, row.names = F,
                        caption = caption)
      } else {
         return(tbl2)
      }
   } else {
      ######## FIGUR
      # Left
      tmp <- cbind(tbl2$yaxis, stack(tbl2[, colnames(tbl2) %in% c("group", "E", "C", "estci")]))
      colnames(tmp)[1] <- "yaxis"
      tmp$hjust <- (tmp$ind != "group") * 0.5
      tmp$vjust <- if(is_continuous) 0.5 else 0.5

      # Tilpas labels baseret på outcome type
      if (is_continuous) {
         x_labels <- c("Subgroups\n ",
                       paste0(levels(d[[group]])[2], "\nmean (95%CI)\nn"),
                       paste0(levels(d[[group]])[1], "\nmean (95%CI)\nn"),
                       paste0(unique(tbl$estci_txt), "\n "))
      } else {
         x_labels <- c("Subgroups",
                       paste0(levels(d[[group]])[2], "\nn/N"),
                       paste0(levels(d[[group]])[1], "\nn/N"),
                       unique(tbl$estci_txt))
      }

      left <- ggplot(tmp, aes(y = yaxis, x = ind, label = values)) +
         geom_text(size = 3, hjust = tmp$hjust, vjust = 0.4, lineheight = 0.85) +
         scale_x_discrete(position = "top", expand = expansion(mult = c(0, 0.25)),
                          labels = x_labels) +
         scale_y_discrete(limits = rev, labels = rev(tbl2$subgroup), drop = FALSE) +
         labs(y = NULL, x = NULL) +
         theme_classic() +
         theme(
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_text(face = "bold", lineheight = 0.85),
            axis.text.y = element_text(vjust = 0.4),
            plot.margin = margin(10, 5, 5, 5)
         )

      # Middle
      # Filtrer rækker ud hvor lcl eller ucl er infinite
      tbl2_plot <- tbl2[is.finite(tbl2$lcl) & is.finite(tbl2$ucl), ]

      if (nrow(tbl2_plot) > 0) {
         # Tjek for pile data
         arrow_data <- NULL
         if (!is.null(xlim)) {
            xlim_min <- ifelse(is.na(xlim[1]), -Inf, xlim[1])
            xlim_max <- ifelse(is.na(xlim[2]), Inf, xlim[2])

            for (i in 1:nrow(tbl2_plot)) {
               # Højre pil hvis UCL er uden for
               if (!is.na(xlim_max) && is.finite(xlim_max) && tbl2_plot$ucl[i] > xlim_max) {
                  arrow_data <- rbind(arrow_data, data.frame(
                     yaxis = tbl2_plot$yaxis[i],
                     x = xlim_max,
                     xend = xlim_max + 0.8,
                     direction = "right",
                     stringsAsFactors = FALSE
                  ))
               }

               # Venstre pil hvis LCL er uden for
               if (!is.na(xlim_min) && is.finite(xlim_min) && tbl2_plot$lcl[i] < xlim_min) {
                  arrow_data <- rbind(arrow_data, data.frame(
                     yaxis = tbl2_plot$yaxis[i],
                     x = xlim_min,
                     xend = xlim_min - 0.8,
                     direction = "left",
                     stringsAsFactors = FALSE
                  ))
               }
            }
         }

         # TILFØJ DENNE LINJE - sikrer at tbl2_plot har alle levels fra tbl2
         tbl2_plot$yaxis <- factor(tbl2_plot$yaxis, levels = levels(tbl2$yaxis))

         # Brug ALLE levels fra tbl2, ikke kun dem i tbl2_plot
         middle <- ggplot(tbl2_plot, aes(x = yaxis, y = est, ymin = lcl,
                                         ymax = ucl)) +
            geom_hline(yintercept = if (is_continuous) 0 else 1, colour = "red") +
            scale_x_discrete(limits = rev(levels(tbl2$yaxis)), drop = FALSE) +  # ÆNDRET: tilføjet drop = FALSE
            ylab(unique(tbl$estci_txt))

         if(is_continuous){
            middle <- middle +
               geom_errorbar(width = 0.1,
                             position = position_nudge(x = 0.15)) +
               geom_point(position = position_nudge(x = 0.15))
         }else{
            middle <- middle +
               geom_errorbar(width = 0.1) +
               geom_point()
         }

         # Tilføj pile hvis der er nogen
         if (!is.null(arrow_data) && nrow(arrow_data) > 0) {
            # TILFØJ OGSÅ HER - sikrer arrow_data har samme levels
            arrow_data$yaxis <- factor(arrow_data$yaxis, levels = levels(tbl2$yaxis))
            x_nudge <- 0
            if(is_continuous) x_nudge <- 0.15

            middle <- middle +
               geom_segment(data = arrow_data,
                            aes(x = yaxis, xend = yaxis, y = x, yend = Inf),
                            arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
                            color = "black", linewidth = 0.5,
                            position = position_nudge(x = x_nudge),
                            inherit.aes = FALSE)
         }

         middle <- middle +
            theme_classic() +
            theme(
               strip.text.y = element_text(face = "bold", size = 12),
               legend.position = "none",
               axis.title.y = element_blank(),
               axis.text.y = element_blank(),
               axis.ticks.y = element_blank(),
               axis.line.y = element_blank(),
               plot.margin = margin(15, 20, 5, 5)
            ) +
            coord_flip(ylim = xlim)

      } else {
         # Hvis alle værdier er infinite, lav et tomt plot med besked
         middle <- ggplot() +
            annotate("text", x = 0.5, y = 0.5,
                     label = "No finite confidence intervals to plot",
                     size = 5, color = "gray50") +
            theme_void() +
            theme(plot.margin = margin())
      }

      # Right
      tmp <- cbind(tbl2$yaxis, stack(tbl2[, colnames(tbl2) %in% c("pval", "p_interaction")]))
      colnames(tmp)[1] <- "yaxis"

      right <- ggplot(tmp, aes(y = yaxis, x = ind, label = values)) +
         geom_text(size = 3, vjust = 0.4, lineheight = 0.85) +
         scale_x_discrete(position = "top", expand = expansion(mult = c(0.2, 0.5)),
                          labels = c(paste0("p-value", if(is_continuous) "\n " else ""),
                                     paste0("p-value\nfor interaction", if(is_continuous) "\n " else ""))) +
         scale_y_discrete(limits = rev, drop = FALSE) +
         labs(y = NULL, x = NULL) +
         theme_classic() +
         theme(
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(face = "bold", lineheight = 0.85),
            plot.margin = margin(10, 5, 5, 5)
         )

      print(patchwork::wrap_plots(left, middle, right, widths = c(4, 3, 2)))

   }
}
