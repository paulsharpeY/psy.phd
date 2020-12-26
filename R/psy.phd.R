globalVariables(c('..scaled..'))

#' Set effect size for a study
#'
#' Set effect size for a study.
#'
#' Calculates mean difference and 95% confidence interval.
#'
#' @param id identifier for result
#' @param study data frame with publication column
#' @param m1 mean for group 1
#' @param m2 mean for group 2
#' @param sd standard deviation
#' @param n1 n in group 1
#' @param n2 n in group 2
#' @importFrom rlang .data
#' @export
#' @return tibble
set_effect <- function(id, data, m1, m2, sd, n1, n2) {
  df <- tibble(
    id     = id,
    mean1  = m1,
    mean2  = m2,
    study  = data$publication,
    d      = d(m1, m2, sd), ci = 0, l = 0, u = 0
  )
  df <- df %>%
    mutate(ci = d_ci95(d, n1, n2)) %>%
    mutate(
      l     = d - .data$ci,
      u     = d + .data$ci,
      group = data$group
    )
}

#' Convert 95% confidence interval to standard error
#'
#' Convert 95% confidence interval to standard error
#'
#' https://training.cochrane.org/handbook/current/chapter-06#section-6-3-1
#'
#' @param u upper interval
#' @param l lower interval
#' @export
#' @return numeric
ci95_to_se <- function(u, l) { (u - l) / 3.92 }

#' 95% confidence interval for Cohen's d (Rosnow & Rosenthal, 2009)
#'
#' 95% confidence interval for Cohen's d (Rosnow & Rosenthal, 2009)
#'
#' 95% confidence interval for Cohen's d (Rosnow & Rosenthal, 2009)
#'
#' @param d Cohen's d
#' @param n1 n in group1
#' @param n2 n in group2
#' @export
#' @return numeric
d_ci95 <- function(d, n1, n2) {
  df <- n1 + n2 - 2
  sqrt((((n1 + n2) / (n1 * n2)) + (d^2 / (2 * df))) * ((n1 + n2) / df))
}

#' Mean difference
#'
#' Mean difference
#'
#' Returns (m1 - m2) / sd. If sd is the pooled standard deviation, then this is Hedge's g.
#'
#' @param m1 mean 1
#' @param m2 mean 2
#' @param sd standard deviation
#' @export
#' @return numeric
d <- function(m1, m2, sd) {
  # https://www.statisticshowto.com/hedges-g/
  (m1 - m2) / sd
}

#' Pooled standard deviation
#'
#' Pooled standard deviation
#'
#' Returns the pooled standard deviation for two groups with same or different n.
#'
#' @param n1 n for group 1
#' @param n2 n for group 2
#' @param sd1 standard deviation for group 1
#' @param sd2 standard deviation for group 2
#' @export
#' @return numeric
sd_pooled <- function(n1, n2, sd1, sd2) {
  # https://www.statisticshowto.com/pooled-standard-deviation/
  sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
}

#' Exclude RT outliers
#'
#' Exclude RT outliers.
#'
#' Returns a list of data frames with keys, 'ok', 'too_fast', and 'too_slow',
#' which contain rows from df where the RT met the inclusion critera, was too
#' fast, or too slow respectively.
#'
#' @param df data frame Must contain columns p, rt
#' @param fast numeric Fast RTs
#' @param slow numeric Standard deviation of slow RTs
#' @importFrom dplyr filter group_by ungroup
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats sd
#' @export
#' @return List of data frames
exclude_rt <- function(df, fast=50, slow=3) {
  # Ben:
  # 1.  If you are aggregating data before modelling it then you need to exclude
  # them. I would keep obs > 50ms and < 3 SD (calculated within each __
  # participant__). 200ms is too long a window to be excluding observations for
  # this experiment. There are multiple components to an RT. Although an RT of <
  # 100ms might be implausible for this experiment, I don’t think you can
  # immediately conclude that there is no signal in all responses < 100ms.
  # Multiple errors will summate but may also be correlated - I don’t think you
  # gain enough by dropping the data to warrant it.
  #
  # 2. If you ware running a mixed-model within-participant then you can just
  # include them. Mixed models perform "shrinkage" so that extreme values are
  # down-weighted, and especially so if they are extreme within-participants.
  # Provided you have enough good data they won’t have much effect.

  too_fast <- df %>% filter(.data$rt < fast)
  too_slow <- df %>% filter(.data$rt > mean(.data$rt) + slow * sd(.data$rt))
  ok       <- df %>% filter(.data$rt > fast & .data$rt < mean(.data$rt) + slow * sd(.data$rt))

  list(ok = ok, too_fast = too_fast, too_slow = too_slow)
}

#' Process demographics survey
#'
#' Process demographics survey.
#'
#' @param survey Data frame containing demographics survey results pre-processed by \code{expfactory::process_expfactory_survey}
#' @param participants Data frame with columns p, condition, token
#' @param by String, column to join survey to participants (default='token')
#' @param extra Boolean, process extra demographics columns (default=F)
#' @importFrom dplyr left_join mutate rename select
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom tidyr pivot_wider
#' @export
#' @return Data frame
process_demographics_survey <- function(survey, participants, by = 'token', extra=F) {
  demographics <- survey %>%
    # DEBT: Some data has multiple values for hand which needs, err, handling
    filter(question != 'Which is your dominant hand?') %>%
    pivot_wider(names_from = .data$question, values_from = .data$value) %>%
    rename(
      age = .data$`How old are you (in years)?`,
      sex = .data$`What was your assigned sex at birth?`,
#      hand = .data$`Which is your dominant hand?`
    ) %>%
    mutate(age = as.numeric(.data$age)) %>%
    mutate(sex = replace(.data$sex, .data$sex == 0, 'M')) %>%
    mutate(sex = replace(.data$sex, .data$sex == 1, 'F')) %>%
    mutate(sex = as.factor(.data$sex))

  # always return these columns
  columns = c('p', 'condition', 'age', 'sex')

  # extra demographics survey questions
  if (extra) {
    demographics <- demographics %>%
    rename(
      meditation_times = .data$`Approximately how many times in your life have you meditated?`,
      meditation_hours = .data$`Approximately how many hours in your life have you spent meditating?`
    ) %>%
      mutate(meditation_times = as.numeric(.data$meditation_times),
             meditation_hours = as.numeric(.data$meditation_hours))
    columns <- c(columns, 'meditation_times', 'meditation_hours')
    }

  demographics <- left_join(participants, demographics, by = by) %>%
    select(columns)
}

### util

#' Get participants
#'
#' Get participants.
#'
#' @param df Data frame
#' @param c Condition
#' @importFrom dplyr count filter select ungroup
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#' @return List
#'
get_participants <- function(df, c) {
  p <-
    df %>%
    ungroup() %>%
    filter(.data$condition == c) %>%
    select(p) %>%
    unique()
  list('participants' = p, 'n' = count(p))
}

### Posner

#' Process Posner task data files
#'
#' Process Posner task data files.
#'
#' @param task_file String
#' @param p Numeric Participant number
#' @importFrom dplyr filter mutate select
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom utils read.csv
#' @export
#' @return Data frame
process_posner <- function(task_file, p) {
  if(!file.exists(task_file)){
    return(data.frame(p=p, file=task_file))
  }
  df <- read.csv(task_file, header = TRUE) %>%
    filter(.data$exp_stage == 'test') %>%
    filter(.data$trial_type == 'poldrack-single-stim') %>%
    filter(! is.na(.data$correct_response)) %>%
    select(.data$rt,.data$key_press,.data$stim_duration,.data$correct,
           .data$correct_response,.data$location,.data$cue,.data$trial_num) %>%
    mutate(p = as.numeric(p)) %>%
    mutate(file=task_file)
}

### Attentional Blink

## demographics

#' Sex totals
#'
#' Sex totals.
#'
#' @param df Data frame
#' @importFrom dplyr group_by summarise
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#' @return Data frame
#'
sex_totals <- function(df) {
  df %>%
    group_by(.data$sex) %>%
    summarise(sex_count = length(.data$sex))
}

#' Age summary
#'
#' Age summary.
#'
#' @param df Data frame
#' @importFrom dplyr summarise
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats sd
#' @export
#' @return Data frame
#'
age_summary <- function(df) {
  df %>%
    summarise(mean_age = round(mean(.data$age), 2), sd_age = round(sd(.data$age), 2))
}

#' Age summary by group
#'
#' Age summary by group.
#'
#' @param df Data frame
#' @importFrom dplyr group_by summarise
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats sd
#' @export
#' @return Data frame
#'
age_summary_by_group <- function(df) {
  df %>%
    group_by(.data$condition) %>%
    summarise(mean_age = round(mean(.data$age), 2), sd_age = round(sd(.data$age), 2))
}

## rsvp

#' Accuracy
#'
#' Accuracy.
#'
#' @param x Data frame
#' @param y Something
#' @importFrom dplyr summarise_all
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @return Data frame
#'
accuracy <- function(x, y) {
  x %>% summarise_all(mean) # on a logical vector calculates % TRUE (correct)
}

#' RSVP Accuracy
#'
#' RSVP Accuracy.
#'
#' @param rsvp Data frame
#' @param participants Data frame
#' @param combined Defaults to FALSE
#' @importFrom dplyr do group_by group_modify left_join mutate select ungroup
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#' @return Data frame
#'
rsvp_accuracy <- function(rsvp, participants, combined=FALSE) {
  if (! combined) {
    df <- rsvp %>%
      select(.data$p, .data$lag, .data$t1_correct, .data$t2.t1) %>%
      group_by(.data$p, .data$lag)
  } else {
    df <- rsvp %>%
      select(.data$study, .data$p, .data$lag, .data$t1_correct, .data$t2.t1) %>%
      group_by(.data$study, .data$p, .data$lag)
  }

  # common to uncombined/combined
  df <- df %>%
    group_modify(accuracy) %>%
    ungroup() %>%
    mutate(lag = as.factor(.data$lag), t1_accuracy = .data$t1_correct * 100,
           t2_accuracy = .data$t2.t1 * 100)

  if (! combined) {
    df <- df %>%
      left_join(participants, by=c('p')) %>%
      mutate(p = as.factor(.data$p)) %>%
      select(.data$p, .data$condition, .data$lag, .data$t1_accuracy, .data$t2_accuracy)
  } else {
    df <- df %>%
      left_join(participants, by=c('study','p')) %>%
      mutate(study = as.factor(.data$study), p = as.factor(.data$p)) %>%
      select(.data$study, .data$p, .data$condition, .data$lag, .data$t1_accuracy,
             .data$t2_accuracy)
  }

  as.data.frame(df) # mute later BayesFactor conversion from tibble to data frame
}

#
#' T1 accuracy by participant
#'
#' T1 accuracy by participant.
#'
#' @param rsvp_accuracy Data frame
#' @importFrom dplyr group_by select summarise ungroup
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#' @return Data frame
#'
t1_accuracy <- function(rsvp_accuracy) {
  rsvp_accuracy %>%
    select(.data$p, .data$condition, .data$t1_accuracy) %>%
    group_by(.data$p, .data$condition) %>%
    summarise(mean_t1_accuracy = mean(.data$t1_accuracy)) %>%
    ungroup()
}

#' Attentional blink, descriptive stats
#'
#' Attentional blink, descriptive stats.
#'
#' @param x Data frame
#' @param y Something
#' @importFrom dplyr mutate_if rename summarise
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats sd
#' @export
#' @return Data frame
#'
ab_descriptives <- function(x, y) {
  df <- x %>%
    rename(ab = .data$`blink`) %>%
    summarise(m_t1 = mean(.data$mean_t1_accuracy), sd_t1 = sd(.data$mean_t1_accuracy),
              m_lag3 = mean(.data$`3`), sd_lag3 = sd(.data$`3`),
              m_lag8 = mean(.data$`8`), sd_lag8 = sd(.data$`8`),
              m_ab = mean(.data$ab), sd_ab = sd(.data$ab)) %>%
    mutate_if(is.numeric, round, 2)
}

#' Attentional blink, t-tests
#'
#' Attentional blink, t-tests.
#'
#' @param x Data frame
#' @param y Something!
#' @param lag1 Numeric
#' @param lag2 Numeric
#' @importFrom BayesFactor ttestBF
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats sd t.test
#' @importFrom tibble tibble
#' @export
#' @return Data frame
#'
ab_t_test <- function(x, y, lag1=3, lag2=8) {
  df <- x %>%
    filter(.data$lag %in% c(lag1,lag2))
  # TECH_DEBT: Best I could come up with for returning objects in a data frame i.e. wrap in list()s
  tibble(t = list(t = t.test(t2_accuracy ~ lag, data = df, paired=TRUE)),
         bayes_t = list(bf = ttestBF(x = df$t2_accuracy[df$lag == lag1],
                                     y = df$t2_accuracy[df$lag == lag2],
                                     paired = TRUE))
  )
}

#' Attentional blink contrast t-tests.
#'
#' Attentional blink contrast t-tests.
#'
#' Does _not_ correct for multiple comparisons.
#' t-tests are are for _paired_ samples
#'
#' @param df Data frame
#' @importFrom BayesFactor ttestBF
#' @importFrom dplyr bind_rows filter
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats sd t.test
#' @importFrom tibble tibble
#' @export
#' @return Data frame
#'
blink_contrasts <- function(df) {
  d <- df %>% filter(.data$condition %in% c('fam','omm'))
  # TECH_DEBT: Best I could come up with for returning objects in a data frame i.e. wrap in list()s
  fo <- tibble(
    condition = 'fam-omm',
    t = list(t = t.test(blink ~ condition, data = d, paired=TRUE)),
           bayes_t = list(bf = ttestBF(x = d$blink[d$condition == 'fam'],
                                       y = d$blink[d$condition == 'omm'],
                                       paired = TRUE))
  )
  d <- df %>% filter(.data$condition %in% c('fam','control'))
  fc <- tibble(
    condition = 'fam-control',
    t = list(t = t.test(blink ~ condition, data = d, paired=TRUE)),
    bayes_t = list(bf = ttestBF(x = d$blink[d$condition == 'fam'],
                                y = d$blink[d$condition == 'control'],
                                paired = TRUE))
  )
  d <- df %>% filter(.data$condition %in% c('omm','control'))
  oc <- tibble(
    condition = 'omm-control',
    t = list(t = t.test(blink ~ condition, data = d, paired=TRUE)),
    bayes_t = list(bf = ttestBF(x = d$blink[d$condition == 'omm'],
                                y = d$blink[d$condition == 'control'],
                                paired = TRUE))
  )
  bind_rows(fo,fc,oc)
}

#' Calculate accuracy difference between lag 3 and lag 8
#'
#' Calculate accuracy difference between lag 3 and lag 8.
#'
#' @param rsvp_accuracy_3_8 Data frame
#' @importFrom dplyr group_by mutate select ungroup
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom tidyr spread
#' @export
#' @return Data frame
#'
blink <- function(rsvp_accuracy_3_8) {
  rsvp_accuracy_3_8 %>%
   select(-t1_accuracy) %>%
   group_by(.data$p) %>%
   spread(key = .data$lag, value = .data$t2_accuracy) %>%
   mutate(blink = .data$`8` - .data$`3`) %>%
   ungroup()
}

#' RSVP 3-8 plot
#'
#' RSVP 3-8 plot.
#'
#' @param rsvp_accuracy Data frame
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes element_text geom_hline ggplot mean_cl_boot position_dodge stat_summary theme xlab ylab
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#' @return Data frame
#'
rsvp_3_8_plot <- function(rsvp_accuracy) {
  rsvp_accuracy_3_8 <- rsvp_accuracy %>%
    filter(.data$lag %in% c(3,8))

  rsvp_accuracy_3_8 %>%
    ggplot(aes(.data$lag, .data$t2_accuracy, group=.data$condition, color=.data$condition)) +
    stat_summary(geom="pointrange", fun.data=mean_cl_boot, position=position_dodge(width=.1)) +
    stat_summary(geom="line", fun.data=mean_cl_boot, position=position_dodge(width=.1), size=1.2) +
    geom_hline(yintercept = 0) +
    ylab("T2|T1 accuracy (% with 95% bootstrap CI)") + xlab("Lag") +
    theme(text = element_text(size=22))
}

# DEPRECATED: written against tidystats 0.3 API which changed radically in 0.4
# report_collapsed_t <- function(freq, bayes) {
#   bayes <- as.data.frame(bayes)
#   results <- list()
#   results <- add_stats(results, freq, identifier = 'freq')
#   bf    <- bayes[1,1]
#   if (bf > 100) { bf <- round(bf,0) } else { bf <- round(bf,3) }
#   s     <- report('freq', results = results) # tidy frequentist string
#   s     <- gsub(", 95.*$",'',s)              # remove CI
#   paste0(s,', *BF* = ', bf)
# }

# DEPRECATED: written against tidystats 0.3 API which changed radically in 0.4
# report_t <- function(df, results, c) {
#   bayes <- as.data.frame(df[df[, 'condition'] == c,'bayes_t'][[1]])
#   bf    <- bayes[1,1]
#   if (bf > 100) { bf <- round(bf,0) } else { bf <- round(bf,3) }
#   s     <- report(c, results = results) # tidy frequentist string
#   s     <- gsub(", 95.*$",'',s)         # remove CI
#   paste0(s,', *BF* = ', bf)
# }

# DEPRECATED: written against tidystats 0.3 API which changed radically in 0.4
# report_f <- function(df_n, df_d, f, p) {
#   p <- if(p < .001) '< .001' else paste0('= ',round(p,2))
#   paste0('*F*(', as.integer(df_n), ',', round(df_d,2), ') = ', round(f, 2), ', *p* ', p)
# }

# t = t.test() object, m = mean
sem <- function(t, m) {
  m / t$statistic # SEM = mean / t
}

#' Returns values for Aladins Bayesian t-tests BF_t()
#'
#' Returns values for Aladins Bayesian t-tests BF_t().
#'
#' @param conditions Character vector
#' @param t_df Numeric
#' @param mean_diff_df Numeric
#' @importFrom dplyr case_when filter mutate select
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#' @return Data frame
#'
aladins <- function(conditions, t_df, mean_diff_df) {
  df <- data.frame('condition' = conditions, meandiff = 0, sem = 0, df = 0)
  for (c in conditions) {
    t <- t_df[t_df[, 'condition'] == c,'t'][[1]][[1]]
    ## get values for BF_t() test
    md <- as.numeric(mean_diff_df %>%
                       filter(.data$condition == c) %>%
                       select(.data$meandiff))
    # SEM for condition
    df <- df %>%
      mutate(
        sem      = case_when(.data$condition == c ~ sem(t, md),  TRUE ~ .$sem),
        meandiff = case_when(.data$condition == c ~ md,          TRUE ~ .$meandiff),
        df       = case_when(.data$condition == c ~ t$parameter, TRUE ~ .$df)
      )
  }
  return(df)
}

#' Report Bayesian t-test in APA format.
#'
#' Report Bayesian t-test in APA format.
#'
#' @param bayes Bayesian t-test object
#' @export
#' @return String
#'
apa_bayes_t <- function(bayes) {
  bayes <- as.data.frame(bayes)
  bf    <- bayes[1,1]
  if (bf > 100) { bf <- round(bf,0) } else { bf <- sprintf("%.2f", round(bf,2)) }
  paste0('$BF$ = ', prettyNum(bf, big.mark = ',', trim = TRUE))
}

#' Format Bayes Factor.
#'
#' Format Bayes Factor.
#'
#' @param bf Bayes Factor object
#' @export
#' @return Character
#'
format_bf <- function(bf) {
  bf     <- as.data.frame(bf)
  result <- bf[1,1]
  if (result > 100) { result <- round(result,0) } else { result <- sprintf("%.2f", round(result,2)) }
  prettyNum(result, big.mark = ',', trim = TRUE)
}

#' Extract numbers from aov object.
#'
#' Extract numbers from aov object.
#'
#' @param aov aov object
#' @export
#' @return tibble
#'
extract_aov <- function(aov, effect='1way') {
  aov <- summary(aov)

  # 2 x 2 ANOVA
  if (effect == 'A') {        # A (first main effect)
    tibble(
      df_n = aov[[1]][[1]]$Df[1],
      df_d = aov [[1]][[1]]$Df[2],
      f    = sprintf("%.2f", aov[[1]][[1]]$'F value'[[1]]),
      p    = sub("^(-?)0.", "\\1.", sprintf("%.3f", aov[[1]][[1]]$'Pr(>F)'[[1]]))
    )
  } else if (effect == 'B') { # B (second main effect)
    tibble(
      df_n = aov[[2]][[1]]$Df[1],
      df_d = aov [[2]][[1]]$Df[3],
      f    = sprintf("%.2f", aov[[2]][[1]]$'F value'[[1]]),
      p    = sub("^(-?)0.", "\\1.", sprintf("%.3f", aov[[2]][[1]]$'Pr(>F)'[[1]]))
    )
  } else if (effect == 'A*B') { # A*B
    tibble(
      df_n = aov[[2]][[1]]$Df[1],
      df_d = aov [[2]][[1]]$Df[3],
      f    = sprintf("%.2f", aov[[2]][[1]]$'F value'[[2]]),
      p    = sub("^(-?)0.", "\\1.", sprintf("%.3f", aov[[2]][[1]]$'Pr(>F)'[[2]]))
    )
  } else { # 1-way ANOVA
    tibble(
      df_n = aov[[1]]$Df[1],
      df_d = aov [[1]]$Df[2],
      f    = sprintf("%.2f", aov[[1]]$'F value'[[1]]),
      p    = sub("^(-?)0.", "\\1.", sprintf("%.3f", aov[[1]]$'Pr(>F)'[[1]]))
    )
  }
}

#' Report aov.
#'
#' Report aov.
#'
#' @param aov aov object
#' @export
#' @return Character
#'
report_aov <- function(aov, effect='1way') {
  t <- extract_aov(aov, effect)
  paste0('\\textit{F}(', t$df_n, ', ', t$df_d, ') = ', t$f, ', \\textit{p} = ', t$p)
}

#' Save frequentist and Baysian t-tests and Cohen's d for T1 and T2|T1
#'
#' Save frequentist and Baysian t-tests and Cohen's d for T1 and T2|T1.
#'
#' @param df Data frame
#' @param group1 String
#' @param group2 String
#' @param dir String Directory
#' @param prefix String
#' @importFrom BayesFactor ttestBF
#' @importFrom dplyr group_by mutate summarise
#' @importFrom effsize cohen.d
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats t.test
#' @export
#' @return Data frame
#'
rsvp_by_group <- function(df, group1, group2, dir, prefix) {
  df <- df %>%
    group_by(.data$p, .data$condition) %>%
    summarise(mean_t2_accuracy = mean(.data$t2_accuracy),
              mean_t1_accuracy = mean(.data$t1_accuracy)) %>%
    ungroup()

  # T2|T1
  freq_t <- t.test(mean_t2_accuracy ~ condition, data = df, paired=FALSE)
  bayes_t <- ttestBF(x = df$mean_t2_accuracy[df$condition == group1],
                     y = df$mean_t2_accuracy[df$condition == group2],
                    paired = FALSE)
  save_rds(freq_t, dir, paste0(prefix,'_t2_med_control_freq_t'))
  save_rds(bayes_t, dir, paste0(prefix,'_t2_med_control_bayes_t'))
  t2_d <- cohen.d(df$mean_t2_accuracy ~ df$condition)
  save_rds(t2_d, dir, paste0(prefix,'_t2_med_control_d'))

  # T1
  freq_t <- t.test(mean_t1_accuracy ~ condition, data = df, paired=FALSE)
  bayes_t <- ttestBF(x = df$mean_t1_accuracy[df$condition == group1],
                     y = df$mean_t1_accuracy[df$condition == group2],
                    paired = FALSE)
  save_rds(freq_t, dir, paste0(prefix,'_t1_med_control_freq_t'))
  save_rds(bayes_t, dir, paste0(prefix,'_t1_med_control_bayes_t'))
  t1_d <- cohen.d(df$mean_t1_accuracy ~ df$condition)
  save_rds(t1_d, dir, paste0(prefix,'_t1_med_control_d'))

  return(df)
}

#' Save data as RDS object.
#'
#' Save data as RDS object.
#'
#' @param object R object
#' @param dir String Directory
#' @param filename String Filename
#' @export
#'
save_rds <- function(object, dir, filename) {
  saveRDS(object, paste(dir,paste0(filename,'.Rd'),sep='/'))
}

## bias testing

#' ANT plots
#'
#' ANT plots.
#'
#' @param df Data frame
#' @param condition Character vector length 2
#' @param xlab String
#' @importFrom dplyr group_by mutate summarise
#' @importFrom ggplot2 aes geom_boxplot geom_density geom_vline ggplot xlab ylab
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stringr str_to_upper
#' @export
#' @return Data frame
#'
ant_plots <- function(df, condition, xlab) {
  means <- df %>%
    group_by(.data$condition) %>%
    summarise(mean = mean(.data$rt)) %>%
    mutate(means = paste("Mean", str_to_upper(.data$condition), sep = "."))

  den <- df %>%
    ggplot(aes(.data$rt, colour=.data$condition)) + geom_density(aes(y=..scaled..)) +
    geom_vline(aes(xintercept = mean, color = condition, linetype = means), data = means) +
    xlab(paste0('RT (ms) for ', xlab)) +
    ylab("Scaled density")
  qq_fam     <- qq_plot(df, condition[1], xlab)
  qq_control <- qq_plot(df, condition[2], xlab)

  box_plot <- df %>% ggplot(aes(x = .data$condition, y = .data$rt)) +
    geom_boxplot(aes(colour=.data$condition))

  return(list(boxplot = box_plot, qq_fam = qq_fam, qq_control = qq_control, density = den))
}

#' QQ plot
#'
#' QQ plot.
#'
#' @param df Data frame
#' @param c String Condition
#' @param xlab String
#' @importFrom dplyr group_by mutate summarise
#' @importFrom ggplot2 aes ggplot labs stat_qq stat_qq_line
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#' @return Data frame
#'
qq_plot <- function(df, c, xlab) {
  df %>%
    filter(.data$condition == c) %>%
    ggplot(aes(sample = .data$rt)) + stat_qq() + stat_qq_line() +
    labs(title = paste0('q-q plot for ', c, ' ', xlab))
}

#' Various tests for normality
#'
#' Various tests for normality.
#'
#' @param x Data frame
#' @importFrom dplyr bind_cols
#' @importFrom kableExtra cell_spec kable kable_styling
#' @importFrom magrittr %>%
#' @importFrom psych describe
#' @importFrom purrr pmap_dfr
#' @importFrom rlang .data
#' @importFrom stats ks.test shapiro.test var
#' @importFrom tidyr pivot_wider
#' @export
#' @return Data frame
#'
normal <- function(x) {
  print(shapiro.test(x)) # requires 3-5000 non-NA values
  # ties should not be present for the Kolmogorov-Smirnov test
  # https://stats.stackexchange.com/questions/232011/ties-should-not-be-present-in-one-sample-kolmgorov-smirnov-test-in-r/232067
  print(ks.test(x, 'pnorm', mean=mean(x), sd=sd(x)))

  stats <- bind_cols(describe(x), var = var(x))

  # calculate z-score for skew and kurtosis
  sk <- spssSkewKurtosis(x)
  # https://blog.az.sg/posts/map-and-walk/
  extra <- sk %>%
    pmap_dfr(function(...) {
      current <- tibble(...)
      current %>%
        mutate(z = current$estimate / current$se)
    }) %>%
    pivot_wider(names_from = .data$stat, values_from = c(.data$estimate, .data$se, .data$z))

  bind_cols(stats, extra) %>%
    round(2) %>%
    mutate(
      z_skew = cell_spec(.data$z_skew, 'html',
                         color = ifelse(abs(.data$z_skew) > 1.96, 'red', 'blue'), bold = T),
      z_kurtosis = cell_spec(.data$z_kurtosis, 'html', color = ifelse(abs(.data$z_kurtosis) > 1.96, 'red',
                                                                      'blue'), bold = T),
    ) %>%
    kable(escape = FALSE) %>%
    kable_styling(font_size = 9) # pander(split.table = Inf)
}

#' Skewness and kurtosis and their standard errors as implement by SPSS
#'
#' Skewness and kurtosis and their standard errors as implement by SPSS.
#'
#' @param x Data frame
#' @importFrom tibble add_row
#' @export
#' @return Data frame
#
#' Reference: pp 451-452 of
#' http://support.spss.com/ProductsExt/SPSS/Documentation/Manuals/16.0/SPSS 16.0 Algorithms.pdf
#
#' See also: Suggestion for Using Powerful and Informative Tests of Normality,
#' Ralph B. D'Agostino, Albert Belanger, Ralph B. D'Agostino, Jr.,
#' The American Statistician, Vol. 44, No. 4 (Nov., 1990), pp. 316-321
#'
#' This gives slightly different values for skew and kurtosis to
#' psych::describe(), which doesn't give SE of skew and kurtosis, which
#' you need to test for normality using a z-score
spssSkewKurtosis=function(x) {
  w          <- length(x)
  m1         <- mean(x)
  m2         <- sum((x-m1)^2)
  m3         <- sum((x-m1)^3)
  m4         <- sum((x-m1)^4)
  s1         <- sd(x)
  skew       <- w*m3/(w-1)/(w-2)/s1^3
  sdskew     <- sqrt( 6*w*(w-1) / ((w-2)*(w+1)*(w+3)) )
  kurtosis   <- (w*(w+1)*m4 - 3*m2^2*(w-1)) / ((w-1)*(w-2)*(w-3)*s1^4)
  sdkurtosis <- sqrt( 4*(w^2-1) * sdskew^2 / ((w-3)*(w+5)) )
  df         <- data.frame(stat = 'skew', estimate = skew, se = sdskew)
  df         <- add_row(df, stat = 'kurtosis', estimate = kurtosis, se = sdkurtosis)
  return(df)
}

### ANT

#' ANT descriptives
#'
#' ANT descriptives by score.
#'
#' @param scores Data frame
#' @importFrom dplyr arrange desc group_by mutate recode_factor summarise
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom tidyr pivot_longer pivot_wider unite
#' @export
#' @return Data frame
#'
ant_descriptives <- function(scores) {
  df <- scores %>%
    group_by(.data$t, .data$group, .data$var) %>%
    summarise(mean_rt=sprintf("%.2f", round(mean(.data$rt), 2)),
              sd_rt=sprintf("%.2f", round(sd(.data$rt), 2))) %>%
    ungroup() %>%
    pivot_wider(names_from = var, values_from = c(.data$mean_rt, .data$sd_rt)) %>%
    unite('alerting', .data$mean_rt_alerting, .data$sd_rt_alerting, sep=' (') %>%
    unite('orienting', .data$mean_rt_orienting, .data$sd_rt_orienting, sep=' (') %>%
    unite('conflict', .data$mean_rt_conflict, .data$sd_rt_conflict, sep=' (') %>%
    mutate(alerting = paste0(.data$alerting, ')'), orienting = paste0(.data$orienting, ')'),
           conflict = paste0(.data$conflict, ')'))

  df$t <- recode_factor(df$t, `1` = 'Pre', `2` = 'Post')
  df %>%  pivot_longer(cols = c(.data$alerting, .data$orienting, .data$conflict),
                       names_to = 'score', values_to = 'value') %>%
    pivot_wider(names_from = c(t), values_from = .data$value) %>%
    arrange(desc(.data$group)) %>%
    mutate(score = stringr::str_to_title(.data$score))
}

#' ANT descriptives by cue type
#'
#' ANT descriptives by cue type.
#'
#' @param df Data frame
#' @importFrom dplyr arrange desc group_by mutate summarise
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom tidyr pivot_longer pivot_wider unite
#' @export
#' @return Data frame
#'
ant_descriptives_by_cue_type <- function(df) {
  df %>%
    group_by(.data$t, .data$group, .data$cue, .data$flanker_type) %>%
    summarise(mean_rt=round(mean(.data$rt)), sd_rt=round(sd(.data$rt))) %>%
    ungroup() %>%
    pivot_wider(names_from = .data$flanker_type, values_from = c(.data$mean_rt, .data$sd_rt)) %>%
    unite('congruent', .data$mean_rt_congruent, .data$sd_rt_congruent, sep=' (') %>%
    unite('incongruent', .data$mean_rt_incongruent, .data$sd_rt_incongruent, sep=' (') %>%
    unite('neutral', .data$mean_rt_neutral, .data$sd_rt_neutral, sep=' (') %>%
    mutate(congruent = paste0(.data$congruent, ')'), incongruent = paste0(.data$incongruent, ')'),
           neutral = paste0(.data$neutral, ')')) %>%
    pivot_longer(cols = c(.data$congruent, .data$incongruent, .data$neutral), names_to = 'target', values_to = 'value') %>%
    pivot_wider(names_from = c(.data$t, .data$cue), values_from = .data$value) %>%
    arrange(desc(.data$group)) %>%
    mutate(target = sub("(.)", "\\U\\1", .data$target, perl=TRUE))
}

#' Compute ANT scores for alerting, orienting and conflict
#'
#' Compute ANT scores for alerting, orienting and conflict.
#'
#' @param df Data frame
#' @importFrom dplyr group_by left_join mutate select
#' @importFrom forcats fct_relevel
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom tidyr pivot_longer pivot_wider unite
#' @export
#' @return Data frame
#'
ant_scores <- function(df) {
  alerting_orienting <- df %>%
    pivot_wider(id_cols = c(.data$p,.data$group,.data$t), names_from = .data$cue,
                values_from = .data$rt, values_fn = list(rt = mean)) %>%
    mutate(alerting = .data$nocue - .data$double, orienting = .data$center - .data$spatial) %>%
    select(.data$p, .data$group, .data$t, .data$alerting, .data$orienting)
  conflict <- df %>%
    pivot_wider(id_cols = c(.data$p,.data$group,.data$t),
                names_from = .data$flanker_type, values_from = .data$rt,
                values_fn = list(rt = mean)) %>%
    mutate(conflict = .data$incongruent - .data$congruent) %>%
    select(.data$p, .data$group, .data$t, .data$conflict)
  result <- left_join(alerting_orienting, conflict, by=c('p', 'group', 't')) %>%
    pivot_longer(cols = c(.data$alerting, .data$orienting, .data$conflict),
                  names_to = 'var', values_to = 'rt') %>%
    mutate(var = factor(var))
  # arrange for plot facets to be LtR: Alerting, Orienting, Conflict
  result$var <- fct_relevel(result$var, 'conflict', after = Inf)
  return(result)
}

#' Generate a forest plot from a brms model
#'
#' Generate a forest plot from a brms model.
#'
#' @param df Data frame
#' @param subgroup String
#' @param var String
#' @param xlab String
#' @importFrom brms brm get_prior hypothesis
#' @importFrom dplyr bind_rows filter mutate select
#' @importFrom ggplot2 geom_density geom_text geom_vline ggplot ggsave theme_bw xlab
#' @importFrom ggridges geom_density_ridges
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stringr str_replace
#' @importFrom tidybayes geom_pointintervalh mean_qi spread_draws
#' @export
#' @return ggplot
#'
forest_bayes <- function(df, subgroup, var, xlab = 'Standardised Mean Difference') {
  # calculate SEM from CI
  df <- df %>%
    filter(score == var & group == subgroup) %>%
    mutate(se = ci95_to_se(u, l), study = factor(study))

  # this shows you the default priors
  get_prior(d | se(se) ~ 1 + (1 | study), data=df)

  df$study <- str_replace(df$study, ",", "")  # remove commas in study names
  # to rebuild model delete cached file var'-rem'
  rem <- brm(
    d | se(se) ~ 1 + (1 | study),
    data = df,
    chains=8, iter=1e4, # FIXME iter=50e4 or 10e4 (Ben: 20e4)
    file = paste(subgroup, var, 'rem', sep = '-')
  )

  # Vuorre also used control=list(adapt_delta = .99). What does it do?

  # extract the posterior samples...
  posterior_samples <- rem %>% as.data.frame()

  # this dataframe has one column per parameter in the model
  # (inluding the random effects, so you get each study's divergence from the mean too)
  posterior_samples %>% names

  # posterior credible interval
  posterior_samples %>% select(b_Intercept) %>% mean_qi()

  # posterior plot
  posterior_samples %>%
    ggplot(aes(b_Intercept)) +
    geom_density() + xlab("Pooled effect size (posterior density)")

  # test of the hypothesis that the pooled effect is larger than .3 or smaller than -.3
  # the Evid.Ratio here is a BayesFactor so we have reasonable evidence against.
  hypothesis(rem, "abs(Intercept)>.3")

  # or calculate the other way. BF=4 that the effect is smaller than < .3, even with so few studies
  hypothesis(rem, "abs(Intercept)<.3")

  # Forest plot
  # Based on https://vuorre.netlify.com/post/2017/01/19/better-forest-plots-from-meta-analytic-models-estimated-with-brms/
  # Don't use brmstools::forest() as brmstools is deprecated
  # This is from https://github.com/mvuorre/brmstools

  # For an explanation of tidybayes::spread_draws(), refer to http://mjskay.github.io/tidybayes/articles/tidy-brms.html
  # Study-specific effects are deviations + average
  out_r <- spread_draws(rem, r_study[study, term], b_Intercept) %>%
    mutate(b_Intercept = r_study + b_Intercept) %>%
    mutate(study = str_replace_all(study, "\\.\\.", "."))

  # add study type for faceting
  # type <- studies %>%
  #   select(publication, type) %>%
  #   mutate(study = str_replace_all(publication, "[, ]", ".")) %>%
  #   mutate(study = str_replace_all(study, "\\.\\.", ".")) %>%
  #   select(-publication)

  #out_r <- left_join(out_r, type, by='study')

  # Average effect
  out_f <- spread_draws(rem, b_Intercept) %>%
    mutate(study = "Average")
  # Combine average and study-specific effects' data frames
  out_all <- bind_rows(out_r, out_f) %>%
    ungroup() %>%
    mutate(study = fct_relevel(study, "Average")) # put Average effect at bottom of the forest plot
  # Data frame of summary numbers
  out_all_sum <- group_by(out_all, study) %>% mean_qi(b_Intercept)
  #  out_all_sum <- left_join(out_all_sum, type, by='study')

  # tidy study names
  #out_r <- out_r %>% mutate(study = recode(study, !!!study.recode))
  #out_all_sum <- out_all_sum %>% mutate(study = recode(study, !!!study.recode))
  #  out_all <- out_all %>% mutate(study = recode(study, !!!study.recode))

  # FIXME: average by group for RCT and CT
  #  out_all_sum <- out_all_sum %>% filter(study != 'Average')
  #  out_all <- out_all %>% filter(study != 'Average')

  #> Warning: unnest() has a new interface. See ?unnest for details.
  #> Try `cols = c(.lower, .upper)`, with `mutate()` needed
  # Draw plot
  forest <- out_all %>%
    ggplot(aes(b_Intercept, study)) +
    geom_density_ridges(
      rel_min_height = 0.01,
      col = NA,
      scale = 1
    ) +
    geom_pointintervalh(
      data = out_all_sum, size = 1
    ) +
    geom_text(
      data = mutate_if(out_all_sum, is.numeric, round, 2),
      # Use glue package to combine strings
      aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf),
      hjust = "inward"
    ) +
    geom_vline(xintercept = 0) +
    #    facet_grid(type ~ ., scales = 'free') +
    xlab(xlab) +
    theme_bw()

  # save plot
  ggsave(filename = paste0('figures/', subgroup, '_', var, '_forest.pdf'), plot = forest,
         units = "in", width = 8.27, height = 11.69)

  return(forest)
}
