globalVariables(c('..scaled..'))

#' Exclude RT outliers
#'
#' Exclude RT outliers.
#'
#' @param df Data frame Must contain columns p, rt
#' @param fast Numeric Fast RTs
#' @param slow Numeric Standar deviation of slow RTs
#' @importFrom dplyr filter group_by ungroup
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats sd
#' @export
#' @return List of data frames
# Excludes outliers from df.
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

  g <- df %>% group_by(.data$p)
  too_fast <- g %>% filter(.data$rt < fast) %>% ungroup()
  too_slow <- g %>% filter(.data$rt > mean(.data$rt) + slow * sd(.data$rt)) %>% ungroup()
  ok <- g %>% filter(.data$rt > fast & .data$rt < mean(.data$rt) + slow * sd(.data$rt)) %>% ungroup()

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
#' @importFrom tidyr spread
#' @export
#' @return Data frame
process_demographics_survey <- function(survey, participants, by = 'token', extra=F) {
  columns <- c('p', 'condition', 'age', 'sex', 'hand')
  demographics <- survey %>%
    spread(key = .data$question, value = .data$value) %>%
    rename(
      age = .data$`How old are you (in years)?`,
      sex = .data$`What was your assigned sex at birth?`,
      hand = .data$`Which is your dominant hand?`
    ) %>%
    mutate(age = as.numeric(.data$age)) %>%
    mutate(sex = replace(.data$sex, .data$sex == 0, 'M')) %>%
    mutate(sex = replace(.data$sex, .data$sex == 1, 'F')) %>%
    mutate(sex = as.factor(.data$sex))

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
  if (bf > 100) { bf <- round(bf,0) } else { bf <- round(bf,2) }
  paste0('$BF$ = ', prettyNum(bf, big.mark = ',', trim=TRUE))
}

#' FAM+OMM vs. control
#'
#' FAM+OMM vs. control.
#'
#' @param df Data frame
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
collapse_meditation <- function(df, dir, prefix) {
  collapsed <- df %>%
    group_by(.data$p, .data$condition) %>%
    summarise(mean_t2_accuracy = mean(.data$t2_accuracy),
              mean_t1_accuracy = mean(.data$t1_accuracy)) %>%
    ungroup %>%
    mutate(condition = ifelse(.data$condition == 'omm' | .data$condition == 'fam', 'meditation', 'control')) %>%
    mutate(condition = as.factor(.data$condition))

  # T2|T1
  freq_t <- t.test(mean_t2_accuracy ~ condition, data = collapsed, paired=FALSE)
  bayes_t <- ttestBF(x = collapsed$mean_t2_accuracy[collapsed$condition == 'meditation'],
                    y = collapsed$mean_t2_accuracy[collapsed$condition == 'control'],
                    paired = FALSE)
  save_rds(freq_t, dir, paste0(prefix,'_t2_med_control_freq_t'))
  save_rds(bayes_t, dir, paste0(prefix,'_t2_med_control_bayes_t'))
  t2_d <- cohen.d(collapsed$mean_t2_accuracy ~ collapsed$condition)
  save_rds(t2_d, dir, paste0(prefix,'_t2_med_control_d'))

  # T1
  freq_t <- t.test(mean_t1_accuracy ~ condition, data = collapsed, paired=FALSE)
  bayes_t <- ttestBF(x = collapsed$mean_t1_accuracy[collapsed$condition == 'meditation'],
                    y = collapsed$mean_t1_accuracy[collapsed$condition == 'control'],
                    paired = FALSE)
  save_rds(freq_t, dir, paste0(prefix,'_t1_med_control_freq_t'))
  save_rds(bayes_t, dir, paste0(prefix,'_t1_med_control_bayes_t'))
  t1_d <- cohen.d(collapsed$mean_t1_accuracy ~ collapsed$condition)
  save_rds(t1_d, dir, paste0(prefix,'_t1_med_control_d'))

  return(collapsed)
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
