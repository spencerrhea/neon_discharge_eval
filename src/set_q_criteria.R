
# This function can be used to set your own criteria to bin NEON's Continuous
# Discharge data into tiers based on your needs or preferences. This function
# allows you to set:
# 1. NSE thresholds to classify rating curves,
# 2. NSE thresholds to classify stage-gauge relationships, and
# 3. percentage thresholds for continuous discharge values extrapolated beyond field measurements.

# This function operates on the neon NEON Continuous Discharge Evaluation found on
# HydroShare: https://www.hydroshare.org/resource/03c52d47d66e40f4854da8397c7d9668/

# For more information about the evaluation dataset itself, see this
# publication: https://doi.org/10.1038/s41597-023-01983-w


#### Function information ####

## Arguments
# q_eval_data = the NEON continuous discharge evaluation table downloaded from
    # HydroShare
# rating_curve_NSE = a numeric vector with two NSE values that set the cutoffs
    # for Tier 1 and Tier 2 rating curves. Values should be between 0 and 1
# percent_q_extrapolated = a numeric vector with two cutoffs for Tier 1 and Tier 2
    # rating curves that indicate the amount of continuous discharge data extrapolated
    # beyond field measurements of discharge. If you wanted to ensure no more than
    # 15% of reported discharge values were above the maximum manually measured
    # discharge value in Tier 1 and no more than 30% in Tier 2 you would set this
    # argument to c(15, 30)
# stage_gauge_NSE = The minimum NSE value for the relationship between stage
    # height and manual gauge height. For all NSEs less than the value set here,
    # the custom_final_qual will read 'regression_flag'
# check_drift = A Boolean argument for use of drift detection in Rhea et al. (2023).
    # If TRUE, when a drift flag is reported in the original evaluation dataset
    # the custom_final_qual column will read "drift_flag"

## Value
# This function returns a table with a new column called 'custom_final_qual' that
# uses the classification schema in Rhea et al. (2023) but with user-specified
# thresholds. All defaults are set to the classification schema in Rhea et al. (2023),
# but users are encouraged to set their own criteria based on their needs.


#### Function ####

library(tidyverse)
set_q_criteria <- function(q_eval_data, rating_curve_NSE = c(0.9, 0.75),
                           percent_q_extrapolated = c(15, 30),
                           stage_gauge_NSE = 0.9, check_drift = TRUE) {

    # Check function inputs
    if(! is.numeric(rating_curve_NSE) | ! is.vector(rating_curve_NSE) | ! length(rating_curve_NSE) == 2){
        stop('rating_curve_NSE must be a numeric vector with 2 values')
    }

    if(any(rating_curve_NSE < 0) | any(rating_curve_NSE > 1)){
        stop('rating_curve_NSE values must be between 0 and 1')
    }

    if(! is.numeric(percent_q_extrapolated) | ! is.vector(percent_q_extrapolated) | ! length(percent_q_extrapolated) == 2){
        stop('percent_q_extrapolated must be a numeric vector with 2 values')
    }

    if(any(percent_q_extrapolated < 0) | any(percent_q_extrapolated > 100)){
        stop('percent_q_extrapolated values must be between 0 and 100')
    }

    if(! is.numeric(stage_gauge_NSE) | ! is.vector(stage_gauge_NSE) | ! length(stage_gauge_NSE) == 1){
        stop('stage_gauge_NSE must be a single numeric value')
    }

    if(any(stage_gauge_NSE < 0) | any(stage_gauge_NSE > 1)){
        stop('stage_gauge_NSE must be between 0 and 1')
    }

    rating_curve_NSE_tier_2 <- min(rating_curve_NSE)
    rating_curve_NSE_tier_1 <- max(rating_curve_NSE)

    percent_q_extrapolated_tier_2 <- max(percent_q_extrapolated)
    percent_q_extrapolated_tier_1 <- min(percent_q_extrapolated)

    # Apply classification
    rc_nse1 <- q_eval_data$rating_curve_NSE >= rating_curve_NSE_tier_1
    rc_nse2 <- q_eval_data$rating_curve_NSE > rating_curve_NSE_tier_2 &
        q_eval_data$rating_curve_NSE < rating_curve_NSE_tier_1
    rc_nse3 <- q_eval_data$rating_curve_NSE <= rating_curve_NSE_tier_2

    extrap1 <- q_eval_data$rating_curve_above_range <= percent_q_extrapolated_tier_1
    extrap2 <- q_eval_data$rating_curve_above_range <= percent_q_extrapolated_tier_2 &
        q_eval_data$rating_curve_above_range > percent_q_extrapolated_tier_1
    extrap3 <- q_eval_data$rating_curve_above_range > percent_q_extrapolated_tier_2

    q_eval_data_custom <- q_eval_data %>%
        mutate(custom_regression_status = ifelse(regression_NSE >= stage_gauge_NSE,
                                                 'good',
                                                 'poor')) %>%
        mutate(custom_regression_status = ifelse(is.na(custom_regression_status),
                                                 'good',
                                                 custom_regression_status)) %>%
        mutate(custom_rating_curve_status = case_when(rc_nse1 & extrap1 ~ 'Tier1',
                                                      rc_nse2 | extrap2 ~ 'Tier2',
                                                      rc_nse3 | extrap3 ~ 'Tier3')) %>%
        mutate(custom_final_qual = ifelse(custom_regression_status == 'good',
                                          custom_rating_curve_status,
                                          'regression_flag'))

    if(check_drift){
        q_eval_data_custom <- q_eval_data_custom %>%
            mutate(custom_final_qual = ifelse(is.na(drift_status),
                                              custom_final_qual,
                                              ifelse(drift_status == 'potential_drift' & !custom_regression_status == 'poor',
                                                     'drift_flag',
                                                     custom_final_qual)))
    }
}

#### Example ####

# # Uncomment to run example #
# # Download and read CSV from HydroShare
# tmp <- tempdir()
# download.file('https://www.hydroshare.org/resource/1a388391632f4277992889e2de152163/data/contents/neon_q_eval.csv',
#               file.path(tmp, 'neon_q_eval.csv'))
# q_eval_data <- read_csv(file.path(tmp, 'neon_q_eval.csv'))
# 
# # Use custom evaluation criteria
# q_eval_data_custom <- set_q_criteria(
#     q_eval_data,
#     rating_curve_NSE = c(0.75, 0.5),
#     percent_q_extrapolated = c(15, 30),
#     stage_gauge_NSE = 0.7,
#     check_drift = TRUE
# )
