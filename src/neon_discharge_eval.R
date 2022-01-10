
# This script evaluates the NEON discharge data in three ways, first the rating 
# curve is evaluated to determine if the data passes a set of criteria for use.
# Site rating curves will fall into the following categories:
# Tier 1
    # Rating curve has an R2 of 0.9 or greater AND
    # No more than 15 percent of the reported discharge values fall outside 
        # of the range of manual gauged measurements
# Tier 2
    # Rating curve has an R2 > 0.75 and R2 < 0.9 OR
    # No more than 30 percent of the reported discharge values fall outside 
        # of the range of manual gauged measurements 
# Tier 3
    # Rating curve has an R2 < 0.75 OR
    # 30 percent of the reported discharge values fall outside 
        # of the range of manual gauged measurements 

# The second evaluation is to detect potential drift in the pressure transducer
# measuring contentious stage. Here we use the manual stage measurements and 
# take a difference between the manual and continuous stage to see when the 
# sensor may be off. This evaluation happens for every month per site with 
# three categories:
    # Likely no drift
        # The mean difference between the manual and continuous stage measurements 
            # is less than 6 cm 
        # No more than 50 percent of the the manual stage measurements fall outside
            # NEON reported uncertainty
    # Potential drift 
        # The mean difference between the manual and continuous stage measurements 
            # is greater than 6 cm
        # More than 50 percent of the the manual stage measurements fall outside
            # NEON reported uncertainty
    # Not assessed 
        # There are not any manual stage measurements in that month

# The final evaluation is to determine the reliability of the pressure-stage
# relationship. To evaluate this relationship we use the r-squared values of the
# relationship between the continuous and manual stage height readings. We then 
# classified each regression into the following categories: 
    # Good
        # r-squared of 0.9 or higher 
        # r-squared between 0.75 and 0.9
        # r-squared of less than 0.75

#### Set up ####

library(tidyverse)
library(lubridate)
library(glue)

# Set working directory 
setwd('../timeseries_experimentation/neon_discharge_eval/')

dir.create('data/data_in/neon',
           recursive = TRUE,
           showWarnings = FALSE)

# Download data 
req = httr::GET(paste0("http://data.neonscience.org/api/v0/products/",
                       'DP4.00130.001'))
txt = httr::content(req, as="text")
neondata = jsonlite::fromJSON(txt, simplifyDataFrame=TRUE, flatten=TRUE)

urls = unlist(neondata$data$siteCodes$availableDataUrls)

avail_sets = stringr::str_match(urls,
                                '(?:.*)/([A-Z]{4})/([0-9]{4}-[0-9]{2})') %>%
    as_tibble(.name_repair='unique') %>%
    rename(url=`...1`, site_code=`...2`, component=`...3`)

neon_streams <- unique(avail_sets$site_code)

# Download continuous discharge 
for(i in 1:length(neon_streams)){

    site_code <- neon_streams[i]
    avail_site_sets <- avail_sets[avail_sets$site_code == site_code, ,
                                  drop=FALSE]
    
    avail_site_sets <- avail_sets %>%
        filter(site_code == !!site_code)
    
    if(! nrow(avail_site_sets)) next

    for(j in 1:nrow(avail_site_sets)){
        
        data_pile <- try(neonUtilities::loadByProduct('DP4.00130.001',
                                                      site=site_code, 
                                                      startdate=avail_site_sets$component[j],
                                                      enddate=avail_site_sets$component[j],
                                                      package='expanded', check.size=FALSE),
                         silent = TRUE)
        
        folder_name <- glue('NEON.D##.{s}.DP4.00130.{c}.expanded',
                             s = site_code,
                             c = avail_site_sets$component[j])
        file_name_dis <- glue('NEON.D04.{s}.DP4.00130.100.100.001.csd_continuousDischarge.{c}.expanded.csv',
                             c = avail_site_sets$component[j],
                             s = site_code)
        file_name_stage <- glue('NEON.D04.{s}.DP4.00130.100.100.001.sdrc_gaugePressureRelationship.{c}.expanded.csv',
                              c = avail_site_sets$component[j],
                              s = site_code)
        raw_data_dest <- glue('{wd}/data/data_in/neon/NEON_discharge-continuous/{p}',
                              wd = getwd(),
                              p = folder_name)

        dir.create(raw_data_dest, recursive = T, showWarnings = F)
        
        raw_data_dest_dis <- glue('{p}/{c}',
                              p = raw_data_dest,
                              c = file_name_dis)
        raw_data_dest_stage <- glue('{p}/{c}',
                                  p = raw_data_dest,
                                  c = file_name_stage)

        
        if(!is.null(data_pile$sdrc_gaugePressureRelationship)) {
            write_csv(data_pile$sdrc_gaugePressureRelationship, raw_data_dest_stage)
        } 
        
        if(!is.null(data_pile$csd_continuousDischarge)) {
            write_csv(data_pile$csd_continuousDischarge, raw_data_dest_dis)
        } 

    }
    
}

# Download rating curve info
req = httr::GET(paste0("http://data.neonscience.org/api/v0/products/",
                       'DP4.00133.001'))
txt = httr::content(req, as="text")
neondata = jsonlite::fromJSON(txt, simplifyDataFrame=TRUE, flatten=TRUE)

urls = unlist(neondata$data$siteCodes$availableDataUrls)

avail_sets = stringr::str_match(urls,
                                '(?:.*)/([A-Z]{4})/([0-9]{4}-[0-9]{2})') %>%
    as_tibble(.name_repair='unique') %>%
    rename(url=`...1`, site_code=`...2`, component=`...3`)


for(i in 1:length(neon_streams)){
    
    site_code <- neon_streams[i]
    avail_site_sets <- avail_sets[avail_sets$site_code == site_code, ,
                                  drop=FALSE]
    
    if(! nrow(avail_site_sets)) next
    
    for(j in 1:nrow(avail_site_sets)){
        
        data_pile <- try(neonUtilities::loadByProduct('DP4.00133.001',
                                                      site=site_code, 
                                                      startdate=avail_site_sets$component[j],
                                                      enddate=avail_site_sets$component[j],
                                                      package='expanded', check.size=FALSE),
                         silent = TRUE)
        
        folder_name <- glue('NEON.D##.{s}.DP4.00133.{c}.expanded',
                            s = site_code,
                            c = avail_site_sets$component[j])
        file_name_dis <- glue('NEON.D04.{s}.DP4.00133.100.100.001.sdrc_resultsResiduals.{c}.expanded.csv',
                              c = avail_site_sets$component[j],
                              s = site_code)
        raw_data_dest <- glue('{wd}/data/data_in/neon/NEON_discharge-rating-curves/{p}',
                              wd = getwd(),
                              p = folder_name)
        
        dir.create(raw_data_dest, recursive = TRUE)
        
        raw_data_dest <- glue('{p}/{c}',
                                  p = raw_data_dest,
                                  c = file_name_dis)
        
        
        if(!is.null(data_pile$sdrc_resultsResiduals)) {
            write_csv(data_pile$sdrc_resultsResiduals, raw_data_dest)
        }
    }
}

# Identify relevant files
neon_q_fils <- list.files('data/data_in/neon/NEON_discharge-continuous/',
                          full.names = T)

# Get unique sites
sites <- unique(str_match(neon_q_fils, '[DP###\\.]([A-Z]{4})')[,2])
sites <- sites[nchar(sites) == 4]

# Remove TOOK and add the two sub sites (THis is need thoughout becuase TOOK 
# is the only NEON site with two stream gauges)
sites <- sites[! grepl('TOOK', sites)]
sites <- c(sites, 'TKIN', 'TKOT') 

# Get rating curve files
all_ratings_fil <- list.files('data/data_in/neon/NEON_discharge-rating-curves/',
                              full.names = T)

# Loop reads in discharge and rating curve data for each sites, applies the various
# tests, and plots the data with shading to indicate flagging
dir.create('plots/',
           showWarnings = FALSE)
all_sites_curve <- tibble()
all_sites_drift <- tibble()
for(i in 1:length(sites)) {

    # Site
    site <- sites[i]

    # Continuous stage 
    cont_stage_files <- list.files('data/data_in/neon/NEON_discharge-continuous/', 
                             full.names = TRUE, recursive = TRUE)

    # Load in continuous Q
    if(site %in% c('TKIN', 'TKOT')){
        
        cont_stage_files <- grep('TOOK', cont_stage_files, value = TRUE)
        cont_stage <- grep('continuousDischarge', cont_stage_files, value = TRUE)
        
        all_q <- suppressMessages(map_dfr(cont_stage, read_csv))
        
        all_q <- all_q %>%
            mutate(siteID = ifelse(stationHorizontalID == 150, 'TKIN', 'TKOT')) %>%
            filter(siteID == !!site)
    } else{
        cont_stage_files <- grep(site, cont_stage_files, value = TRUE)
        cont_stage <- grep('continuousDischarge', cont_stage_files, value = TRUE)
        
        all_q <- suppressMessages(map_dfr(cont_stage, read_csv))
    }

    # Read in manual gauging
    stage_q_rel <- grep('gaugePressure', cont_stage_files, value = TRUE)
    all_manual_stage <- suppressMessages(map_dfr(stage_q_rel, read_csv)) %>%
        select(endDate, gaugeHeight, stationHorizontalID)
    
    if(site %in% c('TKIN', 'TKOT')){
        all_manual_stage <- all_manual_stage %>%
            mutate(siteID = ifelse(stationHorizontalID == 150, 'TKIN', 'TKOT')) %>%
            select(-stationHorizontalID)
    } else{
        all_manual_stage <- all_manual_stage %>%
            select(-stationHorizontalID)
    }


    #### Check continuous stage with manual measured stage height
    stage_verify <- full_join(all_q, all_manual_stage, by = 'endDate')

    drift_check <- stage_verify %>%
        mutate(year = year(endDate),
               month = month(endDate)) %>%
        filter(!is.na(gaugeHeight)) %>%
        mutate(dif = abs(gaugeHeight-equivalentStage)) %>%
        mutate(outside_uncert = ifelse(gaugeHeight > equivalentStage+stageUnc | gaugeHeight < equivalentStage-stageUnc, 1, 0)) %>%
        group_by(year, month) %>%
        summarise(n = n(),
                  mean_dif = mean(dif, na.rm = TRUE),
                  outside_uncert = sum(outside_uncert, na.rm = TRUE),
                  mean_uncertaintiy = mean(stageUnc, na.rm = TRUE)) %>%
        mutate(prop_outside = outside_uncert/n)
    
    years <- unique(drift_check$year)
    all_years_months <- tibble()
    for(k in 1:length(years)) {
        this_year <- tibble(year = years[k],
                            month = 1:12)
        
        all_years_months <- rbind(all_years_months, this_year)
    }

    # Flag data that has many points falling outside neon uncertainty and 
    # months where the mean difference between continuous stage and manual 
    # stage measurements are greater that 6 cm
    drift_months <- drift_check %>%
        mutate(drift_status = ifelse(prop_outside >= 0.5 | mean_dif >= 0.06, 'potential_drift', 'likely_no_drift')) %>%
        full_join(., all_years_months, by = c('year', 'month')) %>%
        mutate(drift_status = ifelse(is.na(n), 'not_assessed', drift_status)) %>%
        mutate(site = !!site) %>%
        mutate(drift_status = ifelse(is.na(drift_status), 'not_assessed', drift_status)) 

    all_sites_drift <- rbind(all_sites_drift, drift_months)


    # Check Rating Curves 
    if(site %in% c('TKIN', 'TKOT')){ site <- 'TOOK'}
    site_fil <- grep(site, all_ratings_fil, value = T)
    site_fil <- list.files(site_fil, full.names = T)
    site_fil <- grep('sdrc_resultsResiduals', site_fil, value = T)
    
    all_rating_red <- suppressMessages(map_dfr(site_fil, read_csv))
    
    if(site == 'TOOK'){
        site <- sites[i]
        all_rating_red <- all_rating_red %>%
            mutate(site = str_split_fixed(curveID, '[.]', n = Inf)[,1]) %>%
            filter(site == !!site) %>%
            mutate(siteID = site) %>%
            select(-site)
    }

    curves <- unique(all_rating_red$curveID)

    site_file <- paste0(site, '.', 'DP4.00130.001.')

    all_curves_r_new <- tibble()
    for(p in 1:length(curves)){
        site <- str_split_fixed(curves[p], '[.]', n = Inf)[1,1]
        year <- str_split_fixed(curves[p], '[.]', n = Inf)[1,2]

        if(! nchar(year) == 4){
            year <- str_split_fixed(year, '-', n = Inf)[1,1]
        }

        # Get con. Q files 
        if(site %in% c('TKIN', 'TKOT')) {
            site <- 'TOOK'
        }

        all_q_curve <- all_q %>%
            filter(!is.na(maxpostDischarge)) %>%
            filter(curveID == curves[p])

        if(nrow(all_q_curve) == 0) next

        start_date <- min(all_q_curve$endDate)
        end_date <- max(all_q_curve$endDate)

        # R2 
        year_curve <- all_rating_red %>%
            filter(curveID == curves[p])

        residual <- year_curve$Y1residual
        simulated <- year_curve$Y1simulated
        R.squared = 1 - sum((residual)^2)/sum((simulated-mean(simulated))^2)

        gaugings_n <- nrow(year_curve)

        # Data outside range 
        max_q <- max(year_curve$Y1unbiased, na.rm = T)
        min_q <- min(year_curve$Y1unbiased, na.rm = T)

        total_n <- all_q_curve %>%
            nrow()
        n_max_over <- all_q_curve %>%
            filter(maxpostDischarge >= max_q) %>%
            nrow()
        n_min_under <- all_q_curve %>%
            filter(maxpostDischarge <= min_q) %>%
            nrow()

        above_range <- round(((n_max_over) / total_n) * 100, 2)
        under_range <- round(((n_min_under) / total_n) * 100, 2)
        outside_range <- round(((n_max_over + n_min_under) / total_n) * 100, 2)

        # Put data into tib
        lit_tib <- tibble(curveID = curves[p],
                          r_squared = R.squared,
                          above_range = above_range,
                          under_range = under_range,
                          outside_range = outside_range,
                          start_date = min(all_q_curve$endDate),
                          end_date = max(all_q_curve$endDate),
                          cont_n = total_n,
                          gaugings_n = gaugings_n)

        all_curves_r_new <- rbind(all_curves_r_new, lit_tib)
    }

    site <- sites[i]

    # Three tiers for rating curves
    all_curves_fin <- all_curves_r_new %>%
        mutate(site = str_split_fixed(curveID, '[.]', n = Inf)[,1],
               curve_year = str_split_fixed(curveID, '[.]', n = Inf)[,2],
               year = str_split_fixed(curve_year, '-', n = Inf)[,1]) %>%
        mutate(curve_pass = case_when(r_squared >= 0.9 & outside_range <= 15 ~ 'Tier1',
                                      (r_squared > 0.75 & r_squared < 0.9) | (outside_range <= 30 & outside_range > 15) ~ 'Tier2',
                                      r_squared <= 0.75 | outside_range > 30 ~ 'Tier3'))

    all_sites_curve <- rbind(all_sites_curve, all_curves_fin)

    this_site <- filter(all_curves_fin, site == !!sites[i])

    all_years <- unique(this_site$year)

    # plotting data
    for(p in 1:length(all_years)) {
        
        start <- as.POSIXct(paste('10', '01', (as.numeric(all_years[p])-1), sep = '-'), format = '%m-%d-%Y')
        end <- as.POSIXct(paste('09', '30', all_years[p], sep = '-'), format = '%m-%d-%Y')
        
        
        year_info <- this_site %>%
            filter(year == !!all_years[p])

        curves <-  all_curves_fin %>%
            filter(site == !!site,
                   year == !!all_years[p]) %>%
            filter(curve_pass %in% c('Tier2', 'Tier3')) %>%
            pull(curveID) %>%
            unique()
        
        all_q_y <- all_q %>%
            filter(endDate >= !!start,
                   endDate <= !!end) %>%
            mutate(endDate = round_date(endDate, 'hour')) %>%
            group_by(endDate) %>%
            summarise(maxpostDischarge = mean(maxpostDischarge, na.rm = T),
                      dischargeFinalQF = max(dischargeFinalQF, na.rm = T)) %>%
            # mutate(SciRvw = as.character(dischargeFinalQFSciRvw)) %>%
            # filter(is.na(SciRvw) | SciRvw == '0') %>%
            ungroup() %>%
            mutate(dischargeFinalQF = ifelse(is.infinite(dischargeFinalQF) | is.na(dischargeFinalQF) | is.nan(dischargeFinalQF), 0, dischargeFinalQF)) %>%
            mutate(dischargeFinalQF = as.character(dischargeFinalQF)) %>%
            arrange(dischargeFinalQF)
        
        fin_plot <- all_q_y %>%
            ggplot() +
            ggthemes::theme_few() +
            scale_color_manual(values = c('black', 'red')) +
            # theme(legend.position = 'none') +
            labs(x = 'Date', y = 'Discharge (L/s)')

        # Highlight drift months 
        
        bad_months_site <- drift_months %>%
            filter(site == !!site) %>%
            filter(drift_status %in% c('potential_drift', 'not_assessed')) %>%
            mutate(water_year = ifelse(month %in% c(10,11,12), year+1, year)) %>%
            filter(water_year == !!all_years[p])
        
        if(! nrow(bad_months_site) == 0){
            for(k in 1:nrow(bad_months_site)){
                
                fill_col <- ifelse(pull(bad_months_site[k,8]) == 'potential_drift', 'blue', 'green')
                min_val <- ymd_hms(paste0(bad_months_site[k,1], '-', bad_months_site[k,2], '-', 1, ' 10:00:00'))
                
                if(bad_months_site[k,2]+1 == 13){
                    new_month <- 1
                    new_year <- bad_months_site[k,1] + 1
                } else{
                    new_month <- bad_months_site[k,2] + 1
                    new_year <- bad_months_site[k,1]
                }
                max_val <- ymd_hms(paste0(new_year, '-',new_month, '-', 1, ' 10:00:00'))
                
                fin_plot <- fin_plot +
                    annotate("rect", xmin=min_val, xmax=max_val,
                             ymin=0, ymax=Inf, alpha=0.2, fill=fill_col)
            }
        }
        
        
        if(! length(curves) == 0){
            for(s in 1:length(curves)) {
                
                cur_start <- year_info %>%
                    filter(curveID == curves[s]) %>%
                    pull(start_date) %>%
                    unique()
                
                if(cur_start < start) {
                    cur_start <- start
                }
                
                #cur_start <- as.character(cur_start)
                
                cur_end <- year_info %>%
                    filter(curveID == curves[s]) %>%
                    pull(end_date) %>%
                    unique()
                
                if(cur_end > end) {
                    cur_end <- end
                }
                
                
                fin_plot <- fin_plot +
                    annotate("rect", xmin=cur_start, xmax=cur_end,
                             ymin=0, ymax=Inf, alpha=0.2, fill='red') 
            }
        }
        
        
        fin_plot <- fin_plot +
            geom_point(aes(endDate, maxpostDischarge, col = dischargeFinalQF), size = 0.2) +
            scale_x_datetime(limits = c(start, end)) +
            ggtitle(paste0(sites[i], ' : ', all_years[p])) 
        
        un_sites <- paste0(site, '_', all_years[p])
        pdf(paste0('plots/', un_sites, '_', all_years[p], '.pdf'))
        print(fin_plot)
        dev.off()
        
    }
}

pdftools::pdf_combine(list.files('plots/', full.names = T), 'data/all_neon_q_eval.pdf')

# Final tables 
final_curves <- all_sites_curve %>%
    mutate(year = as.numeric(year)) %>%
    select(site, year, curveID, start_date, end_date, curve_pass, r_squared,
           above_range, under_range, outside_range, cont_n, gaugings_n, curve_year)

write_csv(final_curves, 'data/neon_rating_curve_qual.csv')



final_drift <- all_sites_drift %>%
    select(site, year, month, drift_status, n, mean_dif, outside_uncert,
           mean_uncertaintiy, prop_outside)

write_csv(final_drift, 'data/drift_detect.csv')

#### Regression check 

# get pressure files
neon_reg_fils <- list.files('data/data_in/neon/NEON_discharge-continuous/', 
                            recursive = TRUE, 
                            full.names = TRUE)

rel_file <- grep('gaugePressureRelationship', 
                 neon_reg_fils, value = TRUE)

all_reg <- map_dfr(rel_file, read.csv)

# initialize loop outputs and inputs
curve_ids <- unique(all_reg$regressionID)
out_reg <- tibble(site = NA, regressionID = curve_ids, startDate = ymd_hm("1999-09-09T09:09Z"), 
                  endDate = ymd_hm("1999-09-09T09:09Z"), n = NA,
                  rSquared = NA, resid_normal = NA, date_sig = NA, date_slope = NA,
                  error = NA)

# begin loop
#i = 1
for(i in 1:nrow(out_reg)){
    
    # filter data by regressionID
    df_reg <- all_reg %>%
        filter(regressionID == curve_ids[i]) %>%
        mutate(startDate = ymd_hms(startDate),
               endDate = ymd_hms(endDate))
    
    # assign curve info
    out_reg$site[i] <- df_reg$siteID[1]
    out_reg$regressionID[i] <- df_reg$regressionID[1]
    out_reg$startDate[i] <- min(df_reg$startDate)
    out_reg$endDate[i] <- max(df_reg$endDate)
    out_reg$n[i] <- nrow(df_reg)
    
    if(nrow(df_reg)<=2){

        out_reg$error[i] <- paste(df_reg$regressionID[1],'only has',nrow(df_reg),'observations.')

    } else{

        # test regression
        fit_reg <- summary(lm(calibratedPressMean~gaugeHeight, data = df_reg))

        if(fit_reg[['coefficients']][2,1] == 0){
            
            out_reg$error[i] <- paste('Regression has slope of 0.')
            
        } else{
            
            out_reg$rSquared[i] <- fit_reg$r.squared
            sh_test <- try(shapiro.test(residuals(fit_reg))$p.value)
            if(sh_test[1] == "Error in shapiro.test(residuals(fit_reg)) : all 'x' values are identical\n"){
                out_reg$resid_normal[i] <- 1
            } else{
                out_reg$resid_normal[i] <- sh_test
            }
            
            
            
            
            # test date effect
            fit_int <- summary(lm(calibratedPressMean~gaugeHeight:startDate+gaugeHeight, data=df_reg))
            # fit_int <- summary(lm(calculatedStage~calibratedPressMean+startDate, data=df_reg))
            out_reg$date_sig[i] <- fit_int[['coefficients']][3,4]
            out_reg$date_slope[i] <- fit_int[['coefficients']][3,1]
            
        }
    }
}

# Classify regressions 
presure_gauge_relation <- out_reg %>%
    select(site, regressionID, start_date = startDate, end_date=endDate, r_squared=rSquared) %>%
    mutate(regesstion_status = case_when(r_squared >= 0.9 ~ 'good',
                                         r_squared >= 0.75 & r_squared < 0.9 ~ 'fair',
                                         r_squared < 0.75 ~ 'poor'))

write_csv(presure_gauge_relation, 'data/regressions_eval.csv')

#### Convert files to monthly time step ####

# Regestions 
all_regestion_months <- tibble()
for(i in 1:nrow(presure_gauge_relation)){
    
    all_dates <- seq.Date(as.Date(presure_gauge_relation[[i,3]]), as.Date(presure_gauge_relation[[i,4]]), by = 'day')
    
    month_years <- tibble(date = all_dates) %>%
        mutate(month = month(date),
               year = year(date)) %>%
        group_by(year, month) %>%
        summarise(n = n()) %>%
        ungroup() %>%
        mutate(regressionID = presure_gauge_relation[[i,2]])
    
    all_regestion_months <- rbind(all_regestion_months, month_years)
}

regession_status <- presure_gauge_relation %>%
    select(regressionID, regesstion_status, site, r_squared)

reg_month_satus <- all_regestion_months %>%
    left_join(regession_status, by = 'regressionID') %>%
    select(site, year, month, regesstion_status, regression_r_squared=r_squared)


# prep rating curves

all_ratings_tibble <- tibble()
for(i in 1:nrow(final_curves)){
    
    all_dates <- seq.Date(as.Date(final_curves[[i,4]]), as.Date(final_curves[[i,5]]), by = 'day')
    
    month_years <- tibble(date = all_dates) %>%
        mutate(month = month(date),
               year = year(date)) %>%
        group_by(year, month) %>%
        summarise(n = n()) %>%
        mutate(curveID = final_curves[[i,3]])
    
    all_ratings_tibble <- rbind(all_ratings_tibble, month_years)
}

final_curves <- final_curves %>%
    select(-year)
rating_curve_final <- left_join(all_ratings_tibble, final_curves, by = 'curveID') %>%
    select(year, month, site, curveID, rating_curve_status=curve_pass,
           rating_curve_r_squared=r_squared, rating_curve_above_range=above_range, 
           rating_curve_under_range=under_range, rating_curve_outside_range=outside_range, 
           gaugings_n)

# Prep drift 
final_drift_prep <- final_drift %>%
    select(site, year, month, drift_status, gaugings_mean_dif = mean_dif,
           drift_outside_uncert = outside_uncert, 
           drift_mean_uncertaintiy = mean_uncertaintiy, 
           drift_prop_outside_uncert = prop_outside)

final_table <- full_join(rating_curve_final, reg_month_satus) %>%
    full_join(., final_drift_prep)

write_csv(final_table, 'data/neon_q_eval_final.csv')

