#############################################################################################
###                                  REPLICATION FILES FOR                                ###
###                                                                                       ###
###  A Costly Commitment: Populism, Economic Performance, and the Quality of Bureaucracy  ###
###                                                                                       ###
###                               SUPPLEMENTAL INFORMATION                                ###
###                                                                                       ###
###                       Luca Bellodi, Massimo Morelli, Matia Vannoni                    ###
###                                                                                       ###
###                     For questions: luca.bellodi@unibocconi.it                         ###
###                                                                                       ###
#############################################################################################


# Install Packages if not installed or if installed with different version

# Data.frame with packages and version
pk = data.frame(
  package = c("devtools", "tidyverse", "rdrobust", "rdpower", "kableExtra", 
              "scales", "rddensity", "plm", "rvest", "lubridate", "readxl"),
  version = c("2.4.3", "1.3.0", "2.0.2", "2.2", "1.3.4", 
              "1.2.0", "2.2", "2.6.1", "1.0.2", "1.8.0", "1.4.0")
)

# Function to install the appropriate version of package
# This is important for the RD packages, for we noticed that different versions produce minimally different results
pk_fun = function(package, version){
  if(!package %in% installed.packages() | packageVersion(package) != version){
    devtools::install_version(package, version = version)}
}

# Apply to each package
for(i in 1:nrow(pk)) pk_fun(package = pk$package[i], version = pk$version[i])

# Load packages
lapply(pk$package, library, character.only = TRUE)

# Load Dataset, rows are municipality-calendar year pairs
rdd_data = readRDS("dataset.Rds")

################################################################
## Table A.1: Confusion Matrix of Populist Candidates/Parties ##
################################################################

# Single elections
p <- rdd_data %>%
  group_by(ISTAT_CODICE_COMUNE, year_election) %>%
  filter(row_number()==1) %>%
  # matrix is 2x2 so keep only mayors supported by up to 2 populist parties
  filter(n_supporting_populist_parties_populist_candidate < 3)

FI <- with(p %>% filter(FI_candidate == 1), 
           data.frame(`Forza Italia` = sum(FI_candidate[n_supporting_populist_parties_populist_candidate == 1]),
                      `Popolo della Libertà` = sum(PDL_candidate),
                      `Lega Nord`= sum(LEGA_candidate),
                      `Fratelli d'Italia` = sum(FDI_candidate),
                      `Movimento 5 Stelle` = sum(M5S_candidate),
                      `Liga Veneta` = sum(LIGA_VENETA_candidate),
                      `Lega d'Azione Meridionale` = sum(LEGA_MERIDIONALE_candidate)))

PDL <- with(p %>% filter(PDL_candidate == 1), 
            data.frame(`Forza Italia` = sum(FI_candidate),
                       `Popolo della Libertà` = sum(PDL_candidate[n_supporting_populist_parties_populist_candidate == 1]),
                       `Lega Nord`= sum(LEGA_candidate),
                       `Fratelli d'Italia` = sum(FDI_candidate),
                       `Movimento 5 Stelle` = sum(M5S_candidate),
                       `Liga Veneta` = sum(LIGA_VENETA_candidate),
                       `Lega d'Azione Meridionale` = sum(LEGA_MERIDIONALE_candidate)))

LEGA <-  with(p %>% filter(LEGA_candidate == 1), 
              data.frame(`Forza Italia` = sum(FI_candidate),
                         `Popolo della Libertà` = sum(PDL_candidate),
                         `Lega Nord`= sum(LEGA_candidate[n_supporting_populist_parties_populist_candidate == 1]),
                         `Fratelli d'Italia` = sum(FDI_candidate),
                         `Movimento 5 Stelle` = sum(M5S_candidate),
                         `Liga Veneta` = sum(LIGA_VENETA_candidate),
                         `Lega d'Azione Meridionale` = sum(LEGA_MERIDIONALE_candidate)))

FDI <- with(p %>% filter(FDI_candidate == 1), 
            data.frame(`Forza Italia` = sum(FI_candidate),
                       `Popolo della Libertà` = sum(PDL_candidate),
                       `Lega Nord`= sum(LEGA_candidate),
                       `Fratelli d'Italia` = sum(FDI_candidate[n_supporting_populist_parties_populist_candidate == 1]),
                       `Movimento 5 Stelle` = sum(M5S_candidate),
                       `Liga Veneta` = sum(LIGA_VENETA_candidate),
                       `Lega d'Azione Meridionale` = sum(LEGA_MERIDIONALE_candidate)))

M5S <- with(p %>% filter(M5S_candidate == 1), 
            data.frame(`Forza Italia` = sum(FI_candidate),
                       `Popolo della Libertà` = sum(PDL_candidate),
                       `Lega Nord`= sum(LEGA_candidate),
                       `Fratelli d'Italia` = sum(FDI_candidate),
                       `Movimento 5 Stelle` = sum(M5S_candidate[n_supporting_populist_parties_populist_candidate == 1]),
                       `Liga Veneta` = sum(LIGA_VENETA_candidate),
                       `Lega d'Azione Meridionale` = sum(LEGA_MERIDIONALE_candidate)))

LIGA_VENETA <- with(p %>% filter(LIGA_VENETA_candidate == 1), 
                    data.frame(`Forza Italia` = sum(FI_candidate),
                               `Popolo della Libertà` = sum(PDL_candidate),
                               `Lega Nord`= sum(LEGA_candidate),
                               `Fratelli d'Italia` = sum(FDI_candidate),
                               `Movimento 5 Stelle` = sum(M5S_candidate),
                               `Liga Veneta` = sum(LIGA_VENETA_candidate[n_supporting_populist_parties_populist_candidate == 1]),
                               `Lega d'Azione Meridionale` = sum(LEGA_MERIDIONALE_candidate)))

LEGA_AZ_MER <- with(p %>% filter(LEGA_MERIDIONALE_candidate == 1), 
                    data.frame(`Forza Italia` = sum(FI_candidate),
                               `Popolo della Libertà` = sum(PDL_candidate),
                               `Lega Nord`= sum(LEGA_candidate),
                               `Fratelli d'Italia` = sum(FDI_candidate),
                               `Movimento 5 Stelle` = sum(M5S_candidate),
                               `Liga Veneta` = sum(LIGA_VENETA_candidate),
                               `Lega d'Azione Meridionale` = sum(LEGA_MERIDIONALE_candidate[n_supporting_populist_parties_populist_candidate == 1])))

populist_mayor <- bind_cols(data.frame(`Populist Party` = c("Forza Italia",
                                                            "Popolo della Libertà",
                                                            "Lega (Nord)",
                                                            "Fratelli d'Italia",
                                                            "Movimento 5 Stelle",
                                                            "Liga Veneta",
                                                            "Lega d'Azione Meridionale")),
                            bind_rows(FI, PDL, LEGA, FDI, M5S, LIGA_VENETA, LEGA_AZ_MER))


populist_mayor$Populist.Party <- populist_mayor$Populist.Party %>% str_replace("Libert.", "Libertà") 
populist_mayor[1:7, 2:8] <- populist_mayor[1:7, 2:8] %>% format(big.mark = ",")
diag <- populist_mayor[1:7,2:8]
diag[lower.tri(diag, diag = F)] <- " "
pop_table <- cbind(populist_mayor[1:7,1], diag)

colnames(pop_table) <- c(" ",
                 "Forza Italia",
                 "Pop. Libertà",
                 "Lega (Nord)",
                 "Fratelli\nd'Italia",
                 "Movimento\n5 Stelle",
                 "Liga Veneta",
                 "Lega Az.\nMeridionale")
                                      
kableExtra::kable(pop_table,
                  booktabs = T, 
                  format.args = list(big.mark = ","),
                  digits = 0,
                  linesep = "",
                  align = c("r", rep("c", 6)),
                  caption = "Table A.1: Distribution of populist candidates across supporting populist parties. Each entry in the matrix represent the number of candidates supported by the parties in the respective row and column. Diagonal of the matrix shows the number of candidates supported by one populist parties. The 169 instances where mayors are supported by the right-wing populist coalition (Forza Italia, Lega (Nord), and Fratelli d'Italia) have been omitted.", escape = F) %>%
  kableExtra::kable_styling(full_width = F, position = "center", latex_options = c("HOLD_position")) %>%
  kableExtra::kable_styling(font_size = 10) %>%
  kableExtra::save_kable(file = "SI_Table_A1.html", self_contained = F)

#################################
## Figure D.1: RD Density Test ##
#################################

# Test at municipality-election years level
dens_data <- rdd_data %>% 
  filter(!is.na(margin)) %>%
  group_by(ISTAT_CODICE_COMUNE, year_election) %>% 
  filter(row_number()==1)

pl <- rdplotdensity(rddensity(dens_data$margin),
                    dens_data$margin,
                    xlabel = "Margin of Victory",
                    ylabel = "Density")

ggsave("SI_Figure_D1.png", pl$Estplot, width = 4, height = 4)


#######################################
## Figure D.1: Balance on Covariates ##
#######################################

# For some variables that are at election-year level, use data at election year
data_ey <- rdd_data %>% group_by(ISTAT_CODICE_COMUNE, year_election) %>% filter(row_number()==1)

# Election Year Covariates

# List of DV
dv <- with(data_ey, list(councillors, degree_mayor, female_mayor, ISTAT_surface_sq_km,
                         idro_risk_surface, white_collar_dummy, mayor_secondary_education, share_f_council,
                         share_f_exec, parties_council, lag_margin, winning_candidate_was_incumbent, 
                         winning_party_was_incumbent_all_parties,
                         winning_party_was_incumbent_at_least_one, age_mayor))

# Same covariates as in main analysis
covs <- dv

dv_names <- c("N. Councillors", "Graduate Mayor", "Female Mayor","Surface (sq.km)",
              "Hydrogeological\nRisk (sq.km)", "White Collar Mayor",
              "Mayor Secondary\nEducation", "% Female\nCouncillors", "% Female\nExec. Members",
              "N. Parties\nin Loc. Council", "Margin of Victory\nPrevious Election", "Incumbent Mayor",
              "Mayor's supporting\nparties are incumbent (all)", "Mayor's supporting\nparties are incumbent (at least 1)",
              "Mayor's Age")

names(dv) <- dv_names
names(covs) <- dv_names

close_elections_balance_ey <- data.frame()

for(i in 1:length(dv)){
  
  cov <- covs[c(1:7)] # keep only covariates as in main specification
  
  cov[dv_names[i]] <- NULL
  cov <- do.call("cbind", cov) %>% cbind(., 
                                         factor(data_ey$population_istat),
                                         factor(data_ey$year_election),
                                         factor(data_ey$ISTAT_CODICE_COMUNE))
  
  rd <- rdrobust(y = dv[[i]],
                 x = data_ey$margin,
                 all = T,
                 covs = cov,
                 cluster = data_ey$ISTAT_CODICE_COMUNE,
                 bwselect = "cerrd")
  
  results <- data.frame(dv = dv_names[i],
                        estimate = rd$coef[1],
                        se = rd$se[3],
                        p.value = rd$pv[3],
                        h = rd$bws[1],
                        conf.low = rd$ci[3],
                        conf.high = rd$ci[6],
                        z = rd$z[3],
                        `Obs. used` = sum(rd$N_h))
  
  close_elections_balance_ey <- rbind(close_elections_balance_ey, results)
  
}


# Year-level covariates
dv <- with(rdd_data, list(log(population_istat), log(1+total_rank_file), log(1+total_managers)))
dv_names <- c("Log Population", "Log N. Rank-and-file\nEmployees", "Log N. Managers")
covs <- dv
names(dv) <- dv_names
names(covs) <- dv_names

close_elections_balance_y <- data.frame()

for(i in 1:length(dv)){
  
  cov <- covs
  
  cov[dv_names[i]] <- NULL
  cov <- do.call("cbind", cov) %>% cbind(., with(rdd_data, 
                                                 cbind(log(population_istat),
                                                       councillors,
                                                       degree_mayor,
                                                       female_mayor,
                                                       factor(year),
                                                       factor(ISTAT_CODICE_COMUNE),
                                                       factor(year_election),
                                                       ISTAT_surface_sq_km,
                                                       idro_risk_surface,
                                                       white_collar_dummy,
                                                       mayor_secondary_education)))
  
  
  rd <- rdrobust(y = dv[[i]],
                 x = rdd_data$margin,
                 all = T,
                 covs = cov,
                 cluster = rdd_data$ISTAT_CODICE_COMUNE,
                 bwselect = "cerrd")
  
  
  results <- data.frame(dv = dv_names[i],
                        estimate = rd$coef[1],
                        se = rd$se[3],
                        p.value = rd$pv[3],
                        h = rd$bws[1],
                        conf.low = rd$ci[3],
                        conf.high = rd$ci[6],
                        z = rd$z[3],
                        `Obs. used` = sum(rd$N_h))
  
  close_elections_balance_y <- rbind(close_elections_balance_y, results)
  
}


close_elections_balance <- bind_rows(
  close_elections_balance_ey,
  close_elections_balance_y) %>%
  mutate(sign = if_else(p.value < 0.05, 1, 0)) %>%
  mutate(estimate_char = estimate %>% as.character %>% str_extract("^.?.?.?.\\..."))

close_elections_balance <-  close_elections_balance %>%
  mutate(class = if_else(str_detect(dv, "Mayor|Margin"), "Mayor", "Municipality"),
         dv = dv %>% str_replace("\\n", " "))

figD2 = ggplot(close_elections_balance, aes(x = z, y = dv, color = factor(sign), shape = factor(sign))) +
  geom_point() +
  geom_vline(xintercept = c(-1.96, 1.96), linetype = 3, color = "red") +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 9)) +
  guides(color = "none") +
  scale_color_manual(values = c("black", "blue")) +
  labs(x = "Robust Z-Statistic", y = " ") +
  facet_grid(rows = vars(class), scales = "free", space = "free") +
  guides(color = "none", shape = "none")

ggsave("SI_Figure_D2.png", figD2, width = 6.2, height = 6)


##################################
## Figure D.3: Placebo Cutoffs ##
#################################

# name and type of dvs
dv_names <- c("Debt Accumulation", "Debt Repayment", "% Cost Overruns", "Turnover Managers", "% Postgraduate Managers")
type <- c(rep("Government\nPerformance", 3), rep("Quality of\nBureaucrats", 2))

# Placebo cutoffs
cut <-  c(-25:25)

data <- list(rdd_data %>% filter(margin < 0),
             rdd_data %>% filter(margin > 0),
             rdd_data)

close_elections_placebo_cutoffs <- data.frame()

for(j in cut){
  
  if(j < 0) rdd_data_cutoff <- data[[1]]
  if(j > 0) rdd_data_cutoff <- data[[2]]
  if(j == 0) rdd_data_cutoff <- data[[3]]
  
  dv <- with(rdd_data_cutoff, list(
    # Performance
    debt_accumulation,
    debt_repayment,
    cost_overrun_dummy,
    # Bureaucrats
    bur_turnover,
    bur_degree
  ))
  
  for(i in 1:length(dv)){
    
    rd_covs <- with(rdd_data_cutoff,  rdrobust(y = dv[[i]],
                                               x = margin,
                                               all = T,
                                               c = j,
                                               cluster = ISTAT_CODICE_COMUNE,
                                               covs = cbind(log(population_istat),
                                                            councillors,
                                                            degree_mayor,
                                                            female_mayor,
                                                            factor(year),
                                                            factor(ISTAT_CODICE_COMUNE),
                                                            factor(year_election),
                                                            ISTAT_surface_sq_km,
                                                            idro_risk_surface,
                                                            white_collar_dummy,
                                                            mayor_secondary_education)))
                                       
    results_cov <- data.frame(dv = dv_names[i],
                              cutoff = j,
                              estimate = rd_covs$coef[1],
                              se = rd_covs$se[3],
                              p.value = rd_covs$pv[3],
                              h = rd_covs$bws[1],
                              conf.low = rd_covs$ci[3],
                              conf.high = rd_covs$ci[6],
                              `Obs. used` = sum(rd_covs$N_h),
                              cov = "With Covariates",
                              type = type[i],
                              n_obs_tot = sum(rd_covs$N))
    
    close_elections_placebo_cutoffs <- bind_rows(close_elections_placebo_cutoffs, results_cov)
    
  }
}


cut = close_elections_placebo_cutoffs %>%
  filter(abs(cutoff) >= 3)

# Adjust multiple testing
dvs <- unique(cut$dv)
adjusted_p <- data.frame()
for(i in dvs) adjusted_p <- rbind(adjusted_p, 
                                  data.frame(dv = i,
                                             p.value = cut$p.value[cut$dv == i],
                                             adj.p = p.adjust(cut$p.value[cut$dv== i], "bonferroni")))

cut <- cut %>%
  left_join(., adjusted_p, by = c("dv", "p.value")) %>%
  mutate(sign = if_else(adj.p < 0.05, 1, 0))

figD3 = ggplot(data = cut, aes(x = cutoff, y = estimate, color = factor(sign))) +
  geom_point(size = 1.2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, size = 0.5) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme_bw() +
  scale_color_manual(values = c("black", "blue", "red")) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size = 8)) +
  facet_grid(rows = vars(fct_relevel(dv,"Debt Accumulation", "Debt Repayment", "% Cost Overruns", "Turnover Managers", "% Postgraduate Managers")), 
             scales = "free_y") +
  labs(x = "Placebo Cutoffs", y = "Estimate") +
  scale_x_continuous(breaks = seq(-25, 25, 5)) +
  scale_y_continuous(label = scales::number_format(accuracy = 0.01)) + 
  guides(color = "none") +
  annotate("rect", xmin = -2, xmax = 2, ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.2) +
  # Add main results
  geom_point(data = close_elections_placebo_cutoffs %>% filter(cutoff == 0), aes(x = 0, y = estimate), shape = 2, color = "red") +
  geom_errorbar(data = close_elections_placebo_cutoffs %>% filter(cutoff == 0), aes(ymin = conf.low, ymax = conf.high), width = 0, color = "red") 

ggsave("SI_figure_D3.png", figD3, width = 7, height = 7)


##################################################################
## Table D.3: % of non-significant estimates at placebo cutoffs ##
##################################################################

pdv = cut %>% 
  group_by(dv) %>%
  summarise(sig = ((1 - mean(sign))*100) %>% round(0))

pdv_tab = pdv[c(3,4,1,5,2),]
pdv_tab = rbind(pdv_tab, data.frame(dv = "Average", sig = mean(pdv$sig) %>% round(0)))
names(pdv_tab) = c("Outcome", "% of non-significant estimates")

kableExtra::kable(pdv_tab, 
                  booktabs = T,
                  format.args = list(big.mark = ","),
                  align = c("l", "r"),
                  linesep = "",
                  caption = "Table D.3: Share of non-significant estimates at placebo cutoffs ranging from -25% to +25% after multiple-testing adjustment separately for each outcome as displayed in Figure D3.", 
                  escape = F) %>%
  kableExtra::row_spec(c(0,6), bold = T) %>%
  kableExtra::save_kable(file = "SI_Table_D3.html", self_contained = F)

################################################
## Table D.4: RD Results with lagged Outcomes ##
################################################

# List of DV
dv <- with(rdd_data, list( 
  # Performance
  debt_accumulation,
  debt_repayment,
  cost_overrun_dummy,
  # Bureaucrats
  bur_turnover,
  bur_degree))

dv_names <- c("Debt\nAccumulation", "Debt\nRepayment", "% Cost Overruns", "Turnover Managers", "% Postgraduate Managers")
type <- c(rep("Government\nPerformance", 3), rep("Quality of\nBureaucrats", 2))

lagged_dv <- data.frame()

for(i in 1:length(dv)){
  
  rd <- with(rdd_data, rdrobust(y = dv[[i]],
                                x = future_margin,
                                all = T,
                                cluster = ISTAT_CODICE_COMUNE))
                             
  
  rd_covs <- with(rdd_data, rdrobust(y = dv[[i]],
                                     x = future_margin,
                                     covs = cbind(log(population_istat),
                                                  councillors,
                                                  degree_mayor,
                                                  female_mayor,
                                                  factor(year),
                                                  factor(ISTAT_CODICE_COMUNE),
                                                  factor(year_election),
                                                  ISTAT_surface_sq_km,
                                                  idro_risk_surface,
                                                  white_collar_dummy,
                                                  mayor_secondary_education),
                                     all = T,
                                     cluster = ISTAT_CODICE_COMUNE))
                                  
  
  results_no_cov <- data.frame(dv = dv_names[i],
                               estimate = rd$coef[1],
                               se = rd$se[3],
                               p.value = rd$pv[3],
                               h = rd$bws[1],
                               conf.low = rd$ci[3],
                               conf.high = rd$ci[6],
                               `Obs. used` = sum(rd$N_h),
                               cov = "No Covariates",
                               type = type[i])
  
  results_cov <- data.frame(dv = dv_names[i],
                            estimate = rd_covs$coef[1],
                            se = rd_covs$se[3],
                            p.value = rd_covs$pv[3],
                            h = rd_covs$bws[1],
                            conf.low = rd_covs$ci[3],
                            conf.high = rd_covs$ci[6],
                            `Obs. used` = sum(rd_covs$N_h),
                            cov = "With Covariates",
                            type = type[i])
  
  lagged_dv <- rbind(lagged_dv,
                     results_no_cov,
                     results_cov)

  
}

table_lag <- turn_reg_table(results = lagged_dv, covariates = T) 

kableExtra::kable(table_lag, booktabs = T,
                  format.args = list(big.mark = ","),
                  align = "c",
                  linesep = "",
                  caption = "Table D.4: RD estimates of the effects of electing a populist mayor in year T on outcomes observed between election T and T-1. Estimates constructed using local polynomial estimators with triangular kernel. Robust 95% confidence interval constructed using bias-correction with cluster robust standard errors at municipality level, h is the MSE-optimal bandwidth. Covariates include: population (log), surface (sq.km), surface at hydro-geological risk (sq.km), number of local councillors, gender, secondary education,  degree, and white-collar job of mayor (all dichotomous), year, municipality, and year-election dummies. Period of analysis: cost overruns 2012-2020, debt accumulation and repayment 2008-2019, turnover and education of bureaucrats 2001-2019.", escape = F) %>%
  kableExtra::add_header_above(c(" " = 1, "Lagged Economic Performance" = 3, "Lagged Quality of Bureaucrats" = 2), bold = T) %>%
  # This extra line does not work in html format
  #kableExtra::add_header_above(c(" " = 1, "Lagged DV" = 5), bold = T) %>%
  kableExtra::kable_paper(full_width = F) %>%
  kableExtra::column_spec(1, italic = T) %>%
  kableExtra::kable_classic() %>%
  kableExtra::kable_styling(font_size = 11) %>%
  kableExtra::save_kable(file = "SI_Table_D4.html", self_contained = T)


########################################
## Figure E.4: Alternative Bandwidths ##
########################################

# List of DV
dv <- with(rdd_data, list( 
  # Performance
  debt_accumulation,
  debt_repayment,
  cost_overrun_dummy,
  # Bureaucrats
  bur_turnover,
  bur_degree))

# DV names without \\ to escape latex characters
dv_names <- c("Debt\nAccumulation", "Debt\nRepayment", "% Cost Overruns", "Turnover Managers", "% Postgraduate Managers")

alternative_bandwidths <- data.frame()

for(i in 1:length(dv_names)){
  
  # Get optimal bandwidths
  bands <- with(rdd_data, rdbwselect(y = dv[[i]],
                                     x = margin,
                                     covs = cbind(log(population_istat),
                                                  councillors,
                                                  degree_mayor,
                                                  female_mayor,
                                                  factor(year),
                                                  factor(ISTAT_CODICE_COMUNE),
                                                  factor(year_election),
                                                  ISTAT_surface_sq_km,
                                                  idro_risk_surface,
                                                  white_collar_dummy,
                                                  mayor_secondary_education),
                                     cluster = ISTAT_CODICE_COMUNE))
  # MSE-minimizing bandwidth
  optimal <- bands$bws[1]
  
  # Alternative main and bias bandwidths
  h_h <- bands$bws[1] * seq(0.5, 2, 0.1)
  h_b <- bands$bws[3] * seq(0.5, 2, 0.1)
  
  # Loop over each bandwidth
  for(j in 1:length(h_h)){
    
    rd_covs <- with(rdd_data, 
                    rdrobust(y = dv[[i]],
                             x = margin,
                             covs = cbind(log(population_istat),
                                          councillors,
                                          degree_mayor,
                                          female_mayor,
                                          factor(year),
                                          factor(ISTAT_CODICE_COMUNE),
                                          factor(year_election),
                                          ISTAT_surface_sq_km,
                                          idro_risk_surface,
                                          white_collar_dummy,
                                          mayor_secondary_education),
                             all = T,
                             h = h_h[j],
                             b = h_b[j],
                             cluster = ISTAT_CODICE_COMUNE))
    
    results_cov <- data.frame(dv = dv_names[i],
                              h_h = h_h[j],
                              h_b = h_b[j],
                              estimate = rd_covs$coef[1],
                              se = rd_covs$se[3],
                              p.value = rd_covs$pv[3],
                              h = rd_covs$bws[1],
                              conf.low = rd_covs$ci[3],
                              conf.high = rd_covs$ci[6],
                              `Obs. used` = sum(rd_covs$Nh),
                              cov = "With Covariates",
                              optimal = h_h[j] == optimal)
    
    alternative_bandwidths <- bind_rows(alternative_bandwidths, results_cov)
    
  }
}

# Create a variable for significance of estimate with alternative bandwidth
alternative_bandwidths$sign <- if_else(alternative_bandwidths$p.value < 0.05, 1, 0)

# Re-level dv_names for plot
alternative_bandwidths$dv <- factor(alternative_bandwidths$dv, 
                                    levels = unique(alternative_bandwidths$dv))

# Results with optimal bandwidth
optimal <- alternative_bandwidths %>% filter(optimal == T)

figE4 = ggplot(alternative_bandwidths, aes(x = h, y = estimate, color = factor(sign))) +
  geom_point(size = 0.9) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high, color = factor(sign)), size = 0.5) +
  geom_hline(yintercept = 0, linetype = 2, size = 0.4) +
  theme_bw() +
  labs(y = "Estimate", x = "Bandwidth") +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text = element_text(size = 8),
        strip.text = element_text(size = 10)) +
  geom_point(data = optimal, aes(x = h, y = estimate), color = "red", size = 0.9) +
  geom_linerange(data = optimal, aes(ymin = conf.low, ymax = conf.high), size = 0.5, color = "red") +
  facet_wrap(~dv, scales = "free") +
  scale_color_manual(values = c("grey20", "grey70")) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))

ggsave("SI_Figure_E4.png", figE4, width = 6.5, height = 4)


################################################
## Figure E.5: Alternative Bandwidth Selector ##
################################################

# Names of selectors
selector <- c("mserd", "msetwo", "cerrd", "certwo")
selector_name <- c("MSE", "MSE Asym.", "CER", "CER Asym.")

alternative_band_selector <- data.frame()

for(i in 1:length(dv)){
  
  for(k in 1:length(selector)){
    
    rd_covs <- with(rdd_data, rdrobust(y = dv[[i]],
                                       x = margin,
                                       # Specify selector
                                       bwselect = selector[k],
                                       covs = cbind(log(population_istat),
                                                    councillors,
                                                    degree_mayor,
                                                    female_mayor,
                                                    factor(year),
                                                    factor(ISTAT_CODICE_COMUNE),
                                                    factor(year_election),
                                                    ISTAT_surface_sq_km,
                                                    idro_risk_surface,
                                                    white_collar_dummy,
                                                    mayor_secondary_education),
                                       all = T,
                                       cluster = ISTAT_CODICE_COMUNE))
    
    
    results_cov <- data.frame(dv = dv_names[i],
                              estimate = rd_covs$coef[1],
                              se = rd_covs$se[3],
                              p.value = rd_covs$pv[3],
                              h = rd_covs$bws[1],
                              conf.low = rd_covs$ci[3],
                              conf.high = rd_covs$ci[6],
                              `Obs. used` = sum(rd_covs$N_h),
                              cov = "With Covariates",
                              bwselect = selector_name[k],
                              type = type[i])
    
    alternative_band_selector <- bind_rows(alternative_band_selector,
                                           results_cov)
    
  }
}

# Re-level outcomes for plot
alternative_band_selector$dv <- factor(alternative_band_selector$dv,
                                       levels = rev(unique(alternative_band_selector$dv)))

# Identify MSE-minimising (used in main analysis)
alternative_band_selector$col = if_else(alternative_band_selector$bwselect == "MSE", 1, 0)

figE5 = ggplot(alternative_band_selector, aes(x = estimate, y = dv, color = factor(col))) +
  geom_point(size = 1.2) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, size = 0.4) +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_bw() +
  scale_color_manual(values = c("black", "blue")) +
  theme(panel.grid = element_blank()) +
  facet_grid(cols = vars(bwselect)) +
  labs(x = NULL, y = "Estimate") +
  guides(color = "none")

ggsave("SI_Figure_E5.png", figE5, width = 6.5, height = 2.4)


###############################
## Table E.2: Power Analysis ##
###############################

# List of DV
dv <- with(rdd_data, list( 
  # Performance
  debt_accumulation,
  debt_repayment,
  cost_overrun_dummy,
  # Bureaucrats
  bur_turnover,
  bur_degree))

dv_names <- c("Debt Accumulation", "Debt Repayment", "% Cost Overruns", "Turnover Managers", "% Postgraduate Managers")

power <- data.frame()

for(i in 1:length(dv)){
  
  # Retrieve main estimate
  rd_covs <- with(rdd_data, rdrobust(y = dv[[i]],
                                     x = margin,
                                     covs = cbind(log(population_istat),
                                                  councillors,
                                                  degree_mayor,
                                                  female_mayor,
                                                  factor(year),
                                                  factor(ISTAT_CODICE_COMUNE),
                                                  factor(year_election),
                                                  ISTAT_surface_sq_km,
                                                  idro_risk_surface,
                                                  white_collar_dummy,
                                                  mayor_secondary_education),
                                     all = T,
                                     cluster = ISTAT_CODICE_COMUNE))
  
  p <- with(rdd_data, rdpower(data = cbind(dv[[i]], margin),
                              cutoff = 0,
                              tau = rd_covs$coef[1],
                              cluster = ISTAT_CODICE_COMUNE))
  
  power <- bind_rows(power,
                     data.frame(dv = dv_names[i],
                                tau = p$tau,
                                power = p$power.rbc))
  
}

# Add true effects
names(power) <- c("Outcome", "Target Effect Size (Estimated Effect Size)", "Power")

power %>%
  kableExtra::kable(booktabs = T,   digits = 3,
                    align = c("l", "c", "c"), linesep = "",
                    caption = "Table E.2: Statistical power achieved by an effect size equal to effects reported in Table 2. Analysis implemented with rdpower package in R. Calculation performed with same estimation and no covariates.", escape = F) %>%
  kableExtra::kable_paper(full_width = F) %>%
  kableExtra::kable_classic() %>%
  kableExtra::kable_styling(font_size = 11) %>%
  kableExtra::save_kable(file = "SI_Table_E5.html", self_contained = F)

############################################
## Table E.3: Main Results, No Covariates ##
############################################

# Specify outcome variables as list
dv <- with(rdd_data, list(
  # Performance
  debt_accumulation,
  debt_repayment,
  cost_overrun_dummy,
  # Bureaucrats
  bur_turnover,
  bur_degree
))

# Names of outcome variables
dv_names <- c("Debt Accumulation", "Debt Repayment", "% Cost Overruns", "Turnover Managers", "% Postgraduate Managers")

# Type of outcomes
type <- c(rep("Government\nPerformance", 3), rep("Quality of\nBureaucrats", 2))

# Data.frame with results
main_results_no_covs <- data.frame()

for(i in 1:length(dv)){
  
  # RDD without covariates
  rd <- with(rdd_data, rdrobust(y = dv[[i]], x = margin, all = T, cluster = ISTAT_CODICE_COMUNE))

  # Results without covariates
  results_no_cov <- data.frame(dv = dv_names_main[i],
                               estimate = rd$coef[1],
                               se = rd$se[3],
                               p.value = rd$pv[3],
                               h = rd$bws[1],
                               conf.low = rd$ci[3],
                               conf.high = rd$ci[6],
                               `Obs. used` = sum(rd$N_h),
                               cov = "No Covariates",
                               type = type[i])
  
  # merge
  main_results_no_covs <- rbind(main_results_no_covs, results_no_cov)
  
}


# Function to convert data.frame to regression table
turn_reg_table <- function(results, covariates){
  
  cova = if_else(covariates == T, "With Covariates", "No Covariates")
  
  results %>% filter(cov == cova) %>%
    select(-p.value) %>%
    mutate_at(c("estimate", "conf.high", "conf.low"), 
              function(x) x %>% round(3) %>% format(nsmall = 3)) %>%
    mutate(h = format(round(h, 3), nsmall = 3),
           Obs..used = format(round(Obs..used, 0), big.mark = ",")) %>%
    mutate_at(c("estimate",  "conf.high", "conf.low", "h"),
              function(x) x %>% as.character %>% str_squish %>% str_trim) %>%
    select(-type) %>%
    mutate(dv = dv %>% str_replace(" (?=(Man)|(Acc)|(Rep)|(Over))", "\n"),
           robust_ci = paste0("[", conf.low, ", ", conf.high, "]")) %>%
    select(dv, estimate, robust_ci, #p.value, 
           h, Obs..used) %>%
    rename("Outcomes" = dv,
           "Estimate" = estimate,
           "95% CI" = robust_ci,
           "Obs. Used" = Obs..used) %>%
    t()
}

# create input for regression table
table_E3 <- turn_reg_table(results = main_results_no_covs, covariates = F) 

kableExtra::kable(table_E3,
                  format.args = list(big.mark = ","), 
                  digits = 3,
                  align = c("c", "c", "c", "c", "c"),
                  linesep = "") %>%
  kableExtra::add_header_above(c(" " = 1, "Economic Performance" = 3, "Quality of Bureaucrats" = 2), bold = T) %>%
  kableExtra::kable_styling(full_width = F, position = "center",
                            bootstrap_options = "condensed") %>%
  kableExtra::kable_paper(full_width = F) %>%
  kableExtra::column_spec(1, italic = T) %>%
  kableExtra::kable_classic() %>%
  kableExtra::kable_styling(font_size = 11) %>%
  kableExtra::save_kable(file = "SI_Table_E3.html", self_contained = T)


######################################################
## Table E.7: Alternative Measures of Cost Overruns ##
######################################################

# Alternative measures, equal to 1 if final costs of procurement contract greater than 5% and 10% of initial adjudicated costs

dv <- with(rdd_data, list(cost_overrun_dummy,
                          cost_overrun_dummy_5pp,
                          cost_overrun_dummy_10pp))

dv_names <- c("% Cost Overruns", "% Cost Overruns (> 5%)", "% Cost Overruns  (> 10%)")

alternative_cost_overruns <- data.frame()

for(i in 1:length(dv)){
  
  rd <- with(rdd_data, rdrobust(y = dv[[i]],
                                x = margin,
                                all = T,
                                cluster = ISTAT_CODICE_COMUNE))
  
  rd_covs <- with(rdd_data, rdrobust(y = dv[[i]],
                                     x = margin,
                                     covs = cbind(log(population_istat),
                                                  councillors,
                                                  degree_mayor,
                                                  female_mayor,
                                                  factor(year),
                                                  factor(ISTAT_CODICE_COMUNE),
                                                  factor(year_election),
                                                  ISTAT_surface_sq_km,
                                                  idro_risk_surface,
                                                  white_collar_dummy,
                                                  mayor_secondary_education),
                                     all = T,
                                     cluster = ISTAT_CODICE_COMUNE))
  
  
  results_cov <- data.frame(dv = dv_names[i],
                            estimate = rd_covs$coef[1],
                            se = rd_covs$se[3],
                            p.value = rd_covs$pv[3],
                            h = rd_covs$bws[1],
                            conf.low = rd_covs$ci[3],
                            conf.high = rd_covs$ci[6],
                            `Obs. used` = sum(rd_covs$N_h),
                            cov = "With Covariates")
  
  alternative_cost_overruns <- rbind(alternative_cost_overruns, results_cov)
  
  
}

# create regression table
alternative_cost_overruns_table <- alternative_cost_overruns %>%
  mutate(dv = kableExtra::linebreak(dv, align = "c"),
         estimate = round(estimate, 3),
         robust_ci = paste0("[", format(round(conf.low, 3), nsmall = 3), ", ", format(round(conf.high, 3), nsmall = 3), "]"),
         Obs..used = format(round(Obs..used, 0), big.mark = ","),
         h = round(h, 2)) %>%
  select(dv, estimate, robust_ci, p.value, h, Obs..used) %>%
  rename("Outcomes" = dv,
         "Estimate" = estimate,
         "Robust 95\\% CI" = robust_ci,
         "Obs. Used" = Obs..used) %>%
  t()

# save regression table
kableExtra::kable(alternative_cost_overruns_table, booktabs = T,
                  format.args = list(big.mark = ","),
                  digits = 3,
                  align = c("c", "c", "c"),
                  linesep = "",
                  caption = "Table E.7: RD estimates of the effect of electing a populist mayor on three alternative measures of cost overruns: percentage of procurement contracts with payments greater than awarded costs (baseline measure used in results in Table 2, and percentage of contracts with payments exceeding costs by at least 5% and 10%. Estimates constructed using local polynomial estimators with triangular kernel. Robust 95% confidence interval constructed using bias-correction with cluster robust standard errors at municipality level, h is the MSE-optimal bandwidth. Covariates include: population (log), surface (sq.km), surface at hydro-geological risk (sq.km), number of local councillors, gender, secondary education,  degree, and white-collar job of mayor (all dichotomous), year, municipality, and year-election dummies. Period of analysis 2012-2020.", escape = F) %>%
  kableExtra::add_header_above(c(" " = 1, "Alternative Measures of Cost Overruns" = 3), bold = T) %>%
  kableExtra::kable_paper(full_width = F) %>%
  kableExtra::column_spec(1, italic = T) %>%
  kableExtra::kable_classic() %>%
  kableExtra::row_spec(1, extra_latex_after = "\\midrule") %>%
  kableExtra::kable_styling(font_size = 11) %>%
  kableExtra::save_kable(file = "SI_Table_E7.html", self_contained = T)


#############################################
## Table F.8: Unpacking Effect on Turnover ##
#############################################

dv <- with(rdd_data, list(turn_hired, turn_fired, bur_turnover))

dv_names <- c("Hirings/Total", "Departures/Total", "Hirings + Departures/Total")

components_of_turnover <- data.frame()

for(i in 1:length(dv)){
  
  rd_covs <- with(rdd_data, rdrobust(y = dv[[i]],
                                     x = margin,
                                     covs = cbind(log(population_istat),
                                                  councillors,
                                                  degree_mayor,
                                                  female_mayor,
                                                  factor(year),
                                                  factor(ISTAT_CODICE_COMUNE),
                                                  factor(year_election),
                                                  ISTAT_surface_sq_km,
                                                  idro_risk_surface,
                                                  white_collar_dummy,
                                                  mayor_secondary_education),
                                     all = T,
                                     cluster = ISTAT_CODICE_COMUNE))
  
  results_cov <- data.frame(dv = dv_names[i],
                            estimate = rd_covs$coef[1],
                            se = rd_covs$se[3],
                            p.value = rd_covs$pv[3],
                            h = rd_covs$bws[1],
                            conf.low = rd_covs$ci[3],
                            conf.high = rd_covs$ci[6],
                            `Obs. used` = sum(rd_covs$N_h),
                            cov = "With Covariates")
  
  components_of_turnover <- rbind(components_of_turnover, results_cov)

}

# create regression table
components_of_turnover_table <- components_of_turnover %>%
  mutate(dv = kableExtra::linebreak(dv, align = "c"),
         estimate = round(estimate, 3),
         robust_ci = paste0("[", format(round(conf.low, 3), nsmall = 3), ", ", format(round(conf.high, 3), nsmall = 3), "]"),
         Obs..used = format(round(Obs..used, 0), big.mark = ","),
         h = round(h, 2)) %>%
  select(dv, estimate, robust_ci, h, Obs..used) %>%
  rename("Outcomes" = dv,
         "Estimate" = estimate,
         "Robust 95\\% CI" = robust_ci,
         "Obs. Used" = Obs..used) %>%
  t()

# save regression table
kableExtra::kable(components_of_turnover_table, booktabs = T,
                  format.args = list(big.mark = ","),
                  digits = 3,
                  align = c("c", "c", "c"),
                  linesep = "",
                  caption = "Table F.8: RD estimates of the effect of electing a populist mayor on managers' turnover and the two hirings and departures components thereof. Estimates constructed using local polynomial estimators with triangular kernel. Robust p-values and confidence interval constructed using bias-correction with cluster robust standard errors at municipality level, h is the MSE-optimal bandwidth. Covariates include: population (log), surface (sq.km), surface at hydro-geological risk (sq.km), number of local councillors, gender, secondary education,  degree, and white-collar job of mayor (all dichotomous), year, municipality, and year-election dummies. Period of analysis: 2001-2019.", escape = F) %>%
  kableExtra::add_header_above(c(" " = 1, "Components of Turnover" = 3), bold = T) %>%
  kableExtra::kable_paper(full_width = F) %>%
  kableExtra::column_spec(1, italic = T) %>%
  kableExtra::kable_classic() %>%
  kableExtra::row_spec(1, extra_latex_after = "\\midrule") %>%
  kableExtra::kable_styling(font_size = 11) %>%
  kableExtra::save_kable(file = "SI_Table_F8.html", self_contained = T)


########################################
## Table F.9: Rank-and-File Employees ##
########################################

dv <- with(rdd_data, list(turnover_rank_file, rank_file_degree))
dv_names <- c("Turnover", "\\% Graduate")
type <- rep("Quality of\nBureaucrats", 2)
rank_file_employees <- data.frame()

for(i in 1:length(dv)){
  
  rd_covs <- with(rdd_data, rdrobust(y = dv[[i]],
                                     x = margin,
                                     covs = cbind(log(population_istat),
                                                  councillors,
                                                  degree_mayor,
                                                  female_mayor,
                                                  factor(year),
                                                  factor(ISTAT_CODICE_COMUNE),
                                                  factor(year_election),
                                                  ISTAT_surface_sq_km,
                                                  idro_risk_surface,
                                                  white_collar_dummy,
                                                  mayor_secondary_education),
                                     all = T,
                                     cluster = ISTAT_CODICE_COMUNE
  ))
  
  results_cov <- data.frame(dv = dv_names[i],
                            estimate = rd_covs$coef[1],
                            se = rd_covs$se[3],
                            p.value = rd_covs$pv[3],
                            h = rd_covs$bws[1],
                            conf.low = rd_covs$ci[3],
                            conf.high = rd_covs$ci[6],
                            `Obs. used` = sum(rd_covs$N_h),
                            cov = "With Covariates",
                            type = type[i])
  
  rank_file_employees <- rbind(rank_file_employees, results_cov)
  
}

# create regression table
rank_file_employees_table <- rank_file_employees %>%
  mutate(dv = dv %>% str_replace(" Managers", "\nManagers") %>%
           str_replace(" (?=(Accumulation)|(Repayment)|(Overruns))", "\n"),
         dv = kableExtra::linebreak(dv, align = "c"),
         estimate = round(estimate, 3),
         robust_ci = paste0("[", round(conf.low, 3), ", ", round(conf.high, 3), "]"),
         Obs..used = format(round(Obs..used, 0), big.mark = ","),
         h = round(h, 2)) %>%
  select(dv, estimate, robust_ci, h, Obs..used) %>%
  rename("Outcomes" = dv,
         "Estimate" = estimate,
         "Robust 95\\% CI" = robust_ci,
         "Obs. Used" = Obs..used) %>%
  t()

# save regression table
kableExtra::kable(rank_file_employees_table, booktabs = T,
                  format.args = list(big.mark = ","),
                  align = "c",
                  linesep = "",
                  caption = "Table F.9: RD estimates of the effect of electing a populist mayor on turnover and education of rank-and-file employees. Estimates constructed using local polynomial estimators with triangular kernel. Robust p-values and confidence interval constructed using bias-correction with cluster robust standard errors at municipality level, h is the MSE-optimal bandwidth. Covariates include: population (log), surface (sq.km), surface at hydro-geological risk (sq.km), number of local councillors, gender, secondary education,  degree, and white-collar job of mayor (all dichotomous), year, year-election. Period of analysis: 2001-2019.", escape = F) %>%
  kableExtra::add_header_above(c(" " = 1, "Rank-and-File Employees" = 2), bold = T) %>%
  kableExtra::kable_styling(full_width = F, position = "center",
                            latex_options = "HOLD_position",
                            bootstrap_options = "condensed") %>%
  kableExtra::kable_paper(full_width = F) %>%
  kableExtra::column_spec(1, italic = T) %>%
  kableExtra::kable_classic() %>%
  kableExtra::row_spec(1, extra_latex_after = "\\midrule") %>%
  kableExtra::kable_styling(font_size = 11) %>%
  kableExtra::save_kable(file = "SI_Table_F9.html", self_contained = T)


#################################################
## Table F.10,11,12,13: Heterogeneity Analysis ##
#################################################

# Table F.10, Difference in estimates in first and second half of government term

# Datasets with obs from first two and last three years of government terms
datasets <- list(rdd_data %>% filter(year_term %in% c(1, 2)),
                 rdd_data %>% filter(year_term >= 3))

# names of term
term <- c("First Half", "Second Half")

# name and type of dvs
dv_names <- c("Debt Accumulation", "Debt Repayment", "% Cost Overruns", "Turnover Managers", "% Postgraduate Managers")
type <- c(rep("Government\nPerformance", 3), rep("Quality of\nBureaucrats", 2))

year_term <- data.frame()

for(j in 1:length(term)){
  
  rd <- datasets[[j]]
  
  # List of DV
  dv <- with(rd, list(
    # Performance
    debt_accumulation,
    debt_repayment,
    cost_overrun_dummy,
    # Bureaucrats
    bur_turnover,
    bur_degree
  ))
  
  
  for(i in 1:length(dv)){
    
    rd_no_cov <- with(rd, rdrobust(y = dv[[i]],
                                 x = margin,
                                 all = T,
                                 cluster = ISTAT_CODICE_COMUNE))
    
    results_cov <- data.frame(dv = dv_names[i],
                              estimate = rd_no_cov$coef[1],
                              se = rd_no_cov$se[3],
                              p.value = rd_no_cov$pv[3],
                              h = rd_no_cov$bws[1],
                              conf.low = rd_no_cov$ci[3],
                              conf.high = rd_no_cov$ci[6],
                              `Obs. used` = sum(rd_no_cov$N_h),
                              cov = "No Covariates",
                              type = type[i],
                              term = term[j])
    
    year_term <- rbind(year_term, results_cov)
  
  }
}

# create table
f_half <- year_term %>% filter(term == "First Half")
s_half <- year_term %>% filter(term == "Second Half")

diff_first_second <- bind_cols(f_half %>% select(dv, estimate, se),
                               s_half %>% select(estimate, se),
                               data.frame(diff = s_half$estimate - f_half$estimate,
                                          se = sqrt(s_half$se^2 + f_half$se^2)) %>%
                                 mutate(p.value = 2*(1-pnorm(abs(diff / se)))))

diff_first_second = diff_first_second %>%
  mutate_all(function(x){
    if(is.numeric(x)){ x %>% round(3)
    }else{x}})

names(diff_first_second)  <- c("Outcome", rep(c("Estimate", "SE"), 2), "Difference", "SE", "p.value")

# save table
kableExtra::kable(diff_first_second, booktabs = T,
                  format.args = list(big.mark = ","),
                  digits = 3,
                  align = c("r", rep("c", 7)),
                  linesep = "",
                  caption = "Table F.10: RD estimates of the effect of electing a populist mayor estimated on two sub-samples of observations in first (first three years) and second half of the government mandate (remaining years), as well as the difference in RD estimates. Same estimation of baseline analysis. To maximize sample size, no covariates were included. Period of analysis: cost overruns 2012-2020, debt accumulation and repayment 2008-2019, turnover and education of bureaucrats 2001-2019.", escape = F) %>%
  kableExtra::add_header_above(c(" " = 1, "First Half" = 2, "Second Half" = 2, "Difference\n(Second-First)" = 3), bold = T) %>%
  kableExtra::kable_paper(full_width = F) %>%
  kableExtra::kable_classic() %>%
  kableExtra::kable_styling(font_size = 11) %>% 
  kableExtra::save_kable(file = "SI_Table_F10.html", self_contained = T)

# Table F.11, Turnover over the government term
rdd_data %>%
  # turnover exists for municipalities with one managers
  filter(total_managers > 0,
         !is.na(year_term)) %>%
  group_by(year_term) %>%
  summarise(m_t = mean(bur_turnover, na.rm = T),
            sd_t = sd(bur_turnover, na.rm =T),
            m_l = mean(total_departures, na.rm = T),
            sd_l = sd(total_departures, na.rm = T),
            m_h = mean(total_hirings, na.rm = T),
            sd_h = sd(total_hirings, na.rm = T)) %>%
  rename("Year of Government Term" = year_term,
         "Mean" = m_l, "SD" = sd_l,
         "Mean " =m_h, "SD " = sd_h,
         " Mean" = m_t,  " SD" = sd_t) %>%
  kableExtra::kable(booktabs = T,   digits = 2, align = "c", linesep = "",
                    caption = "Descriptive statistics of turnover, hired and fired managers over the government term. Departures and Hirings are averages of the number of managers that leave and join the municipality. Turnover is measured as the sum of managers who join and leave divided by the total number of managers in any given year.", escape = F) %>%
  kableExtra::add_header_above(c(" " = 1, "Turnover" = 2, "Departures" = 2, "Hirings" = 2), bold = T) %>%
  kableExtra::kable_paper(full_width = F) %>%
  kableExtra::kable_classic() %>%
  kableExtra::kable_styling(font_size = 11) %>%
  kableExtra::save_kable(file = "SI_Table_F11.html", self_contained = T)


# Table F.12 and F.13
 # - F.12: Difference in estimates in municipalities in North or Centre-South of Country
 # - F.13: Difference in estimates in municipalities with population above-below median

# Median population
med_pop = median(rdd_data$population_istat[!is.na(rdd_data$margin)], na.rm = T)

datasets <- list(rdd_data %>% filter(north_region == 1),
                 rdd_data %>% filter(north_region == 0),
                 rdd_data %>% filter(population_istat > med_pop),
                 rdd_data %>% filter(population_istat <= med_pop))

# name of subsample
subsample <- c("North", "Centre-South","Large M.", "Small M.")

# name and type of dvs
dv_names <- c("Debt Accumulation", "Debt Repayment", "% Cost Overruns", "Turnover Managers", "% Postgraduate Managers")
type <- c(rep("Government\nPerformance", 3), rep("Quality of\nBureaucrats", 2))

north_south_median_population <- data.frame()

for(j in 1:length(datasets)){
  
  rd <- datasets[[j]]
  
  # List of DV
  dv <- with(rd, list(
    # Performance
    debt_accumulation,
    debt_repayment,
    cost_overrun_dummy,
    # Bureaucrats
    bur_turnover,
    bur_degree
  ))
  
  
  for(i in 1:length(dv)){
    
    rd_no_cov <- with(rd, rdrobust(y = dv[[i]],
                                   x = margin,
                                   all = T,
                                   cluster = ISTAT_CODICE_COMUNE))
                                
    results_cov <- data.frame(dv = dv_names[i],
                              estimate = rd_no_cov$coef[1],
                              se = rd_no_cov$se[3],
                              p.value = rd_no_cov$pv[3],
                              h = rd_no_cov$bws[1],
                              conf.low = rd_no_cov$ci[3],
                              conf.high = rd_no_cov$ci[6],
                              `Obs. used` = sum(rd_no_cov$N_h),
                              cov = "No Covariates",
                              type = type[i],
                              subsample = subsample[j])
    
    north_south_median_population <- rbind(north_south_median_population, results_cov)

  }
}


# round digits
north_south_median_population <- north_south_median_population %>%
  mutate_all(function(x){
    if(is.numeric(x)){ x %>% round(3)
    }else{x}})

# Difference 
north <- north_south_median_population %>% filter(subsample == "North")
south <- north_south_median_population %>% filter(subsample == "Centre-South")
large <- north_south_median_population %>% filter(subsample == "Large M.")
small <- north_south_median_population %>% filter(subsample == "Small M.")

diff_north_south <- bind_cols(south %>% select(dv, estimate, se),
                              north %>% select(estimate, se),
                              data.frame(diff = south$estimate - north$estimate,
                                         se = sqrt(south$se^2 + north$se^2)) %>%
                                mutate(p.value = 2*(1-pnorm(abs(diff / se))))
)

names(diff_north_south)  <- c("Outcome", rep(c("Estimate", "SE"), 2), "Difference", "SE", "p.value")               


diff_pop_size <- bind_cols(large %>% select(dv, estimate, se),
                           small %>% select(estimate, se),
                           data.frame(diff = large$estimate - small$estimate,
                                      se = sqrt(large$se^2 + small$se^2)) %>%
                             mutate(p.value = 2*(1-pnorm(abs(diff / se))))
)

names(diff_pop_size)  <- c("Outcome", rep(c("Estimate", "SE"), 2), "Difference", "SE", "p.value")               

# save table F.12
kableExtra::kable(diff_north_south, booktabs = T,
                  format.args = list(big.mark = ","),
                  align = c("r", rep("c", 7)),
                  digits = 3,
                  linesep = "",
                  caption = "Table F.12: RD estimates of the effect of electing a populist mayor estimated on two sub-samples of observations in the North and Centre-South of the country, as well as the difference in RD estimates. Same estimation of baseline analysis. To maximize sample size, no covariates were included. Northern regions are Veneto, Trentino-Alto Adige, Piemonte, Lombardia, Liguria, Friuli-Venezia Giulia, Emilia-Romagna. Centre and southern regions are the remaining 13 regions. Period of analysis: cost overruns 2012-2020, debt accumulation and repayment 2008-2019, turnover and education of bureaucrats 2001-2019.
", escape = F) %>%
  kableExtra::add_header_above(c(" " = 1, "Centre/South" = 2, "North" = 2, "Difference\n(Centre/South-North)" = 3), bold = T) %>%
  kableExtra::kable_paper(full_width = F) %>%
  kableExtra::kable_classic() %>%
  kableExtra::kable_styling(font_size = 11) %>%
  kableExtra::save_kable(file = "SI_Table_F12.html", self_contained = T)

# save table F.13
kableExtra::kable(diff_pop_size, booktabs = T,
                  format.args = list(big.mark = ","),
                  align = c("r", rep("c", 7)),
                  digits = 3,
                  linesep = "",
                  caption = "Table F.13: RD estimates of the effect of electing a populist mayor estimated on two sub-samples of observations above and below the median population size (8,612 inhabitants), as well as the difference in RD estimates. Same estimation of baseline analysis. To maximize sample size, no covariates were included. Period of analysis: cost overruns 2012-2020, debt accumulation and repayment 2008-2019, turnover and education of bureaucrats 2001-2019.", escape = F) %>%
  kableExtra::add_header_above(c(" " = 1, "Above\nMedian Population" = 2, "Below\nMedian Population" = 2, "Difference\n(Above-Below)" = 3), bold = T) %>%
  kableExtra::kable_paper(full_width = F) %>%
  kableExtra::kable_classic() %>%
  kableExtra::kable_styling(font_size = 11) %>%
  kableExtra::save_kable(file = "SI_Table_F13.html", self_contained = T)

  


