library(data.table)
library(ggplot2)
library(lme4)

dat <- fread("/projects/bga_lab/DATA_REPOSITORIES/UNC_HARPER_MOUSE_DATA/Harper_PSO_MB_LITTER_03212023.csv")
colnames(dat)[c(3,6)] <- c("Stress_group","buried_marbles")
head(dat); dim(dat)

   # Subjects Sex Stress_group genotype litter buried_marbles
# 1:       A1   M            S       HT    PB6             16
# 2:       A2   M            S       WT    PB6             15
# 3:       A3   M            S       HT    PB6             14
# 4:       A4   M            S       WT    PB6             11
# 5:       A5   M            S       HT    PB6             15
# 6:       A6   M            S       WT    PB3             13
# [1] 95  6


table(dat$Stress_group)
# NS  S
# 45 50

table(dat$genotype)
# HT WT
# 46 49

table(dat$litter)
 # PB1 PB10 PB12 PB13 PB16 PB18 PB25 PB28  PB3 PB34  PB6  PB9
   # 7    4   10   10    6    9    8    9    8    8    8    8
   
   
table(dat$buried_marbles)
 # 2  3  4  5  7  8  9 10 11 12 13 14 15 16 17 18 19 20
 # 2  1  2  2  2  3  2  6  9  9  5  9  9 10 14  5  3  2
 

# Remove any missing data
dat <- na.omit(dat)



# Analyze marble burying data
mb_model <- lmer(buried_marbles ~ Stress_group*genotype + (1|litter),data = dat)
summary(mb_model)

# Linear mixed model fit by REML ['lmerMod']
# Formula: buried_marbles ~ Stress_group * genotype + (1 | litter)
   # Data: dat

# REML criterion at convergence: 524.8

# Scaled residuals:
    # Min      1Q  Median      3Q     Max
# -2.5103 -0.5872  0.2311  0.6050  2.2713

# Random effects:
 # Groups   Name        Variance Std.Dev.
 # litter   (Intercept)  2.292   1.514
 # Residual             14.957   3.867
# Number of obs: 95, groups:  litter, 12

# Fixed effects:
                         # Estimate Std. Error t value
# (Intercept)               12.4172     1.0248  12.117
# Stress_groupS              1.0595     1.4505   0.730
# genotypeWT                 0.1098     1.1698   0.094
# Stress_groupS:genotypeWT   0.6110     1.6249   0.376

# Correlation of Fixed Effects:
            # (Intr) Strs_S gntyWT
# Stress_grpS -0.707
# genotypeWT  -0.552  0.390
# Strss_gS:WT  0.397 -0.573 -0.720


# It creates variables for stressed vs. non-stressed dams and genotype mutation status, 
# removes any missing data, and analyzes the data from each behavioral test 
# using linear mixed-effects. 
# The models include stress, genotype mutation status, and litter ID as fixed effects. 


# Conduct two-way repeated measures ANOVA
ano_model <- aov(buried_marbles ~ Stress_group * genotype + Error(litter/genotype),data = dat)

# Check for significant effects
summary(ano_model)

# Error: litter
                      # Df Sum Sq Mean Sq F value Pr(>F)
# Stress_group           1  36.42   36.42   0.974  0.353
# genotype               1  27.28   27.28   0.729  0.418
# Stress_group:genotype  1  10.37   10.37   0.277  0.613
# Residuals              8 299.21   37.40

# Error: litter:genotype
                      # Df Sum Sq Mean Sq F value Pr(>F)
# genotype               1   2.29   2.294   0.086  0.775
# Stress_group:genotype  1   0.78   0.780   0.029  0.867
# Residuals             10 265.69  26.569

# Error: Within
          # Df Sum Sq Mean Sq F value Pr(>F)
# Residuals 71  951.9   13.41


###### Post-hoc comparisons using Fisher's PLSD  #############3
library(lsmeans)
#perform Fisher's LSD
lsmeans(ano_model, pairwise ~ Stress_group * genotype, protect = "adjusted")


# Note: re-fitting model with sum-to-zero contrasts
# NOTE: Results are based on intra-block estimates and are biased.
# $lsmeans
 # Stress_group genotype lsmean   SE   df lower.CL upper.CL
 # NS           HT         12.7 1.20 17.1     10.2     15.2
 # S            HT         13.5 1.18 16.8     11.0     16.0
 # NS           WT         12.8 1.20 17.1     10.3     15.3
 # S            WT         14.0 1.18 16.8     11.5     16.5

# Warning: EMMs are biased unless design is perfectly balanced 
# Confidence level used: 0.95 

# $contrasts
 # contrast      estimate   SE   df t.ratio p.value
 # NS HT - S HT    -0.818 1.69 16.9  -0.484  0.9616
 # NS HT - NS WT   -0.129 1.58 10.0  -0.082  0.9998
 # NS HT - S WT    -1.325 1.69 16.9  -0.784  0.8606
 # S HT - NS WT     0.688 1.69 16.9   0.407  0.9764
 # S HT - S WT     -0.507 1.54 10.0  -0.330  0.9868
 # NS WT - S WT    -1.196 1.69 16.9  -0.708  0.8927

# P value adjustment: tukey method for comparing a family of 4 estimates 



# To perform post-hoc comparisons, we use the lsmeans() function from the lsmeans package. 
# The pairwise argument specifies that we want to obtain pairwise comparisons between 
# the levels of the independent variables  The protect argument is set to "adjusted" 
# to protect against inflation of the family-wise error 
# rate due to multiple comparisons.



################################
################################
###############################

