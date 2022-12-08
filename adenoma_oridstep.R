# Title and author information --------------------------------------------
#!/usr/bin/R

#########################################
#                                       #
#     2021_11_29_adenoma_ordistep.R     #
#                                       #
#########################################

#Title: impact of diet and health history on the gut microbiome
#
#Copyright (C) 2021-2022  Christopher A. Gaulke
#author contact: chris.gaulke@gmail.com
#
#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#(at your option) any later version.
#
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#
#For a copy of the GNU General Public License see
#<http://www.gnu.org/licenses/>.

#Purpose: To determine how patient diet and health history impacts the mb


# SET ENVIRONMENT -------------------------------------------------------------
library(ggplot2)
library(vegan)
library(ggpubr)
options("stringsAsFactors" = F)

# DATA: IMPORT DATA -------------------------------------------------------------


adenoma_asv.df <- read.table("data/2021_11_29_adenoma_asv_table.txt",
                             sep = "\t",
                             header = T,
                             row.names = 1)

adenoma_genus.df <- read.table("data/2021_11_29_adenoma_genus_table.txt",
                               sep = "\t",
                               header = T,
                               row.names = 1)

adenoma_metadata.df <- read.table("data/2021_11_29_adenoma_metadata.txt",
                                  sep = "\t",
                                  header = T,
                                  row.names = 1)

#add an age bin

adenoma_metadata.df$age_bin <- cut(x = adenoma_metadata.df$Age,
                            breaks = c(40,50,60,70,80),
                            labels= c(1,2,3,4)
                            )

# DATA: SUBSET BASED ON TYPE ----------------------------------------------

#get rownames for types
fecal.names         <- rownames(adenoma_metadata.df[which(adenoma_metadata.df$type == "Fecal"),])
oral.names          <- rownames(adenoma_metadata.df[which(adenoma_metadata.df$type == "Oral"),])
mucosal.names       <- rownames(adenoma_metadata.df[which(adenoma_metadata.df$type == "Mucosal"),])

#get asvs
fecal_asv.df        <- adenoma_asv.df[which(rownames(adenoma_asv.df) %in% fecal.names),]
oral_asv.df         <- adenoma_asv.df[which(rownames(adenoma_asv.df) %in% oral.names),]
mucosal_asv.df      <- adenoma_asv.df[which(rownames(adenoma_asv.df) %in% mucosal.names),]

#filter asvs
fecal_asv.df        <- fecal_asv.df[,which(colSums(fecal_asv.df)>0)]
oral_asv.df         <- oral_asv.df[,which(colSums(oral_asv.df)>0)]
mucosal_asv.df      <- mucosal_asv.df[,which(colSums(mucosal_asv.df)>0)]

#get genera
fecal_genus.df      <- adenoma_genus.df[which(rownames(adenoma_genus.df) %in% fecal.names),]
oral_genus.df       <- adenoma_genus.df[which(rownames(adenoma_genus.df) %in% oral.names),]
mucosal_genus.df    <- adenoma_genus.df[which(rownames(adenoma_genus.df) %in% mucosal.names),]

#filter genera
fecal_genus.df      <- fecal_genus.df[,which(colSums(fecal_genus.df)>0)]
oral_genus.df       <- oral_genus.df[,which(colSums(oral_genus.df)>0)]
mucosal_genus.df    <- mucosal_genus.df[,which(colSums(mucosal_genus.df)>0)]

#now metadata
fecal_metadata.df   <- adenoma_metadata.df[which(rownames(adenoma_metadata.df) %in% fecal.names),]
oral_metadata.df    <- adenoma_metadata.df[which(rownames(adenoma_metadata.df) %in% oral.names),]
mucosal_metadata.df <- adenoma_metadata.df[which(rownames(adenoma_metadata.df) %in% mucosal.names),]

#filter

#fecal
fecal_metadata.df <- na.omit(
  fecal_metadata.df[,which(colnames(fecal_metadata.df) %in%
                             c(
                               "Bistol",
                               "age_bin",
                               "Gend",
                               "BMI",
                               "Polyp_N",
                               "Adenoma",
                               "RedMeat",
                               "ProcMeat",
                               "Vegi",
                               "Fruit",
                               "Grain",
                               "Ferment",
                               "RegActiv",
                               "EtOHN",
                               "GERD",
                               "Cancer",
                               "AI",
                               "Diabetes",
                               "GI",
                               "M_ASA",
                               "M_NSAID",
                               "M_VitD",
                               "M_VitE",
                               "M_Ca",
                               "M_Met",
                               "M_HRT",
                               "M_PB",
                               "M_ETC"
                             ))])


#oral
oral_metadata.df <- na.omit(
  oral_metadata.df[,which(colnames(oral_metadata.df) %in%
                            c(
                              "Bistol",
                              "age_bin",
                              "Gend",
                              "BMI",
                              "Polyp_N",
                              "Adenoma",
                              "RedMeat",
                              "ProcMeat",
                              "Vegi",
                              "Fruit",
                              "Grain",
                              "Ferment",
                              "RegActiv",
                              "EtOHN",
                              "GERD",
                              "Cancer",
                              "AI",
                              "Diabetes",
                              "GI",
                              "M_ASA",
                              "M_NSAID",
                              "M_VitD",
                              "M_VitE",
                              "M_Ca",
                              "M_Met",
                              "M_HRT",
                              "M_PB",
                              "M_ETC"
                            ))])

#mucosal

mucosal_metadata.df <- na.omit(
  mucosal_metadata.df[,which(colnames(mucosal_metadata.df) %in%
                               c(
                                 "Bistol",
                                 "age_bin",
                                 "Gend",
                                 "BMI",
                                 "Polyp_N",
                                 "Adenoma",
                                 "RedMeat",
                                 "ProcMeat",
                                 "Vegi",
                                 "Fruit",
                                 "Grain",
                                 "Ferment",
                                 "RegActiv",
                                 "EtOHN",
                                 "GERD",
                                 "Cancer",
                                 "AI",
                                 "Diabetes",
                                 "GI",
                                 "M_ASA",
                                 "M_NSAID",
                                 "M_VitD",
                                 "M_VitE",
                                 "M_Ca",
                                 "M_Met",
                                 "M_HRT",
                                 "M_PB",
                                 "M_ETC"
                               ))])

# DATA: NORMALIZE ---------------------------------------------------------

#normalize ASV

fecal_asv_capscale.df   <- decostand(fecal_asv.df, method = "log")
oral_asv_capscale.df    <- decostand(oral_asv.df, method = "log")
mucosal_asv_capscale.df <- decostand(mucosal_asv.df, method = "log")

#normalize genus

fecal_genus_capscale.df   <- decostand(fecal_genus.df, method = "log")
oral_genus_capscale.df    <- decostand(oral_genus.df, method = "log")
mucosal_genus_capscale.df <- decostand(mucosal_genus.df, method = "log")

# ANALYSIS: FECAL ASV ORDISTEP ------------------------------------------------

#filter
fecal_asv_capscale.df <- fecal_asv_capscale.df[which(rownames(fecal_asv_capscale.df) %in% rownames(fecal_metadata.df)),]

set.seed(731)
fecal_capscale_asv.capscale_full <- capscale(fecal_asv_capscale.df ~
                                               factor(Bistol) +
                                               age_bin +
                                               factor(Gend) +
                                               BMI +
                                               Polyp_N +
                                               Adenoma +
                                               RedMeat +
                                               ProcMeat +
                                               Vegi +
                                               Fruit +
                                               Grain +
                                               Ferment +
                                               factor(RegActiv) +
                                               EtOHN +
                                               factor(GERD) +
                                               factor(Cancer) +
                                               factor(AI) +
                                               factor(Diabetes) +
                                               factor(GI) +
                                               factor(M_ASA) +
                                               factor(M_NSAID) +
                                               factor(M_VitD) +
                                               factor(M_VitE) +
                                               factor(M_Ca) +
                                               factor(M_Met) +
                                               factor(M_HRT) +
                                               factor(M_PB) +
                                               factor(M_ETC)
                                             ,
                                             data = fecal_metadata.df,
                                             na.action = na.omit)

set.seed(731)
fecal_asv_capscale_full.ordistep <- ordistep(fecal_capscale_asv.capscale_full,
                                             na.action = na.omit,
                                             direction = "both",
                                             trace = F)

#get sig
set.seed(731)
anova(fecal_asv_capscale_full.ordistep) # P = 0.001

set.seed(731)
fecal_asv_capscale_full_ordistep.aov <-
  anova(fecal_asv_capscale_full.ordistep, by = "terms")

# Df Variance      F Pr(>F)
# factor(Bistol)    8   300.16 1.1212  0.023 *
# BMI               1    41.03 1.2261  0.043 *
# ProcMeat          1    40.13 1.1990  0.068 .
# Fruit             1    50.76 1.5169  0.005 **
# factor(RegActiv)  1    70.81 2.1160  0.001 ***
# EtOHN             1    39.50 1.1803  0.084 .
# factor(M_VitD)    1    39.70 1.1864  0.071 .
# factor(M_HRT)     1    39.78 1.1888  0.071 .
# Residual         73  2442.95

RsquareAdj(fecal_asv_capscale_full.ordistep) # adjusted R2 = 0.04

# ANALYSIS: ORAL ASV ORDISTEP ------------------------------------------------

#filter
oral_asv_capscale.df <- oral_asv_capscale.df[which(rownames(oral_asv_capscale.df) %in% rownames(oral_metadata.df)),]

set.seed(731)
oral_capscale_asv.capscale_full <- capscale(oral_asv_capscale.df ~
                                              factor(Bistol) +
                                              age_bin +
                                              factor(Gend) +
                                              BMI +
                                              Polyp_N +
                                              Adenoma +
                                              RedMeat +
                                              ProcMeat +
                                              Vegi +
                                              Fruit +
                                              Grain +
                                              Ferment +
                                              factor(RegActiv) +
                                              EtOHN +
                                              factor(GERD) +
                                              factor(Cancer) +
                                              factor(AI) +
                                              factor(Diabetes) +
                                              factor(GI) +
                                              factor(M_ASA) +
                                              factor(M_NSAID) +
                                              factor(M_VitD) +
                                              factor(M_VitE) +
                                              factor(M_Ca) +
                                              factor(M_Met) +
                                              factor(M_HRT) +
                                              factor(M_PB) +
                                              factor(M_ETC)
                                            ,
                                            data = oral_metadata.df,
                                            na.action = na.omit)

set.seed(731)
oral_asv_capscale_full.ordistep <- ordistep(oral_capscale_asv.capscale_full,
                                            na.action = na.omit,
                                            direction = "both",
                                            trace = F)

#get sig
set.seed(731)
anova(oral_asv_capscale_full.ordistep) # P = 0.001

set.seed(731)
oral_asv_capscale_full_ordistep.aov <-
  anova(oral_asv_capscale_full.ordistep, by = "terms")

# Df Variance      F Pr(>F)
# BMI             1    28.11 1.8312  0.006 **
# Adenoma         1    23.68 1.5430  0.041 *
# EtOHN           1    23.12 1.5065  0.039 *
# factor(AI)      1    22.35 1.4561  0.041 *
# factor(M_VitE)  1    21.02 1.3692  0.062 .
# Residual       78  1197.23

RsquareAdj(oral_asv_capscale_full.ordistep) # adjusted R2 = 0.03

# ANALYSIS: MUCOSAL ASV ORDISTEP ------------------------------------------------

#filter
mucosal_asv_capscale.df <- mucosal_asv_capscale.df[which(rownames(mucosal_asv_capscale.df) %in% rownames(mucosal_metadata.df)),]

set.seed(731)
mucosal_capscale_asv.capscale_full <- capscale(mucosal_asv_capscale.df ~
                                                 factor(Bistol) +
                                                 age_bin +
                                                 factor(Gend) +
                                                 BMI +
                                                 Polyp_N +
                                                 Adenoma +
                                                 RedMeat +
                                                 ProcMeat +
                                                 Vegi +
                                                 Fruit +
                                                 Grain +
                                                 Ferment +
                                                 factor(RegActiv) +
                                                 EtOHN +
                                                 factor(GERD) +
                                                 factor(Cancer) +
                                                 factor(AI) +
                                                 factor(Diabetes) +
                                                 factor(GI) +
                                                 factor(M_ASA) +
                                                 factor(M_NSAID) +
                                                 factor(M_VitD) +
                                                 factor(M_VitE) +
                                                 factor(M_Ca) +
                                                 factor(M_Met) +
                                                 factor(M_HRT) +
                                                 factor(M_PB) +
                                                 factor(M_ETC)
                                               ,
                                               data = mucosal_metadata.df,
                                               na.action = na.omit)

set.seed(731)
mucosal_asv_capscale_full.ordistep <- ordistep(mucosal_capscale_asv.capscale_full,
                                               na.action = na.omit,
                                               direction = "both",
                                               trace = F)

#get sig
set.seed(731)
anova(mucosal_asv_capscale_full.ordistep) # P = 0.001

set.seed(731)
mucosal_asv_capscale_full_ordistep.aov <-
  anova(mucosal_asv_capscale_full.ordistep, by = "terms")

# Df Variance      F Pr(>F)
# factor(Bistol)     8   225.26 5.7254  0.001 ***
# age_bin            3    86.27 5.8474  0.001 ***
# factor(Gend)       1    21.28 4.3261  0.001 ***
# BMI                1    22.02 4.4765  0.001 ***
# Polyp_N            1    41.75 8.4886  0.001 ***
# Adenoma            1    31.57 6.4190  0.001 ***
# RedMeat            1    21.56 4.3849  0.001 ***
# ProcMeat           1    23.98 4.8769  0.001 ***
# Vegi               1    20.31 4.1300  0.001 ***
# Fruit              1    30.58 6.2189  0.001 ***
# Grain              1    20.69 4.2079  0.001 ***
# Ferment            1    24.27 4.9353  0.001 ***
# factor(RegActiv)   1    39.01 7.9327  0.001 ***
# EtOHN              1    22.74 4.6233  0.001 ***
# factor(GERD)       1    23.19 4.7145  0.001 ***
# factor(Cancer)     1    24.37 4.9564  0.001 ***
# factor(AI)         1    20.92 4.2544  0.001 ***
# factor(Diabetes)   3    87.53 5.9326  0.001 ***
# factor(GI)         1    20.90 4.2501  0.001 ***
# factor(M_ASA)      1    20.32 4.1312  0.001 ***
# factor(M_NSAID)    1    23.95 4.8707  0.001 ***
# factor(M_VitD)     1    23.66 4.8105  0.001 ***
# factor(M_VitE)     1    21.26 4.3238  0.001 ***
# factor(M_Ca)       1    20.24 4.1163  0.001 ***
# factor(M_Met)      1    29.30 5.9581  0.001 ***
# factor(M_HRT)      1    21.19 4.3078  0.001 ***
# factor(M_PB)       1    21.85 4.4421  0.001 ***
# factor(M_ETC)      1    18.78 3.8189  0.001 ***
# Residual         292  1436.03
RsquareAdj(mucosal_asv_capscale_full.ordistep) # adjusted R2 = 0.33

# ANALYSIS: FECAL GENUS ORDISTEP ------------------------------------------------

#filter
fecal_genus_capscale.df <- fecal_genus_capscale.df[which(rownames(fecal_genus_capscale.df) %in% rownames(fecal_metadata.df)),]

set.seed(731)
fecal_capscale_genus.capscale_full <- capscale(fecal_genus_capscale.df ~
                                                 factor(Bistol) +
                                                 age_bin +
                                                 factor(Gend) +
                                                 BMI +
                                                 Polyp_N +
                                                 Adenoma +
                                                 RedMeat +
                                                 ProcMeat +
                                                 Vegi +
                                                 Fruit +
                                                 Grain +
                                                 Ferment +
                                                 factor(RegActiv) +
                                                 EtOHN +
                                                 factor(GERD) +
                                                 factor(Cancer) +
                                                 factor(AI) +
                                                 factor(Diabetes) +
                                                 factor(GI) +
                                                 factor(M_ASA) +
                                                 factor(M_NSAID) +
                                                 factor(M_VitD) +
                                                 factor(M_VitE) +
                                                 factor(M_Ca) +
                                                 factor(M_Met) +
                                                 factor(M_HRT) +
                                                 factor(M_PB) +
                                                 factor(M_ETC)
                                               ,
                                               data = fecal_metadata.df,
                                               na.action = na.omit)

set.seed(731)
fecal_genus_capscale_full.ordistep <- ordistep(fecal_capscale_genus.capscale_full,
                                               na.action = na.omit,
                                               direction = "both",
                                               trace = F)

#get sig
set.seed(731)
anova(fecal_genus_capscale_full.ordistep) # P = 0.001

set.seed(731)
fecal_genus_capscale_full_ordistep.aov <-
  anova(fecal_genus_capscale_full.ordistep, by = "terms")

# Df Variance      F Pr(>F)
# factor(Bistol)    8    79.18 1.2710  0.018 *
# age_bin           3    30.66 1.3122  0.027 *
# ProcMeat          1    12.08 1.5515  0.033 *
# Fruit             1    14.38 1.8463  0.011 *
# Grain             1    12.09 1.5524  0.038 *
# factor(RegActiv)  1    19.61 2.5182  0.001 ***
# EtOHN             1    12.22 1.5698  0.032 *
# factor(Diabetes)  3    31.80 1.3613  0.027 *
# factor(M_VitD)    1    13.65 1.7524  0.011 *
# factor(M_HRT)     1    13.96 1.7931  0.014 *
# Residual         67   521.75

RsquareAdj(fecal_genus_capscale_full.ordistep) # adjusted R2 = 0.10

# ANALYSIS: ORAL GENUS ORDISTEP ------------------------------------------------

#filter
oral_genus_capscale.df <- oral_genus_capscale.df[which(rownames(oral_genus_capscale.df) %in% rownames(oral_metadata.df)),]

set.seed(731)
oral_capscale_genus.capscale_full <- capscale(oral_genus_capscale.df ~
                                                factor(Bistol) +
                                                age_bin +
                                                factor(Gend) +
                                                BMI +
                                                Polyp_N +
                                                Adenoma +
                                                RedMeat +
                                                ProcMeat +
                                                Vegi +
                                                Fruit +
                                                Grain +
                                                Ferment +
                                                factor(RegActiv) +
                                                EtOHN +
                                                factor(GERD) +
                                                factor(Cancer) +
                                                factor(AI) +
                                                factor(Diabetes) +
                                                factor(GI) +
                                                factor(M_ASA) +
                                                factor(M_NSAID) +
                                                factor(M_VitD) +
                                                factor(M_VitE) +
                                                factor(M_Ca) +
                                                factor(M_Met) +
                                                factor(M_HRT) +
                                                factor(M_PB) +
                                                factor(M_ETC)
                                              ,
                                              data = oral_metadata.df,
                                              na.action = na.omit)

set.seed(731)
oral_genus_capscale_full.ordistep <- ordistep(oral_capscale_genus.capscale_full,
                                              na.action = na.omit,
                                              direction = "both",
                                              trace = F)

#get sig
set.seed(731)
anova(oral_genus_capscale_full.ordistep) # P = 0.001

set.seed(731)
oral_genus_capscale_full_ordistep.aov <-
  anova(oral_genus_capscale_full.ordistep, by = "terms")

# Df Variance      F Pr(>F)
# factor(Bistol)  8   37.669 1.4454  0.008 **
# BMI             1   10.776 3.3078  0.002 **
# RedMeat         1    5.405 1.6591  0.068 .
# Ferment         1    5.049 1.5499  0.108
# factor(M_Ca)    1    7.864 2.4140  0.015 *
# Residual       71  231.296

RsquareAdj(oral_genus_capscale_full.ordistep) # adjusted R2 = 0.09

# ANALYSIS: MUCOSAL GENUS ORDISTEP ------------------------------------------------

#filter
mucosal_genus_capscale.df <- mucosal_genus_capscale.df[which(rownames(mucosal_genus_capscale.df) %in% rownames(mucosal_metadata.df)),]

set.seed(731)
mucosal_capscale_genus.capscale_full <- capscale(mucosal_genus_capscale.df ~
                                                   factor(Bistol) +
                                                   age_bin +
                                                   factor(Gend) +
                                                   BMI +
                                                   Polyp_N +
                                                   Adenoma +
                                                   RedMeat +
                                                   ProcMeat +
                                                   Vegi +
                                                   Fruit +
                                                   Grain +
                                                   Ferment +
                                                   factor(RegActiv) +
                                                   EtOHN +
                                                   factor(GERD) +
                                                   factor(Cancer) +
                                                   factor(AI) +
                                                   factor(Diabetes) +
                                                   factor(GI) +
                                                   factor(M_ASA) +
                                                   factor(M_NSAID) +
                                                   factor(M_VitD) +
                                                   factor(M_VitE) +
                                                   factor(M_Ca) +
                                                   factor(M_Met) +
                                                   factor(M_HRT) +
                                                   factor(M_PB) +
                                                   factor(M_ETC)
                                                 ,
                                                 data = mucosal_metadata.df,
                                                 na.action = na.omit)

set.seed(731)
mucosal_genus_capscale_full.ordistep <- ordistep(mucosal_capscale_genus.capscale_full,
                                                 na.action = na.omit,
                                                 direction = "both",
                                                 trace = F)

#get sig
set.seed(731)
anova(mucosal_genus_capscale_full.ordistep) # P = 0.001

set.seed(731)
mucosal_genus_capscale_full_ordistep.aov <-
  anova(mucosal_genus_capscale_full.ordistep, by = "terms")

# Df Variance      F Pr(>F)
# factor(Bistol)     8    67.55 5.3953  0.001 ***
# age_bin            3    29.72 6.3311  0.001 ***
# factor(Gend)       1     6.10 3.9008  0.001 ***
# BMI                1     5.00 3.1958  0.001 ***
# Polyp_N            1    14.01 8.9504  0.001 ***
# Adenoma            1     8.73 5.5769  0.001 ***
# RedMeat            1     6.34 4.0504  0.001 ***
# ProcMeat           1     7.67 4.9028  0.001 ***
# Vegi               1     8.33 5.3250  0.001 ***
# Fruit              1    10.26 6.5536  0.001 ***
# Grain              1     7.80 4.9852  0.001 ***
# Ferment            1     8.48 5.4158  0.001 ***
# factor(RegActiv)   1    12.65 8.0804  0.001 ***
# EtOHN              1     7.89 5.0409  0.001 ***
# factor(GERD)       1     6.66 4.2553  0.001 ***
# factor(Cancer)     1     7.02 4.4834  0.001 ***
# factor(AI)         1     5.90 3.7676  0.001 ***
# factor(Diabetes)   3    29.65 6.3143  0.001 ***
# factor(GI)         1     5.67 3.6234  0.001 ***
# factor(M_ASA)      1     5.73 3.6636  0.001 ***
# factor(M_NSAID)    1     6.09 3.8905  0.001 ***
# factor(M_VitD)     1     8.52 5.4472  0.001 ***
# factor(M_VitE)     1     5.50 3.5168  0.001 ***
# factor(M_Ca)       1     6.30 4.0267  0.001 ***
# factor(M_Met)      1     8.79 5.6176  0.001 ***
# factor(M_HRT)      1     7.02 4.4869  0.001 ***
# factor(M_PB)       1     5.73 3.6612  0.001 ***
# factor(M_ETC)      1     4.90 3.1309  0.001 ***
# Residual         292   456.98
RsquareAdj(mucosal_genus_capscale_full.ordistep) # adjusted R2 = 0.33

# ANALYSIS: FECAL GENUS ENVFIT ------------------------------------------

fecal_ordistep.scores <- scores(fecal_capscale_genus.capscale_full)

fecal_ordistep.fit <- envfit(fecal_genus_capscale_full.ordistep ~
                               factor(Bistol) +
                               age_bin +
                               ProcMeat +
                               Fruit +
                               Grain +
                               factor(RegActiv) +
                               EtOHN +
                               factor(Diabetes) +
                               factor(M_VitD) +
                               factor(M_HRT),
                             fecal_metadata.df,
                             perm=999,
                             display="lc")


#to get arrows scores(fecal_ordistep.fit, "bp")
#then find scaling factor vegan:::ordiArrowMul(fecal_ordistep.fit)
#should be able to figure out rise over run then
#note the scaling determines the length and is based off r2 of the
#fit so could come up with a sensible scaling factor so that we could
#use better scaling tools

#build object

fecal_ordistep_biplot.df <-
  as.data.frame(fecal_ordistep.scores$sites)

fecal_ordistep_biplot.df$polyp <-
  as.numeric((fecal_metadata.df$Polyp_N) > 0)

#get min and max for plot dimension and vector scaling
min.cap1 <- min(fecal_ordistep_biplot.df$CAP1)
max.cap1 <- max(fecal_ordistep_biplot.df$CAP1)
min.cap2 <- min(fecal_ordistep_biplot.df$CAP2)
max.cap2 <- max(fecal_ordistep_biplot.df$CAP2)

scale.factor <- max(abs(c(min.cap1,min.cap2,max.cap1,max.cap2)))
scale.factor <- scale.factor + sqrt(scale.factor)


#make a df of vector information
fecal_ordistep.vectors <-
  as.data.frame(scores(fecal_ordistep.fit, "vectors"))

#add fit stats
fecal_ordistep.vectors$r_val <-
  fecal_ordistep.fit$vectors$r

fecal_ordistep.vectors$p_val <-
  fecal_ordistep.fit$vectors$pvals

#add endings
fecal_ordistep.vectors$c1_end <-
  fecal_ordistep.vectors$CAP1 * scale.factor

fecal_ordistep.vectors$c2_end <-
  fecal_ordistep.vectors$CAP2 * scale.factor

fecal_ordistep.vectors$c1_begin <- 0
fecal_ordistep.vectors$c2_begin <- 0
fecal_ordistep.vectors$name <- rownames(fecal_ordistep.vectors)
fecal_ordistep.vectors$text_x <- fecal_ordistep.vectors$c1_end + c(-.5,0,0,0)
fecal_ordistep.vectors$text_y <- fecal_ordistep.vectors$c2_end + c(-.5,-.5,-.2,.3)

#Now try to plot this mess

pdf("figs/fecal_genus_ordiplot.pdf")

fecal_ordistep.biplot <- ggplot(fecal_ordistep_biplot.df,
                                aes(x = CAP1,
                                    y = CAP2,
                                    #fill = factor(polyp)
                                    ))

fecal_ordistep.biplot +
  geom_point(shape = 21, size = 4, alpha = .7, fill = "#568F55")+
  #scale_fill_brewer("Polyp Former",
  #                  type = "qual",
  #                  palette = 2,
  #                  direction = 1
  #)+
  geom_segment(data = fecal_ordistep.vectors,size =1,
               aes(x = c1_begin,
                   y = c2_begin,
                   xend = c1_end,
                   yend = c2_end),
               arrow = arrow(length = unit(0.02, "npc")),
               inherit.aes = FALSE)+
  annotate("text", size = 6, fontface = "bold",
           x = fecal_ordistep.vectors$text_x,
           y = fecal_ordistep.vectors$text_y,
           label = fecal_ordistep.vectors[,"name"])+
  theme(text = element_text(size=18, colour = "black"),
        panel.grid.major = element_line(color = "grey94"),
        panel.grid.minor = element_line(color = "grey94"),
        panel.background = element_blank(),
        axis.text        = element_text(colour = "black"),
        axis.line        = element_blank(),
        legend.key       = element_rect(fill=NA),
        legend.position  = "top",
        panel.border     = element_rect(colour = "black", fill=NA, size=1)
  )
dev.off()

# ANALYSIS: ORAL GENUS ENVFIT ------------------------------------------

oral_ordistep.scores <- scores(oral_capscale_genus.capscale_full)

oral_ordistep.fit <- envfit(oral_genus_capscale_full.ordistep ~
                              factor(Bistol) +
                              BMI +
                              RedMeat +
                              Ferment +
                              factor(M_Ca),
                            oral_metadata.df,
                            perm=999,
                            display="lc")


#to get arrows scores(oral_ordistep.fit, "bp")
#then find scaling factor vegan:::ordiArrowMul(oral_ordistep.fit)
#should be able to figure out rise over run then
#note the scaling determines the length and is based off r2 of the
#fit so could come up with a sensible scaling factor so that we could
#use better scaling tools

#build object

oral_ordistep_biplot.df <-
  as.data.frame(oral_ordistep.scores$sites)

oral_ordistep_biplot.df$polyp <-
  as.numeric((oral_metadata.df$Polyp_N) > 0)

#get min and max for plot dimension and vector scaling
min.cap1 <- min(oral_ordistep_biplot.df$CAP1)
max.cap1 <- max(oral_ordistep_biplot.df$CAP1)
min.cap2 <- min(oral_ordistep_biplot.df$CAP2)
max.cap2 <- max(oral_ordistep_biplot.df$CAP2)

scale.factor <- max(abs(c(min.cap1,min.cap2,max.cap1,max.cap2)))
scale.factor <- scale.factor + sqrt(scale.factor)


#make a df of vector information
oral_ordistep.vectors <-
  as.data.frame(scores(oral_ordistep.fit, "vectors"))

#add fit stats
oral_ordistep.vectors$r_val <-
  oral_ordistep.fit$vectors$r

oral_ordistep.vectors$p_val <-
  oral_ordistep.fit$vectors$pvals

#add endings
oral_ordistep.vectors$c1_end <-
  oral_ordistep.vectors$CAP1 * scale.factor

oral_ordistep.vectors$c2_end <-
  oral_ordistep.vectors$CAP2 * scale.factor

oral_ordistep.vectors$c1_begin <- 0
oral_ordistep.vectors$c2_begin <- 0
oral_ordistep.vectors$name <- rownames(oral_ordistep.vectors)
oral_ordistep.vectors$text_x <- oral_ordistep.vectors$c1_end + c(0,0,0)
oral_ordistep.vectors$text_y <- oral_ordistep.vectors$c2_end + c(-.2,.5,-.2)

#Now try to plot this mess

pdf("figs/oral_genus_ordiplot.pdf")
oral_ordistep.biplot <- ggplot(oral_ordistep_biplot.df,
                               aes(x = CAP1,
                                   y = CAP2,
                                   #fill = factor(polyp)
                                   ))

oral_ordistep.biplot +
  geom_point(shape = 21, size = 4, alpha = .7, fill = "#7840A1")+
  #scale_fill_brewer("Polyp Former",
  #                  type = "qual",
  #                  palette = 2,
  #                  direction = 1
  #)+
  geom_segment(data = oral_ordistep.vectors,size =1,
               aes(x = c1_begin,
                   y = c2_begin,
                   xend = c1_end,
                   yend = c2_end),
               arrow = arrow(length = unit(0.02, "npc")),
               inherit.aes = FALSE)+
  annotate("text", size = 6, fontface = "bold",
           x = oral_ordistep.vectors$text_x,
           y = oral_ordistep.vectors$text_y,
           label = oral_ordistep.vectors[,"name"])+
  theme(text = element_text(size=18, colour = "black"),
        panel.grid.major = element_line(color = "grey94"),
        panel.grid.minor = element_line(color = "grey94"),
        panel.background = element_blank(),
        axis.text        = element_text(colour = "black"),
        axis.line        = element_blank(),
        legend.key       = element_rect(fill=NA),
        legend.position  = "top",
        panel.border     = element_rect(colour = "black", fill=NA, size=1)
  )

dev.off()

# ANALYSIS: MUCOSAL GENUS ENVFIT ------------------------------------------

mucosal_ordistep.scores <- scores(mucosal_capscale_genus.capscale_full)

mucosal_ordistep.fit <- envfit(mucosal_genus_capscale_full.ordistep ~
                                 factor(Bistol) +
                                 age_bin +
                                 factor(Gend) +
                                 BMI +
                                 Polyp_N +
                                 Adenoma +
                                 RedMeat +
                                 ProcMeat +
                                 Vegi +
                                 Fruit +
                                 Grain +
                                 Ferment +
                                 factor(RegActiv) +
                                 EtOHN +
                                 factor(GERD) +
                                 factor(Cancer) +
                                 factor(AI) +
                                 factor(Diabetes) +
                                 factor(GI) +
                                 factor(M_ASA) +
                                 factor(M_NSAID) +
                                 factor(M_VitD) +
                                 factor(M_VitE) +
                                 factor(M_Ca) +
                                 factor(M_Met) +
                                 factor(M_HRT) +
                                 factor(M_PB) +
                                 factor(M_ETC),
                               mucosal_metadata.df,
                               perm=999,
                               display="lc")


#to get arrows scores(mucosal_ordistep.fit, "bp")
#then find scaling factor vegan:::ordiArrowMul(mucosal_ordistep.fit)
#should be able to figure out rise over run then
#note the scaling determines the length and is based off r2 of the
#fit so could come up with a sensible scaling factor so that we could
#use better scaling tools

#build object

mucosal_ordistep_biplot.df <-
  as.data.frame(mucosal_ordistep.scores$sites)

mucosal_ordistep_biplot.df$polyp <-
  as.numeric((mucosal_metadata.df$Polyp_N) > 0)

#get min and max for plot dimension and vector scaling
min.cap1 <- min(mucosal_ordistep_biplot.df$CAP1)
max.cap1 <- max(mucosal_ordistep_biplot.df$CAP1)
min.cap2 <- min(mucosal_ordistep_biplot.df$CAP2)
max.cap2 <- max(mucosal_ordistep_biplot.df$CAP2)

scale.factor <- max(abs(c(min.cap1,min.cap2,max.cap1,max.cap2)))
scale.factor <- scale.factor + sqrt(scale.factor)


#make a df of vector information
mucosal_ordistep.vectors <-
  as.data.frame(scores(mucosal_ordistep.fit, "vectors"))

#add fit stats
mucosal_ordistep.vectors$r_val <-
  mucosal_ordistep.fit$vectors$r

mucosal_ordistep.vectors$p_val <-
  mucosal_ordistep.fit$vectors$pvals

#add endings
mucosal_ordistep.vectors$c1_end <-
  mucosal_ordistep.vectors$CAP1 * scale.factor

mucosal_ordistep.vectors$c2_end <-
  mucosal_ordistep.vectors$CAP2 * scale.factor

mucosal_ordistep.vectors$c1_begin <- 0
mucosal_ordistep.vectors$c2_begin <- 0
mucosal_ordistep.vectors$name <- rownames(mucosal_ordistep.vectors)
mucosal_ordistep.vectors$text_x <- mucosal_ordistep.vectors$c1_end + c(-.25,0,-.5,-.1,.7,.4,.4,0,0,.4)
mucosal_ordistep.vectors$text_y <- mucosal_ordistep.vectors$c2_end + c(0,.2,-.1,-.1,0,0,0,.2,-.05,-.1)

#Now try to plot this mess

pdf("figs/mucosal_genus_ordiplot.pdf")
mucosal_ordistep.biplot <- ggplot(mucosal_ordistep_biplot.df,
                                  aes(x = CAP1,
                                      y = CAP2,
                                      #fill = factor(polyp)
                                      ))

mucosal_ordistep.biplot +
  geom_point(shape = 21, size = 4, alpha = .7, fill = "#BE88E5")+
  #scale_fill_brewer("Polyp Former",
  #                  type = "qual",
  #                  palette = 2,
  #                  direction = 1
  #)+
  geom_segment(data = mucosal_ordistep.vectors,size =1,
               aes(x = c1_begin,
                   y = c2_begin,
                   xend = c1_end,
                   yend = c2_end),
               arrow = arrow(length = unit(0.02, "npc")),
               inherit.aes = FALSE)+
  annotate("text", size = 6, fontface = "bold",
           x = mucosal_ordistep.vectors$text_x,
           y = mucosal_ordistep.vectors$text_y,
           label = mucosal_ordistep.vectors[,"name"])+
  theme(text = element_text(size=18, colour = "black"),
        panel.grid.major = element_line(color = "grey94"),
        panel.grid.minor = element_line(color = "grey94"),
        panel.background = element_blank(),
        axis.text        = element_text(colour = "black"),
        axis.line        = element_blank(),
        legend.key       = element_rect(fill=NA),
        legend.position  = "top",
        panel.border     = element_rect(colour = "black", fill=NA, size=1)
  )

dev.off()

# ANALYSIS: FECAL ASV ENVFIT ------------------------------------------

fecal_asv_ordistep.scores <- scores(fecal_capscale_asv.capscale_full)

fecal_asv_ordistep.fit <- envfit(fecal_asv_capscale_full.ordistep ~
                               factor(Bistol) +
                               BMI +
                               ProcMeat +
                               Fruit +
                               factor(RegActiv) +
                               EtOHN +
                               factor(M_VitD) +
                               factor(M_HRT),
                             fecal_metadata.df,
                             perm=999,
                             display="lc")


#to get arrows scores(fecal_ordistep.fit, "bp")
#then find scaling factor vegan:::ordiArrowMul(fecal_ordistep.fit)
#should be able to figure out rise over run then
#note the scaling determines the length and is based off r2 of the
#fit so could come up with a sensible scaling factor so that we could
#use better scaling tools

#build object

fecal_asv_ordistep_biplot.df <-
  as.data.frame(fecal_asv_ordistep.scores$sites)

fecal_asv_ordistep_biplot.df$polyp <-
  as.numeric((fecal_metadata.df$Polyp_N) > 0)

#get min and max for plot dimension and vector scaling
min.cap1 <- min(fecal_asv_ordistep_biplot.df$CAP1)
max.cap1 <- max(fecal_asv_ordistep_biplot.df$CAP1)
min.cap2 <- min(fecal_asv_ordistep_biplot.df$CAP2)
max.cap2 <- max(fecal_asv_ordistep_biplot.df$CAP2)

scale.factor <- max(abs(c(min.cap1,min.cap2,max.cap1,max.cap2)))
scale.factor <- scale.factor + sqrt(scale.factor)


#make a df of vector information
fecal_asv_ordistep.vectors <-
  as.data.frame(scores(fecal_asv_ordistep.fit, "vectors"))

#add fit stats
fecal_asv_ordistep.vectors$r_val <-
  fecal_asv_ordistep.fit$vectors$r

fecal_asv_ordistep.vectors$p_val <-
  fecal_asv_ordistep.fit$vectors$pvals

#add endings
fecal_asv_ordistep.vectors$c1_end <-
  fecal_asv_ordistep.vectors$CAP1 * scale.factor

fecal_asv_ordistep.vectors$c2_end <-
  fecal_asv_ordistep.vectors$CAP2 * scale.factor

fecal_asv_ordistep.vectors$c1_begin <- 0
fecal_asv_ordistep.vectors$c2_begin <- 0
fecal_asv_ordistep.vectors$name <- rownames(fecal_asv_ordistep.vectors)
fecal_asv_ordistep.vectors$text_x <- fecal_asv_ordistep.vectors$c1_end + c(0,0,0,-1)
fecal_asv_ordistep.vectors$text_y <- fecal_asv_ordistep.vectors$c2_end + c(.5,.5,.6,0)

#Now try to plot this mess

pdf("figs/fecal_asv_ordiplot.pdf")

fecal_asv_ordistep.biplot <- ggplot(fecal_asv_ordistep_biplot.df,
                                aes(x = CAP1,
                                    y = CAP2,
                                    fill = factor(polyp)))

fecal_asv_ordistep.biplot +
  geom_point(shape = 21, size = 4, alpha = .7)+
  scale_fill_brewer("Polyp Former",
                    type = "qual",
                    palette = 2,
                    direction = 1
  )+
  geom_segment(data = fecal_asv_ordistep.vectors,size =1,
               aes(x = c1_begin,
                   y = c2_begin,
                   xend = c1_end,
                   yend = c2_end),
               arrow = arrow(length = unit(0.02, "npc")),
               inherit.aes = FALSE)+
  annotate("text", size = 6, fontface = "bold",
           x = fecal_asv_ordistep.vectors$text_x,
           y = fecal_asv_ordistep.vectors$text_y,
           label = fecal_asv_ordistep.vectors[,"name"])+
  theme(text = element_text(size=18, colour = "black"),
        panel.grid.major = element_line(color = "grey94"),
        panel.grid.minor = element_line(color = "grey94"),
        panel.background = element_blank(),
        axis.text        = element_text(colour = "black"),
        axis.line        = element_blank(),
        legend.key       = element_rect(fill=NA),
        legend.position  = "top",
        panel.border     = element_rect(colour = "black", fill=NA, size=1)
  )

dev.off()

# ANALYSIS: ORAL ASV ENVFIT ------------------------------------------

oral_asv_ordistep.scores <- scores(oral_capscale_asv.capscale_full)

oral_asv_ordistep.fit <- envfit(oral_asv_capscale_full.ordistep ~
                              BMI +
                              Adenoma+
                              EtOHN +
                              factor(AI) +
                              factor(M_VitE),
                            oral_metadata.df,
                            perm=999,
                            display="lc")


#to get arrows scores(oral_ordistep.fit, "bp")
#then find scaling factor vegan:::ordiArrowMul(oral_ordistep.fit)
#should be able to figure out rise over run then
#note the scaling determines the length and is based off r2 of the
#fit so could come up with a sensible scaling factor so that we could
#use better scaling tools

#build object

oral_asv_ordistep_biplot.df <-
  as.data.frame(oral_asv_ordistep.scores$sites)

oral_asv_ordistep_biplot.df$polyp <-
  as.numeric((oral_metadata.df$Polyp_N) > 0)

#get min and max for plot dimension and vector scaling
min.cap1 <- min(oral_asv_ordistep_biplot.df$CAP1)
max.cap1 <- max(oral_asv_ordistep_biplot.df$CAP1)
min.cap2 <- min(oral_asv_ordistep_biplot.df$CAP2)
max.cap2 <- max(oral_asv_ordistep_biplot.df$CAP2)

scale.factor <- max(abs(c(min.cap1,min.cap2,max.cap1,max.cap2)))
scale.factor <- scale.factor + sqrt(scale.factor)


#make a df of vector information
oral_asv_ordistep.vectors <-
  as.data.frame(scores(oral_asv_ordistep.fit, "vectors"))

#add fit stats
oral_asv_ordistep.vectors$r_val <-
  oral_asv_ordistep.fit$vectors$r

oral_asv_ordistep.vectors$p_val <-
  oral_asv_ordistep.fit$vectors$pvals

#add endings
oral_asv_ordistep.vectors$c1_end <-
  oral_asv_ordistep.vectors$CAP1 * scale.factor

oral_asv_ordistep.vectors$c2_end <-
  oral_asv_ordistep.vectors$CAP2 * scale.factor

oral_asv_ordistep.vectors$c1_begin <- 0
oral_asv_ordistep.vectors$c2_begin <- 0
oral_asv_ordistep.vectors$name <- rownames(oral_asv_ordistep.vectors)
oral_asv_ordistep.vectors$text_x <- oral_asv_ordistep.vectors$c1_end + c(0,0,0)
oral_asv_ordistep.vectors$text_y <- oral_asv_ordistep.vectors$c2_end + c(-.2,.5,-.2)

#Now try to plot this mess

pdf("figs/oral_asv_ordiplot.pdf")

oral_asv_ordistep.biplot <- ggplot(oral_asv_ordistep_biplot.df,
                               aes(x = CAP1,
                                   y = CAP2,
                                   fill = factor(polyp)))

oral_asv_ordistep.biplot +
  geom_point(shape = 21, size = 4, alpha = .7)+
  scale_fill_brewer("Polyp Former",
                    type = "qual",
                    palette = 2,
                    direction = 1
  )+
  geom_segment(data = oral_asv_ordistep.vectors,size =1,
               aes(x = c1_begin,
                   y = c2_begin,
                   xend = c1_end,
                   yend = c2_end),
               arrow = arrow(length = unit(0.02, "npc")),
               inherit.aes = FALSE)+
  annotate("text", size = 6, fontface = "bold",
           x = oral_asv_ordistep.vectors$text_x,
           y = oral_asv_ordistep.vectors$text_y,
           label = oral_asv_ordistep.vectors[,"name"])+
  theme(text = element_text(size=18, colour = "black"),
        panel.grid.major = element_line(color = "grey94"),
        panel.grid.minor = element_line(color = "grey94"),
        panel.background = element_blank(),
        axis.text        = element_text(colour = "black"),
        axis.line        = element_blank(),
        legend.key       = element_rect(fill=NA),
        legend.position  = "top",
        panel.border     = element_rect(colour = "black", fill=NA, size=1)
  )

dev.off()

# ANALYSIS: MUCOSAL ASV ENVFIT ------------------------------------------

mucosal_asv_ordistep.scores <- scores(mucosal_capscale_asv.capscale_full)

mucosal_asv_ordistep.fit <- envfit(mucosal_asv_capscale_full.ordistep ~
                                 factor(Bistol) +
                                 age_bin +
                                 factor(Gend) +
                                 BMI +
                                 Polyp_N +
                                 Adenoma +
                                 RedMeat +
                                 ProcMeat +
                                 Vegi +
                                 Fruit +
                                 Grain +
                                 Ferment +
                                 factor(RegActiv) +
                                 EtOHN +
                                 factor(GERD) +
                                 factor(Cancer) +
                                 factor(AI) +
                                 factor(Diabetes) +
                                 factor(GI) +
                                 factor(M_ASA) +
                                 factor(M_NSAID) +
                                 factor(M_VitD) +
                                 factor(M_VitE) +
                                 factor(M_Ca) +
                                 factor(M_Met) +
                                 factor(M_HRT) +
                                 factor(M_PB) +
                                 factor(M_ETC),
                               mucosal_metadata.df,
                               perm=999,
                               display="lc")


#to get arrows scores(mucosal_ordistep.fit, "bp")
#then find scaling factor vegan:::ordiArrowMul(mucosal_ordistep.fit)
#should be able to figure out rise over run then
#note the scaling determines the length and is based off r2 of the
#fit so could come up with a sensible scaling factor so that we could
#use better scaling tools

#build object

mucosal_asv_ordistep_biplot.df <-
  as.data.frame(mucosal_asv_ordistep.scores$sites)

mucosal_asv_ordistep_biplot.df$polyp <-
  as.numeric((mucosal_metadata.df$Polyp_N) > 0)

#get min and max for plot dimension and vector scaling
min.cap1 <- min(mucosal_asv_ordistep_biplot.df$CAP1)
max.cap1 <- max(mucosal_asv_ordistep_biplot.df$CAP1)
min.cap2 <- min(mucosal_asv_ordistep_biplot.df$CAP2)
max.cap2 <- max(mucosal_asv_ordistep_biplot.df$CAP2)

scale.factor <- max(abs(c(min.cap1,min.cap2,max.cap1,max.cap2)))
scale.factor <- scale.factor + sqrt(scale.factor)


#make a df of vector information
mucosal_asv_ordistep.vectors <-
  as.data.frame(scores(mucosal_asv_ordistep.fit, "vectors"))

#add fit stats
mucosal_asv_ordistep.vectors$r_val <-
  mucosal_asv_ordistep.fit$vectors$r

mucosal_asv_ordistep.vectors$p_val <-
  mucosal_asv_ordistep.fit$vectors$pvals

#add endings
mucosal_asv_ordistep.vectors$c1_end <-
  mucosal_asv_ordistep.vectors$CAP1 * scale.factor

mucosal_asv_ordistep.vectors$c2_end <-
  mucosal_asv_ordistep.vectors$CAP2 * scale.factor

mucosal_asv_ordistep.vectors$c1_begin <- 0
mucosal_asv_ordistep.vectors$c2_begin <- 0
mucosal_asv_ordistep.vectors$name <- rownames(mucosal_asv_ordistep.vectors)
mucosal_asv_ordistep.vectors$text_x <- mucosal_asv_ordistep.vectors$c1_end + c(-.25,0,-.5,-.1,.7,.4,.4,0,0,.4)
mucosal_asv_ordistep.vectors$text_y <- mucosal_asv_ordistep.vectors$c2_end + c(0,.2,-.1,-.1,0,0,0,.2,-.05,-.1)

#Now try to plot this mess

pdf("figs/mucosal_asv_ordiplot.pdf")
mucosal_asv_ordistep.biplot <- ggplot(mucosal_asv_ordistep_biplot.df,
                                  aes(x = CAP1,
                                      y = CAP2,
                                      fill = factor(polyp)))

mucosal_asv_ordistep.biplot +
  geom_point(shape = 21, size = 4, alpha = .7)+
  scale_fill_brewer("Polyp Former",
                    type = "qual",
                    palette = 2,
                    direction = 1
  )+
  geom_segment(data = mucosal_asv_ordistep.vectors,size =1,
               aes(x = c1_begin,
                   y = c2_begin,
                   xend = c1_end,
                   yend = c2_end),
               arrow = arrow(length = unit(0.02, "npc")),
               inherit.aes = FALSE)+
  annotate("text", size = 6, fontface = "bold",
           x = mucosal_asv_ordistep.vectors$text_x,
           y = mucosal_asv_ordistep.vectors$text_y,
           label = mucosal_asv_ordistep.vectors[,"name"])+
  theme(text = element_text(size=18, colour = "black"),
        panel.grid.major = element_line(color = "grey94"),
        panel.grid.minor = element_line(color = "grey94"),
        panel.background = element_blank(),
        axis.text        = element_text(colour = "black"),
        axis.line        = element_blank(),
        legend.key       = element_rect(fill=NA),
        legend.position  = "top",
        panel.border     = element_rect(colour = "black", fill=NA, size=1)
  )

dev.off()


# ANALYSIS: MAKE MEAN MUCOSAL DATAFRAMES ------------------------------------

# Because there  are multiple mucosal samples for each patient we need to first
# average the values for all samples. While there exist more complex ways of
# doing this we will try to keep things simple here.
mucosal_metadata.df$id <- sapply(rownames(mucosal_metadata.df), FUN = function(x){strsplit(x, "-")[[1]][5]})
mucosal_metadata.df$file_name <- rownames(mucosal_metadata.df)

shared.ids <- rownames(mucosal_asv_capscale.df)
shared.ids <- sapply(shared.ids, FUN = function(x){strsplit(x, "-")[[1]][5]})
names(shared.ids) <- NULL
shared.ids <- unique(shared.ids)

#ASV
df.nrow <- length(shared.ids)
df.ncol <- ncol(mucosal_asv_capscale.df)

mucosal_mean_asv.df <- matrix(nrow = df.nrow, ncol = df.ncol, NA )

rownames(mucosal_mean_asv.df) <- paste0("s_",shared.ids)
colnames(mucosal_mean_asv.df) <- colnames(mucosal_asv_capscale.df)

for (i in shared.ids) {
  ids <-
    unlist(subset(mucosal_metadata.df,
                  subset = id == i,
                  select = "file_name"))

  temp.df <-
    mucosal_asv_capscale.df[which(rownames(mucosal_asv_capscale.df) %in% ids), ]

  cmeans <- colMeans(temp.df)
  rid <- paste0("s_", i)
  mucosal_mean_asv.df[rid,] <- cmeans

}


#GENUS

df.nrow <- length(shared.ids)
df.ncol <- ncol(mucosal_genus_capscale.df)

mucosal_mean_genus.df <- matrix(nrow = df.nrow, ncol = df.ncol, NA )

rownames(mucosal_mean_genus.df) <- paste0("s_",shared.ids)
colnames(mucosal_mean_genus.df) <- colnames(mucosal_genus_capscale.df)

for (i in shared.ids) {
  ids <-
    unlist(subset(mucosal_metadata.df,
                  subset = id == i,
                  select = "file_name"))

  temp.df <-
    mucosal_genus_capscale.df[which(rownames(mucosal_genus_capscale.df) %in% ids), ]

  cmeans <- colMeans(temp.df)
  rid <- paste0("s_", i)
  mucosal_mean_genus.df[rid,] <- cmeans

}

#metadata
mucosal_mean_metadata.df <- mucosal_metadata.df
mucosal_mean_metadata.df$nid <- paste0("s_",mucosal_mean_metadata.df$id)

#get lists of duplicate and unique indices
id_list <- NULL
index.list <- NULL
for(i in 1:nrow(mucosal_mean_metadata.df)){
  if(mucosal_mean_metadata.df[i,"nid"] %in% id_list){
    index.list <- c(index.list,i)
  }else{
    id_list <- c(id_list, mucosal_mean_metadata.df[i,"nid"])
  }
}

#get rid of duplicate indices

mucosal_mean_metadata.df <- mucosal_mean_metadata.df[-index.list,]
rownames(mucosal_mean_metadata.df) <- mucosal_mean_metadata.df$nid

#double check the order of the metadata and data tables are the same
all(rownames(mucosal_mean_asv.df) == rownames(mucosal_mean_metadata.df))
all(rownames(mucosal_mean_genus.df) == rownames(mucosal_mean_metadata.df))

# ANALYSIS: MEAN MUCOSAL ASV ORDISTEP ------------------------------------------------

#filter
mucosal_mean_asv_capscale.df <- mucosal_mean_asv.df[which(rownames(mucosal_mean_asv.df) %in% rownames(mucosal_mean_metadata.df)),]

set.seed(731)
mucosal_mean_capscale_asv.capscale_full <- capscale(mucosal_mean_asv_capscale.df ~
                                                 factor(Bistol) +
                                                 age_bin +
                                                 factor(Gend) +
                                                 BMI +
                                                 Polyp_N +
                                                 Adenoma +
                                                 RedMeat +
                                                 ProcMeat +
                                                 Vegi +
                                                 Fruit +
                                                 Grain +
                                                 Ferment +
                                                 factor(RegActiv) +
                                                 EtOHN +
                                                 factor(GERD) +
                                                 factor(Cancer) +
                                                 factor(AI) +
                                                 factor(Diabetes) +
                                                 factor(GI) +
                                                 factor(M_ASA) +
                                                 factor(M_NSAID) +
                                                 factor(M_VitD) +
                                                 factor(M_VitE) +
                                                 factor(M_Ca) +
                                                 factor(M_Met) +
                                                 factor(M_HRT) +
                                                 factor(M_PB) +
                                                 factor(M_ETC)
                                               ,
                                               data = mucosal_mean_metadata.df,
                                               na.action = na.omit)

set.seed(731)
mucosal_mean_asv_capscale_full.ordistep <- ordistep(mucosal_mean_capscale_asv.capscale_full,
                                               na.action = na.omit,
                                               direction = "both",
                                               trace = F)

#get sig
set.seed(731)
anova(mucosal_mean_asv_capscale_full.ordistep) # P = 0.001

set.seed(731)
mucosal_mean_asv_capscale_full_ordistep.aov <-
  anova(mucosal_mean_asv_capscale_full.ordistep, by = "terms")

# Df Variance      F Pr(>F)
# Adenoma           1    46.38 2.1340  0.001 ***
# ProcMeat          1    27.77 1.2778  0.045 *
# Vegi              1    25.86 1.1902  0.099 .
# Fruit             1    37.92 1.7449  0.003 **
# Ferment           1    28.72 1.3217  0.033 *
# factor(GERD)      1    37.19 1.7114  0.003 **
# factor(Diabetes)  3    91.34 1.4010  0.005 **
# factor(GI)        1    27.64 1.2720  0.060 .
# factor(M_NSAID)   1    27.74 1.2763  0.052 .
# factor(M_PB)      1    27.73 1.2760  0.060 .
# Residual         77  1673.33

RsquareAdj(mucosal_mean_asv_capscale_full.ordistep) # adjusted R2 = 0.06

# ANALYSIS: MEAN MUCOSAL GENUS ORDISTEP ------------------------------------------------

#filter
mucosal_mean_genus_capscale.df <- mucosal_mean_genus.df[which(rownames(mucosal_mean_genus.df) %in% rownames(mucosal_mean_metadata.df)),]

set.seed(731)
mucosal_mean_capscale_genus.capscale_full <- capscale(mucosal_mean_genus_capscale.df ~
                                                        factor(Bistol) +
                                                        age_bin +
                                                        factor(Gend) +
                                                        BMI +
                                                        Polyp_N +
                                                        Adenoma +
                                                        RedMeat +
                                                        ProcMeat +
                                                        Vegi +
                                                        Fruit +
                                                        Grain +
                                                        Ferment +
                                                        factor(RegActiv) +
                                                        EtOHN +
                                                        factor(GERD) +
                                                        factor(Cancer) +
                                                        factor(AI) +
                                                        factor(Diabetes) +
                                                        factor(GI) +
                                                        factor(M_ASA) +
                                                        factor(M_NSAID) +
                                                        factor(M_VitD) +
                                                        factor(M_VitE) +
                                                        factor(M_Ca) +
                                                        factor(M_Met) +
                                                        factor(M_HRT) +
                                                        factor(M_PB) +
                                                        factor(M_ETC)
                                                      ,
                                                      data = mucosal_mean_metadata.df,
                                                      na.action = na.omit)

set.seed(731)
mucosal_mean_genus_capscale_full.ordistep <- ordistep(mucosal_mean_capscale_genus.capscale_full,
                                                      na.action = na.omit,
                                                      direction = "both",
                                                      trace = F)

#get sig
set.seed(731)
anova(mucosal_mean_genus_capscale_full.ordistep) # P = 0.001

set.seed(731)
mucosal_mean_genus_capscale_full_ordistep.aov <-
  anova(mucosal_mean_genus_capscale_full.ordistep, by = "terms")

# Df Variance      F Pr(>F)
# age_bin           3    27.28 1.4087  0.013 *
# Adenoma           1    15.76 2.4413  0.005 **
# Fruit             1    12.22 1.8930  0.007 **
# Grain             1     8.00 1.2400  0.154
# Ferment           1     9.47 1.4674  0.062 .
# EtOHN             1     7.91 1.2258  0.147
# factor(Diabetes)  3    38.42 1.9837  0.002 **
# factor(M_VitD)    1     9.39 1.4547  0.046 *
# factor(M_HRT)     1    11.28 1.7474  0.017 *
# Residual         76   490.60

RsquareAdj(mucosal_mean_genus_capscale_full.ordistep) # adjusted R2 = 0.09

# ANALYSIS: MEAN MUCOSAL ASV ENVFIT ------------------------------------------

mucosal_mean_asv_ordistep.scores <- scores(mucosal_mean_capscale_asv.capscale_full)

mucosal_mean_asv_ordistep.fit <- envfit(mucosal_mean_asv_capscale_full.ordistep ~
                                          Adenoma +
                                          ProcMeat +
                                          Vegi +
                                          Fruit +
                                          Ferment +
                                          factor(GERD) +
                                          factor(Diabetes) +
                                          factor(GI) +
                                          factor(M_NSAID) +
                                          factor(M_PB),
                                        mucosal_mean_metadata.df,
                                        perm=999,
                                        display="lc")


#to get arrows scores(mucosal_ordistep.fit, "bp")
#then find scaling factor vegan:::ordiArrowMul(mucosal_ordistep.fit)
#should be able to figure out rise over run then
#note the scaling determines the length and is based off r2 of the
#fit so could come up with a sensible scaling factor so that we could
#use better scaling tools

#build object

mucosal_mean_asv_ordistep_biplot.df <-
  as.data.frame(mucosal_mean_asv_ordistep.scores$sites)

mucosal_mean_asv_ordistep_biplot.df$polyp <-
  as.numeric((mucosal_mean_metadata.df$Polyp_N) > 0)

#get min and max for plot dimension and vector scaling
min.cap1 <- min(mucosal_mean_asv_ordistep_biplot.df$CAP1)
max.cap1 <- max(mucosal_mean_asv_ordistep_biplot.df$CAP1)
min.cap2 <- min(mucosal_mean_asv_ordistep_biplot.df$CAP2)
max.cap2 <- max(mucosal_mean_asv_ordistep_biplot.df$CAP2)

scale.factor <- max(abs(c(min.cap1,min.cap2,max.cap1,max.cap2)))
scale.factor <- scale.factor + sqrt(scale.factor)


#make a df of vector information
mucosal_mean_asv_ordistep.vectors <-
  as.data.frame(scores(mucosal_mean_asv_ordistep.fit, "vectors"))

#add fit stats
mucosal_mean_asv_ordistep.vectors$r_val <-
  mucosal_mean_asv_ordistep.fit$vectors$r

mucosal_mean_asv_ordistep.vectors$p_val <-
  mucosal_mean_asv_ordistep.fit$vectors$pvals

#add endings
mucosal_mean_asv_ordistep.vectors$c1_end <-
  mucosal_mean_asv_ordistep.vectors$CAP1 * scale.factor

mucosal_mean_asv_ordistep.vectors$c2_end <-
  mucosal_mean_asv_ordistep.vectors$CAP2 * scale.factor

mucosal_mean_asv_ordistep.vectors$c1_begin <- 0
mucosal_mean_asv_ordistep.vectors$c2_begin <- 0
mucosal_mean_asv_ordistep.vectors$name <- rownames(mucosal_mean_asv_ordistep.vectors)
mucosal_mean_asv_ordistep.vectors$text_x <- mucosal_mean_asv_ordistep.vectors$c1_end + c(-.25,0,0,.3,.7)
mucosal_mean_asv_ordistep.vectors$text_y <- mucosal_mean_asv_ordistep.vectors$c2_end + c(-.5,-.5,-.3,-.5,.5)

#Now try to plot this mess

pdf("figs/mucosal_mean_asv_ordiplot.pdf")
mucosal_mean_asv_ordistep.biplot <- ggplot(mucosal_mean_asv_ordistep_biplot.df,
                                           aes(x = CAP1,
                                               y = CAP2,
                                               fill = factor(polyp)))

mucosal_mean_asv_ordistep.biplot +
  geom_point(shape = 21, size = 4, alpha = .7)+
  scale_fill_brewer("Polyp Former",
                    type = "qual",
                    palette = 2,
                    direction = 1
  )+
  geom_segment(data = mucosal_mean_asv_ordistep.vectors,size =1,
               aes(x = c1_begin,
                   y = c2_begin,
                   xend = c1_end,
                   yend = c2_end),
               arrow = arrow(length = unit(0.02, "npc")),
               inherit.aes = FALSE)+
  annotate("text", size = 6, fontface = "bold",
           x = mucosal_mean_asv_ordistep.vectors$text_x,
           y = mucosal_mean_asv_ordistep.vectors$text_y,
           label = mucosal_mean_asv_ordistep.vectors[,"name"])+
  theme(text = element_text(size=18, colour = "black"),
        panel.grid.major = element_line(color = "grey94"),
        panel.grid.minor = element_line(color = "grey94"),
        panel.background = element_blank(),
        axis.text        = element_text(colour = "black"),
        axis.line        = element_blank(),
        legend.key       = element_rect(fill=NA),
        legend.position  = "top",
        panel.border     = element_rect(colour = "black", fill=NA, size=1)
  )

dev.off()


# ANALYSIS: MEAN MUCOSAL GENUS ENVFIT -------------------------------------

mucosal_mean_genus_ordistep.scores <- scores(mucosal_mean_capscale_genus.capscale_full)

mucosal_mean_genus_ordistep.fit <- envfit(mucosal_mean_genus_capscale_full.ordistep ~
                                            age_bin +
                                            Adenoma +
                                            Fruit +
                                            Grain +
                                            Ferment +
                                            EtOHN +
                                            factor(Diabetes) +
                                            factor(M_VitD) +
                                            factor(M_HRT),
                                          mucosal_mean_metadata.df,
                                          perm=999,
                                          display="lc")


#to get arrows scores(mucosal_ordistep.fit, "bp")
#then find scaling factor vegan:::ordiArrowMul(mucosal_ordistep.fit)
#should be able to figure out rise over run then
#note the scaling determines the length and is based off r2 of the
#fit so could come up with a sensible scaling factor so that we could
#use better scaling tools

#build object

mucosal_mean_genus_ordistep_biplot.df <-
  as.data.frame(mucosal_mean_genus_ordistep.scores$sites)

mucosal_mean_genus_ordistep_biplot.df$polyp <-
  as.numeric((mucosal_mean_metadata.df$Polyp_N) > 0)

#get min and max for plot dimension and vector scaling
min.cap1 <- min(mucosal_mean_genus_ordistep_biplot.df$CAP1)
max.cap1 <- max(mucosal_mean_genus_ordistep_biplot.df$CAP1)
min.cap2 <- min(mucosal_mean_genus_ordistep_biplot.df$CAP2)
max.cap2 <- max(mucosal_mean_genus_ordistep_biplot.df$CAP2)

scale.factor <- max(abs(c(min.cap1,min.cap2,max.cap1,max.cap2)))
scale.factor <- scale.factor + sqrt(scale.factor)


#make a df of vector information
mucosal_mean_genus_ordistep.vectors <-
  as.data.frame(scores(mucosal_mean_genus_ordistep.fit, "vectors"))

#add fit stats
mucosal_mean_genus_ordistep.vectors$r_val <-
  mucosal_mean_genus_ordistep.fit$vectors$r

mucosal_mean_genus_ordistep.vectors$p_val <-
  mucosal_mean_genus_ordistep.fit$vectors$pvals

#add endings
mucosal_mean_genus_ordistep.vectors$c1_end <-
  mucosal_mean_genus_ordistep.vectors$CAP1 * scale.factor

mucosal_mean_genus_ordistep.vectors$c2_end <-
  mucosal_mean_genus_ordistep.vectors$CAP2 * scale.factor

mucosal_mean_genus_ordistep.vectors$c1_begin <- 0
mucosal_mean_genus_ordistep.vectors$c2_begin <- 0
mucosal_mean_genus_ordistep.vectors$name <- rownames(mucosal_mean_genus_ordistep.vectors)
mucosal_mean_genus_ordistep.vectors$text_x <- mucosal_mean_genus_ordistep.vectors$c1_end + c(-.25,0,-.5,-.1,.7)
mucosal_mean_genus_ordistep.vectors$text_y <- mucosal_mean_genus_ordistep.vectors$c2_end + c(.5,-.2,.2,-.2,-.2)

#Now try to plot this mess

pdf("figs/mucosal_mean_genus_ordiplot.pdf")
mucosal_mean_genus_ordistep.biplot <- ggplot(mucosal_mean_genus_ordistep_biplot.df,
                                             aes(x = CAP1,
                                                 y = CAP2,
                                              #   fill = factor(polyp)
                                                 )
                                             )

mucosal_mean_genus_ordistep.biplot +
  geom_point(shape = 21, size = 4, alpha = .7, fill = "steelblue")+
  scale_fill_brewer("Polyp Former",
                    type = "qual",
                    palette = 2,
                    direction = 1
  )+
  geom_segment(data = mucosal_mean_genus_ordistep.vectors,size =1,
               aes(x = c1_begin,
                   y = c2_begin,
                   xend = c1_end,
                   yend = c2_end),
               arrow = arrow(length = unit(0.02, "npc")),
               inherit.aes = FALSE)+
  annotate("text", size = 6, fontface = "bold",
           x = mucosal_mean_genus_ordistep.vectors$text_x,
           y = mucosal_mean_genus_ordistep.vectors$text_y,
           label = mucosal_mean_genus_ordistep.vectors[,"name"])+
  theme(text = element_text(size=18, colour = "black"),
        panel.grid.major = element_line(color = "grey94"),
        panel.grid.minor = element_line(color = "grey94"),
        panel.background = element_blank(),
        axis.text        = element_text(colour = "black"),
        axis.line        = element_blank(),
        legend.key       = element_rect(fill=NA),
        legend.position  = "top",
        panel.border     = element_rect(colour = "black", fill=NA, size=1)
  )

dev.off()

# ANALYSIS: ORDISTEP and ENVFIT FDR ------------------------------------------------

ordistep.fdr <- data.frame(  pval = na.omit(c(fecal_genus_capscale_full_ordistep.aov$`Pr(>F)`,
                                            oral_genus_capscale_full_ordistep.aov$`Pr(>F)`,
                                            mucosal_mean_genus_capscale_full_ordistep.aov$`Pr(>F)`) ),
  fdr = p.adjust(na.omit(c(fecal_genus_capscale_full_ordistep.aov$`Pr(>F)`,
                           oral_genus_capscale_full_ordistep.aov$`Pr(>F)`,
                           mucosal_mean_genus_capscale_full_ordistep.aov$`Pr(>F)`) ),
                 method = "fdr"))

envfit.fdr <- data.frame(names = names(c(fecal_ordistep.fit$vectors$pvals,
                                         fecal_ordistep.fit$factors$pvals,
                                         oral_ordistep.fit$vectors$pvals,
                                         oral_ordistep.fit$factors$pvals,
                                         mucosal_mean_genus_ordistep.fit$vectors$pvals,
                                         mucosal_mean_genus_ordistep.fit$factors$pvals
                          )),
                          pval = c(fecal_ordistep.fit$vectors$pvals,
                                  fecal_ordistep.fit$factors$pvals,
                                  oral_ordistep.fit$vectors$pvals,
                                  oral_ordistep.fit$factors$pvals,
                                  mucosal_mean_genus_ordistep.fit$vectors$pvals,
                                  mucosal_mean_genus_ordistep.fit$factors$pvals
                                  ),
                         fdr = p.adjust(c(fecal_ordistep.fit$vectors$pvals,
                                 fecal_ordistep.fit$factors$pvals,
                                 oral_ordistep.fit$vectors$pvals,
                                 oral_ordistep.fit$factors$pvals,
                                 mucosal_mean_genus_ordistep.fit$vectors$pvals,
                                 mucosal_mean_genus_ordistep.fit$factors$pvals),
                                method = "fdr")
          )

# ANALYSIS: BOXPLOT OF MUCOSAL ADENOMA  --------------------------------------

#get ordination results
adenoma_box.df <- mucosal_mean_genus_ordistep_biplot.df

#add number of adenomas
adenoma_box.df$adenoma <- mucosal_mean_metadata.df$Adenoma

#Select only the adenoma positives
adenoma_box.df <- adenoma_box.df[which(adenoma_box.df$polyp == 1),]


#lower right CAP1 gt 1 and cap2 lt 0

LR <- adenoma_box.df[which(adenoma_box.df$CAP1 >  0 & adenoma_box.df$CAP2 < 0), "adenoma"]
UL <- adenoma_box.df[which(adenoma_box.df$CAP1 < 0 & adenoma_box.df$CAP2 > 0), "adenoma"]

#visualize and test
boxplot(LR, UL)
wilcox.test(LR, UL)


LR <- rownames(adenoma_box.df[which(adenoma_box.df$CAP1 >  0 & adenoma_box.df$CAP2 < 0), ])
UL <- rownames(adenoma_box.df[which(adenoma_box.df$CAP1 < 0 & adenoma_box.df$CAP2 > 0), ])


adenoma_box.melt <- adenoma_box.df
adenoma_box.melt$location <- 0


adenoma_box.melt <- adenoma_box.melt[which(rownames(adenoma_box.melt) %in% c(LR,UL)),]

adenoma_box.melt$location <- ifelse(rownames(adenoma_box.melt) %in% LR, yes = "LR", no = "UL")


adenoma_box.plot <- ggplot(adenoma_box.melt, aes(x = location,
                                                 y = adenoma,
                                                 fill = location))

adenoma_box.plot +
  geom_boxplot() +
  ylab("Adenoma Burden")+
  xlab("")+
  scale_fill_brewer(c("Quadrant"))+
  stat_compare_means(label = "p.signif", comparisons = list(c("LR", "UL")),
                     size = 6, face = "bold",
                     tip.length = 0.01)+
  theme(aspect.ratio = 1.7,
        text = element_text(size=18, colour = "black"),
        panel.grid.major = element_line(color = "grey94"),
        panel.grid.minor = element_line(color = "grey94"),
        panel.background = element_blank(),
        axis.text        = element_text(colour = "black"),
        axis.line        = element_blank(),
        legend.key       = element_rect(fill=NA),
        legend.position  = "top",
        panel.border     = element_rect(colour = "black", fill=NA, size=1)
  )


# ORGANIZE PLOT -----------------------------------------------------------

fig1a <-  ggplot(oral_ordistep_biplot.df,
                                        aes(x = CAP1,
                                            y = CAP2,
                                        )) +
  geom_point(shape = 21, size = 4, alpha = .7, fill = "#7840A1")+
  geom_segment(data = oral_ordistep.vectors,size =1,
               aes(x = c1_begin,
                   y = c2_begin,
                   xend = c1_end,
                   yend = c2_end),
               arrow = arrow(length = unit(0.02, "npc")),
               inherit.aes = FALSE)+
  annotate("text", size = 6, fontface = "bold",
           x = oral_ordistep.vectors$text_x,
           y = oral_ordistep.vectors$text_y,
           label = oral_ordistep.vectors[,"name"])+
  theme(text = element_text(size=18, colour = "black"),
        panel.grid.major = element_line(color = "grey94"),
        panel.grid.minor = element_line(color = "grey94"),
        panel.background = element_blank(),
        axis.text        = element_text(colour = "black"),
        axis.line        = element_blank(),
        legend.key       = element_rect(fill=NA),
        legend.position  = "top",
        panel.border     = element_rect(colour = "black", fill=NA, size=1)
  )

fig1b <- ggplot(fecal_ordistep_biplot.df,
                aes(x = CAP1,
                    y = CAP2,
                )) +
  geom_point(shape = 21, size = 4, alpha = .7, fill = "#568F55")+
  geom_segment(data = fecal_ordistep.vectors,size =1,
               aes(x = c1_begin,
                   y = c2_begin,
                   xend = c1_end,
                   yend = c2_end),
               arrow = arrow(length = unit(0.02, "npc")),
               inherit.aes = FALSE)+
  annotate("text", size = 6, fontface = "bold",
           x = fecal_ordistep.vectors$text_x,
           y = fecal_ordistep.vectors$text_y,
           label = fecal_ordistep.vectors[,"name"])+
  theme(text = element_text(size=18, colour = "black"),
        panel.grid.major = element_line(color = "grey94"),
        panel.grid.minor = element_line(color = "grey94"),
        panel.background = element_blank(),
        axis.text        = element_text(colour = "black"),
        axis.line        = element_blank(),
        legend.key       = element_rect(fill=NA),
        legend.position  = "top",
        panel.border     = element_rect(colour = "black", fill=NA, size=1)
  )

fig1c <- ggplot(mucosal_mean_genus_ordistep_biplot.df,
                aes(x = CAP1,
                    y = CAP2,
                )) +
  geom_point(shape = 21, size = 4, alpha = .7, fill = "steelblue")+
  geom_segment(data = mucosal_mean_genus_ordistep.vectors,size =1,
               aes(x = c1_begin,
                   y = c2_begin,
                   xend = c1_end,
                   yend = c2_end),
               arrow = arrow(length = unit(0.02, "npc")),
               inherit.aes = FALSE)+
  annotate("text", size = 6, fontface = "bold",
           x = mucosal_mean_genus_ordistep.vectors$text_x,
           y = mucosal_mean_genus_ordistep.vectors$text_y,
           label = mucosal_mean_genus_ordistep.vectors[,"name"])+
  theme(text = element_text(size=18, colour = "black"),
        panel.grid.major = element_line(color = "grey94"),
        panel.grid.minor = element_line(color = "grey94"),
        panel.background = element_blank(),
        axis.text        = element_text(colour = "black"),
        axis.line        = element_blank(),
        legend.key       = element_rect(fill=NA),
        legend.position  = "top",
        panel.border     = element_rect(colour = "black", fill=NA, size=1)
  )


fig1d <- adenoma_box.plot +
  geom_boxplot() +
  ylab("Adenoma Burden")+
  xlab("")+
  scale_fill_brewer("")+
  stat_compare_means(label = "p.signif",
                     comparisons = list(c("LR", "UL")),
                     size = 6,
                     face = "bold",
                     vjust = 0,
                     tip.length = 0,
                     label.y = 18.5 )+
  theme(aspect.ratio = 1.7,
        text = element_text(size=18, colour = "black"),
        panel.grid.major = element_line(color = "grey94"),
        panel.grid.minor = element_line(color = "grey94"),
        panel.background = element_blank(),
        axis.text        = element_text(colour = "black"),
        axis.line        = element_blank(),
        legend.key       = element_rect(fill=NA),
        legend.position  = "none",
        panel.border     = element_rect(colour = "black", fill=NA, size=1)
  )+
  ylim(c(0,20))

#pdf("figs/figure1d.pdf", width = 3, height = 5)
fig1d
#dev.off()

#pdf("figs/figure1.pdf", width = 14, height = 6)
cowplot::plot_grid(fig1a, fig1b, fig1c, fig1d,ncol = 3, labels = c("A","B","C","D"),
          label_size = 18)
#dev.off()



