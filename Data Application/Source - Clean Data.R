########################################
### Total.Data: Load Viable Datasets ###
########################################

# data.1 <- Available Upon Request
# data.2 <- Available Upon Request
# data.3 <- Available Upon Request



###############################
### Data.1: Clean Variables ###
###############################

# Subject ID      ---> Check
# Treatx: Numeric ---> Check
# OHD:    Numeric ---> Check
# OH2D:   Numeric ---> Check
# PTH:    Numeric ---> Check
# Ca:     Numeric ---> Check
# P:      Numeric ---> Check
# VitD:   Factor  ---> Fixed Below
data.1$vitD_Cat <- ifelse(is.na(data.1$vitD), NA,
                          ifelse(data.1$vitD %in% c("1F/1F"), 1,
                                 ifelse(data.1$vitD %in% c("1F/2", "1S/1F", "1S/2", "2/2"), 2,
                                        ifelse(data.1$vitD %in% c("1S/1S"), 3, "Error"))))
data.1$vitD_Cat <- factor(data.1$vitD_Cat)
class(data.1$vitD_Cat)

data.1$vitD <- factor(data.1$vitD, levels=c("1F/1F", "1F/2", "1S/1F", "1S/1S", "1S/2", "2/2"), labels=1:6)
class(data.1$vitD)

# GestAge: Numeric ---> Check
# Feed:    Drop    ---> Fixed Below
data.1 <- data.1[,-which(colnames(data.1)=="feed")]

# Formula: Integer ---> Check
# Race:    Integer ---> Check
# Momage:  Integer ---> Check
# Mombmi:  Integer ---> Check
# Antacid: Integer ---> Check
# medOHDcat2: Drop ---> Fixed Below
data.1 <- data.1[,-which(colnames(data.1)=="medOHDcat2")]

# momOHD_12: Integer ---> Check
# momOHD_28: Integer ---> Check
# momOHD_36: Integer ---> Check
# momOH2Ds (12, 28, 36): Drop ---> Fixed Below
data.1 <- data.1[,-which(colnames(data.1) %in% c("momOH2D_12", "momOH2D_28", "momOH2D_36"))]

# momPTH_12: Integer ---> Check
# momPTH_28: Integer ---> Check
# momPTH_36: Integer ---> Check
# momCa_12: Integer ---> Check
# momCa_28: Integer ---> Check
# momCa_36: Integer ---> Check
# momP_12: Integer ---> Check
# momP_28: Integer ---> Check
# momP_36: Integer ---> Check
# Drop Remaining (hyp, edit, statuses, and extents)
drop.vars <- which(colnames(data.1) %in% c("hyp", "edit", "eh_status", "eh_status1", "eh_status2", "eh_extent", "eh_extent1", "eh_extent2",
                                           "op_status", "op_status1", "op_status2", "op_extent", "op_extent1", "op_extent2",
                                           "peb_status", "peb_status1", "peb_status2", "peb_extent", "peb_extent1", "peb_extent2",
                                           "dc_status", "dc_status1", "dc_status2", "dc_extent", "dc_extent1", "dc_extent2",
                                           "dc_treat", "dc_treat1", "dc_treat2"))
data.1 <- data.1[,-drop.vars]



###############################
### Data.2: Clean Variables ###
###############################

### Update Outcome Values as Numeric for Each Outcome ###

## Outcome: Enamel Hypoplasia ##

# Tooth E #
data.2$ehc_slr_1 <- as.numeric(data.2$ehc_slr_1)
data.2$ehm_slr_1 <- as.numeric(data.2$ehm_slr_1)
data.2$ehi_slr_1 <- as.numeric(data.2$ehi_slr_1)

# Tooth F #
data.2$fhc_slr_1 <- as.numeric(data.2$fhc_slr_1)
data.2$fhm_slr_1 <- as.numeric(data.2$fhm_slr_1)
data.2$fhi_slr_1 <- as.numeric(data.2$fhi_slr_1)



## Outcome: Opacity ##

# Tooth E #
data.2$eoc_slr_1 <- as.numeric(data.2$eoc_slr_1)
data.2$eom_slr_1 <- as.numeric(data.2$eom_slr_1)
data.2$eoi_slr_1 <- as.numeric(data.2$eoi_slr_1)

# Tooth F #
data.2$foc_slr_1 <- as.numeric(data.2$foc_slr_1)
data.2$fom_slr_1 <- as.numeric(data.2$fom_slr_1)
data.2$foi_slr_1 <- as.numeric(data.2$foi_slr_1)



## Outcome: Post-Eruptive Breakdown ## 

# Tooth E #
data.2$epc_slr_1 <- as.numeric(data.2$epc_slr_1)
data.2$epm_slr_1 <- as.numeric(data.2$epm_slr_1)
data.2$epi_slr_1 <- as.numeric(data.2$epi_slr_1)

# Tooth F #
data.2$fpc_slr_1 <- as.numeric(data.2$fpc_slr_1)
data.2$fpm_slr_1 <- as.numeric(data.2$fpm_slr_1)
data.2$fpi_slr_1 <- as.numeric(data.2$fpi_slr_1)



## Outcome: Dental Caries ##

# Tooth E #
data.2$edc_slr_1 <- as.numeric(data.2$edc_slr_1)
data.2$edm_slr_1 <- as.numeric(data.2$edm_slr_1)
data.2$edi_slr_1 <- as.numeric(data.2$edi_slr_1)

# Tooth F #
data.2$fdc_slr_1 <- as.numeric(data.2$fdc_slr_1)
data.2$fdm_slr_1 <- as.numeric(data.2$fdm_slr_1)
data.2$fdi_slr_1 <- as.numeric(data.2$fdi_slr_1)



### Create Aggregate Outcomes by Region ###

## Outcome: Enamel Hypoplasia ##

# Cervical #
data.2$hc <- ifelse(rowSums(cbind(data.2$ehc_slr_1, data.2$fhc_slr_1), na.rm=TRUE) > 0, 1, 0)
data.2$hc <- ifelse((is.na(data.2$ehc_slr_1)&is.na(data.2$fhc_slr_1))|(data.2$hc==0&(is.na(data.2$ehc_slr_1)|is.na(data.2$fhc_slr_1))), NA, data.2$hc)

# Middle #
data.2$hm <- ifelse(rowSums(cbind(data.2$ehm_slr_1, data.2$fhm_slr_1), na.rm=TRUE) > 0, 1, 0)
data.2$hm <- ifelse((is.na(data.2$ehm_slr_1)&is.na(data.2$fhm_slr_1))|(data.2$hm==0&(is.na(data.2$ehm_slr_1)|is.na(data.2$fhm_slr_1))), NA, data.2$hm)

# Incisal #
data.2$hi <- ifelse(rowSums(cbind(data.2$ehi_slr_1, data.2$fhi_slr_1), na.rm=TRUE) > 0, 1, 0)
data.2$hi <- ifelse((is.na(data.2$ehi_slr_1)&is.na(data.2$fhi_slr_1))|(data.2$hi==0&(is.na(data.2$ehi_slr_1)|is.na(data.2$fhi_slr_1))), NA, data.2$hi)



## Outcome: Opacity ##

# Cervical #
data.2$oc <- ifelse(rowSums(cbind(data.2$eoc_slr_1, data.2$foc_slr_1), na.rm=TRUE) > 0, 1, 0)
data.2$oc <- ifelse((is.na(data.2$eoc_slr_1)&is.na(data.2$foc_slr_1))|(data.2$oc==0&(is.na(data.2$eoc_slr_1)|is.na(data.2$foc_slr_1))), NA, data.2$oc)

# Middle #
data.2$om <- ifelse(rowSums(cbind(data.2$eom_slr_1, data.2$fom_slr_1), na.rm=TRUE) > 0, 1, 0)
data.2$om <- ifelse((is.na(data.2$eom_slr_1)&is.na(data.2$fom_slr_1))|(data.2$om==0&(is.na(data.2$eom_slr_1)|is.na(data.2$fom_slr_1))), NA, data.2$om)

# Incisal #
data.2$oi <- ifelse(rowSums(cbind(data.2$eoi_slr_1, data.2$foi_slr_1), na.rm=TRUE) > 0, 1, 0)
data.2$oi <- ifelse((is.na(data.2$eoi_slr_1)&is.na(data.2$foi_slr_1))|(data.2$oi==0&(is.na(data.2$eoi_slr_1)|is.na(data.2$foi_slr_1))), NA, data.2$oi)



## Outcome: Post-Eruptive Breakdown ##

# Cervical #
data.2$pc <- ifelse(rowSums(cbind(data.2$epc_slr_1, data.2$fpc_slr_1), na.rm=TRUE) > 0, 1, 0)
data.2$pc <- ifelse((is.na(data.2$epc_slr_1)&is.na(data.2$fpc_slr_1))|(data.2$pc==0&(is.na(data.2$epc_slr_1)|is.na(data.2$fpc_slr_1))), NA, data.2$pc)

# Middle #
data.2$pm <- ifelse(rowSums(cbind(data.2$epm_slr_1, data.2$fpm_slr_1), na.rm=TRUE) > 0, 1, 0)
data.2$pm <- ifelse((is.na(data.2$epm_slr_1)&is.na(data.2$fpm_slr_1))|(data.2$pm==0&(is.na(data.2$epm_slr_1)|is.na(data.2$fpm_slr_1))), NA, data.2$pm)

# Incisal #
data.2$pi <- ifelse(rowSums(cbind(data.2$epi_slr_1, data.2$fpi_slr_1), na.rm=TRUE) > 0, 1, 0)
data.2$pi <- ifelse((is.na(data.2$epi_slr_1)&is.na(data.2$fpi_slr_1))|(data.2$pi==0&(is.na(data.2$epi_slr_1)|is.na(data.2$fpi_slr_1))), NA, data.2$pi)



## Outcome: Dental Caries ##

# Cervical #
data.2$dc <- ifelse(rowSums(cbind(data.2$edc_slr_1, data.2$fdc_slr_1), na.rm=TRUE) > 0, 1, 0)
data.2$dc <- ifelse((is.na(data.2$edc_slr_1)&is.na(data.2$fdc_slr_1))|(data.2$dc==0&(is.na(data.2$edc_slr_1)|is.na(data.2$fdc_slr_1))), NA, data.2$dc)

# Middle #
data.2$dm <- ifelse(rowSums(cbind(data.2$edm_slr_1, data.2$fdm_slr_1), na.rm=TRUE) > 0, 1, 0)
data.2$dm <- ifelse((is.na(data.2$edm_slr_1)&is.na(data.2$fdm_slr_1))|(data.2$dm==0&(is.na(data.2$edm_slr_1)|is.na(data.2$fdm_slr_1))), NA, data.2$dm)

# Incisal #
data.2$di <- ifelse(rowSums(cbind(data.2$edi_slr_1, data.2$fdi_slr_1), na.rm=TRUE) > 0, 1, 0)
data.2$di <- ifelse((is.na(data.2$edi_slr_1)&is.na(data.2$fdi_slr_1))|(data.2$di==0&(is.na(data.2$edi_slr_1)|is.na(data.2$fdi_slr_1))), NA, data.2$di)



### Create Cleaned Datasets for Only Outcomes and Subjects IDs ###

## Outcome: Enamel Hypoplasia ##
keep.vars.eh <- which(colnames(data.2) %in% c("id", "hc", "hm", "hi"))
data.2.eh <- data.2[, keep.vars.eh]

## Outcome: Opacity ##
keep.vars.op <- which(colnames(data.2) %in% c("id", "oc", "om", "oi"))
data.2.op <- data.2[, keep.vars.op]

## Outcome: Post-Eruptive Breakdown ##
keep.vars.peb <- which(colnames(data.2) %in% c("id", "pc", "pm", "pi"))
data.2.peb <- data.2[, keep.vars.peb]

## Outcome: Dental Caries ##
keep.vars.dc <- which(colnames(data.2) %in% c("id", "dc", "dm", "di"))
data.2.dc <- data.2[, keep.vars.dc]



###############################
### Data.3: Clean Variables ###
###############################

## Summary of Variables in Dataset ##

# b_id (Subject ID Identifier) --> Check
# b_fl: Binary Character [Ever Visited Dentist] (18 missing) --> Note: Add to Model (05.10)
# b_dds: Binary Character [Brushes with Fluoride Toothpaste] (18 missing) --> Note: Add to Model (05.10)
# b_fltx: Binary Character [Fluoride Varnish Put on Teeth] (29 missing) --> Add to Model (05.09)
# b_age: Continuous [Child's Age] --> Note: Add to Model (05.09) 
# b_race: Categorical [3 = Black AA, 4 = Hisp, 5 = White Cauc] --> Note: Don't Add to Model (05.09)
# b_sex: Categorical [1 = Female, 2 = Male, 4 = ?] --> Note: Observation coded as "4" needs to be identified // Add to Model (05.10)
# b_sm_ct: Count [Strep Mutans Count, CFUs/ml] (3 missing) --> Note: Add to Model (05.10)
# b_sn: Binary Character [Dentocult 1 = yes, 0 = no] (3 missing) --> Note: Don't Add to Model (05.10)



## Clean Variables to be Merged ##

# b_age: Continuous
data.3$b_age <- as.numeric(data.3$b_age)

# b_fl: Binary (0 = No, 1 = Yes)
data.3$b_fl <- as.numeric(data.3$b_fl)

# b_dds: Binary (0 = No, 1 = Yes)
data.3$b_dds <- as.numeric(data.3$b_dds)

# b_fltx: Binary (0 = No, 1 = Yes)
data.3$b_fltx <- as.numeric(data.3$b_fltx)

# b_sex: 
data.3$b_sex[24] <- 2
data.3$b_sex <- as.numeric(data.3$b_sex) - 1
# Recoded: 0 = Female, 1 = Male

# b_sm_ct:
data.3$b_sm_ct <- as.numeric(data.3$b_sm_ct)



## Keep Only Variables to be Merged ##
keep.vars.data.3 <- which(colnames(data.3) %in% c("b_id", "b_age", "b_fl", "b_dds", "b_fltx", "b_sex", "b_sm_ct"))
data.3.merge <- data.3[, keep.vars.data.3]



#########################################################
### Full Dataset: Merge All Datasets for Each OUtcome ###
#########################################################

### Merge Datasets to Combine All Predictors ###

## Outcome: Enamel Hypoplasia ##

# Merge Datasheets
td.eh <- merge(data.1, data.2.eh, by.x="subjectnumber", by.y="id")
total.data.eh <- merge(td.eh, data.3.merge, by.x="subjectnumber", by.y="b_id")

# Adjust Subject Number
total.data.eh$subjectnumber <- order(total.data.eh$subjectnumber)

# Adjust Design for Treatment
trt.design.eh <- model.matrix(total.data.eh$subjectnumber ~ factor(total.data.eh$treatx))[,2:3]



## Outcome: Opacity ##

# Merge Datasets
td.op <- merge(data.1, data.2.op, by.x="subjectnumber", by.y="id")
total.data.op <- merge(td.op, data.3.merge, by.x="subjectnumber", by.y="b_id")

# Adjust Subject Number
total.data.op$subjectnumber <- order(total.data.op$subjectnumber)

# Adjust Design for Treatment
trt.design.op <- model.matrix(total.data.op$subjectnumber ~ factor(total.data.op$treatx))[,2:3]



## Outcome: Post-Eruptive Breakdown ##

# Merge Datasets
td.peb <- merge(data.1, data.2.peb, by.x="subjectnumber", by.y="id")
total.data.peb <- merge(td.peb, data.3.merge, by.x="subjectnumber", by.y="b_id")

# Adjust Subject Number
total.data.peb$subjectnumber <- order(total.data.peb$subjectnumber)

# Adjust Design for Treatment
trt.design.peb <- model.matrix(total.data.peb$subjectnumber ~ factor(total.data.peb$treatx))[,2:3]



## Outcome: Dental Caries ##

# Merge Datasets
td.dc <- merge(data.1, data.2.dc, by.x="subjectnumber", by.y="id")
total.data.dc <- merge(td.dc, data.3.merge, by.x="subjectnumber", by.y="b_id")

# Adjust Subject Number
total.data.dc$subjectnumber <- order(total.data.dc$subjectnumber)

# Adjust Design for Treatment
trt.design.dc <- model.matrix(total.data.dc$subjectnumber ~ factor(total.data.dc$treatx))[,2:3]



##################################################################
### Remove All Objects Except Cleaned Datasets for Source Code ###
##################################################################

### Remove Objects ###
rm(list=ls()[!ls() %in% c("total.data.dc", "total.data.eh", "total.data.op", "total.data.peb")])