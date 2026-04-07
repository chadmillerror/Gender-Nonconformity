setwd("/Users/chadmiller/OneDrive/Research/Misc or Collaborations/Gender Medical Milestones")
library(readxl)
# one rater's file
d_original_1 <- read_xlsx("Milestones_Completed_Dajae_ST-7.22.25.xlsx")
# one observation is in a fifth column instead of the fourth. The observation in the fourth (but not the third) is missing
# so I assume it belongs in the fourth column
d_original_1[!is.na(d_original_1[,5]),]
d_original_1[!is.na(d_original_1[,5]),4]<- d_original_1[!is.na(d_original_1[,5]),5]
d_original <- d_original_1[,1:4]
# Function combines mistakenly split rows (and fixes split text) if ratings are equivalent. NA otherwise
collapserows <- function(data, row1) {
  row2 <- row1+1
  data.frame(row1, paste(data[row1,2], data[row2,2]), 
    ifelse(data[row1,3]==data[row2, 3], data[row1,3], NA), 
    ifelse(data[row1,4]==data[row2, 4], data[row1,4], NA))
}
# collapsing problematic ID/row numbers that were mistakenly split
problematicrows <- c(118,179, 195, 209, 291, 539, 629, 660, 692, 1141, 1173, 1380, 1831, 1901, 2180, 2360, 2367, 2425, 2488, 2497,
                     2661, 2779, 2849, 2959, 3022)
d_corrected <- data.frame(matrix(sapply(problematicrows, FUN=collapserows, data=d_original), ncol=4, byrow = T))
# ID/row numbers with outdated text that won't be used
outdatedrows <- c(725,726, 1057, 1058)
d_original[sapply(d_original[,1],  function(x) x %in% outdatedrows),3:4] <- NA
colnames(d_corrected) <- colnames(d_original)
# removing problematic rows, adding corrected rows, then sorting by ID Number
d <- rbind(d_original[-c(problematicrows, problematicrows+1),], d_corrected) |> data.frame()
d[,c(1,3,4)] <- sapply(d[,c(1,3,4)], as.numeric)
d$Milestones.Description <- as.character(d$Milestones.Description)
d <- sort_by(d, d$Subject.no.)
rownames(d) <- NULL
# another rater file
o_original <- read_xlsx("Milestones_Completed_Oliver_ST-09.26.25 Updated.xlsx")
o <- o_original[,-5]

# another rater file

c <- read_xlsx("Milestones_Completed_Camila_ST-09.26.25.xlsx")

# another rater file

dn <- read_xlsx("Milestones_Completed_Daneal_ST-09.26.25.xlsx")[,-c(5:6)]
# see tests below for reasoning. Rater coded the same description twice
dn$`Masculinity Rating`[1199] <- NA
dn$`Femininity Rating`[1199] <- NA

# another rater file

j <- read_xlsx("Milestones_Completed_Jon_ST-09.26.25.xlsx")
j$`Femininity Rating`[j$`Femininity Rating`>7 & !is.na(j$`Femininity Rating`)] <- 5

# Combined
data <- data.frame("Milestone_Number"=d$`Subject.no.`,
                   "Description"=c$`Milestones Description`,
                   "R1Femininity"=j$`Femininity Rating`,
                   "R1Masculinity"=j$`Masculinity Rating`,
                   "R2Femininity"=dn$`Femininity Rating`,
                   "R2Masculinity"=dn$`Masculinity Rating`,
                   "R3Femininity"=c$`Femininity Rating`,
                   "R3Masculinity"=c$`Masculinity Rating`,
                   "R4Femininity"=o$`Femininity Rating`,
                   "R4Masculinity"=o$`Masculinity Rating`,
                   "R5Femininity"=d$Femininity.Rating,
                   "R5Masculinity"=d$Masculinity.Rating)

write.csv(data, file = "raterdata.csv", row.names = F)

# Second Dataset
milestonedata <- read.csv("Milestones_dataset_6.17.25.csv")[,1:13]
problematicrows <- c(118,179, 195, 209, 291, 539, 629, 660, 692, 1141, 1173, 1380, 1831, 1901, 2180, 2360, 2367, 2425, 2488, 2497,
                     2661, 2779, 2849, 2959, 3022)
correcteddescriptions <- sapply(problematicrows, function(x) paste(milestonedata[x,]$Milestones.Description, 
                                                                   milestonedata[(x+1),]$Milestones.Description))
milestonedata[problematicrows,]$Milestones.Description <- correcteddescriptions
milestonedata_corrected <- milestonedata[-(problematicrows+1),]

milestonedata_corrected$Milestones.Description[(milestonedata_corrected$Milestones.Description!=data$Description)]
data$Description[(milestonedata_corrected$Milestones.Description!=data$Description)]

write.csv(milestonedata_corrected, file = "milestonedata.csv", row.names = F)

combineddata <- cbind(data, milestonedata_corrected[,-c(1:3)])

write.csv(combineddata, file = "finalcombineddata.csv", row.names = F)

# Data Checking 

sum(d$Subject.no.==o$`Subject no.`)
sum(d$Milestones.Description==o$`Milestones Description`)
data.frame(d$Milestones.Description, o$`Milestones Description`, d$Milestones.Description==o$`Milestones Description`) |> View()
data.frame(d$Milestones.Description, milestonedata_corrected$Milestones.Description, d$Milestones.Description==milestonedata_corrected$Milestones.Description) |> View()
# Discrepancies are close enough 

# DN-1199 wrong
sum(dn$'Milestones Description'==milestonedata_corrected$Milestones.Description)
data.frame(dn$'Milestones Description', milestonedata_corrected$Milestones.Description,
           dn$'Milestones Description'==milestonedata_corrected$Milestones.Description) |> View()

# fine
sum(o$'Milestones Description'==milestonedata_corrected$Milestones.Description)
data.frame(o$'Milestones Description', milestonedata_corrected$Milestones.Description,
           o$'Milestones Description'==milestonedata_corrected$Milestones.Description) |> View()

# fine
sum(j$'Milestones Description'==milestonedata_corrected$Milestones.Description)
data.frame(j$'Milestones Description', milestonedata_corrected$Milestones.Description,
           j$'Milestones Description'==milestonedata_corrected$Milestones.Description) |> View()

# fine
sum(c$'Milestones Description'==milestonedata_corrected$Milestones.Description)
data.frame(c$'Milestones Description', milestonedata_corrected$Milestones.Description,
           c$'Milestones Description'==milestonedata_corrected$Milestones.Description) |> View()

# Cutting Sports Medicine

combineddata_nosports <- subset(combineddata, Specialty != "Sports Medicine")

# Adding Ortho Sports (2) and Pediatric Critical Care (3)
setwd("/Users/chadmiller/OneDrive/Research/Misc or Collaborations/Gender Medical Milestones/new_coding_sheets_coded")
dn2 <- read_xlsx("Milestone Coding Sheet 2 Daneal 26.1.26.xlsx")
dn3 <- read_xlsx("Milestones Coding Sheet 3 Daneal 26.1.26.xlsx")

o2 <- read_xlsx("Milestone Coding Sheet 2 Oliver.xlsx")
o2 <- o2[,-5]
unique(o2$`Milestone Description`) |> length()
o2$`Milestone Description`[duplicated(o2$`Milestone Description`)]
o3 <- read_xlsx("Milestones Coding Sheet 3 Oliver.xlsx")
unique(o3$`Milestone Description`) |> length()

c2 <- read_xlsx("Milestone Coding Sheet 2 Camila.xlsx")
c2 <- c2[,-5]
c3 <- read_xlsx("Milestones Coding Sheet 3 Camila.xlsx")

# undo rater's combination attempt
j23 <- read_xlsx("Milestone Coding Sheet Jan 26 Jon.xlsx")
j2 <- j23[1:170,]
j3 <- j23[-c(1:171),]

# first 2 raters rows and rated descriptions are aligned
sum(o2$`Milestone Description`==dn2$`Milestone Description`)
sum(o3$`Milestone Description`==dn3$`Milestone Description`)
sum(o2$`Subject no.`==dn2$`Subject no.`)
sum(o3$`Subject no.`==dn3$`Subject No.`)

# last 2 raters rows and rated descriptions are aligned
sum(j2$`Milestone Description`==c2$`Milestone Description`)
sum(j3$`Milestone Description`==c3$`Milestone Description`)
sum(j2$`Subject no.`==c2$`Subject no.`)
sum(j3$`Subject no.`==c3$`Subject No.`)

# 4th and 1st rater rows and rated descriptions are aligned, thus, all four are aligned
sum(j2$`Milestone Description`==o2$`Milestone Description`)
sum(j3$`Milestone Description`==o3$`Milestone Description`)
sum(j2$`Subject no.`==o2$`Subject no.`)
sum(j3$`Subject no.`==o3$`Subject No.`)

# Create Ortho Sports Dataset
data2 <- data.frame("Milestone_Number"=c2$`Subject no.`+nrow(combineddata),
                    "Description"=c2$`Milestone Description`,
                    "R1Femininity"=j2$`Femininity Rating`,
                    "R1Masculinity"=j2$`Masculinity Rating`,
                    "R2Femininity"=dn2$`Femininity Rating`,
                    "R2Masculinity"=dn2$`Masculinity Rating`,
                    "R3Femininity"=c2$`Femininity Rating`,
                    "R3Masculinity"=c2$`Masculinity Rating`,
                    "R4Femininity"=o2$`Femninity Rating`,
                    "R4Masculinity"=o2$`Masculinity Rating`,
                    "R5Femininity"=c(NA),
                    "R5Masculinity"=c(NA))
data2$Specialty <- c("Orthopaedic Sports Medicine")

# Create Pediatric Crit Care Dataset
data3 <- data.frame("Milestone_Number"=c3$`Subject No.`+nrow(combineddata)+nrow(data2),
                    "Description"=c3$`Milestone Description`,
                    "R1Femininity"=j3$`Femininity Rating`,
                    "R1Masculinity"=j3$`Masculinity Rating`,
                    "R2Femininity"=dn3$`Femininity Rating`,
                    "R2Masculinity"=dn3$`Masculinity Rating`,
                    "R3Femininity"=c3$`Femininity Rating`,
                    "R3Masculinity"=c3$`Masculinity Rating`,
                    "R4Femininity"=o3$`Femininity Rating`,
                    "R4Masculinity"=o3$`Masculinity Rating`,
                    "R5Femininity"=c(NA),
                    "R5Masculinity"=c(NA))
data3$Specialty <- c("Pediatric Critical Care Medicine")
data$Specialty <- combineddata$Specialty
data3$Description
if(sum(startsWith(data3$Description,"Assesses the patient‚Äôs"))==1) 
{data3$Description[startsWith(data3$Description,"Assesses the patient‚Äôs")] <- "Assesses the patient’s and family’s/caregivers’ prognostic awareness and identifies preferences for receiving prognostic information" 
  }
library(readr)
setwd("/Users/chadmiller/OneDrive/Research/Misc or Collaborations/Gender Medical Milestones")
# utf8 works better with text than standard csv
totalmilestones <- readr::read_csv("milestones_and_examples_2026-03-25_cleaned_and_updated_utf8.csv")

# Ortho Sports
# Filling in Information Raters were Blind to
totalmilestones_ortho <- subset(totalmilestones, Specialty == "Orthopaedic Sports Medicine")
data2$Competency <- c(NA)
data2$Subcompetency.Description <- c(NA)
data2$Occurrences <- c(NA)
data2$Milestones.Level <- c(NA)
i <- 1
# Identifying Column of description (to find the milestone level) and Row (to find Competency and Subcompetency information)
for (i in 1:nrow(data2)) {
  x <- c(1:14)[grepl(data2$Description[i], totalmilestones_ortho)]
  if(length(x)==0) { 
    data2$Competency[i] <- NA
    data2$Subcompetency.Description[i] <- NA
  } else if(length(x)>1) { 
    data2$Competency[i] <- "Multiple Columns"
    data2$Subcompetency.Description[i] <- "Multiple Columns"
  } else {
    y <- c(1:nrow(totalmilestones_ortho))[grepl(data2$Description[i], totalmilestones_ortho[[x]])]
    data2$Competency[i] <- totalmilestones_ortho$Competency[y[1]]
    data2$Subcompetency.Description[i] <- totalmilestones_ortho$`Subcompetency Description`[y[1]]
    data2$Milestones.Level[i] <- x-4
    data2$Occurrences[i] <- length(y)
  }
}
# Note that with duplicate descriptions (for rows), I'm using the first competency, subcompetency, and level
sum(is.na(data2$Milestones.Level))
sum(data2$Competency=="Multiple Columns", na.rm=T)
totalmilestones_ortho

# Pediatric Critical Care Medicine
# Filling in Information Raters were Blind to
totalmilestones_pccm <- subset(totalmilestones, Specialty == "Pediatric Critical Care Medicine")
data3$Competency <- c(NA)
data3$Subcompetency.Description <- c(NA)
data3$Occurrences <- c(NA)
data3$Milestones.Level <- c(NA)
i <- 1
# Identifying Column of description (to find the milestone level) and Row (to find Competency and Subcompetency information)
for (i in 1:nrow(data3)) {
  x <- c(1:14)[grepl(data3$Description[i], totalmilestones_pccm)]
  if(length(x)==0) { 
    data3$Competency[i] <- NA
    data3$Subcompetency.Description[i] <- NA
  } else if(length(x)>1) { 
    data3$Competency[i] <- "Multiple Columns"
    data3$Subcompetency.Description[i] <- "Multiple Columns"
  } else {
    y <- c(1:nrow(totalmilestones_pccm))[grepl(data3$Description[i], totalmilestones_pccm[[x]])]
    data3$Competency[i] <- totalmilestones_pccm$Competency[y[1]]
    data3$Subcompetency.Description[i] <- totalmilestones_pccm$`Subcompetency Description`[y[1]]
    data3$Milestones.Level[i] <- x-4
    data3$Occurrences[i] <- length(y)
  }
}
# Note that with duplicate descriptions (for rows), I'm using the first competency, subcompetency, and level
sum(is.na(data3$Milestones.Level))
sum(data3$Competency=="Multiple Columns", na.rm=T)
# will fix NAs manually (or try tweaking text detection code) and duplicate Multiple columns
c(1:14)[grepl(data3$Description[15], totalmilestones_pccm)]

# Create Matching Columns to Combine Original Data with Ortho Sports and Pediatric Critical Care
updateddata <- data.frame(data, "Competency"=combineddata$Competency, 
                          "Subcompetency.Description"=combineddata$Subcompetency.Description, 
                          "Occurrences"=rep(NA, nrow(data)), "Milestones.Level"=combineddata$Milestones.Level)
updateddata_combined <- rbind(updateddata,data2,data3)

write.csv(updateddata_combined, file = "finalcombineddata.03.24.2026.csv", row.names = F)

# Testing New Rater's File

newmilestones <- read_xlsx("/Users/chadmiller/Downloads/coding_sheet.xlsx")
unique(newmilestones$`Milestone Description`) |> length()

dom <- read_xlsx("/Users/chadmiller/Downloads/coding_sheet_new_coder.xlsx")
unique(c(updateddata_combined$Description, newmilestones$`Milestone Description`)) |> length() 
unique(dom$`Milestone Description`) |> length()

allmilestones <- c(updateddata_combined$Description, newmilestones$`Milestone Description`)
allmilestones_unique <- unique(allmilestones)
# all of these are good, just have an extra space in the updateddata_combined dataset
dom$`Milestone Description`[dom$`Milestone Description` %in% allmilestones==F]
allmilestones[allmilestones%in%dom$`Milestone Description`==F] 
