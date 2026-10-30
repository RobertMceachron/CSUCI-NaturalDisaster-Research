library(tidyverse) # Managing Data
library(dplyr) # Managing Data
library(readxl) # For Excel
library(haven) # For SPSS Output
library(ltm) # For Reliability 




###########################
#        Study #1         #
###########################



###########################
#        Study #2         #
###########################

  ##############################
  # Data Cleaning/Organization #
  ##############################

# Import Data
  rawQualtrics <- read.csv("data/study2/qualtrics.csv")
  rawPhys <- read_xlsx("data/study2/physiological.xlsx")
  rawUnreal <- read.csv("data/study2/unrealoutput.csv")
  rawDebrief <- read_xlsx("data/study2/debrief.xlsx")

# Qualtrics Data
#################
  # Remove all non-participants
    dataQualtrics <- rawQualtrics
    dataQualtrics$Participant <- as.numeric(dataQualtrics$Participant)
    dataQualtrics <- dataQualtrics |>
                      filter(!is.na(Participant))
  
  # Remove label rows
    dataQualtrics <- dataQualtrics[c(-1,-2),]
  
  # Compute composite scores
    # Empathy
      scaleEmpathy <- dataQualtrics |> 
                        dplyr::select(contains("Empathy"))
      scaleEmpathy <- scaleEmpathy |>
                        mutate(across(everything(), as.numeric))
      
      compositeEmpathy <- rowSums(scaleEmpathy)
      compositeEmpathy <- as.data.frame(compositeEmpathy)
      compositeEmpathy <- compositeEmpathy/16
      
    # Altruism
      scaleAltruism <- dataQualtrics |> 
        dplyr::select(contains("Altruism"))
      scaleAltruism <- scaleAltruism |>
        mutate(across(everything(), as.numeric))
      
      compositeAltrusim <- rowSums(scaleAltruism)
      compositeAltrusim <- as.data.frame(compositeAltrusim)
      compositeAltrusim <- compositeAltrusim/20
      
    # Courage
      scaleCourage <- dataQualtrics |> 
        dplyr::select(contains("Courage"))
      scaleCourage <- scaleCourage |>
        mutate(across(everything(), as.numeric))
      
      compositeCourage <- rowSums(scaleCourage)
      compositeCourage <- as.data.frame(compositeCourage)
      compositeCourage <- compositeCourage/23
  
  # Grab Important Columns
    dataQualtricsClean <- dataQualtrics |> 
                            dplyr::select(c("Participant", "VGUse", "VRExperience", "MotionSickness",
                                            "Gender", "Age", "Ethnicity", "Disasters_4", "Disasters_6"))
    dataQualtricsClean <- dataQualtricsClean |>
                            mutate(across(everything(), as.numeric))
    
  # Adds Mean Values & Renames
    dataQualtricsClean$Empathy <- compositeEmpathy
    dataQualtricsClean$Altrusim <- compositeAltrusim
    dataQualtricsClean$Courage <- compositeCourage
    dataQualtricsClean <- as.data.frame(dataQualtricsClean)
    
    names(dataQualtricsClean) <- c("Participant", "VGUse", "VRExperience", "MotionSickness",
                                      "Gender", "Age", "Ethnicity", "Disasters_4", "Disasters_6", "Empathy",
                                      "Courage", "Altruism")
  
# Unreal Data
#################
  # Rename Participant and remove label
    dataUnreal <- rawUnreal |>
                    rename_at('ParticipantID', ~'Participant') 
    dataUnreal <- dataUnreal[-1, ]
    
# Debrief Data
#################
  # Rename Participant and Remove Label
    dataDebrief <- rawDebrief |>
      rename_at('Participant number', ~'Participant') 
  # Grabs Important Columns for Qualitative Analysis
    dataDebrief <- dataDebrief |> 
      dplyr::select(c("Participant", "1B) Y/N", "3B) Y/N", "4B) Y/N"))
    names(dataDebrief) <- c("Participant", "1B) Y/N", "3B) Y/N", "4B) Y/N")
    
# Physiological Data
#####################
  # Rename Participant and Remove Label
    dataPhys <- rawPhys |>
      rename_at('Phone', ~'Participant') 
    
    
# Final Output
###############
  # Merges Data
    data <- merge(dataQualtricsClean, dataUnreal, by = "Participant", all = TRUE)
    data <- merge(data, dataPhys, by = "Participant", all = TRUE)

  # Writes Save Files
    write_sav(data, "data/finalAnalysis/Combined_Data.sav")
    write_csv(data, "data/finalAnalysis/Combined_Data.csv")

  #######################################
  #        Descriptive Statistics       #
  #######################################

# Reliability Check
  cronbach.alpha(scaleEmpathy, na.rm = TRUE)
  cronbach.alpha(scaleAltruism, na.rm = TRUE)
  cronbach.alpha(scaleCourage, na.rm = TRUE)



