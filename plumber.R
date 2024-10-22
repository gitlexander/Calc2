 library(readr)
library(readxl)
library(ggplot2)
library(data.table)
library(tidyr)
library(car)
library(dplyr)
library(caret)
library(MASS)
library(lsa)
library(plumber)

#* Get the list of subtasks
#* @get /subtasks
function(nage,ngender,nmednum,nmedhisnum,ncog,nphy,nvis,nmotiv,nbusy,nhear) {
  
  df <- read_excel("Data96.xlsx")
  dfcsv <- read.csv("Data96.csv")
  
  df$`dlvt summary score` <- as.numeric(dfcsv$dlvt.summary.score)
  
  nid <- 97
  nage <- as.numeric(nage)
  ngender <- as.numeric(ngender)
  nmednum <- as.numeric(nmednum)
  nmedhisnum <- as.numeric(nmedhisnum)
  ncog <- as.numeric(ncog)
  nphy <- as.numeric(nphy)
  nvis <- as.numeric(nvis)
  nmotiv <- as.numeric(nmotiv)
  nbusy <- as.numeric(nbusy)
  nhear <- as.numeric(nhear)
  
  # change column names for products
  colnames(df)[4] <- "PD1"
  colnames(df)[5] <- "PD2"
  colnames(df)[6] <- "PD3"
  colnames(df)[7] <- "PD4"
  colnames(df)[8] <- "PD5"
  colnames(df)[9] <- "PD6"
  colnames(df)[10] <- "PD7"
  colnames(df)[11] <- "PD8"
  colnames(df)[12] <- "PD9"
  colnames(df)[13] <- "PD10"
  colnames(df)[14] <- "PD11"
  colnames(df)[15] <- "PD12"
  colnames(df)[16] <- "PD13"
  
  
  # add whisper test percentage column
  df$`whisper test left`[df$`whisper test left` == 2] <- 0
  df$`whisper test right`[df$`whisper test right` == 2] <- 0
  df$gender <- factor(ifelse(df$gender == 1, 0, 1))
  # add a column for total number of medical conditions
  df$totalmedhist <- rowSums(df[, c(23:43)])
  
  # create a new data frame that takes in only the important variables
  
  # columns for subtasks
  subtask1columns <- sort(c(seq(from = 85, to = 240, by = 5), seq(from = 88, to = 243, by = 5)))
  democolumns <- c(1, 17, 18, 22, 350) 
  
  # limit columns:
  # SMAT cognitive: 52; SMAT physical: 58; vision: SMAT 61 or DLVT 80; SEAMS (motivation): 72; MPED (environment): 75 or 77; hearing: 69
  limitcolumns <- c(52, 58, 61, 72, 75, 77, 71, 69)
  
  df2 <- df[,c(democolumns, limitcolumns, subtask1columns)]
  
  # rename the columns
  colnames(df2) <- c("ID", "Age", "Gender", "Mednum", "Medhist", "Cog", "Phy", "SMAT_vis", "SEAMS", "MPED_busy", "MPED_routine", "DLVT_vis", "Hear", "A1c", "A1f",
                     "A2c", "A2f", "A3c", "A3f", "A4c", "A4f", "A5c", "A5f", "A6c", "A6f", "B1c", "B1f", "B2c", "B2f", "B3c", "B3f", "B4c", "B4f",
                     "C1c", "C1f", "C2c", "C2f", "D1c", "D1f", "D2c", "D2f", "D3c", "D3f", "D4c", "D4f", "D5c", "D5f", "D6c", "D6f", "G1c", "G1f",
                     "G2c", "G2f", "G3c", "G3f", "H1c", "H1f", "I1c", "I1f", "I2c", "I2f", "I3c", "I3f", "M1c", "M1f", "P1c", "P1f", "R1c", "R1f", 
                     "R2c", "R2f", "T1c", "T1f", "T2c", "T2f", "U1c", "U1f") 
  
  df3 <- df2 %>%
    group_by(ID) %>%
    summarise(
      Age = first(Age), Gender = first(Gender), Mednum = first(Mednum), 
      Medhist = first(Medhist), Cog = first(Cog), Phy = first(Phy), 
      SMAT_vis = first(SMAT_vis), SEAMS = first(SEAMS), 
      MPED_busy = first(MPED_busy), MPED_routine = first(MPED_routine),
      DLVT_vis = first(DLVT_vis), Hear = first(Hear),
      A1 = sum(A1c),
      A1fail = sum(A1f),
      A2 = sum(A2c),
      A2fail = sum(A2f),
      A3 = sum(A3c),
      A3fail = sum(A3f),
      A4 = sum(A4c),
      A4fail = sum(A4f),
      A5 = sum(A5c),
      A5fail = sum(A5f),
      A6 = sum(A6c),
      A6fail = sum(A6f),
      B1 = sum(B1c),
      B1fail = sum(B1f),
      B2 = sum(B2c),
      B2fail = sum(B2f),
      B3 = sum(B3c),
      B3fail = sum(B3f),
      B4 = sum(B4c),
      B4fail = sum(B4f),
      C1 = sum(C1c),
      C1fail = sum(C1f),
      C2 = sum(C2c),
      C2fail = sum(C2f),
      D1 = sum(D1c),
      D1fail = sum(D1f),
      D2 = sum(D2c),
      D2fail = sum(D2f),
      D3 = sum(D3c),
      D3fail = sum(D3f),
      D4 = sum(D4c),
      D4fail = sum(D4f),
      D5 = sum(D5c),
      D5fail = sum(D5f),
      D6 = sum(D6c),
      D6fail = sum(D6f),
      G1 = sum(G1c),
      G1fail = sum(G1f),
      G2 = sum(G2c),
      G2fail = sum(G2f),
      G3 = sum(G3c),
      G3fail = sum(G3f),
      H1 = sum(H1c),
      H1fail = sum(H1f),
      I1 = sum(I1c),
      I1fail = sum(I1f),
      I2 = sum(I2c),
      I2fail = sum(I2f),
      I3 = sum(I3c),
      I3fail = sum(I3f),
      M1 = sum(M1c),
      M1fail = sum(M1f),
      P1 = sum(P1c),
      P1fail = sum(P1f),
      R1 = sum(R1c),
      R1fail = sum(R1f),
      R2 = sum(R2c),
      R2fail = sum(R2f),
      T1 = sum(T1c),
      T1fail = sum(T1f),
      T2 = sum(T2c),
      T2fail = sum(T2f),
      U1 = sum(U1c),
      U1fail = sum(U1f)
    )
  
  # New row with some data
  new_row <- c(ID=nid,Age=nage,Gender=ngender,Mednum=nmednum,Medhist=nmedhisnum,Cog=ncog,Phy=nphy,DLVT_vis=nvis,SEAMS=nmotiv,MPED_busy=nbusy,MPED_routine=0,Hear=nhear)
  
  # Create a new row with all columns
  new_row_full <- setNames(as.list(rep(0, ncol(df3))), names(df3))
  
  # Update with the specified values
  new_row_full[names(new_row)] <- new_row
  
  # Convert to data frame
  new_row_df <- as.data.frame(new_row_full, stringsAsFactors = FALSE)
  
  # Add the new row to the existing data frame
  df3 <- rbind(df3, new_row_df)
  
  df3$SMAT_vis <- as.numeric(df3$SMAT_vis)
  df3$Hear <- as.factor(df3$Hear)
  
  # standardizing continuous covariates
  
  df3$Age_std <- (df3$Age - mean(df3$Age))/sd(df3$Age)
  df3$Mednum_std <- (df3$Mednum - mean(df3$Mednum))/sd(df3$Mednum)
  df3$Medhist_std <- (df3$Medhist - mean(df3$Medhist))/sd(df3$Medhist)
  df3$Cog_std <- (df3$Cog - mean(df3$Cog))/sd(df3$Cog)
  df3$Phy_std <- (df3$Phy - mean(df3$Phy))/sd(df3$Phy)
  df3$SMAT_vis_std <- (df3$SMAT_vis - mean(df3$SMAT_vis))/sd(df3$SMAT_vis)
  df3$SEAMS_std <- (df3$SEAMS - mean(df3$SEAMS))/sd(df3$SEAMS)
  df3$MPED_busy_std <- (df3$MPED_busy - mean(df3$MPED_busy))/sd(df3$MPED_busy)
  df3$MPED_routine_std <- (df3$MPED_routine - mean(df3$MPED_routine))/sd(df3$MPED_routine)
  df3$DLVT_vis_std <- (df3$DLVT_vis - mean(df3$DLVT_vis))/sd(df3$DLVT_vis)
  
  # Splitting the data in Training and Predicting
  
  # Get important columns from df3
  
  PPMiden <- df3[c("ID", "Gender", "Mednum_std", "Medhist_std", "Cog_std", 
                   "Phy_std", "SMAT_vis_std", "SEAMS_std", "MPED_busy_std", 
                   "DLVT_vis_std", "Hear")]
  
  # randomly select 3 rows from PPMiden to demonstrate later how prediction works
  #set.seed(123)
  #remove <- sample(nrow(PPMiden), 3)
  #predicting <- PPMiden[c(remove),]
  #training <- PPMiden[-c(remove),]
  
  #select the newly added row for prediction
  # Find the index of the new row
  new_row_id <- df3$ID[nrow(df3)] # since the new row is the last one added
  
  # Create the new predicting dataset
  predicting <- PPMiden[PPMiden$ID == new_row_id, , drop = FALSE]
  training <- PPMiden[PPMiden$ID != new_row_id, , drop = FALSE]
  
  # perform cosine similarity to identify Mp% of the training data that is the most similar to each participant in `predicting`
  
  idenID <- function(predicting_i, training, Mp) {
    
    testingvecnoID <-as.numeric(predicting_i)[-1]
    testingvecnoID[c(1, 10)] <- testingvecnoID[c(1, 10)]-1 # since as.numeric added 1 to the binary variables
    
    # change training data into all numeric values
    trainingnoID <- training[-1]
    trainingnoID$Gender <- as.numeric(trainingnoID$Gender)-1
    trainingnoID$Hear <- as.numeric(trainingnoID$Hear) -1
    
    # calculate cosine similarity metric of every value in the training data with one testing data and append to the training data
    training$csm <- apply(trainingnoID, 1, function(x) cosine(testingvecnoID, x))
    
    training <- training[order(training$csm, decreasing = TRUE),]
    
    # sort the training data by decreasing csm, identify the ID of participants in the top Mp%
    M <- round(length(training$ID)*Mp, 0)
    return(training$ID[1:M])
    
  }
  
  
  newperson1_simID <- idenID(predicting[1,], training, 0.80)
  
  predicting$ID
  
  # first, we do some more data cleaning to make it into a format that we want:
  
  df4 <- df3 %>% pivot_longer(cols = c("A1", "A2", "A3", "A4", "A5", "A6", 
                                       "B1", "B2", "B3", "B4", "C1", "C2", 
                                       "D1", "D2", "D3", "D4", "D5", "D6", 
                                       "G1", "G2", "G3", "H1", "I1", "I2", 
                                       "I3", "M1", "P1", "R1", "R2", "T1", 
                                       "T2", "U1"),
                              names_to = "Subtasks",
                              values_to = "Count")
  #write.csv(df4, "df4.csv")
  #write.csv(df3, "df3o.csv")
  
  # create a new vector failcount of length 2560. Three things are needed to find the correct element to append to this vector: the subtask, the patient ID, and its failure moment for the corresponding subtask.
  
  # writing the above in a function:
  failvec <- c()
  for (i in 1:length(df4$ID)) {
    
    # to find the correct subtask:
    identify_st <- df4[i,]$Subtasks
    # change the vector name to be able to find the failure moment:
    identify_failcolumn <- paste(identify_st, "fail", sep = "")
    # identify the correct participant:
    identify_ID <- df4[i,]$ID
    # then the value to be appended to the ith element in the vector is:
    truefailval <- df3[df3$ID == identify_ID,][identify_failcolumn]
    
    failvec <- c(failvec, truefailval)
  }
  
  df4$Fail <- as.numeric(failvec)
  
  # create a new variable, proportion based on the count and fail columns
  df4$Proportion <- (df4$Count - df4$Fail)/df4$Count
  
  dfnoNA <- df4[df4$Count != 0,]
  
  dfnoNA$Subtasks <- as.factor(dfnoNA$Subtasks)
  
  # It looks like some subtasks have no split in the data (i.e. everyone was successful). Let's check:
  dfnoNA$Proportion[dfnoNA$Subtasks == "D1"]
  
  # Should we remove these subtasks from the model? Let's create a new dataframe 
  dfnoNA2 <- dfnoNA[!dfnoNA$Subtasks %in% c("D1"), ]
  dfnoNA2$Subtasks <- droplevels(dfnoNA2$Subtasks)
  
  #write.csv(dfnoNA2, df3noNA2.csv")
  
  # Most similar participants to P31
  
  #The top 80% of the participants in the training data that is the most similar to Participant 31 are:
  print("TrainingID for P31:")
  sort(newperson1_simID)
  
  # Fitting predictive models
  #To predict the success rate of each subtask for a new participant, we train a generalized linear model on a personalized dataset (i.e. top 80% of the most similar participants to this new participant). We consider two models, one including the SMAT vision score, and the other including the DLVT vision score:
  
  # for newperson1
  training1 <- subset(dfnoNA2, ID %in% newperson1_simID)
  
  training1$Gender <- as.factor(training1$Gender)
  training1$Hear <- as.factor(training1$Hear)
  
  # for P31:
  
  model31_2 <- glm(Proportion ~ Gender + Mednum + Medhist + Cog + Phy  
                   + DLVT_vis + SEAMS + MPED_busy + Hear + Subtasks,
                   data = training1,
                   weights = Count,
                   family = binomial(link = "logit"))
  
  # Making predictions
  
  makingpred <- function(newdata, subtask_list, fittedmodel) {
    
    newdata <- as.data.frame(newdata)
    newdata <- newdata[rep(seq_len(nrow(newdata)), length.out = 30), ]
    newdata$Subtasks <- subtask_list
    newdata$Subtasks <- as.factor(newdata$Subtasks)
    
    newdata$logodds <- as.numeric(predict(fittedmodel, newdata))
    newdata$prop <- exp(newdata$logodds)/(1+exp(newdata$logodds))
    
    return(newdata[order(newdata$prop, decreasing = TRUE),])
    
    
  }
  
  subtask_list <- c("A1", "A2", "A3", "A4", "A5", "A6", "B1", "B2", "B3", "B4", 
                    "C1", "C2", "D2", "D3", "D4", "D5", "D6", "G1", "G2", "G3", 
                    "H1", "I1", "I2", "I3", "M1", "P1", "R1", "R2", "T1", "U1")
  
  # participant 1
  #newparticipant1 <- dfnoNA2[dfnoNA2$ID == predicting$ID[1],]
  #newparticipant1_2 <- df3[c("ID", "Gender", "Mednum", "Medhist", "Cog", "Phy", "DLVT_vis", "SEAMS", "MPED_busy", "Hear")]
  #newparticipant1 <- predicting[1,]
  newparticipant1_2 <- df3[df3$ID == 97, c("ID", "Gender", "Mednum", "Medhist", "Cog", "Phy", "DLVT_vis", "SEAMS", "MPED_busy", "Hear")]
  
  pred1_2 <- makingpred(newparticipant1_2, subtask_list, model31_2)
  
  
  #We predict the success rate of each subtask for P31, P79, and P51 using the fitted models above. 
  
  ## P31 
  
  #P31 has the following characteristics:
  
  pred1_2[1,c(2:10)]
  
  #Using `model31_2`, which has DLVT vision score instead of SMAT vision score, we get the following output:
  
  pred1_2[c(1, 11:13)]
  
  # Conclusion
  
  #Thus, the subtasks predicted to have the highest success rate to the lowest success rate for P31 are
  
  as.vector(pred1_2$Subtasks)
  
  #when we use the DLVT vision score. There is a slight difference in the ordering of some subtasks from fitting two different models.
  
  #In conclusion, we use GLM to predict the proportion of success for each subtask for the three different participants: P31, P79, and P51. We showed that not only do these participants have different success rates for the subtasks, but also the order of the subtasks from the most successful to the least successful also differs. This is because to predict the success rate of the subtasks, we have used a model fitted on a personalized data that only contain 80% of the original data that are the most similar to each of the three participants - this way, we can consider the unique characteristics of the new participants to predict their success rate of each subtask, and recommend products that are better suited to their situation.
  
  
  # Appendix
  
  #Summary of models fit for P31 using SMAT:
  #summary(model31_1)
  
  #Summary of models fit for P31 using DLVT:
  #summary(model31_2)
  
  #7. Conclusion
  
  result1 <- data.frame(subID=pred1_2$Subtasks, probability=round((pred1_2$prop)*100))
  # Assuming you already have result1 and descriptions_df data frames
  
  # Create the descriptions data frame
  descriptions_df <- data.frame(
    subID = c("A1", "A2", "A3", "A4", "A5", "A6", "B1", "B2", "B3", "B4", 
              "C1", "C2", "D2", "D3", "D4", "D5", "D6", "G1", "G2", 
              "G3", "H1", "I1", "I2", "I3", "M1", "P1", "R1", "R2", "T1", "U1"),
    description = c(description = c(
      "Locate the battery/cartridge compartment/medication cavity",
      "Place/insert batteries correctly",
      "Lift open or close a battery compartment door",
      "Slide in/out battery compartment door",
      "Slide a tab/button",
      "Check/ensure/verify the device is on or the lock is placed in position/Follow instructions/Ensure indicator light flashes",
      "Flip device",
      "Insert key and rotate",
      "Press and rotate a lid",
      "Open a lid by lifting",
      "Press and hold a button on a device",
      "Press a button on a device",
      "Open pill box or compartment or tray or door by sliding",
      "Pick up the correct pillbox/pill organizer/open correct compartment",
      "Insert/fill/place medication in compartment/pillbox/pill organizer",
      "Close lid",
      "Put stickers on pillbox dividers",
      "Remove the medication",
      "Grab/hold the device",
      "Place hand over open slot",
      "Rotate the carousel three days from today’s date",
      "Locate and touch on an icon/button on an app or screen",
      "Enter/type data in a digital app/screen",
      "Scroll through the options on a digital screen",
      "Align and insert cartridge into the designated slot",
      "Tear packaging",
      "Rotate retaining clips at each end of the device in an open/close position",
      "Align connectors to one another and gently push card into the device",
      "Pierce cavity barrier",
      "Pull blister pack away from a connected device"
    )
    ) 
  )
  
  # Merge result1 with descriptions_df to add descriptions
  result1 <- merge(result1, descriptions_df, by.x = "subID", by.y = "subID")
  
  lists <- list(
    MedQ = list("A1", "A2", "A3", "C1", "C2", "B4", "D4", "D5", "G1"),
    MedGlider = list("A1", "A2", "A4", "A5", "B4", "C2", "D2", "D4", "D5", "G1"),
    VitaCarry = list("A1", "A2", "A5", "A6", "C2", "D4", "D5", "G1"),
    epill = list("A4", "A5", "B4", "C2", "D2", "D4", "D5", "G1"),
    hour = list("B4", "C2", "D4", "D5", "G1"),
    ennovea = list("A1", "A2", "A6", "C2", "D1", "D3", "D4", "D5", "D6", "G1", "H1"),
    pillbox = list("B4", "C2", "D3", "D4", "D5", "G1"),
    medcentre = list("A1", "A2", "A5", "A6", "B4", "C2", "D3", "D4", "G1"),
    elliegrid = list("D2", "D4", "D5", "G1", "G2", "I1", "I2", "I3"),
    medready = list("A1", "A2", "A5", "A6", "B1", "B2", "B3", "B4", "C1", "C2", "D2", "D4"),
    gms = list("A1", "A2", "A6", "B1", "B2", "B4", "C1", "C2", "D4", "D5", "G2", "G3"),
    spencer = list("A1", "A6", "C2", "G1", "I1", "I3", "M1", "P1"),
    jones = list("A1", "A6", "G1", "G2", "R1", "R2", "T1", "U1")
  )
  
  # Function to calculate average probability for a given list of subtask IDs
  average_probability <- function(subtasks) {
    # Filter the dataframe for the given subtasks
    filtered_df <- result1[result1$subID %in% subtasks, ]
    
    # Calculate the mean probability
    mean(filtered_df$probability, na.rm = TRUE)
  }
  
  # Apply the function to each list in the lists object
  average_probabilities <- lapply(lists, average_probability)
  
  # Display the average probabilities for each group
  #print(average_probabilities)
  probdf <- as.data.frame(average_probabilities)
  
  # View the updated result1
  #print(result1)
  combined <- merge(result1,probdf)
  sortedcombined <- combined[order(-combined$probability), ]
  print(sortedcombined)
  #write.csv(sortedcombined,"sortedcombineddf.csv", row.names = TRUE)
  
}
