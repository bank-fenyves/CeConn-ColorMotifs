## Source data need to be imported as "data". See README file on github for further instructions.

library("easypackages")
libraries("tidyr", "dplyr","openxlsx", "readxl")


# Start from scratch, data import and 
{
data_NT <- read_excel("Supplementary File S1 (input file for sign prediction and rendomization).xlsx", sheet = "1. NT expr")
data_pred <- read_excel("Supplementary File S1 (input file for sign prediction and rendomization).xlsx", sheet = "5_Prediction")
data_Rec <- read_excel("Supplementary File S1 (input file for sign prediction and rendomization).xlsx", sheet = "4. Receptor type expression ")

##Preparation of data
rownames(data_NT) <- data_NT$Neuron_class
data_NT =  as.matrix(data_NT)
data_NT[is.na(data_NT)] <- "0"
data_NT = as.data.frame(data_NT)
data_pred[,"Glu_Pos"] = 0
data_pred[,"Glu_Neg"] = 0
data_pred[,"ACh_Pos"] = 0
data_pred[,"ACh_Neg"] = 0
data_pred[,"GABA_Pos"] = 0
data_pred[,"GABA_Neg"] = 0
data_pred[,"Pred"] = 0
data_pred = as.matrix(data_pred)
data_pred = as.data.frame(data_pred)
data_pred <- data_pred[,-c(2,3,5,6)]
rownames(data_Rec) <- data_Rec$Neuron_Class
data_Rec <- as.matrix(data_Rec)
data_NT = as.data.frame(data_NT)
six = list("Glu_Pos", "Glu_Neg", "ACh_Pos", "ACh_Neg", "GABA_Pos", "GABA_Neg")
data_NT[,"Neurotransmitters"] = paste0(data_NT[,"Dom_NT"], ".", data_NT[,"Alter_NT"])
}

## Fills out what kind of interneuronal communication exists by the 6 categories (Glu-ACh-GABA * pos-neg), for the whole network
{for (i in 1:nrow(data_pred)) {
  source = data_pred[i,"Source"]
  target = data_pred[i,"Target"]
  NT_Dom = data_NT[source,"Dom_NT"]
  NT_Alt = data_NT[source,"Alter_NT"]
# ehhez a data_Rec matrix formátumban kell legyen
  for (x in six) {
    if (grepl(NT_Dom, x) == TRUE && data_Rec[target,x] > 0) {
      data_pred[i,x] <- data_Rec[target,x]}
    else if (grepl(NT_Alt, x) == TRUE && data_Rec[target,x] > 0) {
      data_pred[i,x] <- data_Rec[target,x]}
    else 
      data_pred[i,x] <- 0}
  }
}

# As numeric
{
data_pred[,c(3)] <- as.numeric(data_pred[,c(3)])
data_pred[,c(4)] <- as.numeric(data_pred[,c(4)])
data_pred[,c(5)] <- as.numeric(data_pred[,c(5)])
data_pred[,c(6)] <- as.numeric(data_pred[,c(6)])
data_pred[,c(7)] <- as.numeric(data_pred[,c(7)])
data_pred[,c(8)] <- as.numeric(data_pred[,c(8)])
}

## Predicts the polarity of all connections
{
for (i in 1:nrow(data_pred)) {
    if (sum(as.double(data_pred[i,c("Glu_Pos", "ACh_Pos", "GABA_Pos")])) - 
        sum(as.double(data_pred[i,c("Glu_Neg", "ACh_Neg", "GABA_Neg")])) > 0)
      data_pred[i,"Pred"] = "+"
    else if (sum(as.double(data_pred[i,c("Glu_Neg", "ACh_Neg", "GABA_Neg")])) - 
             sum(as.double(data_pred[i,c("Glu_Pos", "ACh_Pos", "GABA_Pos")])) > 0)
      data_pred[i,"Pred"] = "-"
    else if (sum(as.double(data_pred[i,c("Glu_Neg", "ACh_Neg", "GABA_Neg")])) - 
             sum(as.double(data_pred[i,c("Glu_Pos", "ACh_Pos", "GABA_Pos")])) == 0)
      data_pred[i,"Pred"] = "unknown"
  else data_pred[i,"Pred"] = "error"
}
}

## Randomization-- and prediction and display -- Randomizing neurotransmitters
set.seed(2000)
{
  data_NT_new <- data_NT
  data_pred_randomNT <- data_pred
  for (r in 1:1000) {
    #Randomization of neurotransmitters, dominant-and-alternative NTs paired
    data_NT_new[,"Neurotransmitters"] = sample(data_NT[,"Neurotransmitters"], replace = FALSE)
    data_NT_new <- as.data.frame(data_NT_new) %>% 
      separate(Neurotransmitters, c("Dom_NT", "Alter_NT"), remove = FALSE)
    
    # assessess transmission type
    for (i in 1:nrow(data_pred_randomNT)) {
      source = data_pred_randomNT[i, "Source"]
      target = data_pred_randomNT[i, "Target"]
      NT_Dom = data_NT_new[data_pred_randomNT[i, "Source"],"Dom_NT"]
      NT_Alt = data_NT_new[data_pred_randomNT[i, "Source"],"Alter_NT"]
      for (x in six) {
        if (grepl(NT_Dom, x) == TRUE && data_Rec[data_pred_randomNT[i, "Target"],x] > 0) {
          data_pred_randomNT[i,x] <- data_Rec[target,x]}
        else if (grepl(NT_Alt, x) == TRUE && data_Rec[data_pred_randomNT[i, "Target"],x] > 0) {
          data_pred_randomNT[i,x] <- data_Rec[target,x]}
        else 
          data_pred_randomNT[i,x] <- 0
      }
    ## Predicts the polarity of all connections
        if (sum(as.double(data_pred_randomNT[i,c("Glu_Pos", "ACh_Pos", "GABA_Pos")])) - 
            sum(as.double(data_pred_randomNT[i,c("Glu_Neg", "ACh_Neg", "GABA_Neg")])) > 0) {
          data_pred_randomNT[i,paste0("Pred_", r)] = "+"}
      else if (sum(as.double(data_pred_randomNT[i,c("Glu_Neg", "ACh_Neg", "GABA_Neg")])) - 
               sum(as.double(data_pred_randomNT[i,c("Glu_Pos", "ACh_Pos", "GABA_Pos")])) > 0) {
        data_pred_randomNT[i,paste0("Pred_", r)] = "-"}
      else if (sum(as.double(data_pred_randomNT[i,c("Glu_Pos", "ACh_Pos", "GABA_Pos")])) - 
                 sum(as.double(data_pred_randomNT[i,c("Glu_Neg", "ACh_Neg", "GABA_Neg")])) == 0) {
        data_pred_randomNT[i,paste0("Pred_", r)] = "unknown"}
    }
  }
  # Exports the predictions in 
  write.csv(data_pred_randomNT, file = "Supplementary Data S1 - export_randomNT (complex-resolved).csv")
}

## Randomization v2 - and display -- Randomizing edge polarities
set.seed(2000)
{
  data_pred_random <- data_pred
  data_pred_randompol <- data_pred
  for (r in 1:1000) {
    #Randomization of predicted polarities
    data_pred_random[,"Pred"] = sample(data_pred_random[,"Pred"], replace = FALSE)
    data_pred_random <- as.data.frame(data_pred_random)
    data_pred_randompol[,paste0("Random_", r)] = data_pred_random$Pred
    }
  # Exports the predictions in 
  write.csv(data_pred_randompol, file = "Supplementary Data S2 - export_randomizedpol (complex-resolved).csv")
}

## Descriptive statistics
stat = matrix(c(1:4000), nrow = 4, ncol = 1000, dimnames = list(c(1:4), c(1:1000)))
for (s in 10:20) {
  pos = stat[1,s] = sum(data_pred[,s] == "+")
  neg = stat[2,s] =sum(data_pred[,s] == "-")
  unp = stat[4,s] =sum(data_pred[,s] == "unknown")
}
stat = matrix(c(1:4000), nrow = 4, ncol = 1000, dimnames = list(c(1:4), c(1:1000)))
stat[1,s] = sum(data_pred[,s] == "+")

stat[,"20"] = c("1")