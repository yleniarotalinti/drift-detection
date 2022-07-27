#########
# READ DATA
#########

read.data <- function(dataset){
  switch (dataset,
          "Abrupt" = {source("data/readers/validationSets_reader.R")},
          "Gradual_W10K" = {source("data/readers/validationSets_reader.R")},
          "Gradual_W20K" = {source("data/readers/validationSets_reader.R")},
          {
            stop("Dataset not found")
          }
  )
}

set.parameters<-function(dataset)
{
  switch (dataset,
          "Abrupt" = {
            granularity <- 5000           #batch (#instances)
            dateFormat <- NaN             #format (%Y , %Y-%m)
            dateColumnLabel <- NaN        #meaningful columns
            classColumnLabel <- "class"
            accumulate.batches <-1        #how many batches to accumulate
          },
          "Gradual_W10K" = {
            granularity <- 5000
            dateFormat <- NaN
            dateColumnLabel <- NaN
            classColumnLabel <- "class"
            accumulate.batches <-1
          },
          "Gradual_W20K" = {
            granularity <- 5000
            dateFormat <- NaN
            dateColumnLabel <- NaN
            classColumnLabel <- "class"
            accumulate.batches <-1
          },
          {
            stop("Dataset not found")
          }
  )
  return(list(granularity=granularity, 
              dateFormat=dateFormat, 
              dateColumnLabel=dateColumnLabel, 
              classColumnLabel=classColumnLabel, 
              accumulate.batches=accumulate.batches)
  )
}

read.annotations<-function(dataset){
  switch (dataset,
          "Abrupt" = {sheet.name <-"3.Abrupt"},
          "Gradual_W10K" = {sheet.name <-"4.Gradual10K"},
          "Gradual_W20K" = {sheet.name <-"5.Gradual20K"},
          {
            print("Dataset not found")
          }
  )
  read_excel("Annotations.xlsx", sheet = sheet.name, skip=3)
}

generate.folders<-function(){
  dir.create("results/")
  for(d in dataset){
    dir.create(paste0("results/", d))
    for(bias in bias.exploration$label){
      dir.create(paste0("results/", d, "/", toupper(bias)))
      for(metric in metrics.names){
        dir.create(paste0("results/",d, "/", toupper(bias), "/", toupper(metric)))
        for (alpha in alphas){
          dir.create(paste0("results/",
                            d, "/",
                            toupper(bias), "/",
                            toupper(metric), "/",
                            "alpha",alpha))
        }
      }
    }
  }
}

#to prevent masking from %+% function of tidyverse
`%+c%` <- crayon::`%+%`

output.description<-function(r, progress, bias, metric, alpha, model, updating, type){
  return(cat(
    blue$underline$bold('CONCEPT DRIFT DETECTION:\n') %+c%
    blue("RUN:", r, "of", run.num, "-", compute.progress(progress), "COMPLETED!\n") %+c%
    blue("Bias check:", set.bias.title(bias), "\n") %+c%
    blue("Metric:", metric, "\n") %+c%
    blue("Alpha:", set.alpha.title(alpha), "\n") %+c%
    blue("Model:", set.title(model), "\n") %+c%
    blue("Updating:", updating, "\n") %+c%
    blue("Check for:", type, "drifts\n")
  ))
}

compute.progress<-function(progress){
  tot <- nrow(bias.exploration)*length(metrics.names)*length(alphas)*length(models.names)*run.num
  return(paste0(round((progress/tot)*100, digits=2), "%"))
}

##########
# HANDLE BATCHES
##########

set.batches<-function(data, granularity){
  batchesList<- get.batches.list(data, granularity)
  
  batches<-list(get.firstBatch(batchesList, accumulate.batches))
  for(i in (accumulate.batches+1):length(batchesList)){
    batches<-append(batches, list(list(batchID=length(batches)+1, 
                                       batchName=names(batchesList)[i],
                                       dateStart=ifelse(flagValidation, ((i-1)*granularity)+1, get.firstDay(names(batchesList)[i])), 
                                       dateEnd=ifelse(flagValidation, i*granularity, get.lastDay(names(batchesList)[i])), 
                                       data=batchesList[[i]])))
  }
  names(batches)<-names(batchesList)[accumulate.batches:length(batchesList)]
  return(batches)
}

get.batches.list<-function(data, granularity){
  if(!flagValidation){
    return(split(
      data, 
      format(data[dateColumnLabel], dateFormat)
    ))
  }
  b.list <- split(data, (seq(nrow(data))-1) %/% granularity)
  names(b.list)<-seq(granularity, nrow(data), by=granularity)/1000
  return(b.list)
}

get.firstBatch<-function(batchesList, accumulate.batches){
  first.batch.data<-data.frame()
  for(a in 1:accumulate.batches){
    first.batch.data<-rbind(first.batch.data, batchesList[[a]])
  }
  return(list(batchID=1, 
              batchName=names(batchesList)[accumulate.batches],
              dateStart=ifelse(flagValidation, 1, get.firstDay(names(batchesList)[1])), 
              dateEnd=ifelse(flagValidation, granularity*accumulate.batches, get.lastDay(names(batchesList)[accumulate.batches])), 
              data=first.batch.data))
}

get.firstDay<-function(dateString){
  flagYearBatches<-ifelse(dateFormat=="%Y", TRUE, FALSE)
  newdate<-as.Date(paste0(dateString, ifelse(flagYearBatches,"/01/01","/01")))
  return(format(newdate, ifelse(flagYearBatches,"%Y/%m","%Y/%m/%d")))
}
get.lastDay<-function(dateString){
  flagYearBatches<-ifelse(dateFormat=="%Y", TRUE, FALSE)
  first.day<-as.Date(paste0(dateString, ifelse(flagYearBatches,"/01/01","/01")))
  newdate<-ceiling_date(first.day, "month")-1
  return(format(newdate, ifelse(flagYearBatches,"%Y/%m", "%Y/%m/%d")))
}

get.batchName<-function(i){
  return(names(batches)[i])
}


########
# DRIFT DETECTION INITILIZATION
########

driftDetection.initialization<-function(bias, metric, alpha, model, update, type){
  drift.detection<-list(
    metric=metric,
    model=model,
    alpha=alpha,
    bias=bias,
    update=update,
    type=type,
    batchStart=batches[[1]]$batchID,
    batchEnd=batches[[1]]$batchID,
    lastDriftBatch=NA,
    trainingSet=list(),
    fittedModel=NULL,
    controlSet=batches[[2]]$batchID,
    nextTestSet=batches[[3]]$batchID,
    controlSetOutput=c(),
    nextTestSetOutput=c(),
    results=list(plot=data.frame(), comparison=data.frame())
  )
  return(drift.detection)
}

#########
# TRAINING SAMPLE (batchStart --> batchEnd)
#########

###
#FUNCTION: this function allows you to sample at least one element for each level of a variable
sample.unbalanced.factors <- function(training, variables){
  #initialization
  out <- data.frame(training[1, ])
  for (v in variables) {
    #each level of each variable is sampled (at least once)
    s <- group_by(training, training[, v]) %>% sample_n(size = 1)
    #get rid of the last column (it is a metadata column)
    out <- rbind(out, s[, -(ncol(training)+1)])
  }
  return(out)
}

###
#FUNCTION: create a balanced dataset undersampling controls (Y=0) without replacement
undersample.controls <- function(dataset){
  #case/controls
  case <- filter(dataset, dataset[classColumnLabel] == 1) 
  controls <- filter(dataset, dataset[classColumnLabel] == 0)
  
  #sampling 
  controls.undersampled <- controls[sample(nrow(controls), 
                                           size = nrow(case),
                                           replace = FALSE),]
  #each level of each unbalanced.var is sampled (at least once)
  unbalanced.factors.sampling <- sample.unbalanced.factors(dataset, unbalanced.var)
  
  #balanced dataset
  df.balanced <- rbind(case, controls.undersampled, unbalanced.factors.sampling) %>%
    select(-all_of(dateColumnLabel))
  
  df.balanced <-df.balanced[sample(nrow(df.balanced)),]
  return(df.balanced)
}

###
#FUNCTION: bootstrapping (sampling with replacement)
bootstrapping <- function(dataset) {
  return(dataset[sample(nrow(dataset),
                        size = nrow(dataset),
                        replace = TRUE), ])
}

get.trainingSet.samples <-function(dataset){
  training.samples<-list()
  for(s in 1:sampling.num){
    sample <- ifelse(flagValidation, list(bootstrapping(dataset)), list(undersample.controls(dataset)))
    training.samples<-append(training.samples, sample)
  }
  names(training.samples)<- sprintf("sample%s",seq(1:sampling.num))
  return(training.samples)
}


get.trainingSet.by.range <-function(batchStart, batchEnd){
  training.data<-data.frame()
  for (b in batchStart:batchEnd){
    training.data<-rbind(training.data, batches[[b]]$data)
  }
  return(training.data)
}

set.trainingSet<-function(drift.detection){
  cat("BUILDING", sampling.num ,"TRAINING SETS - sampling from interval", 
      as.character(batches[[drift.detection$batchStart]]$dateStart), "to", as.character(batches[[drift.detection$batchEnd]]$dateEnd), "\n")
  trainingSet<-get.trainingSet.by.range(drift.detection$batchStart, drift.detection$batchEnd)
  trainingSet.samples<-get.trainingSet.samples(trainingSet)
  return(trainingSet.samples)
}


########
# MODEL TRAIN
########

train.model<-function(drift.detection){
  fittedModel<- vector("list", sampling.num)
  names(fittedModel)<-sprintf("model%s", seq(1:sampling.num))
  cat("TRAINING MODEL", toupper(drift.detection$model), "\n")
  for(s in 1:sampling.num){
    trainingdata<-drift.detection$trainingSet[[s]]
      switch (drift.detection$model,
        "lr" = {
          fittedModel[[s]] <- glm(
            as.formula(paste(classColumnLabel, "~.")),
            data = trainingdata, 
            family = binomial, maxit = 100)
        },
        "nb" = {
          fittedModel[[s]] <- naiveBayes(
            as.formula(paste(classColumnLabel, "~.")),
            data = trainingdata
          )
        },
        "nn" = {
          fittedModel[[s]] <- nn.training(trainingdata, 30, s)
        }, 
        "rf" = {
          fittedModel[[s]] <- randomForest(as.formula(paste(classColumnLabel, "~.")),
                                           data = trainingdata, 
                                           ntree = ifelse(dataset=="COVID",200,500))
        },
        {
          stop('MODEL NOT FOUND')
        }
      )
  }
  return(fittedModel)
}


#######
# PREDICTIONS 
#######

filterData.byBias<-function(data.to.filter){
  data<- data.to.filter
  if(!flagValidation) {data <- select(data, -all_of(dateColumnLabel))}
  if(drift.detection$bias!="NOBIAS"){
    bias.record<-bias.exploration[which(bias.exploration$label==drift.detection$bias),]
    return(data[which(data[bias.record$variable]==bias.record$level),])
  } 
  return(data)
}

#@set %in% c("controlSet", "nextTestSet")
get.predictions.prob<-function(drift.detection, set){
  predictions.prob<-list()
  batchID<-drift.detection[[set]]
  newdata<-filterData.byBias(batches[[batchID]]$data)
  for(s in 1:sampling.num){
    fitted.model<-drift.detection$fittedModel[[s]]
    switch (drift.detection$model,
            "lr" = {
              predictions.prob[[s]]<-predict(fitted.model, 
                                             newdata, 
                                             type = "response")
            },
            "nb" = {
              predictions.prob[[s]]<-predict(fitted.model, 
                                             newdata, 
                                             type = "raw")[, 2]
            },
            "nn" = {
              predictions.prob[[s]]<-nn.predict(fitted.model, 
                                                newdata,
                                                get.min.max(drift.detection$trainingSet[[s]]))
            }, 
            "rf" = {
              predictions.prob[[s]]<- predict(fitted.model, 
                                              newdata, 
                                              type = "prob")[, 2]
            },
            {
              stop('MODEL NOT FOUND')
            }
    )
    
  }
  return(predictions.prob)
}

#####
# METRIC COMPUTATION
####

optimal.threshold <- function(predict, response, W.FN, W.FP) {
  r <- pROC::roc(response, as.vector(predict), direction="<", quiet="TRUE")
  th <- r$thresholds[is.finite(r$thresholds)] #-Inf, +Inf
  return(th[which.min(W.FN*(1-r$sensitivities) + W.FP*(1-r$specificities))])
}

get.nextTestSet.output<-function(drift.detection){
  return(drift.detection$nextTestSetOutput)
}

get.metrics <-function(drift.detection, set){
  cat("COMPUTING", toupper(drift.detection$metric), "ON", set, "- batch", get.batchName(drift.detection[[set]]), "\n")
  batchID<-drift.detection[[set]]
  trueClass<-filterData.byBias(batches[[batchID]]$data)[[classColumnLabel]]
  predictions.prob<-get.predictions.prob(drift.detection, set)
  
  output<-c()
  for(s in 1:sampling.num){
    #ensure class predicted has 2 levels
    predClass<-as.factor(ifelse(predictions.prob[[s]]>0.5,1,0))
    levels(predClass)<-levels(trueClass)
    switch (drift.detection$metric,
            "accuracy" = {
              output<-c(output,
                        as.numeric(
                          confusionMatrix(data=predClass, 
                                          reference=trueClass,
                                          positive="1")$overall["Accuracy"])
              )
            },
            "sensitivity" = {
              output<-c(output,
                        as.numeric(
                          confusionMatrix(data=predClass, 
                                          reference=trueClass,
                                          positive="1")$byClass["Sensitivity"])
              )
            },
            "specificity" = {
              output<-c(output,
                        as.numeric(
                          confusionMatrix(data=predClass, 
                                          reference=trueClass,
                                          positive="1")$byClass["Specificity"])
              )
            },
            "recall" = {
              output<-c(output,
                        as.numeric(
                          confusionMatrix(data=predClass, 
                                          reference=trueClass,
                                          mode = "everything",
                                          positive="1")$byClass["Recall"])
              )
            },
            "precision" = {
              output<-c(output,
                        as.numeric(
                          confusionMatrix(data=predClass, 
                                          reference=trueClass,
                                          mode = "everything",
                                          positive="1")$byClass["Precision"])
              )
            },
            "F1" = {
              output<-c(output,
                        as.numeric(
                          confusionMatrix(data=predClass, 
                                          reference=trueClass,
                                          mode = "everything",
                                          positive="1")$byClass["F1"])
              )
            },
            "auc" = {
              output<-c(output,
                        filter(
                          precrec::auc(evalmod(
                            scores = predictions.prob[[s]],
                            labels = trueClass)
                          ), curvetypes=="ROC")$aucs
              )
              
            },
            "prauc" = {
              output<-c(output,
                        filter(
                          precrec::auc(evalmod(
                            scores = predictions.prob[[s]],
                            labels = trueClass)
                          ), curvetypes=="PRC")$aucs
              )
            },
            "threshold" = {
              output<-c(output,
                        optimal.threshold(predict = predictions.prob[[s]], response = trueClass, W.FN=weight.FN, W.FP=weight.FP)
                        )
            },
            {
              stop('METRIC NOT FOUND')
            }
    )
  }
  return(output)
}


#######
# DRIFT DETECTION
#######

compare.distributions<-function(drift.detection){
  cat("#########\n")
  cat("COMPARING DISTRIBUTIONS: controlset batch", get.batchName(drift.detection$controlSet), "- nextTestSet batch", get.batchName(drift.detection$nextTestSet), "\n")
  labels <- rep(c("controlSet", "nextTestSet"), c(sampling.num, sampling.num))
  outputs<-c(as.numeric(format(round(drift.detection$controlSetOutput, 2), nsmall = 2)), 
             as.numeric(format(round(drift.detection$nextTestSetOutput, 2), nsmall = 2)))
  
  testDF <- data.frame(group=labels, metric=outputs)
  test <- wilcox.test(testDF$metric ~ testDF$group, alternative = "two.sided")
  
  validation.record <- data.frame(
    trainingSet=sprintf("%s to %s", batches[[drift.detection$batchStart]]$dateStart, batches[[drift.detection$batchEnd]]$dateEnd),
    controlSet=get.batchName(drift.detection$controlSet),
    nextTestSet=get.batchName(drift.detection$nextTestSet),
    p.value=test$p.value,
    alpha=drift.detection$alpha,
    changeDetect = !is.na(test$p.value) & test$p.value < as.numeric(drift.detection$alpha),
    direction = ifelse(mean(drift.detection$controlSetOutput) < mean(drift.detection$nextTestSetOutput), "UP", "DOWN"),
    differenceOfmean= round(mean(drift.detection$controlSetOutput) - mean(drift.detection$nextTestSetOutput), digits = 4),
    nextTestSetMean= mean(drift.detection$nextTestSetOutput)
  )
  if(validation.record$changeDetect) cat(red$underline$bold("DRIFT DETECTED!\n")) else cat(green$underline$bold("DRIFT NOT DETECTED!\n"))
  cat("#########\n")
  return(rbind(drift.detection$results$comparison, validation.record))
}


set.plot.records<-function(drift.detection, set){
  plot.records<-data.frame()
  outputSet<-ifelse(set=="controlSet", "controlSetOutput", "nextTestSetOutput")
  plot.records<-rbind(plot.records,
                      data.frame(
                          trainingSet=rep(sprintf("%s to %s", 
                                                  batches[[drift.detection$batchStart]]$dateStart, 
                                                  batches[[drift.detection$batchEnd]]$dateEnd)),
                          testOn=rep(get.batchName(drift.detection[[set]])),
                          metric=drift.detection[[outputSet]]
                      ))
  return(rbind(drift.detection$results$plot, plot.records))
}

#POST-MARKET SURVEILLANCE
check.drifts<-function(drift.detection, alpha.value){
  comparison<-drift.detection$results$comparison
  metric.distributions<-split(drift.detection[["results"]]$plot, drift.detection[["results"]]$plot$testOn)
  for(i in 2:length(metric.distributions)){
    labels <- rep(c("controlSet", "nextTestSet"), c(sampling.num, sampling.num))
    outputs<-c(metric.distributions[[1]]$metric, metric.distributions[[i]]$metric)
    
    cat("#########\n")
    cat("COMPARING", toupper(drift.detection$metric),"DISTRIBUTIONS", paste0("(alpha=", alpha.value,"): controlset batch"), unique(metric.distributions[[1]]$testOn), "- nextTestSet batch", unique(metric.distributions[[i]]$testOn), "\n")
    
    testDF <- data.frame(group=labels, metric=outputs)
    test <- wilcox.test(testDF$metric ~ testDF$group, alternative = "two.sided")
    
    validation.record <- data.frame(
      trainingSet=sprintf("%s to %s", batches[[drift.detection$batchStart]]$dateStart, batches[[drift.detection$batchEnd]]$dateEnd),
      controlSet=unique(metric.distributions[[1]]$testOn),
      nextTestSet=unique(metric.distributions[[i]]$testOn),
      p.value=test$p.value,
      alpha=alpha.value,
      changeDetect = !is.na(test$p.value) & test$p.value < as.numeric(alpha.value),
      direction = ifelse(mean(metric.distributions[[1]]$metric) < mean(metric.distributions[[i]]$metric), "UP", "DOWN"),
      differenceOfmean= round(mean(metric.distributions[[1]]$metric) - mean(metric.distributions[[i]]$metric), digits = 4),
      nextTestSetMean= mean(metric.distributions[[i]]$metric)
    )
    if(validation.record$changeDetect) cat(red$underline$bold("DRIFT DETECTED!\n")) else cat(green$underline$bold("DRIFT NOT DETECTED!\n"))
    comparison <- rbind(comparison, validation.record)
  }
  return(comparison)
}


#######
# SHOW RESULTS
#######

get.confidenceIntervals<-function(plot.records){
  confidence.intervals <- plot.records %>%
    group_by(trainingSet, testOn) %>%
    summarise(
      mean = mean(metric),
      lci = if(var(metric)==0) mean(metric)
      else t.test(metric, conf.level = 0.95)$conf.int[1]
      ,
      uci = if(var(metric)==0) mean(metric)
      else t.test(metric, conf.level = 0.95)$conf.int[2]
      , .groups = "drop"
    )  %>%
    arrange(testOn)
  return(confidence.intervals)
}


set.title <- function(model){
  switch (model,
          "nb" = {return("Naive Bayes")},
          "lr" = {return ("Logistic Regression")},
          "tan" = {return("Tree-Augmented Naive")},
          "nn" = {return ("Neural network")},
          "rf" = {return ("Random forest")}
  )
}

set.bias.title<-function(bias){
  bias.record<-bias.exploration[which(bias.exploration$label==bias),]
  switch (bias.record$label,
    "NOBIAS" = {
      return("NONE")
    },
    {
      return(sprintf("on var.%s-level: %s", toupper(bias.record$variable), toupper(bias.record$level)))
    }
  )
}

set.alpha.title<-function(alpha){
  return(paste0(as.numeric(alpha)*100, "%"))
}

set.metric.title<-function(metric){
  switch (metric,
    "threshold" = {
      return (sprintf("threshold-weights:%s(FP),%s(FN)", weight.FP, weight.FN))
    },
    {
      return (metric)
    }
  )
}

plot.driftDetection<-function(drift.detection){
  confidence.intervals<-get.confidenceIntervals(drift.detection$results$plot)
  g <- 
    ggplot(data = confidence.intervals, aes(y = mean, x = testOn, color=trainingSet))+
    geom_ribbon(aes(x= testOn, ymin=lci, ymax=uci, group=trainingSet, fill=trainingSet), alpha = 0.1, show.legend = FALSE)+
    geom_line(aes(group=1))+
    geom_point(size = 1.5)+
    scale_color_manual(
      name = "Training set range",
      values = viridis(length(unique(confidence.intervals$trainingSet)))
    )+
    scale_fill_manual(
      name = "Training set range",
      values = viridis(length(unique(confidence.intervals$trainingSet)))
    )+
    scale_y_continuous(breaks = seq(0, 1, by = 0.10))+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.9, hjust=1, size=11, 
                                     color=ifelse(annotations$ConceptDriftPosition==0, "black", "red")),
          axis.text.y = element_text(size=11),
          axis.title.x = element_text(margin = ggplot2::margin(t = 10)),
          axis.title.y = element_text(vjust=2.5, size=12),
          plot.title = element_text(size=15, face="bold"), #, hjust = 0.5),
          plot.subtitle = element_text(size=11),
          legend.title = element_text(size=11),
          legend.text = element_text(size=11)) +
    geom_vline(
      xintercept = subset(drift.detection$results$comparison, changeDetect==TRUE & direction=="DOWN")$nextTestSet,
      linetype="dashed",
      color='red') +
    geom_vline(
      xintercept = subset(drift.detection$results$comparison, changeDetect==TRUE & direction=="UP")$nextTestSet,
      linetype="dashed",
      color="#117733") +
    labs(title= sprintf("Concept drift detection"),
         subtitle = paste0("Model: ", set.title(drift.detection$model),"\n",
                           "Bias check: ", set.bias.title(drift.detection$bias), "\n",
                           "Metric :", set.metric.title(drift.detection$metric), "\n",
                           "Alpha: ", set.alpha.title(drift.detection$alpha),"\n",
                           "Updating: ", drift.detection$update, "\n",
                           "Drifts: ", drift.detection$type, "\n"))+
    xlab("")+
    ylab(drift.detection$metric)+
    coord_cartesian(ylim = c(min(confidence.intervals$mean, confidence.intervals$lci, confidence.intervals$uci)-0.1,
                             max(confidence.intervals$mean, confidence.intervals$lci, confidence.intervals$uci)+0.1))
  return(g)
}

order.drifts.byStrength <-function(drift.detection){
  return (arrange(subset(drift.detection$results$comparison, changeDetect==TRUE),
                  desc(abs(differenceOfmean))
                  )$nextTestSet)
}

get.fileName<-function(drift.detection){
  return(paste0("results/",
               dataset, "/",
               toupper(drift.detection$bias), "/",
               toupper(drift.detection$metric), "/",
               #"alpha", drift.detection$alpha, "/",
               "Run", r, 
               "_model", toupper(drift.detection$model), 
               "_metric", toupper(drift.detection$metric),
               "_alpha", drift.detection$alpha, 
               ".png", sep=""))
}

save.plot<-function(drift.detection){
  drift.plot<-suppressWarnings(plot.driftDetection(drift.detection))
  ordered.drifts<-order.drifts.byStrength(drift.detection)
  
  png(file=get.fileName(drift.detection), width=750, height=437)
  if(length(ordered.drifts)!=0){
    print(drift.plot + 
            annotate(x=ordered.drifts,
                     y=+Inf,
                     label=as.character(c(1:length(ordered.drifts))),vjust=2,geom="label"))
  } else {
    print(drift.plot)
  }
  dev.off()
  return("PLOT SAVED!")
}


#########
# VALIDATION TABLE 
#########

validationTableObj.initialization<-function(){
  validation.table.obj<- replicate(nrow(bias.exploration),
                            vector("list", length(metrics.names)),
                            simplify = FALSE)
  names(validation.table.obj) <- bias.exploration$label
  for(bias in bias.exploration$label){
    names(validation.table.obj[[bias]])<-metrics.names
    for(metric in metrics.names){
      validation.table.obj[[bias]][[metric]]<-list()
      for(alpha in alphas){
        validation.table.obj[[bias]][[metric]][[alpha]]<-data.frame(batches=names(batches),
                                                        annotations=annotations$ConceptDriftPosition)
      }
    }
  }
  return(validation.table.obj)
}



get.validationTable.record<-function(drift.detection){
  label <- paste("Model:", toupper(drift.detection$model), 
                 "-Run", r, 
                 ",alpha=", drift.detection$alpha, 
                 sep="")
  #default
  new.column<-data.frame(c("First train set", "First test set", rep(FALSE, length(batches)-2)))
  colnames(new.column)<-label
  
  detections<-subset(drift.detection$results$comparison, changeDetect==TRUE & alpha==drift.detection$alpha)
  if(nrow(detections)!=0) {
    new.column[which(names(batches) %in% detections$nextTestSet),]<-paste0(detections$changeDetect,
                                                                          "-", detections$direction,
                                                                          "(",round(detections$nextTestSetMean,digits = 2),
                                                                          ",d:", round(detections$differenceOfmean,digits = 2), ")")
  }
  return(new.column)
}

update.validationTableObj<-function(drift.detection, validation.table.obj){
  new.column<-get.validationTable.record(drift.detection)
  previous.table<-validation.table.obj[[drift.detection$bias]][[drift.detection$metric]][[drift.detection$alpha]]
  
  updated.table<-cbind(previous.table, 
                       new.column)
  return(updated.table)
}


#########
# UPDATES
#########
increase.set<-function(drift.detection, set){
  outputSet<-ifelse(set=="controlSet", "controlSetOutput", "nextTestSetOutput")
  updated.drift.detection<-drift.detection
  
  updated.drift.detection[[set]]<-drift.detection[[set]]+1
  updated.drift.detection[[outputSet]]<-c()
  
  return(updated.drift.detection)
}


update <- function(drift.detection){
  updated.drift.detection<-drift.detection
  
  if(drift.detection$update=="forget" & !is.na(drift.detection$lastDriftBatch)) updated.drift.detection$batchStart <-drift.detection$lastDriftBatch
  updated.drift.detection$batchEnd <-drift.detection$nextTestSet-1
  updated.drift.detection$lastDriftBatch<-drift.detection$nextTestSet
  
  updated.drift.detection$controlSet <-updated.drift.detection$batchEnd+1
  updated.drift.detection$nextTestSet<- updated.drift.detection$controlSet+1
  
  #set up new training set and model
  updated.drift.detection<-clean.model(updated.drift.detection)
  
  return(updated.drift.detection)
}

clean.model<-function(drift.detection){
  updated.drift.detection<-drift.detection
  
  updated.drift.detection$controlSetOutput<-c()
  updated.drift.detection$nextTestSetOutput<-c()
  updated.drift.detection$trainingSet<-list()
  updated.drift.detection$fittedModel<-NULL
  
  return(updated.drift.detection)
}


flag.changeDetect<-function(drift.detection){
  if(dim(drift.detection$results$comparison)[2]==0){
    return(FALSE)
  }
  return(tail(drift.detection$results$comparison$changeDetect, n=1))
}

flag.firstTrain<-function(drift.detection){
  if(drift.detection$nextTestSet==3){
    return(TRUE)
  }
  return(FALSE)
}


########
# SUMMARIZED RESULTS
#######

###
#FUNCTION: parsing strings and return a number 
parsing_string <- function(s) as.numeric(sub(".*?([-+]?\\d*\\.?\\d+).*", "\\1", s))

###
#FUNCTION: computes the mean of the drift detected over the runs and generates excels files with the results
#ATTENTION: careful with the ordering of the columns by model name (alphabetical order)
write.results<-function(){
  validation.table.obj.sum<-validationTableObj.initialization()
  cat("Generating files excel with the results..\n")
  rows.to.skip <-2
  cols.to.skip <-2
 for (bias in bias.exploration$label){
   for (metric in metrics.names){
      #create a workbook
      wb <- createWorkbook()
      for (alpha in alphas){
        #order the column of the results
        current.table<-validation.table.obj[[bias]][[metric]][[alpha]][ , order(names(validation.table.obj[[bias]][[metric]][[alpha]]))]
        validation.table.obj[[bias]][[metric]][[alpha]]<-current.table
        
        addWorksheet(wb, paste0("Results_", metric, "alpha_", alpha))
        addWorksheet(wb, paste0("Summ.res_", metric, "alpha_", alpha))
        
        writeData(wb, paste0("Results_", metric, "alpha_", alpha), current.table, startRow = 1, startCol = 1)
        
        for (k in 1:length(models.names)){
          out<-c()
          col.start <- (cols.to.skip+1)+run.num*(k-1)
          col.end <- col.start+(run.num-1)
          
          for (row in (rows.to.skip+1):length(batches)){
            x<-current.table[row, col.start:col.end] 
            x.drifts <-gsub('.$', '', x[grepl("TRUE", x, fixed=TRUE)])
            drift.detected<-length(x.drifts)
            mean.difference <- ifelse(drift.detected!=0, round(mean(as.numeric(gsub(".*d:", '', x.drifts))), digits = 2) ,0)
            mean <- ifelse(drift.detected!=0, round(mean(as.numeric(gsub(".*\\((.*),.*", '\\1', x.drifts))), digits = 2) ,0)
            out <- c(out, paste0(round((drift.detected/run.num)*100, digits = 2), "%",
                                 if(drift.detected!=0) {
                                   paste0("-",
                                          if(mean.difference<0) paste0("UP", paste0("(", mean, ")"))  
                                          else if (mean.difference>0) paste0("DOWN", paste0("(", mean, ")"))  
                                   )
                                 } else {""}
            )
            )
          }
          label <- paste("Model:", models.names[k], sep="")
          validation.table.obj.sum[[bias]][[metric]][[alpha]][label] <- c("First training set", "First test set", out)
        }
        
        writeData(wb, paste0("Summ.res_", metric, "alpha_", alpha), validation.table.obj.sum[[bias]][[metric]][[alpha]], startRow = 1, startCol = 1)
      }
      saveWorkbook(wb, 
                   file = paste("results/",
                                dataset, "/",
                                bias, "/",
                                metric, "_", 
                                bias, ".xlsx", sep=""), 
                   overwrite = TRUE)
      rm(wb)
    }
  }
  cat("FILES GENERATED!\n")
  return(validation.table.obj.sum)
}
