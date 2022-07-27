###STRUCTURE-BASED AND UNSUPERVISED-BASED DRIFT DETECTION FUNCTIONS

########
# MAIN FUNCTIONS
########

## 1.STRUCTURE-BASED FUNCTION
#compute feature importance of a dataset
get.featureImportance<-function(dataset, class){
  if(!is.na(dateColumnLabel)){dataset <- dataset %>% select(-dateColumnLabel)}
  model <- randomForest(as.formula(paste(class, "~.")),
                        data = dataset, ntree=75)
  i<-varImp(model) %>% mutate(Overall=Overall/sum(Overall))
  return(i)
}

## 2.DISTRIBUTION-BASED FUNCTIONS
#compute univariate distribution proportion for each range of data
get.variableProportion <- function(variableData){
  p <- as.data.frame(prop.table(table(variableData))) %>% tibble::column_to_rownames("variableData")
  return(p)
}

## 3.CORRELATION-BASED FUNCTIONS
#compute correlation-matrix distances
get.features.byType<-function(data){
  data<-select(data, -all_of(classColumnLabel))
  factors.cols <- names(Filter(is.factor, data))
  #categorical
  categorical.cols<-c()
  factors.levels <- sapply(data[,factors.cols], levels)
  for (i in 1:length(factors.levels)){
    if(length(factors.levels[[i]])>2){
      categorical.cols<-append(categorical.cols, names(factors.levels[i]))
    }
  }
  #binary
  binary.cols <- setdiff(factors.cols, categorical.cols)
  #continuous
  continuous.cols <- names(Filter(is.numeric, data))
  return(list(numerical=continuous.cols,
              categorical=categorical.cols,
              binary=binary.cols))
}

get.correlationMatrix<-function(data, type){
  switch (type,
          "numerical" = {
            corr.matrix<-cor(data, method = "pearson")
          },
          "categorical" = {
            corr.matrix<-char_cor(data)
          },
          "binary" = {
            corr.matrix<-tetrachoric(data.matrix(data))
          }
  )
  return(corr.matrix)
}

get.correlationDistances<-function(batches){
  distances<-c()
  variables.byType<-get.features.byType(df)
  
  for(i in 1:length(batches)){
    data<- select(get.trainingSet.by.range(1,i), -all_of(classColumnLabel))
    numerical.data <- data %>% select(variables.byType$numerical)
    cat.data <- data %>% select(variables.byType$categorical) 
    binary.data<- data %>% select(variables.byType$binary) 
    
    if(i==1){
      ref.corrMatrix.num <- ifelse(length(variables.byType$numerical)!=0, get.correlationMatrix(numerical.data, "numerical"), NaN)
      ref.corrMatrix.cat <- get.correlationMatrix(cat.data, "categorical")
      ref.corrMatrix.binary<- ifelse(length(variables.byType$binary)!=0, get.correlationMatrix(binary.data, "binary")[[1]], NaN)
    } else{
      current.corrMatrix.num <- ifelse(length(variables.byType$numerical)!=0, get.correlationMatrix(numerical.data, "numerical"), NaN)
      current.corrMatrix.cat <- get.correlationMatrix(cat.data, "categorical")
      current.corrMatrix.binary<-ifelse(length(variables.byType$binary)!=0, get.correlationMatrix(binary.data, "binary")[[1]], NaN)
      
      distance.num<-ifelse(length(variables.byType$numerical)!=0, sum(abs(current.corrMatrix.num-ref.corrMatrix.num))/2, 0)
      distance.cat<-sum(abs(current.corrMatrix.cat -ref.corrMatrix.cat))/2
      distance.binary<-ifelse(length(variables.byType$binary)!=0, sum(abs(current.corrMatrix.binary -ref.corrMatrix.binary))/2, 0)
      
      distances <- c(distances, distance.num + distance.cat+distance.binary)
    }
  }
  return(data.frame(sample=names(batches)[-1], distance=distances))
}


########
# COMPUTE ANALYSIS AND DISTANCES
########

append.metric <-function(metric.df, newMetric){
  return(if(is_empty(metric.df)) 
    {newMetric} 
    else{cbind(metric.df, newMetric)})
}

#compute features importance (@variableName=NULL) or feature distribution analysis (@variableName=featureName)
get.metric.df <- function(batches, variableName=NULL){
  metric.df<-data.frame()
  for (j in 1:length(batches)){
    if(is.null(variableName)){
      data<-get.trainingSet.by.range(1,j)
      newMetric<- get.featureImportance(data, classColumnLabel)
    } else {
      data<-select(get.trainingSet.by.range(1,j), variableName)
      newMetric<- get.variableProportion(data)
    }
    metric.df<-append.metric(metric.df, newMetric)
  }
  names(metric.df)<-names(batches)
  return(metric.df)
}

#compute eucledian distances between two vectors
euclidean <- function(a, b) sqrt(sum((a - b)^2))

#compute eucledian distances between the reference vector and the following ones
get.distances<-function(metric.df){
  out.distance <- c()
  for(i in 2:ncol(metric.df)){
    d<-euclidean(metric.df[,1], metric.df[,i])
    out.distance<-c(out.distance,d)
  }
  return(data.frame(sample=names(metric.df)[-1], distance=out.distance))
}


########
# PLOT RESUTLS
########

#1. PLOT FEATURE IMPORTANCE
plot.featureImportance<-function(featureImportance, upperBoundBatch){
  importance<-featureImportance %>% tibble::rownames_to_column("var") %>% arrange(desc(Overall)) %>% slice(1:9) 
  p <- ggplot(importance, aes(x=reorder(var, Overall), y=Overall)) + 
    geom_point(size=2, color="brown1") +
    theme_light()+
    theme(axis.text.y = element_text(size=15),
          plot.title = element_text(face="bold")) +
    geom_segment(aes(x=var,xend=var,y=0,yend=Overall), color="brown1", size = 0.7) +
    labs(title="Feature influence",
         subtitle = paste("Model trained up to batch:", upperBoundBatch),
         x ="", y = "Importance")+
    coord_flip()
    return(p)
}

plot.featureImportance.df<-function(featureImportance.df){
  #plot feature importances
  for(i in 1:ncol(featureImportance.df)){
    featureImportance <- data.frame(Overall=featureImportance.df[,i])
    rownames(featureImportance)<-rownames(featureImportance.df)
    
    png(file=paste0("Results/",
                    toupper(dataset), "/", "featureInfluence_",
                    i, #names(featureImportance.df)[i], 
                    ".png"), 
        width=350, height=437)
    print(plot.featureImportance(featureImportance, names(featureImportance.df)[i]))
    dev.off()
  }
}

#2. PLOT DISTANCE FROM REFERENCE MODEL 
# @metricName in {featureImportance, distribution, correlation}
plot.distances<-function(distances, metricName, variableName=NULL){
  custom.color<- 
    switch (metricName,
      "featureImportance" = {"dodgerblue1"},
      "distribution" = {"chartreuse4"},
      "correlation" = {"darkorchid3"},
      {
        "gray30"
      }
    )
  custom.title<- 
    switch (metricName,
            "featureImportance" = {"Features importance changes"},
            "distribution" = {paste(variableName,"Distribution changes")},
            "correlation" = {"Correlation matrix changes"},
            {"Area under precision-recall curve changes"}
    )
  
  ggplot(data=distances, aes(x=sample, y=distance, group=1)) +
    geom_line(color=custom.color, size=0.7)+
    geom_point(color=custom.color, size=1.7)+
    geom_rect(
      aes(
        xmin = "",
        xmax = names(batches)[1],
        ymin = -Inf,
        ymax = 1
      ),
      fill = custom.color,
      alpha = 0.03
    ) +
    xlim("", names(batches)[1], distances$sample)+
    theme(
      #axis.text.x = element_text(angle = 90, vjust = 0.9, hjust=1),
      legend.position = "none",
      panel.grid.major.x = element_line(
        color =ifelse(c(0, 
                        (as.numeric(annotations$ConceptDriftPosition)+as.numeric(annotations$ConceptDriftWindow))-2) == 0, 
                      "gray60", "brown1"),
        size = 0.4,
        linetype = ifelse(c(0, as.numeric(annotations$ConceptDriftPosition)-1) == 1, "solid", 
                          ifelse(c(0,as.numeric(annotations$ConceptDriftWindow)-1) == 1 || !any(annotations$ConceptDriftWindow==1), "dotted", "dashed"))
      ))+
    coord_cartesian(ylim = c(min(distances$distance), #- 0.05, 
                             max(distances$distance))
                             )+
    labs(title=custom.title,
         #subtitle = paste("dataset:", dataset),
          x = "", #batches (#instances)", 
          y = "distance")
}




