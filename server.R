library(shiny)
library(rjson)
library(ggplot2)

getData = function(input) {
  # parse data, convert to data frame, make everything numeric
  data = as.data.frame(do.call('rbind', fromJSON(input$shotData)));
  data$x = as.numeric(data$x);
  data$y = as.numeric(data$y);
  data$attempts = as.numeric(data$attempts);
  data$made = as.numeric(data$made);
  
  # calculate length from hoop
  data$distance = round(sqrt(abs(data$x - 25)^2 - abs(data$y - 5.25)^2));
  
  # aggregate
  attemptsData = aggregate(attempts ~ distance, data=data, sum);
  makesData = aggregate(made ~ distance, data=data, sum);
  aggrData = merge(attemptsData, makesData, by=c('distance'));
  aggrData$accuracy = aggrData$made/aggrData$attempts;
  
  return(aggrData);
};

shinyServer(
  function(input, output) {
    output$frequencyByDistance <- renderPlot({
      aggrData = getData(input);
      
      # plot graph
      qplot(x=distance, y=attempts, data=aggrData, geom="bar", stat="identity") + 
        ggtitle("Shot Frequency By Distance");
    })
    
    output$accuracyByDistance <- renderPlot({
      aggrData = getData(input);
      
      # plot graph
      qplot(x=distance, y=accuracy, data=aggrData, geom="bar", stat="identity") + 
        ggtitle("Shot Accuracy By Distance");
    })
  }
)