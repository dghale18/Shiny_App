library(shiny)
library(ggplot2)
library(dplyr)
library(babynames)
library(stringr)



# Access via https://doma-ghale.shinyapps.io/BabyNames/


babyname_1 <- babynames
babyname_1$year <- as.numeric(as.character(babyname_1$year))
babyname_1$name <- tolower(babyname_1$name)

ui <- fluidPage(
  titlePanel("View baby name popularity"),
  
  sidebarLayout(
    sidebarPanel(

      
      textInput("name", 
                "Baby name(s)",
                 ""),
      
      selectInput("sex", 
                  "Sex", 
                  list("Male", "Female", "Combined")),
      
      checkboxInput("n", 
                    "Plot number instead of proportion", 
                    FALSE),  
      
      sliderInput("year", 
                  label = "Year Range",
                  min = min(babyname_1$year), 
                  max = max(babyname_1$year), 
                  value = c( min(babyname_1$year), max(babyname_1$year))
 )
    	),
    
    mainPanel(
      plotOutput("lineplot")
    )
  )
)



server <- function(input, output) {
  
  
  output$lineplot <- renderPlot({
    
  	

  # create a dataframe with new sex category "Combined" 
    
    
    if(input$sex == "Male"){
      sex_str = "M"
    } else if (input$sex == "Female"){
      sex_str = "F"
    } else {
      sex_str = c("M", "F")
    }
    
    # multiple names using string_r  
    # 
    curr_names <- filter(babyname_1, 
                         name %in% str_trim(strsplit( tolower(input$name), "," )[[1]] ),  
                         sex == sex_str)
    

    
  # changing the y axis value and label    
    
    if(input$n == FALSE){
      curr_names <- curr_names %>%
        group_by(name, year) %>%
        mutate(y_val = mean(prop))
      y_lab <- "Proportion of babies"
    } else{
      curr_names <- curr_names %>% 
        group_by(name, year) %>%
        mutate(y_val = sum(n))
      y_lab <- "Number of babies"
    }

    
    curr_names_2 <- curr_names %>% 
    	filter( year >= input$year[1] & year <= input$year[2])
    
    
      
    ggplot(curr_names_2, aes(year, y_val)) + 
      geom_line(aes(color = name)) +
      ylab(y_lab) + 
      xlab("Year")
    
    
    
    
  })  # end renderPlot
  
}  # end server
  
  
  
  shinyApp(ui = ui, server = server)
  
