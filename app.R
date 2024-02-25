#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(ggplot2)
library(shiny)
library(shiny)
library(shinyalert)


#data frames
major_df = read_csv("enrollment_by_gender.csv")
names(major_df) = major_df[1,]
major_df = major_df[-1,]
major_df = pivot_longer(major_df, cols = c(Female, Male), names_to = "Gender", values_to = "Count")
colnames(major_df)[which(names(major_df) == "Plan School/College")] = "College"
colnames(major_df)[which(names(major_df) == "Academic Plan")] = "Major"
colnames(major_df)[which(names(major_df) == "Grand Total")] = "Total"
colnames(major_df)[which(names(major_df) == "Count")] = "Students"


race_df = read_csv("enrollment_by_race.csv")
names(race_df) = race_df[1,]
race_df = race_df[-1,]
colnames(race_df)[which(names(race_df) == "Plan School/College")] = "College"
colnames(race_df)[which(names(race_df) == "Academic Plan")] = "Major"
colnames(race_df)[which(names(race_df) == "Grand Total")] = "Total"
colnames(race_df)[which(names(race_df) == "Am. Indian")] = "Indigenous"
colnames(race_df)[which(names(race_df) == "2 or more races")] = "Multiple"
colnames(race_df)[which(names(race_df) == "Intl.")] = "International"
race_df = pivot_longer(race_df, cols = c(Hispanic, Black, Indigenous, Asian, Hawaiian, White, Multiple, Unknown, International), names_to = "Race_Ethnicity", values_to = "Students")
race_df = race_df[-(1:9), ]


level_df = read_csv("enrollment_by_grade.csv")
names(level_df) = level_df[2,]
level_df = level_df[-(1:3),]
colnames(level_df)[which(names(level_df) == "Plan School/College")] = "College"
colnames(level_df)[which(names(level_df) == "Academic Plan")] = "Major"
level_df = pivot_longer(level_df, cols = c(Freshman, Sophomore, Junior, Senior), names_to = "Year", values_to = "Students")


enrollment_by_gender = read_csv("enrollment_by_gender.csv", show_col_types = FALSE)
names(enrollment_by_gender) = enrollment_by_gender[1,]
enrollment_by_gender = enrollment_by_gender[-(1:2),]
colnames(enrollment_by_gender)[which(names(enrollment_by_gender) == "Academic Plan")] = "Major"
colnames(enrollment_by_gender)[which(names(enrollment_by_gender) == "Plan School/College")] = "College"
colnames(enrollment_by_gender)[which(names(enrollment_by_gender) == "Grand Total")] = "Total"
enrollment_by_gender = select(enrollment_by_gender, -Unknown, -Other)


enrollment_by_race = read_csv("enrollment_by_race.csv", show_col_types = FALSE)
names(enrollment_by_race) = enrollment_by_race[1,]
colnames(enrollment_by_race)[which(names(enrollment_by_race) == "Academic Plan")] = "Major"
colnames(enrollment_by_race)[which(names(enrollment_by_race) == "Plan School/College")] = "College"
colnames(enrollment_by_race)[which(names(enrollment_by_race) == "Am. Indian")] = "Indigenous"
colnames(enrollment_by_race)[which(names(enrollment_by_race) == "2 or more races")] = "Multiple"
colnames(enrollment_by_race)[which(names(enrollment_by_race) == "Intl.")] = "International"
colnames(enrollment_by_race)[which(names(enrollment_by_race) == "Grand Total")] = "Total"
enrollment_by_race = enrollment_by_race[-(1:2),]



enrollment_by_grade = read_csv("enrollment_by_grade.csv", show_col_types = FALSE)
names(enrollment_by_grade) = enrollment_by_grade[2,]
enrollment_by_grade = enrollment_by_grade[-(1:3),]
colnames(enrollment_by_grade)[which(names(enrollment_by_grade) == "Academic Plan")] = "Major"
colnames(enrollment_by_grade)[which(names(enrollment_by_grade) == "Plan School/College")] = "College"
enrollment_by_grade = select(enrollment_by_grade, -Masters, -College)


tuition = read_csv("tuition.csv", show_col_types = FALSE)
colnames(tuition)[which(names(tuition) == "Program Name")] = "Major"

four_year_plan = read_csv("four_year_plan.csv", show_col_types = FALSE)
names(four_year_plan) = four_year_plan[1,]
four_year_plan = four_year_plan[-(1:2),]
colnames(four_year_plan)[which(names(four_year_plan) == "Academic Plan")] = "Major"



#functions
gender_output = function(major) {
  data = major_df %>% filter(Major == major)
  data$perc = round((as.numeric(data$Students) / as.numeric(data$Total)), 2)
  data$label = paste0(data$Gender, "\nStudents: ", as.numeric(data$Students), ", ", as.numeric((data$perc) * 100), "%")
  data$max = cumsum(data$perc)
  data$min = c(0, head(data$max, n = -1))
  data$position = (data$max + data$min) / 2
  donut = ggplot(data, aes(ymax=max, ymin=min, xmax=4, xmin=3, fill=Gender)) +
    geom_rect() +
    coord_polar(theta="y") +
    xlim(c(2, 4)) +
    geom_label( x=3.5, aes(y=position, label=label), size=6) +
    scale_fill_brewer(palette=4) +
    scale_color_brewer(palette=4) +
    theme_void() +
    theme(legend.position = "none")
  return(donut)
}


level_output = function(major) {
  data = level_df %>% filter (Major == major)
  data$perc = round((as.numeric(data$Students) / as.numeric(data$Total)), 2)
  data$label = paste0(as.numeric((data$perc) * 100), "%")
  data$max = cumsum(data$perc)
  data$min = c(0, head(data$max, n = -1))
  data$position = (data$max + data$min) / 2
  pie = ggplot(data, aes(x="", y=as.numeric(Students), fill=Year))+
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start=0) +
    scale_fill_brewer(palette=4) +
    theme_void()
  return(pie)
}


race_output = function(major) {
  data = race_df %>% filter (Major == major)
  bar = ggplot (data = data, aes(x = Race_Ethnicity, y = as.numeric(as.character(Students)), fill=Race_Ethnicity)) +
    labs(x= "Race/Ethnicity", y="Number of Students", "Race/Ethnicity Distribution Based on Major")+
    geom_bar(stat="identity") +
    geom_text(aes(label=Students), vjust =-0.2)
  return(bar)
}


df_search = function(data, search_input){
  df =  data %>% filter (Major == search_input)
  return(df)
}


tuition_search = function(data, search_input) {
  if (grepl("&", search_input)){
    new_search = gsub("&", "and", search_input)
  }
  else{
    new_search = search_input
  }
  key = strsplit(new_search, " ")[[1]]
  final = paste(key[-length(key)], collapse = " ")
  instate_result = (data %>% filter (Major == final))["WI Resident"]
  outstate_result = (data %>% filter (Major == final))["Non-resident"]
  recip_result = (data %>% filter (Major == final))["MN Reciprocity"]
  
  instate_fees = paste("In-state tuition for this major is", instate_result, ".")
  outstate_fees = paste("Non-resident tuition for this major is", outstate_result, ".")
  recip_fees = paste("MN Reciprocity tuition for this major is", recip_result, ".")
  fees = list(instate_fees, outstate_fees, recip_fees)
  return(print(fees))
}


plan_search = function(data, search_input) {
  filtered = data %>% filter(Major == search_input)
  link = filtered$Link
  return(link)
}



#shiny web page
ui = fluidPage(
  titlePanel("Major Explorer"),
  sidebarLayout(
    sidebarPanel(
      helpText("Look up a major to get more information about it!"),
      selectInput("major", "Select a major:", choices = unique(race_df$Major)),
      actionButton("click", "GO")
    ),
    mainPanel(
      
      plotOutput("gender_plot"),
      plotOutput("race_plot"),
      plotOutput("level_plot"),
      tableOutput("gender_data"),
      tableOutput("race_data"),
      tableOutput("grade_data"),
      verbatimTextOutput("tuition_info"),
      htmlOutput("hyperlink")
      
    )
  )
)


server = function(input, output) {
  reactive_data = reactiveVal(list())
  reactive_rows = reactiveVal(list())
  money_list = reactiveVal(NULL)
  
  
  observeEvent(input$click, {
    input_val = input$major
    
    
    gender_chart = gender_output(input_val)
    race_chart = race_output(input_val)
    level_chart = level_output(input_val)
    reactive_data(list(gender_chart, race_chart, level_chart))
    
    gender_data = df_search(enrollment_by_gender, input_val)
    race_data = df_search(enrollment_by_race, input_val)
    grade_data = df_search(enrollment_by_grade, input_val)
    plan_link = plan_search(four_year_plan, input_val)
    reactive_rows(list(gender_data, race_data, grade_data, plan_link))
    
    money_info = tuition_search(tuition, input_val)
    money_list(money_info)
  })
  
  
  
  output$gender_plot = renderPlot({
    all_charts = reactive_data()
    if (!is.null(all_charts)) {
      print(all_charts[1])
    }
  })
  
  output$race_plot = renderPlot({
    all_charts = reactive_data()
    if(!is.null(all_charts)){
      print(all_charts[2])
    }
  })
  
  output$level_plot = renderPlot({
    all_charts = reactive_data()
    if(!is.null(all_charts)){
      print(all_charts[3])
    }
  })
  
  output$gender_data  = renderTable({
    search_result = reactive_rows()
    if(!is.null(search_result)){
      search_result[1]
    }
  })
  
  output$race_data  = renderTable({
    search_result = reactive_rows()
    if(!is.null(search_result)){
      search_result[2]
    }
  })
  
  output$grade_data  = renderTable({
    search_result = reactive_rows()
    if(!is.null(search_result)){
      search_result[3]
    }
  })
  
  output$tuition_info = renderPrint({
    req(money_list())
    print(money_list())
  })
  
  output$hyperlink = renderUI({
    search_result = reactive_rows()
    if(!is.null(search_result)){
      HTML(paste("To go to this major's four year plan, click ", "<a href='", search_result[4], "' target='_blank'>here</a>."))
    }
    
  })
  
}


shinyApp(ui = ui, server = server)
