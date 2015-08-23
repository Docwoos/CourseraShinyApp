ui<-fluidPage(
    headerPanel('21 Day Fix Helper'),
    sidebarLayout(
        sidebarPanel(
            numericInput(inputId = "num",
                        label = "Enter Your Current Weight",
                        value = 150),
            numericInput(inputId = "num2",
                         label = "Enter Your Target Weight",
                         value = 130),
            actionButton(inputId = "go",
                         label = "Calculate"),
            h3("Documentation"),
            h4("Purpose:"),
            h5("This app helps simplify your food and water requirements during the 21 Day Fix"),
            h5("Based on your weight plus your expected additional burn from the workouts and then 
               minus the targeted daily loss you plan your meals based upon a set of food groups 
               with defined portion sizes and you drink a calculated amount of water. In addition
               by setting a target weight you can quickly see how much you plan to lose."),
            h4("How to use this App:"),
            h5("Using the app is simple, just enter your current weight in the indicated field 
               along with your target weight below that. You will instantly get your container 
               limits and after clicking 'Calculate' your targeted loss and water requirments will
               be calculated and dispalyed.")
            
            , style = "background-color:#b8dae1"),
        mainPanel(
            h2("Key Information:"),
            h4(textOutput("stat")),
            h4(textOutput("stat2")),
            "___________________",
            h4(textOutput("stat3")),
            h4(textOutput("stat4")),
            "___________________",
            h3("You get to eat:"),
            h4(textOutput("green"), style = "color:green"),
            h4(textOutput("purple"), style = "color:purple"),
            h4(textOutput("red"), style = "color:red"),
            h4(textOutput("yellow"), style = "color:yellow"),
            h4(textOutput("blue"), style = "color:blue"),
            h4(textOutput("orange"), style = "color:orange"),
            h4(textOutput("spoons"), style = "color:black"),
            h3("Containers Per Day!")

                  , style = "background-color:#b8dae1")

    )
)

server <- function(input, output){
    weight <- eventReactive(input$go, {
        (((input$num *11)-350))
    })
    dataContainers <- reactiveValues()
    dataContainers$a <- c(6,4,6,4,1,1,6)
    dataContainers$b <- c(5,3,5,4,1,1,5)
    dataContainers$c <- c(4,3,4,3,1,1,4)
    dataContainers$d <- c(3,2,4,2,1,1,2)


    
    dataLose <- eventReactive(input$go, {
        (input$num - input$num2)
    })
    dataPerc <- eventReactive(input$go, {
        round((((input$num - input$num2)/input$num)*100), digits = 2)
    })
    dataWater <- eventReactive(input$go, {
        round((input$num / 2), digits = 0)
    })
    dataGlasses <- eventReactive(input$go, {
        round((input$num / 16), digits = 0)
    })
    output$stat <- renderText({
        paste("You plan to lose ",as.character(dataLose()), "Pounds")
        
    })
    output$stat2 <- renderText({
        paste("That is ",as.character(dataPerc()), "Percent of your current body weight")
        
    })
    output$stat3 <- renderText({
        paste("You need to Drink",as.character(dataWater()), "Ounces of Water per day")
        
    })
    output$stat4 <- renderText({
        paste("Thats",as.character(dataGlasses()), "(8 oz.) Glasses a day")
        
    })
    output$green <- renderText({
        if( (((input$num *11)-350)) >= 2100){
            paste(dataContainers$a[1],"Vegetables")
        }else{
            if((((input$num *11)-350))<2100 & (((input$num *11)-350))>=1800){
                paste(dataContainers$b[1],"Vegetables")
            }else{
                if((((input$num *11)-350))<1800 & (((input$num *11)-350))>=1500){
                    paste(dataContainers$c[1],"Vegetables")
                }else{
                    paste(dataContainers$d[1],"Vegetables")
            }
            
        }
        }
        
    })
    output$purple <- renderText({
        if( (((input$num *11)-350)) >= 2100){
            paste(dataContainers$a[2],"Fruit")
        }else{
            if((((input$num *11)-350))<2100 & (((input$num *11)-350))>=1800){
                paste(dataContainers$b[2],"Fruit")
            }else{
                if((((input$num *11)-350))<1800 & (((input$num *11)-350))>=1500){
                    paste(dataContainers$c[2],"Fruit")
                }else{
                    paste(dataContainers$d[2],"Fruit")
                }
                
            }
        }
        
    })
    output$red <- renderText({
        if( (((input$num *11)-350)) >= 2100){
            paste(dataContainers$a[3],"Protein")
        }else{
            if((((input$num *11)-350))<2100 & (((input$num *11)-350))>=1800){
                paste(dataContainers$b[3],"Protein")
            }else{
                if((((input$num *11)-350))<1800 & (((input$num *11)-350))>=1500){
                    paste(dataContainers$c[3],"Protein")
                }else{
                    paste(dataContainers$d[3],"Protein")
                }
                
            }
        }
        
    })
    output$yellow <- renderText({
        if( (((input$num *11)-350)) >= 2100){
            paste(dataContainers$a[4],"Carbs")
        }else{
            if((((input$num *11)-350))<2100 & (((input$num *11)-350))>=1800){
                paste(dataContainers$b[4],"Carbs")
            }else{
                if((((input$num *11)-350))<1800 & (((input$num *11)-350))>=1500){
                    paste(dataContainers$c[4],"Carbs")
                }else{
                    paste(dataContainers$d[4],"Carbs")
                }
                
            }
        }
        
    })
    output$blue <- renderText({
        if( (((input$num *11)-350)) >= 2100){
            paste(dataContainers$a[5],"Healthy Fats")
        }else{
            if((((input$num *11)-350))<2100 & (((input$num *11)-350))>=1800){
                paste(dataContainers$b[5],"Healthy Fats")
            }else{
                if((((input$num *11)-350))<1800 & (((input$num *11)-350))>=1500){
                    paste(dataContainers$c[5],"Healthy Fats")
                }else{
                    paste(dataContainers$d[5],"Healthy Fats")
                }
                
            }
        }
        
    })
    output$orange <- renderText({
        if( (((input$num *11)-350)) >= 2100){
            paste(dataContainers$a[6],"Seeds & Nuts")
        }else{
            if((((input$num *11)-350))<2100 & (((input$num *11)-350))>=1800){
                paste(dataContainers$b[6],"Seeds & Nuts")
            }else{
                if((((input$num *11)-350))<1800 & (((input$num *11)-350))>=1500){
                    paste(dataContainers$c[6],"Seeds & Nuts")
                }else{
                    paste(dataContainers$d[6],"Seeds & Nuts")
                }
                
            }
        }
        
    })
    output$spoons <- renderText({
        if( (((input$num *11)-350)) >= 2100){
            paste(dataContainers$a[7],"Oil TableSpoons")
        }else{
            if((((input$num *11)-350))<2100 & (((input$num *11)-350))>=1800){
                paste(dataContainers$b[7],"Oil TableSpoons")
            }else{
                if((((input$num *11)-350))<1800 & (((input$num *11)-350))>=1500){
                    paste(dataContainers$c[7],"Oil TableSpoons")
                }else{
                    paste(dataContainers$d[7],"Oil Teaspoons")
                }
                
            }
        }
        
    })
    
}

shinyApp(ui = ui, server = server)