library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(ggplot2)

ui <- dashboardPage(
    dashboardHeader(title = "Credit Card Customers"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("About", tabName = "i1"),
        menuItem("Univarite Analysis", tabName = "i2"),
        menuItem("Bivariate Analysis", tabName = "i3"),
        menuItem("Multivariate Analysis", tabName = "i5"),
        menuItem("Conclusion", tabName = "i4")
      )
    ),
    
    dashboardBody(
      shinyDashboardThemes(
        theme = "purple_gradient"
      ),
      tabItems(
        tabItem(
          "i1",
          h1("About"),
          fluidPage(
            p("A credit card is a thin rectangular piece of plastic or metal issued by a bank or financial services company
              that allows cardholders to borrow funds with which to pay for goods and services with merchants that accept
              cards for payment. Credit cards impose the condition that cardholders pay back the borrowed money, plus
              any applicable interest, as well as any additional agreed-upon charges, either in full by the billing date or
              over time. Credit cards come with some benefits:",
              br(),br(),
              "• You can use a credit card to make a large purchase which can be repaid in smaller installments over some time",
              br(),
              "• A credit card is relatively safer to carry as against cash and is accepted mostly everywhere",
              br(),
              "• If you have a good credit score, you can avail of additional cash back, lower interest rates, and many more exciting offers",
              br(),br(),
                "Having such benefits, the usage of credit cards is increasing in the modern age. In this project, we want to
              analyze the credit card user base and attempt to find different trends."),
            br(),br(),
            img(src = "img1.jpg", height = "250", width = "360px", style="display: block; margin-left: auto; margin-right: auto;"),
            hr(),h3("DataSet Description"),
            p("This data set consists of 10,000 customers mentioning their age, salary, marital status, credit card limit, credit card category, etc. There are nearly 18 features. The Dataset has been obtained from Kaggle.",
              br(),
              uiOutput("DataSet_Link")
              ),
            
            tableOutput("parameters_description")
          )
        ),
        
        tabItem(
          "i2",
          h1("Univariate Analysis"),
          fluidPage(
            sidebarLayout(
              sidebarPanel(
                selectInput(
                  label = "Choose the Univariate Plot",
                  inputId = "id2",
                  list("Customer Age","Gender","Education Level", "Income Category", "Card Category", "Months on Book", "Credit Limit", "Total Revolving Balance", "Total Transaction Amount", "Total Transaction Count", "Average Utilization Ratio")
                ),
                actionButton(
                  inputId = "uni",
                  label = "Apply"
                )
              ),
              mainPanel(
                plotOutput("id2"),
                textOutput("text2")
              )
            )
          )
        ),
        
        tabItem(
          "i3",
          titlePanel("Bivariate Analysis"),
          tabsetPanel(
            sidebarLayout(
              sidebarPanel(
                selectInput(
                  inputId = "id3",
                  label = "Choose the demography: ",
                  list("Income Category", "Gender", "Age", "Education Level", "Marital Status")
                ),
                actionButton(
                  inputId = "bi",
                  label = "Apply"
                ),
                hr()
              ),
              mainPanel()
            ),
            
            tabPanel(
              "Credit Limit",
              fluidRow(
                mainPanel(
                  plotOutput("a1"),
                  textOutput("t1")
                )
              )
            ),
            
            tabPanel(
              "Total Revolving Balance",
              fluidRow(
                mainPanel(
                  plotOutput("a2"),
                  textOutput("t2")
                )
              )
            ),
            
            tabPanel(
              "Total Transaction Amount",
              fluidRow(
                mainPanel(
                  plotOutput("a3"),
                  textOutput("t3")
                )
              )
            ),
            
            tabPanel(
              "Total Transaction Count",
              fluidRow(
                mainPanel(
                  plotOutput("a4"),
                  textOutput("t4")
                )
              )
            ),
            
            tabPanel(
              "Average Utilization Ratio",
              fluidRow(
                mainPanel(
                  plotOutput("a5"),
                  textOutput("t5")
                )
              )
            ),
          )
        ),
        
        tabItem(
          "i5",
          titlePanel("Multivariate Analysis"),
          fluidPage(
            sidebarLayout(
              sidebarPanel(
                selectInput(
                  label = "Choose the Multivariate Plot",
                  inputId = "id5",
                  list("Revolving Balance and Income Category","Utilization Ratio and Education Level","Utilization Ratio and Card Limit", "Transaction Amount and Revolving Balance", "Transaction Amount and Card Categories", "Credit Limit and Education Level", "Transaction Amount and Education Level")
                ),
                actionButton(
                  inputId = "mul",
                  label = "Apply"
                )
              ),
              mainPanel(
                plotOutput("id5"),
                textOutput("text5")
              )
            )
          )
          
        ),
        
        tabItem(
          "i4",
          h2("Key observations from the graphical analysis of the dataset are: "),
          fluidPage(
            br(),
            h4("• The total revolving balance is centered around the median and it tends to a normal distribution. Also,
most people like to keep a zero-revolving balance."),
            br(),
            h4("• Despite having different education qualifications, the total revolving balance amount is more or less the
same across different demographics. So we can say, having different educational backgrounds doesn’t
affect one’s financial nature."),
            br(),
            h4("• The credit card limit and utilization ratio are in a logarithmic relation. That is the higher the credit
limit, the fewer customers are using it. Also from the graph, we notice the utilization ratio peaks at
around 2500 credit limit, which says, having such a credit limit would benefit the card issuing company."),
            br(),
            h4("• No matter what the transaction amount is, the total revolving balance stays around the median, with
large points at 0 and maximum. So we can say the total revolving balance is independent of the
transaction amount."),
            br(),
            h4("• We also notice that the transaction amount is pretty much independent of the number of transactions. So
users who spend more on credit cards, don’t necessarily use them more than others."),
            br(),
            h4("• Different education background does not affect the transaction amount across gender. It's focued at 3 points around 2500, 7000 and 15000."),
            br(),
            h4("• Females and people belonging to lower salary groups tend to have higher card utilization ratio. So if the card issuing companies can increase their credit limit, it might be profitable for the companies."),
            br(),br()
          )
        )
      )
    )
)

server <- function(input, output){
  df <- read.csv("./data/BankChurners.csv")
  df[c(22,23)] <- NULL
  
  url <- a("Link to DataSet", href = "https://www.kaggle.com/datasets/sakshigoyal7/credit-card-customers")
  output$DataSet_Link <- renderUI({
    tagList("", url)
  })
    
  output$parameters_description <- renderTable({
    c1 <- c("CLIENTNUM","Attrition_Flag","Customer_Age","Gender","Dependent_count","Education_Level","Marital_Status","Income_Category","Card_Category","Months_on_book","Total_Relationship_Count","Months_Inactive_12_Mon","Contacts_Count_12_Mon","Credit_Limit","Total_Revolving_Bal","Avg_Open_To_Buy","Total_Amt_Chng_Q4_Q1","Total_Trans_Amt","Total_Trans_Ct","Total_Ct_Chng_Q4_Q1","Avg_Utilization_Ratio")
    c2 <- c("Client number. An unique identifier for the customer holding the account","If the account is closed then 1 else 0 - Customer activity variable","Customer’s Age in Years - Demographic variable","M = Male, F = Female - Demographic variable","Number of dependents - Demographic variable","Educational Qualification of the account holder (example: high school, college
graduate, etc) - Demographic variable","Married, Single, Divorced, Unknown - Demographic variable","Annual Income Category of the account holder (< $40K, $40K - 60K, $60K -
$80K, $80K-$120K, > $120K) - Demographic variable","Type of Card (Blue, Silver, Gold, Platinum) - Product Variable","Period of relationship with bank","Total no. of products held by the customer","No. of months inactive in the last 12 months","No. of Contacts in the last 12 months","Credit Limit on the Credit Card","Total Revolving Balance on the Credit Card","Open to Buy Credit Line (Average of last 12 months)","Change in Transaction Amount (Quarter 4 over Quarter 1)","Total Transaction Amount (Last 12 months)","Total Transaction Count (Last 12 months)","Change in Transaction Count (Quarter 4 over Quarter 1)","Average Card Utilization Ratio")
    data.frame(cbind(Variable_Name = c1, Description = c2))
  })
  
  observeEvent(
    input$uni,
    if(input$id2 == "Customer Age"){
      output$id2 <- renderPlot({
        ggplot(df, aes(Customer_Age)) + geom_histogram(aes(color = I("black"), fill = I("Light Blue")), binwidth = 5) + xlab("Customer Age")+ ylab("Number of Customers") + ggtitle("Age-wise Credit Card users")
      })
      output$text2 <- renderPrint({
        print("So we see the most credit card users are middle aged people")
      })
    }
    else if(input$id2 == "Gender"){
      output$id2 <- renderPlot({
        df2 <- data.frame(table(df$Gender))
        colnames(df2) <- c("Gender", "Freq")
        r <- ggplot(df2, aes(x = "", y = Freq, fill = Gender))
        r + coord_polar("y") + geom_bar(stat = "identity", width = 1) + scale_fill_brewer(palette = "Set1")
      })
      output$text2 <- renderPrint({
        print("Women are more prone to use credit cards than men")
      })
    }
    else if(input$id2 == "Education Level"){
      output$id2 <- renderPlot({
        df2 <- data.frame(table(df$Education_Level))
        colnames(df2) <- c("Education_Level", "Freq")
        r <- ggplot(df2, aes(x = "", y = Freq, fill = Education_Level))
        r + coord_polar("y") + geom_bar(stat = "identity", width = 1) + scale_fill_brewer("Education Level", palette = "Set1")
      })
      output$text2 <- renderPrint({
        print("So we see most credit card users are Graduates")
      })
    }
    else if(input$id2 == "Income Category"){
      output$id2 <- renderPlot({
        df2 <- data.frame(table(df$Income_Category))
        colnames(df2) <- c("Income_Category", "Freq")
        r <- ggplot(df2, aes(x = "", y = Freq, fill = Income_Category))
        r + coord_polar("y") + geom_bar(stat = "identity", width = 1) + scale_fill_brewer("Income Category", palette = "Set1")
      })
      output$text2 <- renderPrint({
        print("People with less income are prone to use credits cards more")
      })
    }
    else if(input$id2 == "Card Category"){
      output$id2 <- renderPlot({
        df2 <- data.frame(table(df$Card_Category))
        colnames(df2) <- c("Card_Category", "Freq")
        r <- ggplot(df2, aes(x = "", y = Freq, fill = Card_Category))
        r + coord_polar("y") + geom_bar(stat = "identity", width = 1) + scale_fill_manual(name = "Card Category", values=c('Blue'= 'Blue','Gold' = 'Yellow','Platinum' = 'Black','Silver' = 'Grey'))
      })
      output$text2 <- renderPrint({
        print("Blue Cards are the most used cards")
      })
    }
    else if(input$id2 == "Months on Book"){
      output$id2 <- renderPlot({
        ggplot(df, aes(Months_on_book)) + geom_histogram(aes(color = I("black"), fill = I("Light Blue")), binwidth = 5) + xlab("Months on book")+ ylab("Number of Customers")
      })
      output$text2 <- renderPrint({
        print("")
      })
    }
    else if(input$id2 == "Credit Limit"){
      output$id2 <- renderPlot({
        ggplot(df, aes(Credit_Limit)) + geom_histogram(aes(color = I("black"), fill = I("pink")), binwidth = 1000) + xlab("Credit Limit")+ ylab("Number of Customers") + ggtitle("Credit Card limit across different card users")
      })
      output$text2 <- renderPrint({
        print("Here we see that most people have their credit limit around 2000 to 3000")
      })
    }
    else if(input$id2 == "Total Revolving Balance"){
      output$id2 <- renderPlot({
        ggplot(df, aes(Total_Revolving_Bal)) + geom_histogram(aes(color = I("black"), fill = I("Light Blue")), binwidth = 200) + xlab("Total Revolving Balance")+ ylab("Number of Customers") + ggtitle("Total Revolving Balance for card users")
      })
      output$text2 <- renderPrint({
        print("So we see for most card users total revolving balance is 0")
      })
    }
    else if(input$id2 == "Total Transaction Amount"){
      output$id2 <- renderPlot({
        ggplot(df, aes(Total_Trans_Amt)) + geom_histogram(aes(color = I("black"), fill = I("light green")), binwidth = 500) + xlab("Total Transaction Amount")+ ylab("Number of Customers")
      })
      output$text2 <- renderPrint({
        print("We see the total transaction done by the users is clusterd between 500 to 5000")
      })
    }
    else if(input$id2 == "Total Transaction Count"){
      output$id2 <- renderPlot({
        ggplot(df, aes(Total_Trans_Ct)) + geom_histogram(aes(color = I("black"), fill = I("light yellow")), binwidth = 5) + xlab("Total Transaction Count")+ ylab("Number of Customers")
      })
      output$text2 <- renderPrint({
        print("The total transaction count is spread across the plot")
      })
    }
    else if(input$id2 == "Average Utilization Ratio"){
      output$id2 <- renderPlot({
        ggplot(df, aes(Avg_Utilization_Ratio)) + geom_histogram(aes(color = I("black"), fill = I("red")), bins = 30) + xlab("Average Utilization Ratio")+ ylab("Number of Customers")
      })
      output$text2 <- renderPrint({
        print("We see that most users has utilization ratio zero. This gives us an idea about the inactive card users.")
      })
    }
  )
  
  observeEvent(
    input$mul,
    if(input$id5 == "Revolving Balance and Income Category"){
      output$id5 <- renderPlot({
        ggplot(data = df, aes(Total_Revolving_Bal)) + geom_histogram(aes(color = I('Black'), fill = Income_Category), bins = 30) + geom_freqpoly(bins = 30, color = 'Black')+ xlab("Total Revolving Balance") + ylab("Count") + facet_wrap(.~Income_Category, ncol = 3) + ggtitle("Total Revolving Balance based on Income Category") + scale_fill_discrete(name = "Income Category")
      })
      output$text5 <- renderPrint({
        print("We see that for all the income categories, total revolving balance is centered around the median and at 0 it's at peak, that is most accounts have no total revolving balance.")
      })
    }
    else if(input$id5 == "Utilization Ratio and Education Level"){
      output$id5 <- renderPlot({
        ggplot(data = df, aes(Education_Level, Avg_Utilization_Ratio, color = Education_Level)) + geom_boxplot(notch = T, varwidth = T) + geom_jitter(size = 0.1, position=position_jitter(0.3), alpha = I(0.2)) + xlab("Total Revolving Balance") + ylab("Count") + ggtitle("Utilization Ratio based on Education Level") + scale_color_discrete(name = "Educaion Level") + theme(axis.text.x = element_text(angle = 315))
      })
      output$text5 <- renderPrint({
        print("We see that despite of having different education qualifications, the total revolving balance amount count is more or less same across different demographics. Also a large number of points are outliers.")
      })
    }
    else if(input$id5 == "Utilization Ratio and Card Limit"){
      output$id5 <- renderPlot({
        ggplot(data = df, aes(Credit_Limit, Avg_Utilization_Ratio)) + geom_point(shape = 1, aes(color = Card_Category), size = I(0.1)) + geom_smooth(color = I("Red"), size = I(0.3), fill = NA) + scale_color_manual(name = "Income Category", values=c('Blue'= 'Blue','Gold' = 'Yellow','Platinum' = 'Black','Silver' = 'Grey')) + xlab("Credit Limit") + ylab("Average Utilization Ratio") + ggtitle("Utilization Ratio and Credit Limit Scatterplot")
      })
      output$text5 <- renderPrint({
        print("Here we can see as the credit limit is increasing, the average utilization ratio is decreasing, in a logarithmic relation. That is the higher the credit limit, the less customers are using it. Also we see the line peaks at around 2500, which is the credit limit, where utilization peaks.")
      })
    }
    else if(input$id5 == "Transaction Amount and Revolving Balance"){
      output$id5 <- renderPlot({
        ggplot(data = df, aes(Total_Trans_Amt, Total_Revolving_Bal)) + geom_point(alpha = I(0.5), aes(color = Income_Category), size = I(0.5)) + geom_smooth(size = I(0.5)) + scale_color_discrete(name = "Income Category") + xlab("Total Transaction Amount") + ylab("Total Revolving Balance") + ggtitle("Transaction Amount and Revolving Balance Scatterplot")
      })
      output$text5 <- renderPrint({
        print("We see the total transaction amount is clustered around 3 points and most these are in between 0 to 5000. Also no matter what the transaction amount is, the total revolving balance stays around the median, with large points at 0 and max.")
      })
    }
    else if(input$id5 == "Transaction Amount and Card Categories"){
      output$id5 <- renderPlot({
        ggplot(data = df, aes(Total_Ct_Chng_Q4_Q1, Total_Amt_Chng_Q4_Q1)) + geom_point(shape = 1, alpha = I(0.5), size = I(0.2), aes(color = Card_Category)) + facet_grid(Card_Category~.) + geom_smooth(size = I(0.2), color = I('Black')) + scale_color_manual(name = "Card Category", values=c('Blue'= 'Blue','Gold' = 'Yellow','Platinum' = 'Black','Silver' = 'Grey')) + xlab("Change in Transaction Count") + ylab("Change in Transaction Amount") + ggtitle("Transaction Amount and Count accross different Card Categories")
      })
      output$text5 <- renderPrint({
        print("We see that across different cards, the transaction amount is hardly effected by the number of transactions made by the users.")
      })
    }
    else if(input$id5 == "Credit Limit and Education Level"){
      output$id5 <- renderPlot({
        ggplot(data = df) + geom_histogram(aes(Credit_Limit, fill = Education_Level, color = I('Black'))) + xlab("Credit Limit") + ylab("Number of Customers") + scale_fill_discrete("Education Level")
      })
      output$text5 <- renderPrint({
        print("So we see, for most of the customers the credit limit is clustered around 2000 and the ratio between different education level is pretty much same")
      })
    }
    else if(input$id5 == "Transaction Amount and Education Level"){
      output$id5 <- renderPlot({
        ggplot(data = df) + geom_histogram(aes(Total_Trans_Amt, fill = Education_Level, color = I('Black')), bins = 30) + facet_grid(.~Gender) + xlab("Total Transaction Amount") + ylab("Number of Customers") + scale_fill_discrete("Education Level")
      })
      output$text5 <- renderPrint({
        print("We see the total tansaction amount is focused around 3 points for both the genders. Also more male customers spend credit cards for higher amount of purchases than female customers")
      })
    }
  )
  
  observeEvent(
    input$bi,

    {output$a1 <- renderPlot({
      
      if(isolate(input$id3) == "Age"){
        data <- df[c("Customer_Age", "Credit_Limit")]
        ggplot(data) + geom_col(aes_string(names(data[1]), "Credit_Limit", fill = names(data[1]))) + xlab("Customer Age") + ylab("Credit Limit")
      }
      else if(isolate(input$id3) == "Gender"){
        data <- df[c("Gender", "Credit_Limit")]
        ggplot(data) + geom_col(aes_string(names(data[1]), "Credit_Limit", fill = names(data[1]), color = names(data[1]))) + ylab("Credit Limit")
      }
      else if(isolate(input$id3) == "Education Level"){
        data <- df[c("Education_Level", "Credit_Limit")]
        ggplot(data) + geom_col(aes_string(names(data[1]), "Credit_Limit", fill = names(data[1]), color = names(data[1]))) + ylab("Credit Limit") + theme(axis.text.x = element_text(angle = 315))
      }
      else if(isolate(input$id3) == "Marital Status"){
        data <- df[c("Marital_Status", "Credit_Limit")]
        ggplot(data) + geom_col(aes_string(names(data[1]), "Credit_Limit", fill = names(data[1]), color = names(data[1]))) + xlab("Marital Status") + ylab("Credit Limit") + scale_fill_discrete("Marital Status") + scale_color_discrete("Marital Status")
      }
      else if(isolate(input$id3) == "Income Category"){
        data <- df[c("Income_Category", "Credit_Limit")]
        ggplot(data, aes_string(names(data[1]), "Credit_Limit", color = names(data[1]))) + geom_boxplot(notch = T, varwidth = T, outlier.size = I(0.1), size = I(0.3)) + geom_jitter(size = 0.1, position=position_jitter(0.3), alpha = I(0.1)) + xlab("Income Category") + ylab("Credit Limit") + scale_fill_discrete("Income Category") + theme(axis.text.x = element_text(angle = 315))
      }
    })
    
    output$t1 <- renderPrint({
      if(isolate(input$id3) == "Age"){
        print("So we see people in their middle age has highest total credit limit. That is we can say most card users are in their middle age.")
      }
      else if(isolate(input$id3) == "Gender"){
        print("Compared to females, males have much more credit limit.")
      }
      else if(isolate(input$id3) == "Education Level"){
        print("Graduates tend to have higher credit limit.")
      }
      else if(isolate(input$id3) == "Marital Status"){
        print("Credit Card users are around equally distributed between Married and Single people")
      }
      else if(isolate(input$id3) == "Income Category"){
        print("So we see that users with higher income, tend to have higher credit limit")
      }
    })
    
    output$a2 <- renderPlot({
      
      if(isolate(input$id3) == "Age"){
        data <- df[c("Customer_Age", "Total_Revolving_Bal")]
        ggplot(data) + geom_col(aes_string(names(data[1]), "Total_Revolving_Bal", fill = names(data[1]))) + xlab("Customer Age") + ylab("Total Revolving Balance")
      }
      else if(isolate(input$id3) == "Gender"){
        data <- df[c("Gender", "Total_Revolving_Bal")]
        ggplot(data) + geom_col(aes_string(names(data[1]), "Total_Revolving_Bal", fill = names(data[1]), color = names(data[1]))) + ylab("Total Revolving Balance")
      }
      else if(isolate(input$id3) == "Education Level"){
        data <- df[c("Education_Level", "Total_Revolving_Bal")]
        ggplot(data) + geom_col(aes_string(names(data[1]), "Total_Revolving_Bal", fill = names(data[1]), color = names(data[1]))) + ylab("Total Revolving Balance") + theme(axis.text.x = element_text(angle = 315))
      }
      else if(isolate(input$id3) == "Marital Status"){
        data <- df[c("Marital_Status", "Total_Revolving_Bal")]
        ggplot(data) + geom_col(aes_string(names(data[1]), "Total_Revolving_Bal", fill = names(data[1]), color = names(data[1]))) + xlab("Marital Status") + ylab("Total Revolving Balance") + scale_fill_discrete("Marital Status") + scale_color_discrete("Marital Status")
      }
      else if(isolate(input$id3) == "Income Category"){
        data <- df[c("Income_Category", "Total_Revolving_Bal")]
        ggplot(data, aes_string(names(data[1]), "Total_Revolving_Bal", color = names(data[1]))) + geom_boxplot(notch = T, varwidth = T, outlier.size = I(0.1), size = I(0.3)) + geom_jitter(size = 0.1, position=position_jitter(0.3), alpha = I(0.1)) + xlab("Income Category") + ylab("Total Revolving Balance") + scale_fill_discrete("Income Category") + theme(axis.text.x = element_text(angle = 315))
      }
    })
    
    output$t2 <- renderPrint({
      if(isolate(input$id3) == "Age"){
        print("Middle aged people tend to have higher revolving balance")
      }
      else if(isolate(input$id3) == "Gender"){
        print("Though females have much less credit card limit, they have much more revolving balance than males")
      }
      else if(isolate(input$id3) == "Education Level"){
        print("As gradutes have more credit card limit, they also have much more revolving balance than the other demographies")
      }
      else if(isolate(input$id3) == "Marital Status"){
        print("Compared to singles, married people have higher revolving balance. So we get an idea about their day to day expenses")
      }
      else if(isolate(input$id3) == "Income Category"){
        print("We see the revolving balance is quite the same across all the different income groups, which is surprising")
      }
    })
    
    output$a3 <- renderPlot({
      
      if(isolate(input$id3) == "Age"){
        data <- df[c("Customer_Age", "Total_Trans_Amt")]
        ggplot(data) + geom_col(aes_string(names(data[1]), "Total_Trans_Amt", fill = names(data[1]))) + xlab("Customer Age") + ylab("Total Transaction Amount")
      }
      else if(isolate(input$id3) == "Gender"){
        data <- df[c("Gender", "Total_Trans_Amt")]
        ggplot(data) + geom_col(aes_string(names(data[1]), "Total_Trans_Amt", fill = names(data[1]), color = names(data[1]))) + ylab("Total Transaction Amount")
      }
      else if(isolate(input$id3) == "Education Level"){
        data <- df[c("Education_Level", "Total_Trans_Amt")]
        ggplot(data) + geom_col(aes_string(names(data[1]), "Total_Trans_Amt", fill = names(data[1]), color = names(data[1]))) + ylab("Total Transaction Amount") + theme(axis.text.x = element_text(angle = 315))
      }
      else if(isolate(input$id3) == "Marital Status"){
        data <- df[c("Marital_Status", "Total_Trans_Amt")]
        ggplot(data) + geom_col(aes_string(names(data[1]), "Total_Trans_Amt", fill = names(data[1]), color = names(data[1]))) + xlab("Marital Status") + ylab("Total Transaction Amount") + scale_fill_discrete("Marital Status") + scale_color_discrete("Marital Status")
      }
      else if(isolate(input$id3) == "Income Category"){
        data <- df[c("Income_Category", "Total_Trans_Amt")]
        ggplot(data, aes_string(names(data[1]), "Total_Trans_Amt", color = names(data[1]))) + geom_boxplot(notch = T, varwidth = T, outlier.size = I(0.1), size = I(0.3)) + geom_jitter(size = 0.1, position=position_jitter(0.3), alpha = I(0.1)) + xlab("Income Category") + ylab("Total Transaction Amount") + scale_fill_discrete("Income Category") + theme(axis.text.x = element_text(angle = 315))
      }
    })
    
    output$t3 <- renderPrint({
      if(isolate(input$id3) == "Age"){
        print("We see that middle aged people use credit cards more than others")
      }
      else if(isolate(input$id3) == "Gender"){
        print("Both males and females do around same amount of transactions using credit cards")
      }
      else if(isolate(input$id3) == "Education Level"){
        print("Gradutes tend to use credit card more than other demographics")
      }
      else if(isolate(input$id3) == "Marital Status"){
        print("Credit card uses is around the same across different groups")
      }
      else if(isolate(input$id3) == "Income Category"){
        print("We see despite having difference in income, people tend to use credit cards the same")
      }
    })
    
    output$a4 <- renderPlot({
      
      if(isolate(input$id3) == "Age"){
        data <- df[c("Customer_Age", "Total_Trans_Ct")]
        ggplot(data) + geom_col(aes_string(names(data[1]), "Total_Trans_Ct", fill = names(data[1]))) + xlab("Customer Age") + ylab("Total Transaction Count")
      }
      else if(isolate(input$id3) == "Gender"){
        data <- df[c("Gender", "Total_Trans_Ct")]
        ggplot(data) + geom_col(aes_string(names(data[1]), "Total_Trans_Ct", fill = names(data[1]), color = names(data[1]))) + ylab("Total Transaction Count")
      }
      else if(isolate(input$id3) == "Education Level"){
        data <- df[c("Education_Level", "Total_Trans_Ct")]
        ggplot(data) + geom_col(aes_string(names(data[1]), "Total_Trans_Ct", fill = names(data[1]), color = names(data[1]))) + ylab("Total Transaction Count") + theme(axis.text.x = element_text(angle = 315))
      }
      else if(isolate(input$id3) == "Marital Status"){
        data <- df[c("Marital_Status", "Total_Trans_Ct")]
        ggplot(data) + geom_col(aes_string(names(data[1]), "Total_Trans_Ct", fill = names(data[1]), color = names(data[1]))) + xlab("Marital Status") + ylab("Total Transaction Count") + scale_fill_discrete("Marital Status") + scale_color_discrete("Marital Status")
      }
      else if(isolate(input$id3) == "Income Category"){
        data <- df[c("Income_Category", "Total_Trans_Ct")]
        ggplot(data, aes_string(names(data[1]), "Total_Trans_Ct", color = names(data[1]))) + geom_boxplot(notch = T, varwidth = T, outlier.size = I(0.1), size = I(0.3)) + geom_jitter(size = 0.1, position=position_jitter(0.3), alpha = I(0.1)) + xlab("Income Category") + ylab("Total Transaction Count") + scale_fill_discrete("Income Category") + theme(axis.text.x = element_text(angle = 315))
      }
    })
    
    output$t4 <- renderPrint({
      if(isolate(input$id3) == "Age"){
        print("Just like transaction amount, middle aged people also use credit cards more")
      }
      else if(isolate(input$id3) == "Gender"){
        print("Surprisingly females tend to use credit cards more than male users")
      }
      else if(isolate(input$id3) == "Education Level"){
        print("We see despite graduates do higher amount of transactions, different demographies tend to use credit cards more. So we can say they use credit cards to do comparatively less amount of purchases")
      }
      else if(isolate(input$id3) == "Marital Status"){
        print("Diffrent groups tend to spend around the same across different transactions")
      }
      else if(isolate(input$id3) == "Income Category"){
        print("We see, across all income groups, card usage is about the same. So they spend around the same amount of money across all the purchases made using credit cards")
      }
    })
    
    output$a5 <- renderPlot({
      
      if(isolate(input$id3) == "Age"){
        data <- df[c("Customer_Age", "Avg_Utilization_Ratio")]
        ggplot(data) + geom_col(aes_string(names(data[1]), "Avg_Utilization_Ratio", fill = names(data[1]))) + xlab("Customer Age") + ylab("Average Utilization Ratio")
      }
      else if(isolate(input$id3) == "Gender"){
        data <- df[c("Gender", "Avg_Utilization_Ratio")]
        ggplot(data) + geom_col(aes_string(names(data[1]), "Avg_Utilization_Ratio", fill = names(data[1]), color = names(data[1]))) + ylab("Average Utilization Ratio")
      }
      else if(isolate(input$id3) == "Education Level"){
        data <- df[c("Education_Level", "Avg_Utilization_Ratio")]
        ggplot(data) + geom_col(aes_string(names(data[1]), "Avg_Utilization_Ratio", fill = names(data[1]), color = names(data[1]))) + ylab("Average Utilization Ratio") + theme(axis.text.x = element_text(angle = 315))
      }
      else if(isolate(input$id3) == "Marital Status"){
        data <- df[c("Marital_Status", "Avg_Utilization_Ratio")]
        ggplot(data) + geom_col(aes_string(names(data[1]), "Avg_Utilization_Ratio", fill = names(data[1]), color = names(data[1]))) + xlab("Marital Status") + ylab("Average Utilization Ratio") + scale_fill_discrete("Marital Status") + scale_color_discrete("Marital Status")
      }
      else if(isolate(input$id3) == "Income Category"){
        data <- df[c("Income_Category", "Avg_Utilization_Ratio")]
        ggplot(data, aes_string(names(data[1]), "Avg_Utilization_Ratio", color = names(data[1]))) + geom_boxplot(notch = T, varwidth = T, outlier.size = I(0.1), size = I(0.3)) + geom_jitter(size = 0.1, position=position_jitter(0.3), alpha = I(0.1)) + xlab("Income Category") + ylab("Average Utilization Ratio") + scale_fill_discrete("Income Category") + theme(axis.text.x = element_text(angle = 315))
      }
    })
    
    output$t5 <- renderPrint({
      if(isolate(input$id3) == "Age"){
        print("We see middle age people tend to utilize credit cards more than others")
      }
      else if(isolate(input$id3) == "Gender"){
        print("Though females have less limit on their cards, they tend to utilize credit cards more")
      }
      else if(isolate(input$id3) == "Education Level"){
        print("Utilization ratio is related to their card usage")
      }
      else if(isolate(input$id3) == "Marital Status"){
        print("Across different demographies, utilization ratio is as expected")
      }
      else if(isolate(input$id3) == "Income Category"){
        print("We see lower income groups tend to have higher utilization ratio, which is surprising enough")
      }
    })
    }
  )
  
    
}

shinyApp(ui, server)
