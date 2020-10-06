#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(DBI)

setwd("C:/Users/zuhre.b/OneDrive - Procter and Gamble/Desktop/CMPE321 - Project2/")

defaultW <- getOption("warn") 

options(warn = -1) 

createDatabase<-function(con){
    dbSendQuery(conn=con,
                "CREATE TABLE Author (
  AuthorID   INTEGER PRIMARY KEY,
  AuthorName TEXT    NOT NULL,
  Age      INTEGER NOT NULL);
")
    
    dbSendQuery(conn=con,
                "CREATE TABLE User (
  PersonalID   INTEGER PRIMARY KEY,
  Name TEXT    NOT NULL);
")
    
    dbSendQuery(conn=con,
                "CREATE TABLE Book (
  ISBN   INTEGER PRIMARY KEY,
  Title TEXT    NOT NULL,
  AuthorID      INTEGER NOT NULL,
  FOREIGN KEY (AuthorID)
  REFERENCES Author (AuthorID)); 
")
    
    dbSendQuery(conn=con,
                "CREATE TABLE BorrowBook (
  ISBN   INTEGER,
  PersonalID INTEGER,
  DueDate DATETIME NOT NULL,
  PRIMARY KEY (ISBN, PersonalID)
  FOREIGN KEY (ISBN) REFERENCES Book (ISBN)
  FOREIGN KEY (PersonalID) REFERENCES User (PersonalID)
  );
")
    
}

fillDatabase<-function(con){
  
    writeTable(con,"User",c(1188,"Berkay Zuhre"))
    writeTable(con,"User",c(1000,"Ali Veli"))
    
    
    writeTable(con,"Author",c(1,"Aziz Nesin",43))
    writeTable(con,"Author",c(2,"Orhan Kemal",56))
    writeTable(con,"Author",c(3,"Yasar Kemal",90))
    writeTable(con,"Author",c(4,"Sabahattin Ali",40))
    writeTable(con,"Author",c(5,"Ayse Kulin",79))
    
}


writeTable<-function(database,tableName,data){
    
    data=cbind.data.frame(t(data))
    
    fieldNames<-dbListFields(database, tableName)
    
    c<-strsplit(fieldNames," ")
    
    colnames(data)<-c
    
    dbWriteTable(conn=database, name=tableName, data, append=T, row.names=F)
    
}

writeBook<-function(database,data){
    
    AuthorStr = paste("SELECT AuthorID FROM Author
                        WHERE Author.AuthorName='",data[3],"'",sep="")
    
    AuthorID = as.integer(dbGetQuery(conn=database,AuthorStr))
    
    data[3]=AuthorID
    
    data=cbind.data.frame(t(data))
    
    fieldNames<-dbListFields(database, "Book")
    
    c<-strsplit(fieldNames," ")
    
    colnames(data)<-c
    
    dbWriteTable(conn=database, name="Book", data, append=T, row.names=F)
    
    
}

deleteBook<-function(database,tableName,primaryKey){
    str=paste('DELETE FROM',tableName,'WHERE ISBN =',primaryKey)
    
    dbExecute(con,str)
}

searchBook<-function(database,tableName,fieldName,filterValue){
    
    if(fieldName!="AuthorName"){
        
        str=paste("SELECT ISBN, Title, Author.AuthorName
FROM Book
    JOIN Author on Book.AuthorID = Author.AuthorID WHERE Book.",fieldName," = '",filterValue,"'",sep="")
        a<-data.frame(dbGetQuery(conn=con,str))
        return(a)}else{
            
            str=paste("SELECT ISBN, Title, Author.AuthorName
FROM Book
    JOIN Author on Book.AuthorID = Author.AuthorID
    WHERE  
    Author.AuthorName = '",filterValue,"'",sep="")
            a<-data.frame(dbGetQuery(conn=con,str))
            return(a)
        }
}

borrowBook<-function(database,bookISBN,personalID){
    str2=paste("SELECT Count(BorrowBook.PersonalID)
          FROM BorrowBook
          Join User on User.PersonalID = BorrowBook.PersonalID
          Where BorrowBook.PersonalID = ",personalID," Group By BorrowBook.PersonalID;
          ")
    
    borrowedBookNum<-as.integer(data.frame(dbGetQuery(conn=con,str2)))
    
    if(is.na(borrowedBookNum)){
      writeTable(con,"BorrowBook",c(bookISBN,personalID,"2020-10-04"))
    }
    
    else if(borrowedBookNum==8){
        return("Maximum book borrow number reached!")
    }else{
        writeTable(con,"BorrowBook",c(bookISBN,personalID,"2020-10-04"))
    }
    
}


con <- dbConnect(RSQLite::SQLite(), "LibraryDB")

#dbSendQuery(conn=con,"CREATE TRIGGER trigger_delete3 BEFORE INSERT on Book
#WHEN NEW.ISBN = 1
#BEGIN
#  SELECT RAISE(ABORT,'ISBN cant be 1');
#END;")


#createDatabase(con)
#fillDatabase(con)



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("CMPE321-Project3 LibraryDB"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
        ),

        # Show a plot of the generated distribution
        mainPanel(
                tabsetPanel(
                    
                tabPanel("Write Book",textInput(inputId = "ISBN","ISBN"),textInput(inputId = "Name","Book Name"),selectInput("author","Select an author from the list",
                
choices=c("Aziz Nesin","Orhan Kemal","Yasar Kemal","Sabahattin Ali","Ayse Kulin")),actionButton("buttonBook", "Write Book to DB")),
                

                tabPanel("Delete Book",textInput(inputId = "ISBN2","ISBN"),actionButton("buttonDelete", "Delete Book from DB")),
                

                tabPanel("SearchBook",selectInput("fieldOptions","Select a field to filter",
                                                               choices=c("Title","ISBN","AuthorName"),
                ),textInput(inputId = "field","Write your filter"),tableOutput("Books")),
                
                tabPanel("Users",tableOutput("Users")),

                tabPanel("BorrowBook",textInput(inputId = "ISBN3","ISBN"),textInput(inputId = "personalID","PersonalID"),actionButton("buttonBorrow", "Borrow a Book")),
                
                tabPanel("BorrowedBooks",textInput(inputId = "personalID2","PersonalID",value = "All"),tableOutput("borrowed")),

                tabPanel("BorrowedNumber",tableOutput("borrowedNumber"))

                )
    )
))

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    output$Books <-renderTable({
        searchBook(con,"Book",input$fieldOptions,input$field)

    })
    
    observeEvent(input$buttonBook, {
        writeBook(database = con,c(input$ISBN,input$Name,input$author))
    })
    
    observeEvent(input$buttonDelete, {
        deleteBook(con,"Book",input$ISBN2)
    })
    
    observeEvent(input$buttonBorrow, {
      
    borrowBook(con,input$ISBN3,input$personalID)
  
    })
    
    output$Users <-renderTable({

        a<-data.frame(dbGetQuery(conn=con,"Select * From User"))
        a
    })
    
    output$borrowed <-renderTable({
      
      if(input$personalID2=="All"){
      data.frame(dbGetQuery(conn=con,"Select * From BorrowBook"))
      }else{
        str=paste("Select * From BorrowBook Where PersonalID=",input$personalID2)
        data.frame(dbGetQuery(conn=con,str))
      }
      
    })
    
    output$borrowedNumber <-renderTable({
      str=paste("SELECT User.Name,User.PersonalID,Count(BorrowBook.PersonalID)
          FROM BorrowBook
          Join User on User.PersonalID = BorrowBook.PersonalID
          Group By BorrowBook.PersonalID;
          ")
      data.frame(dbGetQuery(conn=con,str))
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
