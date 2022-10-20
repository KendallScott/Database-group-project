library(DBI)
library(odbc)

con <- dbConnect(odbc(),
                 Driver = "ODBC Driver 13 for SQL Server",
                 Server="policeshooting.database.windows.net,",
                 Database="police_shootings",
                 UID = "data_admin",
                 PWD    = rstudioapi::askForPassword("Database password"),
                 Port ="1443")

# Database-group-project

DS7330 File Organization and Database Management Term Project

Project Title: Police Shootings in the United States and Underlying Trends

Investigators:
Tadd Backus: tbackus@mail.smu.edu
Varun Gopal: varung@mail.smu.edu
Kendall Scott: kendallscott@mail.smu.edu
Roslyn Smith: roslyns@mail.smu.edu

Problem Statement: What are the trends in police shootings in the United States since 2015?

The Washington Post has a database called Fatal Force which tracks fatal shootings in the US by a police officer in the line of duty based on news, social media, and police reports since Jan 2015. We will investigate the volume of police shootings split by geography, demographics, police processes, population, etc. and identify insights to determine if there has been any change in trends over time.

Methodology: Our primary data source is the Washington Post Police Shootings Data GitHub. We will supplement with additional metrics and create new variables as needed to complete the investigation and identify the trends and insights.

Milestones:

· Identify questions to be answered
· Source primary and supplemental data and create new variables as needed
· Create database to house data
· Populate data
· Perform queries (MySQL, R, Python) to address questions
· Visualize results and provide insights (R Shiny App, charts / other visualizations)

Initial Questions:

· What are the police shooting trends by geography, demographics, population, etc.?

· Are there any unexpected trends by victim demographics (gender, age, racial group, etc.) over time?

· Are any factors during the police / victim interaction (armed / unarmed, threat level, fleeing / not fleeting, body cam on / off, etc.) that result in a higher rate of deaths?

· Additional questions may arise as more data is sourced.
