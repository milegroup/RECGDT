library(shiny)
#The shiny package is used to create the graphic interface basics.
library(shinyWidgets)
#The shinyWidgets package is used to add richer elements to the interface, like the background image.
library(stringi)
#The stringi package is used to indent specific menus related to a more generic one.
library(shinyjs)
#The shinyjs package is used to enable and disable buttons, depending on the context.
library(recgTesis)
#The recgTesis package contains all the core functionalities of the app.

baseECGData <- readRDS(file = "baseECGData.rds")
modelBradycardia <- readRDS(file = "modelBradycardia.rds")
modelIschemia <- readRDS(file = "modelIschemia.rds")
modelMyocardialInfarction <- readRDS(file = "modelMyocardialInfarction.rds")
modelTachycardia <- readRDS(file = "modelTachycardia.rds")
modelVentricularHypertrophy <- readRDS(file = "modelVentricularHypertrophy.rds")
modelWPWSyndrome <- readRDS(file = "modelWPWSyndrome.rds")

#ECGData <- CreateECGData(Verbose = TRUE)
ECGData <- baseECGData

ui <- fluidPage(
  #CSS rules to establish the height of the Shiny window, the font for the name of the app and the font for the new menus.
  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Bungee');
                    
                    div.tabbable 
                    {
                    height: 850px !important;
                    }
                    
                    h1
                    {
                    font-family: 'Bungee', cursive;
                    font-size: 3.5em;
                    font-weight: bold;
                    }
                    
                    .qt pre
                    {
                    /*font-family: monospace !important;*/
                    font-family: Arial, Helvetica, Consolas, monospace !important;
                    }
                    
                    pre
                    {
                    /*font-family: monospace !important;*/
                    font-family: Arial, Helvetica, Consolas, monospace !important;
                    }
                    
                    "))
    ),
  #The shinyjs library is used to enable and disable buttons when needed.
  shinyjs::useShinyjs(),
  titlePanel(h1("ECGDT: ECG Diagnosis Tool v.1.1", align = "right", tags$style("h1{font-weight: bold;}"))),
  setBackgroundImage(src = "/ecg-background.png"),
  #Application menu, composed of 13 items, some of them with multiple tabs inside (2 or 4).
  #Some menu items will be hidden and will only appear if the data they need to process is available.
  navlistPanel(
    #Menu panel width is diminished from default values to get more space for contents.
    #Certain non-basic instructions can also be hidden to leave more space for contents.
    widths = c(3,8),
    "Menu",
    id = "Menu",
    tabPanel(
      "Load ECG",
      #Icons are scaled to make them bigger. 
      icon = icon("cloud-upload-alt", "fa-2x"),
      tabsetPanel(
        tabPanel(
          "ASCII",
          h2("Welcome to the ECG Diagnosis Tool ECGDT"),
          hr(),
          #Each instructions paragraph has an identifier, in order to show or hide it, depending on the user's preferences.
          h4(id = "Inst1", "You can upload any ASCII ECG record file from your computer by clicking on 'Browse...'. When the upload to the server is finished, you can also specify a calibration factor. To load the file into the system, you have to click on 'Load ECG file'."),
          h4(id = "Inst2", "Once the data needed for them is available, different new submenus will start to appear in the left part of the app."),
          h4(id = "Inst3", "You can restart the process by clicking on 'Upload another file'."),
          hr(),
          h4(id = "Inst4", "After loading the ECG record, you will be able to:"),
          #Text for newly available menus is shown in a different format.
          pre(id = "Inst5", h5("    - Export the ECG record to an ASCII file (in the 'Export ECG' submenu)."),
              h5("    - Plot the signal (in the 'Show signal' submenu)."), 
              h5("    - Obtain beat positions (in the 'Beat detection' submenu).")),
          hr(),
          fileInput("ECGRecordFile", "Select an ECG file"),
          textInput("calibrationFactor", "Specify a calibration factor", 1),
          hr(),
          actionButton("butLoadECGASCII","Load ECG file"),
          actionButton("butLoadAnotherECGASCII","Upload another file"),
          h4(textOutput("loadedFileASCII"))
        ),
        tabPanel(
          "WFDB",
          h2("Welcome to the ECG Diagnosis Tool ECGDT"),
          hr(),
          h4(id = "Inst6", "You can upload any WFDB ECG record file (in 16 or 212 format) from your computer by clicking on 'Browse...'. When the upload to the server is finished, you have to select the correct file type. To load the file into the system, you have to click on 'Load ECG file'."),
          h4(id = "Inst7", "Once the data needed for them is available, different new submenus will start to appear in the left part of the app."),
          h4(id = "Inst8", "You can restart the process by clicking on 'Upload another file'."),
          hr(),
          h4(id = "Inst9", "After loading the ECG record, you will be able to:"),
          pre(id = "Inst10", h5("   - Export the ECG record to an ASCII file (in the 'Export ECG' submenu)."),
              h5("   - Plot the signal (in the 'Show signal' submenu)."),
              h5("   - Obtain beat positions (in the 'Beat detection' submenu).")),
          hr(),
          fileInput("ECGRecordFile", "Select an ECG file"),
          selectInput("typeWFDBList", "Select a WFDB file type", c("16", "212")),
          hr(),
          actionButton("butLoadECGWFDB", "Load ECG file"),
          actionButton("butLoadAnotherECGWFDB","Upload another file"),
          h4(textOutput("loadedFileWFDB"))
        )
      )
    ),
    tabPanel(
      #Menus related to other will have indentation before their names.
      paste(stri_dup(intToUtf8(160), 6),
            "Export ECG"),
      icon = icon("cloud-download-alt", "fa-2x", lib = "font-awesome"),
      #Once a record is loaded in the application, a text will show at the top of each menu window the name of the current record.
      h3(textOutput("currentRecord1"), align = "center", tags$style("#currentRecord1{color: #337AB7; font-weight: bold;}")),
      hr(),
      h4(id = "Inst11", "After manually specifying the file name and a path, you can can export the ECG record to an ASCII file, by clicking on 'Export ECG'."),
      hr(),
      sidebarLayout(
        sidebarPanel(
          textInput("fileNameECG", "Specify a file name:", ""),
          textInput("filePathECG", "Specify a file path:", ""),
          hr(),
          actionButton("butExportECG","Export ECG")
        ),
        mainPanel()
      ),
      h4(textOutput("exportedECG"))
    ),
    tabPanel(
      paste(stri_dup(intToUtf8(160), 6),
            "Show Signal"),
      icon = icon("chart-area", "fa-2x"),
      tabsetPanel(
        tabPanel(
          "Show all the signal",
          h3(textOutput("currentRecord2"), align = "center", tags$style("#currentRecord2{color: #337AB7; font-weight: bold;}")),
          hr(),
          h4(id = "Inst12", "By selecting a channel from the list, you can continuously plot the signal from the specified channel in the loaded ECG record."),
          h4(id = "Inst13", "Consider the following channel reciprocation when using 12-lead ECG records: '1 - (I)', '2 - (II)', '3 - (III)', '4 - (AVR)', '5 - (AVL)', '6 - (AVF)', '7 - (V1)', '8 - (V2)', '9 - (V3)', '10 - (V4)', '11 - (V5)', '12 - (V6)'."),
          h4(id = "Inst14", "To start visualizing, click on 'Show'. The signal is refreshed in 5000-sample segments each 1.5 seconds. Once the end of the signal has been reached, the plot automatically starts again. Nevertheless, the plot can also be stopped at any time, by clicking on 'Stop'. With the plot stopped, you will be able to manually move backwards or advance a 5000-samples segment, using the 'Previous' and 'Next' buttons."),
          hr(),
          sidebarLayout(
            sidebarPanel(
              br(),
              br(),
              br(),
              br(),
              selectInput("channelListShow", "Select a channel from the ECG", c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")),
              hr(),
              #Continuous all-signal plots can be paused, to return or advance manually by 5000-samples segments.
              actionButton("butShowECG","Show"),
              br(),
              actionButton("butShowECGStepPrevious"," << Previous"),
              actionButton("butShowECGStepNext","Next >>"),
              actionButton("butStopShowECG","Stop"),
              br(),
              br(),
              br(),
              br()
            ),
            mainPanel(
              plotOutput("plotAllSignal")
            )
          )
        ),
        tabPanel(
          "Show signal segment",
          h3(textOutput("currentRecord3"), align = "center", tags$style("#currentRecord3{color: #337AB7; font-weight: bold;}")),
          hr(),
          h4(id = "Inst15", "By selecting a channel from the list, and manually specify the starting and ending samples, you can plot any given segment of the signal from the specified channel in the loaded ECG record."),
          h4(id = "Inst16", "Consider the following channel reciprocation when using 12-lead ECG records: '1 - (I)', '2 - (II)', '3 - (III)', '4 - (AVR)', '5 - (AVL)', '6 - (AVF)', '7 - (V1)', '8 - (V2)', '9 - (V3)', '10 - (V4)', '11 - (V5)', '12 - (V6)'. Have in mind the plot size restrictions indicated."),
          h4(id = "Inst17", "To plot the specified signal segment, click on 'Show'."),
          hr(),
          sidebarLayout(
            sidebarPanel(
              selectInput("channelListShowSegment", "Select a channel from the ECG", c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")),
              h5("You must specify values of start and end points that differ more than 10 samples and less than 10.000 samples."),
              #Default values for segment plots are established at 5000 samples, starting at the first one.
              textInput("segmentStart", "From sample:", 1),
              textInput("segmentEnd", "To sample:", 5000),
              hr(),
              actionButton("butShowECGSegment","Show")
            ),
            mainPanel(
              plotOutput("plotSignalSegment")
            )
          )
        )
      )
    ),
    tabPanel(
      "Beat detection",
      icon = icon("heart", "fa-2x"),
      h3(textOutput("currentRecord4"), align = "center", tags$style("#currentRecord4{color: #337AB7; font-weight: bold;}")),
      hr(),
      h4(id = "Inst18", "You can perform beat detection over all the channels of the loaded ECG record. To obtain beat positions, you can click on 'Detect beats'. Please, note that this task will detect beats inside each one of the channels of the record, obtaining mono-channel beat positions. For multi-channel or global beat positions, please refer to the 'Global beat detection' submenu."),
      hr(),
      h4(id = "Inst19", "After this step, you will be able to:"),
      pre(id = "Inst20", h5("   -Export positions for the beats detected on a given channel (in the 'Export detected beats on a single channel' submenu)."),
          h5("   -Export positions for the beats detected on all of the channels of the record (in the 'Export detected beats on all channels' submenu)."),
          h5("   -Plot the signal with the positions obtained for the beats marked (in the 'Show detected beats' submenu)."),
          h5("   -Perform global or multi-channel detection (in the 'Global beat detection' submenu).")),
      hr(),
      actionButton("butDetectECG","Detect beats"),
      h4(textOutput("detectedAllChannelBeats"))
    ),
    tabPanel(
      paste(stri_dup(intToUtf8(160), 6),
            "Export detected beats on a single channel"),
      icon = icon("file-contract", "fa-2x"),
      h3(textOutput("currentRecord6"), align = "center", tags$style("#currentRecord6{color: #337AB7; font-weight: bold;}")),
      hr(),
      h4(id = "Inst23", "After selecting a channel from the ECG, and manually specifying the file name and a path, you can can export positions for the beats detected on the given channel to an ASCII file, by clicking on 'Export detected beats'."),
      h4(id = "Inst24", "Consider the following channel reciprocation when using 12-lead ECG records: '1 - (I)', '2 - (II)', '3 - (III)', '4 - (AVR)', '5 - (AVL)', '6 - (AVF)', '7 - (V1)', '8 - (V2)', '9 - (V3)', '10 - (V4)', '11 - (V5)', '12 - (V6)'."),
      hr(),
      sidebarLayout(
        sidebarPanel(
          textInput("fileNameBeats", "Specify a file name:", ""),
          textInput("filePathBeats", "Specify a file path:", ""),
          selectInput("channelListExportBeats", "Select a channel from the ECG", c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")),
          hr(),
          actionButton("butExportBeats","Export detected beats")
        ),
        mainPanel()
      ),
      h4(textOutput("exportedBeats"))
    ),
    tabPanel(
      paste(stri_dup(intToUtf8(160), 6),
            "Export detected beats on all channels"),
      icon = icon("file-medical-alt", "fa-2x"),
      h3(textOutput("currentRecord7"), align = "center", tags$style("#currentRecord7{color: #337AB7; font-weight: bold;}")),
      hr(),
      h4(id = "Inst25", "After manually specifying a root file name and a path, you can can export positions for the beats detected on each channel to an ASCII file, by clicking on 'Export detected beats'."),
      h4(id = "Inst26", "Each channel will create an ASCII file, with a name composed by the root file name given, followed by '_X', being X the channel number."),
      hr(),
      sidebarLayout(
        sidebarPanel(
          textInput("fileNameBeatsAllChannels", "Specify a file name:", ""),
          textInput("filePathBeatsAllChannels", "Specify a file path:", ""),
          hr(),
          actionButton("butExportBeatsAllCh","Export detected beats")
        ),
        mainPanel()
      ),
      h4(textOutput("exportedBeatsAllChannels"))
    ),
    tabPanel(
      paste(stri_dup(intToUtf8(160), 6),
            "Show detected beats"),
      icon = icon("check", "fa-2x"),
      tabsetPanel(
        tabPanel(
          "Show beats detected on all the signal",
          h3(textOutput("currentRecord8"), align = "center", tags$style("#currentRecord8{color: #337AB7; font-weight: bold;}")),
          hr(),
          h4(id = "Inst27", "By selecting a channel from the list, you can continuously plot the signal from the specified channel in the loaded ECG record, with the positions marked for the detected beats (in green)."),
          h4(id = "Inst28", "Consider the following channel reciprocation when using 12-lead ECG records: '1 - (I)', '2 - (II)', '3 - (III)', '4 - (AVR)', '5 - (AVL)', '6 - (AVF)', '7 - (V1)', '8 - (V2)', '9 - (V3)', '10 - (V4)', '11 - (V5)', '12 - (V6)'."),
          h4(id = "Inst29", "To start visualizing, click on 'Show detected beats'. The signal is refreshed in 5000-sample segments each 1.5 seconds. Once the end of the signal has been reached, the plot automatically starts again. Nevertheless, the plot can also be stopped at any time, by clicking on 'Stop'. With the plot stopped, you will be able to manually move backwards or advance a 5000-samples segment, using the 'Previous' and 'Next' buttons."),
          hr(),
          sidebarLayout(
            sidebarPanel(
              br(),
              br(),
              br(),
              br(),
              selectInput("channelListShowDetection", "Select a channel from the ECG", c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")),
              hr(),
              actionButton("butShowBeats","Show detected beats"),
              br(),
              actionButton("butShowBeatsStepPrevious"," << Previous"),
              actionButton("butShowBeatsStepNext","Next >>"),
              actionButton("butStopShowBeats","Stop"),
              br(),
              br(),
              br(),
              br()
            ),
            mainPanel(
              plotOutput("plotAllDetection")
            )
          )
        ),
        tabPanel(
          "Show beats detected on a segment of the signal",
          h3(textOutput("currentRecord9"), align = "center", tags$style("#currentRecord9{color: #337AB7; font-weight: bold;}")),
          hr(),
          h4(id = "Inst30", "By selecting a channel from the list, and manually specify the starting and ending samples, you can plot any given segment of the signal from the specified channel in the loaded ECG record, with the positions marked for the detected beats (in green)."),
          h4(id = "Inst31", "Consider the following channel reciprocation when using 12-lead ECG records: '1 - (I)', '2 - (II)', '3 - (III)', '4 - (AVR)', '5 - (AVL)', '6 - (AVF)', '7 - (V1)', '8 - (V2)', '9 - (V3)', '10 - (V4)', '11 - (V5)', '12 - (V6)'. Have in mind the plot size restrictions indicated."),
          h4(id = "Inst32", "To plot the specified signal segment, click on 'Show detected beats'."),
          hr(),
          sidebarLayout(
            sidebarPanel(
              selectInput("channelListShowDetectionSegment", "Select a channel from the ECG", c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")),
              h5("You must specify values of start and end points that differ more than 10 samples and less than 10.000 samples."),
              textInput("segmentStartDetection", "From sample:", 1),
              textInput("segmentEndDetection", "To sample:", 5000),
              hr(),
              actionButton("butShowBeatsSegment","Show detected beats")
            ),
            mainPanel(
              plotOutput("plotDetectionSegment")
            )
          )
        )
      )
    ),
    tabPanel(
      "Global beat detection",
      icon = icon("heartbeat", "fa-2x"),
      h3(textOutput("currentRecord10"), align = "center", tags$style("#currentRecord10{color: #337AB7; font-weight: bold;}")),
      hr(),
      h4(id = "Inst33", "You can perform global or multi-channel beat detection, using the positions of the beats detected on all the channels of the loaded ECG record, in the former beat detection phase. To obtain global beat positions, you can click on 'Obtain global beat positions'. These positions are computed by considering the presence of the same beat in the different channels. This ensures more precise beat positions than on mono-channel beat detection."),
      hr(),
      h4(id = "Inst34", "After this step, you will be able to:"),
      pre(id = "Inst35", h5("   -Plot the signal with the global positions obtained for the beats marked (in the 'Show global detected beats' submenu)."),
          h5("   -Create a basal beat (in the 'Create basal beat' submenu)."),
          h5("   -Delineate waves inside a global detected beat (in the 'Delineate waves in beat' submenu)."),
          h5("   -Obtain a candidate diagnose of the patient cardiac health status (in the 'Aided diagnosis' submenu).")),
      hr(),
      actionButton("butGlobal","Obtain global beat positions"),
      h4(textOutput("detectedGlobalBeats"))
    ),
    tabPanel(
      paste(stri_dup(intToUtf8(160), 6),
            "Show global detected beats"),
      icon = icon("check-double", "fa-2x"),
      tabsetPanel(
        tabPanel(
          "Show global beats detected on all the signal",
          h3(textOutput("currentRecord11"), align = "center", tags$style("#currentRecord11{color: #337AB7; font-weight: bold;}")),
          hr(),
          h4(id = "Inst36", "By selecting a channel from the list, you can continuously plot the signal from the specified channel in the loaded ECG record, with the positions marked for the global detected beats (in dashed blue)."),
          h4(id = "Inst37", "Consider the following channel reciprocation when using 12-lead ECG records: '1 - (I)', '2 - (II)', '3 - (III)', '4 - (AVR)', '5 - (AVL)', '6 - (AVF)', '7 - (V1)', '8 - (V2)', '9 - (V3)', '10 - (V4)', '11 - (V5)', '12 - (V6)'."),
          h4(id = "Inst38", "To start visualizing, click on 'Show global detected beats'. The signal is refreshed in 5000-sample segments each 1.5 seconds. Once the end of the signal has been reached, the plot automatically starts again. Nevertheless, the plot can also be stopped at any time, by clicking on 'Stop'. With the plot stopped, you will be able to manually move backwards or advance a 5000-samples segment, using the 'Previous' and 'Next' buttons."),
          hr(),
          sidebarLayout(
            sidebarPanel(
              br(),
              br(),
              br(),
              br(),
              selectInput("channelListShowGlobalDetection", "Select a channel from the ECG", c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")),
              hr(),
              actionButton("butShowGlobalBeats","Show global detected beats"),
              br(),
              actionButton("butShowGlobalBeatsStepPrevious"," << Previous"),
              actionButton("butShowGlobalBeatsStepNext","Next >>"),
              actionButton("butStopShowGlobalBeats","Stop"),
              br(),
              br(),
              br(),
              br()
            ),
            mainPanel(
              plotOutput("plotAllGlobalDetection")
            )
          )
        ),
        tabPanel(
          "Show global beats detected on a segment of the signal",
          h3(textOutput("currentRecord12"), align = "center", tags$style("#currentRecord12{color: #337AB7; font-weight: bold;}")),
          hr(),
          h4(id = "Inst39", "By selecting a channel from the list, and manually specify the starting and ending samples, you can plot any given segment of the signal from the specified channel in the loaded ECG record, with the positions marked for the global detected beats (in dashed blue)."),
          h4(id = "Inst40", "Consider the following channel reciprocation when using 12-lead ECG records: '1 - (I)', '2 - (II)', '3 - (III)', '4 - (AVR)', '5 - (AVL)', '6 - (AVF)', '7 - (V1)', '8 - (V2)', '9 - (V3)', '10 - (V4)', '11 - (V5)', '12 - (V6)'. To visualize it, click on 'Show'. Have in mind the plot size restrictions indicated."),
          h4(id = "Inst41", "To start visualizing, click on 'Show global detected beats'."),
          hr(),
          sidebarLayout(
            sidebarPanel(
              selectInput("channelListShowGlobalDetectionSegment", "Select a channel from the ECG", c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")),
              h5("You must specify values of start and end points that differ more than 10 samples and less than 10.000 samples."),
              textInput("segmentStartGlobalDetection", "From sample:", 1),
              textInput("segmentEndGlobalDetection", "To sample:", 5000),
              hr(),
              actionButton("butShowGlobalBeatsSegment","Show global detected beats")
            ),
            mainPanel(
              plotOutput("plotGlobalDetectionSegment")
            )
          )
        ),
        tabPanel(
          "Compare global and individual beats detected on all signal",
          h3(textOutput("currentRecord13"), align = "center", tags$style("#currentRecord13{color: #337AB7; font-weight: bold;}")),
          hr(),
          h4(id = "Inst42", "By selecting a channel from the list, you can continuously plot the signal from the specified channel in the loaded ECG record, with the positions marked for the detected beats on the channel (in green), and for the global detected beats (in dashed blue)."),
          h4(id = "Inst43", "Consider the following channel reciprocation when using 12-lead ECG records: '1 - (I)', '2 - (II)', '3 - (III)', '4 - (AVR)', '5 - (AVL)', '6 - (AVF)', '7 - (V1)', '8 - (V2)', '9 - (V3)', '10 - (V4)', '11 - (V5)', '12 - (V6)'."),
          h4(id = "Inst44", "To start visualizing, click on 'Show global & individual detected beats'. The signal is refreshed in 5000-sample segments each 1.5 seconds. Once the end of the signal has been reached, the plot automatically starts again. Nevertheless, the plot can also be stopped at any time, by clicking on 'Stop'. With the plot stopped, you will be able to manually move backwards or advance a 5000-samples segment, using the 'Previous' and 'Next' buttons."),
          hr(),
          sidebarLayout(
            sidebarPanel(
              br(),
              br(),
              br(),
              br(),
              selectInput("channelListCompareGlobalDetection", "Select a channel from the ECG", c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")),
              hr(),
              actionButton("butCompareGlobalBeats","Show global & individual detected beats"),
              br(),
              actionButton("butCompareGlobalBeatsStepPrevious"," << Previous"),
              actionButton("butCompareGlobalBeatsStepNext","Next >>"),
              actionButton("butStopCompareGlobalBeats","Stop"),
              br(),
              br(),
              br(),
              br()
            ),
            mainPanel(
              plotOutput("plotCompareAllGlobalDetection")
            )
          )
        ),
        tabPanel(
          "Compare global and individual beats detected on a segment of the signal",
          h3(textOutput("currentRecord14"), align = "center", tags$style("#currentRecord14{color: #337AB7; font-weight: bold;}")),
          hr(),
          h4(id = "Inst45", "By selecting a channel from the list, and manually specify the starting and ending samples, you can plot any given segment of the signal from the specified channel in the loaded ECG record, with the positions marked for the detected beats on the channel (in green), and for the global detected beats (in dashed blue)."),
          h4(id = "Inst46", "Consider the following channel reciprocation when using 12-lead ECG records: '1 - (I)', '2 - (II)', '3 - (III)', '4 - (AVR)', '5 - (AVL)', '6 - (AVF)', '7 - (V1)', '8 - (V2)', '9 - (V3)', '10 - (V4)', '11 - (V5)', '12 - (V6)'. To visualize it, click on 'Show'. Have in mind the plot size restrictions indicated."),
          h4(id = "Inst47", "To start visualizing, click on 'Show global & individual detected beats'."),
          hr(),
          sidebarLayout(
            sidebarPanel(
              selectInput("channelListCompareGlobalDetectionSegment", "Select a channel from the ECG", c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")),
              h5("You must specify values of start and end points that differ more than 10 samples and less than 10.000 samples."),
              textInput("segmentStartCompareGlobalDetection", "From sample:", 1),
              textInput("segmentEndCompareGlobalDetection", "To sample:", 5000),
              hr(),
              actionButton("butCompareGlobalBeatsSegment","Show global & individual detected beats")
            ),
            mainPanel(
              plotOutput("plotCompareGlobalDetectionSegment")
            )
          )
        )
      )
    ),
    tabPanel(
      "Create basal beat",
      icon = icon("eye-dropper", "fa-2x"),
      h3(textOutput("currentRecord15"), align = "center", tags$style("#currentRecord15{color: #337AB7; font-weight: bold;}")),
      hr(),
      h4(id = "Inst48", "A basal beat is an average or idealized beat created for a given one, by using some previous and posterior beats, and certain signal segment before and after the QRS (or R peak) position."),
      h4(id = "Inst49", "You can create a customized basal beat, by selecting the channel of the ECG record, manually specifying the number of the beat amongst the global detected beat positions, the ms to use before the QRS position, the ms to use after the QRS position, and the amount of previous and posterior beats. Predefined values are also offered."),
      h4(id = "Inst50", "Consider the following channel reciprocation when using 12-lead ECG records: '1 - (I)', '2 - (II)', '3 - (III)', '4 - (AVR)', '5 - (AVL)', '6 - (AVF)', '7 - (V1)', '8 - (V2)', '9 - (V3)', '10 - (V4)', '11 - (V5)', '12 - (V6)'."),
      hr(),
      sidebarLayout(
        sidebarPanel(
          selectInput("channelListBasalBeat", "Select a channel from the ECG", c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")),
          #Basal beat creation menu will offer predefined values, that can be modified by the user.
          textInput("beatNo", "Base beat:", 6),
          textInput("msBefore", "ms to use before the QRS location:", 200),
          textInput("msAfter", "ms to use after the QRS location:", 500),
          textInput("beatsBeforeAfter", "Beats used before and after the selected beat:", 5),
          hr(),
          actionButton("butBasalBeat","Compute")
        ),
        mainPanel(
          plotOutput("plotBasal")
        )
      )
    ),
    tabPanel(
      "Delineate waves in beat",
      icon = icon("hand-holding-heart", "fa-2x"),
      icon2 = icon("signature", "fa-2x"),
      h3(textOutput("currentRecord16"), align = "center", tags$style("#currentRecord16{color: #337AB7; font-weight: bold;}")),
      hr(),
      h4(id = "Inst51", "You can delineate waves in the basal beat of a specified beat, by selecting the channel of the ECG record, and manually specifying the number of the beat amongst the global detected beat positions."),
      h4(id = "Inst52", "After this process, the basal beat will be represented below, whith its delineated waves highlighted in colors (green for P wave, red for QRS complex, and blue for T wave), and its P, Q, R, S and T peaks marked on the signal."),
      h4(id = "Inst53", "Consider the following channel reciprocation when using 12-lead ECG records: '1 - (I)', '2 - (II)', '3 - (III)', '4 - (AVR)', '5 - (AVL)', '6 - (AVF)', '7 - (V1)', '8 - (V2)', '9 - (V3)', '10 - (V4)', '11 - (V5)', '12 - (V6)'."),
      hr(),
      sidebarLayout(
        sidebarPanel(
          br(),
          br(),
          br(),
          selectInput("channelListDelineate", "Select a channel from the ECG", c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")),
          textInput("beatNum", "Base beat:", 6),
          hr(),
          actionButton("butDelineate","Delineate beat"),
          br(),
          br(),
          br()
        ),
        mainPanel(
          plotOutput("plotDelineation")
        )
      )
    ),
    tabPanel(
      "Aided diagnosis",
      icon = icon("user-md", "fa-2x"),
      h3(textOutput("currentRecord17"), align = "center", tags$style("#currentRecord17{color: #337AB7; font-weight: bold;}")),
      hr(),
      h4(id = "Inst54", "You can obtain a candidate diagnose, offered by the system after evaluating waves, peaks and intervals inside a series of basal beats created using beats of the channels available, and inside each channel,from different segments of the signal."),
      h4("The system provides a list of possible medical conditions affecting the patient, with their scores:"),
      h4("   -All six diseases the system is capable of diagnose appear listed in the box below."),
      h4("   -Each disease has its score of being present in the current record, expressed in the 0.00-1.00 range."),
      h4("   -Bigger scores show bigger likelihood. Scores above 0.20-0.25 should be taken into consideration."),
      h4("   -Remember, the user is always the responsible of the final decision."),
      hr(),
      h2(textOutput("channelAndBeat")),
      sidebarLayout(
        sidebarPanel(
          h4("Disease probability:"),
          h5(textOutput("predBradycardia")),
          h5(textOutput("predIschemia")),
          h5(textOutput("predMyocardialInfarction")),
          h5(textOutput("predTachycardia")),
		  h5(textOutput("predVentricularHipertrophy")),											   
          h5(textOutput("predWPWSyndrome")),
          hr(),
          actionButton("butDiagnose","Obtain a candidate diagnose for this patient")
        ),
        mainPanel(
          plotOutput("plotDelineationDiagnose")
        )
      )
    ),
    tabPanel(
      "Settings",
      #Settings menu allows the user to hide unnecessary instructions and to show them again, to get statistic information about the current record and its processing, and to learn more about the current ECGDT version.
      icon = icon("cogs", "fa-2x"),
      h3(textOutput("currentRecord18"), align = "center", tags$style("#currentRecord18{color: #337AB7; font-weight: bold;}")),
      hr(),
      h2("Expert mode - Instructions over the application"),
      h4("If you have used the application previously and know how everything works, you can activate 'Expert mode' to hide some instructions over the different menus, in order to have more room for the content (for example, for plots)."),
      h4("You can switch it off whenever you want, to show again all the information."),
      switchInput(inputId = "switchExpertMode", value = FALSE),
      hr(),
      h2("Statistics for the current record"),
      h4("You can obtain more information about the record you are currently working with. This option is only available when the record is properly loaded. Statistics about beats detected on all channels and global beats obtained will be shown once this tasks are processed."),
      actionButton("butStatistics","Show more info about the current record"),
      hr(),
      h2("About"),
      h4("You can obtain more information for the ECGDT current version."),
      actionButton("butMore","Show info about ECGDT"),
      hr()
    )
  )
    )
server <- function(input, output, session) {
  #Once the R session has ended, the application will be killed.
  session$onSessionEnded(stopApp)
  #Reactive variables are used all along the application to retain their contents between the different menus.
  positionSignal <- reactiveValues(counter = 1)
  positionBeats <- reactiveValues(counter = 1)
  positionGlobalBeats <- reactiveValues(counter = 1)
  positionGlobalBeatsCompare <- reactiveValues(counter = 1)
  rv <- reactiveValues(ECGData = ECGData)
  channels <- c()
  #Messages with the amount of detected beats for statistics will be a text if the beats have not been detected yet.
  detectedBeatsPerChannel <- reactiveValues(beats = c("Beats not available yet. To obtain this statistic, detect beats before in the 'Beat detection' menu"))
  globalDetectedBeats <- reactiveValues(beats = c("Global beats not available yet. To obtain this statistic, detect global beats before in the 'Global beat detection' menu."))
  #Some buttons will be disabled until the option is available.
  #The user cannot load a record until the file isn't uploaded to the server.
  shinyjs::disable("butLoadECGASCII")
  shinyjs::disable("butLoadAnotherECGASCII")
  shinyjs::disable("butLoadECGWFDB")
  shinyjs::disable("butLoadAnotherECGWFDB")
  shinyjs::disable("butShowInstructions")
  shinyjs::disable("butShowECGStepPrevious")
  shinyjs::disable("butShowECGStepNext")
  #Plots cannot be displayed until their data is available.
  shinyjs::disable("butStopShowECG")
  shinyjs::disable("butShowBeatsStepPrevious")
  shinyjs::disable("butShowBeatsStepNext")
  shinyjs::disable("butStopShowBeats")
  shinyjs::disable("butShowGlobalBeatsStepPrevious")
  shinyjs::disable("butShowGlobalBeatsStepNext")
  shinyjs::disable("butStopShowGlobalBeats")
  shinyjs::disable("butCompareGlobalBeatsStepPrevious")
  shinyjs::disable("butCompareGlobalBeatsStepNext")
  shinyjs::disable("butStopCompareGlobalBeats")
  shinyjs::disable("butStatistics")
  #Some menus will be hidden until the data they use is available.
  hideTab(inputId = "Menu", target = paste(stri_dup(intToUtf8(160), 6),"Export ECG"))
  hideTab(inputId = "Menu", target = paste(stri_dup(intToUtf8(160), 6),"Show Signal"))
  hideTab(inputId = "Menu", target = "Beat detection")
  hideTab(inputId = "Menu", target = paste(stri_dup(intToUtf8(160), 6),"Export detected beats on a single channel"))
  hideTab(inputId = "Menu", target = paste(stri_dup(intToUtf8(160), 6),"Export detected beats on all channels"))
  hideTab(inputId = "Menu", target = paste(stri_dup(intToUtf8(160), 6),"Show detected beats"))
  hideTab(inputId = "Menu", target = "Global beat detection")
  hideTab(inputId = "Menu", target = paste(stri_dup(intToUtf8(160), 6),"Show global detected beats"))
  hideTab(inputId = "Menu", target = "Create basal beat")
  hideTab(inputId = "Menu", target = "Delineate waves in beat")
  hideTab(inputId = "Menu", target = "Aided diagnosis")
  
  #When a file is uploaded to the server, the buttons to load the record into the system will be enabled.
  observeEvent(input$ECGRecordFile, {
    shinyjs::enable("butLoadECGASCII")
    shinyjs::enable("butLoadECGWFDB")
  })
  
  observeEvent(input$butLoadECGASCII, {
    withProgress(message = paste("Loading File", input$ECGRecordFile$name), value = 1,
                 {
                   rv$ECGData <- LoadECGAscii(rv$ECGData, input$ECGRecordFile$datapath, as.numeric(input$calibrationFactor))
                   #With the record loaded on the system, the user will be able to plot the signal, detect beats and export the ECG record; so that, these options will be shown in the menu.
                   showTab(inputId = "Menu", target = paste(stri_dup(intToUtf8(160), 6),"Export ECG"))
                   showTab(inputId = "Menu", target = paste(stri_dup(intToUtf8(160), 6),"Show Signal"))
                   showTab(inputId = "Menu", target = "Beat detection")
                   #Load buttons will be disabled. Instead, buttons to load another file will be enabled. If the user wants to work with another record, besides the file loading, the system will also restart its values, hide menus, disable buttons,...
                   shinyjs::disable("butLoadECGASCII")
                   shinyjs::disable("butLoadECGWFDB")
                   shinyjs::enable("butLoadAnotherECGASCII")
                   shinyjs::enable("butLoadAnotherECGWFDB")
                 })
    output$loadedFileASCII <- renderText({print(paste("File", input$ECGRecordFile$name, "successfully loaded"))})
    shinyjs::show("loadedFileASCII")
    shinyjs::enable("butStatistics")
    #When a record is loaded, a hint will appear on top of each screen, showing the id of the record the user are working with.
    output$currentRecord1 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
    output$currentRecord2 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
    output$currentRecord3 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
    output$currentRecord4 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
    # output$currentRecord5 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
    output$currentRecord6 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
    output$currentRecord7 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
    output$currentRecord8 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
    output$currentRecord9 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
    output$currentRecord10 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
    output$currentRecord11 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
    output$currentRecord12 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
    output$currentRecord13 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
    output$currentRecord14 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
    output$currentRecord15 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
    output$currentRecord16 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
    output$currentRecord17 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
    output$currentRecord18 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
  })
  
  observeEvent(input$butLoadAnotherECGASCII, {
    #If the user wants to work with another ECG record, the system will restart its status.
    ECGData <- baseECGData
    #To hide again non available menu options and buttons, when loading a new file without closing the app.
    shinyjs::disable("butLoadECGASCII")
    shinyjs::disable("butLoadECGWFDB")
    shinyjs::disable("butShowECGStepPrevious")
    shinyjs::disable("butShowECGStepNext")
    #Plots cannot be displayed until their data is available.
    shinyjs::disable("butStopShowECG")
    shinyjs::enable("butShowECG")
    shinyjs::disable("butShowBeatsStepPrevious")
    shinyjs::disable("butShowBeatsStepNext")
    shinyjs::disable("butStopShowBeats")
    shinyjs::enable("butShowBeats")
    shinyjs::disable("butShowGlobalBeatsStepPrevious")
    shinyjs::disable("butShowGlobalBeatsStepNext")
    shinyjs::disable("butStopShowGlobalBeats")
    shinyjs::enable("butShowGlobalBeats")
    shinyjs::disable("butCompareGlobalBeatsStepPrevious")
    shinyjs::disable("butCompareGlobalBeatsStepNext")
    shinyjs::disable("butStopCompareGlobalBeats")
    shinyjs::enable("butCompareGlobalBeats")
    shinyjs::disable("butStatistics")
    detectedBeatsPerChannel$beats <- c("Beats not available yet. To obtain this statistic, detect beats before in the 'Beat detection' menu")
    globalDetectedBeats$beats <- c("Global beats not available yet. To obtain this statistic, detect global beats before in the 'Global beat detection' menu.")
    hideTab(inputId = "Menu", target = paste(stri_dup(intToUtf8(160), 6),"Export ECG"))
    hideTab(inputId = "Menu", target = paste(stri_dup(intToUtf8(160), 6),"Show Signal"))
    hideTab(inputId = "Menu", target = "Beat detection")
    hideTab(inputId = "Menu", target = paste(stri_dup(intToUtf8(160), 6),"Export detected beats on a single channel"))
    hideTab(inputId = "Menu", target = paste(stri_dup(intToUtf8(160), 6),"Export detected beats on all channels"))
    hideTab(inputId = "Menu", target = paste(stri_dup(intToUtf8(160), 6),"Show detected beats"))
    hideTab(inputId = "Menu", target = "Global beat detection")
    hideTab(inputId = "Menu", target = paste(stri_dup(intToUtf8(160), 6),"Show global detected beats"))
    hideTab(inputId = "Menu", target = "Create basal beat")
    hideTab(inputId = "Menu", target = "Delineate waves in beat")
    hideTab(inputId = "Menu", target = "Aided diagnosis")
    #Input values will be reset from previous contents.
    shinyjs::reset("ECGRecordFile")
    shinyjs::reset("calibrationFactor")
    shinyjs::hide("loadedFileASCII")
    shinyjs::hide("loadedFileWFDB")
    shinyjs::reset("fileNameECG")
    shinyjs::reset("filePathECG")
    shinyjs::hide("exportedECG")
    positionSignal$counter <- 1
    shinyjs::hide("plotAllSignal")
    shinyjs::reset("segmentStart")
    shinyjs::reset("segmentEnd")
    shinyjs::hide("plotSignalSegment")
    shinyjs::hide("detectedAllChannelBeats")
    shinyjs::hide("detectedChannelBeats")
    shinyjs::reset("fileNameBeats")
    shinyjs::reset("filePathBeats")
    shinyjs::hide("exportedBeats")
    shinyjs::reset("fileNameBeatsAllChannels")
    shinyjs::reset("filePathBeatsAllChannels")
    shinyjs::hide("exportedBeatsAllChannels")
    positionBeats$counter <- 1
    shinyjs::hide("plotAllDetection")
    shinyjs::reset("segmentStartDetection")
    shinyjs::reset("segmentEndDetection")
    shinyjs::hide("plotDetectionSegment")
    shinyjs::hide("detectedGlobalBeats")
    positionGlobalBeats$counter <- 1
    shinyjs::hide("plotAllGlobalDetection")
    shinyjs::reset("segmentStartGlobalDetection")
    shinyjs::reset("segmentEndGlobalDetection")
    shinyjs::hide("plotGlobalDetectionSegment")
    positionGlobalBeatsCompare$counter <- 1
    shinyjs::hide("plotCompareAllGlobalDetection")
    shinyjs::reset("segmentStartCompareGlobalDetection")
    shinyjs::reset("segmentEndCompareGlobalDetection")
    shinyjs::hide("plotCompareGlobalDetectionSegment")
    shinyjs::reset("beatNo")
    shinyjs::reset("msBefore")
    shinyjs::reset("msAfter")
    shinyjs::reset("beatsBeforeAfter")
    shinyjs::hide("plotBasal")
    shinyjs::reset("beatNum")
    shinyjs::hide("plotDelineation")
    shinyjs::hide("predBradycardia")
    shinyjs::hide("predIschemia")
    shinyjs::hide("predMyocardialInfarction")
    shinyjs::hide("predTachycardia")
    shinyjs::hide("predVentricularHipertrophy")
    shinyjs::hide("predWPWSyndrome")
  })
  
  observeEvent(input$butLoadECGWFDB, {
    withProgress(message = paste("Loading File", input$ECGRecordFile$name), value = 1,
                 {
                   rv$ECGData <- LoadECGWFDB(rv$ECGData, "ECGRecordFile", input$typeWFDBList, "/home/vmvisunha/ECG_DBs/")
                   showTab(inputId = "Menu", target = paste(stri_dup(intToUtf8(160), 6),"Export ECG"))
                   showTab(inputId = "Menu", target = paste(stri_dup(intToUtf8(160), 6),"Show Signal"))
                   showTab(inputId = "Menu", target = "Beat detection")
                   shinyjs::disable("butLoadECGASCII")
                   shinyjs::disable("butLoadECGWFDB")
                   shinyjs::enable("butLoadAnotherECGASCII")
                   shinyjs::enable("butLoadAnotherECGWFDB")
                 })
    output$loadedFileWFDB <- renderText({print(paste("File", input$ECGRecordFile$name, "successfully loaded"))})
    shinyjs::show("loadedFileWFDB")
    #When a record is loaded, a hint will appear on top of each screen, showing the id of the record the user are working with.
    output$currentRecord1 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
    output$currentRecord2 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
    output$currentRecord3 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
    output$currentRecord4 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
    output$currentRecord6 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
    output$currentRecord7 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
    output$currentRecord8 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
    output$currentRecord9 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
    output$currentRecord10 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
    output$currentRecord11 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
    output$currentRecord12 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
    output$currentRecord13 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
    output$currentRecord14 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
    output$currentRecord15 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
    output$currentRecord16 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
    output$currentRecord17 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
    output$currentRecord18 <- renderText({print(paste("You are currently working with record", rv$ECGData$id))})
  })
  
  observeEvent(input$butLoadAnotherECGWFDB, {
    ECGData <- baseECGData
    #To hide again non available menu options and buttons, when loading a new file without closing the app.
    shinyjs::disable("butLoadECGASCII")
    shinyjs::disable("butLoadECGWFDB")              
    shinyjs::disable("butShowECGStepPrevious")
    shinyjs::disable("butShowECGStepNext")
    #Plots cannot be displayed until their data is available.
    shinyjs::disable("butStopShowECG")
    shinyjs::enable("butShowECG")
    shinyjs::disable("butShowBeatsStepPrevious")
    shinyjs::disable("butShowBeatsStepNext")
    shinyjs::disable("butStopShowBeats")
    shinyjs::enable("butShowBeats")
    shinyjs::disable("butShowGlobalBeatsStepPrevious")
    shinyjs::disable("butShowGlobalBeatsStepNext")
    shinyjs::disable("butStopShowGlobalBeats")
    shinyjs::enable("butShowGlobalBeats")
    shinyjs::disable("butCompareGlobalBeatsStepPrevious")
    shinyjs::disable("butCompareGlobalBeatsStepNext")
    shinyjs::disable("butStopCompareGlobalBeats")
    shinyjs::enable("butCompareGlobalBeats")
    shinyjs::disable("butStatistics")
    detectedBeatsPerChannel$beats <- c("Beats not available yet. To obtain this statistic, detect beats before in the 'Beat detection' menu")
    globalDetectedBeats$beats <- c("Global beats not available yet. To obtain this statistic, detect global beats before in the 'Global beat detection' menu.")
    hideTab(inputId = "Menu", target = paste(stri_dup(intToUtf8(160), 6),"Export ECG"))
    hideTab(inputId = "Menu", target = paste(stri_dup(intToUtf8(160), 6),"Show Signal"))
    hideTab(inputId = "Menu", target = "Beat detection")
    hideTab(inputId = "Menu", target = paste(stri_dup(intToUtf8(160), 6),"Export detected beats on a single channel"))
    hideTab(inputId = "Menu", target = paste(stri_dup(intToUtf8(160), 6),"Export detected beats on all channels"))
    hideTab(inputId = "Menu", target = paste(stri_dup(intToUtf8(160), 6),"Show detected beats"))
    hideTab(inputId = "Menu", target = "Global beat detection")
    hideTab(inputId = "Menu", target = paste(stri_dup(intToUtf8(160), 6),"Show global detected beats"))
    hideTab(inputId = "Menu", target = "Create basal beat")
    hideTab(inputId = "Menu", target = "Delineate waves in beat")
    hideTab(inputId = "Menu", target = "Aided diagnosis")
    #Input values will be reset from previous contents.
    shinyjs::reset("ECGRecordFile")
    shinyjs::reset("calibrationFactor")
    shinyjs::hide("loadedFileASCII")
    shinyjs::hide("loadedFileWFDB")
    shinyjs::reset("fileNameECG")
    shinyjs::reset("filePathECG")
    shinyjs::hide("exportedECG")
    positionSignal$counter <- 1
    shinyjs::hide("plotAllSignal")
    shinyjs::reset("segmentStart")
    shinyjs::reset("segmentEnd")
    shinyjs::hide("plotSignalSegment")
    shinyjs::hide("detectedAllChannelBeats")
    shinyjs::hide("detectedChannelBeats")
    shinyjs::reset("fileNameBeats")
    shinyjs::reset("filePathBeats")
    shinyjs::hide("exportedBeats")
    shinyjs::reset("fileNameBeatsAllChannels")
    shinyjs::reset("filePathBeatsAllChannels")
    shinyjs::hide("exportedBeatsAllChannels")
    positionBeats$counter <- 1
    shinyjs::hide("plotAllDetection")
    shinyjs::reset("segmentStartDetection")
    shinyjs::reset("segmentEndDetection")
    shinyjs::hide("plotDetectionSegment")
    shinyjs::hide("detectedGlobalBeats")
    positionGlobalBeats$counter <- 1
    shinyjs::hide("plotAllGlobalDetection")
    shinyjs::reset("segmentStartGlobalDetection")
    shinyjs::reset("segmentEndGlobalDetection")
    shinyjs::hide("plotGlobalDetectionSegment")
    positionGlobalBeatsCompare$counter <- 1
    shinyjs::hide("plotCompareAllGlobalDetection")
    shinyjs::reset("segmentStartCompareGlobalDetection")
    shinyjs::reset("segmentEndCompareGlobalDetection")
    shinyjs::hide("plotCompareGlobalDetectionSegment")
    shinyjs::reset("beatNo")
    shinyjs::reset("msBefore")
    shinyjs::reset("msAfter")
    shinyjs::reset("beatsBeforeAfter")
    shinyjs::hide("plotBasal")
    shinyjs::reset("beatNum")
    shinyjs::hide("plotDelineation")
    shinyjs::hide("predBradycardia")
    shinyjs::hide("predIschemia")
    shinyjs::hide("predMyocardialInfarction")
    shinyjs::hide("predTachycardia")
    shinyjs::hide("predVentricularHipertrophy")
    shinyjs::hide("predWPWSyndrome")
  })
  
  observeEvent(input$butExportECG, {
    withProgress(message = 'Exporting ECG...', value = 1,
                 {  
                   ExportECG(rv$ECGData, input$fileNameECG, input$filePathECG)
                 })
    output$exportedECG <- renderText({"ECG successfully exported"})
    shinyjs::show("exportedECG")
  })
  
  #Channel selectors will have initially 12 values, but will be dynamically adapted to the number of channels the record has.
  observe({
    channelAmount <- as.numeric(rv$ECGData$nLeads)
    updateSelectInput(session = session, 
                      inputId = "channelListShow",
                      choices = head(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), n = channelAmount)
    )
  })
  
  observe({
    channelAmount <- as.numeric(rv$ECGData$nLeads)
    updateSelectInput(session = session, 
                      inputId = "channelListShowSegment",
                      choices = head(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), n = channelAmount)
    )
  })
  
  observe({
    channelAmount <- as.numeric(rv$ECGData$nLeads)
    updateSelectInput(session = session, 
                      inputId = "channelListExportBeats",
                      choices = head(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), n = channelAmount)
    )
  })
  
  observe({
    channelAmount <- as.numeric(rv$ECGData$nLeads)
    updateSelectInput(session = session, 
                      inputId = "channelListShowDetection",
                      choices = head(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), n = channelAmount)
    )
  })
  
  observe({
    channelAmount <- as.numeric(rv$ECGData$nLeads)
    updateSelectInput(session = session, 
                      inputId = "channelListShowDetectionSegment",
                      choices = head(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), n = channelAmount)
    )
  })
  
  observe({
    channelAmount <- as.numeric(rv$ECGData$nLeads)
    updateSelectInput(session = session, 
                      inputId = "channelListShowGlobalDetection",
                      choices = head(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), n = channelAmount)
    )
  })
  
  observe({
    channelAmount <- as.numeric(rv$ECGData$nLeads)
    updateSelectInput(session = session, 
                      inputId = "channelListShowGlobalDetectionSegment",
                      choices = head(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), n = channelAmount)
    )
  })
  
  observe({
    channelAmount <- as.numeric(rv$ECGData$nLeads)
    updateSelectInput(session = session, 
                      inputId = "channelListCompareGlobalDetection",
                      choices = head(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), n = channelAmount)
    )
  })
  
  observe({
    channelAmount <- as.numeric(rv$ECGData$nLeads)
    updateSelectInput(session = session, 
                      inputId = "channelListCompareGlobalDetectionSegment",
                      choices = head(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), n = channelAmount)
    )
  })
  
  observe({
    channelAmount <- as.numeric(rv$ECGData$nLeads)
    updateSelectInput(session = session, 
                      inputId = "channelListBasalBeat",
                      choices = head(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), n = channelAmount)
    )
  })
  
  observe({
    channelAmount <- as.numeric(rv$ECGData$nLeads)
    updateSelectInput(session = session, 
                      inputId = "channelListDelineate",
                      choices = head(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), n = channelAmount)
    )
  })
  
  #The Previous button cannot be clicked once the signal start has been reached.
  observeEvent(input$butShowECGStepPrevious,{
    if(positionSignal$counter < 5000)
    {
      shinyjs::disable("butShowECGStepPrevious")
    }
    else
    {
      #If this action is allowed, the position will be reduced in 5000 samples.
      positionSignal$counter <- isolate(positionSignal$counter)-5000
    }
    
  })
  
  #The Next button cannot be clicked once the signal end has been reached.
  observeEvent(input$butShowECGStepNext, {
    if(positionSignal$counter > length(rv$ECGData$lead[[as.numeric(input$channelListShow)]]$val)-5000)
    {
      shinyjs::disable("butShowECGStepNext")
    }
    else
    {
      #If this action is allowed, the position will be augmented in 5000 samples.
      positionSignal$counter <- isolate(positionSignal$counter)+5000
    }
  })
  
  
  #Stop button will only be shown if the plot is active. Show and step buttons will be automatically disabled once the plot has started.
  observeEvent(input$butShowECG, {
    shinyjs::disable("butStopShowECG")
    shinyjs::enable("butStopShowECG")
    shinyjs::disable("butShowECG")
    shinyjs::disable("butShowECGStepPrevious")
    shinyjs::disable("butShowECGStepNext")
    shinyjs::show("plotAllSignal")
    output$plotAllSignal <- renderPlot({
      stopShowECG <- isolate(input$butStopShowECG)
      showECG <- isolate(input$butShowECG)
	  
      if(stopShowECG < showECG)
      {
        #When the plot is in automatic mode, it will be refreshed each 1500 ms.
        invalidateLater(millis = 1500, session = getDefaultReactiveDomain())
      }
	  
      #Every time the stop button is clicked, the plot remains static in the last rendered segment. Show and step buttons will be enabled, and stop button will be disabled.
      if(stopShowECG >= showECG)
      {
        isolate(PlotECGSegment(rv$ECGData, as.numeric(input$channelListShow), positionSignal$counter, positionSignal$counter+4999))
        positionSignal$counter <- isolate(positionSignal$counter)-5000
        shinyjs::disable("butStopShowECG")
        shinyjs::enable("butShowECG")
        shinyjs::enable("butShowECGStepPrevious")
        shinyjs::enable("butShowECGStepNext")
      }

      positionSignal$counter <- isolate(positionSignal$counter)+5000
	  
      if(positionSignal$counter < length(rv$ECGData$lead[[as.numeric(input$channelListShow)]]$val) && stopShowECG < showECG)
      {
        PlotECGSegment(rv$ECGData, as.numeric(input$channelListShow), positionSignal$counter, positionSignal$counter+4999)
      }
	  
      if(positionSignal$counter >= length(rv$ECGData$lead[[as.numeric(input$channelListShow)]]$val))
      {
        isolate(PlotECGSegment(rv$ECGData, as.numeric(input$channelListShow), positionSignal$counter-4999, positionSignal$counter))
        positionSignal$counter <- 1
      }
    })
	
  })
  
  observeEvent(input$butShowECGSegment, {
    shinyjs::show("plotSignalSegment")
    output$plotSignalSegment <- renderPlot({PlotECGSegment(rv$ECGData, as.numeric(input$channelListShowSegment), as.numeric(input$segmentStart), as.numeric(input$segmentEnd))})
  })
  
  observeEvent(input$butDetectECG, {
    withProgress(message = 'Detecting beats...', value = 1,
                 {  
                   rv$ECGData <- QRSDetectionAllCh(rv$ECGData)
                   showTab(inputId = "Menu", target = paste(stri_dup(intToUtf8(160), 6),"Show detected beats"))
                   showTab(inputId = "Menu", target = paste(stri_dup(intToUtf8(160), 6),"Export detected beats on a single channel"))
                   showTab(inputId = "Menu", target = paste(stri_dup(intToUtf8(160), 6),"Export detected beats on all channels"))
                   showTab(inputId = "Menu", target = "Global beat detection")
                 })
    output$detectedAllChannelBeats <- renderText({"Beats successfully detected on all channels"})
    detectedBeatsPerChannel$beats <- c()
    
    #Once beat detection has been performed, the detected beats message for the statistics updates its contents to show the amounts of beats detected.
    for (j in 1:rv$ECGData$nLeads)
    {
      detectedBeatsPerChannel$beats <- c(detectedBeatsPerChannel$beats, paste(rv$ECGData$lead[[j]]$id, ": ", length(rv$ECGData$lead[[j]]$beat),  sep = "", " beats"))
    }
    
    shinyjs::show("detectedAllChannelBeats")
  })
  
  observeEvent(input$butExportBeats, {
    withProgress(message = 'Exporting beats...', value = 1,
                 {  
                   ExportBeats(rv$ECGData, as.numeric(input$channelListExportBeats), input$fileNameBeats, input$filePathBeats)
                 })
    output$exportedBeats <- renderText({"Channel beats successfully exported"})
    shinyjs::show("exportedBeats")
  })
  
  observeEvent(input$butExportBeatsAllCh, {
    withProgress(message = 'Exporting beats...', value = 1,
                 {  
                   ExportBeatsAllCh(rv$ECGData, input$fileNameBeatsAllChannels, input$filePathBeatsAllChannels)
                 })
    output$exportedBeatsAllChannels <- renderText({"All channel beats successfully exported"})
    shinyjs::show("exportedBeatsAllChannels")
  })
  
  observeEvent(input$butShowBeatsStepPrevious,{
    if(positionBeats$counter < 5000)
    {
      shinyjs::disable("butShowBeatsStepPrevious")
    }
    else
    {
      positionBeats$counter <- isolate(positionBeats$counter)-5000
    }
    
  })
  
  observeEvent(input$butShowBeatsStepNext, {
    if(positionBeats$counter > length(rv$ECGData$lead[[as.numeric(input$channelListShowDetection)]]$val)-5000)
    {
      shinyjs::disable("butShowBeatsStepNext")
    }
    else
    {
      positionBeats$counter <- isolate(positionBeats$counter)+5000
    }
  })
  
  observeEvent(input$butShowBeats, {
    shinyjs::enable("butStopShowBeats")
    shinyjs::disable("butShowBeats")
    shinyjs::disable("butShowBeatsStepPrevious")
    shinyjs::disable("butShowBeatsStepNext")
    shinyjs::show("plotAllDetection")
    output$plotAllDetection <- renderPlot({
      stopShowBeats <- isolate(input$butStopShowBeats)
      showBeats <- isolate(input$butShowBeats)
	  
      if(stopShowBeats < showBeats)
      {
        invalidateLater(millis = 1500, session = getDefaultReactiveDomain())
      }
	  
      if(stopShowBeats >= showBeats)
      {
        isolate(PlotDetectionSegment(rv$ECGData, as.numeric(input$channelListShowDetection), positionBeats$counter, positionBeats$counter+4999))
        positionBeats$counter <- isolate(positionBeats$counter)-5000
        shinyjs::disable("butStopShowBeats")
        shinyjs::enable("butShowBeats")
        shinyjs::enable("butShowBeatsStepPrevious")
        shinyjs::enable("butShowBeatsStepNext")
      }
      
      positionBeats$counter <- isolate(positionBeats$counter) + 5000
	  
      if(positionBeats$counter < length(rv$ECGData$lead[[as.numeric(input$channelListShowDetection)]]$val) && stopShowBeats < showBeats)
      {
        PlotDetectionSegment(rv$ECGData, as.numeric(input$channelListShowDetection), positionBeats$counter, positionBeats$counter+4999)
      }
	  
      if(positionBeats$counter >= length(rv$ECGData$lead[[as.numeric(input$channelListShowDetection)]]$val))
      {
        isolate(PlotDetectionSegment(rv$ECGData, as.numeric(input$channelListShowDetection), positionBeats$counter-4999, positionBeats$counter))
        positionBeats$counter <- 1
      }
    })   
  })
  
  observeEvent(input$butShowBeatsSegment, {
    shinyjs::show("plotDetectionSegment")
    output$plotDetectionSegment <- renderPlot({PlotDetectionSegment(rv$ECGData, as.numeric(input$channelListShowDetectionSegment), as.numeric(input$segmentStartDetection), as.numeric(input$segmentEndDetection))})
  })
  
  observeEvent(input$butGlobal, {
    withProgress(message = 'Detecting global beats...', value = 1,
                 {  
                   rv$ECGData <- GlobalBeats(rv$ECGData)
                   showTab(inputId = "Menu", target = paste(stri_dup(intToUtf8(160), 6),"Show global detected beats"))
                   showTab(inputId = "Menu", target = "Create basal beat")
                   showTab(inputId = "Menu", target = "Delineate waves in beat")
                   showTab(inputId = "Menu", target = "Aided diagnosis")
                   shinyjs::enable("butStatistics")
                 })
    output$detectedGlobalBeats <- renderText({"Global beats successfully detected"})
    #Once global beat detection has been performed, the global detected beats message for the statistics updates its contents to show the amount of beats globally detected.
    globalDetectedBeats$beats <- paste(length(rv$ECGData$beat), " beats.")
    shinyjs::show("detectedGlobalBeats")
  })
  
  observeEvent(input$butShowGlobalBeatsStepPrevious,{
    if(positionGlobalBeats$counter < 5000)
    {
      shinyjs::disable("butShowGlobalBeatsStepPrevious")
    }
    else
    {
      positionGlobalBeats$counter <- isolate(positionGlobalBeats$counter)-5000
    }
    
  })
  
  observeEvent(input$butShowGlobalBeatsStepNext, {
    if(positionGlobalBeats$counter > length(rv$ECGData$lead[[as.numeric(input$channelListShowGlobalDetection)]]$val)-5000)
    {
      shinyjs::disable("butShowGlobalBeatsStepNext")
    }
    else
    {
      positionGlobalBeats$counter <- isolate(positionGlobalBeats$counter)+5000
    }
  })
  
  observeEvent(input$butShowGlobalBeats, {
    shinyjs::enable("butStopShowGlobalBeats")
    shinyjs::disable("butShowGlobalBeats")
    shinyjs::disable("butShowGlobalBeatsStepPrevious")
    shinyjs::disable("butShowGlobalBeatsStepNext")
    shinyjs::show("plotAllGlobalDetection")
    output$plotAllGlobalDetection <- renderPlot({
      stopShowGlobalBeats <- isolate(input$butStopShowGlobalBeats)
      showGlobalBeats <- isolate(input$butShowGlobalBeats)
	  
      if(stopShowGlobalBeats < showGlobalBeats)
      {
        invalidateLater(millis = 1500, session = getDefaultReactiveDomain())
      }
	  
      if(stopShowGlobalBeats >= showGlobalBeats)
      {
        isolate(PlotGlobalDetectionSegment(rv$ECGData, as.numeric(input$channelListShowGlobalDetection), positionGlobalBeats$counter, positionGlobalBeats$counter+4999))
        positionGlobalBeats$counter <- isolate(positionGlobalBeats$counter)-5000
        shinyjs::disable("butStopShowGlobalBeats")
        shinyjs::enable("butShowGlobalBeats")
        shinyjs::enable("butShowGlobalBeatsStepPrevious")
        shinyjs::enable("butShowGlobalBeatsStepNext")
      }
      
      positionGlobalBeats$counter <- isolate(positionGlobalBeats$counter) + 5000
      if(positionGlobalBeats$counter < length(rv$ECGData$lead[[as.numeric(input$channelListShowGlobalDetection)]]$val) && stopShowGlobalBeats < showGlobalBeats)
      {
        PlotGlobalDetectionSegment(rv$ECGData, as.numeric(input$channelListShowGlobalDetection), positionGlobalBeats$counter, positionGlobalBeats$counter+4999)
      }
      if(positionGlobalBeats$counter >= length(rv$ECGData$lead[[as.numeric(input$channelListShowGlobalDetection)]]$val))
      {
        isolate(PlotGlobalDetectionSegment(rv$ECGData, as.numeric(input$channelListShowGlobalDetection), positionGlobalBeats$counter-4999, positionGlobalBeats$counter))
        positionGlobalBeats$counter <- 1
      }
    })
  })
  
  observeEvent(input$butShowGlobalBeatsSegment, {
    shinyjs::show("plotGlobalDetectionSegment")
    output$plotGlobalDetectionSegment <- renderPlot({PlotGlobalDetectionSegment(rv$ECGData, as.numeric(input$channelListShowGlobalDetectionSegment), as.numeric(input$segmentStartGlobalDetection), as.numeric(input$segmentEndGlobalDetection))})
  })
  
  observeEvent(input$butCompareGlobalBeatsStepPrevious,{
    if(positionGlobalBeatsCompare$counter < 5000)
    {
      shinyjs::disable("butCompareGlobalBeatsStepPrevious")
    }
    else
    {
      positionGlobalBeatsCompare$counter <- isolate(positionGlobalBeatsCompare$counter)-5000
    }
    
  })
  
  observeEvent(input$butCompareGlobalBeatsStepNext, {
    if(positionGlobalBeatsCompare$counter > length(rv$ECGData$lead[[as.numeric(input$channelListCompareGlobalDetection)]]$val)-5000)
    {
      shinyjs::disable("butCompareGlobalBeatsStepNext")
    }
    else
    {
      positionGlobalBeatsCompare$counter <- isolate(positionGlobalBeatsCompare$counter)+5000
    }
  })
  
  observeEvent(input$butCompareGlobalBeats, {
    shinyjs::enable("butStopCompareGlobalBeats")
    shinyjs::disable("butCompareGlobalBeats")
    shinyjs::disable("butCompareGlobalBeatsStepPrevious")
    shinyjs::disable("butCompareGlobalBeatsStepNext")
    shinyjs::show("plotCompareAllGlobalDetection")
    output$plotCompareAllGlobalDetection <- renderPlot({
      stopCompareGlobalBeats <- isolate(input$butStopCompareGlobalBeats)
      compareGlobalBeats <- isolate(input$butCompareGlobalBeats)
	  
      if(stopCompareGlobalBeats < compareGlobalBeats)
      {
        invalidateLater(millis = 1500, session = getDefaultReactiveDomain())
      }
	  
      if(stopCompareGlobalBeats >= compareGlobalBeats)
      {
        isolate(PlotMonoMultiDetectionSegment(rv$ECGData, as.numeric(input$channelListCompareGlobalDetection), positionGlobalBeatsCompare$counter, positionGlobalBeatsCompare$counter+4999))
        positionGlobalBeatsCompare$counter <- isolate(positionGlobalBeatsCompare$counter)-5000
        shinyjs::disable("butStopCompareGlobalBeats")
        shinyjs::enable("butCompareGlobalBeats")
        shinyjs::enable("butCompareGlobalBeatsStepPrevious")
        shinyjs::enable("butCompareGlobalBeatsStepNext")
      }
      
      positionGlobalBeatsCompare$counter <- isolate(positionGlobalBeatsCompare$counter) + 5000
	  
      if(positionGlobalBeatsCompare$counter < length(rv$ECGData$lead[[as.numeric(input$channelListCompareGlobalDetection)]]$val) && stopCompareGlobalBeats < compareGlobalBeats)
      {
        PlotMonoMultiDetectionSegment(rv$ECGData, as.numeric(input$channelListCompareGlobalDetection), positionGlobalBeatsCompare$counter, positionGlobalBeatsCompare$counter+4999)
      }
	  
      if(positionGlobalBeatsCompare$counter >= length(rv$ECGData$lead[[as.numeric(input$channelListCompareGlobalDetection)]]$val))
      {
        isolate(PlotMonoMultiDetectionSegment(rv$ECGData, as.numeric(input$channelListCompareGlobalDetection), positionGlobalBeatsCompare$counter-4999, positionGlobalBeatsCompare$counter))
        positionGlobalBeatsCompare$counter <- 1
      }
    })
  })
  
  observeEvent(input$butCompareGlobalBeatsSegment, {
    shinyjs::show("plotCompareGlobalDetectionSegment")
    output$plotCompareGlobalDetectionSegment <- renderPlot({PlotMonoMultiDetectionSegment(rv$ECGData, as.numeric(input$channelListCompareGlobalDetectionSegment), as.numeric(input$segmentStartCompareGlobalDetection), as.numeric(input$segmentEndCompareGlobalDetection))})
  })
  
  observeEvent(input$butBasalBeat, {
    withProgress(message = 'Creating basal beat...', value = 1,
                 {  
                   rv$ECGData <- AdaptedBasalBeat(rv$ECGData, as.numeric(input$channelListBasalBeat), as.numeric(input$beatNo), as.numeric(input$beatsBeforeAfter))
                 })
    shinyjs::show("plotBasal")
    output$plotBasal <- renderPlot({PlotBasalBeat(rv$ECGData, as.numeric(input$channelListBasalBeat), as.numeric(input$beatNo))})
  })
  
  observeEvent(input$butDelineate, {
    withProgress(message = 'Delineating beat...', value = 1,
                 {  
                   rv$ECGData <- ECGDelineator(rv$ECGData, as.numeric(input$channelListDelineate), as.numeric(input$beatNum))
                 })
    shinyjs::show("plotDelineation")
    output$plotDelineation <- renderPlot({PlotDelineation(rv$ECGData, as.numeric(input$channelListDelineate), as.numeric(input$beatNum))})
  })
  
  
   observeEvent(input$butDiagnose, {
    withProgress(message = 'Diagnosing record...', value = 1,
                 {  
                   rv$ECGData <- DiagnoseDiseases(rv$ECGData, modelBradycardia, modelIschemia, modelMyocardialInfarction, modelTachycardia, modelVentricularHypertrophy, modelWPWSyndrome)
                 })
  
  shinyjs::show("predBradycardia")
  shinyjs::show("predIschemia")
  shinyjs::show("predMyocardialInfarction")
  shinyjs::show("predTachycardia")
  shinyjs::show("predVentricularHipertrophy")
  shinyjs::show("predWPWSyndrome")
  
  output$predBradycardia <- renderText({print(paste("Bradycardia:", format(round(rv$ECGData$diagnostics$bradycardia, digits = 2), nsmall = 2)))})
  output$predIschemia <- renderText({print(paste("Ischemia:", format(round(rv$ECGData$diagnostics$ischemia, digits = 2), nsmall = 2)))})
  output$predMyocardialInfarction <- renderText({print(paste("Myocardial infarction:", format(round(rv$ECGData$diagnostics$myocardialInfarction, digits = 2), nsmall = 2)))})
  output$predTachycardia <- renderText({print(paste("Tachycardia:", format(round(rv$ECGData$diagnostics$tachycardia, digits = 2), nsmall = 2)))})
  output$predVentricularHipertrophy <- renderText({print(paste("Ventricular hypertrophy:", format(round(rv$ECGData$diagnostics$ventricularHypertrophy, digits = 2), nsmall = 2)))})
  output$predWPWSyndrome <- renderText({print(paste("Wolff-Parkinson-White syndrome:", format(round(rv$ECGData$diagnostics$wpwSyndrome, digits = 2), nsmall = 2)))})	
    
  })
  
  observeEvent(input$switchExpertMode,{
    #If the user chooses this option, non-essential instructions will be hidden to gain more space for contents.
    if(input$switchExpertMode == TRUE)
    {
      shinyjs::hide("Inst2")
      shinyjs::hide("Inst3")
      shinyjs::hide("Inst4")
      shinyjs::hide("Inst5")
      shinyjs::hide("Inst7")
      shinyjs::hide("Inst8")
      shinyjs::hide("Inst9")
      shinyjs::hide("Inst10")
      shinyjs::hide("Inst13")
      shinyjs::hide("Inst14")
      shinyjs::hide("Inst16")
      shinyjs::hide("Inst17")
      shinyjs::hide("Inst19")
      shinyjs::hide("Inst20")
      shinyjs::hide("Inst24")
      shinyjs::hide("Inst26")
      shinyjs::hide("Inst28")
      shinyjs::hide("Inst29")
      shinyjs::hide("Inst31")
      shinyjs::hide("Inst32")
      shinyjs::hide("Inst34")
      shinyjs::hide("Inst35")
      shinyjs::hide("Inst37")
      shinyjs::hide("Inst38")
      shinyjs::hide("Inst40")
      shinyjs::hide("Inst41")
      shinyjs::hide("Inst43")
      shinyjs::hide("Inst44")
      shinyjs::hide("Inst46")
      shinyjs::hide("Inst47")
      shinyjs::hide("Inst48")
      shinyjs::hide("Inst50")
      shinyjs::hide("Inst52")
      shinyjs::hide("Inst53")
    }
    else
    {
      #If the user deactivates this option, all instructions will be shown again.
      shinyjs::show("Inst2")
      shinyjs::show("Inst3")
      shinyjs::show("Inst4")
      shinyjs::show("Inst5")
      shinyjs::show("Inst7")
      shinyjs::show("Inst8")
      shinyjs::show("Inst9")
      shinyjs::show("Inst10")
      shinyjs::show("Inst13")
      shinyjs::show("Inst14")
      shinyjs::show("Inst16")
      shinyjs::show("Inst17")
      shinyjs::show("Inst19")
      shinyjs::show("Inst20")
      shinyjs::show("Inst24")
      shinyjs::show("Inst26")
      shinyjs::show("Inst28")
      shinyjs::show("Inst29")
      shinyjs::show("Inst31")
      shinyjs::show("Inst32")
      shinyjs::show("Inst34")
      shinyjs::show("Inst35")
      shinyjs::show("Inst37")
      shinyjs::show("Inst38")
      shinyjs::show("Inst40")
      shinyjs::show("Inst41")
      shinyjs::show("Inst43")
      shinyjs::show("Inst44")
      shinyjs::show("Inst46")
      shinyjs::show("Inst47")
      shinyjs::show("Inst48")
      shinyjs::show("Inst50")
      shinyjs::show("Inst52")
      shinyjs::show("Inst53")
    }
  })
  
  observeEvent(input$butStatistics,{
    #Once they are available, statistics for the current record can be obtained and shown in a new dialog box.
    for (i in 1:rv$ECGData$nLeads)
    {
      channels <- c(channels, rv$ECGData$lead[[i]]$id)
    }
    
    idECG <- paste(rv$ECGData$id, ".", sep = "")
    channelsECG <- paste(c(channels), collapse = ", ")
    channelsECG <- paste(channelsECG, ".", sep = "")
    beatsECG <- paste(c(detectedBeatsPerChannel$beats), collapse = ", ")
    beatsECG <- paste(beatsECG, ".", sep = "")
    
    showModal(modalDialog(
      title = "Statistics for the current record",
      "Record ID:", idECG, br(), br(), "Sampling frequency: ", rv$ECGData$sFreq, " Hz.", br(), br(), "Number of samples per channel: ", rv$ECGData$nSamples, " samples.", br(), br(), "Number of channels: ", rv$ECGData$nLeads, " leads.", br(), br(), "Channels available: ", channelsECG, br(), br(), "Number of beats detected on each channel: ", beatsECG, br(), br(), "Number of global beats detected: ", globalDetectedBeats$beats)
    )
  })
  
  observeEvent(input$butMore,{
    #Extra information about ECGDT can be obtained and shown in a new dialog box.
    showModal(modalDialog(
      title = "About ECGDT",
      "ECGDT: ECG Diagnosis Tool", hr(), "v.1.1", hr(), img(src='/logo_lia_Large_316.png', width = 150, height = 100), "Copyright, LIA2 Group, 2019"))
  })
}
shinyApp(ui = ui, server = server)
