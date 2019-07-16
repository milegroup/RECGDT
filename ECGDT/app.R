library(shiny)
ui <- fluidPage(
  theme = "/home/vmvisunha/Escritorio/ECGTesis/ECGDT/www/united.css",
  titlePanel(h1("ECG Diagnostic Tool")),
  #img(src='/home/vmvisunha/Escritorio/ECGTesis/ECGDT/www/ECG2.jpg', align = "right", height = 250, width = 250),
  navlistPanel(
    tabPanel(
      "Load ECG",
      tabsetPanel(
        tabPanel(
          "ASCII",
          fileInput("ECGRecordFile", "Select an ECG file"),
          hr(),
          textInput("calibrationFactor", "Specify a calibration factor", 1),
          actionButton("butLoadECGASCII","Load ECG file")
        ),
        tabPanel(
          "WFDB",
          fileInput("ECGRecordFile", "Select an ECG file"),
          hr(),
          selectInput("typeWFDBList", "Select a WFDB file type", c("16", "212")),
          actionButton("butLoadECGWFDB", "Load ECG file")
        )
      )
    ),
    
    tabPanel(
      "Show signal",
      tabsetPanel(
        tabPanel(
          "Show all the signal",
          selectInput("channelListShow", "Select a channel from the ECG", c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")),
          actionButton("butShowECG","Show")
        ),
        tabPanel(
          "Show signal segment",
          selectInput("channelListShowSegment", "Select a channel from the ECG", c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")),
          h5("You must specify values of start and end points that differ more than 10 samples and less than 10.000 samples."),
          textInput("segmentStart", "From sample:", 1),
          textInput("segmentEnd", "To sample:", 10000),
          actionButton("butShowECGSegment","Show")
        )
      )
    ),
    tabPanel(
      "Beat detection",
      tabsetPanel(
        tabPanel(
          "On all channels",
          actionButton("butDetectECG","Detect Beats")
        ),
        tabPanel(
          "On specific channel",
          selectInput("channelListDetect", "Select a channel from the ECG", c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")),
          actionButton("butDetectECGChannel","Detect Beats")
        )
      )
    ),
    tabPanel(
      "Show detected beats",
      tabsetPanel(
        tabPanel(
          "Show beats detected on all the signal",
          selectInput("channelListShowDetection", "Select a channel from the ECG", c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")),
          actionButton("butShowBeats","Show detected beats")
        ),
        tabPanel(
          "Show beats detected on a segment of the signal",
          selectInput("channelListShowDetectionSegment", "Select a channel from the ECG", c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")),
          h5("You must specify values of start and end points that differ more than 10 samples and less than 10.000 samples."),
          textInput("segmentStartDetection", "From sample:", 1),
          textInput("segmentEndDetection", "To sample:", 10000),
          actionButton("butShowBeatsSegment","Show detected beats")
        )
      )
    ),
    tabPanel(
      "Global detection",
      actionButton("butGlobal","Obtain global Beat positions")
    ),
    tabPanel(
      "Show global detected beats",
      tabsetPanel(
        tabPanel(
          "Show global beats detected on all the signal",
          selectInput("channelListShowGlobalDetection", "Select a channel from the ECG", c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")),
          actionButton("butShowGlobalBeats","Show global detected beats")
        ),
        tabPanel(
          "Show global beats detected on a segment of the signal",
          selectInput("channelListShowGlobalDetectionSegment", "Select a channel from the ECG", c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")),
          h5("You must specify values of start and end points that differ more than 10 samples and less than 10.000 samples."),
          textInput("segmentStartGlobalDetection", "From sample:", 1),
          textInput("segmentEndGlobalDetection", "To sample:", 10000),
          actionButton("butShowGlobalBeatsSegment","Show global detected beats")
        )
      )
    ),
    tabPanel(
      "Create Basal beat",
      selectInput("channelListBasalBeat", "Select a channel from the ECG", c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")),
      textInput("beatNo", "Base beat:", 6),
      textInput("msBefore", "ms to use Before the QRS location:", 200),
      textInput("msAfter", "ms to use After the QRS location::", 600),
      textInput("beatsBeforeAfter", "Beats used Before and After the selected beat:", 5),
      actionButton("butBasalBeat","Compute")
    ),
    tabPanel(
      "Delineate waves in beat",
      selectInput("channelListDelineate", "Select a channel from the ECG", c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")),
      textInput("beatNum", "Base beat:", 6),
      actionButton("butDelineate","Delineate Beat")
    ),
    tabPanel(
      "Export ECG",
      textInput("fileNameECG", "Specify a file name", ""),
      textInput("filePathECG", "Specify a file path", ""),
      selectInput("channelListExportECG", "Select a channel from the ECG", c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")),
      actionButton("butExportECG","Export ECG")
    ),
    tabPanel(
      "Export detected beats",
      textInput("fileNameBeats", "Specify a file name", ""),
      textInput("filePathBeats", "Specify a file path", ""),
      selectInput("channelListExportBeats", "Select a channel from the ECG", c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")),
      actionButton("butExportBeats","Export detected beats")
    ),
    tabPanel(
      "Export detected beats on all channels",
      textInput("fileNameBeatsAllch", "Specify a file name", ""),
      textInput("filePathBeatsAllch", "Specify a file path", ""),
      actionButton("butExportBeatsAllCh","Export detected beats")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$butLoadECGASCII, {
    ECGBase <- CreateECGData(Verbose = TRUE)
    ECGBase <- LoadECGAscii(ECGBase, ECGRecordFile, "/home/vmvisunha/ECG_DBs/", as.numeric(input$calibrationFactor))
  })
  
  observeEvent(input$butLoadECGWFDB, {
    ECGBase <- CreateECGData(Verbose = TRUE)
    ECGBase <- LoadECGWFDB(ECGBase, "100.hea", input$typeWFDBList, "/home/vmvisunha/ECG_DBs/")
  })
  
  observeEvent(input$butShowECG, {
    PlotECG(ECGBase, as.numeric(input$channelListShow))
  })
  
  observeEvent(input$butShowECGSegment, {
    PlotECGSegment(ECGBase, as.numeric(input$channelListShowSegment), as.numeric(input$segmentStart), as.numeric(input$segmentEnd))
  })
  
  observeEvent(input$butDetectECG, {
    ECGBase <- QRSDetectionAllCh(ECGBase)
  })
  
  observeEvent(input$butDetectECGChannel, {
    ECGBase <- QRSDetection(ECGBase, as.numeric(input$channelListDetect))
  })
  
  observeEvent(input$butShowBeats, {
    PlotDetection(ECGBase, as.numeric(input$channelListShowDetection))
  })
  
  observeEvent(input$butShowBeatsSegment, {
    PlotDetectionSegment(ECGBase, as.numeric(input$channelListShowDetectionSegment), as.numeric(input$segmentStartDetection), as.numeric(input$segmentEndDetection))
  })
  
  observeEvent(input$butGlobal, {
    ECGBase <- GlobalBeats(ECGBase)
  })
  
  observeEvent(input$butShowGlobalBeats, {
    PlotGlobalDetection(ECGBase, as.numeric(input$channelListShowGlobalDetection))
  })
  
  observeEvent(input$butShowGlobalBeatsSegment, {
    PlotGlobalDetectionSegment(ECGBase, as.numeric(input$channelListShowGlobalDetectionSegment), as.numeric(input$segmentStartGlobalDetection), as.numeric(input$segmentEndGlobalDetection))
  })
  
  observeEvent(input$butBasalBeat, {
    ECGBase <- CreateBasalBeat(ECGBase, as.numeric(input$channelListBasalBeat), as.numeric(input$beatNo), as.numeric(input$msBefore), as.numeric(input$msAfter), as.numeric(input$beatsBeforeAfter))
  })
  
  observeEvent(input$butDelineate, {
    ECGBase <- ECGDelineator(ECGBase, as.numeric(input$beatNum))
    PlotDelineation(ECGBase, as.numeric(input$beatNum))
  })
  
  observeEvent(input$butExportECG, {
    ExportECG(ECGBase, "8", "/home/vmvisunha/ECG_DBs/")
  })
  
  observeEvent(input$butExportBeats, {
    ExportBeats(ECGBase, "beats_I01", as.numeric(input$channelListExportBeats), "/home/vmvisunha/ValidationFiles/")
  })
  
  observeEvent(input$butExportBeatsAllCh, {
    ExportBeatsAllCh(ECGBase, "beats_I01", "/home/vmvisunha/ValidationFiles/")
  })
}
shinyApp(ui = ui, server = server)