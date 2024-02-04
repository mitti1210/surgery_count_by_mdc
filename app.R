library(shiny)
library(tidyverse)
library(DT)
library(leaflet)
library(openxlsx)
library(here)


hospital_overview <- 
  read_rds(here("rds", "hospital_overview.rds")) |> 
  rename(軽度 = lon, 緯度 = lat)
prefecture_municipality <- read_rds(here("rds", "prefecture_municipality.rds"))
prefecture <- fct_inorder(prefecture_municipality$prefecture) |> unique() |> as.character()
mdcs <- list.files("rds", pattern = "MDC")
read_rds(here("rds", mdcs[1]))
mdc <- read_rds(here("rds", "mdc.rds"))




# UI定義
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(width = 4,
      selectInput("MDC", "MDC:", choices = str_c("MDC", str_pad(1:18, width = 2, pad = 0))),
      selectInput("prefecture", "都道府県", choices = c("選択してください", prefecture), selected = ""),
      uiOutput("dynamicMunicipality"),
      uiOutput("dynamicHospital"),
      uiOutput("dynamicMdc"),
      selectInput("category", "件数/日数:", choices = c("全て", "件数", "日数", "手術件数の集計（97を使用）", "手術件数の集計（97輸血以外の再掲を使用）", "病名ごとの合計"), selected = "全て", multiple = FALSE),
      selectInput("year", "年度（地図を開いて選択）:", choices = c("", rev(2012:2021)), selected = "", selectize = TRUE),
      downloadButton("download", "Download Filtered Data")
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Table", DTOutput("dataTable")),
                  tabPanel("Map", 
                           leafletOutput("map", height = "600px"))
      ),
      HTML("<b>Tableについて</b><br>
            都道府県名を選択するとデータが表示されます<br>
            個別のMDCの番号がわからない場合は右上の検索窓から病名の候補を入力してください<br>
            右上の検索窓を使ってもデータのダウンロードや地図には反映されません<br>
            データをダウンロードする時は右上の検索窓ではなく左のメニューから選択してください<br>
            年度を選択してもテーブルデータには反映しないようにしています<br>
            <br>
            <b>Mapについて</b><br>
            Map画面上で「都道府県名」「個別のMDC」「年度」を改めて選択するとデータが更新されます<br>
            表示されない場合は一旦別の選択をして戻してください<br>
            地図は件数/日数の「病名ごとの合計」を使用しています<br>
            ")
    )
  )
)

# サーバー関数定義
server <- function(input, output, session) {
  # 市区町村の動的UI
  output$dynamicMunicipality <- renderUI({
    municipality <- prefecture_municipality %>%
      filter(prefecture == input$prefecture) %>%
      pull(municipality)
    selectInput("municipality", "市区町村", choices = c("全て", municipality), multiple = TRUE, selectize = TRUE)
  })
  # 病院名の動的UI
  output$dynamicHospital <- renderUI({
    hospital_table <- hospital_overview %>%
      filter(都道府県名 == input$prefecture)
    if(any("全て" %in% input$municipality)){
      hosptal <- pull(hospital_table, "施設名_最新") |> unique()
    } else {
      hospital <- 
        filter(hospital_table, 市町村名 %in% input$municipality) |> 
        pull(施設名) |> unique()
    }
    selectInput("hospital", "病院名", choices = c("全て", hospital), multiple = TRUE, selectize = TRUE)
  })
  
  output$dynamicMdc <- renderUI({
    # ここでは例としてhospital_overviewからMDCの選択肢を生成
    # 実際には、適切なデータセットからMDCの選択肢を生成してください
    mdcChoices <- mdc |> filter(mdc == input$MDC) |> pull(MDC)
    selectInput("mdc", "個別のMDC（地図では選択が必要）", choices = c("全て", mdcChoices), selected = "全て", multiple = FALSE, selectize = TRUE)
  })
  
  
  
  
  # フィルタリングされたデータのリアクティブ式
  filteredData <- reactive({
    mdc <- mdcs[str_detect(mdcs, input$MDC)]
    
    data <- read_rds(here("rds", mdc))
    data <- data %>% 
      filter(都道府県名 == input$prefecture) |> 
      rename(軽度 = lon, 緯度 = lat)
    
    # 市区町村が選択されていない、または「全て」が選択されている場合、このフィルターをスキップ
    if (!is.null(input$municipality) && length(input$municipality) > 0 && !"全て" %in% input$municipality) {
      data <- data %>% filter(市町村名 %in% input$municipality)
    }
    
    # 施設名が選択されていない、または「全て」が選択されている場合、このフィルターをスキップ
    if (!is.null(input$hospital) && length(input$hospital) > 0 && !"全て" %in% input$hospital) {
      data <- data %>% filter(施設名 %in% input$hospital)
    }
    
    if (!is.null(input$mdc) && input$mdc != "" && input$mdc != "全て") {
      data <- data %>% filter(MDC == input$mdc)
    }
    
    
    #件数と日数を分ける。すべての場合はこのフィルターをスキップ
    if (input$category == "件数") {
      data <- data %>% filter(項目 == "件数")
    } else if (input$category == "日数") {
      data <- data %>% filter(項目 == "日数")
    } else if (input$category == "手術件数の集計（97を使用）") {
      data <- 
        data |> filter(項目 == "件数") |> 
        filter(サブ != "97輸血以外の再掲") |> 
        summarize(値 = sum(値, na.rm = TRUE), .by = c(告示番号,	市町村番号,	都道府県名,	市町村名,	施設名,	施設名_最新,	年度,	MDC,	病名,	項目,	手術, 軽度, 緯度)) |> 
        select(告示番号:手術, 値, 軽度, 緯度)
    } else if (input$category == "手術件数の集計（97輸血以外の再掲を使用）") {
      data_transfusion <- 
        data |> filter(項目 == "件数" & サブ %in% c("97", "97輸血以外の再掲")) |> 
        mutate(
          値 = if_else(サブ == "97", 値, -1*値),
          手術 = "なし") |> 
        summarize(値 = sum(値), .by = c(告示番号,	市町村番号,	都道府県名,	市町村名,	施設名,	施設名_最新,	年度,	MDC,	病名,	項目, 手術, 軽度, 緯度)) |> 
        select(告示番号:手術, 値, 軽度, 緯度)
      data <- 
        data |> filter(項目 == "件数") |> 
        filter(サブ != "97") |> 
        bind_rows(data_transfusion) |> 
        summarize(値 = sum(値, na.rm = TRUE), .by = c(告示番号,	市町村番号,	都道府県名,	市町村名,	施設名,	施設名_最新,	年度,	MDC,	病名,	項目,	手術, 軽度, 緯度)) |> 
        select(告示番号:手術, 値, 軽度, 緯度)
    } else if (input$category == "病名ごとの合計") {
      data <- 
        data |> filter(項目 == "件数") |> 
        filter(サブ != "97輸血以外の再掲") |> 
        summarize(値 = sum(値, na.rm = TRUE), .by = c(告示番号,	市町村番号,	都道府県名,	市町村名,	施設名,	施設名_最新,	年度,	MDC,	病名,	項目, 軽度, 緯度)) |> 
        select(告示番号:項目, 値, 軽度, 緯度)
    }
    
    return(data)
  })

  
  # データテーブルの表示
  output$dataTable <- renderDT({
    datatable(filteredData(), options = list(pageLength = 50))
  })
  

  
  
  mapData <- reactive({
    mdc <- mdcs[str_detect(mdcs, input$MDC)]
    
    map_data <- read_rds(here("rds", mdc))
    map_data <- map_data %>% 
      rename(軽度 = lon, 緯度 = lat) |> 
      filter(項目 == "件数") |> 
      filter(サブ != "97輸血以外の再掲") |> 
      filter(年度 == input$year) %>% 
      summarize(値 = sum(値, na.rm = TRUE), .by = c(告示番号,	市町村番号,	都道府県名,	市町村名,	施設名,	施設名_最新,	年度,	MDC,	病名,	項目, 軽度, 緯度)) |> 
      select(告示番号:項目, MDC, 値, 軽度, 緯度)
    
    # MDCによるフィルタリングを追加
    if (!is.null(input$mdc) && input$mdc != "" && input$mdc != "全て") {
      map_data <- map_data %>% filter(MDC == input$mdc)
    }
    
    # 選択された都道府県に基づいてデータをフィルタリング
    if (!is.null(input$prefecture) && input$prefecture != "") {
      mapData <- map_data %>%
        filter(都道府県名 == input$prefecture)
    } else {
      mapData <- map_data
    }
    
    # フィルタリングされたデータを返す
    return(mapData)
  })
  
  # 地図の初期表示
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%  # OpenStreetMapのタイルを追加
      setView(lng = 141.3334851	, lat = 43.0552652, zoom = 12)  # 初期ビューを設定
  })
  
  
  
  
  
  
  observe({
    
    # 選択された都道府県に基づくデータを取得
    map_data <- mapData() # mapData は、選択された都道府県に応じたデータを返すリアクティブ式
    
    # 地図を更新
    if (nrow(map_data) > 0) {
      leafletProxy("map", data = map_data) %>% 
        clearMarkers() %>%
        clearShapes() %>%
        addTiles() %>%
        setView(lng = map_data$軽度[1], lat = map_data$緯度[1], zoom = 12) %>%
        addCircles(
          lng = ~軽度, lat = ~緯度, weight = 1,
          radius = ~sqrt(値 / pi) * 100, popup = ~paste(施設名_最新, 値)
        )
    } else {
      leafletProxy("map") %>% 
        clearMarkers() %>%
        clearShapes() %>%
        addTiles() %>%
        setView(lng = 135.0, lat = 35.0, zoom = 12) # デフォルトビュー
    }
  }) # ignoreNULL = TRUE を設定すると、初期状態（NULLの場合）ではイベントがトリガーされません
  

  
  # ダウンロードハンドラー
  output$download <- downloadHandler(
    filename = function() {
      paste0(input$MDC, "_filtered.csv")
    },
    content = function(file) {
      write_excel_csv(filteredData(), file)
    }
  )
}

# アプリを起動
shinyApp(ui = ui, server = server)
