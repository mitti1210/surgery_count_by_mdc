library(shiny)
library(tidyverse)
library(DT)
library(leaflet)
library(openxlsx)
library(here)
library(DT)

hospital_overview <- read_rds(here("rds", "hospital_overview.rds")) |> rename(経度 = lon, 緯度 = lat)
prefecture_municipality <- read_rds(here("rds", "prefecture_municipality.rds"))
prefecture <- fct_inorder(prefecture_municipality$prefecture) |> unique() |> as.character()
mdcs <- list.files("rds", pattern = "MDC")
read_rds(here("rds", mdcs[1]))
mdc <- read_rds(here("rds", "mdc.rds"))

selectMDC <- str_c(
  MDC = str_c("MDC", str_pad(c(1:18), width = 2, pad = 0)),
  title = c(
    "神経系疾患",
    "眼科系疾患",
    "耳鼻咽喉科系疾患",
    "呼吸器系疾患",
    "循環器系疾患",
    "消化器系疾患、肝臓・胆道・膵臓疾患",
    "筋骨格系疾患",
    "皮膚・皮下組織の疾患",
    "乳房の疾患",
    "内分泌・栄養・代謝に関する疾患",
    "腎・尿路系疾患及び男性生殖器系疾患",
    "女性生殖器系疾患及び産褥期疾患・異常妊娠分娩",
    "血液・造血器・免疫臓器の疾患",
    "新生児疾患、先天性奇形",
    "小児疾患",
    "外傷・熱傷・中毒",
    "精神疾患",
    "その他の疾患"
  ),
  sep = " "
)


# UI定義
ui <- fluidPage(
  titlePanel("MDC別 疾患別手術別集計 Download Helper"),
  sidebarLayout(
    sidebarPanel(width = 4,
      selectInput("prefecture", "都道府県*1", choices = c("選択してください", prefecture), selected = ""),
      selectInput("MDC", "MDC", choices = selectMDC),
      uiOutput("dynamicMdc"),
      selectInput("year", "年度*2*3:", choices = c("全て", rev(2012:2022)), selected = "", selectize = TRUE),
      uiOutput("dynamicMunicipality"),
      uiOutput("dynamicHospital"),
      selectInput("category", "項目*5*7", choices = c("件数", "手術件数(輸血を含む)", "手術件数(輸血以外)", "病名ごとの合計", "MDC合計", "在院日数", "全て"), selected = "件数", multiple = FALSE),
      #sliderInput("year", "年度*5*6", min = 2012, max = 2022, value = 2021, step = 1),
      downloadButton("download", "Download Table Data"),
      HTML(
        "<br>
        *1:最初に都道府県名を入力<br>
        *2:選択しなければすべての年度が表示される<br>
        *3:地図を使う場合は年度を選択する<br>
        *4:複数選択可（Back space, Delで削除）<br>
        *5:地図には反映させていない<br>
        *6:市区町村を選択すると病院名が選択できる<br>
        *7:<br>
        件数:元データから集計を加えていない値<br>
        手術件数(輸血を含む):輸血を手術ありとして集計<br>
        手術件数(輸血以外):輸血を手術なしとして集計<br>
        病名ごとの合計:個別のMDC毎の手術あり・なしを合計した件数<br>
        MDC合計:選んだMDC01-18の合計件数<br>
        在院日数:元データから集計を加えていない在院日数<br>
        全て：件数も在院日数も入っている元データと同じ値<br>
        "
      )
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Table", DTOutput("dataTable")),
                  tabPanel("Map", 
                           leafletOutput("map", height = "600px"))
      ),
      HTML(
        "
        [公益社団法人 日本医業経営コンサルタント協会　令和５年度　情報活用コンペティション]の応募作品になります。<br>
        コンペティションの規則に従い、現在は特定のオープンソースライセンスをプロジェクトに適用していません。<br>
        コンペティション期間中は、作品のライセンスに関しては特に明言しておらず、すべての権利は作者に帰属します。<br>
        コンペティション終了後、改めて公開の有無と作品のライセンスについて再評価し、公開の際は適切なライセンスを選定する予定です。<br>
        その際は、<a href='https://github.com/mitti1210/surgery_count_by_mdc/blob/main/'>github</a>の`LICENSE.md`ファイルおよびこの`README.md`を更新します。<br>
        予告なく公開を終了することがあります。
        "
        )
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
    selectInput("municipality", "市区町村*4*5", choices = c("全て", municipality), multiple = TRUE, selectize = TRUE)
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
    selectInput("hospital", "病院名*4*5*6", choices = c("全て", hospital), multiple = TRUE, selectize = TRUE)
  })
  
  output$dynamicMdc <- renderUI({
    # ここでは例としてhospital_overviewからMDCの選択肢を生成
    # 実際には、適切なデータセットからMDCの選択肢を生成してください
    mdcChoices <- mdc |> filter(mdc == str_extract(input$MDC, "MDC[01][0-9]")) |> pull(MDC_choice)
    selectInput("mdc", "個別のMDC", choices = c("全て", mdcChoices), selected = "全て", multiple = FALSE, selectize = TRUE)
  })
  
  
  
  
  # フィルタリングされたデータのリアクティブ式
  filteredData <- reactive({
    mdc <- mdcs[str_detect(mdcs, str_extract(input$MDC, "MDC[01][0-9]"))]
    
    data <- read_rds(here("rds", mdc))
    data <- data %>% 
      filter(都道府県名 == input$prefecture)
    
    # 年度でフィルタリング
    if (!is.null(input$year) && input$year != "全て") {
      data <- data %>% filter(年度 == input$year)
    }
    
    # 市区町村が選択されていない、または「全て」が選択されている場合、このフィルターをスキップ
    if (!is.null(input$municipality) && length(input$municipality) > 0 && !"全て" %in% input$municipality) {
      data <- data %>% filter(市町村名 %in% input$municipality)
    }
    
    # 施設名が選択されていない、または「全て」が選択されている場合、このフィルターをスキップ
    if (!is.null(input$hospital) && length(input$hospital) > 0 && !"全て" %in% input$hospital) {
      data <- data %>% filter(施設名 %in% input$hospital)
    }
    
    if (!is.null(input$mdc) && input$mdc != "" && input$mdc != "全て") {
      index <- str_extract(input$mdc, "[0-9a-zA-Z]*")
      data <- data %>% filter(MDC == index)
    }
    
    
    #件数と在院日数を分ける。すべての場合はこのフィルターをスキップ
    if (input$category == "件数") {
      data <- data %>% filter(項目 == "件数")
    } else if (input$category == "在院日数") {
      data <- data %>% filter(項目 == "在院日数") |> mutate(値 = round(値, 1))
    } else if (input$category == "手術件数(輸血を含む)") {
      data <- 
        data |> filter(項目 == "件数") |> 
        filter(サブ != "97（輸血以外の再掲）") |> 
        summarize(値 = sum(値, na.rm = TRUE), .by = c(告示番号,	市町村番号,	都道府県名,	市町村名,	施設名,	施設名_最新,	年度,	MDC,	病名,	項目,	手術, 経度, 緯度)) |> 
        select(告示番号:手術, 値, 経度, 緯度)
    } else if (input$category == "手術件数(輸血以外)") {
      data_transfusion <- 
        data |> filter(項目 == "件数" & サブ %in% c("97", "97（輸血以外の再掲）")) |> 
        mutate(
          値 = if_else(サブ == "97", 値, -1*値),
          手術 = "なし") |> 
        summarize(値 = sum(値), .by = c(告示番号,	市町村番号,	都道府県名,	市町村名,	施設名,	施設名_最新,	年度,	MDC,	病名,	項目, 手術, 経度, 緯度)) |> 
        select(告示番号:手術, 値, 経度, 緯度)
      data <- 
        data |> filter(項目 == "件数") |> 
        filter(サブ != "97") |> 
        bind_rows(data_transfusion) |> 
        summarize(値 = sum(値, na.rm = TRUE), .by = c(告示番号,	市町村番号,	都道府県名,	市町村名,	施設名,	施設名_最新,	年度,	MDC,	病名,	項目,	手術, 経度, 緯度)) |> 
        select(告示番号:手術, 値, 経度, 緯度)
    } else if (input$category == "病名ごとの合計") {
      data <- 
        data |> filter(項目 == "件数") |> 
        filter(サブ != "97（輸血以外の再掲）") |> 
        summarize(値 = sum(値, na.rm = TRUE), .by = c(告示番号,	市町村番号,	都道府県名,	市町村名,	施設名,	施設名_最新,	年度,	MDC,	病名,	項目, 経度, 緯度)) |> 
        select(告示番号:項目, 値, 経度, 緯度)
    } else if (input$category == "MDC合計") {
      data <- 
        data |> filter(項目 == "件数") |> 
        filter(サブ != "97（輸血以外の再掲）") |> 
        summarize(値 = sum(値, na.rm = TRUE), .by = c(告示番号,	市町村番号,	都道府県名,	市町村名,	施設名,	施設名_最新,	年度,	経度, 緯度)) |> 
        select(告示番号:年度, 値, 経度, 緯度)
    }
    
    return(data)
  })

  
  # データテーブルの表示
  output$dataTable <- renderDT({
    datatable(filteredData(), options = list(pageLength = 10))
  })
  

  mapData <- reactive({
    mdc <- mdcs[str_detect(mdcs, str_extract(input$MDC, "MDC[01][0-9]"))]
    
    map_data <- read_rds(here("rds", mdc))
    
    # 選択された都道府県でフィルタリング
    if (!is.null(input$prefecture) && input$prefecture != "") {
      map_data <- map_data %>% 
        filter(都道府県名 == input$prefecture)
    }
    
    # 項目が"件数"、サブが"97（輸血以外の再掲）"ではないものをフィルタリング
    map_data <- map_data %>%
      filter(項目 == "件数", サブ != "97（輸血以外の再掲）")
    
    # 年度でフィルタリング
    if (!is.null(input$year) && input$year != "全て") {
      map_data <- map_data %>% filter(年度 == input$year)
    }
    
    # 「個別のMDC」の選択に応じて条件分岐
    if (is.null(input$mdc) || input$mdc == "" || input$mdc == "全て") {
      # 「個別のMDC」が全てもしくは何も選択されていない場合
      map_data <- map_data %>%
        summarize(値 = sum(値, na.rm = TRUE), .by = c(告示番号, 市町村番号, 都道府県名, 市町村名, 施設名, 施設名_最新, 年度, 経度, 緯度)) %>%
        select(告示番号:年度, 値, 経度, 緯度)
    } else {
      # 「個別のMDC」が特定の値で選択されている場合
      map_data <- map_data %>%
        filter(MDC == str_extract(input$mdc, "[0-9a-zA-Z]*")) %>%
        summarize(値 = sum(値, na.rm = TRUE), .by = c(告示番号, 市町村番号, 都道府県名, 市町村名, 施設名, 施設名_最新, 年度, MDC, 病名, 項目, 経度, 緯度)) %>%
        select(告示番号:項目, MDC, 値, 経度, 緯度)
    }
    
    return(map_data)
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
        setView(lng = map_data$経度[1], lat = map_data$緯度[1], zoom = 12) %>%
        addCircles(
          lng = ~経度, lat = ~緯度, weight = 1,
          radius = ~sqrt(値 / pi) * 50, popup = ~paste(施設名_最新, 値)
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
      paste0(str_extract(input$MDC, "MDC[01][0-9]"), "_filtered.csv")
    },
    content = function(file) {
      write_excel_csv(filteredData(), file)
    }
  )
}

# アプリを起動
shinyApp(ui = ui, server = server)


