library(shiny)
library(rtweet)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
requireNamespace('httr', quietly = TRUE)
requireNamespace('shinythemes', quietly = TRUE)
requireNamespace('DT', quietly = TRUE)
library(glue)
library(xts)
library(dygraphs)
library(lubridate)

filterRule <- "norncinclt|defendcharlotte|cltcc|rnc"

clt_tweets <- readRDS("clt_tweets.rds") %>%
  filter(str_detect(str_to_lower(text),filterRule)) %>%
  mutate(hashtags = map(hashtags, tolower))

top_10_hashtags <- clt_tweets %>% 
  filter(!is.na(hashtags)) %>% 
  pull(hashtags) %>% 
  unlist %>% 
  data_frame(`Top 10 Hashtags` = .) %>% 
  group_by(`Top 10 Hashtags`) %>% 
  tally(sort = TRUE) %>% 
  top_n(10, n) %>% select(-n)

related_hashtags <-   clt_tweets %>% 
  filter(map_lgl(hashtags, function(hl) length(intersect(hl, top_10_hashtags$`Top 10 Hashtags`)) > 0)) %>% 
  pull(hashtags) %>% 
  map_dfr(function(hs) {
    x <- map(seq_along(hs), function(i) c(paste(hs[i], hs[i:length(hs)]), paste(hs[i:length(hs)], hs[i])))
    data_frame(tags = unlist(x))
  }) %>% 
  arrange(tags) %>% 
  filter(!duplicated(tags)) %>% 
  mutate(
    tags = str_split(tags, ' '),
    tag = map_chr(tags, ~ .[1]),
    related = map_chr(tags, ~ .[2])
  ) %>% 
  select(-tags) %>% 
  filter(tag != related, 
         tag %in% top_10_hashtags$`Top 10 Hashtags`,
         related %in% top_10_hashtags$`Top 10 Hashtags`)

#source("init.R")

get_tweet_blockquote <- function(screen_name, status_id) {
  bq <- httr::GET(glue("https://publish.twitter.com/oembed?url=https://twitter.com/{screen_name}/status/{status_id}?omit_script=true"))
  if (bq$status_code >= 400)
    '<blockquote style="font-size: 90%">Sorry, unable to get tweet ¯\\_(ツ)_/¯</blockquote>'
  else {
    httr::parsed_content(bq)$html
  }
}

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$script(src="twitter.js")),
  titlePanel("Charlotte-RNC 2020 Tweets"),
  theme = shinythemes::shinytheme('yeti'),

  sidebarLayout(
    sidebarPanel(
    width = 4,
    wellPanel(
      selectInput('view', 'Tweet Group', c('Popular', "Pictures", "All")),
      uiOutput('help_text'),
      uiOutput('filters')
    ),
    wellPanel(class = 'tweetbox', htmlOutput('tweet')),
    tags$div(class = 'colophon',
             tagList(
               tags$p(
                 "Made with", HTML("&#x2764;&#xFE0F;"), "+",  HTML("\u2615\uFE0F"), "by",
                 tags$a(href = 'https://twitter.com/ryanwesslen/', '@ryanwesslen'),
                 ' (original dashboard by ',
                 tags$a(href = 'https://twitter.com/grrrck/', '@grrrck'),
                 ') ',
                 'with',  HTML("&#x1F4AA;"), 'from',
                 HTML(paste(
                   tags$a(href = 'http://rtweet.info/', 'rtweet'),
                   tags$a(href = 'https://www.rstudio.com/', 'RStudio'),
                   tags$a(href = 'https://shiny.rstudio.com/', 'Shiny'),
                   tags$a(href = 'https://www.tidyverse.org/', 'tidyverse'),
                   sep = ', '
                 ))),
               tags$p(
                 HTML("&#x1F4BE;"), tags$a(href = 'https://github.com/ryanwesslen/rnc-charlotte', 'View source on GitHub')
                 , "or", downloadLink('download_tweets', "Download Tweets")
               ),
               tags$p("Tweets used keyword search (case insensitive): 'norncinclt or defendcharlotte or cltcc or rnc'"),
               tags$p(
                 paste0("Last update: ",Sys.time())
               )
             )
      )

    ),

  mainPanel(
    dygraphs::dygraphOutput('graph', height = "300px"),
    tags$hr(),
    DT::dataTableOutput('table')
  ),
  
  position = "left"
  
)
  

)

server <- function(input, output) {
  output$help_text <- renderUI({
    req(input$view)
    switch(
      input$view,
      'Popular' = helpText(HTML("&#x1F4AF;"),  "Most popular (retweets + favs) first"),
      'Pictures' = helpText(HTML("&#x1F4F8;"),  "Tweets that come with a picture"),
      'All' = helpText(HTML("&#x1F917;"), "All the tweets"),
      NULL
    )
  })
  tweets <- reactive({

    x <- switch(
      input$view,
      'All' = clt_tweets %>%
        arrange(desc(retweet_count + favorite_count),
                -map_int(mentions_screen_name, length)),
      'Pictures' = clt_tweets %>% filter(!is_retweet, !is.na(media_url)),
      clt_tweets
    )
    
    if (input$view %in% c('All', 'Popular','Pictures')) {
      if (length(input$filter_binary)) {
        for (filter_binary in input$filter_binary) {
          x <- switch(
            filter_binary,
            #'Not Retweet' = filter(x, !is_retweet),
            'Not Quote' = filter(x, !is_quote),
            'Has Media' = filter(x, !is.na(media_url)),
            'Has Link' = filter(x, !is.na(urls_url)),
            "Retweeted" = filter(x, retweet_count > 0),
            "Favorited" = filter(x, favorite_count > 0)
          )
        }
      }
      if (length(input$filter_hashtag)) {
        x <- filter(x, !is.null(hashtags))
        for (filter_hashtag in input$filter_hashtag) {
          x <- filter(x, map_lgl(hashtags, function(h) filter_hashtag %in% h))
        }
      }
    }
    x
  })
  
  tweetCount <- reactive({

    counts <- tweets() %>%
      mutate(created_at = round_time(created_at, "hours"), Type = ifelse(is_retweet,"Retweet","Post")) %>%
      select(time = created_at, Type) %>% # keep only columns
      count(time, Type) %>% # count by terms
      tidyr::spread(key = Type, value = n, fill = 0) # pivot to xts format
    
     xts(
      x = counts[,-1],
      order.by = counts$time,
      tz = "EDT"
    )
    
  })
  
  output$graph <- renderDygraph({

    dygraph(tweetCount(),  main = "Hourly Tweet Count", group = "combine") %>%
      dyLegend(show = "onmouseover") %>%
      dyOptions(colors = RColorBrewer::brewer.pal(8, "Dark2"), includeZero = TRUE) %>%
      dyAxis("x", drawGrid = FALSE) %>%
      dyOptions(useDataTimezone = TRUE) %>%
      dyRoller(rollPeriod = 1)
  })
  
  hashtags_related <- reactive({
    req(input$view %in% c('All', 'Pictures','Popular'))
    if (is.null(input$filter_hashtag) || input$filter_hashtag == '') return(top_10_hashtags)
    limit_to_tags <- related_hashtags %>% 
      filter(tag %in% input$filter_hashtag) %>% 
      pull(related) %>% 
      unique()
    top_10_hashtags %>% 
      filter(`Top 10 Hashtags` %in% c(limit_to_tags, input$filter_hashtag)) %>% 
      pull(`Top 10 Hashtags`)
  })
  
  output$filters <- renderUI({
    selected_hashtags <- isolate(input$filter_hashtag)
    selected_binary <- isolate(input$filter_binary)
    if (input$view %in% c('All','Pictures','Popular')) {
      tagList(
        checkboxGroupInput('filter_binary', 'Tweet Filters', 
                           choices = c(#"Not Retweet", 
                                       "Not Quote", "Has Media", "Has Link", "Retweeted", "Favorited"), 
                           selected = selected_binary,
                           inline = TRUE),
        selectizeInput('filter_hashtag', 'Hashtags', choices = c("", hashtags_related()), selected = selected_hashtags, 
                       multiple = TRUE, options = list(plugins = list('remove_button')), width = "100%")
      )
    }
  })
  
  tableTweets <- reactive({
    if (!is.null(input$graph_date_window)) {
      minTime <- input$graph_date_window[[1]]
      maxTime <- input$graph_date_window[[2]]
    } else {
      minTime <- min(clt_tweets$created_at)
      maxTime <- max(clt_tweets$created_at)
    }
    
    filter(tweets(), created_at > minTime & created_at < maxTime & is_retweet == FALSE)
  })
  
  output$table <- DT::renderDataTable({
    tableTweets() %>%
      select(created_at, screen_name, text, retweet_count, favorite_count, mentions_screen_name) %>% 
      mutate(created_at = strftime(created_at, '%F %T', tz = 'US/Eastern'),
             mentions_screen_name = map_chr(mentions_screen_name, paste, collapse = ', '),
             mentions_screen_name = ifelse(mentions_screen_name == 'NA', '', mentions_screen_name))
  },
  selection = 'single', 
  rownames = FALSE, 
  colnames = c("Timestamp", "User", "Tweet", "RT", "Likes", "Mentioned"), 
  filter = 'top',
  order = list(list(4, 'desc'), list(5, 'desc')),
  options = list(lengthMenu = c(5, 10, 25, 50, 100), pageLength = 5)
  )
  
  output$tweet <- renderText({
    if (!is.null(input$table_rows_selected)) {

      tableTweets() %>% 
        slice(input$table_rows_selected) %>% 
        mutate(
          html = suppressWarnings(get_tweet_blockquote(screen_name, status_id))
        ) %>% 
        pull(html)
    } else {
      HTML('<blockquote style="font-size: 90%">Choose a tweet from the table...</blockquote>')
    }
  })
  
  output$download_tweets <-  downloadHandler(
    filename = function() {
      paste("clt_tweets", Sys.Date(), ".RDS", sep="")
    },
    content = function(file) {
      saveRDS(clt_tweets, file)
    }
  )
}

shinyApp(ui = ui, server = server)