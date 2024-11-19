#install.packages(c("binman","wdman","rvest","tidyverse","RSelenium"))
#install.packages(c("caTools","seleniumPipes","openxlsx","httr","urltools"))

# 터미널 에서 크롬드라이버, 셀레늄 등 깔려있는 폴더 들어가서 아래 명령어 실행
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.141.59.jar -port 4567

library(tidyverse)
library(caTools)
library(RSelenium)
library(binman)
library(wdman)
library(rvest)
library(openxlsx)
library(httr)
library(urltools)
library(jsonlite)


# 포트를 지정합니다. 
port <- 4567L

# 리모트 드라이버를 설정합니다.
remote <- remoteDriver(remoteServerAddr = 'localhost', port = port, browserName = 'chrome')

# 리모트 웹 브라우저를 엽니다.
remote$open()


# 일반 게시판 크롤링 함수 선언 ============================================================
crawl_general <- function(URL, category, str_start, str_end, itemURL, itemURL2, css_table, css_title, css_date, css_link, pagetype, datetype1, datetype2) {
  
  remote$navigate(url = URL)
  Sys.sleep(time = 2)
  
  # 페이지 소스를 받아옵니다.
  res <- remote$getPageSource() %>% `[[`(1)
  
  # 해당 내용을 변수에 저장
  res %>% 
    read_html(encoding = 'EUC-KR') %>% 
    html_node(css = css_table) -> items
  
  # 제목 따오기
  items %>% 
    html_nodes(css = css_title) %>% 
    html_text(trim = TRUE) -> titles
  if ( datetype1 == 'a' ) { # 제목이 링크 attr에 들어있는 경우
    items %>% 
      html_nodes(css = css_title) %>% 
      html_attr(name = 'title') -> titles
  }
  if ( datetype1 == "L" ) { # 제목에 공백이 많이 들어간 것 제거(산림청)
    titles <- gsub("\t", "", titles)
    titles <- gsub("  ", "", titles)
  }
  
  # 작성일 따오기
  items %>% 
    html_nodes(css = css_date) %>% 
    html_text(trim = TRUE) -> dates
  if ( datetype1 == 'dot' | datetype2 == 'dot' ) { # 날짜형식이 2021.01.01 과 같을 경우 2021-01-01 형식으로 변환
    dates <- gsub("\\.", "-", dates)
    if ( datetype2 == 'end' ) { # 날짜형식이 2021.01.01. 과 같을 경우 맨 끝에 - 제거
      dates <- substr(dates, 1, nchar(dates)-1)
    }
  } else if ( datetype1 == 'mogef' ) { # 여가부 일부 게시판은 상단 중요게시물 2개 삭제 필요
    dates <- dates[-c(1:2)]
  } else if ( datetype1 == 'no' ) { # 간행물 같이 날짜 정보가 없는 경우 "날짜정보없음"이라고 표시
    dates <- '날짜정보없음'
  } else if ( datetype1 == 'dotspace' ) { # 날짜형식이 2021. 1.11. 과 같을 경우 (해수부)
    dates <- gsub(" ", "0", dates)
    dates <- gsub("\\.", "-", dates)
    dates <- substr(dates, 1, nchar(dates)-1)
  } else if ( datetype1 == 'slash' ) { # 날짜형식이 2021/01/11 과 같을 경우 (기상청)
    dates <- gsub("/", "-", dates)
  } else if ( datetype1 == 'bokji' ) { # '발행일 : 2021-01-25' 같이 써있어서 '발행일 :'을 지우는 경우
    dates <- gsub("[^0-9-]", "", dates)
  } else if ( datetype1 == 'epa' ) { # 환경보전협회 게시판 상단 1행 삭제 필요
    dates <- dates[-1]
  } else if ( datetype1 == 'kihasa' ) { # 보건사회연구원 보건복지포럼 예외처리
    dates <- substr(dates, 1, 9)
  } else if ( datetype1 == 'krict' ) { # 등록일2021-05-26 17:54:49 과 같은 형식일 경우 (화연)
    dates <- substr(dates, 4, 13)
  }
  if ( datetype2 == 'long' ) { # 농식품부 게시판 일부는 시간은 제거하고 날짜만
    dates <- substr(dates, 1, 10)
  } else if ( datetype2 == 'krei' ) { # 농경연 예외처리
    dates <- gsub("[^0-9-]", "", dates)
    dates <- substr(dates, 1, nchar(dates)-1)
  }
  
  # 링크 따오기(앞부분 생략된 url 추가)
  if ( pagetype == "type1") { # 앞부분을 붙이기만 하면 되는 경우
    items %>% 
      html_nodes(css = css_link) %>% 
      html_attr(name = 'href') -> links
    links <- paste(itemURL, links, sep = "")
  } else if ( pagetype == "type2" ) { # 링크를 가져올 수 없는 형식의 게시판은 게시판 자체 링크로 갈음
    links <- URL
  } else if ( pagetype == "onclick" ) { # onclick에 자바스크립트 정보가 있는 경우 유형 1
    items %>% 
      html_nodes(css = css_link) %>% 
      html_attr(name = 'onclick') -> links
    links <- gsub('[^0-9]', "", links) # 숫자 빼고 다 제거
    if ( datetype2 == 'kic' ) { # 형사정책연구원 예외처리
      links <- substr(links, 1, 5)
    }
    links <- paste(itemURL, links, sep = "")
    if ( datetype2 == 'mof' ) { # 해수부 등 예외처리
      links <- paste(links, itemURL2, sep = "")
    }
  } else if ( pagetype == "href" ) { # href에 자바스크립트 정보가 있는 경우 유형 1
    items %>% 
      html_nodes(css = css_link) %>% 
      html_attr(name = 'href') -> links
    links <- gsub('[^0-9]', "", links)
    links <- paste(itemURL, links, sep = "")
    if ( datetype1 == 'mogef' ) {
      links <- links[-c(1:2)]
    }
    if ( datetype1 == 'add' ) {
      links <- paste(links, itemURL2, sep = "")
    }
  } else if ( pagetype == "onclicks2" ) { # onclick에 자바스크립트 정보가 있는 경우
    items %>% 
      html_nodes(css = css_link) %>% 
      html_attr(name = 'onclick') -> links
    attrs <- substr(links, str_start, str_end)
    links <- paste(itemURL, attrs, itemURL2, attrs, sep = "")
  } else if ( pagetype == "href2" ) { # href에 자바스크립트 정보가 있는 경우 유형 2
    items %>% 
      html_nodes(css = css_link) %>% 
      html_attr(name = 'href') -> links
    links <- substr(links, str_start, str_end)
    links <- paste(itemURL, links, sep = "")
    if ( datetype2 == 'hira' ) { # 심평원 예외처리
      links <- paste(links, itemURL2, sep = "")
    }
  } else if ( pagetype == "onclicks") { # onclick에 자바스크립트 정보가 있는 경우 유형 2
    items %>% 
      html_nodes(css = css_link) %>% 
      html_attr(name = 'onclick') -> links
    attrs <- substr(links, str_start, str_end)
    if ( datetype2 == 'nosa' ) {
      links <- paste(itemURL, attrs, sep = "")
    } else {
      links <- paste(itemURL, attrs, itemURL2, sep = "")
    }
  } else if ( pagetype == "dot" ) { # 맨 앞 . 을 제거하고 가공해야 하는 경우
    items %>%  
      html_nodes(css = css_link) %>% 
      html_attr(name = 'href') -> links
    links <- substr(links, 2, nchar(links)) 
    links <- paste(itemURL, links, sep = "")
  } else if ( pagetype == "dotdot" ) { # 맨 앞 .. 를 제거해야 하는 경우
    items %>%  
      html_nodes(css = css_link) %>% 
      html_attr(name = 'href') -> links
    links <- substr(links, 3, nchar(links)) 
    links <- paste(itemURL, links, sep = "")
  } else if ( pagetype == "korad" ) { # 원자력환경공단 case
    items %>%  
      html_nodes(css = css_link) %>% 
      html_attr(name = 'data-keyvalue') -> links1
    items %>%  
      html_nodes(css = css_link) %>% 
      html_attr(name = 'data-keyvalue2') -> links2
    links2 <- substr(links2, 2, nchar(links2))
    links <- paste(itemURL, links2, itemURL2, links1, sep = "")
  } else if ( pagetype == 'seereal' ) { # 토지주택공사 게시판 링크는 사이트로 갈음(링크 못따옴)
    links <- itemURL
  } else if ( pagetype == 'iitp' ) { # 정보통신기획평가원 예외처리
    items %>% 
      html_nodes(css = css_link) %>% 
      html_attr(name = 'onclick') -> links
    links <- gsub('\"', "", links)
    links <- gsub('[()]', "", links)
    links <- gsub(" ", "", links)
    links <- gsub('post_to_url', "", links)
    links <- gsub(',post;', "", links)
    if ( datetype1 == "iitp" ) {
      links <- substr(links, 2, nchar(links))
    }
    links <- paste(itemURL, links, sep = "")
  } else { # 링크 가공이 필요 없는 경우
    items %>% 
      html_nodes(css = css_link) %>% 
      html_attr(name = 'href') -> links
  }
  
  # 데이터 프레임을 생성하여 리턴. 팩터 형식으로 저장되는걸 문자열로 바꿔줌
  temp <- data.frame(titles, dates, links, category, stringsAsFactors = FALSE)
  temp <- temp[!(temp$dates == ""), ] # 해수부 공지글은 날짜정보가 없으므로 삭제
  names(temp) <- c("제목", "등록일", "links", "category")
  
  if ( datetype1 == "no" ) { # 간행물의 경우 상위 2개만 남김
    temp <- temp[c(1:2), ]
  }
  if ( datetype1 == "kdca" ) { # 감염병 표본감시 주간소식지는 맨 윗행(제목행) 제거
    temp <- temp[-1, ]
  }
  
  return(temp)
  
}


# 농식품부, 중기부 보도자료 게시판용 크롤링 함수 선언 ==================================================
crawl_press <- function(category, str_start, str_end, itemURL, itemURL2, css_table, css_title, css_date, css_link, pagetype, datetype1, datetype2) {
  
  # 페이지 소스를 받아옵니다.
  res <- remote$getPageSource() %>% `[[`(1)
  Sys.sleep(time = 1)
  
  # 해당 내용을 변수에 저장
  res %>% 
    read_html(encoding = 'EUC-KR') %>% 
    html_node(css = css_table) -> items
  
  # 제목 따오기
  items %>% 
    html_nodes(css = css_title) %>% 
    html_text(trim = TRUE) -> titles
  
  # 작성일 따오기. 시간은 제거하고 날짜만
  items %>% 
    html_nodes(css = css_date) %>% 
    html_text(trim = TRUE) -> dates
  if ( datetype1 == 'dot' ) { # 날짜형식이 2021.01.01 과 같을 경우 2021-01-01 형식으로 변환
    dates <- gsub("\\.", "-", dates)
  }
  if ( datetype2 == 'long' ) { # 농식품부 게시판 일부는 시간은 제거하고 날짜만
    dates <- substr(dates, 1, 10)
  }
  
  # 링크 따오기(링크 가공)
  if ( pagetype == "type1") {
    items %>% 
      html_nodes(css = css_link) %>% 
      html_attr(name = 'href') -> links
    links <- paste(itemURL, links, sep = "")
  } else if ( pagetype == "onclicks2" ) {
    items %>% 
      html_nodes(css = css_link) %>% 
      html_attr(name = 'onclick') -> onclick
    attrs <- substr(onclick, str_start, str_end)
    links <- paste(itemURL, attrs, itemURL2, attrs, sep = "")
  }
  
  # 데이터 프레임을 생성하여 리턴. 팩터 형식으로 저장되는걸 문자열로 바꿔줌
  temp <- data.frame(titles, dates, links, category, stringsAsFactors=FALSE)
  names(temp) <- c("제목", "등록일", "links", "category")
  
  return(temp)
  
}


# 산업부 보도자료 게시판용 크롤링 함수 선언 =========================================
crawl_motie_press <- function(category) {
  
  # 페이지 소스를 받아옵니다.
  res <- remote$getPageSource() %>% `[[`(1)
  Sys.sleep(time = 1)
  
  # 해당 내용을 변수에 저장
  res %>% 
    read_html(encoding = 'EUC-KR') %>% 
    html_node(css = '#content > article > div.common_list > table') -> items
  
  # 제목 따오기. 해명/자료 표시
  items %>% 
    html_nodes(css = 'tbody > tr > td.al > div > em') %>% 
    html_text(trim = TRUE) -> header
  items %>% 
    html_nodes(css = 'tbody > tr > td.al > div > a') %>% 
    html_text(trim = TRUE) -> titles
  titles <- paste(header, "/", titles, sep = '')
  
  # 작성일 따오기
  items %>% 
    html_nodes(css = 'tbody > tr > td:nth-child(5)') %>% 
    html_text(trim = TRUE) -> dates
  
  # 링크 따오기(http://www.motie.go.kr/motie/ne/presse/press2/bbs/ 이 생략되어 있으므로 추가)
  items %>% 
    html_nodes(css = 'tbody > tr > td.al > div > a') %>% 
    html_attr(name = 'href') -> links
  links <- paste('http://www.motie.go.kr/motie/ne/presse/press2/bbs/', links, sep = "")
  
  # 데이터 프레임을 생성하여 리턴. 팩터 형식으로 저장되는걸 문자열로 바꿔줌
  temp <- data.frame(titles, dates, links, category, stringsAsFactors=FALSE)
  names(temp) <- c("제목", "등록일", "links", "category")
  
  return(temp)
  
}


# json 형태(수도권매립지관리공사) 게시판 크롤링 함수 선언 ===================================================
crawl_json <- function (category, ajax, query1, query2, query3, URL) {
  # 'XHR' 탭에 있는 파일의 내용을 이용하여 HTTP 요청을 실행합니다. 
  res <- GET(url = ajax, 
             query = list(caNo = query1,
                          bcId = query2,
                          page = query3))
  Sys.sleep(time = 1)
  
  # 콘텐트 형태가 'JSON'이므로 응답 메시지 바디를 출력하여 확인합니다. 
  json <- res %>% content(as = 'text') %>% fromJSON()
  
  # json 객체의 구조를 파악합니다. 
  titles <- json$list$bdTitle
  dates <- json$list$bdRegdate
  link <- URL
  
  # 데이터 프레임을 생성하여 리턴. 팩터 형식으로 저장되는걸 문자열로 바꿔줌
  temp <- data.frame(titles, dates, link, category, stringsAsFactors=FALSE)
  names(temp) <- c("제목", "등록일", "links", "category")
  
  return(temp)
}



# 진짜 실행 ======================================================================================

# 최종 출력을 위한 데이터프레임 초기화
items <- data.frame()

# URL 리스트가 들어있는 파일 열기
# URL_list 엑셀파일이 있는 폴더를 working directory 로 설정하고 이하 코드 실행
list <- read.xlsx("URL_list.xlsx")

# 부처, 기관별 크롤링 함수 실행
for (i in 1:nrow(list)) {
  
  if (list$name[i] == '농식품부 (1) 보도자료') {
    # 게시판 url로 이동하고 1페이지 긁어옴
    remote$navigate(url = list$link_board[i])
    Sys.sleep(time = 1)
    items <- bind_rows(items, crawl_press(list$name[i], list$substr_start[i], list$substr_end[i], list$link_item1[i], list$link_item2[i], list$css_table[i], list$css_title[i], list$css_date[i], list$css_link[i], list$pagetype[i], list$datetype1[i], list$datetype2[i]))
    # 2페이지로 이동하는 버튼 클릭하고 2페이지 크롤링
    button <- remote$findElement(using = 'xpath', '//*[@id="menu293_obj2012"]/div[2]/form[3]/div/div/ul/li[2]/a')
    button$clickElement()
    Sys.sleep(time = 1)
    items <- bind_rows(items, crawl_press(list$name[i], list$substr_start[i], list$substr_end[i], list$link_item1[i], list$link_item2[i], list$css_table[i], list$css_title[i], list$css_date[i], list$css_link[i], list$pagetype[i], list$datetype1[i], list$datetype2[i]))
  } 
  
  else if (list$name[i] == '산업부 (1) 보도/해명') {
    # 게시판 url로 이동하고 1페이지 긁어옴
    remote$navigate(url = list$link_board[i])
    Sys.sleep(time = 1)
    items <- bind_rows(items, crawl_motie_press(list$name[i]))
    # 2페이지로 이동하는 버튼 클릭하고 크롤링
    button <- remote$findElement(using = 'xpath', '//*[@id="content"]/article/div[2]/div[4]/span[2]/a')
    button$clickElement()
    Sys.sleep(time = 1)
    items <- bind_rows(items, crawl_motie_press(list$name[i]))
    # 3페이지
    button <- remote$findElement(using = 'xpath', '//*[@id="content"]/article/div[2]/div[4]/span[3]/a')
    button$clickElement()
    Sys.sleep(time = 1)
    items <- bind_rows(items, crawl_motie_press(list$name[i]))
  } 
  
  else if (list$name[i] == '중소벤처기업부 (1) 보도자료') {
    # 게시판 url로 이동하고 1페이지 긁어옴
    remote$navigate(url = list$link_board[i])
    Sys.sleep(time = 1)
    items <- bind_rows(items, crawl_press(list$name[i], list$substr_start[i], list$substr_end[i], list$link_item1[i], list$link_item2[i], list$css_table[i], list$css_title[i], list$css_date[i], list$css_link[i], list$pagetype[i], list$datetype1[i], list$datetype2[i]))
    # 2페이지로 이동하는 버튼 클릭하고 크롤링
    button <- remote$findElement(using = 'xpath', '//*[@id="contents_inner"]/div/div[4]/ul/li[2]/a')
    button$clickElement()
    Sys.sleep(time = 1)
    items <- bind_rows(items, crawl_press(list$name[i], list$substr_start[i], list$substr_end[i], list$link_item1[i], list$link_item2[i], list$css_table[i], list$css_title[i], list$css_date[i], list$css_link[i], list$pagetype[i], list$datetype1[i], list$datetype2[i]))
    # 3페이지
    button <- remote$findElement(using = 'xpath', '//*[@id="contents_inner"]/div/div[4]/ul/li[4]/a')
    button$clickElement()
    Sys.sleep(time = 1)
    items <- bind_rows(items, crawl_press(list$name[i], list$substr_start[i], list$substr_end[i], list$link_item1[i], list$link_item2[i], list$css_table[i], list$css_title[i], list$css_date[i], list$css_link[i], list$pagetype[i], list$datetype1[i], list$datetype2[i]))
  }
  
  else if (list$institution[i] == '수도권매립지관리공사') {
    items <- bind_rows(items, crawl_json(list$name[i], list$link_item1[i], list$css_table[i], list$css_title[i], list$css_date[i], list$link_board[i]))
  }
  
  else { 
    items <- bind_rows(items, crawl_general(list$link_board[i], list$name[i], list$substr_start[i], list$substr_end[i], list$link_item1[i], list$link_item2[i], list$css_table[i], list$css_title[i], list$css_date[i], list$css_link[i], list$pagetype[i], list$datetype1[i], list$datetype2[i]))
  }
  
  cat(list$name[i], '게시판 수집 완료\n')
  
}

# 일주일 넘게 지난 항목 제거(선택사항)
for (i in 1:nrow(items)) { # 일주일 넘게 지났으면 '0' 표시
  if ( items$등록일[i] != "날짜정보없음" & items$category[i] != '한국보건사회연구원 보건복지포럼' ) {
    if ( as.Date(items$등록일[i]) - Sys.Date() <= -7 ) {
      items$links[i] <- '0'
    }
  }
}
items <- items[!(items$links == '0'), ]


# 결과를 엑셀파일로 출력

wb <- createWorkbook() # 객체 선언
addWorksheet(wb, "Sheet1")

x <- items$links # 하이퍼링크 삽입을 위한 작업
class(x) <- "hyperlink"

# 데이터프레임을 기록하고 하이퍼링크를 위에 덮어씀
writeDataTable(wb, sheet = 1, x = items, startCol = 1, startRow = 1, tableStyle = "TableStyleLight11")
writeData(wb, sheet = 1, x = x, startCol = 3, startRow = 2)

filename <- paste("output_", Sys.Date(), ".xlsx", sep = "") # 파일이름 생성

saveWorkbook(wb, filename, overwrite = TRUE) # 저장


# ======================= 끝 =====================================