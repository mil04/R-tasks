### Przetwarzanie Danych Ustrukturyzowanych 2023L
### Praca domowa nr. 3
###
### UWAGA:
### nazwy funkcji oraz ich parametrow powinny pozostac niezmienione.
###  
### Wskazane fragmenty kodu przed wyslaniem rozwiazania powinny zostac 
### zakomentowane
###

# -----------------------------------------------------------------------------#
# Wczytanie danych oraz pakietow.
# !!! Przed wyslaniem zakomentuj ten fragment
# -----------------------------------------------------------------------------#
#Posts <- read.csv("D:/kodes/PDUsem2/PracD1/Posts.csv.gz")
#Users <- read.csv("D:/kodes/PDUsem2/PracD1/Users.csv.gz")
#Comments <- read.csv("D:/kodes/PDUsem2/PracD1/Comments.csv.gz")
#install.packages("sqldf")
#library("sqldf")
#install.packages("dplyr")
#library('dplyr')
#install.packages("data.table") 
#library("data.table") 



# -----------------------------------------------------------------------------#
# Zadanie 1
# -----------------------------------------------------------------------------#


sql_1 <- function(Users){
df_sql <-sqldf::sqldf("
SELECT Location, SUM(UpVotes) as TotalUpVotes
FROM Users
WHERE Location != ''
GROUP BY Location
ORDER BY TotalUpVotes DESC
LIMIT 10
") 
}

base_1 <- function(Users){
  #wybieramy z Users tulko gdzie Location nie jest puste
  base <- subset(Users, Location != '')
  #grupujemy po Location i sumujemy UpVotes
  base <- aggregate(base$UpVotes, by=base['Location'], sum)
  #zamieniamy nazwy kolumn, bo po aggregate nie sa ustawione
  colnames(base)<-c("Location","TotalUpVotes")
  #ustawiamy wierszy tak, zeby TotalUpVotes byli ustawione malejaco
  base <- base[order(base$TotalUpVotes, decreasing = TRUE),]
  #wybieramy pierwsze 10 wierszy
  base <- head(base, n = 10)
}


dplyr_1 <- function(Users){
  dplyr<- Users %>%
    #wybieramy z Users tulko gdzie Location nie jest puste
    filter(Location != '') %>% 
    #grupujemy po Location
    group_by(Location) %>%
    #sumujemy UpVotes i ustawiamy nazwe kolumny
    summarise(TotalUpVotes=sum(UpVotes)) %>% 
    #ustawiamy wierszy tak, zeby TotalUpVotes byli ustawione malejaco
    arrange(desc(TotalUpVotes)) %>% 
    #wybieramy pierwsze 10 wierszy
    slice_head(n=10)
}

table_1 <- function(Users){
  #tworzymy obiekt data.table zeby moc z nim pracowac
  data<-data.table::data.table(Users)
  df_table<-data[,.(TotalUpVotes = sum(UpVotes)), by = Location
  #pogrupowalismy po Location, podsumowalismy UpVotes i zapisalismy w ToyalUpVotes
    ][Location != ''
   #wybralismy tulko werszy gdzie Location nie jest puste
    ][order(-TotalUpVotes)
   #ustawilismy TotalUpVotes malejaco
    ][1:10
   #wybralismy pierwsze 10 wierszy
    ]
}

# Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem
#dplyr::all_equal(sql_1(Users), base_1(Users))
#dplyr::all_equal(sql_1(Users), dplyr_1(Users))
#dplyr::all_equal(sql_1(Users), table_1(Users))

# Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem
#microbenchmark::microbenchmark(
#  sqldf = sql_1(Users),
#  base = base_1(Users),
#  dplyr = dplyr_1(Users),
#  data.table = table_1(Users),
#  times=25L
#  )

#Unit: milliseconds
#expr      min       lq      mean   median       uq      max neval
#sqldf 286.6489 288.4185 306.38679 290.1795 310.2419 398.5415    25
#base 182.3610 186.6319 205.95998 188.5834 240.4276 262.4469    25
#dplyr  44.5283  45.2807  50.46583  47.0955  48.0379 122.7197    25
#data.table  20.1205  23.7335  27.48638  24.7527  26.1656  81.5643    25


# -----------------------------------------------------------------------------#
# Zadanie 2
# -----------------------------------------------------------------------------#

sql_2 <- function(Posts){
  df_sql2 <-sqldf::sqldf("
  SELECT STRFTIME('%Y', CreationDate) AS Year, STRFTIME('%m', CreationDate) AS Month,
  COUNT(*) AS PostsNumber, MAX(Score) AS MaxScore
  FROM Posts
  WHERE PostTypeId IN (1, 2)
  GROUP BY Year, Month
  HAVING PostsNumber > 1000
  ")
}

base_2 <- function(Posts){
  #wybieramy z Posts tylko dane z PostTypeId 1 albo 2
  base2 <- Posts[Posts$PostTypeId %in% c(1, 2), ]
  #tworzymy kolumny Year i Month biarac dane z CreationDate
  base2$Year <- format(as.Date(base2$CreationDate), '%Y')
  base2$Month <- format(as.Date(base2$CreationDate), '%m')
  #tworzymy ramke danych  gdzie PostNumer - PostTypeId grupowane po Year i Month razem
  #a potem zliczne liczbe wierzy
  grouped <- aggregate(cbind(PostsNumber = base2$PostTypeId) ~ Year + Month,
                       data = base2, FUN = function(x) {length(x)})
  #tworzymy MaxScore zliczajac maximum Score grupujac po Year i Month 
  grouped$MaxScore <- aggregate(base2$Score, by = list(base2$Year, base2$Month)
                                , FUN = max)$x
  #wybieramy tylko dane z PostsNumber>1000
  base2 <- grouped[grouped$PostsNumber > 1000, ]
  
}

dplyr_2 <- function(Posts){
  df_dplyr2 <- Posts %>%
  #wybieramy z Posts tylko dane z PostTypeId 1 albo 2
  filter(PostTypeId %in% c(1, 2)) %>%
  #grupujemy po Year i Month (dane z CreationDate)
  group_by(Year = format(as.Date(CreationDate), '%Y'), Month = format(as.Date(CreationDate), '%m')) %>%
  #tworzymy MaxScore zliczajac maximum Score i zliczamy liczbe wierzy
  summarise(PostsNumber = n(), MaxScore = max(Score)) %>%
  #wybieramy tylko dane z PostsNumber>1000
  filter(PostsNumber > 1000)

}


table_2 <- function(Posts){
  #tworzymy obiekt data.table zeby moc z nim pracowac
  table2 <- as.data.table(Posts)
  #tworzymy kolumny Year i Month biarac dane z CreationDate
  table2[, Year := format(as.Date(CreationDate), '%Y')]
  table2[, Month := format(as.Date(CreationDate), '%m')]
  #wybieramy tylko dane z PostTypeId 1 albo 2
  table2 <- table2[PostTypeId %in% c(1, 2)]
  #tworzymy MaxScore zliczajac maximum Score, zliczamy liczbe wierzy i grupujemy po Year i Month
  table2 <- table2[, .(PostsNumber = .N, MaxScore = max(Score)), by = .(Year, Month)]
  #wybieramy tylko dane z PostsNumber>1000
  table2 <- table2[PostsNumber > 1000]
}

# Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem
#dplyr::all_equal(sql_2(Posts), base_2(Posts))
#dplyr::all_equal(sql_2(Posts), dplyr_2(Posts))
#dplyr::all_equal(sql_2(Posts), table_2(Posts))

# Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem
#microbenchmark::microbenchmark(
#  sqldf = sql_2(Posts),
#  base = base_2(Posts),
#  dplyr = dplyr_2(Posts),
#  data.table = table_2(Posts),
#  times=25L
#)

#Unit: seconds
#expr      min       lq     mean   median       uq      max neval
#sqldf 1.827982 1.874929 1.946037 1.935497 1.988727 2.223967    25
#base 2.532376 2.560443 2.681832 2.675555 2.740997 3.001954    25
#dplyr 2.303494 2.400722 2.473926 2.450857 2.550746 2.718800    25
#data.table 2.388952 2.459465 2.558322 2.490192 2.604620 2.999091    25

# -----------------------------------------------------------------------------#
# Zadanie 3
# -----------------------------------------------------------------------------#

sql_3 <- function(Posts, Users){
  
  Questions <- sqldf(
    'SELECT OwnerUserId, SUM(ViewCount) as TotalViews
     FROM Posts 
     WHERE PostTypeId = 1
     GROUP BY OwnerUserId')
  
  sqldf("
          SELECT Id, DisplayName, TotalViews
          FROM Questions
          JOIN Users
          ON Users.Id = Questions.OwnerUserId
          ORDER BY TotalViews DESC
          LIMIT 10")
}

base_3 <- function(Posts, Users){
  #zliczamy sume ViewCount dla kazdego unikalnego OwnerUserId
  base3 <- aggregate(ViewCount ~ OwnerUserId, data = Posts, FUN = sum, na.rm = TRUE)
  #zmieniamy nazwy kolumn
  colnames(base3)<-c("OwnerUserId","TotalViews")
  #laczymy Users i base3 po Id i OwnerUserId 
  base3_m <- merge(Users, base3, by.x = "Id", by.y = "OwnerUserId", all.x = TRUE)
  #wybieramy kolumny "Id", "DisplayName", "TotalViews"
  base3_m <- base3_m[c("Id", "DisplayName", "TotalViews")]
  #ustawiamy porzadek tak zeby ToyalViews sie zmniejszalo 
  base3_m <- base3_m[order(base3_m$TotalViews, decreasing = TRUE), ]
  #wybieramy pierwsze 10 wierszy
  base3_m <- base3_m[1:10, ]
}


dplyr_3 <- function(Posts, Users){
  dflyr3_q <- Posts %>%
  #grupujemy po OwnerUserId z Posts
    group_by(OwnerUserId) %>%
  #tworzymy TotalViews - suma ViewCount
    summarize(TotalViews = sum(ViewCount, na.rm = TRUE))
  #laczymy Users i dflyr3_q po Id i OwnerUserId 
  dplyr3 <- inner_join( Users,dflyr3_q, by = c( "Id"="OwnerUserId" )) %>%
  #wybieramy kolumny "Id", "DisplayName", "TotalViews"
    select(Id, DisplayName, TotalViews) %>%
  #ustawiamy porzadek tak zeby ToyalViews sie zmniejszalo i wybieramy pierwsze 10 wierszy
    arrange(desc(TotalViews)) %>% head(10)
}


table_3 <- function(Posts, Users){
  #tworzymy obiekt data.table zeby moc z nim pracowac
  table3_p <- as.data.table(Posts)
  table3_u <- as.data.table(Users)
  #zliczamy sume ViewCount dla kazdego unikalnego OwnerUserId
  table3 <- table3_p[, .(TotalViews = sum(ViewCount, na.rm = TRUE)), by = OwnerUserId]
  #OwnerUserId zamieniamy na Id
  setnames(table3, "OwnerUserId", "Id")
  #laczymy table3_u, table3 po Id
  table3_m <- merge(table3_u, table3, by = "Id", all.x = TRUE)
  #wybieramy kolumny "Id", "DisplayName", "TotalViews"
  table3_m <- table3_m[, .(Id, DisplayName, TotalViews)]
  #ustawiamy porzadek tak zeby ToyalViews sie zmniejszalo i wybieramy pierwsze 10 wierszy
  table3_m <- table3_m[order(-TotalViews)]
  table3_m <- table3_m[1:10]
}

# Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem
#dplyr::all_equal(sql_3(Posts, Users), base_3(Posts,Users))
#dplyr::all_equal(sql_3(Posts,Users), dplyr_3(Posts,Users))
#dplyr::all_equal(sql_3(Posts, Users), table_3(Posts,Users))

# Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem
#microbenchmark::microbenchmark(
#  sqldf = sql_3(Posts,Users),
#  base = base_3(Posts,Users),
#  dplyr = dplyr_3(Posts,Users),
#  data.table = table_3(Posts,Users),
#  times = 25L
#)
#Unit: milliseconds
#expr       min        lq       mean    median        uq       max neval
#sqldf 2004.7378 2027.9621 2096.88206 2098.7786 2123.9436 2389.5464    25
#base  582.0407  592.2260  637.73935  603.2811  658.8630  958.5126    25
#dplyr  115.5837  125.7180  162.59172  182.4688  191.2452  234.2661    25
#data.table   46.6235   48.0181   61.10676   50.4777   54.9637  145.9396    25
# -----------------------------------------------------------------------------#
# Zadanie  4
# -----------------------------------------------------------------------------#

sql_4 <- function(Posts, Users){
  df_sql2 <-sqldf::sqldf("
  SELECT DisplayName, QuestionsNumber, AnswersNumber, Location, Reputation, UpVotes, DownVotes
  FROM (
    SELECT *
      FROM (
        SELECT COUNT(*) as AnswersNumber, OwnerUserId
        FROM Posts
        WHERE PostTypeId = 2
        GROUP BY OwnerUserId
      ) AS Answers
    JOIN
    (
      SELECT COUNT(*) as QuestionsNumber, OwnerUserId
      FROM Posts
      WHERE PostTypeId = 1
      GROUP BY OwnerUserId
    ) AS Questions
    ON Answers.OwnerUserId = Questions.OwnerUserId
    WHERE AnswersNumber > QuestionsNumber
    ORDER BY AnswersNumber DESC
    LIMIT 5
  ) AS PostsCounts
  JOIN Users
  ON PostsCounts.OwnerUserId = Users.Id
  ")
}

base_4 <- function(Posts, Users){
  #zliczamy liczbe PostTypeId=2 dla kazdego unikalnego OwnweUserId
  base4_a <- aggregate(PostTypeId ~ OwnerUserId, data = Posts, FUN = function(x) sum(x == 2))
  #zmieniamy nazwy kolumn
  colnames(base4_a) <- c("OwnerUserId", "AnswersNumber")
  #zliczamy liczbe PostTypeId=1 dla kazdego unikalnego OwnweUserId
  base4_q <- aggregate(PostTypeId ~ OwnerUserId, data = Posts, FUN = function(x) sum(x == 1))
  #zmieniamy nazwy kolumn
  colnames(base4_q) <- c("OwnerUserId", "QuestionsNumber")
  
  #laczymy base4_a i base4_q po OwnerUserId
  base4_poscoun <- merge(base4_a, base4_q, by = "OwnerUserId", all.x = TRUE)
  #wybieramy tylko dane gdzie AnswersNumber > QuestionsNumber i OwnerUserId jest zdefiniowane
  base4_poscoun <- subset(base4_poscoun, AnswersNumber > QuestionsNumber & !is.na(OwnerUserId))
  # ustawiamy porzadek tak zeby AnsersNumber sie zmniejszalo
  base4_poscoun <- base4_poscoun[order(base4_poscoun$AnswersNumber, decreasing = TRUE), ]
  #wybieramy pierwsze 5 wierszy
  base4_poscoun <- head(base4_poscoun, 5)
  
  #laczymy base4_poscoun z Users po OwnerUserId i Id
  base4 <- merge(base4_poscoun, Users, by.x = "OwnerUserId", by.y = "Id")
  #wybieramy tylko kolumny "DisplayName", "QuestionsNumber", "AnswersNumber",
  #"Location", "Reputation", "UpVotes", "DownVotes"
  base4 <- base4[c("DisplayName", "QuestionsNumber", "AnswersNumber",
                         "Location", "Reputation", "UpVotes", "DownVotes")]

}

dplyr_4 <- function(Posts, Users){
  dplyr_a <- Posts %>%
    #wybieramy z Posts tylko te wierszy gdzie PostTypeId = 2
    filter(PostTypeId == 2) %>%
    #grupujemy po OwnerUserId
    group_by(OwnerUserId) %>%
    #zliczamy liczbe wierszy 
    summarize(AnswersNumber = n())
  dplyr_q <- Posts %>%
    #wybieramy z Posts tylko te wierszy gdzie PostTypeId = 1
    filter(PostTypeId == 1) %>% 
    #grupujemy po OwnerUserId
    group_by(OwnerUserId) %>%
    #zliczamy liczbe wierszy 
    summarize(QuestionsNumber = n())
  #laczymy dplyr_a i dplyr_q po OwnerUserId
  dplyr_poscoun <- inner_join(dplyr_a, dplyr_q, by = "OwnerUserId")%>%
    #wybieramy tylko dane gdzie AnswersNumber > QuestionsNumber i OwnerUserId jest zdefiniowane
    filter(AnswersNumber > QuestionsNumber & !is.na(OwnerUserId))%>%
    # ustawiamy porzadek tak zeby AnsersNumber sie zmniejszalo i wybieramy pierwsze 5 wierszy
    arrange(desc(AnswersNumber)) %>% head(5)
  #laczymy dplyr_poscoun z Users po OwnerUserId i Id
  dplyr4 <- inner_join(dplyr_poscoun, Users, by = c("OwnerUserId" = "Id"))%>%
    #wybieramy tylko kolumny "DisplayName", "QuestionsNumber", "AnswersNumber",
    #"Location", "Reputation", "UpVotes", "DownVotes"
       select(DisplayName, QuestionsNumber, AnswersNumber, Location, Reputation, UpVotes, DownVotes)
}

table_4 <- function(Posts, Users){
  #tworzymy obiekt data.table zeby moc z nim pracowac
  table4_p <- data.table(Posts)
  table4_u <- data.table(Users)
  
  #wybieramy tylko te wierszy gdzie PostTypeId = 2(=1), grupujemy po OwnerUserId i zliczamy
  table4_a <- table4_p[PostTypeId == 2, .(AnswersNumber = sum(.N)), by = OwnerUserId]
  table4_q <- table4_p[PostTypeId == 1, .(QuestionsNumber = sum(.N)), by = OwnerUserId]
  
  #laczymy table4_a, table4_q po OwnerUserId
  table4_poscoun <- merge(table4_a, table4_q, by = "OwnerUserId", all.x = TRUE)
  #wybieramy tylko dane gdzie AnswersNumber > QuestionsNumber i OwnerUserId jest zdefiniowane
  table4_poscoun <- table4_poscoun[AnswersNumber > QuestionsNumber & !is.na(OwnerUserId)]
  # ustawiamy porzadek tak zeby AnsersNumber sie zmniejszalo i wybieramy pierwsze 5 wierszy
  table4_poscoun <- table4_poscoun[order(AnswersNumber, decreasing = TRUE)]
  table4_poscoun <- head(table4_poscoun, 5)
  
  #laczymy table4_poscoun, table4_u po OwnerUserId i Id
  table4 <- merge(table4_poscoun, table4_u, by.x = "OwnerUserId", by.y = "Id")
  #wybieramy tylko kolumny "DisplayName", "QuestionsNumber", "AnswersNumber",
  #"Location", "Reputation", "UpVotes", "DownVotes"
  table4 <- table4[, .(DisplayName, QuestionsNumber, AnswersNumber, Location, Reputation, UpVotes, DownVotes)]
}

# Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem
#dplyr::all_equal(sql_4(Posts, Users), base_4(Posts,Users))
#dplyr::all_equal(sql_4(Posts,Users), dplyr_4(Posts,Users))
#dplyr::all_equal(sql_4(Posts, Users), table_4(Posts,Users))

# Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem
#microbenchmark::microbenchmark(
#  sqldf = sql_4(Posts,Users),
#  base = base_4(Posts,Users),
#  dplyr = dplyr_4(Posts,Users),
#  data.table = table_4(Posts,Users),
#  times=25L
#)
#Unit: milliseconds
#expr       min        lq      mean    median        uq       max neval
#sqldf 3223.1675 3251.2173 3318.1881 3281.1515 3352.2992 3573.1051    25
#base 2077.4356 2146.7552 2210.5223 2168.4235 2223.9373 2521.8893    25
#dplyr  600.4472  681.0496  713.6756  695.1278  726.8772  958.3054    25
#data.table  115.7919  125.2961  177.4549  146.5695  228.6314  277.6674    25

# -----------------------------------------------------------------------------#
# Zadanie 5
# -----------------------------------------------------------------------------#

sql_5 <- function(Posts, Comments, Users){
  con <- sqldf()
  CmtTotScr <- sqldf('SELECT PostId, SUM(Score) AS CommentsTotalScore
                        FROM Comments
                        GROUP BY PostId')
  PostsBestComments <- sqldf('
        SELECT Posts.OwnerUserId, Posts.Title, Posts.CommentCount, Posts.ViewCount, 
               CmtTotScr.CommentsTotalScore
        FROM CmtTotScr
        JOIN Posts ON Posts.Id = CmtTotScr.PostId
        WHERE Posts.PostTypeId=1')
  
  sqldf('SELECT Title, CommentCount, ViewCount, CommentsTotalScore, DisplayName, Reputation, Location
            FROM PostsBestComments
            JOIN Users ON PostsBestComments.OwnerUserId = Users.Id
            ORDER BY CommentsTotalScore DESC
            LIMIT 10')
}

base_5 <- function(Posts, Comments, Users){
  #liczymy sume Score dla kazdego PostId
  base5_comm <- aggregate(Score ~ PostId, data = Comments, FUN = sum)
  #zamieniamy nazwy kolumn
  colnames(base5_comm) <- c("PostId","CommentsTotalScore")
  
  #laczymy Posts i base5_comm po "Id" i "PostId"
  base5_m <- merge(Posts, base5_comm , by.x = "Id", by.y = "PostId", all.x = TRUE)
  #wybieramy tylko dane gdzie PostTypeId = 1
  base5_m <- base5_m[base5_m$PostTypeId == 1, ]
  #bierzemy kolumny "Title", "CommentCount", "ViewCount", "CommentsTotalScore", "OwnerUserId"
  base5_m <- base5_m[c("Title", "CommentCount", "ViewCount", "CommentsTotalScore", "OwnerUserId")]

  #laczymy base5_m i Users po "OwnerUserId" i "Id"
  base5_m2 <- merge(base5_m, Users, by.x = "OwnerUserId", by.y = "Id", all.x = TRUE)
  #wybieramy kolumny "Title", "CommentCount", "ViewCount", "CommentsTotalScore", "DisplayName", "Reputation", "Location"
  base5_m2 <- base5_m2[c("Title", "CommentCount", "ViewCount", "CommentsTotalScore", "DisplayName", "Reputation", "Location")]

  #ustawiamy porzadek tak zeby ommentsTotalScore sie zmniejszalo
  base5_m2 <- base5_m2[order(base5_m2$CommentsTotalScore, decreasing = TRUE), ]
  #wybieramy tylko te dane gdzie DisplayName jest zdefiniowane
  base5_m2 <- base5_m2[!is.na(base5_m2$DisplayName),]
  #wybieramy pierwsze 10 wierszy
  base5_m2 <- head(base5_m2, 10)

}

dplyr_5 <- function(Posts, Comments, Users){
  dplyr5 <- Posts %>% 
    #Posts laczymy z Comments(gdzie wierszy sa grupowane po PostId i policzona jest suma Score) po Id i PostId
    inner_join(Comments %>% group_by(PostId) %>% summarise(CommentsTotalScore = sum(Score)),
      by = c("Id" = "PostId")) %>%
    #wybieramy dane gdzie PostTypeId=1
    filter(PostTypeId == 1) %>% 
    #wybieramy kolumny Title, CommentCount, ViewCount, CommentsTotalScore, OwnerUserId
    select(Title, CommentCount, ViewCount, CommentsTotalScore, OwnerUserId) %>%
    #laczymy resultat z Users po OwnerUserId i Id
    inner_join(Users, by = c("OwnerUserId" = "Id")) %>%
    #wybieramy kolumny Title, CommentCount, ViewCount, CommentsTotalScore,DisplayName, Reputation, Location
    select(Title, CommentCount, ViewCount, CommentsTotalScore,
      DisplayName, Reputation, Location) %>% arrange(desc(CommentsTotalScore)) %>%
    #bierzemy pierwsze 10 wierszy
    head(10)
}

table_5 <- function(Posts, Comments, Users){
  #tworzymy obiekt data.table zeby moc z nim pracowac
  table5_p <- as.data.table(Posts)
  table5_c <- as.data.table(Comments)
  table5_u <- as.data.table(Users)
  
  ##liczymy sume Score dla kazdego PostId
  table5_comm <- table5_c[, .(CommentsTotalScore = sum(Score)), by = "PostId"]
  #laczymy table5_p, base5_comm po  "Id", "PostId"
  table5_m <- merge(table5_p, table5_comm, by.x = "Id", by.y = "PostId", all.x = TRUE)
  #wybieramy tylko dane gdzie PostTypeId = 1
  table5_m <- table5_m[PostTypeId == 1]
  #bierzemy tylko kolumny Title, CommentCount, ViewCount, CommentsTotalScore, OwnerUserId
  table5_m <- table5_m[, .(Title, CommentCount, ViewCount, CommentsTotalScore, OwnerUserId)]
 
  #laczymy table5_m, table5_u po "OwnerUserId","Id"
  table5_m2 <- merge(table5_m, table5_u, by.x = "OwnerUserId", by.y = "Id", all.x = TRUE)
  #bierzemy kolumny Title, CommentCount, ViewCount, CommentsTotalScore, DisplayName, Reputation, Location
  table5_m2<- table5_m2[, .(Title, CommentCount, ViewCount, CommentsTotalScore, DisplayName, Reputation, Location)]

  #ustawiamy wierszy tak zeby CommentsTotalScore sie zmniejszalo
  table5_m2 <- table5_m2[order(-CommentsTotalScore)]
  #wybieramy tylko te dane gdzie DisplayName jest zdefiniowane
  table5_m2 <- table5_m2[!is.na(DisplayName)]
  #bierzemy pierwsze 10 wierszy
  table5_m2 <- head(table5_m2, 10)
}

# Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem
#dplyr::all_equal(dplyr_5(Posts,Comments,Users), base_5(Posts,Comments,Users))
#dplyr::all_equal(sql_5(Posts,Comments,Users), dplyr_5(Posts,Comments,Users))
#dplyr::all_equal(dplyr_5(Posts,Comments,Users), table_5(Posts,Comments,Users))

# Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem
#microbenchmark::microbenchmark(
#  sqldf = sql_5(Posts,Comments,Users),
#  base = base_5(Posts,Comments,Users),
#  dplyr = dplyr_5(Posts,Comments,Users),
#  data.table = table_5(Posts,Comments,Users),
#  times=25L
#)
#Unit: milliseconds
#expr       min        lq      mean    median        uq       max neval
#sqldf 4044.4923 4090.9631 4169.7217 4154.8416 4213.9125 4358.8062    25
#base 2799.4898 2948.7709 3130.4414 3118.8301 3287.6998 3571.4523    25
#dplyr  403.1319  500.9039  604.3634  553.6939  663.0972 1011.1302    25
#data.table  159.8423  182.5106  290.0432  286.7726  334.2832  550.3907    25


