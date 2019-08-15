library("dplyr")
library("readr")
library("stringr")
library("tidyr")
library("ggplot2")
library("fuzzyjoin")
library("purrr")
library('readxl')
library('magrittr')
# library('httr') 
# library('jsonlite')


setwd("/home/olena/R/workshop/")

# 1. Визначаємо університети з найбільшою кількістю ризикованих закупівель --------

# Ми будемо працювати з базою закладів вищої освіти, що публікується ЄДЕБО у форматі відкритих даних. 

# я попередньо завантажила ці дані

zvo_data <- read.csv("data/zvo_data.csv", stringsAsFactors=FALSE) 

 # так само ми могли отримати ці дані за допомогою API #
# request <- GET(url = 'https://registry.edbo.gov.ua/api/universities/?ut=high&lc=80&exp=json')
# zvo_data <- content(request, as = "text", encoding = "UTF-8") %>%
#   fromJSON(flatten = TRUE) %>%
#   data.frame()

# поглянемо на дані

zvo_data %>% head() %>% View()

glimpse(zvo_data)

# датасет містить багато неважливої для нас інформації, від якої варто позбутись, використовуючи select()
# нас цікавлять лише державні ЗВО, тому ми позбудемось від приватних ЗВО; так само відкинемо коледжі;
zvo_data <- zvo_data %>% 
  filter(university_financing_type_name != 'Приватна') %>%
  filter(education_type_name != 'Коледж') %>% 
  select(c(university_name, university_short_name, university_id, university_edrpou, university_governance_type_name, 
           region_name, university_address)) 

str(zvo_data)

# колеги зі студентської антикорупційної ініціативи Studwatch вже знайшли інформацію про закупівлі цих ЗВО
# уявімо, що нам потрібно приєднати ці дані до бази ЄДЕБО

tenders_info <- read.csv("data/tenders_info.csv", stringsAsFactors=FALSE) 

tenders_info %>% head() %>% View()

glimpse(tenders_info)
summary(tenders_info)

# перевіримо чи немає дублів у колонках з назвами ЗВО

length(unique(zvo_data$university_name)) 
length(unique(tenders_info$university_name)) 

# let's join

zvo_data %>% 
  left_join(tenders_info, by = 'university_name') %>% 
  select(university_name, university_short_name, doporog_money_perc, all_money) %>%
  View()

# і одразу перевіримо скільки не поєдналось

zvo_data %>% 
  left_join(tenders_info, by = 'university_name') %>% 
  select(university_name, doporog_money_perc, all_money) %>%
  filter(is.na(doporog_money_perc)) %>% 
  count() 
  
# погляньмо які саме значення не поєднались

setdiff(tenders_info$university_name, zvo_data$university_name) %>% View()

# уніфікація назв ЗВО за допомогою stringr

# перевіримо чи зустрічаються в назвах ЗВО латинські літери замість кириличних
unique(na.omit(unlist(strsplit(unlist(tenders_info$university_name), "[^a-zA-Z]+"))))

# виправляємо помилки, коли замість кириличних літер використовуються латинські

tenders_info <- tenders_info %>%
  mutate(university_name = university_name %>%
          str_to_lower() %>% 
          str_replace_all('a', 'а') %>%
          str_replace_all('c', 'с') %>%
          str_replace_all('x', 'х') %>%
          str_replace_all('i', 'і') %>%
          str_replace_all('l', 'і') %>%
          str_replace_all('o', 'о') %>%
          str_replace_all('O', 'о') %>%
          str_replace_all('0', 'о') %>%
          str_replace_all('h', 'н') %>% 
          str_replace_all('m', 'м') %>% 
          str_replace_all('1', 'і')
         )

# те ж саме у вигляді функції застосуємо до бази ЄДЕБО

replace_letters <- function(str) {
  res <- str %>% 
    str_to_lower() %>% 
    str_replace_all('a', 'а') %>%
    str_replace_all('c', 'с') %>%
    str_replace_all('x', 'х') %>%
    str_replace_all('i', 'і') %>%
    str_replace_all('l', 'і') %>%
    str_replace_all('o', 'о') %>%
    str_replace_all('O', 'о') %>%
    str_replace_all('0', 'о') %>%
    str_replace_all('h', 'н') %>% 
    str_replace_all('m', 'м') %>% 
    str_replace_all('1', 'і')
  
  return(res)
} 

zvo_data <- zvo_data %>% 
  mutate(university_name = replace_letters(university_name))

# ще одна спроба

zvo_data %>% 
  left_join(tenders_info, by = 'university_name') %>% 
  select(university_name, doporog_money_perc, all_money) %>%
  View()

# на скільки більше університетів мають відповідники?

zvo_data %>% 
  left_join(tenders_info, by = 'university_name') %>% 
  select(university_name, doporog_money_perc, all_money) %>%
  filter(is.na(doporog_money_perc)) %>% 
  count()

# непогано, але недостатньо
# погляньмо які значення не поєднались і цього разу

setdiff(tenders_info$university_name, zvo_data$university_name) %>% View()

# позбуваємося пробілів на початку та кінці значення, а також знаків пунктуації

tenders_info <- tenders_info %>%
  mutate(university_name = university_name %>%
  str_replace_all('[[:punct:]=+|$^]', ' ') %>% 
  str_trim())

# те ж саме у вигляді функції

replace_punct <- function(str) {
  res <- str %>% 
    str_replace_all('[[:punct:]=+|$^]', ' ') %>% 
    str_trim()
    return(res)
} 

zvo_data <- zvo_data %>% 
  mutate(university_name = replace_punct(university_name))

# 3й раунд поєднання (одразу порахуємо скільки не поєдналось)

zvo_data %>% 
  left_join(tenders_info, by = 'university_name') %>% 
  select(university_name, doporog_money_perc, all_money) %>%
  filter(is.na(doporog_money_perc)) %>% 
  count()

# і перевіримо хто залишився

setdiff(tenders_info$university_name, zvo_data$university_name) %>% View()

## у нас все ще лишаються ЗВО, для яких не знайшлось відповідника через одруківки
# щоб знайти їх використаємо fuzzyjoin

# немає сенсу приєднувати весь датасет, попрацюємо лише з "проблемними" ЗВО

# спочатку обираємо ті, які успішно поєднуються

joined_zvo <- zvo_data %>% 
  left_join(tenders_info, by = 'university_name') %>% 
  select(university_name, university_short_name, doporog_money_perc, all_money) %>%
  filter(!is.na(doporog_money_perc))

# в R можна зробити собі власний зручний синтаксис навіть для операторів

`%not_in%` <- purrr::negate(`%in%`)

zvo_data %>% 
  select(university_name) %>%
  filter(university_name %not_in% joined_zvo$university_name) %>%
  stringdist_left_join(tenders_info, by = 'university_name', distance_col = 'n') %>% 
  filter(!is.na(n)) %>%
  View()

# це виглядає непогано, поєднаємо це в один результат

joined_zvo_tenders <- rbind(
  zvo_data %>% 
    left_join(tenders_info, by = 'university_name') %>% 
    select(university_name, university_short_name, doporog_money_perc, all_money) %>%
    filter(!is.na(doporog_money_perc)),
  zvo_data %>% 
    select(university_name, university_short_name) %>%
    filter(university_name %not_in% joined_zvo$university_name) %>%
    stringdist_left_join(tenders_info, by = 'university_name', distance_col = 'n') %>% 
    filter(!is.na(n)) %>% 
    select(university_name.x, university_short_name, doporog_money_perc, all_money) %>%
    rename(university_name = university_name.x)
)

# таким чином, у нас лишилось 15 ЗВО, яким не знайшлось відповідника
setdiff(zvo_data$university_name, joined_zvo_tenders$university_name) %>% View()

# погляньмо на скаттерплот з залежністю обсягу закупівель та частки коштів, які проводяться за "допорогами"  

joined_zvo_tenders %>%
  mutate(all_money = round(all_money/1000000, 2)) %>%
  ggplot(aes(x = all_money, y = doporog_money_perc, alpha = .7)) +
  geom_point(size = 2.5) + 
  scale_x_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 16, hjust = 0, vjust = -2),
    plot.caption = element_text(size = 12, hjust = 0.95, vjust = -1.5),
    legend.position = "none"
  ) +
  labs(title = 'Частка "допорогів" у закупівлях українських вишів',
       caption = 'Дані: Prozorro', 
       family = "Open Sans", col = '#5D646F',
       x = 'Обсяг закупівель, млн грн',
       y = 'Частка допорогових закупівкель, %') 

# цікавіше буде подивитись на інтерактивний графік

library('rbokeh')

p <- figure(width = 800, height = 550) %>%
  ly_points(all_money, doporog_money_perc, 
            data = joined_zvo_tenders %>% mutate(all_money = round(all_money/1000000, 2)) %>% 
              mutate(doporog_money_perc = round(doporog_money_perc)), 
            hover = list(university_short_name, all_money, doporog_money_perc))
p

# 1.1 Визначаємо університети з найбільшою забудов на території --------

## додамо дані ДАБІ та подивимось як часто університети не лише проводять допороги, а й віддають землю під забудову

zvo_dabi <- read_csv("data/zvo_dabi.csv", col_types = cols(edrpou = col_character()))

glimpse(zvo_dabi)

# в датасеті ДАБІ немає назв, лише ЄДРПОУ, тому спочатку ми маємо "повернути" колонку з ЄДРПОУ з датасету zvo_data

joined_zvo_tenders %>%
  left_join(zvo_data, by = 'university_name') %>%
  left_join(zvo_dabi, by = c('university_edrpou' = 'edrpou')) %>% View()

typeof(zvo_data$university_edrpou)

# варто змінити тип колонки з ЄДРПОУ на character

zvo_data <- zvo_data %>% mutate(university_edrpou = as.character(university_edrpou))

joined_zvo_tenders <- joined_zvo_tenders %>%
  left_join(zvo_data, by = 'university_name') %>% 
  select(university_name, university_short_name.x, doporog_money_perc, all_money, university_edrpou) %>%
  rename(university_short_name = university_short_name.x)

# і додати нулі 

joined_zvo_tenders <- joined_zvo_tenders %>%
  mutate(n = nchar(university_edrpou)) %>% 
  mutate(university_edrpou = ifelse(n == 7 | n == 6, str_pad(university_edrpou, 8, pad = "0"), university_edrpou)) %>%
  select(-n)

# переконаймося що ключ для поєднання (колонка з ЄДРПОУ) у першому датасеті - унікальний

joined_zvo_tenders %>%
  select(university_edrpou) %>%
  distinct() %>%
  count()

# тепер можемо поєднувати

joined_zvo_tenders_dabi <- joined_zvo_tenders %>%
  left_join(zvo_dabi, by = c('university_edrpou' = 'edrpou')) %>% 
  filter(!is.na(type_of_construction)) 

# тепер пошукаємо хто на території яких ЗВО проводиться забудова 
# шукати варто по паттерну "житло", який може зустрічатись у колонках "document" та "type_of_construction", 
# тож ми застосуємо метод filter_at

joined_zvo_tenders_dabi %>% 
  filter_at(vars(document, type_of_construction), any_vars(str_detect(., 'житло'))) %>%  
  count()

# відфільтруймо ті ЗВО, де ведеться житлове будівництво та порахуємо скільки таких випадків для кожного

joined_zvo_tenders_dabi %>% 
  filter_at(vars(document, type_of_construction), any_vars(str_detect(., 'житло'))) %>% 
  group_by(university_name) %>%
  summarise(dabi_n = n()) %>% 
  left_join(joined_zvo_tenders %>% select(university_name, doporog_money_perc), by = 'university_name') %>% 
  arrange(desc(dabi_n)) %>% 
  View()

joined_zvo_tenders_dabi %>% 
  filter_at(vars(document, type_of_construction), any_vars(str_detect(., 'житло'))) %>%
  group_by(university_name) %>%
  summarise(dabi_n = n()) %>%
  left_join(joined_zvo_tenders %>% select(university_name, doporog_money_perc), by = 'university_name') %>%
  ggplot(aes(x = doporog_money_perc, y = dabi_n, alpha = .7)) +
  geom_point(size = 4) + 
  scale_x_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 16, hjust = 0, vjust = -2),
    plot.caption = element_text(size = 12, hjust = 0.95, vjust = -1.5),
    legend.position = "none"
  ) +
  labs(title = '"Допороги" та забудови українських вишів',
       caption = 'Дані: Prozorro, ДАБІ', 
       family = "Open Sans", col = '#5D646F',
       y = 'Кількість будівництв',
       x = 'Частка допорогових закупівкель, %') 


# 2. Куди йдуть та як повертаються кошти Радикальної партії --------

expenses <- read_excel("data/rpl_expenses.xlsx", sheet = 'salary')
donations <- read_excel("data/rpl_donations.xlsx")

# оскільки наш ключ імена - є сенс їх попередньо "почистити"

# перевіримо чи зустрічаються в іменах латинські літери
unique(na.omit(unlist(strsplit(unlist(donations$`ПІБ (для фіз. осіб)/Назва (для юр. осіб)`), "[^a-zA-Z]+"))))
unique(na.omit(unlist(strsplit(unlist(salaries$`ПІБ (для фіз. осіб)/Назва (для юр. осіб)`), "[^a-zA-Z]+"))))

# нас цікавлять тільки зарплати

salaries <- expenses %>% filter(`Цільове призначення платежу` == 'Заробітна плата')

# створюємо функції, яка уніфікує для нас імена в колонках з іменами

clean_names <- function (str) {
  res <- str %>% 
    str_to_lower() %>% 
    str_replace_all('c', 'с') %>%
    str_replace_all('[[:punct:]=+|$^]', ' ') %>% 
    str_trim() 
  return(res)
}

# заодно позбудемось цієї жахливої назви колонки

salaries <- salaries %>% 
  rename(name = `ПІБ (для фіз. осіб)/Назва (для юр. осіб)`) %>%
  mutate(name = clean_names(name)) 

donations <- donations %>% 
  rename(name = `ПІБ (для фіз. осіб)/Назва (для юр. осіб)`) %>%
  mutate(name = clean_names(name))

# і одразу ж перевіримо чи є помилки в іменах, які завадять нам поєднати дані

salaries %>% 
  select(name) %>% distinct() %>%
  stringdist_left_join(donations %>% select(name) %>% distinct(), by = 'name', distance_col = 'n') %>% View()

# тепер ми готові до можливих проблем та можемо поєднувати
# нам потрібно отримати список імен, які зустрічають в обох датасетах

salaries_donations_common <- rbind(
  salaries %>% select(name) %>% 
    semi_join(donations %>% select(name), by = 'name') %>% distinct(), 
  
  salaries %>% select(name) %>% 
    stringdist_left_join(donations %>% select(name), by = 'name', distance_col = 'n') %>% filter(n >= 1) %>% 
    select(name.x) %>% rename(name = name.x) %>% distinct()
)

# скільки отримали від партії

salaries %>% filter(name %in% salaries_donations_common$name) %>% summarise(sum(`Сума (грн)`, na.rm = TRUE))

# і скільки віддали 

donations %>% filter(name %in% salaries_donations_common$name) %>% summarise(sum(`Сума (грн)`, na.rm = TRUE)) 

# 3. Спробуємо деанонімізувати учасників ЗНО та побачити чи поїхали з рідної області заради навчання --------

zno <- read.csv("data/zno.csv", stringsAsFactors=FALSE) 
vstup <- read.csv("data/vstup.csv", stringsAsFactors=FALSE) 

###### масив ЗНО дуже об'ємний, тому для нашого заняття я сформувала менший датасет і зробила це так:
# zno <- sample_n(zno_data, 100000)
# 
# zno <- zno %>% select(OUTID, REGNAME, matches("ball100"))
# 
# zno <- zno %>%
#   mutate(inozBall100 = ifelse(!is.na(fraBall100), fraBall100, NA)) %>%
#   mutate(inozBall100 = ifelse(!is.na(deuBall100), deuBall100, inozBall100)) %>%
#   mutate(inozBall100 = ifelse(!is.na(spaBall100), spaBall100, inozBall100)) %>%
#   select(-c("fraBall100", "deuBall100", "spaBall100"))
  
  # ви можете знайти повний масив тут: https://zno.testportal.com.ua/opendata 

# дані про вступ також зробила компактнішими
# vstup <- vstup %>% select(name, ukr_mark, math, bio, hist, eng, geo, him, phiz, region, inoz)

glimpse(zno)
glimpse(vstup)

# ми будемо поєднувати вступників за балами ЗНО, тож варто привести у відповідність типи даних у відповідних колонках

cols <- colnames(zno %>% select(matches("ball100")))
zno[ ,cols] %<>% lapply(function(x) str_replace(x, ',', '.'))
zno[ ,cols] %<>% lapply(function(x) str_replace(x, '.0', ''))
zno[zno == 'null'] <- NA

vstup_cols <- colnames(vstup %>% select(-c('name', 'region')))
vstup[ ,vstup_cols] %<>% lapply(function(x) str_replace(x, ',', '.'))
vstup[ ,vstup_cols] %<>% lapply(function(x) as.character(x))

# це той випадок, коли поєднання може дати багато дублів, спробуймо:

zno_vstup <- zno %>%
  left_join(vstup, by = c(
    'UkrBall100' = 'ukr_mark',
    'mathBall100' = 'math',
    'bioBall100' = 'bio',
    'histBall100' = 'hist',
    'engBall100' = 'eng',
    'geoBall100' = 'geo',
    'chemBall100' = 'him',
    'physBall100' = 'phiz',
    'inozBall100' = 'inoz'))

# подивимось на унікальні та на дублі

doubles <- zno_vstup %>% count(OUTID) %>% filter(n>1) %>% 
    left_join(zno, by='OUTID') %>% 
    select(OUTID, REGNAME, n)

unique <- zno_vstup %>%
  filter(OUTID %not_in% doubles$OUTID) %>%
  filter(!is.na(name)) %>%
  select(OUTID, name, REGNAME, region)

# подивимось хто з них переїхав заради навчання, а хто лишився

aggregated <- unique %>% 
  mutate(REGNAME = REGNAME %>% str_replace('м.Київ', 'м. Київ')) %>%
  mutate(feature = ifelse(REGNAME == region, 'stayed', 'moved')) %>%
  group_by(REGNAME, feature) %>%
  count() %>%
  ungroup() %>%
  group_by(REGNAME) %>%
  mutate(rel.freq = paste0(100 * n/sum(n))) %>% 
  mutate(rel.freq = round(as.numeric(rel.freq))) 

aggregated %>%
  ggplot(aes(REGNAME, rel.freq, fill = feature)) + 
  geom_bar(stat="identity", position = "dodge") +
  theme_minimal() +
  coord_flip()
