{-# LANGUAGE PartialTypeSignatures, LambdaCase #-}
module TeX
  ( resume
  , Mode(..)
  ) where

import Control.Monad
import Control.Monad.Reader
import Data.Functor

data Mode = Ru | En
    deriving (Show, Eq)

type Resume = Reader Mode String

ru_en :: _ -> _ -> Resume
ru_en r e = ask <&> \case
    Ru -> r
    En -> e

en = ru_en []
ru = flip ru_en $ []

tex name r e = do
    lang <- ru_en r e
    return $ "\\" <> name <> "{" <> lang <> "}"

item :: _ -> _ -> Resume
item r e = ru_en r e >>= return . ("\\item " <>)

combine :: [Resume] -> Resume
combine [] = return []
combine (r:rs) = f <$> r <*> combine rs
    where
     f "" y = y
     f x "" = x
     f x y = x <> "\n" <> y

name = tex "name"
address = tex "address"
email e = address e e
github g = address g g
br = pure "\\\\"

section :: _ -> _ -> [Resume] -> Resume
section r e p = do
    body <- combine p
    t <- ru_en r e
    return $
        "\\begin{rSection}{" <> t <> "}" 
        <> body <> "\n"
        <> "\\end{rSection}"

subsection :: _ -> _ -> [Resume] -> Resume
subsection r e p = do
    body <- combine p
    t <- ru_en r e
    return $
        "\\begin{rSubsection}{" <> t <> "}"
        <> body <> "\n"
        <> "\\end{rSubsection}"

tabular p = do
    body <- combine p
    return $
        "\\begin{tabular}{@{} >{\\bfseries}l @{\\hspace{6ex}} l @{}}\n"
        <> body <> "\n"
        <> "\\end{tabular}"

header :: _ -> _ -> Resume
header r e = do
    p <- ru_en r e
    return $ "{" <> p <> "}"

nsu = header "НГУ, Новосибирск, Россия" "NSU, Novosibirsk, Russia"

resume :: Resume
resume = 
    combine [
      pure "\\documentclass[11pt]{resume}"

    , name "Y. B." "Y. B."
    , address "Новосибирск, Россия" "Novosibirsk, Russia"
    , email "+7 (***) *** ** ** \\\\ nn@gmail.com"
    , github "Github: @eloczka"

    , pure "\\begin{document}"

-- | Education section
    , section "Образование" "Education" [
        pure "\\textbf"
      , nsu
      , pure "\\hfill \\textit"
      , header "Сент. 2022 - Сейчас" "Sep. 2022 - Present"
      , br
      , en "Bachelor student in mathematics"
      , ru "Бакалавр по специальности Математика"
    ]

-- | Experience section.
    , section "Опыт работы" "Experience" [
        pure " "  
      , subsection "Irodori team" "Irodori team" [
            header "Ноябрь 2022 - Сейчас" "Nov. 2022 - Present"
          , header "Junior Elixir Developer" "Junior Elixir Developer"
          , pure "{}"
          , item "Программно определяемая сеть, серверная часть." 
                 "Software Defined Network application, server side."
          , item "Веб приложение с AppFolio API, серверная часть."
                 "Web application integrated with an AppFolio API, server side."
      ]
    ]

-- | Skills section
    , section "Навыки" "Skills" [
        pure " "
      , tabular [
            en "Main Languages & Haskell, Elixir, Go, C"
          , ru "Основные языки & Haskell, Elixir, Go, C"
          , br
          , en "Comfortable with & C++, Vala, Python, Shell"
          , ru "Имею опыт & C++, Vala, Python, Shell"
          , br
          , en "Technologies & Unix, REST, HTTP, GTK-3, OTP, MACID"
          , ru "Технологии & Unix, REST, HTTP, GTK-3, OTP, MACID"
          , br
          , en "Tools & Postman, Swagger, Git, CMake, $\\mbox{\\LaTeX}$, Reduce"
          , ru "Инструменты & Postman, Swagger, Git, CMake, $\\mbox{\\LaTeX}$, Reduce"
          , br
          , en "Other & Rich expirience in Algorithms and Data Structures."
          , ru "Другое & Хорошо ориентируюсь в алгоритмах и структурах данных."
      ]
    ]

-- | Languages section
    , section "Языки" "Languages" [
        pure " "
      , tabular [
            en "Russian & Native"
          , ru "Русский & Носитель"
          , br
          , en "English & Upper-Intermidate, B2"
          , ru "Английский & Upper-Intermidate, B2"
          , br
          , en "French, German & Elementary, A2"
          , ru "Французский, Немецкий & Elementary, A2"
      ]
    ]

-- | CP section
    , section "Олимпиадное программирование" "Competitive programming" [
        pure " "
      -- | ROI
      , subsection "ВсОШ по информатике, 11ый класс"
                   "Russian Olympiad in Informatics" [
            pure "{2021 - 2022}{}{}"
          , item "Третье место на региональном этапе, призёр."
                 "3rd place in the Regional stage, winner."
      ]
      -- | Other
      , subsection "Призёр отборочного этапа, финалист"
                   "Qualifying stage winner, final stage participant" [
            pure "{2021 - 2022}"
          , en "{11th grade}{}"
          , ru "{11ый класс}{}"
          , item "Олимпида СПбГУ по информатике" "SpbSU Olympiad in Informatics"
          , item "Всесибирская Открытая Олимпиада школьников по информатике"
                 "All-Siberian Olympiad in Informatics"
          , item "Олимпиада ВШЭ по инфрматике" "HSE Olympiad in Informatics"
          , item "Шаг в будущее (МГТУ им. Баумана)" "BMSTU Programming Olympiad"
          , item "Когнитивные технологии (МИСиС)"
                 "MISIS Programming Olympiad (Cognitive Technology)"
      ]
    ]

-- | End
    , pure "\\end{document}"
    ]

