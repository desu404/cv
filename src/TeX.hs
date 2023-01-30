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
    , github "Github: @desu404"

    , pure "\\begin{document}"

-- | Education section
    , section "Образование" "Education" [
        pure "\\textbf"
      , nsu
      , pure "\\hfill \\textit"
      , header "Сент. 2022 - Сейчас" "Sep. 2022 - Present"
      , pure "\\\\"
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
          , en "\\item Software Defined Network application, server side."
          , ru "\\item Программно определяемая сеть, серверная часть."
          , en "\\item Web application integrated with an AppFolio API, server side."
          , ru "\\item Веб приложение с AppFolio API, серверная часть."
        ]
    ]

-- | End
    , pure "\\end{document}"
    ]

