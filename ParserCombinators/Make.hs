{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Data.Text (pack)
import Filesystem.Path.CurrentOS (encodeString)

pandocOpts :: Text -> Text -> [ Text ]
pandocOpts inputFile outputFile =
    [ "--smart"
    , "--standalone"
    , "-t"
    , "revealjs"
    , "-H"
    , "./Documentation/template/reveal"
    , "--no-highlight"
    , "--slide-level=2"
    , "-f"
    , "markdown"
    , "-o"
    , outputFile <> ".html"
    , inputFile
    ]

main :: IO ExitCode
main = do
    proc "pandoc" (pandocOpts "Documentation/presentation.md" "Documentation/presentation") empty
