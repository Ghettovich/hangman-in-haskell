drawStickFigure :: String
drawStickFigure = unlines $ ["=========",
                             "|    |",
                             "|   " ++ "_O_",
                             "|   " ++ " |  ",
                             "|   " ++ "/ \\"
                             ]


main = putStr drawStickFigure