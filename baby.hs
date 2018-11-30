doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x < 100
    then x * 2
    else x

boomBang xs = [ if x < 10 then "BOOM" else "BANG" | x <- xs, odd x ]

nouns = ["hobo", "frog", "pope"]
adjectives = ["scheming", "angry", "rose-cheeked"]
combination = [ adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]

length' xs = sum [ 1 | _ <- xs]