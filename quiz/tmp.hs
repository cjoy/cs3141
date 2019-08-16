data Id a b where
    Refl :: Id x x

f :: Id a Int -> a