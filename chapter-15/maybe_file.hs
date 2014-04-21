module File where

import Control.Monad
--import Data.List (break)

(-:) :: a -> (a -> b) -> b
a -: f = f a

type Name = String
type Data = String
data FSItem = File Name Data
            | Folder Name [FSItem]
            deriving Show

myDisk :: FSItem
myDisk =
    Folder "root"
        [ File "goat_yelling_like_man.wmv" "baaaaaa"
        , File "pope_time.avi" "god bless"
        , Folder "pics"
            [ File "ape_throwing_up.jpg" "bleargh"
            , File "watermelon_smash.gif" "smash!!"
            , File "skull_man(scary).bmp" "Yikes!"
            ]
        , File "dijon_poupon.doc" "best mustard"
        , Folder "programs"
            [ File "fartwizard.exe" "10gotofart"
            , File "owl_bandit.dmg" "mov eax, h00t"
            , File "not_a_virus.exe" "really not a virus"
            , Folder "source code"
                [ File "best_hs_prog.hs" "main = print (fix error)"
                , File "random.hs" "main = print 4"
                ]
            ]
        ]

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)

type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> Maybe FSZipper
fsUp (item, FSCrumb name ls rs:bs) = return (Folder name (ls ++ [item] ++ rs), bs)
fsUp (_, []) = Nothing

fsTo :: Name -> FSZipper -> Maybe FSZipper
fsTo name (Folder folderName items, bs)
    | null result = Nothing
    | otherwise = let item:rs = result in return (item, FSCrumb folderName ls rs:bs)
    where (ls, result) = break (nameIs name) items
fsTo _ (File _ _, _) = Nothing

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Folder _ items, bs) = (Folder newName items, bs)
fsRename newName (File _ dat, bs) = (File newName dat, bs)

fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder folderName items, bs) =
    (Folder folderName (item:items), bs)
