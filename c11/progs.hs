data OperatingSystem =
      GnuPlusLinux
    | OpenBSDPlus
    | Mac
    | Windows
    deriving (Eq, Show, Enum)

data ProgLang =
      Haskell
    | Agda
    | Idris
    | PureScript
    deriving (Eq, Show, Enum)

data Programmer =
      Programmer { os:: OperatingSystem
                 , lang :: ProgLang }
      deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [
    GnuPlusLinux
    , OpenBSDPlus
    , Mac
    , Windows
  ]

allLanguages :: [ProgLang]
allLanguages =
  [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [(Programmer x y) | x <- allOperatingSystems,
                                     y <- allLanguages ]

allProgs :: [Programmer]
allProgs = [(Programmer x y) | x <- enumFrom GnuPlusLinux,
                               y <- enumFrom Haskell ]
