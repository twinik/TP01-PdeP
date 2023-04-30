{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

data Sustancia
  = Elemento
      { nombre :: String,
        simboloQuimico :: String,
        especie :: String,
        numeroAtomico :: Int
      }
  | Compuesto
      { nombre :: String,
        simboloQuimico :: String,
        especie :: String,
        componentes :: [Componente]
      }
  deriving (Show)

data Componente = Componente
  { sustancia :: Sustancia,
    cantidad :: Int
  }
  deriving (Show)

{- Punto 1 -}
hidrogeno :: Sustancia
hidrogeno =
  Elemento
    { nombre = "Hidrogeno",
      simboloQuimico = "H",
      especie = "No metal",
      numeroAtomico = 1
    }

oxigeno :: Sustancia
oxigeno =
  Elemento
    { nombre = "Oxigeno",
      simboloQuimico = "O",
      especie = "No metal",
      numeroAtomico = 8
    }

agua :: Sustancia
agua =
  Compuesto
    { nombre = "Agua",
      simboloQuimico = "H2O",
      especie = "No metal",
      componentes = [Componente hidrogeno 2, Componente oxigeno 1]
    }

{- Punto 2 -}
especieDeSustancia :: Sustancia -> String
especieDeSustancia (Elemento _ _ especie _) = especie
especieDeSustancia (Compuesto _ _ especie _) = especie

esElemento :: Sustancia -> Bool
esElemento (Elemento {}) = True
esElemento (Compuesto {}) = False

conduceBien :: Sustancia -> String
conduceBien sustancia
  | especieDeSustancia sustancia == "Metal" = "Conduce bien calor y electricidad"
  | especieDeSustancia sustancia == "Gas noble" && esElemento sustancia = "Conduce bien electricidad"
  | especieDeSustancia sustancia == "Halogeno" && (not . esElemento) sustancia = "Conduce bien calor"
  | otherwise = "No es buen conductor"

conduceBienCalor :: Sustancia -> Bool
conduceBienCalor sustancia =
  especieDeSustancia sustancia == "Metal" || especieDeSustancia sustancia == "Halogeno"

conduceBienElectricidad :: Sustancia -> Bool
conduceBienElectricidad sustancia =
  especieDeSustancia sustancia == "Metal" || especieDeSustancia sustancia == "Gas noble"

{- Punto 3 -}
nombreDeSustancia :: Sustancia -> String
nombreDeSustancia (Elemento nombre _ _ _) = nombre
nombreDeSustancia (Compuesto nombre _ _ _) = nombre

dosVocales :: [String]
dosVocales = ["ae", "ai", "ao", "au", "ea", "ei", "eo", "eu", "ia", "ie", "io", "iu", "ua", "ue", "ui", "uo"]

nombreUnion :: String -> String
nombreUnion elemento
  | filter (\n -> n == drop (((+ (-2)) . length) elemento) elemento) dosVocales /= [] = (take ((+ (-2)) $ length elemento) elemento) ++ "uro"
  | filter ((==) . last $ elemento) ['a', 'e', 'i', 'o', 'u'] /= [] = (take ((+ (-1)) $ length elemento) elemento) ++ "uro"
  | otherwise = elemento ++ "uro"

{- Punto 4 -}
combinar :: Sustancia -> Sustancia -> String
combinar sustancia1 sustancia2 = (nombreUnion . nombreDeSustancia) sustancia1 ++ " de " ++ nombreDeSustancia sustancia2