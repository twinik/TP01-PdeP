{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

data Especie = Metal | NoMetal | Halogeno | GasNoble deriving (Show)

data Sustancia
  = Elemento
      { nombre :: String,
        simboloQuimico :: String,
        especie :: Especie,
        numeroAtomico :: Int
      }
  | Compuesto
      { nombre :: String,
        especie :: Especie,
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
      especie = NoMetal,
      numeroAtomico = 1
    }

oxigeno :: Sustancia
oxigeno =
  Elemento
    { nombre = "Oxigeno",
      simboloQuimico = "O",
      especie = NoMetal,
      numeroAtomico = 8
    }

agua :: Sustancia
agua =
  Compuesto
    { nombre = "Agua",
      especie = NoMetal,
      componentes = [Componente hidrogeno 2, Componente oxigeno 1]
    }

{- Punto 2 -}
{- especieDeSustancia :: Sustancia -> String
especieDeSustancia (Elemento _ _ especie _) = especie
especieDeSustancia (Compuesto _ especie _) = especie -}

esElemento :: Sustancia -> Bool
esElemento (Elemento {}) = True
esElemento (Compuesto {}) = False

{- conduceBien :: Sustancia -> String
conduceBien sustancia
  | especieDeSustancia sustancia == "Metal" = "Conduce bien calor y electricidad"
  | especieDeSustancia sustancia == "Gas noble" && esElemento sustancia = "Conduce bien electricidad"
  | especieDeSustancia sustancia == "Halogeno" && (not . esElemento) sustancia = "Conduce bien calor"
  | otherwise = "No es buen conductor"
 -}
-- conduceBien :: Sustancia -> String -> Bool

{- Punto 3 -}
nombreDeSustancia :: Sustancia -> String
nombreDeSustancia (Elemento nombre _ _ _) = nombre
nombreDeSustancia (Compuesto nombre _ _) = nombre

dosVocales :: [String]
dosVocales = ["ae", "ai", "ao", "au", "ea", "ei", "eo", "eu", "ia", "ie", "io", "iu", "ua", "ue", "ui", "uo"]

nombreUnion :: String -> String
nombreUnion elemento
  | filter (\n -> n == drop (((+ (-2)) . length) elemento) elemento) dosVocales /= [] = (take ((+ (-2)) $ length elemento) elemento) ++ "uro"
  | filter ((==) . last $ elemento) ['a', 'e', 'i', 'o', 'u'] /= [] = (take ((+ (-1)) $ length elemento) elemento) ++ "uro"
  | otherwise = elemento ++ "uro"

{- Punto 4 -}
combinarSustancias :: Sustancia -> Sustancia -> String
combinarSustancias sustancia1 sustancia2 = (nombreUnion . nombreDeSustancia) sustancia1 ++ " de " ++ nombreDeSustancia sustancia2

{- Punto 5 -}
sustanciaComponente :: Componente -> Sustancia
sustanciaComponente (Componente sustancia _) = sustancia

combinarNombreComponentes :: Componente -> Componente -> String
combinarNombreComponentes componente1 componente2 = combinarSustancias (sustanciaComponente componente1) (sustanciaComponente componente2)

mezclarDosComponentes :: Componente -> Componente -> Sustancia
mezclarDosComponentes componente1 componente2 =
  Compuesto
    { nombre = (combinarNombreComponentes componente1 componente2),
      especie = NoMetal,
      componentes = [componente1, componente2]
    }

{- Punto 6 -}
cantidadComponente :: Componente -> String
cantidadComponente (Componente _ cantidad)
  | cantidad == 1 = ""
  | otherwise = show cantidad

formulaComponente :: Componente -> String
formulaComponente componente
  | (esElemento . sustanciaComponente) componente = (simboloQuimico . sustanciaComponente $ componente) ++ (cantidadComponente componente)
  | otherwise = "(" ++ concat (map (formulaComponente) (componentes . sustanciaComponente $ componente)) ++ ")" ++ (cantidadComponente componente)

formulaSustancia :: Sustancia -> String
formulaSustancia (Elemento _ simboloQuimico _ _) = simboloQuimico
formulaSustancia (Compuesto _ _ componentes) = concat (map (formulaComponente) componentes)