module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | Papas | PatiVegano | BaconDeTofu | PanIntegral
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente Papas = 10
precioIngrediente PatiVegano = 10
precioIngrediente BaconDeTofu = 10
precioIngrediente PanIntegral = 3
data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

--No agregamos precio final aca porque lo usamos directamente en las pruebas, para ver que lo que estamos modificando realmente sea asi

--Parte 1
agrandar :: Hamburguesa -> Hamburguesa
agrandar hamburguesa
    | any (== Carne) (ingredientes hamburguesa) = hamburguesa {ingredientes = ingredientes hamburguesa ++ [Carne]}
    | any (== Pollo) (ingredientes hamburguesa) = hamburguesa {ingredientes = ingredientes hamburguesa ++ [Pollo]}
    | any (== PatiVegano) (ingredientes hamburguesa) = hamburguesa {ingredientes = ingredientes hamburguesa ++ [PatiVegano]}

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente ingrediente hamburguesa = hamburguesa {ingredientes = ingredientes hamburguesa ++ [ingrediente]}

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento porcentaje hamburguesa = hamburguesa {precioBase = precioBase hamburguesa - obtenerPorcentaje porcentaje (precioBase hamburguesa)}

obtenerPorcentaje :: Number -> Number -> Number
obtenerPorcentaje porcentaje numero = (porcentaje * numero) / 100

--Parte 2
delDia :: Hamburguesa -> Hamburguesa
delDia hamburguesa =  descuento 30 (hamburguesa {ingredientes = ingredientes hamburguesa ++ [Papas]})

--Parte 3
hacerVeggie :: Hamburguesa -> Hamburguesa
hacerVeggie hamburguesa = hamburguesa {
    ingredientes = map reemplazarSiNoEsVeggie (ingredientes hamburguesa)
}

reemplazarSiNoEsVeggie :: Ingrediente -> Ingrediente
reemplazarSiNoEsVeggie ingrediente
    | ingrediente == Carne = PatiVegano
    | ingrediente == Pollo = PatiVegano
    | ingrediente == Cheddar = QuesoDeAlmendras
    | ingrediente == Panceta = BaconDeTofu
    | otherwise = ingrediente 

cambiarPanDePati :: Hamburguesa -> Hamburguesa
cambiarPanDePati hamburguesa = hamburguesa {ingredientes = map reemplazarPanPorIntegral (ingredientes hamburguesa)}

reemplazarPanPorIntegral :: Ingrediente -> Ingrediente
reemplazarPanPorIntegral Pan = PanIntegral
reemplazarPanPorIntegral ingredienteNoPan = ingredienteNoPan

precioHamburguesa :: Hamburguesa -> Number
precioHamburguesa hamburguesa = precioBase hamburguesa + (sum . map precioIngrediente $ ingredientes hamburguesa)
