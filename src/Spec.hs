module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)
cuartoDeLibra = Hamburguesa {precioBase = 20, ingredientes = [Pan, Carne, Cheddar, Pan]}
pdepBurguer = descuento 20 . agregarIngrediente Cheddar . agregarIngrediente Panceta . agrandar . agrandar $ cuartoDeLibra
dobleCuarto = agrandar. agregarIngrediente Cheddar $ cuartoDeLibra
bidPdep = agregarIngrediente Curry dobleCuarto 
dobleCuartoVegano = cambiarPanDePati . hacerVeggie $ dobleCuarto

correrTests :: IO ()
correrTests = hspec $ do
    describe "TP 6" $ do
        describe "Parte 1:" $ do
            it "la pdep burger es un cuarto de libra agrandado 2 veces con panceta, cheddar y 20% de descuento. Su precio final deberia ser 110. " $ do
                precioHamburguesa pdepBurguer `shouldBe` 110
        describe "Parte 2:" $ do
            it "la dobleCuarto es un cuarto de libra con carne y cheddar, el precio final deberia ser 84." $ do
                precioHamburguesa dobleCuarto `shouldBe` 84
            it "bigPdep es un doble cuarto con curry, el precio final deberia ser 89." $ do
                (precioHamburguesa . agregarIngrediente Curry $ dobleCuarto) `shouldBe` 89
            it "dobleCuarto del dia deberia valer 88" $ do
                (precioHamburguesa . delDia $ dobleCuarto) `shouldBe` 88
        describe "Parte 3:" $ do
            it "dobleCuartoVegano es un dobleCuarto pero veggie y con pan integral, deberia valer 76" $ do -- esta cuenta la hicimos a mano no es que lo sacamos de los tests :)
                precioHamburguesa dobleCuartoVegano `shouldBe` 76

