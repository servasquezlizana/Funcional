module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Función estaEnBuenEstado" $ do
    
    it "Un Peugeot no debería estar en buen estado" $ do
      estaEnBuenEstado peugeot `shouldBe` False

    it "Un Lamborghini con 99s de tiempo y 7 de desgaste de chasis debería estar en buen estado" $ do
      estaEnBuenEstado lamborghini `shouldBe` True

    it "Un Fiat con 99s de tiempo y 33 de desgaste de chasis no debería estar en buen estado" $ do
      estaEnBuenEstado fiat `shouldBe` False

    it "Un Ferrari con 130s de tiempo, 50 de ruedas y 30 de chasis debería estar en buen estado" $ do
      estaEnBuenEstado ferrari1 `shouldBe` True

    it "Un Ferrari con 15s de tiempo, 50 de ruedas y 45 de chasis no debería estar en buen estado" $ do
      estaEnBuenEstado ferrari2 `shouldBe` False

    it "Un Ferrari con 150s de tiempo, 70 de ruedas y 30 de chasis no debería estar en buen estado" $ do
      estaEnBuenEstado ferrari3 `shouldBe` False

-- Definiciones de los autos de la tabla
peugeot :: Auto
peugeot = Auto "Peugeot" "208" 0 0 150 [] 0

lamborghini :: Auto
lamborghini = Auto "Lamborghini" "Aventador" 0 7 350 [] 99

fiat :: Auto
fiat = Auto "Fiat" "600" 0 33 110 [] 99

ferrari1 :: Auto
ferrari1 = Auto "Ferrari" "F40" 50 30 320 [] 130

ferrari2 :: Auto
ferrari2 = Auto "Ferrari" "F50" 50 45 325 [] 15

ferrari3 :: Auto
ferrari3 = Auto "Ferrari" "Enzo" 70 30 350 [] 150