module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "El pdepreludat se instaló correctamente" $ do
      doble 1 `shouldBe` 2

  describe "Ejercicios de Aplicacion Parcial | " $ do
    it "siguiente/1" $ do
      siguiente 1 `shouldBe` 2
      siguiente 5 `shouldBe` 6

    it "mitad/1" $ do
      mitad 2 `shouldBe` 1
      mitad 10 `shouldBe` 5

    it "inversa/1" $ do
      inversa 10 `shouldBe` 0.1
      inversa 2 `shouldBe` 0.5

    it "triple/1" $ do
      triple 3 `shouldBe` 9
      triple 1 `shouldBe` 3

    it "esNumeroPositivo/1" $ do
      esNumeroPositivo 0.99 `shouldBe` True
      esNumeroPositivo (-1) `shouldBe` False
      esNumeroPositivo 60 `shouldBe` True
      esNumeroPositivo (-0.39) `shouldBe` False

  describe "Ejercicios de Composicion | " $ do
    it "esMultiploDe/2" $ do
      esMultiploDe 3 9 `shouldBe` True
      esMultiploDe 5 25 `shouldBe` True
      esMultiploDe 2 9 `shouldBe`False

    it "esBisiesto/1" $ do
      esBisiesto 2026 `shouldBe` False
      esBisiesto 2024 `shouldBe` True
      esBisiesto 2020 `shouldBe` True
      esBisiesto 2000 `shouldBe` False

    it "inversaRaizCuadrada/1" $ do
      inversaRaizCuadada 4 `shouldBe` 0.5
      inversaRaizCuadada 25 `shouldBe` 0.2

    it "incrementMCuadradoN/2" $ do
      incrementMCuadradoN 3 2 `shouldBe` 11
      incrementMCuadradoN 2 3 `shouldBe` 7

    it "esResultadoPar/2" $ do
      esResultadoPar 2 5 `shouldBe` True
      esResultadoPar 3 2 `shouldBe` False
