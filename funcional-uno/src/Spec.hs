module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "El pdepreludat se instaló correctamente" $ do
      doble 1 `shouldBe` 2

  describe "Test de Funcional 1: Primeros ejercicios" $ do
    it "esMultiploDeTres/1" $ do
      esMultiploDeTres 9 `shouldBe` True
      esMultiploDeTres 10 `shouldBe` False
    
    it "esMultiploDeTres/1" $ do
      esMultiploDeTres 9 `shouldBe` True
    
    it "esMultiploDe/2" $ do
      esMultiploDe 3 9 `shouldBe` True
      esMultiploDe 5 25 `shouldBe` True
      esMultiploDe 2 9 `shouldBe`False

    it "cubo/1" $ do
      cubo 9 `shouldBe` 729
      cubo 2 `shouldBe` 8
    
    it "area/2" $ do
      area 2 9 `shouldBe` 18
    
    it "esBisiesto/1" $ do
      esBisiesto 2026 `shouldBe` False
      esBisiesto 2024 `shouldBe` True
      esBisiesto 2020 `shouldBe` True
      esBisiesto 2000 `shouldBe` False

    it "celciusToFahr/1" $ do
      celciusToFahr (-40) `shouldBe` (-40)
      celciusToFahr 10 `shouldBe` 50

    it "fahrToCelsius /1" $ do
      fahrToCelcius 50 `shouldBe` 10
      fahrToCelcius (-40) `shouldBe` (-40)

    it "haceFrio/1" $ do
      haceFrioF 41 `shouldBe` True
      haceFrioF 50 `shouldBe` False

    it "mcm/2" $ do
      mcm 72 50 `shouldBe` 1800
      mcm 6 33 `shouldBe` 66

    it "dispersion/3" $ do
      dispersion 322 283 294 `shouldBe` 39
      dispersion 80 100 101 `shouldBe` 21

    it "diasParejos/3" $ do
      diasParejos 400 300 200 `shouldBe` False
      diasParejos 275 300 295 `shouldBe` True

    it "diasLocos/3" $ do
      diasLocos 400 300 200 `shouldBe` True
      diasLocos 275 300 295 `shouldBe` False

    it "diasNormales/3" $ do
      diasNormales 400 300 200 `shouldBe` False
      diasNormales 275 300 295 `shouldBe` False  
      diasNormales 260 300 295 `shouldBe` True  

    it "pesoPino/1" $ do
      pesoPino 200 `shouldBe` 600
      pesoPino 500 `shouldBe` 1000  

    it "esPesoUtil/1" $ do
      esPesoUtil 500 `shouldBe` True
      esPesoUtil 1100 `shouldBe` False  
      esPesoUtil 200 `shouldBe` False

    it "sirvePino/1" $ do
      sirvePino 200 `shouldBe` True
      sirvePino 500 `shouldBe` False

    it "esCuadradoPerfecto/1" $ do
      esCuadradoPerfecto 4 `shouldBe` True
      esCuadradoPerfecto 5 `shouldBe` False
      esCuadradoPerfecto 81 `shouldBe` True
      esCuadradoPerfecto 80 `shouldBe` False