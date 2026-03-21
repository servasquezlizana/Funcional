module Spec where
import PdePreludat
import Library
import Test.Hspec
import GHC.RTS.Flags (ProfFlags(descrSelector), CCFlags (doCostCentres))

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "El pdepreludat se instaló correctamente" $ do
      doble 1 `shouldBe` 2
  
  describe "Funcional 3 - Ejercicios" $ do
    it "fst3/1" $ do
      fst3 (1,2,3) `shouldBe` 1

    it "snd3/1" $ do
      snd3 (1,2,3) `shouldBe` 2

    it "trd3/1" $ do
      trd3 (1,2,3) `shouldBe` 3

    it "aplicar/2" $ do
      aplicar ((2*), (3*)) 8 `shouldBe` (16, 24)
      aplicar ((3+), (2*)) 8 `shouldBe` (11, 16)

    it "cuentaBizarra/1" $ do
      cuentaBizarra (5,8) `shouldBe` 40
      cuentaBizarra (8,5) `shouldBe` 13
      cuentaBizarra (5,29) `shouldBe` 24
    
    it "esNotaBochorno/1" $ do
      esNotaBochorno 7 `shouldBe` False
      esNotaBochorno 5 `shouldBe` True

    it "aprobo/1" $ do
      aprobo (5,6) `shouldBe` False
      aprobo (6,6) `shouldBe` True
      aprobo (6,8) `shouldBe` True

    it "promociono/1" $ do
      promociono (8,7) `shouldBe` True
      promociono (10, 5) `shouldBe` False 

    it "aproboPrimerParcial/1" $ do
      aproboPrimerParcial (6,5) `shouldBe` True 
      aproboPrimerParcial (3,7) `shouldBe` False 

    it "notasFinales/1" $ do
      notasFinales ((2,7),(6,-1)) `shouldBe` (6,7)
      notasFinales ((2,2),(6,2)) `shouldBe` (6,2)
      notasFinales ((8,7),(-1,-1)) `shouldBe` (8,7)

    it "recursa/1" $ do
      recursa ((2,7),(6,-1)) `shouldBe` False
      recursa ((2,7),(5,-1)) `shouldBe` True

    it "recupero/1" $ do
      recupero ((2,7),(6,-1)) `shouldBe` True
      recupero ((6,7),(-1,-1)) `shouldBe` False

    it "recuperoDeGusto/1" $ do
      recuperoDeGusto ((7,8),(9,-1)) `shouldBe` True
      recuperoDeGusto ((7,8),(-1,-1)) `shouldBe` False

    it "esMayorDeEdad/1" $ do
      esMayorDeEdad ("Juan", 18) `shouldBe` True
      esMayorDeEdad ("Juan", 17) `shouldBe` False

    it "calcular/1" $ do
      calcular (4,5) `shouldBe` (8,6)
      calcular (3,7) `shouldBe` (3,8)