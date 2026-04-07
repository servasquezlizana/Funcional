module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de kata 1" $ do
    it "nivelDeCool/2 para nivel 3" $ do
      nivelDeCool "neuquen" "X" `shouldBe` 3
      nivelDeCool "ana" "Gravity Falls" `shouldBe` 3
      nivelDeCool "neuquen" "Hey Arnold!" `shouldBe` 3
      nivelDeCool "neuquen" "FRIENDS" `shouldBe` 3
      nivelDeCool "carlos" "Hey Arnold!" `shouldBe` 3

    it "nivelDeCool/2 para nivel 2" $ do
      nivelDeCool "ernesto" "How I Meet Your Mother" `shouldBe` 2
      nivelDeCool "juan" "Los Simpsons" `shouldBe` 2
      nivelDeCool "pedro" "Gravity Falls" `shouldBe` 2

    it "nivelDeCool/2 para nivel 1" $ do
      nivelDeCool "ernesto" "Flash" `shouldBe` 1
      nivelDeCool "sergio" "CSI" `shouldBe` 1

    it "nivelDeCool/2 para nivel 0" $ do
      nivelDeCool "maria magdalena" "Gravity Falls" `shouldBe` 0
      nivelDeCool "pedro picapiedra" "Gravity Falls" `shouldBe` 0