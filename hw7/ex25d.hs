-- ex25d.hs

h :: Int -> Int -> Double
h 0 0 = 0.0
h (-1) _ = 0.0
h _ (-1) = 0.0
h s c = (fromIntegral s / fromIntegral (s + c)) * (h (s-1) c)
          + (fromIntegral c / fromIntegral (s + c)) * (h s (c-1))
          + (fromIntegral (max s c) / fromIntegral (s + c))
