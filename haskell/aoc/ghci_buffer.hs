:{
let 
    det ((a, b), (c, d)) = a * d - c * b
    inv m@((a, b), (c, d)) = let d' = det m in if d' == 0 then Nothing else Just ((d `div` d', -b `div` d'), (-c `div` d', a `div` d'))
    ((a, b), (c, d)) .* (x, y) = (a * x + b * y, c * x + d * y)
    mcols ((x1, y1), (x2, y2)) = ((x1, x2), (y1, y2))
    cost m p = inv m <&> (.* p) >>= (\v -> if (uncurry (&&) . both (<= 100)) v then return v else Nothing)
    sle m@((a, b), (c, d)) v = let (d', m') = (det m, ((d, -b), (-c, a))) in both (`div` d') $ m' .* v

    m1 :: ((Int, Int), (Int, Int)) = ((94, 34), (22, 67))
    v1 :: (Int, Int) = (8400, 5400)

    m2 :: ((Int, Int), (Int, Int))  = ((26, 66), (67, 21))
    v2 :: (Int, Int)  = (12748, 12176)

    m3 :: ((Int, Int), (Int, Int))  = ((17, 86), (84, 37))
    v3 :: (Int, Int)  = (7870, 6450)

    m4 :: ((Int, Int), (Int, Int))  = ((69, 23), (27, 71))
    v4 :: (Int, Int)  = (18641, 10279)

    ms = [m1, m2, m3, m4]
    vs = [v1, v2, v3, v4]
  in sle (mcols m1) v1
:}
