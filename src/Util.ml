let rec splitAt n l =
  if n > 0 then
    let (x::xs) = l in
    let (a, b) = splitAt (n - 1) xs in
    (x::a, b)
  else
    ([], l)

