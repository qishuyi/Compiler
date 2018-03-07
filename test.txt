let fact = fix f n ->
  if n > 0 then
    n * f (n - 1)
  else
    1
in
  fact 5
