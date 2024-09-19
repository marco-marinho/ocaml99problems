let get_range start stop =
  let rec dec acc curr =
    if curr < stop then acc else dec (curr :: acc) (curr - 1)
  in
  let rec inc acc curr =
    if curr > stop then acc else inc (curr :: acc) (curr + 1)
  in
  if start < stop then inc [] start else dec [] start
