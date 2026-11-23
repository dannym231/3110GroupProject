The plain dump covers the tour without reading the keyboard.

  $ monopoly --dump > tour.txt
  $ grep -Fq 'Log: Alex to roll.' tour.txt
  $ grep -Fq 'Log: Alex bought Baltic Avenue for $60.' tour.txt
  $ grep -Fq 'Log: Alex passed on Baltic Avenue.' tour.txt
  $ grep -Fq 'Blake owes Alex $4 rent for Baltic Avenue.' tour.txt
  $ grep -Fq 'Advance to Go. Collect $200.' tour.txt
  $ grep -Fq 'Drew is in jail. 2 turns left. Fine $50.' tour.txt
  $ grep -Fq 'preview only, no socket opened' tour.txt
  $ grep -Fq 'Casey is bankrupt: owes $200 tax' tour.txt
  $ grep -Fqx 'Alex wins' tour.txt
  $ grep -c "$(printf '\033')" tour.txt
  0
  [1]

A missing or non-numeric seed is rejected before the game starts.

  $ monopoly --seed
  usage: monopoly [--dump | --tour | --seed N] [name ...]
  [2]

  $ monopoly --seed nope
  usage: monopoly [--dump | --tour | --seed N] [name ...]
  [2]
