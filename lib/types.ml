type shift = L | R
type rot = CCW | CW
type drop = Soft | Hard
type move = Shift of shift | Rot of rot | Drop of drop | Hold | Quit

type pip = {
  i:            int;
  loc:          int*int;
  r:            int;
  ghost:        int;
}

type state = {
  score:        int;
  board:        int array array;
  pip:          pip option;
  hold:         int option;
  queue:        int list;
  bag:          int list;
}

type cfg = {
  width:        int;
  height:       int;
  scale:        int;
  margin_side:  int;
  margin_top:   int;
  colors:       Graphics.color array;
  piece_rots:   (int*int) list list list;
  offsets:      (int*int) array array array;
}
