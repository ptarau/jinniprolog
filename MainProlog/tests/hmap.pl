hmaps_init:-
  val(hmaps_,ctr_,MaxH),
  for(H,1,MaxH),
  hmap_clean(H),
  fail.
hmaps_init:-
  let(hmaps_,ctr_,0).

hmap_clean(H):-
  HX is 0-H,
  val(hpos_,H,MaxI),
  for(I,1,MaxI),
    val(HX,I,K),
    hmap_rm(H,K),
  fail.
hmap_clean(H):-
  let(hpos_,H,0).

hmap_new(H):-
  var(H),
  val(hmap_,ctr_,N),
  H is N+1,
  set(hmap_,ctr_,H),
  let(hpos_,H,0).

hmap_put(H,K,V):-
  HX is 0-H,
  val(H,K,V),
  !,
  val(hpos_,H,N1),
  def(HX,N1,K).
hmap_put(H,K,V):-
  HX is 0-H,
  def(H,K,V),
  val(hpos_,H,N),
  N1 is N+1,
  set(hpos_,H,N1),
  def(HX,N1,K).

hmap_get(H,K,V):-
  val(H,K,V).

hmap_rm(H,K):-
  rm(H,K).