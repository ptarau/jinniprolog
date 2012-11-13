:-[comb].

rshow(T):-val(show,tool,prolog3d),!,rli_call(localhost,prolog3d,gshow(T),_).
%rshow(T):-val(show,tool,prolog3d),!,prolog3d:show_term(T,30).
rshow(T):-T\=='$null',rli_call(localhost,renoir,rshow(T),_).
  
h(N):-int2ffs(N,T),println(T),rshow(T).
dh(N):-int2ffs(N,T),term_to_cat(T,C),dualize(C),rshow(C).

% ?-hx("10010010").
hx(B0):-bits_to_bigint(B0,B),hcomplexity(B,R),println(complexity=R),big2hcat(1,B,C),rshow(C).

gx(B0):-bits_to_bigint(B0,B),gcomplexity(B,R),println(complexity=R),big2hcat(1,B,C),rshow(C).

% ?-hd("001101000","00001111").
hd(A0,B0):-
   bits_to_bigint(A0,A),
   bits_to_bigint(B0,B),
   hdistance(A,B,HD),
   gdistance(A,B,GD),
   sdistance(A,B,SD),
   println(distances(hd(HD),gd(GD),sd(SD))).

% ?-bh(3,"10011010").   
bh(N,B0):-
   \+integer(B0),
   bits_to_bigint(B0,B),
   heval(N,B,R),write_codes("heval="),bigint_print(R),nl,
   neval(B,RR),write_codes("neval="),bigint_print(RR),nl,
   big2hcat(N,B,C),rshow(C),
   sleep(10),
   dualize(C),rshow(C).
   
dbh(N,B0):-
  bits_to_bigint(B0,B),
  big2hcat(N,B,C),
  dualize(C),
  rshow(C).

%?- bh(555).
bh(B0):-to_bigint(B0,B),big2hcat(B,C),rshow(C).
dbh(B0):-to_bigint(B0,B),big2hcat(B,C),dualize(C),rshow(C).

% ?-ur(333).
ur(N):-int2ur(N,1,T),println(T),rshow(T).
dur(N):-int2ur(N,1,T),term_to_cat(T,C),dualize(C),rshow(C).

u(N):-u(N,1).
du(N):-du(N,1).

u(N,U):-int2urcat(N,U,C),rshow(C).
du(N,U):-int2urcat(N,U,C),dualize(C),rshow(C).

v(B:N):-int2vurcat(B:N,C),rshow(C).
dv(B:N):-int2vurcat(B:N,C),dualize(C),rshow(C).

c(N):-cantor_tree(N,T),println(T),rshow(T).
dc(N):-cantor_tree(N,T),term_to_cat(T,C),dualize(C),rshow(C).

% usable also for BDDs - shannon expansion
bt(N):-bitmix_tree(N,T),println(T),rshow(T).
dbt(N):-bitmix_tree(N,T),term_to_cat(T,C),dualize(C),rshow(C).

b(N):-int2bxcat(N,C),rshow(C).
db(N):-int2bxcat(N,C),dualize(C),rshow(C).

sg(N):-int2graph(N,C),rshow(C).
dsg(N):-int2graph(N,C),dualize(C),rshow(C).

g(N):-to_bigint(N,B),bigint2igraph(B,C),rshow(C).
dg(N):-to_bigint(N,B),bigint2igraph(B,C),dualize(C),rshow(C).

i(N):-to_bigint(N,B),bigint2cat(4,2,B,C),rshow(C).

tu(N):-to_bigint(N,B),bigint2cat(3,B,C),rshow(C).

rg:-rg(64).

rg(Bits):-BR is_bigint random(Bits),g(BR).
 
p(N):-unpair_tree(3,N,T),rshow(T).

t(N):-t(3,N).
t(Bits,N):-int2ncat(Bits,N,C),rshow(C).

dt(N):-dt(3,N).
dt(Bits,N):-int2ncat(Bits,N,C),dualize(C),rshow(C).

all:-
  B=4,
  random(X),N is abs(2+X mod 1000),
  println(showing(b=B,n=N)),
  v(B:N),sleep(2),
  dv(B:N),sleep(2),
  h(N),sleep(2),
  dh(N),sleep(2),
  ur(N),sleep(2),
  dur(N),sleep(2),
  c(N),sleep(2),
  bt(N),sleep(2),
  b(N),sleep(2),
  db(N),sleep(2),
  g(N),sleep(2),
  dg(N),sleep(2).
  
  
% end
  