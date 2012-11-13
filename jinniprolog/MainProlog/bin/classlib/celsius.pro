:-[fahrenheit].

celsius:-fahrenheit.

celsius(T):-fahrenheit(T).

convert(ToFarenheit):-
  temperature=>Tc,
  ToFarenheit is (9/5)*Tc+32 .

convert:-
  convert(ToFarenheit),
  show_temperature(ToFarenheit).
  
show_temperature(ToFarenheit):-
  println(fahrenheit=ToFarenheit).

  