:-[celsius].

fahrenheit:-
  temperature<=0.

fahrenheit(Temperature):-
  temperature<=Temperature.
  
convert(ToCelsius):-
  temperature=>Tf,
  ToCelsius is (5/9)*(Tf-32).

show_temperature(ToCelsius):-
  println(celsius=ToCelsius).
  