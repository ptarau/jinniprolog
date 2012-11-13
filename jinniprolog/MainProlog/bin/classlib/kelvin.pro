:-[celsius].

kelvin:-celsius.

kelvin(T):-celsius(T).

convert(ToCelsius):-
  temperature=>Tk,
  ToCelsius is Tk-273.16.
  
show_temperature(ToCelsius):-
  println(celsius=ToCelsius).

 