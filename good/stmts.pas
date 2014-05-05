/*
	Program prezentuje podstawowe funkcjonalnosci na 8 punktów np.
	+ instrukcje warunkowe: if
	+ petla while
	+ zmienne i operacje na zmiennych

	Dodatkowo:
	+ drukowanie na ekran
	
*/

var
	x : Integer;
	y : Integer;
	z : Integer;
	n : Integer;
	counter : Integer;
	dwa : Integer;
	trzy : Integer;
	cztery : Integer;
	dziesiec : Integer;
	i : Integer;

begin
	x := 3 + 2 ;
	x := x + 1 ;
	y := x + 3 ;
	z := 1 ;
	if z < 2 then print "OK" endif;
	if z < 3 then print z endif;
	print 'a';
	
	n := 10 ;
	counter := 0 ;
	i := 0 ;
	while i < n do
	begin
		i := i + 1 ;
		counter := counter + 1;
		counter := counter + 1;
	end

	dwa := 8 / 4;
	trzy := 4 - 1;
	cztery := 1 + 3;
	dziesiec := 2 * 5;

end.