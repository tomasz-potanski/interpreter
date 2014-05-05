//
//	Prezentacja działania instrukcji wypisywania
//

var
	x : Integer ;
	y : String;
	z : Boolean;

begin
	print <- ("Ala ma kota!");
	print <- (True);
	print <- (False);
	x := 0;
	y := "... ale Ola jest ładniejsza ;)";
	z := True;
	print <-- (x);
	print <-- (y);
	print <-- (z);

/*	x := 1;
	print <-- (x);

	x := 2;
	print <-- (x);
	print <- (x+2);
	print <- (3*7);
	print <- (False);
*/
end.
