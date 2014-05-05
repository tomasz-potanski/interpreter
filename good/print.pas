//
//	Prezentacja działania instrukcji wypisywania
//

var
	x : Integer ;
	y : String;
	z : Boolean;
	a : Array[1..10] of Integer;
	b : Array[1..10] of String;
	c : Array[1..10] of Boolean;

begin
	print <- ("Ala ma kota!");
	print <- (True);
	print <- (False);
	print <- (2<3);
	x := 0;
	y := "... ale Ola jest ladniejsza ;)";
	z := True;
	print <-- (x);
	print <-- (y);
	print <-- (z);

	a[1] := 0;
	b[1] := "... ale Ola jest ladniejsza ;)";
	c[1] := 2 < 3;
	print <-- (a[1]);
	print <-- (b[1]);
	print <-- (c[1]);

	x := 1;
	print <-- (x);

	x := 2;
	print <-- (x);
	print <- (x+2);
	print <- (3*7);
	print <- (False);


end.
