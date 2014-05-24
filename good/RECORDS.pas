var
	rec : 	Record
			tytul : String;
			cena : Integer;
			dostepna : Boolean;		
		RecEnd;

begin
	rec.cena := 55;
	print <-- (rec.cena);
	rec.dostepna := 1 < 3;
	print <-- (rec.dostepna);
	rec.tytul := "Podstawy hackowania";
	print <-- (rec.tytul);
end.