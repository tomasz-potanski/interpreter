// Program prezentuje implementacje instrukcji warunkowej: if
begin

	if (1 < 2) then print <- ("OK") ; endif

	if (2 < 3) then print <- ("OK"); else print <- ("ZLE") ; endif

	if (3 < 2) then print <- ("ZLE"); elif (1 < 2) then print <- ("OK") ; endif

	if (3 < 2) then print <-("ZLE"); elif (2 < 1) then print <- ("ZLE"); else print <- ("OK") ; endif
end.