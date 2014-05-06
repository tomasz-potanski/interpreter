var
	b : Array[1..10] of Boolean;
begin
	//if True then print "OK"; endif
	if 2+2 then print <- ("OK"); endif
	b[1] := 2 > 1;
	if b[1] then print <- ("OK"); endif
end.