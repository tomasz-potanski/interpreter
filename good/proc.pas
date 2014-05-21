var
	globA : Integer ;
	globB : Integer;
	x : Integer;
proc
	procedure moja ( ) ;
	var
		locX : Integer ;
	begin
		moja2 ( ) ;
	end

	procedure moja3 ( ) ;
	var
		locX : Integer ;
	begin
		print <-- (globB);
		++globB;
	end

	procedure moja2 ( ) ;
	var
		locY : Integer ;
	begin
		print <- ("Dzialam!") ;
		if (globA == 0) then 
		begin		
			globA:= 1;
			moja2();
		end
		endif
	end
	procedure wart ( zm : Integer; ) ;
	var
		locX : Integer ;
	begin
		print <-- (zm);
	end
begin
	print <- ("+++++=!") ;
	moja();
	globA := 0;
	moja();
	x := 10;
	while (x > 0) do begin
		--x;
		moja3();
	end
	wart(999);
end.