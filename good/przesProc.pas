var 
	glob : Integer;
proc
	procedure f( glob : Integer ; );
	begin
		print <--(glob);		
		glob := 20;
		print <--(glob);
	end

begin
	glob := 10;
	print <--(glob);
	f(15);
	print <--(glob);
end.