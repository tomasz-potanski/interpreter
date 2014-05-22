var
	//f : Function ( Integer) : Integer;
	global : Integer;

proc
	procedure wart ( g : Function ( Integer) : Integer; ) ;
	begin
		g(global+1);
	end

	function drukuj ( aa : Integer ; ) : Integer;
	begin
		print <-- (global);
	end

begin
	global := 8;
	wart( drukuj );
	
end.