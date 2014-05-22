var
	f : Function ( Integer) : Integer;
	global : Integer;

proc
	function drukuj ( aa : Integer;) : Integer ;
	begin
		print <-- (aa);
	end

	procedure wywolaj (g : Function (Integer) : Integer;);
	begin
		g(global+1);
	end

begin
	global := 8;
	
end.