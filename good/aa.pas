var
	tab : Array[0..10] of Integer;

proc
	function nn ( ss : Array[0..10] of Integer; ) : Void;
	begin
		print <-- (ss[1]);
		ss[1] := 4;
		
	end

begin

	tab[1] := 9;
	nn(tab);
	print <-- (tab[1]);
end.