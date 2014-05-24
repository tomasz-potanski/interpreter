var
	a : Integer;
proc
	function druk( co : Integer;) : Void;
	begin
		function drukx2(co2 : Integer; ) : Void;
		begin
			co2:=2*co2;
			print <-- (co2);
		end;
		print <--(co);
		drukx2(co);
	end

begin
	a := 10;
	druk(a);
end.