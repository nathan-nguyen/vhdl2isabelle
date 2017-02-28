architecture rtl of iu3 is

  function plus(a:integer; b: integer) return integer is
  begin
    return a+b;
  end function plus;

  function return_constant(a:integer) return integer is
  begin
    return a;
  end function return_constant;

  function plust(a:integer) return integer is
  variable tmp: integer;
  begin
    tmp := plus(a, 1) + return_constant(a);
    return plus(a, 1)+1;
  end function plust;

begin
end;

entity iu3 is

end;
