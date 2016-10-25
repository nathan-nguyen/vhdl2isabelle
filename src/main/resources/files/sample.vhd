entity iu3 is
  generic (
    nwin     : integer range 2 to 32 := 8;
    isets    : integer range 1 to 4 := 1;
    dsets    : integer range 1 to 4 := 1;
    fpu      : integer range 0 to 15 := 0;
    v8       : integer range 0 to 63 := 0;
    cp, mac  : integer range 0 to 1 := 0;
    dsu      : integer range 0 to 1 := 0;
    nwp      : integer range 0 to 4 := 0;
    pclow    : integer range 0 to 2 := 2;
    notag    : integer range 0 to 1 := 0;
    index    : integer range 0 to 15:= 0;
    lddel    : integer range 1 to 2 := 2;
    irfwt    : integer range 0 to 1 := 0;
    disas    : integer range 0 to 2 := 0;
    tbuf     : integer range 0 to 128 := 0;     -- trace buf size in kB (0 - no trace buffer)
    pwd      : integer range 0 to 2 := 0;       -- power-down
    svt      : integer range 0 to 1 := 0;       -- single-vector trapping
    rstaddr  : integer := 16#00000#;            -- reset vector MSB address
    smp      : integer range 0 to 15 := 0;      -- support SMP systems
    fabtech  : integer range 0 to NTECH := 0;
    clk2x    : integer := 0;
    bp       : integer range 0 to 2 := 1;
    npasi    : integer range 0 to 1 := 0;
    pwrpsr   : integer range 0 to 1  := 0
  );
  port (
    clk   : in  std_ulogic;
    rstn  : in  std_ulogic;
    holdn : in  std_ulogic;
    ici   : out icache_in_type;
    ico   : in  icache_out_type;
    --dci   : out dcache_in_type;
    --dco   : in  dcache_out_type;
    --rfi   : out iregfile_in_type;
    --rfo   : in  iregfile_out_type;
    --irqi  : in  l3_irq_in_type;
    --irqo  : out l3_irq_out_type;
    --dbgi  : in  l3_debug_in_type;
    --dbgo  : out l3_debug_out_type;
    --muli  : out mul32_in_type;
    --mulo  : in  mul32_out_type;
    --divi  : out div32_in_type;
    --divo  : in  div32_out_type;
    --fpo   : in  fpc_out_type;
    --fpi   : out fpc_in_type;
    --cpo   : in  fpc_out_type;
    --cpi   : out fpc_in_type;
    --tbo   : in  tracebuf_out_type;
    --tbi   : out tracebuf_in_type;
    --tbo_2p : in  tracebuf_2p_out_type;
    --tbi_2p : out tracebuf_2p_in_type;
    sclk   : in  std_ulogic
    );


  attribute sync_set_reset of rstn : signal is "true";
end;

architecture rtl of iu3 is

  procedure change_tbuf(tracebuf_2p: in boolean; tbuf: out integer; tbuf_in: in integer) is
    begin
      if (TRACEBUF_2P) then
        tbuf := (tbuf-64);
      else
        tbuf := tbuf_in;
      end if;
    end procedure change_tbuf;


  --CONSTANT addr : std_logic_vector(9 downto 0) := (others => '0');

  function get_tbuf(tracebuf_2p: boolean; tbuf: integer) return integer is
    begin
      change_tbuf(true, tbuf, tbuf);
      if (TRACEBUF_2P) then
        return(tbuf-64);
      else
        return(tbuf);
      end if;
    end function get_tbuf;


  --constant functionCallConst : integer := 1 + get_tbuf(true, tbuf);


begin
end;