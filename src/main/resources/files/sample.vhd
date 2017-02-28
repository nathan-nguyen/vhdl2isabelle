------------------------------------------------------------------------------
--  This file is a part of the GRLIB VHDL IP LIBRARY
--  Copyright (C) 2003 - 2008, Gaisler Research
--  Copyright (C) 2008 - 2014, Aeroflex Gaisler
--  Copyright (C) 2015, Cobham Gaisler
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-----------------------------------------------------------------------------
-- Entity:      iu3
-- File:        iu3.vhd
-- Author:      Jiri Gaisler, Edvin Catovic, Gaisler Research
-- Description: LEON3 7-stage integer pipline
------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library grlib;
use grlib.config_types.all;
use grlib.config.all;
use grlib.sparc.all;
use grlib.stdlib.all;
library techmap;
use techmap.gencomp.all;
library gaisler;
use gaisler.leon3.all;
use gaisler.libiu.all;
use gaisler.libfpu.all;
use gaisler.arith.all;
-- pragma translate_off
use grlib.sparc_disas.all;
-- pragma translate_on

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
    dci   : out dcache_in_type;
    dco   : in  dcache_out_type;
    rfi   : out iregfile_in_type;
    rfo   : in  iregfile_out_type;
    irqi  : in  l3_irq_in_type;
    irqo  : out l3_irq_out_type;
    dbgi  : in  l3_debug_in_type;
    dbgo  : out l3_debug_out_type;
    muli  : out mul32_in_type;
    mulo  : in  mul32_out_type;
    divi  : out div32_in_type;
    divo  : in  div32_out_type;
    fpo   : in  fpc_out_type;
    fpi   : out fpc_in_type;
    cpo   : in  fpc_out_type;
    cpi   : out fpc_in_type;
    tbo   : in  tracebuf_out_type;
    tbi   : out tracebuf_in_type;
    tbo_2p : in  tracebuf_2p_out_type;
    tbi_2p : out tracebuf_2p_in_type;
    sclk   : in  std_ulogic
    );


  attribute sync_set_reset of rstn : signal is "true";
end;

architecture rtl of iu3 is

  function get_tbuf(tracebuf_2p: boolean; tbuf: integer) return integer is
  begin
    if (TRACEBUF_2P) then
        return(tbuf-64);
    else
        return(tbuf);
    end if;
  end function get_tbuf;

  constant ISETMSB : integer := log2x(isets)-1;
  constant DSETMSB : integer := log2x(dsets)-1;
  constant RFBITS : integer range 6 to 10 := log2(NWIN+1) + 4;
  constant NWINLOG2   : integer range 1 to 5 := log2(NWIN);
  constant CWPOPT : boolean := (NWIN = (2**NWINLOG2));
  constant CWPMIN : std_logic_vector(NWINLOG2-1 downto 0) := (others => '0');
--  constant CWPMAX : std_logic_vector(NWINLOG2-1 downto 0) :=                                                          -- HERE
--        conv_std_logic_vector(NWIN-1, NWINLOG2);                                                                      -- HERE
  constant FPEN   : boolean := (fpu /= 0);
  constant CPEN   : boolean := (cp = 1);
  constant MULEN  : boolean := (v8 /= 0);
  constant MULTYPE: integer := (v8 / 16);
  constant DIVEN  : boolean := (v8 /= 0);
  constant MACEN  : boolean := (mac = 1);
  constant MACPIPE: boolean := (mac = 1) and (v8/2 = 1);
  constant IMPL   : integer := 15;
  constant VER    : integer := 3;
  constant DBGUNIT : boolean := (dsu = 1);
  constant TRACEBUF    : boolean := (tbuf /= 0);
  constant TRACEBUF_2P : boolean := (tbuf > 64);
  constant TBUFBITS : integer := 6;                                                                                     --  constant TBUFBITS : integer := 10 + log2(get_tbuf(TRACEBUF_2P, tbuf)) - 4;
  constant PWRD1  : boolean := false; --(pwd = 1) and not (index /= 0);
  constant PWRD2  : boolean := (pwd /= 0); --(pwd = 2) or (index /= 0);
  constant RS1OPT : boolean := (is_fpga(FABTECH) /= 0);
  constant DYNRST : boolean := (rstaddr = 16#FFFFF#);

  constant CASAEN : boolean := (notag = 0) and (lddel = 1);
  signal BPRED : std_logic;
  signal BLOCKBPMISS: std_logic;

  subtype word is std_logic_vector(31 downto 0);
  subtype pctype is std_logic_vector(31 downto PCLOW);
  subtype rfatype is std_logic_vector(RFBITS-1 downto 0);
  subtype cwptype is std_logic_vector(NWINLOG2-1 downto 0);
  type icdtype is array (0 to isets-1) of word;
  type dcdtype is array (0 to dsets-1) of word;


  type dc_in_type is record
    signed, enaddr, read, write, lock, dsuen : std_ulogic;
    size : std_logic_vector(1 downto 0);
    asi  : std_logic_vector(7 downto 0);
  end record;

  type pipeline_ctrl_type is record
    pc    : pctype;
    inst  : word;
    cnt   : std_logic_vector(1 downto 0);
    rd    : rfatype;
    tt    : std_logic_vector(5 downto 0);
    trap  : std_ulogic;
    annul : std_ulogic;
    wreg  : std_ulogic;
    wicc  : std_ulogic;
    wy    : std_ulogic;
    ld    : std_ulogic;
    pv    : std_ulogic;
    rett  : std_ulogic;
  end record;

  type fetch_reg_type is record
    pc     : pctype;
    branch : std_ulogic;
  end record;

  type decode_reg_type is record
    pc    : pctype;
    inst  : icdtype;
    cwp   : cwptype;
    set   : std_logic_vector(ISETMSB downto 0);
    mexc  : std_ulogic;
    cnt   : std_logic_vector(1 downto 0);
    pv    : std_ulogic;
    annul : std_ulogic;
    inull : std_ulogic;
    step  : std_ulogic;
    divrdy: std_ulogic;
    pcheld: std_ulogic;
  end record;

  type regacc_reg_type is record
    ctrl  : pipeline_ctrl_type;
    rs1   : std_logic_vector(4 downto 0);
    rfa1, rfa2 : rfatype;
    rsel1, rsel2 : std_logic_vector(2 downto 0);
    rfe1, rfe2 : std_ulogic;
    cwp   : cwptype;
    imm   : word;
    ldcheck1 : std_ulogic;
    ldcheck2 : std_ulogic;
    ldchkra : std_ulogic;
    ldchkex : std_ulogic;
    su : std_ulogic;
    et : std_ulogic;
    wovf : std_ulogic;
    wunf : std_ulogic;
    ticc : std_ulogic;
    jmpl : std_ulogic;
    step  : std_ulogic;
    mulstart : std_ulogic;
    divstart : std_ulogic;
    bp, nobp : std_ulogic;
    bpimiss : std_ulogic;
  end record;

  type execute_reg_type is record
    ctrl   : pipeline_ctrl_type;
    op1    : word;
    op2    : word;
    aluop  : std_logic_vector(2 downto 0);      -- Alu operation
    alusel : std_logic_vector(1 downto 0);      -- Alu result select
    aluadd : std_ulogic;
    alucin : std_ulogic;
    ldbp1, ldbp2 : std_ulogic;
    invop2 : std_ulogic;
    shcnt  : std_logic_vector(4 downto 0);      -- shift count
    sari   : std_ulogic;                                -- shift msb
    shleft : std_ulogic;                                -- shift left/right
    ymsb   : std_ulogic;                                -- shift left/right
    rd     : std_logic_vector(4 downto 0);
    jmpl   : std_ulogic;
    su     : std_ulogic;
    et     : std_ulogic;
    cwp    : cwptype;
    icc    : std_logic_vector(3 downto 0);
    mulstep: std_ulogic;
    mul    : std_ulogic;
    mac    : std_ulogic;
    bp     : std_ulogic;
    rfe1, rfe2 : std_ulogic;
  end record;

  type memory_reg_type is record
    ctrl   : pipeline_ctrl_type;
    result : word;
    y      : word;
    icc    : std_logic_vector(3 downto 0);
    nalign : std_ulogic;
    dci    : dc_in_type;
    werr   : std_ulogic;
    wcwp   : std_ulogic;
    irqen  : std_ulogic;
    irqen2 : std_ulogic;
    mac    : std_ulogic;
    divz   : std_ulogic;
    su     : std_ulogic;
    mul    : std_ulogic;
    casa   : std_ulogic;
    casaz  : std_ulogic;
  end record;

  type exception_state is (run, trap, dsu1, dsu2);

  type exception_reg_type is record
    ctrl   : pipeline_ctrl_type;
    result : word;
    y      : word;
    icc    : std_logic_vector( 3 downto 0);
    annul_all : std_ulogic;
    data   : dcdtype;
    set    : std_logic_vector(DSETMSB downto 0);
    mexc   : std_ulogic;
    dci    : dc_in_type;
    laddr  : std_logic_vector(1 downto 0);
    rstate : exception_state;
    npc    : std_logic_vector(2 downto 0);
    intack : std_ulogic;
    ipend  : std_ulogic;
    mac    : std_ulogic;
    debug  : std_ulogic;
    nerror : std_ulogic;
    ipmask : std_ulogic;
  end record;

  type dsu_registers is record
    tt      : std_logic_vector(7 downto 0);
    err     : std_ulogic;
    tbufcnt : std_logic_vector(TBUFBITS-1 downto 0);
    asi     : std_logic_vector(7 downto 0);
    crdy    : std_logic_vector(2 downto 1);  -- diag cache access ready
    tfilt   : std_logic_vector(3 downto 0);  -- trace filter
    cfc     : std_logic_vector(6 downto 0);  -- control-flow change
    tlim    : std_logic_vector(2 downto 0);
    tov     : std_ulogic;
    tovb    : std_ulogic;
  end record;

  type irestart_register is record
    addr   : pctype;
    pwd    : std_ulogic;
  end record;


  type pwd_register_type is record
    pwd    : std_ulogic;
    error  : std_ulogic;
  end record;

  type special_register_type is record
    cwp    : cwptype;                                -- current window pointer
    icc    : std_logic_vector(3 downto 0);        -- integer condition codes
    tt     : std_logic_vector(7 downto 0);        -- trap type
    tba    : std_logic_vector(19 downto 0);       -- trap base address
    wim    : std_logic_vector(NWIN-1 downto 0);       -- window invalid mask
    pil    : std_logic_vector(3 downto 0);        -- processor interrupt level
    ec     : std_ulogic;                                  -- enable CP
    ef     : std_ulogic;                                  -- enable FP
    ps     : std_ulogic;                                  -- previous supervisor flag
    s      : std_ulogic;                                  -- supervisor flag
    et     : std_ulogic;                                  -- enable traps
    y      : word;
    asr18  : word;
    svt    : std_ulogic;                                  -- enable traps
    dwt    : std_ulogic;                           -- disable write error trap
    dbp    : std_ulogic;                           -- disable branch prediction
    dbprepl: std_ulogic;                -- Disable speculative Icache miss/replacement
  end record;

  type write_reg_type is record
    s      : special_register_type;
    result : word;
    wa     : rfatype;
    wreg   : std_ulogic;
    except : std_ulogic;
  end record;

  type registers is record
    f  : fetch_reg_type;
    d  : decode_reg_type;
    a  : regacc_reg_type;
    e  : execute_reg_type;
    m  : memory_reg_type;
    x  : exception_reg_type;
    w  : write_reg_type;
  end record;

  type exception_type is record
    pri   : std_ulogic;
    ill   : std_ulogic;
    fpdis : std_ulogic;
    cpdis : std_ulogic;
    wovf  : std_ulogic;
    wunf  : std_ulogic;
    ticc  : std_ulogic;
  end record;

  type watchpoint_register is record
    addr    : std_logic_vector(31 downto 2);  -- watchpoint address
    mask    : std_logic_vector(31 downto 2);  -- watchpoint mask
    exec    : std_ulogic;                           -- trap on instruction
    load    : std_ulogic;                           -- trap on load
    store   : std_ulogic;                           -- trap on store
  end record;

  type watchpoint_registers is array (0 to 3) of watchpoint_register;

  function dbgexc(
    r     : registers; dbgi : l3_debug_in_type;
    trap  : std_ulogic;
    tt    : std_logic_vector(7 downto 0);
    dsur  : dsu_registers) return std_ulogic is
    variable dmode : std_ulogic;
  begin
    dmode := '0';
    if (not r.x.ctrl.annul and trap) = '1' then
      if (((tt = "00" & TT_WATCH) and (dbgi.bwatch = '1')) or
          ((dbgi.bsoft = '1') and (tt = "10000001")) or
          (dbgi.btrapa = '1') or
          ((dbgi.btrape = '1') and not ((tt(5 downto 0) = TT_PRIV) or
            (tt(5 downto 0) = TT_FPDIS) or (tt(5 downto 0) = TT_WINOF) or
            (tt(5 downto 0) = TT_WINUF) or (tt(5 downto 4) = "01") or (tt(7) = '1'))) or
          (((not r.w.s.et) and dbgi.berror) = '1')) then
        dmode := '1';
      end if;
    end if;
    return(dmode);
  end;

  function dbgerr(r : registers; dbgi : l3_debug_in_type;
                  tt : std_logic_vector(7 downto 0))
  return std_ulogic is
    variable err : std_ulogic;
  begin
    err := not r.w.s.et;
    if (((dbgi.dbreak = '1') and (tt = ("00" & TT_WATCH))) or
        ((dbgi.bsoft = '1') and (tt = ("10000001")))) then
      err := '0';
    end if;
    return(err);
  end;


  procedure diagwr(r    : in registers;
                   dsur : in dsu_registers;
                   ir   : in irestart_register;
                   dbg  : in l3_debug_in_type;
                   wpr  : in watchpoint_registers;
                   s    : out special_register_type;
                   vwpr : out watchpoint_registers;
                   asi : out std_logic_vector(7 downto 0);
                   pc, npc  : out pctype;
                   tbufcnt : out std_logic_vector(TBUFBITS-1 downto 0);
                   tfilt : out std_logic_vector(3 downto 0);
                   wr : out std_ulogic;
                   addr : out std_logic_vector(9 downto 0);
                   data : out word;
                   fpcwr : out std_ulogic) is
  variable i : integer range 0 to 3;
  begin
    s := r.w.s; pc := r.f.pc; npc := ir.addr; wr := '0';
    vwpr := wpr; asi := dsur.asi; addr := (others => '0');
    data := dbg.ddata;
    tbufcnt := dsur.tbufcnt; fpcwr := '0'; tfilt := dsur.tfilt;
      if (dbg.dsuen and dbg.denable and dbg.dwrite) = '1' then
        case dbg.daddr(23 downto 20) is
          when "0001" =>
            if (dbg.daddr(16) = '1' and dbg.daddr(2) = '0' ) and TRACEBUF then -- trace buffer control reg
              tbufcnt := dbg.ddata(TBUFBITS-1 downto 0);
              tfilt   := dbg.ddata(31 downto 28);
            end if;
          when "0011" => -- IU reg file
            if dbg.daddr(12) = '0' then
              wr := '1';
              addr := (others => '0');
              addr(RFBITS-1 downto 0) := dbg.daddr(RFBITS+1 downto 2);
            else  -- FPC
              fpcwr := '1';
            end if;
          when "0100" => -- IU special registers
            case dbg.daddr(7 downto 6) is
              when "00" => -- IU regs Y - TBUF ctrl reg
                case dbg.daddr(5 downto 2) is
                  when "0000" => -- Y
                    s.y := dbg.ddata;
                  when "0001" => -- PSR
                    s.cwp := dbg.ddata(NWINLOG2-1 downto 0);
                    s.icc := dbg.ddata(23 downto 20);
                    s.ec  := dbg.ddata(13);
                    if FPEN then s.ef := dbg.ddata(12); end if;
                    s.pil := dbg.ddata(11 downto 8);
                    s.s   := dbg.ddata(7);
                    s.ps  := dbg.ddata(6);
                    s.et  := dbg.ddata(5);
                  when "0010" => -- WIM
                    s.wim := dbg.ddata(NWIN-1 downto 0);
                  when "0011" => -- TBR
                    s.tba := dbg.ddata(31 downto 12);
                    s.tt  := dbg.ddata(11 downto 4);
                  when "0100" => -- PC
                    pc := dbg.ddata(31 downto PCLOW);
                  when "0101" => -- NPC
                    npc := dbg.ddata(31 downto PCLOW);
                  when "0110" => --FSR
                    fpcwr := '1';
                  when "0111" => --CFSR
                  when "1001" => -- ASI reg
                    asi := dbg.ddata(7 downto 0);
                  when others =>
                end case;
              when "01" => -- ASR16 - ASR31
                case dbg.daddr(5 downto 2) is
                when "0001" =>  -- %ASR17
                  if bp = 2 then s.dbp := dbg.ddata(27); end if;
                  if bp = 2 then s.dbprepl := dbg.ddata(25); end if;
                  s.dwt := dbg.ddata(14);
                  s.svt := dbg.ddata(13);
                when "0010" =>  -- %ASR18
                  if MACEN then s.asr18 := dbg.ddata; end if;
                when "1000" =>          -- %ASR24 - %ASR31
                  vwpr(0).addr := dbg.ddata(31 downto 2);
                  vwpr(0).exec := dbg.ddata(0);
                when "1001" =>
                  vwpr(0).mask := dbg.ddata(31 downto 2);
                  vwpr(0).load := dbg.ddata(1);
                  vwpr(0).store := dbg.ddata(0);
                when "1010" =>
                  vwpr(1).addr := dbg.ddata(31 downto 2);
                  vwpr(1).exec := dbg.ddata(0);
                when "1011" =>
                  vwpr(1).mask := dbg.ddata(31 downto 2);
                  vwpr(1).load := dbg.ddata(1);
                  vwpr(1).store := dbg.ddata(0);
                when "1100" =>
                  vwpr(2).addr := dbg.ddata(31 downto 2);
                  vwpr(2).exec := dbg.ddata(0);
                when "1101" =>
                  vwpr(2).mask := dbg.ddata(31 downto 2);
                  vwpr(2).load := dbg.ddata(1);
                  vwpr(2).store := dbg.ddata(0);
                when "1110" =>
                  vwpr(3).addr := dbg.ddata(31 downto 2);
                  vwpr(3).exec := dbg.ddata(0);
                when "1111" => --
                  vwpr(3).mask := dbg.ddata(31 downto 2);
                  vwpr(3).load := dbg.ddata(1);
                  vwpr(3).store := dbg.ddata(0);
                when others => --
                end case;
-- disabled due to bug in XST
--                  i := conv_integer(dbg.daddr(4 downto 3));
--                  if dbg.daddr(2) = '0' then
--                    vwpr(i).addr := dbg.ddata(31 downto 2);
--                    vwpr(i).exec := dbg.ddata(0);
--                  else
--                    vwpr(i).mask := dbg.ddata(31 downto 2);
--                    vwpr(i).load := dbg.ddata(1);
--                    vwpr(i).store := dbg.ddata(0);
--                  end if;
              when others =>
            end case;
          when others =>
        end case;
      end if;
  end;

  function asr17_gen ( r : in registers) return word is
  variable asr17 : word;
  variable fpu2 : integer range 0 to 3;
  begin
    asr17 := zero32;
    asr17(31 downto 28) := conv_std_logic_vector(index, 4);
    if bp = 2 then asr17(27) := r.w.s.dbp; end if;
    if notag = 0 then asr17(26) := '1'; end if; -- CASA and tagged arith
    if bp = 2 then asr17(25) := r.w.s.dbprepl; end if;
    if (clk2x > 8) then
      asr17(16 downto 15) := conv_std_logic_vector(clk2x-8, 2);
      asr17(17) := '1';
    elsif (clk2x > 0) then
      asr17(16 downto 15) := conv_std_logic_vector(clk2x, 2);
    end if;
    asr17(14) := r.w.s.dwt;
    if svt = 1 then asr17(13) := r.w.s.svt; end if;
    if lddel = 2 then asr17(12) := '1'; end if;
    if (fpu > 0) and (fpu < 8) then fpu2 := 1;
    elsif (fpu >= 8) and (fpu < 15) then fpu2 := 3;
    elsif fpu = 15 then fpu2 := 2;
    else fpu2 := 0; end if;
    asr17(11 downto 10) := conv_std_logic_vector(fpu2, 2);
    if mac = 1 then asr17(9) := '1'; end if;
    if v8 /= 0 then asr17(8) := '1'; end if;
    asr17(7 downto 5) := conv_std_logic_vector(nwp, 3);
    asr17(4 downto 0) := conv_std_logic_vector(nwin-1, 5);
    return(asr17);
  end;

  procedure diagread(dbgi   : in l3_debug_in_type;
                     r      : in registers;
                     dsur   : in dsu_registers;
                     ir     : in irestart_register;
                     wpr    : in watchpoint_registers;
                     dco    : in  dcache_out_type;
                     tbufo  : in tracebuf_out_type;
                     tbufo_2p : in tracebuf_2p_out_type;
                     data : out word) is
    variable cwp : std_logic_vector(4 downto 0);
    variable rd : std_logic_vector(4 downto 0);
    variable i : integer range 0 to 3;
  begin
    data := (others => '0'); cwp := (others => '0');
    cwp(NWINLOG2-1 downto 0) := r.w.s.cwp;
      case dbgi.daddr(22 downto 20) is
        when "001" => -- trace buffer
          if TRACEBUF then
            if dbgi.daddr(16) = '1' then -- trace buffer control reg
              if dbgi.daddr(2) = '0' then
                data(TBUFBITS-1 downto 0) := dsur.tbufcnt;
                data(31 downto 28) := dsur.tfilt;
              else
                data(23) := dsur.tov;
                data(26 downto 24) := dsur.tlim;
                data(27) := dsur.tovb;
              end if;
            else
              if TRACEBUF_2P then
                case dbgi.daddr(3 downto 2) is
                when "00" => data := tbufo_2p.data(127 downto 96);
                when "01" => data := tbufo_2p.data(95 downto 64);
                when "10" => data := tbufo_2p.data(63 downto 32);
                when others => data := tbufo_2p.data(31 downto 0);
                end case;
              else
                case dbgi.daddr(3 downto 2) is
                when "00" => data := tbufo.data(127 downto 96);
                when "01" => data := tbufo.data(95 downto 64);
                when "10" => data := tbufo.data(63 downto 32);
                when others => data := tbufo.data(31 downto 0);
                end case;
              end if;
            end if;
          end if;
        when "011" => -- IU reg file
          if dbgi.daddr(12) = '0' then
            if dbgi.daddr(11) = '0' then
                data := rfo.data1(31 downto 0);
              else data := rfo.data2(31 downto 0); end if;
          else
              data := fpo.dbg.data;
          end if;
        when "100" => -- IU regs
          case dbgi.daddr(7 downto 6) is
            when "00" => -- IU regs Y - TBUF ctrl reg
              case dbgi.daddr(5 downto 2) is
                when "0000" =>
                  data := r.w.s.y;
                when "0001" =>
                  data := conv_std_logic_vector(IMPL, 4) & conv_std_logic_vector(VER, 4) &
                          r.w.s.icc & "000000" & r.w.s.ec & r.w.s.ef & r.w.s.pil &
                          r.w.s.s & r.w.s.ps & r.w.s.et & cwp;
                when "0010" =>
                  data(NWIN-1 downto 0) := r.w.s.wim;
                when "0011" =>
                  data := r.w.s.tba & r.w.s.tt & "0000";
                when "0100" =>
                  data(31 downto PCLOW) := r.f.pc;
                when "0101" =>
                  data(31 downto PCLOW) := ir.addr;
                when "0110" => -- FSR
                  data := fpo.dbg.data;
                when "0111" => -- CPSR
                when "1000" => -- TT reg
                  data(12 downto 4) := dsur.err & dsur.tt;
                when "1001" => -- ASI reg
                  data(7 downto 0) := dsur.asi;
                when others =>
              end case;
            when "01" =>
              if dbgi.daddr(5) = '0' then
                if dbgi.daddr(4 downto 2) = "001" then -- %ASR17
                  data := asr17_gen(r);
                elsif MACEN and  dbgi.daddr(4 downto 2) = "010" then -- %ASR18
                  data := r.w.s.asr18;
                end if;
              else  -- %ASR24 - %ASR31
                i := conv_integer(dbgi.daddr(4 downto 3));                                           --
                if dbgi.daddr(2) = '0' then
                  data(31 downto 2) := wpr(i).addr;
                  data(0) := wpr(i).exec;
                else
                  data(31 downto 2) := wpr(i).mask;
                  data(1) := wpr(i).load;
                  data(0) := wpr(i).store;
                end if;
              end if;
            when others =>
          end case;
        when "111" =>
          data := r.x.data(conv_integer(r.x.set));
        when others =>
      end case;
  end;

  function itfilt (inst : word; filter : std_logic_vector(3 downto 0); trap, cfc : std_logic) return std_ulogic is
  variable tren : std_ulogic;
  begin
    tren := '0';
    case filter is
    when "0001" => 	-- Bicc, SETHI
      if inst(31 downto 30) = "00" then tren := '1'; end if;
    when "0010" => 	-- Control-flow change
      if (inst(31 downto 30) = "01") -- Call
      or ((inst(31 downto 30) = "00") and (inst(23 downto 22) /= "00")) --Bicc
--      or ((inst(31 downto 30) = "10") and (inst(24 downto 19) = JMPL)) --Jmpl
--      or ((inst(31 downto 30) = "10") and (inst(24 downto 19) = RETT)) --Rett
      or (trap = '1') or (cfc = '1')
      then tren := '1'; end if;
    when "0100" => 	-- Call
      if inst(31 downto 30) = "01" then tren := '1'; end if;
    when "1000" => 	-- Normal instructions
      if inst(31 downto 30) = "10" then tren := '1'; end if;
    when "1100" => 	-- LDST
      if inst(31 downto 30) = "11" then tren := '1'; end if;
    when "1101" =>      -- LDST from alternate space
      if inst(31 downto 30) = "11" and inst(24 downto 23) = "01" then tren := '1'; end if;
    when "1110" =>      -- LDST from alternate space 0x80 - 0xFF
      if inst(31 downto 30) = "11" and inst(24 downto 23) = "01" and inst(12) = '1' then tren := '1'; end if;
    when others => tren := '1';
    end case;
    return(tren);
  end;

begin

end;