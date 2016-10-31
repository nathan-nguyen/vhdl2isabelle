  stimulus : process
    begin
      idbits_out <= IDBITS - 1;
    end process stimulus;

  procedure change_tbuf(tracebuf_2p: in boolean; tbuf: out integer; tbuf_in: in integer) is
    begin
      if (TRACEBUF_2P) then
        tbuf := (tbuf-64);
      else
        tbuf := tbuf_in;
      end if;
    end procedure change_tbuf;

  function get_tbuf(tracebuf_2p: boolean; tbuf: integer) return integer is
    begin
      change_tbuf(true, tbuf, tbuf);
      if (TRACEBUF_2P) then
        return(tbuf-64);
      else
        return(tbuf);
      end if;
    end function get_tbuf;
