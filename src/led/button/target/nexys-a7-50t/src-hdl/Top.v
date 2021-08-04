module Top(input        BTNC,
           input        BTNL,
           input        BTNR,
           output [2:0] LED
           );

   topEntity u_topEntity
     (.BTNL(BTNL),
      .BTNC(BTNC),
      .BTNR(BTNR),
      .LEDS(LED[2:0])
      );
   
endmodule
