module Top(input CLK_100MHZ,
           input [7:0]  SW,
           output [7:0] LED
           );
   
   topEntity u_topEntity
     (.CLK(CLK_100MHZ),
      .SWITCHES(SW),
      .LEDS(LED)
      );
   
endmodule
