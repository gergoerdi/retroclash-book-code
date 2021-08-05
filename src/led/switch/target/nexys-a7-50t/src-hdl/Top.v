module Top(input [7:0]  SW,
           output [7:0] LED
           );
   
   topEntity u_topEntity
     (.SWITCHES(SW),
      .LEDS(LED)
      );
   
endmodule
