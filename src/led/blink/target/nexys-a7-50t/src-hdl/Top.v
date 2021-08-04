module Top(
           input         CLK100MHZ,
           output [1:0] LED,
           );

   topEntity u_topEntity
     (.CLK(CLK100MHZ),
      .LED(LED[0])
      );
   
endmodule
