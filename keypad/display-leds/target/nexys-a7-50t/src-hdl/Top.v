module Top(
           input         CLK100MHZ,
           output [15:0] LED,
           inout [10:1]  JA
           );

   Keypad u_Keypad
     (.CLK(CLK100MHZ),
      .ROWS(JA[10:7]),
      .COLS(JA[4:1]),
      .LEDS(LED[15:0])
      );
   
endmodule
