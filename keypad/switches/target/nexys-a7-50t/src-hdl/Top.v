module Top(
           output [3:0] LED,
           input [3:0]  SW,
           inout [10:1] JA
           );

   Keypad u_Keypad
     (.ROWS(JA[10:7]),
      .COLS(JA[4:1]),
      .SWITCHES(SW[3:0]),
      .LEDS(LED[3:0])
      );
   
endmodule
