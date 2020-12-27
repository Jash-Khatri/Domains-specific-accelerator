package tb_vectoradd;

import vectorAddition:: *;


(* synthesize *)
module mkTest (Empty);
	
	
	VectorAdder_ifc v <- mkVectorAdder;
	Reg#(int) step <- mkReg(0);

   	Reg#(Int#(CBDATASIZE)) x <- mkReg(0);
	Reg#(Int#(CBDATASIZE)) y <- mkReg(0);
	Reg#(Int#(CBDATASIZE)) z <- mkReg(0);
	Reg#(Int#(CBDATASIZE)) w <- mkReg(0);
		
	// here there should be the rule to initailze the memory

	rule step0(step == 0);
	   v.mem_store(50,-600);
	   step <= 1;
	endrule	

	rule step1(step == 1);		
	   v.mem_store(51,3);
   	   step <= 2;
	endrule

	rule step2(step == 2);	
    	   v.mem_store(52,2);
	   step <= 3;
	endrule

	rule step3(step == 3);
	   v.mem_store(60,-210);
       	   step <= 4;
	endrule
				
	  rule step4(step == 4);
	   v.mem_store(61, 10);
       	   step <= 5;
	endrule

	   rule step5(step == 5);
	   v.mem_store(62,-1);
       	   step <= 6;
	endrule
	
	rule step6(step == 6);
	   v.mem_store(63,800);
       	   step <= 7;
	endrule

	rule step7(step == 7);
	   v.mem_store(53,-2000);
       	   step <= 8;
	endrule
		
	// run the accelerator..
	rule rl_go(step == 8);
	
	v.put_src1 (50);
	v.put_src2 (60);
	v.put_dst (110);
	v.put_bsize (4);
	step <= 9;
	endrule


	// get the results and print them

	rule rl_getres1(step == 9 && v.iscomplete() );
	x <= unpack(v.mem_load( 110 ));
	step <= 10;
	endrule

	rule rl_getres2(step == 10 && v.iscomplete() );
	y <= unpack(v.mem_load( 111 ));
	step <= 11;
	endrule
	
	rule rl_getre3(step == 11 && v.iscomplete() );
	z <= unpack(v.mem_load( 112 ));
	step <= 12;
	endrule

	rule rl_getre4(step == 12 && v.iscomplete() );
	w <= unpack(v.mem_load( 113 ));
	step <= 13;
	endrule
	
	
	rule rl_getres4(step == 13 && v.iscomplete() );
	$display ("The final vector: [%d %d %d %d]",x,y,z,w);
	$display("\n\n");
	$finish();
	endrule
	

endmodule

endpackage
