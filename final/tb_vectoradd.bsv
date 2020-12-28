package tb_vectoradd;

import vectorAddition:: *;
import StmtFSM :: *;

(* synthesize *)
module mkTest (Empty);
	
	
	VectorAdder_ifc v <- mkVectorAdder;
	Reg#(int) step <- mkReg(0);

   	Reg#(Int#(CBDATASIZE)) x <- mkReg(0);
	Reg#(Int#(CBDATASIZE)) y <- mkReg(0);
	Reg#(Int#(CBDATASIZE)) z <- mkReg(0);
	Reg#(Int#(CBDATASIZE)) w <- mkReg(0);
		
	// rule to initailze the memory
	Stmt test_seq = seq

	   	v.mem_store(50,-600);		// initialize the mem where vector1 is present
	   	v.mem_store(51,3);
    	   	v.mem_store(52,2);
		v.mem_store(53,-2000);

	   	v.mem_store(60,-210);		// initialize the mem where vector2 is present
	   	v.mem_store(61, 10);
	   	v.mem_store(62,-1);
	   	v.mem_store(63,800);
	   	
      
		// run the accelerator..
		par
		v.put_src1 (50);		// give src1
		v.put_src2 (60);		// give src2
		v.put_dst (110);		// give dst
		v.put_bsize (4);		// give block-size
		step <= 1;	
		endpar
	endseq;	

	Stmt test_seq1 = seq
	   	x <= unpack(v.mem_load( 110 ));		// get the values stored from the dst 
		y <= unpack(v.mem_load( 111 ));
		z <= unpack(v.mem_load( 112 ));
		w <= unpack(v.mem_load( 113 ));
		step <= 2;

	endseq;	


	FSM testfsm <- mkFSM (test_seq);
	FSM testfsm1 <- mkFSM (test_seq1);

	rule run1 ( True );
		testfsm.start ;
	endrule

	rule run2 (v.iscomplete() && step == 1  );								
		testfsm1.start ;
	endrule
	
	rule rl_print(step == 2);
	$display ("The final vector:\n [ %d %d %d %d ]\n",x,y,z,w);		// print the results
	$display("\n\n");
	$finish();
	endrule
	
endmodule

endpackage
