package vectorAddition;

import CBusMod::*;
import Vector :: *;	// Memory is defined as the vector

////////////////////////////////////////////////////////////////////////////////
///
//// Specific configuration bus size parameters
typedef 8 CBADDRSIZE; //size of configuration address bus to decode
typedef 8 CBDATASIZE; //size of configuration data bus 

typedef ModWithCBus#(CBADDRSIZE, CBDATASIZE, i) MyModWithCBus#(type i);
typedef CBus#(CBADDRSIZE, CBDATASIZE) MyCBus;

////////////////////////////////////////////////////////////////////////////////
///
// Defining all the register present in the accelerator here and there associated actions...

interface CSR1#(type size_t);		// control bit to interact between accelerator and CPU
   method Bool   isZero();
   method Action load(Bit#(size_t) newval);
endinterface

interface CSR2#(type size_t);		// control bit to interact between accelerator and CPU
   method Bool   isZero();
   method Action load(Bit#(size_t) newval);
endinterface

interface Src1Addr#(type size_t);	// control bits to interact between accelerator and CPU
   method Reg#(Bit#(size_t)) value();
   method Action increment();
   method Action load(Bit#(size_t) newval);     
endinterface

interface Src2Addr#(type size_t);	// control bits to interact between accelerator and CPU
   method Reg#(Bit#(size_t)) value();
   method Action increment();
   method Action load(Bit#(size_t) newval);    
endinterface

interface DstAddr#(type size_t);	// control bits to interact between accelerator and CPU
   method Reg#(Bit#(size_t)) value();
   method Action increment();
   method Action load(Bit#(size_t) newval);    
endinterface

interface Control_reg1#(type size_t);		// controls the access to the memory
   method Bool   isZero();
   method Action load(Bit#(size_t) newval);     
endinterface

interface Control_reg2#(type size_t);		// controls the access to the memory
   method Bool   isZero();   
   method Action load(Bit#(size_t) newval);     
endinterface

interface MAR1#(type size_t);			// memory address register1..
   method Reg#(Bit#(size_t)) value();
   method Action load(Bit#(size_t) newval);    
endinterface

interface MBR1#(type size_t);			// memory buffer register1..
   method Reg#(Bit#(size_t)) value();
   method Action load(Bit#(size_t) newval);    
endinterface

// define one more register attached to the acc that will store the blocksize//////////////////

// defining interface for the DMemory
interface DMem;
method (Bit#(CBDATASIZE)) load (Bit#(CBADDRSIZE) mar);
method Action store (Bit#(CBADDRSIZE) mar, Bit#(CBDATASIZE) mbr);
endinterface

// module for the DMem
module mkDMem(DMem);
	
	Vector #( 256, Reg#(Bit#(CBDATASIZE)) ) memory_array  <- replicateM (mkReg(10));
	Reg#(Bit#(CBDATASIZE)) r <- mkReg(0);

	method (Bit#(CBDATASIZE)) load (Bit#(CBADDRSIZE) mar);
		return memory_array[mar];
	endmethod

	method Action store (Bit#(CBADDRSIZE) mar, Bit#(CBDATASIZE) mbr);
		memory_array[mar] <= mbr;
	endmethod

endmodule

// DMem defination end..

// defining a CPU interface
interface CPU;
method (Bit#(CBDATASIZE)) returnflag();
method (Bit#(CBADDRSIZE)) returnsrc1addr();
method (Bit#(CBADDRSIZE)) returnsrc2addr();
method (Bit#(CBADDRSIZE)) returndstaddr();
endinterface

// module for CPU..
module mkCPU(CPU);
	Reg#(Bit#(CBDATASIZE)) flag <- mkReg(1);
	Reg#(Bit#(CBADDRSIZE)) src1 <- mkReg(60);
	Reg#(Bit#(CBADDRSIZE)) src2 <- mkReg(40);
	Reg#(Bit#(CBADDRSIZE)) dst <- mkReg(100);
	
	method (Bit#(CBDATASIZE)) returnflag();
		return flag;
	endmethod

	method (Bit#(CBADDRSIZE)) returnsrc1addr();
		return src1;
	endmethod

	method (Bit#(CBADDRSIZE)) returnsrc2addr();
		return src2;
	endmethod
	
	method (Bit#(CBADDRSIZE)) returndstaddr();
		return dst;
	endmethod

endmodule

// CPU interface ends here..

////////////////////////////////////////////////////////////////////////////////
///
// Exposing the accelerator's register to the CBus to they can be access by the CPU and MEMORY...

(* synthesize *)
module mkCSR1Synth(IWithCBus#(MyCBus, CSR1#(8)));
   let ifc();
   exposeCBusIFC#(mkCSR1) _temp(ifc);
   return (ifc);
endmodule

module [MyModWithCBus] mkCSR1(CSR1#(size_t))
   provisos(Add#(size_t, k, CBDATASIZE));
    
   Reg#(Bit#(size_t)) csr1 <- mkCBRegRW(10, 0);
   method Bool isZero();
      return (csr1 == 0);
   endmethod
   
   method Action load(Bit#(size_t) newval);
      csr1 <= newval;
   endmethod

endmodule

(* synthesize *)
module mkCSR2Synth(IWithCBus#(MyCBus, CSR2#(8)));
   let ifc1();
   exposeCBusIFC#(mkCSR2) _temp(ifc1);
   return (ifc1);
endmodule

module [MyModWithCBus] mkCSR2(CSR2#(size_t))
   provisos(Add#(size_t, k1, CBDATASIZE));
    
   Reg#(Bit#(size_t)) csr2 <- mkCBRegRW(11, 0);
   method Bool isZero();
      return (csr2 == 0);
   endmethod
   
   method Action load(Bit#(size_t) newval);
      csr2 <= newval;
   endmethod

endmodule

(* synthesize *)
module mkSrc1AddrSynth(IWithCBus#(MyCBus, Src1Addr#(8)));
   let ifc2();
   exposeCBusIFC#(mkSrc1Addr) _temp(ifc2);
   return (ifc2);
endmodule

module [MyModWithCBus] mkSrc1Addr(Src1Addr#(size_t))
   provisos(Add#(size_t, k3, CBDATASIZE));
    
   Reg#(Bit#(size_t)) src1 <- mkCBRegRW(12, 0);
   method Reg#(Bit#(size_t)) value();
      return src1;
   endmethod

    method Action increment();
      src1 <= src1 + 1;
   endmethod
   
   method Action load(Bit#(size_t) newval);
      src1 <= newval;
   endmethod

endmodule

(* synthesize *)
module mkSrc2AddrSynth(IWithCBus#(MyCBus, Src2Addr#(8)));
   let ifc3();
   exposeCBusIFC#(mkSrc2Addr) _temp(ifc3);
   return (ifc3);
endmodule

module [MyModWithCBus] mkSrc2Addr(Src2Addr#(size_t))
   provisos(Add#(size_t, k4, CBDATASIZE));
    
   Reg#(Bit#(size_t)) src2 <- mkCBRegRW(13, 0);
   method Reg#(Bit#(size_t)) value();
      return src2;
   endmethod

    method Action increment();
      src2 <= src2 + 1;
   endmethod
   
   method Action load(Bit#(size_t) newval);
      src2 <= newval;
   endmethod

endmodule

(* synthesize *)
module mkDstAddrSynth(IWithCBus#(MyCBus, DstAddr#(8)));
   let ifc4();
   exposeCBusIFC#(mkDstAddr) _temp(ifc4);
   return (ifc4);
endmodule

module [MyModWithCBus] mkDstAddr(DstAddr#(size_t))
   provisos(Add#(size_t, k5, CBDATASIZE));
    
   Reg#(Bit#(size_t)) dst <- mkCBRegRW(14, 0);
   method Reg#(Bit#(size_t)) value();
      return dst;
   endmethod

    method Action increment();
      dst <= dst + 1;
   endmethod
   
   method Action load(Bit#(size_t) newval);
      dst <= newval;
   endmethod

endmodule

(* synthesize *)
module mkControlReg1Synth( IWithCBus#(MyCBus, Control_reg1#(8)) );
   let ifc5();
   exposeCBusIFC#(mkControlReg1) _temp(ifc5);
   return (ifc5);
endmodule

module [MyModWithCBus] mkControlReg1(Control_reg1#(size_t))
   provisos(Add#(size_t, k6, CBDATASIZE));
    
   Reg#(Bit#(size_t)) cr1 <- mkCBRegRW(15, 0);
   method Bool isZero();
      return (cr1 == 0);
   endmethod   

   method Action load(Bit#(size_t) newval);
      cr1 <= newval;
   endmethod

endmodule

(* synthesize *)
module mkControlReg2Synth( IWithCBus#(MyCBus, Control_reg2#(8)) );
   let ifc6();
   exposeCBusIFC#(mkControlReg2) _temp(ifc6);
   return (ifc6);
endmodule

module [MyModWithCBus] mkControlReg2(Control_reg2#(size_t))
   provisos(Add#(size_t, k7, CBDATASIZE));
    
   Reg#(Bit#(size_t)) cr2 <- mkCBRegRW(16, 0);
   method Bool isZero();
      return (cr2 == 0);
   endmethod   
   
   method Action load(Bit#(size_t) newval);
      cr2 <= newval;
   endmethod

endmodule

//MAR1
(* synthesize *)
module mkMAR1Synth( IWithCBus#(MyCBus, MAR1#(8)) );
   let ifc7();
   exposeCBusIFC#(mkMAR1) _temp(ifc7);
   return (ifc7);
endmodule

module [MyModWithCBus] mkMAR1(MAR1#(size_t))
   provisos(Add#(size_t, k8, CBDATASIZE));
    
   Reg#(Bit#(size_t)) mar <- mkCBRegRW(17, 0);   
   method Reg#(Bit#(size_t)) value();
      return mar;
   endmethod
   method Action load(Bit#(size_t) newval);
      mar <= newval;
   endmethod

endmodule

//MBR1
(* synthesize *)
module mkMBR1Synth( IWithCBus#(MyCBus, MBR1#(8)) );
   let ifc8();
   exposeCBusIFC#(mkMBR1) _temp(ifc8);
   return (ifc8);
endmodule

module [MyModWithCBus] mkMBR1(MBR1#(size_t))
   provisos(Add#(size_t, k9, CBDATASIZE));
    
   Reg#(Bit#(size_t)) mbr <- mkCBRegRW(18, 0);   
   method Reg#(Bit#(size_t)) value();
      return mbr;
   endmethod
   method Action load(Bit#(size_t) newval);
      mbr <= newval;
   endmethod

endmodule


// define one more register attached to the acc that will store the blocksize////////////

// end of exposing the register to CBus 

////////////////////////////////////////////////////////////////////////////////
///
// Simple test-bench code...

(* synthesize *)
module mkVectorAdder(Empty);

	// indicates if all the operation are finished and should we end the simulation
	// specific register declaration

   Reg#(Bool) loadread1addr <- mkReg(False);
   Reg#(Bool) doneread1 <- mkReg(False);

   Reg#(Bool) loadread2addr <- mkReg(False);
   Reg#(Bool) doneread2 <- mkReg(False);

   Reg#(Bool) doneadd <- mkReg(False);
   Reg#(Bool) donesim <- mkReg(False);

	// define one more register here that will count the number of elements that are read, added and written to the memory//////////////

   Reg#(Bit#(CBDATASIZE)) op1 <- mkReg(0);


	// instaintate the memory module..
	DMem memory <- mkDMem;

	// instiantiate the CPU module..
	CPU cpu <- mkCPU;

   let csr1_ifc();
   mkCSR1Synth add_csr1(csr1_ifc);

   let csr2_ifc1();
   mkCSR2Synth add_csr2(csr2_ifc1);

	let src1_ifc2();
   mkSrc1AddrSynth add_src1(src1_ifc2);

	let src2_ifc3();
   mkSrc2AddrSynth add_src2(src2_ifc3);

	let dst_ifc4();
   mkDstAddrSynth add_dst(dst_ifc4);

	let control_reg1();
   mkControlReg1Synth add_cr1(control_reg1);

	let control_reg2();
   mkControlReg2Synth add_cr2(control_reg2);

	let mar();
   mkMAR1Synth add_mar(mar);

	let mbr();
   mkMBR1Synth add_mbr(mbr);

	// instiantiate the register which store the blocksize here/////////////////
   
   rule display_value (True);
      let read_value = fromMaybe(0, csr1_ifc.cbus_ifc.read(10));
      $display ("Current Value of CSR1 %2d at time:", read_value, $time);

	let read_value1 = fromMaybe(0, csr2_ifc1.cbus_ifc.read(11));
      $display ("Current Value of CSR2 %2d at time:", read_value1, $time);

	let read_value2 = fromMaybe(0, src1_ifc2.cbus_ifc.read(12));
      $display ("Current address at source1 %2d at time:", read_value2, $time);

	let read_value3 = fromMaybe(0, src2_ifc3.cbus_ifc.read(13));
      $display ("Current address at source2 %2d at time:", read_value3, $time);

	let read_value4 = fromMaybe(0, dst_ifc4.cbus_ifc.read(14));
      $display ("Current address at dest %2d at time:", read_value4, $time);

	let read_value5 = fromMaybe(0, control_reg1.cbus_ifc.read(15));
      $display ("Current address at cr1 %2d at time:", read_value5, $time);

	let read_value6 = fromMaybe(0, control_reg2.cbus_ifc.read(16));
      $display ("Current address at cr2 %2d at time:", read_value6, $time);

	let read_value7 = fromMaybe(0, mar.cbus_ifc.read(17));
      $display ("Current address at MAR %2d at time:", read_value7, $time);

	let read_value8 = fromMaybe(0, mbr.cbus_ifc.read(18));
      $display ("Current address at MBR %2d at time:", read_value8, $time);

	$display("\n\n");
	// print the blocksize here for the reference////////////

   endrule

	// step1 accelerator shows it readiness
   	rule init_csr2_via_acc (csr2_ifc1.device_ifc.isZero());
   	   csr2_ifc1.device_ifc.load(1);		// Device is telling to cpu that it is ready for the data transfer
	   memory.store(60,6);				// we are initializing the mem loc from where we will read the source 1
	   memory.store(40,7);				// we are initializing the mem loc from where we will read the source 2
		// initialize the vector//////////////
   	endrule


	// make this rule execute only once /////////////////////////
	// step2 CPU initializes the registers of the accelerator. 
   	rule init_csr1_reg_via_cpu (!csr2_ifc1.device_ifc.isZero());
      		csr1_ifc.cbus_ifc.write( 10,cpu.returnflag() );			// CPU is telling that it has initilaized the addresses in src1, scr2, dst by setting csr1 as 1.
		 src1_ifc2.cbus_ifc.write( 12,cpu.returnsrc1addr() );		// CPU initilaized the address in src1
		src2_ifc3.cbus_ifc.write( 13,cpu.returnsrc2addr() );		// CPU initilaized the address in scr2
		dst_ifc4.cbus_ifc.write( 14,cpu.returndstaddr() );		// CPU initilaized the address in dst
			// initailize the block-size value from CPU here///////////////////
  	endrule


	// make this rule execute only once /////////////////////////
	// step3 Memory shows its readiness by making cr1 as 1. 
	rule memory_read_ready ( !csr1_ifc.device_ifc.isZero() );
		control_reg1.cbus_ifc.write(15,1);		// Memory will make the memory enable as true..
	endrule


	// step4 if the Memory is ready for the operation(i.e. cr1 == 1), then Accelerator loads the address to be read from memory in the MAR
	rule read_op1_from_memory ( !control_reg1.device_ifc.isZero() && !loadread1addr );

		// preprocessing
		mar.device_ifc.load( src1_ifc2.device_ifc.value() );	// accelarator will read the value of scr1 and will write into the MAR 
		control_reg2.device_ifc.load(0);			// accelarator will make cr2 as 0 indicates that it is doing read operation from memory..

		loadread1addr <= True;				
   	endrule

	// do read operation from src1 address..
	rule readsrc1( !control_reg1.device_ifc.isZero() && !doneread1 && loadread1addr );

		// invoke memory module's read
		mbr.cbus_ifc.write(18, memory.load( fromMaybe(0, mar.cbus_ifc.read(17) ) ) ); // Memory will read the value of mar do the load op from addr [mar] and then write to mbr		
		
		// display the value at mem loc [mar]
		let z = memory.load( mar.device_ifc.value() );		// just for the display purpose			
		$display ("\n\nValue at memory location [%d] read is = %d", mar.device_ifc.value(), z );		
		doneread1 <= True;		// finish read1	
	endrule


	// step5 if doneread1 and Memory is ready for the operation(i.e. cr1 == 1), then Accelerator loads the address to be read from memory in the MAR do read operation from memory
	rule read_op2_from_memory ( !control_reg1.device_ifc.isZero() && doneread1 && !loadread2addr );	

		// preprocessing
		op1 <= mbr.device_ifc.value();				// accelerator will store the read value from mem loc [mar] in its previous read op to the register op1

		mar.device_ifc.load(src2_ifc3.device_ifc.value());	// accelarator will read the value of scr2 and will write into the MAR 
		control_reg2.device_ifc.load(0);			// accelarator will make cr2 as 0 indicates that it is doing read operation from memory..
		loadread2addr <= True;	
	endrule
	
	// do read operattion from src2 address..
	rule readsrc2(!control_reg1.device_ifc.isZero() && doneread1 && loadread2addr && !doneread2 );
		
		// invoke memory module's read	
		mbr.cbus_ifc.write(18, memory.load( fromMaybe(0, mar.cbus_ifc.read(17) ) ) );	// Memory will read the value of mar do the load op from addr [mar] and then write to mbr
		
		// display the value at mem loc [mar]
		let z = memory.load(mar.device_ifc.value());			
		$display ("\n\nValue at memory location [%d] read is = %d", mar.device_ifc.value(), z);	
		doneread2 <= True;			// finish read2
	endrule


	//step6 if doneread1 and doneread2, then do the addition operation and store it in op1. Write the op1 to memory.
	rule do_addition (!control_reg1.device_ifc.isZero() && doneread2 && doneread1 && !doneadd );
		// do the addition
		mbr.device_ifc.load( op1 + mbr.device_ifc.value() );			// accelarator takes value store in op1 and mbr and adds them store back to mbr
		//load dst to mar than write results stored in op1 to memory..
		mar.device_ifc.load(dst_ifc4.device_ifc.value());			// accelarator will read the value of dst and will write into the MAR 
		control_reg2.device_ifc.load(1);					// accelarator will make cr2 as 1 indicates that it is doing write operation to the memory..
		doneadd <= True;
	endrule
	
	rule writeresults(!control_reg1.device_ifc.isZero() && doneread2 && doneread1 && doneadd && !donesim );
		// write to the mem loc given by [mar] value mbr
	    memory.store( fromMaybe(0, mar.cbus_ifc.read(17) ), fromMaybe(0, mbr.cbus_ifc.read(18) ) ); // Memory will read the value of mar and mbr from cbus and will write mbr value at the addr [mar]
	    donesim <= True; 			//finish the simulation
	endrule



	//step7 if donesim is set then addition is finished print the final value at dst and end the simulation
	rule done (donesim);
	
		// done will now not finish the sim immediately///////////////////
		// make all the 7 registers defined in this module as False/////////////////
		// increment the scr1, scr2 and dst value by one using our increment method of this register//////////////////

		// check if done by comparing the register value which counts(defined in this module) number of cycles with the register which stores the blocksize/////////////////////
		// if both matches then end the sim.///////////////////

		// just for the display purpose		
		let z = memory.load( mar.device_ifc.value() );			// load the value at mem loc [mar] for display
		$display ("\n\nValue at memory location [%d] read is = %d", mar.device_ifc.value(), z);	
      	 	$finish();
   	endrule

   
endmodule

////////////////////////////////////////////////////////////////////////////////

endpackage

