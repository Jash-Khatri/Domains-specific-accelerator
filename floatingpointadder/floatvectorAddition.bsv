/***
Author : Jash Khatri (CS19S018)
Time of creation : 24 December 2020 at 15:26 IST
Bluespec Code for : Domain specific accelerator for Floating Point Vector Addition with dual ported memory
***/

/**
* Main Module Name : mkVectorAdder

* Parameters: 
	[in] Src1: Stores the starting address for the source Vector1. 
	[in] Src2: Stores the starting address for the source Vector2.
	[out] Dst: Stores the starting adress for the destination Vector.
	[in] bSize: Number of samples in each vector 
	
* Returns  : None

* Module Description: 

  Element-by-element addition of two floating point vectors.
  Dst[n] = Src1[n] + Src2[n],   0 <= n < bSize.
	
  Accelerator module mkVectorAdder read the elements stored at memory location Src1 and Src2 does the addition and stores the results at the Dst memory location.
  Later the Src1, Src2 and Dst are incremented to point the next element in the Vectors. This process is continued until bSize number of elements are processed.

* DataType supported and Parameterization: Elements of the Vectors has to be an 32-bit floating point values as per the IEEE-754 standards. Since, the size of the floating point values is 32-bit the value corresponding to the CBDATASIZE in parameters.bsv has to be greater than or equal to 32 for error-free compilation of the code.
	
	For instance: Following is the valid size for the CBDATASIZE in the parameters.bsv:
	 typedef 32 CBDATASIZE;
	Similarly, 
	 typedef 35 CBDATASIZE;
*/
package floatvectorAddition;

import CBusMod:: *;	// CBus module imported
import Vector :: *;	// Vector module imported
import FloatingPoint :: *;	// Import the library for supporting the floating point values
import parameters :: *; // import the tunable parameter from the parameter.bsv file 

typedef ModWithCBus#(CBADDRSIZE, CBDATASIZE, i) MyModWithCBus#(type i);
typedef CBus#(CBADDRSIZE, CBDATASIZE) MyCBus;

// Defining all the registers present in the accelerator here and there associated actions...

interface SUCCESS#(type size_t);		// Interface for CSR bit to interact between accelerator and CPU. Informs CPU that accelerator has finished its computation
   method Bool   isZero();
   method Action load(Bit#(size_t) newval);
endinterface

interface CSR1#(type size_t);		// Interface for CSR bit using this accelerator know if CPU has initialized the src1, src2, dst addresses and block-size.
   method Bool   isZero();
   method Action load(Bit#(size_t) newval);
endinterface

interface CSR2#(type size_t);		// CSR bit to interact between accelerator and CPU. Using this bit accelerator shows its readyness for the execution.
   method Bool   isZero();
   method Action load(Bit#(size_t) newval);
endinterface

interface Src1Addr#(type size_t);	// Interface for register which stores the source1 address.
   method Reg#(Bit#(size_t)) value();
   method Action increment();
   method Action load(Bit#(size_t) newval);     
endinterface

interface Src2Addr#(type size_t);	// Interface for register which Stores the source2 address.
   method Reg#(Bit#(size_t)) value();
   method Action increment();
   method Action load(Bit#(size_t) newval);    
endinterface

interface DstAddr#(type size_t);	// Interface for register which stores the destination address.
   method Reg#(Bit#(size_t)) value();
   method Action increment();
   method Action load(Bit#(size_t) newval);    
endinterface

interface Control_reg1#(type size_t);		// Interface to register which controls the access to the memory. Memory sets this bit if it is ready for operation else reset. 
   method Bool   isZero();
   method Action load(Bit#(size_t) newval);     
endinterface

interface Control_reg2#(type size_t);		// Interface to register which indicates which memory operation(read/write) is being performed on the memory.
   method Bool   isZero();   
   method Action load(Bit#(size_t) newval);     
endinterface

interface MAR1#(type size_t);			// Interface for the memory address register1..
   method Reg#(Bit#(size_t)) value();
   method Action load(Bit#(size_t) newval);    
endinterface

interface MBR1#(type size_t);			// Interface for the memory buffer register1..
   method Reg#(Bit#(size_t)) value();
   method Action load(Bit#(size_t) newval);    
endinterface

interface MAR2#(type size_t);			// Interface for the memory address register2..
   method Reg#(Bit#(size_t)) value();
   method Action load(Bit#(size_t) newval);    
endinterface

interface MBR2#(type size_t);			// Interface for the memory buffer register2..
   method Reg#(Bit#(size_t)) value();
   method Action load(Bit#(size_t) newval);    
endinterface

// define one more register attached to the acc that will store the blocksize
interface BLKSIZE#(type size_t);			// stores the block-size
   method Reg#(Bit#(size_t)) value();
   method Action load(Bit#(size_t) newval);    
endinterface

// defining interface for the Data Memory
interface DMem;
method (Bit#(CBDATASIZE)) load (Bit#(CBDATASIZE) mar);
method Action store (Bit#(CBDATASIZE) mar, Bit#(CBDATASIZE) mbr);

method (Bit#(CBDATASIZE)) load1 (Bit#(CBDATASIZE) mar);
method Action store1 (Bit#(CBDATASIZE) mar, Bit#(CBDATASIZE) mbr);
endinterface

// module for the Data Memory
module mkDMem(DMem);
	
	Vector #( 128, Reg#(Bit#(CBDATASIZE)) ) memory_array  <- replicateM (mkReg(0));
	Reg#(Bit#(CBDATASIZE)) r <- mkReg(0);

	method (Bit#(CBDATASIZE)) load (Bit#(CBDATASIZE) mar);		// load operation to the memory
		return memory_array[mar];
	endmethod

	method Action store (Bit#(CBDATASIZE) mar, Bit#(CBDATASIZE) mbr);	// store operation in to the memory
		memory_array[mar] <= mbr;
	endmethod

	method (Bit#(CBDATASIZE)) load1 (Bit#(CBDATASIZE) mar);		// Dual ported hence two load methods to load at same time in memory
		return memory_array[mar];
	endmethod

	method Action store1 (Bit#(CBDATASIZE) mar, Bit#(CBDATASIZE) mbr);	// Dual ported hence two store methods to store at same time in memory
		memory_array[mar] <= mbr;
	endmethod

endmodule

// Data Memory defination end..

// defining a CPU interface
interface CPU;
method (Bit#(CBDATASIZE)) returnflag();		// CPU returns the flag which indicates if CPU has initialized Src1, Src2, Dst and Block-size
method (Bit#(CBDATASIZE)) returnsrc1addr();	// CPU gives the source1 address to accelerator using this method.
method (Bit#(CBDATASIZE)) returnsrc2addr();	// CPU gives the source2 address to accelerator using this method.
method (Bit#(CBDATASIZE)) returndstaddr();	// CPU gives the destination address to accelerator using this method.
method (Bit#(CBDATASIZE)) returnblocksize();	// CPU gives the block-size to accelerator using this method.

method Action putsrc1addr (Bit#(CBDATASIZE) s1);  // Method used to put Src1 address inside the CPU obtained from the Test-bench
method Action putsrc2addr (Bit#(CBDATASIZE) s2);  // Method used to put Src2 address inside the CPU obtained from the Test-bench
method Action putdstaddr (Bit#(CBDATASIZE) d);	  // Method used to put Dst address inside the CPU obtained from the Test-bench
method Action putbsize (Bit#(CBDATASIZE) b);	  // Method used to put block-size value inside the CPU obtained from the Test-bench

endinterface

// module for CPU..
module mkCPU(CPU);
	Reg#(Bit#(CBDATASIZE)) flag <- mkReg(1);	// register used to store the flag value
	Reg#(Bit#(CBDATASIZE)) src1 <- mkReg(0);	// register used to store the source1 address 
	Reg#(Bit#(CBDATASIZE)) src2 <- mkReg(0);	// register used to store the source2 address
	Reg#(Bit#(CBDATASIZE)) dst <- mkReg(0);		// register used to store the Destination address
	Reg#(Bit#(CBDATASIZE)) bsize <- mkReg(0);	// register used to store the block-size

	method (Bit#(CBDATASIZE)) returnflag();
		return flag;
	endmethod

	method (Bit#(CBDATASIZE)) returnsrc1addr();
		return src1;
	endmethod

	method (Bit#(CBDATASIZE)) returnsrc2addr();
		return src2;
	endmethod
	
	method (Bit#(CBDATASIZE)) returndstaddr();
		return dst;
	endmethod

	method (Bit#(CBDATASIZE)) returnblocksize();
		return bsize;
	endmethod

	method Action putsrc1addr (Bit#(CBDATASIZE) s1);
		src1 <= s1;
	endmethod

	method Action putsrc2addr (Bit#(CBDATASIZE) s2);
		src2 <= s2;
	endmethod

	method Action putdstaddr (Bit#(CBDATASIZE) d);
		dst <= d;
	endmethod

	method Action putbsize (Bit#(CBDATASIZE) b);
		bsize <= b;
	endmethod

endmodule

// CPU interface ends here..

// Exposing the accelerator's register to the CBus so they can be access by the CPU and MEMORY...

(* synthesize *)
module mkSUCCESSSynth(IWithCBus#(MyCBus, SUCCESS#(CBDATASIZE)));
   let ifc();
   exposeCBusIFC#(mkSUCCESS) _temp(ifc);
   return (ifc);
endmodule

module [MyModWithCBus] mkSUCCESS(SUCCESS#(size_t))
   provisos(Add#(size_t, k, CBDATASIZE));
    
   Reg#(Bit#(size_t)) sc <- mkCBRegRW(9, 0);
   method Bool isZero();
      return (sc == 0);
   endmethod
   
   method Action load(Bit#(size_t) newval);
      sc <= newval;
   endmethod

endmodule

(* synthesize *)
module mkCSR1Synth(IWithCBus#(MyCBus, CSR1#(CBDATASIZE)));
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
module mkCSR2Synth(IWithCBus#(MyCBus, CSR2#(CBDATASIZE)));
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
module mkSrc1AddrSynth(IWithCBus#(MyCBus, Src1Addr#(CBDATASIZE)));
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
module mkSrc2AddrSynth(IWithCBus#(MyCBus, Src2Addr#(CBDATASIZE)));
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
module mkDstAddrSynth(IWithCBus#(MyCBus, DstAddr#(CBDATASIZE)));
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
module mkControlReg1Synth( IWithCBus#(MyCBus, Control_reg1#(CBDATASIZE)) );
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
module mkControlReg2Synth( IWithCBus#(MyCBus, Control_reg2#(CBDATASIZE)) );
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
module mkMAR1Synth( IWithCBus#(MyCBus, MAR1#(CBDATASIZE)) );
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
module mkMBR1Synth( IWithCBus#(MyCBus, MBR1#(CBDATASIZE)) );
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

//MAR2
(* synthesize *)
module mkMAR2Synth( IWithCBus#(MyCBus, MAR2#(CBDATASIZE)) );
   let ifc7();
   exposeCBusIFC#(mkMAR2) _temp(ifc7);
   return (ifc7);
endmodule

module [MyModWithCBus] mkMAR2(MAR2#(size_t))
   provisos(Add#(size_t, k8, CBDATASIZE));
    
   Reg#(Bit#(size_t)) mar2 <- mkCBRegRW(20, 0);   
   method Reg#(Bit#(size_t)) value();
      return mar2;
   endmethod
   method Action load(Bit#(size_t) newval);
      mar2 <= newval;
   endmethod

endmodule

//MBR2
(* synthesize *)
module mkMBR2Synth( IWithCBus#(MyCBus, MBR2#(CBDATASIZE)) );
   let ifc8();
   exposeCBusIFC#(mkMBR2) _temp(ifc8);
   return (ifc8);
endmodule

module [MyModWithCBus] mkMBR2(MBR2#(size_t))
   provisos(Add#(size_t, k9, CBDATASIZE));
    
   Reg#(Bit#(size_t)) mbr2 <- mkCBRegRW(21, 0);   
   method Reg#(Bit#(size_t)) value();
      return mbr2;
   endmethod
   method Action load(Bit#(size_t) newval);
      mbr2 <= newval;
   endmethod

endmodule

//Blksize
(* synthesize *)
module mkBLKSIZESynth( IWithCBus#(MyCBus, BLKSIZE#(CBDATASIZE)) );
   let ifc9();
   exposeCBusIFC#(mkBLKSIZE) _temp(ifc9);
   return (ifc9);
endmodule

module [MyModWithCBus] mkBLKSIZE(BLKSIZE#(size_t))
   provisos(Add#(size_t, k10, CBDATASIZE));
    
   Reg#(Bit#(size_t)) bsize <- mkCBRegRW(19, 0);   
   method Reg#(Bit#(size_t)) value();
      return bsize;
   endmethod
   method Action load(Bit#(size_t) newval);
      bsize <= newval;
   endmethod

endmodule

// end of exposing the register to CBus 

// Interface defining the methods that can be accessed by the test-bench
interface VectorAdder_ifc;

	method Action put_src1 (Bit#(CBDATASIZE) src1);		// This method allows test-bench to put the address of source1 in CPU.
	method Action put_src2 (Bit#(CBDATASIZE) src2);		// This method allows test-bench to put the address of source2 in CPU.
	method Action put_dst (Bit#(CBDATASIZE) dst);		// This method allows test-bench to put the address of destination to CPU.
	method Action put_bsize (Bit#(CBDATASIZE) bsize);	// This method allows test-bench to put the block-size value to the CPU.

	method (Bit#(CBDATASIZE)) mem_load ( Bit#(CBDATASIZE) addr );		// This method allows test-bench to load data from the memory.
	method Action mem_store ( Bit#(CBDATASIZE) addr , Bit#(CBDATASIZE) val ); // This method allows test-bench to store the data to the memory.
	
	method (Bit#(CBDATASIZE)) mem_load1 ( Bit#(CBDATASIZE) addr );		// Dual-ported memory hence two load operation that can be accessed by test-bench
	method Action mem_store1 ( Bit#(CBDATASIZE) addr , Bit#(CBDATASIZE) val ); // Dual-ported memory hence two store operation that can be accessed by test-bench

	method (Bool)  iscomplete();	// This method informs the test-bench if the accelarator has finished its execution or not.

endinterface: VectorAdder_ifc


// The main accelerator module. It implements the VectorAdder_ifc interface for interacting with the external test-bench.
(* synthesize *)
module mkVectorAdder(VectorAdder_ifc);

	// specific register declaration

   Reg#(Bool) cpufinishinit <- mkReg(False);	// Bool Register which is set to true when the CPU has initialized the Src1, Src2, Dst and Block-size 
   Reg#(Bool) memorysignalsent <- mkReg(False);	// Bool Register which is set to true when memory sets the memory enable operation as true by setting control_reg1 to 1.

   Reg#(Bool) loadread1addr <- mkReg(False);	// Bool Register which is set to true when Src1 address and Src2 address are loaded into the MAR and MAR1
   Reg#(Bool) doneread1 <- mkReg(False);	// Bool Register which is set to true when value at the address Src1 and Src2 are successfully read from the DMem and loaded into MBR and MBR1

   Reg#(Bool) doneadd <- mkReg(False);		// Bool Register which is set to true when added float value is loaded back into mbr
   Reg#(Bool) donesim <- mkReg(False);		// Bool Register which is set to true when results of the addition are stored back to Dst memory location

   Reg#(Bool) donefloatadd <- mkReg(False);	// Bool Register which is set to true when floating point addition is done
   Reg#(Bool) doneunpack <- mkReg(False);	// Bool Register which is set to true when values from mbr are loaded into floating point registers

	// Register that will count the number of elements that are already processed for the addition in Vector1 and Vector2. Initially no element is processed.
	Reg#(Bit#(CBDATASIZE)) countelements <- mkReg(0);

	// bool register to check if the values are initialized by testbench or not..
	Reg#(Bool) got_src1 <- mkReg(False);	// Bool Register which is set to true when test-bench initializes the source1 address
   	Reg#(Bool) got_src2 <- mkReg(False);	// Bool Register which is set to true when test-bench initializes the source2 address
	Reg#(Bool) got_dst <- mkReg(False);	// Bool Register which is set to true when test-bench initializes the destination address
   	Reg#(Bool) got_bsize <- mkReg(False);	// Bool Register which is set to true when test-bench initializes the block-size value

	// FLoating point registers
	Reg#(Float) f1 <- mkReg(0);
	Reg#(Float) f2 <- mkReg(0);
	Reg#(Float) f3 <- mkReg(0);

	// instantiate the memory module..
	DMem memory <- mkDMem;

	// instantiate the CPU module..
	CPU cpu <- mkCPU;

   // instantiate the register which store the execution status. That is execution is complete or not.
   let successbit();
   mkSUCCESSSynth add_success(successbit);

   // instantiate the register which store signal sent from CPU after the initialization from the CPU.
   let csr1_ifc();
   mkCSR1Synth add_csr1(csr1_ifc);

   // instantiate the register which store the ready signal sent from the accelerator.
   let csr2_ifc1();
   mkCSR2Synth add_csr2(csr2_ifc1);

	// instantiate the register which store the source1 address
	let src1_ifc2();
   mkSrc1AddrSynth add_src1(src1_ifc2);

	// instantiate the register which store the source2 address
	let src2_ifc3();
   mkSrc2AddrSynth add_src2(src2_ifc3);

	// instantiate the register which store the destination address
	let dst_ifc4();
   mkDstAddrSynth add_dst(dst_ifc4);

	// instantiate the register which store the ready signal from memory
	let control_reg1();
   mkControlReg1Synth add_cr1(control_reg1);

	// instantiate the register which store the type of operation(read or write) performed on the memory.
	let control_reg2();
   mkControlReg2Synth add_cr2(control_reg2);

	// instantiate the first Memory Address Register
	let mar();
   mkMAR1Synth add_mar(mar);

	// instantiate the first Memory Buffer Register
	let mbr();
   mkMBR1Synth add_mbr(mbr);

	// instantiate the second Memory Address Register
	let mar1();
   mkMAR2Synth add_mar1(mar1);

	// instantiate the second Memory Buffer Register
	let mbr1();
   mkMBR2Synth add_mbr1(mbr1);

	// instantiate the register which store the blocksize 
	let  bsize();
   mkBLKSIZESynth add_bsize(bsize);

	// step1 accelerator shows it readiness
   	rule init_csr2_via_acc ( got_src1 && got_src2 && got_dst && got_bsize && csr2_ifc1.device_ifc.isZero());
   	   csr2_ifc1.device_ifc.load(1);		// Accelerator is telling to cpu that it is ready for the data transfer
   	endrule

	// step2 CPU initializes the registers of the accelerator. 
   	rule init_csr1_reg_via_cpu (!csr2_ifc1.device_ifc.isZero() && !cpufinishinit );
      		csr1_ifc.cbus_ifc.write( 10,cpu.returnflag() );			// CPU is telling that it has initilaized the addresses in src1, scr2, dst by setting csr1 as 1 via CBus.
		 src1_ifc2.cbus_ifc.write( 12,cpu.returnsrc1addr() );		// CPU initilaized the address in src1 register via CBus
		src2_ifc3.cbus_ifc.write( 13,cpu.returnsrc2addr() );		// CPU initilaized the address in scr2 register via CBus
		dst_ifc4.cbus_ifc.write( 14,cpu.returndstaddr() );		// CPU initilaized the address in dst register via CBus
		bsize.cbus_ifc.write( 19,cpu.returnblocksize() );		// CPU initialized the block-size in bsize register via CBus
		
		cpufinishinit <= True;		// set it to True as CPU has finished it initialization
  	endrule

	// step3 Memory shows its readiness by making control_reg1 as 1. 
	rule memory_read_ready ( !csr1_ifc.device_ifc.isZero() && !memorysignalsent );
		control_reg1.cbus_ifc.write(15,1);		// Memory will inform that it is ready by making the memory enable as true via CBus
		
		memorysignalsent <= True;	// set it to True as Memory is ready from the operation
	endrule


	// step4 if the Memory is ready for the operation(i.e. control_reg1 == 1) and successbit is reset, then Accelerator loads the address to be read from memory in the MAR1 and MAR2
	rule read_op1_from_memory ( !control_reg1.device_ifc.isZero() && !loadread1addr && successbit.device_ifc.isZero() );

		mar.device_ifc.load( src1_ifc2.device_ifc.value() );	// accelarator will read the value of scr1 and will write into the mar 
		mar1.device_ifc.load(src2_ifc3.device_ifc.value());	// accelarator will read the value of scr2 and will write into the mar1

		control_reg2.device_ifc.load(0);			// accelarator will make control_reg2 as 0 indicates that it is doing read operation from memory..

		loadread1addr <= True;					// addresses loaded in the mar and mar1 hence set it to true.
   	endrule

	// do read operation from src1 and src2 addresses..
	rule readsrc1( !control_reg1.device_ifc.isZero() && !doneread1 && loadread1addr );
		// invoke memory module's read
	 mbr.cbus_ifc.write(18, memory.load( fromMaybe(0, mar.cbus_ifc.read(17) ) ) ); // Memory will read the value of mar via CBus do the load op from addr [mar] and then write to mbr via CBus		
		// invoke memory module's read	
	 mbr1.cbus_ifc.write(21, memory.load1( fromMaybe(0, mar1.cbus_ifc.read(20) ) ) ); // Memory will read the value of mar1 via CBus do the load op from addr [mar1] and then write to mbr1 via CBus
						

		doneread1 <= True;		// set it to True as reads are finished
	endrule

	// step5 if reading is finished then load the values present in mbr and mbr1 into float registers f1 and f2 respectively. 
	rule mbr_to_float(!control_reg1.device_ifc.isZero() && doneread1 && !doneunpack );
		f1 <= unpack( truncate(mbr.device_ifc.value() ));
		f2 <= unpack( truncate(mbr1.device_ifc.value() ));
		doneunpack <= True;		// done loading into the registers hence set it to true
	endrule

	// add the float values in f1 and f2 and store it in f3 register.
	rule add_float(!control_reg1.device_ifc.isZero() && doneread1 && doneunpack && !donefloatadd );
		f3 <= f1+f2;
		donefloatadd<=True;		// floating point addition is finished hence set it to true
	endrule

	// step6 if doneread1, then do the addition operation and store it in MBR.
	rule do_addition (!control_reg1.device_ifc.isZero() && doneread1 && doneunpack && donefloatadd && !doneadd );

		// do the addition
		mbr.device_ifc.load( zeroExtend( pack(f3) ) ); // accelarator takes value stored in f3 register and store it back to mbr
		//load Dst address to MAR
		mar.device_ifc.load(dst_ifc4.device_ifc.value());			// accelarator will read the value of Dst reigster and will write into the MAR 
		control_reg2.device_ifc.load(1);					// accelarator will make control_reg2 as 1. Indicates that it is doing write operation to the memory..
		doneadd <= True;
	endrule
	
	// Write the MBR data to memory.
	rule writeresults(!control_reg1.device_ifc.isZero() && doneread1 && doneunpack && donefloatadd && doneadd && !donesim );
	    // write to the mem loc given by [mar] value mbr
	    memory.store( fromMaybe(0, mar.cbus_ifc.read(17) ), fromMaybe(0, mbr.cbus_ifc.read(18) ) ); // Memory will read the value of mar and mbr from cbus and will write mbr value at the addr [mar]
	    countelements <= countelements + 1;		// increment the number of elements processed
	    
	    donesim <= True; 			//finished addition of current element of the vector and stored it properly in memory hence set it to True
	endrule



	//step7 if donesim is set do the bookkeeping work.
	rule donestep (donesim);
	
		// Initialize all the register to false.
   		loadread1addr <= False;
   		doneread1 <= False;

   		doneadd <= False;
   		donesim <= False;
	
		doneunpack <= False;
		donefloatadd <= False;

		// Accelerator increments the scr1, scr2 and dst address by one using increment method
		src1_ifc2.device_ifc.increment();		// accelerator increments the src1 address
		src2_ifc3.device_ifc.increment();		// accelerator increments the src2 address
		dst_ifc4.device_ifc.increment();		// accelerator increments the dst address


		// check if done by comparing the register value which counts number of cycles with the register which stores the blocksize
		// if both matches then end the simulation
		if( countelements == bsize.device_ifc.value() )			// accelerator checks if number of elements process from Vector1,2 is equal to the block size
		begin
			successbit.device_ifc.load(1);			// Vector addition is finished hence accelerator will set successbit to 1
		end
   	endrule

	// Working of the below defined methods are explained in VectorAdder_ifc

	method Action put_src1 (Bit#(CBDATASIZE) src1) if (! got_src1);
		cpu.putsrc1addr(src1);		// put the src1 address in the cpu
		got_src1 <= True;
	endmethod

	method Action put_src2 (Bit#(CBDATASIZE) src2) if (! got_src2);
		cpu.putsrc2addr(src2);		// put the src2 address in the cpu
		got_src2 <= True;
	endmethod

	method Action put_dst (Bit#(CBDATASIZE) dst) if (! got_dst);
		cpu.putdstaddr(dst);		// put the dst address in the cpu
		got_dst <= True;
	endmethod

	method Action put_bsize (Bit#(CBDATASIZE) bsize) if (! got_bsize);
		cpu.putbsize(bsize);		// put the block-size value in the cpu
		got_bsize <= True;
	endmethod

	method (Bit#(CBDATASIZE)) mem_load ( Bit#(CBDATASIZE) addr );
		 return memory.load(addr);	// loads the value from memory location addr
	endmethod
	
	method Action mem_store ( Bit#(CBDATASIZE) addr , Bit#(CBDATASIZE) val );
   		memory.store(addr,val);		// store the value val at memory locaction addr
	endmethod

	method (Bit#(CBDATASIZE)) mem_load1 ( Bit#(CBDATASIZE) addr );
		 return memory.load1(addr);	// loads the value from memory location addr
	endmethod
	
	method Action mem_store1 ( Bit#(CBDATASIZE) addr , Bit#(CBDATASIZE) val );
   		memory.store1(addr,val);	// store the value val at memory locaction addr
	endmethod

	method (Bool) iscomplete();
		return ( !successbit.device_ifc.isZero() );		// return the status of success bit to get the status of execution(complete or not)
	endmethod

endmodule

////////////////////////////////////////////////////////////////////////////////

endpackage

