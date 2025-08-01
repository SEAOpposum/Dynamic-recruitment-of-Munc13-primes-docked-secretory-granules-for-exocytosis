//// 
roiManager("reset");
run("Clear Results");
run("Set Measurements...", "area mean redirect=None decimal=3");

Direc=getDir("Cells and Bg ROI save");
     



	m = nSlices;
	ch12=getImageID();
	selectImage(ch12);
	makeRectangle(0, 0, 255, 255);
	run("Duplicate...", "duplicate");
	ch1=getImageID();
	selectImage(ch12);
	makeRectangle(255, 0, 511, 255);
	run("Duplicate...", "duplicate");
	ch2=getImageID();
	selectImage(ch12);
	close();
	selectImage(ch2);

	run("Select None");
	px=newArray(1);
	sig=newArray(1);
	SignalTOTAL=newArray(1);
	sigbg=newArray(1);
	DF=newArray(1);
	
	gpx=newArray(1);
	gsig=newArray(1);
	gSignalTOTAL=newArray(1);
	gsigbg=newArray(1);
	gDF=newArray(1);
	
	
	
	
		for (a = 1; a <= m; a++) {
			
			selectImage(ch2);
			
			roiManager("open", Direc+"Cells.zip" );	 
			roiManager("select", a-1);
			setSlice(a);
				
				run("Measure");
			
				c= Table.size;
				b=nResults;

				px[a]=getResult("Area", b-1);
				sig[a]=getResult("Mean", b-1);
				SignalTOTAL[a]=sig[a]*px[a];
				
				
				Overlay.remove;
				run("Select None");
			
			selectImage(ch2);
			roiManager("reset");
			roiManager("open", Direc+"bg.zip");	 
			roiManager("select", a-1);
			setSlice(a);
			
				run("Measure");
			
				
				b=nResults;
				sigbg[a]=getResult("Mean", b-1);
				DF[a]=sig[a]-sigbg[a];
				
				run("Clear Results");
				
				Overlay.remove;
				run("Select None");
				roiManager("reset");
				
			selectImage(ch1);
			roiManager("open", Direc+"Cells.zip" );	 
			roiManager("select", a-1);
			setSlice(a);
			
			run("Measure");
			
				c= Table.size;
				b=nResults;

				gpx[a]=getResult("Area", b-1);
				gsig[a]=getResult("Mean", b-1);
				gSignalTOTAL[a]=gsig[a]*gpx[a];
				
		
	
				Overlay.remove;
				run("Select None");
			
			selectImage(ch1);
			roiManager("reset");
			roiManager("open", Direc+"bg.zip");	 
			roiManager("select", a-1);
			setSlice(a);
			
				run("Measure");
			
				
				b=nResults;
				gsigbg[a]=getResult("Mean", b-1);
				gDF[a]=gsig[a]-gsigbg[a];
				
				run("Clear Results");
				
				Overlay.remove;
				run("Select None");
				roiManager("reset");
				
		
			
		};
	
		run("Select None");

  		selectImage(ch2);
		close();
		selectImage(ch1);
		close();
		close("*");
	
	Table.create("Table1");
	Table.setColumn("area",px);
	Table.setColumn("meanRed",sig);
	Table.setColumn("auRed", SignalTOTAL);
	Table.setColumn("meanbgRed", sigbg);
	Table.setColumn("DFRed", DF);
	
	Table.setColumn("meanGreen",gsig);
	Table.setColumn("auGreen", gSignalTOTAL);
	Table.setColumn("meanbgGreen", gsigbg);
	Table.setColumn("DFGreen", gDF);
	
//run("Read and Write Excel", "file=["+Direc+"\""+"ProteinLocalization.xlsx] sheet=Cells");   
