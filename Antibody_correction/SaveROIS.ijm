///// To make the ROI for the expression
roiManager("reset");
Direc=getDir("Cells and Bg ROI save");

a=getImageID();
fotos=nSlices;

for (i = 0; i < fotos; i++) {
	
	selectImage(a);
	setSlice(i+1);
	
	makeRectangle(255, 0, 511, 255);
	run("Duplicate...", "duplicate");
	ch2=getImageID();
	setSlice(i+1);
	waitForUser("Select Cell"); 
	roiManager("add");
	
	selectImage(ch2);
	close();

}
roiManager("save",Direc+"Cells.zip" );
roiManager("reset");
for (i = 0; i < fotos; i++) {
	selectImage(a);
	setSlice(i+1);
	
	makeRectangle(255, 0, 511, 255);
	run("Duplicate...", "duplicate");
	ch2=getImageID();
	setSlice(i+1);
	waitForUser("Select Bg"); 
	roiManager("add");
	
	selectImage(ch2);
	close();

}
roiManager("save",Direc+"bg.zip");
close("*");