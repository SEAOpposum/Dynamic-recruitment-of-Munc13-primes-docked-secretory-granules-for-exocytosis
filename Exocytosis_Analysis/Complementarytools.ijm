// This example macro tool creates circular selections.
// Press ctrl-i (Macros>Install Macros) to reinstall after
// making changes. Double click on the tool icon (a circle)
// to set the radius of the circle.
// There is more information about macro tools at
//   http://imagej.nih.gov/ij/developer/macro/macros.html#tools
// and many more examples at
//   http://imagej.nih.gov/ij/macros/tools/

var radius = 3;
var Selection= newArray("Left","Rigth");

macro "Select channel Action Tool - B05C090F0099C900F9099"{
	Dialog.create("Select Channel with events");
	Dialog.addChoice("Channel", Selection,"Right");
	Dialog.show();
	Selections=Dialog.getChoice();
	if (Selections=="Left"){
		makeRectangle(0, 0, 256, 256);
	}else {
		makeRectangle(256, 0, 256, 256);
		}
run("Duplicate...", "duplicate");

}

macro "Circle Tool - C009O0099C090O9099C900O5999" {
   getCursorLoc(x, y, z, flags);
   makeOval(x-radius, y-radius, radius*2, radius*2);
   roiManager("add");   
}

macro "Circle Tool Options" {
   radius = getNumber("Radius: ", radius);
}
macro "Time Profile Action Tool - C555T2f18T" {
Dialog.create("Select ROI index");
	Selections= Dialog.addNumber("ROI#", 0);
	Dialog.show();
	number=Dialog.getNumber();
	roiManager("select", number);
	run("Plot Z-axis Profile");
}

macro "Save Regions Action Tool - C555T2f18S" {
	run("Set Measurements...", "area mean centroid center bounding stack redirect=None decimal=3");
	//The ROIs have a radius of 3 diam= 6 pixel therfore the center is inpixel 4 in every axis
	Directory=getDir("Save results in:");
	Dialog.create("Save ROIs");
	Dialog.addString("Video ID", "0007");
	Dialog.show();
	ID=Dialog.getString();
	run("Clear Results");
	roiManager("deselect");
	roiManager("Measure");
	saveAs("Results", ""+Directory+"/"+ID+".txt");
	
}
macro "Save Bg Action Tool - C555T2f18B" {
	run("Set Measurements...", "area mean centroid center bounding stack redirect=None decimal=3");
	//The ROIs have a radius of 3 diam= 6 pixel therfore the center is inpixel 4 in every axis
	Directory=getDir("Save Bg results in:");
	Dialog.create("Save ROIs");
	Dialog.addString("Video ID", "0007");
	Dialog.show();
	ID=Dialog.getString();
	run("Clear Results");
	roiManager("deselect");
	roiManager("Measure");
	saveAs("Results", ""+Directory+"/"+ID+"-bg.txt");
	
}

macro "Load Regions Action Tool - C009O5555" {
	//The ROIs have a radius of 3 diam= 6 pixel therfore the center is inpixel 4 in every axis
	roiManager("reset");
	Directory=getDir("Load region in:");
	Dialog.create("Load ROIs");
	Dialog.addString("Video ID", "0007");
	Dialog.addNumber("Radius", 3);
	Dialog.addChoice("which side", newArray("left","right"));
	Dialog.addChoice("where did you pick the events", newArray("left","right"));
	Dialog.show();
	ID=Dialog.getString();
	radius=Dialog.getNumber();
	side=Dialog.getChoice();
	Pick=Dialog.getChoice();
	run("Clear Results");
	run("Results... ", "open=["+Directory+ID+".txt]");
	regions=nResults
	for (i = 0; i < regions; i++) {
		x=getResult("X", i);
		y=getResult("Y", i);
		if (side=="right"){
			if (Pick=="left") {
			makeOval((x-radius)+256, y-radius, radius*2, radius*2);
		}}if (side=="left"){
			if (Pick=="left") {
			makeOval((x-radius), y-radius, radius*2, radius*2);
		}}if (side=="left"){if (Pick=="right") {
			makeOval((x-radius)-256, (y-radius), radius*2, radius*2);
		}}if (side=="right"){ if(Pick=="right") {
			makeOval((x-radius), (y-radius), radius*2, radius*2);
		}}

		frame=getResult("Slice", i);
		setSlice(frame);
		roiManager("add");
		roiManager("deselect");
	}
	

	
}
macro "Square Tool - R099O0000" {
   getCursorLoc(x, y, z, flags);
   setSlice(nSlices);
   makeRectangle(x-radius, y-radius, radius*2, radius*2);
   roiManager("add");   
}

macro "Square Tool Options" {
   radius = getNumber("Radius: ", radius);
}