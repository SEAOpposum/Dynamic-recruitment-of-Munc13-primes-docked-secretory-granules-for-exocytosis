//ROI file should contain cell ROI in index 0 and Bg ROI in index 1
// This ROI give the granules density and the expression
roiManager("reset");
run("Clear Results");
run("Set Measurements...", "area mean redirect=None decimal=3");



  dir = getDirectory("Choose Cells Directory");
  dirROI = getDirectory("Choose ROIs Directory");
  
  Dialog.create("Which side are the granules?");
  Dialog.addChoice("Number of channels", newArray("one","two"));
  Dialog.addChoice("Side:", newArray("left","right"));
  Dialog.addNumber("PixelSize", 0.11);
  Dialog.show();
  
  Channels=Dialog.getChoice();
  side=Dialog.getChoice();
  pxsz=Dialog.getNumber();
  
   //setBatchMode(true);
      
   count = 0;
   countFiles(dir);
   n = 0;
   processFiles(dir);
   //print(count+" files processed");


   
 
   function countFiles(dir) {
      list = getFileList(dir);
      list= Array.sort(list);
      listROI= getFileList(dirROI);
      listROI= Array.sort(listROI);
    
    for (i=0; i<list.length; i++) {
          if (endsWith(list[i], "/"))
              countFiles(""+dir+list[i]);
          else
              count++;
      }
  }

   function processFiles(dir) {
      list = getFileList(dir);
      list= Array.sort(list);
      listROI= getFileList(dirROI);
      listROI= Array.sort(listROI);
      
      
      for (i=0; i<list.length; i++) {
   
          if (endsWith(list[i], "/"))
              processFiles(""+dir+list[i]); 
          else {
             showProgress(n++, count);
             path = dir+list[i];
             processFile(path);
          }
      }
  }

function processFile(path) {
       if (endsWith(path, ".stk")) {
           open(path);


ff=getImageID();        
m = nSlices;


//for (ii=1; ii<=m; ii++) { 
	//print(ii);
	setSlice(1);
	Title= File.getName(list[i]);
	ROIa= File.getName(listROI[i]);
	roiManager("Open", ""+dirROI+listROI[i]+"");
    roiManager("Select", 0);
	

	run("Find Maxima...", "noise=10 output=Count exclude");
	
	getStatistics(area,mean);
	setResult("Image", i,""+Title+"");
	setResult("ROI", i,""+ROIa+"");
	setResult("Area",i, area);
	
	getSelectionBounds(x, y, width, height);
	if (Channels=="two") {

		if (side=="right") {
			setSelectionLocation(x-256,y);
	
		}else {
			setSelectionLocation(x+256,y);
		}
	}

		
	getStatistics(area,mean);
	setResult("MeanCell ", i, mean);
    setResult("AreaReal", i, area*(pxsz*pxsz));
	setResult("grDensity", i, getResult("Count", i)/(area*(pxsz*pxsz)));
	setSelectionLocation(x,y);
	run("Find Maxima...", "noise=10 output=[Point Selection] exclude");

	updateResults();
	Overlay.remove;

//};

 roiManager("Select", 1);
	

//for (ii=1; ii<=m; ii++) { 

	setSlice(1);

	getSelectionBounds(x, y, width, height);	
	if (Channels=="Two") {

		if (side=="right") {
			setSelectionLocation(x-256,y);
	
		}else {
			setSelectionLocation(x+256,y);
		}
	}
	getStatistics(area,mean);
	setResult("MeanBg ", i, mean);
	
	updateResults();
//};

    close("*");
	roiManager("reset"); 

      }
  };
//run("Clear Results");
run("Read and Write Excel", "file=["+dir+"CellsInfo.xlsx] sheet=Cells");   

