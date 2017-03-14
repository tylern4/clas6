// Declare Bos and ClasEvent objects at beginning of program 
Bos *bos = new Bos(); 
ClasEvent clasevent("p:pi+:pi-"); 

// Open the BOS file
if(!(bos->InFile("some_file.bos"))){ // open the file
  cout << "Unable to open BOS file: some_file.bos" << endl;
  continue; // skip the file or return 0 if only file
}

// Loop over events in the file
while(events_processed < max && bos->GetBOS()){

  events_processed++;

  clasevent.SetClasEvent(bos);  // init ClasEvent for this event

  while(clasevent.GetEvent(bos)){ // loop over all g p -> p pi+ pi- combos

	// analyze the event...

  }
  bos->CleanBOS(); // drop banks...get ready for next event
}
bos->CloseInFile(); // close input file

