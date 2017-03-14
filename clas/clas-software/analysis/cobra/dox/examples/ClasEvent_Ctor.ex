// create a ClasEvent object looking for proton,pi+,pi- detected
ClasEvent clasevent("p:pi+:pi-");

// or as a pointer
ClasEvent *ce_ptr = new ClasEvent("p:pi+:pi-");

// or looking for proton proton pi-
ClasEvent ce_pppim("p:p:pi-");

// or k+ proton
ClasEvent ce_pkp("p:k+");

// etc...
