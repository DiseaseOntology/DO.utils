# parse_html_table() works

    Code
      parse_html_table("data/html/table1.html")
    Output
      # A tibble: 16 x 3
         X1                              X2             X3                           
         <chr>                           <chr>          <chr>                        
       1 Alzheimer Gene Assoc Classifier GeneSurrounder MEDSHARE                     
       2 ANDES                           GGCDA          metaCDA                      
       3 BioNELL                         GSWHomoNet     MicroPIE                     
       4 BPLLDA                          HIT            MSMCDA                       
       5 brainMI                         IDDAP          MultiDS-MDA                  
       6 BRWLDA                          iLncDA-RSN     OM-GGAT                      
       7 CDA-SKAG                        InfAcrOnt      Oncodomains                  
       8 Cognitive DDx Asst in Rare Dis  PPDAMEGCN      <NA>                         
       9 DDCM: Disease-Drug Corr Method  iPiDA-LTR      PreOptique                   
      10 DHOSGR                          iPiDA-SWGCN    PRISM                        
      11 DLMPM                           RGCNCDA        <NA>                         
      12 Drug-Combo-Generator            LDAGM          RMDGCN                       
      13 eGARD                           LEDAP          SVMDO                        
      14 EHDChat                         M2PP           UPHO: Urban Pop Health Observ
      15 EPOM: Epigen Overlap Measure    MAGCNSE        VRNetzer                     
      16 GCDSAEMDA                       MedGraphNet    <NA>                         

# parse_html_table() `type` arg works

    Code
      parse_html_table("data/html/mattable1.html", type = "mat-table")
    Output
      # A tibble: 26 x 5
         Name                                       AUI   Vocabulary `Term Type` Code 
         <chr>                                      <chr> <chr>      <chr>       <chr>
       1 Acute laryngitis and/or tracheitis         A346~ SNOMEDCT_~ PT          2764~
       2 Acute laryngotracheitis                    A289~ SNOMEDCT_~ PT          6437~
       3 Acute laryngitis and/or tracheitis (disor~ A346~ SNOMEDCT_~ FN          2764~
       4 Acute laryngotracheitis (disorder)         A346~ SNOMEDCT_~ FN          6437~
       5 Acute laryngitis/tracheitis                A309~ SNOMEDCT_~ SY          2764~
       6 Acute laryngotracheitis                    A256~ MDR        LLT         1000~
       7 Laryngo-tracheitis acute NOS               A257~ MDR        LLT         1002~
       8 Laryngotracheitis acute NOS                A257~ MDR        LLT         1002~
       9 Acute laryngotracheitis                    A027~ ICD10      PT          J04.2
      10 Acute laryngotracheitis                    A027~ ICD10AM    PT          J04.2
      # i 16 more rows

---

    Code
      parse_html_table("data/html/mattable1.html", type = "unknown")
    Output
      # A tibble: 26 x 5
         Name                                       AUI   Vocabulary `Term Type` Code 
         <chr>                                      <chr> <chr>      <chr>       <chr>
       1 Acute laryngitis and/or tracheitis         A346~ SNOMEDCT_~ PT          2764~
       2 Acute laryngotracheitis                    A289~ SNOMEDCT_~ PT          6437~
       3 Acute laryngitis and/or tracheitis (disor~ A346~ SNOMEDCT_~ FN          2764~
       4 Acute laryngotracheitis (disorder)         A346~ SNOMEDCT_~ FN          6437~
       5 Acute laryngitis/tracheitis                A309~ SNOMEDCT_~ SY          2764~
       6 Acute laryngotracheitis                    A256~ MDR        LLT         1000~
       7 Laryngo-tracheitis acute NOS               A257~ MDR        LLT         1002~
       8 Laryngotracheitis acute NOS                A257~ MDR        LLT         1002~
       9 Acute laryngotracheitis                    A027~ ICD10      PT          J04.2
      10 Acute laryngotracheitis                    A027~ ICD10AM    PT          J04.2
      # i 16 more rows

---

    Code
      parse_html_table("data/html/table1.html", type = "unknown")
    Output
      # A tibble: 16 x 3
         X1                              X2             X3                           
         <chr>                           <chr>          <chr>                        
       1 Alzheimer Gene Assoc Classifier GeneSurrounder MEDSHARE                     
       2 ANDES                           GGCDA          metaCDA                      
       3 BioNELL                         GSWHomoNet     MicroPIE                     
       4 BPLLDA                          HIT            MSMCDA                       
       5 brainMI                         IDDAP          MultiDS-MDA                  
       6 BRWLDA                          iLncDA-RSN     OM-GGAT                      
       7 CDA-SKAG                        InfAcrOnt      Oncodomains                  
       8 Cognitive DDx Asst in Rare Dis  PPDAMEGCN      <NA>                         
       9 DDCM: Disease-Drug Corr Method  iPiDA-LTR      PreOptique                   
      10 DHOSGR                          iPiDA-SWGCN    PRISM                        
      11 DLMPM                           RGCNCDA        <NA>                         
      12 Drug-Combo-Generator            LDAGM          RMDGCN                       
      13 eGARD                           LEDAP          SVMDO                        
      14 EHDChat                         M2PP           UPHO: Urban Pop Health Observ
      15 EPOM: Epigen Overlap Measure    MAGCNSE        VRNetzer                     
      16 GCDSAEMDA                       MedGraphNet    <NA>                         

# parse_html_table() `n` arg works

    Code
      parse_html_table("data/html/table3.html", type = "unknown", n = 3L)
    Output
      # A tibble: 17 x 3
         X1                              X2             X3                           
         <chr>                           <chr>          <chr>                        
       1 Alzheimer Gene Assoc Classifier GeneSurrounder MEDSHARE                     
       2 ANDES                           GGCDA          metaCDA                      
       3 BioNELL                         GSWHomoNet     MicroPIE                     
       4 BPLLDA                          HIT            MSMCDA                       
       5 brainMI                         IDDAP          MultiDS-MDA                  
       6 BRWLDA                          iLncDA-RSN     OM-GGAT                      
       7 CDA-SKAG                        InfAcrOnt      Oncodomains                  
       8 Cognitive DDx Asst in Rare Dis  IntNetLncSim   ORPheUS                      
       9 CVSARRP                         iPiDA-GCN      PPDAMEGCN                    
      10 DDCM: Disease-Drug Corr Method  iPiDA-LTR      PreOptique                   
      11 DHOSGR                          iPiDA-SWGCN    PRISM                        
      12 DLMPM                           iSG-PDA        RGCNCDA                      
      13 Drug-Combo-Generator            LDAGM          RMDGCN                       
      14 eGARD                           LEDAP          SVMDO                        
      15 EHDChat                         M2PP           UPHO: Urban Pop Health Observ
      16 EPOM: Epigen Overlap Measure    MAGCNSE        VRNetzer                     
      17 GCDSAEMDA                       MedGraphNet    <NA>                         

---

    Code
      parse_html_table("data/html/mattable2.html", type = "unknown", n = 2L)
    Output
      # A tibble: 35 x 5
         Name                                 AUI       Vocabulary  `Term Type` Code  
         <chr>                                <chr>     <chr>       <chr>       <chr> 
       1 Major Depressive Disorder            A7559006  MTH         PN          NOCODE
       2 Depressive Disorder, Major           A6956549  MSH         MH          D0038~
       3 Major Depressive Disorder            A26630752 MSH         ET          D0038~
       4 Clinical Depression                  A36798188 MSH         ET          D0038~
       5 Major depressive disorder            A3548389  SNOMEDCT_US PT          37014~
       6 Major depressive disorder (disorder) A3548382  SNOMEDCT_US FN          37014~
       7 Major depression                     A3322128  SNOMEDCT_US SY          37014~
       8 Major depression                     A2851350  MDR         PT          10057~
       9 Major depressive illness             A25727529 MDR         LLT         10025~
      10 Major depression                     A25689021 MDR         LLT         10057~
      # i 25 more rows

