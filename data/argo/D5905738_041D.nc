CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  
   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-08T08:00:53Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  7p   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8P   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8X   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8\   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  8`   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  8�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  9,   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    9l   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           9t   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    9�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            9�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           9�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           9�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        ;�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        P  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   L0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     P  PD   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     P  d�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  t�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �H   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  �\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �`   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  �t   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   0   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   L   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    T   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        t   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        |   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �(   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �      � �Argo profile    3.1 1.2 19500101000000  20181108080053  20210722160155  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               )   )DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @،��h]�@،��h]�11  @،�����@،�����@5���̷�@5���̷��c��&V���c��&V��11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?L��@ff@9��@y��@���@�  @�33A   A  A!��A>ffA`  A�  A���A���A�ffA���A�33A�33A���B33B��BffB  B   B(  B0  B8ffBA33BH��BP��BW��B_��BhffBpffBxffB�  B�33B�ffB�  B�33B�33B�33B�33B���B�  B���B�  B�  B���B�33B�ffB�33B�33B���B�33B�33B�33Bؙ�B���B�33B�ffB�  B�33B�B�  B�33B���C   C�C33C�fC  C
  C�C�C�C33C�C  C�fC�CffC33C   C"33C$�C%��C(  C*33C,L�C.L�C033C1�fC4  C633C8�C9�fC<  C>�C@33CBL�CD�CE�fCH�CJ  CK��CN  CP�CR  CS��CU�fCX�CZ  C[��C^�C_�fCa�fCd  Ce�fCg�fCj33Cl�Cm�fCp�Cr  Cs�fCv�CxL�Cz33C|  C~L�C��C�  C�  C��3C��C��C��C��C��3C�&fC��C��C�  C��3C��C��C�  C��C��C�  C�&fC�&fC��C�  C�  C��3C��fC�  C��3C��fC��C�&fC��C�  C�&fC��C�  C��C��C��3C�&fC��C��3C��C�&fC��C��fC��3C�  C�  C��C�&fC�&fC��C��fC��fC��3C��C��C��C��C��fC��3C��3C�  C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C�  C�  C��C��C��C��C��C�&fC�&fC��C��fC��fC��fC��3C�  C�  C��C��C��C�  C��C��C��3C�  C�  C�  C��C��C�&fC�&fC��C�ٚC��fC��3C��3C�  C��C�&fC��C��fDL�D�fDFfD	�3DffDfD��D3D��D�D�3D33D � D#l�D&3D(� D+ffD.3D0�3D3�fD6�D8� D;33D=��D@@ DB��DEfDGl�DIٚDLFfDN� DQ,�DS� DVfDXl�DZ��D]L�D_�fDbS3Dd� DgS3Di��DlFfDn��Dq�Dss3Du�fDx  DzffD|FfD~��D�c3D�y�D���D�� D��3D��fD�3D�&fD�<�D�\�D�� D���D��fD���D�)�D�VfD�y�D��fD��fD�fD�)�D�P D�s3D���D���D�ٚD��3D��D�,�D�L�D�i�D���D���D���D��fD��D�@ D�` D�� D���D���D��3D�ٚD��3D�	�D��D�6fD�P D�ffD��fD��fD�ɚD��D�3D�<�D�i�D���D��fD��D�)�D�` Dœ3D���D��fD�  D�L�D˃3D̬�D��3D�3D�C3D�|�Dҹ�D��3D�  D�\�D׌�D�� D��fD�	�D�,�D�P D�s3DߖfD๚D��3D��D�3D��D�,�D�I�D�i�D�|�D� D�3D� D���D��D�fD� D�D� D�3D�fD�� D�y�D�p D�i�D�` D�Y�D�9�D�,�D��D�fD��fD�� E k3E �3EY�E��EL�EA�E4�E�fE�E3E	vfE
i�E�fE>fE8 E��E��E6fE@ E� E��EP EL�E�3E�3EVfE[3E��EњE!NfE"FfE#��E%3E%��E'4�E(|�E)��E*��E,1�E-nfE.��E/�E1|�E2��E4  E4�3E63E7c3E8�fE9�E;( E<h E?�3EB� EEٚEI4�EL#3EOT�ERnfEUi�EX� E[�fE^�3Ea��Ee$�Eh Ek��Eny�Eq�3Et�3Ew� Ez��E~	�E��3E��E��fE�
fE�T E���E���E�M�E��3E���E�=�E���E��E�!�E�� E���E�3E�X E���E�3E�L�E��3E��E�:fE��fE��3?333?   ?��?��?��?   ?��?��?333?333?333?333?333?333?L��?fff?�  ?���?�ff?�33?�  ?ٙ�?�33@��@��@&ff@@  @L��@fff@y��@�ff@�33@���@�ff@�33@�  @���@���@陚@�ffA33A33A33A��A!��A)��A333A9��AA��AK33AS33A[33Ac33Al��At��A|��A�ffA���A�ffA�ffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414441414444411111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ?�ff@&ff@Y��@���@���@�  @�33A  A  A)��AFffAh  A�  A���A���A�ffA���A�33A�33A���B33B
��BffB  B"  B*  B2  B:ffBC33BJ��BR��BY��Ba��BjffBrffBzffB�  B�33B�ffB�  B�33B�33B�33B�33B���B�  B���B�  B�  B���B�33B�ffB�33B�33B���B�33B�33B�33Bٙ�B���B�33B�ffB�  B�33B�B�  B�33B���C � C��C�3CffC� C
� C��C��C��C�3C��C� CffC��C�fC�3C � C"�3C$��C&L�C(� C*�3C,��C.��C0�3C2ffC4� C6�3C8��C:ffC<� C>��C@�3CB��CD��CFffCH��CJ� CLL�CN� CP��CR� CTL�CVffCX��CZ� C\L�C^��C`ffCbffCd� CfffChffCj�3Cl��CnffCp��Cr� CtffCv��Cx��Cz�3C|� C~��C�Y�C�@ C�@ C�33C�Y�C�Y�C�L�C�L�C�33C�ffC�Y�C�L�C�@ C�33C�Y�C�Y�C�@ C�Y�C�Y�C�@ C�ffC�ffC�L�C�@ C�@ C�33C�&fC�@ C�33C�&fC�L�C�ffC�Y�C�@ C�ffC�Y�C�@ C�Y�C�Y�C�33C�ffC�L�C�33C�L�C�ffC�L�C�&fC�33C�@ C�@ C�Y�C�ffC�ffC�L�C�&fC�&fC�33C�L�C�Y�C�Y�C�L�C�&fC�33C�33C�@ C�L�C�L�C�Y�C�Y�C�Y�C�L�C�L�C�L�C�L�C�@ C�L�C�L�C�Y�C�Y�C�Y�C�L�C�Y�C�L�C�L�C�@ C�@ C�L�C�Y�C�Y�C�Y�C�Y�C�ffC�ffC�L�C�&fC�&fC�&fC�33C�@ C�@ C�L�C�L�C�Y�C�@ C�L�C�L�C�33C�@ C�@ C�@ C�Y�C�Y�C�ffC�ffC�L�C��C�&fC�33C�33C�@ C�L�C�ffC�L�C�&fDl�D�fDffD	�3D�fD&fD��D33D��D9�D�3DS3D � D#��D&33D(� D+�fD.33D0�3D3�fD6,�D8� D;S3D=ٚD@` DB��DE&fDG��DI��DLffDN� DQL�DS� DV&fDX��DZ��D]l�D_�fDbs3De  Dgs3Di��DlffDn��Dq9�Ds�3Du�fDx@ Dz�fD|ffD~��D�s3D���D���D�� D��3D��fD�3D�6fD�L�D�l�D�� D���D��fD��D�9�D�ffD���D��fD��fD�fD�9�D�` D��3D���D�ɚD��D�3D��D�<�D�\�D�y�D���D���D���D�fD�)�D�P D�p D�� D���D���D��3D��D�3D��D�,�D�FfD�` D�vfD��fD��fD�ٚD���D�#3D�L�D�y�D���D��fD���D�9�D�p Dţ3D���D�fD�0 D�\�D˓3D̼�D��3D�#3D�S3Dь�D�ɚD�3D�0 D�l�Dל�D�� D��fD��D�<�D�` Dރ3DߦfD�ɚD��3D���D�3D�)�D�<�D�Y�D�y�D��D� D�3D� D���D��D�fD� D�D� D�3D��fD�� D���D�� D�y�D�p D�i�D�I�D�<�D�,�D�fD�fD�� E s3E �3Ea�E��ET�EI�E<�E�fE�E3E	~fE
q�E�fEFfE@ E��E��E>fEH E� E��EX ET�E�3E�3E^fEc3E��EٚE!VfE"NfE#��E%3E%��E'<�E(��E)��E+�E,9�E-vfE.��E/�E1��E2��E4 E4�3E6#3E7k3E8�fE9�E;0 E<p E?�3EB� EE�EI<�EL+3EO\�ERvfEUq�EX� E[�fE^�3Eb�Ee,�Eh  Ek��En��Eq�3Et�3Ew� Ez��E~�E��3E��E��fE�fE�X E���E���E�Q�E��3E���E�A�E���E��E�%�E�� E���E�3E�\ E���E�3E�P�E��3E��E�>fE��fE��3G�O�?�  G�O�G�O�G�O�?�  G�O�?���G�O�G�O�G�O�G�O�G�O�?���?�ff?�33?�  ?���?�ff?�33@   @��@��@,��@9��@Fff@`  @l��@�33@���@�ff@�33@���@�ff@�33@�  @���@���@���A33A33A33A33A!��A)��A1��A;33AA��AI��AS33A[33Ac33Ak33At��A|��A�ffA�ffA���A�ffA�ffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414441414444411111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                              @ v@ V@ �@ �@ !s@ (�@ 0x@ 6�@ =q@ D�@ Q=@ _�@ m:@ {�@ ��@ �<@ ��@ ��@ �w@ �|@ ܀@ �y@ �q@j@@�@,`@:�@I�@V�@dZ@o�@}�@��@�H@��@��@�>@є@��@�4@��@�@*@!s@/�@<�@K@X�@e�@t�@�@�@��@��@�@ƨ@�O@�T@�@@�E@�@�@&;@5?@@�@O0@^5@i�@x&@�|@�@�m@�@�k@�@׹@�@�@  @�@�@+�@7�@DD@SI@`B@k�@z�@��@�<@��@��@�w@��@��@��@�@j@�@ @.l@:�@F�@V@b�@oF@~K@��@��@��@�9@�>@�7@܀@�4@�~@%@{@!s@/@>�@K�@X@g@t@�@�@�@�@�R@�@��@�H@��@��@�@B@&;@3�@@,@P�@]�@j@ww@�p@�$@��@�@�@��@�
@�@�e@ �@�@O@(G@5?@DD@Q=@^5@m�@|�@��@�0@��@��@�&@�*@��@�m@��@	@	b@	g@	.l@	:�@	FQ@	T�@	b�@	p�@	�@	��@	��@	��@	��@	�2@	ψ@	ލ@	��@	��@
�@
@
!s@
/@
=q@
K�@
Yn@
g�@
uk@
�@
�@
��@
�Y@
�@
��@
�O@
��@
�L@
��@�@�@&�@3�@A�@N�@\)@j@x�@�|@�$@��@�!@��@�@խ@�T@��@�Q@�@O@)�@7L@E�@Q�@`B@m�@z3@��@�0@��@��@��@��@܀@��@�@@b@
@,`@:�@I�@V@a�@�@)�@m�@��@��@A�@��@�o@@T�@��@��@%�@n�@��@  @H]@�h@܀@&;@k.@�~@��@<�@��@��@@E�@��@�@�@O�@��@�O@�@V�@��@ލ@$.@i�@��@�L@3�@uk@��@��@7L@ww@��@��@&�@c�@�@��@�@V@��@�|@�@F�@�p@@@B8@�@�2@]@?}@�@��@ ]@ ?}@ ~K@ �j@ �9@!8�@!v@!�-@!�@@"+�@"i!@"��@"�T@#!s@#^�@#�a@#܀@$O@$X�@$�0@$�C@%J@%H]@%��@%��@%�9@&5�@&r@&�@&�y@'&�@'dZ@'�z@'��@(g@(^�@(�@(�/@)�@)^5@)�z@)�@*&;@*i!@*��@*�@+(G@+j@+��@+�4@,-@,m�@,��@,�e@-7L@-ww@-�@-��@.=q@.|?@.�^@.�~@/6�@/t�@/��@/��@0-@0hs@0��@0��@1�@1Wb@1��@1ψ@2
=@2A�@2z�@2�9@2��@3 @3UU@3��@3�&@3�q@4*S@4_�@4��@4�c@4��@533@5hs@5�<@5�@5�Q@61'@6dZ@6�0@6��@6��@70x@7e	@7�<@8 �@8hs@9�@9�@:�@:��@;V@;��@<C�@<�@=M$@=�k@>bN@>Ӡ@?ww@?�@@�\@@�9@A��@B@B�9@C#�@CĜ@D0x@D��@E<�@E�O@Fg�@F�@GWb@G�T@Hj@H�@Ix&@I�Q@J��@K
�@K�^@LB�@L��@M*S@M��@N>�@Nȴ@OR�@O�t@Pb�@Q�h@S""@Tj@U�@WB@Xv@Yȴ@[V@\m:@]ψ@_�@`k�@aĜ@c�@d{�@e��@go@hg�@i��@k@lc�@m��@o�@pk�@p��@p�@q)�@qg@q��@q��@r7L@r�P@rƨ@sC@sO�@s�m@s��@t$�@tX�@t��@t�@u)�@uv�@u��@u�e@v?}@vs_G�O�@ jG�O�G�O�G�O�@ jG�O�@ G�O�G�O�G�O�G�O�G�O�@ �@ v@ %@ �@ �@ �@ 	�@ 
=@ �@ �@ @ b@ �@ {@ �@ �@ �@ �@ g@ !s@ #�@ &;@ (�@ +�@ /@ 1�@ 4�@ 7�@ ;d@ >�@ A�@ D�@ H]@ Lu@ O0@ R�@ V�@ Z@ ]�@ `�@ e	@ hs@ k�@ oF@ r@ v@ y�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�jA�n�A�n�A�l�A�l�A�x�A�|�A�t�A�|�A�z�A�z�A�|�A�|�A�|�AԁAԅAԅAԅAԇ+AԅAԉ7AԋDAԏ\Aԏ\Aԏ\Aԕ�Aԕ�Aԕ�Aԝ�Aԛ�Aԙ�Aԉ7A�S�A��A�1'A��A���Aˡ�A�G�A���A�;dA�&�A�XA���A�t�A�jA�p�A��yA�7LA�|�A�VA�A�{A�A�~�A�
=A���A���A�?}A��A�\)A��A�A�hsA���A���A��
A�K�A�ƨA��A�A�;dA���A��yA�z�A���A�E�A�bNA�r�A�$�A��!A�E�A���A��/A��A��^A��A���A�O�A�z�A��HA��hA�9XA��A��jA��jA���A�A�A�K�A��;A��A���A��jA��A�jA�=qA�{A�9XA{hsAy�;Ayl�AxȴAu&�Aq�^Aq�PAq�AqC�AoVAnbNAn=qAml�AkK�Aj�!Ai�Af�`Ac�A`E�A]�AZ�/AX�`AW��AR�jARE�AR1'AO�hAN-AL  AJ{AH�AH5?AF  ACG�AB�!ABjABE�AB�A@^5A>��A<��A;��A;�A9�A7�A77LA65?A5�7A4�HA4$�A4�A3��A3+A2z�A1�A0ZA/�A.�HA.�+A.bNA-�A-O�A,�\A,(�A+�PA*�A*5?A)�#A)"�A'p�A&�A&M�A%33A#K�A"=qA�;A33A��Ax�AAO�A��A��A��A;dA9XAp�An�A�AO�AQ�A�7A�yAz�Al�A�uA�-A�RAVA�
A/A�A�`A�jAƨA
�DA	�PA�A�\AK�AƨA�AE�A��A^5A�TA|�AVA �A z�@��w@��@��R@��#@�7L@��@��w@�
=@�7L@��w@�+@�v�@�X@�z�@�K�@���@��#@�&�@��m@�@���@�ƨ@�V@�J@�7@��@�I�@� �@��@�  @۾w@�C�@�1@���@��@�9X@ũ�@�b@�`B@�@���@�J@�A�@��^@�@� �@�ff@��@���@�v�@�?}@��y@���@���@�~�@��@�l�@�~�@�j@�S�@�v�@�@�%@�r�@���@�^5@���@�V@�\)@���@�V@���@��@�ƨ@���@�{@�hs@�&�@�Q�@�ƨ@�o@�$�@���@���@���@+@~��@{o@yhs@wl�@tZ@r~�@o��@m/@i�7@g�@e��@c�m@a��@`bN@^ȴ@]��@]V@\j@[�@[��@Y�#@Y7L@V��@S��@R�@P1'@NE�@MV@KdZ@IX@H�9@G�;@F�+@D��@C��@A��@@ �@?;d@<I�@;@9x�@8Q�@7��@6�@65?@5`B@3�F@2^5@1�#@1hs@0��@0r�@.ff@.{@,I�@+ƨ@*n�@*=q@)�^@(b@&��@%�h@$��@$I�@#S�@"-@ ��@ b@K�@�@��@�-@��@j@dZ@~�@��@Ĝ@Q�@+@v�@�-@/@Z@ƨ@S�@J@��@A�@�P@v�@�+@/@Z@�@
��@	��@	��@	x�@��@A�@;d@��@p�@I�@�
@t�@�H@=q@��@7L@ Ĝ@ A�?�\)?��?��?���?�ff?���?�J?��;?�R?��?��?�?��H?��?�K�?�E�?��?㕁?�n�?�  ?���?ܬ?�(�?���?���?�Q�?׮?�?Ձ?��?�z�?��?��?�Ĝ?ϝ�?�5??�p�?�I�?�C�?�"�?ʟ�?ə�?���?�+?�ȴ?Ƈ+?�ȴ?�?�S�?�M�?�hs?���?�\)?��-?�O�?��D?�C�?�"�?���?�"�?�~�?�^5?���?�7L?�7L?�x�?��#?�=q?���?�dZ?��m?�I�?�j?��D?��D?��D?��?���?��?�/?�O�?�p�?��h?���?��?��?�{?�5??�5??�5??�V?�V?���?��R?��A�^5A�bNA�n�A�hsA�p�A�jA�n�A�n�A�jA�l�A�l�A�jA�ffA�ffA�hsA�jA�l�A�l�A�l�A�n�A�r�A�p�A�jA�n�A�p�A�p�A�p�A�l�A�jA�l�A�l�A�jA�n�A�l�A�r�A�x�A�~�A�~�A�x�A�r�A�t�A�~�A�z�A�x�A�z�A�z�A�|�A�z�A�x�A�z�A�|�A�|�A�z�A�|�A�|�A�|�A�z�A�z�A�|�A�z�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                              A�jA�n�A�n�A�l�A�l�A�x�A�|�A�t�A�|�A�z�A�z�A�|�A�|�A�|�AԁAԅAԅAԅAԇ+AԅAԉ7AԋDAԏ\Aԏ\Aԏ\Aԕ�Aԕ�Aԕ�Aԝ�Aԛ�Aԙ�Aԉ7A�S�A��A�1'A��A���Aˡ�A�G�A���A�;dA�&�A�XA���A�t�A�jA�p�A��yA�7LA�|�A�VA�A�{A�A�~�A�
=A���A���A�?}A��A�\)A��A�A�hsA���A���A��
A�K�A�ƨA��A�A�;dA���A��yA�z�A���A�E�A�bNA�r�A�$�A��!A�E�A���A��/A��A��^A��A���A�O�A�z�A��HA��hA�9XA��A��jA��jA���A�A�A�K�A��;A��A���A��jA��A�jA�=qA�{A�9XA{hsAy�;Ayl�AxȴAu&�Aq�^Aq�PAq�AqC�AoVAnbNAn=qAml�AkK�Aj�!Ai�Af�`Ac�A`E�A]�AZ�/AX�`AW��AR�jARE�AR1'AO�hAN-AL  AJ{AH�AH5?AF  ACG�AB�!ABjABE�AB�A@^5A>��A<��A;��A;�A9�A7�A77LA65?A5�7A4�HA4$�A4�A3��A3+A2z�A1�A0ZA/�A.�HA.�+A.bNA-�A-O�A,�\A,(�A+�PA*�A*5?A)�#A)"�A'p�A&�A&M�A%33A#K�A"=qA�;A33A��Ax�AAO�A��A��A��A;dA9XAp�An�A�AO�AQ�A�7A�yAz�Al�A�uA�-A�RAVA�
A/A�A�`A�jAƨA
�DA	�PA�A�\AK�AƨA�AE�A��A^5A�TA|�AVA �A z�@��w@��@��R@��#@�7L@��@��w@�
=@�7L@��w@�+@�v�@�X@�z�@�K�@���@��#@�&�@��m@�@���@�ƨ@�V@�J@�7@��@�I�@� �@��@�  @۾w@�C�@�1@���@��@�9X@ũ�@�b@�`B@�@���@�J@�A�@��^@�@� �@�ff@��@���@�v�@�?}@��y@���@���@�~�@��@�l�@�~�@�j@�S�@�v�@�@�%@�r�@���@�^5@���@�V@�\)@���@�V@���@��@�ƨ@���@�{@�hs@�&�@�Q�@�ƨ@�o@�$�@���@���@���@+@~��@{o@yhs@wl�@tZ@r~�@o��@m/@i�7@g�@e��@c�m@a��@`bN@^ȴ@]��@]V@\j@[�@[��@Y�#@Y7L@V��@S��@R�@P1'@NE�@MV@KdZ@IX@H�9@G�;@F�+@D��@C��@A��@@ �@?;d@<I�@;@9x�@8Q�@7��@6�@65?@5`B@3�F@2^5@1�#@1hs@0��@0r�@.ff@.{@,I�@+ƨ@*n�@*=q@)�^@(b@&��@%�h@$��@$I�@#S�@"-@ ��@ b@K�@�@��@�-@��@j@dZ@~�@��@Ĝ@Q�@+@v�@�-@/@Z@ƨ@S�@J@��@A�@�P@v�@�+@/@Z@�@
��@	��@	��@	x�@��@A�@;d@��@p�@I�@�
@t�@�H@=q@��@7L@ Ĝ@ A�?�\)?��?��?���?�ff?���?�J?��;?�R?��?��?�?��H?��?�K�?�E�?��?㕁?�n�?�  ?���?ܬ?�(�?���?���?�Q�?׮?�?Ձ?��?�z�?��?��?�Ĝ?ϝ�?�5??�p�?�I�?�C�?�"�?ʟ�?ə�?���?�+?�ȴ?Ƈ+?�ȴ?�?�S�?�M�?�hs?���?�\)?��-?�O�?��D?�C�?�"�?���?�"�?�~�?�^5?���?�7L?�7L?�x�?��#?�=q?���?�dZ?��m?�I�?�j?��D?��D?��D?��?���?��?�/?�O�?�p�?��h?���?��?��?�{?�5??�5??�5??�V?�V?���?��R?��A�^5A�bNA�n�A�hsA�p�A�jA�n�A�n�A�jA�l�A�l�A�jA�ffA�ffA�hsA�jA�l�A�l�A�l�A�n�A�r�A�p�A�jA�n�A�p�A�p�A�p�A�l�A�jA�l�A�l�A�jA�n�A�l�A�r�A�x�A�~�A�~�A�x�A�r�A�t�A�~�A�z�A�x�A�z�A�z�A�|�A�z�A�x�A�z�A�|�A�|�A�z�A�|�A�|�A�|�A�z�A�z�A�|�A�z�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�)B�)B�)B�;B�ZBB�B;dB`BBr�B�B��B��B�'B�9B�FB�9B�'B�B�'B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�bB�VB�%B�+B}�Bu�BaHB]/B]/B^5BZBR�B@�B?}B33B �B�B�5B��BĜB�3B��B|�BcTBZBH�B;dB5?B/B$�B�B1BB
�B
��B
��B
�B
��B
�\B
|�B
hsB
cTB
`BB
M�B
2-B
+B
&�B
!�B
B	��B	��B	�B	�B	�TB	�yB	�fB	�;B	��B	��B	ɺB	�FB	��B	�oB	�B	m�B	]/B	S�B	%�B	5?B	7LB	0!B	"�B	�B	JB	%B	%B��B��B�B�B�B�yB�;B�/B��B��B��BÖB�qB�XB�RB�FB�'B�'B�!B�B�B��B��B��B��B��B��B��B�oB�VB�DB�1B�%B�B� B~�By�Bx�Bu�Br�Bm�Bl�BiyBcTB_;B]/B\)BYBVBS�BO�BM�BK�BK�BJ�BI�BJ�BD�BG�BD�BH�BF�BB�BB�B?}B?}B@�B?}B?}B?}B>wB=qB<jB:^B;dB:^B8RB7LB7LB6FB6FB49B5?B49B49B49B33B2-B33B2-B1'B1'B1'B1'B1'B0!B1'B2-B2-B1'B33B49B5?B6FB7LB7LB8RB;dB<jB<jBC�BF�BG�BE�BG�BM�B��B��B��B��B�-B�^B��B�9B�B��B��B��B�wB�jB�RBɺB��B�BB��B	uB	�B	�B	)�B	@�B	\)B	hsB	r�B	z�B	�7B	�PB	��B	�B	�9B	�LB	�jB	�}B	B	ƨB	ȴB	��B	�B	�B	�)B	�;B	�ZB	�`B	�mB	�B	�B	��B	��B	��B	��B	��B	��B
  B
B
B
B
1B

=B
JB
bB
bB
oB
{B
�B
�B
�B
�B
�B
 �B
"�B
#�B
#�B
#�B
$�B
'�B
)�B
-B
/B
1'B
2-B
5?B
49B
49B
6FB
8RB
8RB
9XB
;dB
=qB
>wB
@�B
A�B
A�B
F�B
F�B
G�B
I�B
J�B
L�B
L�B
M�B
N�B
P�B
Q�B
P�B
Q�B
Q�B
VB
T�B
VB
W
B
W
B
W
B
XB
ZB
ZB
[#B
ZB
[#B
\)B
\)B
^5B
_;B
_;B
_;B
aHB
`BB
bNB
bNB
dZB
dZB
e`B
ffB
gmB
hsB
hsB
jB
jB
k�B
k�B
l�B
n�B
o�B
o�B
p�B
q�B
p�B
r�B
r�B
s�B
t�B
v�B
u�B
u�B
v�B
v�B
x�B
y�B
z�B
{�B
|�B
}�B
}�B
}�B
~�B
~�B
~�B
�B
�B
�B
�B
�%B
�1B
�+B
�7B
�DB
�PB
�PB
�VB
�\B
�VB
�\B
�hB
�oB
�uB
�{B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
�B
�B
�B
�B
�'B
�'B
�3B
�3B
�9B
�?B
�FB
�FB
�FB
�LB
�RB
�^B
�XB
�^B
�^B
�^B
�dB
�dB
�^B
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�jB
�dB
�^B
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�^B
�dB
�dB
�dB
�jB
�dB
�jB
�dB�#B�B�B�B�B�B�B�B�B�B�B�B�B�B�#B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�#B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                              B�
B�
B�B�B�B�B�B�
B�B�B�B�B�B�B�B�B�B�B�B�B�B�
B�B�B�B�B�
B�B�B�B�B�/B�NBB�B9XB^5Bp�B�B��B��B�B�-B�9B�-B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�VB�JB�B�B{�Bs�B_;B[#B[#B\)BXBP�B>wB=qB1'B�B�B�)B��BB�'B��Bz�BaHBXBF�B9XB33B-B"�B{B%B  B
�B
��B
�wB
�B
��B
�PB
z�B
ffB
aHB
^5B
K�B
0!B
(�B
$�B
�B
B	��B	�B	�B	�B	�HB	�mB	�ZB	�/B	��B	��B	ǮB	�9B	��B	�bB	�B	k�B	[#B	Q�B	#�B	33B	5?B	.B	 �B	�B	
=B	B	B��B�B�B�B�B�mB�/B�#B��B��B��B��B�dB�LB�FB�9B�B�B�B�B��B��B��B��B��B��B��B�{B�bB�JB�7B�%B�B�B}�B|�Bw�Bv�Bs�Bp�Bk�BjBgmBaHB]/B[#BZBW
BS�BQ�BM�BK�BI�BI�BH�BG�BH�BB�BE�BB�BF�BD�B@�B@�B=qB=qB>wB=qB=qB=qB<jB;dB:^B8RB9XB8RB6FB5?B5?B49B49B2-B33B2-B2-B2-B1'B0!B1'B0!B/B/B/B/B/B.B/B0!B0!B/B1'B2-B33B49B5?B5?B6FB9XB:^B:^BA�BD�BE�BC�BE�BK�B��B��B�uB��B�!B�RB�}B�-B�B��B��B��B�jB�^B�FBǮB��B�5B��B	hB	�B	�B	'�B	>wB	ZB	ffB	p�B	x�B	�+B	�DB	��B	�B	�-B	�?B	�^B	�qB	��B	ĜB	ƨB	��B	��B	�B	�B	�/B	�TB	�ZB	�fB	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
B
B
+B
	7B
DB
\B
\B
hB
uB
�B
�B
�B
�B
�B
�B
!�B
"�B
"�B
"�B
#�B
&�B
(�B
,B
.B
0!B
1'B
49B
33B
33B
5?B
7LB
7LB
8RB
:^B
<jB
=qB
?}B
@�B
@�B
E�B
E�B
F�B
H�B
I�B
K�B
K�B
L�B
M�B
O�B
P�B
O�B
P�B
P�B
T�B
S�B
T�B
VB
VB
VB
W
B
YB
YB
ZB
YB
ZB
[#B
[#B
]/B
^5B
^5B
^5B
`BB
_;B
aHB
aHB
cTB
cTB
dZB
e`B
ffB
gmB
gmB
iyB
iyB
k�B
k�B
l�B
n�B
o�B
o�B
p�B
q�B
p�B
r�B
r�B
s�B
t�B
v�B
u�B
u�B
v�B
v�B
x�B
y�B
z�B
{�B
|�B
}�B
}�B
}�B
~�B
~�B
~�B
�B
�B
�B
�B
�%B
�1B
�+B
�7B
�DB
�PB
�PB
�VB
�\B
�VB
�\B
�hB
�oB
�uB
�{B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
��B
�B
�B
�B
�B
�B
�!B
�-B
�-B
�9B
�9B
�?B
�FB
�LB
�LB
�RB
�XB
�^B
�jB
�dB
�jB
�jB
�jB
�qB
�qB
�jB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�wB
�qB
�jB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�jB
�qB
�qB
�qB
�wB
�qB
�}B
�wB�B�
B��B�
B�B�
B�B��B�B�
B�B�
B�
B�
B�B�
B�
B�
B�
B�
B�B�B�
B�
B�
B�B�B�
B�B�
B�B�B�B�
B�
B�B�B�B��B�B�B�B�B�
B�B�B�B�B�B�
B�
B�B�B�B�B�B�
B�B�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                              <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201811080800532021061413525320210614135253202106141746522021061417465220210614174652201811080800532021061413525320210614135253202106141746522021061417465220210614174652PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                    surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018110808005320181108080053  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018110808005320181108080053QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018110808005320181108080053QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015520210722160155IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                