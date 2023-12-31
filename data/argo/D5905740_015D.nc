CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-08-05T07:03:40Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  7d   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8D   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  8�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  9    WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    9`   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           9h   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    9x   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            9|   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
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
resolution        =���   axis      Z        0  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   L   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     0  P   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     0  dL   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  t|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  �$   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  �`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ѐ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  ՜   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   4   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   <   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar           HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar            HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       (   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �t   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �t   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �t   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � 
tArgo profile    3.1 1.2 19500101000000  20180805070340  20210722161420  5905740 5905740 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7220                            7220                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12003                           12003                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�o�~�F*@�o�~�F*11  @�o�}'�@�o�}'�@*p�$ q@*p�$ q�cH�����cH����11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?L��?�33@@  @�33@�  @�  @�  A��A  A#33A>ffA`  A�  A���A���A�  A���A���A�  A�  B ��BffB��B  B   B'��B0  B8ffB@ffBH��BPffBXffB`ffBhffBpffBxffB�33B�33B�33B�ffB�ffB�ffB�ffB�ffB�33B�33B�  B���B�33B���B�ffB�ffB�33B�33B���B�  B�ffBԙ�B�ffBܙ�B���B���B�33B뙚B���B�33B���B�  C L�C33C  C�3C  C
L�C33C33C  C  C�fC��C�CffCL�C�C   C!�fC$33C&� C(ffC*33C,  C-��C0  C233C4  C6�C833C9�fC;�fC>  C@  CB�CC�fCF  CG�fCJ33CL33CN  CP33CR  CS�fCV�CX33CZ33C\L�C^L�C`L�Cb33Cd33Cf�Ch�Cj33Cl33Cn�Cp33Cr�Ct�Cv  Cx�Cz�C|�C~�C��C�  C��C��C��C��C��C��C�&fC�&fC�  C�ٚC��fC��3C�  C��C�&fC��C��fC�  C��C�&fC��C��3C��C�&fC��C�  C��C�  C��fC��3C��C��C�  C��C��C��C��C��C�  C�  C��C��C��3C��C�&fC�&fC��C��C�  C��C��3C��fC�  C��fC�ٚC�  C��C�  C��fC��C��C��C��fC��C�  C�ٚC��3C��C�&fC�33C�&fC��C�&fC��C�  C��C�  C��fC�  C�  C��fC��C��C�  C�&fC��C��3C��C��C��3C��C��C��3C��C�  C��3C��C�  C��fC��C�&fC��C�  C��C��C�  C�&fC��C�  C�&fC��C��3C��C�&fC��C�  C��C��C��3C�  C��3DffD3DٚD
��D9�D��D� D� D  DٚD� D Y�D#�D%� D(� D+FfD-��D0��D3ffD63D8��D;�3D>9�D@�3DC�3DF` DI3DK��DNy�DQ�DS��DVl�DX�3D[y�D^�D`�fDb��De�fDh  Dj�fDm9�Do�fDrL�Dt�3Dw�3Dz&fD|Y�D~�fD��fD�	�D�P D���D��3D�)�D�vfD�ɚD��D�c3D���D���D�L�D��fD���D�I�D���D��3D�I�D��3D�3D�` D��3D�&fD�� D���D�9�D�� D�fD�l�D�ٚD�6fD��fD��3D�S3D���D�fD�i�D�� D�3D�ffD���D�	�D�` D��3D�  D�<�D���D�ɚD��D�FfDĀ DŹ�D���D�  D�Y�Dʉ�D�� D��3D�#3D�L�DІfDѼ�D��3D�#3D�L�D�y�Dװ D��3D��D�\�Dܐ D�ɚD�fD�C3D�y�D�3D��fD�#3D�` D�fD�� D�3D�9�D�l�D��D���D��D�33D�c3D� D���D��fD��D�FfD�� D��fD�� D��fD� D�33E .fE �fEVfE��E��E�E��E6fEɚE\�E� E�fE$�E� EL�E�3E	t�E
	�E
��E4�E�fEd�E� E��E� EfE� E�3E� E3E� E��E� EVfE` E� E� E k3E!~fE"��E$�E%&fE&4�E'�fE(�3E*p E+�3E,��E.( E/0 E08 E1�3E2ٚE4s3E5� E6� E8,�E9;3E:L�E;� E>�3EA�fED�EG�fEK�EN��EQ��ETl�EWɚE[3E^�Ea Ed\�Eg)�EjK3Emh Ep��Es� EwfEz�E}NfE� E�� E�X E�ŚE�e�E� E�|�E� E���E�<�E��3E�h�E��3E��fE��fE���E�)�E��3E�0 E���E�jfE��3E��3E�  E��3E�3E�� E�D E��fE�� E�5�E���E��?333?333?��?fff?L��?333?333?333?333?333?L��?L��?L��?333?333?fff?   ?L��?L��?L��?fff?�  ?���?���?�33?�  ?���?�ff?�33@��@&ff@,��@@  @Fff@`  @y��@�  @���@�ff@�  @���@�33@�  @���@�ff@���@���@���A��A33A33A33A#33A+33A333A;33AA��AK33AS33A\��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441444444144441414411111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ?�ff@��@`  @�33@�  @�  @�  A	��A  A+33AFffAh  A�  A���A���A�  A���A���A�  A�  B��B
ffB��B  B"  B)��B2  B:ffBBffBJ��BRffBZffBbffBjffBrffBzffB�33B�33B�33B�ffB�ffB�ffB�ffB�ffB�33B�33B�  B���B�33B���B�ffB�ffB�33B�33B���B�  B�ffBՙ�B�ffBݙ�B���B���B�33B왚B���B�33B���B�  C ��C�3C� C33C� C
��C�3C�3C� C� CffCL�C��C�fC��C��C � C"ffC$�3C'  C(�fC*�3C,� C.L�C0� C2�3C4� C6��C8�3C:ffC<ffC>� C@� CB��CDffCF� CHffCJ�3CL�3CN� CP�3CR� CTffCV��CX�3CZ�3C\��C^��C`��Cb�3Cd�3Cf��Ch��Cj�3Cl�3Cn��Cp�3Cr��Ct��Cv� Cx��Cz��C|��C~��C�L�C�@ C�L�C�L�C�L�C�Y�C�Y�C�Y�C�ffC�ffC�@ C��C�&fC�33C�@ C�Y�C�ffC�L�C�&fC�@ C�L�C�ffC�L�C�33C�L�C�ffC�L�C�@ C�Y�C�@ C�&fC�33C�L�C�L�C�@ C�Y�C�Y�C�Y�C�L�C�L�C�@ C�@ C�Y�C�L�C�33C�L�C�ffC�ffC�L�C�L�C�@ C�L�C�33C�&fC�@ C�&fC��C�@ C�L�C�@ C�&fC�L�C�Y�C�L�C�&fC�L�C�@ C��C�33C�L�C�ffC�s3C�ffC�L�C�ffC�Y�C�@ C�L�C�@ C�&fC�@ C�@ C�&fC�L�C�L�C�@ C�ffC�Y�C�33C�Y�C�L�C�33C�Y�C�L�C�33C�L�C�@ C�33C�L�C�@ C�&fC�L�C�ffC�Y�C�@ C�Y�C�Y�C�@ C�ffC�L�C�@ C�ffC�L�C�33C�L�C�ffC�L�C�@ C�Y�C�L�C�33C�@ C�33D�fD33D��D
��DY�D�D� D� D@ D��D� D y�D#,�D%� D(� D+ffD.�D0ٚD3�fD633D8��D;�3D>Y�DA3DC�3DF� DI33DK��DN��DQ9�DSٚDV��DY3D[��D^,�D`�fDc�De�fDh@ Dj�fDmY�Do�fDrl�Du3Dw�3DzFfD|y�DfD��fD��D�` D���D��3D�9�D��fD�ٚD�)�D�s3D���D�	�D�\�D��fD���D�Y�D���D�3D�Y�D��3D�3D�p D��3D�6fD�� D���D�I�D�� D�fD�|�D��D�FfD��fD�3D�c3D�ɚD�&fD�y�D�� D�#3D�vfD�ɚD��D�p D��3D� D�L�D���D�ٚD��D�VfDĐ D�ɚD���D�0 D�i�Dʙ�D�� D�3D�33D�\�DЖfD���D�3D�33D�\�D։�D�� D��3D�,�D�l�Dܠ D�ٚD�fD�S3DቚD��3D��fD�33D�p D�fD�� D�3D�I�D�|�D���D���D��D�C3D�s3D� D���D��fD�)�D�VfD�� D��fD�� D��fD�  D�C3E 6fE �fE^fE��E��E�E��E>fEњEd�E  E�fE,�E� ET�E�3E	|�E
�E
��E<�E�fEl�E� E��E� EfE� E�3E� E3E� E��E� E^fEh E� E� E s3E!�fE"��E$$�E%.fE&<�E'�fE(�3E*x E+�3E,��E.0 E/8 E0@ E1�3E2�E4{3E5� E6� E84�E9C3E:T�E;� E>�3EA�fED�EHfEK�EN��EQ��ETt�EWњE[3E^$�Ea Edd�Eg1�EjS3Emp Ep��Es� Ew&fEz	�E}VfE� E�� E�\ E�ɚE�i�E� E���E�  E���E�@�E��3E�l�E��3E��fE��fE���E�-�E��3E�4 E���E�nfE��3E��3E�$ E��3E�#3E�� E�H E��fE�� E�9�E���E��G�O�G�O�?���G�O�G�O�G�O�G�O�G�O�G�O�?���G�O�G�O�G�O�G�O�?���G�O�?�  G�O�G�O�?�ff?�33?�  ?���?ٙ�?�33@   @ff@33@��@9��@Fff@L��@`  @fff@�  @���@�  @���@�ff@�  @���@�33@�  @���@�ff@���@���AffA��A33A33A#33A+33A333A;33AC33AI��AS33A[33Ad��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441444444144441414411111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                          @ v@ �@ {@ �@ ""@ (�@ /�@ 7L@ =q@ E�@ Q=@ _�@ m:@ {�@ ��@ �0@ ��@ �-@ �&@ ��@ ��@ ��@ �@j@@
@,`@:�@H]@V�@c�@qS@~�@��@�H@��@��@�>@��@�;@��@��@1@�@"�@0x@=q@Ji@Yn@hs@uk@�@�@��@��@�R@�W@խ@�@��@��@	�@�@$.@2�@A�@M�@\)@k�@x�@�@��@�m@�!@�@��@�
@�@�@��@V@
@+@7L@DD@Q=@`�@p�@}�@��@�0@�z@�~@��@��@�#@�y@�@�@@�@-@9X@G�@T�@dZ@r@~K@�P@��@��@��@��@є@��@�@�9@1@�@"�@0x@>�@Lu@Yn@g�@t�@�d@�\@��@�Y@�@ƨ@�O@�H@�@�E@
�@B@&�@4�@B�@P�@\)@g�@v@�p@��@��@�!@�k@�@�
@�`@�e@ �@�@�@+@7L@DD@SI@_�@k�@z3@�7@��@��@��@��@�*@�#@��@��@	j@	o@	g@	+�@	:�@	I�@	Wb@	c�@	qS@	~K@	��@	��@	��@	��@	�2@	�*@	��@	�4@	�,@
v@
*@
#�@
0x@
<@
K�@
X�@
dZ@
s_@
�d@
�h@
��@
��@
�@
�@
��@
�H@
�@
��@�@�@%�@1�@A�@O0@\)@k�@x�@�p@�$@�@�f@�@�@�[@�`@�Y@�Q@V@O@'�@7L@FQ@SI@_�@n�@|?@��@�<@��@�~@�2@�|@��@��@��@@@ @-@9X@G�@T�@��@1�@}�@�W@b@[z@�A@�Y@:@�p@�7@�@dZ@�@�,@D�@��@��@"�@k�@�F@@Ji@��@��@(�@r�@�@%@M�@��@�;@$.@i!@�r@�@5�@{�@@�@M�@�u@�h@ �@hs@��@��@0x@uk@��@j@Ji@�@խ@�@e	@��@�@9X@�W@ȴ@@X@�z@�(@ 3�@ |�@ ƨ@!�@!\)@!��@!�@"=q@"��@"�C@#�@#k.@#��@$v@$O�@$��@$�`@%0x@%|�@%�W@&�@&X�@&�@&�y@'1�@'y�@'@(
�@(Q�@(��@(܀@) �@)e�@)��@)�@*.l@*o�@*�~@*�e@+5@@+ww@+�@+��@,9X@,|?@,�w@- �@-A�@-�@-�2@.j@.D�@.��@.�@/�@/P�@/�#@/׹@0�@0\�@0�a@0��@1%�@1g�@1��@1�4@2.l@2o�@2��@2�@35�@3t�@3��@3��@45�@4uk@4��@4� @5:@5uk@5��@5�@633@6qS@6��@6�@@7/@7oF@7�!@7�@8+@8i!@8��@8�@9(�@9i!@9�M@9�@:'�@:g�@:��@:�`@;&;@;e	@;��@;�@<ff@<�@=b�@=��@>��@?o@?��@@]@@�~@A"�@A��@BF�@B�R@C_�@C��@Dr@D�l@EYn@Fv@Fv�@F�(@G��@H1@H�R@I-�@I�z@JN�@J�&@K/�@K��@LO1@L��@Mp�@M�@N��@O1@O|�@P(�@QO�@R�f@T �@UT�@V��@X&;@Yc�@Z�z@\�@]r@^Ĝ@`�@aoF@b�@c� @eK@f��@h
=@io�@j�Z@l�@mN�@n�~@p{@qLv@r�r@t�@uR�@v��@xV@y`B@z��@|*@}[z@~ě@�8@���@�Yn@�]@���@�Wb@�p@��r@�V�@��@��4@�N*@�j@���@��=@���@��@�8�@�VG�O�G�O�@ G�O�G�O�G�O�G�O�G�O�G�O�@ �G�O�G�O�G�O�G�O�@ �G�O�@ jG�O�G�O�@ v@ %@ �@ �@ 1@ 	�@ 
=@ 
�@ J@ �@ b@ �@ o@ {@ *@ �@ �@ O@ [@  @ ""@ $.@ &;@ (�@ +�@ -�@ /@ 2�@ 5�@ 8�@ ;d@ >�@ B8@ E�@ I@ Lu@ O�@ R�@ V�@ Z@ ^5G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�VA�ZA�^5A�\)A�^5A�`BA�^5A�`BA�`BA�`BA�bNA�ffA�hsA�hsA�hsA�hsA�dZA�bNA�^5A�ZA�\)A�\)A�S�A�M�A�M�A�M�A�M�A�O�A�C�A��AخA�v�A���A�%A՝�AӋDAЉ7A�VAɇ+A�A���A���A�7LA�5?A�?}A��
A���A�Q�A�jA��A�^5A���A�l�A�(�A��hA��-A���A�ĜA��!A�VA�A�A���A��A�G�A�ZA�G�A�|�A�ƨA���A�1A��A�A��jA�&�A�ZA���A���A�bA�A�M�A|�HAoO�AfbNA`A�A]
=AX�`AQƨAO�AL�/AJ1AHz�AFZAD��AC�PAA�A>��A=?}A;&�A9S�A8E�A7�^A7�A6��A6^5A6A�A6-A6 �A6�A5��A5�TA5�;A5��A5�FA5�7A5x�A5l�A5dZA5K�A5;dA5"�A4��A4��A4�9A4�\A4bNA4$�A3��A3A3l�A3O�A3�A2�/A2�9A2 �A1�-A1p�A1%A/�-A/+A.�DA-ƨA-oA,�`A,�DA+��A*�!A*E�A)��A)�A'�-A&ZA$�jA"A�A!A Q�A ��A!�FA"9XA"n�A!�TA ��A�TA`BA(�A?}A9XAAp�A+A�jA1'A�AK�A+A"�AK�A\)A��A��A�A5?A9XA�jA�yA�A��AȴAz�AA��A��AM�A��A?}A�
AS�A�wA;dA�jAn�A(�A��A�A��A(�A��A�yAA�A=qA�wAp�A%A
�9A
�A
VA
JA	�#A	dZA��A~�A^5A^5A^5A��A�PA+A�9Av�AM�A��A|�AO�A&�A�Ar�AA��A"�A�jAA�A��AdZA �jA ff@�ƨ@�o@��!@��@���@� �@���@�{@���@�/@�P@�x�@�E�@�?}@���@��@�"�@�
=@���@Ԭ@�`B@��y@�A�@���@�|�@�p�@�=q@���@�M�@��@���@���@�?}@�@�"�@�\)@�O�@�r�@��/@���@�j@��R@�x�@�Z@�l�@��@���@�@�@�1@���@�J@�bN@��H@�V@�  @�"�@�E�@���@�I�@�t�@�V@�Z@�;d@�V@�@��@�(�@~{@|I�@z�!@w�P@u@s��@q��@p�@n�R@m/@j=q@h  @fv�@e/@cƨ@b^5@a�@_��@]p�@[dZ@Z�@Y��@X�9@W
=@T��@SdZ@Q��@P �@N@Lj@J�H@IX@G�w@E�h@DI�@B�!@A�@?l�@=�@<��@;�@9��@7�@7\)@5�@3��@2��@0��@/l�@.��@-@,�/@+t�@)G�@'�@'
=@%@$�D@$(�@"~�@!hs@ Q�@ȴ@{@/@Z@33@��@�9@bN@
=@@�/@S�@��@��@X@r�@\)@��@@V@��@z�@�m@S�@
�@
�\@	�@	&�@A�@K�@v�@p�@�@�@z�@Z@��@dZ@�@-@x�@&�@ ��@ A�@   ?�;d?���?��h?�/?�j?�ƨ?��?���?�l�?�?���?��?���?�J?�  ?�;d?���?�I�?�?�^5?陚?�9?���?�ȴ?��?��?�9X?���?�J?�%?��?��;?�V?�p�?ܬ?��m?�dZ?ڟ�?�~�?ٺ^?ش9?�b?׮?֧�?�?�9X?�33?��?У�?���?�O�?�I�?���?ə�?Ǯ?Ƨ�?�z�?�&�?�  ?�v�?��h?�I�?�C�?��?�r�?�l�?�ff?��T?�?�`B?��/?�Z?�9X?���?�S�?���?�n�?�-?���?���?���?���?�J?�M�?���?���?��
?�z�?��?�?�ȴ?��P?�Q�?�r�?��9?���?��?�7LA�XA�XA�XA�ZA�ZA�`BA�XA�ZA�XA�XA�VA�XA�XA�Q�A�S�A�VA�VA�Q�A�O�A�M�A�M�A�Q�A�O�A�O�A�Q�A�Q�A�XA�VA�VA�`BA�`BA�^5A�^5A�\)A�\)A�\)A�\)A�^5A�\)A�^5A�`BA�`BA�`BA�^5A�^5A�^5A�`BA�`BA�`BA�`BA�`BA�`BA�`BA�`BA�`BA�bNA�dZA�dZA�dZA�dZG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                          A�VA�ZA�^5A�\)A�^5A�`BA�^5A�`BA�`BA�`BA�bNA�ffA�hsA�hsA�hsA�hsA�dZA�bNA�^5A�ZA�\)A�\)A�S�A�M�A�M�A�M�A�M�A�O�A�C�A��AخA�v�A���A�%A՝�AӋDAЉ7A�VAɇ+A�A���A���A�7LA�5?A�?}A��
A���A�Q�A�jA��A�^5A���A�l�A�(�A��hA��-A���A�ĜA��!A�VA�A�A���A��A�G�A�ZA�G�A�|�A�ƨA���A�1A��A�A��jA�&�A�ZA���A���A�bA�A�M�A|�HAoO�AfbNA`A�A]
=AX�`AQƨAO�AL�/AJ1AHz�AFZAD��AC�PAA�A>��A=?}A;&�A9S�A8E�A7�^A7�A6��A6^5A6A�A6-A6 �A6�A5��A5�TA5�;A5��A5�FA5�7A5x�A5l�A5dZA5K�A5;dA5"�A4��A4��A4�9A4�\A4bNA4$�A3��A3A3l�A3O�A3�A2�/A2�9A2 �A1�-A1p�A1%A/�-A/+A.�DA-ƨA-oA,�`A,�DA+��A*�!A*E�A)��A)�A'�-A&ZA$�jA"A�A!A Q�A ��A!�FA"9XA"n�A!�TA ��A�TA`BA(�A?}A9XAAp�A+A�jA1'A�AK�A+A"�AK�A\)A��A��A�A5?A9XA�jA�yA�A��AȴAz�AA��A��AM�A��A?}A�
AS�A�wA;dA�jAn�A(�A��A�A��A(�A��A�yAA�A=qA�wAp�A%A
�9A
�A
VA
JA	�#A	dZA��A~�A^5A^5A^5A��A�PA+A�9Av�AM�A��A|�AO�A&�A�Ar�AA��A"�A�jAA�A��AdZA �jA ff@�ƨ@�o@��!@��@���@� �@���@�{@���@�/@�P@�x�@�E�@�?}@���@��@�"�@�
=@���@Ԭ@�`B@��y@�A�@���@�|�@�p�@�=q@���@�M�@��@���@���@�?}@�@�"�@�\)@�O�@�r�@��/@���@�j@��R@�x�@�Z@�l�@��@���@�@�@�1@���@�J@�bN@��H@�V@�  @�"�@�E�@���@�I�@�t�@�V@�Z@�;d@�V@�@��@�(�@~{@|I�@z�!@w�P@u@s��@q��@p�@n�R@m/@j=q@h  @fv�@e/@cƨ@b^5@a�@_��@]p�@[dZ@Z�@Y��@X�9@W
=@T��@SdZ@Q��@P �@N@Lj@J�H@IX@G�w@E�h@DI�@B�!@A�@?l�@=�@<��@;�@9��@7�@7\)@5�@3��@2��@0��@/l�@.��@-@,�/@+t�@)G�@'�@'
=@%@$�D@$(�@"~�@!hs@ Q�@ȴ@{@/@Z@33@��@�9@bN@
=@@�/@S�@��@��@X@r�@\)@��@@V@��@z�@�m@S�@
�@
�\@	�@	&�@A�@K�@v�@p�@�@�@z�@Z@��@dZ@�@-@x�@&�@ ��@ A�@   ?�;d?���?��h?�/?�j?�ƨ?��?���?�l�?�?���?��?���?�J?�  ?�;d?���?�I�?�?�^5?陚?�9?���?�ȴ?��?��?�9X?���?�J?�%?��?��;?�V?�p�?ܬ?��m?�dZ?ڟ�?�~�?ٺ^?ش9?�b?׮?֧�?�?�9X?�33?��?У�?���?�O�?�I�?���?ə�?Ǯ?Ƨ�?�z�?�&�?�  ?�v�?��h?�I�?�C�?��?�r�?�l�?�ff?��T?�?�`B?��/?�Z?�9X?���?�S�?���?�n�?�-?���?���?���?���?�J?�M�?���?���?��
?�z�?��?�?�ȴ?��P?�Q�?�r�?��9?���?��?�7LA�XA�XA�XA�ZA�ZA�`BA�XA�ZA�XA�XA�VA�XA�XA�Q�A�S�A�VA�VA�Q�A�O�A�M�A�M�A�Q�A�O�A�O�A�Q�A�Q�A�XA�VA�VA�`BA�`BA�^5A�^5A�\)A�\)A�\)A�\)A�^5A�\)A�^5A�`BA�`BA�`BA�^5A�^5A�^5A�`BA�`BA�`BA�`BA�`BA�`BA�`BA�`BA�`BA�bNA�dZA�dZA�dZA�dZG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	+B	+B	)�B	+B	+B	)�B	+B	)�B	+B	)�B	+B	+B	+B	,B	,B	+B	,B	.B	.B	0!B	0!B	/B	1'B	2-B	2-B	33B	2-B	33B	1'B	/B	0!B	-B	&�B	�B	oB	#�B	/B	�DB	�B	�)B
n�B
�-B
�B
�B7LBP�By�Br�B�PB��B�B��B�jB�BB+B��B��B�mB�LB��B��B��B�bB�BZBI�B33B+BuB
��B
�B
�?B
� B
dZB
B�B
VB	�B	�TB	��B	XB	B�HB�XB��B�BaHBO�B5?B2-B �B�BVB+B��B��B�B��B	7B�B �B9XBP�B]/Bm�B�B�DB�hB��B�B�'BB�B�B	JB	�B	)�B	1'B	7LB	C�B	N�B	_;B	k�B	r�B	{�B	�1B	�hB	��B	��B	��B	�B	�LB	�dB	ɺB	��B	�
B	�#B	�B	�B	��B	��B	��B	��B	��B
  B
%B

=B
JB
PB

=B

=B
B	��B	�B	��B
\B
-B
C�B
L�B
L�B
L�B
N�B
L�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
Q�B
Q�B
R�B
R�B
T�B
W
B
ZB
[#B
aHB
e`B
k�B
r�B
v�B
�B
�1B
�bB
�bB
�bB
�\B
�JB
�=B
�%B
~�B
|�B
v�B
l�B
N�B
J�B
D�B
?}B
>wB
<jB
=qB
=qB
<jB
:^B
7LB
<jB
A�B
>wB
=qB
=qB
>wB
;dB
;dB
<jB
=qB
<jB
;dB
:^B
=qB
<jB
<jB
<jB
;dB
;dB
9XB
9XB
9XB
9XB
8RB
6FB
7LB
6FB
6FB
5?B
5?B
49B
33B
33B
2-B
/B
/B
-B
+B
+B
'�B
'�B
)�B
(�B
&�B
"�B
�B
�B
�B
�B
!�B
�B
�B
�B
DB
	7B
JB
PB
%B
\B
\B
VB
VB
\B
hB
VB
\B
VB
DB
VB
PB
PB
\B
bB
bB
JB
JB
DB
JB

=B
DB
hB
uB
�B
�B
�B
�B
�B
�B
�B
"�B
#�B
&�B
%�B
(�B
,B
.B
/B
2-B
2-B
2-B
49B
7LB
7LB
8RB
9XB
9XB
;dB
=qB
=qB
>wB
@�B
@�B
B�B
E�B
E�B
F�B
F�B
I�B
K�B
K�B
L�B
M�B
M�B
O�B
N�B
P�B
R�B
R�B
R�B
R�B
T�B
W
B
XB
XB
ZB
\)B
[#B
\)B
]/B
]/B
^5B
`BB
aHB
`BB
bNB
bNB
cTB
cTB
dZB
e`B
e`B
ffB
hsB
iyB
jB
k�B
k�B
l�B
l�B
m�B
o�B
p�B
o�B
p�B
r�B
r�B
s�B
t�B
t�B
v�B
v�B
w�B
w�B
y�B
z�B
z�B
{�B
{�B
|�B
}�B
~�B
� B
�B
� B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�+B
�1B
�=B
�=B
�JB
�JB
�JB
�JB
�PB
�PB
�VB
�VB
�\B
�\B
�hB
�hB
�hB
�oB
�oB
�uB
�uB
�{B
��B
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
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�'B
�'B
�-B
�3B
�3B
�?B
�9B
�9B
�?B
�FB
�LB
�LB
�RB
�RB
�^B
�^B
�jB
�jB
�wB
�wB
�}B
�}B
��B
��B
��B
��B
��B
B
ÖB
ÖB
ÖB
ĜB
ŢB
ŢB
ŢB
ŢB
ŢB
ƨB
ŢB
ǮB
ƨB
ƨB
ŢB
ŢB
ƨB
ƨB
ƨB
ƨB
ŢB
ƨB
ƨB	+B	+B	+B	+B	+B	+B	)�B	(�B	+B	+B	,B	)�B	)�B	+B	,B	+B	)�B	,B	&�B	)�B	)�B	(�B	+B	+B	+B	+B	)�B	+B	+B	)�B	)�B	+B	)�B	+B	)�B	)�B	+B	+B	+B	+B	)�B	)�B	)�B	)�B	+B	+B	+B	)�B	)�B	+B	)�B	)�B	)�B	+B	)�B	+B	+B	+B	+B	+G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                          B	*�B	*�B	)�B	*�B	*�B	)�B	*�B	)�B	*�B	)�B	*�B	*�B	*�B	+�B	+�B	*�B	+�B	-�B	-�B	0B	0B	.�B	1B	2B	2B	3B	2B	3B	1B	/B	0
B	,�B	&�B	�B	ZB	#�B	/B	�0B	� B	�B
n�B
�B
�sB
�yB7:BP�By�Br�B�@B��B��B��B�\B�~BBB��B��B�bB�AB��B��B��B�XB�BZBI�B3+B*�BnB
��B
�B
�9B
�B
dTB
B�B
PB	�B	�OB	��B	XB	B�BB�RB��B�BaBBO�B5:B2(B �B�BSB(B��B��B�B��B	6B�B �B9YBP�B]1Bm�B�B�HB�lB��B�B�-BB�B�B	RB	�B	*B	11B	7WB	C�B	N�B	_HB	k�B	r�B	{�B	�@B	�xB	��B	��B	�B	�&B	�^B	�wB	��B	�B	�B	�8B	�B	�B	��B	��B	��B	�B	�B
 B
?B

XB
eB
lB

YB

ZB
/B	��B	��B	��B
{B
-.B
C�B
L�B
L�B
L�B
N�B
L�B
Q	B
RB
RB
RB
RB
SB
RB
RB
SB
SB
U'B
W4B
ZGB
[NB
atB
e�B
k�B
r�B
v�B
�HB
�`B
��B
��B
��B
��B
�|B
�pB
�XB
.B
}"B
v�B
l�B
OB
J�B
D�B
?�B
>�B
<�B
=�B
=�B
<�B
:�B
7�B
<�B
A�B
>�B
=�B
=�B
>�B
;�B
;�B
<�B
=�B
<�B
;�B
:�B
=�B
<�B
<�B
<�B
;�B
;�B
9�B
9�B
9�B
9�B
8�B
6�B
7�B
6�B
6�B
5�B
5�B
4�B
3�B
3�B
2{B
/jB
/jB
-^B
+SB
+SB
(BB
(BB
*OB
)IB
'=B
#%B
 B
B
B
B
"(B
B
B
�B
�B
	�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
	B
�B
�B
�B
�B

�B
�B
%B
4B
IB
`B
nB
qB
zB
�B
 �B
#�B
$�B
'�B
&�B
)�B
,�B
.�B
0B
3B
3B
3"B
51B
8GB
8JB
9SB
:]B
:`B
<oB
>B
>�B
?�B
A�B
A�B
C�B
F�B
F�B
G�B
G�B
J�B
L�B
L�B
NB
OB
OB
Q B
PB
R-B
T=B
T@B
TDB
TGB
VVB
XeB
YoB
YrB
[�B
]�B
\�B
]�B
^�B
^�B
_�B
a�B
b�B
a�B
c�B
c�B
d�B
d�B
e�B
f�B
f�B
g�B
jB
kB
l!B
m)B
m,B
n5B
n8B
oAB
qQB
rZB
qVB
r_B
tnB
tqB
uzB
v�B
v�B
x�B
x�B
y�B
y�B
{�B
|�B
|�B
}�B
}�B
~�B
�B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�&B
�#B
�1B
�4B
�7B
�@B
�IB
�WB
�ZB
�jB
�mB
�pB
�rB
�{B
�~B
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
�B
�B
� B
�%B
�1B
�<B
�DB
�OB
�ZB
�iB
�tB
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
�B
�B
�B
�B
�&B
�+B
�0B
�DB
�CB
�UB
�VB
�cB
�zB
��B
��B
��B
��B
��B
��B
�	B
�B
�"B
�7B
�NB
�aB
�qB
��B
��B
��B
��B
��B
��B
�B
�B
�-B
�;B
�QB
�gB
�uB
ƅB
ƕB
ǪB
ȿB
��B
��B
��B
�	B
�B
�(B
�7B
�FB
�\B
�fB
́B
̊B
̚B
ˢB
˱B
��B
��B
��B
��B
��B
��B
��B	*�B	*�B	*�B	*�B	*�B	*�B	)�B	(�B	*�B	*�B	+�B	)�B	)�B	*�B	+�B	*�B	)�B	+�B	&�B	)�B	)�B	(�B	*�B	*�B	*�B	*�B	)�B	*�B	*�B	)�B	)�B	*�B	)�B	*�B	)�B	)�B	*�B	*�B	*�B	*�B	)�B	)�B	)�B	)�B	*�B	*�B	*�B	)�B	)�B	*�B	)�B	)�B	)�B	*�B	)�B	*�B	*�B	*�B	*�B	*�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                          <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201808050703402021061413573820210614135738202107221611382021072216113820210722161138201808050703402021061413573820210614135738202107221611382021072216113820210722161138PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018080507034020180805070340  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018080507034020180805070340QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018080507034020180805070340QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216142020210722161420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                