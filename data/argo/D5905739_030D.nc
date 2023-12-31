CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-08-31T18:28:50Z creation      
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
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  O�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    _�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  c�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  s�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ϔ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Ӕ   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   	   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   	   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � 	   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   	�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   	�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    	�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        	�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        	�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       	�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    	�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �4   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �4   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �4   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � 4Argo profile    3.1 1.2 19500101000000  20180831182850  20210617131502  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�{7<My�@�{7<My�11  @�{733N�@�{733N�@6�%��R�@6�%��R��c�yЦv�c�yЦv11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?�  @��@@  @�  @�  @�  @�  A��A  A!��AA��Ad��A���A���A�33A���A���A�  A�33A�  B ffB  B��B  B ffB(ffB0  B8ffBA33BH��BP��BX��B`��BhffBp��Bx  B��B�  B�33B�33B�  B�  B�  B�ffB�ffB�33B���B�33B�ffB�  B�  B�33B�ffB�33B�33B���B�  B���B�  B���B�33B�  B���B�33B�ffB�33B���B���C   C  C�C�C�C
�C  C  C  C  C�CL�C�C�fC  C33C L�C"33C$�C%�fC'��C*�C,  C-�fC033C233C4�C6  C7�fC:33C<�C>�C@�CB  CDL�CF33CH�CJL�CL33CN�CO�fCQ��CT  CVL�CX33CZ  C\33C^�C_�fCb�Cd33CfL�Ch�Ci�fCk�fCn  Cp�Cr33CtL�Cv�Cw��Cz  C|33C~33C��C��3C�  C��C��3C�ٚC��fC�  C��C�&fC�&fC��C��3C�  C��C�&fC��C�  C��C�&fC��C��fC��3C��C��C�  C�ٚC��fC�  C��C��C��fC��C��C�  C��fC��3C�  C��C�&fC��C��3C��C�&fC��C��3C��C��C��3C��C�&fC��C��C��C��3C��C��C��3C�&fC��C��C��C�  C�  C�  C�  C�  C��fC��3C��3C��fC��C�&fC��C�  C��C��fC��C��C��fC��C�  C��fC��C�  C��fC��C�&fC��C��C�  C��fC��3C�  C��C��C��3C��C�&fC��C�  C��C�  C��3C��C�  C��3C��C�  C��fC��3C��C��3C��fC��3C��C��C�  C��fC��3C��C��C��3C�&fC�33C��3C�  C��D �D �3D ��Ds3D�3DFfD9�D	� DL�D�3D�fDL�D��D��D@ D��D!�3D$Y�D'�D)��D,S3D.��D1� D4�D6�fD933D;�fD>L�D@�3DCl�DE��DH9�DJ�fDM3DOy�DQ�fDT9�DV�fDY�D[��D]��D`ffDbٚDe@ Dg� Di��DlY�Dn�3Dq  DsY�Du�3Dx  DzFfD|,�D~� D�p D�� D��3D�3D�P D��3D��fD�� D�&fD�i�D�� D���D�9�D��fD�� D�#3D�i�D��3D�3D�P D���D�� D�fD�VfD��3D���D�fD�33D�ffD��fD��3D��fD� D�,�D�VfD�i�D��3D��3D�� D�ٚD�� D��D�&fD�C3D�l�D��3D���D�� D�fD�0 D�S3D�� D��fD��3D���D�0 D�\�DĐ Dż�D���D�fD�<�D�c3Dˉ�D̼�D�� D� D�33D�VfDҀ Dӣ3D�ɚD��3D��D�9�D�\�D�|�DۖfDܳ3D���D��D�  D��D�3D�)�D�9�D�FfD�` D�l�D� D�3D�fD��D��D�� D��fD���D�	�D��D�)�D�9�D�FfD�` D�p D�|�D��fD�� D���D���D��fD��fD��3E � E�E�fE&fE�fE8 E��ED�EɚEL�EX ES3E�3E	� E.fE� E��E�3EY�EFfE� E Eh ES3E� E�E�3EX E��E3ET�E �3E!� E"�3E$8 E%��E&�fE'��E) E*h E+�fE-fE. E/p E0��E1� E333E4�fE5��E6�fE8H E9��E:�3E;�3E?6fEB�EE[3EH1�EKY�EN~fEQ� EU3EW�fEZ�fE^fEa�fEd{3Egs3Ej�3Em�fEq$�Es��Ew�Ez\�E}�3E�P E���E�e�E���E�| E�� E�(�E�l >���?   >���>���?   ?   >���>���>���?   >���>���?   ?   >���?   >���?   >���>���>���?   ?��?   ?333?L��?�  ?���?���?�33?�33?�ff@   @33@   @,��@9��@S33@fff@l��@�33@���@�33@�33@���@���@�ff@�33@���@�  @���A33A33A33A33A!��A)��A1��A;33AA��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141144141441441414441141111114111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                     ?fff?�  @,��@`  @�  @�  @�  @�  A	��A  A)��AI��Al��A���A���A�33A���Ař�A�  A�33A�  BffB
  B��B  B"ffB*ffB2  B:ffBC33BJ��BR��BZ��Bb��BjffBr��Bz  B���B�  B�33B�33B�  B�  B�  B�ffB�ffB�33B���B�33B�ffB�  B�  B�33B�ffB�33B�33B���B�  B���B�  B���B�33B�  B���B�33B�ffB�33B���B���C � C� C��C��C��C
��C� C� C� C� C��C��C��CffC� C�3C ��C"�3C$��C&ffC(L�C*��C,� C.ffC0�3C2�3C4��C6� C8ffC:�3C<��C>��C@��CB� CD��CF�3CH��CJ��CL�3CN��CPffCRL�CT� CV��CX�3CZ� C\�3C^��C`ffCb��Cd�3Cf��Ch��CjffClffCn� Cp��Cr�3Ct��Cv��CxL�Cz� C|�3C~�3C�L�C�33C�@ C�L�C�33C��C�&fC�@ C�L�C�ffC�ffC�L�C�33C�@ C�Y�C�ffC�Y�C�@ C�L�C�ffC�L�C�&fC�33C�L�C�Y�C�@ C��C�&fC�@ C�Y�C�L�C�&fC�L�C�Y�C�@ C�&fC�33C�@ C�L�C�ffC�L�C�33C�L�C�ffC�L�C�33C�Y�C�L�C�33C�L�C�ffC�Y�C�Y�C�L�C�33C�L�C�L�C�33C�ffC�Y�C�Y�C�L�C�@ C�@ C�@ C�@ C�@ C�&fC�33C�33C�&fC�L�C�ffC�Y�C�@ C�L�C�&fC�L�C�L�C�&fC�L�C�@ C�&fC�L�C�@ C�&fC�Y�C�ffC�Y�C�L�C�@ C�&fC�33C�@ C�Y�C�L�C�33C�L�C�ffC�Y�C�@ C�L�C�@ C�33C�L�C�@ C�33C�L�C�@ C�&fC�33C�L�C�33C�&fC�33C�L�C�Y�C�@ C�&fC�33C�L�C�Y�C�33C�ffC�s3C�33C�@ C�L�D ,�D �3D�D�3D3DffDY�D	� Dl�D3D�fDl�D�D��D` D�D!�3D$y�D',�D)ٚD,s3D/�D1� D49�D6�fD9S3D;�fD>l�D@�3DC��DE��DHY�DJ�fDM33DO��DRfDTY�DV�fDY9�D[��D^�D`�fDb��De` Dg� Dj�Dly�Dn�3Dq  Dsy�Du�3Dx  DzffD|L�D~� D�� D�� D��3D�#3D�` D��3D��fD�  D�6fD�y�D�� D�	�D�I�D��fD�� D�33D�y�D��3D�3D�` D���D�� D�&fD�ffD��3D���D�fD�C3D�vfD��fD��3D��fD�  D�<�D�ffD�y�D��3D��3D�� D��D�  D��D�6fD�S3D�|�D��3D���D�� D�fD�@ D�c3D�� D��fD��3D��D�@ D�l�DĠ D���D���D�&fD�L�D�s3D˙�D���D�� D�  D�C3D�ffDҐ Dӳ3D�ٚD�3D�)�D�I�D�l�Dڌ�DۦfD��3D���D���D� D��D�#3D�9�D�I�D�VfD�p D�|�D� D�3D�fD��D���D�� D��fD�	�D��D�)�D�9�D�I�D�VfD�p D�� D���D��fD�� D���D���D��fD��fE �E � E�E�fE.fE�fE@ EɚEL�EњET�E` E[3E�3E	� E6fE� E��E�3Ea�ENfE� E Ep E[3E� E!�E3E` E��E3E\�E �3E!� E"�3E$@ E%��E&�fE'ɚE) E*p E+�fE-&fE. E/x E0��E1� E3;3E4�fE5��E6�fE8P E9��E:�3E;�3E?>fEB�EEc3EH9�EKa�EN�fEQ� EU3EW�fE[fE^&fEa�fEd�3Eg{3Ej�3EnfEq,�Et�Ew�Ezd�E}�3E�T E���E�i�E���E�� E�� E�,�E�p ?fffG�O�?L��?fffG�O�G�O�?L��G�O�?fffG�O�G�O�?fffG�O�G�O�?fffG�O�?fffG�O�G�O�G�O�?fff?�  G�O�?�  ?���?�ff?�  ?���?ٙ�G�O�?�33@33@   @333@@  @L��@Y��@s33@�33@�ff@�33@���@�33@�33@���@ə�@�ff@�33@���A   AffA33A33A33A#33A)��A1��A9��AC33AI��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141144141441441414441141111114111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                     @ �@ �@ @ {@ O@ ""@ (�@ /�@ 7L@ =q@ D�@ R�@ a�@ n�@ {�@ ��@ ��@ �5@ �~@ �w@ ��@ �#@ �@ �@j@�@g@,`@:�@I�@V�@dZ@r@�@��@��@�A@�9@@��@ލ@�@�,@�@�@#�@0x@<�@K�@Z@ff@t@�d@��@��@��@��@��@��@�H@�@@�E@
=@6@&;@4�@A�@M$@[z@i�@ww@��@�u@�@��@��@�c@�
@�@�@@V@�@(�@7�@FQ@SI@`B@l�@y�@�7@�0@�(@��@��@�|@�t@�m@� @@�@g@,`@<@I@V@e	@r@~�@�D@�<@�A@��@��@�7@�;@�4@�~@�@�@$.@0x@<�@Ji@X�@g@uk@��@�@��@��@��@�W@�O@��@��@�E@	�@�@$.@33@A�@P�@^5@j@v�@�@�$@�y@�r@��@�@�@�`@��@�Q@V@�@(�@4�@B�@Q�@`�@m�@y�@�7@��@��@�!@�w@��@�#@�(@�q@	�@	�@	 �@	-@	9X@	I@	V@	bN@	qS@	�W@	�P@	��@	��@	�9@	�>@	��@	�/@	�@	��@
1@
*@
""@
/�@
=q@
K@
X�@
e	@
s_@
�@
��@
��@
��@
��@
��@
�O@
��@
�@
�E@�@�@%�@1�@A�@N�@Z�@k.@y�@�|@�u@�m@��@�@�c@�h@�`@�@ �@�@�@(�@7L@DD@Q=@`B@m:@z3@�7@�0@�y@��@��@�@�@�m@�q@�@@[@+�@:�@I@T�@e	@s_@}�@��@�H@��@��@��@��@܀@�`@l�@�~@� @?}@�7@є@�@bN@��@�@?}@��@є@�@a�@��@��@5�@{�@�2@�@Lu@�h@�h@B@[z@��@��@!s@c�@�(@�`@(G@k.@�f@�@2�@t@��@�@5�@v@��@�@5?@t@�-@�@%�@ff@�A@��@-@p�@�-@�@6�@x�@��@j@I�@��@��@O@c�@�M@�@ 7L@ ~K@ ��@!�@!Lu@!��@!�O@"6@"Z@"�H@"��@#�@#\�@#��@#�t@$6@$V�@$�i@$�|@%
�@%G�@%��@%�&@%��@&7�@&t�@&�9@&�@'2�@'p�@'�r@'��@(-@(m:@(�@(�4@)+�@)m:@)�f@)��@*/@*o�@*�r@*�@@+-@+k�@+�f@+�@,,`@,j@,��@,�@-&;@-e	@-��@-�T@. �@.^�@.�U@.�h@/*@/Q=@/��@/�c@0�@0:�@0v@0�!@0�y@1%�@1^�@1��@1�O@2@2F�@2�@2��@2� @31�@3k�@3��@3��@4�@4SI@4�\@4�c@5�@5;d@5p�@5��@5�@6 @6]�@6�H@6�
@7@7M�@7��@7��@7��@87L@8oF@8��@8��@9Q�@9�@:]�@:��@;bN@;��@<c�@<��@=��@=��@>��@?(�@?�&@@#�@@��@AR�@A��@BG�@B�#@Ck.@C�9@D�P@D��@E�@F@F�z@G2�@G�u@H""@H��@IF�@I�/@J@�@J�t@Kv@K�t@Luk@Mb@Mv@N�@N�m@O3�@O�#@P#�@Q��@R��@T4�@Uj@V�>@X�@Y~K@Z�y@\	@]o�@^�J@`<�@a|?@b��@d*S@e�C@f�T@h�@ihs@j�C@l1'@m~K@nƨ@p @qqS@r@sO@sV@s�\@ �G�O�@ @ �G�O�G�O�@ G�O�@ �G�O�G�O�@ �G�O�G�O�@ �G�O�@ �G�O�G�O�G�O�@ �@ jG�O�@ j@ �@ v@ �@ �@ 1G�O�@ 	�@ J@ �@ �@ @ o@ �@ �@ �@ B@ �@ 
@ g@ "�@ $�@ '�@ *S@ -@ /@ 33@ 5�@ 7�@ ;d@ >�@ B8@ D�@ H]@ K�@ O�@ R�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�`BA�ffA�S�A�M�A�S�A�O�A�VA�\)A�S�A��A��A��
A���Aӡ�A�l�A� �A��#A�v�A�bA�p�A���A�x�A�/A���A��Aϩ�AϋDA�dZA��A�  A���A�S�A�v�A�VA�/Aŏ\A¡�A�z�A��#A��A�p�A�Q�A��A��jA��RA��A�A��-A�hsA���A��A��HA�C�A���A�=qA���A���A���A�VA�S�A��wA���A��+A��TA�JA�ƨA��HA��A�ĜA�
=A��yA��A��A��`A�~�A�E�A�C�A�E�A���A�XA�M�A�ffA�9XA��
A�\)A�\)A��HA�1A�A�A�K�A�E�A���A��#A���A�1A��9A��uA���A�v�A�O�A�p�A}C�Ay;dAx �Awx�Au�7As�Ar�HAq��Ao�#Am��Aj��Ai�^Ai�7AiC�Ah�AgdZAf9XAe?}Ac�#Ab�uAb$�Aa�
AaXA`1'A\��AYl�AW�#AWG�AV��AU�AUO�AT�AT=qAR��AQ��AQ+AO�AOt�AO
=AM�-ALbNAJz�AI33AH�RAHQ�AG��AFĜAE�FAES�AD�AD(�AC�^AC��ABI�AA��AA`BAA7LAA"�A@z�A?VA=ƨA=�A<ȴA<�DA<Q�A<{A;�A;oA8�9A7A6�jA6{A5�A5XA5;dA4ĜA4E�A3dZA2E�A1"�A/�A.ĜA.z�A-ƨA+?}A*VA)l�A(�HA(��A';dA$M�A#XA"1A!%A �RA!%A �`A �!A �DA A�A�
A`BA-AQ�A�A�A��A��A�A�^AoA��A33A�jA�+AZA �A�A1A�PAS�A�Av�A=qAS�A%AjA/A��A
�jA
5?A	��A	&�A�A�RAjA�A��AVAx�A%At�AQ�A�^Al�A ��A �@��m@���@���@�ƨ@� �@���@�C�@�\@��@�^@�@���@��H@�?}@�b@�o@�-@�G�@���@��`@�V@ΰ!@�33@ǥ�@��@��@��\@�C�@��;@�-@�I�@�=q@�"�@���@�^5@��9@�"�@�p�@��y@��@�5?@�`B@�z�@�(�@��
@��@���@��@�C�@�M�@�9X@�S�@���@�%@��F@�@�V@��@�@��@�hs@�%@�bN@��@��@���@��@���@�ȴ@��7@���@�w@~��@}O�@{t�@z~�@xr�@w|�@v��@up�@s�F@r=q@pr�@o
=@m�T@m`B@j=q@i&�@f$�@c@_�;@_�@^V@\1@ZJ@Y%@W�@S�m@R-@P��@O�;@O�@M`B@Kƨ@Kt�@J�\@I%@H  @F�R@E�-@D�@B�@A�@@�`@?K�@=`B@<9X@;�m@:n�@9x�@7�@7�P@6ȴ@5O�@3��@3@1��@0Q�@/l�@.E�@-V@,�j@,�@+dZ@)�#@)�7@)�@(b@&��@&ff@%@%O�@$��@"�@"-@!x�@ �9@�@��@�T@O�@��@o@n�@J@G�@��@+@�@�@?}@1@S�@�H@M�@�^@�7@�7@b@|�@;d@��@��@�@j@��@�
@t�@o@
��@
=q@	��@	G�@A�@�@�P@��@ff@��@O�@33@�?���?��h?���?�K�?�E�?�33?�hs??�{?�h?���?���?��?�?䛦?�n�?�hs?޸R?��?�"�?���?׮?֧�?��T?�Z?��?��?ҏ\?��`?Ͼw?ϝ�?Η�?�{?�O�?̬?�C�?�dZ?���?�=q?�7L?�r�?ȓu?��y?ļj?°!?��?�hs?�A�?�|�?���?��-?�O�?�I�?�dZ?��H?�^5?�X?�x�?�7L?���?�x�?���?���?�^5?��H?���?�I�?�/?�O�?�p�?��hA�hsA�jA�ZA�XA�G�A�E�A�M�A�`BA�`BA�^5A�p�A�ffA�l�A�\)A�hsA�ffA�XA�ffA�^5A�hsA�jA�n�A�p�A�r�A�p�A�n�A�dZA�\)A�`BA�ZA�ZA�Q�A�S�A�S�A�S�A�O�A�M�A�I�A�M�A�O�A�VA�XA�XA�M�A�K�A�S�A�XA�\)A�\)A�^5A�ZA�M�A�"�A��A�  A��A��;A��/A��A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                     A�`BA�ffA�S�A�M�A�S�A�O�A�VA�\)A�S�A��A��A��
A���Aӡ�A�l�A� �A��#A�v�A�bA�p�A���A�x�A�/A���A��Aϩ�AϋDA�dZA��A�  A���A�S�A�v�A�VA�/Aŏ\A¡�A�z�A��#A��A�p�A�Q�A��A��jA��RA��A�A��-A�hsA���A��A��HA�C�A���A�=qA���A���A���A�VA�S�A��wA���A��+A��TA�JA�ƨA��HA��A�ĜA�
=A��yA��A��A��`A�~�A�E�A�C�A�E�A���A�XA�M�A�ffA�9XA��
A�\)A�\)A��HA�1A�A�A�K�A�E�A���A��#A���A�1A��9A��uA���A�v�A�O�A�p�A}C�Ay;dAx �Awx�Au�7As�Ar�HAq��Ao�#Am��Aj��Ai�^Ai�7AiC�Ah�AgdZAf9XAe?}Ac�#Ab�uAb$�Aa�
AaXA`1'A\��AYl�AW�#AWG�AV��AU�AUO�AT�AT=qAR��AQ��AQ+AO�AOt�AO
=AM�-ALbNAJz�AI33AH�RAHQ�AG��AFĜAE�FAES�AD�AD(�AC�^AC��ABI�AA��AA`BAA7LAA"�A@z�A?VA=ƨA=�A<ȴA<�DA<Q�A<{A;�A;oA8�9A7A6�jA6{A5�A5XA5;dA4ĜA4E�A3dZA2E�A1"�A/�A.ĜA.z�A-ƨA+?}A*VA)l�A(�HA(��A';dA$M�A#XA"1A!%A �RA!%A �`A �!A �DA A�A�
A`BA-AQ�A�A�A��A��A�A�^AoA��A33A�jA�+AZA �A�A1A�PAS�A�Av�A=qAS�A%AjA/A��A
�jA
5?A	��A	&�A�A�RAjA�A��AVAx�A%At�AQ�A�^Al�A ��A �@��m@���@���@�ƨ@� �@���@�C�@�\@��@�^@�@���@��H@�?}@�b@�o@�-@�G�@���@��`@�V@ΰ!@�33@ǥ�@��@��@��\@�C�@��;@�-@�I�@�=q@�"�@���@�^5@��9@�"�@�p�@��y@��@�5?@�`B@�z�@�(�@��
@��@���@��@�C�@�M�@�9X@�S�@���@�%@��F@�@�V@��@�@��@�hs@�%@�bN@��@��@���@��@���@�ȴ@��7@���@�w@~��@}O�@{t�@z~�@xr�@w|�@v��@up�@s�F@r=q@pr�@o
=@m�T@m`B@j=q@i&�@f$�@c@_�;@_�@^V@\1@ZJ@Y%@W�@S�m@R-@P��@O�;@O�@M`B@Kƨ@Kt�@J�\@I%@H  @F�R@E�-@D�@B�@A�@@�`@?K�@=`B@<9X@;�m@:n�@9x�@7�@7�P@6ȴ@5O�@3��@3@1��@0Q�@/l�@.E�@-V@,�j@,�@+dZ@)�#@)�7@)�@(b@&��@&ff@%@%O�@$��@"�@"-@!x�@ �9@�@��@�T@O�@��@o@n�@J@G�@��@+@�@�@?}@1@S�@�H@M�@�^@�7@�7@b@|�@;d@��@��@�@j@��@�
@t�@o@
��@
=q@	��@	G�@A�@�@�P@��@ff@��@O�@33@�?���?��h?���?�K�?�E�?�33?�hs??�{?�h?���?���?��?�?䛦?�n�?�hs?޸R?��?�"�?���?׮?֧�?��T?�Z?��?��?ҏ\?��`?Ͼw?ϝ�?Η�?�{?�O�?̬?�C�?�dZ?���?�=q?�7L?�r�?ȓu?��y?ļj?°!?��?�hs?�A�?�|�?���?��-?�O�?�I�?�dZ?��H?�^5?�X?�x�?�7L?���?�x�?���?���?�^5?��H?���?�I�?�/?�O�?�p�?��hA�hsA�jA�ZA�XA�G�A�E�A�M�A�`BA�`BA�^5A�p�A�ffA�l�A�\)A�hsA�ffA�XA�ffA�^5A�hsA�jA�n�A�p�A�r�A�p�A�n�A�dZA�\)A�`BA�ZA�ZA�Q�A�S�A�S�A�S�A�O�A�M�A�I�A�M�A�O�A�VA�XA�XA�M�A�K�A�S�A�XA�\)A�\)A�^5A�ZA�M�A�"�A��A�  A��A��;A��/A��A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                     ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bo�Bn�Bn�Bn�Bo�Bn�Bn�Bn�Bl�Bn�Bl�Bm�Bl�Bk�BjBiyBhsBe`BdZBaHBbNB^5BYBT�BS�BT�BS�BQ�BN�BP�BVBW
B]/B\)Bm�B�VB��B�/B�NB��BĜB��B��B��B��B��B��BȴBȴBȴBÖB�}B�dB�RB�?B�3B��B��B��B��B�hB�JB�B~�Bt�BjB`BBW
B;dB5?B7LB=qB=qB+BB�B�HB��BƨB�3B��B�Bm�BT�B�B  B
�B
�BB
��B
��B
�9B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�oB
iyB
VB
L�B
C�B
6FB
+B
$�B
�B
PB
B	�B	�sB	�`B	�NB	�/B	��B	��B	ƨB	�qB	�?B	�-B	�B	�B	��B	�7B	z�B	r�B	m�B	iyB	dZB	bNB	cTB	bNB	]/B	S�B	L�B	M�B	M�B	J�B	I�B	C�B	8RB	7LB	49B	1'B	1'B	.B	,B	)�B	&�B	"�B	!�B	 �B	�B	�B	�B	�B	�B	oB	
=B	B��B��B��B��B�B�B�yB�NB�/B�B�B��B�B�B�B�B��B��BƨBB�wB�dB�-B��B��B��B��B�{B�Bt�Bw�Bo�Bn�B~�B�+B�+B�+B�+B�%B�B�B~�Bx�Br�Bq�Bm�BhsBgmBdZBbNB^5B^5B_;B^5B]/B]/B[#BZB\)B\)B[#B]/B]/B[#B[#BW
BR�BL�BJ�BI�BE�BG�BH�BG�BG�BH�BC�BA�BC�BD�BB�BA�BA�B@�B?}B@�B>wB>wB?}B>wB=qB=qB=qB>wB=qB<jB;dB;dB;dB?}BA�BB�BC�BE�BE�BXB`BBr�B�B�%B�=B��B��B�3B�XB�^B�}BɺB�B�fB�B��B	
=B	�B	8RB	M�B	XB	jB	u�B	y�B	�B	�\B	��B	��B	��B	��B	�LB	�dB	��B	ŢB	��B	��B	�B	�B	�5B	�;B	�`B	�sB	�yB	�B	�B	�B	�B	��B	��B	��B	��B	��B
  B
B
B
%B
1B

=B
DB
JB
\B
\B
oB
uB
�B
�B
�B
�B
�B
 �B
"�B
#�B
$�B
%�B
&�B
&�B
(�B
.B
/B
/B
1'B
2-B
5?B
7LB
5?B
6FB
8RB
8RB
;dB
<jB
<jB
>wB
?}B
A�B
B�B
C�B
D�B
D�B
F�B
E�B
H�B
H�B
H�B
J�B
K�B
K�B
M�B
O�B
O�B
P�B
R�B
Q�B
S�B
S�B
VB
VB
W
B
XB
YB
YB
ZB
ZB
\)B
]/B
]/B
^5B
_;B
`BB
`BB
aHB
aHB
cTB
cTB
dZB
dZB
e`B
e`B
gmB
ffB
ffB
gmB
iyB
iyB
jB
jB
k�B
l�B
k�B
m�B
n�B
n�B
o�B
o�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
s�B
s�B
t�B
w�B
x�B
x�B
y�B
y�B
z�B
{�B
}�B
� B
�B
�B
�B
�B
�B
�%B
�=B
�=B
�JB
�JB
�VB
�\B
�bB
�bB
�hB
�oB
�oB
�uB
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
�B
�B
�B
�B
�!B
�!B
�'B
�-B
�3B
�9B
�?B
�?B
�FB
�LB
�LB
�RB
�RB
�RB
�RB
�XB
�XB
�XB
�XB
�RB
�RB
�RB
�RBo�Bn�Bn�Bo�Bm�Bn�Bq�Bn�Bl�Br�Bn�Bn�Bn�Bq�Bk�Bq�Bn�Bo�Bp�Bn�Bp�Bo�Bn�Bn�Bl�Bn�Bn�Bo�Bn�Bm�Bn�Bn�Bn�Bn�Bn�Bn�Bm�Bo�Bo�Bp�Bn�Bm�Bm�Bn�Bn�Bn�Bn�Bn�Bn�Bn�Bn�BjBn�Bn�Bl�Bl�Bm�Bm�Bm�Bm�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                     BovBnqBnqBnqBoxBnrBnrBnrBlfBnsBlfBmmBlhBkbBj]BiWBhRBe?Bd:Ba)Bb/B^BX�BT�BS�BT�BS�BQ�BN�BP�BU�BV�B]B\BmzB�?B̶B�B�8B��BćB�tB��B��B˳B��B˴BȢBȢBȣBÅB�mB�TB�CB�0B�%B��B��B��B��B�\B�?B��B~�Bt�BjvB`9BWB;\B58B7EB=jB=kB*�BB�B�DB��BƥB�0B��B�Bm�BT�B�B
��B
�B
�BB
��B
��B
�:B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�vB
i�B
VB
L�B
C�B
6NB
+B
$�B
�B
ZB
B	�B	�B	�lB	�[B	�<B	��B	��B	ƷB	��B	�OB	�=B	�,B	�B	��B	�IB	z�B	r�B	m�B	i�B	dnB	bcB	cjB	bdB	]FB	TB	L�B	M�B	M�B	J�B	I�B	C�B	8mB	7gB	4UB	1CB	1DB	.1B	,&B	*B	'B	"�B	!�B	 �B	�B	�B	�B	�B	�B	�B	
aB	1B�B�B��B��B��B��B�B�wB�XB�AB�.B�)B�/B�0B�1B�1B� B�B��B¾B��B��B�^B�'B��B��B��B��B�?Bt�BxBo�Bn�B0B�aB�bB�bB�cB�^B�XB�LB4ByBr�Bq�Bm�Bh�Bg�Bd�Bb�B^tB^tB_{B^uB]pB]pB[eBZ_B\lB\lB[gB]sB]tB[iB[iBWQBS9BMBK	BJBE�BG�BH�BG�BG�BI BC�BA�BC�BD�BB�BA�BA�B@�B?�B@�B>�B>�B?�B>�B=�B=�B=�B>�B=�B<�B;�B;�B;�B?�BA�BB�BC�BE�BE�BXpB`�BsB��B��B��B�<B�dB��B��B��B��B�=BڣB��B�$B�vB	
�B	GB	8�B	NnB	X�B	k!B	vhB	z�B	��B	�	B	�1B	�_B	��B	��B	�B	�"B	�JB	�fB	͔B	ӼB	��B	��B	�B	�B	�8B	�NB	�WB	�_B	�nB	�~B	�B	��B	��B	��B	��B	��B
 �B
B
B
'B
	6B
EB
OB
XB
mB
pB
�B
�B
�B
�B
�B
�B
�B
!�B
$ B
%	B
&B
'B
($B
('B
*7B
/WB
0aB
0dB
2sB
3{B
6�B
8�B
6�B
7�B
9�B
9�B
<�B
=�B
=�B
?�B
@�B
B�B
D B
E
B
FB
FB
H$B
G!B
J6B
J8B
J;B
LKB
MTB
MWB
OfB
QtB
QwB
R�B
T�B
S�B
U�B
U�B
W�B
W�B
X�B
Y�B
Z�B
Z�B
[�B
[�B
]�B
^�B
^�B
_�B
aB
bB
bB
cB
cB
e+B
e-B
f6B
f8B
gAB
gDB
iSB
hOB
hQB
i[B
kiB
klB
luB
lwB
m�B
n�B
m�B
o�B
p�B
p�B
q�B
q�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
u�B
u�B
v�B
y�B
{B
{B
|B
|B
}B
~ B
�2B
�BB
�UB
�`B
�nB
�uB
�yB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�%B
�8B
�>B
�NB
�[B
�hB
�oB
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
�B
�B
�$B
�$B
�BB
�\B
�xB
��B
��B
��B
��B
��B
��B
�B
�B
�3B
�GB
�VB
�mB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
� B
�$B
�'B
�*BovBnpBnpBovBmiBnpBq�BnpBlcBr�BnpBnpBnpBq�Bk]Bq�BnpBovBp|BnpBp|BovBnpBnpBldBnqBnqBowBnqBmjBnqBnqBnqBnqBnqBnqBmjBowBowBp}BnrBmkBmkBnrBnrBnrBnrBnrBnrBnsBnsBjZBnsBnsBlfBlfBmlBmmBmmBmmG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201808311828502021061413553620210614135536202106171312452021061713124520210617131245201808311828502021061413553620210614135536202106171312452021061713124520210617131245PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018083118285020180831182850  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018083118285020180831182850QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018083118285020180831182850QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713150220210617131502IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                