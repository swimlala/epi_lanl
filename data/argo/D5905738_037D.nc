CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-20T01:25:17Z creation      
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
resolution        =���   axis      Z           ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   L    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        P   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        d0   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        tP   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        �x   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   
�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   
�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   
�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   
�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � 
�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    \   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        |   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � 	�      � 	�Argo profile    3.1 1.2 19500101000000  20181020012517  20210722160154  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               %   %DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @؇�,���@؇�,���11  @؇�""' @؇�""' @6��� �@6��� ��c���cI{�c���cI{11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?333@   @@  @�33@�  @���@�  A��A��A!��A@  Aa��A�  A���A���A���A���A���A���A���B   B  B  B  B   B'��B0ffB8ffB@ffBH��BP  BXffB`ffBh  BpffBx  B�33B�33B�  B�  B�  B�  B�  B���B���B���B�  B�33B�33B�  B�33B�33B�33B�ffB�33B̙�Bϙ�B���B�  B�33B�ffB�  B�  B�33B�33B�ffB���B���C   C�fC  C�fC�fC	�fC  C�fCL�C33C�CL�CL�C�C  C��C   C"33C$L�C&  C'��C)��C+�fC.  C0�C2�C4  C6�C8�C:L�C<  C=��C@  CB  CD�CF33CH�CI�fCL�CN�CP  CQ�fCS��CV�CX33CZ33C\33C^�C`33Ca��Cc�fCe�fCh  Cj�Cl  Cn33Co��Cq��Cs�fCv  Cx  Cz  C|�C~�C��C��C�  C�  C��C��C��fC��C�&fC��C�  C��C�  C��fC��3C�  C��C��C��C�&fC��C��fC�  C��C��C��C��C�&fC�&fC�  C��fC�  C��C��C��3C��C��C�  C�&fC��C��3C��C��C��3C��C��C�  C�&fC��C�  C�  C��3C��C��C�  C�  C��fC��C��C�  C��C��C�  C��C��C��3C��C��C�  C�&fC�&fC��C��C�  C�  C��3C��C��C�  C�  C��3C��C��C�  C�  C�  C�  C�  C�  C��3C�  C��C��C��C��C��C��C��C��C��3C��fC��3C��3C��C��C��C��C��C��C�  C��C�  C�  C��3C��C�&fC��C�  C�  C�  C��3C�  C��3C�&fC��3C�&fC�L�C��C�ٚD � D3Dy�D�D��D�3DS3D  D��D� DFfD�3D� DL�D�3D �fD#�D%�fD(�D*�3D-FfD/�3D2Y�D4��D7Y�D9ٚD<l�D>�fDAy�DD�DF�3DI,�DK� DN` DQ�DS�fDVffDY�D[�3D^` Da3DcٚDf�3DiS3DlfDn��Dq@ DsٚDvs3Dy  D{��D}��D�  D�ffD���D�� D�33D�p D�� D�� D�6fD�|�D���D�3D�I�D�� D�ٚD�&fD�l�D���D�fD�L�D��fD��3D�6fD��fD�� D��D�l�D�� D�3D�L�D���D��D�<�D���D�ٚD�0 D�� D�� D�#3D�p D��3D��3D�<�D��3D��fD���D�0 D�VfD�� D�� D��fD�3D�33D�S3D�i�D�y�DƉ�Dǣ3DȶfD���D���D��3D��fD���D�3D�3D�	�D��D�	�D�	�D�  D�  D�  D�3D��D�3D�  D�#3D�0 D�@ D�P D�` D�l�D�vfD㉚D䙚D��D��3D��3D���D��fD�3D��D�)�D�9�D�P D�ffD� D�fD�3D�ɚD�ٚD��fD� D�&fD�<�D�@ D�Y�D�s3D���D��fE k3E  E��E$�E�fEH EٚEh E�3E� E( E�3EL�E�fE	3E
$�EC3E` E	�E!�E1�EFfE�3E� EffEl�E� E�3EffEi�E�3E�Ek3E l�E!��E#  E$3E%��E&�fE(fE)  E*��E+��E-33E.1�E/�3E0�3E2�E3 E4~fE5�3E6ɚE8#3E9x E:�3E< E?fEA�3EED�EH�fEK[3EN�3EQ�3ET��EW��E[)�E^nfEa@ Ed�fEg��Ej��Em�3Eq.fEt�Ew;3EzI�E}� E�D E��3E�` E���E�{3E�&fE��fE�H�E�� E�L E�� E�h E��E�O3?   ?   >���?��?   ?��?   ?   ?   ?   ?   >���>���>���?   ?��?��?333?333?fff?fff?���?�33?���?ٙ�@ff@��@   @,��@Fff@S33@fff@y��@�ff@�  @���@�ff@�33@���@�ff@�33@�  @�  @���A��A��A��A33A!��A(  A1��A8  A>ffAFffANffAT��A\��Ad��Al��As33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441414444444411414141111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ?fff?���@   @`  @�33@�  @���@�  A	��A��A)��AH  Ai��A�  A���A���A���A���A���A���A���B  B
  B  B  B"  B)��B2ffB:ffBBffBJ��BR  BZffBbffBj  BrffBz  B�33B�33B�  B�  B�  B�  B�  B���B���B���B�  B�33B�33B�  B�33B�33B�33B�ffB�33B͙�BЙ�B���B�  B�33B�ffB�  B�  B�33B�33B�ffB���B���C � CffC� CffCffC
ffC� CffC��C�3C��C��C��C��C� CL�C � C"�3C$��C&� C(L�C*L�C,ffC.� C0��C2��C4� C6��C8��C:��C<� C>L�C@� CB� CD��CF�3CH��CJffCL��CN��CP� CRffCTL�CV��CX�3CZ�3C\�3C^��C`�3CbL�CdffCfffCh� Cj��Cl� Cn�3CpL�CrL�CtffCv� Cx� Cz� C|��C~��C�Y�C�L�C�@ C�@ C�Y�C�L�C�&fC�L�C�ffC�Y�C�@ C�Y�C�@ C�&fC�33C�@ C�L�C�L�C�Y�C�ffC�L�C�&fC�@ C�L�C�L�C�Y�C�Y�C�ffC�ffC�@ C�&fC�@ C�Y�C�L�C�33C�Y�C�L�C�@ C�ffC�L�C�33C�Y�C�L�C�33C�Y�C�L�C�@ C�ffC�Y�C�@ C�@ C�33C�Y�C�L�C�@ C�@ C�&fC�L�C�L�C�@ C�Y�C�Y�C�@ C�Y�C�L�C�33C�Y�C�L�C�@ C�ffC�ffC�L�C�L�C�@ C�@ C�33C�Y�C�Y�C�@ C�@ C�33C�Y�C�Y�C�@ C�@ C�@ C�@ C�@ C�@ C�33C�@ C�L�C�L�C�L�C�L�C�Y�C�Y�C�Y�C�Y�C�33C�&fC�33C�33C�Y�C�Y�C�Y�C�L�C�L�C�L�C�@ C�L�C�@ C�@ C�33C�Y�C�ffC�L�C�@ C�@ C�@ C�33C�@ C�33C�ffC�33C�ffC���C�L�D �D � D33D��D,�D��D3Ds3D  DٚD� DffD3D� Dl�D3D �fD#9�D%�fD(9�D*�3D-ffD/�3D2y�D4��D7y�D9��D<��D?fDA��DD,�DF�3DIL�DK� DN� DQ,�DS�fDV�fDY,�D[�3D^� Da33Dc��Df�3Dis3Dl&fDn��Dq` Ds��Dv�3Dy  D{��D}��D�0 D�vfD���D�  D�C3D�� D�� D�  D�FfD���D���D�3D�Y�D�� D��D�6fD�|�D���D�fD�\�D��fD��3D�FfD��fD�� D�,�D�|�D�� D�3D�\�D���D���D�L�D���D��D�@ D�� D�� D�33D�� D��3D�3D�L�D��3D��fD��D�@ D�ffD�� D�� D��fD�#3D�C3D�c3D�y�Dŉ�Dƙ�Dǳ3D��fD���D���D�3D�fD��D�3D�3D��D��D��D��D� D� D� D�3D��D�#3D�0 D�33D�@ D�P D�` D�p D�|�D�fD㙚D䩚D��D��3D��3D���D�fD�3D�)�D�9�D�I�D�` D�vfD� D�fD��3D�ٚD��D�fD�  D�6fD�L�D�P D�i�D��3D���D��fE s3E E��E,�E�fEP E�Ep E3E� E0 E�3ET�E�fE	3E
,�EK3Eh E�E)�E9�ENfE�3E� EnfEt�E� E�3EnfEq�E�3E�Es3E t�E"�E# E$3E%��E&�fE(&fE)( E*��E+��E-;3E.9�E/�3E0�3E2$�E3  E4�fE5�3E6њE8+3E9� E:�3E<  E?fEB3EEL�EH�fEKc3EN�3EQ�3ETɚEX�E[1�E^vfEaH Ed�fEg��Ej��Em�3Eq6fEt�EwC3EzQ�E}� E�H E��3E�d E���E�3E�*fE��fE�L�E�� E�P E�� E�l E��E�S3G�O�G�O�?L��G�O�?�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?fff?�  G�O�?���G�O�?���G�O�?�33?ٙ�?�33@ff@��@&ff@,��@@  @L��@fff@s33@�33@���@�ff@�  @���@�ff@�33@���@�ff@�33@�  A   AffA��A��A��A#33A)��A0  A9��A@  AFffANffAVffA\��Ad��Al��At��A{33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441414444444411414141111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                        @ �@ �@ �@ {@ �@ ""@ (G@ /�@ 7L@ >@ D�@ Q�@ `B@ m:@ {�@ ��@ ��@ ��@ �-@ ��@ �|@ �t@ �@ ��@j@@
@-@:�@H]@V�@b�@qS@~�@��@�H@�A@��@�>@�7@��@�@�,@�@�@!s@.l@=q@K�@Yn@ff@t�@�d@�@�a@�Y@�^@Ĝ@��@�H@�@��@
=@�@&;@3�@B8@M$@[z@i�@v�@�@�@��@�f@��@ȴ@�@�@�@@�@�@(�@5?@DD@SI@a�@m:@y�@�+@��@��@�-@��@��@�#@��@��@j@�@�@,`@:�@I@V@bN@qS@~�@��@��@��@��@��@є@�;@�4@��@v@�@!s@/�@>@K@Z@e	@r�@�@�\@�@��@�@ƨ@��@��@��@��@�@�@$.@3�@B�@O�@\)@k.@ww@��@�@�m@��@�k@��@�@�`@��@  @V@�@*S@7�@FQ@S�@_�@k�@z�@��@��@�(@��@��@��@܀@��@�@	�@	�@	
@	-�@	:�@	G�@	Wb@	dZ@	p�@	~K@	�D@	��@	��@	��@	@	��@	ލ@	�4@	�,@
1@
�@
""@
1'@
>@
Ji@
Z@
g@
t@
��@
�h@
��@
�Y@
�R@
��@
��@
�@
�L@
��@
=@6@&�@4�@@�@N�@\)@i�@ww@�@�@�m@��@�k@�@׹@�@�@^@@�@'�@5�@C�@SI@`�@n�@{�@�7@��@��@�-@�&@��@��@�y@��@@@�@,`@9X@G�@T�@e	@o�@�W@�@�H@�5@��@Ĝ@ψ@�;@�@@��@��@Ӡ@
@i�@��@��@G�@��@�@g@e�@��@�@@5?@{�@�2@%@I@��@��@B@\�@�(@�y@.l@uk@��@j@Lu@��@ލ@&�@oF@�R@@M�@�<@�T@-@uk@��@�@I�@�\@��@@UU@��@��@%�@j@�@�Y@6�@|?@��@%@K�@�i@�
@[@dZ@��@�@ 7�@ }�@ ��@!
�@!SI@!��@!�H@"(G@"o�@"��@"�E@#C�@#��@#�C@$�@$bN@$�M@$�Y@%:@%��@%�@&@&V@&�H@&��@'&;@'k.@'�f@'��@(-�@(p�@(�~@(�L@)0x@)m�@)�Y@)�@* �@*Z�@*��@*є@+�@+F�@+�d@+��@+�@,)�@,`A@,�<@,ψ@-v@-<@-p�@-�A@-��@.*@.M�@.��@.�&@.�q@//�@/i�@/��@/��@06@0O�@0��@0Ĝ@0�Q@1:�@1t�@1��@1�y@2"�@2^5@2�<@2�C@3�@3I@3�@3��@3�E@48�@4r�@4�r@4�@5&�@5bN@5��@5խ@6�@6P�@6�P@6��@7
=@7H]@7�+@7�J@8j@8A�@8~K@8�@8��@9=q@9|?@9�^@9�}@:uk@:��@;k.@;�@<�I@=�@=��@=��@>��@?O@?�w@@.l@@Ӡ@A>�@A��@BO1@B�@C`B@D�@Dr�@E�@E��@E��@F�z@G�@G�@H(�@H��@I?}@I�@JR�@J�@K]�@K��@Li�@M�@M��@M�E@N��@O""@O��@P@�@Q�p@R��@T+@U��@V��@X*S@Y��@Zƨ@\&;@]�d@^�l@`O@a�p@b�(@dg@euk@f�l@h$/@i|?@j�@l6�@mt@nȴ@pO@qqS@r��@t.l@uy�@v�#@x{@ym:@z@|{@}x&@}�9G�O�G�O�@ G�O�@ jG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@ �@ jG�O�@ G�O�@ �G�O�@ %@ 1@ 	�@ 
�@ �@ V@ @ @ o@ *@ �@ �@ �@ �@ �@  �@ #�@ &;@ (G@ *S@ -@ /�@ 33@ 5�@ 8�@ <@ ?}@ B8@ D�@ G�@ K�@ N�@ Q=@ T�@ X@ Z�@ ^5@ a�@ e	@ g�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�n�A�jA�z�A�v�A�v�A�~�A�~�A�z�A�z�A�z�A�z�AցA֋DA։7A�|�A�hsA�E�A�7LA�33A�5?A�1'A�+A�%A�ZA�bA���A���A��A���AԬAԍPAԋDAԇ+A�Q�A���A�|�A��/A�bNAǮAċDA�?}A�n�A��7A�1'A�jA�dZA�"�A�$�A��A���A�A�ƨA��uA���A�ĜA��HA�
=A�\)A�~�A�n�A��TA��+A�r�A�JA�bA�ZA��;A�r�A��yA�%A�XA���A���A��A���A��A���A� �A��
A��7A�7LA���A��^A��uA�C�A�"�A�+A���A��A�A���A�v�A��A�{A�oA��7A�ƨA���A�JA��hA���A��A��A�hsA�G�A�XA�p�A��RA�7LA���A��A���A�r�A�oA��uA�{A��A�t�AƨAx�A~$�Az�+Aw�
Au�hAt�+As��As�ApE�Ao`BAo�Al��Aj��Ag�Ad�yAc�Aa"�A_�A\�HAZQ�AX�AWoAV1AUS�AT��AT  AR�`AR1AQO�AP�9AP�AO�AN��AN1AM��AM�7AMt�AM�ALQ�AK?}AI�AHVAFA�AD�AA�wA@��A@9XA@1A?�A>JA=x�A<1'A:�A8�!A7x�A4�A2�jA2bA1`BA1+A0�A0(�A.�DA-O�A+�A)l�A(Q�A'|�A&�/A&VA%K�A#G�A"��A"1'A!�A z�A�A1'A��A�FA�7AȴA{AO�A-A��A�AdZA�mA��AjA��A�HAA�A��Ap�A
jA	�mA�\A�PAS�AK�A?}AVA��Ap�A�A�^A��A ff@�"�@��!@��7@��j@�t�@���@�n�@�^5@�M�@��@��h@��@��9@��9@�@�ff@�&�@�j@�j@�9X@�@�+@�K�@�;d@�-@�G�@� �@��@�p�@�=q@�1@��H@�hs@��;@�E�@�j@��@��@�ȴ@�@�O�@���@��H@�33@�A�@���@���@�V@�9X@��@���@���@�Q�@�;d@��@���@��H@�n�@��@�p�@��@� �@�@�5?@��`@��j@�1@��+@�x�@�Z@�S�@�5?@���@���@���@��@��u@��@��y@�$�@�7L@���@�S�@��#@�Z@
=@}V@{��@y��@w+@t��@s�m@oK�@m�T@lj@j�H@g�w@f5?@e�h@cdZ@a��@`1'@^5?@]�@\Z@Z�\@YX@W��@V�y@U�@Tj@S@Q��@P �@Nv�@M?}@L�@L9X@J�\@I�#@HQ�@D��@Co@B-@@�`@?�P@?�@>@=�h@;��@:M�@9��@9&�@8  @6V@5O�@41@2�\@2^5@1��@01'@/�@.E�@-�-@,��@+�@*��@)��@)G�@(bN@'�@%��@$�@#�@!��@ �`@�@�R@V@��@��@�@ƨ@��@��@7L@ �@\)@5?@?}@��@�H@n�@G�@&�@��@�9@��@��@5?@��@�@Z@�
@33@
=q@	hs@	7L@Ĝ@�u@ �@�w@
=@E�@@��@��@j@ƨ@33@-@��@x�@x�@ �9?��w?�v�?�V?��?��-?�j?�"�?��?��+?��
?�M�?�Ĝ?��?�?�C�?���?�l�?��?䛦?�J?��`?��?��?ޗ�?�/?�C�?��#?��?��?�+?�E�?ա�?��?ӕ�?���?�G�?Ѓ?��;?Η�?�v�?�{?�V?�j?�dZ?�^5?�7L?�b?�
=?�E�?��?���?�A�?�\)?���?��R?��h?�I�?�ƨ?��?�dZ?�dZ?�dZ?�"�?�dZ?�"�?�?�"�?�"�?�dZ?���?��m?�I�?���?�O�?��-?�5??���?�;d?�  ?���?�%?�%A�~�A�z�A�v�A�p�A�p�A�t�A�n�A�p�A�bNA�dZA�dZA�dZA�dZA�hsA�jA�hsA�jA�ffA�ffA�dZA�ffA�dZA�hsA�x�A�z�A�z�A�x�A�z�A�v�A�t�A�v�A�t�A�t�A�x�A�z�A�~�A�~�A�|�A�~�A�~�A�z�A�z�A�x�A�x�A�z�A�z�A�x�A�z�A�z�A�z�A�z�A�z�AցAև+A֋DA։7A֋DA֋DA։7A։7G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                        A�n�A�jA�z�A�v�A�v�A�~�A�~�A�z�A�z�A�z�A�z�AցA֋DA։7A�|�A�hsA�E�A�7LA�33A�5?A�1'A�+A�%A�ZA�bA���A���A��A���AԬAԍPAԋDAԇ+A�Q�A���A�|�A��/A�bNAǮAċDA�?}A�n�A��7A�1'A�jA�dZA�"�A�$�A��A���A�A�ƨA��uA���A�ĜA��HA�
=A�\)A�~�A�n�A��TA��+A�r�A�JA�bA�ZA��;A�r�A��yA�%A�XA���A���A��A���A��A���A� �A��
A��7A�7LA���A��^A��uA�C�A�"�A�+A���A��A�A���A�v�A��A�{A�oA��7A�ƨA���A�JA��hA���A��A��A�hsA�G�A�XA�p�A��RA�7LA���A��A���A�r�A�oA��uA�{A��A�t�AƨAx�A~$�Az�+Aw�
Au�hAt�+As��As�ApE�Ao`BAo�Al��Aj��Ag�Ad�yAc�Aa"�A_�A\�HAZQ�AX�AWoAV1AUS�AT��AT  AR�`AR1AQO�AP�9AP�AO�AN��AN1AM��AM�7AMt�AM�ALQ�AK?}AI�AHVAFA�AD�AA�wA@��A@9XA@1A?�A>JA=x�A<1'A:�A8�!A7x�A4�A2�jA2bA1`BA1+A0�A0(�A.�DA-O�A+�A)l�A(Q�A'|�A&�/A&VA%K�A#G�A"��A"1'A!�A z�A�A1'A��A�FA�7AȴA{AO�A-A��A�AdZA�mA��AjA��A�HAA�A��Ap�A
jA	�mA�\A�PAS�AK�A?}AVA��Ap�A�A�^A��A ff@�"�@��!@��7@��j@�t�@���@�n�@�^5@�M�@��@��h@��@��9@��9@�@�ff@�&�@�j@�j@�9X@�@�+@�K�@�;d@�-@�G�@� �@��@�p�@�=q@�1@��H@�hs@��;@�E�@�j@��@��@�ȴ@�@�O�@���@��H@�33@�A�@���@���@�V@�9X@��@���@���@�Q�@�;d@��@���@��H@�n�@��@�p�@��@� �@�@�5?@��`@��j@�1@��+@�x�@�Z@�S�@�5?@���@���@���@��@��u@��@��y@�$�@�7L@���@�S�@��#@�Z@
=@}V@{��@y��@w+@t��@s�m@oK�@m�T@lj@j�H@g�w@f5?@e�h@cdZ@a��@`1'@^5?@]�@\Z@Z�\@YX@W��@V�y@U�@Tj@S@Q��@P �@Nv�@M?}@L�@L9X@J�\@I�#@HQ�@D��@Co@B-@@�`@?�P@?�@>@=�h@;��@:M�@9��@9&�@8  @6V@5O�@41@2�\@2^5@1��@01'@/�@.E�@-�-@,��@+�@*��@)��@)G�@(bN@'�@%��@$�@#�@!��@ �`@�@�R@V@��@��@�@ƨ@��@��@7L@ �@\)@5?@?}@��@�H@n�@G�@&�@��@�9@��@��@5?@��@�@Z@�
@33@
=q@	hs@	7L@Ĝ@�u@ �@�w@
=@E�@@��@��@j@ƨ@33@-@��@x�@x�@ �9?��w?�v�?�V?��?��-?�j?�"�?��?��+?��
?�M�?�Ĝ?��?�?�C�?���?�l�?��?䛦?�J?��`?��?��?ޗ�?�/?�C�?��#?��?��?�+?�E�?ա�?��?ӕ�?���?�G�?Ѓ?��;?Η�?�v�?�{?�V?�j?�dZ?�^5?�7L?�b?�
=?�E�?��?���?�A�?�\)?���?��R?��h?�I�?�ƨ?��?�dZ?�dZ?�dZ?�"�?�dZ?�"�?�?�"�?�"�?�dZ?���?��m?�I�?���?�O�?��-?�5??���?�;d?�  ?���?�%?�%A�~�A�z�A�v�A�p�A�p�A�t�A�n�A�p�A�bNA�dZA�dZA�dZA�dZA�hsA�jA�hsA�jA�ffA�ffA�dZA�ffA�dZA�hsA�x�A�z�A�z�A�x�A�z�A�v�A�t�A�v�A�t�A�t�A�x�A�z�A�~�A�~�A�|�A�~�A�~�A�z�A�z�A�x�A�x�A�z�A�z�A�x�A�z�A�z�A�z�A�z�A�z�AցAև+A֋DA։7A֋DA֋DA։7A։7G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�}B�}B�wB�wB�wB�qB�qB�qB�qB�wB�qB�}B�}B�}B�qB�dB�^B�^B�^B�dB�^B�wB�B7LBN�BL�BT�BbNBbNBbNBe`Bk�Bl�Be`B[#BT�BZBR�BjBz�B�B�7B�=B�{B�hB��B��B��B��B��B��B�B�B��B��B��B�B��B��B��B��B��B��B�{B�\B�B~�B~�B]/BB�B:^B8RB2-B+B&�B �B�B�B{BoBhBoB�B�BB��B�B�B�sB�;B��BɺB�RB��B��B�oB�Bz�Bk�BhsB[#BP�B<jB-B�B
�mB
�/B
��B
��B
�B
��B
�1B
�B
{�B
t�B
dZB
^5B
YB
S�B
R�B
?}B
/B
bB
B	��B	��B	�B	�ZB	�BB	�/B	ƨB	�dB	�B	��B	��B	��B	��B	��B	��B	�DB	�B	~�B	x�B	t�B	o�B	hsB	`BB	\)B	W
B	S�B	P�B	M�B	H�B	G�B	E�B	E�B	A�B	>wB	:^B	49B	-B	#�B	�B	hB	\B	JB	
=B	+B	B	  B��B�B�`B�BǮB�}B�dB�FB�?B�-B�B��B��B�uB�1B�B{�By�Bw�Bs�Br�Br�Bo�Bo�BffBhsBffBe`BdZBbNBbNB`BB_;BZBYB]/BXBR�BO�BL�BK�BI�BH�BI�BE�BG�BC�BA�BD�BD�BC�BB�BB�BB�BA�BA�BB�B;dB=qB@�B>wB?}B=qB<jB=qB<jB<jB<jB;dB;dB:^B:^B9XB<jB9XB;dB:^B:^B9XB9XB9XB9XB9XB9XB9XB7LB9XB7LB6FB6FB6FB5?B8RB2-B9XB:^B9XB?}BP�BW
BdZBm�Bs�By�B~�B� B�hB��B��B�qB��B�TB�B	1B	&�B	9XB	E�B	^5B	k�B	x�B	�+B	��B	��B	��B	�B	�!B	�FB	�^B	��B	ǮB	��B	��B	�)B	�HB	�`B	�sB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
B
1B
	7B
JB
\B
uB
oB
{B
{B
�B
�B
�B
�B
�B
�B
"�B
#�B
$�B
&�B
'�B
+B
+B
,B
-B
.B
0!B
2-B
5?B
5?B
6FB
7LB
8RB
9XB
:^B
>wB
?}B
@�B
B�B
C�B
C�B
D�B
D�B
G�B
G�B
H�B
H�B
H�B
K�B
K�B
M�B
O�B
N�B
O�B
P�B
Q�B
R�B
S�B
T�B
T�B
W
B
W
B
W
B
XB
ZB
[#B
[#B
[#B
^5B
_;B
aHB
bNB
bNB
cTB
cTB
dZB
dZB
e`B
ffB
ffB
gmB
gmB
hsB
iyB
k�B
l�B
l�B
n�B
n�B
n�B
n�B
p�B
q�B
q�B
q�B
r�B
r�B
s�B
t�B
v�B
v�B
v�B
w�B
x�B
v�B
x�B
y�B
y�B
z�B
{�B
{�B
|�B
}�B
~�B
~�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�+B
�+B
�7B
�=B
�DB
�PB
�VB
�VB
�bB
�oB
�uB
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
��B
��B
�B
�B
�B
�B
�'B
�'B
�-B
�3B
�9B
�FB
�FB
�FB
�LB
�LB
�LB
�RB
�RB
�XB
�^B
�^B
�^B
�^B
�dB
�dB
�dB
�dB
�dB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�qB�}B�wB�}B��B�qB�}B��B�wB��B�}B�wB�wB�}B�}B�wB�}B�wB�}B�}B�}B�}B�}B��B�}B�wB�}B�}B�qB�wB�}B�wB�wB�wB�wB�wB�qB�qB�wB�qB�qB�qB�qB�qB�qB�qB�qB�wB�qB�qB�qB�qB�wB��B��B�}B�}B�}B�}B�}B�}G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                        B�qB�qB�jB�jB�jB�dB�dB�dB�dB�jB�dB�qB�qB�qB�dB�XB�RB�RB�RB�XB�RB�jB�B5?BL�BJ�BR�B`BB`BB`BBcTBiyBjBcTBYBR�BXBP�BhsBx�B�B�+B�1B�oB�\B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B�oB�PB�B|�B|�B[#B@�B8RB6FB0!B(�B$�B�B�B�BoBbB\BbB{BuB  B��B�B�B�fB�/B��BǮB�FB��B��B�bB�Bx�BiyBffBYBN�B:^B+B�B
�`B
�#B
��B
�}B
��B
��B
�%B
�B
y�B
r�B
bNB
\)B
W
B
Q�B
P�B
=qB
-B
VB
  B	��B	��B	�B	�NB	�5B	�#B	ĜB	�XB	�B	��B	��B	��B	��B	��B	�uB	�7B	�B	|�B	v�B	r�B	m�B	ffB	^5B	ZB	T�B	Q�B	N�B	K�B	F�B	E�B	C�B	C�B	?}B	<jB	8RB	2-B	+B	!�B	�B	\B	PB	
=B	1B	B	  B��B��B�B�TB�BŢB�qB�XB�9B�3B�!B��B��B��B�hB�%B� By�Bw�Bu�Bq�Bp�Bp�Bm�Bm�BdZBffBdZBcTBbNB`BB`BB^5B]/BXBW
B[#BVBP�BM�BJ�BI�BG�BF�BG�BC�BE�BA�B?}BB�BB�BA�B@�B@�B@�B?}B?}B@�B9XB;dB>wB<jB=qB;dB:^B;dB:^B:^B:^B9XB9XB8RB8RB7LB:^B7LB9XB8RB8RB7LB7LB7LB7LB7LB7LB7LB5?B7LB5?B49B49B49B33B6FB0!B7LB8RB7LB=qBN�BT�BbNBk�Bq�Bw�B|�B}�B�\B��B��B�dB��B�HB�B	%B	$�B	7LB	C�B	\)B	iyB	v�B	�B	��B	��B	��B	��B	�B	�?B	�XB	�}B	ƨB	��B	��B	�#B	�BB	�ZB	�mB	�yB	�B	�B	�B	�B	��B	��B	��B	��B	��B
B
+B
1B
DB
VB
oB
hB
uB
uB
�B
�B
�B
�B
�B
�B
!�B
"�B
#�B
%�B
&�B
)�B
)�B
+B
,B
-B
/B
1'B
49B
49B
5?B
6FB
7LB
8RB
9XB
=qB
>wB
?}B
A�B
B�B
B�B
C�B
C�B
F�B
F�B
G�B
G�B
G�B
J�B
J�B
L�B
N�B
M�B
N�B
O�B
P�B
Q�B
R�B
S�B
S�B
VB
VB
VB
W
B
YB
ZB
ZB
ZB
]/B
^5B
`BB
bNB
bNB
cTB
cTB
dZB
dZB
e`B
ffB
ffB
gmB
gmB
hsB
iyB
k�B
l�B
l�B
n�B
n�B
n�B
n�B
p�B
q�B
q�B
q�B
r�B
r�B
s�B
t�B
v�B
v�B
v�B
w�B
x�B
v�B
x�B
y�B
y�B
z�B
{�B
{�B
|�B
}�B
~�B
~�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�+B
�+B
�7B
�=B
�DB
�PB
�VB
�VB
�bB
�oB
�uB
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
��B
�B
�B
�B
�B
�!B
�-B
�-B
�3B
�9B
�FB
�RB
�RB
�RB
�XB
�XB
�XB
�^B
�^B
�dB
�jB
�jB
�jB
�jB
�qB
�qB
�qB
�wB
�wB
�}B
�}B
�}B
�}B
�}B
�}B
�}B
��B�qB�jB�qB�wB�dB�qB�}B�jB�wB�qB�jB�jB�qB�qB�jB�qB�jB�qB�qB�qB�qB�qB�wB�qB�jB�qB�qB�dB�jB�qB�jB�jB�jB�jB�jB�dB�dB�jB�dB�dB�dB�dB�dB�dB�dB�dB�jB�dB�dB�dB�dB�jB�wB�wB�qB�qB�qB�qB�qB�qG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                        <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201810200125172021061413524620210614135246202106141746482021061417464820210614174648201810200125172021061413524620210614135246202106141746482021061417464820210614174648PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                    surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018102001251720181020012517  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018102001251720181020012517QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018102001251720181020012517QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015420210722160154IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                