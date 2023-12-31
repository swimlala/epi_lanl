CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  	   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:53Z creation      
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
resolution        =���   axis      Z        H  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   L   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     H  P0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `x   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     H  d�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  t�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  �0   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �x   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  �0   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �x   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  ֌   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   <   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   L   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   T   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � \   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                        HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar            HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        (   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �|   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �|   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � |Argo profile    3.1 1.2 19500101000000  20180724220253  20210617131459  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�q{�"��@�q{�"��11  @�q{�9@@�q{�9@@6ե��v@6ե��v�c��f���c��f��11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?�  @   @@  @�  @�  @�33@�ffA��AffA$��AC33A`  A~ffA�  A���A�  A�  A�  A���A���B ��B��B  B��B��B(  B/��B8ffB@ffBHffBPffBX  B`  BhffBpffBx  B�  B�33B�  B���B���B���B�  B�33B�ffB�ffB�33B���B���B���B�  B�33B�33B�33B���B���B���B�ffB�ffB�33B�  B���B�ffB�33B�  B���B�ffB�  C   C��C�C  C��C
�CL�C�C  CL�C33C�C�C�fC�C  C�fC"33C$�C%�fC(33C*�C,�C.  C/��C2�C4�C5��C8�C:  C<  C>L�C@33CB�CDL�CFL�CH�CJ�CL  CN33CP�CQ�fCT33CV  CW�fCZ33C\33C^�C`  Ca��Cd33Cf�Ch  Cj  Ck�fCn�Cp�Cq�fCt�Cv�Cw��Cz�C|L�C~33C��C�&fC��C��fC�  C��C�&fC��C��3C��C��C�  C��fC��fC��3C��C��C�&fC�  C��fC��fC�  C��C��C��C�  C��fC��fC��3C�  C��C�&fC�  C�ٚC��fC�  C��C�  C��fC�  C��C�  C��3C��C�  C��fC��C�  C��fC�  C��C�&fC��C�  C��C�&fC��C�  C��C�  C��fC�  C��C�&fC��C��fC�  C��C��C�  C�ٚC��fC��fC��3C��C��C��C�33C��C��fC��3C�  C��C�&fC��C��3C��C�&fC��C��fC��3C�  C��C�&fC��C��3C�  C��C��C��C��fC��3C�  C��C�&fC��C��fC�  C��C�&fC�  C��fC��3C��C��C�  C��fC�  C��C�&fC��C��3C��3C���DffDfD� D
,�D��Dl�D33D��D� DS3D�D�3D"��D%Y�D(3D*��D-� D0Y�D3�D5��D8ffD;  D=��D@S3DB�3DE�fDH�DJ��DM3DO��DR�DT�3DWfDYl�D[�3D^L�D`��Dc&fDe� Dh33Dj�fDm  Do� Dr33Dt�fDwS3Dy�fD|fD~�3D��3D���D�&fD�s3D�� D� D�Y�D�� D��D�33D�|�D��fD�fD�I�D��3D�� D�3D�C3D�y�D��fD��fD�33D�p D��3D�� D�33D�l�D��fD��3D��D�VfD���D�� D���D�3D�9�D�c3D���D���D��fD���D�fD�9�D�S3D�s3D���D��fD�� D��3D�� D�	�D�0 D�I�D�ffD��fD£3D��fD��3D�  D�  D�6fD�S3D�l�DˆfD̙�Dͳ3D�ٚD���D��D�@ D�c3DԆfDթ�D�� D��fD��D�9�D�c3D܀ Dݠ D޼�D��3D���D��D�6fD�P D�p D�3D��D�3D���D��fD�  D� D�  D�0 D�<�D�S3D�` D�p D�y�D�� D��fD�� D��3D���D��3D��3D��3D���D��3D�� E p E �Et�E��E� EfE��E E�fE3E.fE��E�fE	њE[3E` E��E� Et�E~fE� E�E E��E��E3EfE�fE�3E�E  E s3E!�E"��E$T�E%L�E&��E(#3E) E*��E+�E,ٚE.;3E/� E0� E1� E3+3E4vfE5��E7�E8a�E9FfE:� E;�fE?,�EB�EEp EHY�EK��EN�fEQ� ET�3EXfE[#3E^�Eah Ed��Egk3Ej�3Em�fEp��Et�Ew$�Ez)�E}�fE�FfE�� E�o3E�3E��3E� E���E��3E�O3E�� E���E�=�E���E��fE�&fE�h E�ɚE�3E�l E��fE�3E�H E���E��E�?3E��fE��3E�.f>���>���>���>���?   >���?   >���>���>���>���>���>���?   ?   ?   ?��?��?L��?fff?���?���?�  ?�  ?ٙ�?�ff@ff@33@&ff@333@Fff@Y��@s33@�  @���@�ff@���@���@���@�  @�  @�  @���@���A33A	��A33A��A!��A)��A0  A8  A>ffAFffAL��AVffA[33Ac33Ak33As33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414141441414144141114141111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ?fff?�  @   @`  @�  @�  @�33@�ffA	��AffA,��AK33Ah  A�33A�  A���A�  A�  A�  A���A���B��B
��B  B��B!��B*  B1��B:ffBBffBJffBRffBZ  Bb  BjffBrffBz  B�  B�33B�  B���B���B���B�  B�33B�ffB�ffB�33B���B���B���B�  B�33B�33B�33B���B���B���B�ffB�ffB�33B�  B���B�ffB�33B�  B���B�ffB�  C � CL�C��C� CL�C
��C��C��C� C��C�3C��C��CffC��C� C ffC"�3C$��C&ffC(�3C*��C,��C.� C0L�C2��C4��C6L�C8��C:� C<� C>��C@�3CB��CD��CF��CH��CJ��CL� CN�3CP��CRffCT�3CV� CXffCZ�3C\�3C^��C`� CbL�Cd�3Cf��Ch� Cj� ClffCn��Cp��CrffCt��Cv��CxL�Cz��C|��C~�3C�L�C�ffC�L�C�&fC�@ C�Y�C�ffC�Y�C�33C�L�C�Y�C�@ C�&fC�&fC�33C�L�C�L�C�ffC�@ C�&fC�&fC�@ C�L�C�Y�C�Y�C�@ C�&fC�&fC�33C�@ C�L�C�ffC�@ C��C�&fC�@ C�L�C�@ C�&fC�@ C�Y�C�@ C�33C�L�C�@ C�&fC�L�C�@ C�&fC�@ C�Y�C�ffC�Y�C�@ C�Y�C�ffC�Y�C�@ C�Y�C�@ C�&fC�@ C�L�C�ffC�L�C�&fC�@ C�L�C�Y�C�@ C��C�&fC�&fC�33C�L�C�L�C�Y�C�s3C�L�C�&fC�33C�@ C�Y�C�ffC�Y�C�33C�L�C�ffC�L�C�&fC�33C�@ C�Y�C�ffC�L�C�33C�@ C�L�C�Y�C�L�C�&fC�33C�@ C�Y�C�ffC�Y�C�&fC�@ C�L�C�ffC�@ C�&fC�33C�L�C�Y�C�@ C�&fC�@ C�Y�C�ffC�Y�C�33C�33C��D�fD&fD� D
L�D��D��DS3D�D� Ds3D,�D�3D"��D%y�D(33D*��D-� D0y�D3,�D5��D8�fD;@ D=ٚD@s3DC3DE�fDH,�DJ��DM33DO��DR9�DT�3DW&fDY��D[�3D^l�D`ٚDcFfDe� DhS3Dj�fDm@ Do� DrS3Dt�fDws3DzfD|&fD~�3D��3D���D�6fD��3D�� D�  D�i�D�� D���D�C3D���D��fD�fD�Y�D��3D�� D�3D�S3D���D��fD�fD�C3D�� D��3D�  D�C3D�|�D��fD��3D�,�D�ffD���D�� D���D�#3D�I�D�s3D���D���D��fD���D�&fD�I�D�c3D��3D���D��fD�� D��3D�  D��D�@ D�Y�D�vfD��fD³3D��fD��3D� D�0 D�FfD�c3D�|�D˖fD̩�D��3D��D�	�D�,�D�P D�s3DԖfDչ�D�� D�fD�)�D�I�D�s3Dܐ Dݰ D���D��3D��D�,�D�FfD�` D� D�3D��D��3D���D��fD� D�  D�0 D�@ D�L�D�c3D�p D� D�D�� D��fD�� D��3D���D��3D��3D��3D���D��3D�� E x E ��E|�E�E� EfE��E E�fE#3E6fE��E�fE	ٚEc3Eh E��E� E|�E�fE� E�E E��E��E3EfE�fE�3E�E E {3E!�E"��E$\�E%T�E&��E(+3E)  E*��E+�E,�E.C3E/� E0� E1� E333E4~fE5��E7�E8i�E9NfE:� E;�fE?4�EB$�EEx EHa�EK��EN�fEQ� ET�3EXfE[+3E^!�Eap Ed��Egs3Ej�3Em�fEp��Et!�Ew,�Ez1�E}�fE�JfE�� E�s3E�3E��3E� E���E��3E�S3E�� E���E�A�E���E��fE�*fE�l E�͚E�3E�p E��fE�3E�L E���E��E�C3E��fE��3E�2fG�O�?L��G�O�?fffG�O�?L��G�O�G�O�?L��G�O�?L��G�O�?fffG�O�G�O�?�  G�O�?���?�ff?�33G�O�?���G�O�@   @��@33@&ff@333@Fff@S33@fff@y��@���@�  @���@�ff@���@���@���@�  @�  @�  @���AffA33A��A33A!��A)��A1��A8  A@  AFffANffAT��A^ffAc33Ak33As33A{33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414141441414144141114141111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                               @ �@ �@ �@ {@ O@ ""@ )�@ 1'@ 7L@ <�@ FQ@ SI@ _�@ l�@ z�@ �7@ �0@ ��@ �~@ ��@ �|@ ��@ �y@ ��@�@b@�@+�@:�@H]@V@c�@p�@~K@��@�H@�A@��@�>@�7@�/@��@�~@�@*@#�@1'@>@Ji@X@e�@t@�d@�@��@��@��@�J@��@�@�@��@	�@B@&;@33@B�@O�@\)@i�@v@��@��@�@��@��@�@�
@�@�@ �@V@�@)�@6�@C�@SI@`B@l�@|?@�7@��@��@�!@��@�|@�@��@��@j@@ @-@<@I�@V@c�@p�@�@��@��@��@��@��@є@�;@�4@�,@v@�@"�@/�@=q@Ji@Yn@g@s_@�d@�@��@�Y@�^@�W@�O@�T@�@�9@
=@B@'�@4�@@,@O0@]�@i�@v@��@�@�@��@��@�c@խ@�T@�Y@ �@@�@(�@5?@B�@Q=@_�@m�@|�@��@�$@�y@�~@��@��@�@�@� @	j@	b@	g@	,`@	8�@	H]@	UU@	a�@	p�@	�@	��@	��@	�A@	�F@	Ĝ@	є@	��@	��@	�,@
v@
{@
"�@
1�@
>@
I�@
X�@
g@
uk@
��@
�P@
��@
�M@
��@
ƨ@
�O@
�@
�@
�E@�@6@%�@4�@B�@O�@[z@j@y�@��@�h@��@�@�@�o@׹@�@�Y@ �@@�@'�@5�@DD@SI@a�@n�@y�@��@��@��@�~@��@�@�#@�y@��@@@ @.l@;d@F�@T�@`B@��@0x@ww@�@�@Lu@�<@�@,`@v@��@J@X@�(@�@7�@�@ψ@B@`�@�Y@��@<�@��@�o@�@V�@�U@��@$�@i!@��@�@1'@r�@�F@�~@:�@~K@Ĝ@�@K@�\@խ@�@a�@��@��@'�@m�@�9@��@A�@��@�7@�@\)@�z@��@/@uk@��@��@A�@�@�@ V@ P�@ �#@ �h@!�@!_�@!��@!�@"-@"o�@"��@"�q@#9X@#|?@#��@$  @$@,@$~�@$��@$�E@%<@%z3@%�F@%�@&4�@&r�@&��@&�4@'(G@'dZ@'�m@'�#@(�@(S�@(��@(��@)�@)I@)��@)��@* �@*=q@*z�@*�F@*�@+/@+k.@+��@+��@, �@,^5@,�U@,�t@-�@-V�@-��@-Ӡ@.o@.P�@.��@.�|@/
=@/G�@/�p@/�>@/�Q@0<�@0x�@0��@0�Y@1-@1i!@1��@1��@2�@2X�@2��@2��@3�@3@,@3{�@3��@3��@4'�@4_�@4��@4�7@5�@5?}@5t�@5��@5��@6!s@6Yn@6��@6��@7@7<@7t�@7�@7�l@8 �@8X�@8�@8��@9@,@9��@:[z@:�|@;uk@;�@<��@<�,@=�z@>�@>��@?+@?��@@?}@@��@ALu@A��@B[z@B�W@Ck�@C�
@Duk@E*@E�W@F[@F�+@G"�@G�@H%�@H�2@IYn@I��@JV�@J�@KN�@K�H@Lr@L�Q@M��@N[@N�Z@O�@O��@P$�@Q�i@R�C@T=q@U{�@V�@X$�@Y�@Z��@\2�@]�@^�>@`,`@a�7@b�@d4�@e��@f��@h&;@ir�@j�k@l+�@mv@n�&@p(G@q��@r�@t�@ur@u�f@v%@v@�@v{�@vє@w
�@wB�@w�<@w�7@x#�@x[z@x�@x�T@y2�@yi�@y�@y��@z<�@z��@z�w@{�G�O�@ G�O�@ �G�O�@ G�O�G�O�@ G�O�@ G�O�@ �G�O�G�O�@ jG�O�@ @ v@ %G�O�@ �G�O�@ 
=@ �@ J@ V@ �@ �@ @ *@ 6@ �@ O@ [@  @ !s@ $�@ (G@ (�@ ,`@ /�@ 2�@ 5�@ 7�@ :�@ >�@ A�@ D�@ H]@ K@ N�@ Q=@ T�@ Wb@ [z@ ]�@ `�@ dZ@ g�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ZA�XA�O�A�K�A�G�A�E�A�I�A�?}A�-A�&�A�$�A�JA��A��A��TA��;A��;A���A���AϾwAϼjAϼjAϺ^AϸRAϸRAϸRAϴ9Aϰ!AύPA���A�-A�ZA���AʋDA�AÝ�ADA��FA�  A��A�A�VA���A�{A�\)A���A� �A��
A�VA�+A�XA�v�A�oA��A�
=A�hsA�-A��mA��A�A�A�A�=qA��A���A�p�A�JA���A�oA���A�\)A���A���A��HA�C�A�VA�ĜA��A��!A�7LA���A��uA��A��jA�1'A�l�A�ZA��+A�I�A�A�7LA�oA�~�A�{A�n�A���A���A�"�A�(�A��A��^A���A�-A���A�A�VA��!A�r�A��A���A�"�A�%A���A�A�|�A���A�l�Ap�A}�A|bAy�Av�yAr-An�jAm�Am;dAl��AlQ�AlM�AiƨAi
=AhbAg&�Af5?Ac��A`�9A_�A_7LA^�yA^�A[��AX��AX~�AWt�AVbAUXAT  AQ��AN��AL�yAL^5AJ��AI
=AH=qAG�AF�+AE`BAD��AC�PAB��AAt�A@ZA?�;A?��A?C�A>1A:�/A9ƨA7XA5��A3A2�A2�RA1\)A0��A0~�A/�
A.bNA.�A.{A. �A-�FA+��A+hsA*�RA*1A)l�A(JA&9XA#p�A!��A!��A �yAx�AjA"�A��A�FAC�A�RA��A�hAr�A�wA;dA$�A?}A�\A�A`BA��A��A"�Ar�A
�`A
E�A	�
A	�hA	\)A��A7LA�A�PAȴA�AQ�AƨA ��A �uA j@��@���@��T@��/@�9X@�Ĝ@�n�@�7L@�V@�9X@�%@��@���@��@��#@��@�O�@�9X@���@�ff@���@���@�;d@���@�7L@�|�@ޗ�@�E�@��H@�"�@���@ӝ�@�r�@���@�n�@�  @�l�@� �@�dZ@��^@�E�@�Ĝ@�hs@���@�/@��@�v�@�Z@���@�%@�-@�Z@�|�@�z�@���@��@�(�@�5?@���@�ȴ@���@��@�-@��@�I�@��/@��P@���@�ff@�G�@���@�b@�O�@���@�7L@��;@�$�@��@�Z@� �@���@�G�@�Q�@~5?@}�@{33@y�7@xb@v$�@s��@q�7@o�w@m�-@kt�@h��@g�P@fv�@e`B@b��@a&�@_K�@]�@\9X@[o@ZJ@XbN@W
=@U?}@T�@So@Q%@O�w@M?}@LI�@KdZ@H��@H  @Fȴ@E�@E�@C��@B�!@@��@?��@?+@>@<I�@;S�@:=q@97L@7�w@6�y@5�T@5`B@4��@3C�@1�@1&�@0�@0b@.V@-�@-?}@,�@+�
@*��@)�@(��@'�@&��@%/@$�D@#ƨ@"��@!��@ r�@l�@ff@�@�@�@dZ@�H@^5@%@K�@$�@`B@��@z�@ƨ@��@~�@�7@��@ �@\)@�@�D@j@��@t�@@
��@
-@	x�@r�@�w@
=@v�@�@�h@O�@�j@�D@�@��@��@J@ �`?���?�(�?�^5?��y?�?}?�Z?�\??�5??���?�^?��?�ȴ?��T?�S�?�7?��;?ޗ�?�/?ۥ�?�=q?ش9?ش9?��?֧�?��?�9X?ӶF?���?�hs?�&�?Ѓ?��?θR?͑h?�j?˅?��H?��#?���?ȴ9?�l�?�+?��/?�Z?�o?���?�bN?�\)?�V?�p�?��?�I�?�I�?�dZ?�"�?���?�~�?���?�=q?�=q?���?�^5?���?��H?�C�?���?�1?��D?�O�?���?��?�{?�{?�5??�v�?�v�?���?��R?��?���?��?��?�\)?�|�?���?���?��w?�  ?�  ?�  ?�A�A�XA�ZA�XA�ZA�ZA�ZA�XA�\)A�ZA�ZA�ZA�^5A�XA�\)A�\)A�ZA�ZA�\)A�ZA�ZA�S�A�VA�VA�S�A�M�A�Q�A�O�A�K�A�M�A�M�A�I�A�G�A�G�A�G�A�E�A�A�A�E�A�G�A�G�A�O�A�E�A�A�A�;dA�/A�(�A�$�A�&�A�&�A�&�A�$�A��A��A�bA�VA���A��A��A���A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                               A�ZA�XA�O�A�K�A�G�A�E�A�I�A�?}A�-A�&�A�$�A�JA��A��A��TA��;A��;A���A���AϾwAϼjAϼjAϺ^AϸRAϸRAϸRAϴ9Aϰ!AύPA���A�-A�ZA���AʋDA�AÝ�ADA��FA�  A��A�A�VA���A�{A�\)A���A� �A��
A�VA�+A�XA�v�A�oA��A�
=A�hsA�-A��mA��A�A�A�A�=qA��A���A�p�A�JA���A�oA���A�\)A���A���A��HA�C�A�VA�ĜA��A��!A�7LA���A��uA��A��jA�1'A�l�A�ZA��+A�I�A�A�7LA�oA�~�A�{A�n�A���A���A�"�A�(�A��A��^A���A�-A���A�A�VA��!A�r�A��A���A�"�A�%A���A�A�|�A���A�l�Ap�A}�A|bAy�Av�yAr-An�jAm�Am;dAl��AlQ�AlM�AiƨAi
=AhbAg&�Af5?Ac��A`�9A_�A_7LA^�yA^�A[��AX��AX~�AWt�AVbAUXAT  AQ��AN��AL�yAL^5AJ��AI
=AH=qAG�AF�+AE`BAD��AC�PAB��AAt�A@ZA?�;A?��A?C�A>1A:�/A9ƨA7XA5��A3A2�A2�RA1\)A0��A0~�A/�
A.bNA.�A.{A. �A-�FA+��A+hsA*�RA*1A)l�A(JA&9XA#p�A!��A!��A �yAx�AjA"�A��A�FAC�A�RA��A�hAr�A�wA;dA$�A?}A�\A�A`BA��A��A"�Ar�A
�`A
E�A	�
A	�hA	\)A��A7LA�A�PAȴA�AQ�AƨA ��A �uA j@��@���@��T@��/@�9X@�Ĝ@�n�@�7L@�V@�9X@�%@��@���@��@��#@��@�O�@�9X@���@�ff@���@���@�;d@���@�7L@�|�@ޗ�@�E�@��H@�"�@���@ӝ�@�r�@���@�n�@�  @�l�@� �@�dZ@��^@�E�@�Ĝ@�hs@���@�/@��@�v�@�Z@���@�%@�-@�Z@�|�@�z�@���@��@�(�@�5?@���@�ȴ@���@��@�-@��@�I�@��/@��P@���@�ff@�G�@���@�b@�O�@���@�7L@��;@�$�@��@�Z@� �@���@�G�@�Q�@~5?@}�@{33@y�7@xb@v$�@s��@q�7@o�w@m�-@kt�@h��@g�P@fv�@e`B@b��@a&�@_K�@]�@\9X@[o@ZJ@XbN@W
=@U?}@T�@So@Q%@O�w@M?}@LI�@KdZ@H��@H  @Fȴ@E�@E�@C��@B�!@@��@?��@?+@>@<I�@;S�@:=q@97L@7�w@6�y@5�T@5`B@4��@3C�@1�@1&�@0�@0b@.V@-�@-?}@,�@+�
@*��@)�@(��@'�@&��@%/@$�D@#ƨ@"��@!��@ r�@l�@ff@�@�@�@dZ@�H@^5@%@K�@$�@`B@��@z�@ƨ@��@~�@�7@��@ �@\)@�@�D@j@��@t�@@
��@
-@	x�@r�@�w@
=@v�@�@�h@O�@�j@�D@�@��@��@J@ �`?���?�(�?�^5?��y?�?}?�Z?�\??�5??���?�^?��?�ȴ?��T?�S�?�7?��;?ޗ�?�/?ۥ�?�=q?ش9?ش9?��?֧�?��?�9X?ӶF?���?�hs?�&�?Ѓ?��?θR?͑h?�j?˅?��H?��#?���?ȴ9?�l�?�+?��/?�Z?�o?���?�bN?�\)?�V?�p�?��?�I�?�I�?�dZ?�"�?���?�~�?���?�=q?�=q?���?�^5?���?��H?�C�?���?�1?��D?�O�?���?��?�{?�{?�5??�v�?�v�?���?��R?��?���?��?��?�\)?�|�?���?���?��w?�  ?�  ?�  ?�A�A�XA�ZA�XA�ZA�ZA�ZA�XA�\)A�ZA�ZA�ZA�^5A�XA�\)A�\)A�ZA�ZA�\)A�ZA�ZA�S�A�VA�VA�S�A�M�A�Q�A�O�A�K�A�M�A�M�A�I�A�G�A�G�A�G�A�E�A�A�A�E�A�G�A�G�A�O�A�E�A�A�A�;dA�/A�(�A�$�A�&�A�&�A�&�A�$�A��A��A�bA�VA���A��A��A���A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�yB
�B
�yB
�yB
�sB
�sB
�mB
�mB
�mB
�mB
�mB
�sB
�sB
�mB
�sB
�yB
�B%�B��B$�B0!B33B33B-B5?B8RB6FBVBffB}�B{�B� B�=B�oB��B�!B�3B�^B�^B��BBB��BÖBĜBĜBÖBŢB�}B�B��B��B��B��B��B��B��B��B�uB�Bx�Bp�BaHB[#BK�BI�BF�B?}B=qB9XB8RB8RB-B#�B!�BVB  B��B��B�^B�'B�B��By�BjB;dB-B �B�BoB%BBB+B  B
�sB
�5B
��B
�DB
k�B
T�B
:^B
%�B
JB	��B	�sB	�B	��B	�^B	��B	�+B	s�B	hsB	cTB	`BB	_;B	_;B	M�B	G�B	C�B	>wB	9XB	0!B	2-B	N�B	P�B	XB	XB	-B	/B	9XB	@�B	B�B	B�B	;dB	2-B	�B	�B	oB	%B	B	B��B��B��B�B�B�yB�fB�TB�NB�/B�/B��B��BĜB�LB�-B��B��B��B��B��B��B�hB�PB�PB�PB�bB�hB�DB�+B�B� Bz�Bw�Bq�Bl�BiyBjBhsBhsBcTB^5BXBW
BT�BQ�BL�BL�BH�BH�BF�BD�BB�BA�BA�B@�B?}B?}B@�B@�B>wB?}B>wB>wB=qB:^B9XB;dB:^B8RB:^B9XB7LB33B33B33B2-B2-B49B5?B33B/B1'B0!B6FB5?B2-B33B2-B2-B2-B1'B1'B2-B5?B7LB5?B6FB8RB9XB9XB?}B@�BE�B�hB��B��B��Bo�Bl�Bt�Bq�Bz�B�B�B�1B�oB��B��B�B�LBŢB��B�/B�mB��B	uB	=qB	^5B	^5B	u�B	�B	w�B	~�B	�%B	�JB	�bB	��B	��B	��B	�B	�wB	�qB	B	��B	��B	�
B	�#B	�B	�)B	�fB	�fB	�mB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
B
B
	7B
VB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
%�B
&�B
'�B
)�B
)�B
-B
-B
0!B
0!B
1'B
33B
49B
5?B
6FB
6FB
:^B
:^B
<jB
<jB
=qB
>wB
@�B
@�B
A�B
A�B
C�B
C�B
E�B
F�B
G�B
I�B
J�B
J�B
J�B
K�B
M�B
L�B
M�B
M�B
N�B
P�B
Q�B
Q�B
Q�B
R�B
S�B
T�B
T�B
W
B
XB
YB
ZB
[#B
]/B
]/B
^5B
_;B
`BB
`BB
aHB
cTB
dZB
e`B
ffB
e`B
gmB
hsB
hsB
hsB
iyB
jB
k�B
m�B
n�B
n�B
p�B
p�B
p�B
p�B
q�B
r�B
s�B
s�B
s�B
u�B
u�B
v�B
v�B
x�B
w�B
w�B
x�B
y�B
z�B
|�B
}�B
� B
� B
�B
�B
�B
�B
�1B
�7B
�=B
�JB
�PB
�VB
�VB
�bB
�hB
�oB
�uB
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
�B
�B
�B
�B
�-B
�-B
�3B
�3B
�9B
�9B
�?B
�FB
�?B
�LB
�LB
�RB
�LB
�LB
�RB
�RB
�XB
�XB
�XB
�XB
�XB
�RB
�XB
�^B
�XB
�XB
�^B
�XB
�XB
�XB
�XB
�XB
�^B
�XB
�^B
�XB
�^B
�XB
�RB
�XB
�^B
�XB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�yB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�yB
�B
�B
�B
�B
�yB
�yB
�B
�sB
�yB
�B
�B
�B
�B
�yB
�B
�yB
�yB
�B
�sB
�yB
�yB
�B
�B
�yB
�yG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                               B
�]B
�^B
�^B
�XB
�XB
�_B
�YB
�YB
�ZB
�ZB
�ZB
�UB
�[B
�VB
�WB
�QB
�RB
�LB
�MB
�NB
�NB
�OB
�UB
�VB
�QB
�WB
�^B
�}B%�B��B$�B0B3B3B,�B5(B8;B60BU�BfPB}�B{�B�B�)B�\B��B�B�!B�MB�MB�sB�BB�{BÈBďBďBÊBŖB�rB�B��B��B��B��B��B��B��B�{B�oB�Bx�Bp�BaDB[ BK�BI�BF�B?|B=pB9XB8RB8SB-B#�B!�BYB B��B��B�cB�,B�B��By�Bj�B;kB-B �B�BxB/BB*B7B B
�B
�BB
��B
�QB
k�B
UB
:lB
%�B
YB	��B	�B	�'B	��B	�oB	��B	�=B	s�B	h�B	cgB	`VB	_OB	_PB	M�B	G�B	C�B	>�B	9oB	09B	2EB	N�B	P�B	X)B	X*B	-(B	/6B	9sB	@�B	B�B	B�B	;�B	2KB	�B	�B	�B	DB	2B	&B�B��B��B��B�B�B�B�xB�sB�TB�UB�B��B��B�sB�UB�B�B� B��B��B��B��B�|B�|B�}B��B��B�rB�ZB�BB�0B{Bx Bq�Bl�Bi�Bj�Bh�Bh�Bc�B^jBXEBW@BU4BR"BMBMBH�BH�BF�BD�BB�BA�BA�B@�B?�B?�B@�B@�B>�B?�B>�B>�B=�B:�B9�B;�B:�B8�B:�B9�B7�B3xB3xB3yB2sB2tB4�B5�B3{B/dB1pB0kB6�B5�B2xB3~B2yB2zB2zB1uB1uB2|B5�B7�B5�B6�B8�B9�B9�B?�B@�BE�B��B�B�B�4BpBl�Bu'BrB{RB��B��B��B��B�B�gB��B��B�/B�iB��B�B��B	B	>B	^�B	^�B	vmB	��B	xB	�B	��B	�B	�B	�@B	�sB	��B	��B	�AB	�>B	�_B	ΦB	��B	��B	��B	��B	�B	�KB	�NB	�XB	�B	�B	�B	�B	��B	��B	��B	��B	��B
 B
B
"B
2B

MB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
#B
$B
'%B
(.B
)8B
+GB
+IB
.^B
.aB
1wB
1yB
2�B
4�B
5�B
6�B
7�B
7�B
;�B
;�B
=�B
=�B
>�B
?�B
A�B
BB
C
B
CB
EB
EB
G-B
H6B
I?B
KMB
LWB
LZB
L\B
MeB
OtB
NpB
OyB
O{B
P�B
R�B
S�B
S�B
S�B
T�B
U�B
V�B
V�B
X�B
Y�B
Z�B
[�B
\�B
_ B
_B
`B
aB
bB
b!B
c)B
e8B
fAB
gIB
hRB
gOB
i^B
jgB
jiB
jlB
ktB
l}B
m�B
o�B
p�B
p�B
r�B
r�B
r�B
r�B
s�B
t�B
u�B
u�B
u�B
w�B
w�B
x�B
x�B
{ B
y�B
y�B
{B
|B
}B
+B
�8B
�IB
�NB
�hB
�tB
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
�B
�B
�B
�+B
�6B
�CB
�VB
�[B
�iB
�nB
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
�B
�B
�B
�B
�+B
�*B
�NB
�\B
�rB
��B
��B
��B
��B
��B
��B
�B
�B
�2B
�BB
�UB
�mB
�vB
��B
��B
��B
��B
��B
��B
��B
�	B
�B
�'B
�6B
�EB
�BB
�LB
�UB
�QB
�UB
�^B
�ZB
�^B
�aB
�dB
�gB
�qB
�mB
�wB
�sB
�}B
�yB
�vB
��B
��B
��B
�]B
�cB
�cB
�WB
�cB
�]B
�]B
�]B
�]B
�]B
�cB
�WB
�]B
�]B
�QB
�]B
�]B
�WB
�]B
�WB
�^B
�^B
�XB
�XB
�^B
�XB
�^B
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�SB
�_B
�_B
�YB
�_B
�SB
�SB
�_B
�MB
�TB
�ZB
�ZB
�ZB
�ZB
�TB
�ZB
�UB
�UB
�[B
�OB
�UB
�UB
�aB
�[B
�VB
�VG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                               <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202532021061413552820210614135528202106171312232021061713122320210617131223201807242202532021061413552820210614135528202106171312232021061713122320210617131223PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422025320180724220253  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422025320180724220253QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422025320180724220253QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713145920210617131459IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                