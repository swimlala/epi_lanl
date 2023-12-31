CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-12-25T06:00:38Z creation      
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
resolution        =���   axis      Z        8  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     8  N�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ^    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     8  a�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  q(   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  �0   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  �8   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  �p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  �x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ȱ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  ̀   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                       HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   (   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   0   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   8   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � @   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar           HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar           HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�          HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                       SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ۸   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  `      �  `Argo profile    3.1 1.2 19500101000000  20181225060038  20210722160156  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               3   3DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @ؘ�C�18@ؘ�C�1811  @ؘ�8�P@ؘ�8�P@5���R@5���R�c�BZ�c �c�BZ�c 11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?�  @   @@  @�33@�33@�33@�33A��AffA$��A@  A`  A�  A�33A���A���A�33A�  A�  A�  B ffBffB  BffB ��B(  B/33B7��B?��BG��BP  BW��B`  BhffBpffBxffB�33B�ffB�33B�33B�  B�  B���B���B�  B�  B���B�33B�ffB�  B���B�  B�33B�  B�ffB���B���Bә�B�ffBܙ�B�ffB�ffB�33B�  BB�  B�33B�  B���C  C33CL�C33C
�C33C  C�fC�C�fC��C�fC  C33C  C��C"  C$33C&L�C(�C)�fC,�C.33C0  C1��C4  C6�C8L�C:�C;�fC>  C@  CB33CD33CFL�CH�CI�fCL  CN33CO�fCQ��CS��CV  CX�CZL�C\�C]�fC`  Cb33CdL�Cf�Cg��Cj  Cl�Cn33Cp33Cr�Cs��Cu�fCx  Cz�C|L�C~33C�fC��C�&fC��C��3C�  C��C�&fC��C�  C��C��C��3C�  C��3C��fC��3C��C��C��3C��C�  C��3C��C��3C�ٚC��3C�  C��C�&fC��C��3C��C��C�  C�ٚC��fC��3C�  C��C�&fC��C�ٚC�ٚC��fC��fC��3C�  C�  C�  C��C��C��C��C�  C�ٚC��fC��3C�  C�  C��C�&fC�&fC��C��fC��3C�  C��C��C��C��C��fC�  C�  C�  C��C�&fC��C��3C��3C�  C��C��C��C��C�&fC�&fC�&fC��C��fC��fC��fC��3C�  C��C��C��C�&fC�&fC�  C��fC��3C�  C�  C��3C��3C��3C��3C��3C�  C��3C��fC��C�33C�&fC�  C��C�&fC�33C��C��fC��3C��3C��3C��3C�  C��3C��3D33D�3D	FfDٚDy�D3D��D33DٚDffD  D �fD#�D%� D(Y�D+  D-��D0&fD2�3D5s3D8�D:��D=3D?� DB,�DD��DG33DI�3DL�DN�fDP��DS` DU� DXS3DZ�3D]FfD_��Db&fDd��Dg�Di�3Dl�Dn��Dq  Ds�3Dv�Dx�fD{3D},�D��D�fD�P D���D��fD���D�0 D�ffD��3D�ɚD���D�0 D�c3D�� D���D��3D�)�D�c3D���D���D���D�#3D�I�D�s3D��fD�� D��fD��D�@ D�c3D��3D���D���D���D�	�D�,�D�P D�l�D���D���D��3D�3D�#3D�FfD�\�D�l�D��fD��3D���D��fD��3D��D�0 D�FfD�c3D�vfD���D��fD£3Dé�DĶfD��fD�� D���D��fD�� D�� D�� D�� D��fD��3D��3D���D��D��D�� D��fD��3D�� D���D�� D�� D�ɚD��fD���D�ٚD��3D��3D��fD��3D��3D�� D�ٚD�� D��3D��3D�ٚD���D��3D��D���D��fD�  D� D��D�,�D�<�D�FfD�VfD�ffD�p D�y�D��3D�l�D�c3D�S3D�C3D�0 D��E 3E y�E � EffE�fE+3EfE�fE��E�3E
ffEd�E�fE�fEk3Ei�E�E� ES3E� E��E�E� Ek3EɚEfEi�E�3E �3E!� E#3E$Q�E%��E'fE(L�E)~fE*� E+� E-a�E.�3E/��E0��E2+3E3ffE4��E5� E7  E8X E9� E:�fE< E?nfEB� EEq�EH�3EK� EO3ER;3>���>���>���>���>L��>���>���>���>L��>���>���?   >���>���>���>���>���?   ?   ?   ?333?L��?�  ?���?���?�33?ٙ�?�33@ff@��@&ff@333@L��@`  @l��@�33@���@�33@���@���@�  @���@ə�@�ff@�  @�  @���A��A33A��A��A   A(  A.ffA4��A<��AC33AK33AQ��AY��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414414441114144114411111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                             ?fff?�  @   @`  @�33@�33@�33@�33A	��AffA,��AH  Ah  A�  A�33A���A���A�33A�  A�  A�  BffB
ffB  BffB"��B*  B133B9��BA��BI��BR  BY��Bb  BjffBrffBzffB�33B�ffB�33B�33B�  B�  B���B���B�  B�  B���B�33B�ffB�  B���B�  B�33B�  B�ffB���B���Bԙ�B�ffBݙ�B�ffB�ffB�33B�  B�B�  B�33B�  C L�C� C�3C��C�3C
��C�3C� CffC��CffCL�CffC� C�3C� C L�C"� C$�3C&��C(��C*ffC,��C.�3C0� C2L�C4� C6��C8��C:��C<ffC>� C@� CB�3CD�3CF��CH��CJffCL� CN�3CPffCRL�CTL�CV� CX��CZ��C\��C^ffC`� Cb�3Cd��Cf��ChL�Cj� Cl��Cn�3Cp�3Cr��CtL�CvffCx� Cz��C|��C~�3C�33C�Y�C�ffC�Y�C�33C�@ C�Y�C�ffC�Y�C�@ C�Y�C�L�C�33C�@ C�33C�&fC�33C�Y�C�L�C�33C�Y�C�@ C�33C�L�C�33C��C�33C�@ C�Y�C�ffC�L�C�33C�L�C�Y�C�@ C��C�&fC�33C�@ C�L�C�ffC�L�C��C��C�&fC�&fC�33C�@ C�@ C�@ C�L�C�Y�C�Y�C�Y�C�@ C��C�&fC�33C�@ C�@ C�Y�C�ffC�ffC�Y�C�&fC�33C�@ C�L�C�Y�C�Y�C�L�C�&fC�@ C�@ C�@ C�Y�C�ffC�L�C�33C�33C�@ C�L�C�Y�C�Y�C�Y�C�ffC�ffC�ffC�L�C�&fC�&fC�&fC�33C�@ C�L�C�L�C�Y�C�ffC�ffC�@ C�&fC�33C�@ C�@ C�33C�33C�33C�33C�33C�@ C�33C�&fC�L�C�s3C�ffC�@ C�L�C�ffC�s3C�L�C�&fC�33C�33C�33C�33C�@ C�33C�33DS3D�3D	ffD��D��D33D��DS3D��D�fD  D �fD#9�D%� D(y�D+  D-��D0FfD2�3D5�3D8,�D:��D=33D?� DBL�DDٚDGS3DI�3DL9�DN�fDQ�DS� DV  DXs3DZ�3D]ffD_ٚDbFfDd��Dg9�Di�3Dl,�Dn��Dq  Ds�3Dv9�Dx�fD{33D}L�D��D�&fD�` D���D��fD��D�@ D�vfD��3D�ٚD��D�@ D�s3D�� D���D�3D�9�D�s3D���D���D��D�33D�Y�D��3D��fD�� D�fD�)�D�P D�s3D��3D���D���D���D��D�<�D�` D�|�D���D���D��3D�3D�33D�VfD�l�D�|�D��fD��3D���D��fD�3D�)�D�@ D�VfD�s3D��fD���D��fD³3Dù�D��fD��fD�� D���D��fD�  D�  D�  D�  D�fD�3D�3D���D���D���D�� D��fD��3D�� D���D�� D�� D�ٚD��fD���D��D��3D��3D��fD��3D��3D�� D��D�� D��3D��3D��D���D��3D���D���D�fD� D�  D�,�D�<�D�L�D�VfD�ffD�vfD�� D���D��3D�|�D�s3D�c3D�S3D�@ D�,�E 3E ��E � EnfE�fE33E&fE�fE��E�3E
nfEl�E�fE�fEs3Eq�E�E� E[3E� E��E�E� Es3EњE&fEq�E�3E �3E!� E#3E$Y�E%��E'&fE(T�E)�fE*� E+� E-i�E.�3E/ɚE1�E233E3nfE4��E5� E7( E8` E9� E:�fE<  E?vfEB� EEy�EH�3EL  EO#3ERC3G�O�?L��G�O�G�O�?333G�O�G�O�G�O�?333?L��?fffG�O�?L��G�O�G�O�?L��?fffG�O�G�O�?�  ?���?�ff?�  ?���?ٙ�?�33@��@��@&ff@9��@Fff@S33@l��@�  @�ff@�33@���@�33@���@���@�  @���@ٙ�@�ff@�  A   A��A��A33A��A!��A(  A0  A6ffA<��AD��AK33AS33AY��Aa��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414414441114144114411111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                             @ �@ �@ �@ {@ �@ "�@ )�@ 0x@ 7L@ <�@ FQ@ Q�@ _�@ m:@ z3@ �7@ ��@ �(@ �~@ �&@ ��@ �#@ ��@ ��@@o@�@+@9X@F�@T�@b�@o�@~K@��@�H@��@��@��@��@ލ@�@�,@%@�@""@/�@<�@K�@Z@ff@r�@��@�@�@��@��@�J@�C@�@��@��@�@�@%�@1�@@�@O0@\)@hs@ww@�|@��@��@��@�@�c@�[@�`@�@��@�@O@*S@6�@B�@Q�@`�@oF@{�@��@��@�5@�~@��@��@�#@�(@�q@�@@�@-�@;d@I�@V@bN@p�@�@�D@�<@��@��@�>@�C@ލ@��@�,@1@�@"�@.l@=q@K�@Z@g�@t�@�W@��@�@�Y@�^@�W@��@�@��@��@	�@�@&�@5?@B8@N�@]�@j@v�@�@�@�@�f@�@�@�[@�@�Y@�Q@V@�@&�@5�@DD@SI@a�@m�@z3@�7@��@��@�r@��@�@�t@��@��@	@	@	�@	+@	8�@	F�@	UU@	b�@	p�@	~�@	�P@	��@	��@	��@	��@	��@	�/@	�@	�,@
1@
�@
$.@
1'@
<@
Ji@
X�@
g@
uk@
�@
�@
��@
��@
�R@
��@
��@
�T@
�@
��@	�@�@&;@4�@B8@O�@^5@k�@y�@��@�h@�@��@�@�c@׹@�`@�@@�@O@'�@5�@DD@Q�@^�@l�@z3@��@��@��@��@��@�|@�/@�(@��@@@!s@-@8�@F�@T�@bN@o�@~K@�D@��@�@^5@��@��@2�@y�@�w@v@M�@�u@�t@g@e�@�@�@=q@�@�@@Z�@��@�m@+@p�@�F@��@?}@��@�J@�@Ji@��@�7@@Wb@�H@�/@g@`�@��@�(@-�@r@��@�9@@,@�d@�@^@E�@��@��@b@SI@��@�
@B@Yn@��@�/@�@`A@�m@��@"�@e	@��@��@ +�@ l�@ �Y@ �(@!)�@!k.@!��@!�y@"'�@"ff@"��@"��@# �@#^�@#�U@#�@$6@$UU@$�@$�C@%b@%O0@%��@%�@&1@&C�@&}�@&��@&�q@'2�@'n�@'�Y@'�(@(%�@(`�@(��@(�h@)�@)Lu@)��@)��@)� @*1'@*i�@*�(@*��@+{@+K@+��@+�R@+�L@,&;@,\�@,�@,�@,��@-33@-g�@-��@-Ӡ@.	�@.@�@.ww@.��@.�@/�@/S�@/��@/�>@/��@00x@0g@0�@0�C@1�@1>@1t�@1��@1�@2�@2S�@2�D@2��@2��@36�@3o�@3��@3�@4�@4V�@4��@4�c@5@5:�@5l�@5�@5�O@6�@6:@6l�@6�a@6��@7j@75�@7�@8dZ@8�@9e�@:@:k.@;�@;y�@<!s@<��@=1'@=��@>> @>��@?I@?�@@K�@@��@A~K@A�@Bx&@C	�@C��@D#�@D�@E
=@E�#@F�@F�o@GM�@G��@HQ=@H�7@IN�@I��@Jx�@J�E@K��@L�@L�C@M�@M��@N""@N�A@O/�@O��@P@�@Q�f@S�@T> @U�#@W�@X]�@Y��G�O�@ G�O�G�O�@ ^G�O�G�O�G�O�@ ^@ @ �G�O�@ G�O�G�O�@ @ �G�O�G�O�@ j@ �@ v@ �@ �@ 1@ 	�@ �@ �@ V@ b@ �@ @ �@ �@ B@ �@ 
@ g@ !s@ $.@ %�@ (G@ +@ -�@ /�@ 33@ 5?@ 8�@ ;d@ >@ A�@ DD@ G�@ Ji@ M$@ P�@ SI@ V�@ Yn@ \�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AŴ9AŲ-AŸRAŮAŰ!A�AžwA���A�A�ƨA�ȴA���A���A���A���A�ȴA�ĜA�A�A���AŸRAũ�Ať�Aş�Aŏ\AŁA�z�A�z�A�z�A�x�A�z�A�x�A�t�A�t�A�l�A�`BA���A�|�A�S�A�A�A�33A��A�jA�E�A�bA��PA�  A��^A�G�A�=qA�=qA���A�E�A��A�bNA��/A�9XA��A�I�A���A���A�M�A�jA���A�hsA��yA��A��A�p�A�n�A�  A�ȴA��`A�C�A��A��`A�E�A�z�A��mA���A�|�A�33A��mA��A�bNA��HA���A��DA�|�A�S�A�1A�Q�A���A�A�A��TA�x�A��A�G�A�1A�=qA�?}A�ƨA�`BA��;A��hA�?}A�M�A�33A�7LA�-A�A��DA���A��A�O�A~ffA|��A{\)Az�Ay`BAwƨAv��Au7LAr��AoVAm`BAk�hAh�uAgC�Ad5?Aa�mA`bA_G�A^ZA]��A[p�AYK�AW�AUt�AT��AT-ASS�AP  AM�TAL�!AL-AJ  AD��ACO�AA%A?x�A>�jA=/A;7LA9�A8A�A7
=A6=qA5�7A4�A41'A3��A2�9A2v�A2 �A1��A0�`A0ZA/�TA/�A-�A,~�A+�mA+;dA*��A*I�A)��A(-A&r�A$��A#��A#
=A"VA!\)A JAȴA=qAA�wAC�An�Ax�At�AJA�7A�DA9XA(�A  A��AQ�AoA�AS�A�RA�A�!A=qA �A�A��A��A
ĜA
=qAĜAAr�A1A+AjA{A
=A��AC�A ��@�K�@�?}@��y@��@��@�=q@�J@��@��m@�@�@�@�j@�@�u@�@��@���@�K�@�$�@�@�@�dZ@ޗ�@�{@ܬ@��@ؓu@���@�E�@�@�Ĝ@�j@��@���@�ƨ@ȣ�@�9X@��@���@�J@��+@���@��-@��@���@�$�@��`@��\@�?}@�Ĝ@�bN@�+@��h@���@�M�@�hs@�  @�1@�\)@�`B@�E�@�Z@��;@�Ĝ@�|�@�o@��
@��@���@� �@�\)@��\@�&�@���@�9X@�dZ@�5?@���@�+@�5?@��7@�9X@+@}��@{@xA�@w�@w;d@u��@r�H@pA�@n�R@m�-@l(�@ko@j-@i�7@iX@g�;@f@dj@b~�@a��@`b@^��@]�@\(�@Z~�@YX@W;d@T�/@R�!@Q��@P1'@M�@L�@I��@F�y@FE�@C�
@CdZ@C33@A�@A�@@bN@>ȴ@>�+@<��@<(�@;�m@;�@:�!@8r�@6��@5`B@4�@4j@4�@2�@2=q@1%@0  @/�w@-`B@,�j@+ƨ@+@*�H@)��@(1'@&�@%�T@%`B@$1@#�F@#o@"n�@"M�@!��@ Ĝ@�w@+@�R@@p�@?}@I�@dZ@-@�`@r�@�+@@j@�
@33@�@��@hs@&�@�`@r�@�@l�@
=@��@�@�j@�@S�@S�@
�@	x�@	&�@�9@b@l�@�@�@ƨ@33@�\@��@��@ A�?���?��?�X?�+?�z�?�%??�p�?�?�j?��#?���?�?�$�?���?��/?�\?���?�  ?�p�?�/?�j?���?ٺ^?�1'?֧�?֧�?֧�?�ff?��/?��?щ7?�%?�  ?�v�?̬?̬?˅?���?ɺ^?��#?ɺ^?�x�?��?�l�?Ƨ�?š�?�Z?�Z?���?��?��`?���?��?��m?��#AžwAżjAź^AŸRAŰ!Aũ�AŰ!AŴ9AŶFAź^AŸRAź^AŸRAŶFAŴ9AŰ!AŬAŮAŰ!Aũ�Aũ�AŰ!AŴ9AŴ9AŴ9AŴ9AŴ9AŶFAź^AŸRAŴ9AŮAŬAŬAũ�AŬAŸRA�A�ĜA�AżjAžwAžwA���AžwA�A�A�A�A�ȴA���A���A�ƨA�ƨA���A�ȴA���A�ȴA���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                             AŴ9AŲ-AŸRAŮAŰ!A�AžwA���A�A�ƨA�ȴA���A���A���A���A�ȴA�ĜA�A�A���AŸRAũ�Ať�Aş�Aŏ\AŁA�z�A�z�A�z�A�x�A�z�A�x�A�t�A�t�A�l�A�`BA���A�|�A�S�A�A�A�33A��A�jA�E�A�bA��PA�  A��^A�G�A�=qA�=qA���A�E�A��A�bNA��/A�9XA��A�I�A���A���A�M�A�jA���A�hsA��yA��A��A�p�A�n�A�  A�ȴA��`A�C�A��A��`A�E�A�z�A��mA���A�|�A�33A��mA��A�bNA��HA���A��DA�|�A�S�A�1A�Q�A���A�A�A��TA�x�A��A�G�A�1A�=qA�?}A�ƨA�`BA��;A��hA�?}A�M�A�33A�7LA�-A�A��DA���A��A�O�A~ffA|��A{\)Az�Ay`BAwƨAv��Au7LAr��AoVAm`BAk�hAh�uAgC�Ad5?Aa�mA`bA_G�A^ZA]��A[p�AYK�AW�AUt�AT��AT-ASS�AP  AM�TAL�!AL-AJ  AD��ACO�AA%A?x�A>�jA=/A;7LA9�A8A�A7
=A6=qA5�7A4�A41'A3��A2�9A2v�A2 �A1��A0�`A0ZA/�TA/�A-�A,~�A+�mA+;dA*��A*I�A)��A(-A&r�A$��A#��A#
=A"VA!\)A JAȴA=qAA�wAC�An�Ax�At�AJA�7A�DA9XA(�A  A��AQ�AoA�AS�A�RA�A�!A=qA �A�A��A��A
ĜA
=qAĜAAr�A1A+AjA{A
=A��AC�A ��@�K�@�?}@��y@��@��@�=q@�J@��@��m@�@�@�@�j@�@�u@�@��@���@�K�@�$�@�@�@�dZ@ޗ�@�{@ܬ@��@ؓu@���@�E�@�@�Ĝ@�j@��@���@�ƨ@ȣ�@�9X@��@���@�J@��+@���@��-@��@���@�$�@��`@��\@�?}@�Ĝ@�bN@�+@��h@���@�M�@�hs@�  @�1@�\)@�`B@�E�@�Z@��;@�Ĝ@�|�@�o@��
@��@���@� �@�\)@��\@�&�@���@�9X@�dZ@�5?@���@�+@�5?@��7@�9X@+@}��@{@xA�@w�@w;d@u��@r�H@pA�@n�R@m�-@l(�@ko@j-@i�7@iX@g�;@f@dj@b~�@a��@`b@^��@]�@\(�@Z~�@YX@W;d@T�/@R�!@Q��@P1'@M�@L�@I��@F�y@FE�@C�
@CdZ@C33@A�@A�@@bN@>ȴ@>�+@<��@<(�@;�m@;�@:�!@8r�@6��@5`B@4�@4j@4�@2�@2=q@1%@0  @/�w@-`B@,�j@+ƨ@+@*�H@)��@(1'@&�@%�T@%`B@$1@#�F@#o@"n�@"M�@!��@ Ĝ@�w@+@�R@@p�@?}@I�@dZ@-@�`@r�@�+@@j@�
@33@�@��@hs@&�@�`@r�@�@l�@
=@��@�@�j@�@S�@S�@
�@	x�@	&�@�9@b@l�@�@�@ƨ@33@�\@��@��@ A�?���?��?�X?�+?�z�?�%??�p�?�?�j?��#?���?�?�$�?���?��/?�\?���?�  ?�p�?�/?�j?���?ٺ^?�1'?֧�?֧�?֧�?�ff?��/?��?щ7?�%?�  ?�v�?̬?̬?˅?���?ɺ^?��#?ɺ^?�x�?��?�l�?Ƨ�?š�?�Z?�Z?���?��?��`?���?��?��m?��#AžwAżjAź^AŸRAŰ!Aũ�AŰ!AŴ9AŶFAź^AŸRAź^AŸRAŶFAŴ9AŰ!AŬAŮAŰ!Aũ�Aũ�AŰ!AŴ9AŴ9AŴ9AŴ9AŴ9AŶFAź^AŸRAŴ9AŮAŬAŬAũ�AŬAŸRA�A�ĜA�AżjAžwAžwA���AžwA�A�A�A�A�ȴA���A���A�ƨA�ƨA���A�ȴA���A�ȴA���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                             ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�yB�sB�sB�mB�mB�sB�sB�yB�sB�yB�sB�sB�yB�yB�yB�sB�B��B��B��B��B��BB�BhsB�B�B�B�B�B�B�B�oB��B��B��B��B��B��B��B��B��B��B��B�uB��B�B�%B~�Bt�Bl�BdZB`BB[#BQ�BM�BF�B?}B;dB8RB5?B2-B/B+B'�BPB��B��B��B��B�5B��BƨB�dB�B��B�By�BhsB]/BP�BK�BC�B9XB!�BbB
��B
�`B
��B
ɺB
��B
�'B
��B
��B
~�B
n�B
e`B
^5B
S�B
L�B
D�B
<jB
1'B
�B	��B	�B	��B	�?B	��B	�\B	�B	{�B	q�B	k�B	dZB	R�B	I�B	:^B	33B	.B	(�B	�B	%B��B��B�B�BǮB�wB�3B�B��B��B��B��B�bB�7B�1B�B�B�DB�\B�uB�uB�uB�oB�VB�PB�DB�=B�%B�B�Bz�Bz�By�Bv�Bp�Bk�Bk�Be`BcTB^5B]/BW
BS�BQ�BP�BN�BL�BG�BA�B=qB=qB8RB7LB7LB5?B49B2-B/B/B.B.B+B+B.B0!B.B.B0!B/B2-B33B2-B2-B0!B,B-B-B.B,B0!B.B.B,B(�B+B.B/B/B-B,B.B/B/B/B/B/B-B0!B1'B33B6FB9XB;dB?}B@�B?}B@�B>wB>wBA�BB�BB�BC�BH�BI�BJ�BK�BJ�Bl�B{�B�\B�uB��B�-B�}B��B�B��B��B	DB	�B	6FB	>wB	C�B	R�B	^5B	hsB	w�B	~�B	�DB	�=B	�PB	�VB	��B	��B	�B	�!B	�FB	�^B	ɺB	��B	��B	�B	�B	�B	�)B	�BB	�TB	�fB	�yB	�B	�B	�B	�B	��B	��B	��B
B
%B
%B
B
+B
JB
VB
bB
hB
uB
{B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
!�B
#�B
%�B
&�B
'�B
)�B
-B
0!B
0!B
33B
5?B
5?B
9XB
;dB
<jB
>wB
=qB
>wB
>wB
?}B
@�B
A�B
B�B
D�B
E�B
D�B
E�B
F�B
H�B
J�B
K�B
K�B
K�B
L�B
M�B
N�B
O�B
P�B
P�B
R�B
R�B
R�B
S�B
S�B
VB
XB
ZB
ZB
[#B
[#B
[#B
]/B
]/B
\)B
^5B
_;B
_;B
aHB
aHB
cTB
cTB
cTB
dZB
e`B
ffB
gmB
gmB
iyB
jB
k�B
k�B
l�B
l�B
m�B
m�B
n�B
n�B
o�B
p�B
o�B
p�B
q�B
q�B
q�B
s�B
t�B
s�B
s�B
v�B
u�B
v�B
w�B
w�B
x�B
|�B
}�B
}�B
~�B
� B
� B
�B
�B
�B
�%B
�%B
�1B
�DB
�PB
�PB
�VB
�PB
�bB
�hB
�hB
�oB
�uB
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
��B
��B
��B
�B
�B
�B
�B
�B
�!B
�'B
�9B
�9B
�?B�B�B�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                             B�sB�sB�sB�sB�yB�sB�sB�mB�sB�sB�mB�mB�mB�sB�sB�mB�mB�mB�mB�mB�fB�`B�`B�ZB�ZB�`B�`B�fB�`B�fB�`B�`B�fB�fB�fB�`B�B��B��B��B��B��B��B�Be`B�B� B�B�B�B�B� B�\B�uB��B�uB��B�uB��B��B��B��B��B�oB�bB�oB~�B�B{�Bq�BiyBaHB]/BXBN�BJ�BC�B<jB8RB5?B2-B/B,B'�B$�B
=B��B��B��B�B�#B��BÖB�RB��B��B� Bv�Be`BZBM�BH�B@�B6FB�BPB
�B
�NB
��B
ƨB
�qB
�B
��B
�uB
{�B
k�B
bNB
[#B
P�B
I�B
A�B
9XB
.B
�B	�B	�sB	��B	�-B	��B	�JB	� B	x�B	n�B	hsB	aHB	O�B	F�B	7LB	0!B	+B	%�B	�B	B��B�B�B�
BĜB�dB�!B�B��B��B��B�oB�PB�%B�B�B�B�1B�JB�bB�bB�bB�\B�DB�=B�1B�+B�B� B}�Bw�Bw�Bv�Bs�Bm�BhsBhsBbNB`BB[#BZBS�BQ�BO�BN�BL�BJ�BE�B?}B;dB;dB6FB5?B5?B33B2-B0!B-B-B,B,B(�B(�B,B.B,B,B.B-B0!B1'B0!B0!B.B)�B+B+B,B)�B.B,B,B)�B&�B(�B,B-B-B+B)�B,B-B-B-B-B-B+B.B/B1'B49B7LB9XB=qB>wB=qB>wB<jB<jB?}B@�B@�BA�BF�BG�BH�BI�BH�BjBy�B�PB�hB��B�!B�qB��B�B��B��B		7B	�B	49B	<jB	A�B	P�B	\)B	ffB	u�B	|�B	�7B	�1B	�DB	�JB	��B	��B	��B	�B	�9B	�RB	ǮB	��B	��B	��B	�
B	�
B	�B	�5B	�HB	�ZB	�mB	�yB	�B	�B	�B	��B	��B	��B	��B
B
B
B
B

=B
JB
VB
\B
hB
oB
{B
{B
�B
�B
�B
�B
�B
�B
 �B
�B
!�B
#�B
$�B
%�B
'�B
+B
.B
.B
2-B
49B
49B
8RB
:^B
;dB
=qB
<jB
=qB
=qB
>wB
?}B
@�B
A�B
C�B
D�B
C�B
D�B
E�B
G�B
I�B
J�B
J�B
J�B
K�B
L�B
M�B
N�B
O�B
O�B
Q�B
Q�B
Q�B
R�B
R�B
T�B
W
B
YB
YB
ZB
ZB
ZB
\)B
\)B
[#B
]/B
^5B
^5B
`BB
`BB
bNB
bNB
bNB
cTB
dZB
e`B
ffB
ffB
hsB
iyB
jB
jB
k�B
k�B
l�B
l�B
m�B
m�B
n�B
o�B
n�B
o�B
p�B
p�B
p�B
r�B
s�B
r�B
r�B
u�B
t�B
u�B
v�B
v�B
w�B
{�B
|�B
|�B
}�B
~�B
~�B
�B
�B
�B
�B
�B
�+B
�=B
�PB
�PB
�VB
�PB
�bB
�hB
�hB
�oB
�uB
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
��B
��B
��B
�B
�B
�B
�B
�!B
�'B
�-B
�?B
�?B
�FB�sB�yB�fB�fB�sB�B�sB�sB�sB�mB�sB�mB�sB�yB�sB�sB�yB�yB�fB�yB�sB�sB�sB�sB�sB�sB�sB�sB�mB�mB�sB�mB�sB�sB�sB�yB�yB�yB�mB�mB�sB�sB�mB�mB�sB�mB�sB�sB�sB�sB�mB�mB�mB�sB�mB�mB�mB�sB�mB�mG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                             <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201812250600382021061413530720210614135307202106141747022021061417470220210614174702201812250600382021061413530720210614135307202106141747022021061417470220210614174702PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.003 (+/-0)                                                                                                                                                                                                    surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.003 (+/-0)                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018122506003820181225060038  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018122506003820181225060038QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018122506003820181225060038QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015620210722160156IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                