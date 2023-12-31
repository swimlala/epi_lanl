CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-30T00:01:42Z creation      
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
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  Lh   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  P�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  a   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  e8   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  u�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  �H   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                       HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   (   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � (      � (Argo profile    3.1 1.2 19500101000000  20181030000142  20210722160154  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               '   'DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @؊8$8=n@؊8$8=n11  @؊8"">0@؊8"">0@6	��H��@6	��H���c�&�q��c�&�q�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?   ?333?�33@@  @�ff@�33@�  @�  A��A��A&ffAA��Aa��A���A���A�  A�  A���A���AᙚA�  B   BffB  B  B   B(ffB/��B8  B@ffBH  BPffBX��B`ffBhffBpffBxffB�ffB�33B�  B�ffB�33B���B�  B�33B�ffB�ffB�ffB�33B���B�  B�33B�33B�33B�ffB�33B�33B�33B�33B�33B�ffB�ffB�ffB�ffB�ffB�  B���B���B�33C �C�fC33CL�C�C
33C�C�C  C  C�fC33C�C�C�C�C   C"  C$L�C&L�C(�C*�C+�fC.33C0  C2  C4  C5�fC8L�C:L�C<�C>�C@  CB33CD�CE�fCH33CJ  CK�fCN�CO�fCQ��CT  CV33CXL�CZ33C\�C^L�C`�Ca�fCd�Cf33Ch  Ci��Ck�fCn  Cp33Cr33Ct33CvL�Cx�Cy�fC|  C~�C��C�&fC�33C�&fC�  C��C��C��fC�  C��C�&fC��C�  C��fC��C��C�&fC��C��fC�  C��C�&fC��C��3C�  C��C��C��fC�  C��C�&fC�  C��fC��3C�  C�  C��C��C�&fC�&fC��C��3C��fC��3C��C��C��C��C�&fC�&fC�33C��C��3C�  C��C��C��C�  C��fC��3C��C�&fC�  C��fC�  C��C�&fC��C�  C��C��C�&fC��C��3C�  C�  C��C�&fC��C�  C��C�  C��fC��3C��C�  C��fC�  C��C�  C�  C��C��C��C��C�  C�&fC��C�  C��C�  C��3C�  C��C��C��3C��C��fC��fC��3C��C��C��C��3C��C�  C��fC�  C��C��C��3C��C��D�D��DFfD	�fDs3D�D��DL�D� D` D� D` D �fD#ffD%��D(� D+�D-�fD/�3D2` D4� D7,�D9��D<fD>s3D@ٚDCFfDE�3DHY�DJ��DMl�DO��DRy�DT�3DWffDY�fD\@ D^�3Da�Dc��Df  Dhs3Dj�fDmY�Do�3DrFfDt��Dw,�Dy��D{��D~&fD�P D��fD���D���D�0 D�c3D���D��fD�3D�L�D�y�D���D�� D�3D�@ D�i�D���D�� D���D�)�D�\�D���D���D��fD�)�D�VfD�|�D���D�� D��3D��D�6fD�Y�D�s3D�� D�� D�ɚD�� D�3D�&fD�L�D�s3D��3D���D��3D��3D���D�  D�3D�&fD�6fD�<�D�C3D�L�D�\�D�ffD�y�D��fD�DÜ�DĦfDŰ D��3D�� D�� D�� D�3D�fD�33D�I�D�ffDА DѶfD��fD���D��D�0 D�I�D�l�Dٌ�Dڠ Dۼ�D�� D��fD���D�3D�#3D�0 D�<�D�L�D�Y�D�l�D�|�D�3D� D� D멚D��D��3D�� D���D���D���D�3D�3D��D�0 D�@ D�S3D�ffD�ffD��fD��3D���D��fD���E �fE3E� E,�E�3ED�EX Ed�E��E��E	c3E
^fE�3E1�E  E~fE� E9�E+3E�fE� EI�E33E��E��E�EP E� E�fE!.fE"+3E#�3E$�fE&1�E'4�E(��E)� E+  E,�3E-� E.� E0C3E11�E2�fE3��E4��E6P E7��E8� E9�fE;X E<�3E?�EB�3EF�EI  EL!�EOvfERVfEU�3EX�fE[��E^��Eb�EefEhS3Ek�3En�fEq�3Et��Ew� E{�E~�E��fE�/3E���E�>fE�ɚE�/3E�s3E��fE�3E�d E���E�fE�VfE��fE��3E�@�E�� E��fE�-�E�� E�њE�0�E�p E��3E�3E�l E��3E��E�E�E��3E���E�6fE��3?   ?   ?   ?   >���?��?   ?��?   ?   ?��?   ?   ?��?333?��?333?L��?�  ?�  ?�ff?�ff?���?�ff@   @33@&ff@,��@@  @S33@`  @y��@�ff@���@�ff@�33@���@���@�33@�33@�  @���@�33A33A  A��AffA   A&ffA.ffA6ffA<��AD��AK33AS33A[33A`  Ai��Ap  Ax  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444414144144114111414111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ?�  ?���@��@`  @�ff@�33@�  @�  A	��A��A.ffAI��Ai��A���A���A�  A�  A���A���A噚A�  B  B
ffB  B  B"  B*ffB1��B:  BBffBJ  BRffBZ��BbffBjffBrffBzffB�ffB�33B�  B�ffB�33B���B�  B�33B�ffB�ffB�ffB�33B���B�  B�33B�33B�33B�ffB�33B�33B�33B�33B�33B�ffB�ffB�ffB�ffB�ffB�  B���B���B�33C ��CffC�3C��C��C
�3C��C��C� C� CffC�3C��C��C��C��C � C"� C$��C&��C(��C*��C,ffC.�3C0� C2� C4� C6ffC8��C:��C<��C>��C@� CB�3CD��CFffCH�3CJ� CLffCN��CPffCRL�CT� CV�3CX��CZ�3C\��C^��C`��CbffCd��Cf�3Ch� CjL�ClffCn� Cp�3Cr�3Ct�3Cv��Cx��CzffC|� C~��C�Y�C�ffC�s3C�ffC�@ C�Y�C�L�C�&fC�@ C�L�C�ffC�Y�C�@ C�&fC�L�C�Y�C�ffC�L�C�&fC�@ C�L�C�ffC�L�C�33C�@ C�L�C�L�C�&fC�@ C�Y�C�ffC�@ C�&fC�33C�@ C�@ C�L�C�Y�C�ffC�ffC�L�C�33C�&fC�33C�L�C�L�C�L�C�Y�C�ffC�ffC�s3C�L�C�33C�@ C�L�C�L�C�Y�C�@ C�&fC�33C�L�C�ffC�@ C�&fC�@ C�Y�C�ffC�Y�C�@ C�L�C�Y�C�ffC�Y�C�33C�@ C�@ C�L�C�ffC�Y�C�@ C�L�C�@ C�&fC�33C�L�C�@ C�&fC�@ C�Y�C�@ C�@ C�Y�C�Y�C�L�C�L�C�@ C�ffC�L�C�@ C�L�C�@ C�33C�@ C�Y�C�L�C�33C�L�C�&fC�&fC�33C�Y�C�Y�C�L�C�33C�Y�C�@ C�&fC�@ C�Y�C�L�C�33C�L�C�L�D9�D��DffD
fD�3D,�D��Dl�D  D� D  D� D!fD#�fD&�D(� D+,�D-�fD03D2� D4� D7L�D9��D<&fD>�3D@��DCffDE�3DHy�DK�DM��DP�DR��DU3DW�fDY�fD\` D^�3Da9�Dc��Df  Dh�3DkfDmy�Do�3DrffDtٚDwL�Dy��D{��D~FfD�` D��fD�ɚD�	�D�@ D�s3D���D��fD�#3D�\�D���D���D�� D�#3D�P D�y�D���D�� D��D�9�D�l�D���D�ɚD�fD�9�D�ffD���D���D�� D��3D��D�FfD�i�D��3D�� D�� D�ٚD�� D�3D�6fD�\�D��3D��3D���D��3D��3D���D� D�#3D�6fD�FfD�L�D�S3D�\�D�l�D�vfD���D��fD�Dì�DĶfD�� D��3D�� D�� D�  D�3D�&fD�C3D�Y�D�vfDР D��fD��fD�	�D�)�D�@ D�Y�D�|�Dٜ�Dڰ D���D�� D��fD��D�#3D�33D�@ D�L�D�\�D�i�D�|�D��D�3D� D� D빚D��D��3D�� D���D���D���D�3D�#3D�,�D�@ D�P D�c3D�vfD�vfD��fD��3D���D��fD���E �fE3E� E4�E�3EL�E` El�E��E��E	k3E
ffE�3E9�E( E�fE� EA�E33E�fE� EQ�E;3E��E��E�EX E� E�fE!6fE"33E#�3E$�fE&9�E'<�E(��E)� E+( E,�3E-� E.� E0K3E19�E2�fE4�E4��E6X E7��E8� E9�fE;` E<�3E?�EB�3EF�EI( EL)�EO~fER^fEU�3EX�fE[��E^��Eb$�EefEh[3Ek�3En�fEq�3Et��Ew� E{�E~	�E��fE�33E���E�BfE�͚E�33E�w3E��fE�#3E�h E���E�fE�ZfE��fE�3E�D�E�� E��fE�1�E�� E�՚E�4�E�t E��3E�3E�p E��3E��E�I�E��3E� �E�:fE��3G�O�G�O�G�O�G�O�?fffG�O�?�  G�O�G�O�?�  G�O�G�O�?�  ?���G�O�?���?���?�ffG�O�?�  G�O�?�ff@ff@33@   @333@Fff@L��@`  @s33@�  @���@�ff@���@�ff@�33@���@ə�@�33@�33@�  @���A��A33A  A��AffA(  A.ffA6ffA>ffAD��AL��AS33A[33Ac33Ah  Aq��Ax  A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444414144144114111414111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       @ j@ �@ �@ {@ �@ "�@ (�@ /�@ 7L@ ?}@ F�@ R�@ `B@ n�@ {�@ ��@ �0@ ��@ �-@ ��@ ��@ �t@ ��@ ��@j@@g@+�@:@H]@UU@c�@r@~�@��@�H@��@�F@�>@�7@�;@�4@�~@�@*@#�@1'@>�@K�@X@ff@t�@�d@�@�a@�Y@�@ƨ@�O@��@�L@��@�@B@&�@33@@,@M�@\�@j@v�@�|@��@�@�r@�k@�@�
@�@�@^@V@�@)�@7L@DD@Q�@a�@oF@{�@�7@��@�5@�~@�&@��@��@�(@��@@�@�@-�@:�@F�@V�@b�@o�@~�@�D@�<@�A@�F@Ĝ@є@ލ@�@��@%@*@#�@/�@<@Ji@X�@g�@uk@�@�h@��@��@�R@ƨ@��@�T@�@��@
=@B@&;@1�@@�@O0@^5@k.@ww@��@�u@��@�!@�k@�@�
@�`@�e@ �@�@O@)�@7L@B�@Q�@`�@oF@z�@�+@��@��@�~@��@�*@܀@�(@�q@	�@	�@	
@	-@	:�@	H]@	V�@	e	@	r�@	�@	��@	��@	�A@	��@	�>@	є@	��@	�(@	�~@
�@
�@
""@
.l@
=q@
Lu@
Z�@
g�@
t@
�d@
��@
�@
�@
��@
��@
Ӡ@
��@
��@
��@
=@�@%�@1�@@,@O0@\)@hs@ww@�|@��@�m@�r@�@�@׹@�@�e@ �@�@�@(�@5�@DD@SI@`B@l�@{�@�+@��@�(@��@��@�|@��@�y@��@@@ @-@9X@H]@V@��@&�@m�@��@�9@B8@��@є@�@\)@�m@�@)�@m�@��@�,@>�@�d@Ĝ@�@G�@��@�@V@P�@�@�O@�@^�@�5@�y@-�@s_@��@��@:�@~K@�2@�@F�@��@�o@V@Q=@��@׹@�@]�@��@�h@�@_�@��@�T@'�@i�@�Y@�@1'@t�@��@��@9X@z�@�j@��@<@|�@�&@�Q@ ?}@ �@ �2@!@!E�@!�+@!�W@"%@"C�@"��@"��@"�Q@#>�@#|�@#�@#��@$33@$oF@$��@$��@%&�@%e�@%��@%��@&
@&Yn@&�u@&ψ@'
=@'D�@'�@'��@'�@()�@(bN@(�U@(��@)�@)I@)�@)�@)�@*,`@*g@*�m@*�t@+{@+O0@+��@+ƨ@,@,>�@,~K@,�@,��@-8�@-v@-�~@-�@.+�@.i!@.��@.��@/O@/V�@/�@/�|@0�@0@�@0z3@0�9@0�@1(G@1bN@1�H@1Ӡ@2�@2FQ@2}�@2��@2��@3(G@3bN@3�U@3׹@4�@4Ji@4�@4�&@4��@54�@5k.@5��@5�`@6!s@6]�@6��@6�[@7o@7N�@7��@7�W@8@8ww@8�(@9�i@9��@:�a@;	�@;�4@<> @<��@=9X@=��@>c�@>��@?bN@?�,@@��@@�L@A��@B[@B�d@C�@C��@D$�@D�J@E1'@E��@FC�@F��@GWb@G�9@Hff@Ij@I��@J
=@J�m@K4�@K�I@L2�@L�o@M1�@M�c@N`B@N@OX@O��@P�@Qލ@S1@T��@U�7@W�@X�p@Y�w@[B@\l�@]�@_v@`y�@a�w@c @d|?@eψ@gB@hhs@i��@k �@l`B@m��@o6@pc�@q��@s�@s[z@s��@s��@t(G@tb�@t��@t��@u1�@uk�@u��@u��@v33@v��@v��@w�@wO�@w�@w�
@x(G@x^�@x�@x�@y3�@yg�@y��@z@z5?@zz2G�O�G�O�G�O�G�O�@ �G�O�@ jG�O�G�O�@ jG�O�G�O�@ j@ G�O�@ @ �@ vG�O�@ �G�O�@ �@ 
�@ J@ �@ �@ �@ o@ {@ �@ �@ �@ �@ 
@  @ "�@ $�@ '�@ )�@ -@ /�@ 2�@ 3�@ 7�@ :@ >@ @,@ DD@ F�@ Ji@ M�@ P�@ S�@ V�@ Z@ ]�@ _�@ c�@ ff@ i�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��TA��`A���AՓuAՅA�ZA�G�A�?}A�1'A�(�A� �A��A�VA�1A�A�A�A�A�A�A�  A�  A�A�{A��A�{A���Aԧ�A�t�A�ZA�=qA�%Aӣ�AҋDAρA�ȴA�ĜA�A��A��A�%A��;A�9XA�^5A�"�A�bA�oA�5?A��A�?}A���A�z�A�A��A���A�~�A�dZA�l�A���A�VA��A���A��A��PA��A�?}A�C�A���A�=qA���A�(�A�l�A�x�A��/A��hA��A��/A�O�A��mA�=qA�M�A��A���A�ffA��;A��9A�|�A��TA�|�A�1'A��A�ffA��A��#A�\)A�{A�Q�A��^A���A�r�A�M�A��#A�l�A�33A��A�bNA��TA�&�A�JA���A�ƨA~��A|�\Ay"�AwdZAt�RArM�AqƨAqO�Ap~�Ao�hAn��Am\)Ajz�Ah �AfȴAe��Ac��Ac;dAb�RAb$�Aa��A`��A^�A]\)A[�
AZz�AY�TAY%AW|�AVM�AU��AT��AS;dAR-AP�APȴAP�AOdZAN�DAM�FAMVALI�AK��AK33AJVAI�TAIXAH��AG+AE�#AEt�AD��AB=qA@1'A?��A>�A>�DA=VA:�A8��A7��A6�yA6bNA5�
A5C�A4��A4  A3C�A2z�A0��A.ZA+�A(�jA'�A'��A'+A&5?A%�A$��A$I�A#��A"^5A!33A -AZA��AjA �A��A�7At�A33A
=A=qAZA��A�hAz�Al�Ar�A��AA�/A�+A��A��A
ZA	��A��A�A��Ap�AC�A"�A�yA �A�Az�A ~�@�&�@��!@�O�@�ƨ@�`B@�ƨ@�$�@�?}@�bN@�v�@��^@�&�@���@�@�%@�z�@�{@��@�E�@��@�?}@��`@�j@�I�@��m@�"�@Ԭ@�G�@̓u@��T@��/@�K�@�bN@�V@�l�@���@��H@��@��y@�ȴ@�-@��w@�v�@�z�@��@��H@��\@��@�X@�Z@��@��y@��@���@��F@��@�%@�(�@��@���@�/@��D@�bN@��@�-@�7L@��@���@�o@��^@��j@��m@�+@�{@��h@�I�@��F@�@��-@�z�@
=@}V@{�
@y�^@x1'@uO�@s�m@r�\@p��@o�P@m��@k�m@jJ@i%@h  @f�y@e�T@ct�@bn�@`Q�@_
=@]p�@\��@Y��@Y%@W�w@Vff@S��@S"�@QG�@O�;@O;d@Lz�@J�H@J��@I�7@Hr�@G|�@EO�@D1@Bn�@A��@?�w@>v�@=p�@<z�@<9X@;S�@:^5@9X@8b@6$�@5p�@3o@1��@0r�@/�@.�R@.5?@-��@-�@,�D@+ƨ@*��@)7L@'�@'�P@&�y@%?}@$�j@#C�@!��@!�^@ �`@   @�@��@p�@Z@��@n�@�7@��@�@
=@v�@��@?}@I�@dZ@�!@7L@�;@�@{@p�@p�@z�@��@�@t�@
�\@
J@	�@	&�@�u@;d@�@E�@@O�@�/@z�@�
@�H@n�@�#@x�@&�@  �?���?�(�?�?�X?���?�\?�|�?�v�?��-?��m?��?��?�u?�E�?�33?���?�  ?ޗ�?�p�?ۥ�?�~�?�7L?�7L?�l�?��y?�E�?�?���?�S�?Ұ!?�n�?�n�?�&�?�bN?��;?�|�?�5??��?�j?�dZ?�?���?��?ə�?���?�K�?�ȴ?�z�?�33?�%?�\)?�5??�O�?���?��?�1?��m?�ƨ?�ƨ?�dZ?�dZ?�?�C�?��?�C�?�C�?�dZ?�ƨ?�(�?��D?�V?�/?�p�?��h?��-?��-?���?�{?�5??�V?�v�?���?���?��R?��?���?��?�;d?�\)?�|�?�|�?���?��w?���?��w?��;?�  ?�  ?� �A��HA��A��HA��A��yA��;A��yA��A��A��A��A��A��A��A��A��A��mA��/A��
A���A���A���A���A���A���A���AռjAՉ7AՇ+AՋDAՍPAՇ+AՏ\A�x�A�dZA�\)A�K�A�I�A�C�A�C�A�A�A�9XA�33A�1'A�/A�+A�&�A�&�A� �A��A��A��A��A��A�{A�bA�oA�JA�1A�1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       A��TA��`A���AՓuAՅA�ZA�G�A�?}A�1'A�(�A� �A��A�VA�1A�A�A�A�A�A�A�  A�  A�A�{A��A�{A���Aԧ�A�t�A�ZA�=qA�%Aӣ�AҋDAρA�ȴA�ĜA�A��A��A�%A��;A�9XA�^5A�"�A�bA�oA�5?A��A�?}A���A�z�A�A��A���A�~�A�dZA�l�A���A�VA��A���A��A��PA��A�?}A�C�A���A�=qA���A�(�A�l�A�x�A��/A��hA��A��/A�O�A��mA�=qA�M�A��A���A�ffA��;A��9A�|�A��TA�|�A�1'A��A�ffA��A��#A�\)A�{A�Q�A��^A���A�r�A�M�A��#A�l�A�33A��A�bNA��TA�&�A�JA���A�ƨA~��A|�\Ay"�AwdZAt�RArM�AqƨAqO�Ap~�Ao�hAn��Am\)Ajz�Ah �AfȴAe��Ac��Ac;dAb�RAb$�Aa��A`��A^�A]\)A[�
AZz�AY�TAY%AW|�AVM�AU��AT��AS;dAR-AP�APȴAP�AOdZAN�DAM�FAMVALI�AK��AK33AJVAI�TAIXAH��AG+AE�#AEt�AD��AB=qA@1'A?��A>�A>�DA=VA:�A8��A7��A6�yA6bNA5�
A5C�A4��A4  A3C�A2z�A0��A.ZA+�A(�jA'�A'��A'+A&5?A%�A$��A$I�A#��A"^5A!33A -AZA��AjA �A��A�7At�A33A
=A=qAZA��A�hAz�Al�Ar�A��AA�/A�+A��A��A
ZA	��A��A�A��Ap�AC�A"�A�yA �A�Az�A ~�@�&�@��!@�O�@�ƨ@�`B@�ƨ@�$�@�?}@�bN@�v�@��^@�&�@���@�@�%@�z�@�{@��@�E�@��@�?}@��`@�j@�I�@��m@�"�@Ԭ@�G�@̓u@��T@��/@�K�@�bN@�V@�l�@���@��H@��@��y@�ȴ@�-@��w@�v�@�z�@��@��H@��\@��@�X@�Z@��@��y@��@���@��F@��@�%@�(�@��@���@�/@��D@�bN@��@�-@�7L@��@���@�o@��^@��j@��m@�+@�{@��h@�I�@��F@�@��-@�z�@
=@}V@{�
@y�^@x1'@uO�@s�m@r�\@p��@o�P@m��@k�m@jJ@i%@h  @f�y@e�T@ct�@bn�@`Q�@_
=@]p�@\��@Y��@Y%@W�w@Vff@S��@S"�@QG�@O�;@O;d@Lz�@J�H@J��@I�7@Hr�@G|�@EO�@D1@Bn�@A��@?�w@>v�@=p�@<z�@<9X@;S�@:^5@9X@8b@6$�@5p�@3o@1��@0r�@/�@.�R@.5?@-��@-�@,�D@+ƨ@*��@)7L@'�@'�P@&�y@%?}@$�j@#C�@!��@!�^@ �`@   @�@��@p�@Z@��@n�@�7@��@�@
=@v�@��@?}@I�@dZ@�!@7L@�;@�@{@p�@p�@z�@��@�@t�@
�\@
J@	�@	&�@�u@;d@�@E�@@O�@�/@z�@�
@�H@n�@�#@x�@&�@  �?���?�(�?�?�X?���?�\?�|�?�v�?��-?��m?��?��?�u?�E�?�33?���?�  ?ޗ�?�p�?ۥ�?�~�?�7L?�7L?�l�?��y?�E�?�?���?�S�?Ұ!?�n�?�n�?�&�?�bN?��;?�|�?�5??��?�j?�dZ?�?���?��?ə�?���?�K�?�ȴ?�z�?�33?�%?�\)?�5??�O�?���?��?�1?��m?�ƨ?�ƨ?�dZ?�dZ?�?�C�?��?�C�?�C�?�dZ?�ƨ?�(�?��D?�V?�/?�p�?��h?��-?��-?���?�{?�5??�V?�v�?���?���?��R?��?���?��?�;d?�\)?�|�?�|�?���?��w?���?��w?��;?�  ?�  ?� �A��HA��A��HA��A��yA��;A��yA��A��A��A��A��A��A��A��A��A��mA��/A��
A���A���A���A���A���A���A���AռjAՉ7AՇ+AՋDAՍPAՇ+AՏ\A�x�A�dZA�\)A�K�A�I�A�C�A�C�A�A�A�9XA�33A�1'A�/A�+A�&�A�&�A� �A��A��A��A��A��A�{A�bA�oA�JA�1A�1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�FBĜB?}B[#B[#B\)B^5B^5B_;B_;B_;BaHB|�B|�Bq�B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B�oB�PB�PB�B�Bx�BcTBXBM�BH�BB�B=qB6FB�B�BhBPBB��B�B�yB�)B��B��B�3B��B��B��B�B��B��B��B��B��B�7B�Bx�Br�Be`B`BB\)BXB=qB49B.B)�B�B
��B
�
B
��B
�^B
�B
�PB
� B
n�B
XB
L�B
;dB
/B
,B
%�B
�B
�B
uB
%B	��B	�B	�TB	�
B	��B	��B	ǮB	ŢB	B	�RB	�B	��B	��B	��B	�oB	�=B	�%B	|�B	y�B	q�B	l�B	bNB	`BB	]/B	YB	VB	O�B	M�B	H�B	F�B	C�B	?}B	=qB	:^B	6FB	33B	'�B	%�B	%�B	�B	oB	\B	
=B	1B	B��B�B�B�mB�HB�;B�)B�B�B��B��BĜB�jB��B��B�bB�JB�1B�B�B|�Bz�Bx�Bt�Bp�Bo�BiyBdZBbNB`BB^5B]/B\)B[#BZBZBW
BYB\)BZB[#BYBXBVBS�BQ�BP�BK�BK�BG�BE�BC�BB�BA�BA�BA�B?}B?}B<jB<jB:^B;dB:^B<jB8RB8RB5?B5?B5?B5?B49B7LB8RB7LB8RB8RB:^B9XB5?B=qB<jB;dB;dB<jB;dB;dB;dB<jBL�BN�BVB[#BffBr�Bw�B�B�7B�1B��B��B�!B�^B�wB�BB�yB��B	�B	#�B	,B	>wB	N�B	[#B	bNB	jB	q�B	z�B	�B	�oB	��B	��B	�RB	ŢB	ǮB	��B	��B	��B	�B	�5B	�HB	�ZB	�fB	�B	�B	�B	�B	��B	��B	��B	��B
  B	��B
B
%B
	7B
	7B
JB
PB
hB
uB
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
#�B
$�B
&�B
'�B
)�B
)�B
-B
.B
/B
1'B
33B
33B
5?B
6FB
6FB
:^B
;dB
:^B
<jB
<jB
=qB
@�B
@�B
B�B
B�B
E�B
E�B
G�B
G�B
F�B
G�B
I�B
I�B
K�B
N�B
M�B
N�B
N�B
P�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
S�B
VB
W
B
W
B
XB
ZB
ZB
]/B
^5B
^5B
_;B
`BB
`BB
bNB
bNB
dZB
dZB
e`B
ffB
gmB
gmB
hsB
iyB
jB
jB
k�B
l�B
l�B
n�B
o�B
p�B
q�B
r�B
q�B
r�B
s�B
t�B
s�B
u�B
u�B
u�B
v�B
u�B
w�B
w�B
y�B
y�B
z�B
z�B
{�B
|�B
}�B
}�B
� B
� B
� B
�B
�B
�B
�B
�%B
�1B
�DB
�JB
�PB
�VB
�\B
�bB
�hB
�bB
�oB
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
��B
��B
��B
�B
�B
�B
�B
�!B
�'B
�-B
�9B
�9B
�9B
�FB
�FB
�FB
�LB
�RB
�RB
�XB
�XB
�XB
�^B
�^B
�dB
�^B
�dB
�dB
�dB
�jB
�dB
�dB
�dB
�jB
�dB
�dB
�dB
�dB
�dB
�dB
�jB
�dB
�dB
�jB
�jB
�jB
�dB
�dB
�dB
�jB
�dB
�jB
�jB
�jB
�jB
�jB
�jB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�9BB=qBYBYBZB\)B\)B]/B]/B]/B_;Bz�Bz�Bo�B�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�bB�DB�DB�B�Bv�BaHBVBK�BF�B@�B;dB49B�B�B\BDBB��B�B�mB�BȴB�wB�'B��B��B��B��B��B��B��B��B�uB�+B~�Bv�Bp�BcTB^5BZBVB;dB2-B,B'�B�B
��B
��B
��B
�RB
��B
�DB
}�B
l�B
VB
J�B
9XB
-B
)�B
#�B
�B
�B
hB
B	��B	�sB	�HB	��B	��B	ɺB	ŢB	ÖB	��B	�FB	�B	��B	��B	�{B	�bB	�1B	�B	z�B	w�B	o�B	jB	`BB	^5B	[#B	W
B	S�B	M�B	K�B	F�B	D�B	A�B	=qB	;dB	8RB	49B	1'B	%�B	#�B	#�B	�B	bB	PB	1B	%B	B��B�B�sB�`B�;B�/B�B�
B��B��B��BB�^B��B��B�VB�=B�%B�B~�Bz�Bx�Bv�Br�Bn�Bm�BgmBbNB`BB^5B\)B[#BZBYBXBXBT�BW
BZBXBYBW
BVBS�BQ�BO�BN�BI�BI�BE�BC�BA�B@�B?}B?}B?}B=qB=qB:^B:^B8RB9XB8RB:^B6FB6FB33B33B33B33B2-B5?B6FB5?B6FB6FB8RB7LB33B;dB:^B9XB9XB:^B9XB9XB9XB:^BJ�BL�BS�BYBdZBp�Bu�B�B�+B�%B��B��B�B�RB�jB�5B�mB��B	{B	!�B	)�B	<jB	L�B	YB	`BB	hsB	o�B	x�B	�B	�bB	��B	��B	�FB	ÖB	ŢB	ɺB	��B	��B	�B	�/B	�BB	�TB	�`B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
B
1B
1B
DB
JB
bB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
#�B
%�B
&�B
(�B
(�B
,B
-B
.B
0!B
2-B
2-B
49B
5?B
5?B
9XB
:^B
9XB
;dB
;dB
<jB
?}B
?}B
A�B
A�B
D�B
D�B
F�B
F�B
E�B
F�B
H�B
H�B
J�B
M�B
L�B
M�B
M�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
R�B
T�B
VB
VB
W
B
YB
YB
\)B
]/B
]/B
^5B
_;B
_;B
aHB
aHB
cTB
cTB
dZB
ffB
gmB
gmB
hsB
iyB
jB
jB
k�B
l�B
l�B
n�B
o�B
p�B
q�B
r�B
q�B
r�B
s�B
t�B
s�B
u�B
u�B
u�B
v�B
u�B
w�B
w�B
y�B
y�B
z�B
z�B
{�B
|�B
}�B
}�B
� B
� B
� B
�B
�B
�B
�B
�%B
�1B
�DB
�JB
�PB
�VB
�\B
�bB
�hB
�bB
�oB
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
��B
��B
��B
�B
�B
�B
�B
�'B
�-B
�3B
�?B
�?B
�FB
�RB
�RB
�RB
�XB
�^B
�^B
�dB
�dB
�dB
�jB
�jB
�qB
�jB
�qB
�qB
�qB
�wB
�qB
�qB
�qB
�}B
�wB
�wB
�wB
�wB
�wB
�wB
�}B
�wB
�wB
�}B
�}B
�}B
�wB
�wB
�wB
�}B
�wB
�}B
�}B
�}B
�}B
�}B
�}B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201810300001422021061413524920210614135249202106141746502021061417465020210614174650201810300001422021061413524920210614135249202106141746502021061417465020210614174650PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                    surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018103000014220181030000142  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018103000014220181030000142QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018103000014220181030000142QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015420210722160154IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                