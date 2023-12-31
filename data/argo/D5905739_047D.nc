CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:00:35Z creation      
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
_FillValue                 �  K�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  O�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  _�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  c�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  s�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �P   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �L   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �4   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   L   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   T   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   \   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   d   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � l   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   	   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    	   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        	0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        	8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       	@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    	H   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �Argo profile    3.1 1.2 19500101000000  20181121120035  20210617131509  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               /   /DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @؏�xjI|@؏�xjI|11  @؏�q�4�@؏�q�4�@6���/�@6���/��c�A�!��c�A�!�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?�  @ff@9��@y��@�  @�33@�33@���A��A(  AC33Aa��A�  A���A���A���A���A���A���A���B ffB  BffBffB ��B(��B0��B933B@  BH  BP��BX  B`  BhffBo��BxffB�ffB�  B�33B�  B�  B�33B�33B�ffB�33B�  B�33B�33B�  B�33B�33B�  B�33B�ffB�  B�  B�  B�  B���B�ffB�ffB�33B�33B�  B�B�ffB�33B�  B���C�CL�C�C�fC
  C�CL�C33C�fC33C  C�fC  C�C33C ffC"�C#��C%��C'�fC)�fC,  C.�C033C2  C3��C5��C8�C:33C<L�C>�C?�fCB�CD  CE��CH  CJ�CLffCN33CP  CR33CT�CV  CX33CZ  C[�fC^�C`  Ca��Cd  Cf�ChL�Cj�Ck�fCn  Cp33Cr�Cs�fCv  Cx�CzL�C|33C~  C��C�&fC��C�  C��C�  C��fC��3C�  C��C��C�&fC�  C��fC��3C�  C��C��C��C�  C�ٚC��3C�  C��C��C��3C�ٚC��fC�  C��C�&fC��C��3C��C��C�&fC��C��3C��3C��C��C��C��fC��3C��C�&fC�  C��fC�  C��C��C��3C��C�&fC��C�  C��C�33C��C��3C�  C��C��C�  C��fC�  C��C��3C�ٚC��fC��3C��C�  C��fC�  C��C�&fC��C��3C��3C�  C��C��C��C��C�&fC�&fC�&fC��C�ٚC��C�33C�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC��C��C�&fC�&fC�33C�33C�&fC��C��C�33C�33C�33C�33C��C��fC��3C��3C��3C��3C�  C��C��C�  C�&fC�&fC�33C�33C�33D �D l�DL�D�3D
ffD  D� D33D��DY�D� Dy�D�D!�fD$9�D&��D)S3D+�3D.�fD1�D3� D6FfD8��D;�fD>&fD@ٚDC� DF,�DH�fDK� DNS3DQ  DS�3DVY�DYfD[�fD^&fD`��DcfDes3Dg�3Dj33Dl�fDn��Dq  Ds@ Du� Dw��Dy��D{�fD~�D�#3D�6fD�L�D�` D�vfD�� D��3D���D���D�� D�� D�	�D�  D�@ D�ffD���D��3D��fD��fD�)�D�VfD��fD���D���D�33D�s3D���D��fD�#3D�ffD��fD�� D�,�D�ffD�� D�� D�fD�P D�� D�� D�� D�#3D�Y�D��3D���D���D��3D�3D�0 D�I�D�` D�l�D�|�D��fD�� D��3D��3D���D�  D�fD�33D�P D�i�DȆfDɩ�D���D���D�fD�FfD�|�DЩ�D�ٚD�3D�,�D�I�D�p DזfDضfD��3D��fD�3D��D�33D�I�D�\�D�p D�y�D� D��D��D��D�� D�ٚD��D��fD�fD�fD�  D�&fD�0 D�9�D�@ D�L�D�S3D�\�D�` D�p D�y�D���D���D���D���D���D��3D��fE �3E E�3E&fE��E>fE� EQ�Ed�E�3E3E	� E
��E�fE&fEfE��E�E� Ek3E^fE�3ED�EA�E�fE��EfE��E��E�E ��E"L�E#��E%fE%�3E'K3E(�fE*3E*��E,L�E-�fE/�E/�E1H E2��E4 E4��E6C3E7��E8� E:9�E;��E<a�E?��EB�3EE�EI;3EL0 EOfERvfEU� EX��E[��E^�fEa��Ee@ Eh9�Ek|�En�fEq�3Et� Ew��Ez�3E~ E�� E�.fE�� E�NfE���E�l E�� E���>���>���?   >���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>L��>���>���>���?��?333?L��?�  ?���?���?�  ?ٙ�@ff@��@&ff@9��@L��@fff@y��@���@�ff@�  @���@�ff@�33@���@ٙ�@陚@�33A��A	��A��A  A   A(  A0  A8  A@  AFffANffAVffA^ffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114444144414444444144111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ?fff?�  @&ff@Y��@���@�  @�33@�33AffA��A0  AK33Ai��A�  A���A���A���A���A���A���A���BffB
  BffBffB"��B*��B2��B;33BB  BJ  BR��BZ  Bb  BjffBq��BzffB�ffB�  B�33B�  B�  B�33B�33B�ffB�33B�  B�33B�33B�  B�33B�33B�  B�33B�ffB�  B�  B�  B�  B���B�ffB�ffB�33B�33B�  B�B�ffB�33B�  C ffC��C��C��CffC
� C��C��C�3CffC�3C� CffC� C��C�3C �fC"��C$L�C&L�C(ffC*ffC,� C.��C0�3C2� C4L�C6L�C8��C:�3C<��C>��C@ffCB��CD� CFL�CH� CJ��CL�fCN�3CP� CR�3CT��CV� CX�3CZ� C\ffC^��C`� CbL�Cd� Cf��Ch��Cj��ClffCn� Cp�3Cr��CtffCv� Cx��Cz��C|�3C~� C�Y�C�ffC�Y�C�@ C�Y�C�@ C�&fC�33C�@ C�Y�C�Y�C�ffC�@ C�&fC�33C�@ C�L�C�Y�C�Y�C�@ C��C�33C�@ C�L�C�Y�C�33C��C�&fC�@ C�Y�C�ffC�L�C�33C�L�C�Y�C�ffC�L�C�33C�33C�L�C�Y�C�L�C�&fC�33C�L�C�ffC�@ C�&fC�@ C�Y�C�Y�C�33C�L�C�ffC�Y�C�@ C�Y�C�s3C�Y�C�33C�@ C�L�C�Y�C�@ C�&fC�@ C�L�C�33C��C�&fC�33C�Y�C�@ C�&fC�@ C�L�C�ffC�L�C�33C�33C�@ C�L�C�L�C�Y�C�Y�C�ffC�ffC�ffC�L�C��C�L�C�s3C�ffC�ffC�ffC�ffC�ffC�ffC�s3C�ffC�Y�C�Y�C�ffC�ffC�s3C�s3C�ffC�L�C�L�C�s3C�s3C�s3C�s3C�Y�C�&fC�33C�33C�33C�33C�@ C�L�C�Y�C�@ C�ffC�ffC�s3C�s3C�s3D 9�D ��Dl�D�3D
�fD  D� DS3D��Dy�D  D��D,�D!�fD$Y�D&��D)s3D,3D.�fD19�D3� D6ffD9�D;�fD>FfD@��DC� DFL�DIfDK� DNs3DQ  DS�3DVy�DY&fD[�fD^FfD`��Dc&fDe�3Dg�3DjS3Dl�fDn��Dq  Ds` Du� DwٚDz�D{�fD~9�D�33D�FfD�\�D�p D��fD�� D��3D���D���D�� D�  D��D�0 D�P D�vfD���D��3D��fD�fD�9�D�ffD��fD���D��D�C3D��3D���D��fD�33D�vfD��fD�  D�<�D�vfD�� D�� D�&fD�` D�� D�� D�  D�33D�i�D��3D���D���D�3D�#3D�@ D�Y�D�p D�|�D���D��fD�� D��3D��3D���D� D�&fD�C3D�` D�y�DȖfDɹ�D���D���D�&fD�VfDό�Dй�D��D�3D�<�D�Y�Dր DצfD��fD��3D��fD�3D�)�D�C3D�Y�D�l�D� D≚D� D��D��D���D�� D��D���D�fD�fD�&fD�0 D�6fD�@ D�I�D�P D�\�D�c3D�l�D�p D�� D���D���D���D���D���D�ɚD��3D��fE �3E E�3E.fE��EFfE� EY�El�E3E3E	� E
��E�fE.fE&fE��E�E  Es3EffE�3EL�EI�E�fE��E&fE��E��E��E ��E"T�E#��E%fE%�3E'S3E(�fE*3E*��E,T�E-�fE/�E/�E1P E2��E4 E4��E6K3E7��E8� E:A�E;��E<i�E?��EB�3EE�EIC3EL8 EO&fER~fEU� EX��E[��E^�fEa��EeH EhA�Ek��En�fEq�3Et� Ew��Ez�3E~ E�� E�2fE�� E�RfE���E�p E�� E���?L��?fffG�O�G�O�G�O�G�O�?L��G�O�G�O�G�O�?L��G�O�G�O�G�O�G�O�G�O�G�O�G�O�?333G�O�G�O�?fff?���?���?�ff?�  ?���?ٙ�@   @��@&ff@9��@Fff@Y��@l��@�33@���@���@�ff@�  @���@�ff@�33@���@陚@���A��A	��A��A��A   A(  A0  A8  A@  AH  ANffAVffA^ffAfffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114444144414444444144111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                   @ �@ �@ V@ �@ �@ ""@ )�@ 0x@ 5�@ >@ G�@ SI@ `B@ m:@ {�@ ��@ ��@ ��@ �-@ ��@ �|@ �#@ �@ �q@@o@ @-�@<@G�@UU@dZ@p�@~K@��@��@��@�F@@��@��@�@��@�@�@"�@/�@>@K�@X�@g@t�@��@�@�a@��@�R@��@Ӡ@��@�L@��@
�@�@%�@5?@B8@O0@\)@i!@x&@�+@�u@��@�@�k@�o@�h@�@�@  @�@O@)�@7�@F�@R�@^5@k�@z3@��@�0@��@��@�&@�o@�@��@� @v@�@
@-@:@FQ@UU@c�@s_@�@��@��@��@��@��@�7@�/@�4@�,@v@{@"�@1�@>@Ji@X�@g�@t�@�@�\@��@��@��@��@��@�T@�L@��@�@�@$.@2�@@�@O�@]�@k�@ww@��@�@�m@��@�@��@�
@�@�@  @V@�@(G@4�@B�@Q�@`�@oF@{�@��@��@�5@��@��@�@��@��@� @	@	�@	
@	-@	<@	G�@	S�@	b�@	r@	�@	�D@	�H@	�M@	�F@	@	є@	��@	��@	�~@
�@
*@
#�@
/�@
<@
K@
Yn@
e�@
r@
�W@
��@
�a@
��@
��@
��@
�O@
�T@
�@
��@	�@�@&;@3�@B8@O�@^5@k�@y�@��@��@�@��@��@�o@�@�@�e@@b@[@*S@7�@FQ@S�@bN@o�@|�@�7@��@��@�9@��@ψ@��@�@�@�@b@
@,`@:�@I@UU@e	@r�@�@��@�U@��@��@7�@|�@�>@
=@Q�@�<@�;@$�@i�@��@� @>@�p@��@�@Wb@��@�@(�@qS@��@ �@H]@�@�t@#�@m�@�R@@K@��@�/@&;@m�@�-@�@7L@y�@�^@�9@:�@x�@��@�Y@/�@l�@��@�#@�@V@��@�@�@B8@~K@�@�@/@i�@��@��@O@X�@��@�\@*@SI@��@�C@ o@ SI@ ��@ ��@!�@!`A@!�(@!�@")�@"n�@"��@"�,@#<�@#�@#@$�@$I@$��@$�7@%@%Q�@%�u@%խ@&*@&T�@&�@&��@'V@'K@'�+@'@'��@(5�@(r@(�@(��@)"�@)^�@)��@)��@*�@*N�@*��@*�W@+v@+C�@+�@+��@,]@,C�@,��@,Ĝ@-@-C�@-�W@-�&@-��@.;d@.x&@.��@.�@/+@/g@/�z@/�/@0�@0P�@0��@0�J@0�Q@19X@1t@1��@1�@2 @2Z@2�#@2��@3�@3=q@3v@3�@3�m@4g@4X@4�\@4�c@5@5<@5r�@5��@5�@6 �@6\�@6��@6��@7@7Lu@7��@7�>@7�Q@8:@8t�@8�(@9��@:�@:�R@;&;@;��@<9X@<�(@=A�@=��@>G�@>�@?M�@?��@@��@@�q@A��@A�E@B�T@C:�@C�z@D> @D��@E?}@E�O@Fi!@F�*@G`�@G�@H��@H�@I��@J�@J��@KV@K��@L:�@L�7@M1�@M��@NS�@N�@Ot�@P@P`B@Q��@S1@Tm�@U��@W�@X^�@Y�@[,`@\��@]��@_	�@`Z�@a�7@c*@dy�@e�@g�@hi�@i�!@k�@lb�@m�c@o�@p\)@q�1@s�@ti�@u�@wC@ @ �G�O�G�O�G�O�G�O�@ G�O�G�O�G�O�@ G�O�G�O�G�O�G�O�G�O�G�O�G�O�@ ^G�O�G�O�@ �@ @ �@ v@ �@ �@ 1@ 
=@ �@ V@ b@ �@ �@ �@ �@ �@ [@  @ ""@ $.@ &�@ )�@ +�@ .l@ 1�@ 3�@ 7L@ :�@ >@ @�@ DD@ G�@ K@ N�@ Q�@ T�@ X@ [z@ ^�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AҋDA�~�Aҗ�Aҡ�AҴ9AҶFAҴ9AҶFAҰ!AҮAҴ9A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ȴA�ƨA���A���A���A��`A�JA�bAқ�A�ƨA��mA�n�A�l�A�bAʇ+A�v�AȾwAȟ�A�C�A�E�A��PA���A�ZA��jA�M�A���A��A��A�jA��#A���A��
A���A�C�A��A�oA��mA��A���A�E�A��TA���A�XA���A��A���A���A��mA��A��/A�VA�5?A���A�bNA�7LA��A�ĜA�33A��A�1'A��uA��FA�oA��9A���A���A�E�A��9A�oA��/A���A�E�A��A�/A��A�9XA�A���A�I�A��A��`A�A�^5A���A�C�A��A�A~^5Ay��Ax�yAw�AuC�As�-ArbAoG�AmO�Al��Al5?Ak�AiAfM�AdQ�Ac��Ac"�Ab{AahsA`�RA_`BA^^5A\��A[��AY|�AW�TAV�/AV��AU��AT��AS�mAR�AQ�PAO�FAOC�AOAN��AN��AMC�AI�PAGC�AC�AB�AA"�A?�A=XA<�RA<z�A;�^A: �A9��A9��A9�A9&�A8�9A7��A6��A3��A21'A1�mA1��A1��A1;dA0��A0��A0$�A/��A/��A/��A.�yA.��A.ZA-�;A-33A,v�A+|�A)�;A%��A$  A#�FA#�A#/A"ffA"bA!��A M�AoA5?A�A~�At�AjA�A�wAS�A�9A��A\)AffAz�A�hAA�HA��AQ�A7LAS�A	�wA��AdZA��A�AVA��A�FA Q�@�K�@��@��@��@�z�@��H@�V@�;d@��H@�ff@�@��@땁@�X@�1@�@���@�/@�@�^5@�(�@���@�t�@�^5@��@׮@֧�@� �@�+@���@�%@��
@�K�@���@��@�A�@�E�@��
@��@�o@���@�`B@���@�C�@��@��H@�V@��@���@��@���@���@��@�/@�l�@�$�@��w@�dZ@�"�@�-@��@���@�p�@�1'@�;d@��u@�33@��-@��@��@�;d@�n�@�G�@�(�@���@��@���@��-@��@�K�@�x�@��j@~�y@~E�@{S�@y��@w��@w;d@v�R@u�@r��@r=q@qG�@o\)@l�@l��@ko@j^5@i�@i�^@h  @e��@c��@`bN@^{@\�/@\Z@Z-@YG�@W�w@W+@U�T@T��@R��@P��@O�P@N�y@M�@Lz�@K�F@K33@IG�@Fff@F@D�j@C��@B��@A��@?
=@=��@<I�@;�m@9��@9x�@9%@6ȴ@6@4(�@3�@2n�@1��@0Ĝ@/��@.5?@-V@,�@*�!@*=q@)�#@)�7@(�@'\)@&@$�/@#�F@"�\@!�^@!�@  �@�;@��@�R@E�@@j@�
@�@�!@J@X@�w@��@$�@��@�
@@=q@��@�9@�@|�@ȴ@�+@5?@��@O�@1@�
@t�@33@@
^5@	�#@��@�9@1'@�P@��@$�@��@p�@/@1@C�@��@-@�@ b?��?��?��H?���?��?���?�33?��?�bN?�v�?�C�?�7L?�K�?�?}?���?�!?���?ߝ�?ޗ�?ݑh?�(�?�=q?�X?�Q�?׍P?��T?�z�?Ұ!?�G�?ϝ�?�;d?Η�?�{?�V?�j?�ƨ?˅?��H?�^5?�X?�Q�?���?�
=?Ƨ�?Ł?��?��7?�;d?�|�?�5??�{?��h?��?�j?�I�?��H?�^5?�^5?��^?���?�X?�x�?��?�~�?��H?�dZ?�dZ?��m?��D?�/?���?���AҋDA҅A҃A҇+A�~�A҃A҅A҅A�~�A҃AҋDAҩ�Aҥ�Aҟ�Aҏ\AґhAҕ�AґhA҉7A҅A҇+A�x�A�x�A�x�AҁA�z�A�~�A҅A҉7AґhAҏ\Aҥ�Aқ�Aҕ�AҴ9AҴ9AҴ9AҶFAҶFAҶFAҴ9AҴ9AҲ-AҴ9AҴ9AҶFAҲ-AҬAҮAҬAҮAҮAҴ9AҸRAҶFA�ĜA�ƨA���A�ƨA�ȴG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                   AҋDA�~�Aҗ�Aҡ�AҴ9AҶFAҴ9AҶFAҰ!AҮAҴ9A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ȴA�ƨA���A���A���A��`A�JA�bAқ�A�ƨA��mA�n�A�l�A�bAʇ+A�v�AȾwAȟ�A�C�A�E�A��PA���A�ZA��jA�M�A���A��A��A�jA��#A���A��
A���A�C�A��A�oA��mA��A���A�E�A��TA���A�XA���A��A���A���A��mA��A��/A�VA�5?A���A�bNA�7LA��A�ĜA�33A��A�1'A��uA��FA�oA��9A���A���A�E�A��9A�oA��/A���A�E�A��A�/A��A�9XA�A���A�I�A��A��`A�A�^5A���A�C�A��A�A~^5Ay��Ax�yAw�AuC�As�-ArbAoG�AmO�Al��Al5?Ak�AiAfM�AdQ�Ac��Ac"�Ab{AahsA`�RA_`BA^^5A\��A[��AY|�AW�TAV�/AV��AU��AT��AS�mAR�AQ�PAO�FAOC�AOAN��AN��AMC�AI�PAGC�AC�AB�AA"�A?�A=XA<�RA<z�A;�^A: �A9��A9��A9�A9&�A8�9A7��A6��A3��A21'A1�mA1��A1��A1;dA0��A0��A0$�A/��A/��A/��A.�yA.��A.ZA-�;A-33A,v�A+|�A)�;A%��A$  A#�FA#�A#/A"ffA"bA!��A M�AoA5?A�A~�At�AjA�A�wAS�A�9A��A\)AffAz�A�hAA�HA��AQ�A7LAS�A	�wA��AdZA��A�AVA��A�FA Q�@�K�@��@��@��@�z�@��H@�V@�;d@��H@�ff@�@��@땁@�X@�1@�@���@�/@�@�^5@�(�@���@�t�@�^5@��@׮@֧�@� �@�+@���@�%@��
@�K�@���@��@�A�@�E�@��
@��@�o@���@�`B@���@�C�@��@��H@�V@��@���@��@���@���@��@�/@�l�@�$�@��w@�dZ@�"�@�-@��@���@�p�@�1'@�;d@��u@�33@��-@��@��@�;d@�n�@�G�@�(�@���@��@���@��-@��@�K�@�x�@��j@~�y@~E�@{S�@y��@w��@w;d@v�R@u�@r��@r=q@qG�@o\)@l�@l��@ko@j^5@i�@i�^@h  @e��@c��@`bN@^{@\�/@\Z@Z-@YG�@W�w@W+@U�T@T��@R��@P��@O�P@N�y@M�@Lz�@K�F@K33@IG�@Fff@F@D�j@C��@B��@A��@?
=@=��@<I�@;�m@9��@9x�@9%@6ȴ@6@4(�@3�@2n�@1��@0Ĝ@/��@.5?@-V@,�@*�!@*=q@)�#@)�7@(�@'\)@&@$�/@#�F@"�\@!�^@!�@  �@�;@��@�R@E�@@j@�
@�@�!@J@X@�w@��@$�@��@�
@@=q@��@�9@�@|�@ȴ@�+@5?@��@O�@1@�
@t�@33@@
^5@	�#@��@�9@1'@�P@��@$�@��@p�@/@1@C�@��@-@�@ b?��?��?��H?���?��?���?�33?��?�bN?�v�?�C�?�7L?�K�?�?}?���?�!?���?ߝ�?ޗ�?ݑh?�(�?�=q?�X?�Q�?׍P?��T?�z�?Ұ!?�G�?ϝ�?�;d?Η�?�{?�V?�j?�ƨ?˅?��H?�^5?�X?�Q�?���?�
=?Ƨ�?Ł?��?��7?�;d?�|�?�5??�{?��h?��?�j?�I�?��H?�^5?�^5?��^?���?�X?�x�?��?�~�?��H?�dZ?�dZ?��m?��D?�/?���?���AҋDA҅A҃A҇+A�~�A҃A҅A҅A�~�A҃AҋDAҩ�Aҥ�Aҟ�Aҏ\AґhAҕ�AґhA҉7A҅A҇+A�x�A�x�A�x�AҁA�z�A�~�A҅A҉7AґhAҏ\Aҥ�Aқ�Aҕ�AҴ9AҴ9AҴ9AҶFAҶFAҶFAҴ9AҴ9AҲ-AҴ9AҴ9AҶFAҲ-AҬAҮAҬAҮAҮAҴ9AҸRAҶFA�ĜA�ƨA���A�ƨA�ȴG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�RB�BB�B%�B.B5?B9XB1'BQ�BYBaHBdZBcTBaHBr�B}�B��B��B��B��B�B��B�{B�uB��B��B�%B�B}�Bz�By�Bv�B�B� B�B�B� B}�Bx�Bu�BgmBR�BH�B:^B-B�BB��B�B�yB�mB�BB�
BƨB�jB�3B�B��B��B�BhsBR�BH�BA�B-B�BDB
�B
�B
��B
ƨB
ÖB
�jB
��B
�\B
� B
p�B
hsB
`BB
T�B
P�B
M�B
>wB
(�B
#�B
�B
PB	��B	�B	�BB	��B	��B	ɺB	B	�3B	��B	�uB	��B	��B	�{B	�VB	�B	y�B	o�B	bNB	W
B	C�B	6FB	49B	@�B	?}B	=qB	49B	,B	(�B	+B	'�B	%�B	$�B	"�B	{B	+B��B�mB�HB�B��B��B��B��B��B��B��B��B��B��B��BɺBȴB�^B�^B�XB�XB�LB�?B�-B�'B�B�!B�-B�'B�B�B�B��B��B��B��B��B�B�B� B}�By�Bw�Bv�Br�Bm�Bk�BhsBe`BaHB_;B]/B\)BZBYBW
BW
BVBR�BS�BR�BS�BR�BO�BO�BK�BE�BE�B@�B?}B>wB;dB:^B7LB5?B8RB6FB5?B33B1'B2-B33B33B6FB6FB6FB6FB7LB6FB5?B6FB5?B5?B5?B7LB6FB9XB;dB?}B@�BA�BB�BC�BF�BG�BH�BH�BL�BK�BK�BW
B^5By�Bw�B{�B�bB��B�B�jB�B��B��B	B	JB	�B	&�B	5?B	:^B	A�B	D�B	J�B	R�B	_;B	r�B	�B	�%B	��B	��B	��B	�B	�'B	�'B	�!B	�-B	�RB	�dB	��B	��B	��B	��B	�
B	�#B	�5B	�ZB	�B	�B	��B	��B	��B	��B	��B
  B
B
B
B
+B
	7B
	7B

=B
DB
VB
VB
hB
oB
uB
uB
�B
�B
�B
�B
!�B
"�B
!�B
$�B
%�B
'�B
'�B
+B
+B
0!B
1'B
2-B
2-B
5?B
49B
5?B
49B
8RB
<jB
;dB
=qB
=qB
>wB
@�B
A�B
B�B
D�B
E�B
F�B
F�B
G�B
H�B
I�B
K�B
L�B
M�B
N�B
N�B
O�B
P�B
Q�B
S�B
T�B
T�B
T�B
T�B
VB
W
B
YB
ZB
ZB
[#B
\)B
]/B
^5B
^5B
^5B
_;B
`BB
`BB
bNB
cTB
cTB
e`B
e`B
ffB
hsB
gmB
iyB
iyB
k�B
l�B
l�B
m�B
n�B
n�B
o�B
p�B
q�B
q�B
q�B
q�B
s�B
s�B
t�B
t�B
t�B
u�B
v�B
w�B
w�B
w�B
x�B
x�B
z�B
y�B
z�B
z�B
|�B
}�B
}�B
}�B
�B
� B
�B
�B
�B
�%B
�1B
�1B
�1B
�=B
�=B
�JB
�VB
�\B
�hB
�oB
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
��B
�B
�B
�B
�!B
�!B
�-B
�'B
�-B
�3B
�9B
�9B
�?B
�FB
�LB
�RB
�RB
�XB
�XB
�XB
�XB
�XB
�XB
�^B
�^B
�^B
�^B
�^B
�^B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�:B�+B|B%�B-�B5*B9DB1BQ�BYBa5BdHBcBBa7Br�B}�B��B��B�}B��B��B��B�lB�gB��B��B�B� B}�Bz�By�Bv�B��B�B��B�B�B}�Bx�Bu�BghBR�BH�B:ZB-
B~BB��B�B�xB�lB�BB�
BƩB�kB�5B�B��B��B�#BhwBR�BH�BA�B-B�BJB
�B
�B
��B
ưB
ßB
�sB
��B
�fB
�
B
p�B
h~B
`NB
U
B
P�B
M�B
>�B
)B
#�B
�B
`B	�
B	��B	�SB	�	B	��B	��B	¢B	�FB	��B	��B	��B	��B	��B	�lB	�6B	y�B	o�B	bfB	W#B	C�B	6_B	4SB	@�B	?�B	=�B	4UB	,%B	)B	+B	(B	&B	$�B	"�B	�B	KB��B�B�iB�9B�B�B�B�B�B�B�
B�B��B��B��B��B��B��B��B��B��B�wB�kB�YB�TB�HB�OB�[B�VB�KB�>B�3B�-B�B�B��B��B�RB�FB�4B~)BzBxBv�Br�Bm�Bk�Bh�Be�Ba�B_uB]iB\dBZXBYSBWGBWGBVBBS0BT6BS1BT7BS2BPBP BLBE�BE�B@�B?�B>�B;�B:�B7�B5�B8�B6�B5�B3zB1oB2uB3|B3|B6�B6�B6�B6�B7�B6�B5�B6�B5�B5�B5�B7�B6�B9�B;�B?�B@�BA�BB�BC�BF�BHBI	BI	BM#BLBLBWfB^�Bz=Bx4B|OB��B�WB�yB��B�!B�IB�@B	�B	�B	B	'rB	5�B	:�B	BB	E1B	KYB	S�B	_�B	sRB	��B	��B	�,B	��B	��B	��B	��B	��B	��B	��B	�B	�+B	�MB	ˎB	͝B	��B	��B	��B	�B	�7B	�eB	�{B	��B	��B	��B	��B	��B
 �B
B
	B
B
'B

6B

8B
AB
JB
_B
aB
vB
�B
�B
�B
�B
�B
�B
�B
"�B
#�B
"�B
&
B
'B
)#B
)&B
,;B
,>B
1`B
2iB
3rB
3uB
6�B
5�B
6�B
5�B
9�B
=�B
<�B
>�B
>�B
?�B
A�B
B�B
C�B
FB
GB
HB
HB
I(B
J0B
K9B
MIB
NQB
OZB
PbB
PeB
QnB
RvB
S�B
U�B
V�B
V�B
V�B
V�B
W�B
X�B
Z�B
[�B
[�B
\�B
]�B
^�B
_�B
_�B
_�B
`�B
bB
b
B
dB
e!B
e$B
g3B
g5B
h>B
jMB
iJB
kXB
k[B
miB
nrB
ntB
o}B
p�B
p�B
q�B
r�B
s�B
s�B
s�B
s�B
u�B
u�B
v�B
v�B
v�B
w�B
x�B
y�B
y�B
y�B
z�B
z�B
}B
{�B
}B
}
B
B
�"B
�%B
�(B
�?B
�AB
�RB
�_B
�kB
�|B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�
B
�B
�"B
�/B
�:B
�AB
�KB
�XB
�_B
�rB
�}B
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
�B
�
B
�B
�#B
�0B
�:B
�KB
�eB
��B
��B
��B
��B
��B
��B
��B
�B
�%B
�4B
�KB
�`B
�vB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�!B
�1B
�@B
�NB
�^B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201811211200352021061413555320210614135553202106171313332021061713133320210617131333201811211200352021061413555320210614135553202106171313332021061713133320210617131333PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018112112003520181121120035  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018112112003520181121120035QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018112112003520181121120035QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713150920210617131509IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                