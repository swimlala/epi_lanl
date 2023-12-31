CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  D   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:56Z creation      
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
resolution        =���   axis      Z        
   ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  E�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     
   H|   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  R�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     
   U$   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
   _D   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  id   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
   k�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  v   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
   x�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
   ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
   �\   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
   �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ˌ   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ˔   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ˜   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ˤ   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  �  ˬ   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                     �P   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �x   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ̀   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                     ̈   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �$   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��Argo profile    3.1 1.2 19500101000000  20180724220256  20210722161417  5905740 5905740 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7220                            7220                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12003                           12003                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�j��@�j��11  @�j��I��@�j��I��@*i^��@*i^���cH�9.��cH�9.�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?��@ff@@  @�  @�  @���@�  A   A  A&ffA@  A^ffA���A���A���A�  A�  A�  A���A�  B   BffB��B��B   B(  B0  B8��B@  BHffBPffBW��B`  BhffBpffBx��B�33B�  B�  B�33B�  B�33B�  B�ffB�ffB�33B�33B���B���B�  B�ffB�33B�  B�33B�  B�ffBЙ�B���B���Bܙ�B���B�ffB�ffB�  B���B�  B�33B�33C �C�C�C  C�C
33C  C  C  C�C�C  C  C�C  C  C �C"�C$  C%�fC(  C*�C,�C.  C/�fC2  C4  C6�C833C:�C<�C>�C@�CB  CD  CF  CH  CJ�CL�CN  CP  CR  CS�fCU�fCX  CZ�C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cw�fCy�fC{�fC~  C�  C��C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C��C��C��C�  C��3C��3C�  C�  C��C��C��C�  C�&fC��C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C��3C��C�  C�  C��C��C�  C�  C�  C��C��C��C�  C��C��C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C��C��C��C��C��C�  C�  C��3C��3C�  C��C��C��C��C�  C�  C�  C�  C�  C��C��C��C��C�  C��C��C�  C�  C��C��C��C��C�  C�  C�  C�  C��3C��C��3C��fC���D � D�D� Ds3D�3D��D�D	l�D
��DٚD��D��Dy�Dy�D�D,�D` D��DfDffD��D�D��D�DffD��D!  D",�D#@ D$L�D%�3D'33D(  D)��D*�3D+��D-ffD.� D/�fD0�fD29�D3�3D4ٚD6&fD7Y�D8l�D9� D;  D<,�D=s3D>� D?� DAS3DBy�DC� DD�3DE�3DGFfDH��DI�fDK&fDLffDM�fDN� ?��?   ?   ?   ?   ?   ?   ?   >���?��?   ?��?333?   ?��?333?   ?   >���?333?   ?��?��?333?��?   ?��?��?L��?fff?���?���?�33?���@   @��@   @&ff@@  @S33@l��@y��@���@�33@�  @���@�ff@�33@�33@���@���@���A��A33A  AffA   A&ffA.ffA4��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444444441411411444141414414111111111111111111111111111111111                                                                                                                                                                                                                                                                        ?fff?���@&ff@`  @�  @�  @���@�  A  A  A.ffAH  AfffA���A���A���A�  A�  A�  A���A�  B  B
ffB��B��B"  B*  B2  B:��BB  BJffBRffBY��Bb  BjffBrffBz��B�33B�  B�  B�33B�  B�33B�  B�ffB�ffB�33B�33B���B���B�  B�ffB�33B�  B�33B�  B�ffBљ�B���B���Bݙ�BᙚB�ffB�ffB�  B���B�  B�33B�33C ��C��C��C� C��C
�3C� C� C� C��C��C� C� C��C� C� C ��C"��C$� C&ffC(� C*��C,��C.� C0ffC2� C4� C6��C8�3C:��C<��C>��C@��CB� CD� CF� CH� CJ��CL��CN� CP� CR� CTffCVffCX� CZ��C\��C^� C`� Cb� Cd� Cf� Ch� Cj� Cl� Cn� Cp� Cr� Ct� Cv� CxffCzffC|ffC~� C�@ C�L�C�@ C�@ C�@ C�33C�@ C�L�C�@ C�@ C�@ C�@ C�@ C�@ C�L�C�@ C�@ C�@ C�L�C�L�C�Y�C�Y�C�@ C�33C�33C�@ C�@ C�L�C�Y�C�L�C�@ C�ffC�Y�C�L�C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�L�C�@ C�33C�@ C�@ C�@ C�L�C�@ C�@ C�@ C�@ C�L�C�@ C�@ C�@ C�L�C�@ C�@ C�33C�L�C�@ C�@ C�L�C�L�C�@ C�@ C�@ C�L�C�L�C�L�C�@ C�L�C�L�C�@ C�@ C�@ C�L�C�@ C�@ C�@ C�@ C�@ C�33C�@ C�L�C�L�C�Y�C�L�C�L�C�@ C�@ C�33C�33C�@ C�Y�C�Y�C�Y�C�L�C�@ C�@ C�@ C�@ C�@ C�Y�C�L�C�L�C�L�C�@ C�L�C�L�C�@ C�@ C�L�C�L�C�L�C�L�C�@ C�@ C�@ C�@ C�33C�L�C�33C��fC���D � D9�D  D�3D3D��D9�D	��D
��D��D�D�D��D��D,�DL�D� D��D&fD�fDٚD,�DٚD9�D�fDٚD!  D"L�D#` D$l�D%�3D'S3D(@ D)��D+3D,�D-�fD.� D/�fD1fD2Y�D3�3D4��D6FfD7y�D8��D:  D;@ D<L�D=�3D>� D@  DAs3DB��DC� DD�3DF3DGffDHٚDJfDKFfDL�fDM�fDN� G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?fffG�O�?�  ?���G�O�?�  ?���G�O�G�O�G�O�?fffG�O�?�  G�O�?���G�O�G�O�?�  G�O�?���?�ff?�33?���?ٙ�?�33@ff@   @,��@@  @Fff@`  @s33@�ff@���@���@�33@�  @���@�ff@�33@�33@���@���A��A	��A33A  AffA(  A.ffA6ffA<��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444444441411411444141414414111111111111111111111111111111111                                                                                                                                                                                                                                                                        @ �@ @ V@ {@ O@ ""@ (G@ /�@ 6�@ =q@ F�@ Q�@ ^�@ m�@ |?@ ��@ �0@ ��@ �~@ ��@ ��@ �t@ ��@ � @�@@�@,`@;d@G�@V@c�@o�@~K@��@�H@��@��@@�7@ލ@�@��@�@�@#�@0x@>@Ji@X@ff@uk@�d@�\@��@��@��@�@�[@�@��@��@�@B@%�@2�@@�@O0@\�@j@x&@��@��@�@�r@��@�c@�
@�`@�@  @�@�@(�@6�@D�@R�@_�@l�@z�@�7@��@��@��@�&@��@�#@�y@�q@@�@g@,`@:@G�@UU@c�@qS@~K@��@��@��@�9@@��@ލ@�@�,@�@{@""@/�@=q@K@X�@ff@t@��@�\@�U@��@��@��@Ӡ@��@��@��@
=@6@%�@3�@@�@N�@\)@i�@ww@�@�u@�m@�@��@�@׹@�@�@  @�@�@(�@6�@D�@SI@`B@m:@|�@��@��@��@�~@�&@��@�t@�@��@	j@	�@	�@	+�@	:@	G�@	UU@	c�@	p�@	~K@	��@	��@	��@	��@	@	�7@	ލ@	�@	�,@
%@
*@
""@
/�@
>@
K�@
X�@
ff@
t@
�d@
�@
��@
��@
�@
ƨ@
Ӡ@
�H@
��@
�E@
=@�@%�@33@@�@M�@\)@j@x&@�|@�u@�@�@��@ȴ@�[@�@�@^@@�@(�@6�@DD@Q�@_�@n�@{�@�7@��@��@�-@��@��@�t@��@�q@@�@�@,`@:@G�@T�@c�@o�@y�@�@��@��@��@ �@I�@]�@�p@��@��@��@	�@$�@M�@i!@�$@��@Ӡ@� @�@A�@e�@��@�U@��@�`@	�@,`@Lu@i�@�|@�!@խ@��@�@<@V�@~�@�@�j@ލ@�@'�@Ji@m�@��@�@Ӡ@��@o@5?@UU@ww@�@�w@��@��@[@A�@i!@�7@�Y@�|@�@
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@ �G�O�@ j@ G�O�@ j@ G�O�G�O�G�O�@ �G�O�@ jG�O�@ G�O�G�O�@ jG�O�@ @ v@ %@ �@ 1@ 	�@ 
�@ �@ @ @ �@ {@ �@ B@ �@ [@ g@ ""@ $.@ &�@ )�@ -@ /@ 2�@ 5?@ 7L@ ;d@ =q@ @,@ DD@ F�@ Ji@ M$G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aڡ�Aڡ�Aک�Aک�Aڴ9AڸRAڸRAڶFAڶFAڶFAھwA�A�A�A���AڸRAں^AڼjAں^AڸRAڬAځA�hsA�Q�A�E�A�E�A�C�A�C�A�;dA�;dA�;dA�=qA�A�A�?}A�=qA�33A�-A�"�A�bAٛ�A֡�A�AǼjAƕ�AŰ!A�"�A�bNA\A�z�A�n�A�p�A��hA��A��wA��jA��`A���A��RA��HA��A�&�A��A�jA�5?A���A���A�hsA�K�A��A�+A�VA��+A�ƨA���A�XA�?}A���A��A|�yAxz�Au�Ap �Anz�Ak�Afv�AbA�A]ƨAY��AUARȴAPZAK%AHJAD��AC�hAB�AA
=A?K�A>�9A=A<ĜA;�-A:�A:9XA9"�A7��A6JA5S�A4�A49XA3��A2�yA2�jA2n�A1�A1S�A0�`A0Q�A/t�A.ĜA-��A,��A+�A+��A)�-A(~�A'&�A&~�A%A%O�A$��A$�A#�A#&�A"ĜA"M�A!��A!K�A!A ZA 1A�A�PA�A��A5?AƨA��A�A7LA%A�yA�!Ar�AZA5?AJA�A�AS�A�HAI�Ap�A��A��An�AQ�AA�hA�`A��AbNA��A|�A&�A�!A$�AA|�A�A�DA$�A��Ap�A�A�+A�A��AC�A�A�A�yA��AM�AA|�A"�A��A��A-A�hA�A
�yA
��A
��A
=qA	�A�/A��Ar�A��A7LAv�A��A��A\)A��A~�AI�A��A��A"�A��A��Az�AZA-A�
A;dA �A �9A �uA Q�@��
@�\)@���@�M�@��^@�`B@���@�(�@���@��
@��w@�"�@���@�x�@�z�@�b@��@�@�~�@�n�@�M�@���@� �@�dZ@���@�ȴ@�@��m@�~�@�/@�\@�G�@���@�l�@���@��y@�V@ߍP@�x�@�C�@��#@׾w@ՙ�@���@�Z@��@Ұ!@��@Л�@�1'@�K�@�E�@ͩ�@̣�@�1@˅@�"�@�^5@��@�j@��y@�5?@�%@�;d@�v�@��D@���@��@�@��`@��D@�ƨ@�
=@�$�@���@� �@�t�@�X@�ƨ@�o@�@���@�@�Z@�\)@�=q@�-@��7@���@��@���Aڝ�Aڡ�Aڥ�Aڥ�Aڥ�Aڥ�Aڥ�Aڡ�Aڡ�Aڡ�Aڣ�Aڧ�Aڧ�Aڧ�Aڗ�Aڝ�Aڝ�Aڟ�Aڡ�Aڟ�Aڟ�Aڣ�Aڥ�Aڣ�Aڡ�Aڡ�Aڟ�Aڛ�Aڛ�Aڝ�Aڝ�Aڟ�Aڥ�Aک�Aک�Aک�Aک�Aک�Aک�Aک�AڮAڶFAڶFAڸRAں^AڸRAں^AڶFAڴ9AڶFAڸRAڶFAڴ9AڶFAڶFAڸRAں^A���AھwA���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                        Aڡ�Aڡ�Aک�Aک�Aڴ9AڸRAڸRAڶFAڶFAڶFAھwA�A�A�A���AڸRAں^AڼjAں^AڸRAڬAځA�hsA�Q�A�E�A�E�A�C�A�C�A�;dA�;dA�;dA�=qA�A�A�?}A�=qA�33A�-A�"�A�bAٛ�A֡�A�AǼjAƕ�AŰ!A�"�A�bNA\A�z�A�n�A�p�A��hA��A��wA��jA��`A���A��RA��HA��A�&�A��A�jA�5?A���A���A�hsA�K�A��A�+A�VA��+A�ƨA���A�XA�?}A���A��A|�yAxz�Au�Ap �Anz�Ak�Afv�AbA�A]ƨAY��AUARȴAPZAK%AHJAD��AC�hAB�AA
=A?K�A>�9A=A<ĜA;�-A:�A:9XA9"�A7��A6JA5S�A4�A49XA3��A2�yA2�jA2n�A1�A1S�A0�`A0Q�A/t�A.ĜA-��A,��A+�A+��A)�-A(~�A'&�A&~�A%A%O�A$��A$�A#�A#&�A"ĜA"M�A!��A!K�A!A ZA 1A�A�PA�A��A5?AƨA��A�A7LA%A�yA�!Ar�AZA5?AJA�A�AS�A�HAI�Ap�A��A��An�AQ�AA�hA�`A��AbNA��A|�A&�A�!A$�AA|�A�A�DA$�A��Ap�A�A�+A�A��AC�A�A�A�yA��AM�AA|�A"�A��A��A-A�hA�A
�yA
��A
��A
=qA	�A�/A��Ar�A��A7LAv�A��A��A\)A��A~�AI�A��A��A"�A��A��Az�AZA-A�
A;dA �A �9A �uA Q�@��
@�\)@���@�M�@��^@�`B@���@�(�@���@��
@��w@�"�@���@�x�@�z�@�b@��@�@�~�@�n�@�M�@���@� �@�dZ@���@�ȴ@�@��m@�~�@�/@�\@�G�@���@�l�@���@��y@�V@ߍP@�x�@�C�@��#@׾w@ՙ�@���@�Z@��@Ұ!@��@Л�@�1'@�K�@�E�@ͩ�@̣�@�1@˅@�"�@�^5@��@�j@��y@�5?@�%@�;d@�v�@��D@���@��@�@��`@��D@�ƨ@�
=@�$�@���@� �@�t�@�X@�ƨ@�o@�@���@�@�Z@�\)@�=q@�-@��7@���@��@���Aڝ�Aڡ�Aڥ�Aڥ�Aڥ�Aڥ�Aڥ�Aڡ�Aڡ�Aڡ�Aڣ�Aڧ�Aڧ�Aڧ�Aڗ�Aڝ�Aڝ�Aڟ�Aڡ�Aڟ�Aڟ�Aڣ�Aڥ�Aڣ�Aڡ�Aڡ�Aڟ�Aڛ�Aڛ�Aڝ�Aڝ�Aڟ�Aڥ�Aک�Aک�Aک�Aک�Aک�Aک�Aک�AڮAڶFAڶFAڸRAں^AڸRAں^AڶFAڴ9AڶFAڸRAڶFAڴ9AڶFAڶFAڸRAں^A���AھwA���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                        ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	m�B	l�B	k�B	k�B	k�B	k�B	jB	k�B	k�B	k�B	k�B	k�B	k�B	k�B	k�B	l�B	l�B	l�B	l�B	l�B	m�B	p�B	q�B	r�B	q�B	s�B	s�B	r�B	r�B	r�B	r�B	r�B	s�B	s�B	s�B	r�B	r�B	r�B	q�B	l�B	ZB	hsB	��B	��B	�B
B
�B
N�B
s�B
��B�B"�B
�ZB
�?BhB;dBP�B�%Bz�BcTBt�B�B�VB~�B^5BG�B"�B
�B
�FB
r�B
K�B
,B

=B	�HB	��B	^5B	@�B	D�B	D�B	6FB	49B	�B	1B��B�fB�B�B�sB��BɺB�sB�B�B�yB�B��B	hB	1'B	C�B	\)B	t�B	�7B	�uB	��B	��B	�qB	�/B	�TB	�B	�B
B
�B
�B
#�B
-B
7LB
<jB
@�B
J�B
L�B
H�B
L�B
O�B
O�B
I�B
K�B
M�B
M�B
O�B
O�B
N�B
N�B
M�B
N�B
O�B
O�B
P�B
P�B
Q�B
R�B
S�B
R�B
S�B
S�B
T�B
W
B
XB
YB
YB
ZB
ZB
YB
YB
XB
YB
YB
XB
YB
XB
XB
W
B
W
B
T�B
R�B
R�B
R�B
VB
T�B
T�B
VB
VB
T�B
R�B
R�B
R�B
P�B
N�B
N�B
L�B
K�B
K�B
L�B
L�B
L�B
L�B
J�B
K�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
I�B
H�B
H�B
G�B
F�B
D�B
C�B
B�B
B�B
B�B
A�B
@�B
>wB
>wB
=qB
<jB
;dB
:^B
9XB
8RB
7LB
6FB
6FB
7LB
8RB
6FB
7LB
8RB
9XB
9XB
9XB
8RB
7LB
7LB
6FB
5?B
5?B
49B
49B
33B
2-B
1'B
/B
/B
.B
-B
.B
.B
.B
.B
0!B
0!B
.B
.B
-B
,B
+B
,B
+B
)�B
'�B
%�B
$�B
$�B
!�B
!�B
!�B
!�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
{B
�B
{B
{B
{B
uB
oB
uB
uB
{B
{B
�B
{B
�B
�B
�B
{B
oB
oB
\B
VB
\B
\B
PB
PB
DB
PB
\B
PB
PB
VB
hB
\B
\B
VB
\B
\B
bB
�B
�B
�B
�B
�B
�B
�B	l�B	l�B	l�B	jB	l�B	k�B	k�B	l�B	m�B	l�B	l�B	l�B	l�B	jB	l�B	l�B	l�B	l�B	l�B	k�B	k�B	m�B	jB	jB	l�B	k�B	jB	l�B	l�B	l�B	l�B	l�B	l�B	k�B	k�B	k�B	k�B	k�B	k�B	l�B	l�B	jB	k�B	k�B	jB	k�B	jB	jB	k�B	k�B	jB	k�B	k�B	k�B	jB	k�B	k�B	jB	k�B	k�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                        B	mkB	leB	k_B	k`B	k`B	k`B	j[B	kaB	kaB	kaB	kbB	kbB	kcB	kdB	kdB	lkB	lkB	llB	lmB	lmB	mtB	p�B	q�B	r�B	q�B	s�B	s�B	r�B	r�B	r�B	r�B	r�B	s�B	s�B	s�B	r�B	r�B	r�B	q�B	lyB	ZB	haB	��B	��B	��B
B
�B
N�B
s�B
��B�B"�B
�LB
�1BZB;WBP�B�Bz�BcHBt�B��B�KB~�B^+BG�B"�B
�B
�=B
r�B
K�B
, B

5B	�@B	��B	^-B	@{B	D�B	D�B	6?B	42B	�B	+B��B�`B�B�B�nB��BɶB�oB�B�B�vB�B��B	fB	1&B	C�B	\)B	t�B	�8B	�vB	��B	��B	�tB	�2B	�XB	�B	�B
B
�B
�B
#�B
-B
7TB
<sB
@�B
J�B
L�B
H�B
L�B
O�B
O�B
I�B
K�B
M�B
M�B
O�B
O�B
N�B
N�B
M�B
N�B
O�B
O�B
P�B
P�B
R B
SB
TB
SB
TB
TB
UB
W"B
X)B
Y0B
Y1B
Z8B
Z8B
Y3B
Y3B
X-B
Y5B
Y5B
X/B
Y6B
X0B
X0B
W+B
W+B
U B
SB
SB
SB
V(B
U#B
U#B
V*B
V*B
U%B
SB
SB
SB
QB
OB
OB
L�B
K�B
K�B
L�B
L�B
L�B
L�B
J�B
K�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
I�B
H�B
H�B
G�B
F�B
D�B
C�B
B�B
B�B
B�B
A�B
@�B
>�B
>�B
=�B
<�B
;�B
:�B
9�B
8�B
7�B
6�B
6�B
7�B
8�B
6�B
7�B
8�B
9�B
9�B
9�B
8�B
7�B
7�B
6�B
5�B
5�B
4�B
4�B
3}B
2xB
1rB
/gB
/hB
.aB
-\B
.bB
.cB
.dB
.dB
0rB
0rB
.fB
.fB
-aB
,[B
+VB
,]B
+WB
*RB
(FB
&:B
%4B
%5B
"$B
"%B
"'B
"(B
B
B
B
B
B
 B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
B

B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
�B
 B
�B
B
B
B
8B
4B
5B
=B
8B
:B
5B	leB	leB	leB	jYB	leB	k_B	k_B	leB	mkB	leB	leB	leB	leB	jYB	leB	leB	leB	leB	leB	k_B	k_B	mkB	jYB	jYB	leB	k_B	jYB	leB	leB	leB	leB	leB	leB	k_B	k_B	k_B	k`B	k`B	k`B	lfB	lfB	jZB	k`B	k`B	jZB	k`B	jZB	j[B	kaB	kaB	j[B	kaB	kaB	kaB	j[B	kbB	kbB	j\B	kbB	kbG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                        <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202562021061413571620210614135716202107221611152021072216111520210722161115201807242202562021061413571620210614135716202107221611152021072216111520210722161115PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422025620180724220256  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422025620180724220256QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422025620180724220256QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216141720210722161417IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                