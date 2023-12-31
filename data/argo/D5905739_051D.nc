CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-12-10T10:00:47Z creation      
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
resolution        =���     �  O|   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  _4   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  c$   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  r�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �4   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   \   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   d   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                      HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                       HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        @   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    P   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �Argo profile    3.1 1.2 19500101000000  20181210100047  20210617131511  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               3   3DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @ؔ��G�l@ؔ��G�l11  @ؔ��l3@ؔ��l3@6�m3	A�@6�m3	A��c��/���c��/��11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?���@ff@@  @�  @�33@�  @���A��A33A$��A@  Aa��A�  A�  A�33A�  A�33A�33A�  A�  B ffB��BffBffB   B'��B0  B8��B@  BH  BO��BX  Ba33Bh��BpffBx  B�  B���B���B���B���B���B���B���B���B�33B�ffB�33B�33B�33B�  B�  B�  B�  B�  B�33B�33B�ffB�33B�33B�  B�ffB�33B�  B�33B�ffB�ffB�  B���C�fC  C�CL�C
�C�fC�C33C  C��C�CL�C33C  C�fC��C"  C$�C&  C'��C*�C,�C-�fC0L�C233C4�C6  C8  C9�fC;�3C=�fC@L�CB33CD�CF  CG�fCJ�CL�CN  CP33CR  CS�fCVL�CX�CZ  C\L�C^L�C`33Cb33Cd�Cf�Ch�Cj  Ck�fCn33Cp�Cq�fCt33Cv�Cx  CzL�C|L�C~  C�&fC�&fC��C�&fC��C��3C��C��C�&fC��C�ٚC��fC��fC��3C��3C�  C��fC��3C��3C��3C��3C��fC��fC�ٚC��fC��C�33C��C��fC�  C�&fC��C��C��C��C��C�  C�  C��3C��3C��3C�  C�  C�  C��C��C��C�&fC��C��fC��fC��3C��C��C�  C��fC��3C�  C�&fC�  C��fC�  C��C�&fC��C��3C�  C��C�&fC��C��3C��C�&fC��C��3C��C��3C��fC��C�  C��fC��C�&fC��C�  C�&fC�&fC�  C��C��C��3C�  C��C�  C��fC�  C��C��C��3C��C�  C��fC��C�&fC��C�  C�&fC��C��3C��C�&fC��C��3C�  C��C��C��C��C��C��C��C��C��fC��C�  C�  C��fC��fC�ٚD l�D3D��DٚD@ D��D
�3DffDٚDFfD�3D` D� Ds3D3D!��D$S3D&��D)��D,FfD.��D1� D4@ D6�3D9ffD;� D>S3D@� DC` DE�fDH,�DJ�fDL��DOl�DQ� DTFfDV�fDY  D[S3D]�fD`fDbY�Dd�3Df��DiL�Dk� Dm�3DpFfDr�3Du3Dw�fDy��D{��D~S3D�c3D���D���D���D�33D�ffD��fD�� D�fD�I�D�y�D�� D��3D�fD�,�D�\�D���D���D���D�3D�<�D�Y�D�|�D�� D��fD��fD�fD�0 D�S3D�y�D���D�� D��3D�	�D�33D�c3D��3D��3D�� D�� D��D�9�D�` D�� D��fD��D�fD�C3D�p D�� D��3D�fD�6fD�` D���D���D���D�33D�l�Dţ3D��3D�fD�0 D�` DˆfD̩�D���D�� D� D�0 D�L�D�c3D�s3DՓ3D֩�D׼�D�ٚD��3D�fD�33D�VfD�|�Dߜ�D�� D���D�  D��D�9�D�S3D�l�D�3D�3D� D�� D��3D�� D���D�  D� D� D��D�#3D�&fD�33D�9�D�@ D�FfD�S3D�FfD�P D�S3D�VfD�VfE ,�E �3E.fE�fE,�E��E( E�fE!�EfE�fE�fE� E
h Ea�E� E�fET�ES3E�3E<�E1�E� E��E�3EH E��E�fEɚEfEa�E � E!�fE#�E$I�E%� E'fE(@ E){3E*�3E,fE-L�E.�fE/ٚE1fE2T�E3�3E4�3E5��E71�E8nfE9��E:�3E<d�E?x EB� EE��EH� EK��EN� ER EUfEXp E[s3E^�fEa��Ed�fEg�fEk�En0 EqH Etd�Ewl�Ez��>���>���>L��>L��>���>L��>L��>���>L��>���>L��>���>���>���?   >���>���>���>���>L��>���>L��>���>���>���>���>���>���>���?   ?333?L��?�  ?���?�33?�  ?�33@ff@��@,��@@  @S33@l��@�  @���@�ff@�33@�  @���@�ff@�ff@�33@�33A��A  AffAffA��A$��A+33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444144141414114141414144441411111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                             ?L��?���@&ff@`  @�  @�33@�  @���A	��A33A,��AH  Ai��A�  A�  A�33A�  A�33A�33A�  A�  BffB
��BffBffB"  B)��B2  B:��BB  BJ  BQ��BZ  Bc33Bj��BrffBz  B�  B���B���B���B���B���B���B���B���B�33B�ffB�33B�33B�33B�  B�  B�  B�  B�  B�33B�33B�ffB�33B�33B�  B�ffB�33B�  B�33B�ffB�ffB�  C L�CffC� C��C��C
��CffC��C�3C� CL�C��C��C�3C� CffC L�C"� C$��C&� C(L�C*��C,��C.ffC0��C2�3C4��C6� C8� C:ffC<33C>ffC@��CB�3CD��CF� CHffCJ��CL��CN� CP�3CR� CTffCV��CX��CZ� C\��C^��C`�3Cb�3Cd��Cf��Ch��Cj� ClffCn�3Cp��CrffCt�3Cv��Cx� Cz��C|��C~� C�ffC�ffC�L�C�ffC�Y�C�33C�L�C�Y�C�ffC�L�C��C�&fC�&fC�33C�33C�@ C�&fC�33C�33C�33C�33C�&fC�&fC��C�&fC�L�C�s3C�L�C�&fC�@ C�ffC�Y�C�Y�C�Y�C�L�C�L�C�@ C�@ C�33C�33C�33C�@ C�@ C�@ C�L�C�Y�C�Y�C�ffC�L�C�&fC�&fC�33C�L�C�Y�C�@ C�&fC�33C�@ C�ffC�@ C�&fC�@ C�L�C�ffC�L�C�33C�@ C�L�C�ffC�L�C�33C�L�C�ffC�L�C�33C�L�C�33C�&fC�L�C�@ C�&fC�L�C�ffC�L�C�@ C�ffC�ffC�@ C�Y�C�L�C�33C�@ C�Y�C�@ C�&fC�@ C�Y�C�L�C�33C�L�C�@ C�&fC�L�C�ffC�L�C�@ C�ffC�L�C�33C�L�C�ffC�L�C�33C�@ C�L�C�L�C�Y�C�Y�C�Y�C�Y�C�Y�C�L�C�&fC�L�C�@ C�@ C�&fC�&fD �D ��D33D��D��D` D��D3D�fD��DffD�3D� D  D�3D33D!��D$s3D'�D)��D,ffD/�D1� D4` D6�3D9�fD<  D>s3DA  DC� DE�fDHL�DJ�fDM�DO��DR  DTffDV�fDY  D[s3D]�fD`&fDby�Dd�3Dg�Dil�Dk� Dn3DpffDr�3Du33Dw�fDz�D|�D~s3D�s3D���D���D��D�C3D�vfD��fD�� D�&fD�Y�D���D�� D��3D�fD�<�D�l�D���D���D���D�#3D�L�D�i�D���D�� D��fD��fD�fD�@ D�c3D���D���D�� D��3D��D�C3D�s3D��3D��3D�� D�  D�)�D�I�D�p D�� D��fD���D�&fD�S3D�� D�� D��3D�fD�FfD�p D���D���D�	�D�C3D�|�Dų3D��3D�fD�@ D�p D˖fD̹�D���D�  D�  D�@ D�\�D�s3Dԃ3Dգ3Dֹ�D���D��D�3D�&fD�C3D�ffDތ�D߬�D�� D���D� D�)�D�I�D�c3D�|�D�3D�3D�� D�� D��3D�� D���D� D�  D�  D�)�D�33D�6fD�C3D�I�D�P D�VfD�c3D�VfD�` D�c3D�ffD�ffE 4�E �3E6fE�fE4�E��E0 E�fE)�E&fE�fEfE� E
p Ei�E� E�fE\�E[3E�3ED�E9�E� E�E�3EP E��E�fEњEfEi�E � E!�fE#�E$Q�E%� E'fE(H E)�3E*�3E,fE-T�E.�fE/�E1fE2\�E3�3E4�3E6�E79�E8vfE9��E:�3E<l�E?� EB� EE��EH� EK��EO  ER EUfEXx E[{3E^�fEa��Ed�fEg�fEk!�En8 EqP Etl�Ewt�Ez��G�O�G�O�G�O�?333G�O�G�O�?333G�O�?333G�O�?333G�O�?L��?fffG�O�?L��G�O�?L��G�O�?333G�O�?333G�O�G�O�G�O�G�O�?L��G�O�?fff?�  ?���?�ff?�  ?ٙ�?�33@   @��@&ff@9��@L��@`  @s33@�ff@�  @���@�ff@�33@�  @ə�@�ff@�ff@�33A��A	��A  AffAffA$��A,��A333G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444144141414114141414144441411111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                             @ @ �@ V@ {@ O@ "�@ (�@ /@ 7L@ >�@ FQ@ Q�@ `B@ m:@ z�@ ��@ �0@ �(@ ��@ �&@ ��@ �#@ �y@ �q@@@
@,`@;d@G�@UU@bN@p�@�W@�P@�H@�A@��@��@ψ@�/@��@��@%@@!s@0x@>�@K�@Yn@g@t@��@�\@�@��@�@ƨ@��@��@�@��@�@�@%�@3�@B8@O�@\)@hs@v�@�@�u@�z@��@�@�@�h@�@��@ �@�@�@(�@5�@B�@Q�@`B@m:@y�@�7@��@�(@��@��@�|@�t@�@�@^@b@ �@-�@:�@G�@T�@c�@qS@~K@�P@��@��@��@�>@�7@��@�@��@1@*@"�@0x@=q@Ji@Z@g@s_@�@�@�@��@�^@��@խ@�T@�@��@�@6@&;@4�@B�@O0@Z@hs@v@�p@�@�m@��@�@ȴ@�[@�@��@��@�@�@)�@9X@D�@P�@_�@oF@|?@��@��@��@�-@�&@��@��@�m@�@	j@	@	�@	-@	;d@	I@	Wb@	c�@	oF@	|�@	�D@	�H@	��@	��@	�2@	ψ@	��@	�@	�,@
v@
{@
"�@
1�@
>@
Ji@
X�@
g@
v@
�d@
��@
��@
��@
�@
�J@
�O@
��@
�@
�E@
=@�@&;@5?@A�@N�@^5@k�@ww@�|@�u@��@�@�@�c@խ@�@�@ �@�@�@(�@5?@D�@S�@`B@m:@|�@�7@��@��@��@��@�@�t@��@�q@�@o@ @-�@;d@H]@S�@c�@p�@~K@��@�<@�5@��@Ĝ@є@��@Q�@��@�C@*@X@�H@��@%�@i�@�!@��@>�@�+@ψ@6@`A@��@�Y@:@�W@ƨ@
=@M$@��@�
@�@Z@�H@�/@ @b�@��@�`@%�@e	@��@�`@$�@a�@��@�@""@a�@�@�T@$.@g@��@�;@ �@c�@��@�m@(G@j@�@�L@33@uk@��@��@6�@x&@��@�~@9X@z3@�@��@ :�@ z3@ ��@ �@!33@!r@!�r@!��@",`@"j@"�M@"�@#%�@#c�@#�z@#��@$"�@$`A@$��@$��@%O@%Z�@%�<@%�
@&�@&V�@&�<@&�h@'�@'X�@'��@'�#@(�@(]�@(�@(��@)!s@)a�@)��@)�m@*)�@*j@*�@*�@+,`@+k.@+�M@+�m@,%�@,b�@,�m@,�/@-�@-R�@-�@-�o@.%@.B�@.~�@.�@.��@/7�@/v�@/�9@/�Y@0/@0m:@0�M@0�@1"�@1^�@1�H@1�O@2@2K@2��@2�&@2�~@333@3m:@3��@3܀@4*@4Lu@4��@4��@4��@5-�@5g@5��@5Ӡ@6
�@6B8@6x�@6�!@6�@7	@7T�@7��@7��@7��@8+�@8`B@8�@9i!@:%@:m:@;�@;x&@<�@<�p@='�@=�#@>4�@>�C@?:�@?Ӡ@@k�@@є@Aff@A��@B��@B�`@Cs_@D �@D��@Eb@E��@F�@F�c@GJi@G�c@HO�@H�h@Ie�@I��@J}�@K�@K��@L�@L�a@M#�@M��@N)�@N��@O5@@O�F@Pa�@Q�~@Sj@TD�@U�@V�@XN�@Y�@Z��@\X�@]��@^�@`> @a�(@b�@dO1@e�m@f�Y@hFQ@i�i@j�eG�O�G�O�G�O�@ ^G�O�G�O�@ ^G�O�@ ^G�O�@ ^G�O�@ @ �G�O�@ G�O�@ G�O�@ ^G�O�@ ^G�O�G�O�G�O�G�O�@ G�O�@ �@ j@ �@ v@ �@ 1@ 	�@ 
=@ �@ V@ b@ o@ {@ �@ B@ O@ [@  @ "�@ %�@ '�@ *S@ -�@ 0x@ 3�@ 7L@ :@ <�@ @,@ B�@ FQ@ IG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aɰ!AɶFA�A�A�ȴA�ƨA���A���A�ƨA�ȴA���A���A���A���A���A���A���A��A��A��A��#A��A��#A��#A��/A��/A��/A��;A��;A��;A��HA��;A��HA��HA��HA��HA��;A��;A��/A��#A�ȴA�\)A�~�A��Aĥ�A�~�A���A��RA��\A��+A��A�v�A���A��+A�;dA�ȴA�t�A�`BA�O�A�JA���A��A�S�A�^5A��A�ĜA��\A��/A���A���A�bNA�{A�-A��\A�^5A���A��A�|�A�?}A�
=A��mA��
A���A�VA��A�bA�oA�(�A�~�A�/A���A��A���A�?}A�bA�ZA���A�
=A�5?A��#A�n�A�JA�&�A�E�A��A��7A�G�A�ZA�ĜA�x�A�%A��+A�^5A�VA�hsA�(�A�ffA��jA�  A���A�A�A���A��yA~Q�Ay\)At�HAr1Ao��An�Am�Ak7LAh�Af~�AeoAd{A`��A]��A[��AY��AY��AY%AU�mAS�^ASoAR��AQ|�APVAO&�AL��AL�AK��AKt�AKXAK?}AJ�AJ{AI��AI�AI�AI�;AI�#AIƨAHffAF1AE%AD1'ABȴAA�;A@ȴA?�A<�!A;��A:ĜA8�uA7&�A45?A2�9A1ƨA1�A0~�A.��A,JA*��A(��A&��A&�DA%��A%��A%S�A$�A#VA"JA"A!�A!|�A �/A �uA �A z�A�
A?}A��A�AĜA1A7LA1'A/A�;A33A��A1'AƨAl�A�A��A��A9XA��A?}A��A�AȴAdZA
1'A	oAz�AbNA�A�A��An�A  A\)AS�AA�A`BAA ��@�\)@�?}@�^5@�|�@�O�@�1@��;@�P@�+@�E�@�dZ@��@���@���@���@�ff@�7@�@��
@�dZ@�S�@◍@�-@߅@�V@�{@ܣ�@��/@+@��R@���@�b@�5?@�@��@�r�@�~�@�I�@�@���@��@��@�;d@���@���@�^5@��@��@��@��R@�~�@�5?@�p�@�l�@�&�@� �@��H@�G�@��D@���@���@��-@�/@��@�\)@���@���@�Z@��@��^@��m@�$�@��@�hs@�@��@~�y@|(�@{�F@{ƨ@{�F@x��@xQ�@u��@t�/@r��@p��@o�@nV@l��@l�@i�^@f��@f@e�@ct�@`�`@_��@^��@]�h@\1@[ƨ@Z=q@X�9@W�@T�@Q�#@P�9@M@L�/@J~�@H��@H�u@G�@FV@EO�@D1@C33@B^5@AG�@@  @?\)@>E�@=�@;�m@:�\@9x�@8bN@6�R@5O�@4Z@49X@3dZ@2�!@1�@1�@/l�@-�-@+��@*��@)hs@(�`@(�u@&��@%��@%�@$1@#ƨ@#�F@"n�@ �@  �@K�@��@�@?}@9X@o@�@��@�!@�@G�@%@r�@��@��@�-@�@ƨ@�!@n�@��@��@�@�u@|�@��@5?@O�@j@�F@
n�@
M�@	7L@Ĝ@�@ �@�P@�y@ff@5?@�T@��@��@(�@�
@@��@��@ ��?��R?��?��m?�^5?��?��+?�?��?��`??�{?�ƨ?���?�+?�E�?���?���?��?��?�O�?�j?�"�?��#?��?�l�?�$�?Լj?ԛ�?�Z?�S�?���?�G�?Ͼw?Η�?�/?��?�C�?�~�?��?ȓu?�1'?�l�?�ȴ?�?��?���?�&�?� �?��R?�5??��-?�V?�"�?�?��?���?�Q�?��9?�r�?��9?��?�7L?��^Aɲ-Aɴ9Aɰ!Aɰ!AɬAɮAɩ�Aɩ�AɬAɮAɮAɬAɩ�AɬAɮAɮAɮAɰ!AɮAɲ-Aɰ!Aɲ-Aɲ-Aɴ9Aɴ9Aɲ-Aɰ!Aɰ!Aɲ-Aɲ-Aɲ-Aɲ-Aɰ!Aɴ9A���AɾwA�A�A���A�A���A�A�ƨA�ȴA���A�ƨA�ĜA���A���A�ȴA���A���A�ȴA�ƨA�ƨA�ƨA�ȴA���A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                             Aɰ!AɶFA�A�A�ȴA�ƨA���A���A�ƨA�ȴA���A���A���A���A���A���A���A��A��A��A��#A��A��#A��#A��/A��/A��/A��;A��;A��;A��HA��;A��HA��HA��HA��HA��;A��;A��/A��#A�ȴA�\)A�~�A��Aĥ�A�~�A���A��RA��\A��+A��A�v�A���A��+A�;dA�ȴA�t�A�`BA�O�A�JA���A��A�S�A�^5A��A�ĜA��\A��/A���A���A�bNA�{A�-A��\A�^5A���A��A�|�A�?}A�
=A��mA��
A���A�VA��A�bA�oA�(�A�~�A�/A���A��A���A�?}A�bA�ZA���A�
=A�5?A��#A�n�A�JA�&�A�E�A��A��7A�G�A�ZA�ĜA�x�A�%A��+A�^5A�VA�hsA�(�A�ffA��jA�  A���A�A�A���A��yA~Q�Ay\)At�HAr1Ao��An�Am�Ak7LAh�Af~�AeoAd{A`��A]��A[��AY��AY��AY%AU�mAS�^ASoAR��AQ|�APVAO&�AL��AL�AK��AKt�AKXAK?}AJ�AJ{AI��AI�AI�AI�;AI�#AIƨAHffAF1AE%AD1'ABȴAA�;A@ȴA?�A<�!A;��A:ĜA8�uA7&�A45?A2�9A1ƨA1�A0~�A.��A,JA*��A(��A&��A&�DA%��A%��A%S�A$�A#VA"JA"A!�A!|�A �/A �uA �A z�A�
A?}A��A�AĜA1A7LA1'A/A�;A33A��A1'AƨAl�A�A��A��A9XA��A?}A��A�AȴAdZA
1'A	oAz�AbNA�A�A��An�A  A\)AS�AA�A`BAA ��@�\)@�?}@�^5@�|�@�O�@�1@��;@�P@�+@�E�@�dZ@��@���@���@���@�ff@�7@�@��
@�dZ@�S�@◍@�-@߅@�V@�{@ܣ�@��/@+@��R@���@�b@�5?@�@��@�r�@�~�@�I�@�@���@��@��@�;d@���@���@�^5@��@��@��@��R@�~�@�5?@�p�@�l�@�&�@� �@��H@�G�@��D@���@���@��-@�/@��@�\)@���@���@�Z@��@��^@��m@�$�@��@�hs@�@��@~�y@|(�@{�F@{ƨ@{�F@x��@xQ�@u��@t�/@r��@p��@o�@nV@l��@l�@i�^@f��@f@e�@ct�@`�`@_��@^��@]�h@\1@[ƨ@Z=q@X�9@W�@T�@Q�#@P�9@M@L�/@J~�@H��@H�u@G�@FV@EO�@D1@C33@B^5@AG�@@  @?\)@>E�@=�@;�m@:�\@9x�@8bN@6�R@5O�@4Z@49X@3dZ@2�!@1�@1�@/l�@-�-@+��@*��@)hs@(�`@(�u@&��@%��@%�@$1@#ƨ@#�F@"n�@ �@  �@K�@��@�@?}@9X@o@�@��@�!@�@G�@%@r�@��@��@�-@�@ƨ@�!@n�@��@��@�@�u@|�@��@5?@O�@j@�F@
n�@
M�@	7L@Ĝ@�@ �@�P@�y@ff@5?@�T@��@��@(�@�
@@��@��@ ��?��R?��?��m?�^5?��?��+?�?��?��`??�{?�ƨ?���?�+?�E�?���?���?��?��?�O�?�j?�"�?��#?��?�l�?�$�?Լj?ԛ�?�Z?�S�?���?�G�?Ͼw?Η�?�/?��?�C�?�~�?��?ȓu?�1'?�l�?�ȴ?�?��?���?�&�?� �?��R?�5??��-?�V?�"�?�?��?���?�Q�?��9?�r�?��9?��?�7L?��^Aɲ-Aɴ9Aɰ!Aɰ!AɬAɮAɩ�Aɩ�AɬAɮAɮAɬAɩ�AɬAɮAɮAɮAɰ!AɮAɲ-Aɰ!Aɲ-Aɲ-Aɴ9Aɴ9Aɲ-Aɰ!Aɰ!Aɲ-Aɲ-Aɲ-Aɲ-Aɰ!Aɴ9A���AɾwA�A�A���A�A���A�A�ƨA�ȴA���A�ƨA�ĜA���A���A�ȴA���A���A�ȴA�ƨA�ƨA�ƨA�ȴA���A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                             ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B~�B~�B}�B~�B� B~�B� B� B~�B� B� B�B� B� B� B� B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B}�By�Bt�Bk�BaHBw�By�Br�Bn�B{�By�By�Bz�By�Bx�Bz�By�Bz�B{�B|�B|�B�B�1B�%B�+B�B�B� B}�B}�B|�Bz�B�B�Bw�Bn�Bn�Bm�Bl�BjBiyBe`BcTBaHB\)BL�B6FB1'B+B!�B�B�BPB��B�B�sB�/B��B��BĜB�jB�B�VBz�BaHBA�B5?B&�BhBJB  B
�)B
��B
��B
�uB
�%B
y�B
m�B
gmB
aHB
T�B
F�B
+B
B	�mB	��B	��B	�FB	��B	��B	�%B	s�B	hsB	aHB	B�B	5?B	%�B	�B	�B	�B	  B�B�B�B�B�yB�/B��B��B��B��B��B�/B�B��B��B��B	  B	  B	  B��B��B��B��B��B�B�B�fB�)B��B��B��BŢB�XB�B��B��B��B��B�uB�hB�7B~�B� B|�By�Bw�Bv�Bs�Bn�Bn�Bn�Bl�BjBhsBhsBhsBgmBe`B^5B_;B]/B[#BYBXBVBR�BQ�BP�BO�BN�BN�BM�BK�BI�BH�BG�BE�BE�BC�BA�BA�B=qB>wB;dB<jB;dB9XB9XB9XB7LB6FB49B5?B6FB6FB6FB49B5?B49B5?B7LB6FB9XB7LB7LB7LB6FB:^BA�BJ�BZB_;BW
BW
BS�BT�B]/Bo�Bw�B~�By�B� B�JB�7B�BjBp�B|�B�B��B��B�9B�9B�wB��B�5B�B��B	hB	�B	"�B	-B	49B	=qB	VB	gmB	jB	l�B	�B	�JB	��B	��B	�'B	�LB	ƨB	ȴB	��B	��B	�B	�B	�/B	�BB	�TB	�mB	�yB	�B	�B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
	7B
	7B

=B
DB
JB
VB
\B
bB
oB
{B
�B
�B
�B
�B
�B
�B
 �B
!�B
#�B
$�B
$�B
&�B
(�B
)�B
,B
/B
/B
33B
2-B
6FB
7LB
7LB
9XB
9XB
;dB
<jB
=qB
>wB
>wB
@�B
@�B
A�B
C�B
D�B
E�B
F�B
G�B
I�B
J�B
J�B
K�B
L�B
M�B
N�B
N�B
P�B
P�B
S�B
T�B
VB
W
B
W
B
XB
YB
ZB
[#B
[#B
[#B
]/B
_;B
`BB
`BB
aHB
`BB
bNB
cTB
dZB
cTB
e`B
cTB
dZB
ffB
gmB
gmB
hsB
hsB
iyB
jB
k�B
m�B
m�B
n�B
l�B
n�B
n�B
p�B
q�B
p�B
r�B
s�B
t�B
u�B
u�B
v�B
w�B
w�B
x�B
y�B
y�B
z�B
z�B
z�B
{�B
{�B
|�B
|�B
}�B
}�B
� B
�B
�B
�B
�B
�B
�+B
�+B
�7B
�=B
�=B
�DB
�JB
�PB
�\B
�bB
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
�B
�B
�B
�B
�'B
�'B
�-B
�3B
�?B
�?B
�FB
�LB
�RB
�RB
�RB
�XB
�XB
�XB
�^B~�B~�B|�B� B|�B~�B~�B~�B~�B}�B}�B~�B~�B~�B}�B~�B}�B}�B}�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B}�B~�B}�B~�B~�B~�B� B~�B� B~�B~�B~�B~�B� B~�B� B~�B~�B~�B~�B� B� B�B� G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                             B~�B~�B}�B~�B�B~�B�B�B~�B�B�B��B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B}�By�Bt�BksBa6Bw�By�Br�Bn�B{�By�By�Bz�By�Bx�Bz�By�Bz�B{�B|�B|�B�B�(B�B�#B�B��B�B}�B}�B|�Bz�B�B�Bw�Bn�Bn�Bm�Bl�BjBizBeaBcVBaJB\,BL�B6JB1+B+B!�B�B�BWB��B�B�{B�8B��B��BĦB�uB�B�bBz�BaTBA�B5LB&�BvBYB B
�8B
��B
��B
��B
�6B
y�B
m�B
gB
a[B
UB
F�B
+B
3B	�B	��B	��B	�\B	�B	��B	�<B	s�B	h�B	a`B	B�B	5XB	%�B	�B	�B	�B	 B��B�B�B�B�B�MB�B��B�B��B�B�PB�B�	B�B�B	 $B	 %B	 %B� B� B�B�B��B��B�B�B�RB�"B��B��B��B��B�@B�B��B��B��B��B��B�fB)B�0B}BzBx Bv�Bs�Bn�Bn�Bn�Bl�Bj�Bh�Bh�Bh�Bg�Be�B^mB_tB]hB[]BYQBXKBV?BS.BR(BQ"BPBOBOBNBLBI�BH�BG�BE�BE�BC�BA�BA�B=�B>�B;�B<�B;�B9�B9�B9�B7�B6�B4�B5�B6�B6�B6�B4�B5�B4�B5�B7�B6�B9�B7�B7�B7�B6�B:�BA�BKBZpB_�BW^BW^BTMBUSB]�Bo�Bx&BRBz3B�YB��B��B�wBj�BqB}SB��B�B�PB��B��B��B�LBޱB�B�iB	�B	5B	#]B	-�B	4�B	>B	V�B	hB	kB	m,B	��B	��B	�bB	��B	��B	��B	�^B	�mB	͈B	ӰB	��B	��B	��B	�B	� B	�<B	�JB	�lB	�B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B

0B

3B
<B
EB
NB
]B
fB
oB
B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
%B
&B
&B
(B
*,B
+5B
-DB
0YB
0\B
4wB
3sB
7�B
8�B
8�B
:�B
:�B
<�B
=�B
>�B
?�B
?�B
A�B
A�B
B�B
EB
FB
GB
HB
I&B
K5B
L?B
LBB
MKB
NTB
O]B
PfB
PhB
RwB
RzB
U�B
V�B
W�B
X�B
X�B
Y�B
Z�B
[�B
\�B
\�B
\�B
^�B
`�B
a�B
b B
cB
bB
dB
eB
f%B
e"B
g1B
e'B
f0B
h?B
iIB
iKB
jTB
jWB
k_B
lhB
mpB
oB
o�B
p�B
n�B
p�B
p�B
r�B
s�B
r�B
t�B
u�B
v�B
w�B
w�B
x�B
y�B
y�B
z�B
{�B
{�B
}B
}B
}B
~B
~B
B
B
�$B
�'B
�8B
�EB
�QB
�\B
�jB
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
�B
�B
�B
�%B
�,B
�>B
�DB
�VB
�\B
�iB
�wB
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
�B
�B
� B
�-B
�4B
�OB
�dB
��B
��B
��B
��B
��B
��B
� B
�B
�*B
�@B
�VB
�kB
�{B
��B
��B
��B
��B
��B~�B~�B|�B�B|�B~�B~�B~�B~�B}�B}�B~�B~�B~�B}�B~�B}�B}�B}�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B}�B~�B}�B~�B~�B~�B�B~�B�B~�B~�B~�B~�B�B~�B�B~�B~�B~�B~�B�B�B��B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                             <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201812101000472021061413555720210614135557202106171313452021061713134520210617131345201812101000472021061413555720210614135557202106171313452021061713134520210617131345PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018121010004720181210100047  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018121010004720181210100047QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018121010004720181210100047QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713151120210617131511IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                