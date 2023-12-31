CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-12-19T13:00:44Z creation      
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
resolution        =���   axis      Z        X  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   L,   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  PD   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  d�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  u   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �d   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  �|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  �D   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  ´   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  �$   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �$   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �$   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    $   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � $Argo profile    3.1 1.2 19500101000000  20181219130044  20210617131512  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               5   5DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @ؗP6�@ؗP6�11  @ؗ-�� @ؗ-�� @6z��zN{@6z��zN{�c��^��c��^�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?�  @   @Fff@�ff@�ff@�  @ٙ�@���AffA#33AC33Aa��A���A���A���A�33A���A���A�33A�33A�33B  B  B  B   B(ffB0  B8  B@  BG��BO��BW��B_��Bh  Bo��Bx  B��B�33B�ffB�ffB�33B�33B�33B�33B�33B�  B�33B�33B�33B�ffB�33B�  B�  B�  B���B�ffB�  B�33Bؙ�B�ffB�ffB�33B�33B�33B�33B�ffB�  B���C   CL�C33C�CL�C
�C  C  C�fCL�CL�C�fC�fC  C�fC�fC   C!�fC$  C&L�C(L�C*33C,�C.�C0L�C2�C4  C633C8L�C:�C;��C=�fC?�fCA�fCC�fCE�fCH�CJ33CL�CN  CP�CR33CS��CV  CW�fCZ�C\33C^  C`  Ca�fCc��Cf�ChL�CjL�Cl33Cn33CpL�CrffCt  Cu��Cw��Cy�fC|33C~L�C��C�  C��C��C��3C��C�&fC��C��C��C�  C�  C��fC��C�  C�  C��3C��3C��C��C��C�&fC��fC��3C��3C��3C�  C��3C��C��C�  C�&fC��C��C�  C��3C�  C��C��C��3C�  C��C��C��C��fC��3C�  C��C�  C��C��C��C��C��C��C�  C��3C��3C��3C�  C�  C�  C�  C�  C��3C��3C��fC��C�&fC�&fC��C��C�  C�  C��C��C��C��C��C��C��C�&fC��C�&fC�&fC�&fC��C�ٚC��3C��3C��C��C�&fC��C��3C��3C��C��C��C�&fC��C��fC��3C��C�&fC��C�  C��C��C��3C��C��C��C��3C�  C�&fC�  C�ٚC��3C�  C��C��C�  C��3C��3C��C��D  D� D	33D��DS3D�3DS3D��DFfD��DS3D� D"` D$��D'S3D)�3D,  D.�3D0��D3` D5ٚD8L�D:�fD=  D?` DA� DD�DF��DH�3DK` DM��DP@ DR�fDU�DW�3DZ3D\�3D_3Da��Dd3Df�fDi�Dk��Dm��Dpl�Dr�3Du,�Dwy�Dy�fD{��D~fD�&fD�C3D�VfD�i�D�� D���D���D�� D���D��3D��3D�3D��D�,�D�<�D�L�D�` D�y�D�� D��fD�� D��fD���D�3D�3D�)�D�<�D�VfD�l�D�y�D���D��fD��fD��fD��fD�� D�� D��fD�  D�3D��D�)�D�)�D�0 D�<�D�I�D�S3D�` D�i�D�vfD��3D���D���D�� D�� D���D�� D���D��fD�� D�vfD�s3D�l�D�i�D�i�D�ffD�ffD�p D�s3D�|�DɆfDʓ3Dˠ D̬�D��3D��3D��3D���D�fD�6fD�S3D�s3D֓3D׳3D���D�� D� D�6fD�S3D�s3Dߓ3D�3D���D���D�	�D��D�9�D�P D�i�D�vfD� D� D�3D��3D��3D��fD���D��D�  D�0 D�<�D�C3D�FfD�FfD�C3D�L�D�6fD�33D�<�D�9�D�6fE �E � E3E�E��E�3EfE3E	� E
~fE�3E��E~fE~fE��E�fEnfEd�E�3E)�E��EffE� E�EA�EffE��E!3E"+3E#��E$��E&)�E'C3E(k3E)�fE+,�E,a�E-�3E.� E0  E1p E2� E3� E4��E66fE7{3E8�3E9�3E;4�E<nfE?�3EB�3EF	�EH�fEK�fEO�ER.fEU� EX� E[��E^�3Eb�Ee)�Eh)�Ek6fEn~fEo	�Eo� Epk3Ep�fEq�3Er�Er�fEsffEs�Et�fEuI�Eu��Ev�fEw�Ew� Ex` Ey3Ey��EzVfEz�3E{�fE|3E|� E}q�E}� >���>L��>L��>���=���>���>L��>L��>L��>���>���>L��>���>���>���?   >���>���>���>���?   ?   ?   ?   ?��?333?333?fff?���?���?���?�  ?���?�ff@ff@��@   @&ff@9��@S33@`  @s33@�ff@�  @���@���@�ff@�ff@�33@�  @�33A��A	��A��A33A#33A,��A4��A@  AFffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441414441441411444114441141114111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ?L��?�  @   @fff@�ff@�ff@�  @陚AffAffA+33AK33Ai��A���A���A���A�33A���A���A�33A�33B��B
  B  B  B"  B*ffB2  B:  BB  BI��BQ��BY��Ba��Bj  Bq��Bz  B���B�33B�ffB�ffB�33B�33B�33B�33B�33B�  B�33B�33B�33B�ffB�33B�  B�  B�  B���B�ffB�  B�33Bٙ�B�ffB�ffB�33B�33B�33B�33B�ffB�  B���C � C��C�3C��C��C
��C� C� CffC��C��CffCffC� CffCffC � C"ffC$� C&��C(��C*�3C,��C.��C0��C2��C4� C6�3C8��C:��C<L�C>ffC@ffCBffCDffCFffCH��CJ�3CL��CN� CP��CR�3CTL�CV� CXffCZ��C\�3C^� C`� CbffCdL�Cf��Ch��Cj��Cl�3Cn�3Cp��Cr�fCt� CvL�CxL�CzffC|�3C~��C�Y�C�@ C�Y�C�L�C�33C�L�C�ffC�Y�C�L�C�L�C�@ C�@ C�&fC�L�C�@ C�@ C�33C�33C�Y�C�Y�C�Y�C�ffC�&fC�33C�33C�33C�@ C�33C�Y�C�L�C�@ C�ffC�Y�C�L�C�@ C�33C�@ C�Y�C�L�C�33C�@ C�L�C�Y�C�L�C�&fC�33C�@ C�L�C�@ C�L�C�L�C�L�C�L�C�L�C�L�C�@ C�33C�33C�33C�@ C�@ C�@ C�@ C�@ C�33C�33C�&fC�L�C�ffC�ffC�Y�C�L�C�@ C�@ C�L�C�L�C�L�C�L�C�L�C�L�C�Y�C�ffC�Y�C�ffC�ffC�ffC�L�C��C�33C�33C�L�C�Y�C�ffC�L�C�33C�33C�L�C�L�C�Y�C�ffC�L�C�&fC�33C�Y�C�ffC�Y�C�@ C�L�C�L�C�33C�L�C�Y�C�L�C�33C�@ C�ffC�@ C��C�33C�@ C�L�C�Y�C�@ C�33C�33C�L�C�Y�D@ D� D	S3D��Ds3D�3Ds3D��DffD��Ds3D   D"� D%�D's3D)�3D,@ D.�3D1�D3� D5��D8l�D:�fD=  D?� DA� DD9�DF��DI3DK� DM��DP` DR�fDU,�DW�3DZ33D\�3D_33Da��Dd33Df�fDi9�Dk��Dn�Dp��Dr�3DuL�Dw��Dy�fD{ٚD~&fD�6fD�S3D�ffD�y�D�� D���D���D�� D���D��3D�3D�3D�)�D�<�D�L�D�\�D�p D���D�� D��fD�� D��fD���D�3D�#3D�9�D�L�D�ffD�|�D���D���D��fD��fD��fD��fD�� D�� D�fD� D�#3D�)�D�9�D�9�D�@ D�L�D�Y�D�c3D�p D�y�D��fD��3D���D���D�� D�� D���D�� D���D��fD�� D��fD��3D�|�D�y�D�y�D�vfD�vfDƀ Dǃ3DȌ�DɖfDʣ3D˰ D̼�D��3D��3D��3D�	�D�&fD�FfD�c3DՃ3D֣3D��3D���D�  D�  D�FfD�c3Dރ3Dߣ3D��3D���D���D��D�,�D�I�D�` D�y�D�fD� D� D��3D��3D��3D��fD��D��D�0 D�@ D�L�D�S3D�VfD�VfD�S3D�\�D�FfD�C3D�L�D�I�D�FfE !�E � E#3E!�E��E�3EfE3E	� E
�fE3E�E�fE�fE�E�fEvfEl�E�3E1�E��EnfE� E	�EI�EnfE��E!3E"33E#��E$��E&1�E'K3E(s3E)�fE+4�E,i�E-�3E.� E0( E1x E2� E3� E4��E6>fE7�3E8�3E:3E;<�E<vfE?�3EB�3EF�EH�fELfEO$�ER6fEU� EX� E[��E^�3Eb	�Ee1�Eh1�Ek>fEn�fEo�Eo� Eps3Ep�fEq�3Er�Er�fEsnfEs��Et�fEuQ�Eu��Ev�fEw!�Ew� Exh Ey#3Ey��Ez^fEz�3E{�fE|3E|� E}y�E}� G�O�G�O�?333G�O�?��G�O�G�O�G�O�?333G�O�G�O�?333G�O�?L��?fffG�O�G�O�G�O�?L��?fffG�O�G�O�G�O�?�  ?���G�O�?���?�33?���G�O�?ٙ�@   @ff@33@&ff@,��@@  @Fff@Y��@s33@�  @���@�ff@�  @���@���@�ff@�ff@�33@�  A��A	��A��A��A#33A+33A4��A<��AH  ANffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441414441441411444114441141114111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 @ @ �@ �@ *@ �@ #�@ (�@ .l@ 5�@ <�@ E�@ SI@ `B@ m�@ |?@ �7@ ��@ ��@ �-@ �w@ �@ ��@ �@ ��@j@@g@,`@:@G�@T�@bN@o�@}�@��@��@�A@�9@�>@є@�;@�4@��@�@*@"�@/�@>@K�@Yn@g�@t�@��@�\@�@��@��@��@�O@�T@�L@��@
�@�@&;@3�@B8@N�@Z�@i�@y�@�|@�u@�z@��@��@�c@�[@�@�e@�Q@�@O@(G@5�@DD@Q=@_�@oF@|�@��@��@��@��@��@��@��@�(@�q@@b@
@+�@9X@F�@V@dZ@qS@~K@��@��@��@��@��@��@�;@�@�,@%@@"�@1�@?}@Lu@Z@hs@v�@��@��@��@��@��@�@��@�H@�L@�E@	�@�@'�@4�@A�@O0@\)@i�@v@��@��@�m@�f@�@��@�h@�@�e@��@�@�@(G@6�@C�@SI@`B@m:@|�@��@��@��@��@�&@�*@�#@�m@��@	@	o@	g@	+@	9X@	G�@	V@	b�@	qS@	~�@	��@	�H@	��@	��@	@	ψ@	�/@	��@	�,@
�@
{@
""@
/�@
<�@
Ji@
Wb@
g@
v@
��@
��@
��@
��@
�R@
ƨ@
�O@
��@
�@
�E@
�@B@'�@4�@B�@P�@^5@j@uk@�p@�@�@�r@��@�@�[@�@�@ �@@[@)�@5?@C�@SI@a�@n�@z�@�7@��@�(@�-@��@�|@��@�@��@j@@
@,`@:�@I@UU@bN@o�@~�@�P@�@\)@�y@�y@.l@r�@��@��@>@�@�@�@Q�@��@�@�@\)@�@��@""@e�@��@��@(�@i�@��@��@-�@oF@�~@�@6�@x&@��@��@B�@�+@�o@b@S�@�H@�/@!s@bN@�5@�@&�@e�@��@��@�@V�@�u@�*@�@DD@�W@�@��@/@j@��@ލ@�@T�@��@ȴ@j@?}@z�@�F@�Y@ -�@ i!@ ��@ ލ@!�@!T�@!��@!�@"v@"?}@"x�@"��@"��@#&�@#_�@#��@#��@$�@$H]@$�W@$�^@$��@%(�@%bN@%��@%�O@&�@&FQ@&�@&�@&�@'(G@'_�@'�0@'�@(j@(8�@(n�@(��@(�h@)V@)C�@)y�@)�!@)�@*�@*UU@*��@*�J@*��@+7L@+p�@+��@+�`@,g@,Yn@,��@,є@-@-K�@-�7@-ƨ@.@.@,@.~K@.��@.��@/7L@/t�@/�-@/�@0+�@0i!@0��@0��@1[@1X�@1��@1�*@2
=@2DD@2~�@2�@2�@3-�@3i!@3�(@3��@4�@4Q=@4�7@4��@4� @5-@5e�@5��@5�|@6%@6<@6r@6��@6��@7�@7�@8�i@8��@9��@:1@:��@;6@;��@<'�@<�@=9X@=܀@>F�@>�l@?P�@?�y@@~�@A@As_@Bj@B�@C�@C��@D<@D�F@E1'@E��@FFQ@F�`@G]�@G��@H[z@I�@I��@J@J��@K%�@K��@LDD@L��@M1�@M�w@NI@Nє@OZ@O��@Pe�@Q��@So@T~�@U�9@W	�@X^5@Y�f@[%�@\t@]�-@_1@`m�@aƨ@cV@d[z@e��@e�E@f:@f�#@fψ@g�@gFQ@g��@g��@h*@hl�@h��@h��@i5�@im�@i@i�,@jI@j~�@jψ@k�@kT�@k��@k�
@l"�@lUUG�O�G�O�@ ^G�O�@  �G�O�G�O�G�O�@ ^G�O�G�O�@ ^G�O�@ @ �G�O�G�O�G�O�@ @ �G�O�G�O�G�O�@ j@ G�O�@ �@ %@ �G�O�@ 1@ 
=@ 
�@ J@ V@ @ @ �@ �@ �@ �@ �@ �@ �@ !s@ $�@ &�@ *S@ -@ /�@ 3�@ 7L@ :�@ >@ B8@ E�@ I�@ M$@ Q�@ T�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�7LA�VA�XA�I�A�C�A�E�A�XA�O�A�Q�A�O�A�M�A�O�A�VA�S�A�ZA�^5A�ZA�Q�A�O�A�M�A�M�A�M�A�O�A�O�A�Q�A�Q�A�Q�A�S�A�S�A�S�A�S�A�Q�A�Q�A�S�A�Q�A�I�A�C�A�v�A���A�{A�t�A��A�hsA�{A���A�r�A��A�A��RA�v�A���A�O�A�"�A���A�$�A�\)A�bNA��A��9A���A��7A��
A��A�E�A�"�A�A��A���A�=qA���A���A�E�A��/A�x�A�ƨA�Q�A��#A���A�l�A�`BA�oA�A�S�A��A��9A�33A�K�A�M�A�VA��#A�\)A��A��A���A�hsA�1A�x�A��;A�\)A��A���A���A�1A�M�A��/A���A��#A�XA���A�1A�O�A�ĜA���A�M�A��HA��A��A�Q�At�A~�A{|�Av�Ar1'An-Am;dAkVAh^5Ag�;Af��Ae��Ae&�Ad��Ad�RAd��Ad�A`�A_S�A^ �A\~�A[S�AZ�AX��AV5?AS
=AR�!ARffAR(�AQ�AQ%AOdZAN$�AL�/ALA�AKdZAIAG��AEK�AC�hABz�AAG�A?�A?XA>v�A<��A<VA< �A:ĜA:$�A9&�A7�mA6�uA6^5A6M�A6  A5XA3�
A2�jA1�A0=qA/A/��A/�7A.M�A-��A-S�A,��A+VA(��A'��A'+A%�FA#%A!A!�A $�A�A��AA�9A�AA��AA��A��A�;A�AffA��A�^A�A|�A��AhsA9XAr�A�A`BA
�HA
ffA��A�wA`BAr�AJA~�A��A��AO�A ��@�V@�Ĝ@�n�@�Z@���@�R@�G�@���@��@���@�"�@��`@�M�@��m@��y@��#@�p�@��@�A�@��
@߅@�"�@ޟ�@�J@���@�ƨ@�;d@�7L@�l�@���@�-@ύP@Ǿw@�x�@�%@�p�@��@�
=@��@���@���@�^5@��y@��#@� �@�ȴ@���@��@���@���@�S�@��-@��@�l�@�@��/@�@��j@�z�@��@��@���@�9X@��@��@�1@�Z@�Q�@�"�@�v�@�X@�A�@�dZ@��+@���@�b@�t�@�@��-@���@�1'@~��@~$�@}�-@}/@|z�@z�@w\)@vv�@t��@r~�@q7L@ol�@m��@k�@j�@fv�@e`B@d�D@c��@a�^@`bN@_�P@]�T@\��@[�
@Z-@W�@V�+@U�-@U?}@S�F@R�H@QG�@P�9@OK�@N{@M/@Lz�@K��@Jn�@I�#@I%@G��@D�@C��@B�\@@Ĝ@?l�@>E�@=`B@;dZ@9��@9��@8  @6�R@5/@49X@3C�@2�!@1�7@01'@/;d@.v�@-?}@+��@+"�@)7L@(r�@'��@'�w@&ȴ@&@%��@%V@#�F@"�@"n�@!�7@!%@   @�w@�@
=@{@�h@/@Z@�H@��@x�@%@�@v�@�@I�@ƨ@C�@��@�\@J@x�@�`@A�@b@�P@5?@��@p�@��@33@
^5@
=q@	��@	G�@��@b@��@|�@�@��@��@�@�/@33@�\@X@ �?��?�{?��?���?�ff?��?�F?�J?�R?���?�1?�^?���?�?�+?��?�n�?�  ?���?�(�?�"�?���?׮?�`B?�z�?ҏ\?�J?�%?�\)?θR?�5??�/?��m?���?��#?�X?���?�1'?Ǯ?�ȴ?�E�?��T?��?��/?�33?°!?�-?�%?��;?��?���?��?�(�?���?��H?��^?�X?�X?���?��u?��u?�r�?��9?��9?���?���?���?���?��?��?��?�7L?��?�X?�X?�x�?���?���?��^?��^?���?���?�=q?��?�^5A�5?A�/A�(�A�5?A�1'A�33A�/A�1'A�/A�/A�1'A�1'A�=qA�-A�E�A�I�A�;dA�7LA�5?A�5?A�=qA�=qA�=qA�A�A�G�A�K�A�VA�^5A�^5A�ZA�ZA�XA�XA�ZA�ZA�VA�XA�Q�A�K�A�C�A�A�A�G�A�C�A�?}A�=qA�M�A�VA�XA�O�A�M�A�M�A�S�A�O�A�O�A�M�A�M�A�O�A�O�A�O�A�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 A�7LA�VA�XA�I�A�C�A�E�A�XA�O�A�Q�A�O�A�M�A�O�A�VA�S�A�ZA�^5A�ZA�Q�A�O�A�M�A�M�A�M�A�O�A�O�A�Q�A�Q�A�Q�A�S�A�S�A�S�A�S�A�Q�A�Q�A�S�A�Q�A�I�A�C�A�v�A���A�{A�t�A��A�hsA�{A���A�r�A��A�A��RA�v�A���A�O�A�"�A���A�$�A�\)A�bNA��A��9A���A��7A��
A��A�E�A�"�A�A��A���A�=qA���A���A�E�A��/A�x�A�ƨA�Q�A��#A���A�l�A�`BA�oA�A�S�A��A��9A�33A�K�A�M�A�VA��#A�\)A��A��A���A�hsA�1A�x�A��;A�\)A��A���A���A�1A�M�A��/A���A��#A�XA���A�1A�O�A�ĜA���A�M�A��HA��A��A�Q�At�A~�A{|�Av�Ar1'An-Am;dAkVAh^5Ag�;Af��Ae��Ae&�Ad��Ad�RAd��Ad�A`�A_S�A^ �A\~�A[S�AZ�AX��AV5?AS
=AR�!ARffAR(�AQ�AQ%AOdZAN$�AL�/ALA�AKdZAIAG��AEK�AC�hABz�AAG�A?�A?XA>v�A<��A<VA< �A:ĜA:$�A9&�A7�mA6�uA6^5A6M�A6  A5XA3�
A2�jA1�A0=qA/A/��A/�7A.M�A-��A-S�A,��A+VA(��A'��A'+A%�FA#%A!A!�A $�A�A��AA�9A�AA��AA��A��A�;A�AffA��A�^A�A|�A��AhsA9XAr�A�A`BA
�HA
ffA��A�wA`BAr�AJA~�A��A��AO�A ��@�V@�Ĝ@�n�@�Z@���@�R@�G�@���@��@���@�"�@��`@�M�@��m@��y@��#@�p�@��@�A�@��
@߅@�"�@ޟ�@�J@���@�ƨ@�;d@�7L@�l�@���@�-@ύP@Ǿw@�x�@�%@�p�@��@�
=@��@���@���@�^5@��y@��#@� �@�ȴ@���@��@���@���@�S�@��-@��@�l�@�@��/@�@��j@�z�@��@��@���@�9X@��@��@�1@�Z@�Q�@�"�@�v�@�X@�A�@�dZ@��+@���@�b@�t�@�@��-@���@�1'@~��@~$�@}�-@}/@|z�@z�@w\)@vv�@t��@r~�@q7L@ol�@m��@k�@j�@fv�@e`B@d�D@c��@a�^@`bN@_�P@]�T@\��@[�
@Z-@W�@V�+@U�-@U?}@S�F@R�H@QG�@P�9@OK�@N{@M/@Lz�@K��@Jn�@I�#@I%@G��@D�@C��@B�\@@Ĝ@?l�@>E�@=`B@;dZ@9��@9��@8  @6�R@5/@49X@3C�@2�!@1�7@01'@/;d@.v�@-?}@+��@+"�@)7L@(r�@'��@'�w@&ȴ@&@%��@%V@#�F@"�@"n�@!�7@!%@   @�w@�@
=@{@�h@/@Z@�H@��@x�@%@�@v�@�@I�@ƨ@C�@��@�\@J@x�@�`@A�@b@�P@5?@��@p�@��@33@
^5@
=q@	��@	G�@��@b@��@|�@�@��@��@�@�/@33@�\@X@ �?��?�{?��?���?�ff?��?�F?�J?�R?���?�1?�^?���?�?�+?��?�n�?�  ?���?�(�?�"�?���?׮?�`B?�z�?ҏ\?�J?�%?�\)?θR?�5??�/?��m?���?��#?�X?���?�1'?Ǯ?�ȴ?�E�?��T?��?��/?�33?°!?�-?�%?��;?��?���?��?�(�?���?��H?��^?�X?�X?���?��u?��u?�r�?��9?��9?���?���?���?���?��?��?��?�7L?��?�X?�X?�x�?���?���?��^?��^?���?���?�=q?��?�^5A�5?A�/A�(�A�5?A�1'A�33A�/A�1'A�/A�/A�1'A�1'A�=qA�-A�E�A�I�A�;dA�7LA�5?A�5?A�=qA�=qA�=qA�A�A�G�A�K�A�VA�^5A�^5A�ZA�ZA�XA�XA�ZA�ZA�VA�XA�Q�A�K�A�C�A�A�A�G�A�C�A�?}A�=qA�M�A�VA�XA�O�A�M�A�M�A�S�A�O�A�O�A�M�A�M�A�O�A�O�A�O�A�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BF�BD�BD�BF�BG�BH�BE�BG�BF�BF�BG�BG�BE�BF�BH�BffBx�B�B�B�+B�+B�1B�=B�=B�DB�DB�DB�JB�DB�JB�JB�JB�JB�JB�JB�JB�JB}�By�Bt�Bv�Bm�Bk�Bt�Bw�Bu�Br�Bw�Bz�Bx�Bw�By�Bx�By�Bx�By�By�Bz�Bw�Bu�Bu�Bu�Bu�Bo�Bq�Bp�Bo�Bo�Bn�BjBiyBgmBaHB[#BVBR�BQ�BO�BN�BM�BI�BF�BC�B>wB9XB#�B�BDB��B��B�B�fB�B��B��BȴB�}B�LB�!B�B��B�{Bz�Bl�BS�B6FB+B �B�B  B
�yB
�B
��B
B
�B
�B
l�B
`BB
VB
M�B
5?B
�B	��B	�TB	�)B	ɺB	��B	�^B	�'B	�B	��B	��B	��B	��B	��B	�DB	�B	z�B	o�B	hsB	cTB	S�B	:^B	49B	0!B	-B	+B	-B	�B	�B	{B	uB	VB	1B��B��B�B�fB�HB�B��B��BɺBȴBƨBÖB�wB�qB�FB�3B�!B�!B�B�B�B��B��B��B��B��B��B��B�hB�bB�VB�=B�Bx�Bx�Bt�BiyBdZBcTB`BB]/BbNB`BB\)B[#B[#BXBR�BS�BQ�BK�BI�BH�BF�BF�BE�BD�BB�B?}B<jB7LB:^B8RB7LB7LB5?B33B49B2-B.B.B+B,B.B/B,B/B.B,B+B,B,B.B,B+B,B.B.B0!B6FB49B7LB7LB7LB8RB9XB9XB:^B;dB;dB<jB>wB@�BD�BH�BJ�BXB_;BW
B\)BiyB�+B�bB�{B��B�B�RBɺB��B��B	B		7B	VB	VB	�B	�B	6FB	D�B	L�B	K�B	N�B	YB	n�B	{�B	�%B	�JB	��B	��B	��B	��B	�9B	�jB	��B	��B	ȴB	��B	��B	�#B	�;B	�ZB	�fB	�yB	�B	�B	��B	��B	��B	��B	��B	��B	��B
  B
B
%B
1B

=B
JB
PB
\B
hB
{B
�B
�B
�B
�B
�B
�B
"�B
#�B
$�B
&�B
(�B
+B
.B
/B
0!B
1'B
2-B
33B
49B
49B
6FB
7LB
6FB
8RB
8RB
9XB
:^B
:^B
;dB
<jB
<jB
=qB
?}B
@�B
A�B
B�B
E�B
F�B
E�B
G�B
H�B
I�B
K�B
K�B
L�B
M�B
O�B
P�B
O�B
Q�B
S�B
R�B
T�B
VB
VB
VB
YB
YB
YB
ZB
[#B
\)B
\)B
]/B
]/B
_;B
_;B
_;B
aHB
aHB
bNB
bNB
cTB
e`B
ffB
e`B
gmB
gmB
iyB
jB
k�B
k�B
l�B
l�B
l�B
m�B
n�B
n�B
o�B
o�B
p�B
r�B
r�B
r�B
s�B
t�B
u�B
u�B
v�B
v�B
v�B
w�B
x�B
x�B
x�B
x�B
z�B
{�B
z�B
}�B
}�B
� B
� B
�B
�B
�B
�B
�+B
�1B
�7B
�=B
�JB
�JB
�PB
�\B
�\B
�bB
�hB
�hB
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
�B
�B
�B
�B
�B
�!B
�!B
�'B
�-B
�3B
�9B
�?B
�FB
�LB
�LB
�LB
�XB
�XB
�XB
�RB
�XB
�XB
�XB
�XB
�XB
�XB
�RB
�XB
�XB
�^B
�XB
�XB
�XB
�XB
�XB
�XB
�^B
�XB
�XB
�XB
�^B
�XBE�BE�BF�BE�BF�BE�BF�BF�BF�BG�BE�BF�BG�BF�BL�BC�BF�BE�BE�BF�BH�BE�BE�BF�BF�BD�BE�BC�BC�BC�BD�BC�BD�BD�BC�BD�BD�BE�BE�BG�BH�BG�BG�BH�BI�BF�BE�BD�BF�BG�BG�BE�BF�BF�BG�BG�BG�BH�BG�BG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 BFBDsBDsBFBG�BH�BEzBG�BF�BF�BG�BG�BE}BF�BH�BfCBx�B��B��B�
B�B�B�B�B�&B�'B�'B�.B�)B�/B�0B�0B�1B�2B�2B�3B�3B}�By�Bt�Bv�Bm|BkqBt�Bw�Bu�Br�Bw�Bz�Bx�Bw�By�Bx�By�Bx�By�By�Bz�Bw�Bu�Bu�Bu�Bu�Bo�Bq�Bp�Bo�Bo�Bn�BjyBisBghBaCB[BV BR�BQ�BO�BN�BM�BI�BF�BC�B>xB9ZB#�B�BGB��B��B�B�kB�B��B��BȻB��B�UB�*B�B��B��Bz�Bl�BTB6RB+B �B�B B
�B
�+B
��B
B
�$B
�B
l�B
`SB
VB
M�B
5QB
�B	��B	�gB	�<B	��B	��B	�rB	�<B	�#B	�B	��B	��B	��B	��B	�]B	�+B	z�B	o�B	h�B	coB	TB	:zB	4UB	0>B	-+B	+ B	-,B	�B	�B	�B	�B	wB	RB�B��B�B�B�kB�AB�"B�B��B��B��BýB��B��B�nB�\B�JB�KB�EB�?B�-B�	B��B��B��B��B��B��B��B��B��B�nB�JByByBt�Bi�Bd�Bc�B`vB]dBb�B`xB\_B[ZB[ZBXHBS+BT1BR%BLBI�BH�BF�BF�BE�BD�BB�B?�B<�B7�B:�B8�B7�B7�B5�B3uB4{B2pB.WB.XB+FB,MB.YB/aB,NB/bB.[B,PB+JB,PB,QB.]B,RB+MB,SB._B.`B0mB6�B4�B7�B7�B7�B8�B9�B9�B:�B;�B;�B<�B>�B@�BD�BI	BKBXkB_�BWkB\�Bi�B��B��B��B�UB��B��B�4B�VB�hB	�B		�B	�B	�B	B	B	6�B	E3B	MgB	LdB	OxB	Y�B	o=B	|�B	��B	��B	�DB	�yB	��B	��B	��B	�)B	�KB	�NB	�|B	ΞB	��B	��B	�B	�1B	�@B	�VB	�jB	�sB	��B	��B	��B	��B	��B	��B	��B
 �B
B
!B
	0B
>B
NB
VB
eB
sB
�B
�B
�B
�B
�B
�B
 �B
#�B
$�B
&B
(B
* B
,/B
/CB
0MB
1VB
2^B
3gB
4oB
5xB
5zB
7�B
8�B
7�B
9�B
9�B
:�B
;�B
;�B
<�B
=�B
=�B
>�B
@�B
A�B
B�B
C�B
GB
HB
GB
I!B
J)B
K2B
MAB
MCB
NLB
OTB
QbB
RkB
QgB
SvB
U�B
T�B
V�B
W�B
W�B
W�B
Z�B
Z�B
Z�B
[�B
\�B
]�B
]�B
^�B
^�B
`�B
`�B
`�B
cB
cB
dB
dB
eB
g)B
h1B
g.B
i>B
i@B
kOB
lXB
m`B
mcB
nlB
nnB
nqB
oyB
p�B
p�B
q�B
q�B
r�B
t�B
t�B
t�B
u�B
v�B
w�B
w�B
x�B
x�B
x�B
y�B
z�B
z�B
z�B
z�B
|�B
~B
}B
�B
�"B
�5B
�:B
�MB
�XB
�fB
�qB
��B
��B
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
�B
�(B
�4B
�?B
�GB
�YB
�dB
�qB
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
�	B
�B
�B
�(B
�4B
�:B
�FB
�\B
�qB
��B
��B
��B
��B
��B
��B
�B
�B
�+B
�BB
�XB
�fB
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
��BEyBEyBFBEyBFBEyBFBFBFBG�BEyBFBG�BFBL�BCmBFBEyBEyBFBH�BEyBEyBFBFBDsBEyBCmBCmBCmBDsBCmBDsBDsBCmBDsBDsBEyBEyBG�BH�BG�BG�BH�BI�BF�BEzBDtBF�BG�BG�BE{BF�BF�BG�BG�BG�BH�BG�BG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201812191300442021061413555920210614135559202106171313502021061713135020210617131350201812191300442021061413555920210614135559202106171313502021061713135020210617131350PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018121913004420181219130044  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018121913004420181219130044QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018121913004420181219130044QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713151220210617131512IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                