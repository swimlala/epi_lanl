CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-08-18T07:03:12Z creation      
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
_FillValue                 4  L�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  P�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  a�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  e�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  v�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  �h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  �d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  �$   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   (   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    L   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        t   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       |   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �      � �Argo profile    3.1 1.2 19500101000000  20180818070312  20210722160151  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�x�)e@�x�)e11  @�x����@�x����@6^�*�)t@6^�*�)t�c�dE#��c�dE#�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                              	   	AB  AA  AA  ?333@   @Fff@�  @�  @�  @�  A   A33A&ffAC33Aa��A���A�  A�33A�33A���Aљ�A�  A�33B   B  BffBffB ��B(ffB/��B8  B@  BHffBPffBW��B`  Bh  Bp  Bx��B�ffB�33B�  B���B�33B�33B�33B�  B�33B�ffB�33B���B�33B�33B���B���B�  B���B���B�ffB�  B�33B�  Bۙ�B�33B�ffB���B�ffB�  B�33B�  B���C   C�C33C�C��C
  CL�C�C�C  C�fCL�C33C�C�C  C �C"�C$33C%��C'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC:  C;�fC>  C@  CBL�CD33CF�CH  CI��CL  CN33CPL�CR  CS��CU��CW�fCZ  C\�C^33C`L�Cb�Cd�Cf33Cg�fCj  Cl�Cn33CpL�Cr33Ct  Cv�CxL�Cz�C|  C~33C��C��3C�  C��C��C��fC��3C��C��C��fC��C�&fC��C�  C�&fC��C�  C��C��C�  C��3C��3C��C��C��3C��C��C��C�  C��3C��C��C��C�  C��3C��C�  C��fC��C��3C��fC��3C��C�  C��fC�  C��C��C�&fC��C��3C�  C��C��C��C��C�&fC��C��fC��fC��3C�  C�  C�  C��C��C��C�  C��C�  C�  C�  C�  C�  C�  C��C��C�  C��C��C��C�&fC�&fC�&fC��C��fC��fC��3C�  C��C��C��C��C�&fC��C��fC��fC��3C�  C�  C��C��C�&fC�  C�ٚC��fC��fC��C��C�  C��3C��C��C��fC�  C��C�  C�  C��C��C��3C��C��D�3D�fDffD9�D�D�3Dy�D  D��D3D�3D �D"��D%�D'�3D*S3D-fD/�fD2�fD5Y�D8&fD:�3D=ٚD@�3DC�fDFS3DI  DK��DNffDP� DSFfDU�3DXfDZl�D\�3D^��Da,�Dcs3De�fDh3Djs3DlٚDoY�DqٚDt` Dv�fDyy�D{��D~9�D�s3D�ɚD��D�s3D���D�#3D�y�D��3D�#3D�vfD��fD� D�\�D��fD�� D�0 D�y�D���D�3D�@ D��3D���D��3D�0 D�i�D�� D�ɚD���D�&fD�L�D�vfD���D���D�� D�  D��D�33D�I�D�i�D���D���D���D��fD��fD�3D�6fD�` D���D���D�� D��D�L�D���D�ɚD�fD�FfDĉ�D���D�fD�c3Dɳ3D���D�@ D͆fD��3D�3D�FfD҉�D���D�  D�6fD�p DئfD���D� D�33D�c3DޖfD�� D��fD� D�33D�Y�D�y�D晚D�fD�� D���D�fD�fD�&fD�9�D�P D�i�D�3D��D�3D��fD���D��3D�3D�fD�,�D�  D�)�D�33D�@ D�I�E +3E �3E9�E�3EA�E�fEL�E�fE\�E�fEl�E��E~fE�E�E	,�E
��E��E0 E$�E��E	�E�Eh E�3E� E)�EfE� E� E�fEK3E� E �E � E"S3E#��E$��E%�fE'VfE(�3E)��E+#3E,fE-��E/fE03E1��E2��E4 E4��E6` E7��E8�3E9�fE;FfE<��E?�fEB��EE�3EI�EL0 EOS3ER��EU[3EX��E[�3E_�Ea�Ee@ Eh�EkK3EnQ�Eq��Et��Ew�fE{#3E}��E�� E�,�E�͚E�Y�E�ٚE�x E���E���E�fE���E�8�E�t�E��3E�)�E�e�E��fE��E�T E��fE�3E�C3E���E��fE�G3E��fE�՚E�(�E�}�E��fE�$�E�w3E�ɚE� E�Q�E���E��fE�E�E�� E���E�;3E���E��E�!�E�t�E�� E��E�h ?��?   ?��?��?��?   ?��?   ?   ?��?   ?   ?   ?��?��?L��?L��?�  ?���?���?�33?ٙ�?�ff@   @��@��@333@9��@L��@`  @l��@�  @���@�33@�  @���@�ff@�  @���@ٙ�@�ff@�33A   A  AffAffAffA&ffA.ffA6ffA<��AH  ANffAT��A\��Ad��Al��At��A~ffA�ffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414441441444141411411111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ?���@   @fff@�  @�  @�  @�  A  A33A.ffAK33Ai��A���A�  A�33A�33A���Aՙ�A�  A�33B  B
  BffBffB"��B*ffB1��B:  BB  BJffBRffBY��Bb  Bj  Br  Bz��B�ffB�33B�  B���B�33B�33B�33B�  B�33B�ffB�33B���B�33B�33B���B���B�  B���B���B�ffB�  B�33B�  Bܙ�B�33B�ffB���B�ffB�  B�33B�  B���C � C��C�3C��CL�C
� C��C��C��C� CffC��C�3C��C��C� C ��C"��C$�3C&L�C(ffC*ffC,ffC.ffC0ffC2ffC4ffC6ffC8ffC:� C<ffC>� C@� CB��CD�3CF��CH� CJL�CL� CN�3CP��CR� CTL�CVL�CXffCZ� C\��C^�3C`��Cb��Cd��Cf�3ChffCj� Cl��Cn�3Cp��Cr�3Ct� Cv��Cx��Cz��C|� C~�3C�L�C�33C�@ C�Y�C�L�C�&fC�33C�Y�C�L�C�&fC�L�C�ffC�L�C�@ C�ffC�Y�C�@ C�Y�C�L�C�@ C�33C�33C�L�C�L�C�33C�Y�C�Y�C�L�C�@ C�33C�Y�C�Y�C�Y�C�@ C�33C�L�C�@ C�&fC�L�C�33C�&fC�33C�Y�C�@ C�&fC�@ C�L�C�Y�C�ffC�Y�C�33C�@ C�L�C�L�C�Y�C�Y�C�ffC�Y�C�&fC�&fC�33C�@ C�@ C�@ C�L�C�L�C�L�C�@ C�L�C�@ C�@ C�@ C�@ C�@ C�@ C�L�C�L�C�@ C�L�C�Y�C�Y�C�ffC�ffC�ffC�L�C�&fC�&fC�33C�@ C�L�C�Y�C�Y�C�Y�C�ffC�L�C�&fC�&fC�33C�@ C�@ C�L�C�Y�C�ffC�@ C��C�&fC�&fC�L�C�Y�C�@ C�33C�Y�C�L�C�&fC�@ C�Y�C�@ C�@ C�Y�C�L�C�33C�L�C�L�D�3D�fD�fDY�D,�D�3D��D  D��D33D�3D 9�D"��D%9�D'�3D*s3D-&fD/�fD2�fD5y�D8FfD;3D=��D@�3DC�fDFs3DI@ DK��DN�fDQ  DSffDU�3DX&fDZ��D\�3D_�DaL�Dc�3De�fDh33Dj�3Dl��Doy�Dq��Dt� DwfDy��D{��D~Y�D��3D�ٚD�,�D��3D���D�33D���D��3D�33D��fD��fD�  D�l�D��fD�  D�@ D���D���D�3D�P D��3D���D�3D�@ D�y�D�� D�ٚD��D�6fD�\�D��fD���D�ɚD�� D� D�,�D�C3D�Y�D�y�D���D���D���D��fD�fD�#3D�FfD�p D���D���D�� D�)�D�\�D���D�ٚD�fD�VfDę�D���D�&fD�s3D��3D�	�D�P D͖fD��3D�3D�VfDҙ�D���D� D�FfD׀ DضfD���D�  D�C3D�s3DަfD�� D��fD�  D�C3D�i�D剚D橚D��fD�� D���D�fD�&fD�6fD�I�D�` D�y�D�3D��D��3D��fD���D�3D�3D�&fD�<�D�0 D�9�D�C3D�P D�Y�E 33E �3EA�E�3EI�E�fET�E�fEd�E�fEt�E��E�fE�E$�E	4�E
��E��E8 E,�E��E�E	�Ep E�3E� E1�E&fE� E  E�fES3E� E �E � E"[3E#��E$��E%�fE'^fE(�3E)��E++3E,&fE-��E/fE0#3E1��E2��E4 E5�E6h E7��E8�3E9�fE;NfE<��E?�fEB��EE�3EI�EL8 EO[3ER��EUc3EX��E[�3E_�Ea�EeH Eh�EkS3EnY�Eq��Et��Ew�fE{+3E}��E�� E�0�E�њE�]�E�ݚE�| E���E���E�
fE���E�<�E�x�E��3E�-�E�i�E��fE��E�X E��fE�3E�G3E���E��fE�K3E��fE�ٚE�,�E���E��fE�(�E�{3E�͚E�  E�U�E���E��fE�I�E�� E���E�?3E���E��E�%�E�x�E�� E��E�l G�O�?�  G�O�G�O�G�O�?�  G�O�G�O�?�  G�O�G�O�G�O�?�  G�O�?���G�O�?�ff?�  G�O�?���?�33@��@33@   @,��@9��@S33@Y��@l��@�  @�ff@�  @���@�33@�  @���@�ff@�  @���@陚@�ffA��A  A  AffAffA&ffA.ffA6ffA>ffAD��AP  AVffA\��Ad��Al��At��A|��A�33A�ffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414441441444141411411111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               @ �@ �@ *@ O@ ""@ (�@ /�@ 6�@ >�@ F�@ SI@ `B@ m�@ z�@ ��@ ��@ ��@ ��@ �&@ �@ �t@ �@ �q@@o@g@+�@:@G�@V@c�@o�@~K@��@��@��@�F@�>@�7@�/@�4@��@�@{@"�@1'@>@Ji@Yn@g@s_@�@�\@�U@��@��@��@�O@�H@�@�E@�@�@&�@33@A�@N�@[z@i�@x&@�|@�u@�@�@��@�@׹@�@�@@@�@)�@6�@D�@R�@`�@k�@z3@��@��@�(@��@�w@�@��@�m@��@�@@�@.l@;d@H]@UU@a�@p�@�@��@��@��@��@��@�7@ލ@��@�9@�@*@#�@/@=q@K�@Z@hs@uk@��@�@�@�Y@�R@�W@�O@��@��@��@
�@�@$�@4�@A�@M$@\�@k�@x&@�@��@��@�@�@�@�
@�@�@ �@V@�@*S@7�@D�@Q�@^�@n�@|?@��@�0@�(@�-@�&@�o@�#@�m@�e@	�@	o@	�@	+@	:@	H]@	V�@	e	@	r@	}�@	��@	�H@	��@	�F@	��@	�C@	�;@	�(@	��@
%@
{@
""@
/�@
>@
K�@
Yn@
ff@
t�@
��@
�\@
�@
��@
�R@
��@
�O@
��@
��@
�E@�@B@'�@5?@B�@O0@Z�@hs@v�@�@�u@��@�r@�@�o@׹@�T@��@�Q@�@O@)�@7�@FQ@Q�@]�@k�@y�@�7@��@��@��@��@�|@�@�@� @j@@ @-@9X@H]@V@��@>@��@��@&�@r�@�@  @E�@��@��@�@X@�U@�T@+@t�@��@
�@X@��@��@@,@��@�#@'�@t@�@@G�@�7@�o@
�@Lu@��@�W@�@B�@�d@�2@@C�@��@�@@V@�U@��@
@g@�!@�~@A�@�D@�O@[@g@��@� @>�@�@�@o@X�@�@�T@ (G@ m�@ �~@ �q@!9X@!{�@!�&@"@"DD@"��@"�J@#�@#C�@#�@#�2@#��@$=q@$z�@$��@$�@%.l@%k�@%�M@%�@&!s@&]�@&��@&׹@'�@'UU@'��@'��@({@(Wb@(��@(܀@) �@)dZ@)��@)�@*2�@*x�@*��@+�@+M$@+��@+�h@,�@,`A@,�4@,�(@-/@-p�@-��@-��@.7�@.z3@.��@.��@/:�@/|?@/��@/��@0:@0x&@0��@0�e@11�@1n�@1��@1�m@2#�@2]�@2��@2�C@3�@3I�@3��@3��@3�E@47�@4s_@4��@4��@5#�@5^�@5��@5�o@6@6=q@6v@6�r@6�y@7"�@7Z@7�u@7�@8v@8@,@8y�@8�9@8�@9'�@9bN@9��@:@:�+@;.l@;��@<=q@<��@=FQ@=��@>K�@>�@?|?@?�H@@~�@@�l@A�@B�@B�p@C�@C�~@DFQ@D��@EB8@E�@F> @Fψ@Ge�@H]@Hhs@I�@Io�@J�@J�9@K#�@K�@L5@@LӠ@M7L@M�7@Ne	@N�W@OX@O�l@Pv@Q�J@SJ@Tuk@U�o@W�@Xuk@Y�O@[1@\hs@]�W@_'�@`c�@a�7@cv@ddZ@e��@g�@hZ�@i�J@k&�@lWb@m�c@o*@px�@q��@so@tt@u�Z@wC@xWa@y��@{�@{D�@{�@{�<@|o@|^5@|�Z@|��@}+@}v�@}��@}�@~?}@~��@~�C@^@H]@��@�@��@�2�@�V@�y,@��@��-@�Ԧ@��&@�O@�=�@�`�@���@��Z@��>@��@��@�+Z@�N�G�O�@ jG�O�G�O�G�O�@ jG�O�G�O�@ jG�O�G�O�G�O�@ jG�O�@ G�O�@ v@ �G�O�@ �@ 	�@ �@ J@ �@ @ b@ @ �@ �@ �@ B@ O@ [@ g@ ""@ $.@ &�@ (�@ +�@ .l@ 1'@ 3�@ 6�@ :@ <�@ @,@ C�@ F�@ Ji@ M�@ P�@ UU@ X@ Z�@ ^5@ a�@ e	@ hs@ l�@ oFG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�JA�+A�/A�1'A�1'A�/A�(�A�-A�1'A�+A�(�A�33A�/A�1'A�33AП�A�/AθRA΃A�Q�A�5?A��A̓A�n�A�ffA�ZA�VA�Q�A�?}A�1'A�
=A��`A�v�Aʙ�A���A�bNA��A��RA�oA�33A���A�JA�9XA���A��mA���A�K�A��A���A�A��DA�1'A��HA�A�A���A�+A��DA��A��7A���A��A���A�9XA��uA�~�A�
=A�+A���A�\)A�r�A�VA���A���A�;dA�{A��/A�ƨA�Q�A��A�S�A��
A�\)A���A���A�S�A�v�A�A��PA�r�A�9XA��A��uA��A�A���A���A�t�A�A���A�|�A��;A�|�A�(�A�VA��A���A��A��^A��A�?}A�"�A��7A�(�A�
=A�C�A�x�A�JA}�Az�uAv�uAvJAt�jAr��Ap^5An�yAnffAm�Am�hAmO�Al��Ak��Ak�Ah��Ah~�Ah-Ad�`Ac��Aa/A]�wAZ��AZ�!AZ��AZ(�AW�AT�AS%AP�DANI�ALbNAK�^AK�AJ��AIAF�RAE��AEO�AD��AD��AD�\ADr�AD$�ACAC|�AC%ABbNA>�A=��A=33A=�A<A�A;;dA:��A:�A9�#A8VA7�^A7?}A7�A6��A5�A5�A5��A5K�A4r�A3�#A3A1C�A.�A.  A,�A,jA*��A)`BA'`BA%��A$z�A#�TA#�PA"��A!
=Al�An�A�jA�!A��A�A��A-A"�A�A$�AC�AZA�A��A�A��AZAA�
A%A�A
ĜA	�A	�wA �A~�A�;AƨA�AA�9A=qAK�A�-A v�@��@�K�@��R@�=q@��@��u@�1@��H@�ȴ@�X@���@�@�5?@�Ĝ@�S�@�$�@���@���@��@�V@��m@�"�@ʇ+@���@�  @���@���@�E�@�\)@� �@���@��@���@�"�@���@�=q@�@�o@��h@�dZ@�
=@���@�o@��#@�/@��`@�?}@�@�Q�@�\)@�
=@�&�@�K�@���@���@���@��`@��
@��H@��@�Q�@��F@�S�@���@��7@�%@�9X@�@~V@|(�@z�\@y&�@w��@u�@uO�@r�!@pr�@oK�@n5?@kS�@hbN@f��@e/@c��@cdZ@bJ@`��@_\)@\�/@[33@Yhs@X �@V�R@U/@T�@R�!@P1'@O�@M/@K��@I�#@H��@H�@G\)@F5?@E?}@D1@Co@B�\@@��@?��@?�@>$�@=O�@<��@;C�@8  @5�@4�@41@2�@2n�@1��@0 �@/�@/
=@.V@,��@,j@+ƨ@*��@*-@)�^@(�`@'�@'\)@'
=@&E�@%?}@$�D@#��@#��@"�H@"~�@!��@!�@�;@�R@�h@��@��@�@�9@A�@��@��@V@$�@V@��@@��@�@hs@�u@�@ȴ@��@�@�
@�@t�@"�@
��@	��@�`@bN@ �@�@�P@�@ȴ@V@$�@��@�h@/@�/@Z@S�@�H@-@ ��@   ?�p�?�(�?�^5?�ff?�9X?���?�bN?�V?��?�7L?�Q�?�$�?�?}?�z�?�o?���?��?��?�Ĝ?��?ܬ?�?�X?�r�?�r�?�l�?��T?���?ӶF?���?�M�?�&�?��`?�  ?���?���?͑h?�V?�(�?��H?��?�ff?Õ�?���?� �?�|�?�V?��-?�j?�"�?�^5?�=q?�^5?���?���?�~�?���?�^5?���?�"�?��?�dZ?��?�(�?��D?�V?��-?���?��?��;?���?���?�Ĝ?��`?�%?�&�?�hs?��7?���?���?��?�J?�-?�M�?�n�?\?°!?���?��?�o?�33?�S�?�S�?�t�?öF?öF?��
?��?��?�9X?�z�?�z�?ļj?��/?���?��?��A�bA�JA�bA�JA�JA�1A�JA�
=A�
=A�
=A�1A�JA�
=A�1A�1A�A�1A�1A�
=A�VA��A��A�33A�1'A�+A�-A�-A�/A�1'A�1'A�1'A�1'A�1'A�33A�1'A�1'A�33A�/A�-A�(�A�&�A�+A�/A�/A�1'A�1'A�/A�+A�$�A�&�A�&�A�+A�+A�1'A�1'A�5?A�33A�1'A�-A�-G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               A�JA�+A�/A�1'A�1'A�/A�(�A�-A�1'A�+A�(�A�33A�/A�1'A�33AП�A�/AθRA΃A�Q�A�5?A��A̓A�n�A�ffA�ZA�VA�Q�A�?}A�1'A�
=A��`A�v�Aʙ�A���A�bNA��A��RA�oA�33A���A�JA�9XA���A��mA���A�K�A��A���A�A��DA�1'A��HA�A�A���A�+A��DA��A��7A���A��A���A�9XA��uA�~�A�
=A�+A���A�\)A�r�A�VA���A���A�;dA�{A��/A�ƨA�Q�A��A�S�A��
A�\)A���A���A�S�A�v�A�A��PA�r�A�9XA��A��uA��A�A���A���A�t�A�A���A�|�A��;A�|�A�(�A�VA��A���A��A��^A��A�?}A�"�A��7A�(�A�
=A�C�A�x�A�JA}�Az�uAv�uAvJAt�jAr��Ap^5An�yAnffAm�Am�hAmO�Al��Ak��Ak�Ah��Ah~�Ah-Ad�`Ac��Aa/A]�wAZ��AZ�!AZ��AZ(�AW�AT�AS%AP�DANI�ALbNAK�^AK�AJ��AIAF�RAE��AEO�AD��AD��AD�\ADr�AD$�ACAC|�AC%ABbNA>�A=��A=33A=�A<A�A;;dA:��A:�A9�#A8VA7�^A7?}A7�A6��A5�A5�A5��A5K�A4r�A3�#A3A1C�A.�A.  A,�A,jA*��A)`BA'`BA%��A$z�A#�TA#�PA"��A!
=Al�An�A�jA�!A��A�A��A-A"�A�A$�AC�AZA�A��A�A��AZAA�
A%A�A
ĜA	�A	�wA �A~�A�;AƨA�AA�9A=qAK�A�-A v�@��@�K�@��R@�=q@��@��u@�1@��H@�ȴ@�X@���@�@�5?@�Ĝ@�S�@�$�@���@���@��@�V@��m@�"�@ʇ+@���@�  @���@���@�E�@�\)@� �@���@��@���@�"�@���@�=q@�@�o@��h@�dZ@�
=@���@�o@��#@�/@��`@�?}@�@�Q�@�\)@�
=@�&�@�K�@���@���@���@��`@��
@��H@��@�Q�@��F@�S�@���@��7@�%@�9X@�@~V@|(�@z�\@y&�@w��@u�@uO�@r�!@pr�@oK�@n5?@kS�@hbN@f��@e/@c��@cdZ@bJ@`��@_\)@\�/@[33@Yhs@X �@V�R@U/@T�@R�!@P1'@O�@M/@K��@I�#@H��@H�@G\)@F5?@E?}@D1@Co@B�\@@��@?��@?�@>$�@=O�@<��@;C�@8  @5�@4�@41@2�@2n�@1��@0 �@/�@/
=@.V@,��@,j@+ƨ@*��@*-@)�^@(�`@'�@'\)@'
=@&E�@%?}@$�D@#��@#��@"�H@"~�@!��@!�@�;@�R@�h@��@��@�@�9@A�@��@��@V@$�@V@��@@��@�@hs@�u@�@ȴ@��@�@�
@�@t�@"�@
��@	��@�`@bN@ �@�@�P@�@ȴ@V@$�@��@�h@/@�/@Z@S�@�H@-@ ��@   ?�p�?�(�?�^5?�ff?�9X?���?�bN?�V?��?�7L?�Q�?�$�?�?}?�z�?�o?���?��?��?�Ĝ?��?ܬ?�?�X?�r�?�r�?�l�?��T?���?ӶF?���?�M�?�&�?��`?�  ?���?���?͑h?�V?�(�?��H?��?�ff?Õ�?���?� �?�|�?�V?��-?�j?�"�?�^5?�=q?�^5?���?���?�~�?���?�^5?���?�"�?��?�dZ?��?�(�?��D?�V?��-?���?��?��;?���?���?�Ĝ?��`?�%?�&�?�hs?��7?���?���?��?�J?�-?�M�?�n�?\?°!?���?��?�o?�33?�S�?�S�?�t�?öF?öF?��
?��?��?�9X?�z�?�z�?ļj?��/?���?��?��A�bA�JA�bA�JA�JA�1A�JA�
=A�
=A�
=A�1A�JA�
=A�1A�1A�A�1A�1A�
=A�VA��A��A�33A�1'A�+A�-A�-A�/A�1'A�1'A�1'A�1'A�1'A�33A�1'A�1'A�33A�/A�-A�(�A�&�A�+A�/A�/A�1'A�1'A�/A�+A�$�A�&�A�&�A�+A�+A�1'A�1'A�5?A�33A�1'A�-A�-G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BƨBŢBŢBŢBŢBĜBŢBŢBŢBŢBŢBĜBŢBŢBŢB�#B)�BC�BK�BN�BO�BO�BM�BL�BK�BK�BK�BK�BI�BI�BF�BB�B@�B?}BbNBw�B� Bz�Bu�Bx�B{�B|�B� B� B�B�B�B� B�B�uB��B��B��B��B�B�B�B�!B�!B��B�B��B��B� Bo�BhsBiyBhsBs�Bx�Bv�Br�Bm�BhsBffB_;BN�B5?B%�B�BhB1B��B�B��BɺB��B�RB��B�hBy�Bm�BcTBQ�BC�B?}B;dB8RB7LB1'B)�B"�B%�B&�B#�B�BoBDB
��B
�B
�yB
�BB
�
B
ÖB
�?B
�\B
aHB
R�B
1'B
/B
/B
'�B
�B
{B
PB

=B
+B
B
B	��B	��B	��B	�yB	�fB	�NB	��B	��B	�9B	��B	��B	�uB	�bB	�=B	w�B	iyB	cTB	Q�B	G�B	=qB	:^B	8RB	6FB	,B	 �B	�B	�B	�B	�B	uB	uB	bB	PB	
=B	%B��B�`B�NB�5B�5B�
B��B��B��B��BÖBȴBȴBȴBŢBÖBBB��B�qB�jB�FB�B��B��B��B��B��B�PB�=B�B� B~�B{�Bv�Br�Bn�Bk�BffB`BBZBYBW
BS�BP�BO�BL�BJ�BF�BA�BC�BB�BA�B?}B?}B?}B=qB>wB<jB=qB<jB5?B6FB8RB6FB49B2-B2-B/B-B)�B0!B0!B/B/B.B.B/B.B.B-B0!B1'B49B7LB8RB9XB;dB;dB<jB>wB>wBE�BP�BYB_;Bl�Bp�B�B�1B�%B�{B��B��B�RBŢB�NB�B��B	1B	\B	�B	"�B	/B	;dB	bNB	ffB	o�B	� B	��B	��B	�B	�'B	�jB	ĜB	��B	�B	�;B	�NB	�fB	�B	�B	��B	��B	��B	��B	��B	��B
  B
B
B
B
+B
1B
1B

=B
1B
%B

=B
bB
bB
{B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
#�B
&�B
&�B
(�B
)�B
+B
-B
/B
0!B
33B
49B
6FB
6FB
7LB
8RB
9XB
:^B
;dB
<jB
<jB
>wB
@�B
@�B
A�B
B�B
A�B
C�B
G�B
I�B
J�B
J�B
K�B
K�B
L�B
N�B
N�B
N�B
O�B
P�B
Q�B
Q�B
S�B
T�B
S�B
VB
W
B
W
B
W
B
YB
ZB
YB
ZB
[#B
\)B
\)B
]/B
]/B
_;B
_;B
aHB
bNB
cTB
e`B
ffB
ffB
gmB
iyB
hsB
iyB
jB
l�B
l�B
k�B
m�B
n�B
n�B
o�B
p�B
r�B
r�B
t�B
u�B
t�B
u�B
u�B
v�B
w�B
x�B
x�B
x�B
x�B
y�B
y�B
z�B
z�B
{�B
{�B
|�B
{�B
|�B
~�B
~�B
�B
�B
�B
�B
�B
�+B
�7B
�DB
�DB
�PB
�PB
�bB
�bB
�bB
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
�B
�B
�B
�!B
�-B
�3B
�3B
�9B
�FB
�FB
�LB
�LB
�LB
�RB
�XB
�XB
�^B
�XB
�^B
�^B
�^B
�dB
�dB
�dB
�dB
�jB
�dB
�dB
�jB
�dB
�dB
�dB
�dB
�dB
�jB
�dB
�dB
�dB
�jB
�jB
�dB
�dB
�jB
�jB
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
�jB
�jB
�dB
�jB
�jB
�dB
�jB
�jB
�jB
�dB
�jB
�jBĜBƨBƨBŢBŢBƨBƨBŢBƨBŢBƨBŢBŢBƨBŢBƨBŢBƨBƨB��BŢBɺBĜBĜBŢBĜBŢBĜBŢBĜBĜBŢBŢBĜBŢBŢBÖBŢBĜBĜBŢBŢBŢBŢBŢBĜBŢBĜBŢBŢBŢBŢBŢBĜBĜBĜBŢBĜBŢBŢG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               BĜBÖBÖBÖBÖBBÖBÖBÖBÖBÖBBÖBÖBÖB�B'�BA�BI�BL�BM�BM�BK�BJ�BI�BI�BI�BI�BG�BG�BD�B@�B>wB=qB`BBu�B}�Bx�Bs�Bv�By�Bz�B}�B}�B�B�B� B}�B�B�hB�uB��B��B��B��B�B�B�B�B��B�B��B��B}�Bm�BffBgmBffBq�Bv�Bt�Bp�Bk�BffBdZB]/BL�B33B#�B�B\B%B��B�B��BǮB�}B�FB��B�\Bw�Bk�BaHBO�BA�B=qB9XB6FB5?B/B'�B �B#�B$�B!�B�BbB	7B
��B
�B
�mB
�5B
��B
��B
�3B
�PB
_;B
P�B
/B
-B
-B
%�B
�B
oB
DB
1B
B
B
  B	��B	��B	�B	�mB	�ZB	�BB	��B	��B	�-B	��B	�{B	�hB	�VB	�1B	u�B	gmB	aHB	O�B	E�B	;dB	8RB	6FB	49B	)�B	�B	�B	�B	{B	uB	hB	hB	VB	DB	1B	B��B�TB�BB�)B�)B��B��B��B��BɺB��BƨBƨBƨBÖB��B��B��B�}B�dB�^B�9B�B��B��B��B��B�{B�JB�7B�B~�B}�Bz�Bu�Bq�Bm�BjBe`B_;BYBXBVBR�BO�BN�BK�BI�BE�B@�BB�BA�B@�B>wB>wB>wB<jB=qB;dB<jB;dB49B5?B7LB5?B33B1'B1'B.B,B(�B/B/B.B.B-B-B.B-B-B,B/B0!B33B6FB7LB8RB:^B:^B;dB=qB=qBD�BO�BXB^5Bk�Bo�B�B�+B�B�uB��B��B�LBĜB�HB�B��B	+B	VB	�B	!�B	.B	:^B	aHB	e`B	n�B	~�B	��B	��B	�B	�!B	�dB	ÖB	��B	��B	�5B	�HB	�`B	�B	�B	�B	��B	��B	��B	��B	��B	��B
  B
B
B
%B
+B
+B
	7B
+B
B
	7B
\B
\B
uB
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
%�B
%�B
'�B
(�B
+B
-B
/B
0!B
33B
49B
6FB
6FB
7LB
8RB
9XB
:^B
;dB
<jB
<jB
>wB
@�B
@�B
A�B
B�B
A�B
C�B
G�B
I�B
J�B
J�B
K�B
K�B
L�B
N�B
N�B
N�B
O�B
P�B
Q�B
Q�B
S�B
T�B
S�B
VB
W
B
W
B
W
B
YB
ZB
YB
ZB
[#B
\)B
\)B
]/B
]/B
_;B
_;B
aHB
bNB
cTB
e`B
ffB
ffB
gmB
iyB
hsB
iyB
jB
l�B
l�B
k�B
m�B
n�B
n�B
o�B
p�B
r�B
r�B
t�B
u�B
t�B
u�B
u�B
v�B
w�B
x�B
x�B
x�B
x�B
y�B
y�B
z�B
z�B
{�B
{�B
|�B
{�B
|�B
~�B
� B
�B
�B
�B
�B
�%B
�1B
�=B
�JB
�JB
�VB
�VB
�hB
�hB
�hB
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
�B
�B
�!B
�'B
�-B
�9B
�?B
�?B
�FB
�RB
�RB
�XB
�XB
�XB
�^B
�dB
�jB
�qB
�jB
�qB
�qB
�qB
�wB
�wB
�wB
�wB
�}B
�wB
�wB
�}B
�wB
�wB
�wB
�wB
�wB
�}B
�wB
�wB
�wB
�}B
�}B
�wB
�wB
��B
��B
�}B
�}B
�}B
��B
�}B
�}B
��B
��B
��B
�}B
��B
��B
�}B
��B
��B
�}B
��B
��B
��B
�}B
��B
��BBĜBĜBÖBÖBĜBĜBÖBĜBÖBĜBÖBÖBĜBÖBĜBÖBĜBĜBɺBÖBǮBBBÖBBÖBBÖBBBÖBÖBBÖBÖB��BÖBBBÖBÖBÖBÖBÖBBÖBBÖBÖBÖBÖBÖBBBBÖBBÖBÖG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201808180703122021061413522920210614135229202106141746342021061417463420210614174634201808180703122021061413522920210614135229202106141746342021061417463420210614174634PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                         surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018081807031220180818070312  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018081807031220180818070312QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018081807031220180818070312QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015120210722160151IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                