CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-12-22T08:00:42Z creation      
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
resolution        =���   axis      Z        (�  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
(  dx   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (�  n�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
(  �8   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (�  �`   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (�  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
(  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (�  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
( %P   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (� /x   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (� X   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
( ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (� ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
( �h   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (� ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   0   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   L   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    T   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        t   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        |   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � �(   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � 
�      � 
�Argo profile    3.1 1.2 19500101000000  20191222080042  20210617131527  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               [   [DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @��m5IH�@��m5IH�11  @��m33I�@��m33I�@2��^N@2��^N�cw
�/��cw
�/�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >L��?�  @   @9��@�  @�  @�33@�33A   AffA$��A>ffA`  A�ffA���A���A���A���A���AᙚA���B   BffBffB  B ffB'��B/��B733B?��BG��BP  BX  B`ffBh  Bo��Bx  B�33B�33B���B���B�  B�ffB�33B�  B���B�33B�ffB�  B�ffB�  B���B�  B�  B�  B�  B̙�B�ffBԙ�B���Bۙ�B�33B�  B癚B�  B���B�B�  B���C L�C33C33C�C  C
  C�fC�fC33C33C�C�C  C  C�fC�fC�fC"  C$  C&  C(  C*  C,  C.�C0  C2L�C4L�C633C8�C:  C<33C>33C@  CB�CD  CE��CG�fCJ  CL  CN33CP33CR33CT33CVL�CXL�CZL�C\33C^33C`33Cb33CdL�CfL�Ch  Cj  ClL�CnL�CpL�CrffCtL�Cv33Cx33Cz33C|33C~33C��C��C��C��C�&fC�&fC��C��3C�  C��C��C�33C��C�  C��C��C��C��3C��C�  C��3C�  C��3C��fC�  C��C��C��3C�  C�  C��fC��C�  C��3C��C��C�  C��3C��fC�  C�  C��3C�&fC��C��C��C�  C��3C��fC��C��C�  C�&fC�&fC��C�  C��3C��3C��fC��C��C�  C�  C��fC��C��C�  C�  C��3C��C��C�  C��C��C��C��C��3C��C��C�  C��C��C�  C��C��C��3C��C��C��3C�  C�&fC��C��fC�  C��C��C��C��C��fC�  C�  C��C�&fC��C��3C��C��C�  C��fC�  C��C�&fC��C�  C��C�&fC��C��3C��C��C�  C��fC�  C��C���C��fC��3C��D �D s3DfD��D3D��D�3Dy�DfD�fDfDffD�3Ds3D  Ds3D��D� D	  D	s3D	��D
s3D
�3D�3D3D��D  D� D��Ds3D3D�fDfD� D��D��D�D�fD�D��DfD� D3D�fD�D�fD  D��D3D�fD��Ds3D�D� D�3D�3DfDy�D�D�3DfDy�D��D��D fD � D �3D!s3D"3D"�3D#�D#�fD$  D$� D%  D%� D%��D&y�D'  D'� D(  D(y�D)  D)� D)��D*� D+  D+� D,fD,� D-�D-�3D.�D.�3D.�3D/y�D0  D0��D1�D1�3D2  D2y�D3fD3�3D3��D4s3D5  D5ffD5�3D6y�D7�D7��D7��D8��D9�D9y�D:3D:� D:��D;�3D<  D<� D<� D=y�D>  D>ffD>��D?�fD?�fD@� DA3DAs3DBfDB��DB�3DC�fDD3DDs3DEfDE�fDE��DF��DG&fDG� DG�fDHy�DI�DIl�DJ  DJ��DJ��DK�fDL3DLs3DM�DM��DN  DNffDN��DO��DO�3DP�fDQ�DQs3DR�DR��DR��DS��DT�DT� DU�DU��DV  DV�3DW  DW� DX3DX� DY  DY�3DZ  DZ�fD[3D[� D\fD\` D]  D]��D]��D^y�D_fD_l�D`  D`��D`��Day�DbfDbffDb�3Dc� Dd3Dd� De  De�fDf�Dfl�Dg  Dg��Dg��Dhy�Di  Di��Dj�Djl�Dj��Dk�fDl�Dl��Dl��Dms3Dm��Dn� Do�Do�3Dp�Dps3Dq  Dq�fDr�Dr�3Ds�Ds� Ds�3Dty�Dt��Du� Dv  Dv� DwfDw�fDxfDx� Dy  Dy� Dz  Dz� D{  D{� D|  D|s3D|�3D}s3D~�D~��DfD�fD�3D�C3D��3D�� D�  D�9�D�y�D�ɚD�	�D�FfD��3D��3D�  D�9�D�vfD��fD�	�D�C3D��3D�� D���D�I�D��3D��3D���D�9�D���D��fD�  D�<�D�y�D��fD�  D�9�D��fD�� D���D�C3D�|�D���D�fD�C3D�|�D�ɚD�  D�9�D���D��3D���D�I�D�� D���D�	�D�C3D�� D���D�fD�@ D�y�D��fD�3D�<�D�y�D��fD�  D�<�D���D��fD�  D�L�D��fD�� D���D�6fD��3D�� D��fD�C3D���D��3D�  D�9�D�vfD��3D�  D�6fD��3D���D��fD�FfD�� D���D�3D�@ D�vfD��fD�  D�<�D��fD�� D���D�FfD�� D���D�fD�C3D�|�D���D�fD�C3D�� D���D��D�C3D�� D���D�	�D�FfD�� D���D�	�D�C3D�� D���D�fD�C3D�� D���D��D�FfD��3D�� D�  D�9�D�vfD�ɚD�3D�@ D�|�D���D�	�D�FfD��3D�� D���D�L�D���D�ɚD�fD�FfD��3D�� D���D�<�D���D�ɚD�	�D�C3D��3D�� D�  D�<�D�� D���D���D�L�D���D��fD�  D�<�D�y�D��fD�	�D�C3D��3D���D���D�L�D���D��fD�fD�C3D�� D���D��D�I�D��fD��3D�3D�@ D�y�D���D�fD�@ D�|�D���D�fD�C3D�|�D��3D��fD�FfD��3D���D�	�D�FfD�� D���D��fD�FfD�� D�� D��D�C3D�|�D�ɚD�3D�<�D���D�� D���D�FfD�� D��fD�  D�L�D��3D���D�	�D�@ D�y�D��3D��D�FfD�� D�ɚD�3D�@ D���D��3D���D�FfD�� D��fD�  D�L�D��fD���D�	�D�<�D�y�D��3D��D�C3D�|�D�ɚD���D�6fD�y�D��3D��D�C3D�|�D�ɚD�  D�9�D3D�� D�fD�C3DÌ�D��3D���D�FfD�|�DĶfD�  D�FfDŐ D��fD���D�C3D�|�DƶfD���D�FfDǐ D��3D���D�FfDȌ�D��3D���D�FfDɌ�D��3D���D�@ DʆfD�� D���D�@ Dˉ�D˼�D��fD�C3Ď�D��3D���D�@ D�y�D͹�D�  D�L�DΆfD�� D�fD�<�D�vfD�� D�fD�<�D�s3Dм�D�3D�P Dщ�D�� D�	�D�@ D�y�D��3D��D�C3DӀ D�ɚD�  D�9�DԆfD�� D��fD�C3D�y�Dճ3D���D�I�Dփ3Dֹ�D�fD�<�D�vfD�� D�	�D�C3D�y�D��3D� D�FfDـ D�ɚD���D�6fDچfD�� D�3D�<�DۆfDۼ�D��fD�FfD܌�D��3D���D�FfD�vfDݳ3D�  D�I�Dރ3D޼�D�fD�<�D�vfD�� D�	�D�C3D�|�D��fD� D�FfD�|�D�ɚD�3D�9�D�fD���D�3D�9�D�fD��D��fD�<�D�fD�� D��fD�C3D��D��3D���D�FfD��D��3D���D�FfD� D��fD�  D�I�D� D蹚D�fD�@ D�y�D��3D���D�6fD� D�ɚD�  D�9�D�3D���D�3D�<�D�3D�� D�3D�9�D�3D���D�3D�<�D�fD�� D��fD�@ D��D�� D���D�FfD�|�D�D�fD�<�D�vfD�� D��D�@ D�|�D��3D� D�FfD� D���D�fD�@ D�D��3D���D�I�D�|�D���D�3D�I�D��3D���D�	�D�@ D�y�D��fD���D�6fD��3D���D�3D�<�D���D���D��D�|�D�  D��fD�� D��fD��fE s3E3E�fE$�E��E>fE��EY�E�fEs3E��E3E��E33E�3E	X E	�fE
vfE�E)�E� EK3E�3El�E�fE��E  E� EFfEvfE�E�fE( E��EH E�fEffE�fE��E�3E+3E� E>fE�3ET�Ea�E�Eh E�Ek3Ep E�fE k3E � E!��E"X E"� E#��E$4�E$��E%��E%�fE&�fE'P E'� E(�fE)3E*3E*t�E*�3E+�fE,L�E,��E-�fE.�E/�E/{3E/�fE0��E1D�E2.fE2��E3fE4fE4x E4�fE5� E6S3E6�3E7� E86fE8�fE9��E:�E:� E;|�E;�E<^fE=FfE=�3E>��E?fE?vfE@Y�E@�3EA��EB�EB�fECK3EDfED�3EES3EE�3EF~fEGFfEG��EHi�EI$�EI|�EJ4�EJ�3EK��EL>fEL� EM<�EM� EN��EO0 EO�fEPx EQ�EQ�fERQ�ER�fES� ET0 ETњEUp EV EV�fEWQ�EW�fEX�3EYA�EY�3EZ��E[D�E[��E\P E]  E]�3E^a�E^��E_s3E`)�E`�Ea9�Ea�Eb��Ec\�Ec��Eds3Ee.fEe�3EfFfEg Eg� Eh,�Eh� EiVfEjfEj� Ek>fElfEl��Em,�Em�3En\�Eo,�Eo��Ep[3Eq#3Eq�3ErI�Es	�Esh Et,�Et�3EuH Ev3Ev��Ew$�Ew�fEx��Ex� Ey�3Ezc3E{�E{�fE|3E|� E}�fE~@ E~�fEFfE�E�P�E�� E���E�G3E���E���E�2fE�~fE��3E��E�c3E�� E�fE�\�E��fE�� E�D�E��3E��E�*fE���E���E�/3E�q�E�� E�fE�]�E���E��E�bfE��3E��3E�< E�� E��3E�( E��3E�� E�3E�p E�� E�3E�Y�E��3E�fE�\�E��3E��3E�BfE���E���E�1�E��3E�� E�+3E�ffE���E�3E�VfE��fE�fE�C3E��fE���E�H E��fE�՚E�(�E�3E���E�"fE�vfE���E��E�jfE���E�3E�U�E�� E��fE�@ >���>���>L��>L��>L��=���>L��>���>L��>L��>L��=���>L��>L��=���>L��>L��>���>���?   ?��?333?L��?�  ?���?�33?ٙ�@   @��@&ff@9��@L��@l��@�  @���@�33@�  @���@���@ə�@ٙ�@陚@�ffA33A33A��A33A$��A,��A6ffA>ffAFffAQ��AX  A`  Ah  AnffAx  A���A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444441144441441414111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ?333?�  @   @Y��@�  @�  @�33@�33A  AffA,��AFffAh  A�ffA���A���A���A���A���A噚A���B  B
ffBffB  B"ffB)��B1��B933BA��BI��BR  BZ  BbffBj  Bq��Bz  B�33B�33B���B���B�  B�ffB�33B�  B���B�33B�ffB�  B�ffB�  B���B�  B�  B�  B�  B͙�B�ffBՙ�B���Bܙ�B�33B�  B虚B�  B���B���B�  B���C ��C�3C�3C��C� C
� CffCffC�3C�3C��C��C� C� CffCffC ffC"� C$� C&� C(� C*� C,� C.��C0� C2��C4��C6�3C8��C:� C<�3C>�3C@� CB��CD� CFL�CHffCJ� CL� CN�3CP�3CR�3CT�3CV��CX��CZ��C\�3C^�3C`�3Cb�3Cd��Cf��Ch� Cj� Cl��Cn��Cp��Cr�fCt��Cv�3Cx�3Cz�3C|�3C~�3C�L�C�L�C�L�C�Y�C�ffC�ffC�Y�C�33C�@ C�L�C�Y�C�s3C�Y�C�@ C�L�C�Y�C�L�C�33C�Y�C�@ C�33C�@ C�33C�&fC�@ C�Y�C�L�C�33C�@ C�@ C�&fC�L�C�@ C�33C�Y�C�Y�C�@ C�33C�&fC�@ C�@ C�33C�ffC�Y�C�L�C�Y�C�@ C�33C�&fC�L�C�L�C�@ C�ffC�ffC�L�C�@ C�33C�33C�&fC�Y�C�L�C�@ C�@ C�&fC�Y�C�L�C�@ C�@ C�33C�Y�C�Y�C�@ C�Y�C�Y�C�L�C�L�C�33C�Y�C�Y�C�@ C�Y�C�L�C�@ C�Y�C�L�C�33C�L�C�L�C�33C�@ C�ffC�L�C�&fC�@ C�L�C�Y�C�Y�C�L�C�&fC�@ C�@ C�Y�C�ffC�Y�C�33C�L�C�Y�C�@ C�&fC�@ C�Y�C�ffC�L�C�@ C�L�C�ffC�Y�C�33C�L�C�Y�C�@ C�&fC�@ C�Y�C��C�&fC�33C�Y�D 9�D �3D&fD��D33D��D3D��D&fD�fD&fD�fD3D�3D  D�3D�D� D	  D	�3D
�D
�3D3D�3D33D��D  D� D�D�3D33D�fD&fD� D�D��D,�D�fD9�D��D&fD� D33D�fD9�D�fD  D��D33D�fD�D�3D,�D� D3D�3D&fD��D9�D�3D&fD��D�D��D &fD � D!3D!�3D"33D"�3D#,�D#�fD$  D$� D%  D%� D&�D&��D'  D'� D(  D(��D)  D)� D*�D*� D+  D+� D,&fD,� D-,�D-�3D.9�D.�3D/3D/��D0  D0��D1,�D1�3D2@ D2��D3&fD3�3D4�D4�3D5  D5�fD63D6��D7,�D7��D8�D8��D99�D9��D:33D:� D;�D;�3D<@ D<� D=  D=��D>  D>�fD?�D?�fD@fD@� DA33DA�3DB&fDB��DC3DC�fDD33DD�3DE&fDE�fDF�DF��DGFfDG� DHfDH��DI,�DI��DJ  DJ��DK�DK�fDL33DL�3DM,�DM��DN  DN�fDO�DO��DP3DP�fDQ9�DQ�3DR,�DR��DS�DS��DT9�DT� DU,�DU��DV  DV�3DW@ DW� DX33DX� DY  DY�3DZ@ DZ�fD[33D[� D\&fD\� D]  D]��D^�D^��D_&fD_��D`  D`��Da�Da��Db&fDb�fDc3Dc� Dd33Dd� De  De�fDf,�Df��Dg  Dg��Dh�Dh��Di  Di��Dj9�Dj��Dk�Dk�fDl,�Dl��Dm�Dm�3Dn�Dn� Do,�Do�3Dp9�Dp�3Dq  Dq�fDr,�Dr�3Ds9�Ds� Dt3Dt��Du�Du� Dv  Dv� Dw&fDw�fDx&fDx� Dy  Dy� Dz  Dz� D{  D{� D|  D|�3D}3D}�3D~9�D~��D&fD�fD�3D�S3D��3D�� D� D�I�D���D�ٚD��D�VfD��3D��3D� D�I�D��fD��fD��D�S3D��3D�� D�	�D�Y�D��3D��3D��D�I�D���D��fD� D�L�D���D��fD� D�I�D��fD�� D�	�D�S3D���D�ɚD�fD�S3D���D�ٚD� D�I�D���D��3D��D�Y�D�� D���D��D�S3D�� D���D�fD�P D���D��fD�3D�L�D���D��fD� D�L�D���D��fD� D�\�D��fD�� D�	�D�FfD��3D�� D�fD�S3D���D��3D� D�I�D��fD��3D� D�FfD��3D���D�fD�VfD�� D�ɚD�3D�P D��fD��fD� D�L�D��fD�� D�	�D�VfD�� D�ɚD�fD�S3D���D���D�fD�S3D�� D���D��D�S3D�� D���D��D�VfD�� D�ɚD��D�S3D�� D���D�fD�S3D�� D���D��D�VfD��3D�� D� D�I�D��fD�ٚD�3D�P D���D�ɚD��D�VfD��3D�� D��D�\�D���D�ٚD�fD�VfD��3D�� D��D�L�D���D�ٚD��D�S3D��3D�� D� D�L�D�� D���D�	�D�\�D���D��fD� D�L�D���D��fD��D�S3D��3D���D��D�\�D���D��fD�fD�S3D�� D���D��D�Y�D��fD��3D�3D�P D���D���D�fD�P D���D�ɚD�fD�S3D���D��3D�fD�VfD��3D���D��D�VfD�� D�ɚD�fD�VfD�� D�� D��D�S3D���D�ٚD�3D�L�D���D�� D�	�D�VfD�� D��fD� D�\�D��3D���D��D�P D���D��3D��D�VfD�� D�ٚD�3D�P D���D��3D��D�VfD�� D��fD� D�\�D��fD�ɚD��D�L�D���D��3D��D�S3D���D�ٚD��D�FfD���D��3D��D�S3D���D�ٚD� D�I�D3D�� D�fD�S3DÜ�D��3D��D�VfDČ�D��fD� D�VfDŠ D��fD��D�S3Dƌ�D��fD��D�VfDǠ D��3D��D�VfDȜ�D��3D�	�D�VfDɜ�D��3D�	�D�P DʖfD�� D�	�D�P D˙�D���D�fD�S3D̜�D��3D�	�D�P D͉�D�ɚD� D�\�DΖfD�� D�fD�L�DφfD�� D�fD�L�DЃ3D���D�3D�` Dљ�D�� D��D�P D҉�D��3D��D�S3DӐ D�ٚD� D�I�DԖfD�� D�fD�S3DՉ�D��3D��D�Y�D֓3D�ɚD�fD�L�D׆fD�� D��D�S3D؉�D��3D�  D�VfDِ D�ٚD��D�FfDږfD�� D�3D�L�DۖfD���D�fD�VfDܜ�D��3D��D�VfD݆fD��3D� D�Y�Dޓ3D���D�fD�L�D߆fD�� D��D�S3D���D��fD�  D�VfD��D�ٚD�3D�I�D�fD���D�3D�I�D�fD���D�fD�L�D�fD�� D�fD�S3D��D��3D�	�D�VfD��D��3D�	�D�VfD� D��fD� D�Y�D� D�ɚD�fD�P D鉚D��3D��D�FfD� D�ٚD� D�I�D�3D���D�3D�L�D�3D�� D�3D�I�D�3D���D�3D�L�D�fD�� D�fD�P D��D�� D�	�D�VfD���D�ɚD�fD�L�D�fD�� D��D�P D��D��3D�  D�VfD� D���D�fD�P D���D��3D��D�Y�D���D�ɚD�3D�Y�D��3D���D��D�P D���D��fD��D�FfD��3D���D�3D�L�D���D���D��D���D� D��fD�� D��fD��fE {3E3E�fE,�E��EFfE��Ea�E�fE{3E��E3E��E;3E�3E	` E	�fE
~fE�E1�E� ES3E�3Et�EfE��E( E� ENfE~fE	�E�fE0 E��EP E�fEnfE�fE��E�3E33E� EFfE�3E\�Ei�E�Ep E�Es3Ex E�fE s3E � E!��E"` E"� E#ɚE$<�E$��E%��E&fE&�fE'X E'� E(�fE)3E*3E*|�E*�3E+�fE,T�E,ɚE-�fE.$�E/�E/�3E/�fE0��E1L�E26fE2��E3fE4fE4� E4�fE5� E6[3E6�3E7� E8>fE8�fE9��E:�E:� E;��E;�E<ffE=NfE=�3E>��E?fE?~fE@a�E@�3EA��EB�EB�fECS3ED&fED�3EE[3EE�3EF�fEGNfEG��EHq�EI,�EI��EJ<�EJ�3EK��ELFfEL� EMD�EM� EN��EO8 EO�fEP� EQ!�EQ�fERY�ER�fES� ET8 ETٚEUx EV EV�fEWY�EW�fEX�3EYI�EY�3EZ��E[L�E[��E\X E] E]�3E^i�E^��E_{3E`1�E`�EaA�Ea��Eb��Ecd�Ec��Ed{3Ee6fEe�3EfNfEg Eg� Eh4�Ei  Ei^fEj&fEj� EkFfElfEl��Em4�En3End�Eo4�Ep�Epc3Eq+3Eq�3ErQ�Es�Esp Et4�Et�3EuP Ev3Ev��Ew,�Ew�fEx��Ey  Ey�3Ezk3E{�E{�fE|#3E|� E}�fE~H E~�fENfE��E�T�E�� E���E�K3E���E���E�6fE��fE��3E��E�g3E�� E�fE�`�E��fE�� E�H�E��3E��E�.fE���E���E�33E�u�E�� E�fE�a�E�ŚE��E�ffE��3E�3E�@ E�� E��3E�, E��3E�� E�3E�t E�� E�3E�]�E��3E�
fE�`�E��3E��3E�FfE���E���E�5�E��3E�� E�/3E�jfE�ŚE�3E�ZfE��fE�
fE�G3E��fE���E�L E��fE�ٚE�,�E��3E���E�&fE�zfE���E��E�nfE���E�3E�Y�E�� E��fE�D G�O�G�O�G�O�G�O�G�O�?��?333G�O�G�O�G�O�G�O�?��G�O�G�O�?��G�O�?333G�O�?fff?�  ?���?���?�ff?�  ?ٙ�?�33@��@   @,��@Fff@Y��@l��@�ff@�  @���@�33@�  @���@���@ٙ�@陚@���A33A33A33A��A#33A,��A4��A>ffAFffANffAY��A`  Ah  Ap  AvffA�  A���A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444441144441441414111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         @ ^@ �@ �@ �@ O@ ""@ )�@ 0x@ 6�@ <�@ FQ@ Q=@ _�@ oF@ {�@ �7@ ��@ ��@ �-@ ��@ �|@ �t@ ��@ �q@j@�@
@+�@8�@F�@T�@b�@p�@~�@��@��@�A@��@�>@ψ@�/@�@��@�@{@!s@0x@>�@K@Z@ff@s_@��@�\@�@��@�^@�W@խ@��@�@�E@
=@�@%�@2�@?}@N�@^5@k�@x�@�|@�u@�m@�@�@ȴ@�h@�@�@ �@�@O@(G@5�@C�@Q�@_�@m:@z�@��@�0@��@�~@�2@��@��@��@��@�@o@�@-@:@FQ@T�@b�@p�@�@�P@��@��@��@Ĝ@�C@�;@��@��@1@�@$.@/�@=q@M$@Z�@hs@v�@��@��@�a@�@��@�W@�O@��@�@��@J@�@&�@2�@@�@O0@]�@l�@x�@�@�u@��@��@�@��@�
@�@�Y@�Q@J@O@*S@7L@C�@Q�@_�@k�@{�@��@��@�5@��@�&@�@�@�@��@	�@	@	 @	-@	;d@	G�@	T�@	a�@	qS@	~�@	��@	��@	�M@	��@	@	ψ@	�/@	�(@	��@
�@
{@
""@
.l@
>�@
K�@
X�@
ff@
s_@
�@
��@
�@
�@
��@
ƨ@
�O@
��@
�L@
��@
=@B@&;@33@B8@O0@[z@j@x&@�p@��@�y@��@�^@�c@׹@�@�@ �@J@O@(�@7�@FQ@SI@^�@m�@|?@��@��@��@��@�2@�|@�t@��@��@�@b@g@-�@:@FQ@UU@dZ@m�@|�@�D@��@��@��@�>@є@��@�@@��@%@*@"�@0x@:�@I�@Wb@ff@r�@�@�\@�@�M@��@Ĝ@�C@�T@��@��@
=@�@$�@1�@B�@O0@\�@i�@v�@��@�$@�@��@�@�@�t@�@�@�@V@O@+�@8�@D�@Q=@^5@n�@z�@�+@�<@��@��@��@��@�#@�m@�@�@�@�@+@8�@I�@Wb@dZ@qS@~K@��@��@�A@�9@��@�7@��@�@�~@�@{@!s@/�@=q@K@Yn@ff@uk@��@�@�@�M@��@��@��@�@��@  @	�@�@'�@1'@?}@N�@Yn@hs@v�@�|@��@��@�r@�w@ȴ@�@�@�@@@O@%�@5�@DD@O0@^�@m�@x&@��@�<@�y@�-@��@�o@�#@�(@�e@@*@
@-�@>@G�@R�@bN@r@|?@��@��@�5@��@Ĝ@��@�;@�@@�,@@�@#�@.l@>@M�@Wb@g�@v�@�@��@��@��@��@ȴ@Ӡ@�T@�Y@��@J@O@%�@5?@DD@O0@^5@m:@x&@��@��@��@�@�@�@��@�@�@��@�@�@&;@5?@DD@S�@b�@m:@{�@��@�$@��@��@�@�@�t@�y@�~@^@b@g@-�@;d@E�@S�@bN@p�@�@��@�U@��@��@�>@є@��@�@@��@v@�@!s@/�@=q@K@Yn@g@t�@��@�\@�@��@�R@��@Ӡ@�H@�@�9@�@�@&�@3�@A�@O0@\�@j@ww@�@�i@�@�!@��@��@׹@�`@�Y@��@�@B@+@7L@D�@Q�@^5@oF@{�@�7@��@�z@��@��@��@��@�@� @j@�@ @,`@8�@H]@T�@a�@r@~�@�D@��@�A@��@Ĝ@��@�/@�@�,@%@�@"�@/�@@,@Lu@X�@e	@r@�d@��@��@�@�R@�J@�\@�@��@�Q@�@�@$/@1'@A�@N�@Z@j@z3@��@��@�@�@�j@�c@��@�`@�@��@ @ O@ '�@ 7L@ DD@ O�@ `�@ m:@ z3@ ��@ �0@ �z@ ��@ �&@ �o@ ��@ ��@ �@!%@!o@!g@!,`@!9X@!Ji@!V@!b�@!s_@!�W@!�P@!��@!��@!��@!�>@!�7@!��@!��@!��@"�@"�@"$�@"1'@">@"K@"X�@"e	@"r@"��@"�@"�@"��@"��@"�@"��@"��@"��@"��@#�@#�@#'�@#4�@#B8@#O0@#\)@#i!@#v�@#��@#��@#�z@#��@#�j@#�c@#�
@#�@#�Y@#�Q@$J@$
@$+@$7�@$DD@$Q=@$^5@$k.@$|�@$�7@$��@$�(@$��@$��@$ψ@$��@$�y@$�q@%j@%b@%!s@%.l@%;d@%H]@%V@%b�@%oF@%�@%�P@%��@%��@%��@%��@%��@%�/@%��@%� @&1@&*@&!s@&1�@&>�@&K@&Wb@&dZ@&uk@&��@&�\@&��@&�Y@&��@&�@&�O@&��@&��@&��@'�@'B@'%�@'1'@'@�@'Q=@'\�@'i!@'y�@'�@'�i@'�@'��@'�@'�c@'�@'�`@'�Y@(�@(V@(�@(*S@(:@(E�@(Q�@(bN@(n�@(y�@(��@(��@(�z@(�-@(��@(�|@(��@(�(@(�@)]@)�@)g@)/@):�@)F�@)Wb@)b�@)oF@)~�@)�\@)��@)��@)��@)�>@)ψ@)�;@)��@)� @*�@*�@*%�@*1'@*<�@*K�@*X@*dZ@*s_@*�@*��@*��@*��@*��@*ȴ@*�O@*��@*�L@*�Q@+
�@+�@+%�@+4�@+@�@+M$@+\)@+k�@+v�@+�@+�u@+�(@+��@+�^@+�c@+խ@+�T@+�Y@,�@,@,O@,*S@,5�@,B8@,Q�@,`�@,l�@,x&@,��@,��@,�A@,��@,�&@,��@,�t@,�@,�q@-%@-�@-�@-.l@-:@-FQ@-V�@-b�@-n�@-~�@-��@-��@-��@-��@-�>@-��@-�;@-��@-� @.�@.�@."�@..l@.>@.N�@.Z@.ff@.v@.�@.�P@.�a@.�@.�@.�J@.��@.��@.��@.��@/�@/�@/$�@/4�@/>�@/K�@/\)@/k�@/x&@/�p@/�#@/��@/�@/��@/�o@/׹@/�@/�@0j@0@0�@0+@07L@0B�@0SI@0bN@0m�@0y�@0��@0��@0��@0��@0��@0��@0�h@0��@0�~@1@1�@1 @1/@1:�@1FQ@1V�@1ff@1r@1~K@1��@1��@1��@1�F@1@1��@1ލ@1��@1� @2�@2�@2""@2.l@2>@2M�@2Yn@2e�@2t�@2�@2�@2��@2�Y@2�@2ƨ@2��@2�@2��@2��@3
=@3�@3%�@31�@3B8@3M�@3Z�@3k.@3v�@3�@3��@3�(@3�@3�@3�@3�t@3�@3�Y@4�@4@4O@4+@47L@4C�@4S�@4^�@4k�@4{�@4��@4��@4�(@4��@4�&@4�o@4��@4�m@4�@5@5�@5g@5+�@5<@5F�@5X@5o�@5��@5��@6�@6SI@6��@6�*@7�@7Ji@7�+@7�>@7�Q@8<@8x&@8�9@8�L@9hs@9��@9ލ@:�@:Z@:��@:�[@;�@;P�@;�|@<
=@<I@<�|@<Ĝ@=�@=A�@=~K@=��@=��@>}�@>�@>�}@?6�@?t�@?�~@?�@@@+�@@i!@@��@A @AZ@A�0@Aψ@B�@BFQ@B�@B�@C(�@C`B@C��@D�@D<�@Dr@D�A@E@EDD@Eww@Eލ@F�@FA�@F��@F��@G5�@Gb�@G��@G�@H#�@H��@H�^@H��@IT�@I��@I��@J�@JI�@J�!@J�;@Kb@Kr�@K�z@L%@L7L@Li!@Lψ@M  @M2�@M��@M��@M��@Nff@N��@N�@O1�@Oc�@O�0@O��@P-@P^�@P��@P�L@QR�@Q�@Q��@R�@R>�@R�@R�o@S(G@SSI@S�f@S�h@T1'@TZ@T��@U%@U.l@U�d@U�C@U��@VFQ@V��@V�#@W$�@WG�@W�i@W�t@X �@Xff@X�f@X�Y@Y7L@Yz2@Y�k@Y�Q@ZDD@Z��@Z�|@[@[UU@[��@[ލ@\$�@\k.@\�-@\��@]B�@]��@]��@^  @^K@^��@^��@_�@_V�@_��@_�@`�@`g@`��@a@a'�@ax�@aȴ@bB@b@,@b�0@b�@c�@cff@c��@c�@d6�@d^�@d�9@e�@e1�@e��@e��@fJ@fc�@f�P@f�@g�@g`B@g�-@g�t@h.l@h�@h�A@h��@iM$@ir�@i�J@j*@j:@j��@j��@kg@kl�@k��@k�H@l/@lz�@l��@l��@m3�@m~�@m��@n
�@nQ=@n�$@n׹@o�@oZ�@o�T@oލ@p	@p\)@p��@p�@q.l@qff@q�@q�@rE�@r|�@r��@s�@s[z@s�$@s��@t$.@t]�@t��@t�y@u<@us_@u��@u��@vDD@v��@v�&@w�@wX�@w�C@w�
@x""@xT�@x�a@x�l@y1�@y{�@y��@y�@z?}@z�7@z�k@{�@{^�@{��@{��@|@|`�@|�f@|��@}+@}v@}��@}�e@~> @~��@~�C@^@H]@�@׹@��@�2�@�U�@�w�@���@��@�݆@���@� m@�A�@�b�G�O�G�O�G�O�G�O�G�O�@  �@ ^G�O�G�O�G�O�G�O�@  �G�O�G�O�@  �G�O�@ ^G�O�@ �@ j@ @ �@ v@ �@ 1@ 	�@ �@ �@ @ �@ �@ �@ B@ O@ [@ g@ ""@ $�@ (G@ +@ .l@ 1�@ 4�@ 7�@ ;d@ >@ B8@ FQ@ I�@ M�@ Q=@ T�@ Yn@ \)@ _�@ b�@ e�@ i�@ n�@ p�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�C�A�C�A�M�A�Q�A�^5A�bNA�dZA�bNA�dZA�dZA�`BA�`BA�`BA�ffA�hsA�ffA�ffA�hsA�hsA�jA�l�A�n�A�n�A�r�A�t�AׁAׁA�|�AׁA׉7A׉7AבhAׅA�|�AׁAׇ+Aׇ+AׅA׉7Aׇ+A׃A׃A׋DA״9Aة�A�9XA�E�A��A��A�JA�|�A�E�A�(�A���A�"�A�S�A�ZA�
=A���A�$�A���A�(�A���A��^A�"�A�  A�r�A�9XA�+A� �A��#A�
=A��+A���A��A�ȴA��DA�x�A�M�A��\A���A���A�{A���A�Q�A���A�%A���A��FA�G�A��
A��yA���A�G�A�A��mA���A��A�=qA�%A���A��RA��A���A��hA�=qA��HA��\A��A�ffA�G�A�t�A�ĜA�S�A���Ay�FAv{Atz�AqXAm�mAl�`Ak%Ah1AfJAe\)Ae
=Ad�RAcl�A^�\AYoAV(�AT-AP��AN1AI��AE;dAB~�AA�A@�RA=VA9A7t�A6 �A4��A37LA1�A0{A.-A,1'A*��A)�7A(�A(�\A(n�A(E�A(JA'�A'��A%A"9XA!�A�\A�A~�A%AZA�7A�jAVA�A��A�A�A��A�PA�A�A/A
��A	�mA	t�A	+A��A��AA?}A�jA�-A��A1'A�PA��An�A��A��A"�A Q�@���@��y@��@��j@�K�@�/@�|�@�x�@�A�@�v�@�G�@�9X@�@��@�7@�M�@�@�33@�dZ@�l�@�\)@�v�@���@��-@���@��F@�1'@� �@��D@�&�@��@���@��@�V@�/@��9@��m@�\)@�~�@�J@�@�  @��@@�7@��@웦@�V@�J@�M�@���@�Ĝ@��m@�F@땁@��
@�C�@�=q@�?}@� �@�~�@�j@�@�K�@�;d@�"�@�`B@�r�@�+@�x�@���@ۅ@���@��y@ڗ�@�-@٩�@�%@ؓu@�1@�K�@֗�@ա�@���@�j@��@Ӿw@��@���@�@�"�@�n�@щ7@�A�@�M�@�`B@���@��@���@���@���@�v�@���@Ͳ-@́@�/@�z�@�"�@��@�p�@�Ĝ@�I�@� �@Ǖ�@Ɨ�@���@�\)@�33@�33@�33@�
=@�^5@�J@���@�G�@��@�Ĝ@�1'@���@�t�@�dZ@�;d@�
=@��!@�~�@��@��T@���@���@�@��-@��@�p�@��@��j@�j@�  @�;d@�@���@�E�@��-@�hs@�G�@�&�@�V@��@��9@���@��D@�r�@�I�@�1@��w@�33@��\@�=q@�J@��T@�hs@�Ĝ@���@�j@���@��@�\)@�
=@���@�5?@��@�hs@��9@�j@��
@�K�@�+@��H@���@�M�@�5?@��#@�O�@�V@���@�Q�@��@�|�@�;d@�+@�"�@�
=@�ff@�@��7@�hs@�V@�Ĝ@���@�r�@� �@���@��@�33@�n�@���@��@�7L@�Ĝ@���@�Ĝ@�r�@�A�@� �@��
@���@�C�@���@�ȴ@���@���@���@���@��T@��^@���@�x�@�/@���@�r�@�bN@�Z@�I�@� �@�ƨ@���@��P@�C�@��H@�ȴ@�V@��#@�x�@��7@�p�@�`B@�G�@���@�z�@�A�@�  @���@��w@�o@�^5@���@�`B@��@�z�@��@��
@���@��@���@�n�@�M�@�@��-@��-@�X@�/@���@�Q�@�  @��
@�ƨ@���@��
@��@��P@�|�@���@��T@��h@�p�@�G�@��@��@��D@�Q�@�Q�@�Q�@�Q�@�Q�@�1'@� �@�b@��w@�dZ@�C�@�"�@�o@��y@���@��\@�$�@�@�?}@���@��@�j@�Z@�(�@�b@��m@���@�o@���@�J@��@��7@�G�@�&�@�V@���@��@���@��u@�S�@�
=@��@���@��\@�^5@�5?@���@���@�@��h@�hs@�G�@���@��j@���@��D@�A�@��m@���@�l�@�o@���@��@��!@���@��+@�^5@�J@��-@���@��h@�x�@�p�@�`B@��@���@�Ĝ@��D@�r�@�Z@�I�@��@��@�P@\)@~E�@}�-@}V@|�@|I�@{�@z=q@y&�@x1'@xĜ@w�@wK�@v��@v��@vv�@u?}@t�/@t�@t��@tj@s��@s33@r�@q��@q&�@p��@p�@o�@o��@o|�@o\)@o
=@nE�@m`B@m/@mV@l�/@k�m@k"�@j��@j^5@i�#@i��@ix�@h��@h�u@h�@hQ�@g�@g�@g�@g�@fȴ@fv�@fV@f{@e��@e�-@e�h@ep�@e�@d�@d�@d�/@d�/@d�j@d�@c��@cdZ@cC�@co@c@b��@b~�@a�#@a7L@`�`@_�@^��@^�@^��@^�+@^E�@^5?@]�@]��@]��@]?}@]V@\��@\j@[ƨ@[�@[S�@[o@Z�@Z��@Y��@Y�@X��@XĜ@X�9@X�9@Xr�@X �@W�;@W�w@W��@V�@V5?@U�@U�T@U�-@U�@UV@T�j@TI�@T�@SC�@Q�#@Q7L@P�`@P��@P��@PQ�@P1'@O�;@O|�@OK�@N�y@N�R@Nv�@NE�@M��@M`B@M/@L�j@Lz�@LI�@L9X@L(�@K��@K@J�H@J��@J��@J�@I%@H�9@H1'@G|�@G\)@G;d@G+@G;d@G;d@G;d@G+@G�@F��@F�y@F�R@F@E�-@Ep�@E?}@E/@E/@E?}@EO�@EO�@EO�@E`B@E`B@E`B@E?}@E�@D�/@DZ@D1@C�
@Cƨ@Ct�@C@B�!@B=q@A��@A&�@@�9@@1'@?K�@>ff@>$�@=�-@=�-@=O�@=�@=�@=V@<��@<(�@;�F@;��@;t�@:�!@9�#@9��@8Ĝ@8 �@7l�@6ȴ@6$�@5��@5`B@5�@4��@4��@4��@4�/@4��@4z�@4�@3�F@3��@3dZ@3C�@2�@1�@1��@1��@1hs@0�`@/��@/�@/|�@/+@.��@.V@.E�@.@-��@-�T@-�-@-�@-�@-p�@-�@,�@,�@+�
@+��@+dZ@+dZ@+C�@+33@+33@*�H@*�!@*n�@*-@*�@)�@)��@)7L@)�@(��@(��@(��@(�u@(�u@(��@(�@(r�@(bN@(Q�@( �@(b@(  @'�w@'|�@'l�@'\)@'\)@'K�@'\)@'+@'
=@%��@%/@$�@$��@$�D@#ƨ@#��@#t�@#dZ@#C�@#@#@"�@"�@"�H@"�\@"^5@"�@!��@!��@!x�@!7L@!%@ �u@;d@ȴ@�R@��@V@@p�@?}@�@V@�@�/@��@�@�D@j@Z@9X@1@�F@�H@�\@n�@^5@M�@=q@-@��@��@X@�@��@r�@��@�P@\)@�@��@ȴ@V@E�@E�@�T@��@?}@?}@�@z�@j@z�@z�@Z@9X@(�@�@�m@ƨ@�F@�@��@C�@"�@"�@"�@��@~�@^5@�@��@�@J@J@��@�@�@�^@��@��@��@G�@�`@�9@��@�u@�u@�u@�@�@bN@�;@��@|�@;d@�@�@��@�@�@�R@��@ff@V@$�@@��@p�@�@��@�@j@9X@�@1@1@1@�
@�F@C�@	��@	�7@	%@�@�w@ȴ@$�@��@��@�F@33@�!@M�@x�@ ��@ A�@  �@   @   ?�|�?�v�?�{?�O�?�(�?��H?���?��^?�x�?���?�+?��+?��T?�z�?���?���?�z�?�9X?�t�?�S�?�J?�G�?�Ĝ?�v�?�{?�5??�5??�{?���?��m?�?�?�?���?��#?�Q�?�K�?�+?�+?�ff?�E�?�?�?�`B?���?�F?�n�?�J?�-?��`?�  ?��?޸R?ޗ�?�V?�V?��?���?�O�?�j?�(�?�ƨ?�?���?��?���?ش9?�l�?�
=?�?���?Լj?�9X?�S�?��?�-?ѩ�?У�?У�?Ѓ?�bN?�|�?θR?�V?���?͑h?�O�?̬?�I�?˥�?˅?ʟ�?�=q?ə�?�x�?��?�7L?���?�r�?�1'?���?�+?�ȴ?�`B?ě�?���?���?��
?Õ�?�S�?�33?���?�-?�G�?�Ĝ?�A�?��;?��w?���?���?�\)?��?��R?���?���?���?�v�?��?���?���?��h?��h?��h?��h?��h?��-?��-?���?���?��-?��h?�p�?�p�?�V?�V?�V?�/?�/?�/?�V?���?���?��?���?��?��?��?��?���?��?��?��?��?�j?�j?�(�?�(�?��m?�1?�1?�(�?�I�?�I�?�j?�j?�I�?�j?�1?�(�?�(�?�(�?�(�?�I�?�1?�ƨ?��m?��m?��m?��m?��m?��m?�1?�(�?�I�?�j?�j?�j?�j?��D?��D?��D?��?���?��?��?�V?�O�?�O�?�O�?�O�?�p�?��h?�p�?�p�?�O�?�p�?�p�?�p�?��-?��-?��-?���?��-?��?���?�{?�5??�{?�{?�{?���?���?��?���?��?�5??�5??�{?�5??�V?�5??�5??���?��R?��R?��R?��?���?��?��?��?��?���?��?���?��R?��R?��R?��R?�V?�V?�V?�5??�5??��?�{?�5??�5??�V?�V?�v�A�=qA�9XA�A�A�C�A�C�A�9XA�;dA�?}A�C�A�G�A�Q�A�I�A�O�A�I�A�K�A�Q�A�=qA�E�A�A�A�?}A�?}A�C�A�A�A�A�A�C�A�G�A�G�A�K�A�S�A�M�A�M�A�XA�\)A�ZA�bNA�`BA�bNA�bNA�bNA�dZA�`BA�bNA�dZA�bNA�dZA�bNA�`BA�`BA�`BA�`BA�`BA�`BA�`BA�bNA�^5A�^5A�`BA�bNA�hsA�hsG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         A�C�A�C�A�M�A�Q�A�^5A�bNA�dZA�bNA�dZA�dZA�`BA�`BA�`BA�ffA�hsA�ffA�ffA�hsA�hsA�jA�l�A�n�A�n�A�r�A�t�AׁAׁA�|�AׁA׉7A׉7AבhAׅA�|�AׁAׇ+Aׇ+AׅA׉7Aׇ+A׃A׃A׋DA״9Aة�A�9XA�E�A��A��A�JA�|�A�E�A�(�A���A�"�A�S�A�ZA�
=A���A�$�A���A�(�A���A��^A�"�A�  A�r�A�9XA�+A� �A��#A�
=A��+A���A��A�ȴA��DA�x�A�M�A��\A���A���A�{A���A�Q�A���A�%A���A��FA�G�A��
A��yA���A�G�A�A��mA���A��A�=qA�%A���A��RA��A���A��hA�=qA��HA��\A��A�ffA�G�A�t�A�ĜA�S�A���Ay�FAv{Atz�AqXAm�mAl�`Ak%Ah1AfJAe\)Ae
=Ad�RAcl�A^�\AYoAV(�AT-AP��AN1AI��AE;dAB~�AA�A@�RA=VA9A7t�A6 �A4��A37LA1�A0{A.-A,1'A*��A)�7A(�A(�\A(n�A(E�A(JA'�A'��A%A"9XA!�A�\A�A~�A%AZA�7A�jAVA�A��A�A�A��A�PA�A�A/A
��A	�mA	t�A	+A��A��AA?}A�jA�-A��A1'A�PA��An�A��A��A"�A Q�@���@��y@��@��j@�K�@�/@�|�@�x�@�A�@�v�@�G�@�9X@�@��@�7@�M�@�@�33@�dZ@�l�@�\)@�v�@���@��-@���@��F@�1'@� �@��D@�&�@��@���@��@�V@�/@��9@��m@�\)@�~�@�J@�@�  @��@@�7@��@웦@�V@�J@�M�@���@�Ĝ@��m@�F@땁@��
@�C�@�=q@�?}@� �@�~�@�j@�@�K�@�;d@�"�@�`B@�r�@�+@�x�@���@ۅ@���@��y@ڗ�@�-@٩�@�%@ؓu@�1@�K�@֗�@ա�@���@�j@��@Ӿw@��@���@�@�"�@�n�@щ7@�A�@�M�@�`B@���@��@���@���@���@�v�@���@Ͳ-@́@�/@�z�@�"�@��@�p�@�Ĝ@�I�@� �@Ǖ�@Ɨ�@���@�\)@�33@�33@�33@�
=@�^5@�J@���@�G�@��@�Ĝ@�1'@���@�t�@�dZ@�;d@�
=@��!@�~�@��@��T@���@���@�@��-@��@�p�@��@��j@�j@�  @�;d@�@���@�E�@��-@�hs@�G�@�&�@�V@��@��9@���@��D@�r�@�I�@�1@��w@�33@��\@�=q@�J@��T@�hs@�Ĝ@���@�j@���@��@�\)@�
=@���@�5?@��@�hs@��9@�j@��
@�K�@�+@��H@���@�M�@�5?@��#@�O�@�V@���@�Q�@��@�|�@�;d@�+@�"�@�
=@�ff@�@��7@�hs@�V@�Ĝ@���@�r�@� �@���@��@�33@�n�@���@��@�7L@�Ĝ@���@�Ĝ@�r�@�A�@� �@��
@���@�C�@���@�ȴ@���@���@���@���@��T@��^@���@�x�@�/@���@�r�@�bN@�Z@�I�@� �@�ƨ@���@��P@�C�@��H@�ȴ@�V@��#@�x�@��7@�p�@�`B@�G�@���@�z�@�A�@�  @���@��w@�o@�^5@���@�`B@��@�z�@��@��
@���@��@���@�n�@�M�@�@��-@��-@�X@�/@���@�Q�@�  @��
@�ƨ@���@��
@��@��P@�|�@���@��T@��h@�p�@�G�@��@��@��D@�Q�@�Q�@�Q�@�Q�@�Q�@�1'@� �@�b@��w@�dZ@�C�@�"�@�o@��y@���@��\@�$�@�@�?}@���@��@�j@�Z@�(�@�b@��m@���@�o@���@�J@��@��7@�G�@�&�@�V@���@��@���@��u@�S�@�
=@��@���@��\@�^5@�5?@���@���@�@��h@�hs@�G�@���@��j@���@��D@�A�@��m@���@�l�@�o@���@��@��!@���@��+@�^5@�J@��-@���@��h@�x�@�p�@�`B@��@���@�Ĝ@��D@�r�@�Z@�I�@��@��@�P@\)@~E�@}�-@}V@|�@|I�@{�@z=q@y&�@x1'@xĜ@w�@wK�@v��@v��@vv�@u?}@t�/@t�@t��@tj@s��@s33@r�@q��@q&�@p��@p�@o�@o��@o|�@o\)@o
=@nE�@m`B@m/@mV@l�/@k�m@k"�@j��@j^5@i�#@i��@ix�@h��@h�u@h�@hQ�@g�@g�@g�@g�@fȴ@fv�@fV@f{@e��@e�-@e�h@ep�@e�@d�@d�@d�/@d�/@d�j@d�@c��@cdZ@cC�@co@c@b��@b~�@a�#@a7L@`�`@_�@^��@^�@^��@^�+@^E�@^5?@]�@]��@]��@]?}@]V@\��@\j@[ƨ@[�@[S�@[o@Z�@Z��@Y��@Y�@X��@XĜ@X�9@X�9@Xr�@X �@W�;@W�w@W��@V�@V5?@U�@U�T@U�-@U�@UV@T�j@TI�@T�@SC�@Q�#@Q7L@P�`@P��@P��@PQ�@P1'@O�;@O|�@OK�@N�y@N�R@Nv�@NE�@M��@M`B@M/@L�j@Lz�@LI�@L9X@L(�@K��@K@J�H@J��@J��@J�@I%@H�9@H1'@G|�@G\)@G;d@G+@G;d@G;d@G;d@G+@G�@F��@F�y@F�R@F@E�-@Ep�@E?}@E/@E/@E?}@EO�@EO�@EO�@E`B@E`B@E`B@E?}@E�@D�/@DZ@D1@C�
@Cƨ@Ct�@C@B�!@B=q@A��@A&�@@�9@@1'@?K�@>ff@>$�@=�-@=�-@=O�@=�@=�@=V@<��@<(�@;�F@;��@;t�@:�!@9�#@9��@8Ĝ@8 �@7l�@6ȴ@6$�@5��@5`B@5�@4��@4��@4��@4�/@4��@4z�@4�@3�F@3��@3dZ@3C�@2�@1�@1��@1��@1hs@0�`@/��@/�@/|�@/+@.��@.V@.E�@.@-��@-�T@-�-@-�@-�@-p�@-�@,�@,�@+�
@+��@+dZ@+dZ@+C�@+33@+33@*�H@*�!@*n�@*-@*�@)�@)��@)7L@)�@(��@(��@(��@(�u@(�u@(��@(�@(r�@(bN@(Q�@( �@(b@(  @'�w@'|�@'l�@'\)@'\)@'K�@'\)@'+@'
=@%��@%/@$�@$��@$�D@#ƨ@#��@#t�@#dZ@#C�@#@#@"�@"�@"�H@"�\@"^5@"�@!��@!��@!x�@!7L@!%@ �u@;d@ȴ@�R@��@V@@p�@?}@�@V@�@�/@��@�@�D@j@Z@9X@1@�F@�H@�\@n�@^5@M�@=q@-@��@��@X@�@��@r�@��@�P@\)@�@��@ȴ@V@E�@E�@�T@��@?}@?}@�@z�@j@z�@z�@Z@9X@(�@�@�m@ƨ@�F@�@��@C�@"�@"�@"�@��@~�@^5@�@��@�@J@J@��@�@�@�^@��@��@��@G�@�`@�9@��@�u@�u@�u@�@�@bN@�;@��@|�@;d@�@�@��@�@�@�R@��@ff@V@$�@@��@p�@�@��@�@j@9X@�@1@1@1@�
@�F@C�@	��@	�7@	%@�@�w@ȴ@$�@��@��@�F@33@�!@M�@x�@ ��@ A�@  �@   @   ?�|�?�v�?�{?�O�?�(�?��H?���?��^?�x�?���?�+?��+?��T?�z�?���?���?�z�?�9X?�t�?�S�?�J?�G�?�Ĝ?�v�?�{?�5??�5??�{?���?��m?�?�?�?���?��#?�Q�?�K�?�+?�+?�ff?�E�?�?�?�`B?���?�F?�n�?�J?�-?��`?�  ?��?޸R?ޗ�?�V?�V?��?���?�O�?�j?�(�?�ƨ?�?���?��?���?ش9?�l�?�
=?�?���?Լj?�9X?�S�?��?�-?ѩ�?У�?У�?Ѓ?�bN?�|�?θR?�V?���?͑h?�O�?̬?�I�?˥�?˅?ʟ�?�=q?ə�?�x�?��?�7L?���?�r�?�1'?���?�+?�ȴ?�`B?ě�?���?���?��
?Õ�?�S�?�33?���?�-?�G�?�Ĝ?�A�?��;?��w?���?���?�\)?��?��R?���?���?���?�v�?��?���?���?��h?��h?��h?��h?��h?��-?��-?���?���?��-?��h?�p�?�p�?�V?�V?�V?�/?�/?�/?�V?���?���?��?���?��?��?��?��?���?��?��?��?��?�j?�j?�(�?�(�?��m?�1?�1?�(�?�I�?�I�?�j?�j?�I�?�j?�1?�(�?�(�?�(�?�(�?�I�?�1?�ƨ?��m?��m?��m?��m?��m?��m?�1?�(�?�I�?�j?�j?�j?�j?��D?��D?��D?��?���?��?��?�V?�O�?�O�?�O�?�O�?�p�?��h?�p�?�p�?�O�?�p�?�p�?�p�?��-?��-?��-?���?��-?��?���?�{?�5??�{?�{?�{?���?���?��?���?��?�5??�5??�{?�5??�V?�5??�5??���?��R?��R?��R?��?���?��?��?��?��?���?��?���?��R?��R?��R?��R?�V?�V?�V?�5??�5??��?�{?�5??�5??�V?�V?�v�A�=qA�9XA�A�A�C�A�C�A�9XA�;dA�?}A�C�A�G�A�Q�A�I�A�O�A�I�A�K�A�Q�A�=qA�E�A�A�A�?}A�?}A�C�A�A�A�A�A�C�A�G�A�G�A�K�A�S�A�M�A�M�A�XA�\)A�ZA�bNA�`BA�bNA�bNA�bNA�dZA�`BA�bNA�dZA�bNA�dZA�bNA�`BA�`BA�`BA�`BA�`BA�`BA�`BA�bNA�^5A�^5A�`BA�bNA�hsA�hsG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	ZB	ZB	ZB	YB	XB	W
B	XB	YB	YB	YB	YB	YB	YB	YB	XB	W
B	W
B	W
B	W
B	W
B	W
B	W
B	W
B	W
B	XB	[#B	[#B	[#B	[#B	\)B	^5B	_;B	[#B	ZB	ZB	[#B	YB	YB	ZB	YB	XB	YB	]/B	t�B	�sB
$�B
=qB
L�B
Q�B
A�B
>wB
7LB
7LB
8RB
5?B
<jB
W
B
aHB
hsB
t�B
z�B
�JB
�3B
��B
�
B
�B
��B
��B
��B
��B%B\B)�BF�BL�BJ�BJ�BC�B^5Bv�Bx�B�B�uB��B��B��B��B��B�VB�=B�By�B}�By�BdZB`BBO�B<jB.BB
�TB
�/B
B
�?B
ƨB
��B
��B
�-B
�%B
{�B
aHB
:^B
#�B
B	��B	��B	�hB	�1B	�%B	�B	�B	u�B	ffB	ZB	W
B	T�B	Q�B	I�B	'�B	VB	  B�B�)B�B�}B�-B��B��B��B�PB��B��B��B��B��B�dB��B��BȴB�LB�!B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B�JB|�Bo�Bm�Bq�Bl�Bk�BhsBgmBhsBhsBk�Br�Bx�Bz�B~�B�B�B�B�B�B�1B�B�%B�B�B�B�B�B�B�B�B�B�%B�%B�+B�JB�JB�PB��B��B��B�!B�B�/B��B	B	+B		7B	
=B	%B	B	DB	�B	�B	"�B	/B	8RB	>wB	J�B	L�B	Q�B	T�B	[#B	e`B	hsB	jB	k�B	l�B	jB	e`B	YB	W
B	T�B	VB	YB	`BB	iyB	o�B	p�B	q�B	t�B	t�B	u�B	{�B	z�B	|�B	{�B	z�B	v�B	v�B	u�B	t�B	v�B	w�B	t�B	s�B	s�B	s�B	r�B	t�B	x�B	|�B	�B	�B	�1B	�7B	�7B	�7B	�%B	�B	�B	�%B	�+B	�PB	�VB	�bB	�bB	�oB	��B	��B	��B	�uB	�bB	�hB	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�B	�B	�-B	�LB	�9B	�?B	�9B	�3B	�-B	�B	��B	��B	�B	�B	�B	�B	�-B	�?B	�9B	�FB	�RB	�XB	�dB	�wB	�}B	�wB	�}B	�}B	��B	��B	B	ÖB	ÖB	ÖB	ÖB	ĜB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�
B	�B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�;B	�;B	�;B	�HB	�ZB	�NB	�fB	�mB	�sB	�mB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
+B
+B
+B
+B
1B
1B
1B
1B
	7B
1B

=B

=B

=B
	7B

=B

=B
DB
DB
DB
JB
DB
JB
JB
JB
JB
JB
PB
VB
\B
bB
hB
hB
hB
oB
hB
hB
hB
hB
hB
hB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
%�B
'�B
#�B
%�B
&�B
&�B
&�B
%�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
+B
+B
+B
+B
,B
,B
,B
,B
,B
-B
-B
/B
.B
/B
/B
/B
0!B
0!B
1'B
2-B
2-B
2-B
33B
33B
33B
33B
33B
49B
49B
5?B
49B
5?B
5?B
5?B
7LB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
B�B
C�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
G�B
H�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
H�B
H�B
H�B
H�B
I�B
J�B
J�B
J�B
I�B
I�B
J�B
I�B
J�B
I�B
J�B
J�B
J�B
K�B
J�B
K�B
K�B
K�B
K�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
Q�B
R�B
R�B
Q�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
S�B
T�B
T�B
T�B
VB
T�B
VB
W
B
XB
XB
XB
ZB
ZB
ZB
[#B
\)B
\)B
\)B
[#B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
_;B
^5B
^5B
_;B
^5B
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
aHB
cTB
cTB
cTB
dZB
cTB
dZB
cTB
cTB
cTB
dZB
cTB
e`B
e`B
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
jB
iyB
jB
iyB
iyB
jB
iyB
jB
iyB
iyB
jB
jB
jB
k�B
k�B
jB
k�B
k�B
k�B
l�B
m�B
m�B
m�B
m�B
n�B
o�B
o�B
n�B
o�B
o�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
t�B
u�B
v�B
v�B
u�B
v�B
w�B
w�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
y�B
y�B
x�B
x�B
y�B
y�B
y�B
y�B
z�B
y�B
y�B
z�B
y�B
z�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
z�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
}�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
�B
� B
� B
�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�1B
�1B
�7B
�7B
�=B
�DB
�DB
�DB
�DB
�JB
�JB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�\B
�bB
�hB
�hB
�uB
�oB
�oB
�{B
�{B
�{B
�{B
��B
��B
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
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�-B
�-B
�-B
�-B
�-B
�-B
�-B
�3B
�3B
�9B
�9B
�9B
�?B
�?B
�?B
�?B
�?B
�FB
�FB
�LB
�FB
�FB
�FB
�LB
�LB
�LB
�RB
�LB
�LB
�RB
�RB
�RB
�RB
�XB
�RB
�XB
�RB
�XB
�XB
�XB
�XB
�XB
�XB
�^B
�^B
�XB
�^B
�XB
�XB
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�dB
�dB
�^B
�dB
�dB
�jB
�dB
�jB
�dB
�dB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�dB
�dB
�jB
�jB
�jB
�jB
�jB
�jB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�wB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�wB
�wB
�qB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�}B
�}B
�}B
�wB
�wB
�wB
�}B
�}B
�}B
�wB
�}B
�}B
��B
�}B
�}B
��B
�}B
��B
�}B
��B
�}B
�}B
��B
�}B
��B
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
B
��B
��B
��B
��B
B
B
B
B
B
B
��B	[#B	\)B	[#B	[#B	XB	[#B	[#B	[#B	\)B	YB	ZB	ZB	ZB	[#B	[#B	XB	[#B	YB	ZB	ZB	[#B	ZB	ZB	ZB	[#B	ZB	ZB	[#B	YB	YB	ZB	YB	XB	XB	W
B	W
B	W
B	XB	XB	XB	YB	YB	XB	YB	YB	YB	YB	YB	YB	YB	YB	YB	YB	YB	YB	ZB	ZB	ZB	XB	XG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         B	VB	VB	VB	T�B	S�B	R�B	S�B	T�B	T�B	T�B	T�B	T�B	T�B	T�B	S�B	R�B	R�B	R�B	R�B	R�B	R�B	R�B	R�B	R�B	S�B	W
B	W
B	W
B	W
B	XB	ZB	[#B	W
B	VB	VB	W
B	T�B	T�B	VB	T�B	S�B	T�B	YB	p�B	�ZB
 �B
9XB
H�B
M�B
=qB
:^B
33B
33B
49B
1'B
8RB
R�B
]/B
dZB
p�B
v�B
�1B
�B
ƨB
��B
�B
��B
��B
��B
��BBDB%�BB�BH�BF�BF�B?}BZBr�Bt�B~�B�\B��B��B��B�uB�hB�=B�%B|�Bu�By�Bu�B`BB\)BK�B8RB)�BB
�;B
�B
�wB
�'B
B
��B
��B
�B
�B
w�B
]/B
6FB
�B	��B	��B	��B	�PB	�B	�B	|�B	~�B	q�B	bNB	VB	R�B	P�B	M�B	E�B	#�B	
=B��B�B�B��B�dB�B��B��B��B�7B��B��B��B��B��B�LB��BɺBĜB�3B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�1Bx�Bk�BiyBm�BhsBgmBdZBcTBdZBdZBgmBn�Bt�Bv�Bz�B|�B|�B|�B�B�B�B�B�B�B�B� B� B� B�B�B�B�B�B�B�B�1B�1B�=B�hB��B��B�B�B�B�B	B	B	%B	%B	B	B	1B	{B	�B	�B	,B	5?B	;dB	G�B	I�B	N�B	Q�B	XB	bNB	e`B	gmB	hsB	iyB	gmB	bNB	VB	S�B	Q�B	R�B	VB	]/B	ffB	l�B	m�B	n�B	q�B	q�B	r�B	x�B	w�B	y�B	x�B	w�B	s�B	s�B	r�B	q�B	s�B	t�B	q�B	p�B	p�B	p�B	o�B	q�B	u�B	y�B	}�B	~�B	�B	�%B	�%B	�%B	�B	�B	�B	�B	�B	�=B	�DB	�PB	�PB	�\B	�{B	�{B	�uB	�bB	�PB	�VB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�9B	�'B	�-B	�'B	�!B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�-B	�'B	�3B	�?B	�FB	�RB	�dB	�jB	�dB	�jB	�jB	�qB	�qB	�}B	��B	��B	��B	��B	��B	ÖB	ĜB	ŢB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�)B	�)B	�)B	�5B	�HB	�;B	�TB	�ZB	�`B	�ZB	�fB	�fB	�fB	�mB	�mB	�sB	�sB	�sB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
B
+B
+B
+B
%B
+B
+B
1B
1B
1B
	7B
1B
	7B
	7B
	7B
	7B
	7B

=B
DB
JB
PB
VB
VB
VB
\B
VB
VB
VB
VB
VB
VB
bB
bB
bB
hB
hB
hB
oB
oB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
$�B
 �B
"�B
#�B
#�B
#�B
"�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
,B
+B
,B
,B
,B
-B
-B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
1'B
2-B
2-B
2-B
49B
33B
49B
49B
49B
49B
49B
49B
49B
49B
49B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
8RB
8RB
8RB
:^B
;dB
;dB
;dB
;dB
;dB
;dB
<jB
;dB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
@�B
A�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
E�B
F�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
F�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
G�B
G�B
H�B
G�B
H�B
G�B
H�B
H�B
H�B
I�B
H�B
I�B
I�B
I�B
I�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
O�B
P�B
P�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
Q�B
R�B
R�B
R�B
S�B
R�B
S�B
T�B
VB
VB
VB
XB
XB
XB
YB
ZB
ZB
ZB
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
]/B
\)B
\)B
]/B
\)B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
_;B
aHB
aHB
aHB
bNB
aHB
bNB
aHB
aHB
aHB
bNB
aHB
cTB
cTB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
ffB
ffB
hsB
gmB
hsB
gmB
gmB
hsB
gmB
hsB
gmB
gmB
hsB
hsB
hsB
iyB
iyB
hsB
iyB
iyB
iyB
jB
k�B
k�B
k�B
k�B
l�B
m�B
m�B
l�B
m�B
m�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
r�B
s�B
t�B
t�B
s�B
t�B
u�B
u�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
v�B
v�B
w�B
w�B
w�B
w�B
x�B
w�B
w�B
x�B
w�B
x�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
x�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
{�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
~�B
}�B
}�B
~�B
}�B
~�B
� B
�B
� B
�B
�B
�B
�B
�B
�B
�%B
�%B
�+B
�+B
�1B
�7B
�7B
�7B
�7B
�=B
�DB
�JB
�JB
�JB
�PB
�PB
�PB
�PB
�PB
�VB
�\B
�bB
�bB
�oB
�hB
�hB
�uB
�uB
�uB
�uB
�{B
�{B
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
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�-B
�-B
�-B
�-B
�-B
�-B
�-B
�3B
�3B
�9B
�9B
�9B
�?B
�?B
�?B
�?B
�?B
�FB
�FB
�LB
�FB
�FB
�FB
�LB
�LB
�LB
�RB
�LB
�LB
�RB
�RB
�RB
�RB
�XB
�RB
�XB
�RB
�XB
�XB
�XB
�XB
�XB
�XB
�^B
�^B
�XB
�^B
�XB
�XB
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�dB
�dB
�^B
�dB
�dB
�jB
�dB
�jB
�dB
�dB
�jB
�qB
�qB
�qB
�qB
�qB
�qB
�jB
�jB
�qB
�qB
�qB
�qB
�qB
�qB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�}B
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�}B
�}B
�wB
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
��B
��B
��B
�}B
�}B
�}B
��B
��B
��B
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
��B
��B
��B
��B
��B
��B
B
��B
��B
��B
B
B
B
��B
B
ĜB
ÖB
ÖB
ÖB
ÖB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ÖB	W
B	XB	W
B	W
B	S�B	W
B	W
B	W
B	XB	T�B	VB	VB	VB	W
B	W
B	S�B	W
B	T�B	VB	VB	W
B	VB	VB	VB	W
B	VB	VB	W
B	T�B	T�B	VB	T�B	S�B	S�B	R�B	R�B	R�B	S�B	S�B	S�B	T�B	T�B	S�B	T�B	T�B	T�B	T�B	T�B	T�B	T�B	T�B	T�B	T�B	T�B	T�B	VB	VB	VB	S�B	S�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201912220800422021061413564020210614135640202106141918392021061419183920210614191839201912220800422021061413564020210614135640202106141918392021061419183920210614191839PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0.0002), vertically averaged dS = -0.004 (+/-0.006)                                                                                                                                                                                           surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0.0002), vertically averaged dS = -0.004 (+/-0.006)                                                                                                                                                                                           Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2019122208004220191222080042  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019122208004220191222080042QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019122208004220191222080042QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713152720210617131527IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                