CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  	   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-12-12T12:00:25Z creation      
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
resolution        =���   axis      Z        (H  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
  d(   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (H  n<   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (H  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (H  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
  �(   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (H  �<   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 #�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (H -�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (H U�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 ~(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (H �<   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (H ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   H   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   P   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   X   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   `   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � h   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   	   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    	   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        	,   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        	4   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       	<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    	D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �      � �Argo profile    3.1 1.2 19500101000000  20191212120025  20210617131526  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               Z   ZDD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @���}Xy1@���}Xy111  @���}'��@���}'��@2s�[-M@@2s�[-M@�cr�n���cr�n��11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?���@ff@Fff@�  @�33@�33@�33A��A��A(  AA��A`  A���A���A���A���A�ffA���A�33A�  B ffBffB��BffB ��B(  B0  B8��B@ffBHffBPffBX  B`  Bh��BpffBxffB�ffB�  B�  B�33B�33B�ffB�ffB�33B�33B�33B�ffB�ffB�  B�33B�ffB�  B���B�33B�ffB���B�  Bә�B���B�33B���B���B���B�ffB�33B���B�ffB���C 33C�CL�C33C  C
33C�C��C  C�CL�C�C  C�CL�C�C��C"  C$33C&  C'��C*  C,33C.  C/��C2  C4L�C6�C8  C:L�C<33C>�C@�CB  CDL�CF33CH  CJL�CL�CN  CPL�CR  CS�fCV33CX�CZ  C\33C^L�C`�Cb  Cd�Ce�fCg�3Ci�fCl�Cn33Cp  Cq�fCt�Cv33Cx�Cy��C|  C~L�C�&fC��C�  C��3C��3C��C��C�  C�&fC��C�  C�&fC��C��C�  C�  C�  C��3C��C��C��C��C��C��C�  C�  C��3C��C�  C��fC�  C��C�&fC��C��fC�  C��C�&fC��C��fC�  C��C��C�&fC��C��fC�  C�  C��C�&fC�&fC�&fC�&fC�33C��C�ٚC�ٚC��fC��C�33C��C��C�&fC�&fC�&fC��C�&fC��C��C�  C�  C��3C��3C��fC��C�  C��C�&fC��C��C��C��C�  C�  C�  C��3C��3C��3C��C��C�  C�&fC��C��C�  C�  C��3C��3C��3C��fC��fC��fC��fC��fC�ٚC��C��C�  C�33C�  C��fC��fC��3C��3C��C��C�  C��fC��3C�  C��C�&fC��C��fC�  C��C��C�33C��fD �fD�Dl�D  D�fD�fDs3D  D��D�Dy�DfD�3D��D��D�Dy�D	fD	��D	�3D
� DfD�3D�Dy�D  D��D� Dl�D�3D� DfD�3D�Ds3D��Dy�D��Dy�DfD��D�D��D�D��D��Dy�D  D�fDfD��D�D��D�Dl�D�3Ds3D�3Dy�D  D� DfD�fD fD ��D!�D!�3D"3D"�3D"��D#s3D#�3D$s3D$�3D%y�D%��D&y�D&��D'y�D(  D(��D)�D)ffD)�3D*s3D*��D+y�D+��D,� D-  D-y�D.fD.��D/fD/�fD0�D0��D13D1�3D2�D2��D2�3D3s3D3��D4y�D5  D5� D6fD6�fD7�D7�3D83D8��D9  D9s3D:fD:��D:��D;y�D<  D<��D=3D=s3D=��D>� D?�D?�3D?��D@y�D@��DA�fDB�DBl�DB�3DCy�DDfDD��DE�DE� DF  DF�fDGfDG�3DH3DHs3DH��DIy�DJ  DJ� DKfDK�fDL3DL�3DM3DM�3DM�fDNl�DN��DOy�DO��DP�fDQ  DQ�fDQ��DR� DR��DS� DT  DT� DUfDU�fDV  DV�fDWfDW�fDXfDX��DY�DY�3DZ�DZs3DZ��D[y�D\  D\� D]  D]� D^fD^�3D_�D_��D`3D`��D`�3Day�Da��Dby�Db��Dc�fDdfDd��De�De��Df3Df�3Dg  Dgs3Dg��Dhs3Di  Di� DjfDj�fDk�Dk��Dl�Dl��Dm3Dm�3Dn3Dn��Dn��Dos3Do�3Dps3Dp�3Dqy�Dq��Dry�Dr��Ds� Ds��Dty�Du  Duy�Du��Dv� Dw  Dw� DxfDx�fDyfDy��Dz�Dz��D{3D{l�D{�3D|l�D|�3D}y�D}��D~y�D  D� D�3D�C3D��fD��fD�	�D�I�D�vfD���D���D�<�D�� D�� D�3D�C3D��3D��fD�	�D�33D�y�D���D�  D�C3D��fD�ɚD���D�@ D��fD��3D���D�@ D��fD���D���D�@ D��3D��3D���D�<�D��3D��fD��fD�@ D��3D��3D���D�@ D��fD���D���D�C3D��fD��fD�  D�FfD���D�� D�  D�FfD���D���D�  D�@ D��fD���D���D�C3D��3D��fD���D�@ D��fD�ɚD���D�@ D��fD��3D���D�@ D��fD�ɚD��fD�@ D��fD��fD� D�<�D��fD���D��fD�@ D��3D�ɚD��D�<�D�� D��fD�	�D�L�D�y�D���D�  D�C3D���D�ɚD� D�<�D�� D�� D�3D�FfD���D�� D���D�<�D�|�D��3D�3D�FfD��fD���D��D�6fD�|�D���D���D�9�D�|�D���D���D�9�D�|�D���D���D�<�D�� D���D�  D�<�D�|�D���D���D�<�D�y�D���D���D�9�D�y�D���D�	�D�I�D��fD��fD�3D�C3D�|�D���D���D�6fD���D�ɚD�	�D�FfD��3D���D�  D�<�D�y�D���D�fD�C3D�� D���D��D�I�D��3D���D���D�I�D��fD�� D���D�9�D���D��3D�  D�L�D���D��3D���D�6fD��fD�� D���D�FfD�� D���D�fD�<�D�vfD��3D��D�FfD�|�D�ɚD���D�9�D��3D�� D�3D�<�D���D�� D���D�FfD���D��fD���D�FfD�� D��3D���D�FfD�� D��fD���D�I�D�� D��fD�  D�I�D��3D���D�fD�<�D�vfD��3D��D�C3D�|�D��fD�  D�9�D��fD���D��fD�FfD�|�D���D�3D�<�D�vfD��3D���D�6fD�� D���D�	�D�@ D�� D�ɚD�fD�@ D�y�D�ɚD�3D�9�D�D��fD�  D�9�D�y�D��fD�  D�<�D�|�DĶfD�	�D�C3Dŀ Dż�D���D�L�DƆfD��fD�  D�C3D�y�DǼ�D��D�FfDȆfD�� D�  D�<�D�y�D�ɚD�fD�FfDʃ3D��fD�  D�@ D�|�D˶fD�	�D�FfD̆fD��3D�3D�<�D�y�D���D�	�D�FfDΆfD��3D�  D�@ Dό�D�ɚD�fD�@ DЀ D�� D���D�9�D�y�D�ɚD�fD�I�D҃3D��3D�  D�@ D�y�DӶfD�	�D�C3Dԃ3DԼ�D���D�6fD�s3D��3D�3D�@ D�y�DֶfD�fD�C3D׃3D׼�D���D�L�D؆fD��3D�3D�<�D�|�Dٹ�D��D�I�DچfD��fD�  D�@ D�|�D���D�	�D�I�D܆fD��3D���D�6fD݌�D�ɚD�fD�C3Dހ D޼�D�  D�L�D߉�D�ɚD�fD�C3D�� D�� D���D�9�DቚD��3D�  D�<�D�|�D�ɚD�fD�FfD� D��D��D�FfD�fD�� D���D�L�D�fD��3D�  D�6fD扚D��fD�  D�L�D牚D��3D���D�6fD� D�� D�fD�@ D鉚D��3D���D�I�D�3D깚D�3D�<�D�vfD��D�	�D�C3D�y�D��fD� D�FfD�|�D�ɚD�3D�9�D�fD�� D��fD�C3D��D��3D���D�FfD�� D�fD�3D�L�D�3D��D�fD�@ D�y�D��3D�	�D�C3D�y�D��3D��D�@ D�y�D���D�	�D�P D��3D���D�3D�I�D���D��3D���D�C3D��fD�� D���D�6fD�� D��3D�	�D�P D��3D���D� D���D�  D�y�D��3D��3D�� D���E ^fE � E[3EٚES3EVfE��EI�E��E�fE8 E�3E� E&fE��E	  E
#3E
�fE3E!�E�3E�E��E�3E�E��E�E E��E3E��E�fEfE��E E	�E�3E3E��E��E�E��E�E�E�3EfE��E��E!�E�fE+3E�3E ɚE!NfE!��E"` E"�E#q�E$�3E%�E%��E& E&�3E'+3E(H E(� E)X E)��E*p E*��E+� E,�fE-#3E-��E.4�E.��E/FfE0X E0�fE1^fE1� E2q�E3� E4�E4��E5�E5��E6#3E79�E7�fE8I�E8� E9S3E:ffE:��E;i�E;�E<l�E=s3E=�E>s3E>�3E@  E@{3E@�3EA|�EA�3EB�fEC{3EC�3EDy�EE{3EE�3EFs3EGnfEG�EHffEH�3EI�fEJT�EJ� EK��EL4�EL�3EM� EN EN|�EOffEO� EP�3EQ33EQ��ER�3ER��ESٚETFfET��EU� EV�EV�EWNfEX,�EX� EY|�EY�fEZT�E[8 E[�3E\� E\�fE]�3E^( E^��E_a�E`9�E`�fEavfEa��Eb�fEc�Ec��Ed` Ee<�Ee�fEf�3Ef�3Eg� Eh33Ei�Eis3EjI�Ej��Ek��Ek�fEl� Em@ En�En��Eoc3EoɚEp6fEq Eqy�ErQ�Er��Es� Es��EtɚEu��Ev3Ev�fEw9�Ex Exs3EyD�Ey��Ezx Ez�fE{��E|fE|�fE}��E~3E~�3EFfE� E�<�E�� E� E�8 E���E��3E�-�E�� E���E�3E�q�E�� E��3E�S3E��3E��E�W3E���E��3E�'3E�y�E��3E�fE�k3E���E��E�]�E���E���E�M�E���E��3E�9�E���E��3E�&fE��fE��fE�2fE�3E���E��E�c3E���E�� E�h E��3E���E�H E���E���E�)�E�vfE�ŚE�6fE���E���E�3E�d�E���E��3E�d E�� E��E�8�E���E��E�( E��fE���>L��>���>L��>���>L��>L��>���>���>���>���>���>���>���>���?   >L��>���>���>���>���?   ?   ?333?L��?�  ?�33?�33?�ff@   @ff@��@,��@9��@L��@Y��@l��@�  @���@���@�33@���@�ff@�  @�  @���@陚@���A��A  A  A  AffA(  A0  A6ffA>ffAD��ANffAT��A\��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141441414444414141414111141111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ?L��?���@&ff@fff@�  @�33@�33@�33A	��A��A0  AI��Ah  A���A���A���A���A�ffA���A�33A�  BffB
ffB��BffB"��B*  B2  B:��BBffBJffBRffBZ  Bb  Bj��BrffBzffB�ffB�  B�  B�33B�33B�ffB�ffB�33B�33B�33B�ffB�ffB�  B�33B�ffB�  B���B�33B�ffB���B�  Bԙ�B���B�33B���B���B���B�ffB�33B���B�ffB���C �3C��C��C�3C� C
�3C��CL�C� C��C��C��C� C��C��C��C L�C"� C$�3C&� C(L�C*� C,�3C.� C0L�C2� C4��C6��C8� C:��C<�3C>��C@��CB� CD��CF�3CH� CJ��CL��CN� CP��CR� CTffCV�3CX��CZ� C\�3C^��C`��Cb� Cd��CfffCh33CjffCl��Cn�3Cp� CrffCt��Cv�3Cx��CzL�C|� C~��C�ffC�Y�C�@ C�33C�33C�Y�C�L�C�@ C�ffC�Y�C�@ C�ffC�Y�C�L�C�@ C�@ C�@ C�33C�Y�C�Y�C�Y�C�Y�C�Y�C�L�C�@ C�@ C�33C�L�C�@ C�&fC�@ C�Y�C�ffC�Y�C�&fC�@ C�L�C�ffC�L�C�&fC�@ C�L�C�Y�C�ffC�L�C�&fC�@ C�@ C�Y�C�ffC�ffC�ffC�ffC�s3C�L�C��C��C�&fC�Y�C�s3C�L�C�L�C�ffC�ffC�ffC�Y�C�ffC�Y�C�L�C�@ C�@ C�33C�33C�&fC�L�C�@ C�L�C�ffC�Y�C�Y�C�Y�C�L�C�@ C�@ C�@ C�33C�33C�33C�L�C�L�C�@ C�ffC�L�C�L�C�@ C�@ C�33C�33C�33C�&fC�&fC�&fC�&fC�&fC��C�L�C�L�C�@ C�s3C�@ C�&fC�&fC�33C�33C�L�C�Y�C�@ C�&fC�33C�@ C�Y�C�ffC�L�C�&fC�@ C�Y�C�Y�C�s3D 3D �fD,�D��D  D�fDfD�3D  D��D9�D��D&fD�3D�D��D9�D��D	&fD	��D
3D
� D&fD�3D9�D��D  D��D  D��D3D� D&fD�3D9�D�3D�D��D�D��D&fD��D,�D��D,�D��D�D��D  D�fD&fD��D,�D��D9�D��D3D�3D3D��D  D� D&fD�fD &fD ��D!,�D!�3D"33D"�3D#�D#�3D$3D$�3D%3D%��D&�D&��D'�D'��D(  D(��D),�D)�fD*3D*�3D+�D+��D,�D,� D-  D-��D.&fD.��D/&fD/�fD0,�D0��D133D1�3D29�D2��D33D3�3D4�D4��D5  D5� D6&fD6�fD7,�D7�3D833D8��D9@ D9�3D:&fD:��D;�D;��D<  D<��D=33D=�3D>�D>� D?,�D?�3D@�D@��DA�DA�fDB,�DB��DC3DC��DD&fDD��DE9�DE� DF  DF�fDG&fDG�3DH33DH�3DI�DI��DJ  DJ� DK&fDK�fDL33DL�3DM33DM�3DNfDN��DO�DO��DP�DP�fDQ  DQ�fDR�DR� DS�DS� DT  DT� DU&fDU�fDV  DV�fDW&fDW�fDX&fDX��DY,�DY�3DZ9�DZ�3D[�D[��D\  D\� D]  D]� D^&fD^�3D_,�D_��D`33D`��Da3Da��Db�Db��Dc�Dc�fDd&fDd��De,�De��Df33Df�3Dg@ Dg�3Dh�Dh�3Di  Di� Dj&fDj�fDk,�Dk��Dl,�Dl��Dm33Dm�3Dn33Dn��Do�Do�3Dp3Dp�3Dq3Dq��Dr�Dr��Ds�Ds� Dt�Dt��Du  Du��Dv�Dv� Dw  Dw� Dx&fDx�fDy&fDy��Dz,�Dz��D{33D{��D|3D|��D}3D}��D~�D~��D  D� D�3D�S3D��fD��fD��D�Y�D��fD�ɚD�	�D�L�D�� D�� D�3D�S3D��3D��fD��D�C3D���D���D� D�S3D��fD�ٚD��D�P D��fD��3D�	�D�P D��fD���D�	�D�P D��3D��3D�	�D�L�D��3D��fD�fD�P D��3D��3D�	�D�P D��fD���D��D�S3D��fD��fD� D�VfD���D�� D� D�VfD���D���D� D�P D��fD���D��D�S3D��3D��fD�	�D�P D��fD�ٚD�	�D�P D��fD��3D��D�P D��fD�ٚD�fD�P D��fD��fD�  D�L�D��fD���D�fD�P D��3D�ٚD��D�L�D�� D��fD��D�\�D���D���D� D�S3D���D�ٚD�  D�L�D�� D�� D�3D�VfD���D�� D�	�D�L�D���D��3D�3D�VfD��fD���D��D�FfD���D���D�	�D�I�D���D���D�	�D�I�D���D���D��D�L�D�� D���D� D�L�D���D���D��D�L�D���D�ɚD�	�D�I�D���D���D��D�Y�D��fD��fD�3D�S3D���D���D�	�D�FfD���D�ٚD��D�VfD��3D���D� D�L�D���D���D�fD�S3D�� D���D��D�Y�D��3D���D��D�Y�D��fD�� D��D�I�D���D��3D� D�\�D���D��3D��D�FfD��fD�� D�	�D�VfD�� D�ɚD�fD�L�D��fD��3D��D�VfD���D�ٚD��D�I�D��3D�� D�3D�L�D���D�� D��D�VfD���D��fD�	�D�VfD�� D��3D��D�VfD�� D��fD��D�Y�D�� D��fD� D�Y�D��3D���D�fD�L�D��fD��3D��D�S3D���D��fD� D�I�D��fD���D�fD�VfD���D�ɚD�3D�L�D��fD��3D��D�FfD�� D���D��D�P D�� D�ٚD�fD�P D���D�ٚD�3D�I�D�D��fD� D�I�DÉ�D��fD� D�L�DČ�D��fD��D�S3DŐ D���D�	�D�\�DƖfD��fD� D�S3Dǉ�D���D��D�VfDȖfD�� D� D�L�Dɉ�D�ٚD�fD�VfDʓ3D��fD� D�P Dˌ�D��fD��D�VfD̖fD��3D�3D�L�D͉�D���D��D�VfDΖfD��3D� D�P DϜ�D�ٚD�fD�P DА D�� D��D�I�Dщ�D�ٚD�fD�Y�Dғ3D��3D� D�P DӉ�D��fD��D�S3Dԓ3D���D��D�FfDՃ3D��3D�3D�P D։�D��fD�fD�S3Dד3D���D�	�D�\�DؖfD��3D�3D�L�Dٌ�D�ɚD��D�Y�DږfD��fD� D�P Dی�D���D��D�Y�DܖfD��3D��D�FfDݜ�D�ٚD�fD�S3Dސ D���D� D�\�Dߙ�D�ٚD�fD�S3D�� D�� D��D�I�DᙚD��3D� D�L�D��D�ٚD�fD�VfD� D���D��D�VfD�fD�� D��D�\�D�fD��3D� D�FfD晚D��fD� D�\�D癚D��3D�	�D�FfD� D�� D�fD�P D陚D��3D��D�Y�D�3D�ɚD�3D�L�D�fD���D��D�S3D쉚D��fD�  D�VfD��D�ٚD�3D�I�D�fD�� D�fD�S3D��D��3D��D�VfD� D��fD�3D�\�D�3D���D�fD�P D�D��3D��D�S3D�D��3D��D�P D�D���D��D�` D��3D�ɚD�3D�Y�D���D��3D�	�D�S3D��fD�� D��D�FfD�� D��3D��D�` D��3D�ɚD�  D���D� D���D�3D��3D�� D�ɚE ffE � Ec3E�E[3E^fE��EQ�E��E�fE@ E�3E� E.fE��E	( E
+3E
�fE#3E)�E�3E!�E��E�3E�E��E�E  E��E3E��E�fEfE��E E�E�3E3E��E��E	�E��E	�E�E�3EfE��E��E)�E�fE33E�3E њE!VfE!��E"h E"�E#y�E$�3E%�E%��E&  E&�3E'33E(P E(� E)` E)��E*x E+�E+� E,�fE-+3E-��E.<�E.��E/NfE0` E0�fE1ffE1� E2y�E3� E4	�E4��E5�E5��E6+3E7A�E7�fE8Q�E8� E9[3E:nfE:��E;q�E;�E<t�E={3E=��E>{3E>�3E@ E@�3EA3EA��EB3ECfEC�3ED3ED��EE�3EE�3EF{3EGvfEG�EHnfEH�3EI�fEJ\�EJ� EKɚEL<�EL�3EM� EN EN��EOnfEO� EP�3EQ;3EQ��ER�3ER��ES�ETNfET��EU� EV	�EV�EWVfEX4�EX� EY��EY�fEZ\�E[@ E[�3E\� E\�fE]�3E^0 E_�E_i�E`A�E`�fEa~fEa��Eb�fEc$�Ec��Edh EeD�Ee�fEf�3Ef�3Eg� Eh;3Ei�Ei{3EjQ�Ej��Ek��Ek�fEl� EmH En$�En��Eok3EoњEp>fEq Eq��ErY�Er��Es� Es��EtњEu��Ev3Ev�fEwA�Ex Ex{3EyL�Ey��Ez� Ez�fE{��E|fE|�fE}��E~3E~�3ENfE� E�@�E�� E� E�< E���E��3E�1�E�� E���E�3E�u�E�� E��3E�W3E��3E��E�[3E���E�3E�+3E�}�E��3E�fE�o3E���E��E�a�E���E��E�Q�E���E��3E�=�E���E��3E�*fE��fE��fE�6fE��3E���E��E�g3E���E�� E�l E��3E� �E�L E���E���E�-�E�zfE�ɚE�:fE���E���E�3E�h�E���E��3E�h E�� E���E�<�E���E��E�, E��fE���?333G�O�?333G�O�G�O�?333G�O�?L��G�O�G�O�G�O�G�O�G�O�?L��G�O�?333G�O�?L��G�O�?fffG�O�?�  ?���?�ff?�  G�O�?�33@33@   @&ff@9��@L��@Y��@l��@y��@�ff@�  @���@���@�33@���@�ff@�  @�  @���@���A��A	��A  A  A   A&ffA0  A8  A>ffAFffAL��AVffA\��Ad��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141441414444414141414111141111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               @ @ �@ V@ *@ O@ "�@ )�@ 0x@ 7L@ ?}@ G�@ R�@ _�@ m�@ {�@ ��@ ��@ ��@ �-@ �w@ ��@ �#@ ��@ �@@o@�@,`@;d@H]@V@c�@p�@~K@�P@�H@��@�F@@�7@ލ@�4@��@1@*@"�@0x@>�@Lu@X�@g@uk@��@��@��@��@��@��@�C@��@�@��@	�@6@&�@3�@@,@O�@^5@k.@x&@�+@�#@�m@�r@�k@�@�
@�`@�e@ �@�@�@+@7L@B�@Q�@`�@m:@y�@��@��@��@�!@�&@��@�#@�@��@�@�@g@,`@<@I@UU@e	@qS@~K@��@��@��@�F@�>@�7@�;@�@��@�@*@!s@-�@<�@K�@Z@ff@s_@�d@��@��@�M@�R@�@խ@�@��@��@	�@B@&;@33@B�@O�@\)@k�@x�@��@��@�m@�@�@��@�h@�@�@^@V@O@(�@5�@D�@Q�@^5@m:@|?@��@��@�y@�~@��@��@�#@�@��@	@	o@	 �@	-@	8�@	G�@	UU@	dZ@	r�@	�W@	��@	��@	��@	��@	��@	�*@	܀@	��@	��@
�@
*@
$.@
1�@
?}@
Lu@
Z�@
g�@
t�@
��@
�\@
�U@
��@
��@
ƨ@
Ӡ@
��@
��@
��@�@B@&;@33@@�@N�@[z@i!@v�@��@�u@�m@�!@�k@�@�
@�@�@�Q@�@�@'�@5?@B�@P�@]�@m�@{�@��@��@��@�!@��@�@��@��@� @j@�@
@,`@;d@I�@V@a�@p�@�@�P@�U@��@��@��@�*@��@�4@�q@v@{@#�@2�@<�@K�@Z�@e�@uk@�p@��@��@�@��@��@�O@�T@�@��@
=@B@""@1'@?}@N�@\�@k�@z3@��@�@��@�f@�@�@�h@�@�@^@b@B@(G@6�@D�@R�@`�@n�@|?@�D@�$@�y@�!@��@�@�t@�@�q@@�@ @-�@<@I�@Wb@`�@oF@|�@��@�<@��@�9@��@ψ@�/@�@��@1@�@ �@.l@<�@Ji@X@ff@t@�@�@�a@�Y@�@�W@��@�T@��@�Q@�@�@$.@2�@@,@N�@\)@j@x&@�|@��@�y@��@�&@�@׹@�@�L@�Q@�@�@+@5?@C�@Q�@`�@oF@x�@��@��@��@��@�@�o@��@��@� @%@{@�@-@:�@I�@Wb@a�@o�@}�@��@��@��@��@Ĝ@�C@��@�@�q@�@o@!s@/@>@K@Yn@e�@t@�@�\@�@��@�@ƨ@Ӡ@��@�@�E@
�@B@&�@5?@C�@M$@[z@i!@ww@�@��@�m@��@��@��@�h@�@�@��@�@�@(G@5�@D�@R�@`�@n�@|?@��@�<@�A@�!@�w@�o@�t@�@�q@@o@ @-�@<�@I�@Wb@e	@s_@|?@��@�<@��@��@��@ψ@�/@��@�,@%@�@""@/@<�@K@X�@ff@t�@�d@�@�a@�@��@�@є@��@��@�9@	�@6@$�@33@@�@O0@\�@k.@x�@�+@��@�a@��@�^@ȴ@�
@�@�@ �@V@�@+@3�@B�@Q=@_�@m�@|?@��@��@��@��@�j@�o@�t@�y@�~@@@g@)�@8�@F�@V@dZ@n�@~K@��@��@��@��@��@��@�/@�4@��@�@{@#�@1�@@�@K@Z@i!@s_@��@�\@�a@�f@��@ƨ@�O@�;@�@��@�@�@$/@33@B8@K�@[z@i�@x�@�+@��@�m@�r@�@��@�\@�@�@��@ �@ �@ +@ 9X@ C�@ Q�@ `�@ oF@ }�@ �+@ ��@ ��@ �-@ �2@ ��@ ��@ �m@ ��@!j@!�@! @!/@!=q@!FQ@!T�@!bN@!qS@!~�@!�P@!��@!��@!��@!��@!ψ@!�/@!�(@!��@"%@"�@" �@".l@"<�@"Ji@"X@"e�@"t@"�@"�\@"�U@"��@"��@"�J@"��@"��@"�@"�9@#�@#�@#(G@#5@@#B�@#O�@#]�@#j@#x&@#�p@#�@#�@#�@#�w@#�o@#�@#�@#�@#�Q@$�@$�@$'�@$9X@$E�@$R�@$_�@$l�@$}�@$��@$��@$�(@$��@$�2@$�*@$�t@$�m@$�e@%v@%�@%�@%/@%<@%H]@%T�@%`�@%r@%~K@%��@%��@%�A@%��@%��@%ψ@%��@%�4@%��@&1@&�@&$/@&/@&<@&K�@&\)@&g@&s_@&��@&�\@&�U@&�@&�@&�W@&�C@&�@&�Y@&�E@'	�@'B@'(�@'4�@'@,@'P�@'\)@'g�@'ww@'�+@'�u@'��@'�r@'�@'�W@'׹@'�m@'�@'�Q@(@(O@('�@(7�@(C�@(O�@(`�@(l�@(y�@(�7@(��@(��@(�-@(�w@(��@(�t@(��@(��@)j@){@) �@)-�@):@)FQ@)Wb@)c�@)oF@)�W@)�P@)��@)��@)��@)��@)�7@)�/@)��@)� @*�@**@*""@*/@*<@*M�@*Z@*g�@*t@*�d@*��@*�U@*�f@*��@*�W@*Ӡ@*�H@*�@@*�9@+J@+B@+&�@+3�@+B8@+N�@+\)@+i!@+uk@+�+@+�#@+��@+��@+�j@+ȴ@+խ@+�m@+�e@,]@,@,�@,(�@,6�@,F�@,S�@,`�@,m:@,z�@,��@,��@,�z@,�!@,�2@,�*@,܀@,��@,�q@-j@-@-[@-*S@-<@-H]@-V@-bN@-o�@-|?@-�7@-�H@-��@-��@-�2@-�*@-�;@-�4@-��@.%@.@.$�@.1'@.>@.K�@.X@.e�@.r�@.�p@.�i@.�a@.�@.�R@.��@.��@.�@.��@.��@/�@/�@/$�@/1'@/C�@/P�@/]�@/j@/ww@/�p@/��@/�(@/�!@/��@/��@/׹@/�@/�Y@/�Q@0J@0[@0)�@06�@0C�@0Q=@0a�@0n�@0|?@0��@0��@0��@0��@0��@0��@0��@0��@0� @1@1@1�@1.l@1;d@1G�@1X@1e	@1qS@1|�@1��@1��@1��@1�F@1@1�C@1ލ@1��@1�9@2�@2@2"�@2/@2;d@2Ji@2Z�@2g@2r�@2�@2��@2�a@2��@2�^@2ƨ@2�C@2�@2��@2��@3
�@3�@3&;@32�@3B8@3N�@3Z@3j@3z3@3��@3�@3��@3�@3�^@3�@3�@3�`@3��@4 �@4b@4O@4'�@45�@4FQ@4UU@4`A@4k�@4{�@4��@4��@4��@4�!@4��@4�*@4��@4�m@4�@5j@5�@5 �@5/�@5:�@5FQ@5X�@5r�@5��@5��@5��@6g@6UU@6��@6�J@6��@71'@7g@7��@8	�@8<@8qS@8��@9b@9DD@9x�@9�@:6@:M$@:��@:�L@;$�@;Z@;�@;��@<3�@<j@<�h@=J@=B�@=x&@=�@>�@>SI@>�7@>�}@?+�@?a�@?�0@@@@7�@@n�@@��@A@AH]@A~�@A��@B'�@B]�@B��@B��@CB�@Cx&@C��@C�y@D#�@D�I@D��@EJ@EG�@E�d@E�k@F1'@Fhs@F�z@F��@GB@GSI@G��@H�@H@�@H|�@H�R@H�e@I/�@I��@I�;@J�@JS�@J��@Jȴ@K=q@Ks_@K�f@K�@L"�@L�0@L�|@M�@MA�@M{�@M�F@N-@Ne�@N�@N�t@Oo@O��@O��@O�q@P-@Pe	@P��@Q
�@QB8@Qx�@Q�@R @RV�@R��@R��@S2�@Sg�@S�a@S�O@TB8@Tuk@T�@U6@UK�@U�@U�F@V!s@VS�@V��@V�@W �@WSI@W�R@W�@X�@X}�@X�@Yo@YB8@Yr�@Y��@Z@Zc�@Z�@Z�2@[""@[O1@[��@[�/@\<@\i�@\�o@\�}@]'�@]��@]�F@^{@^@,@^�a@^�c@_$/@_O1@_�Z@_�[@`2�@`^5@`�@`�@aB�@ap�@a��@a��@bZ@b�|@b�@co@coF@c��@c�q@d$/@d�W@d�f@e�@e:@e�<@eĜ@f#�@fO1@f}�@f�t@g�@gc�@g�@g�@h�@hqS@h�o@h� @iQ=@i{�@i�
@j]@jZ�@j��@j��@k	�@kc�@k��@k�l@l> @lg�@l��@l��@mDD@mm�@m��@nO@nDD@n�I@n��@o�@off@o��@o�/@p*T@pww@p�T@p��@q5�@q�@qȴ@r�@rX@rz2@r��@s%@sI�@s��@s�O@t�@t]�@t�@t�@u*T@un�@u��@u�@v7�@vz2@v��@w �@wa�@w�y@w�@x&;@xg�@x��@x�@y%�@y�@y�J@z@zDD@z�d@z�>@{�@{FP@{��@{�'@|-@|m�@|�f@|�4@})�@}i!@}��@~j@~>�@~{�@~�O@@G�@��@�O@ ^G�O�@ ^G�O�G�O�@ ^G�O�@ G�O�G�O�G�O�G�O�G�O�@ G�O�@ ^G�O�@ G�O�@ �G�O�@ j@ �@ v@ �G�O�@ 	�@ J@ �@ V@ b@ o@ �@ �@ 6@ B@ O@ 
@ !s@ "�@ $�@ &�@ (�@ ,`@ /@ 1�@ 5?@ 7L@ :@ =q@ @�@ C�@ G�@ K@ M�@ Q=@ S�@ X@ Z�@ ^5G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�G�A�E�A�Q�A�VA�VA�S�A�M�A�M�A�M�A�M�A�M�A�O�A�VA�VA�O�A�VA�\)A�^5A�^5A�^5A�^5A�`BA�^5A�^5A�\)A�\)A�\)A�ZA�XA�S�A�Q�A�S�A�S�A�S�A�S�A�S�A�C�A��A�A���A���A���A��A��#A�bNA���A�l�Aɰ!A�VA�z�A��/A�hsA���A�A�A��A���A��A���A�p�A���A�XA�
=A���A�+A���A�ĜA�ZA�/A��A�ZA���A�oA��DA��
A��wA��^A�A��
A�hsA��hA��A�v�A��TA���A���A��A�M�A���A��^A���A��9A��A��wA��A��A�5?A���A���A��DA�bA�z�A��A��PA���A�5?A���A���A��A��\A�wA|�A{��Ay�Av�!Au�AtAr��Aq��An�!An�Am�^Am33AjJAh��AgoAe`BAb�A\�AZ-AX��AWG�AT�ASAPĜAM�7AL1AJ  AG��AG
=AD�yAA�A?&�A=�A<ffA:��A:jA9XA7�A5O�A4r�A3�wA1K�A0�RA0�A/��A/+A-��A-G�A,{A+�7A*�A)�A(�HA&  A$�/A$bA#�7A"��A ��A33A  AdZAVA��A1'AoA{A&�A�RA�mA��AA�TAZA$�A��A�#A�+A��AĜA�uAl�A	�^AbNA��A��At�AO�A�A�A�A�7A�A�jA�7A��A��@��-@��y@�@��/@�G�@��9@�(�@���@���@���@���@�r�@�Q�@��@���@�
=@�=q@�9X@�@�ȴ@��@�x�@��/@�P@�@��@�S�@@�p�@���@�F@��@�!@ꗍ@���@�h@�-@��@��T@��@蛦@�K�@�@�t�@��@���@�R@�~�@�v�@�n�@�M�@�5?@�h@���@��@�@�z�@�z�@�@�z�@�Z@�@���@�\@�{@�@�/@���@���@�Z@��
@߶F@ޟ�@��@�|�@ٺ^@؋D@� �@׮@ׅ@�~�@���@��y@�v�@�=q@��@պ^@Ցh@�`B@��m@���@�z�@��m@υ@ϕ�@Ο�@Ώ\@͙�@�Ĝ@�Q�@�I�@� �@�1'@�|�@��@ʇ+@�@���@�~�@�^5@�ƨ@�ff@��@Ų-@Ƨ�@Ǯ@��@��@Ĭ@Å@��@���@��D@��D@��D@��@��@��u@�(�@�b@���@�A�@�bN@�bN@�bN@� �@�9X@�1'@� �@� �@�t�@��+@�7L@��P@���@�E�@��T@�J@���@��@�"�@�+@�"�@�@��@��R@���@�v�@�M�@�M�@�{@�@��#@�G�@�Ĝ@��D@�bN@�I�@�1@�  @��w@�33@�
=@��@���@��@�@��@���@��`@�A�@��@��@�=q@��@��@��@���@��@�9X@��
@��@�dZ@���@��@���@���@���@�^5@�-@��@�%@��j@�1@��m@��P@�ȴ@��@��h@�?}@��@�Ĝ@��u@�Q�@� �@���@�o@��@���@�v�@���@��7@��@�z�@�1@��@�|�@�"�@�
=@��@�V@�-@�J@���@���@��h@��7@�&�@���@��@�r�@�  @��m@���@�C�@�33@���@�@���@��@���@�x�@�/@���@���@� �@���@�l�@�+@��@�v�@�@�&�@��@��u@�A�@�b@���@�;d@���@���@��\@��\@��\@��\@�~�@�v�@�V@��@��-@�x�@�%@�bN@���@��;@��
@��
@��@�;d@�
=@��!@��\@�J@��-@�p�@�%@���@��`@��j@�Z@��;@���@�l�@�o@��y@��R@��\@�v�@�^5@�5?@�@���@�?}@���@���@�I�@�A�@�1'@���@��@���@���@��@�t�@�C�@�"�@��@��R@�^5@��@��T@�?}@�%@�Ĝ@��@�j@�Z@�Q�@�A�@�1'@�9X@�9X@�1@��m@�l�@�o@�~�@��@���@�hs@�`B@�O�@��@��@�j@�  @�ƨ@���@���@���@���@���@��@���@���@�^5@�=q@�-@�$�@�J@�@�@���@��@��@�@���@��7@�/@���@���@�z�@�j@�Q�@��@��@~��@~�@~$�@}p�@}/@|�@|��@|9X@|�@|�@{��@{��@{�
@{��@{�@{�@{�@{�@{C�@z�\@zM�@y��@yhs@y%@x�9@x�@w�;@v��@v��@v�+@v5?@u`B@t�j@t9X@t1@s��@s33@r�!@rn�@r=q@r�@rJ@q��@p��@pA�@o�w@o\)@o;d@n�y@nV@n5?@m�T@m@m�h@l�/@lj@l(�@k�
@k33@k33@j��@j-@ix�@h�`@h��@h��@h�u@h�@hr�@hr�@hr�@hbN@hQ�@hA�@g�@g�@g�@g\)@f�y@f5?@e�@d�@d�@c@bn�@bM�@b=q@b-@bJ@a�^@`�`@`��@`��@`A�@` �@_�P@^��@^��@^ff@^E�@^{@]��@]��@\��@\�j@\�@\��@\I�@[��@Z^5@Y�@Yhs@Y7L@XĜ@XQ�@XA�@X1'@X  @W��@W\)@V�R@Vv�@VE�@U�h@U�@T�@Tj@TI�@T�@S�
@SdZ@R^5@Q�@QX@P�`@PĜ@P��@P �@O��@O�w@O�P@Nv�@M�-@M`B@MO�@M/@L��@L�D@L(�@K��@K��@KC�@J��@JJ@I�7@IX@IG�@I7L@H�`@H�9@H  @G;d@F�y@FV@E�@E��@EO�@E�@D��@D9X@D1@C�m@B��@B�@AX@@�@@ �@@  @?�@?��@?l�@?
=@>��@>v�@>V@>$�@>{@=�T@=��@=�@<�D@<9X@;��@;ƨ@;��@;�@;C�@:�@:�H@:�!@:~�@:~�@:^5@:=q@:�@9��@97L@8��@8r�@8Q�@8Q�@81'@7�@7��@7�w@7l�@6��@6V@6$�@5p�@4�/@49X@41@3��@3�m@3dZ@2��@2�!@2�\@2n�@2n�@2^5@2�@1��@1��@1%@0�@0 �@/�;@/��@/;d@.ȴ@.�+@.v�@.ff@.5?@-�-@-p�@-�@-V@-V@-V@,��@,�@,��@,9X@,(�@+��@+ƨ@+�@*�H@*��@)��@)hs@(�`@(�9@(r�@(b@'|�@'
=@&��@&v�@&@%�@%�T@%�T@%�T@%@%��@%`B@%�@$�@$�D@$1@$1@#��@$1@#�m@#�m@#ƨ@#ƨ@#�@#C�@#"�@"�@"��@"��@"�\@"n�@"=q@"�@!�#@!�^@!��@!�@ �`@ ��@ Ĝ@ 1'@�w@K�@�@��@�y@�y@�R@�R@��@v�@v�@ff@V@V@E�@$�@@�T@��@�@O�@�/@�j@�j@�@�j@�j@(�@�@S�@C�@"�@@�@�H@��@��@�!@��@�!@~�@M�@�@��@�#@��@�^@��@��@x�@%@��@1'@��@\)@�@�R@�+@v�@5?@�T@@�h@�@O�@�@V@��@��@�m@t�@C�@33@o@@��@~�@~�@n�@n�@^5@^5@M�@=q@�@��@��@�^@��@�7@&�@�@%@��@�`@�u@r�@A�@  @�;@�w@�w@��@l�@\)@\)@K�@+@{@�@�@dZ@@
M�@	��@Ĝ@�y@�-@�D@1@�@J@ ��@ �u?��;?��R?�V?�j?��m?���?�X?���?�K�?�E�?�9X?��
?�?��?�7?�%?��;???�V?�{?�V?�I�?�1?�dZ?�^5?�^?���?�u?�Q�?�?�l�?�ff?�`B?�Z?��
?�S�?���?�-?�bN?߾w?�\)?޸R?ݲ-?�j?�1?�dZ?ۅ?��H?�=q?���?�7L?�r�?�Q�?��?���?�
=?��T?��?ӶF?Ұ!?�n�?�M�?ѩ�?�\)?�;d?��?�{?���?���?��?���?̋D?̋D?̋D?��m?˥�?˥�?�C�?�?���?���?ʟ�?�^5?�=q?��?���?��#?�X?���?ȴ9?�r�?�Q�?�1'?��?��?��?���?�+?�+?�
=?�
=?�
=?Ƨ�?�ff?�ff?�E�?�ff?�E�?�E�?�$�?�$�?�$�?�$�?�$�?�$�?�?�?��T?�?š�?Ł?��?���?�9X?�Z?��?��
?öF?Õ�?öF?öF?Õ�?�33?�o?��?��?�o?�33?��?�n�?�M�?�M�?�-?�-?�J?��?�G�?�G�?�%?��?�bN?�A�?�bN?�bN?� �?�A�?��w?��;?��w?�;d?��?��?���?�v�?�V?�V?�V?��-?��-?���?��?���?���?���?��h?�p�?�p�?�p�?�O�?�V?�/?��?�V?��?�V?�/?�O�?�O�?�p�?�p�?�p�?��h?��h?��h?�O�?�/?�O�?�p�?�p�?�p�?�p�?�p�?��h?��h?��-?��h?��h?��h?��-?���?���?���?��?���?��?��?��?�5??�5??��?�5??�{?�{?�V?�V?�V?�5??�{?�v�?�V?�V?�V?���?���?��R?��R?���?���?�v�?�v�?�v�?�v�?���?���?���?��R?���?��?�;d?�;d?�\)?�\)?�\)?�\)?�\)?�;d?�;d?��?���?��?�;d?�;dA�A�A�E�A�C�A�=qA�?}A�=qA�C�A�G�A�K�A�I�A�M�A�O�A�K�A�I�A�I�A�I�A�K�A�I�A�M�A�K�A�M�A�E�A�E�A�E�A�C�A�G�A�I�A�M�A�M�A�S�A�VA�S�A�VA�VA�VA�S�A�VA�VA�VA�VA�O�A�M�A�K�A�M�A�M�A�O�A�O�A�M�A�M�A�O�A�M�A�M�A�M�A�K�A�K�A�M�A�O�A�S�A�VA�VG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               A�G�A�E�A�Q�A�VA�VA�S�A�M�A�M�A�M�A�M�A�M�A�O�A�VA�VA�O�A�VA�\)A�^5A�^5A�^5A�^5A�`BA�^5A�^5A�\)A�\)A�\)A�ZA�XA�S�A�Q�A�S�A�S�A�S�A�S�A�S�A�C�A��A�A���A���A���A��A��#A�bNA���A�l�Aɰ!A�VA�z�A��/A�hsA���A�A�A��A���A��A���A�p�A���A�XA�
=A���A�+A���A�ĜA�ZA�/A��A�ZA���A�oA��DA��
A��wA��^A�A��
A�hsA��hA��A�v�A��TA���A���A��A�M�A���A��^A���A��9A��A��wA��A��A�5?A���A���A��DA�bA�z�A��A��PA���A�5?A���A���A��A��\A�wA|�A{��Ay�Av�!Au�AtAr��Aq��An�!An�Am�^Am33AjJAh��AgoAe`BAb�A\�AZ-AX��AWG�AT�ASAPĜAM�7AL1AJ  AG��AG
=AD�yAA�A?&�A=�A<ffA:��A:jA9XA7�A5O�A4r�A3�wA1K�A0�RA0�A/��A/+A-��A-G�A,{A+�7A*�A)�A(�HA&  A$�/A$bA#�7A"��A ��A33A  AdZAVA��A1'AoA{A&�A�RA�mA��AA�TAZA$�A��A�#A�+A��AĜA�uAl�A	�^AbNA��A��At�AO�A�A�A�A�7A�A�jA�7A��A��@��-@��y@�@��/@�G�@��9@�(�@���@���@���@���@�r�@�Q�@��@���@�
=@�=q@�9X@�@�ȴ@��@�x�@��/@�P@�@��@�S�@@�p�@���@�F@��@�!@ꗍ@���@�h@�-@��@��T@��@蛦@�K�@�@�t�@��@���@�R@�~�@�v�@�n�@�M�@�5?@�h@���@��@�@�z�@�z�@�@�z�@�Z@�@���@�\@�{@�@�/@���@���@�Z@��
@߶F@ޟ�@��@�|�@ٺ^@؋D@� �@׮@ׅ@�~�@���@��y@�v�@�=q@��@պ^@Ցh@�`B@��m@���@�z�@��m@υ@ϕ�@Ο�@Ώ\@͙�@�Ĝ@�Q�@�I�@� �@�1'@�|�@��@ʇ+@�@���@�~�@�^5@�ƨ@�ff@��@Ų-@Ƨ�@Ǯ@��@��@Ĭ@Å@��@���@��D@��D@��D@��@��@��u@�(�@�b@���@�A�@�bN@�bN@�bN@� �@�9X@�1'@� �@� �@�t�@��+@�7L@��P@���@�E�@��T@�J@���@��@�"�@�+@�"�@�@��@��R@���@�v�@�M�@�M�@�{@�@��#@�G�@�Ĝ@��D@�bN@�I�@�1@�  @��w@�33@�
=@��@���@��@�@��@���@��`@�A�@��@��@�=q@��@��@��@���@��@�9X@��
@��@�dZ@���@��@���@���@���@�^5@�-@��@�%@��j@�1@��m@��P@�ȴ@��@��h@�?}@��@�Ĝ@��u@�Q�@� �@���@�o@��@���@�v�@���@��7@��@�z�@�1@��@�|�@�"�@�
=@��@�V@�-@�J@���@���@��h@��7@�&�@���@��@�r�@�  @��m@���@�C�@�33@���@�@���@��@���@�x�@�/@���@���@� �@���@�l�@�+@��@�v�@�@�&�@��@��u@�A�@�b@���@�;d@���@���@��\@��\@��\@��\@�~�@�v�@�V@��@��-@�x�@�%@�bN@���@��;@��
@��
@��@�;d@�
=@��!@��\@�J@��-@�p�@�%@���@��`@��j@�Z@��;@���@�l�@�o@��y@��R@��\@�v�@�^5@�5?@�@���@�?}@���@���@�I�@�A�@�1'@���@��@���@���@��@�t�@�C�@�"�@��@��R@�^5@��@��T@�?}@�%@�Ĝ@��@�j@�Z@�Q�@�A�@�1'@�9X@�9X@�1@��m@�l�@�o@�~�@��@���@�hs@�`B@�O�@��@��@�j@�  @�ƨ@���@���@���@���@���@��@���@���@�^5@�=q@�-@�$�@�J@�@�@���@��@��@�@���@��7@�/@���@���@�z�@�j@�Q�@��@��@~��@~�@~$�@}p�@}/@|�@|��@|9X@|�@|�@{��@{��@{�
@{��@{�@{�@{�@{�@{C�@z�\@zM�@y��@yhs@y%@x�9@x�@w�;@v��@v��@v�+@v5?@u`B@t�j@t9X@t1@s��@s33@r�!@rn�@r=q@r�@rJ@q��@p��@pA�@o�w@o\)@o;d@n�y@nV@n5?@m�T@m@m�h@l�/@lj@l(�@k�
@k33@k33@j��@j-@ix�@h�`@h��@h��@h�u@h�@hr�@hr�@hr�@hbN@hQ�@hA�@g�@g�@g�@g\)@f�y@f5?@e�@d�@d�@c@bn�@bM�@b=q@b-@bJ@a�^@`�`@`��@`��@`A�@` �@_�P@^��@^��@^ff@^E�@^{@]��@]��@\��@\�j@\�@\��@\I�@[��@Z^5@Y�@Yhs@Y7L@XĜ@XQ�@XA�@X1'@X  @W��@W\)@V�R@Vv�@VE�@U�h@U�@T�@Tj@TI�@T�@S�
@SdZ@R^5@Q�@QX@P�`@PĜ@P��@P �@O��@O�w@O�P@Nv�@M�-@M`B@MO�@M/@L��@L�D@L(�@K��@K��@KC�@J��@JJ@I�7@IX@IG�@I7L@H�`@H�9@H  @G;d@F�y@FV@E�@E��@EO�@E�@D��@D9X@D1@C�m@B��@B�@AX@@�@@ �@@  @?�@?��@?l�@?
=@>��@>v�@>V@>$�@>{@=�T@=��@=�@<�D@<9X@;��@;ƨ@;��@;�@;C�@:�@:�H@:�!@:~�@:~�@:^5@:=q@:�@9��@97L@8��@8r�@8Q�@8Q�@81'@7�@7��@7�w@7l�@6��@6V@6$�@5p�@4�/@49X@41@3��@3�m@3dZ@2��@2�!@2�\@2n�@2n�@2^5@2�@1��@1��@1%@0�@0 �@/�;@/��@/;d@.ȴ@.�+@.v�@.ff@.5?@-�-@-p�@-�@-V@-V@-V@,��@,�@,��@,9X@,(�@+��@+ƨ@+�@*�H@*��@)��@)hs@(�`@(�9@(r�@(b@'|�@'
=@&��@&v�@&@%�@%�T@%�T@%�T@%@%��@%`B@%�@$�@$�D@$1@$1@#��@$1@#�m@#�m@#ƨ@#ƨ@#�@#C�@#"�@"�@"��@"��@"�\@"n�@"=q@"�@!�#@!�^@!��@!�@ �`@ ��@ Ĝ@ 1'@�w@K�@�@��@�y@�y@�R@�R@��@v�@v�@ff@V@V@E�@$�@@�T@��@�@O�@�/@�j@�j@�@�j@�j@(�@�@S�@C�@"�@@�@�H@��@��@�!@��@�!@~�@M�@�@��@�#@��@�^@��@��@x�@%@��@1'@��@\)@�@�R@�+@v�@5?@�T@@�h@�@O�@�@V@��@��@�m@t�@C�@33@o@@��@~�@~�@n�@n�@^5@^5@M�@=q@�@��@��@�^@��@�7@&�@�@%@��@�`@�u@r�@A�@  @�;@�w@�w@��@l�@\)@\)@K�@+@{@�@�@dZ@@
M�@	��@Ĝ@�y@�-@�D@1@�@J@ ��@ �u?��;?��R?�V?�j?��m?���?�X?���?�K�?�E�?�9X?��
?�?��?�7?�%?��;???�V?�{?�V?�I�?�1?�dZ?�^5?�^?���?�u?�Q�?�?�l�?�ff?�`B?�Z?��
?�S�?���?�-?�bN?߾w?�\)?޸R?ݲ-?�j?�1?�dZ?ۅ?��H?�=q?���?�7L?�r�?�Q�?��?���?�
=?��T?��?ӶF?Ұ!?�n�?�M�?ѩ�?�\)?�;d?��?�{?���?���?��?���?̋D?̋D?̋D?��m?˥�?˥�?�C�?�?���?���?ʟ�?�^5?�=q?��?���?��#?�X?���?ȴ9?�r�?�Q�?�1'?��?��?��?���?�+?�+?�
=?�
=?�
=?Ƨ�?�ff?�ff?�E�?�ff?�E�?�E�?�$�?�$�?�$�?�$�?�$�?�$�?�?�?��T?�?š�?Ł?��?���?�9X?�Z?��?��
?öF?Õ�?öF?öF?Õ�?�33?�o?��?��?�o?�33?��?�n�?�M�?�M�?�-?�-?�J?��?�G�?�G�?�%?��?�bN?�A�?�bN?�bN?� �?�A�?��w?��;?��w?�;d?��?��?���?�v�?�V?�V?�V?��-?��-?���?��?���?���?���?��h?�p�?�p�?�p�?�O�?�V?�/?��?�V?��?�V?�/?�O�?�O�?�p�?�p�?�p�?��h?��h?��h?�O�?�/?�O�?�p�?�p�?�p�?�p�?�p�?��h?��h?��-?��h?��h?��h?��-?���?���?���?��?���?��?��?��?�5??�5??��?�5??�{?�{?�V?�V?�V?�5??�{?�v�?�V?�V?�V?���?���?��R?��R?���?���?�v�?�v�?�v�?�v�?���?���?���?��R?���?��?�;d?�;d?�\)?�\)?�\)?�\)?�\)?�;d?�;d?��?���?��?�;d?�;dA�A�A�E�A�C�A�=qA�?}A�=qA�C�A�G�A�K�A�I�A�M�A�O�A�K�A�I�A�I�A�I�A�K�A�I�A�M�A�K�A�M�A�E�A�E�A�E�A�C�A�G�A�I�A�M�A�M�A�S�A�VA�S�A�VA�VA�VA�S�A�VA�VA�VA�VA�O�A�M�A�K�A�M�A�M�A�O�A�O�A�M�A�M�A�O�A�M�A�M�A�M�A�K�A�K�A�M�A�O�A�S�A�VA�VG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�uB	�uB	�uB	�uB	�uB	�oB	�oB	�uB	�uB	�oB	�oB	�{B	�{B	�uB	�uB	��B	��B	��B	��B	��B	��B	�uB	�{B	��B	�B	�B	�9B	ÖB	��B	�HB	�B	�B
B
VB
�B
�B
#�B
'�B
)�B
)�B
+B
+B
+B
+B
)�B
,B
~�B
�^B
��B
ĜB
ƨB
��B
�B�B+BB�BYBffBm�Bt�B��B�qBƨB��B�;B�sB�B1B$�B1'BC�BE�BG�BG�BC�B?}BC�BH�B[#B]/B^5BZBZBYBQ�BN�BK�BJ�BH�B>wB1'B"�B�BJBB��B��BƨB�XB��BC�B�B  B
�9B
��B
�{B
L�B
�B

=B	�)B	��B	��B	�^B	�B	��B	��B	�\B	~�B	p�B	iyB	e`B	_;B	E�B	F�B	6FB	,B	�B	JB	B��B�B�yB�ZB�B��B��BȴB��BŢBǮB��BȴBȴBĜBƨBĜBĜB�}B�}B��B�dBƨBŢBŢBŢBŢBǮBȴB��B��B��B�`B�mB��B	%B	B	B��B�ZB�qB�-B��B��B��B��B��B��B��B��B��B�?B��BȴB��B��B�B��B��B	B	%B��B��B��B�B�B�B�B�yB�sB�B��B	1B	�B	�B	8RB	9XB	PB��B�B�B�B��B��B	
=B	�B	#�B	)�B	'�B	+B	0!B	5?B	9XB	=qB	:^B	A�B	G�B	G�B	H�B	K�B	I�B	J�B	R�B	ZB	XB	S�B	Q�B	P�B	O�B	S�B	YB	_;B	dZB	e`B	hsB	m�B	p�B	q�B	p�B	t�B	w�B	w�B	w�B	v�B	v�B	u�B	u�B	u�B	v�B	w�B	}�B	�B	�B	�DB	�VB	�\B	�\B	�\B	�bB	�uB	�uB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	�oB	�hB	�bB	�\B	�VB	�VB	�\B	�\B	�PB	�uB	�oB	��B	��B	��B	�{B	�{B	�{B	�VB	�1B	�=B	�JB	�VB	�VB	�PB	�VB	�DB	�PB	�PB	�\B	�bB	�oB	�oB	�oB	�hB	��B	��B	��B	��B	�hB	�PB	�DB	�JB	��B	��B	��B	��B	�uB	�VB	�JB	�DB	�JB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�9B	�RB	B	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�
B	�
B	�B	�B	�B	�
B	�
B	�B	�#B	�)B	�)B	�/B	�5B	�;B	�BB	�BB	�HB	�HB	�NB	�NB	�HB	�NB	�NB	�HB	�ZB	�ZB	�ZB	�`B	�`B	�fB	�fB	�fB	�sB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B

=B

=B
DB

=B
DB
DB
JB
DB
JB
JB
JB
DB
JB
JB
PB
PB
JB
JB
JB
PB
PB
JB
PB
PB
PB
PB
PB
VB
VB
\B
\B
\B
\B
\B
bB
bB
bB
hB
bB
bB
hB
bB
hB
hB
hB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
(�B
)�B
+B
+B
,B
,B
,B
,B
-B
-B
.B
.B
.B
/B
/B
0!B
0!B
1'B
0!B
1'B
1'B
1'B
2-B
33B
2-B
2-B
2-B
33B
2-B
33B
33B
33B
33B
49B
33B
49B
49B
49B
6FB
6FB
7LB
7LB
7LB
8RB
7LB
8RB
7LB
8RB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
:^B
9XB
:^B
9XB
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
<jB
=qB
=qB
>wB
>wB
>wB
?}B
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
A�B
B�B
A�B
B�B
B�B
C�B
B�B
D�B
D�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
F�B
F�B
F�B
F�B
G�B
H�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
I�B
I�B
H�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
L�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
R�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
VB
W
B
VB
VB
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
XB
ZB
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
[#B
[#B
[#B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
_;B
^5B
^5B
_;B
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
`BB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
ffB
e`B
ffB
e`B
ffB
ffB
e`B
ffB
ffB
ffB
ffB
gmB
ffB
hsB
hsB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
iyB
hsB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
l�B
k�B
l�B
l�B
l�B
l�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
l�B
m�B
m�B
m�B
m�B
n�B
m�B
m�B
n�B
m�B
m�B
n�B
o�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
o�B
o�B
o�B
p�B
o�B
p�B
p�B
q�B
q�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
w�B
w�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
x�B
y�B
y�B
z�B
z�B
y�B
z�B
y�B
z�B
z�B
z�B
z�B
{�B
z�B
|�B
}�B
~�B
�B
�B
�B
�B
�B
�B
�%B
�+B
�1B
�1B
�7B
�=B
�7B
�7B
�7B
�=B
�PB
�VB
�\B
�\B
�\B
�hB
�hB
�uB
�uB
�oB
�uB
�{B
�{B
�{B
��B
��B
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
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
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
��B
�B
�B
�B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�B
�!B
�'B
�'B
�'B
�'B
�-B
�'B
�-B
�-B
�-B
�-B
�-B
�-B
�-B
�3B
�-B
�3B
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
�?B
�FB
�?B
�FB
�LB
�FB
�LB
�FB
�FB
�LB
�LB
�RB
�LB
�FB
�LB
�LB
�RB
�XB
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
�XB
�XB
�XB
�^B
�XB
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�dB
�^B
�dB
�dB
�^B
�^B
�^B
�dB
�^B
�^B
�dB
�dB
�dB
�jB
�dB
�jB
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
�jB
�qB
�jB
�qB
�qB
�jB
�jB
�qB
�jB
�qB
�qB
�qB
�qB
�qB
�qB
�wB
�qB
�qB
�wB
�wB
�qB
�qB
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
�wB
�wB
�wB
�wB
�wB
�}B
�}B
�wB
�wB
�wB
�}B
��B
�}B
��B
�}B
�}B
�}B	�{B	�uB	�uB	�uB	�uB	�{B	�uB	�uB	�oB	�uB	�uB	�oB	�uB	�oB	�uB	�oB	�uB	�uB	�uB	�uB	�oB	�uB	�uB	�uB	�uB	�uB	�uB	�uB	�uB	�uB	�{B	�uB	�uB	�uB	�uB	�uB	�uB	�uB	�oB	�oB	�oB	�oB	�oB	�uB	�uB	�oB	�uB	�uB	�oB	�oB	�oB	�uB	�oB	�oB	�uB	�oB	�{B	��B	��B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               B	�VB	�VB	�VB	�VB	�VB	�PB	�PB	�VB	�VB	�PB	�PB	�\B	�\B	�VB	�VB	�bB	�uB	�hB	�hB	�bB	�bB	�VB	�\B	�uB	��B	��B	�B	�wB	ȴB	�)B	�`B	�yB	��B
	7B
bB
�B
�B
"�B
$�B
$�B
%�B
%�B
%�B
%�B
$�B
&�B
y�B
�?B
ǮB
�}B
��B
ȴB
�BhB%�B=qBS�BaHBhsBo�B��B�RB��BɺB�B�TB�mBB�B,B>wB@�BB�BB�B>wB:^B>wBC�BVBXBYBT�BT�BS�BL�BI�BF�BE�BC�B9XB,B�BuB+B��B�B��B��B�9B�hB>wBhB
��B
�B
��B
�\B
G�B
{B
B	�
B	��B	ƨB	�?B	��B	��B	�oB	�=B	y�B	k�B	dZB	`BB	ZB	@�B	A�B	1'B	&�B	�B	+B��B��B�B�ZB�;B��B��B��BÖBŢB��BBŢBÖBÖB�}B��B�}B�}B�^B�^B�dB�FB��B��B��B��B��BBÖB��B��B��B�BB�NB��B	B��B��B��B�;B�RB�B��B��B��B�{B�hB�{B��B��B��B�!B�dBÖBƨBƨB��B��B��B��B	B��B�B�B�B�B�sB�mB�ZB�TB�yB��B	B	uB	�B	33B	49B	1B�B�fB�fB�B�B��B	B	hB	�B	$�B	"�B	%�B	+B	0!B	49B	8RB	5?B	<jB	B�B	B�B	C�B	F�B	D�B	E�B	M�B	T�B	R�B	N�B	L�B	K�B	J�B	N�B	S�B	ZB	_;B	`BB	cTB	hsB	k�B	l�B	k�B	o�B	r�B	r�B	r�B	q�B	q�B	p�B	p�B	p�B	q�B	r�B	x�B	|�B	|�B	�%B	�7B	�=B	�=B	�=B	�DB	�VB	�VB	�VB	�\B	�bB	�oB	�oB	�oB	�oB	�oB	�hB	�VB	�PB	�JB	�DB	�=B	�=B	�DB	�DB	�7B	�\B	�VB	�oB	�oB	�oB	�bB	�bB	�bB	�=B	�B	�%B	�1B	�=B	�=B	�7B	�=B	�+B	�7B	�7B	�DB	�JB	�VB	�VB	�VB	�PB	�oB	�{B	�{B	�uB	�PB	�7B	�+B	�1B	�uB	��B	��B	�oB	�\B	�=B	�1B	�+B	�1B	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�9B	�wB	ÖB	ƨB	ǮB	ǮB	ȴB	ǮB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�5B	�5B	�/B	�5B	�5B	�/B	�BB	�BB	�BB	�HB	�HB	�NB	�NB	�NB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�fB	�fB	�mB	�mB	�fB	�fB	�mB	�sB	�sB	�yB	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
%B
+B
+B
1B
+B
1B
1B
1B
+B
1B
1B
	7B
	7B
1B
1B
1B
	7B
	7B
1B
	7B
	7B
	7B
	7B
	7B

=B

=B
DB
DB
DB
DB
DB
JB
JB
JB
PB
JB
JB
PB
JB
PB
PB
PB
VB
hB
hB
oB
oB
hB
oB
oB
uB
uB
{B
�B
�B
�B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
 �B
 �B
 �B
!�B
!�B
!�B
"�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
%�B
$�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
+B
+B
,B
,B
-B
,B
-B
-B
-B
.B
/B
.B
.B
.B
/B
.B
/B
/B
/B
/B
0!B
/B
0!B
0!B
0!B
2-B
2-B
33B
33B
33B
49B
33B
49B
33B
49B
33B
49B
49B
49B
49B
49B
5?B
6FB
5?B
6FB
5?B
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
8RB
9XB
9XB
:^B
:^B
:^B
;dB
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
=qB
>wB
=qB
>wB
>wB
@�B
?}B
@�B
A�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
D�B
E�B
D�B
D�B
D�B
E�B
F�B
F�B
F�B
F�B
F�B
E�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
I�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
O�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
W
B
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
YB
ZB
ZB
ZB
ZB
[#B
[#B
\)B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
aHB
aHB
aHB
aHB
aHB
cTB
bNB
cTB
bNB
cTB
cTB
bNB
cTB
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
dZB
dZB
dZB
e`B
e`B
e`B
e`B
ffB
e`B
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
iyB
hsB
iyB
iyB
iyB
iyB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
iyB
jB
jB
jB
jB
k�B
jB
jB
k�B
jB
jB
k�B
l�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
l�B
l�B
l�B
m�B
l�B
m�B
m�B
n�B
n�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
q�B
p�B
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
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
u�B
v�B
v�B
w�B
w�B
v�B
w�B
v�B
w�B
w�B
w�B
w�B
x�B
w�B
y�B
z�B
{�B
}�B
}�B
~�B
~�B
� B
�B
�B
�B
�B
�B
�%B
�+B
�%B
�%B
�%B
�+B
�=B
�DB
�JB
�JB
�JB
�VB
�VB
�bB
�bB
�\B
�hB
�oB
�oB
�oB
�uB
�uB
�oB
�oB
�{B
�{B
�{B
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
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
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
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�!B
�!B
�'B
�!B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�-B
�'B
�-B
�-B
�-B
�3B
�3B
�3B
�9B
�9B
�9B
�9B
�9B
�9B
�?B
�9B
�?B
�FB
�?B
�FB
�?B
�?B
�FB
�FB
�LB
�FB
�?B
�FB
�FB
�LB
�RB
�RB
�LB
�RB
�LB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�XB
�RB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�dB
�^B
�dB
�dB
�^B
�^B
�^B
�dB
�^B
�^B
�dB
�dB
�dB
�jB
�dB
�jB
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
�jB
�qB
�jB
�qB
�qB
�jB
�jB
�qB
�jB
�qB
�qB
�qB
�qB
�qB
�qB
�wB
�qB
�qB
�wB
�wB
�qB
�qB
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
�wB
�wB
�wB
�wB
�wB
�}B
�}B
�wB
�wB
�wB
�}B
��B
�}B
��B
�}B
�}B
�}B	�\B	�VB	�VB	�VB	�VB	�\B	�VB	�VB	�PB	�VB	�VB	�PB	�VB	�PB	�VB	�PB	�VB	�VB	�VB	�VB	�PB	�VB	�VB	�VB	�VB	�VB	�VB	�VB	�VB	�VB	�\B	�VB	�VB	�VB	�VB	�VB	�VB	�VB	�PB	�PB	�PB	�PB	�PB	�VB	�VB	�PB	�VB	�VB	�PB	�PB	�PB	�VB	�PB	�PB	�VB	�PB	�\B	�bB	�bB	�bG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201912121200252021061413563920210614135639202106141918382021061419183820210614191838201912121200252021061413563920210614135639202106141918382021061419183820210614191838PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0.0001), vertically averaged dS = -0.005 (+/-0.004)                                                                                                                                                                                           surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0.0001), vertically averaged dS = -0.005 (+/-0.004)                                                                                                                                                                                           Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2019121212002520191212120025  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019121212002520191212120025QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019121212002520191212120025QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713152620210617131526IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                