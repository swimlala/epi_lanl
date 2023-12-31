CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  /   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-12-17T01:04:39Z creation      
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
resolution        =���   axis      Z        )x  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
`  eX   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     )x  o�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
`  �0   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     )x  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )x  �   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
`  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )x  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
` *X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )x 4�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )x ^0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
` ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )x �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
` ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )x ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   `   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   |   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                       	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �        �  Argo profile    3.1 1.2 19500101000000  20201217010439  20210722160210  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               |   |DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�J��ӧ�@�J��ӧ�11  @�J���5@�J���5@0�F�]c�@0�F�]c��c������c�����11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?�  @ff@Fff@�33@�33@�  @���A   A33A&ffAA��A`  A~ffA���A�  A���A�  A�ffA���A���A�33B��B��B��B��B(  B0  B8ffB@��BH��BP��BX  B`  Bh  Bp  BxffB�33B�ffB�33B�ffB�33B�33B���B�  B�33B�ffB�ffB�  B�  B�ffB�33B�33B�  B�  BǙ�B�  B�  B���B�  B���Bߙ�B���B���B�  B�  B�33B�33B�ffC L�C�fC�fC  C  C
�C�C33C�fC�fC�fC�fC�fC�fC  C�fC   C!�fC$33C&33C(33C*�C+�fC.�C0  C1�fC4�C6�C7�fC:L�C<33C>33C@  CA�fCD33CF�CG�fCJ�CL33CN�CO��CR�CTL�CV  CW�fCZ33C\33C^�C`L�Ca��Cc�fCf  Cg�fCj�Cl�CnL�Co�fCr  Ct  Cv�CxL�Cy�fC|�C~�C��C��C��C��C��C��C��3C��C�  C��3C��C�&fC��C��3C��C�&fC�&fC��C��fC��fC��fC��fC��fC��fC��3C�  C��C��C�  C��fC��3C��C�&fC��C��3C��C�&fC�&fC��C��C�  C�  C��3C��C��C�  C�&fC��C��C�  C��3C��C��C�  C��C��C�  C��C��C�  C��C�  C��3C��C�  C��fC�  C��C��C��3C�  C�&fC��C��3C�  C��C�&fC��C��3C�  C��C�&fC�&fC�&fC��C��fC��3C��3C�  C��C��C�&fC��C��fC�  C�  C��C��C��C��C�  C��fC��fC��3C�  C��C��C�&fC��C��fC�  C��C��C��C�33C�  C��fC��fC��3C�  C��C��C�  C��3C��3C�  C��C��D �D ��D ��Dy�D��D�fD  D�3D3D��D�Dy�DfD��D3D�3D�3Dy�D	  D	�fD
�D
s3D  D��D3D� D  D�fD�Dl�D��D� D�D��D��D� D�Ds3D��D� D�D��D��D� DfDffD��D�fD�fDy�DfDffD��D��D��D� D3Dl�D  D�3D��Dy�DfDffD��D �fD �fD!y�D"fD"�3D#&fD#�fD$3D$��D%  D%��D&  D&y�D'�D'��D'�3D(�fD)3D)�3D*&fD*�fD+3D+� D,  D,�fD-3D-y�D.fD.��D.��D/y�D0fD0��D1�D1s3D2  D2��D2�fD3y�D4  D4�3D5  D5y�D6fD6�3D6�3D7y�D8�D8�3D9�D9y�D:fD:�fD;3D;� D<  D<�fD=�D=ffD=�3D>s3D?fD?��D@3D@� DA  DA�fDB�DBl�DC  DC��DC�fDDs3DD��DE��DF�DFs3DGfDG��DG�3DH� DI�DIl�DJ  DJ�fDJ��DKy�DLfDLl�DL��DM� DN3DN� DO  DO` DO�3DPy�DQfDQ�3DQ�3DR� DS�DSl�DS��DT�fDU3DU�fDV  DV��DW�DWs3DXfDX�3DX��DY�fDZ3DZl�D[  D[��D[��D\y�D]�D]l�D^  D^�3D^��D_y�D`fD`�3Da&fDa�fDb3Db�3Dc  Dc�fDd3Dds3DefDe� De��Df�3Dg  Dgy�Dh�Dh�3Di�Di�fDjfDjffDj�3Dky�DlfDl��Dl��Dm��Dn�Dny�Do�Do��Do��Dp�fDq�DqffDq��Dr� Ds�Ds��Ds��Dt�fDu3Duy�DvfDv��Dv��Dwy�DxfDx�3Dy�Dyy�DzfDz��Dz�3D{� D|�D|�3D}  D}s3D~  D~�fD�D��D��D�9�D�|�D�� D�fD�I�D���D���D�  D�@ D��fD��3D�fD�C3D���D���D���D�<�D�� D�� D�  D�FfD��fD���D��D�L�D���D��fD���D�6fD�y�D���D���D�9�D�y�D���D���D�9�D�|�D���D���D�@ D��3D��fD�	�D�L�D�y�D���D�  D�C3D��3D��3D�fD�FfD��3D�ɚD�	�D�9�D�y�D�� D���D�<�D��fD�� D�fD�FfD��fD�ɚD�fD�FfD���D�ɚD�	�D�L�D���D�ɚD�	�D�I�D���D��fD�3D�C3D�� D�� D���D�9�D�vfD���D�	�D�I�D��fD��fD�3D�C3D�� D�� D���D�<�D���D���D��D�I�D���D��fD�fD�I�D��3D��3D�3D�@ D�|�D���D��D�I�D���D�ɚD�fD�FfD��3D�� D�  D�<�D���D���D��D�L�D���D��3D�3D�@ D��3D�� D�  D�@ D�� D���D���D�<�D�|�D���D���D�<�D�|�D���D�  D�<�D�|�D���D���D�<�D�|�D���D���D�L�D���D��fD�fD�C3D��fD��3D�3D�C3D��3D��3D�3D�C3D��3D��fD�fD�I�D���D�ɚD�fD�FfD��3D��3D�3D�C3D��3D��fD�3D�C3D�� D��3D�  D�@ D��3D��3D�3D�C3D��3D��3D�  D�C3D�|�D�� D�  D�@ D�|�D���D���D�<�D�y�D���D�	�D�L�D���D���D�fD�@ D�� D���D���D�9�D�y�D�ɚD�	�D�FfD��3D�� D�3D�<�D���D�ɚD�	�D�C3D��3D�� D�  D�L�D���D��fD�3D�@ D�|�D���D���D�6fD���D���D�	�D�FfD��3D���D���D�L�D���D��3D�3D�C3D��3D���D���D�<�D���D�ɚD�	�D�FfD��fD��fD�3D�@ D�|�D���D���D�I�D3D�� D���D�<�D�s3DöfD�fD�@ D�|�D���D�	�D�@ D�|�DŹ�D�	�D�@ D�y�D��fD�  D�9�DǆfD�� D���D�I�Dȃ3Dȼ�D�fD�@ D�y�D��fD���D�6fDʃ3D���D�3D�<�Dˉ�D��3D���D�I�D̀ D̹�D�fD�@ D�vfD��fD���D�9�D΃3Dμ�D���D�C3Dϐ D��fD�  D�I�DЃ3Dй�D�3D�P Dу3Dѹ�D�3D�L�D҃3DҼ�D�fD�<�D�vfD�� D��D�C3D�y�D��3D��D�FfDՀ D���D�3D�<�D։�D��3D���D�FfD׀ D׼�D�	�D�C3D�|�D�ɚD�  D�<�DنfD�� D���D�FfDڐ D��fD�  D�I�Dۃ3Dۼ�D��D�@ D�y�D��3D���D�6fD݀ D���D�fD�<�DކfD��3D�  D�9�D�vfD��3D���D�9�D��3D��D���D�C3D�|�D�fD�  D�L�D�3D�� D��D�C3D� D�� D�	�D�C3D� D乚D�	�D�C3D�3D��D���D�FfD�3D�� D���D�6fD�3D�� D���D�I�D�fD��3D�  D�<�D��D��fD�	�D�@ D�|�D�ɚD�	�D�<�D�y�D�fD�fD�@ D�y�D�ɚD�3D�C3D� D���D��D�L�DD��fD�fD�C3D�|�D��D���D�I�D��fD��fD�3D�@ D� D��D��D�FfD�3D�� D���D�L�D�D��3D�  D�@ D� D���D��D�I�D���D��3D�3D�@ D�|�D���D��fD�C3D�|�D���D�	�D�FfD��3D�� D���D�I�D��fD��fD�  D�3D�)�D�c3D��fE i�E��E8 EњEh E�fE��E&fE�3EL�E�3Ek3E��E� E	��E
$�E
� E;3E�3EK3Ea�E�fEa�E� EnfE{3E�fE��E	�E�fE�fE&fE��E33E�3E�3EP E�3Ei�E� Ey�E3EfE�fE1�E�fENfEٚEk3E� E �E � E!;3E!њE"a�E"�3E#��E$fE$��E%�fE&c3E&�3E'�3E(3E(��E)9�E)�3E*[3E*� E+��E,~fE-�E-�3E.�E/  E/��E0�E0��E1!�E2+3E2��E3�E4�E4� E5	�E5��E6vfE6�3E7c3E8[3E8�fE9D�E:;3E:�3E;!�E<3E<��E<�3E=� E>[3E?P E?�3E@9�EA+3EA�fEB3EB�3ECl�EC��ED� EEA�EE��EF��EG3EH�EHt�EH�fEI�3EJ8 EK!�EK��EL  EL�EMVfEN8 EN��EO EP�EPnfEQVfEQ��ER4�ES3ES{3ETY�ET��EU��EV EV�EWX EX4�EX��EY~fEY� EZ��E[#3E\  E\ffE]D�E]��E^�E^� E_a�E`;3E`�fEa{3Ea�fEb��Ec3Ec�fEdc3Ee<�Ee��Ef��Ef�fEg��Eh.fEi Eix EjVfEj�3Ek� El  El� Em@ En3En�3Eo` Eo� Ep� Ep�fEq� Er33Es�Esi�Et9�Et� Euk3Ev9�Ev��Ewi�Ew�3Ex��Ex�3Ey��Ez� Ez��E{�fE|[3E|��E}q�E~0 E~��EVfE�
fE�h�E��fE��fE�W3E��3E��E�D E�q�E�њE�/3E�ZfE��fE�fE�i�E�� E�� E�H�E��3E��fE�RfE�}�E���E�, E���E��fE�, E�VfE��fE� E�Y�E���E�  E�P E���E��E�BfE���E���E�6fE���E�� E�$�E�vfE�� E� E�c3E��3E�3E�Q�E���E��fE�8 E�� E��3E� E�h E��3E�'3E�t E���E�3E�^fE��fE���E�A�E��fE�� E�D E�� E�ݚE�( E�o3E���E�"fE�h E��3E�� E�h�E���E�fE�O3E�� E�� E�2fE�{3E��fE�,�E�t E���E��E�S3E��3E�� E�RfE���E�� E�I�E��3E���E�@�E�}�E�ٚE� E�m�E��3E�fE�T�E���E�3E�ZfE�� E��3>���>���>���>���>���>���>���?   ?   ?333?333?L��?�  ?���?�33?�  ?ٙ�@   @ff@��@&ff@9��@L��@`  @l��@�33@�  @�ff@�  @�  @���@�ff@�33@�33@�33A   A  AffAffAffA(  A,��A4��A<��AFffAL��AT��A\��Ac33Ai��As33A{33A���A���A���A�  A���A�  A�  A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444141141411111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     ?fff?�  @&ff@fff@�33@�33@�  @���A  A33A.ffAI��Ah  A�33A���A�  A���A�  A�ffA���A���B��B	��B��B��B!��B*  B2  B:ffBB��BJ��BR��BZ  Bb  Bj  Br  BzffB�33B�ffB�33B�ffB�33B�33B���B�  B�33B�ffB�ffB�  B�  B�ffB�33B�33B�  B�  Bș�B�  B�  B���B�  B���B���B���B���B�  B�  B�33B�33B�ffC ��CffCffC� C� C
��C��C�3CffCffCffCffCffCffC� CffC � C"ffC$�3C&�3C(�3C*��C,ffC.��C0� C2ffC4��C6��C8ffC:��C<�3C>�3C@� CBffCD�3CF��CHffCJ��CL�3CN��CPL�CR��CT��CV� CXffCZ�3C\�3C^��C`��CbL�CdffCf� ChffCj��Cl��Cn��CpffCr� Ct� Cv��Cx��CzffC|��C~��C�L�C�Y�C�L�C�L�C�L�C�L�C�33C�L�C�@ C�33C�L�C�ffC�Y�C�33C�L�C�ffC�ffC�L�C�&fC�&fC�&fC�&fC�&fC�&fC�33C�@ C�L�C�Y�C�@ C�&fC�33C�L�C�ffC�L�C�33C�L�C�ffC�ffC�L�C�L�C�@ C�@ C�33C�Y�C�Y�C�@ C�ffC�Y�C�L�C�@ C�33C�Y�C�Y�C�@ C�Y�C�L�C�@ C�Y�C�Y�C�@ C�Y�C�@ C�33C�Y�C�@ C�&fC�@ C�Y�C�L�C�33C�@ C�ffC�Y�C�33C�@ C�Y�C�ffC�L�C�33C�@ C�L�C�ffC�ffC�ffC�L�C�&fC�33C�33C�@ C�L�C�Y�C�ffC�L�C�&fC�@ C�@ C�L�C�L�C�Y�C�Y�C�@ C�&fC�&fC�33C�@ C�L�C�Y�C�ffC�L�C�&fC�@ C�L�C�L�C�Y�C�s3C�@ C�&fC�&fC�33C�@ C�L�C�Y�C�@ C�33C�33C�@ C�Y�C�Y�D 9�D ��D�D��D�D�fD  D�3D33D��D9�D��D&fD��D33D�3D3D��D	  D	�fD
,�D
�3D  D��D33D� D  D�fD,�D��D�D� D,�D��D�D� D,�D�3D�D� D,�D��D�D� D&fD�fD�D�fDfD��D&fD�fD�D��D�D� D33D��D  D�3D�D��D&fD�fD �D �fD!fD!��D"&fD"�3D#FfD#�fD$33D$��D%  D%��D&@ D&��D',�D'��D(3D(�fD)33D)�3D*FfD*�fD+33D+� D,  D,�fD-33D-��D.&fD.��D/�D/��D0&fD0��D19�D1�3D2  D2��D3fD3��D4  D4�3D5@ D5��D6&fD6�3D73D7��D8,�D8�3D99�D9��D:&fD:�fD;33D;� D<  D<�fD=,�D=�fD>3D>�3D?&fD?��D@33D@� DA  DA�fDB,�DB��DC  DC��DDfDD�3DE�DE��DF9�DF�3DG&fDG��DH3DH� DI,�DI��DJ  DJ�fDK�DK��DL&fDL��DM�DM� DN33DN� DO  DO� DP3DP��DQ&fDQ�3DR3DR� DS,�DS��DT�DT�fDU33DU�fDV  DV��DW9�DW�3DX&fDX�3DY�DY�fDZ33DZ��D[  D[��D\�D\��D],�D]��D^  D^�3D_�D_��D`&fD`�3DaFfDa�fDb33Db�3Dc  Dc�fDd33Dd�3De&fDe� Df�Df�3Dg@ Dg��Dh,�Dh�3Di9�Di�fDj&fDj�fDk3Dk��Dl&fDl��Dm�Dm��Dn9�Dn��Do,�Do��Dp�Dp�fDq,�Dq�fDr�Dr� Ds,�Ds��Dt�Dt�fDu33Du��Dv&fDv��Dw�Dw��Dx&fDx�3Dy9�Dy��Dz&fDz��D{3D{� D|,�D|�3D}@ D}�3D~  D~�fD,�D��D�fD�I�D���D�� D�fD�Y�D���D�ɚD� D�P D��fD��3D�fD�S3D���D���D�	�D�L�D�� D�� D� D�VfD��fD���D��D�\�D���D��fD�	�D�FfD���D���D�	�D�I�D���D���D��D�I�D���D�ɚD��D�P D��3D��fD��D�\�D���D���D� D�S3D��3D��3D�fD�VfD��3D�ٚD��D�I�D���D�� D��D�L�D��fD�� D�fD�VfD��fD�ٚD�fD�VfD���D�ٚD��D�\�D���D�ٚD��D�Y�D���D��fD�3D�S3D�� D�� D��D�I�D��fD���D��D�Y�D��fD��fD�3D�S3D�� D�� D��D�L�D���D���D��D�Y�D���D��fD�fD�Y�D��3D��3D�3D�P D���D���D��D�Y�D���D�ٚD�fD�VfD��3D�� D� D�L�D���D���D��D�\�D���D��3D�3D�P D��3D�� D� D�P D�� D���D��D�L�D���D�ɚD��D�L�D���D���D� D�L�D���D���D��D�L�D���D�ɚD��D�\�D���D��fD�fD�S3D��fD��3D�3D�S3D��3D��3D�3D�S3D��3D��fD�fD�Y�D���D�ٚD�fD�VfD��3D��3D�3D�S3D��3D��fD�3D�S3D�� D��3D� D�P D��3D��3D�3D�S3D��3D��3D� D�S3D���D�� D� D�P D���D���D��D�L�D���D�ɚD��D�\�D���D���D�fD�P D�� D���D��D�I�D���D�ٚD��D�VfD��3D�� D�3D�L�D���D�ٚD��D�S3D��3D�� D� D�\�D���D��fD�3D�P D���D���D�	�D�FfD���D���D��D�VfD��3D���D��D�\�D���D��3D�3D�S3D��3D���D��D�L�D���D�ٚD��D�VfD��fD��fD�3D�P D���D���D�	�D�Y�D3D�� D�	�D�L�DÃ3D��fD�fD�P DČ�D���D��D�P DŌ�D�ɚD��D�P DƉ�D��fD� D�I�DǖfD�� D�	�D�Y�Dȓ3D���D�fD�P Dɉ�D��fD��D�FfDʓ3D���D�3D�L�D˙�D��3D�	�D�Y�D̐ D�ɚD�fD�P D͆fD��fD�	�D�I�DΓ3D���D�	�D�S3DϠ D��fD� D�Y�DГ3D�ɚD�3D�` Dѓ3D�ɚD�3D�\�Dғ3D���D�fD�L�DӆfD�� D��D�S3Dԉ�D��3D��D�VfDՐ D���D�3D�L�D֙�D��3D��D�VfDא D���D��D�S3D،�D�ٚD� D�L�DٖfD�� D�	�D�VfDڠ D��fD� D�Y�Dۓ3D���D��D�P D܉�D��3D��D�FfDݐ D���D�fD�L�DޖfD��3D� D�I�D߆fD��3D��D�I�D��3D���D�	�D�S3D��D��fD� D�\�D�3D�� D��D�S3D� D�� D��D�S3D� D�ɚD��D�S3D�3D���D��D�VfD�3D�� D�	�D�FfD�3D�� D��D�Y�D�fD��3D� D�L�D��D��fD��D�P D��D�ٚD��D�L�D뉚D��fD�fD�P D쉚D�ٚD�3D�S3D� D���D��D�\�DD��fD�fD�S3D��D���D�	�D�Y�D�fD��fD�3D�P D� D���D��D�VfD�3D�� D��D�\�D�D��3D� D�P D�� D���D��D�Y�D���D��3D�3D�P D���D�ɚD�fD�S3D���D�ɚD��D�VfD��3D�� D�	�D�Y�D��fD��fD� D�3D�9�D�s3D��fE q�E��E@ EٚEp EfE��E.fE�3ET�E�3Es3E�E� E	��E
,�E
� EC3E�3ES3Ei�E�fEi�E� EvfE�3EfE��E�E�fE�fE.fE��E;3E�3E�3EX E�3Eq�E� E��E3E&fE�fE9�E�fEVfE�Es3E� E !�E � E!C3E!ٚE"i�E"�3E#��E$fE$��E%�fE&k3E'3E'�3E(#3E(��E)A�E)�3E*c3E*� E,�E,�fE-�E-�3E.�E/( E/��E0$�E0��E1)�E233E2��E3$�E4!�E4� E5�E5��E6~fE6�3E7k3E8c3E8�fE9L�E:C3E:�3E;)�E<3E<��E=3E=� E>c3E?X E?�3E@A�EA33EA�fEB3EC3ECt�EC��ED� EEI�EE��EF��EG3EH�EH|�EH�fEI�3EJ@ EK)�EK��EL EL�EM^fEN@ EN��EO  EP	�EPvfEQ^fEQ��ER<�ES3ES�3ETa�ETɚEU��EV EV�EW` EX<�EX��EY�fEY� EZ��E[+3E\ E\nfE]L�E]��E^!�E_  E_i�E`C3E`�fEa�3Ea�fEb��Ec#3Ec�fEdk3EeD�Ee��Ef��Ef�fEgɚEh6fEi Ei� Ej^fEj�3Ek� El El� EmH En#3En�3Eoh Eo� Ep� EqfEq� Er;3Es�Esq�EtA�Et� Eus3EvA�Ev��Ewq�Ew�3Ex��Ex�3Ey��Ez� Ez��E{�fE|c3E|��E}y�E~8 E~��E^fE�fE�l�E��fE��fE�[3E��3E��E�H E�u�E�՚E�33E�^fE��fE�fE�m�E�� E�� E�L�E��3E��fE�VfE���E���E�0 E���E��fE�0 E�ZfE��fE� E�]�E���E� E�T E���E���E�FfE���E���E�:fE���E�� E�(�E�zfE�� E� E�g3E��3E�3E�U�E���E��fE�< E�� E��3E�  E�l E��3E�+3E�x E�ŚE�3E�bfE��fE���E�E�E��fE�� E�H E�� E��E�, E�s3E���E�&fE�l E��3E�� E�l�E���E�fE�S3E�� E�� E�6fE�3E��fE�0�E�x E���E��E�W3E��3E�� E�VfE���E�� E�M�E��3E���E�D�E���E�ݚE� E�q�E��3E�fE�X�E���E�3E�^fE�� E��3G�O�G�O�G�O�?L��G�O�?L��?fffG�O�?�  G�O�?���?�ff?�  ?ٙ�?�33@   @��@   @&ff@9��@Fff@Y��@l��@�  @�ff@�33@�  @�ff@�  @�  @ə�@�ff@�33@�33A��A  A  AffAffA&ffA0  A4��A<��AD��ANffAT��A\��Ad��Ak33Aq��A{33A���A���A���A���A�  A���A�  A�  A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444141141411111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     @ �@ �@ V@ *@ �@ "�@ (�@ /@ 6�@ >�@ F�@ R�@ _�@ l�@ {�@ ��@ ��@ ��@ �!@ ��@ �|@ ��@ �m@ �@�@b@�@,`@:�@I@V�@dZ@p�@~K@��@��@��@��@��@��@�;@�4@��@%@{@"�@1'@>�@K@X�@g�@t�@�d@�\@�@�M@�R@��@��@�H@�@@�9@	�@6@%�@33@A�@O0@]�@k�@v�@�p@��@�m@��@�k@��@�[@�@�@�Q@�@�@(�@5�@DD@Q=@`�@n�@|?@�7@��@��@�~@�w@�|@�#@�m@��@�@o@�@+�@;d@H]@T�@c�@r@~�@��@�H@�M@��@��@є@�;@�4@�9@v@�@""@/@>@K�@Z�@e�@t@��@�@�@��@�@ƨ@�O@�@�@�E@
�@�@$�@3�@@�@M�@\�@k�@x�@�p@�u@�y@�!@�k@�@խ@�T@��@��@J@�@(�@7L@E�@Q�@^5@l�@{�@��@��@�(@�-@�2@��@�#@��@��@	j@	b@	 @	-�@	:@	I�@	V�@	c�@	p�@	}�@	�P@	��@	�A@	�F@	�>@	�7@	�;@	��@	�,@
1@
{@
!s@
1'@
=q@
I�@
X�@
g�@
t�@
�@
�\@
�@
�@
��@
��@
��@
�T@
�@
��@
=@�@'�@5?@B�@O0@Z�@i!@v�@�@�u@��@�!@�k@�@�
@�@�@ �@@�@(�@5?@B�@Q=@_�@m�@|?@��@��@�y@�~@��@�|@��@��@��@@�@
@,`@:�@I@UU@bN@o�@~K@�P@��@��@��@��@ψ@�/@�4@�,@�@�@$�@2�@<�@K�@Z@hs@v@�W@��@�@�Y@��@Ĝ@Ӡ@�@��@  @
=@�@&�@1'@@,@N�@]�@l�@uk@�@�$@�@�f@��@��@��@�@�Y@ �@
�@�@)�@3�@C�@R�@\�@l�@|?@�|@�0@��@�r@�&@��@�h@�m@�q@ �@b@g@)�@9X@H]@Wb@g@qS@�W@��@��@��@�R@��@є@��@�(@��@�@�@&;@0x@?}@N�@X�@g@v@�@�@�a@��@��@ƨ@��@�@�@��@�@*@$�@33@B�@Q�@[z@j@y�@��@�@��@�!@�w@ȴ@׹@�`@�e@j@�@�@*S@3�@B�@P�@`A@n�@|�@��@�0@��@��@�@��@��@�`@�e@�@o@!s@+@:�@I@S�@b�@r@|?@��@�H@�5@�9@�>@�*@�/@�@�9@
=@{@�@.l@<�@K�@Z�@e	@t@�@�P@�U@�Y@�^@�@Ӡ@�@�@�9@
�@�@#�@3�@B�@Lu@\)@k.@uk@�p@�$@�a@�@��@�W@�\@�`@�e@@V@[@+@6�@D�@S�@^5@m�@~K@��@�<@�A@��@��@��@�/@�4@�q@ �@�@
@-@<�@F�@V�@e�@o�@�@��@��@��@�F@��@ψ@��@��@��@%@*@$.@/@>@Lu@V�@e�@t�@��@�@�U@�Y@��@Ĝ@Ӡ@�@��@  @�@�@&;@4�@B8@Lu@Z�@i!@ww@�|@��@�(@��@��@�c@�h@�`@�@ �@�@
@'�@5�@DD@Q�@_�@n�@|?@�D@��@��@�9@�@�o@�h@�@�@@�@[@+�@9X@FQ@T�@a�@o�@~K@��@��@�M@��@�2@ψ@��@�4@��@�@�@#�@0x@?}@M$@Wb@e	@t@�@��@�a@��@��@�W@��@�T@�L@��@J@�@'�@5�@C�@P�@^5@k�@y�@�|@�u@�@�@��@ȴ@խ@�@�@ @ �@ �@ *S@ 7L@ D�@ Q�@ _�@ l�@ z3@ �D@ ��@ ��@ ��@ �2@ �*@ ��@ �(@ �q@!@!�@!�@!+�@!<�@!Ji@!Wb@!e	@!r�@!�@!�P@!�H@!�A@!��@!��@!��@!��@!�@@!��@"�@"*@""�@"/�@">@"K@"X�@"ff@"t@"�@"��@"�U@"��@"��@"�J@"��@"��@"�@@"��@#	�@#6@#$�@#2�@#@,@#M�@#Z�@#i!@#z3@#�+@#�#@#��@#��@#�@#�@#׹@#�`@#�@$ �@$V@$�@$)�@$7�@$E�@$S�@$a�@$oF@$|?@$��@$��@$��@$�-@$��@$�|@$��@$��@$�q@%j@%�@%�@%,`@%:�@%H]@%V@%c�@%qS@%~�@%��@%�H@%��@%��@%@%�7@%�/@%��@%�~@&%@&@& �@&1�@&@,@&M�@&[z@&g�@&t@&��@&��@&�U@&�M@&��@&�@&խ@&�@&�@&��@'
�@'6@'(G@'5@@'B�@'O0@'\�@'i�@'ww@'��@'��@'��@'��@'��@'ȴ@'�\@'�T@'�L@(�@(b@([@(*S@(7L@(C�@(Q=@(bN@(oF@({�@(�7@(��@(��@(��@(�w@(�@(܀@(�(@(��@)�@)o@) @)-@):@)F�@)T�@)a�@)r�@)~�@)��@)�<@)��@)�-@)��@)є@)��@)��@)��@*�@*{@*!s@*.l@*?}@*K@*Wb@*g�@*t@*�W@*��@*�@*�M@*�^@*ƨ@*��@*�@*��@*�9@+�@+6@+#�@+3�@+C�@+O0@+[z@+k�@+x&@+��@+��@+�m@+��@+�@+�c@+��@+�@+��@+��@,V@,�@,'�@,7L@,G�@,SI@,_�@,oF@,{�@,�+@,��@,�A@,�-@,��@,�|@,�/@,��@,�@-�@-b@-�@-,`@-<�@-H]@-S�@-c�@-s_@-�@-��@-�U@-��@-�9@-Ĝ@-��@-�/@-��@-�,@.%@.�@."�@./@.?}@.K@.X@.g�@.t@.�W@.��@.�m@.�@.�R@.�@.�O@.��@.�@.��@/�@/�@/$�@/1'@/@�@/Q=@/]�@/i!@/x�@/��@/��@/�@/�@/�j@/ȴ@/խ@/�`@/�@/��@0V@0�@0&�@06�@0F�@0R�@0_�@0o�@0{�@0��@0��@0��@0�-@0�&@0�o@0܀@0��@0�q@1�@1b@1 @1-@1:@1FQ@1SI@1c�@1p�@1}�@1��@1��@1��@1��@1��@1��@1�;@1�@1�,@2%@2�@2$/@2/@2<@2I@2Z@2ff@2r�@2��@2�@2��@2��@2��@2ȴ@2�\@2�T@2�L@2��@3
�@36@3$�@31�@3B�@3O�@3]�@3j@3ww@3�@3�@3�(@3�r@3�j@3�c@3�\@3�m@3�e@4 �@4�@4O@4(�@45�@4F�@4S�@4a�@4m�@4{�@4��@4��@4�z@4�r@4��@4�@4�@4�(@4� @5@5@5[@5.l@5;d@5I@5UU@5�>@6@6D�@6�|@6�@7P�@7�\@7��@8@8Q=@8�@8ψ@9@9M$@9��@9�W@:v@:@�@:�@:��@;,`@;g�@;��@;��@<R�@<��@<��@<�,@=2�@=�4@=�/@>�@>O1@>��@>��@?5�@?oF@?��@?�@@V�@@�\@@��@A�@A@�@A{�@A�F@B/@Bi!@B��@B��@C	@CYn@C��@D{@DO1@D��@D��@E
�@EH]@E�|@EĜ@F�@FA�@F��@F��@G>�@G|?@G��@G�q@H3�@Hr@H�r@H�@Ia�@I��@I�C@J�@JD�@J�R@J�4@K$/@KZ�@K�u@L�@L7L@Lk�@L׹@M
=@M> @MqS@M��@N�@N>�@N��@N��@OJ@Ouk@O�4@O׹@P>�@PoF@P��@Q�@Q7�@Q�m@Qє@R@Rk.@R�T@R�*@S1'@Sa�@S��@S�,@T)�@TZ�@T��@T�L@UWb@U�+@U��@VB@VG�@V�Z@V�@W
=@Wm�@W�T@W��@X+@X\)@X��@X�@@YQ=@Y�W@Y�!@Z@Z;d@Z�I@Zƨ@[$/@[Q�@[�-@[�H@\?}@\k�@\�@\��@]R�@]�@]��@^	�@^hs@^�u@^�>@_""@_O1@_�@_��@`4�@`^�@`�^@`�@aC�@ar@a��@a�9@bYn@b�p@b��@cb@cp�@c�@c��@d&�@d�@d�~@e�@e:@e��@e��@f""@fN�@f�A@f��@g,`@gV�@g�!@g�#@h3�@h\)@h�F@iV@i8�@i�@i��@j�@j7�@j��@j�H@k�@k[z@k�@k�C@l"�@lt@l��@l�@mB�@m�u@m�^@nI@n^�@n�p@n�h@o(�@oO�@o��@o�@p�@pe	@p� @p��@qK@qp�@q�k@r	�@rS�@r�@r��@sV@sX�@s�@s�'@t33@tWa@t�y@t�@u4�@u{�@u@v�@vLv@v��@vլ@w�@w`B@w��@w��@x,`@xqS@x��@x�,@y=q@y�@y�J@z	�@zLv@z�P@z��@{@{Q�@{�@{Ӡ@|{@|v�@|��@|�,@};e@}}�@}�1@~@~B8@~�@~�1@  @_�@�m@�@�@�/r@�O1@�{�@���@���@��
@�+@�'�@�H�@�iy@��=@���@��i@��y@�6@�5@@�S�@�o�@��<@���@���@��w@��@�H@�aH@��7@���@��o@��@��@�3�@�L�@�s@���@���@�խ@��9@� @�EJ@�ff@�|?G�O�G�O�G�O�@ G�O�@ @ �G�O�@ jG�O�@ �@ v@ �@ 1@ 	�@ 
=@ �@ �@ V@ b@ �@ �@ �@ �@ B@ �@ �@  @ ""@ %�@ '�@ *S@ -@ 0x@ 3�@ 6�@ :@ <�@ @,@ C�@ G�@ I�@ M$@ P�@ T�@ Wb@ Z�@ ^5@ `�@ c�@ g�@ k.@ m�@ r@ uk@ ww@ {�@ ~K@ ��@ �G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AӉ7AӋDAӟ�AӰ!AӰ!AӴ9AӶFAӴ9AӶFAӸRAӾwA�A�ĜA�ƨA�ȴA�ȴA���A���A���A���A���A���A���A��#A��/A��#A���A���A��A��
A��#A��/A��/A��;A��HA��mA�bA�+A�&�A�&�A�"�A�  A���AӬA�`BAҺ^Aџ�A·+A�%A�{Aʇ+Aǩ�A�JAőhAľwAÅA���A�`BA�=qA��A�dZA�l�A�x�A�A�A�-A���A��wA�1'A�A�A�$�A��^A�jA��-A��A���A���A���A��jA� �A���A�XA��^A��A�oA�M�A��\A�C�A��;A�7LA��DA��A�  A�n�A��A��A��PA�$�A��DA���A��DA���A��uA���A�\)A��^A���A��PA�G�A�&�A��;A�ĜA��A{"�Ar��Ap-Am�Aj�Af��Ac\)AaC�A`�RA]K�AWK�AUoAS��APVAN��AM�;AL��AI%AG��AG33AE��AC��AC|�AC+ABbNA@��A=?}A;��A:�jA9�-A8�DA7dZA6=qA6{A5p�A4(�A3��A3�PA3A2ȴA2E�A1�A.9XA.~�A.~�A,9XA)��A)�A(ȴA&�jA%VA#��A!hsA�hA/A�/A�uA5?A��A�A�uA�
A{A�A�
A��A  A�AE�A9XA��A�;A�AM�A�\A��A��A��A��A�7A"�A=qA{Ap�A7LA�A�AVA�AA��A�#Ax�A\)AO�AoA�RAI�A �A�#A�7AK�A��A=qA �A�TA�hA`BA9XAt�A
��A
A�A	�TA	A	��A	x�A	"�A	oA�HA�+A��A�A�A$�Ap�AoA��AffAM�AƨA\)A��A�A��A��A��A��AM�A$�A1Ax�AVA ��A ��A �/A ��A (�A b@��m@��@�o@�@���@��D@�Z@�(�@��
@�\)@�"�@�^5@�@�G�@���@���@��@��@�P@�C�@�o@��y@�@�=q@��@��T@���@��@��@�x�@�`B@���@��@�Q�@�  @@�\)@��@�ff@���@���@��;@�S�@�"�@���@�D@畁@���@�\@���@�j@�dZ@�33@�;d@�33@�33@�33@�o@��y@���@�!@�ff@�=q@�p�@�`B@�bN@�l�@�"�@��H@�~�@�@�X@��;@�K�@�ȴ@ڰ!@ڏ\@�^5@�M�@�J@ى7@ٲ-@�?}@؛�@ؼj@؛�@ؓu@�Z@�|�@�;d@��y@�ȴ@�ff@���@�X@�O�@��`@�A�@Ӆ@�"�@��@ҸR@�@�x�@У�@��;@�dZ@��@���@�V@�`B@�7L@̴9@���@��y@�V@��@�@��T@�-@�{@�@�@ɡ�@���@Ǯ@�33@�@őh@��`@�(�@���@å�@�l�@�S�@�+@��H@��@��^@���@���@���@�p�@�z�@��;@��@���@�|�@�S�@�33@�
=@��!@�$�@��u@�A�@��w@�"�@�v�@��#@��-@�@��^@�x�@�O�@�%@���@�r�@���@��
@�\)@�@�M�@���@�hs@���@�I�@� �@��@���@��F@�;d@���@��\@�V@�$�@�{@���@�p�@�?}@�/@�Ĝ@�r�@�b@�ƨ@���@�C�@���@�$�@���@���@���@�p�@�O�@�&�@�Z@��@�ȴ@��\@�n�@�5?@���@��-@�x�@�X@�Ĝ@�b@���@�"�@��!@�M�@��@���@��h@�X@��@���@��@�bN@�I�@��;@��@�K�@���@��\@�-@���@��@�O�@��@��@��@�V@��/@��9@�Z@��w@�|�@�C�@��@���@�ff@�-@���@���@�/@��/@��D@�I�@��
@�\)@��@���@�n�@�5?@�{@���@���@���@��@�?}@��@��@�bN@�9X@� �@��
@��P@�\)@�C�@��y@��@���@��@��@��#@��-@�?}@�%@���@�Z@��@��
@��P@�o@��H@�ȴ@�ȴ@���@�V@�=q@��@���@��#@���@�G�@���@��j@��D@�r�@�A�@��@��@�|�@�K�@�K�@�33@��@���@�-@��^@�x�@�`B@�G�@�?}@�&�@��@��@�A�@�(�@�b@���@���@��P@�33@�
=@��@���@�E�@�J@��T@���@��-@�V@���@��j@��9@��@�9X@��
@���@�|�@�K�@��!@�^5@��T@���@�`B@��@���@��u@�1'@��P@�+@��y@��@��@��!@���@�v�@�E�@�=q@��#@��7@�G�@��@���@��D@�j@� �@�;@\)@~�R@~V@}��@}V@|�@|�D@|j@{�
@{t�@{C�@{"�@z�@z��@zn�@z^5@y��@yG�@x��@x�u@xr�@xb@w;d@v�@v��@v�+@v{@u�@u��@u�-@u�@t��@t1@s33@r�@q��@qG�@p�@pA�@o�@o��@o+@n�@n�+@nE�@n$�@m�T@mp�@mp�@m�@lZ@k��@kC�@j�H@j=q@i��@ihs@iG�@i7L@i&�@i&�@i&�@i�@h��@hr�@hb@g|�@fE�@f@e`B@e?}@e/@d��@d�j@d�@dj@d(�@ct�@b��@b-@a�#@a��@aG�@a�@`�`@`��@` �@_�;@_�@^��@\z�@[�
@[t�@[C�@Z~�@Y��@Y�@Y��@Y��@Yhs@Y%@X�9@XQ�@X �@W�@W;d@V�R@V5?@U�@T��@T��@T��@T�@T�@T�@T��@T�D@T�D@Tz�@TZ@T9X@T1@S�F@SS�@R�@R�\@R�@Qhs@QX@QX@P��@PA�@O�P@O|�@O;d@O�@O�@O�@O
=@O
=@N�@N�y@N�+@N{@M�-@MO�@M/@L��@Lz�@KdZ@Jn�@I��@I��@I��@I��@I�7@I��@Ix�@IX@IG�@HĜ@H �@G�w@GK�@F��@Fv�@FV@F5?@E�@E@E@E��@D�/@C��@C"�@B^5@AX@A�@@�`@@�@@Q�@@ �@?��@?��@>ȴ@=��@=�@<�@<�/@<��@<�@<z�@<Z@;�
@;S�@:��@:�\@:�\@:n�@:M�@:M�@:-@9��@9��@9x�@9&�@8��@8bN@7;d@6�y@6ȴ@6�R@6��@6v�@6V@6V@5�T@5�h@5O�@5O�@5�@4��@4�@4I�@4�@3��@3�m@3ƨ@3�@333@2�@2��@2��@2�!@2��@2�\@2�\@2~�@2^5@2=q@1�@1G�@1%@0�`@0Ĝ@0��@0  @/�;@/��@/�w@/�@/l�@.�@.��@.ff@.E�@.$�@.{@-�@-�T@-�-@,��@,�/@,�@,��@,��@,��@,I�@+��@+��@+33@+o@*�@*��@*�!@*�!@*�\@*^5@)��@)hs@)X@)&�@)�@)%@(��@(��@(bN@(1'@'�w@'��@'\)@&��@&��@&�+@&@%`B@$�j@$�@$�D@$I�@$(�@$(�@$(�@$(�@$(�@$�@$�@#��@#�m@#ƨ@#C�@"��@"�@!�#@!�@ r�@��@��@K�@��@@�@O�@V@�/@�@9X@�m@ƨ@��@dZ@�!@�!@n�@=q@�@�#@�#@��@X@%@�`@�`@Ĝ@r�@1'@��@�P@l�@l�@+@�R@��@�+@v�@$�@�T@��@@�-@��@�h@/@�/@�@j@@�@x�@��@��@�w@�@5?@O�@��@��@S�@
�\@
-@	�#@	G�@�;@
=@$�@{@�@9X@(�@�@�@��@ �`@ bN?��?���?��-?���?���?�dZ?�X?�b?���?�?�?�9X?�t�?���?�n�?�-?���?�G�?�  ??�;d?��?���?�O�?�D?�?�=q?�7L?���?��y?�ff?���?�j?���?�t�?���?�n�?�n�?�J?�J?�G�?�%?�  ?�\)?���?�/?�I�?�1?�dZ?�C�?���?��?ٺ^?�X?���?�r�?�r�?���?��y?�ff?�$�?�?��?���?���?�Z?��?��
?���?�n�?�M�?ѩ�?щ7?�G�?У�?Ѓ?� �?��;?Ͼw?�;d?Η�?�5??͑h?�/?��?�1?˅?�"�?��?��#?�X?���?�r�?�Q�?�1'?���?�+?��y?Ƨ�?�ff?�?��T?��T?š�?��?ļj?�z�?�Z?��?���?�o?�o?�33?\?�-?�J?���?���?���?��7?�%?�%?��`?���?��?�bN?��?�A�?�A�?���?�|�?��?���?���?���?�{?��?��?��-?��-?��-?��-?�p�?�p�?�p�?�/?�O�?�/?��?��?���?�I�?�I�?�j?�1?�(�?�I�?�ƨ?��m?���?�ƨ?���?���?�C�?�"�?�?���?���?���?���?���?���?���?���?���?���?���?�~�?���?���?��H?�?��H?�"�?�?�"�?�?�?�?�"�?�?�?�"�?�?�?�?�"�?�"�?�?��H?��H?��H?���?�?�"�?�C�?�C�?�C�?�"�?�C�?�"�?�"�?�"�?�C�?�C�?�C�?�dZ?�dZ?���?���?�dZ?��?��?���?���?��?�dZ?�dZ?�dZ?��?��?�dZ?��?�dZ?�C�?�"�?�C�?�"�?�"�?�"�?�"�?�?�?��H?���?���?���?���?�?��H?��H?���?���?��H?�?�"�?�"�?�"�?�C�?��?��?�ƨ?�ƨ?�dZ?�C�?�"�?�C�?�dZ?��?�C�?��H?��H?��H?��H?��H?�=q?��?��#?��^?��^?��#?���?���?��#?���?��#?���?��?��?�7L?�x�?�x�?���AӍPAӍPAӏ\AӍPAӋDAӉ7AӅAӃAӇ+AӋDAӇ+AӇ+AӇ+AӋDAӑhAӏ\Aӝ�Aӛ�Aӛ�Aӥ�AӰ!AӮAӰ!AӰ!AӰ!AӲ-AӰ!AӲ-AӴ9AӶFAӶFAӴ9AӴ9AӴ9AӶFAӴ9AӶFAӸRAӸRAӼjAӾwA���A���A�A�ĜA�ĜA�ƨA�ĜA�ĜA�ĜA�ƨA�ƨA�ƨA�ƨA�ȴA�ƨA�ƨA�ȴA�ȴA�ȴG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     AӉ7AӋDAӟ�AӰ!AӰ!AӴ9AӶFAӴ9AӶFAӸRAӾwA�A�ĜA�ƨA�ȴA�ȴA���A���A���A���A���A���A���A��#A��/A��#A���A���A��A��
A��#A��/A��/A��;A��HA��mA�bA�+A�&�A�&�A�"�A�  A���AӬA�`BAҺ^Aџ�A·+A�%A�{Aʇ+Aǩ�A�JAőhAľwAÅA���A�`BA�=qA��A�dZA�l�A�x�A�A�A�-A���A��wA�1'A�A�A�$�A��^A�jA��-A��A���A���A���A��jA� �A���A�XA��^A��A�oA�M�A��\A�C�A��;A�7LA��DA��A�  A�n�A��A��A��PA�$�A��DA���A��DA���A��uA���A�\)A��^A���A��PA�G�A�&�A��;A�ĜA��A{"�Ar��Ap-Am�Aj�Af��Ac\)AaC�A`�RA]K�AWK�AUoAS��APVAN��AM�;AL��AI%AG��AG33AE��AC��AC|�AC+ABbNA@��A=?}A;��A:�jA9�-A8�DA7dZA6=qA6{A5p�A4(�A3��A3�PA3A2ȴA2E�A1�A.9XA.~�A.~�A,9XA)��A)�A(ȴA&�jA%VA#��A!hsA�hA/A�/A�uA5?A��A�A�uA�
A{A�A�
A��A  A�AE�A9XA��A�;A�AM�A�\A��A��A��A��A�7A"�A=qA{Ap�A7LA�A�AVA�AA��A�#Ax�A\)AO�AoA�RAI�A �A�#A�7AK�A��A=qA �A�TA�hA`BA9XAt�A
��A
A�A	�TA	A	��A	x�A	"�A	oA�HA�+A��A�A�A$�Ap�AoA��AffAM�AƨA\)A��A�A��A��A��A��AM�A$�A1Ax�AVA ��A ��A �/A ��A (�A b@��m@��@�o@�@���@��D@�Z@�(�@��
@�\)@�"�@�^5@�@�G�@���@���@��@��@�P@�C�@�o@��y@�@�=q@��@��T@���@��@��@�x�@�`B@���@��@�Q�@�  @@�\)@��@�ff@���@���@��;@�S�@�"�@���@�D@畁@���@�\@���@�j@�dZ@�33@�;d@�33@�33@�33@�o@��y@���@�!@�ff@�=q@�p�@�`B@�bN@�l�@�"�@��H@�~�@�@�X@��;@�K�@�ȴ@ڰ!@ڏ\@�^5@�M�@�J@ى7@ٲ-@�?}@؛�@ؼj@؛�@ؓu@�Z@�|�@�;d@��y@�ȴ@�ff@���@�X@�O�@��`@�A�@Ӆ@�"�@��@ҸR@�@�x�@У�@��;@�dZ@��@���@�V@�`B@�7L@̴9@���@��y@�V@��@�@��T@�-@�{@�@�@ɡ�@���@Ǯ@�33@�@őh@��`@�(�@���@å�@�l�@�S�@�+@��H@��@��^@���@���@���@�p�@�z�@��;@��@���@�|�@�S�@�33@�
=@��!@�$�@��u@�A�@��w@�"�@�v�@��#@��-@�@��^@�x�@�O�@�%@���@�r�@���@��
@�\)@�@�M�@���@�hs@���@�I�@� �@��@���@��F@�;d@���@��\@�V@�$�@�{@���@�p�@�?}@�/@�Ĝ@�r�@�b@�ƨ@���@�C�@���@�$�@���@���@���@�p�@�O�@�&�@�Z@��@�ȴ@��\@�n�@�5?@���@��-@�x�@�X@�Ĝ@�b@���@�"�@��!@�M�@��@���@��h@�X@��@���@��@�bN@�I�@��;@��@�K�@���@��\@�-@���@��@�O�@��@��@��@�V@��/@��9@�Z@��w@�|�@�C�@��@���@�ff@�-@���@���@�/@��/@��D@�I�@��
@�\)@��@���@�n�@�5?@�{@���@���@���@��@�?}@��@��@�bN@�9X@� �@��
@��P@�\)@�C�@��y@��@���@��@��@��#@��-@�?}@�%@���@�Z@��@��
@��P@�o@��H@�ȴ@�ȴ@���@�V@�=q@��@���@��#@���@�G�@���@��j@��D@�r�@�A�@��@��@�|�@�K�@�K�@�33@��@���@�-@��^@�x�@�`B@�G�@�?}@�&�@��@��@�A�@�(�@�b@���@���@��P@�33@�
=@��@���@�E�@�J@��T@���@��-@�V@���@��j@��9@��@�9X@��
@���@�|�@�K�@��!@�^5@��T@���@�`B@��@���@��u@�1'@��P@�+@��y@��@��@��!@���@�v�@�E�@�=q@��#@��7@�G�@��@���@��D@�j@� �@�;@\)@~�R@~V@}��@}V@|�@|�D@|j@{�
@{t�@{C�@{"�@z�@z��@zn�@z^5@y��@yG�@x��@x�u@xr�@xb@w;d@v�@v��@v�+@v{@u�@u��@u�-@u�@t��@t1@s33@r�@q��@qG�@p�@pA�@o�@o��@o+@n�@n�+@nE�@n$�@m�T@mp�@mp�@m�@lZ@k��@kC�@j�H@j=q@i��@ihs@iG�@i7L@i&�@i&�@i&�@i�@h��@hr�@hb@g|�@fE�@f@e`B@e?}@e/@d��@d�j@d�@dj@d(�@ct�@b��@b-@a�#@a��@aG�@a�@`�`@`��@` �@_�;@_�@^��@\z�@[�
@[t�@[C�@Z~�@Y��@Y�@Y��@Y��@Yhs@Y%@X�9@XQ�@X �@W�@W;d@V�R@V5?@U�@T��@T��@T��@T�@T�@T�@T��@T�D@T�D@Tz�@TZ@T9X@T1@S�F@SS�@R�@R�\@R�@Qhs@QX@QX@P��@PA�@O�P@O|�@O;d@O�@O�@O�@O
=@O
=@N�@N�y@N�+@N{@M�-@MO�@M/@L��@Lz�@KdZ@Jn�@I��@I��@I��@I��@I�7@I��@Ix�@IX@IG�@HĜ@H �@G�w@GK�@F��@Fv�@FV@F5?@E�@E@E@E��@D�/@C��@C"�@B^5@AX@A�@@�`@@�@@Q�@@ �@?��@?��@>ȴ@=��@=�@<�@<�/@<��@<�@<z�@<Z@;�
@;S�@:��@:�\@:�\@:n�@:M�@:M�@:-@9��@9��@9x�@9&�@8��@8bN@7;d@6�y@6ȴ@6�R@6��@6v�@6V@6V@5�T@5�h@5O�@5O�@5�@4��@4�@4I�@4�@3��@3�m@3ƨ@3�@333@2�@2��@2��@2�!@2��@2�\@2�\@2~�@2^5@2=q@1�@1G�@1%@0�`@0Ĝ@0��@0  @/�;@/��@/�w@/�@/l�@.�@.��@.ff@.E�@.$�@.{@-�@-�T@-�-@,��@,�/@,�@,��@,��@,��@,I�@+��@+��@+33@+o@*�@*��@*�!@*�!@*�\@*^5@)��@)hs@)X@)&�@)�@)%@(��@(��@(bN@(1'@'�w@'��@'\)@&��@&��@&�+@&@%`B@$�j@$�@$�D@$I�@$(�@$(�@$(�@$(�@$(�@$�@$�@#��@#�m@#ƨ@#C�@"��@"�@!�#@!�@ r�@��@��@K�@��@@�@O�@V@�/@�@9X@�m@ƨ@��@dZ@�!@�!@n�@=q@�@�#@�#@��@X@%@�`@�`@Ĝ@r�@1'@��@�P@l�@l�@+@�R@��@�+@v�@$�@�T@��@@�-@��@�h@/@�/@�@j@@�@x�@��@��@�w@�@5?@O�@��@��@S�@
�\@
-@	�#@	G�@�;@
=@$�@{@�@9X@(�@�@�@��@ �`@ bN?��?���?��-?���?���?�dZ?�X?�b?���?�?�?�9X?�t�?���?�n�?�-?���?�G�?�  ??�;d?��?���?�O�?�D?�?�=q?�7L?���?��y?�ff?���?�j?���?�t�?���?�n�?�n�?�J?�J?�G�?�%?�  ?�\)?���?�/?�I�?�1?�dZ?�C�?���?��?ٺ^?�X?���?�r�?�r�?���?��y?�ff?�$�?�?��?���?���?�Z?��?��
?���?�n�?�M�?ѩ�?щ7?�G�?У�?Ѓ?� �?��;?Ͼw?�;d?Η�?�5??͑h?�/?��?�1?˅?�"�?��?��#?�X?���?�r�?�Q�?�1'?���?�+?��y?Ƨ�?�ff?�?��T?��T?š�?��?ļj?�z�?�Z?��?���?�o?�o?�33?\?�-?�J?���?���?���?��7?�%?�%?��`?���?��?�bN?��?�A�?�A�?���?�|�?��?���?���?���?�{?��?��?��-?��-?��-?��-?�p�?�p�?�p�?�/?�O�?�/?��?��?���?�I�?�I�?�j?�1?�(�?�I�?�ƨ?��m?���?�ƨ?���?���?�C�?�"�?�?���?���?���?���?���?���?���?���?���?���?���?�~�?���?���?��H?�?��H?�"�?�?�"�?�?�?�?�"�?�?�?�"�?�?�?�?�"�?�"�?�?��H?��H?��H?���?�?�"�?�C�?�C�?�C�?�"�?�C�?�"�?�"�?�"�?�C�?�C�?�C�?�dZ?�dZ?���?���?�dZ?��?��?���?���?��?�dZ?�dZ?�dZ?��?��?�dZ?��?�dZ?�C�?�"�?�C�?�"�?�"�?�"�?�"�?�?�?��H?���?���?���?���?�?��H?��H?���?���?��H?�?�"�?�"�?�"�?�C�?��?��?�ƨ?�ƨ?�dZ?�C�?�"�?�C�?�dZ?��?�C�?��H?��H?��H?��H?��H?�=q?��?��#?��^?��^?��#?���?���?��#?���?��#?���?��?��?�7L?�x�?�x�?���AӍPAӍPAӏ\AӍPAӋDAӉ7AӅAӃAӇ+AӋDAӇ+AӇ+AӇ+AӋDAӑhAӏ\Aӝ�Aӛ�Aӛ�Aӥ�AӰ!AӮAӰ!AӰ!AӰ!AӲ-AӰ!AӲ-AӴ9AӶFAӶFAӴ9AӴ9AӴ9AӶFAӴ9AӶFAӸRAӸRAӼjAӾwA���A���A�A�ĜA�ĜA�ƨA�ĜA�ĜA�ĜA�ƨA�ƨA�ƨA�ƨA�ȴA�ƨA�ƨA�ȴA�ȴA�ȴG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	K�B	K�B	K�B	J�B	K�B	J�B	J�B	K�B	J�B	K�B	J�B	J�B	J�B	I�B	I�B	J�B	I�B	I�B	I�B	I�B	I�B	I�B	I�B	I�B	I�B	I�B	J�B	J�B	J�B	J�B	J�B	I�B	I�B	I�B	J�B	M�B	r�B	�{B	��B	��B	��B	��B	��B	��B	��B	�dB	��B
/B
iyB
y�B
x�B
iyB
p�B
p�B
q�B
��B
��B
��B
�RB
ŢB
�}B
�`B
��B
�fB
�B
�)B
��BJB�B�B2-B$�B"�B:^BcTB��B�9B�FB�wB��B��B�NB�BB�B��B��B�}B�RB�!B��B�'B��B_;BO�B>wBVBZBq�Bn�B�PBw�Be`BZBT�B@�B�B
�yB
�3B
�bB
gmB
VB	��B	�^B	��B	�B	]/B	VB	9XB	+B	+B	 �B	�B	�B	�B	�B	)�B	0!B	9XB	A�B	:^B	;dB	cTB	�+B	��B	��B	�B	ǮB	��B	��B	��B	�;B	�#B	�/B	�`B	��B
\B
oB
hB
�B
.B
5?B
:^B
;dB
,B
�B
&�B
,B
�B
JB
hB
	7B	��B	�B	�B	�;B	�#B	�)B	�;B	�fB	�B	�B	�B	��B	��B	�B	�B	�B	�TB	�HB	�#B	�)B	�;B	�/B	�ZB	�B	��B
B
JB
�B
�B
�B
!�B
#�B
$�B
'�B
)�B
+B
,B
-B
-B
.B
/B
.B
/B
,B
-B
,B
.B
0!B
2-B
33B
33B
49B
5?B
5?B
9XB
<jB
?}B
?}B
=qB
6FB
49B
1'B
.B
/B
2-B
33B
1'B
2-B
49B
49B
33B
1'B
0!B
+B
#�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
#�B
%�B
&�B
'�B
&�B
&�B
#�B
!�B
"�B
"�B
"�B
!�B
#�B
%�B
%�B
#�B
#�B
 �B
�B
�B
oB
hB
bB
hB
bB
\B
VB
PB
VB
VB
VB
\B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
uB
uB
uB
oB
bB
\B
hB
bB
\B
VB
PB
VB
VB
PB
PB
PB
JB
PB
JB
JB
PB
PB
JB
DB
	7B
%B
B
B
B
B
  B
B
B
  B	��B	��B	��B	��B	��B
B
B
B
B
%B
+B
+B
+B
%B
%B
%B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
	7B

=B

=B
DB
PB
DB
DB
	7B
+B
+B
+B
%B
B
B
B
B
B
B
%B
%B
%B
%B
1B
+B

=B

=B

=B

=B

=B

=B
DB
DB
DB

=B
JB

=B
	7B

=B
	7B

=B
DB
PB
VB
VB
bB
bB
bB
bB
hB
hB
bB
bB
bB
bB
\B
VB
\B
\B
bB
bB
hB
hB
hB
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
�B
�B
�B
{B
�B
�B
�B
�B
�B
{B
{B
{B
{B
uB
uB
uB
uB
uB
uB
{B
{B
uB
{B
{B
{B
�B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
"�B
#�B
#�B
$�B
$�B
#�B
#�B
"�B
"�B
!�B
!�B
!�B
!�B
!�B
!�B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
$�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
)�B
(�B
(�B
)�B
)�B
+B
+B
+B
,B
,B
+B
+B
,B
,B
,B
,B
-B
,B
-B
-B
-B
-B
-B
-B
-B
.B
/B
/B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
33B
33B
33B
49B
49B
6FB
7LB
7LB
7LB
7LB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
=qB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
?}B
>wB
@�B
@�B
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
@�B
A�B
A�B
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
C�B
C�B
D�B
E�B
E�B
E�B
F�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
H�B
H�B
H�B
H�B
G�B
H�B
G�B
H�B
H�B
I�B
H�B
I�B
K�B
K�B
K�B
J�B
K�B
L�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
O�B
N�B
O�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
P�B
Q�B
Q�B
Q�B
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
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
T�B
W
B
W
B
W
B
W
B
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
YB
YB
YB
YB
YB
ZB
YB
YB
YB
ZB
[#B
\)B
]/B
\)B
]/B
\)B
]/B
]/B
\)B
\)B
^5B
^5B
^5B
_;B
_;B
^5B
_;B
_;B
^5B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
bNB
aHB
bNB
bNB
bNB
cTB
cTB
bNB
cTB
cTB
cTB
dZB
cTB
cTB
cTB
dZB
dZB
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
e`B
e`B
e`B
e`B
ffB
ffB
e`B
e`B
ffB
ffB
ffB
ffB
e`B
ffB
gmB
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
hsB
iyB
iyB
hsB
hsB
hsB
hsB
hsB
hsB
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
jB
k�B
l�B
l�B
l�B
k�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
n�B
o�B
p�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
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
t�B
u�B
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
w�B
v�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
y�B
x�B
x�B
x�B
y�B
y�B
z�B
y�B
z�B
z�B
z�B
z�B
{�B
z�B
{�B
{�B
{�B
{�B
{�B
~�B
~�B
~�B
�B
� B
�B
�B
�B
�B
�%B
�+B
�+B
�1B
�+B
�+B
�1B
�=B
�DB
�JB
�DB
�PB
�PB
�VB
�VB
�bB
�bB
�bB
�hB
�oB
�oB
�hB
�oB
�uB
�oB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�B
�B
�!B
�!B
�'B
�!B
�!B
�!B
�'B
�!B
�'B
�'B
�-B
�-B
�3B
�-B
�3B
�-B
�9B
�9B
�3B
�9B
�9B
�9B
�9B
�?B
�?B
�?B
�FB
�?B
�?B
�FB
�LB
�FB
�FB
�LB
�LB
�LB
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
�^B
�^B
�XB
�XB
�XB
�^B
�^B
�^B
�^B
�^B
�dB
�^B
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�jB
�jB
�jB
�jB
�qB
�qB
�jB
�qB
�qB
�jB
�qB
�qB
�wB
�qB
�qB
�qB
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
��B
�}B
��B
��B
�}B
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
��B
B
��B
��B
B
��B
��B
��B
B
B
B
B
B
B
B
ÖB
B
B
B
ÖB
B
ÖB
ÖB
ÖB
ÖB
ÖB
ÖB
ÖB
ÖB
ÖB
ĜB
ĜB
ÖB
ĜB
ÖB
ĜB
ÖB
ŢB
ĜB
ĜB
ĜB
ŢB
ĜB
ŢB
ŢB
ŢB
ŢB
ŢB
ƨB
ƨB
ŢB
ƨB
ƨB
ƨB
ƨB
ŢB
ƨB
ŢB
ŢB
ƨB
ƨB
ŢB
ƨB
ŢB
ƨB
ǮB
ƨB
ǮB
ǮB
ƨB
ƨB
ƨB
ǮB
ƨB
ǮB
ǮB
ǮB
ȴB
ǮB
ȴB
ȴB
ȴB
ȴB
ȴB
ɺB
ȴB
ȴB
ȴB
ȴB
ɺB
ɺB
ɺB
ɺB
ɺB
ɺB	K�B	L�B	L�B	K�B	K�B	J�B	K�B	L�B	M�B	K�B	K�B	K�B	L�B	L�B	J�B	L�B	J�B	K�B	K�B	L�B	I�B	K�B	J�B	J�B	K�B	J�B	K�B	K�B	J�B	J�B	J�B	J�B	K�B	K�B	J�B	J�B	J�B	J�B	K�B	J�B	J�B	J�B	J�B	J�B	J�B	I�B	I�B	J�B	J�B	J�B	I�B	I�B	I�B	J�B	I�B	J�B	I�B	I�B	J�B	J�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     B	D�B	D�B	D�B	C�B	D�B	C�B	C�B	D�B	C�B	D�B	C�B	C�B	C�B	B�B	B�B	C�B	B�B	B�B	B�B	B�B	B�B	B�B	B�B	B�B	B�B	B�B	C�B	C�B	C�B	C�B	C�B	B�B	B�B	B�B	C�B	F�B	k�B	�PB	�oB	�uB	��B	��B	��B	��B	��B	�9B	��B
'�B
bNB
r�B
q�B
bNB
iyB
iyB
jB
��B
�uB
��B
�'B
�wB
�RB
�5B
�B
�;B
��B
��B
�BB\B�B+B�B�B33B\)B�uB�B�B�LBĜBƨB�#B�B��BɺBÖB�RB�'B��B��B��B��BXBH�B7LBN�BR�BjBgmB�%Bp�B^5BR�BM�B9XBoB
�NB
�B
�7B
`BB
N�B	�B	�3B	�\B	y�B	VB	N�B	2-B	#�B	#�B	�B	oB	�B	�B	�B	"�B	(�B	2-B	:^B	33B	49B	\)B	� B	�hB	��B	��B	��B	ƨB	ȴB	��B	�B	��B	�B	�5B	��B
1B
DB

=B
�B
&�B
.B
33B
49B
$�B
{B
�B
$�B
bB
B

=B
B	�B	�B	�TB	�B	��B	�B	�B	�;B	�fB	�sB	�B	�B	�B	�B	�sB	�`B	�/B	�#B	��B	�B	�B	�
B	�5B	�mB	�B	��B
%B
�B
�B
�B
�B
�B
�B
!�B
#�B
$�B
%�B
&�B
&�B
'�B
(�B
'�B
(�B
%�B
&�B
%�B
'�B
)�B
,B
-B
-B
.B
/B
/B
33B
6FB
9XB
9XB
7LB
0!B
.B
+B
'�B
(�B
,B
-B
+B
,B
.B
.B
-B
+B
)�B
$�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
{B
{B
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
bB
JB
DB

=B
DB

=B
	7B
1B
+B
1B
1B
1B
	7B
JB
oB
uB
uB
uB
oB
oB
oB
hB
hB
bB
\B
PB
PB
PB
PB
JB

=B
	7B
DB

=B
	7B
1B
+B
1B
1B
+B
+B
+B
%B
+B
%B
%B
+B
+B
%B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
  B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
+B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
B
B
B
B
B
B
+B
1B
1B

=B

=B

=B

=B
DB
DB

=B

=B

=B

=B
	7B
1B
	7B
	7B

=B

=B
DB
DB
DB
DB
DB
DB
JB
JB
PB
PB
PB
VB
VB
\B
\B
\B
bB
\B
\B
VB
bB
bB
bB
\B
bB
VB
VB
VB
VB
PB
PB
PB
PB
PB
PB
VB
VB
PB
VB
VB
VB
\B
\B
VB
VB
\B
\B
\B
\B
\B
\B
\B
\B
\B
bB
bB
hB
bB
bB
bB
bB
bB
bB
bB
hB
bB
oB
oB
oB
�B
{B
�B
{B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
#�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
%�B
&�B
$�B
%�B
%�B
&�B
&�B
&�B
'�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
)�B
)�B
+B
+B
+B
,B
,B
-B
-B
.B
.B
.B
/B
/B
1'B
2-B
2-B
2-B
2-B
33B
49B
49B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
8RB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
:^B
9XB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
;dB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
@�B
@�B
@�B
A�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
C�B
C�B
C�B
C�B
B�B
C�B
B�B
C�B
C�B
D�B
C�B
D�B
F�B
F�B
F�B
E�B
F�B
G�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
J�B
I�B
J�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
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
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
O�B
Q�B
Q�B
Q�B
Q�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
S�B
S�B
S�B
T�B
VB
W
B
XB
W
B
XB
W
B
XB
XB
W
B
W
B
YB
YB
YB
ZB
ZB
YB
ZB
ZB
YB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
\)B
]/B
]/B
]/B
^5B
^5B
]/B
^5B
^5B
^5B
_;B
^5B
^5B
^5B
_;B
_;B
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
`BB
`BB
aHB
aHB
`BB
`BB
aHB
aHB
aHB
aHB
`BB
aHB
bNB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
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
gmB
gmB
gmB
ffB
gmB
gmB
gmB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
jB
iyB
jB
k�B
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
jB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
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
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
t�B
s�B
s�B
s�B
t�B
t�B
u�B
t�B
u�B
u�B
u�B
u�B
v�B
u�B
v�B
v�B
v�B
v�B
v�B
y�B
y�B
y�B
{�B
z�B
|�B
|�B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�%B
�+B
�1B
�+B
�7B
�7B
�=B
�=B
�JB
�JB
�JB
�PB
�VB
�VB
�PB
�VB
�\B
�VB
�bB
�bB
�hB
�hB
�bB
�uB
�oB
�uB
�uB
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
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�B
�!B
�B
�'B
�'B
�!B
�'B
�'B
�'B
�'B
�-B
�-B
�-B
�3B
�-B
�-B
�3B
�9B
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
�FB
�FB
�FB
�LB
�LB
�FB
�FB
�FB
�LB
�LB
�LB
�LB
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
�XB
�XB
�XB
�XB
�^B
�dB
�^B
�dB
�dB
�^B
�dB
�dB
�jB
�dB
�dB
�dB
�jB
�jB
�jB
�qB
�qB
�qB
�jB
�qB
�qB
�wB
�qB
�wB
�qB
�wB
�wB
�qB
�qB
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
�}B
�}B
�wB
�}B
�}B
�}B
�}B
�}B
�}B
��B
�}B
�}B
��B
�}B
�}B
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
��B
��B
��B
��B
B
B
��B
B
��B
B
��B
ÖB
B
B
B
ĜB
ÖB
ĜB
ĜB
ĜB
ĜB
ĜB
ŢB
ŢB
ĜB
ŢB
ŢB
ŢB
ŢB
ĜB
ŢB
ĜB
ĜB
ŢB
ŢB
ĜB
ŢB
ĜB
ŢB
ƨB
ŢB
ƨB
ƨB
ŢB
ŢB
ŢB
ƨB
ŢB
ƨB
ƨB
ƨB
ǮB
ƨB
ǮB
ǮB
ǮB
ǮB
ǮB
ȴB
ǮB
ǮB
ǮB
ǮB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB	D�B	E�B	E�B	D�B	D�B	C�B	D�B	E�B	F�B	D�B	D�B	D�B	E�B	E�B	C�B	E�B	C�B	D�B	D�B	E�B	B�B	D�B	C�B	C�B	D�B	C�B	D�B	D�B	C�B	C�B	C�B	C�B	D�B	D�B	C�B	C�B	C�B	C�B	D�B	C�B	C�B	C�B	C�B	C�B	C�B	B�B	B�B	C�B	C�B	C�B	B�B	B�B	B�B	C�B	B�B	C�B	B�B	B�B	C�B	C�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�202012170104392021061413544120210614135441202106141748242021061417482420210614174824202012170104392021061413544120210614135441202106141748242021061417482420210614174824PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9998 (+/-0), vertically averaged dS = -0.007 (+/-0)                                                                                                                                                                                                    surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9998 (+/-0), vertically averaged dS = -0.007 (+/-0)                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2020121701043920201217010439  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020121701043920201217010439QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020121701043920201217010439QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216021020210722160210IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                