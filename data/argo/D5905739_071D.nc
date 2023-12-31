CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-06-06T19:00:32Z creation      
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
resolution        =���   axis      Z        %`  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	X  a4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     %`  j�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	X  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     %`  �D   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     %`  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	X  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     %`  �\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	X �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     %`    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     %` At   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	X f�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     %` p,   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	X ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     %` ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �L   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �h   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �p   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � �D   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � ��Argo profile    3.1 1.2 19500101000000  20190606190032  20210617131520  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               G   GDD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @ؾ�Vٿ�@ؾ�Vٿ�11  @ؾ�O�P@ؾ�O�P@5��;dZ@5��;dZ�c�w�|�c�w�|11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?fff?�33@9��@�  @�  @�33@�33A��A  A#33A@  Ac33A�  A���A���A���A�  A�  A�33A�  B ��BffB  BffB ��B(ffB0  B8ffB@��BHffBP  BX  B`  Bh  BpffBw��B�  B�  B�  B�33B�  B���B�33B�  B���B�33B�ffB�ffB�ffB�33B�  B�  B�33B�  BǙ�B˙�B�  B�ffB�33Bۙ�B���B�33B虚B�ffB�  B�33B���B�33B���C�C33C  C��C	�fC  C33C  C��C  C33C  C��C  C33C L�C"�C#��C&  C(33C*  C+��C-�fC0  C2�C4L�C6�C7��C9�fC<  C>�C@L�CB  CC�3CE��CG�fCI�fCL  CN�CP  CR�CT33CV33CX33CZ33C\33C^33C`33Cb33Cd�Cf33Ch33CjL�ClL�CnL�Cp  Cr  CtL�Cv33CxL�Cz33C|L�C~L�C�&fC�&fC�33C�&fC�&fC�&fC��C��fC��fC��fC��fC��fC��fC��fC��3C��3C�  C�  C��C��C��C��C�&fC��C�&fC�&fC�&fC�  C�ٚC��fC��fC��fC��3C��3C��3C��3C��fC��fC��fC��3C��3C��3C��3C��fC��C��C��C��C��C�  C��3C��3C��3C��fC��fC�ٚC�  C�&fC�&fC��C��C��C��C�  C�  C�  C��C�  C��C��C�&fC�&fC�  C�ٚC��fC��3C�  C�  C�  C��C��C�  C��fC��3C�  C��C�&fC��C��fC��3C��C��C�  C�ٚC��fC��fC��3C��3C��3C��3C��3C�  C�  C��C��C��C�&fC�&fC�&fC�33C��C�&fC�&fC�&fC�&fC�33C��C��fC�ٚC��C�&fC�&fC��3C�&fC�&fC��C��C��D fD �fD  Dy�D�3Ds3D�D��D�D�fD  Dy�D�3D�3D�D�fD  Ds3D	3D	�fD
  D
y�D
�3Ds3D��D�3D3D��DfD�fD  D�fD  Ds3D�D��DfD�fD  Dy�D��Ds3D�3Dl�D�3Ds3D�3Dy�D��Dy�D  D� DfD� D  Ds3D�3Ds3D�3DffD�3D�3D�D��D �D � D ��D!y�D"�D"��D#3D#� D$  D$s3D$��D%�fD%�3D&s3D'3D'�fD'��D(�3D)fD)y�D*3D*�fD*��D+�3D+��D,y�D-3D-� D-��D.�3D/  D/s3D0�D0y�D0�3D1�fD2  D2��D3  D3�3D4fD4� D53D5�fD6  D6�3D7fD7s3D8�D8� D8�3D9�fD:  D:y�D;�D;� D;��D<��D=  D=l�D>�D>y�D>��D?��D@  D@s3DA�DA� DA�3DB��DC  DCs3DD3DD� DD��DE��DFfDFs3DG3DG�fDG��DH�3DI�DI�fDI�3DJs3DK�DKy�DK��DL��DL��DMs3DNfDN��DOfDO�fDP�DP�fDP��DQ��DQ��DRl�DS  DSs3DS��DT�fDT��DUl�DV�DV� DV�3DW��DW��DXs3DYfDY��DZ�DZ� D[�D[�fD[��D\��D]fD]� D^�D^��D_fD_y�D_��D`��D`��Das3DbfDby�Db�3Dc�fDc��Ddl�DefDes3De��Df�fDg  Dg��DhfDhy�Dh��Di� Di�3Djl�DkfDky�Dk��Dl��Dl��Dml�DnfDn� Dn��Do�fDp  Dps3Dq�Dq� Dq��Dr�3DsfDss3Dt3Dt� Dt�3Du�fDv  Dvl�Dw�Dw� Dw�3Dx�3Dy  Dyy�Dz3Dz�fDz��D{�3D|  D|s3D}�D}� D}�3D~��D~��Ds3D�fD�@ D�vfD��fD���D�6fD��fD�� D���D�I�D��3D�� D��fD�6fD�� D���D���D�I�D��fD�� D���D�6fD��fD�� D���D�L�D��fD��fD�3D�@ D�� D�� D���D�6fD��fD��3D�3D�<�D�� D���D���D�6fD�vfD��fD�3D�C3D�|�D���D��D�FfD��3D��3D�  D�9�D�y�D�ɚD�fD�<�D���D��fD�3D�<�D�|�D�ɚD�3D�@ D�|�D��fD�fD�@ D�y�D��fD�3D�9�D��fD�� D��fD�FfD�� D���D�	�D�C3D�|�D���D�3D�<�D��fD�� D�3D�<�D��fD���D�fD�<�D�� D�ɚD���D�6fD�|�D��3D��D�<�D�s3D���D�  D�C3D���D���D��3D�6fD�y�D�� D�3D�FfD���D���D�  D�33D�vfD���D�  D�@ D��3D��fD�fD�I�D���D���D� D�C3D�vfD��fD���D�<�D��fD�ɚD� D�C3D�vfD���D�  D�FfD���D�� D�  D�6fD�y�D���D�  D�FfD���D���D�  D�33D�vfD�� D�  D�I�D���D�� D�3D�6fD�|�D�� D�3D�FfD�� D�� D��fD�<�D�� D��fD�	�D�<�D�vfD���D�  D�C3D���D�ɚD�  D�6fD�|�D��3D��D�P D��3D���D�  D�FfD���D�� D�	�D�@ D���D�� D�fD�<�D��fD���D�3D�9�D��3D���D�3D�6fD�� D�ɚD�3D�9�D��3D���D�fD�<�D���D���D��fD�@ D���D�� D���D�C3D���D��3D���D�FfD�|�D��fD�  D�FfD�� D���D�  D�L�D��fD�� D�fD�C3D�y�D�� D�	�D�<�D�vfD�� D�	�D�@ D�vfD��3D� D�@ D�y�D�� D�fD�L�D��3D��fD�3D�L�D�� D���D�  D�I�D�� D��3D���D�C3D�D��3D��fD�@ DÆfD�� D�3D�9�DĀ D��fD��D�@ D�y�Dż�D�3D�FfDƌ�D�� D�fD�6fDǀ D��3D�	�D�L�DȌ�D�� D�  D�33D�vfDɹ�D�  D�C3Dʃ3D��fD�3D�FfDˆfD��fD�fD�I�D̆fD�ɚD�	�D�L�D͌�D���D��D�L�DΉ�D��fD�fD�C3Dσ3D��3D�3D�@ DЀ D�� D���D�6fD�vfD�� D�	�D�FfD҆fD��3D�  D�@ DӀ D�� D���D�9�D�vfD��3D��D�L�DՉ�D�ɚD�fD�C3Dփ3D��3D�3D�@ D׀ D׼�D���D�6fD�s3D��3D� D�P Dِ D���D��D�I�Dډ�D��fD�3D�<�D�y�D��3D��D�I�D܃3Dܼ�D���D�9�D݀ D���D�	�D�C3D�|�D޼�D�3D�P D߉�D�ɚD�fD�FfD�� D��D���D�9�D�vfDṚD��fD�6fD�s3D��3D� D�L�D㉚D��fD�3D�C3D� D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�9�D�y�D��D���D�@ D� D��D�3D�@ D�3D��3D�3D�C3D� D�� D�  D�<�D�|�D�� D�  D�C3D�fD��fD�fD�L�D�fD�ɚD�	�D�I�D�fD�ɚD�	�D�C3D� D��3D�  D�C3D�� D�� D���D�9�D�y�D�fD��fD�6fD�vfD��3D�3D�L�D��D�ɚD�3D�C3D�|�D���D���D�9�D�vfD��fD��fD�@ D�� D�� D� D�L�D���D�ɚD�fD�C3D��3D�� D�  D�<�D�|�D���D���D�y�D�S3D�9�E fE ~fEl�E�EY�ENfE�3E<�E+3E��E EfE�3E�3E�fE	a�E	ٚE
��EA�E�fE�fE  E��E� E�3E[3E;3E�fE�fE�3E� E9�E E��EffE�3EA�E&fE�3EvfE�fE��E( E Ep E�3E� E!�E�fEffE>fE�fE � E �3E!��E"9�E#  E#�3E#��E$�fE%T�E&9�E&�fE'�3E'� E(k3E)Q�E)��E*�3E+ E+��E,ffE,њE-�3E.  E.��E/ffE0FfE0�fE13E1�E2X E333E3��E4l�E5;3E5��E6i�E6��E7�3E7�fE8� E9��E9��E:��E;i�E;��E<�fE=C3E=�fE>[3E? E?q�E@,�E@� EA�fEB<�EB��EC@ EC� ED�fEE6fEE� EF~fEG$�EGɚEHnfEI�EI� EJ^fEJ� EKNfEK�fEL� EM,�EM�3ENl�EOVfEO� EP��EQ)�EQ� ERa�ER��ES�fET#3ET�fEUI�EV( EV�fEWS3EW�fEXvfEYL�EYٚEZffEZ�fE[�3E\VfE\� E]k3E^>fE^ɚE_X E`&fE`�3Ea>fEaɚEb��Ec  Ec�Eda�Ee$�Ee��EfffEf��Eg� Eh3Eh�3Eis3Ej Ej�3Ek33Ek�fEl��Em1�Em�fEnk3Eo3Eo�fEpQ�Ep�3Eqk3Er3Er��Es` Et	�>���>���>���>���>���>���>���>L��>���>���>���>���>���>���>���>���>���>���>���>���>���?   >���>���>���>���?   ?��?L��?fff?���?�ff?���?ٙ�@   @33@,��@9��@L��@l��@y��@���@�ff@�  @���@���@�33@�  @���@陚@���A��A33A33A33A!��A+33A1��A8  AA��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141414414144444144141444411111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ?fff?�33@��@Y��@�  @�  @�33@�33A	��A  A+33AH  Ak33A�  A���A���A���A�  A�  A�33A�  B��B
ffB  BffB"��B*ffB2  B:ffBB��BJffBR  BZ  Bb  Bj  BrffBy��B�  B�  B�  B�33B�  B���B�33B�  B���B�33B�ffB�ffB�ffB�33B�  B�  B�33B�  Bș�B̙�B�  B�ffB�33Bܙ�B���B�33B陚B�ffB�  B�33B���B�33C ffC��C�3C� CL�C
ffC� C�3C� CL�C� C�3C� CL�C� C�3C ��C"��C$L�C&� C(�3C*� C,L�C.ffC0� C2��C4��C6��C8L�C:ffC<� C>��C@��CB� CD33CFL�CHffCJffCL� CN��CP� CR��CT�3CV�3CX�3CZ�3C\�3C^�3C`�3Cb�3Cd��Cf�3Ch�3Cj��Cl��Cn��Cp� Cr� Ct��Cv�3Cx��Cz�3C|��C~��C�ffC�ffC�s3C�ffC�ffC�ffC�L�C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�33C�@ C�@ C�L�C�L�C�L�C�Y�C�ffC�Y�C�ffC�ffC�ffC�@ C��C�&fC�&fC�&fC�33C�33C�33C�33C�&fC�&fC�&fC�33C�33C�33C�33C�&fC�L�C�Y�C�Y�C�L�C�L�C�@ C�33C�33C�33C�&fC�&fC��C�@ C�ffC�ffC�Y�C�Y�C�Y�C�L�C�@ C�@ C�@ C�L�C�@ C�L�C�L�C�ffC�ffC�@ C��C�&fC�33C�@ C�@ C�@ C�Y�C�Y�C�@ C�&fC�33C�@ C�Y�C�ffC�L�C�&fC�33C�L�C�Y�C�@ C��C�&fC�&fC�33C�33C�33C�33C�33C�@ C�@ C�L�C�L�C�Y�C�ffC�ffC�ffC�s3C�Y�C�ffC�ffC�ffC�ffC�s3C�L�C�&fC��C�L�C�ffC�ffC�33C�ffC�ffC�Y�C�L�C�Y�D &fD �fD  D��D3D�3D9�D��D,�D�fD  D��D3D�3D,�D�fD  D�3D	33D	�fD
  D
��D3D�3D�D�3D33D��D&fD�fD  D�fD  D�3D9�D��D&fD�fD  D��D�D�3D3D��D3D�3D3D��D�D��D  D� D&fD� D  D�3D3D�3D3D�fD3D�3D,�D��D ,�D � D!�D!��D"9�D"��D#33D#� D$  D$�3D%�D%�fD&3D&�3D'33D'�fD(�D(�3D)&fD)��D*33D*�fD+�D+�3D,�D,��D-33D-� D.�D.�3D/  D/�3D0,�D0��D13D1�fD2@ D2��D3  D3�3D4&fD4� D533D5�fD6  D6�3D7&fD7�3D8,�D8� D93D9�fD:  D:��D;,�D;� D<�D<��D=  D=��D>,�D>��D?�D?��D@  D@�3DA,�DA� DB3DB��DC  DC�3DD33DD� DE�DE��DF&fDF�3DG33DG�fDH�DH�3DI,�DI�fDJ3DJ�3DK,�DK��DL�DL��DM�DM�3DN&fDN��DO&fDO�fDP9�DP�fDQ�DQ��DR�DR��DS  DS�3DT�DT�fDU�DU��DV,�DV� DW3DW��DX�DX�3DY&fDY��DZ,�DZ� D[9�D[�fD\�D\��D]&fD]� D^9�D^��D_&fD_��D`�D`��Da�Da�3Db&fDb��Dc3Dc�fDd�Dd��De&fDe�3Df�Df�fDg@ Dg��Dh&fDh��Di�Di� Dj3Dj��Dk&fDk��Dl�Dl��Dm�Dm��Dn&fDn� Do�Do�fDp  Dp�3Dq,�Dq� Dr�Dr�3Ds&fDs�3Dt33Dt� Du3Du�fDv  Dv��Dw,�Dw� Dx3Dx�3Dy  Dy��Dz33Dz�fD{�D{�3D|  D|�3D},�D}� D~3D~��D�D�3D�fD�P D��fD��fD��D�FfD��fD�� D��D�Y�D��3D�� D�fD�FfD�� D���D�	�D�Y�D��fD�� D��D�FfD��fD�� D��D�\�D��fD��fD�3D�P D�� D�� D�	�D�FfD��fD��3D�3D�L�D�� D�ɚD�	�D�FfD��fD��fD�3D�S3D���D���D��D�VfD��3D��3D� D�I�D���D�ٚD�fD�L�D���D��fD�3D�L�D���D�ٚD�3D�P D���D��fD�fD�P D���D��fD�3D�I�D��fD�� D�fD�VfD�� D���D��D�S3D���D���D�3D�L�D��fD�� D�3D�L�D��fD���D�fD�L�D�� D�ٚD��D�FfD���D��3D��D�L�D��3D�ɚD� D�S3D���D���D�3D�FfD���D�� D�3D�VfD���D���D� D�C3D��fD�ɚD� D�P D��3D��fD�fD�Y�D���D���D�  D�S3D��fD��fD�	�D�L�D��fD�ٚD�  D�S3D��fD���D� D�VfD���D�� D� D�FfD���D�ɚD� D�VfD���D���D� D�C3D��fD�� D� D�Y�D���D�� D�3D�FfD���D�� D�3D�VfD�� D�� D�fD�L�D�� D��fD��D�L�D��fD�ɚD� D�S3D���D�ٚD� D�FfD���D��3D��D�` D��3D�ɚD� D�VfD���D�� D��D�P D���D�� D�fD�L�D��fD���D�3D�I�D��3D���D�3D�FfD�� D�ٚD�3D�I�D��3D���D�fD�L�D���D���D�fD�P D���D�� D�	�D�S3D���D��3D��D�VfD���D��fD� D�VfD�� D�ɚD� D�\�D��fD�� D�fD�S3D���D�� D��D�L�D��fD�� D��D�P D��fD��3D�  D�P D���D�� D�fD�\�D��3D��fD�3D�\�D�� D�ɚD� D�Y�D�� D��3D��D�S3D�D��3D�fD�P DÖfD�� D�3D�I�DĐ D��fD��D�P Dŉ�D���D�3D�VfDƜ�D�� D�fD�FfDǐ D��3D��D�\�DȜ�D�� D� D�C3DɆfD�ɚD� D�S3Dʓ3D��fD�3D�VfD˖fD��fD�fD�Y�D̖fD�ٚD��D�\�D͜�D���D��D�\�DΙ�D��fD�fD�S3Dϓ3D��3D�3D�P DА D�� D��D�FfDцfD�� D��D�VfDҖfD��3D� D�P DӐ D�� D��D�I�DԆfD��3D��D�\�Dՙ�D�ٚD�fD�S3D֓3D��3D�3D�P Dא D���D��D�FfD؃3D��3D�  D�` D٠ D���D��D�Y�Dڙ�D��fD�3D�L�Dۉ�D��3D��D�Y�Dܓ3D���D��D�I�Dݐ D���D��D�S3Dތ�D���D�3D�` Dߙ�D�ٚD�fD�VfD�� D���D�	�D�I�D�fD�ɚD�fD�FfD�3D��3D�  D�\�D㙚D��fD�3D�S3D� D���D�	�D�L�D��D���D�	�D�L�D��D���D��D�I�D牚D���D��D�P D� D���D�3D�P D�3D��3D�3D�S3D� D�� D� D�L�D��D�� D� D�S3D�fD��fD�fD�\�D�fD�ٚD��D�Y�D�fD�ٚD��D�S3D� D��3D� D�S3D� D�� D��D�I�D�D��fD�fD�FfD�fD��3D�#3D�\�D��D�ٚD�3D�S3D��D���D��D�I�D��fD��fD�fD�P D�� D�� D�  D�\�D���D�ٚD�fD�S3D��3D�� D� D�L�D���D�ɚD�	�D���D�c3D�I�E fE �fEt�E�Ea�EVfE�3ED�E33E��E  EfE�3E3E�fE	i�E	�E
��EI�E�fE�fE( E��E� E�3Ec3EC3E�fE�fE�3E� EA�E  E��EnfE�3EI�E.fE�3E~fE�fE��E0 E Ex E�3E� E)�EfEnfEFfE�fE � E �3E!��E"A�E#( E#�3E$�E$�fE%\�E&A�E&�fE'�3E(  E(s3E)Y�E)��E*�3E+ E+��E,nfE,ٚE-�3E.( E/�E/nfE0NfE0�fE1#3E1��E2` E3;3E3��E4t�E5C3E5��E6q�E6��E7�3E7�fE8� E9��E9��E:��E;q�E;ɚE<�fE=K3E=�fE>c3E?  E?y�E@4�E@� EA�fEBD�EB��ECH EC� ED�fEE>fEE� EF�fEG,�EGњEHvfEI�EI� EJffEJ� EKVfEK�fEL� EM4�EM�3ENt�EO^fEO� EP��EQ1�EQ� ERi�ES�ES�fET+3ET�fEUQ�EV0 EV�fEW[3EW�fEX~fEYT�EY�EZnfEZ�fE[�3E\^fE\� E]s3E^FfE^њE_` E`.fE`�3EaFfEaњEb��Ec( Ec�Edi�Ee,�Ee��EfnfEf��Eg� Eh#3Eh�3Ei{3Ej Ej�3Ek;3Ek�fEl��Em9�Em�fEns3Eo3Eo�fEpY�Ep�3Eqs3Er3Er��Esh Et�?L��G�O�?L��G�O�?L��G�O�G�O�?333G�O�?L��G�O�G�O�G�O�G�O�G�O�?L��G�O�G�O�?L��G�O�?fffG�O�G�O�G�O�G�O�?fff?�  ?���?�ff?�33?���?�ff@ff@��@   @333@L��@Y��@l��@�ff@���@���@�ff@�  @���@ə�@�33@�  @���@���A��A��A33A33A#33A)��A333A9��A@  AI��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141414414144444144141444411111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                @ �@ %@ �@ �@ O@ ""@ )�@ 0x@ 7L@ =q@ E�@ Q�@ `�@ m:@ {�@ ��@ ��@ ��@ �~@ �w@ ��@ ��@ ��@ ��@@o@g@,`@:�@I@V@b�@p�@~K@��@�H@��@��@@�7@ލ@�@�~@�@{@!s@0x@>�@Lu@Z@g@t@��@�@�@�M@��@��@��@��@�@��@
�@�@&�@33@A�@P�@\�@i!@x&@�|@��@�@�f@��@��@�
@�T@�Y@^@�@�@(�@7�@FQ@R�@^5@m:@|?@��@��@�(@�~@��@��@�#@�@�@j@�@ �@,`@7�@FQ@T�@bN@p�@~�@��@�H@��@�F@��@є@�;@��@��@1@*@#�@1'@?}@M$@Z�@ff@t@��@��@�@�@�^@�@խ@�T@�@��@J@�@&;@1�@?}@M$@Z�@hs@v@��@�@��@�@��@�@׹@�`@�@@@[@+@8�@DD@O�@^5@k�@y�@��@��@�(@��@��@�o@�@�m@�@	�@	b@	[@	-@	;d@	I@	V@	c�@	p�@	}�@	�D@	��@	��@	��@	��@	�7@	��@	�@	��@
1@
�@
"�@
/�@
=q@
K@
Yn@
ff@
t�@
�d@
�h@
�@
��@
�F@
Ĝ@
��@
�H@
��@
��@�@B@%�@1�@@,@N�@]�@k�@x&@��@�@�@�r@��@�W@խ@�T@�@�Q@�@�@(G@6�@DD@R�@`B@n�@|�@��@�<@��@��@�2@��@܀@�(@�~@@�@�@-@<@I�@T�@e	@r�@�@��@��@��@��@@ψ@܀@�(@��@1@�@"�@/�@<�@I�@Z�@g�@t�@��@��@�@�Y@�R@�J@�C@��@��@��@J@B@&;@3�@@�@O0@\)@hs@z3@�|@�u@�@�@�@ȴ@խ@�T@�L@��@J@�@(G@5�@C�@Q�@_�@m�@z�@��@��@�y@�!@��@�@�@�(@� @�@o@�@+�@9X@Ji@V�@e	@p�@~K@��@��@��@��@�2@�C@ލ@��@�9@�@�@$.@0x@<�@M$@X@e�@v@��@��@�@��@��@�W@��@��@�@  @�@�@'�@3�@@�@P�@\�@i�@y�@��@�h@��@�@�^@�@�
@�@�@  @�@�@(�@4�@E�@Q=@]�@n�@z�@�+@��@��@�!@��@��@�@�(@��@�@�@g@+@<@H]@T�@e	@r@~�@��@�<@��@�9@��@є@�/@�(@��@	�@*@"�@2�@>@Ji@Z@e�@r@��@��@��@�Y@��@��@��@�H@�@��@	�@�@&;@5�@B8@N�@^�@j@v�@��@�u@�m@��@�@�@�\@�@�@�Q@J@�@(G@5?@D�@Q=@]�@m�@y�@�|@��@�A@��@��@�@�h@�@�e@^@�@
@*S@;d@F�@SI@c�@p�@|?@��@��@��@�F@@ψ@��@�4@��@�@{@ �@0x@=q@I@Z@ff@r�@��@�\@�U@��@�@�J@խ@�H@�@��@
=@�@&�@2�@?}@O�@\)@g�@x�@�p@��@��@�@�@�o@׹@�@�L@��@�@�@'�@8�@E�@Q�@^�@k.@|?@��@��@��@��@��@�|@�t@�@��@@@ @-@:�@F�@UU@a�@oF@|?@��@��@��@��@��@ψ@��@��@��@�@{@ �@.l@?}@Lu@X@i!@uk@�d@��@�U@��@�@��@��@�;@�L@��@�@B@&;@1�@B8@N�@Z@k.@ww@�p@��@�@�f@�w@�@�\@�@��@  �@ �@ �@ +�@ 7�@ C�@ Q�@ a�@ l�@ x�@ ��@ ��@ ��@ ��@ �j@ �o@ �t@ ��@ ��@!�@!V@!�@!+@!:@!H]@!V�@!e	@!s_@!~K@!�7@!��@!��@!��@!@!��@!�;@!��@!�9@"�@"6@"%�@"0x@";d@"I@"Wb@"e�@"uk@"��@"��@"��@"��@"��@"��@"��@"�@"�Y@"��@#1@#�@#$/@#33@#B8@#P�@#^�@#i�@#t�@#�@#��@#�m@#�!@#�w@#��@#׹@#�@#�@$  @$V@$�@$,`@$6�@$B8@$Q=@$_�@$n�@$|�@$��@$�#@$�z@$�~@$��@$��@$܀@$�@$�@%�@%�@%!s@%/�@%:�@%FQ@%UU@%dZ@%s_@%��@%��@%��@%�M@%�R@%��@%ψ@%�;@%�@@%��@&v@&*@&$�@&0x@&;d@&K@&Z�@&g@&r�@&�d@&�@&�a@&��@&�^@&�J@&є@&�H@&�@&��@'�@'�@'(G@'3�@'@,@'O�@'[z@'g�@'ww@'�|@'��@'�@'�@'�w@'��@'�
@'�@'�@'��@(�@([@((G@(4�@(DD@(S�@(_�@(k.@({�@(��@(�0@(�z@(�~@(��@(ψ@(�#@(�@(�q@)%@)@)[@),`@)<@)G�@)R�@)bN@)qS@)�@)��@)��@)�A@)�F@)��@)��@)܀@)�@)��@*	�@*{@* �@*/@*>@*Lu@*[z@*i�@*uk@*�@*�\@*��@*��@*�@*ȴ@*�
@*�H@*�4@*��@+�@+�@+&;@+3�@+B8@+O0@+]�@+k.@+x�@+�|@+��@+��@+�!@+��@+�@+��@+�m@+�@,�@,�@,�@,*S@,7L@,D�@,R�@,`A@,m:@,z�@,��@,��@,��@,�r@,�&@,��@,��@,�y@,�q@-j@-@-�@-,`@-9X@-FQ@-SI@-c�@-s_@-�@-��@-��@-��@-��@-�>@-��@-ލ@-�@-�,@.%@.�@. @.-@.>@.N�@.\)@.i�@.v�@.�p@.�i@.�@.�@.�@.�J@.�C@.��@.�@.��@/
�@/6@/$�@/1�@/@�@/Q=@/^5@/j@/v�@/�p@/�u@/��@/�!@/��@/��@/�h@/�@/�@/��@0J@0B@0'�@04�@0B8@0O0@0`A@0p�@0}�@0��@0��@0��@0�-@0�&@0�@0�@0�m@0�@1�@1�@1
@1+�@19X@1F�@1S�@1a�@1o�@1}�@1��@1��@1��@1��@1@1��@1ލ@1�4@1��@2�@2{@2""@2/@2<�@2K@2X�@2g@2uk@2�@2��@2��@2�@2�^@2�@2խ@2�@2��@2��@3
�@3�@3&;@333@3A�@3N�@3\)@3i!@3v@3��@3��@3�a@3�@3��@3�@3�#@3�m@3�@4@4V@4�@4(G@45�@4C�@4P�@4]�@4k.@4x�@4��@4��@4�A@4��@4��@4��@4܀@4�y@4�q@5@5@5�@5+�@59X@5FQ@5S�@5܀@6
�@6<@6�(@6��@78�@7j@7��@8%@87�@8k�@8є@9@96�@9��@9є@:�@:l�@:��@:��@;8�@;j@;��@<%@<6�@<hs@<��@<��@=*S@=��@=��@>6@>E�@>��@>��@?/�@?\�@?�w@?��@@�@@}�@@�@A�@A9X@A�<@A��@B%�@BQ�@B�@B��@C
�@Ci!@C��@C�@D	@Dz�@D��@E�@E7L@E��@E�W@E��@F[z@F��@F�4@G�@G|?@G��@G��@H> @Hk�@H�*@H��@I-@I��@I�k@J�@JK@J�M@J�[@K5�@KbN@K��@K�4@L�@Luk@L�@L�9@MSI@M|�@M�O@M��@NSI@N}�@NӠ@O'�@OP�@O��@O�q@P�@Po�@P��@P�l@Q7�@Q��@Q��@Q��@RK@R��@R��@S@SN�@S�0@S�/@T$�@Ti�@T��@T��@U> @U�p@U�@V@VX@Vz�@V�w@W�@WG�@W��@W�*@X@Xv�@X�R@X�9@Y> @Y��@Y�>@Z@ZC�@Z�@Z��@[ �@[_�@[��@[�;@\	@\[z@\��@\�@]/@]l�@]�W@^�@^=q@^x�@^��@_V@_K@_�(@_�;@`�@`V@`��@`�@a:�@aqS@aĜ@a�9@bM�@b��@bӠ@c1@cSI@c��@c��@d�@dZ@d��@d�@e3�@ev�@e��@e��@fC�@f�7@f��@g]@gI@g�@g�
@hg@ G�O�@ G�O�@ G�O�G�O�@ ^G�O�@ G�O�G�O�G�O�G�O�G�O�@ G�O�G�O�@ G�O�@ �G�O�G�O�G�O�G�O�@ �@ j@ @ v@ %@ �@ �@ 
�@ �@ �@ �@ o@ �@ �@ B@ �@ [@  @ ""@ $�@ '�@ )�@ ,`@ /@ 1�@ 5?@ 8�@ ;d@ >�@ B8@ D�@ I@ K�@ N�@ R�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�VA�Q�A�VA�ZA�hsA�jA�jA�jA�ffA�ffA�l�A�l�A�n�A�n�A�n�A�hsA�n�A�bNA�Q�A�%A�jA�{AżjA�A�&�A�jA��/A�JA���A��A�C�A�9XA�l�A��PA��FA�XA�XA�C�A�=qA��/A�$�A���A�?}A���A�O�A��A�ȴA��A��!A���A�JA�M�A�x�A��A��A���A�bA�ȴA��7A��PA�S�A� �A��yA��A�(�A�|�A�  A��DA�XA��A��TA��A�dZA� �A�x�A��A���A�C�A�
=A���A�r�A�I�A��/A�t�A���A�1'A��^A��A�`BA�A�A�&�A��A�A�ZA���A�M�A��A���A���A�x�A��A���A�A���A��^A���A�&�A�C�A�x�A�G�A�l�A��+A�l�A� �A�ȴA��/A�ȴA�$�A��A�z�A�z�A���AK�Az��Ax��AxVAxI�Ax(�Aw�As�Aq`BAp�HAoAn�9AkƨAi`BAe��Ab��AahsA_�7A[%AY��AYl�AV�RAQ��AO�AM�PALn�AJ�yAH-AGoAF��AF5?AE�FAEO�AC�AC��AAp�A>�!A>bA<��A:��A9�A8M�A7��A7�#A733A5�TA4��A4JA3�A1�;A0��A0VA.��A,�jA(�A'��A&�A$Q�A#�A!��A Q�A�TA`BA�A
=A��A�mA��A-A�/A��AbNAbA�-AS�AAE�A��A~�A�hA�A��Av�AA�A�A��A�9A��AO�A
bNA	O�AĜA$�A"�A1A
=A�9Ar�AVA�AC�A1A Z@���@���@���@���@�V@�  @�=q@�E�@���@�1@��@�$�@�hs@�7L@�1@�M�@���@���@�=q@�+@���@�9@�dZ@�V@��@�@��@�Z@ߥ�@�@��m@ڧ�@�-@�7L@�9X@�  @Լj@�?}@��@Η�@�X@���@̣�@̓u@�r�@�Z@�9X@�(�@�  @�S�@�=q@��@Ǯ@��@��#@�`B@��@ċD@��m@�@�v�@�G�@���@�I�@��w@�|�@�+@��@�o@�K�@���@�|�@�S�@���@��+@��-@��@�I�@��w@���@�-@��@���@���@��@���@��@���@�1'@���@�x�@��@��u@�j@�Q�@�I�@�1'@���@��P@��+@�-@��#@�`B@��@��D@�r�@�A�@�l�@�dZ@��@�`B@���@��/@�Ĝ@���@��@���@�\)@��H@�n�@��T@�O�@��@���@�Z@�9X@�ƨ@��P@�l�@�
=@��@��H@�ȴ@��+@�V@�$�@��@��T@�@��h@��7@���@��#@�@�x�@��@�(�@��w@��@�C�@�ȴ@�ff@�=q@�@���@�`B@�G�@�7L@�7L@�/@�&�@��@��/@�Z@�(�@�K�@��@��R@��\@�^5@��@��@��T@�p�@�%@��9@��@��@���@��D@�I�@��;@�;d@���@�v�@��@���@��^@��h@�`B@�?}@��9@��;@��@���@���@�33@��y@�^5@�5?@�-@�J@��#@�hs@���@��@���@�S�@��R@�"�@��@���@�ff@��@�hs@�%@���@��9@�  @��P@�|�@�dZ@�C�@��@�ff@�v�@�n�@�$�@��#@���@�p�@���@�X@���@��D@�t�@���@�ȴ@���@���@�X@��@��9@��@��@�9X@���@��@�\)@�;d@�"�@�o@��y@���@��\@��+@�{@��@��T@��7@�`B@�7L@�%@���@��@�X@�hs@�p�@�x�@�p�@�x�@��@�x�@�G�@�/@�&�@��@��@�%@���@��/@��@�I�@���@��F@���@�;d@�@���@��y@�V@��^@���@�O�@�?}@�/@�%@��@�Ĝ@�Ĝ@�Ĝ@��u@�(�@� �@�(�@�(�@�1'@�(�@�9X@��@��@\)@K�@\)@�@
=@~�y@~��@~ff@}�@|Z@|I�@|1@{ƨ@{�@{t�@{S�@{dZ@{S�@{@z~�@zJ@y�#@y�#@y�#@y�^@y�#@y��@x�`@xbN@x �@x  @w�;@w�@w\)@v�+@vV@u�T@u�@t��@t9X@s33@r^5@r=q@r=q@q��@q��@q�^@q�7@qhs@qX@qG�@q7L@q�@q%@p��@p�`@p�`@p�9@pr�@pbN@pA�@p �@o�;@o\)@o
=@m�@l�@l(�@k��@k�m@kƨ@j��@jJ@i��@h�9@hQ�@hb@g�;@g�@g�P@g�P@gl�@g\)@g;d@g+@g�@fv�@fE�@e��@e/@d��@d1@cƨ@c��@cS�@c33@c"�@c33@co@a�@aG�@a7L@a�@`��@`��@`A�@`  @_K�@_�@^�@^��@^$�@^@]@\�/@[�
@[33@Z�@Z��@Z��@Zn�@Y��@Y&�@X�9@W�@W�w@W|�@W
=@V��@VV@V{@V@U�-@U��@U�@T�@T�@T1@Sƨ@S��@St�@SS�@S"�@S@R�@R�!@R�\@R^5@R=q@R-@Q��@Q��@Qx�@QX@Q�@P��@PĜ@PbN@P �@O��@OK�@N�y@N��@NV@N5?@M�@M@M�-@M@M�-@M�-@Mp�@L�D@Lz�@L1@K�F@KS�@Ko@J��@J~�@I��@I��@IG�@H��@HA�@G�;@G�@Fv�@FE�@F5?@F@E��@EO�@D��@D��@D��@D��@D�/@D�/@D1@CdZ@CdZ@C@B=q@A�#@AX@@�9@@�@@b@?�P@>��@>�y@>�@>�R@>��@>ff@>E�@>@=�@=@=O�@<�@<��@<�j@<z�@<�@;�m@;�F@;dZ@:�H@:�!@:�\@:~�@:^5@:J@9�@9x�@9�@8��@8A�@8  @7�P@7+@6�y@6ȴ@6v�@6E�@6{@6$�@5�@5��@5�-@5�h@5`B@4��@4�@3�
@3�F@3dZ@3S�@3@2��@2�\@2M�@2=q@2=q@2=q@2=q@2=q@2�@1�@0�`@0A�@01'@/�@/��@/�P@/l�@/+@/
=@.�y@.v�@-��@-�@,�/@,j@,1@+ƨ@+�F@+��@+��@+�@+t�@+�@+�@+�@+t�@+dZ@+S�@+33@+@*n�@)��@)�@(��@(��@(  @'�P@'
=@&ȴ@&ȴ@&�R@&v�@&5?@&{@&@%�-@%�@%O�@%�@$��@$�@$�/@$j@$�@#��@#dZ@#S�@#@"�@"�H@"��@"n�@"�@!�@!�^@!��@!G�@!&�@!&�@!�@ �`@ ��@ �u@ r�@   @�@�;@��@��@�@|�@K�@�y@�+@V@$�@��@�h@O�@�@�@�@�F@�F@��@o@��@�\@-@�#@��@hs@7L@�`@�u@1'@  @�@�;@l�@
=@�@��@�+@V@5?@�T@��@�-@��@��@��@�@p�@`B@�@��@�/@��@I�@(�@1@t�@o@�H@�\@^5@M�@�@J@��@�@�#@��@Ĝ@ �@�;@�w@��@|�@K�@;d@�@��@ȴ@��@��@5?@{@@@��@`B@O�@?}@/@�@V@�@�@(�@ƨ@�F@��@C�@@
��@
�\@
�\@
~�@
~�@
~�@
n�@
M�@
M�@
M�@
-@
�@
�@
�@	��@	��@	�7@	G�@	7L@	7L@	&�@	%@��@�@��@ȴ@p�@��@(�@@��@�@��@X?�\)?���?��h?�I�?�1?��H?��?�r�?��+?��/?�Z?�S�?�J?��?�&�?�w?�w??�R?�{?�V?�j?�"�?ꟾ?�^?�^?�u?�?�+?�$�?��?�9X?��
?��?���?� �?��?��?ݲ-?�V?�j?�1?�dZ?�?��?�Q�?�K�?�
=?�?�`B?�?}?��/?��/?�9X?���?�o?��?���?Ұ!?�&�?Ѓ?Ͼw?Ͼw?ϝ�?�;d?��?��?�p�?�/?̬?˥�?˅?���?ʟ�?���?���?���?ʟ�?�=q?�X?���?�Q�?���?Ǯ?�K�?�+?��y?Ƈ+?�$�?�?�?Ł?��?ļj?��/?��
?Õ�?öF?�t�?�S�?�o?�33?�33?��?°!?°!?�n�?�M�?��?�&�?�&�?�%?��`?���?�A�?��?�A�?�  ?��w?�\)?���?���?�v�?�{?�{?�{?��?�O�?�/?�O�?�p�?�p�?�V?��?��?��?���?��?��?���?���?��?��?��?�j?�(�?�(�?�1?�ƨ?��m?�ƨ?���?���?���?���?���?�dZ?��?���?�ƨ?�ƨ?�1?��m?��m?���?���?��m?�1?�(�?�(�?�1?��m?��m?��m?�ƨ?�1?��m?��m?���A�\)A�^5A�bNA�^5A�`BA�`BA�^5A�^5A�ZA�^5A�^5A�VA�O�A�K�A�O�A�S�A�Q�A�Q�A�O�A�O�A�S�A�K�A�K�A�M�A�K�A�K�A�M�A�M�A�O�A�O�A�S�A�VA�VA�VA�VA�VA�XA�ZA�\)A�ffA�hsA�jA�jA�jA�jA�l�A�jA�jA�jA�hsA�hsA�dZA�ffA�ffA�jA�jA�n�A�l�A�jA�n�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                A�VA�Q�A�VA�ZA�hsA�jA�jA�jA�ffA�ffA�l�A�l�A�n�A�n�A�n�A�hsA�n�A�bNA�Q�A�%A�jA�{AżjA�A�&�A�jA��/A�JA���A��A�C�A�9XA�l�A��PA��FA�XA�XA�C�A�=qA��/A�$�A���A�?}A���A�O�A��A�ȴA��A��!A���A�JA�M�A�x�A��A��A���A�bA�ȴA��7A��PA�S�A� �A��yA��A�(�A�|�A�  A��DA�XA��A��TA��A�dZA� �A�x�A��A���A�C�A�
=A���A�r�A�I�A��/A�t�A���A�1'A��^A��A�`BA�A�A�&�A��A�A�ZA���A�M�A��A���A���A�x�A��A���A�A���A��^A���A�&�A�C�A�x�A�G�A�l�A��+A�l�A� �A�ȴA��/A�ȴA�$�A��A�z�A�z�A���AK�Az��Ax��AxVAxI�Ax(�Aw�As�Aq`BAp�HAoAn�9AkƨAi`BAe��Ab��AahsA_�7A[%AY��AYl�AV�RAQ��AO�AM�PALn�AJ�yAH-AGoAF��AF5?AE�FAEO�AC�AC��AAp�A>�!A>bA<��A:��A9�A8M�A7��A7�#A733A5�TA4��A4JA3�A1�;A0��A0VA.��A,�jA(�A'��A&�A$Q�A#�A!��A Q�A�TA`BA�A
=A��A�mA��A-A�/A��AbNAbA�-AS�AAE�A��A~�A�hA�A��Av�AA�A�A��A�9A��AO�A
bNA	O�AĜA$�A"�A1A
=A�9Ar�AVA�AC�A1A Z@���@���@���@���@�V@�  @�=q@�E�@���@�1@��@�$�@�hs@�7L@�1@�M�@���@���@�=q@�+@���@�9@�dZ@�V@��@�@��@�Z@ߥ�@�@��m@ڧ�@�-@�7L@�9X@�  @Լj@�?}@��@Η�@�X@���@̣�@̓u@�r�@�Z@�9X@�(�@�  @�S�@�=q@��@Ǯ@��@��#@�`B@��@ċD@��m@�@�v�@�G�@���@�I�@��w@�|�@�+@��@�o@�K�@���@�|�@�S�@���@��+@��-@��@�I�@��w@���@�-@��@���@���@��@���@��@���@�1'@���@�x�@��@��u@�j@�Q�@�I�@�1'@���@��P@��+@�-@��#@�`B@��@��D@�r�@�A�@�l�@�dZ@��@�`B@���@��/@�Ĝ@���@��@���@�\)@��H@�n�@��T@�O�@��@���@�Z@�9X@�ƨ@��P@�l�@�
=@��@��H@�ȴ@��+@�V@�$�@��@��T@�@��h@��7@���@��#@�@�x�@��@�(�@��w@��@�C�@�ȴ@�ff@�=q@�@���@�`B@�G�@�7L@�7L@�/@�&�@��@��/@�Z@�(�@�K�@��@��R@��\@�^5@��@��@��T@�p�@�%@��9@��@��@���@��D@�I�@��;@�;d@���@�v�@��@���@��^@��h@�`B@�?}@��9@��;@��@���@���@�33@��y@�^5@�5?@�-@�J@��#@�hs@���@��@���@�S�@��R@�"�@��@���@�ff@��@�hs@�%@���@��9@�  @��P@�|�@�dZ@�C�@��@�ff@�v�@�n�@�$�@��#@���@�p�@���@�X@���@��D@�t�@���@�ȴ@���@���@�X@��@��9@��@��@�9X@���@��@�\)@�;d@�"�@�o@��y@���@��\@��+@�{@��@��T@��7@�`B@�7L@�%@���@��@�X@�hs@�p�@�x�@�p�@�x�@��@�x�@�G�@�/@�&�@��@��@�%@���@��/@��@�I�@���@��F@���@�;d@�@���@��y@�V@��^@���@�O�@�?}@�/@�%@��@�Ĝ@�Ĝ@�Ĝ@��u@�(�@� �@�(�@�(�@�1'@�(�@�9X@��@��@\)@K�@\)@�@
=@~�y@~��@~ff@}�@|Z@|I�@|1@{ƨ@{�@{t�@{S�@{dZ@{S�@{@z~�@zJ@y�#@y�#@y�#@y�^@y�#@y��@x�`@xbN@x �@x  @w�;@w�@w\)@v�+@vV@u�T@u�@t��@t9X@s33@r^5@r=q@r=q@q��@q��@q�^@q�7@qhs@qX@qG�@q7L@q�@q%@p��@p�`@p�`@p�9@pr�@pbN@pA�@p �@o�;@o\)@o
=@m�@l�@l(�@k��@k�m@kƨ@j��@jJ@i��@h�9@hQ�@hb@g�;@g�@g�P@g�P@gl�@g\)@g;d@g+@g�@fv�@fE�@e��@e/@d��@d1@cƨ@c��@cS�@c33@c"�@c33@co@a�@aG�@a7L@a�@`��@`��@`A�@`  @_K�@_�@^�@^��@^$�@^@]@\�/@[�
@[33@Z�@Z��@Z��@Zn�@Y��@Y&�@X�9@W�@W�w@W|�@W
=@V��@VV@V{@V@U�-@U��@U�@T�@T�@T1@Sƨ@S��@St�@SS�@S"�@S@R�@R�!@R�\@R^5@R=q@R-@Q��@Q��@Qx�@QX@Q�@P��@PĜ@PbN@P �@O��@OK�@N�y@N��@NV@N5?@M�@M@M�-@M@M�-@M�-@Mp�@L�D@Lz�@L1@K�F@KS�@Ko@J��@J~�@I��@I��@IG�@H��@HA�@G�;@G�@Fv�@FE�@F5?@F@E��@EO�@D��@D��@D��@D��@D�/@D�/@D1@CdZ@CdZ@C@B=q@A�#@AX@@�9@@�@@b@?�P@>��@>�y@>�@>�R@>��@>ff@>E�@>@=�@=@=O�@<�@<��@<�j@<z�@<�@;�m@;�F@;dZ@:�H@:�!@:�\@:~�@:^5@:J@9�@9x�@9�@8��@8A�@8  @7�P@7+@6�y@6ȴ@6v�@6E�@6{@6$�@5�@5��@5�-@5�h@5`B@4��@4�@3�
@3�F@3dZ@3S�@3@2��@2�\@2M�@2=q@2=q@2=q@2=q@2=q@2�@1�@0�`@0A�@01'@/�@/��@/�P@/l�@/+@/
=@.�y@.v�@-��@-�@,�/@,j@,1@+ƨ@+�F@+��@+��@+�@+t�@+�@+�@+�@+t�@+dZ@+S�@+33@+@*n�@)��@)�@(��@(��@(  @'�P@'
=@&ȴ@&ȴ@&�R@&v�@&5?@&{@&@%�-@%�@%O�@%�@$��@$�@$�/@$j@$�@#��@#dZ@#S�@#@"�@"�H@"��@"n�@"�@!�@!�^@!��@!G�@!&�@!&�@!�@ �`@ ��@ �u@ r�@   @�@�;@��@��@�@|�@K�@�y@�+@V@$�@��@�h@O�@�@�@�@�F@�F@��@o@��@�\@-@�#@��@hs@7L@�`@�u@1'@  @�@�;@l�@
=@�@��@�+@V@5?@�T@��@�-@��@��@��@�@p�@`B@�@��@�/@��@I�@(�@1@t�@o@�H@�\@^5@M�@�@J@��@�@�#@��@Ĝ@ �@�;@�w@��@|�@K�@;d@�@��@ȴ@��@��@5?@{@@@��@`B@O�@?}@/@�@V@�@�@(�@ƨ@�F@��@C�@@
��@
�\@
�\@
~�@
~�@
~�@
n�@
M�@
M�@
M�@
-@
�@
�@
�@	��@	��@	�7@	G�@	7L@	7L@	&�@	%@��@�@��@ȴ@p�@��@(�@@��@�@��@X?�\)?���?��h?�I�?�1?��H?��?�r�?��+?��/?�Z?�S�?�J?��?�&�?�w?�w??�R?�{?�V?�j?�"�?ꟾ?�^?�^?�u?�?�+?�$�?��?�9X?��
?��?���?� �?��?��?ݲ-?�V?�j?�1?�dZ?�?��?�Q�?�K�?�
=?�?�`B?�?}?��/?��/?�9X?���?�o?��?���?Ұ!?�&�?Ѓ?Ͼw?Ͼw?ϝ�?�;d?��?��?�p�?�/?̬?˥�?˅?���?ʟ�?���?���?���?ʟ�?�=q?�X?���?�Q�?���?Ǯ?�K�?�+?��y?Ƈ+?�$�?�?�?Ł?��?ļj?��/?��
?Õ�?öF?�t�?�S�?�o?�33?�33?��?°!?°!?�n�?�M�?��?�&�?�&�?�%?��`?���?�A�?��?�A�?�  ?��w?�\)?���?���?�v�?�{?�{?�{?��?�O�?�/?�O�?�p�?�p�?�V?��?��?��?���?��?��?���?���?��?��?��?�j?�(�?�(�?�1?�ƨ?��m?�ƨ?���?���?���?���?���?�dZ?��?���?�ƨ?�ƨ?�1?��m?��m?���?���?��m?�1?�(�?�(�?�1?��m?��m?��m?�ƨ?�1?��m?��m?���A�\)A�^5A�bNA�^5A�`BA�`BA�^5A�^5A�ZA�^5A�^5A�VA�O�A�K�A�O�A�S�A�Q�A�Q�A�O�A�O�A�S�A�K�A�K�A�M�A�K�A�K�A�M�A�M�A�O�A�O�A�S�A�VA�VA�VA�VA�VA�XA�ZA�\)A�ffA�hsA�jA�jA�jA�jA�l�A�jA�jA�jA�hsA�hsA�dZA�ffA�ffA�jA�jA�n�A�l�A�jA�n�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B� B�B�B�B�%B�+B�%B~�Bt�B[#BT�B^5Bu�Bs�Br�B�B�PB�B��B#�BB�BVBA�B8RB:^BC�BO�BT�BaHBk�Bx�B�B�B�Bz�Bs�Bn�Bu�B�hB��B�7B�JB�=Br�Be`Bn�B�uB��B�3B�RBBB�qB�^BĜBŢBƨBƨB��BȴBŢB��B�qB�RB�!B�B��B��B� B~�B�7B��B��B�oB�\B�PB�DB�7Bt�Bk�Be`B[#BE�B:^B"�B�BhB1B��B��B�B�B�BB�BÖB��B�Bm�B>wBoB1B
��B
�B
B
��B
��B
|�B
k�B
_;B
G�B
"�B
\B
PB
JB

=B
B	�fB	�;B	�)B	�B	�B	ÖB	�XB	��B	�JB	�B	n�B	Q�B	G�B	B�B	$�B	\B	B��B��B�B�TB�5B�)B�#B�B�B�BB�NB��BŢBB�^B�9B�B��B��B��B��B��B��B��B�oB�JB�DB�%B�Bs�Bl�BiyBdZB`BB`BBYBXBVBS�BT�BR�BQ�BQ�BQ�BN�BM�BH�BJ�BJ�BH�BG�BE�BC�BC�BB�BB�BC�BB�BB�BB�BB�BD�BD�BC�BD�BC�BD�BD�BB�BA�BA�BA�B?}B?}B?}B=qB<jB;dB?}B?}B?}B>wB>wBI�BK�BK�BN�BR�BW
BXBYB[#B[#BZB[#BT�BT�BT�BN�BQ�BO�BJ�BI�BJ�BS�BXBYBZBVBO�BL�BO�BQ�BS�B^5BdZBiyBn�Bp�Bu�Bv�Bu�Bu�Bu�Bt�Bu�Bt�Bt�Bt�Bt�Bv�Bz�B|�B�B�B�B�B�%B�+B�1B�VB�PB�bB�hB�{B��B��B��B��B�9B�jB�wB��B��BȴB��B��B��B��B�
B�#B�5B�;B�HB�ZB�sB�B�B�B�B�B��B��B��B��B��B��B��B	%B	
=B	PB	VB	bB	�B	�B	#�B	+B	+B	+B	)�B	,B	,B	+B	,B	-B	/B	0!B	49B	;dB	>wB	B�B	C�B	E�B	J�B	M�B	T�B	XB	XB	ZB	[#B	\)B	]/B	_;B	aHB	ffB	k�B	o�B	r�B	r�B	t�B	x�B	{�B	}�B	� B	�%B	�1B	�DB	�JB	�PB	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	�9B	�LB	�LB	�RB	�XB	�jB	�qB	�wB	B	ŢB	ĜB	ĜB	ŢB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�/B	�)B	�)B	�)B	�NB	�NB	�ZB	�ZB	�ZB	�ZB	�ZB	�TB	�TB	�TB	�ZB	�ZB	�TB	�ZB	�ZB	�`B	�fB	�sB	�sB	�sB	�mB	�B	�B	�B	�B	�B	�B	�B	�yB	�mB	�mB	�fB	�fB	�mB	�mB	�mB	�mB	�sB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
1B
+B
	7B
	7B
	7B
	7B
	7B

=B
DB
DB
JB
JB
DB
JB
JB
DB
JB
JB
JB
DB
DB
DB
DB
JB
JB
PB
JB
PB
PB
VB
\B
bB
\B
\B
\B
\B
bB
bB
hB
oB
oB
oB
oB
oB
uB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
"�B
#�B
$�B
%�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
(�B
(�B
(�B
)�B
(�B
)�B
)�B
+B
,B
-B
-B
-B
-B
-B
/B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
1'B
1'B
2-B
2-B
33B
33B
33B
33B
33B
5?B
49B
5?B
5?B
5?B
5?B
5?B
6FB
7LB
7LB
7LB
7LB
7LB
8RB
9XB
9XB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
:^B
;dB
;dB
;dB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
D�B
D�B
E�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
E�B
E�B
F�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
J�B
J�B
K�B
J�B
K�B
K�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
O�B
P�B
Q�B
Q�B
R�B
Q�B
Q�B
R�B
S�B
S�B
R�B
R�B
R�B
S�B
S�B
R�B
T�B
T�B
VB
VB
T�B
T�B
VB
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
VB
VB
XB
XB
XB
YB
YB
YB
YB
ZB
YB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
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
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
bNB
aHB
aHB
aHB
aHB
bNB
aHB
bNB
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
dZB
dZB
dZB
e`B
dZB
dZB
e`B
e`B
e`B
ffB
gmB
ffB
ffB
ffB
ffB
gmB
ffB
ffB
gmB
gmB
gmB
gmB
ffB
gmB
gmB
gmB
ffB
gmB
gmB
gmB
ffB
gmB
hsB
hsB
hsB
iyB
iyB
iyB
jB
iyB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
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
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
r�B
r�B
s�B
r�B
r�B
s�B
s�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
u�B
t�B
u�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
w�B
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
y�B
y�B
y�B
y�B
{�B
{�B
|�B
}�B
~�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�%B
�+B
�+B
�1B
�7B
�7B
�=B
�=B
�=B
�DB
�PB
�JB
�JB
�JB
�VB
�VB
�\B
�\B
�\B
�bB
�bB
�hB
�hB
�hB
�oB
�oB
�uB
�{B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�'B
�'B
�'B
�-B
�-B
�'B
�-B
�-B
�-B
�-B
�-B
�3B
�3B
�3B
�3B
�3B
�3B
�9B
�9B
�9B
�9B
�?B
�9B
�?B
�?B
�9B
�9B
�?B
�?B
�FB
�?B
�FB
�?B
�?B
�FB
�FB
�?B
�FB
�FB
�FB
�LB
�LB
�FB
�FB
�LB
�FB
�LB
�LB
�RB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�RB
�RB
�RB� B� B~�B~�B~�B}�B~�B~�B~�B~�B|�B}�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B}�B~�B~�B~�B~�B~�B� B~�B~�B~�B~�B~�B~�B~�B~�B� B~�B~�B~�B~�B~�B� B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B�B��B��B��B�B�
B�B~�Bt�B[BT�B^Bu�Bs�Br�B��B�5B��B��B#�BBvBU�BAqB8:B:GBCBO�BT�Ba3BkpBx�B�B�B�Bz�Bs�Bn�Bu�B�XB��B�(B�;B�/Br�BeSBn�B�iB��B�(B�HBBB�iB�VBĕBśBƢBƢBʼBȰBŞB��B�nB�PB�B�B��B��B� B~�B�8B��B��B�rB�`B�TB�IB�<Bt�Bk�BegB[*BE�B:fB"�B�BrB;B��B��B�B�B�OB�$BäB��B�!Bm�B>�B~BAB
�B
�B
 B
�B
��B
} B
k�B
_NB
G�B
"�B
pB
eB
_B

SB
"B	�}B	�RB	�AB	�/B	�)B	ïB	�qB	��B	�dB	�&B	n�B	RB	G�B	B�B	$�B	xB	/B�B��B�B�rB�TB�HB�CB�>B�8B�dB�pB�B��B²B��B�]B�3B�B�	B��B��B��B��B��B��B�sB�nB�OB�1Bs�Bl�Bi�Bd�B`oB`oBYDBX>BV2BT'BU-BS"BRBRBRBOBNBH�BJ�BJ�BH�BG�BE�BC�BC�BB�BB�BC�BB�BB�BB�BB�BD�BD�BC�BD�BC�BD�BD�BB�BA�BA�BA�B?�B?�B?�B=�B<�B;�B?�B?�B?�B>�B>�BJ BLBLBO BS:BWSBXYBYaB[mB[nBZhB[oBUJBUKBUKBO'BR:BP.BKBJ
BKBTIBXaBYiBZpBVWBP3BM!BP4BRABTNB^�Bd�Bi�Bn�Bp�BvBw"BvBvBvBuBvBuBuBuBuBw(B{@B}NB�gB��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�RB��B��B��B��B��B�!B�.B�5B�GB�[B�zBۓBަB߬B�B��B��B�B�B�B�B�,B�?B�FB�MB�_B�`B�hB�hB	�B	
�B	�B	�B	�B	B	*B	$VB	+�B	+�B	+�B	*}B	,�B	,�B	+�B	,�B	-�B	/�B	0�B	4�B	;�B	>�B	CB	DB	F+B	KJB	N]B	U�B	X�B	X�B	Z�B	[�B	\�B	]�B	_�B	a�B	f�B	lB	p/B	sBB	sBB	uOB	yhB	|{B	~�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�(B	�AB	�TB	�ZB	�[B	�cB	�cB	�dB	�dB	�qB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�6B	�JB	�DB	�EB	�LB	�LB	�SB	�YB	�fB	�mB	�nB	�tB	�{B	΂B	ψB	ЏB	ЏB	АB	ҞB	ӤB	ӥB	ԫB	ָB	׿B	׿B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�$B	�+B	�9B	�9B	�:B	�4B	�GB	�SB	�TB	�UB	�OB	�PB	�JB	�EB	�9B	�:B	�3B	�4B	�<B	�<B	�=B	�=B	�DB	�EB	�EB	�LB	�LB	�MB	�TB	�ZB	�[B	�[B	�bB	�cB	�cB	�jB	�jB	�rB	�yB	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
B
B
B
B
�B
�B
�B
�B
 B
 B
B
B
B

B
B
	#B
B

*B

+B

+B

,B

,B
3B
;B
;B
BB
BB
=B
DB
DB
?B
FB
FB
GB
AB
BB
BB
CB
JB
JB
QB
KB
RB
SB
YB
`B
fB
aB
aB
bB
cB
iB
jB
qB
xB
yB
yB
zB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
$ B
$B
$B
%B
&B
'B
)#B
*)B
**B
*+B
*+B
*,B
+2B
*-B
*.B
*.B
+5B
*/B
+6B
+6B
,=B
-DB
.JB
.KB
.LB
.LB
.MB
0ZB
1aB
1aB
1bB
2iB
2iB
2jB
3pB
2kB
2lB
3rB
3sB
4yB
4zB
4zB
4{B
4|B
6�B
5�B
6�B
6�B
6�B
6�B
6�B
7�B
8�B
8�B
8�B
8�B
8�B
9�B
:�B
:�B
9�B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
;�B
;�B
;�B
;�B
;�B
<�B
<�B
;�B
<�B
<�B
<�B
>�B
>�B
?�B
?�B
@�B
@�B
@�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
FB
FB
G
B
FB
GB
GB
GB
HB
HB
HB
HB
GB
GB
HB
IB
IB
IB
J%B
J%B
K,B
K-B
K-B
L5B
L5B
M<B
M=B
L7B
L8B
M>B
L9B
M@B
M@B
MAB
MAB
NHB
OOB
OOB
OPB
OPB
OQB
OQB
ORB
PYB
PYB
PZB
Q`B
QaB
QbB
QbB
QcB
RiB
RjB
QeB
RkB
SsB
SsB
TzB
SuB
SuB
T|B
U�B
U�B
T~B
T~B
TB
U�B
U�B
T�B
V�B
V�B
W�B
W�B
V�B
V�B
W�B
X�B
X�B
X�B
X�B
W�B
X�B
X�B
X�B
W�B
W�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
[�B
Z�B
[�B
[�B
\�B
\�B
\�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
^�B
^�B
^�B
^�B
_�B
_�B
_�B
`�B
`�B
`�B
a�B
a�B
a�B
a�B
a�B
a�B
dB
b�B
b�B
b�B
b�B
dB
b�B
dB
dB
dB
dB
eB
eB
fB
fB
fB
fB
fB
fB
fB
fB
gB
fB
fB
g!B
g"B
g"B
h)B
i1B
h*B
h+B
h+B
h,B
i3B
h-B
h.B
i5B
i6B
i6B
i7B
h1B
i8B
i9B
i9B
h3B
i;B
i;B
i<B
h5B
i=B
jDB
jDB
jEB
kKB
kLB
kMB
lSB
kNB
lTB
lUB
m[B
m\B
m]B
m]B
m^B
m_B
m_B
nfB
nfB
ngB
nhB
nhB
niB
ooB
opB
oqB
oqB
orB
orB
osB
otB
otB
ouB
ouB
p}B
p~B
p~B
pB
pB
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
t�B
t�B
u�B
t�B
t�B
u�B
u�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
w�B
v�B
w�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
y�B
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
{�B
{�B
{�B
}�B
~B
B
�B
� B
�(B
�*B
�/B
�7B
�9B
�JB
�LB
�[B
�`B
�\B
�jB
�uB
�wB
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
� B
�
B
�B
�B
�B
�#B
�+B
�-B
�=B
�?B
�CB
�EB
�IB
�RB
�\B
�^B
�iB
�kB
�uB
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
��B
��B
��B
�B
�B
�
B
�B
�B
�B
�#B
�%B
�(B
�2B
�4B
�8B
�;B
�=B
�AB
�DB
�RB
�VB
�YB
�UB
�_B
�hB
�kB
�iB
�pB
�tB
�}B
�zB
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
��B
��B
��B
��B
��B
��B
��B
�B
�B
�
B
�B
�B
�B
�B
�B
�B
�B
�)B
�+B
�5B
�2B
�<B
�7B
�:B
�EB
�HB
�DB
�NB
�QB
�TB
�^B
�`B
�^B
�`B
�iB
�fB
�pB
�rB
�{B
�yB
�|B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B�B�B~�B~�B~�B}�B~�B~�B~�B~�B|�B}�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B}�B~�B~�B~�B~�B~�B�B~�B~�B~�B~�B~�B~�B~�B~�B�B~�B~�B~�B~�B~�B�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201906061900322021061413561720210614135617202106171314392021061713143920210617131439201906061900322021061413561720210614135617202106171314392021061713143920210617131439PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2019060619003220190606190032  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019060619003220190606190032QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019060619003220190606190032QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713152020210617131520IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                