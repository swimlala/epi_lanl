CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   g   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2006-10-12T04:49:58Z creation;2014-07-22T22:21:32Z update;2015-06-09T20:06:59Z conversion to V3.1;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      
_FillValue               conventions       Argo reference table 1          6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       standard_name         time   
resolution        >�����h�   axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�����h�        8l   LATITUDE               	long_name         &Latitude of the station, best estimate     units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        standard_name         latitude   axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        standard_name         	longitude      axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure     axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  h  ;<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure       �  ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  h  =@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  =�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  ?D   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  h  @�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  AH   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  h  B�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  CL   PSAL         
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  D�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  h  F�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  F�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  h  H�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  H�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  J�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   K   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   T   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ]   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  f   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    f�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    f�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    f�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    f�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  f�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    f�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    f�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    g    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         g   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         g   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        g   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    gArgo profile    3.1 1.2 19500101000000  5900650 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               LA   JA  20061012044958  20150614054517  A5_23632_076                    2C  D   APEX                            1557                            013004                          846 @�@}B.E1   @�@�6͎�@4qhr� ��c��1'1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�  A  Aa��A�ffA���A�  B��B33B2ffBE33BY33Bn  B���B�33B�ffB�33B�  B�  B���B�ffB���B�  B�33B�  B���C  C��CL�CffC� C33C� C$33C)33C.� C3L�C8ffC=L�CB�CG�CP�fC[33CeffCo� Cy�C���C�� C���C��3C��3C�� C���C�� C�� C��fC��fC���C�� C�ffCǙ�C̙�Cь�Cֳ3C۳3C�fC�3C�� CC��fC���D��D��DٚD� D��D�3DٚD$� D)ٚD.ٚD3ٚD8��D=�3DB� DG��DL��DQ�3DV� D[� D`� De�fDjٚDo� Dt�3Dy�3D�&fD�s3D��fD��fD��D�i�D���D�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ffA33A\��A�  A�ffA陚B��B  B133BD  BX  Bl��B�33B���B���B���B�ffB�ffB�33B���B�33B�ffB㙚B�ffB�33C �3CL�C  C�C33C�fC33C#�fC(�fC.33C3  C8�C=  CA��CF��CP��CZ�fCe�Co33Cx��C�s3C�Y�C�s3C���C���C���C�ffC�Y�C���C�� C�� C�ffC�Y�C�@ C�s3C�s3C�ffC֌�Cی�C�� C��CꙚC�s3C� C�ffD��D��D�fD��D��D� D�fD$��D)�fD.�fD3�fD8��D=� DB��DG��DL�fDQ� DV��D[��D`��De�3Dj�fDo��Dt� Dy� D��D�i�D���D���D�3D�` D��3D�	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�n�A�{A��Aں^Aڛ�A�|�A�jA�=qA�z�A��mA�O�A�"�A�VAԲ-A�1'A�;dA�7LA�p�A���A�~�A���A��A�9XA��A�dZA�K�A�S�A�"�A��A��A��^A��A�1'A��A�&�A���A�+A��HA�ĜA�?}A��
A��A���AjA�AVn�AShsARv�AJ  A>=qA8v�A0�`A.M�A(VA"�\A1'A�A�+A�
Ap�A	�wA5?AVA�@��@�hs@���@�"�@�M�@ݲ-@ܼj@�bN@���@�bN@�J@��@��;@��@��T@�x�@�\)@�ff@�@�I�@�$�@��!@� �@��@��;@�^5@��;@���@��D@���@�n�@���@�Ĝ@}�@t��@pr�@c�m@\�@U�@P �1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�n�A�{A��Aں^Aڛ�A�|�A�jA�=qA�z�A��mA�O�A�"�A�VAԲ-A�1'A�;dA�7LA�p�A���A�~�A���A��A�9XA��A�dZA�K�A�S�A�"�A��A��A��^A��A�1'A��A�&�A���A�+A��HA�ĜA�?}A��
A��A���AjA�AVn�AShsARv�AJ  A>=qA8v�A0�`A.M�A(VA"�\A1'A�A�+A�
Ap�A	�wA5?AVA�@��@�hs@���@�"�@�M�@ݲ-@ܼj@�bN@���@�bN@�J@��@��;@��@��T@�x�@�\)@�ff@�@�I�@�$�@��!@� �@��@��;@�^5@��;@���@��D@���@�n�@���@�Ĝ@}�@t��@pr�@c�m@\�@U�@P �1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB~�B� B�B�B�+B�+B�+B�%Bz�B�B�%B��B��B�LB�}B�B��B��B�^B�BgmBw�B�uB��B��B�3B�RB�sB�B�sB�;B�#B��B��B�VBs�B33B�B�wB��B6FB
�XB
H�B	F�B�;B��B��B��B�B� B�DB��B��B��B��B��B��B�%B�%B�+Bo�B�B�+B� B{�Bn�Bo�BjBjBk�Bs�B�B��B�RBɺB�;B	uB	.B	H�B	P�B	`BB	w�B	�+B	��B	��B	�B	B	��B	��B	�/B	�ZB	�mB	�B	��B	��B
DB
DB
�B
�B
&�B
.B
5?B
:^1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B~�B� B�B�B�+B�+B�+B�+B{�B�B�+B��B��B�LB��B�'B��B�B�jB��BhsB{�B�{B��B��B�?B�XB�yB�B�B�BB�)B��BB�\Bu�B6FB��B��B��B:^B
�jB
N�B	L�B�BB��B��B�B�B�B�JB��B��B��B��B��B��B�+B�+B�7Bo�B�B�7B�B|�Bo�Bo�Bk�BjBl�Bt�B�B��B�RBɺB�;B	uB	.B	H�B	P�B	`BB	w�B	�+B	��B	��B	�B	B	��B	��B	�/B	�ZB	�mB	�B	��B	��B
DB
DB
�B
�B
&�B
.B
5?B
:^1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - SP, where SP is SURFACE PRESSURE (minus 5 dbar for Apf-6,7,8) from next cycle.                                                                                                                                                           none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = RecalS = PSAL(PRES_ADJUSTED,TEMP,Conductivity)                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = celltm_sbe41(RecalS,TEMP,PRES_ADJUSTED,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P= dbar since the start of the profile for each samples.                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            SP(NextCycle) = 0.3 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                               The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity Recalculation using PRES_ADJUSTED. PSAL_ADJ_ERR : SBE sensor accuracy & CTM adjustment                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200706281403462007062814034620070628140346200706290235492007062902354920070629023549200707020000002007070200000020070702000000  JA  ARFMfmtp2.3                                                                 20061012044958  IP                  G�O�G�O�G�O�                JA  ARCAsspa2.1                                                                 20061012044959  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcp2.5                                                                 20061012044959  QCP$                G�O�G�O�G�O�            FB7CJA  ARGQaqcp2.5                                                                 20061012044959  QCP$                G�O�G�O�G�O�            FB40JA  ARUP                                                                        20061012045731                      G�O�G�O�G�O�                JA  ARFMfmtp2.3                                                                 20061016034337  IP                  G�O�G�O�G�O�                JA  ARCAsspa2.1                                                                 20061016034337  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcp2.5                                                                 20061016034337  QCP$                G�O�G�O�G�O�            FB7CJA  ARGQaqcp2.5                                                                 20061016034337  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrelo2.1                                                                 20061016034337  CV  TIME            G�O�G�O�F�                JA  ARGQrelo2.1                                                                 20061016034337  CV  LAT$            G�O�G�O�A��D                JA  ARGQrelo2.1                                                                 20061016034337  CV  LON$            G�O�G�O����                JA  ARUP                                                                        20061016035421                      G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20070628140346  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20070628140346  CV  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20070629023549  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 2.0 SeHyD1.0                                                        20070702000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20071001072758  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20071001091721                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312120644  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318072635  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318073126                      G�O�G�O�G�O�                JM  AREQREJM1.0                                                                 20140715031150  CF  POSITION_QC     G�O�G�O�@�                  JA  RFMTcnvd2.1                                                                 20140722222025  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20140722222132                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609200652                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614054517                      G�O�G�O�G�O�                