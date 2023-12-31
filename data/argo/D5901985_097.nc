CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   e   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       :2015-05-19T04:02:48Z creation;2016-11-17T02:42:06Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7,   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7l   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    80   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           84   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8<   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8@   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8H   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8P   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8X   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8\   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8l   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8p   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9p   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9t   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  h  ;   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ;p   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  h  =   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  =l   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  h  ?    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ?h   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  h  @�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Ad   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  h  B�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  C`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  h  D�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  E\   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  F�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  H�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  J   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    Jx   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    Px   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    Vx   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  \x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    \�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    \�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    \�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    \�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  \�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ]   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ],   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ]0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ]@   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ]D   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ]H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ]LArgo profile    3.1 1.2 19500101000000  20150519040248  20161129234510  5901985 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               aA   JA  P7_97922_097                    2C  D   PROVOR                          09027                           5815A03                         841 @�ڷ�A�1   @�ڷ� @5�hr� ��c�$�/1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?L��@�ffAffA|��A�  A�  A�33B��B"��B7��BH��B_33Bq��B�  B�ffB�ffB���B���B�ffB�  B�  Bҙ�Bݙ�B癚B�  B�  C��C�C  C��C�fCL�CffC%��C*ffC0�C4� C9��C>�fCCffCH33CR��C[�fCf��Cp�fCz�3C�Y�C�s3C�@ C�33C��C�Y�C�ffC�� C��fC�  C��fC�@ C�Y�CÀ CȌ�C�s3C�L�C�33C��3C�  C��C�&fC�&fC��3C�Y�D� D&fD��D  D�D9�D �D$�3D*3D/,�D43D9�D>  DC&fDH3DL�3DRL�DW�D\&fDa  Df&fDk@ Dp�Dt�3Dy� D�S3D���D���D��D�\�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ?333@�33A��A{33A�33A�33A�ffB33B"ffB733BHffB^��Bq33B���B�33B�33B���B���B�33B���B���B�ffB�ffB�ffB���B���C� C  C�fC�3C��C33CL�C%� C*L�C0  C4ffC9� C>��CCL�CH�CR�3C[��Cf�3Cp��Cz��C�L�C�ffC�33C�&fC��C�L�C�Y�C��3C�ٚC��3C�ٚC�33C�L�C�s3CȀ C�ffC�@ C�&fC��fC��3C�  C��C��C��fC�L�DٚD  D�3D��DfD33D 3D$��D*�D/&fD4�D9fD>�DC  DH�DL��DRFfDWfD\  Da�Df  Dk9�DpfDt��DyٚD�P D���D�ɚD�	�D�Y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�~�A�z�A�z�A�|�A�z�A�z�A�z�A�z�A�dZA�O�A�+A�{A��jA�p�A�"�A�ƨA��A��A��uA��A��HA�JA��DA�Q�A�ƨA�VA�p�A���A�hsA���A��A�K�A�(�A�7LA�33A�^5A���A���A�v�A�oA��PA�K�A{�Av1Ap1Afr�A\(�AVJAD��A=
=A3A*�uA'�A!�mA�^A�^A&�A�`An�AbNAp�A
�A�A�@��/@�J@�(�@���@��-@�Ĝ@�K�@���@�Z@��@��-@��@�1'@�@��@�33@��@��F@���@��@��-@���@��y@�=q@�`B@�(�@�V@�@�^5@�{@�@�@x  @o��@i�@`b@^�y11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�~�A�z�A�z�A�|�A�z�A�z�A�z�A�z�A�dZA�O�A�+A�{A��jA�p�A�"�A�ƨA��A��A��uA��A��HA�JA��DA�Q�A�ƨA�VA�p�A���A�hsA���A��A�K�A�(�A�7LA�33A�^5A���A���A�v�A�oA��PA�K�A{�Av1Ap1Afr�A\(�AVJAD��A=
=A3A*�uA'�A!�mA�^A�^A&�A�`An�AbNAp�A
�A�A�@��/@�J@�(�@���@��-@�Ĝ@�K�@���@�Z@��@��-@��@�1'@�@��@�33@��@��F@���@��@��-@���@��y@�=q@�`B@�(�@�V@�@�^5@�{@�@�@x  @o��@i�@`b@^�y21111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�1B�7B�7B�=B�7B�7B�7B�7B�+B�%B�%B�B�B� B~�Bz�Bw�Bp�BdZB[#BW
BM�B?}B2-B-B�B
=B��B�B�B$�B�B�BDB�TB��BdZB49B�#Bp�B
�B
��B
C�B
�B	�B	�-B	r�B	H�B	B�`B�}B��B��B�PB�1B�B|�B�7B�oB��B�oB�7B�=B�=Bl�Bn�Bv�B�uB�!B�3B�RB�LBĜB�TB	B	%B	#�B	2-B	B�B	M�B	ffB	t�B	�B	�7B	��B	��B	�B	�3B	�dB	B	ɺB	��B	�
B	�ZB	�`B	�B	��B
\B
�B
$�B
&�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�B�7B�7B�=B�7B�7B�7B�7B�+B�%B�%B�B�'B� B~�Bz�Bw�Bp�BdZB[#BW$BM�B?}B2-B-B�B
=B�B�B�B$�B�B�BDB�TB��BdtB4TB�#Bp�B
��B
��B
C�B
�B	�B	�GB	r�B	H�B	B�zB��B��B��B�jB�1B�B}B�7B�oB��B��B�7B�=B�XBl�Bn�Bv�B��B�;B�MB�lB�fBĜB�nB	-B	%B	#�B	2-B	B�B	M�B	ffB	t�B	�'B	�7B	��B	��B	�5B	�MB	�B	ªB	��B	��B	�$B	�tB	�zB	�B	��B
vB
�B
$�B
&�41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.1(dbar); PO2=0.0(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201402110702132014021107021320140211070213201608161405282016081614052820160816140528JA  ARFMdecpP7_g                                                                20150519040247  IP                  G�O�G�O�G�O�                JA  ARFMfmtp3.1                                                                 20150519040248  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20150519040249  QCP$                G�O�G�O�G�O�              1CJA  ARGQrqcpt19d                                                                20150519040250  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20150519040250  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcprnst                                                                20150519040250  QCP$                G�O�G�O�G�O�          600000JA  ARGQrqcprnst                                                                20150519040250  QCF$                G�O�G�O�G�O�          200000JA  ARUP                                                                        20150519104504                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20140203000000  CF  PSAL_ADJUSTED_QC?L��?L��G�O�                JM  ARSQJMQC2.0                                                                 20140203000000  CF  TEMP_ADJUSTED_QC?L��?L��G�O�                JM  ARGQJMQC2.0                                                                 20140203043331  CV  JULD            G�O�G�O�F�ջ                JM  ARCAJMQC2.0                                                                 20140210220213  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20140210220213  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2014V1                                                       20160816050528  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20161129234510                      G�O�G�O�G�O�                