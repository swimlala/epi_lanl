CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2013-03-04T22:03:24Z creation;2015-04-24T01:37:44Z conversion to V3.1;2016-11-17T02:46:07Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ;l   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  =�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  >$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ?�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  @h   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  B8   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  B�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  D|   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  D�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  F�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  G4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  I   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  L�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    M   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    S   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    Y   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  _   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    _X   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    _\   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    _`   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    _d   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  _h   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    _�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    _�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    _�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         _�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         _�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        _�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20130304220324  20161129234513  5901985 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               @A   JA  P7_97922_064                    2C  D   PROVOR                          09027                           5815A03                         841 @ֈ:0*�1   @ֈ=�ۗ�@4F$�/��c���l�D1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?L��@�ffA   Ay��A���A�  A�ffB	33B ��B1��BF��B\  Bn��B�ffB�  B���B�33B���B���B���Bə�B�33Bۙ�B�ffB�  B�  C� C� C� CffC��C�fC �C$L�C(��C/��C533C9ffC>�3CCffCHL�CR� C\��Cf  Co��Cy��C�33C�� C�&fC���C��C��C��C�ٚC���C�ffC��C�@ C�33C�33C�33C�@ C�@ C�@ C�L�C�&fC�L�C�&fC��C�ffC��D3D�fD&fDfD,�D  D fD%&fD*  D/fD4�D9&fD>�DC�DG��DL��DR�DW  D[��Da�De�3Dk  Do� Dt��Dz�D�<�D��fD���D�3D�S3D�|�D��3D��D�@ D��3D��3D���D�P Dԙ�D��fD�3D�FfD��D��fD�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?fff@���A!��A{33A�ffA���A�33B	��B!33B2  BG33B\ffBo33B���B�33B���B�ffB���B���B���B���B�ffB���B噚B�33B�33C��C��C��C� C�3C  C 33C$ffC(�3C/�3C5L�C9� C>��CC� CHffCR��C\�3Cf�Co�3Cy�3C�@ C���C�33C�ٚC��C��C��C��fC�ٚC�s3C��C�L�C�@ C�@ C�@ C�L�C�L�C�L�C�Y�C�33C�Y�C�33C�&fC�s3C�&fD�D��D,�D�D33DfD �D%,�D*&fD/�D4  D9,�D>  DC3DH  DM  DR  DW&fD\  Da  De��Dk&fDo�fDt�3Dz3D�@ D���D�� D�fD�VfD�� D��fD� D�C3D��fD��fD���D�S3DԜ�D�ٚD�fD�I�D� D�ٚD�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��
A��A��A��#A��/A��;A��;A��HA��TA��mA��A��yA��A��A��A��A��A���A��A��A�ȴA��A���A��A���A�  A�^5A���A�C�A�-A�dZA��A��9A���A���A��hA�t�A��RA��!A���A�O�A��AwVAn��Ag�
A_�AR�AOK�AI�AA|�A5l�A/S�A&�A#|�A�A��AA��A	��A&�A��A9XA�!@���@@�v�@�9X@�C�@�"�@۶F@��;@�Ĝ@��@�|�@�n�@���@�+@��T@��@��@��!@��
@���@��@��@�Z@�1'@�$�@�G�@�~�@�@�=q@�v�@�bN@���@�~�@~�@uV@h��@]�@T1@L�@D�@=��@8��@2=q@,�j@%?}@\)@�@$�@��@ƨ@Ĝ@�21111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��
A��A��A��#A��/A��;A��;A��HA��TA��mA��A��yA��A��A��A��A��A���A��A��A�ȴA��A���A��A���A�  A�^5A���A�C�A�-A�dZA��A��9A���A���A��hA�t�A��RA��!A���A�O�A��AwVAn��Ag�
A_�AR�AOK�AI�AA|�A5l�A/S�A&�A#|�A�A��AA��A	��A&�A��A9XA�!@���@@�v�@�9X@�C�@�"�@۶F@��;@�Ĝ@��@�|�@�n�@���@�+@��T@��@��@��!@��
@���@��@��@�Z@�1'@�$�@�G�@�~�@�@�=q@�v�@�bN@���@�~�@~�@uV@h��@]�@T1@L�@D�@=��@8��@2=q@,�j@%?}@\)@�@$�@��@ƨ@Ĝ@�21111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BC�BJ�BK�BK�BJ�BJ�BK�BL�BL�BL�BK�BJ�BL�BK�BK�BJ�BK�BK�BK�BL�BK�BA�B.B(�B�B	7B�B�TB��B��B�XB�B��Br�B?}B{B�B�B9XB�B
��B
B
w�B
PB	�B	�B	x�B	@�B	-B	oB�BƨB�B��B�hB�Bp�Bq�BaHB\)Bn�Bn�Bn�BdZBZBYB^5BW
B� B�JB��B�!BŢB��BǮB��B��B	�B	@�B	L�B	jB	��B	�B	�RB	��B	��B	��B	��B	�B	�)B	�HB	�B	��B
B
B
	7B
bB
�B
�B
&�B
/B
5?B
9XB
?}B
C�B
I�B
N�B
T�B
_;B
dZB
jB
o�B
s�B
x�B
{�B
~�41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�BJ�BK�BK�BJ�BJ�BK�BL�BL�BL�BK�BJ�BL�BK�BK�BJ�BK�BK�BK�BL�BK�BA�B.B(�B�B	7B�B�TB��B��B�XB�B��Br�B?}B{B��B�B9XB�B
��B
B
w�B
6B	�B	�B	x�B	@�B	-B	oB�BƨB�B��B�hB�Bp�Bq�BabB\)Bn�Bn�Bn�BdZBZ7BYB^5BW
B�B�JB��B�;BŢB� BǮB��B��B	�B	@�B	L�B	jB	��B	�B	�RB	��B	��B	��B	��B	��B	�)B	�HB	�qB	��B
 �B
B
	7B
bB
�B
�B
&�B
/B
5?B
9XB
?}B
C�B
I�B
N�B
T�B
_;B
d@B
jeB
o�B
s�B
x�B
{�B
~�41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.0(dbar); PO2=0.1(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201307281653092013072816530920130728165309201608161358582016081613585820160816135858JA  ARFMdecpP7_h                                                                20130304220303  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20130304220324  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20130304220326  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20130304220330  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20130304220331  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20130304220333  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20130304220333  CF  PSAL            ?L��?L��?�                  JA  ARGQpump1.0                                                                 20130304220333  CF  TEMP            ?L��?L��?�                  JA  ARUP                                                                        20130304221738                      G�O�G�O�G�O�                JA  ARFMdecpP7_h                                                                20130307190102  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20130307191016  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20130307191017  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20130307191022  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20130307191022  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20130307191022  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20130307191022  CF  PSAL            ?L��?L��?�                  JA  ARGQpump1.0                                                                 20130307191022  CF  TEMP            ?L��?L��?�                  JA  ARUP                                                                        20130307192101                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150424013744                      G�O�G�O�G�O�                JA  ARUP                                                                        20150503004510                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130728075309  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130728075309  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2014V1                                                       20160816045858  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20161129234513                      G�O�G�O�G�O�                