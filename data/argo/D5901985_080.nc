CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2013-08-11T21:59:35Z creation;2015-04-24T01:37:44Z conversion to V3.1;2016-11-17T02:44:12Z update;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20130811215935  20161129234510  5901985 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               PA   JA  P7_97922_080                    2C  D   PROVOR                          09027                           5815A03                         841 @ְ:�韀1   @ְ=?� @6�I�^5�c��hr�1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?��@�ffA!��Ai��A�  A�ffA�33BffB   B4ffBJffB^ffBp  B���B���B�ffB���B�33B�33B�ffB���Bә�Bܙ�B癚B�ffB�33C��C�C� C  C��C�C ��C%�C*�3C/  C3��C9L�C=��CB� CH� CQ�3C\  Ce�fCo�fCz�fC�@ C�L�C�� C�@ C�Y�C���C�33C��C�s3C��fC��fC��C���C�� C�� C�� C�ٚC��C�33C�33C��C�&fC�s3C�33C�@ D  D��D�D&fD  D  D fD$ٚD*&fD/fD4�D9&fD=�3DB��DH  DM�DR3DV� D\fD`�3Df�Dk9�Dp,�Du�Dz�D�L�D���D���D�3D�\�D�� D��fD��3D�P D���D�� D�	�D�I�D�vfD���D� D�C3D퉚D�ɚD�P 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?333@���A#33Ak33A���A�33A�  B��B ffB4��BJ��B^��BpffB���B���B���B���B�ffB�ffB���B�  B���B���B���B�B�ffC�fC33C��C�C�3C33C �3C%33C*��C/�C3�3C9ffC=�fCB��CH��CQ��C\�Cf  Cp  C{  C�L�C�Y�C���C�L�C�ffC���C�@ C��C�� C��3C��3C�&fC�ٚC���C���C���C��fC��C�@ C�@ C�&fC�33C�� C�@ C�L�D&fD  D  D,�D&fD&fD �D$� D*,�D/�D4  D9,�D=��DB�3DHfDM3DR�DV�fD\�D`��Df3Dk@ Dp33Du  Dz  D�P D���D���D�fD�` D��3D�ٚD��fD�S3D�� D��3D��D�L�D�y�D�� D�3D�FfD��D���D�S311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A���A��`Aϴ9AϬAϮAϩ�Aϛ�Aϙ�A�A�A�S�Aȇ+A�ȴA��HA���A��HA��/A��uA��RA�bA���A�v�A�VA��
A���A�jA��A��9A�{A���A�A�Q�A��`A�x�A�(�A��-A�v�A��A���A�ZA�ȴA�1A��A��jA��hAt=qAlQ�AgdZA_�mA^�/AY`BAM��AB�uA@�A6��A3��A.�A'C�A%7LA �An�AAA�AVA�jA�A�#@�ȴ@�b@���@�j@�C�@�-@�/@��!@�%@�"�@��@���@���@���@���@�dZ@��@��@�hs@�dZ@�V@�x�@�/@���@��+@��;@�{@���@�7L@w\)@m�@g\)@`Ĝ@Z=q@R�@Kƨ@Ep�@@r�@:~�@7K�@2^5@.�+@)�@#��@
=@dZ@v�@��@l�21111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A���A��`Aϴ9AϬAϮAϩ�Aϛ�Aϙ�A�A�A�S�Aȇ+A�ȴA��HA���A��HA��/A��uA��RA�bA���A�v�A�VA��
A���A�jA��A��9A�{A���A�A�Q�A��`A�x�A�(�A��-A�v�A��A���A�ZA�ȴA�1A��A��jA��hAt=qAlQ�AgdZA_�mA^�/AY`BAM��AB�uA@�A6��A3��A.�A'C�A%7LA �An�AAA�AVA�jA�A�#@�ȴ@�b@���@�j@�C�@�-@�/@��!@�%@�"�@��@���@���@���@���@�dZ@��@��@�hs@�dZ@�V@�x�@�/@���@��+@��;@�{@���@�7L@w\)@m�@g\)@`Ĝ@Z=q@R�@Kƨ@Ep�@@r�@:~�@7K�@2^5@.�+@)�@#��@
=@dZ@v�@��@l�21111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�JB�\B�VB�JB�PB�PB�PB�PB�JB�DB�1Bx�Bo�BjBu�By�B�JB��B��B�oB��B��B��B�uB�JB�=B{�Bt�BjBbNBS�BO�B<jB �B+B�ZB��B�PBbNB1'B�B�oB	7B
��B
[#B	��B	��B	�B	�\B	�=B	l�B	hB��B��BĜB��B�XB��B��B�\B�By�Bs�B�1Bx�BffB`BB]/B\)B\)BD�Bo�B�=B��B�uB�1B��B��B	/B	<jB	B�B	T�B	bNB	�B	�7B	��B	�9B	�jB	ƨB	�XB	�}B	�;B	�fB	�B	�B	��B	��B	��B
+B
VB
�B
!�B
,B
2-B
<jB
B�B
F�B
L�B
Q�B
VB
\)B
aHB
dZB
hsB
l�B
o�41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�B�\B�<B�JB�PB�PB�6B�PB�0B�DB�Bx�Bo�BjBu�By�B�JB��B��B�oB��B��B��B�uB�JB�#B{�Bt�BjeBb4BS�BO�B<PB �B+B�ZB��B�PBb4B1B�B�oB	B
��B
[	B	��B	ʦB	��B	�\B	�=B	l�B	NB��B��BĜB��B�XB��B��B�BB�By�Bs�B�1Bx�BfLB`BB]/B\B\BD�Bo�B�#B��B�[B�1B��B��B	/ B	<jB	BuB	T�B	b4B	��B	�B	��B	�B	�jB	ƎB	�>B	�cB	�;B	�LB	�wB	��B	��B	��B	��B
B
VB
sB
!�B
+�B
2B
<PB
B�B
F�B
L�B
Q�B
VB
\B
aHB
d@B
hXB
lqB
o�41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.0(dbar); PO2=0.1(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201308250704362013082507043620130825070436201608161402072016081614020720160816140207JA  ARFMdecpP7_h                                                                20130811215915  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20130811215935  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20130811215937  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20130811215941  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20130811215941  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20130811215942  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20130811215942  CF  PSAL            ?��?��?�                  JA  ARGQpump1.0                                                                 20130811215942  CF  TEMP            ?��?��?�                  JA  ARUP                                                                        20130811221506                      G�O�G�O�G�O�                JA  ARFMdecpP7_h                                                                20130814185715  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20130814190532  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20130814190533  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20130814190537  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20130814190537  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20130814190538  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20130814190538  CF  PSAL            ?��?��?�                  JA  ARGQpump1.0                                                                 20130814190538  CF  TEMP            ?��?��?�                  JA  ARUP                                                                        20130814191913                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150424013744                      G�O�G�O�G�O�                JA  ARUP                                                                        20150503004509                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130824220436  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130824220436  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2014V1                                                       20160816050207  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20161129234510                      G�O�G�O�G�O�                