CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-01-27T06:53:11Z creation;2020-01-30T21:54:00Z conversion to V3.1;2020-12-25T04:16:01Z update;     
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
_FillValue                  t  ;h   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  =�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  >   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ?�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  @\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  B(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  B�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  Dh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  D�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  F�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  G   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  H�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  L�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    L�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    R�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    X�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  ^�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    _4   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    _8   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    _<   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    _@   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  _D   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    _�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    _�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    _�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         _�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         _�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        _�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20200127065311  20210115031508  5904992 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  V4_131538_155                   2C  D   ARVOR                           OIN-13JAP-ARL-66                5607A07                         844 @��=(�1   @��D`�� @< �n���c��`A�71   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?fff@�ffA(  At��A�ffA���A홚B33B ��B4  BH  B[33BnffB�  B���B�  B�  B�ffB�33B���B�ffBљ�B�33B�33B�33B���C��C��CffC��C� C��C �C%L�C)��C/ffC4  C8�3C=�3CB� CG� CR  C[�fCf  Co�fCz�fC�Y�C��C�@ C�&fC�@ C��3C�  C��C��C�ffC�@ C��C�Y�C�&fC��C�@ C�ٚC�Y�C�33C�3C�L�C�@ C�  C��fC�L�D,�D,�D�DfD�fD  D 33D%,�D)�fD/33D43D9�D=��DC33DH  DMfDRfDW9�D\&fDa�Df�Dj� Dp&fDt�fDz�D�P D���D�ɚD��D�VfD��fD���D�	�D�@ D���D���D�	�D�@ Dԓ3D�� D���D�FfD�|�D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ?fff@�ffA(  At��A�ffA���A홚B33B ��B4  BH  B[33BnffB�  B���B�  B�  B�ffB�33B���B�ffBљ�B�33B�33B�33B���C��C��CffC��C� C��C �C%L�C)��C/ffC4  C8�3C=�3CB� CG� CR  C[�fCf  Co�fCz�fC�Y�C��C�@ C�&fC�@ C��3C�  C��C��C�ffC�@ C��C�Y�C�&fC��C�@ C�ٚC�Y�C�33C�3C�L�C�@ C�  C��fC�L�D,�D,�D�DfD�fD  D 33D%,�D)�fD/33D43D9�D=��DC33DH  DMfDRfDW9�D\&fDa�Df�Dj� Dp&fDt�fDz�D�P D���D�ɚD��D�VfD��fD���D�	�D�@ D���D���D�	�D�@ Dԓ3D�� D���D�FfD�|�D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A�bNA�"�A��mA��uA��-A�/A��
A�S�A���A��^A���A���A�XA�ffA��PA�XA�bA��A��yA��A�oA��\A�  A��FA�ƨA��#A�x�A���A�+A��A�p�A��A�VA�r�A�-A�z�A{;dAwl�As�^Ao�;AhĜAb �AY�^APz�AI�#AF��ABbNA<�!A9�A4=qA2��A/��A.A+�A(-A$A�A!t�AJA�#At�A�
AVA
�9A��A�A J@���@�@�hs@�9X@և+@��
@���@�M�@��9@�z�@�=q@���@��/@���@�Q�@��@�x�@�33@�r�@�n�@���@�X@�(�@{��@y�7@t�/@r=q@o;d@mV@co@\9X@U��@O|�@HĜ@Ct�@=��@6�y@01'@+t�@&��@!x�@�j@�P@��@+@t�@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A�bNA�"�A��mA��uA��-A�/A��
A�S�A���A��^A���A���A�XA�ffA��PA�XA�bA��A��yA��A�oA��\A�  A��FA�ƨA��#A�x�A���A�+A��A�p�A��A�VA�r�A�-A�z�A{;dAwl�As�^Ao�;AhĜAb �AY�^APz�AI�#AF��ABbNA<�!A9�A4=qA2��A/��A.A+�A(-A$A�A!t�AJA�#At�A�
AVA
�9A��A�A J@���@�@�hs@�9X@և+@��
@���@�M�@��9@�z�@�=q@���@��/@���@�Q�@��@�x�@�33@�r�@�n�@���@�X@�(�@{��@y�7@t�/@r=q@o;d@mV@co@\9X@U��@O|�@HĜ@Ct�@=��@6�y@01'@+t�@&��@!x�@�j@�P@��@+@t�@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BO�BT�BT�BW
B�\B��B�B��B��B��B��B��B��B��B��B�B�B[#B"�B�BÖB�wB��B�{Br�Be`BXBP�B33B�BPBB
�B
��B
�'B
�B
bNB
I�B
(�B
\B	�B	�wB	�bB	\)B	-B	hB	+B��B��B��B�FB�B��B��B�uB�PB�+B}�Bp�BdZBVBL�BC�B8RB33B+B �B�B{B\BPB+B+B\B�B'�B33BG�B\)Bm�B�=B��B�^B��B�NB��B	+B	�B	1'B	=qB	P�B	[#B	p�B	z�B	�+B	��B	�RB	��B	�mB	��B

=B
�B
"�B
.B
9XB
A�B
H�B
P�B
W
B
_;B
e`B
iyB
o�B
u�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BO�BT�BT�BW
B�\B��B�B��B��B��B��B��B��B��B��B�B�B[#B"�B�BÖB�wB��B�{Br�Be`BXBP�B33B�BPBB
�B
��B
�'B
�B
bNB
I�B
(�B
\B	�B	�wB	�bB	\)B	-B	hB	+B��B��B��B�FB�B��B��B�uB�PB�+B}�Bp�BdZBVBL�BC�B8RB33B+B �B�B{B\BPB+B+BBB�B'�B33BG�B\)Bm�B�=B��B�^B��B�NB��B	+B	�B	1'B	=qB	P�B	[#B	p�B	z�B	�+B	��B	�RB	��B	�mB	��B

XB
�B
"�B
.B
9XB
A�B
H�B
P�B
W
B
_VB
e`B
iyB
o�B
u�B
x�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.1(dbar); PO2=0.1(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202002100015342020021000153420200210001534202002110012202020021100122020200211001220JA  ARFMdecpV4_b                                                                20200127065310  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200127065311  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200127065311  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200127065312  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200127065312  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200127065313  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20200127065511                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20200130215317  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200130215358  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200130215358  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200130215359  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200130215359  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200130215359  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200130215400                      G�O�G�O�G�O�                JA  ARUP                                                                        20200130215449                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20200131000000  CF  PSAL_ADJUSTED_QC?fff?fffG�O�                JM  ARCAJMQC2.0                                                                 20200209151534  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200209151534  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20200210151220  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20210115031508                      G�O�G�O�G�O�                