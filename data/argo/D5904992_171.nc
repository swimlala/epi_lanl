CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-07-06T06:52:51Z creation;2020-07-09T21:53:52Z conversion to V3.1;2020-12-25T04:13:22Z update;     
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
_FillValue                  �  L�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   M   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   V   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    h�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    h�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    h�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    h�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  h�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    h�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    h�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    h�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         i   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         i   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        i   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    iArgo profile    3.1 1.2 19500101000000  20200706065251  20210115031508  5904992 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  V4_131538_171                   2C  D   ARVOR                           OIN-13JAP-ARL-66                5607A07                         844 @�&}|�G 1   @�&��/ @:��hr��c�n��O�1   ARGOS   A   B   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       >���@�  A33Ap  A���A�  A���BffB"��B6��BJffBZ  Br  B�33B���B���B���B�  B�33B�ffB���B�  B�33B�33B�33B�33C�3C� C33C�fC� CL�C �fC%  C*33C/  C4� C8��C>��CB��CHffCR� C\33Cf��Cp�CzffC��C�@ C�Y�C��3C�33C��fC�@ C��3C�ffC�  C�L�C��fC�33C��3C�  C�L�C��C�33C�&fC�L�C��C��3C�33C�ٚC��D3D&fD��D�D3D9�D 3D%&fD*9�D/33D433D8��D>&fDCfDH  DM3DQ��DW  D[��Da  Df�DkfDpfDu,�Dz,�D�FfD���D�ɚD��D�@ D��3D���D��D�S3D��fD���D�3D�0 DԆfD��fD�fD�FfD�fD�ɚ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 >���@�  A33Ap  A���A�  A���BffB"��B6��BJffBZ  Br  B�33B���B���B���B�  B�33B�ffB���B�  B�33B�33B�33B�33C�3C� C33C�fC� CL�C �fC%  C*33C/  C4� C8��C>��CB��CHffCR� C\33Cf��Cp�CzffC��C�@ C�Y�C��3C�33C��fC�@ C��3C�ffC�  C�L�C��fC�33C��3C�  C�L�C��C�33C�&fC�L�C��C��3C�33C�ٚC��D3D&fD��D�D3D9�D 3D%&fD*9�D/33D433D8��D>&fDCfDH  DM3DQ��DW  D[��Da  Df�DkfDpfDu,�Dz,�D�FfD���D�ɚD��D�@ D��3D���D��D�S3D��fD���D�3D�0 DԆfD��fD�fD�FfD�fD�ɚ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A̓A�;dA�A˼jA˧�A˅A�v�A�p�A��A���A�ȴA��A���A��/A���A�1'A�oA��#A��^A���A�G�A��yA���A�VA���A�p�A�;dA�=qA�7LA�(�A��A��DA��;A~z�Ax�AwC�Ar��Ap�DAn1'Ak&�Af��A[�mATQ�AM�;AGx�AAS�A<=qA8�9A5\)A21'A,E�A(��A&�9A#O�A�A`BA��A��A�A�A�A �A
�A��A{@�dZ@��@�/@�@��@��@ԓu@�^5@���@���@��@�5?@��u@�hs@�b@�ff@�n�@��;@�?}@��
@�r�@�-@���@�@{�m@xA�@s"�@o�@l�D@j-@f�@_�;@Y��@Rn�@K�@E��@=��@8��@2=q@-�h@(r�@#t�@�w@Z@��@�F@�@�@�@�/1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A̓A�;dA�A˼jA˧�A˅A�v�A�p�A��A���A�ȴA��A���A��/A���A�1'A�oA��#A��^A���A�G�A��yA���A�VA���A�p�A�;dA�=qA�7LA�(�A��A��DA��;A~z�Ax�AwC�Ar��Ap�DAn1'Ak&�Af��A[�mATQ�AM�;AGx�AAS�A<=qA8�9A5\)A21'A,E�A(��A&�9A#O�A�A`BA��A��A�A�A�A �A
�A��A{@�dZ@��@�/@�@��@��@ԓu@�^5@���@���@��@�5?@��u@�hs@�b@�ff@�n�@��;@�?}@��
@�r�@�-@���@�@{�m@xA�@s"�@o�@l�D@j-@f�@_�;@Y��@Rn�@K�@E��@=��@8��@2=q@-�h@(r�@#t�@�w@Z@��@�F@�@�@�@�/3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B#�B�B�B�B��B�Bk�BhsB]/BR�BA�B=qB<jB8RB2-B)�B{B%B�fB�5B�B��Bx�BL�B�BB
��B
��B
�=B
n�B
XB
F�B
�B
oB	��B	�ZB	��B	B	��B	l�B	A�B	�B��B�B�ZB�BǮB�FB��B�{B�JB�Bx�Bo�BiyBdZB]/BXBL�BD�B;dB2-B)�B"�B�B�BuBVB	7B+BDB�B�B%�B6FBG�B_;Bv�B��B�-BÖB�
B�NB��B	JB	�B	?}B	Q�B	cTB	x�B	�1B	��B	��B	�B	ȴB	�/B	�B
+B
{B
$�B
.B
8RB
@�B
H�B
P�B
VB
ZB
`BB
dZB
jB
n�B
s�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B$B�BB�BյB�9Bl�BlBabBT�BB[B>]B>B9XB3MB+�B�B1B�B��B�B��B{0BO�B�B�B
�jB
�!B
��B
p!B
ZB
I7B
�B
aB	�$B	�zB	��B	ĜB	��B	n�B	C-B	!�B	 �B� B�FB�BȴB��B��B�B�PB�By�BpUBj0BeB]�BYKBM�BE�B<�B3B+6B#�B;B�B�B(B
=B1B�B�B!B&fB6�BHKB_�BwLB��B�|B��B�?B�B�B	~B	B	?�B	R B	c�B	y$B	�KB	��B	��B	�"B	��B	�IB	��B
EB
�B
$�B
.IB
8RB
@�B
H�B
Q B
VB
Z7B
`\B
dtB
j�B
n�B
s�B
x�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
</O<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              PO1=0.1(dbar); PO2=0.1(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202007200015272020072000152720200720001527202007200200122020072002001220200720020012202007210012032020072100120320200721001203  JA  ARFMdecpV4_b                                                                20200706065250  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200706065251  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200706065251  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200706065252  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200706065252  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200706065257  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20200706065416                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20200709215309  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200709215350  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200709215350  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200709215351  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200709215351  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200709215352  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200709215352                      G�O�G�O�G�O�                JA  ARUP                                                                        20200709215441                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20200710000000  CF  PSAL_ADJUSTED_QC>���>���G�O�                JM  ARSQJMQC2.0                                                                 20200710000000  CF  TEMP_ADJUSTED_QC>���>���G�O�                JM  ARCAJMQC2.0                                                                 20200719151527  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200719151527  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20200719170012  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20200720151203  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20210115031508                      G�O�G�O�G�O�                