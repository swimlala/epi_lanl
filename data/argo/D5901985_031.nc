CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2012-04-09T10:04:29Z creation;2015-04-24T01:37:44Z conversion to V3.1;2016-11-17T02:50:03Z update;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20120409100429  20161129232527  5901985 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  P7_97922_031                    2C  D   PROVOR                          09027                           5815A03                         841 @�5�韀1   @�5�:Ӏ@3�t�j�d<Q��1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?fff@���A#33AvffA�33A�33A���B��B!33B6  BK33B_��Bp  B�33B�33B�33B���B�ffB���B�  BǙ�Bҙ�Bۙ�B�  B�ffB���C�3C��C33CffC�fC  C� C$  C)�fC.ffC4�3C9� C?  CC�3CH�3CR  C\ffCe�fCp��CzL�C��3C�� C���C��3C�ٚC��3C��C���C�ٚC���C���C�&fC�&fC�ffC�L�C�@ C�&fC��C��C�  C�&fC�33C�@ C��fC�33D��DfD@ D3D  D  D &fD%  D)��D.��D4  D8�3D>�DB�3DH�DL� DQ��DW9�D\�Da�DffDk  Dp�DufDy� D�33D�l�D���D�fD�9�D��fD��fD�3D�C3D��fD��3D�3D�,�DԆfD�ɚD�3D�VfD�p D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ?L��@�ffA!��At��A�ffA�ffA�  B33B ��B5��BJ��B_33Bo��B�  B�  B�  B�ffB�33B�ffB���B�ffB�ffB�ffB���B�33B���C��C� C�CL�C��C�fCffC#�fC)��C.L�C4��C9ffC>�fCC��CH��CQ�fC\L�Ce��Cp� Cz33C��fC��3C�� C��fC���C��fC��C�� C���C�� C�� C��C��C�Y�C�@ C�33C��C��C��C��3C��C�&fC�33C�ٚC�&fD�fD  D9�D�D�D��D   D$��D)�3D.�3D3��D8��D>3DB��DHfDLٚDQ�fDW33D\3DafDf  Dj��Dp3Du  DyٚD�0 D�i�D���D�3D�6fD��3D��3D� D�@ D��3D�� D�  D�)�Dԃ3D��fD� D�S3D�l�D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aĕ�Aď\AąAąAāA�|�A�~�AāAăAăAąAć+AąAô9A��`A�7LA��A��A��+A��!A�%A�%A��`A��HA�ƨA��A��A��!A�O�A��TA�|�A���A�  A�jA�{A�VA��uA�VA��^A�ȴA�z�A�VA��PA�1A��A|JArM�Ah��A_�ATVAN�HAG��A@A�A9�A.r�A%�A�FA��AffAE�A��A�AE�A^5@�t�@��R@��#@�p�@�I�@�33@�@ם�@�M�@�r�@��!@��j@��@�bN@��y@���@�;d@�Q�@�x�@�1'@�?}@���@�ff@�33@�r�@��\@��u@�J@�(�@�@���@�E�@��@z�@sC�@i7L@_�;@W�;@QG�@G�@=p�@4�/@.V@*J@&v�@"-@�T@t�@��@�@�w2111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aĕ�Aď\AąAąAāA�|�A�~�AāAăAăAąAć+AąAô9A��`A�7LA��A��A��+A��!A�%A�%A��`A��HA�ƨA��A��A��!A�O�A��TA�|�A���A�  A�jA�{A�VA��uA�VA��^A�ȴA�z�A�VA��PA�1A��A|JArM�Ah��A_�ATVAN�HAG��A@A�A9�A.r�A%�A�FA��AffAE�A��A�AE�A^5@�t�@��R@��#@�p�@�I�@�33@�@ם�@�M�@�r�@��!@��j@��@�bN@��y@���@�;d@�Q�@�x�@�1'@�?}@���@�ff@�33@�r�@��\@��u@�J@�(�@�@���@�E�@��@z�@sC�@i7L@_�;@W�;@QG�@G�@=p�@4�/@.V@*J@&v�@"-@�T@t�@��@�@�w2111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BR�BS�BS�BS�BS�BT�BT�BT�BVBW
BXB\)Be`Bx�Be`B�=B�%B|�By�B�RB�BPB�B(�B�B�B	7B��B�TB�B�B��B��BǮB�wB�VBaHB�HBƨB�B�hB>wB!�B%B
��B
@�B	��B	�jB	�+B	N�B	0!B	\B�B�yB�^B�B��B��B�!B�B�XB��BB�qB�dBȴB��B�)B�TB�HB�HB��B��B	�B	0!B	@�B	R�B	gmB	x�B	� B	��B	��B	�XB	ŢB	�
B	�BB	�sB	�B	��B	��B	��B
B
B
	7B
JB
\B
�B
�B
!�B
(�B
0!B
7LB
<jB
E�B
M�B
T�B
[#B
_;B
cTB
gmB
jB
l�B
p�B
t�B
w�4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�BS�BS�BTBS�BT�BT�BT�BVBW$BXB\)Be`Bx�Be`B�=B�%B|�By�B�RB�BjB�B)B�B�B	RB��B�nB�7B�B��B��B��B��B�VBabB�HBƨB�B�hB>wB!�B%B
��B
@�B	��B	��B	�EB	N�B	0;B	vB��B�B�xB�IB��B��B�UB�OB�rB��BªB��B�B��B�,B�]B�nB�bB�bB��B��B	�B	0!B	@�B	SB	g�B	x�B	�B	��B	�
B	�rB	żB	�$B	�\B	�sB	�B	��B	��B	�B
B
9B
	7B
dB
vB
�B
�B
!�B
(�B
0;B
7fB
<jB
E�B
M�B
UB
[=B
_;B
cTB
g�B
jB
l�B
p�B
t�B
w�4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.1(dbar); PO2=0.0(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201307281651412013072816514120130728165141201608161352592016081613525920160816135259JA  ARFMdecpP7_g                                                                20120409100425  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20120409100429  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20120409100430  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20120409100435  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20120409100435  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20120409100435  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20120409100435  CF  PSAL            ?fff?fff?�                  JA  ARGQpump1.0                                                                 20120409100435  CF  TEMP            ?fff?fff?�                  JA  ARUP                                                                        20120409101935                      G�O�G�O�G�O�                JA  ARFMdecpP7_g                                                                20120411160222  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20120411160855  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20120411160857  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20120411160901  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20120411160901  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20120411160902  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20120411160902  CF  PSAL            ?fff?fff?�                  JA  ARGQpump1.0                                                                 20120411160902  CF  TEMP            ?fff?fff?�                  JA  ARUP                                                                        20120411161717                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150424013744                      G�O�G�O�G�O�                JA  ARUP                                                                        20150503002513                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130728075141  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130728075141  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2014V1                                                       20160816045259  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20161129232527                      G�O�G�O�G�O�                