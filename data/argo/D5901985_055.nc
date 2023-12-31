CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2012-12-05T10:02:27Z creation;2015-04-24T01:37:44Z conversion to V3.1;2016-11-17T02:47:12Z update;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20121205100227  20161129234514  5901985 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               7A   JA  P7_97922_055                    2C  D   PROVOR                          09027                           5815A03                         841 @�q��*� 1   @�q��� @3��hr��c����o1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       >L��@�ffA��Ax  A���A�  A�33B
��BffB2  BI��B\ffBn��B���B���B�  B���B�33B�33B���B�ffB�ffB�33B晚B�B�ffC33C  C33C�3CL�C�fC �3C%�C*�C/�3C4�fC9��C=�fCCL�CH  CQ��C[��Cf�Co��Cz33C�� C�s3C�ffC�  C�33C��C��C�33C�ٚC�Y�C�@ C��C�L�C�33C��C�&fC�33C�@ C�  C��fC晚C� C�s3C�  C�Y�D&fD  D&fD,�D  D��D &fD%  D)ٚD.�3D49�D93D>3DC,�DH3DM,�DR  DW&fD\fDa�Df&fDk�DpfDufDy�3D�33D�� D��fD�3D�L�D���D��3D�3D�L�D�s3D���D��3D�FfDԜ�D��3D� D�6fD�3D� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 >���@���AffAy��A�ffA���A�  B33B��B2ffBJ  B\��Bo33B���B���B�33B���B�ffB�ffB���Bʙ�Bә�B�ffB���B���B���CL�C�CL�C��CffC  C ��C%33C*33C/��C5  C9�3C>  CCffCH�CQ�3C[�fCf33Co�fCzL�C���C�� C�s3C��C�@ C��C�&fC�@ C��fC�ffC�L�C�&fC�Y�C�@ C�&fC�33C�@ C�L�C��C��3C�fC��C�� C��C�ffD,�D&fD,�D33D&fD�3D ,�D%fD)� D.��D4@ D9�D>�DC33DH�DM33DR&fDW,�D\�Da  Df,�Dk3Dp�Du�Dy��D�6fD��3D�ɚD�fD�P D���D��fD�fD�P D�vfD���D��fD�I�DԠ D��fD�3D�9�D�fD�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�
=A���A��`A��#A��A��
A���A���A�ĜAɸRAɰ!AɁA�A�A�&�A�-A�I�A��FA�A�A��#A�ffA�r�A��A�\)A���A���A�-A�+A�33A��A�VA�K�A��!A���A�Q�A�hsA���A��DA�JA���A�Az�Ag;dA\��AI%A=x�A933A0��A)�A$  A!�
A~�A�/A?}Az�A �AJA
^5AoA�@�v�@� �@�I�@��@�(�@�z�@�t�@�-@��@��@�ff@ʸR@�@�?}@��m@��D@���@�  @��\@��m@��-@���@�%@���@�J@�\)@��9@���@�@�/@�o@���@�+@��@���@��#@{��@r��@i�^@\�@R��@I�7@B��@>�@9�#@3�
@-/@(Ĝ@"^5@�@�u@�
@K�@"�@�P2111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�
=A���A��`A��#A��A��
A���A���A�ĜAɸRAɰ!AɁA�A�A�&�A�-A�I�A��FA�A�A��#A�ffA�r�A��A�\)A���A���A�-A�+A�33A��A�VA�K�A��!A���A�Q�A�hsA���A��DA�JA���A�Az�Ag;dA\��AI%A=x�A933A0��A)�A$  A!�
A~�A�/A?}Az�A �AJA
^5AoA�@�v�@� �@�I�@��@�(�@�z�@�t�@�-@��@��@�ff@ʸR@�@�?}@��m@��D@���@�  @��\@��m@��-@���@�%@���@�J@�\)@��9@���@�@�/@�o@���@�+@��@���@��#@{��@r��@i�^@\�@R��@I�7@B��@>�@9�#@3�
@-/@(Ĝ@"^5@�@�u@�
@K�@"�@�P2111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�BBoBhBhBoBhBoBhBbB\BVBJB%BBB�HB�'B��B�BƨB�FB�LB�?B�B�B��B�+BiyB@�B+B��B�B�3B��Br�B0!B
�/B
B
��B
m�B
2-B	�/B	e`B	'�B��B��B��B�hB�JB�+B�%B�B}�Bx�By�Bx�Bx�Bv�Bx�By�Bv�By�B�PB�DB��B��B��B��B��B�B�wB�BB	B	�B	-B	>wB	W
B	cTB	�B	��B	��B	�B	�jB	ȴB	�#B	�HB	�mB	�B	�B	��B	��B	��B
B
+B
DB
JB
\B
�B
"�B
)�B
33B
;dB
A�B
C�B
F�B
K�B
R�B
[#B
bNB
ffB
k�B
o�B
s�B
x�B
|�4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�BoBhBhBoBhBoBhBHB\BVBJB%BBB�HB�'B��B�BƨB�+B�2B�%B��B�B��B�+BiyB@�B+B��B�B�3B��Br�B0B
�/B
B
��B
mwB
2-B	�/B	e`B	'�B��B��B��B�hB�dB�+B�%B�3B}�Bx�By�Bx�Bx�Bv�Bx�By�Bv�By�B�PB�DB��B��B��B��B��B�B�wB�BB	B	�B	-B	>wB	W
B	cTB	�B	��B	��B	�B	�jB	ȴB	�#B	�HB	�mB	�kB	�B	��B	��B	��B
 �B
B
DB
0B
BB
�B
"�B
)�B
33B
;dB
A�B
C�B
F�B
K�B
R�B
[	B
bNB
fLB
k�B
o�B
s�B
x�B
|�4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.1(dbar); PO2=0.2(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201307281652462013072816524620130728165246201608161357272016081613572720160816135727JA  ARFMdecpP7_g                                                                20121205100224  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20121205100227  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20121205100229  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20121205100233  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20121205100233  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20121205100234  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20121205100234  CF  PSAL            >L��>L��?�                  JA  ARGQpump1.0                                                                 20121205100234  CF  TEMP            >L��>L��?�                  JA  ARUP                                                                        20121205101733                      G�O�G�O�G�O�                JA  ARFMdecpP7_g                                                                20121207160106  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20121207161026  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20121207161027  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20121207161032  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20121207161032  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20121207161032  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20121207161032  CF  PSAL            >L��>L��?�                  JA  ARGQpump1.0                                                                 20121207161032  CF  TEMP            >L��>L��?�                  JA  ARUP                                                                        20121207162136                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150424013744                      G�O�G�O�G�O�                JA  ARUP                                                                        20150503002514                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130728075246  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130728075246  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2014V1                                                       20160816045727  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20161129234514                      G�O�G�O�G�O�                