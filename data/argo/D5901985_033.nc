CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2012-04-29T10:03:50Z creation;2015-04-24T01:37:44Z conversion to V3.1;2016-11-17T02:49:49Z update;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20120429100350  20161129232525  5901985 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               !A   JA  P7_97922_033                    2C  D   PROVOR                          09027                           5815A03                         841 @�:�����1   @�:�9D��@3���m�d1x���1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?   @�ffA!��Al��A���A�ffA�  B33B��B2��BH  B]33Bn  B���B�  B���B�33B�33B���B���B�  B�33B�ffB���B�B�  C��C�CL�C� C  C� C 33C%33C)33C.�3C433C8ffC=�fCC33CHL�CQ�fC\ffCf�fCq  Cz��C�  C�ffC�33C�@ C��C��C�33C�ٚC�� C��3C��C�  C�� C�ffC�@ C̀ CҌ�C�33C�ffC��C�@ C�Y�C�Y�C�@ C�&fDfD��D�D�3DfD��D�fD$� D)��D/33D43D9  D=�3DC&fDG� DMfDR�DVٚD\�D`�3Df9�Dk�Dp  DufDz,�D�FfD���D��fD��D�&fD�� D���D�3D�S3D���D���D�fD�I�Dԃ3D���D��D�S3D� D��D�ɚ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?��@���A#33AnffA�ffA�33A���B��B  B333BHffB]��BnffB���B�33B�  B�ffB�ffB�  B���B�33B�ffBݙ�B�  B���B�33C�fC33CffC��C�C��C L�C%L�C)L�C.��C4L�C8� C>  CCL�CHffCR  C\� Cg  Cq�Cz�fC��C�s3C�@ C�L�C�&fC�&fC�@ C��fC���C�� C�&fC��C���C�s3C�L�C͌�Cҙ�C�@ C�s3C�&fC�L�C�ffC�ffC�L�C�33D�D�3D3DٚD�D  D��D$�fD)�3D/9�D4�D9&fD=��DC,�DG�fDM�DR3DV� D\  D`ٚDf@ Dk  Dp&fDu�Dz33D�I�D���D�ɚD� D�)�D��3D�� D�fD�VfD�� D���D��D�L�DԆfD�� D�  D�VfD�3D� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�=qA�oA���A���A�ƨAżjAũ�A�v�A�n�A�hsAę�A�?}A��^A�v�A�VA�`BA��A�VA�l�A���A���A��9A��A��uA�+A���A���A���A�A�A��A��A���A��RA��yA��hA�=qA��!A�%A�`BA�hsA�&�A���A�\)A��mAw��Am�Ab-AW�AO��AG�wA?XA:��A5/A+�TA't�A#��A!VAK�A��AJA
v�A �AVA�+@��T@�%@�R@��;@띲@�bN@߮@�M�@Гu@�/@ǍP@�ȴ@�b@��j@��@�-@���@��@�S�@���@�x�@�1'@�&�@�ȴ@���@�J@�9X@���@�z�@�ȴ@�  @���@�E�@��u@}p�@sdZ@j=q@_+@X��@P�`@HbN@@�@9��@1�@,1@&{@!��@(�@�@@�@�21111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�=qA�oA���A���A�ƨAżjAũ�A�v�A�n�A�hsAę�A�?}A��^A�v�A�VA�`BA��A�VA�l�A���A���A��9A��A��uA�+A���A���A���A�A�A��A��A���A��RA��yA��hA�=qA��!A�%A�`BA�hsA�&�A���A�\)A��mAw��Am�Ab-AW�AO��AG�wA?XA:��A5/A+�TA't�A#��A!VAK�A��AJA
v�A �AVA�+@��T@�%@�R@��;@띲@�bN@߮@�M�@Гu@�/@ǍP@�ȴ@�b@��j@��@�-@���@��@�S�@���@�x�@�1'@�&�@�ȴ@���@�J@�9X@���@�z�@�ȴ@�  @���@�E�@��u@}p�@sdZ@j=q@_+@X��@P�`@HbN@@�@9��@1�@,1@&{@!��@(�@�@@�@�21111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
\)B
cTB
cTB
cTB
cTB
cTB
aHB
`BB
`BB
_;B
_;B
N�B
T�B
\)B
��B
�#BffBBdZBt�Bm�B�B{�B��B�PB~�B_;BZBP�BD�B>wB33BC�B>wB<jB:^B33B �BB�sB�}BP�B
��B
5?B	��B	ffB	;dB	�B��B�#BɺB�jB�B��B��B��B��B�B�3B�3B��B�oB�JB�JB�VB�VB�JB�oB�hB�hB�uB� BǮB��B��B	bB	�B	=qB	P�B	jB	t�B	�hB	��B	�-B	�?B	ǮB	��B	��B	�#B	�ZB	�B	�B	��B	��B	��B	��B

=B
oB
�B
"�B
(�B
2-B
6FB
>wB
D�B
L�B
R�B
XB
]/B
bNB
ffB
k�B
n�B
q�B
u�B
w�41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�B
cTB
cTB
cTB
cTB
cTB
aHB
`BB
`BB
_;B
_;B
N�B
T�B
\)B
��B
�#BffBBdZBt�BmwB�B{�B��B�PB~�B_;BZBP�BD�B>wB33BC�B>]B<jB:^B3B �B�B�sB�cBP�B
��B
5?B	��B	ffB	;dB	�B��B�=BɺB��B�B��B��B��B��B�/B�MB�3B��B��B�JB�dB�VB�VB�dB��B�hB��B�uB� BǮB��B��B	bB	�B	=qB	P�B	jeB	t�B	�hB	��B	�-B	�?B	ǔB	̳B	��B	�#B	�@B	�B	�B	��B	��B	��B	��B

=B
oB
�B
"�B
(�B
2-B
6FB
>wB
D�B
L�B
R�B
XB
]/B
bNB
fLB
k�B
n}B
q�B
u�B
w�41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.0(dbar); PO2=0.1(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201307281651462013072816514620130728165146201608161353212016081613532120160816135321JA  ARFMdecpP7_g                                                                20120429100347  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20120429100350  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20120429100352  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20120429100356  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20120429100356  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20120429100357  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20120429100357  CF  PSAL            ?   ?   ?�                  JA  ARGQpump1.0                                                                 20120429100357  CF  TEMP            ?   ?   ?�                  JA  ARUP                                                                        20120429101735                      G�O�G�O�G�O�                JA  ARFMdecpP7_g                                                                20120501160257  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20120501160923  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20120501160924  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20120501160929  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20120501160929  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20120501160929  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20120501160929  CF  PSAL            ?   ?   ?�                  JA  ARGQpump1.0                                                                 20120501160929  CF  TEMP            ?   ?   ?�                  JA  ARUP                                                                        20120501161851                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150424013744                      G�O�G�O�G�O�                JA  ARUP                                                                        20150503002513                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130728075146  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130728075146  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2014V1                                                       20160816045321  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20161129232525                      G�O�G�O�G�O�                