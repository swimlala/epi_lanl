CDF   !   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2013-05-13T22:02:02Z creation;2015-04-24T01:37:44Z conversion to V3.1;2016-11-17T02:45:17Z update;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20130513220202  20161129234512  5901985 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               GA   JA  P7_97922_071                    2C  D   PROVOR                          09027                           5815A03                         841 @֙�W���1   @֙�<� @6�`A�7L�d0�t�j1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       >L��@�  A33AnffA�ffAř�A�  B��B!33B733BJ  B^  Br��B�ffB���B�ffB�33B�ffB���B�33Bș�B���B���B�33B�ffB�  C��C33C�3CL�C�fC  C��C$ffC)�C-��C4  C8�3C=L�CC33CG��CRffC]L�Cg�Cp�fCzL�C��C�ٚC���C��C�Y�C�&fC��fC�ٚC��fC��C��3C���C��3C�C�  C�L�C�Y�C�ffC�  C�@ C�&fC���C��3C�33C�@ D&fD��D�D�3D��D3D FfD$��D*&fD.�3D3� D8��D=��DC  DHfDM�DR  DV�3D[�fDa  DffDkfDp  Du�Dz  D�9�D�� D�� D��D�I�D�y�D�ɚD�3D�Y�D��fD��3D� D�c3Dԃ3Dڬ�D��D�0 D�|�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 =���@���A��Al��A���A���A�33BffB ��B6��BI��B]��BrffB�33B���B�33B�  B�33B���B�  B�ffBЙ�Bۙ�B�  B�33B���C� C�C��C33C��C�fC� C$L�C)  C-�3C3�fC8��C=33CC�CG� CRL�C]33Cg  Cp��Cz33C��C���C�� C��C�L�C��C�ٚC���C�ٚC��C��fC�� C��fC C��3C�@ C�L�C�Y�C��3C�33C��C�� C��fC�&fC�33D  D�fDfD��D�fD�D @ D$�3D*  D.��D3ٚD8�3D=�3DB��DH  DMfDR�DV��D[� Da�Df  Dk  Do��DufDy��D�6fD�|�D���D��D�FfD�vfD��fD�  D�VfD��3D�� D��D�` DԀ Dک�D�	�D�,�D�y�D�ɚ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��A��A��A��A�"�A�$�A�7LA�A�E�A���A���A���A�"�A�ĜA��uA�;dA��A�t�A��\A��hA�p�A���A��A���A�E�A�ƨA�Q�A��A� �A�
=A�  A�r�A�oA�9XA�XA�A�A�JA�+A��A��DA�$�A���A�n�A�p�AzApn�Am7LAd��Aa7LAU"�AN��AF��AB��A<E�A7��A4�RA0��A)�7A#�-A!XA?}A�A+AA+AQ�A�
A�@�+@��#@��@��@��u@�{@��@�{@�o@�$�@�dZ@��@�t�@�9X@���@��j@���@��\@��u@���@���@���@���@�dZ@��@�K�@���@�/@zM�@o��@e�h@]`B@W�w@N��@H  @D�@=@8��@2J@+�@&{@"-@�-@�@�@��2211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��A��A��A��A�"�A�$�A�7LA�A�E�A���A���A���A�"�A�ĜA��uA�;dA��A�t�A��\A��hA�p�A���A��A���A�E�A�ƨA�Q�A��A� �A�
=A�  A�r�A�oA�9XA�XA�A�A�JA�+A��A��DA�$�A���A�n�A�p�AzApn�Am7LAd��Aa7LAU"�AN��AF��AB��A<E�A7��A4�RA0��A)�7A#�-A!XA?}A�A+AA+AQ�A�
A�@�+@��#@��@��@��u@�{@��@�{@�o@�$�@�dZ@��@�t�@�9X@���@��j@���@��\@��u@���@���@���@���@�dZ@��@�K�@���@�/@zM�@o��@e�h@]`B@W�w@N��@H  @D�@=@8��@2J@+�@&{@"-@�-@�@�@��1211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BiyBk�BjBk�Bk�BjBiyB}�B��B�B��B��B��B��B��B��B��B�PBx�Bx�Bn�BcTBe`B� By�Bu�Bv�By�Bm�B�DB�=B�B_;B�fB�B�9B�JBu�BH�B��B�;B��B�B
�B
x�B
	7B	�^B	��B	k�B	P�B	�B��B�/B��B�LB��B��B��B�bB�DB�=B{�Bu�Bl�BhsBcTBcTBiyBiyBp�Bn�Bp�By�B�B��B��B��B�B��B	PB	�B	#�B	D�B	t�B	�B	~�B	�B	��B	��B	�^B	ŢB	��B	�)B	�mB	�B	�B
B

=B
uB
{B
�B
(�B
0!B
8RB
<jB
C�B
F�B
L�B
R�B
XB
\)B
aHB
gmB
l�B
o�4411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bi�G�O�BjBk�Bk�Bj�Bi�B}�B��B�"B��B��B��B�B��B��B��B�jBx�Bx�Bn�BcTBe`B� By�Bu�Bv�By�Bm�B�^B�=B�-B_;B�fB�B�TB�JBu�BH�B�B�;B��B�B
�B
x�B
	7B	�xB	��B	k�B	Q B	�B�B�IB�B��B�0B��B��B�}B�^B�rB|Bu�Bl�Bh�BcnBc�Bi�Bi�Bp�Bn�Bp�BzB� B��B��B��B�B�B	jB	�B	#�B	D�B	t�B	�'B	B	�9B	��B	��B	�xB	żB	��B	�CB	�mB	�B	��B
'B

XB
�B
�B
�B
)B
0;B
8lB
<�B
C�B
F�B
L�B
R�B
XB
\CB
abB
g�B
l�B
o�1411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.1(dbar); PO2=0.0(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201410221907062014102219070620141022190706201608161400252016081614002520160816140025JA  ARFMdecpP7_h                                                                20130513220143  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20130513220202  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20130513220203  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20130513220207  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20130513220208  QCP$                G�O�G�O�G�O�            FB40JA  ARGQpump1.0                                                                 20130513220208  CF  PSAL            >L��@�  ?�                  JA  ARGQpump1.0                                                                 20130513220208  CF  TEMP            >L��@�  ?�                  JA  ARUP                                                                        20130513221513                      G�O�G�O�G�O�                JA  ARFMdecpP7_h                                                                20130516155905  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20130516160821  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20130516160822  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20130516160827  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20130516160827  QCP$                G�O�G�O�G�O�            FB40JA  ARGQpump1.0                                                                 20130516160827  CF  PSAL            >L��@�  ?�                  JA  ARGQpump1.0                                                                 20130516160827  CF  TEMP            >L��@�  ?�                  JA  ARUP                                                                        20130516162150                      G�O�G�O�G�O�                JM  ARFMjmbcP7_h                                                                20131118015517  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20131118015704  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20131118015706  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20131118015710  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20131118015710  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20131118015711  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20131118015711  CF  PSAL            >L��@�  ?�                  JA  ARGQpump1.0                                                                 20131118015711  CF  TEMP            >L��@�  ?�                  JA  ARUP                                                                        20131118042214                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150424013744                      G�O�G�O�G�O�                JA  ARUP                                                                        20150503004512                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20130827000000  CF  PSAL_ADJUSTED_QC>L��>L��G�O�                JM  ARSQJMQC2.0                                                                 20130827000000  CF  TEMP_ADJUSTED_QC>L��>L��G�O�                JM  ARCAJMQC2.0                                                                 20141022100706  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20141022100706  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2014V1                                                       20160816050025  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20161129234512                      G�O�G�O�G�O�                