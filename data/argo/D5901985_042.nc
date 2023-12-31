CDF   !   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2012-07-28T10:02:51Z creation;2015-04-24T01:37:44Z conversion to V3.1;2016-11-17T02:48:44Z update;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20120728100251  20161129232526  5901985 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               *A   JA  P7_97922_042                    2C  D   PROVOR                          09027                           5815A03                         841 @�Q91�ր1   @�Q:� @2޸Q��c�;dZ�1   ARGOS   B   B   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                           @�ffA   AnffA���A�33A�  B
��B��B4  BI33B]��Bq33B���B�  B���B�ffB���B���B���B�33B�33B�  B���B���B���C��C33C� C�fC�3C� C��C$�3C)�3C/  C4L�C8L�C=� CB��CG��CR�3C\� Cf33Cp��Cz��C�ffC�&fC�  C��C�Y�C��C��C�ٚC��3C�ٚC��3C��3C�ٚC�� Cǳ3C�� CҌ�C��C�@ C�Y�C�@ C��C�@ C�L�C�s3D9�D3D3D3D�3D��D�3D%33D*fD/&fD43D9fD>  DC3DG� DL� DQ�3DV��D[�fD`�3Df  Dk33Dp3Du�Dz,�D�,�D�y�D�� D�	�D�Y�D�� D���D���D�@ D�p D���D�  D�<�D�p Dڳ3D�3D�Y�D퉚D�3D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�@�33AffAl��A�  A�ffA�33B
ffB33B3��BH��B]33Bp��B�ffB���B�ffB�33B�ffB�ffB���B�  B�  B���B晚BB�ffC�3C�CffC��C��CffC� C$��C)��C.�fC433C833C=ffCB� CG�3CR��C\ffCf�Cp� Cz�3C�Y�C��C��3C�  C�L�C��C��C���C��fC���C��fC��fC���C³3CǦfC̳3CҀ C��C�33C�L�C�33C�  C�33C�@ C�ffD33D�D�D�D��D�fD��D%,�D*  D/  D4�D9  D>�DC�DGٚDLٚDQ��DV�3D[� D`��De��Dk,�Dp�Du3Dz&fD�)�D�vfD���D�fD�VfD�|�D���D��fD�<�D�l�D�ɚD���D�9�D�l�Dڰ D� D�VfD�fD� D�� 41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A΃A��/A���AͼjAͲ-AͮAͲ-AͰ!AͬA�S�A�z�A˥�A��/A�7LA�-A�`BA��HA���A���A���A�t�A�A���A�l�A���A��!A�oA��A���A��`A�A�`BA�A�Q�A�^5A��wA�dZA�ZA���A���A��A��;A��hA�A|�At�HAf��AY�AP�RAGhsA@�A:A�A7`BA3A.ȴA(�+A$^5A��A\)AffA�A	?}A;dA�T@��@�9@��@�^@�dZ@�h@�v�@�@�ƨ@�K�@��@�r�@�A�@�X@���@�ff@�A�@��@��j@�
=@�?}@��T@���@���@�I�@�5?@���@�"�@�b@�`B@�;d@���@���@{�F@t�@i&�@^@V�@N��@H��@@�u@6��@0Ĝ@*n�@&�R@ r�@�F@��@Ĝ@�D@
�H@	�21111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�A��/A���AͼjAͲ-AͮAͲ-AͰ!AͬA�S�A�z�A˥�A��/A�7LA�-A�`BA��HA���A���A���A�t�A�A���A�l�A���A��!A�oA��A���A��`A�A�`BA�A�Q�A�^5A��wA�dZA�ZA���A���A��A��;A��hA�A|�At�HAf��AY�AP�RAGhsA@�A:A�A7`BA3A.ȴA(�+A$^5A��A\)AffA�A	?}A;dA�T@��@�9@��@�^@�dZ@�h@�v�@�@�ƨ@�K�@��@�r�@�A�@�X@���@�ff@�A�@��@��j@�
=@�?}@��T@���@���@�I�@�5?@���@�"�@�b@�`B@�;d@���@���@{�F@t�@i&�@^@V�@N��@H��@@�u@6��@0Ĝ@*n�@&�R@ r�@�F@��@Ĝ@�D@
�H@	�41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B<jBI�BI�BI�BJ�BK�BJ�BL�BM�BH�B.B#�B'�BQ�BdZBL�B`BB�hB�{B��B�B�B�9BɺB�BȴB�BƨB��B��B�+BO�B33B��B�ZB��BVB5?B�B
�B
�TB
�B
��B
�B
S�B
�B	�FB	hsB	5?B	�B��B�fB�)B��B�wB�B��B��B�VB�%B}�B�1B�=B�B~�Bw�Bx�B~�B�{B��B��B�B�qB�B��B	\B	/B	G�B	z�B	�{B	��B	��B	�B	��B	ǮB	��B	�)B	�ZB	�sB	�B	��B
  B
%B

=B
VB
hB
�B
�B
!�B
,B
2-B
5?B
:^B
A�B
I�B
S�B
YB
^5B
aHB
gmB
k�B
n�B
v�B
z�B
|�B
~�41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�BI�BI�BI�BJ�BK�BJ�BL�BM�BH�B.B#�B'�BQ�BdZBL�B`\B�hB��B��B�B�B�9BɺB�BȴB�BƨB�B��B�EBO�B33B�B�ZB��BVB5?B�B
�B
�nB
�B
��B
�B
TB
�B	�`B	h�B	5ZB	�B�B�B�CB�B��B�5B��B��B�pB�?B~(B�fB�XB�'BBw�By	BB��B��B��B�5B��B�9B��B	vB	/5B	G�B	z�B	��B	��B	��B	�)B	��B	ǮB	��B	�CB	�ZB	�sB	�B	��B
 B
?B

XB
pB
�B
�B
�B
!�B
,"B
2GB
5ZB
:xB
A�B
I�B
S�B
Y1B
^OB
abB
gmB
k�B
n�B
v�B
z�B
|�B
~�41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.1(dbar); PO2=0.0(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201307281652112013072816521120130728165211201608161354512016081613545120160816135451JA  ARFMdecpP7_g                                                                20120728100247  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20120728100251  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20120728100252  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20120728100256  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20120728100257  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8d                                                                20120728100257  QCF$                G�O�G�O�G�O�            4100JA  ARGQrqcpt16b                                                                20120728100257  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20120728101704                      G�O�G�O�G�O�                JA  ARFMdecpP7_g                                                                20120730160617  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20120730161438  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20120730161440  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20120730161444  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20120730161444  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8d                                                                20120730161444  QCF$                G�O�G�O�G�O�            4100JA  ARGQrqcpt16b                                                                20120730161445  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20120730162323                      G�O�G�O�G�O�                JA  ARFMdecpP7_g                                                                20120914041752  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20120914041916  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20120914041918  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20120914041922  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20120914041922  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20120914041922  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20120914041923  CF  PSAL                    ?�                  JA  ARGQpump1.0                                                                 20120914041923  CF  TEMP                    ?�                  JA  ARUP                                                                        20120914043528                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150424013744                      G�O�G�O�G�O�                JA  ARUP                                                                        20150503002514                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130728075211  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130728075211  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20160802000000  CF  PRES_ADJUSTED_QC       G�O�                JM  ARSQJMQC2.0                                                                 20160802000000  CF  TEMP_ADJUSTED_QC       G�O�                JM  ARSQOW  1.1 CTD2014V1                                                       20160816045451  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20161129232526                      G�O�G�O�G�O�                