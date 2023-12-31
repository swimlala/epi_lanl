CDF   !   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2012-03-10T10:05:58Z creation;2015-04-24T01:37:44Z conversion to V3.1;2016-11-17T02:50:25Z update;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20120310100558  20161129232520  5901985 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  P7_97922_028                    2C  D   PROVOR                          09027                           5815A03                         841 @�.9B.E 1   @�.;%�	�@3B��`A��d_��v�1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?333@���A!��Ah  A���A���A�  B	��B33B4ffBI��B^  Bn  B���B���B���B�33B���B���B�ffB�  B�ffB�33B���B�ffB�ffC� C�fC��CL�C  C��C �C%  C*  C/33C4L�C9ffC>33CC  CG�fCQ�fC\�Ce��CpL�C{  C�33C�Y�C�33C�Y�C�33C�ٚC�  C�@ C�33C��3C�Y�C��3C�&fC��C�ٚC��C��3C��3C܌�C�ffC�&fC�Y�C�Y�C�s3C�L�D  D&fD  D3D�DٚD��D$�fD*fD/,�D49�D9�D>�DC3DHfDM�DQ� DW�D[�fD`�3De�fDk33Dp3Du&fDz&fD�FfD�|�D�� D���D�L�D���D���D�fD�L�D��3D��3D� D�0 DԆfDڹ�D�3D�6fD�3D��fD��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?��@�ffA   AfffA�  A�  A�33B	33B��B4  BI33B]��Bm��B�ffB���B���B�  B�ffB�ffB�33B���B�33B�  B癚B�33B�33CffC��C�3C33C�fC�3C   C$�fC)�fC/�C433C9L�C>�CB�fCG��CQ��C\  Ce� Cp33Cz�fC�&fC�L�C�&fC�L�C�&fC���C��3C�33C�&fC��fC�L�C��fC��C�  C���C�  C��fC��fC܀ C�Y�C��C�L�C�L�C�ffC�@ D��D  D�D�DfD�3D�3D$� D*  D/&fD433D9fD>fDC�DH  DM3DQٚDW3D[� D`��De� Dk,�Dp�Du  Dz  D�C3D�y�D���D���D�I�D��fD��fD�3D�I�D�� D�� D��D�,�Dԃ3DڶfD�  D�33D� D��3D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A��A��A��A��A��A��A��A���A���A���A���A�JA��A�dZAǑhAǩ�A�p�A�M�A�{A��A�S�A�7LAš�AĸRA��A���A��A��DA��-A�z�A� �A��`A���A�t�A���A���A��^A�x�A��A�oA���A�G�A���A���Ax�9Aj��Ac/AZ5?AS7LAH�/ADr�A=K�A7��A3O�A-dZA(1A#�A�mAJAA^5A
�yA��A�@�(�@�V@��H@�;d@�D@��@�t�@�33@�I�@�z�@�=q@�^5@���@���@�A�@�J@�@�ȴ@���@��w@��@� �@���@�E�@�A�@��-@�Z@���@��m@�x�@���@��\@���@vv�@j�\@a7L@W�@O�@Hb@A�@8�@2n�@-O�@(b@#o@K�@@��@33@K�@�22111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A��A��A��A��A��A��A��A���A���A���A���A�JA��A�dZAǑhAǩ�A�p�A�M�A�{A��A�S�A�7LAš�AĸRA��A���A��A��DA��-A�z�A� �A��`A���A�t�A���A���A��^A�x�A��A�oA���A�G�A���A���Ax�9Aj��Ac/AZ5?AS7LAH�/ADr�A=K�A7��A3O�A-dZA(1A#�A�mAJAA^5A
�yA��A�@�(�@�V@��H@�;d@�D@��@�t�@�33@�I�@�z�@�=q@�^5@���@���@�A�@�J@�@�ȴ@���@��w@��@� �@���@�E�@�A�@��-@�Z@���@��m@�x�@���@��\@���@vv�@j�\@a7L@W�@O�@Hb@A�@8�@2n�@-O�@(b@#o@K�@@��@33@K�@�22111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B��BB\BG�B�=B��B�B�jB�qBŢB�#B�5B�fB&�BA�B#�B��B�/BJB�BB��B�9B�PBYBM�BD�B�B�B��B_;BB
ȴB
z�B
5?B	��B	��B	n�B	H�B	%�B	uB��B�HB��B�qB�B��B��B�VB�{B�jBƨB��B�FB�B��B�VB�+B�=B��B�'BĜB�ZB��B	�B	1'B	K�B	cTB	x�B	�oB	��B	��B	�jB	ĜB	��B	�
B	�BB	�B	�B	��B	��B	��B
  B
B
+B
\B
�B
�B
&�B
/B
6FB
>wB
D�B
J�B
P�B
W
B
[#B
`BB
dZB
hsB
k�B
o�B
t�B
w�B
y�44111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�B��B��B��B��B��B��B��B��B��B�BBvBG�B�=B��B�B�jB��BŢB�=B�5B�B&�BA�B#�B��B�/BJB�BB��B�TB�PBYBM�BD�B�B�B��B_;BB
��B
z�B
5ZB	�B	��B	n�B	H�B	%�B	�B�B�bB��B��B�IB��B��B�pB��B��B��B��B�zB�B��B�pB�EB�XB��B�ABĶB�tB�B	�B	1AB	K�B	cnB	x�B	�oB	��B	�B	��B	ĶB	��B	�$B	�\B	�B	�B	��B	��B	�B
 B
3B
EB
\B
�B
�B
&�B
/5B
6`B
>wB
D�B
J�B
P�B
W$B
[#B
`\B
dtB
h�B
k�B
o�B
t�B
w�B
y�44111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.1(dbar); PO2=0.0(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201307281651332013072816513320130728165133201608161352402016081613524020160816135240JA  ARFMdecpP7_g                                                                20120310100554  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20120310100558  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20120310100559  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20120310100604  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20120310100604  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20120310100604  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20120310100604  CF  PSAL            ?333@���?�                  JA  ARGQpump1.0                                                                 20120310100604  CF  TEMP            ?333@���?�                  JA  ARUP                                                                        20120310101920                      G�O�G�O�G�O�                JA  ARFMdecpP7_g                                                                20120312160437  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20120312161047  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20120312161049  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20120312161053  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20120312161053  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20120312161054  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20120312161054  CF  PSAL            ?333@���?�                  JA  ARGQpump1.0                                                                 20120312161054  CF  TEMP            ?333@���?�                  JA  ARUP                                                                        20120312162003                      G�O�G�O�G�O�                JM  ARFMjmbcP7_g                                                                20121002060455  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20121002060531  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20121002060533  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20121002060537  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20121002060537  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20121002060538  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20121002060538  CF  PSAL            ?333@���?�                  JA  ARGQpump1.0                                                                 20121002060538  CF  TEMP            ?333@���?�                  JA  ARUP                                                                        20121002072116                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150424013744                      G�O�G�O�G�O�                JA  ARUP                                                                        20150503002510                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130728075133  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130728075133  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2014V1                                                       20160816045240  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20161129232520                      G�O�G�O�G�O�                