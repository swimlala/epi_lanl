CDF   !   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2011-09-12T16:08:35Z creation;2015-04-24T01:37:44Z conversion to V3.1;2016-11-17T02:52:33Z update;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20110912160835  20161129232521  5901985 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               
A   JA  P7_97922_010                    2C  D   PROVOR                          09027                           5815A03                         841 @�9Eg��1   @�=��J @1����+�dbM��1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       >L��@�  A33Ac33A���A���A�33BffB!��B6ffBI33B]33Bq��B�  B�  B�  B�  B�  B���B���B�ffB�ffB�33B�33B�  B�ffC�3C  C��CffC  CL�C �3C%�fC)�fC/�C4ffC9L�C>��CC��CHffCR�3C\33Ce�3Co�3Cy� C�33C��C�� C�@ C�ffC�ffC�Y�C�L�C�s3C�@ C�ffC�  C�Y�C�  C��C��3C�  Cֳ3C��C���C��C��3C�fC��C��fD��D9�D�D�3D��D  D   D$ٚD*3D.�3D3� D8� D=��DC  DG��DL�3DQ�fDV��D\�Da  Df�Dk3Do��DtٚDzFfD�FfD���D�� D��D�P D���D��fD��D�FfD���D��3D�  D�P DԌ�DڶfD�	�D�Y�D� D��D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111>���@�33A��Ad��A�ffA���A�  B��B"  B6��BI��B]��Br  B�33B�33B�33B�33B�33B���B���Bə�Bә�B�ffB�ffB�33B���C��C�C�3C� C�CffC ��C&  C*  C/33C4� C9ffC>�3CC�3CH� CR��C\L�Ce��Co��Cy��C�@ C�&fC���C�L�C�s3C�s3C�ffC�Y�C�� C�L�C�s3C��C�ffC��C�&fC�  C��C�� C��C�ٚC噚C�  C�3C�&fC��3D�3D@ D3D��D�3DfD fD$� D*�D.ٚD3�fD8�fD=� DCfDG�3DLٚDQ��DV�3D\3DafDf3Dk�Do�3Dt� DzL�D�I�D���D��3D� D�S3D�� D�ɚD�  D�I�D���D��fD�3D�S3DԐ Dڹ�D��D�\�D�3D� D� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A՗�A�E�A��A��A�bA�
=A��A�ȴAӰ!AӅA�1'A���AҬA��A�{AξwA�5?A���A�7LA��yAʡ�A�9XA�(�A�G�A��
A��HA��HA��#A�=qA���A���A��DA�Q�A�n�A��A��yA�z�A��HA��jA�%A�n�A��;A��wA���A���A��RA��Aq��Adn�A]�AU�AI��AC��A<�A0��A,VA&  A�hAXA��A�wA	O�A�@�V@��R@���@��
@��@�?}@��`@�j@��@Ə\@�{@���@��w@��j@�E�@���@�;d@���@��/@��7@�dZ@�A�@�@�l�@�
=@�S�@��/@��7@��w@���@���@��u@��w@\)@x�9@l��@cC�@]p�@T�@L��@D��@>v�@5�@.�+@)��@$Z@\)@^5@�T@7L@��@
�H22111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A՗�A�E�A��A��A�bA�
=A��A�ȴAӰ!AӅA�1'A���AҬA��A�{AξwA�5?A���A�7LA��yAʡ�A�9XA�(�A�G�A��
A��HA��HA��#A�=qA���A���A��DA�Q�A�n�A��A��yA�z�A��HA��jA�%A�n�A��;A��wA���A���A��RA��Aq��Adn�A]�AU�AI��AC��A<�A0��A,VA&  A�hAXA��A�wA	O�A�@�V@��R@���@��
@��@�?}@��`@�j@��@Ə\@�{@���@��w@��j@�E�@���@�;d@���@��/@��7@�dZ@�A�@�@�l�@�
=@�S�@��/@��7@��w@���@���@��u@��w@\)@x�9@l��@cC�@]p�@T�@L��@D��@>v�@5�@.�+@)��@$Z@\)@^5@�T@7L@��@
�H22111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�jBƨBŢBƨBƨBƨBŢBÖB�}B�B�B��B��B��B�uB��B�\B�VB�JBz�B�B}�B}�B|�Bx�B}�B�BÖBB7LB[#B��B�9BÖB��B��B��B��BǮBB�?BYB �B��B��B;dB
�sB
o�B	��B	� B	\)B	 �B�B�B��B�`B��B	bB��B�3B�RB�?B�B��B�VB�uB��B��B�B��B�NB��B	B	$�B	7LB	H�B	VB	jB	x�B	�JB	��B	��B	�9B	�qB	ƨB	�B	�HB	�ZB	�B	��B	��B
B
1B
DB
VB
uB
�B
�B
&�B
-B
6FB
:^B
=qB
C�B
H�B
N�B
T�B
[#B
_;B
dZB
iyB
n�B
q�B
v�B
z�B
}�44111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�BňBƨBƨBƨBŢBÖB�}B�B�B��B��B��B�uB��B�\B�VB�JBz�B�B}�B}�B|�Bx�B}�B�BÖBB7LB[#B��B�9BÖB��BοB��B��BǔBB�%BYB �B��B��B;dB
�sB
o�B	��B	� B	\)B	 �B�B�B��B�`B��B	bB�B�3B�RB�?B�B��B�pB�uB��B�B�B��B�NB�B	B	$�B	7LB	H�B	VB	jB	x�B	�JB	��B	��B	�9B	�qB	ƎB	�B	�-B	�@B	�}B	��B	��B
B
B
DB
VB
uB
�B
�B
&�B
-B
6+B
:^B
=VB
C{B
H�B
N�B
T�B
[	B
_;B
d@B
i_B
n�B
q�B
v�B
z�B
}�44111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.0(dbar); PO2=0.1(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201307281650292013072816502920130728165029201608161349182016081613491820160816134918JA  ARFMdecpP7_d                                                                20110912160829  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20110912160835  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20110912160836  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20110912160840  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8c                                                                20110912160840  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20110912160841  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20110912160841  CF  PSAL            >L��@�  ?�                  JA  ARGQpump1.0                                                                 20110912160841  CF  TEMP            >L��@�  ?�                  JA  ARUP                                                                        20110912162151                      G�O�G�O�G�O�                JA  ARFMdecpP7_d                                                                20110915010916  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20110915011647  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20110915011648  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20110915011653  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8c                                                                20110915011653  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20110915011653  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20110915011653  CF  PSAL            >L��@�  ?�                  JA  ARGQpump1.0                                                                 20110915011653  CF  TEMP            >L��@�  ?�                  JA  ARUP                                                                        20110915012601                      G�O�G�O�G�O�                JA  ARFMdecpP7_e                                                                20111021064708  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20111021064925  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20111021064926  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20111021064930  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8c                                                                20111021064931  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20111021064931  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20111021064931  CF  PSAL            >L��@�  ?�                  JA  ARGQpump1.0                                                                 20111021064931  CF  TEMP            >L��@�  ?�                  JA  ARUP                                                                        20111021065124                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150424013744                      G�O�G�O�G�O�                JA  ARUP                                                                        20150503002511                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130728075029  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130728075029  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2014V1                                                       20160816044918  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20161129232521                      G�O�G�O�G�O�                