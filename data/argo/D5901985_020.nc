CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2011-12-21T10:11:06Z creation;2015-04-24T01:37:44Z conversion to V3.1;2016-11-17T02:51:22Z update;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20111221101106  20161129232529  5901985 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  P7_97922_020                    2C  D   PROVOR                          09027                           5815A03                         841 @�9�|�1   @�@"R��@3^vȴ9X�dl ě��1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?���@�33A$��Aq��A�33A�  A���B��B!��B6��BH  B]33Br  B�ffB���B���B�33B���B�  B���B�ffB���B�  B噚B�  B�33C�CL�C33C  C��C�fC �3C%� C*�3C/L�C433C933C>33CC�CH  CR33C\L�Ce�3Cp33Cz  C��C��fC�ٚC�&fC���C�ffC��C�  C�33C��3C�@ C�ٚC�&fC��C�  C�@ C�33C�ٚC��fC��C��3C��3C���C�� C�33DfD�D&fD  D��DfD��D%  D*  D/�D3�3D9�D=��DC&fDH&fDM&fDR,�DW�D[�3D`��Df&fDk,�DpfDu�Dz  D�P D��fD��fD�  D�@ D���D��3D��D�L�D���D��fD�  D�I�DԀ D��3D�	�D�P D�3D��D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?���@�  A#33Ap  A�ffA�33A�  BffB!33B6ffBG��B\��Bq��B�33B�ffB���B�  B�ffB���B���B�33Bҙ�B���B�ffB���B�  C  C33C�C�fC� C��C ��C%ffC*��C/33C4�C9�C>�CC  CG�fCR�C\33Ce��Cp�Cy�fC�  C���C���C��C���C�Y�C�  C��3C�&fC��fC�33C���C��C��C��3C�33C�&fC���C�ٚC�  C��fC��fC�� C�s3C�&fD  D3D  D�D�3D  D�3D$��D)��D/fD3��D9fD=�fDC  DH  DM  DR&fDWfD[��D`�3Df  Dk&fDp  DufDy��D�L�D��3D��3D���D�<�D���D�� D��D�I�D��fD��3D��D�FfD�|�D�� D�fD�L�D� D�D�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�G�A�E�A�?}A�;dA�=qA�=qA�9XA�;dA�;dA�=qA�=qA�?}A�A�A�E�A�G�A�I�A�I�A�G�A�K�A�E�A�$�A�VA��HA���A��+A�`BA�n�A�(�A���A�|�A�%A�A�M�A�VA���A��+A�A��hA���A�K�A�jA��A�`BA�A�XAv��Aj�HAa�AS/ALĜA@A5|�A1+A*�+A'K�A$ZA!��A�A�uA�hA�`AdZA33A9XAo@��@���@� �@�!@�;d@�v�@���@�|�@��@�;d@�=q@��T@�;d@��/@���@�o@���@��@���@�-@�A�@���@�A�@��@���@�Q�@�@��!@��u@�ff@�1'@��T@�b@w
=@mp�@f@\9X@R��@K��@E�h@?;d@8��@2J@+o@%�@!�@�@%@/@��@{21111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�G�A�E�A�?}A�;dA�=qA�=qA�9XA�;dA�;dA�=qA�=qA�?}A�A�A�E�A�G�A�I�A�I�A�G�A�K�A�E�A�$�A�VA��HA���A��+A�`BA�n�A�(�A���A�|�A�%A�A�M�A�VA���A��+A�A��hA���A�K�A�jA��A�`BA�A�XAv��Aj�HAa�AS/ALĜA@A5|�A1+A*�+A'K�A$ZA!��A�A�uA�hA�`AdZA33A9XAo@��@���@� �@�!@�;d@�v�@���@�|�@��@�;d@�=q@��T@�;d@��/@���@�o@���@��@���@�-@�A�@���@�A�@��@���@�Q�@�@��!@��u@�ff@�1'@��T@�b@w
=@mp�@f@\9X@R��@K��@E�h@?;d@8��@2J@+o@%�@!�@�@%@/@��@{21111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B1'B2-B2-B2-B33B2-B2-B2-B2-B2-B2-B2-B2-B33B2-B33B33B33B33B1'B0!BDBVB�B�BĜB��B{BbB  B�`B�XB�DB@�B �B�B�B�#B�qB�!B�VBP�B �B
�B
��B
,B	�
B	��B	L�B	)�B�B��BŢB�qB�)B�ZB�B�TB��B��BB�wB�jB�#B�
B�B�;B�ZB�B��B	oB	�B	.B	;dB	@�B	N�B	ffB	u�B	}�B	��B	�B	�^B	ÖB	��B	��B	�B	�BB	�TB	�mB	�B	�B	��B	��B
B
B
	7B
uB
�B
!�B
(�B
.B
5?B
>wB
B�B
F�B
L�B
Q�B
XB
^5B
cTB
ffB
jB
m�B
q�B
t�B
x�41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�B2-B2-B2-B33B2-B2GB2-B2-B2-B2GB2GB2-B33B2-B33B33B33B33B1'B0!B^BVB�B�+BĶB��B{BbB B�`B�XB�DB@�B �B�B�B�#B�qB�!B�VBP�B �B
�B
��B
,B	�$B	��B	L�B	*B�B�B��B��B�CB�tB�B�nB�B��BªB��B��B�=B�$B�+B�VB�tB��B��B	�B	�B	.IB	;B	@�B	N�B	ffB	u�B	~B	��B	�B	�xB	ÖB	��B	�B	�B	�\B	�nB	�B	�B	�B	��B	�B
'B
3B
	RB
uB
�B
!�B
)B
.B
5ZB
>�B
B�B
F�B
L�B
RB
XB
^OB
cTB
ffB
j�B
m�B
q�B
t�B
x�41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.1(dbar); PO2=0.0(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201307281651112013072816511120130728165111201608161351042016081613510420160816135104JA  ARFMdecpP7_e                                                                20111221101103  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20111221101106  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20111221101109  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20111221101113  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8c                                                                20111221101114  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20111221101115  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20111221101115  CF  PSAL            ?���?���?�                  JA  ARGQpump1.0                                                                 20111221101115  CF  TEMP            ?���?���?�                  JA  ARUP                                                                        20111221102606                      G�O�G�O�G�O�                JA  ARFMdecpP7_e                                                                20111223160801  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20111223161323  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20111223161324  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20111223161328  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8c                                                                20111223161329  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20111223161329  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20111223161329  CF  PSAL            ?���?���?�                  JA  ARGQpump1.0                                                                 20111223161329  CF  TEMP            ?���?���?�                  JA  ARUP                                                                        20111223162117                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150424013744                      G�O�G�O�G�O�                JA  ARUP                                                                        20150503002515                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130728075111  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130728075111  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2014V1                                                       20160816045104  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20161129232529                      G�O�G�O�G�O�                