CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-04-19T06:52:09Z creation;2020-04-22T15:52:46Z conversion to V3.1;2022-11-10T04:20:24Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ;�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  <H   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  >   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  >�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  @\   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  @�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  B�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  C   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  D�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  EX   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  G(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  G�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Il   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  K<   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  M   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   M�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   V�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  h�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    i   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    i    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    i$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    i(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  i,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    il   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    i|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    i�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         i�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         i�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        i�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20200419065209  20221117231506  5905222 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               EA   JA  V4_131533_069                   2C  D   ARVOR                           OIN-13JAP-ARL-61                5607A07                         844 @�����1   @��/�c @.��t�j�dӥ�S��1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?��@�  A��As33A�33A�33A�  B
ffB��B6ffBI��B]33Bp��B33B�33B���B�ffB�  B���B�33B�33B���B�  B���B�B�33C33C�3C�fC33C��CffC � C%��C*��C.��C4� C8�3C>L�CB��CG� CQ��CZ��Ce� Cp  Cz�C�33C�s3C��fC��3C�&fC�  C��fC��C�� C��3C�� C��C���C��CǦfC�  C��3Cֳ3C�  C�  C��fC�� C��C��3C�&fD� D�fD��D�fD�fD33D ,�D%�D*  D/fD3��D8�fD=��DC3DG� DMfDR,�DW  D[� Da3De��Dk3DpfDt��Dy��D�,�D��fD�� D�3D�<�D���D�� D�  D�S3D�vfD���D��D�Y�DԖfD�ٚD�  D�VfD�3D�ɚD�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?   @���A33Aq��A�ffA�ffA�33B
  BffB6  BI33B\��BpffB~��B�  B���B�33B���B�ffB�  B�  Bҙ�B���B癚B�ffB�  C�C��C��C�C� CL�C ffC%�3C*�3C.�3C4ffC8��C>33CB�3CGffCQ� CZ�3CeffCo�fCz  C�&fC�ffC���C��fC��C��3C�ٚC�  C��3C��fC��3C�  C���C��CǙ�C��3C��fC֦fC��3C��3C�ٚC�3C�  C��fC��D��D� D�3D� D� D,�D &fD%3D*�D/  D3�fD8� D=�fDC�DGٚDM  DR&fDV��D[ٚDa�De�fDk�Dp  Dt�fDy�3D�)�D��3D���D�  D�9�D��fD���D���D�P D�s3D���D�fD�VfDԓ3D��fD���D�S3D� D��fD�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���Aգ�Aՙ�A�^5A�`BA�\)A�ZA�O�A҃A�~�A�K�A�A�^5AάA��/A�~�A̴9A�dZA�33A�ƨA�jA�M�A�VA¸RA��!A���A��#A�%A�ƨA���A���A��`A��9A��#A���Az��Avv�At�`Am?}AiC�AWoAM
=ADZA<��A6ffA41A0�yA.E�A$��A!�mAA��A��A�A�jAt�A$�AS�A~�AAS�A	ƨAhsA�/@��@�`B@�?}@���@�7L@��;@�hs@ա�@�-@ѡ�@��@ʸR@���@�b@��@���@�I�@�O�@��@�"�@��F@��@�9X@�Ĝ@�7L@��y@��@�&�@���@���@���@�~�@��^@�/@t��@kƨ@b�@W�@S��@Ko@@�`@:-@2��@+dZ@%��@
=@t�@��@33@@	&�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���Aգ�Aՙ�A�^5A�`BA�\)A�ZA�O�A҃A�~�A�K�A�A�^5AάA��/A�~�A̴9A�dZA�33A�ƨA�jA�M�A�VA¸RA��!A���A��#A�%A�ƨA���A���A��`A��9A��#A���Az��Avv�At�`Am?}AiC�AWoAM
=ADZA<��A6ffA41A0�yA.E�A$��A!�mAA��A��A�A�jAt�A$�AS�A~�AAS�A	ƨAhsA�/@��@�`B@�?}@���@�7L@��;@�hs@ա�@�-@ѡ�@��@ʸR@���@�b@��@���@�I�@�O�@��@�"�@��F@��@�9X@�Ĝ@�7L@��y@��@�&�@���@���@���@�~�@��^@�/@t��@kƨ@b�@W�@S��@Ko@@�`@:-@2��@+dZ@%��@
=@t�@��@33@@	&�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�1B�bB�hB�hB�hB�oB�hB�\B�FB�NB�B	VB	)�B	/B	:^B	8RB	@�B	R�B	hsB	v�B	�DB	�?B	��B
K�B
��B
��B
��B
�B
�ZB
ŢB
��B
��B
VB
(�B	��B	B	��B	��B	p�B	I�B	1B�mB��BȴBƨBBȴB��BɺBȴBȴB�#B	B	ffB	�DB	��B	�9B	�qB	�qB	��B	��B	��B	ȴB	�3B	��B	ZB	e`B	l�B	u�B	|�B	� B	�hB	��B	��B	ŢB	ɺB	��B	��B	�B	�B	�ZB	��B	��B	��B	��B
B
JB
bB
uB
�B
�B
�B
 �B
!�B
$�B
%�B
/B
49B
:^B
=qB
E�B
I�B
K�B
O�B
W
B
^5B
bNB
iyB
n�B
s�B
v�B
y�B
~�B
�B
�=B
�J33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B�B��B	�B	*�B	/�B	:�B	9	B	@�B	S@B	h�B	w2B	�0B	��B
uB
U�B
ؓB
�B
��B
�B
�8B
�jB
��B
�$B
[�B
-B
mB	ĶB	�*B	��B	shB	S&B	B�B��B�rB�_B�{B�lBĶBʦB��B�=BܬB	 �B	f�B	��B	��B	��B	��B	�(B	̘B	�\B	ϑB	�	B	��B	��B	Z�B	e�B	l�B	vB	}VB	�OB	��B	��B	�>B	��B	�#B	�BB	�&B	�B	�kB	��B	�B	�B	�DB	�DB
�B
�B
�B
�B
�B
�B
�B
!B
!�B
%B
&2B
/OB
4nB
:�B
=�B
E�B
I�B
K�B
P.B
W$B
^OB
bhB
i�B
n�B
s�B
v�B
y�B
.B
�'B
�=B
�d33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<,1<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              PO1=-0.2(dbar); PO2=-0.3(dbar)                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202005030016132020050300161320200503001613202210251312122022102513121220221025131212202210251807362022102518073620221025180736  JA  ARFMdecpV4_b                                                                20200419065208  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200419065209  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200419065209  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200419065210  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200419065210  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200419065210  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20200419065345                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20200422155217  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200422155244  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200422155245  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200422155245  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200422155245  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200422155246  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200422155246                      G�O�G�O�G�O�                JA  ARUP                                                                        20200422155334                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20200422151606  QCP$                G�O�G�O�G�O�7DEB7C          JM  ARGQrqcjv291                                                                20200422151606  QCF$                G�O�G�O�G�O�200000          JM  ARCAJMQC2.0                                                                 20200502151613  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200502151613  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221025041212  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20221025090736  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221117231506                      G�O�G�O�G�O�                