CDF   #   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2013-08-02T01:00:41Z creation;2015-04-24T01:37:44Z conversion to V3.1;2016-11-17T02:44:21Z update;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20130802010041  20161129234515  5901985 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               OA   JA  P7_97922_079                    2C  D   PROVOR                          09027                           5815A03                         841 @֭��b��1   @֭�eC! @7\(��c�Ƨ1   ARGOS   A   A   A   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?��@�33A��At��A���A�33A���B33B"ffB4ffBHffB]33Bu33B�33B���B�ffB�  B���B�  B���B���Bҙ�Bܙ�B�ffB�B���C� C�fCffC  C� C�fC �fC%��C*��C/33C4�3C9��C>  CB��CH33CQ��C[ffCf�Co�3C{33C�� C�L�C�Y�C��3C�  C��C��C�s3C�L�C��3C��C�  C�ٚC��3Cǳ3C�� C��3C֙�C��fC�fC�33C�&fC�@ C�ffC�ffD3D�D��D��D  D��D�3D$ٚD*  D/33D4  D9fD>�DC  DH,�DM3DR,�DW&fD\33D`��Df  Dk&fDp�Du3Dz  D�I�D�y�D�� D��D�Y�D��fD���D��D�VfD���D��3D�	�D�P DԐ D��3D�fD�6fD홚D��D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?   @�  A33As33A���A�ffA�  B
��B"  B4  BH  B\��Bt��B�  B���B�33B���B�ffB���B���Bə�B�ffB�ffB�33B�ffB���CffC��CL�C�fCffC��C ��C%� C*�3C/�C4��C9�3C=�fCB�3CH�CQ�3C[L�Cf  Co��C{�C�s3C�@ C�L�C��fC��3C��C�  C�ffC�@ C��fC��C��3C���C��fCǦfC̳3C��fC֌�C�ٚC���C�&fC��C�33C�Y�C�Y�D�DfD�3D�fD��D�3D��D$�3D*�D/,�D3��D9  D>3DC�DH&fDM�DR&fDW  D\,�D`�fDf�Dk  DpfDu�Dz�D�FfD�vfD���D�	�D�VfD��3D���D�fD�S3D���D�� D�fD�L�DԌ�D�� D�3D�33D�fD�D�ɚ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A��
AҬAҝ�A҉7A�dZA�\)A�VA�K�A�oA�VA͇+A�A�A�ƨA���A�z�A���A�7LA�/A�A���A���A�C�A�z�A�l�A��uA�5?A���A���A�|�A�M�A�(�A�`BA���A�VA�\)A�C�A�%A�
=A��A�O�A�/A���A��A�ȴA�(�A|�AxA�As�Am�wAahsAZȴAQ�#AK�AC�hA>��A7�A2n�A.��A&�9A"JA��A(�A�#AE�A;dA��AS�@�@�;d@��H@�E�@أ�@�  @ɡ�@�S�@�"�@�M�@�@�@�?}@�hs@���@�J@�C�@���@���@�+@�j@��P@�r�@��^@���@�5?@��F@�X@}?}@r��@j�H@d9X@["�@Tj@J�!@C��@=/@:�\@5��@-�@*-@%`B@��@1@Q�@"�@�w@��21111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A��
AҬAҝ�A҉7A�dZA�\)A�VA�K�A�oA�VA͇+A�A�A�ƨA���A�z�A���A�7LA�/A�A���A���A�C�A�z�A�l�A��uA�5?A���A���A�|�A�M�A�(�A�`BA���A�VA�\)A�C�A�%A�
=A��A�O�A�/A���A��A�ȴA�(�A|�AxA�As�Am�wAahsAZȴAQ�#AK�AC�hA>��A7�A2n�A.��A&�9A"JA��A(�A�#AE�A;dA��AS�@�@�;d@��H@�E�@أ�@�  @ɡ�@�S�@�"�@�M�@�@�@�?}@�hs@���@�J@�C�@���@���@�+@�j@��P@�r�@��^@���@�5?@��F@�X@}?}@r��@j�H@d9X@["�@Tj@J�!@C��@=/@:�\@5��@-�@*-@%`B@��@1@Q�@"�@�w@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B\)BhsBhsBhsBhsBgmBhsBiyBl�Bq�Bv�B�JBaHBT�BO�BZBe`BYBbNB}�B�B�B�\B��B��B�hB~�BiyB`BBS�BA�B\B�HB�B�BB��B��Bx�B�`B�^B��B�JBF�B	7B
��B
E�B
�B	��B	��B	�B	^5B	9XB	�B	  B�sB��B�}B�B��B�PBt�Bl�BffB^5B`BB_;BbNB`BBVB\)Bp�Bt�B�7B�{B��B�B��B�`B	PB	uB	�B	O�B	l�B	�DB	��B	��B	��B	�FB	ȴB	��B	��B	�;B	�HB	�`B	�B	��B	��B	��B
	7B
{B
�B
)�B
0!B
49B
?}B
D�B
F�B
W
B
\)B
aHB
e`B
jB
m�B
q�B
r�41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B\)Bh�BhsBh�Bh�BgmBh�Bi�Bl�Bq�Bv�B�JBabBT�BO�BZBezBYBbhB~B�-B�B�\B��B��B��BBi�B`BBS�BA�BvB�HB�B�BB��B��Bx�B�zB�^B��B�JBF�B	7B
��B
E�B
�B	��B	�B	�-B	^5B	9rB	�B	 B�B��B��B�5B��B�jBt�Bl�Bf�B^jB`\B_VBbhB`vBVB\]Bp�Bt�B�RB��B��B�B�B�zB	jB	�B	�B	O�B	l�B	�^B	��B	��B	�B	�`B	��B	� B	� B	�VB	�bB	�`B	�B	��B	��B	�B
	RB
{B
�B
)�B
0!B
4TB
?�B
D�B
F�B
W
B
\CB
aHB
e`B
j�B
m�B
q�B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.1(dbar); PO2=0.0(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201410221907062014102219070620141022190706201608161401522016081614015220160816140152JA  ARFMdecpP7_h                                                                20130802010021  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20130802010041  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20130802010042  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20130802010046  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20130802010046  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20130802010047  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20130802010048  CF  PSAL            ?��?��?�                  JA  ARGQpump1.0                                                                 20130802010048  CF  TEMP            ?��?��?�                  JA  ARUP                                                                        20130802012220                      G�O�G�O�G�O�                JA  ARFMdecpP7_h                                                                20130804185712  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20130804190503  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20130804190504  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20130804190509  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20130804190509  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20130804190509  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20130804190510  CF  PSAL            ?��?��?�                  JA  ARGQpump1.0                                                                 20130804190510  CF  TEMP            ?��?��?�                  JA  ARUP                                                                        20130804191638                      G�O�G�O�G�O�                JM  ARFMjmbcP7_h                                                                20131118021256  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20131118021445  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20131118021447  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20131118021451  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20131118021452  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20131118021452  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20131118021452  CF  PSAL            ?��?��?�                  JA  ARGQpump1.0                                                                 20131118021452  CF  TEMP            ?��?��?�                  JA  ARUP                                                                        20131118042217                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150424013744                      G�O�G�O�G�O�                JA  ARUP                                                                        20150503004513                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20130827000000  CF  PSAL_ADJUSTED_QC?��?��G�O�                JM  ARSQJMQC2.0                                                                 20130827000000  CF  TEMP_ADJUSTED_QC?��?��G�O�                JM  ARCAJMQC2.0                                                                 20141022100706  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20141022100706  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2014V1                                                       20160816050152  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20161129234515                      G�O�G�O�G�O�                