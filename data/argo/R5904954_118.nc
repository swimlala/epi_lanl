CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:15Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z          9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       BD   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  IT   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       K   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       R(   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Y8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       Z�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  b   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       c�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       j�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       s�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  z�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       |�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20181005191715  20181005191715  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               vA   AO  6557                            2B  A   APEX                            7468                            062512                          846 @����Y��1   @���ffy@4�z�G��d_;dZ�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      vA   A   B   @�  @�  @���A   A@  A`  A�  A�  A���A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`��Bg33Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�33B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  C   C  C  C  C  C
  C  C  C  C�C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.�C0  C2  C4  C6  C7�fC:  C<  C=�fC@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CU�fCX  CZ�C\  C^  C_�fCa�fCc�fCe�fCh  Cj  Cl  Cm�fCo�fCr  Ct  Cv  Cx  Cz  C|  C~  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C��3C��3C��C�  C��C��C�  C�  C�  C��3C�  C��C�  C�  C��C�  C�  C��3C��3C��3C��3C��3C��C��3C�  C�  C��C�  C�  C��3C��C��C�  C�  C�  C�  C�  C��C��3C�  C�  C�  C��3C��C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��C�  C��C�  C��fC�  C�  C��3C�  C��C��3C�  C��3D   D � D  D� DfD�fD��Dy�DfDy�DfD� D��Dy�D��D� DfD� D�3D	y�D
fD
� D
��D�fD  D� D��Dy�DfDy�D��Dy�D��Dy�D  D� D��D� D  D� D  D� D��D� DfD�fD  D�fDfDy�D  D� DfDy�DfDy�D��Dy�D��D�fD  Ds3D��D�fD   D �fD ��D!y�D"  D"� D.�fD/fD/�fD0  D0� D0�3D1y�D1��D2y�D2��D3� D4  D4� D4��D5y�D6  D6� D6��D7s3D8  D8�fD9  D9� D:fD:�fD;fD;�fD<  D<�fD=  D=� D>fD>� D>��D?y�D?��D@� DAfDA� DA��DBy�DC  DC� DD  DD�fDE  DE� DF  DF� DG  DG� DG��DH� DH��DI� DJ  DJ�fDKfDK� DL  DL� DMfDM�fDN  DN� DO  DO� DO��DP� DQfDQ� DQ��DR�fDSfDS� DT  DT� DT��DU� DV  DV�fDW  DW� DX  DX� DX��DY� DZ  DZ� D[fD[�fD\fD\� D\��D]y�D^  D^�fD_fD_� D_��D`� DafDa�fDb�Db� Db��Dc� DdfDd� Dd��Dey�De��Dfy�Dg  Dg�fDhfDh�fDifDi�fDj  Djy�Dj��Dk� DlfDl� DmfDm�fDn  Dn� Dn��Do� DpfDp� Dq  Dq�fDr  Dr� DsfDs��DtfDt�fDu  Du�fDvfDv�fDw  Dwy�Dw��Dy��D�2�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�  @�  AffA$  AD  Ad  A�  A�  A���A�  A�  A�  A�  A�  B  B	  B  B  B!  B)ffB1  B9  BA  BI  BQ  BY  Ba��Bh33Bq  By  B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�L�B�� B�� B��3B�� B�� B�� BĀ BȀ B�L�BЀ BԀ B؀ B܀ B�� B� B� B� B�L�B�L�B�� B�� C @ C@ C@ C@ C@ C
@ C@ C@ C@ CY�C@ C@ C@ C@ C@ C@ C @ C"@ C$@ C&@ C(@ C*@ C,@ C.Y�C0@ C2@ C4@ C6@ C8&fC:@ C<@ C>&fC@@ CB@ CD@ CF@ CH@ CJ@ CL@ CN@ CP@ CR@ CT@ CV&fCX@ CZY�C\@ C^@ C`&fCb&fCd&fCf&fCh@ Cj@ Cl@ Cn&fCp&fCr@ Ct@ Cv@ Cx@ Cz@ C|@ C~@ C�  C�3C�  C�  C�  C�  C�  C�  C�  C�,�C�3C�3C�,�C�  C�,�C�,�C�  C�  C�  C�3C�  C�9�C�  C�  C�,�C�  C�  C�3C�3C�3C�3C�3C�,�C�3C�  C�  C�,�C�  C�  C�3C�,�C�,�C�  C�  C�  C�  C�  C�,�C�3C�  C�  C�  C�3C�,�C�  C�  C�  C�  C�  C�  C�,�C�,�C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�3C�,�C�  C�,�C�  C�fC�  C�  C�3C�  C�,�C�3C�  C�3D  D � D D� DfD�fD	�D��DfD��DfD� D	�D��D	�D� DfD� D	3D	��D
fD
� D	�D�fD D� D	�D��DfD��D	�D��D	�D��D D� D	�D� D D� D D� D	�D� DfD�fD D�fDfD��D D� DfD��DfD��D	�D��D	�D�fD D�3D	�D�fD  D �fD!	�D!��D" D"� D.�fD/fD/�fD0 D0� D13D1��D2	�D2��D3	�D3� D4 D4� D5	�D5��D6 D6� D7	�D7�3D8 D8�fD9 D9� D:fD:�fD;fD;�fD< D<�fD= D=� D>fD>� D?	�D?��D@	�D@� DAfDA� DB	�DB��DC DC� DD DD�fDE DE� DF DF� DG DG� DH	�DH� DI	�DI� DJ DJ�fDKfDK� DL DL� DMfDM�fDN DN� DO DO� DP	�DP� DQfDQ� DR	�DR�fDSfDS� DT DT� DU	�DU� DV DV�fDW DW� DX DX� DY	�DY� DZ DZ� D[fD[�fD\fD\� D]	�D]��D^ D^�fD_fD_� D`	�D`� DafDa�fDb�Db� Dc	�Dc� DdfDd� De	�De��Df	�Df��Dg Dg�fDhfDh�fDifDi�fDj Dj��Dk	�Dk� DlfDl� DmfDm�fDn Dn� Do	�Do� DpfDp� Dq Dq�fDr Dr� DsfDs��DtfDt�fDu Du�fDvfDv�fDw Dw��Dx	�Dy��D�:�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�dZA�\)A�\)A�bNA�ffA�hsA�~�A�|�A�|�A�~�A�|�A�x�A�p�A�l�A�n�A�p�A�t�A�|�A�t�A�\)A�33A���A۰!A��A��A��A���A�t�A��A���A�\)A�AƶFA�(�A�=qA���A�Q�A�A�/A��A��DA���A�`BA�"�A��RA��hA��A�dZA�|�A���A��TA��A���A�dZA�9XA�ƨA�~�A�oA�G�A��TA���A�?}A�ƨA���A�p�A���A���A��A�C�A��A���A��A��
A�{A���A�bNA�=qA�1A��HA��7A�-A��A�z�A��A���A��jA��jA�p�A� �A�ffA�t�A���A�M�A��wA�VA��hA�ZA~�DA|��Ay�mAyS�Aw�mAv�\AtI�As��AsK�ArI�Ao�-Al-Ag�Af=qAb  A`(�A]�TA[x�AZ5?AWl�AT��AS�ARVAQS�AP=qAOVAN1'AL�!AJbAIXAHffAFA�AC�;AA+A>��A>1A<5?A;x�A;�A:�jA9�TA7O�A6��A6z�A6bNA6=qA�;AhsA�RA�PA�A�AdZAĜA��A/A�`A�!AA
=A��AVA�!AffAVA�9A �A�AS�A�HA�
AG�A�DA��A��A�`A��A/A��AffAx�A5?A V@��@�$�@�7L@�"�@���@���@���@�Ĝ@�  @��@�;d@�\)@�\)@��@�33@�@�M�@�/@���@�Z@�ȴ@��
@�
=@���@���@݉7@���@�5?@׾w@�$�@�@�%@љ�@��/@���@�dZ@ͩ�@��m@�t�@˥�@ʰ!@��#@ģ�@§�@��9@�ff@���@�Ĝ@���@���@��y@��^@��u@�Z@���@�l�@���@�$�@���@�-@�dZ@��@�%@�~�@�p�@��@��9@��@��h@���@���@�hs@��9@��h@��#@��`@�l�@�S�@�"�@��!@�V@�E�@���@�K�@�
=@���@�-@��y@�o@��@��+@��+@���@���@�~�@���@���@��`@��T@�-@���@�%@���@�r�@���@���@��y@�dZ@�9X@�1@��;@���@��
@��F@���@�+@�@��\@�@�v�@���@�Ĝ@�%@��j@�1'@���@�j@�A�@�|�@�33@�o@��y@��@��H@��-@��7@�hs@�x�@�p�@��7@���@�G�@���@��`@��`@��@�p�@��h@��h@�V@���@��u@�dZ@�o@�M�@�&�@���@��@��D@��9@��@���@�G�@���@��@���@��@�Ĝ@��9@��u@�(�@��@��@�o@��@��H@��!@�v�@�v�@�n�@�V@�@�7L@�7L@�V@��/@���@��D@�(�@��@��@���@���@��\@�^5@�E�@���@��^@���@��@�p�@��@��`@���@�Ĝ@��9@��D@�Q�@�1'@�  @�ƨ@���@�t�@�C�@��@���@���@�~�@�V@�$�@��@�J@��^@�p�@��@���@���@�r�@��m@�33@���@�=q@��T@���@��#@���@�O�@��u@�I�@�A�@�I�@��@�1@��
@��F@�l�@��@�ȴ@�E�@��T@���@�V@�&�@�hs@�G�@���@��@�|�@�;d@��!@�{@�@�X@�Ĝ@�j@�(�@�1@|�@
=@|�@m�@\��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�dZA�\)A�\)A�bNA�ffA�hsA�~�A�|�A�|�A�~�A�|�A�x�A�p�A�l�A�n�A�p�A�t�A�|�A�t�A�\)A�33A���A۰!A��A��A��A���A�t�A��A���A�\)A�AƶFA�(�A�=qA���A�Q�A�A�/A��A��DA���A�`BA�"�A��RA��hA��A�dZA�|�A���A��TA��A���A�dZA�9XA�ƨA�~�A�oA�G�A��TA���A�?}A�ƨA���A�p�A���A���A��A�C�A��A���A��A��
A�{A���A�bNA�=qA�1A��HA��7A�-A��A�z�A��A���A��jA��jA�p�A� �A�ffA�t�A���A�M�A��wA�VA��hA�ZA~�DA|��Ay�mAyS�Aw�mAv�\AtI�As��AsK�ArI�Ao�-Al-Ag�Af=qAb  A`(�A]�TA[x�AZ5?AWl�AT��AS�ARVAQS�AP=qAOVAN1'AL�!AJbAIXAHffAFA�AC�;AA+A>��A>1A<5?A;x�A;�A:�jA9�TA7O�A6��A6z�A6bNA6=qA�;AhsA�RA�PA�A�AdZAĜA��A/A�`A�!AA
=A��AVA�!AffAVA�9A �A�AS�A�HA�
AG�A�DA��A��A�`A��A/A��AffAx�A5?A V@��@�$�@�7L@�"�@���@���@���@�Ĝ@�  @��@�;d@�\)@�\)@��@�33@�@�M�@�/@���@�Z@�ȴ@��
@�
=@���@���@݉7@���@�5?@׾w@�$�@�@�%@љ�@��/@���@�dZ@ͩ�@��m@�t�@˥�@ʰ!@��#@ģ�@§�@��9@�ff@���@�Ĝ@���@���@��y@��^@��u@�Z@���@�l�@���@�$�@���@�-@�dZ@��@�%@�~�@�p�@��@��9@��@��h@���@���@�hs@��9@��h@��#@��`@�l�@�S�@�"�@��!@�V@�E�@���@�K�@�
=@���@�-@��y@�o@��@��+@��+@���@���@�~�@���@���@��`@��T@�-@���@�%@���@�r�@���@���@��y@�dZ@�9X@�1@��;@���@��
@��F@���@�+@�@��\@�@�v�@���@�Ĝ@�%@��j@�1'@���@�j@�A�@�|�@�33@�o@��y@��@��H@��-@��7@�hs@�x�@�p�@��7@���@�G�@���@��`@��`@��@�p�@��h@��h@�V@���@��u@�dZ@�o@�M�@�&�@���@��@��D@��9@��@���@�G�@���@��@���@��@�Ĝ@��9@��u@�(�@��@��@�o@��@��H@��!@�v�@�v�@�n�@�V@�@�7L@�7L@�V@��/@���@��D@�(�@��@��@���@���@��\@�^5@�E�@���@��^@���@��@�p�@��@��`@���@�Ĝ@��9@��D@�Q�@�1'@�  @�ƨ@���@�t�@�C�@��@���@���@�~�@�V@�$�@��@�J@��^@�p�@��@���@���@�r�@��m@�33@���@�=q@��T@���@��#@���@�O�@��u@�I�@�A�@�I�@��@�1@��
@��F@�l�@��@�ȴ@�E�@��T@���@�V@�&�@�hs@�G�@���@��@�|�@�;d@��!@�{@�@�X@�Ĝ@�j@�(�@�1@|�@
=@|�@m�@\��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BYBYBYBYBYBYBXBXBXBXBXBXBXBYBYBYBZB]/B`BBaHBcTBe`Be`BdZB]/BE�BD�BF�BQ�BS�BVBW
BXBZB[#Bo�Bz�B|�B~�B�B�B�+B�=B�DB�\B�JB�JB�PB�=B�%B�B}�B|�Bx�Bl�BjBiyBffBaHB[#BQ�BM�BJ�BA�B2-B�BB��B�B�ZB��B�'B��Br�B]/B[#BXBVBR�BL�BD�B�B
��B
�B
�mB
�#B
��B
�B
��B
�\B
�B
y�B
q�B
iyB
_;B
T�B
C�B
49B
&�B
�B
hB
1B	��B	�B	�B	�yB	�ZB	�
B	�}B	��B	��B	�1B	z�B	k�B	]/B	R�B	C�B	33B	)�B	$�B	�B	�B	oB	JB	%B��B��B�B�sB�HB�B��B��BŢBÖB��B�}B�jB�LB�FB�?B�9B�3B�B�B�B�B�B�B�B� B�B�B�B�B�%B�B}�B{�Bz�By�By�Bx�By�Bt�Bk�B[#BS�BR�BYB_;Bx�B~�B� B|�B|�B{�B|�B~�Bw�Bp�Bn�Bm�Bk�BiyBhsBaHB\)BYBW
BR�BH�BI�B]/BhsBv�Bx�Bw�Bv�Bu�Bq�BjBffB_;BgmBaHB]/BbNBgmBgmBjBhsBe`BffBk�BffBbNB`BB`BBgmBiyBl�BbNB[#BW
BW
BYBW
BZBZB]/B`BBaHBaHB`BBffBv�B�B��B�oB�1B�B��B�B��B��B�-BĜB��B�B�)B�B�B��B��B��B��B	  B	  B��B��B�B�B�B��B��B��B	1B	PB	�B	�B	�B	"�B	$�B	+B	:^B	>wB	=qB	6FB	0!B	?}B	K�B	K�B	L�B	M�B	N�B	P�B	S�B	\)B	bNB	cTB	dZB	m�B	o�B	q�B	t�B	v�B	w�B	|�B	�B%�B	�\B	�uB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�LB	�^B	�jB	�wB	�wB	�wB	�}B	��B	�wB	�^B	�XB	�RB	�jB	B	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�BB	�HB	�NB	�NB	�NB	�ZB	�`B	�fB	�fB	�fB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
%z22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222242222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222BYBYBYBYBYBYBXBXBXBXBXBXBXBYBYBYBZB]/B`BBaHBcTBe`Be`BdZB]/BE�BD�BF�BQ�BS�BVBW
BXBZB[#Bo�Bz�B|�B~�B�B�B�+B�=B�DB�\B�JB�JB�PB�=B�%B�B}�B|�Bx�Bl�BjBiyBffBaHB[#BQ�BM�BJ�BA�B2-B�BB��B�B�ZB��B�'B��Br�B]/B[#BXBVBR�BL�BD�B�B
��B
�B
�mB
�#B
��B
�B
��B
�\B
�B
y�B
q�B
iyB
_;B
T�B
C�B
49B
&�B
�B
hB
1B	��B	�B	�B	�yB	�ZB	�
B	�}B	��B	��B	�1B	z�B	k�B	]/B	R�B	C�B	33B	)�B	$�B	�B	�B	oB	JB	%B��B��B�B�sB�HB�B��B��BŢBÖB��B�}B�jB�LB�FB�?B�9B�3B�B�B�B�B�B�B�B� B�B�B�B�B�%B�B}�B{�Bz�By�By�Bx�By�Bt�Bk�B[#BS�BR�BYB_;Bx�B~�B� B|�B|�B{�B|�B~�Bw�Bp�Bn�Bm�Bk�BiyBhsBaHB\)BYBW
BR�BH�BI�B]/BhsBv�Bx�Bw�Bv�Bu�Bq�BjBffB_;BgmBaHB]/BbNBgmBgmBjBhsBe`BffBk�BffBbNB`BB`BBgmBiyBl�BbNB[#BW
BW
BYBW
BZBZB]/B`BBaHBaHB`BBffBv�B�B��B�oB�1B�B��B�B��B��B�-BĜB��B�B�)B�B�B��B��B��B��B	  B	  B��B��B�B�B�B��B��B��B	1B	PB	�B	�B	�B	"�B	$�B	+B	:^B	>wB	=qB	6FB	0!B	?}B	K�B	K�B	L�B	M�B	N�B	P�B	S�B	\)B	bNB	cTB	dZB	m�B	o�B	q�B	t�B	v�B	w�B	|�B	�B%�B	�\B	�uB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�LB	�^B	�jB	�wB	�wB	�wB	�}B	��B	�wB	�^B	�XB	�RB	�jB	B	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�BB	�HB	�NB	�NB	�NB	�ZB	�`B	�fB	�fB	�fB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
%z22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222242222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.25 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191715                              AO  ARCAADJP                                                                    20181005191715    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191715  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191715  QCF$                G�O�G�O�G�O�8000            