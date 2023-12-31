CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:17Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190617  20181005190617  5904953 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6432                            2B  A   APEX                            7467                            062512                          846 @ס/e�O�1   @ס/���@3�Ƨ�c�XbM�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @�33@�  @���AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  A�33B  B  B  B   B(  B0  B7��B?��BH  BPffBXffB`  Bh  Bp  Bx  B�  B�  B���B�  B�  B���B�  B���B���B���B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B���C   C  C  C  C  C
  C�C�C  C  C  C  C�fC  C  C  C�fC!�fC#�fC%�fC(  C*  C+�fC-�fC/�fC2  C4  C6  C8  C:  C;�fC=�fC@  CB  CC�fCF  CH  CJ  CL  CN  CO�fCR  CT�CV  CX  CZ�C\�C^�C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn  Co�fCr  Ct  Cv  Cx  Cz  C|�C~  C��C��C�  C��3C��C�  C�  C�  C�  C��C��C��3C��3C�  C��C��C��C��C��3C��fC��C�  C��3C��C��3C��C��3C��C��3C�  C�  C�  C��3C�  C��C��3C�  C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��3C�  C��C��3C�  C�  C��C�  C��C��3C�  C��3C��C��3C�  C��C��C��3C��3C��C�  C�  C��C��3C��C��3C�  C�  C��3C��C�  C��fC�  C��C��C�  C��fC�  C�  C��C��C�  C��fC��3C��3C��fC��3C�  C��C��C��C�  C��fC��fC��fC��fC�  C��C�  C�  C��C�  C�  C��C�  C�  C�  C�  C��C��C��C�  C��3C�  C�  C�  D   D �fDfD� D  D� D  D�fDfD� D��D�fD  Dy�D  Dy�DfD� D	fD	�fD
fD
� D
��D� DfD� D��D� DfD� DfD� D  D� D  D� D  D� D  D� D  D� DfD� D��D�fD  D�fD  D�fD  D� D��Dy�D  D� D��Dy�D  D�fDfD�fDfD�fD fD �fD!�D!� D!��D"y�D"�3D#y�D$  D$��D%�D%��D&  D&s3D&��D'y�D'��D(� D)fD)� D*  D*�fD+  D+y�D,  D,y�D,��D-� D.  D.�fD/fD/�fD0fD0��D1�D1� D1��D2� D3fD3��D4  D4s3D4�3D5s3D5��D6y�D7  D7�fD8  D8� D9fD9��D:fD:�fD;fD;�fD<fD<��D=fD=��D>fD>� D>��D?� D@fD@� D@��DAy�DA�3DBs3DB��DCy�DC��DDy�DD��DEs3DE��DF� DGfDG�fDHfDH� DI  DI�fDJfDJ� DKfDK��DL  DLy�DMfDM�fDNfDN� DN��DO� DP�DP�fDQ  DQy�DR  DR�fDSfDSy�DT  DT� DU  DUy�DU�3DVy�DW  DW� DW��DXy�DY  DY��DZ�DZ�fD[  D[�fD\  D\�fD]fD]� D^fD^�fD^��D_y�D_��D`y�Da  Da� Da��Dby�Dc  Dc��Dd  Ddy�De  De� De��Dfy�Df��Dg� DhfDh�fDi�Di� Di��Dj� DkfDky�Dl  Dl�fDmfDm�fDnfDn� Dn��Do� Dp  Dp� Dq  Dqy�Dq��Dr�fDs  Ds� Dt  Dt� Du  Duy�Dv  Dv�fDwfDwy�Dw��Dyz=D�)H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�33@�  AffA"ffAD  Ad  A�  A�  A�  A�  A�  A�  A�  A�  B ��B	  B  B  B!  B)  B1  B8��B@��BI  BQffBYffBa  Bi  Bq  By  B�� B�� B�L�B�� B�� B�L�B�� B�L�B�L�B�L�B�� B�� B�� B�� B�� B��3B�� BĀ BȀ B̀ BЀ BԀ B؀ B܀ B�L�B� B� B� B�� B� B�� B�L�C @ C@ C@ C@ C@ C
@ CY�CY�C@ C@ C@ C@ C&fC@ C@ C@ C &fC"&fC$&fC&&fC(@ C*@ C,&fC.&fC0&fC2@ C4@ C6@ C8@ C:@ C<&fC>&fC@@ CB@ CD&fCF@ CH@ CJ@ CL@ CN@ CP&fCR@ CTY�CV@ CX@ CZY�C\Y�C^Y�C`@ Cb@ Cd@ Cf@ Ch@ Cj@ ClY�Cn@ Cp&fCr@ Ct@ Cv@ Cx@ Cz@ C|Y�C~@ C�,�C�9�C�  C�3C�,�C�  C�  C�  C�  C�,�C�,�C�3C�3C�  C�,�C�,�C�9�C�9�C�3C�fC�,�C�  C�3C�9�C�3C�,�C�3C�,�C�3C�  C�  C�  C�3C�  C�,�C�3C�  C�3C�  C�,�C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�3C�  C�,�C�3C�  C�  C�,�C�  C�,�C�3C�  C�3C�9�C�3C�  C�9�C�9�C�3C�3C�9�C�  C�  C�9�C�3C�,�C�3C�  C�  C�3C�9�C�  C�fC�  C�9�C�9�C�  C�fC�  C�  C�,�C�9�C�  C�fC�3C�3C�fC�3C�  C�9�C�9�C�9�C�  C�fC�fC�fC�fC�  C�9�C�  C�  C�,�C�  C�  C�,�C�  C�  C�  C�  C�,�C�,�C�,�C�  C�3C�  C�  C�  D  D �fDfD� D D� D D�fDfD� D	�D�fD D��D D��DfD� D	fD	�fD
fD
� D	�D� DfD� D	�D� DfD� DfD� D D� D D� D D� D D� D D� DfD� D	�D�fD D�fD D�fD D� D	�D��D D� D	�D��D D�fDfD�fDfD�fD fD �fD!�D!� D"	�D"��D#3D#��D$ D$��D%�D%��D& D&�3D'	�D'��D(	�D(� D)fD)� D* D*�fD+ D+��D, D,��D-	�D-� D. D.�fD/fD/�fD0fD0��D1�D1� D2	�D2� D3fD3��D4 D4�3D53D5�3D6	�D6��D7 D7�fD8 D8� D9fD9��D:fD:�fD;fD;�fD<fD<��D=fD=��D>fD>� D?	�D?� D@fD@� DA	�DA��DB3DB�3DC	�DC��DD	�DD��DE	�DE�3DF	�DF� DGfDG�fDHfDH� DI DI�fDJfDJ� DKfDK��DL DL��DMfDM�fDNfDN� DO	�DO� DP�DP�fDQ DQ��DR DR�fDSfDS��DT DT� DU DU��DV3DV��DW DW� DX	�DX��DY DY��DZ�DZ�fD[ D[�fD\ D\�fD]fD]� D^fD^�fD_	�D_��D`	�D`��Da Da� Db	�Db��Dc Dc��Dd Dd��De De� Df	�Df��Dg	�Dg� DhfDh�fDi�Di� Dj	�Dj� DkfDk��Dl Dl�fDmfDm�fDnfDn� Do	�Do� Dp Dp� Dq Dq��Dr	�Dr�fDs Ds� Dt Dt� Du Du��Dv Dv�fDwfDw��Dw��Dy�=D�1H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A���A���A�ƨAə�A���AǗ�A�&�A�VA��A��A�ƨA�5?A��A��;A��`A��A��/A�AŲ-Aŕ�A�r�A�\)A�Q�A�G�A�A�A�-A��A��A��A��A��A�%A���A��;A��
A�
=A�p�A�ffA�=qA�bA��A���Aę�A�bA�;dA¼jAA�A�A��A���A�A��A��PA�\)A��A���A�O�A��uA��A��A���A�9XA�O�A��-A�S�A�$�A��^A�^5A��TA�A�A��A���A���A�bA�ƨA� �A��A�^5A�(�A��A�K�A�
=A�dZA�E�A�ƨA��A���A��jA���A�ȴA�ZA���A�XA��A�hsA�ZA���A���A�?}Az�Ax=qAu�An�Ak\)Agx�Ae�Ab$�A^�A\1'A[;dAY��AYO�AX��AXM�AW�PAV^5AR�APQ�AO��AOl�ANn�ALbNAJ��AH~�AG�#AF�AC��AA�^AA�A?��A>ffA<v�A;�PA:1'A8E�A7A6~�A5�
A4VA2r�A1p�A0v�A.��A-�A-G�A-7LA,�A*��A)G�A((�A'hsA'
=A&ffA"��A!`BA jA�-AVAO�A��A\)A�A;dAv�A��AK�AO�A�#A/A��A��A��A
�A
~�A	�A�A  A�yA��A&�AĜAjA5?A�A�hAȴA;d@��@�-@� �@��D@�A�@��@�Q�@�R@�j@��@�G�@�j@�I�@�w@�1'@���@�@���@�K�@�7L@���@�?}@�r�@�r�@�9X@�M�@��u@ޟ�@�=q@�=q@�J@��@ܣ�@��`@���@�dZ@�@�E�@���@�Q�@�dZ@���@�=q@�"�@�/@�  @��H@�j@�  @�"�@�@�7L@��@���@��@�bN@��m@�;d@��@�5?@�@�7L@�Q�@���@�K�@���@�ff@�-@��^@��@�j@�  @��F@�l�@���@���@��T@�@���@�x�@�`B@�O�@��@��u@�Q�@� �@���@�+@�
=@���@�@��!@��@���@�hs@��@��@���@�Z@��w@�C�@�ȴ@���@�ff@�$�@���@��@���@���@�Z@���@�;d@�@�^5@���@��@�G�@�z�@�z�@��@�Q�@�  @�  @�  @�  @���@��F@�ȴ@��@�{@�@��@��T@���@��@��@�/@�Ĝ@��u@�z�@�A�@��@��@�33@�dZ@�l�@�dZ@�C�@�@�^5@��T@�X@�X@�O�@�X@���@��@���@��7@��@���@��9@���@��D@�z�@�bN@�Q�@�9X@� �@�b@�  @���@��@��;@���@�ƨ@��@���@�K�@�33@�"�@�
=@��@���@�n�@�@���@���@�`B@�?}@�V@��@�Ĝ@��@�b@��@���@�=q@��h@��/@���@�"�@��H@���@�v�@�^5@�M�@�=q@�$�@�J@��@���@�O�@���@�b@���@��w@�|�@�+@��!@�5?@��#@��h@�?}@�9X@���@���@�t�@�;d@��+@�n�@�M�@���@��#@���@��-@�X@�`B@�hs@�`B@�?}@��`@���@�z�@�Z@�9X@�1@���@�t�@���@�~�@�ff@��@��@�V@���@�A�@���@��m@��m@���@�9X@�Z@�I�@��;@�t�@�l�@�33@��@��R@�E�@��T@���@�x�@�O�@���@���@�Ĝ@��j@��j@��@���@��u@�9X@� �@���@��@�l�@�o@��H@���@�ȴ@��\@�n�@�-@��-@�G�@��@�%@��u@� �@�  @�@��@~�!@i�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A���A���A���A���A�ƨAə�A���AǗ�A�&�A�VA��A��A�ƨA�5?A��A��;A��`A��A��/A�AŲ-Aŕ�A�r�A�\)A�Q�A�G�A�A�A�-A��A��A��A��A��A�%A���A��;A��
A�
=A�p�A�ffA�=qA�bA��A���Aę�A�bA�;dA¼jAA�A�A��A���A�A��A��PA�\)A��A���A�O�A��uA��A��A���A�9XA�O�A��-A�S�A�$�A��^A�^5A��TA�A�A��A���A���A�bA�ƨA� �A��A�^5A�(�A��A�K�A�
=A�dZA�E�A�ƨA��A���A��jA���A�ȴA�ZA���A�XA��A�hsA�ZA���A���A�?}Az�Ax=qAu�An�Ak\)Agx�Ae�Ab$�A^�A\1'A[;dAY��AYO�AX��AXM�AW�PAV^5AR�APQ�AO��AOl�ANn�ALbNAJ��AH~�AG�#AF�AC��AA�^AA�A?��A>ffA<v�A;�PA:1'A8E�A7A6~�A5�
A4VA2r�A1p�A0v�A.��A-�A-G�A-7LA,�A*��A)G�A((�A'hsA'
=A&ffA"��A!`BA jA�-AVAO�A��A\)A�A;dAv�A��AK�AO�A�#A/A��A��A��A
�A
~�A	�A�A  A�yA��A&�AĜAjA5?A�A�hAȴA;d@��@�-@� �@��D@�A�@��@�Q�@�R@�j@��@�G�@�j@�I�@�w@�1'@���@�@���@�K�@�7L@���@�?}@�r�@�r�@�9X@�M�@��u@ޟ�@�=q@�=q@�J@��@ܣ�@��`@���@�dZ@�@�E�@���@�Q�@�dZ@���@�=q@�"�@�/@�  @��H@�j@�  @�"�@�@�7L@��@���@��@�bN@��m@�;d@��@�5?@�@�7L@�Q�@���@�K�@���@�ff@�-@��^@��@�j@�  @��F@�l�@���@���@��T@�@���@�x�@�`B@�O�@��@��u@�Q�@� �@���@�+@�
=@���@�@��!@��@���@�hs@��@��@���@�Z@��w@�C�@�ȴ@���@�ff@�$�@���@��@���@���@�Z@���@�;d@�@�^5@���@��@�G�@�z�@�z�@��@�Q�@�  @�  @�  @�  @���@��F@�ȴ@��@�{@�@��@��T@���@��@��@�/@�Ĝ@��u@�z�@�A�@��@��@�33@�dZ@�l�@�dZ@�C�@�@�^5@��T@�X@�X@�O�@�X@���@��@���@��7@��@���@��9@���@��D@�z�@�bN@�Q�@�9X@� �@�b@�  @���@��@��;@���@�ƨ@��@���@�K�@�33@�"�@�
=@��@���@�n�@�@���@���@�`B@�?}@�V@��@�Ĝ@��@�b@��@���@�=q@��h@��/@���@�"�@��H@���@�v�@�^5@�M�@�=q@�$�@�J@��@���@�O�@���@�b@���@��w@�|�@�+@��!@�5?@��#@��h@�?}@�9X@���@���@�t�@�;d@��+@�n�@�M�@���@��#@���@��-@�X@�`B@�hs@�`B@�?}@��`@���@�z�@�Z@�9X@�1@���@�t�@���@�~�@�ff@��@��@�V@���@�A�@���@��m@��m@���@�9X@�Z@�I�@��;@�t�@�l�@�33@��@��R@�E�@��T@���@�x�@�O�@���@���@�Ĝ@��j@��j@��@���@��u@�9X@� �@���@��@�l�@�o@��H@���@�ȴ@��\@�n�@�-@��-@�G�@��@�%@��u@� �@�  @�@��@~�!@i�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B+B1B1B1BDBhB�B&�B>wBdZBp�Bp�Bo�Br�Bv�B�B�+B�7B�JB�\B�PB�DB�=B�+B�B�B� B� B� B~�B}�B}�B}�B}�B}�B�B�7B��B��B��B�mB�B�B��B��B��BBBPBoB�B�B �B(�B33BT�B_;BaHBffBk�Bs�B{�B~�B�%B� B{�B�JB�bB�?B�9B�!B�B��B��B��B�B� Bx�Bo�Be`B^5BS�BJ�BF�BD�B=qB/B"�B�BuBDB��B��B��B��Br�BXB9XBuB
ŢB
q�B
/B
oB	�fB	��B	�-B	�VB	s�B	^5B	L�B	6FB	%�B	�B	�B	hB	\B	PB	DB	
=B	1B	B��B��B��B�B�ZB�;B�#B�B��BB��B��B�}B�wB�}B�jB�jB�qB�LB�?B�3B�-B�!B�B��B��B��B��B��B��B�B�B�-B�^B�XB�3B��B��B��B��B��B��B��B��B�'B��B��B��B�B�^B�dB��B��B��B��B��B��B��B��B�{B�uB��B��B��B��B��B��B��B�B�B�?B�RB�?B�-B�dB�dB�FB�!B�B�B�'B�dB�wB�}BȴB��B�B��B�B�mB�
B��BȴBȴBɺBǮBǮB��B�
B�#B�5B�;B�HB�B��B��B��B��B��B�B�B�/B�;B�)B�5B�BB�HB�ZB�ZB�mB�B�B�B�B�B��B��B��B��B��B��B	  B	B	B	%B	DB	VB	VB	hB	�B	�B	�B	�B	�B	#�B	(�B	)�B	)�B	+B	-B	-B	.B	/B	2-B	49B	5?B	8RB	<jB	=qB	=qB	=qB	?}B	D�B	E�B	H�B	J�B	O�B	O�B	R�B	XB	[#B	_;B	`BB	aHB	bNB	ffB	k�B	o�B	p�B	q�B	r�B	u�B	x�B	z�B	{�B	z�B	{�B	}�B	}�B	~�B	�B	�7B	�7B	�=B	�=B	�=B	�=B	�=B	�JB	�\B	�hB	�hB	�hB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�?B	�LB	�RB	�XB	�^B	�dB	�jB	�jB	�jB	�qB	�qB	�wB	�wB	�wB	�wB	�}B	�}B	��B	��B	ÖB	ÖB	ĜB	ĜB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�/B	�;B	�5B	�;B	�;B	�NB	�NB	�TB	�TB	�TB	�TB	�`B	�fB	�`B	�`B	�ZB	�`B	�ZB	�ZB	�`B	�fB	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
+B
1B
1B
1B
1B
	7B
	7B

=B
DB
DB
DB
JB
JB
JB
JB
JB
PB
\B
\B
bB
bB
bB
�B
�B
$�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B+B1B1B1BDBhB�B&�B>wBdZBp�Bp�Bo�Br�Bv�B�B�+B�7B�JB�\B�PB�DB�=B�+B�B�B� B� B� B~�B}�B}�B}�B}�B}�B�B�7B��B��B��B�mB�B�B��B��B��BBBPBoB�B�B �B(�B33BT�B_;BaHBffBk�Bs�B{�B~�B�%B� B{�B�JB�bB�?B�9B�!B�B��B��B��B�B� Bx�Bo�Be`B^5BS�BJ�BF�BD�B=qB/B"�B�BuBDB��B��B��B��Br�BXB9XBuB
ŢB
q�B
/B
oB	�fB	��B	�-B	�VB	s�B	^5B	L�B	6FB	%�B	�B	�B	hB	\B	PB	DB	
=B	1B	B��B��B��B�B�ZB�;B�#B�B��BB��B��B�}B�wB�}B�jB�jB�qB�LB�?B�3B�-B�!B�B��B��B��B��B��B��B�B�B�-B�^B�XB�3B��B��B��B��B��B��B��B��B�'B��B��B��B�B�^B�dB��B��B��B��B��B��B��B��B�{B�uB��B��B��B��B��B��B��B�B�B�?B�RB�?B�-B�dB�dB�FB�!B�B�B�'B�dB�wB�}BȴB��B�B��B�B�mB�
B��BȴBȴBɺBǮBǮB��B�
B�#B�5B�;B�HB�B��B��B��B��B��B�B�B�/B�;B�)B�5B�BB�HB�ZB�ZB�mB�B�B�B�B�B��B��B��B��B��B��B	  B	B	B	%B	DB	VB	VB	hB	�B	�B	�B	�B	�B	#�B	(�B	)�B	)�B	+B	-B	-B	.B	/B	2-B	49B	5?B	8RB	<jB	=qB	=qB	=qB	?}B	D�B	E�B	H�B	J�B	O�B	O�B	R�B	XB	[#B	_;B	`BB	aHB	bNB	ffB	k�B	o�B	p�B	q�B	r�B	u�B	x�B	z�B	{�B	z�B	{�B	}�B	}�B	~�B	�B	�7B	�7B	�=B	�=B	�=B	�=B	�=B	�JB	�\B	�hB	�hB	�hB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�?B	�LB	�RB	�XB	�^B	�dB	�jB	�jB	�jB	�qB	�qB	�wB	�wB	�wB	�wB	�}B	�}B	��B	��B	ÖB	ÖB	ĜB	ĜB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�/B	�;B	�5B	�;B	�;B	�NB	�NB	�TB	�TB	�TB	�TB	�`B	�fB	�`B	�`B	�ZB	�`B	�ZB	�ZB	�`B	�fB	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
+B
1B
1B
1B
1B
	7B
	7B

=B
DB
DB
DB
JB
JB
JB
JB
JB
PB
\B
\B
bB
bB
bB
�B
�B
$�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.25 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190617                              AO  ARCAADJP                                                                    20181005190617    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190617  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190617  QCF$                G�O�G�O�G�O�8000            