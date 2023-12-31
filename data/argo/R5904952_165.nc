CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:42Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190542  20181005190542  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��b���1   @���$\@0�����c�I�^5?1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�33@�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A���A���B   B  B  B  B ffB(  B0  B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CU�fCX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cu�fCx  Cz  C|  C~  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C��3C��3C�  C��3C�  C�  C��C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3D   D � D  D� D  D� D  D� D  D� D  D�fDfD�fD  D� D  D�fD	  D	� D
  D
� D  D� D��Dy�D��Dy�D��D� D��Dy�D  D�fD  D� D  D� D  D� D��D� D  Dy�D  D�fD  D� DfD� D  D� D  D� DfD�fD  D� DfD�fD  Dy�D  D� D   D y�D!  D!� D"  D"� D#  D#�fD$  D$y�D%  D%� D&  D&�fD'  D'� D(fD(� D(��D)y�D*fD*�fD+  D+� D,fD,�fD-fD-�fD.  D.� D/  D/� D0fD0� D0��D1� D2  D2� D3  D3� D4  D4� D5fD5� D5��D6y�D7  D7�fD8  D8y�D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DBfDB� DC  DC� DD  DDy�DE  DE� DE��DF� DG  DGy�DG��DH� DIfDI� DJ  DJ� DK  DK� DL  DL� DL��DM� DNfDN� DO  DOy�DP  DP�fDQ  DQ� DR  DR� DS  DS� DS��DT� DU  DU� DVfDV� DV��DW� DX  DX� DY  DY� DY��DZ� D[  D[� D\  D\� D]  D]�fD^  D^y�D^��D_y�D`  D`� Da  Da� Da��Db� Dc  Dc� Dd  Dd�fDefDe� Df  Df� Dg  Dg� Dh  Dh� Di  Diy�Di��Dj� Dk  Dk� Dl  Dly�Dm  Dm� Dn  Dn� Dn��Doy�Dp  Dp� Dq  Dq�fDrfDr� Dr��Dsy�Dt  Dt� DufDu�fDvfDv� Dw  Dw� Dw� Dy�D�?\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�p�@�=qA�A%�AE�Ae�A�A��\A��\A��\A\Aҏ\A�\)A�\)BG�B	G�BG�BG�B!�B)G�B1G�B8�HBAG�BIG�BQG�BYG�BaG�BiG�BqG�ByG�B��
B���B���B���B��
B���B���B���B���B���B���B���B���B�
=B�p�B���B���Bģ�Bȣ�Ḅ�BУ�B��
Bأ�Bܣ�B��B��B��B��B��B���B���B���C Q�CQ�Ck�CQ�CQ�C
Q�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�C Q�C"Q�C$Q�C&Q�C(Q�C*Q�C,Q�C.Q�C0Q�C2Q�C4Q�C6Q�C8Q�C:k�C<Q�C>Q�C@Q�CBQ�CDQ�CFQ�CHQ�CJQ�CLQ�CNQ�CPQ�CRQ�CTQ�CV8RCXQ�CZQ�C\Q�C^Q�C`Q�CbQ�CdQ�CfQ�ChQ�CjQ�ClQ�CnQ�CpQ�CrQ�CtQ�Cv8RCxQ�CzQ�C|Q�C~Q�C�(�C�(�C�(�C�5�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�(�C�(�C�)C�(�C�(�C�(�C�)C�)C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�)C�(�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�5�C�5�C�(�C�(�C�(�C�(�C�(�C�)C�)C�(�C�)C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�)C�)C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�)C�)C�(�C�(�C�(�C�5�C�(�C�)C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�)D {D �{D{D�{D{D�{D{D�{D{D�{D{D��D�D��D{D�{D{D��D	{D	�{D
{D
�{D{D�{DD�DD�DD�{DD�D{D��D{D�{D{D�{D{D�{DD�{D{D�D{D��D{D�{D�D�{D{D�{D{D�{D�D��D{D�{D�D��D{D�D{D�{D {D �D!{D!�{D"{D"�{D#{D#��D${D$�D%{D%�{D&{D&��D'{D'�{D(�D(�{D)D)�D*�D*��D+{D+�{D,�D,��D-�D-��D.{D.�{D/{D/�{D0�D0�{D1D1�{D2{D2�{D3{D3�{D4{D4�{D5�D5�{D6D6�D7{D7��D8{D8�D9{D9�{D:{D:�{D;{D;�{D<{D<�{D={D=�{D>{D>�{D?{D?�{D@{D@�{DA{DA�{DB�DB�{DC{DC�{DD{DD�DE{DE�{DFDF�{DG{DG�DHDH�{DI�DI�{DJ{DJ�{DK{DK�{DL{DL�{DMDM�{DN�DN�{DO{DO�DP{DP��DQ{DQ�{DR{DR�{DS{DS�{DTDT�{DU{DU�{DV�DV�{DWDW�{DX{DX�{DY{DY�{DZDZ�{D[{D[�{D\{D\�{D]{D]��D^{D^�D_D_�D`{D`�{Da{Da�{DbDb�{Dc{Dc�{Dd{Dd��De�De�{Df{Df�{Dg{Dg�{Dh{Dh�{Di{Di�DjDj�{Dk{Dk�{Dl{Dl�Dm{Dm�{Dn{Dn�{DoDo�Dp{Dp�{Dq{Dq��Dr�Dr�{DsDs�Dt{Dt�{Du�Du��Dv�Dv�{Dw{Dw�{Dw�{Dy��D�I�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�7LA�"�Aѥ�A�
=A��A���A��A��TA���AуA�E�A�9XA�=qA�?}A�A�A�?}A�?}A�=qA�=qA�=qA�9XA�33A�(�A�JAа!AЏ\AЃA�l�A��A�A��/AϋDA�/A�oA�VA�1A���A��/AήA�jA��A͍PA�ƨA� �A��A�K�AǶFA�$�AċDA�l�Aå�A��A�v�A��7A�Q�A��7A���A�S�A��A��A���A��A��uA��7A��A�E�A�-A��\A��A��A�VA���A���A�{A��^A��A�~�A�;dA�I�A�VA��A��A��!A�
=A�z�A�"�A��yA�7LA���A��
A��#A���A��RA�A�oA��A��!A�oA�dZA���A�?}A���A�hsA�ȴA���A��/A��;A���A�?}A�-A{�FAyAx��Aw�PAt��ArĜAk�Ah��Ad-A^�`A]ƨA[�AU�hAQ��AM��AL1'AJ�`AIAG�AEx�ACAB�AA��A@��A=��A:A8��A8�A7��A69XA5%A3�-A3\)A3�A1�
A0�yA0bA/�7A.��A,��A*z�A'33A&$�A%��A$�9A"��A M�A�#A�A��A&�A�AC�A��A�A?}A�+A��AȴA�
A�`A�DAƨA�A�9AffA1'A��A9XA�-A�9AM�A  A�mA��A
ĜA
I�A	�
A	+A1'A��A��A1'Ap�A��A��Ax�A�Az�Ar�AE�A��A ��@�X@��7@���@�X@��/@���@�
=@��-@�t�@�^5@���@�X@�z�@�t�@�;d@���@�5?@�O�@�S�@�x�@�j@땁@�+@��H@�ff@�u@�1@��@�X@���@�ƨ@��@���@◍@�-@�9@�|�@��@��@�C�@��@؋D@�t�@�v�@թ�@�%@�(�@�K�@Ұ!@��#@Ь@Ϯ@Ο�@��T@͉7@͡�@͙�@�Ĝ@���@�C�@�"�@�
=@��H@ʏ\@ɉ7@�b@���@�ff@��`@�Ĝ@�Ĝ@���@��y@�n�@�~�@+@�@���@��H@�v�@��@���@�x�@�hs@�G�@��9@�I�@�1@��w@��@�n�@��-@��D@��@�K�@���@�E�@���@���@��j@�1'@���@��m@���@��@�5?@���@��h@�X@��j@�j@�Q�@�b@��F@�\)@��H@�$�@��@�X@���@��/@���@�I�@���@��;@��P@�33@��@��!@���@���@�E�@�@��7@�x�@�G�@���@�1'@��
@���@���@�"�@���@�v�@�J@���@��@���@���@�A�@��P@�C�@��\@�{@��@�{@��-@��@�1'@�(�@�  @��@��P@�l�@���@���@�?}@��@���@���@�z�@�Z@�Q�@� �@�  @�  @��@�  @��P@�K�@�dZ@�dZ@�\)@�o@��@��\@�-@�@���@��7@�7L@�V@���@��@�Z@�1@��@�t�@�+@��\@��+@��+@�M�@�@��^@�x�@�r�@�(�@��P@�;d@��@�ȴ@��\@��@���@�`B@�X@��@�z�@�b@�S�@���@��H@�ȴ@��!@���@���@�ff@�V@�@�X@�7L@�&�@���@�Ĝ@��u@�Q�@�  @��@��@�33@��@��!@�^5@���@���@��7@�X@�?}@�%@��/@��j@��u@�Q�@�1@���@���@��@��@��@��m@��m@��w@�t�@�33@�o@��@��R@���@��+@�5?@�p�@�7L@��@��`@���@�9X@�1@��
@���@�ƨ@��w@��@�+@���@���@�~�@�n�@�M�@�5?@�-@���@�|@z��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�7LA�"�Aѥ�A�
=A��A���A��A��TA���AуA�E�A�9XA�=qA�?}A�A�A�?}A�?}A�=qA�=qA�=qA�9XA�33A�(�A�JAа!AЏ\AЃA�l�A��A�A��/AϋDA�/A�oA�VA�1A���A��/AήA�jA��A͍PA�ƨA� �A��A�K�AǶFA�$�AċDA�l�Aå�A��A�v�A��7A�Q�A��7A���A�S�A��A��A���A��A��uA��7A��A�E�A�-A��\A��A��A�VA���A���A�{A��^A��A�~�A�;dA�I�A�VA��A��A��!A�
=A�z�A�"�A��yA�7LA���A��
A��#A���A��RA�A�oA��A��!A�oA�dZA���A�?}A���A�hsA�ȴA���A��/A��;A���A�?}A�-A{�FAyAx��Aw�PAt��ArĜAk�Ah��Ad-A^�`A]ƨA[�AU�hAQ��AM��AL1'AJ�`AIAG�AEx�ACAB�AA��A@��A=��A:A8��A8�A7��A69XA5%A3�-A3\)A3�A1�
A0�yA0bA/�7A.��A,��A*z�A'33A&$�A%��A$�9A"��A M�A�#A�A��A&�A�AC�A��A�A?}A�+A��AȴA�
A�`A�DAƨA�A�9AffA1'A��A9XA�-A�9AM�A  A�mA��A
ĜA
I�A	�
A	+A1'A��A��A1'Ap�A��A��Ax�A�Az�Ar�AE�A��A ��@�X@��7@���@�X@��/@���@�
=@��-@�t�@�^5@���@�X@�z�@�t�@�;d@���@�5?@�O�@�S�@�x�@�j@땁@�+@��H@�ff@�u@�1@��@�X@���@�ƨ@��@���@◍@�-@�9@�|�@��@��@�C�@��@؋D@�t�@�v�@թ�@�%@�(�@�K�@Ұ!@��#@Ь@Ϯ@Ο�@��T@͉7@͡�@͙�@�Ĝ@���@�C�@�"�@�
=@��H@ʏ\@ɉ7@�b@���@�ff@��`@�Ĝ@�Ĝ@���@��y@�n�@�~�@+@�@���@��H@�v�@��@���@�x�@�hs@�G�@��9@�I�@�1@��w@��@�n�@��-@��D@��@�K�@���@�E�@���@���@��j@�1'@���@��m@���@��@�5?@���@��h@�X@��j@�j@�Q�@�b@��F@�\)@��H@�$�@��@�X@���@��/@���@�I�@���@��;@��P@�33@��@��!@���@���@�E�@�@��7@�x�@�G�@���@�1'@��
@���@���@�"�@���@�v�@�J@���@��@���@���@�A�@��P@�C�@��\@�{@��@�{@��-@��@�1'@�(�@�  @��@��P@�l�@���@���@�?}@��@���@���@�z�@�Z@�Q�@� �@�  @�  @��@�  @��P@�K�@�dZ@�dZ@�\)@�o@��@��\@�-@�@���@��7@�7L@�V@���@��@�Z@�1@��@�t�@�+@��\@��+@��+@�M�@�@��^@�x�@�r�@�(�@��P@�;d@��@�ȴ@��\@��@���@�`B@�X@��@�z�@�b@�S�@���@��H@�ȴ@��!@���@���@�ff@�V@�@�X@�7L@�&�@���@�Ĝ@��u@�Q�@�  @��@��@�33@��@��!@�^5@���@���@��7@�X@�?}@�%@��/@��j@��u@�Q�@�1@���@���@��@��@��@��m@��m@��w@�t�@�33@�o@��@��R@���@��+@�5?@�p�@�7L@��@��`@���@�9X@�1@��
@���@�ƨ@��w@��@�+@���@���@�~�@�n�@�M�@�5?@�-@���@�|@z��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�mB
1B
��BD�BH�BQ�BS�BVBT�BK�BO�BYB\)B]/B^5B^5B_;B_;B_;B`BB`BB_;B^5B[#BT�BS�BT�BVBR�B]/Bt�B~�B�%B�7B�JB�bB��B��B��B��B��B��BŢB��B��B�B��BVB(�B)�B2-B6FB9XB9XB>wBI�BZBZBYBXBP�BJ�BcTBp�Bs�Bm�Bo�Bo�BjBgmBdZBaHBaHBdZBe`BdZB^5BW
BM�B>wB-B$�B�B�BB�B�mB�5BȴB�jB�'B��B�uBk�BL�B&�B1B
�B
�NB
�
B
��B
��B
�jB
�oB
s�B
aHB
N�B
;dB
+B
B	�jB	��B	��B	��B	�JB	�%B	u�B	iyB	VB	C�B	:^B	-B	JB��B�yB�NB�5B�B��B��BɺBǮBĜB��B�^B�?B�3B�-B�'B�-B�-B�-B�'B�!B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B�B�-B�XB�jB�^B�jB�jB�dB�RB�FB��BĜBƨBĜBBBĜBĜBȴBɺB��B��B�B�
B��B��B��B��B��B�B�B�)B�5B�;B�NB�TB�TB�NB�TB�`B�fB�fB�fB�mB�B�B�sB�`B�ZB�TB�`B�HB�`B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B	  B	%B	
=B	
=B	DB	VB	\B	bB	uB	�B	�B	 �B	 �B	&�B	-B	1'B	49B	5?B	6FB	7LB	6FB	7LB	6FB	7LB	9XB	<jB	@�B	B�B	D�B	E�B	E�B	I�B	N�B	O�B	P�B	R�B	VB	YB	^5B	cTB	e`B	e`B	gmB	k�B	m�B	o�B	q�B	t�B	w�B	{�B	~�B	� B	� B	~�B	~�B	�B	�B	�B	�B	�B	�B	�B	�%B	�1B	�=B	�JB	�JB	�VB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�-B	�9B	�LB	�^B	�dB	�qB	�}B	�}B	B	ĜB	ĜB	ƨB	ȴB	ȴB	ȴB	ȴB	ȴB	ȴB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�#B	�)B	�/B	�5B	�;B	�BB	�NB	�TB	�ZB	�ZB	�fB	�fB	�mB	�mB	�mB	�mB	�mB	�mB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
1B
1B
1B
1B
1B
1B
1B
	7B
	7B
	7B
	7B

=B

=B
	7B

=B
DB
DB
JB
JB
JB
PB
VB
VB
\B
\B
\B
\B
\B
bB
hB
hB
hB
hB
hB
hB
�B
�B
'm22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B�mB
1B
��BD�BH�BQ�BS�BVBT�BK�BO�BYB\)B]/B^5B^5B_;B_;B_;B`BB`BB_;B^5B[#BT�BS�BT�BVBR�B]/Bt�B~�B�%B�7B�JB�bB��B��B��B��B��B��BŢB��B��B�B��BVB(�B)�B2-B6FB9XB9XB>wBI�BZBZBYBXBP�BJ�BcTBp�Bs�Bm�Bo�Bo�BjBgmBdZBaHBaHBdZBe`BdZB^5BW
BM�B>wB-B$�B�B�BB�B�mB�5BȴB�jB�'B��B�uBk�BL�B&�B1B
�B
�NB
�
B
��B
��B
�jB
�oB
s�B
aHB
N�B
;dB
+B
B	�jB	��B	��B	��B	�JB	�%B	u�B	iyB	VB	C�B	:^B	-B	JB��B�yB�NB�5B�B��B��BɺBǮBĜB��B�^B�?B�3B�-B�'B�-B�-B�-B�'B�!B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B�B�-B�XB�jB�^B�jB�jB�dB�RB�FB��BĜBƨBĜBBBĜBĜBȴBɺB��B��B�B�
B��B��B��B��B��B�B�B�)B�5B�;B�NB�TB�TB�NB�TB�`B�fB�fB�fB�mB�B�B�sB�`B�ZB�TB�`B�HB�`B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B	  B	%B	
=B	
=B	DB	VB	\B	bB	uB	�B	�B	 �B	 �B	&�B	-B	1'B	49B	5?B	6FB	7LB	6FB	7LB	6FB	7LB	9XB	<jB	@�B	B�B	D�B	E�B	E�B	I�B	N�B	O�B	P�B	R�B	VB	YB	^5B	cTB	e`B	e`B	gmB	k�B	m�B	o�B	q�B	t�B	w�B	{�B	~�B	� B	� B	~�B	~�B	�B	�B	�B	�B	�B	�B	�B	�%B	�1B	�=B	�JB	�JB	�VB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�-B	�9B	�LB	�^B	�dB	�qB	�}B	�}B	B	ĜB	ĜB	ƨB	ȴB	ȴB	ȴB	ȴB	ȴB	ȴB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�#B	�)B	�/B	�5B	�;B	�BB	�NB	�TB	�ZB	�ZB	�fB	�fB	�mB	�mB	�mB	�mB	�mB	�mB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
1B
1B
1B
1B
1B
1B
1B
	7B
	7B
	7B
	7B

=B

=B
	7B

=B
DB
DB
JB
JB
JB
PB
VB
VB
\B
\B
\B
\B
\B
bB
hB
hB
hB
hB
hB
hB
�B
�B
'm22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.32 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190542                              AO  ARCAADJP                                                                    20181005190542    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190542  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190542  QCF$                G�O�G�O�G�O�8000            