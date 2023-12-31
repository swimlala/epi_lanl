CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-12-04T23:00:08Z AOML 3.0 creation; 2016-06-01T00:08:22Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20141204230008  20160531170822  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               cA   AO  4055_7112_099                   2C  D   APEX                            5374                            041511                          846 @�(i ���1   @�(i�̀@9�5?|��d1�7Kƨ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    cA   A   A   @�33@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CA�fCD  CF�CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DWfDW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3DyS3D�fD�I�D�p D���D�  D�C3D��3D��fD���D�VfD���Dǰ D��fD�0 D�s3D�ٚD��D�L�D�i�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�p�@�=qA�A%�AE�Ae�A��\A��\A��\A��\A\Aҏ\A�\A�\BG�B	G�BG�BG�B!G�B)G�B1G�B9G�BAG�BIG�BQG�BYG�BaG�BiG�BqG�ByG�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B��
B���Bģ�Bȣ�Ḅ�BУ�Bԣ�Bأ�Bܣ�B��B��B��B��B��B���B���B���C Q�CQ�CQ�CQ�CQ�C
Q�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�C Q�C"Q�C$Q�C&Q�C(Q�C*Q�C,Q�C.Q�C0Q�C2Q�C4Q�C6Q�C8Q�C:Q�C<Q�C>Q�C@Q�CB8RCDQ�CFk�CHQ�CJQ�CLQ�CNQ�CPQ�CRQ�CTQ�CVQ�CXQ�CZQ�C\Q�C^Q�C`Q�CbQ�CdQ�CfQ�ChQ�CjQ�ClQ�CnQ�CpQ�CrQ�CtQ�CvQ�CxQ�CzQ�C|Q�C~Q�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�D {D �{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D	{D	�{D
{D
�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D {D �{D!{D!�{D"{D"�{D#{D#�{D${D$�{D%{D%�{D&{D&�{D'{D'�{D({D(�{D){D)�{D*{D*�{D+{D+�{D,{D,�{D-{D-�{D.{D.�{D/{D/�{D0{D0�{D1{D1�{D2{D2�{D3{D3�{D4{D4�{D5{D5�{D6{D6�{D7{D7�{D8{D8�{D9{D9�{D:{D:�{D;{D;�{D<{D<�{D={D=�{D>{D>�{D?{D?�{D@{D@�{DA{DA�{DB{DB�{DC{DC�{DD{DD�{DE{DE�{DF{DF�{DG{DG�{DH{DH�{DI{DI�{DJ{DJ�{DK{DK�{DL{DL�{DM{DM�{DN{DN�{DO{DO�{DP{DP�{DQ{DQ�{DR{DR�{DS{DS�{DT{DT�{DU{DU�{DV{DV�{DW�DW�{DX{DX�{DY{DY�{DZ{DZ�{D[{D[�{D\{D\�{D]{D]�{D^{D^�{D_{D_�{D`{D`�{Da{Da�{Db{Db�{Dc{Dc�{Dd{Dd�{De{De�{Df{Df�{Dg{Dg�{Dh{Dh�{Di{Di�{Dj{Dj�{Dk{Dk�{Dl{Dl�{Dm{Dm�{Dn{Dn�{Do{Do�{Dp{Dp�{Dq{Dq�{Dr{Dr�{Ds{Ds�{Dt{Dt��Dyg�D� �D�S�D�z=D��
D�
=D�MpD��pD�УD��D�`�D���DǺ=D��D�:=D�}pD���D�#�D�W
D�s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A��-A��-A��A��A���A���A���A���A�|�A�XA�M�A��A���A��yA��
A���A�ĜA��^A��9A��A���A���A���A��PA��7A��PA�t�A�&�A���A�1'A��A�z�A�M�A�
=A��wA�ƨA���A�t�A�ƨA���A�VA�~�A�A��A���A�JA���A�ZA��A���A��uA�\)A�7LA�r�A�bA�I�A�$�A�r�A��A�ȴA��!A�ȴA��^A��-A���A�\)A�n�A�ĜA�C�A��A���A�C�A�1'A�"�A��PA��mA�33A�bA���A�-A���A�bNA��A�1'A�  A"�A{G�AxȴAt�/As�Ar��Ar^5Ar  Ap�uAp�uAp�Apv�AoC�ApJAp~�Aox�An�AljAkK�Ak/AjĜAh=qAdbNAb�Aa�;A`I�A_dZA_?}A]S�A[/AZ�+AY�hAX��AXz�AW��AW
=AV�RAV1'AU�AUt�AU?}AUVAT�yAT^5AS`BARE�AQ�AP��AOx�AL��AK�AJ~�AJ=qAI�-AI;dAIoAHr�AG�AEO�ACK�AB�RAB  A?p�A>ĜA>VA=C�A<�RA:��A8-A6��A6��A5��A5%A4jA2��A0��A/��A/+A.A�A-?}A-%A,�jA,ZA+�
A+�A+7LA*�A(ffA'�7A'C�A&��A$��A$^5A"�A"M�A!�A!��A!`BA �`A ��A  �A  A;dA��A$�A"�A��AVA�A��A�A9XA~�An�AZA=qA��A��AG�A%A�AAA��A9XA�\A�DA��A�TA�A
��A
�A
bNA	�TA	7LA	%A�jAz�AAl�A�/A5?A{A��A�#A��AO�A&�AA�A��A��AZA -@���@��w@���@�/@���@�A�@�l�@�ȴ@�/@�@���@�j@�+@�/@�l�@�J@�X@�1'@睲@���@�`B@�O�@�j@��@��@�l�@�X@�Z@�l�@��y@؛�@��@ԛ�@�&�@Ͼw@��#@̬@�I�@�C�@�$�@���@ɡ�@�p�@��@�  @ǶF@�|�@�S�@���@�M�@��@Å@��-@�1'@��w@���@�hs@���@�1@��R@�E�@���@��@�V@�@��@���@�(�@�;d@�{@�1'@�;d@�v�@���@��/@���@�"�@���@�M�@�@��7@�hs@�G�@��`@�r�@���@�"�@�n�@�Z@���@���@��@�@��7@�O�@�1'@���@�$�@��7@���@���@�Z@�@��h@�/@���@�Z@���@�K�@�n�@��@�V@���@��j@��j@��j@��9@��9@��@���@�A�@��m@�;d@�"�@��H@���@��!@�v�@�p�@���@�z�@�I�@�9X@��@���@��;@�+@��y@�ȴ@�^5@���@�X@��@�Ĝ@�z�@�Q�@��@��m@�K�@��R@�^5@�-@�J@���@��T@���@�hs@�O�@�&�@���@��/@��@��@�I�@�  @���@�C�@���@��!@�~�@�V@�=q@��@�@��@��#@���@���@��#@��#@��T@��#@��#@�@��^@�7L@���@���@�Ĝ@���@�z�@�Q�@�(�@�(�@�1'@��@���@��w@��m@�ƨ@�|�@���@��H@��@���@�ȴ@���@�V@�E�@�5?@��@��#@��^@���@�p�@�V@���@���@�Ĝ@��j@�r�@�A�@�1@�@l�@K�@~ȴ@~@|z�@|1@{�
@{33@{"�@{"�@z�@y��@y��@yhs@yhs@yX@y&�@x��@x��@x��@x  @w�w@w�w@u�@kS�@b-@X1'@Q%@J=q@A��@;dZ@6@/�;@*��@%�-@"^5@|�@��@;d@+@
=q@$�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A��-A��-A��A��A���A���A���A���A�|�A�XA�M�A��A���A��yA��
A���A�ĜA��^A��9A��A���A���A���A��PA��7A��PA�t�A�&�A���A�1'A��A�z�A�M�A�
=A��wA�ƨA���A�t�A�ƨA���A�VA�~�A�A��A���A�JA���A�ZA��A���A��uA�\)A�7LA�r�A�bA�I�A�$�A�r�A��A�ȴA��!A�ȴA��^A��-A���A�\)A�n�A�ĜA�C�A��A���A�C�A�1'A�"�A��PA��mA�33A�bA���A�-A���A�bNA��A�1'A�  A"�A{G�AxȴAt�/As�Ar��Ar^5Ar  Ap�uAp�uAp�Apv�AoC�ApJAp~�Aox�An�AljAkK�Ak/AjĜAh=qAdbNAb�Aa�;A`I�A_dZA_?}A]S�A[/AZ�+AY�hAX��AXz�AW��AW
=AV�RAV1'AU�AUt�AU?}AUVAT�yAT^5AS`BARE�AQ�AP��AOx�AL��AK�AJ~�AJ=qAI�-AI;dAIoAHr�AG�AEO�ACK�AB�RAB  A?p�A>ĜA>VA=C�A<�RA:��A8-A6��A6��A5��A5%A4jA2��A0��A/��A/+A.A�A-?}A-%A,�jA,ZA+�
A+�A+7LA*�A(ffA'�7A'C�A&��A$��A$^5A"�A"M�A!�A!��A!`BA �`A ��A  �A  A;dA��A$�A"�A��AVA�A��A�A9XA~�An�AZA=qA��A��AG�A%A�AAA��A9XA�\A�DA��A�TA�A
��A
�A
bNA	�TA	7LA	%A�jAz�AAl�A�/A5?A{A��A�#A��AO�A&�AA�A��A��AZA -@���@��w@���@�/@���@�A�@�l�@�ȴ@�/@�@���@�j@�+@�/@�l�@�J@�X@�1'@睲@���@�`B@�O�@�j@��@��@�l�@�X@�Z@�l�@��y@؛�@��@ԛ�@�&�@Ͼw@��#@̬@�I�@�C�@�$�@���@ɡ�@�p�@��@�  @ǶF@�|�@�S�@���@�M�@��@Å@��-@�1'@��w@���@�hs@���@�1@��R@�E�@���@��@�V@�@��@���@�(�@�;d@�{@�1'@�;d@�v�@���@��/@���@�"�@���@�M�@�@��7@�hs@�G�@��`@�r�@���@�"�@�n�@�Z@���@���@��@�@��7@�O�@�1'@���@�$�@��7@���@���@�Z@�@��h@�/@���@�Z@���@�K�@�n�@��@�V@���@��j@��j@��j@��9@��9@��@���@�A�@��m@�;d@�"�@��H@���@��!@�v�@�p�@���@�z�@�I�@�9X@��@���@��;@�+@��y@�ȴ@�^5@���@�X@��@�Ĝ@�z�@�Q�@��@��m@�K�@��R@�^5@�-@�J@���@��T@���@�hs@�O�@�&�@���@��/@��@��@�I�@�  @���@�C�@���@��!@�~�@�V@�=q@��@�@��@��#@���@���@��#@��#@��T@��#@��#@�@��^@�7L@���@���@�Ĝ@���@�z�@�Q�@�(�@�(�@�1'@��@���@��w@��m@�ƨ@�|�@���@��H@��@���@�ȴ@���@�V@�E�@�5?@��@��#@��^@���@�p�@�V@���@���@�Ĝ@��j@�r�@�A�@�1@�@l�@K�@~ȴ@~@|z�@|1@{�
@{33@{"�@{"�@z�@y��@y��@yhs@yhs@yX@y&�@x��@x��@x��@x  @w�w@w�w@u�@kS�@b-@X1'@Q%@J=q@A��@;dZ@6@/�;@*��@%�-@"^5@|�@��@;d@+@
=q@$�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�DB�DB�DB�DB�DB�DB�DB�DB�DB�DB�DB�DB�DB�=B�1B�%B�%B�B�B�B�B�B�B�B�B�B�B�B�B}�Bp�BXB�B�DB_;B6FB0!B(�B�BuBVB�yB��B�qB�?B�B��B��B�hB�DB�BiyBC�B>wB:^B7LB&�B�B��B�mB�BǮB�3B��B�oB�hB�\B�Bu�Be`BdZB`BBZBL�BG�B1'B�B
��B
��B
�'B
��B
�=B
�B
z�B
q�B
hsB
YB
C�B
0!B
�B
B	�mB	�#B	��B	��B	��B	ƨB	��B	�/B	�;B	��B	�yB	��B	��B	��B	�fB	�B	�
B	��B	��B	u�B	aHB	[#B	VB	O�B	M�B	C�B	<jB	9XB	7LB	5?B	33B	0!B	.B	-B	+B	)�B	(�B	'�B	&�B	%�B	"�B	�B	�B	�B	{B	PB	1B	B	B	B	B	  B��B��B��B�B�B�B�B�`B�NB�;B�)B�B��BɺBƨBĜBB�}B�dB�FB�!B�B�B��B��B��B��B��B��B��B��B��B��B�{B�uB�\B�JB�7B�%B�B�B�B�B~�B}�By�Bu�Bs�Bq�Bo�Bl�Bk�BjBhsBffBdZBaHB^5B^5B]/B]/B[#BZBYBXBVBS�BT�BS�BP�BK�BG�BD�B@�B>wB=qB=qB<jB:^B9XB8RB7LB6FB5?B33B1'B0!B0!B/B/B.B-B,B)�B(�B%�B �B�B�B�B�B�B�B�B�B�B�B{BoBhB\B\BVBVBPBPBJBPBPBJBDB
=B
=B	7B	7B	7B	7B+B+B+B%B+B+B	7B	7B	7B	7B
=BDBDBDBDBJBJBJBJBJBDBDBJBVBbBbBhBuB{B{B�B�B�B�B�B�B �B!�B!�B"�B$�B)�B,B.B0!B2-B5?B8RB9XB:^B;dB=qB=qB=qB>wB?}BA�BC�BD�BL�BO�BR�BW
BXBXBXB\)BcTBe`BhsBk�Bk�Bm�Bs�B{�B}�B� B�B�B�1B�PB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�^B�dB�qB�qB�wB�}B��BŢBǮBȴB��B��B��B�
B�B�B�#B�/B�5B�ZB�B�B�B�B�B�B��B��B��B��B��B��B��B	B	B	+B	JB	\B	uB	�B	�B	�B	�B	�B	�B	 �B	"�B	#�B	&�B	+B	,B	.B	1'B	2-B	49B	49B	49B	7LB	:^B	=qB	>wB	@�B	B�B	C�B	D�B	D�B	D�B	C�B	D�B	F�B	I�B	L�B	O�B	P�B	P�B	P�B	R�B	YB	[#B	\)B	]/B	_;B	bNB	dZB	e`B	hsB	l�B	p�B	r�B	s�B	t�B	w�B	x�B	z�B	z�B	|�B	|�B	~�B	�B	�=B	�PB	�PB	�\B	�bB	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ƨB	�)B	�B
B
hB
 �B
+B
33B
<jB
C�B
J�B
N�B
R�B
YB
]/B
gmB
m�B
r�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�7B�7B�7B�5B�8B�8B�5B�7B�7B�7B�7B�5B�4B�.B�"B�B�B�B�B�B�B�B�B�B�B��B��B��B��B}�Bp�BXB�jB�0B_#B6.B0B(�BxBYB7B�_B˧B�SB� B��B��B�qB�IB�#B��BiYBCwB>YB:BB7-B&�B�B��B�LB��BǑB�B��B�PB�JB�:B��Bu�BeABd<B`#BZ BL�BG�B1B�B
��B
��B
�
B
��B
� B
��B
z�B
q�B
hWB
X�B
C{B
0B
�B
B	�TB	�B	��B	��B	ͺB	ƑB	��B	�B	�#B	��B	�`B	��B	��B	��B	�OB	��B	��B	̳B	��B	u�B	a5B	[B	U�B	O�B	M�B	C�B	<XB	9DB	78B	5*B	3B	0B	.B	,�B	*�B	)�B	(�B	'�B	&�B	%�B	"�B	�B	�B	�B	hB	>B	 B	B	 B	�B	 �B��B��B��B��B�B�B�B�lB�NB�?B�*B�B��B��BɪBƘBČB�~B�mB�SB�8B�B�B��B��B��B��B��B��B��B��B��B��B�vB�nB�eB�MB�;B�*B�B�	B�B��B��B~�B}�By�Bu�Bs�Bq�Bo�BlzBkxBjpBheBfXBdLBa;B^'B^&B]!B] B[BZBYBXBU�BS�BT�BS�BP�BK�BG�BD�B@tB>jB=dB=eB<^B:RB91B8CB7?B68B51B3&B1B0B0B.�B/B.B,�B+�B)�B(�B%�B �B�B�B�BxByBqB�BfB]BuBTB`BABOB4B/B/B)B*B!B)B)B"B5B
B
B	B	B	)B	(B BBB�BBB	B	B	B	(B
BB6B7BB!B"BB B!B4B4B!B-B9BSBZBhBPBQB{B~B}B�B�B�B �B!�B!�B"�B$�B)�B+�B. B0B2B5,B8@B9GB:JB;RB=_B=`B=aB>fB?jBAwBC�BD�BL�BO�BR�BV�BW�BW�BW�B\BcBBeJBh_BkqBkrBm~Bs�B{�B}�B�B��B�B�B�:B�eB�}B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�EB�LB�WB�XB�`B�cB�iBňBǖBțBˬB��B��B��B��B�B�B�B�B�AB�fB�vB�B��B��B�B��B��B��B��B��B��B��B	�B	B	B	.B	@B	WB	kB	wB	�B	�B	�B	�B	 �B	"�B	#�B	&�B	*�B	+�B	-�B	1	B	2B	4B	4B	4B	70B	:@B	=TB	>VB	@gB	BqB	CwB	DB	D�B	DB	CxB	DB	F�B	I�B	L�B	O�B	P�B	P�B	P�B	R�B	X�B	[B	\
B	]B	_B	b0B	d:B	e>B	hSB	ljB	p�B	r�B	s�B	t�B	w�B	x�B	z�B	z�B	|�B	|�B	~�B	��B	�B	�.B	�.B	�;B	�BB	�:B	�BB	�SB	�_B	�fB	�fB	�lB	�tB	�xB	�xB	�xB	��B	��B	��B	��B	ƅB	�B	�B
�B
CB
 �B
*�B
3B
<DB
CpB
J�B
N�B
R�B
X�B
]B
gHB
mjB
r�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.32 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708222016053117082220160531170822  AO  ARCAADJP                                                                    20141204230008    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20141204230008  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20141204230008  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170822  IP                  G�O�G�O�G�O�                