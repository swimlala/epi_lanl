CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-09-17T19:15:45Z AOML 3.0 creation; 2016-06-01T00:08:26Z UW 3.1 conversion     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150917191545  20160531170826  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4055_7112_127                   2C  D   APEX                            5374                            041511                          846 @�p���1   @�p���@:U\(��d`A�7L1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB��B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBpffBx  B��B���B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` Dy,�D���D�C3D���D��3D���D�C3D��fD���D��D�\�D�� DǼ�D�  D�33DچfD�ɚD��3D�)�D�s31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�p�@�=qA�A%�AE�Ae�A��\A��\A��\A��\A\Aҏ\A�\A�\BG�B	�B�HBG�B!G�B)G�B1G�B9G�BAG�BIG�BQG�BYG�BaG�Bi�Bq�ByG�B�p�B�p�B���B��
B���B���B���B���B���B���B���B���B���B���B���B���B���Bģ�Bȣ�Ḅ�BУ�Bԣ�Bأ�Bܣ�B��B��B��B��B��B���B���B���C Q�CQ�CQ�CQ�CQ�C
Q�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�C Q�C"Q�C$Q�C&Q�C(Q�C*Q�C,Q�C.Q�C0Q�C2Q�C4Q�C6Q�C8Q�C:Q�C<Q�C>Q�C@Q�CBQ�CDQ�CFQ�CHQ�CJQ�CLQ�CNQ�CPQ�CRQ�CTQ�CVQ�CXQ�CZQ�C\Q�C^Q�C`Q�CbQ�CdQ�CfQ�ChQ�CjQ�ClQ�CnQ�CpQ�CrQ�CtQ�CvQ�CxQ�CzQ�C|Q�C~Q�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�D {D �{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D	{D	�{D
{D
�{D{D�{D{D�D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D�D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D {D �{D!{D!�{D"{D"�{D#{D#�{D${D$�{D%{D%�{D&{D&�{D'{D'�{D({D(�{D){D)�{D*{D*�{D+{D+�{D,{D,�{D-{D-�{D.{D.�{D/{D/�{D0{D0�{D1{D1�{D2{D2�{D3{D3�{D4{D4�{D5{D5�{D6{D6�{D7{D7�{D8{D8�{D9{D9�{D:{D:�{D;{D;�{D<{D<�{D={D=�{D>{D>�{D?{D?�{D@{D@�{DA{DA�{DB{DB�{DC{DC�{DD{DD�{DE{DE�{DF{DF�{DG{DG�{DH{DH�{DI{DI�{DJ{DJ�{DK{DK�{DL{DL�{DM{DM�{DN{DN�{DO{DO�{DP{DP�{DQ{DQ�{DR{DR�{DS{DS�{DT{DT�{DU{DU�{DV{DV�{DW{DW�{DX{DX�{DY{DY�{DZ{DZ�{D[{D[�{D\{D\�{D]{D]�{D^{D^�{D_{D_�{D`{D`�{Da{Da�{Db{Db�{Dc{Dc�{Dd{Dd�{De{De�{Df{Df�{Dg{Dg�{Dh{Dh�{Di{Di�{Dj{Dj�{Dk{Dk�{Dl{Dl�{Dm{Dm�{Dn{Dn�{Do{Do�{Dp{Dp�{Dq{Dq�{Dr{Dr�{Ds{Ds�{Dt{Dtt{DyAHD�
D�MpD��
D��pD��
D�MpD���D��
D���D�g
D��=D��
D�
=D�=pDڐ�D���D��pD�3�D�}p1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ĜA�ȴA�ĜAپwA�A�A���A���A���A���A���A���A���A���AپwA٬Aأ�AՓuA�;dA��AѺ^A�A��A�7LA�n�A���A�%A�Q�A�`BA���A��`A�hsA�?}A��A��A�9XA���A�9XA��`A�jA��A��`A�G�A�jA�+A���A�9XA�C�A��A�G�A��wA��A�bNA�`BA��A���A�~�A��HA��A���A��A��/A�~�A�A���A��A��A�&�A��A��hA���A���A�~�A�{A�;dA��TA���A�dZA���A��^A��A�VAdZA7LA~��A~5?A}��A|r�A{�^A{�A{`BA{O�Az1'Aw\)At��ArZAq�AqAp��Apn�Ao�Ao&�Am|�Ak�TAk�Ak�Aj��AjĜAj�DAi�Ah�HAe�PAdjAbI�Aa|�A`�A^v�A]x�A\A[;dAZ��AZbNAY?}AW�AU%ATZAT(�AS�-AR�uARI�AQ�;AP�jAN��AN��ANI�AM��AM�TAM��AMAM�-AM�hAM�7AM|�AM?}AL�RAL�AK�AJ�/AJ �AIAI�PAH�AG�mAGp�AF��AEK�AD��AD{ABĜAA`BA@A?XA>�A<��A9%A7l�A5�A5
=A4{A1?}A.�jA.�+A.^5A-�
A,E�A+;dA)��A(�/A(�uA(n�A(A'S�A&VA%��A$��A$1A"^5A"A!|�A!33A �`A �RA�AS�A��AdZA��A(�A�A�wA��AhsA?}AA�RA�A�\A;dA1'AM�AoA�#A
=A=qA�7AG�A&�AA�jA��A�DAE�A�TAx�A
��A	��A	S�AG�Az�A{AS�A��A��AM�A(�A{A�FAA��Ar�AAVA  �@�=q@��y@��m@��T@�O�@��@��/@���@��#@�@�P@��@���@�M�@�F@�5?@��@�A�@�K�@�\@�j@��@�?}@�t�@��H@ڇ+@�J@؃@���@ӶF@�7L@�+@���@�hs@�/@� �@�S�@�$�@Ɂ@�&�@��/@ȼj@�Z@�v�@�Z@Å@���@�-@��@���@���@�x�@�X@�7L@��u@��F@�o@��@��@�5?@���@�/@��/@��@�(�@���@�+@�|�@�v�@�@��-@��@�@�j@�t�@�o@��@�+@�J@�?}@�V@���@���@�"�@��@��@�`B@��@���@�ƨ@��+@�v�@�n�@���@�O�@�V@�9X@��H@�E�@��-@�?}@�&�@���@�Ĝ@��D@�Q�@��F@�|�@�\)@�+@�n�@��@�hs@��/@�(�@��;@��F@�|�@�;d@�ȴ@�ff@�$�@��h@��@�|�@�33@�
=@��+@�5?@�@�@�x�@��h@���@���@��-@���@�p�@���@�bN@���@�|�@�K�@���@�n�@�5?@��#@��^@��-@���@�x�@��@��9@�r�@��F@�l�@�+@���@�^5@�E�@�$�@�{@��@��T@���@���@���@�K�@���@���@���@�E�@��@�x�@���@�r�@
=@~�y@~��@\)@K�@�(�@��u@��@��@�I�@��@�1'@�r�@�1'@� �@���@�J@���@�o@��H@���@�^5@�E�@��@��@��7@�x�@�p�@�G�@�/@�V@��@��@�r�@� �@\)@K�@�P@
=@~E�@}�-@}�@}O�@}V@|�@{S�@{"�@{@z�H@z��@y�@y��@y&�@x�`@xĜ@x�u@xQ�@w��@wK�@v��@v�@u�T@tZ@tz�@t�@s"�@q��@p�@o+@mO�@e?}@_��@Vv�@R=q@Mp�@F��@?l�@;ƨ@5�T@,�@&ff@"�\@{@��@
=@�
@G�@�h@	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ĜA�ȴA�ĜAپwA�A�A���A���A���A���A���A���A���A���AپwA٬Aأ�AՓuA�;dA��AѺ^A�A��A�7LA�n�A���A�%A�Q�A�`BA���A��`A�hsA�?}A��A��A�9XA���A�9XA��`A�jA��A��`A�G�A�jA�+A���A�9XA�C�A��A�G�A��wA��A�bNA�`BA��A���A�~�A��HA��A���A��A��/A�~�A�A���A��A��A�&�A��A��hA���A���A�~�A�{A�;dA��TA���A�dZA���A��^A��A�VAdZA7LA~��A~5?A}��A|r�A{�^A{�A{`BA{O�Az1'Aw\)At��ArZAq�AqAp��Apn�Ao�Ao&�Am|�Ak�TAk�Ak�Aj��AjĜAj�DAi�Ah�HAe�PAdjAbI�Aa|�A`�A^v�A]x�A\A[;dAZ��AZbNAY?}AW�AU%ATZAT(�AS�-AR�uARI�AQ�;AP�jAN��AN��ANI�AM��AM�TAM��AMAM�-AM�hAM�7AM|�AM?}AL�RAL�AK�AJ�/AJ �AIAI�PAH�AG�mAGp�AF��AEK�AD��AD{ABĜAA`BA@A?XA>�A<��A9%A7l�A5�A5
=A4{A1?}A.�jA.�+A.^5A-�
A,E�A+;dA)��A(�/A(�uA(n�A(A'S�A&VA%��A$��A$1A"^5A"A!|�A!33A �`A �RA�AS�A��AdZA��A(�A�A�wA��AhsA?}AA�RA�A�\A;dA1'AM�AoA�#A
=A=qA�7AG�A&�AA�jA��A�DAE�A�TAx�A
��A	��A	S�AG�Az�A{AS�A��A��AM�A(�A{A�FAA��Ar�AAVA  �@�=q@��y@��m@��T@�O�@��@��/@���@��#@�@�P@��@���@�M�@�F@�5?@��@�A�@�K�@�\@�j@��@�?}@�t�@��H@ڇ+@�J@؃@���@ӶF@�7L@�+@���@�hs@�/@� �@�S�@�$�@Ɂ@�&�@��/@ȼj@�Z@�v�@�Z@Å@���@�-@��@���@���@�x�@�X@�7L@��u@��F@�o@��@��@�5?@���@�/@��/@��@�(�@���@�+@�|�@�v�@�@��-@��@�@�j@�t�@�o@��@�+@�J@�?}@�V@���@���@�"�@��@��@�`B@��@���@�ƨ@��+@�v�@�n�@���@�O�@�V@�9X@��H@�E�@��-@�?}@�&�@���@�Ĝ@��D@�Q�@��F@�|�@�\)@�+@�n�@��@�hs@��/@�(�@��;@��F@�|�@�;d@�ȴ@�ff@�$�@��h@��@�|�@�33@�
=@��+@�5?@�@�@�x�@��h@���@���@��-@���@�p�@���@�bN@���@�|�@�K�@���@�n�@�5?@��#@��^@��-@���@�x�@��@��9@�r�@��F@�l�@�+@���@�^5@�E�@�$�@�{@��@��T@���@���@���@�K�@���@���@���@�E�@��@�x�@���@�r�@
=@~�y@~��@\)@K�@�(�@��u@��@��@�I�@��@�1'@�r�@�1'@� �@���@�J@���@�o@��H@���@�^5@�E�@��@��@��7@�x�@�p�@�G�@�/@�V@��@��@�r�@� �@\)@K�@�P@
=@~E�@}�-@}�@}O�@}V@|�@{S�@{"�@{@z�H@z��@y�@y��@y&�@x�`@xĜ@x�u@xQ�@w��@wK�@v��@v�@u�T@tZ@tz�@t�@s"�@q��@p�@o+@mO�@e?}@_��@Vv�@R=q@Mp�@F��@?l�@;ƨ@5�T@,�@&ff@"�\@{@��@
=@�
@G�@�h@	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BZBÖB�B�B��BȴB�dB�'B��B�PB� Bz�Bx�BdZB"�B�B+B�#B�B��BdZBT�BG�BF�BL�BS�BXBYBW
BT�BM�BG�B@�B:^B33B%�B�B�BuBVBB��B�ZB�B��BƨB�jB��B�uB�%BiyBXBN�BB�B0!B"�B�BPB%BB
��B
�B
�#B
��B
��B
�?B
�3B
�'B
��B
��B
��B
��B
��B
��B
��B
�JB
l�B
L�B
6FB
.B
,B
(�B
#�B
�B
{B
	7B	��B	��B	��B	��B	��B	�B	�B	�BB	ƨB	�^B	��B	��B	��B	�PB	�B	|�B	x�B	u�B	q�B	jB	aHB	VB	Q�B	P�B	O�B	M�B	K�B	H�B	E�B	K�B	P�B	T�B	W
B	XB	YB	ZB	\)B	_;B	`BB	aHB	aHB	aHB	`BB	_;B	]/B	\)B	\)B	[#B	XB	S�B	P�B	K�B	D�B	@�B	:^B	33B	,B	$�B	�B	�B	+B�B�HB�ZB�NB�)B��BɺBɺBǮBĜB��B�jB�RB�?B�9B�3B�3B�-B�B�B��B��B��B��B��B��B��B�uB�bB�PB�+B�B}�B|�B{�B{�Bz�By�Bx�Bw�Bt�Bq�Bl�BiyBdZB`BB^5B[#BYBW
BVBT�BS�BS�BR�BR�BQ�BP�BO�BM�BK�BH�BE�BB�BA�B?}B>wB=qB=qB<jB<jB;dB:^B9XB8RB7LB5?B33B1'B.B+B)�B)�B(�B(�B(�B&�B%�B$�B#�B"�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B"�B"�B"�B"�B"�B"�B"�B$�B&�B&�B)�B.B2-B33B49B49B5?B8RB:^B?}BG�BI�BI�BH�BH�BN�BP�BT�BYB]/B_;BaHBbNBbNBaHBffBhsBhsBk�Bm�Bn�Bo�Bq�Bv�Bv�Bv�Bw�By�By�B|�B�B�%B�+B�1B�7B�=B�VB�\B�bB�uB�{B�{B��B��B��B��B��B��B��B��B��B�B�B�!B�'B�3B�jB��B��BĜBɺB��B��B�B�/B�BB�TB�fB�fB�fB�fB�yB�B��B��B��B��B	  B	B	+B	+B	+B	+B	+B	+B	+B	%B	%B	+B	1B	DB	PB	VB	\B	bB	oB	�B	�B	�B	!�B	&�B	+B	-B	1'B	33B	5?B	6FB	7LB	:^B	?}B	B�B	D�B	E�B	G�B	L�B	O�B	P�B	P�B	O�B	Q�B	R�B	T�B	VB	XB	]/B	ffB	k�B	o�B	q�B	s�B	v�B	w�B	y�B	{�B	� B	� B	�B	�B	�B	�%B	�+B	�DB	�PB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�-B	�-B	�9B	�?B	�FB	�RB	�XB	�XB	�XB	�XB	�dB	�jB	�jB	�dB	�XB	�dB	��B	�BB	�B
  B
{B
�B
 �B
)�B
0!B
8RB
D�B
J�B
O�B
VB
[#B
`BB
cTB
gmB
k�B
p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�qB�pB�qB�qB�qB�sB�qB�sB�sB�sB�}B�{B�{B��B��B��B��BZ
BÄB�B��B��BȡB�TB�B��B�;B�Bz�Bx�BdCB"�BsBB�
B��B�|Bd<BT�BG�BF�BL�BS�BW�BX�BV�BT�BM�BG�B@cB:>B3B%�B�BqBVB6BB��B�;B��B̮BƊB�JB��B�VB�BiXBW�BN�BBrB0B"�B�B1BB �B
��B
�hB
�B
̭B
�eB
�!B
�B
�
B
��B
��B
��B
�wB
�sB
�oB
�eB
�,B
loB
L�B
6+B
-�B
+�B
(�B
#�B
�B
bB
	B	��B	��B	��B	��B	��B	�B	�mB	�*B	ƑB	�FB	��B	��B	��B	�;B	�	B	|�B	x�B	u�B	q�B	jjB	a4B	U�B	Q�B	P�B	O�B	M�B	K�B	H�B	E�B	K�B	P�B	T�B	V�B	W�B	YB	ZB	\B	_'B	`+B	a5B	a1B	a3B	`+B	_&B	]B	\B	\B	[B	W�B	S�B	P�B	K�B	D�B	@nB	:HB	3B	+�B	$�B	�B	mB	B�xB�6B�FB�>B�B��BɨBɪBǛBċB�qB�WB�AB�+B�*B�!B�!B�B�B��B��B��B��B��B��B�wB�qB�fB�RB�@B�B��B}�B|�B{�B{�Bz�By�Bx�Bw�Bt�Bq�Bl|BijBdIB`3B^'B[BYBV�BU�BT�BS�BS�BR�BR�BQ�BP�BO�BM�BK�BH�BE�BB�BAzB?mB>jB=dB=cB<[B<]B;XB:PB9LB8CB7?B50B3$B1B.B*�B)�B)�B(�B(�B(�B&�B%�B$�B#�B"�B �B�B�B�B�B�B�B�B|B�B�B�B~B�BsB}BdB~B�B�B~B�B�B}B}B�B�B�B�B�B�B�B�B �B!�B"�B"�B"�B"�B"�B"�B"�B$�B&�B&�B)�B-�B2B3$B4(B4(B5/B8BB:LB?kBG�BI�BI�BH�BH�BN�BP�BT�BYB]B_'Ba6Bb;Bb7Ba4BfRBhaBh`BkqBm|Bn�Bo�Bq�Bv�Bv�Bv�Bw�By�By�B|�B�B�B�B�B�$B�(B�>B�FB�KB�]B�dB�cB�iB��B��B��B��B��B��B��B��B��B��B�B�B�B�RB�jB�nBĆBɢBͻB��B��B�B�&B�:B�LB�IB�IB�JB�]B�~B��B��B��B��B��B	�B	B	B	B	B	B	B	B		B	
B	B	B	(B	3B	:B	BB	FB	RB	jB	qB	�B	!�B	&�B	*�B	,�B	1B	3B	5!B	6(B	7/B	:AB	?`B	BrB	D}B	E�B	G�B	L�B	O�B	P�B	P�B	O�B	Q�B	R�B	T�B	U�B	W�B	]B	fGB	kgB	o�B	q�B	s�B	v�B	w�B	y�B	{�B	�B	�B	��B	��B	��B	�B	�	B	�"B	�0B	�8B	�GB	�UB	�]B	�sB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�/B	�6B	�5B	�6B	�6B	�EB	�IB	�JB	�CB	�6B	�AB	̩B	�B	�|B	��B
VB
iB
 �B
)�B
/�B
8.B
DwB
J�B
O�B
U�B
Z�B
`B
c-B
gGB
k]B
p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.32 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708262016053117082620160531170826  AO  ARCAADJP                                                                    20150917191545    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150917191545  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150917191545  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170826  IP                  G�O�G�O�G�O�                