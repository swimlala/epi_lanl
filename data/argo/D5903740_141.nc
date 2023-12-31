CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-02-08T03:15:31Z AOML 3.0 creation; 2016-06-01T00:08:29Z UW 3.1 conversion     
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
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160208031531  20160531170829  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055_7112_141                   2C  D   APEX                            5374                            041511                          846 @ד��FW1   @ד��(@:+I�^�c���+1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B���B���B���B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DP��DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dry�Dr��Ds� Dt  Dts3Dys3D�	�D�9�D��3D�ٚD�fD�9�D���D��3D��fD�I�D��fD��fD� D�,�Dړ3D��D��fD�0 D�\�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @N{@�=q@�=qA�A%�AE�Ae�A��\A��\A��\A��\A\Aҏ\A�\A�\BG�B	G�BG�BG�B!G�B)G�B1G�B9G�BAG�BIG�BQG�BYG�BaG�BiG�BqG�ByG�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bģ�Bȣ�Ḅ�BУ�Bԣ�Bأ�Bܣ�B��B��B�
=B��B�p�B�p�B�p�B���C Q�CQ�CQ�CQ�CQ�C
Q�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�C Q�C"Q�C$Q�C&Q�C(Q�C*Q�C,Q�C.Q�C0Q�C2Q�C4Q�C6Q�C8Q�C:Q�C<Q�C>Q�C@Q�CBQ�CDQ�CFQ�CHQ�CJQ�CLQ�CNQ�CPQ�CRQ�CTQ�CVQ�CXQ�CZQ�C\Q�C^Q�C`Q�CbQ�CdQ�CfQ�ChQ�CjQ�ClQ�CnQ�CpQ�CrQ�CtQ�CvQ�CxQ�CzQ�C|Q�C~Q�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�D {D �{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D	{D	�{D
{D
�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D {D �{D!{D!�{D"{D"�{D#{D#�{D${D$�{D%{D%�{D&{D&�{D'{D'�{D({D(�{D){D)�{D*{D*�{D+{D+�{D,{D,�{D-{D-�{D.{D.�{D/{D/�{D0{D0�{D1{D1�{D2{D2�{D3{D3�{D4{D4�{D5{D5�{D6{D6�{D7{D7�{D8{D8�{D9{D9�{D:{D:�{D;{D;�{D<{D<�{D={D=�{D>{D>�{D?{D?�{D@{D@�{DA{DA�{DB{DB�{DC{DC�{DD{DD�{DE{DE�{DF{DF�{DG{DG�{DH{DH�{DI{DI�{DJ{DJ�{DK{DK�{DL{DL�{DM{DM�{DN{DN�{DO{DO�{DP{DP�{DQDQ�{DR{DR�{DS{DS�{DT{DT�{DU{DU�{DV{DV�{DW{DW�{DX{DX�{DY{DY�{DZ{DZ�{D[{D[�{D\{D\�{D]{D]�{D^{D^�{D_{D_�{D`{D`�{Da{Da�{Db{Db�{Dc{Dc�{Dd{Dd�{De{De�{Df{Df�{Dg{Dg�{Dh{Dh�{Di{Di�{Dj{Dj�{Dk{Dk�{Dl{Dl�{Dm{Dm�{Dn{Dn�{Do{Do�{Dp{Dp�{Dq{Dq�{Dr{Dr�DsDs�{Dt{Dt��Dy��D��D�C�D��pD���D��D�C�D��
D��pD� �D�S�D���D��D�=D�7
DڝpD��
D� �D�:=D�g
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A��/A��HA��HA��;A��#A��/A��A��#A��
A���A���A���A���A���A���A���A���A���A���A���A�ƨA���A�A�ĜA���A��\A��A��-A��yA���A�r�A�hsA�S�A��A���A�Q�A��A��A�  A�`BA�1'A��A��A�oA�%A�A�JA�A���A��mA��9A��DA�I�A��mA�C�A��7A��TA��!A���A�n�A�jA�  A�ȴA��A�9XA���A�t�A��jA�S�A���A�(�A�A�A���A��uA�|�A�E�A��
A�z�A�=qA��A�ZA�=qA�~�A�~�A�I�A�G�A��DA�$�A�9XA�ȴA�K�A�A���A�`BA�+A�K�A�+A~jA{ƨAz �Ay�Ay�^Ay�PAyhsAw��Av�!Av�As?}Ao�TAm�FAjE�Ah��Aet�Ad(�A`��A^�A\��A["�AYVAW�AWG�AW
=AV�`AW/AV��AU�#AUK�ASt�AQ�wAOx�AM��AL�ALI�AK;dAJ�AIXAH�`AH��AH-AF�yAFQ�AE�AE�^AD�ABQ�A@z�A?��A>��A>Q�A=O�A<ĜA<��A<A�A<JA:ĜA8ȴA7�A7S�A6$�A5A4$�A3XA2��A1�A0bNA/S�A.��A.jA.{A-�hA-�A,VA+�TA+�PA+�A*v�A)��A)&�A&��A&1A$��A#XA#"�A#VA"ȴA"1A!�A ��A ��A�^AK�A��A9XA��A�+A�TAffAAt�AVA�/A�HAn�AE�A�TA�A-Ap�A��A�uA=qA{AAl�A�A �A|�AĜAVA�A"�A�#A�7AK�A
�RA
  A~�AƨA�A��AƨA�7AXAoA��A9XA�yA-A��A�@���@�t�@�^5@�%@�@�r�@��;@�"�@��#@�1'@�^5@�@�  @�bN@�
=@�~�@��T@蛦@�"�@�&�@��m@�~�@�Z@��@�;d@�O�@�l�@���@և+@��@ԋD@��@��y@Ѻ^@���@�1@�"�@��@���@��@�1@��@ɑh@�7L@ȴ9@�r�@� �@�K�@�=q@�%@�I�@��@��-@��`@��R@��7@���@�+@�v�@��@�&�@��@�;d@�=q@�`B@���@���@��D@�Z@�1'@���@��P@��+@��7@��@�z�@��@�o@�ȴ@�~�@��-@��9@�I�@���@�^5@�J@��7@��D@�C�@��@�7L@��9@���@�@�M�@��@��u@�9X@�S�@��@���@�n�@��7@��u@���@��@��+@�M�@�@��T@��-@���@�`B@�V@�Ĝ@�bN@���@���@�+@�
=@���@��@��^@���@�x�@�O�@�O�@���@�I�@�A�@�1'@���@�l�@�33@��@��R@�ff@���@�X@�/@��@�%@�r�@���@�l�@�+@��y@��!@�E�@��T@�V@�Z@�9X@�1@��w@�33@�@��H@��@���@���@�n�@��@�@�X@���@��9@��@�Z@�9X@� �@� �@�1@��w@�;d@�
=@���@�V@�=q@�5?@�5?@�5?@�{@���@��#@��#@�?}@�V@���@�Q�@�A�@�9X@�1@���@���@�K�@�
=@���@��@�"�@���@��@�|�@���@���@�ȴ@���@�~�@�~�@��\@��@��!@��T@�p�@��@���@��@��`@��@���@�%@�V@�%@���@��D@�9X@�(�@�@|�@
=@~�R@~��@}�-@|�@{�m@{ƨ@{��@{S�@{"�@z��@zn�@y�@y��@yx�@y7L@u�h@ihs@d(�@]V@Vȴ@P�`@JJ@BJ@97L@3"�@-�T@(  @#dZ@9X@+@n�@
=@�@�;@�/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A���A���A��/A��HA��HA��;A��#A��/A��A��#A��
A���A���A���A���A���A���A���A���A���A���A���A�ƨA���A�A�ĜA���A��\A��A��-A��yA���A�r�A�hsA�S�A��A���A�Q�A��A��A�  A�`BA�1'A��A��A�oA�%A�A�JA�A���A��mA��9A��DA�I�A��mA�C�A��7A��TA��!A���A�n�A�jA�  A�ȴA��A�9XA���A�t�A��jA�S�A���A�(�A�A�A���A��uA�|�A�E�A��
A�z�A�=qA��A�ZA�=qA�~�A�~�A�I�A�G�A��DA�$�A�9XA�ȴA�K�A�A���A�`BA�+A�K�A�+A~jA{ƨAz �Ay�Ay�^Ay�PAyhsAw��Av�!Av�As?}Ao�TAm�FAjE�Ah��Aet�Ad(�A`��A^�A\��A["�AYVAW�AWG�AW
=AV�`AW/AV��AU�#AUK�ASt�AQ�wAOx�AM��AL�ALI�AK;dAJ�AIXAH�`AH��AH-AF�yAFQ�AE�AE�^AD�ABQ�A@z�A?��A>��A>Q�A=O�A<ĜA<��A<A�A<JA:ĜA8ȴA7�A7S�A6$�A5A4$�A3XA2��A1�A0bNA/S�A.��A.jA.{A-�hA-�A,VA+�TA+�PA+�A*v�A)��A)&�A&��A&1A$��A#XA#"�A#VA"ȴA"1A!�A ��A ��A�^AK�A��A9XA��A�+A�TAffAAt�AVA�/A�HAn�AE�A�TA�A-Ap�A��A�uA=qA{AAl�A�A �A|�AĜAVA�A"�A�#A�7AK�A
�RA
  A~�AƨA�A��AƨA�7AXAoA��A9XA�yA-A��A�@���@�t�@�^5@�%@�@�r�@��;@�"�@��#@�1'@�^5@�@�  @�bN@�
=@�~�@��T@蛦@�"�@�&�@��m@�~�@�Z@��@�;d@�O�@�l�@���@և+@��@ԋD@��@��y@Ѻ^@���@�1@�"�@��@���@��@�1@��@ɑh@�7L@ȴ9@�r�@� �@�K�@�=q@�%@�I�@��@��-@��`@��R@��7@���@�+@�v�@��@�&�@��@�;d@�=q@�`B@���@���@��D@�Z@�1'@���@��P@��+@��7@��@�z�@��@�o@�ȴ@�~�@��-@��9@�I�@���@�^5@�J@��7@��D@�C�@��@�7L@��9@���@�@�M�@��@��u@�9X@�S�@��@���@�n�@��7@��u@���@��@��+@�M�@�@��T@��-@���@�`B@�V@�Ĝ@�bN@���@���@�+@�
=@���@��@��^@���@�x�@�O�@�O�@���@�I�@�A�@�1'@���@�l�@�33@��@��R@�ff@���@�X@�/@��@�%@�r�@���@�l�@�+@��y@��!@�E�@��T@�V@�Z@�9X@�1@��w@�33@�@��H@��@���@���@�n�@��@�@�X@���@��9@��@�Z@�9X@� �@� �@�1@��w@�;d@�
=@���@�V@�=q@�5?@�5?@�5?@�{@���@��#@��#@�?}@�V@���@�Q�@�A�@�9X@�1@���@���@�K�@�
=@���@��@�"�@���@��@�|�@���@���@�ȴ@���@�~�@�~�@��\@��@��!@��T@�p�@��@���@��@��`@��@���@�%@�V@�%@���@��D@�9X@�(�@�@|�@
=@~�R@~��@}�-@|�@{�m@{ƨ@{��@{S�@{"�@z��@zn�@y�@y��@yx�@y7L@u�h@ihs@d(�@]V@Vȴ@P�`@JJ@BJ@97L@3"�@-�T@(  @#dZ@9X@+@n�@
=@�@�;@�/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBW
BW
BW
BVBR�BI�B?}B7LB33B,B&�B�B�B�B{BoBhB\B+B	7B	7B
=B
=B
=B
=BhB{BuBoBJB	7B	7BBB�B��B�BQ�B=qB1'B+B'�B#�B�B�B1B��B��B�B�fB�B��B��B��BȴBB�jB�LB�!B��B�%Bx�BiyBdZBT�BE�B.BuB
��B
�B
��B
�wB
�RB
�'B
��B
�DB
iyB
R�B
J�B
K�B
L�B
N�B
O�B
@�B
33B
(�B
\B	�B	�B	�qB	�-B	��B	�PB	r�B	aHB	W
B	J�B	>wB	5?B	49B	5?B	9XB	B�B	D�B	=qB	:^B	'�B	�B	JB	%B	B��B��B��B��B��B	  B��B��B��B�B�B�yB�5B�B�#B�B�
B��B��B��B�)B�#B�
B��B��B��B��B��BȴBĜB�}B�XB�LB�-B�!B�B�B��B��B��B��B��B��B��B��B��B�oB�VB�=B�+B�+B�%B�B�B�B� B}�B}�B� B}�Bz�Bo�Bk�BiyBdZBdZBdZBgmBjBjBl�Bl�BjBhsBffBe`BcTBaHB`BB_;B^5B\)BZBYBXBW
BT�BS�BP�BO�BM�BL�BJ�BF�BC�B@�B@�B?}B>wB=qB=qB<jB;dB9XB7LB6FB5?B33B33B2-B1'B/B-B,B+B)�B'�B&�B&�B%�B%�B#�B"�B"�B!�B�B�B�B�B�B�B�B�B�B �B!�B!�B �B!�B"�B!�B"�B#�B#�B$�B%�B%�B%�B%�B&�B(�B(�B(�B(�B(�B(�B)�B+B,B-B.B.B1'B33B49B6FB7LB8RB9XB<jB>wB@�BC�BD�BE�BE�BF�BF�BG�BG�BJ�BM�BN�BO�BR�BT�BT�BVBW
B[#B\)B]/BcTBdZBdZBgmBk�Bp�Bs�Bt�Bx�Bz�B}�B�B�B�B�+B�+B�+B�1B�=B�VB�oB��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�3B�?B�?B�LB�XB�dB�dB�}BȴB��B��B��B��B��B�B�B�#B�/B�NB�TB�TB�TB�NB�ZB�B�B�B�B��B��B��B��B��B��B��B��B	B	B	B	B	B	B	%B		7B	JB	VB	\B	bB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	&�B	+B	-B	-B	-B	-B	-B	.B	2-B	5?B	7LB	7LB	7LB	7LB	7LB	7LB	;dB	A�B	D�B	D�B	E�B	I�B	N�B	R�B	S�B	VB	W
B	XB	YB	ZB	ZB	[#B	aHB	cTB	bNB	cTB	e`B	gmB	iyB	jB	l�B	n�B	o�B	p�B	q�B	q�B	u�B	y�B	z�B	|�B	~�B	�B	�B	�B	�%B	�=B	�PB	�VB	�\B	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	�mB	��B
%B
uB
�B
!�B
1'B
9XB
@�B
G�B
M�B
VB
\)B
bNB
e`B
jB
n�B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BW�BW�BW�BW�BW�BW�BW�BW�BW�BW�BW�BW�BW�BW�BW�BW�BW�BW�BW�BW�BW�BW�BW�BW�BW�BV�BV�BV�BU�BR�BI�B?cB74B3B+�B&�B�B�BuB_BTBLBABB	B	B
"B
"B
"B
"BNB`BTBTB1B	B	BB �B�B��B��BQ�B=TB1B*�B'�B#�B�BiBB��B��B�B�IB��B��BλB̯BȚB�qB�JB�.B�B�nB�Bx�BiYBd=BT�BE�B-�BWB
��B
�_B
��B
�[B
�7B
�B
��B
�'B
i]B
R�B
J�B
K�B
L�B
N�B
O�B
@jB
3B
(�B
EB	��B	��B	�ZB	�B	��B	�:B	r�B	a3B	V�B	J�B	>dB	5-B	4'B	5,B	9BB	B~B	D�B	=]B	:KB	'�B	�B	5B	B	�B��B��B��B��B��B��B��B��B��B�B�B�iB�%B�B�B�B��B��B��B��B�B�B��B��B��B��B��B˵BȥBčB�lB�HB�=B�B�B�B��B��B��B��B��B��B��B��B��B��B�`B�FB�0B�B�B�B�B�B��B�B}�B}�B�B}�Bz�Bo�BkyBilBdKBdKBdJBgaBjoBjsBlzBl{BjsBheBfXBeOBcFBa:B`3B_+B^&B\BZBY	BXBV�BT�BS�BP�BO�BM�BL�BJ�BF�BC�B@vB@uB?qB>hB=cB=dB<\B;VB9/B7>B66B53B3$B3'B2 B1B/B,�B+�B*�B)�B'�B&�B&�B%�B%�B#�B"�B"�B!�B�B�B�B�B�B�B�B�B�B �B!�B!�B �B!�B"�B!�B"�B#�B#�B$�B%�B%�B%�B%�B&�B(�B(�B(�B(�B(�B(�B)�B*�B+�B,�B.B.B1B3#B4)B63B7;B8BB9HB<XB>gB@tBC�BD�BE�BE�BF�BF�BG�BG�BJ�BM�BN�BO�BR�BT�BT�BU�BV�B[B\B]BcABdEBdEBgXBkqBp�Bs�Bt�Bx�Bz�B}�B��B�B�B�B�B�B�B�'B�BB�XB�vB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�(B�'B�3B�@B�LB�MB�eBȜB˯B˯B˯B��B��B��B��B�B�B�5B�:B�9B�9B�6B�@B�jB�uB�B�B��B��B��B��B��B��B��B��B	�B	�B	�B	B	B	B	B		B	-B	<B	BB	FB	XB	^B	dB	kB	kB	iB	oB	�B	�B	�B	"�B	&�B	*�B	,�B	,�B	,�B	,�B	,�B	-�B	2B	5!B	7.B	7.B	7.B	7.B	7-B	7,B	;FB	AlB	D�B	D~B	E�B	I�B	N�B	R�B	S�B	U�B	V�B	W�B	X�B	Y�B	Y�B	[B	a*B	c5B	b-B	c6B	e>B	gNB	iYB	j_B	lkB	nyB	o~B	p�B	q�B	q�B	u�B	y�B	z�B	|�B	~�B	��B	��B	��B	�B	�B	�-B	�5B	�;B	�EB	�HB	�UB	�^B	�kB	�sB	�{B	��B	��B	��B	�JB	��B
B
PB
|B
!�B
1 B
92B
@_B
G�B
M�B
U�B
\B
b(B
e<B
jYB
nrB
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.32 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708292016053117082920160531170829  AO  ARCAADJP                                                                    20160208031531    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160208031531  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160208031531  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170829  IP                  G�O�G�O�G�O�                