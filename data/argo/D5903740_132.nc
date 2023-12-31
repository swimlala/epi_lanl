CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-11-07T20:16:08Z AOML 3.0 creation; 2016-06-01T00:08:27Z UW 3.1 conversion     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20151107201608  20160531170827  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055_7112_132                   2C  D   APEX                            5374                            041511                          846 @�|�GM�1   @�|�ڤ�@;'�z�H�cݩ��l�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @���@�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy�3D�3D�I�D��fD�� D� D�I�D��3D�� D�  D�0 D�ffD��fD�  D�C3Dڃ3D�ٚD�	�D�<�D�y�D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@�=qA�A#�AE�Ae�A��\A��\A��\A��\A\Aҏ\A�\A�\BG�B	G�BG�BG�B!G�B)G�B1G�B9G�BAG�BIG�BQG�BYG�BaG�BiG�BqG�ByG�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B��
B���B���Bģ�Bȣ�Ḅ�BУ�Bԣ�Bأ�Bܣ�B��B��B��B��B��B���B���B���C Q�CQ�CQ�CQ�CQ�C
Q�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�C Q�C"Q�C$Q�C&Q�C(Q�C*Q�C,Q�C.Q�C0Q�C2Q�C4Q�C6Q�C8Q�C:Q�C<Q�C>Q�C@Q�CBQ�CDQ�CFQ�CHQ�CJQ�CLQ�CNQ�CPQ�CRQ�CTQ�CVQ�CXQ�CZQ�C\Q�C^Q�C`Q�CbQ�CdQ�CfQ�ChQ�CjQ�ClQ�CnQ�CpQ�CrQ�CtQ�CvQ�CxQ�CzQ�C|Q�C~Q�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�D {D �{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D	{D	�{D
{D
�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D {D �{D!{D!�{D"{D"�{D#{D#�{D${D$�{D%{D%�{D&{D&�{D'{D'�{D({D(�{D){D)�{D*{D*�{D+{D+�{D,{D,�{D-{D-�{D.{D.�{D/{D/�{D0{D0�{D1{D1�{D2{D2�{D3{D3�{D4{D4�{D5{D5�{D6{D6�{D7{D7�{D8{D8�{D9{D9�{D:{D:�{D;{D;�{D<{D<�{D={D=�{D>{D>�{D?{D?�{D@{D@�{DA{DA�{DB{DB�{DC{DC�{DD{DD�{DE{DE�{DF{DF�{DG{DG�{DH{DH�{DI{DI�{DJ{DJ�{DK{DK�{DL{DL�{DM{DM�{DN{DN�{DO{DO�{DP{DP�{DQ{DQ�{DR{DR�{DS{DS�{DT{DT�{DU{DU�{DV{DV�{DW{DW�{DX{DX�{DY{DY�{DZ{DZ�{D[{D[�{D\{D\�{D]{D]�{D^{D^�{D_{D_�{D`{D`�{Da{Da�{Db{Db�{Dc{Dc�{Dd{Dd�{De{De�{Df{Df�{Dg{Dg�{Dh{Dh�{Di{Di�{Dj{Dj�{Dk{Dk�{Dl{Dl�{Dm{Dm�{Dn{Dn�{Do{Do�{Dp{Dp�{Dq{Dq�{Dr{Dr�{Ds{Ds�{Dt{Dt�{Dt�HDy��D�pD�S�D���D��=D�=D�S�D��pD��=D�
=D�:=D�p�D�УD�
=D�MpDڍpD���D��D�G
D��D��=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�VA�VA�
=A�JA�&�A�&�A�(�A�(�A�+A�+A�+A�/A�/A�1'A�/A�1'A�1'A�1'A�1'A�1'A�33A�(�A�oA��TA�v�A�  A�E�Aɝ�A�;dA���AēuA��A�5?A�ȴA�`BA��RA�G�A�-A�A�A��uA���A��yA��\A��A�  A�?}A���A��`A��PA��!A�/A���A��
A���A���A��A��;A��A�E�A���A�M�A��wA���A��yA�(�A�7LA��PA���A�
=A��A�I�A���A���A��wA��RA�~�A���A��A��AAp�A~�/A~A|M�A{G�Az^5Ax��Av�`Au�AtĜAs�7As"�ArffAq�wAp��Ao��An^5Am�Al1AkhsAj��Aj�DAj1Ai+Ag�Af��Afv�Af9XAf{Ae�hAd��Ac�Ab9XAa�^AaC�AaoA`ȴA_p�A^z�A^E�A^$�A^{A^bA]��A]�hA[��AX��AWl�AVM�AV�AU��AU�ASp�AR��AQ33APr�AO�mAOp�AO&�AN��AN9XAK�mAK`BAJ��AI�mAIK�AH�9AG��AD��AD1AD  AC�FAC"�AB  AA&�A@I�A?C�A=A<�!A:~�A9?}A8��A8�A8M�A6�9A5�FA4jA3�A3��A2ZA0��A/�A/��A.��A-A,��A,~�A+l�A*�RA)�-A)G�A'G�A%�A${A#"�A!S�A VA $�A�A��A�;AM�A��AS�A��AbNA�+A?}A�A�AA�`Ar�A�-A�PAdZA�+Ax�A�AQ�A�/A�A��A�A��AA\)A
�`A
 �A�AƨA��A�PA|�AK�A��A^5A  AK�A~�A5?A�!A�Ahs@��
@���@��+@���@���@�j@��w@��@���@�^5@�-@��@�Z@��@�&�@���@�^5@�j@���@��`@�R@�7L@��@��m@�t�@�33@�+@�V@��@��`@�=q@�@��/@�  @�K�@�$�@�/@� �@֗�@�1'@��@�x�@�V@д9@���@̬@˾w@�E�@ȋD@Ǖ�@Ɵ�@��@���@��@��@���@�l�@��+@���@�?}@��@�dZ@��!@��+@�=q@�@�O�@���@��@���@��F@��@�X@���@� �@�ƨ@���@��@�@���@�=q@�{@���@��@�dZ@��@��m@��@�|�@�33@�ȴ@�=q@���@���@�1'@���@�V@�-@�{@��@�$�@��h@�?}@��@���@�1@���@�ƨ@��w@��@�K�@�@�@�@�@���@���@�ff@���@�O�@��`@��y@�@�V@��j@��@��@��u@�I�@�b@���@�\)@�33@�
=@���@���@���@�5?@��T@��#@��#@��#@�@�hs@�&�@��;@�x�@��@�j@�1'@�  @�ƨ@���@��@�@�@���@��@�X@��@���@�9X@��@���@�K�@�;d@��@���@�E�@��T@��#@��^@��h@��7@��h@��@���@���@��@�O�@�/@���@���@��j@�S�@��!@�n�@�ff@�v�@�v�@�~�@�ȴ@�S�@��@���@��@��u@�9X@��@��@�1@��@�"�@�K�@�K�@�dZ@�dZ@�dZ@�\)@�dZ@�  @�K�@��@��@��^@���@��^@��^@��-@���@���@���@���@���@���@��-@��-@��7@�/@��/@�r�@� �@�  @��@�ƨ@���@�dZ@�C�@��y@��R@�n�@�-@���@��@���@��h@��@�X@�&�@��`@���@��9@���@�1'@��@{ƨ@qx�@j~�@b-@Z�@T1@M�@F�y@A�@;dZ@7\)@333@,9X@&��@+@��@�@��@�;@��@	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�VA�VA�
=A�JA�&�A�&�A�(�A�(�A�+A�+A�+A�/A�/A�1'A�/A�1'A�1'A�1'A�1'A�1'A�33A�(�A�oA��TA�v�A�  A�E�Aɝ�A�;dA���AēuA��A�5?A�ȴA�`BA��RA�G�A�-A�A�A��uA���A��yA��\A��A�  A�?}A���A��`A��PA��!A�/A���A��
A���A���A��A��;A��A�E�A���A�M�A��wA���A��yA�(�A�7LA��PA���A�
=A��A�I�A���A���A��wA��RA�~�A���A��A��AAp�A~�/A~A|M�A{G�Az^5Ax��Av�`Au�AtĜAs�7As"�ArffAq�wAp��Ao��An^5Am�Al1AkhsAj��Aj�DAj1Ai+Ag�Af��Afv�Af9XAf{Ae�hAd��Ac�Ab9XAa�^AaC�AaoA`ȴA_p�A^z�A^E�A^$�A^{A^bA]��A]�hA[��AX��AWl�AVM�AV�AU��AU�ASp�AR��AQ33APr�AO�mAOp�AO&�AN��AN9XAK�mAK`BAJ��AI�mAIK�AH�9AG��AD��AD1AD  AC�FAC"�AB  AA&�A@I�A?C�A=A<�!A:~�A9?}A8��A8�A8M�A6�9A5�FA4jA3�A3��A2ZA0��A/�A/��A.��A-A,��A,~�A+l�A*�RA)�-A)G�A'G�A%�A${A#"�A!S�A VA $�A�A��A�;AM�A��AS�A��AbNA�+A?}A�A�AA�`Ar�A�-A�PAdZA�+Ax�A�AQ�A�/A�A��A�A��AA\)A
�`A
 �A�AƨA��A�PA|�AK�A��A^5A  AK�A~�A5?A�!A�Ahs@��
@���@��+@���@���@�j@��w@��@���@�^5@�-@��@�Z@��@�&�@���@�^5@�j@���@��`@�R@�7L@��@��m@�t�@�33@�+@�V@��@��`@�=q@�@��/@�  @�K�@�$�@�/@� �@֗�@�1'@��@�x�@�V@д9@���@̬@˾w@�E�@ȋD@Ǖ�@Ɵ�@��@���@��@��@���@�l�@��+@���@�?}@��@�dZ@��!@��+@�=q@�@�O�@���@��@���@��F@��@�X@���@� �@�ƨ@���@��@�@���@�=q@�{@���@��@�dZ@��@��m@��@�|�@�33@�ȴ@�=q@���@���@�1'@���@�V@�-@�{@��@�$�@��h@�?}@��@���@�1@���@�ƨ@��w@��@�K�@�@�@�@�@���@���@�ff@���@�O�@��`@��y@�@�V@��j@��@��@��u@�I�@�b@���@�\)@�33@�
=@���@���@���@�5?@��T@��#@��#@��#@�@�hs@�&�@��;@�x�@��@�j@�1'@�  @�ƨ@���@��@�@�@���@��@�X@��@���@�9X@��@���@�K�@�;d@��@���@�E�@��T@��#@��^@��h@��7@��h@��@���@���@��@�O�@�/@���@���@��j@�S�@��!@�n�@�ff@�v�@�v�@�~�@�ȴ@�S�@��@���@��@��u@�9X@��@��@�1@��@�"�@�K�@�K�@�dZ@�dZ@�dZ@�\)@�dZ@�  @�K�@��@��@��^@���@��^@��^@��-@���@���@���@���@���@���@��-@��-@��7@�/@��/@�r�@� �@�  @��@�ƨ@���@�dZ@�C�@��y@��R@�n�@�-@���@��@���@��h@��@�X@�&�@��`@���@��9@���@�1'@��@{ƨ@qx�@j~�@b-@Z�@T1@M�@F�y@A�@;dZ@7\)@333@,9X@&��@+@��@�@��@�;@��@	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBN�BN�BN�BN�BP�BO�BP�BP�BP�BP�BP�BP�BP�BP�BQ�BQ�BQ�BR�BT�BYB[#Be`Bw�B�PB�{B��B��B��B��B�uB~�BgmBP�B7LB�B+B��B��B�B�/BĜB�?B��BhsBZBR�BC�B,B�BbBVBJBB�B�B��B��B��BȴBÖB�dB�-B��B�B\)BL�B@�B49B&�B�B+BB
��B
�yB
��B
��B
�}B
�'B
��B
��B
��B
��B
��B
�JB
�B
}�B
q�B
dZB
]/B
W
B
P�B
N�B
J�B
D�B
=qB
5?B
+B
!�B
�B
�B
�B
{B
\B
+B	��B	�B	�B	�B	�B	�yB	�ZB	�
B	��B	ɺB	ƨB	ŢB	B	�dB	�FB	�9B	�9B	�3B	�-B	�!B	�B	��B	�B	z�B	v�B	v�B	v�B	r�B	gmB	bNB	]/B	XB	T�B	Q�B	O�B	L�B	F�B	<jB	:^B	6FB	6FB	9XB	6FB	,B	�B	hB	bB	VB	PB	DB	%B��B��B��B��B�B�mB�fB�ZB�;B�
B��B��BȴBƨB�}B�^B�LB�?B�3B�-B�B�B��B��B��B��B��B�uB�\B�DB�B�B�B�B� Bz�Bw�Bv�Bt�Bs�Bo�Bl�BiyBhsBhsBgmBffBe`Be`Be`BcTBcTBaHB`BB\)BT�BQ�BP�BO�BL�BI�BH�BF�BF�BF�BF�BF�BF�BE�BD�BD�BC�BB�B@�B?}B=qB;dB9XB6FB6FB5?B49B49B33B33B2-B1'B1'B1'B0!B/B-B.B-B+B)�B'�B'�B%�B$�B%�B$�B$�B$�B#�B"�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B#�B$�B%�B'�B'�B/B2-B2-B33B49B6FB5?B6FB7LB7LB7LB7LB8RB8RB:^B;dB;dB:^B<jBF�BG�BI�BI�BI�BJ�BJ�BK�BM�BM�BN�BQ�BYB_;BhsBjBjBk�Bk�Bl�Bn�Br�Bu�Bx�Bz�Bz�B{�B|�Bz�B}�B~�B~�B�B�B�%B�%B�%B�%B�+B�1B�1B�1B�1B�1B�+B�1B�1B�=B�=B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�jB�}B��B��BBBȴB��B��B�B�B�5B�BB�TB�`B�fB�fB�mB�B�B�B�B��B��B	B	B	B	+B		7B	PB	bB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	#�B	&�B	,B	-B	0!B	1'B	1'B	5?B	>wB	@�B	A�B	D�B	J�B	O�B	O�B	R�B	R�B	T�B	T�B	YB	`BB	bNB	aHB	aHB	cTB	dZB	e`B	e`B	ffB	ffB	ffB	gmB	hsB	k�B	p�B	v�B	z�B	{�B	~�B	�B	�B	�%B	�+B	�+B	�1B	�7B	�=B	�=B	�=B	�DB	�JB	�VB	�\B	�bB	�hB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	ŢB	�;B	�B
B
bB
�B
$�B
+B
2-B
7LB
;dB
C�B
J�B
S�B
ZB
`BB
dZB
gmB
l�B
p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BN�BN�BN�BN�BP�BO�BP�BP�BP�BP�BP�BP�BP�BP�BQ�BQ�BQ�BR�BT�BYB[	BeMBw�B�?B�gB��B��B�oB�lB�aB~�BgTBP�B77BrBB��B��B��B�B�B�%B�tBhTBZBR�BCxB+�BrBBB4B*B�B�B��B��BϿB̮BȗB�vB�GB�B��B��B\
BL�B@eB4B&�BbBB �B
��B
�[B
ϼB
˪B
�bB
�B
��B
��B
��B
��B
�pB
�/B
�B
}�B
q�B
d>B
]B
V�B
P�B
N�B
J�B
D�B
=UB
5#B
*�B
!�B
�B
yB
lB
`B
DB
B	��B	�B	�B	�B	�xB	�aB	�@B	��B	̳B	ɢB	ƐB	ŉB	�wB	�NB	�.B	� B	�!B	�B	�B	�	B	��B	�zB	�B	z�B	v�B	v�B	v�B	r�B	gXB	b8B	]B	W�B	T�B	Q�B	O�B	L�B	F�B	<SB	:HB	62B	60B	9DB	61B	+�B	tB	UB	NB	EB	=B	0B	B��B��B��B��B�~B�[B�TB�JB�)B��B��BʰBȥBƖB�mB�OB�;B�/B�"B�B�
B��B��B��B��B��B�rB�fB�MB�6B�B��B��B��B�Bz�Bw�Bv�Bt�Bs�Bo�Bl{BijBhfBhfBg]BfZBeQBeQBeSBcDBcFBa<B`4B\BT�BQ�BP�BO�BL�BI�BH�BF�BF�BF�BF�BF�BF�BE�BD�BD�BC�BB�B@uB?oB=dB;XB9KB67B68B51B4B4,B3$B3$B2!B1B1B1B0B/B-B.B- B*�B)�B'�B'�B%�B$�B%�B$�B$�B$�B#�B"�B!�B �B�B�B�B�B�B}B�B~B�B�B�B�B�B�B~B�B�B�B!�B#�B$�B%�B'�B'�B/B2B2B3"B4)B65B5-B65B79B7;B7:B7<B8AB8BB:KB;RB;SB:MB<WBF�BG�BI�BI�BI�BJ�BJ�BK�BM�BM�BN�BQ�BYB_&Bh_BjlBjkBksBksBlxBn�Br�Bu�Bx�Bz�Bz�B{�B|�Bz�B}�B~�B~�B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�(B�%B�VB�iB�vB�}B�~B�|B�vB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�RB�cB�rB�qB�vB�uBȚBξB��B��B��B�B�)B�<B�EB�LB�KB�RB�eB�fB�B�B��B��B	�B	�B	B	B		B	5B	DB	XB	eB	kB	oB	oB	�B	�B	�B	�B	�B	�B	!�B	"�B	#�B	&�B	+�B	,�B	0B	1B	1	B	5!B	>XB	@eB	AnB	DB	J�B	O�B	O�B	R�B	R�B	T�B	T�B	X�B	`"B	b1B	a'B	a*B	c6B	d:B	e>B	eAB	fFB	fFB	fGB	gOB	hUB	keB	p�B	v�B	z�B	{�B	~�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�"B	�)B	�7B	�9B	�@B	�DB	�MB	�MB	�UB	�^B	�`B	�eB	�mB	�sB	�~B	��B	��B	ŁB	�B	�B
�B
>B
uB
$�B
*�B
2
B
7)B
;AB
CrB
J�B
S�B
Y�B
`B
d4B
gFB
lfB
p}111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.32 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708272016053117082720160531170827  AO  ARCAADJP                                                                    20151107201608    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151107201608  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151107201608  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170827  IP                  G�O�G�O�G�O�                