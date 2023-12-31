CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:35Z AOML 3.0 creation; 2016-06-01T00:08:15Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230835  20160531170815  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               ;A   AO  4055_7112_059                   2C  D   APEX                            5374                            041511                          846 @���|e�1   @���l��@:=/��w�c�A�7K�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ;A   B   B   @���@�  A   A   A@  A^ffA~ffA�  A�  A�  A�33A�  A�  A�  B   B  B  B  B ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,�fD-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D��D�C3D���D���D�	�D�9�D�� D�� D�fD�,�D�� D��3D���D�33D�y�D��D��D�9�D�3D�ɚ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@�=qA�A%�AE�Ac�A�A��\A��\A��\A�Aҏ\A�\A�\BG�B	G�BG�BG�B!�B)G�B1G�B9G�BAG�BIG�BQG�BYG�BaG�BiG�BqG�ByG�B��
B�p�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bģ�Bȣ�Ḅ�BУ�Bԣ�Bأ�Bܣ�B��B��B��B��B��B���B���B���C Q�CQ�CQ�CQ�CQ�C
Q�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�C Q�C"Q�C$Q�C&Q�C(Q�C*Q�C,Q�C.Q�C0Q�C2Q�C4Q�C6Q�C8Q�C:Q�C<Q�C>Q�C@Q�CBQ�CDQ�CFQ�CHQ�CJQ�CLQ�CNQ�CPQ�CRQ�CTQ�CVQ�CXQ�CZQ�C\Q�C^Q�C`Q�CbQ�CdQ�CfQ�ChQ�CjQ�ClQ�CnQ�CpQ�CrQ�CtQ�CvQ�CxQ�CzQ�C|Q�C~Q�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�D {D �{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D	{D	�{D
{D
�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D {D �{D!{D!�{D"{D"�{D#{D#�{D${D$�{D%{D%�{D&{D&�{D'{D'�{D({D(�{D){D)�{D*{D*�{D+{D+�{D,{D,��D-{D-�{D.{D.�{D/{D/�{D0{D0�{D1{D1�{D2{D2�{D3{D3�{D4{D4�{D5{D5�{D6{D6�{D7{D7�{D8{D8�{D9{D9�{D:{D:�{D;{D;�{D<{D<�{D={D=�{D>{D>�{D?{D?�{D@{D@�{DA{DA�{DB{DB�{DC{DC�{DD{DD�{DE{DE�{DF{DF�{DG{DG�{DH{DH�{DI{DI�{DJ{DJ�{DK{DK�{DL{DL�{DM{DM�{DN{DN�{DO{DO�{DP{DP�{DQ{DQ�{DR{DR�{DS{DS�{DT{DT�{DU{DU�{DV{DV�{DW{DW�{DX{DX�{DY{DY�{DZ{DZ�{D[{D[�{D\{D\�{D]{D]�{D^{D^�{D_{D_�{D`{D`�{Da{Da�{Db{Db�{Dc{Dc�{Dd{Dd�{De{De�{Df{Df�{Dg{Dg�{Dh{Dh�{Di{Di�{Dj{Dj�{Dk{Dk�{Dl{Dl�{Dm{Dm�{Dn{Dn�{Do{Do�{Dp{Dp�{Dq{Dq�{Dr{Dr�{Ds{Ds�{Dt{Dt�{Dt��Dy�HD�'
D�MpD��
D���D��D�C�D��=D��=D��D�7
D��=D��pD�
D�=pDڃ�D��
D�
D�C�D�pD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A�oA�oA�oA��A�bA��A��A��A��A�"�A�"�A�"�A�$�A�&�A�&�A�$�A�&�A�+A�(�A�(�A�+A�+A�-A��
A���A�l�A�p�A�7LA���A��A���A�(�A���A�A���A��A�1A���A�bNA��\A�"�A�9XA��TA��FA�;dA�S�A��A�O�A���A��TA�p�A�5?A��yA���A���A���A���A��#A�t�A�&�A�r�A��A�(�A��wA��9A��9A��/A�(�A���A��`A�A�A��A�VA�=qA��A�"�A�~�A�ZA���A���A���A�"�A�9XA��
A���A�
=A�$�A�l�A�bA��A��
A���A�5?A�JA�M�A��A��A�r�A���A���A}�A|VAw�AvJAuG�At��ArI�Aqx�Ao��AmoAkVAjr�Ai�Ai?}Ag�7Afz�Ad��A`�9A_�-A_�A\�yA[VAY+AW�#AVZAT�jAT(�AS��AR��AQO�AO��ANA�AM�hAL�AK"�AI�TAH�+AE��ACC�AB�jAA�A?�
A>{A;�#A:��A:��A:(�A9G�A8�A7ƨA7;dA6��A6n�A6E�A6�A5�hA4�HA3�^A3VA2  A1�A0��A0bNA0�A/;dA.ȴA-�
A-`BA,ȴA,�\A,ffA+/A)hsA(z�A(9XA'�A'�A&�/A&��A&ZA%�FA%XA$n�A#&�A"I�A!|�A VA�yA��A��Av�A�A�A7LAr�A�mA��Al�A�/Ap�A��AjA-A�PA�
AG�A�A��AJAn�A=qA��A`BAVA
��A
�A�A��An�A\)AI�At�A�yA�A �+@��R@�n�@��@�  @�$�@���@���@���@� �@�@�(�@�"�@�+@�r�@�A�@�S�@�R@�^5@���@���@���@�\)@�!@��@�j@�C�@��@��D@�Z@�|�@�$�@ݡ�@ܼj@�Q�@��@۶F@�
=@�~�@��@ى7@�X@�j@ա�@ӍP@Ѻ^@ϝ�@���@��@�x�@�&�@̃@˥�@�`B@Ǖ�@�n�@��T@�7L@ļj@Õ�@�ȴ@�5?@�V@��@�5?@��`@�n�@�V@��y@���@�bN@� �@���@��@�K�@�@���@�-@�dZ@��@�+@���@���@���@���@��\@�n�@�5?@�@���@��/@��!@�@��h@�/@���@��@���@�|�@�dZ@�S�@�S�@�S�@�C�@�
=@�@��m@��@�V@��-@�?}@���@�I�@��@�ff@��@�X@��/@���@�j@� �@���@�ȴ@���@��@��`@���@��u@�I�@�1'@� �@���@�ƨ@��P@�\)@�33@��@�@��@��H@�ȴ@���@�5?@��@���@��-@�G�@��D@��m@��H@�~�@�V@�-@��@��^@��@�X@��@��/@���@��u@�1'@��@��@�t�@�33@���@���@�E�@��@���@��^@���@���@���@���@��7@�hs@�?}@�&�@�V@��@���@�Ĝ@��9@���@�I�@��P@�o@���@��@���@�v�@�^5@�-@�&�@���@�Ĝ@��@���@���@��u@�z�@�Q�@�1'@�1@���@��R@�ff@�V@�=q@�$�@�J@���@��#@���@�x�@�O�@�&�@�%@��@��`@��/@��9@�z�@�I�@� �@�1@�;@�@~�R@~{@}@}��@}O�@}�@|�@|j@|I�@{��@{��@{��@z~�@y��@y�@y��@y��@yx�@yG�@y7L@y�@y%@x��@x��@x�9@x��@x�u@xr�@xQ�@x1'@x �@w��@w�P@vȴ@s�m@g�w@a��@V��@L1@G�w@Ahs@:=q@7|�@1�@+S�@'�@!��@9X@��@/@hs@t�@	�@?}@-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��A�oA�oA�oA��A�bA��A��A��A��A�"�A�"�A�"�A�$�A�&�A�&�A�$�A�&�A�+A�(�A�(�A�+A�+A�-A��
A���A�l�A�p�A�7LA���A��A���A�(�A���A�A���A��A�1A���A�bNA��\A�"�A�9XA��TA��FA�;dA�S�A��A�O�A���A��TA�p�A�5?A��yA���A���A���A���A��#A�t�A�&�A�r�A��A�(�A��wA��9A��9A��/A�(�A���A��`A�A�A��A�VA�=qA��A�"�A�~�A�ZA���A���A���A�"�A�9XA��
A���A�
=A�$�A�l�A�bA��A��
A���A�5?A�JA�M�A��A��A�r�A���A���A}�A|VAw�AvJAuG�At��ArI�Aqx�Ao��AmoAkVAjr�Ai�Ai?}Ag�7Afz�Ad��A`�9A_�-A_�A\�yA[VAY+AW�#AVZAT�jAT(�AS��AR��AQO�AO��ANA�AM�hAL�AK"�AI�TAH�+AE��ACC�AB�jAA�A?�
A>{A;�#A:��A:��A:(�A9G�A8�A7ƨA7;dA6��A6n�A6E�A6�A5�hA4�HA3�^A3VA2  A1�A0��A0bNA0�A/;dA.ȴA-�
A-`BA,ȴA,�\A,ffA+/A)hsA(z�A(9XA'�A'�A&�/A&��A&ZA%�FA%XA$n�A#&�A"I�A!|�A VA�yA��A��Av�A�A�A7LAr�A�mA��Al�A�/Ap�A��AjA-A�PA�
AG�A�A��AJAn�A=qA��A`BAVA
��A
�A�A��An�A\)AI�At�A�yA�A �+@��R@�n�@��@�  @�$�@���@���@���@� �@�@�(�@�"�@�+@�r�@�A�@�S�@�R@�^5@���@���@���@�\)@�!@��@�j@�C�@��@��D@�Z@�|�@�$�@ݡ�@ܼj@�Q�@��@۶F@�
=@�~�@��@ى7@�X@�j@ա�@ӍP@Ѻ^@ϝ�@���@��@�x�@�&�@̃@˥�@�`B@Ǖ�@�n�@��T@�7L@ļj@Õ�@�ȴ@�5?@�V@��@�5?@��`@�n�@�V@��y@���@�bN@� �@���@��@�K�@�@���@�-@�dZ@��@�+@���@���@���@���@��\@�n�@�5?@�@���@��/@��!@�@��h@�/@���@��@���@�|�@�dZ@�S�@�S�@�S�@�C�@�
=@�@��m@��@�V@��-@�?}@���@�I�@��@�ff@��@�X@��/@���@�j@� �@���@�ȴ@���@��@��`@���@��u@�I�@�1'@� �@���@�ƨ@��P@�\)@�33@��@�@��@��H@�ȴ@���@�5?@��@���@��-@�G�@��D@��m@��H@�~�@�V@�-@��@��^@��@�X@��@��/@���@��u@�1'@��@��@�t�@�33@���@���@�E�@��@���@��^@���@���@���@���@��7@�hs@�?}@�&�@�V@��@���@�Ĝ@��9@���@�I�@��P@�o@���@��@���@�v�@�^5@�-@�&�@���@�Ĝ@��@���@���@��u@�z�@�Q�@�1'@�1@���@��R@�ff@�V@�=q@�$�@�J@���@��#@���@�x�@�O�@�&�@�%@��@��`@��/@��9@�z�@�I�@� �@�1@�;@�@~�R@~{@}@}��@}O�@}�@|�@|j@|I�@{��@{��@{��@z~�@y��@y�@y��@y��@yx�@yG�@y7L@y�@y%@x��@x��@x�9@x��@x�u@xr�@xQ�@x1'@x �@w��@w�PG�O�@s�m@g�w@a��@V��@L1@G�w@Ahs@:=q@7|�@1�@+S�@'�@!��@9X@��@/@hs@t�@	�@?}@-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BE�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BE�BD�BC�B'�B�TB� BhsBW
BN�BK�BB�B<jB7LB5?B1'B)�B"�B �B�B�BbBDBVBoB�B,B,B�BJB
=BB�B�`B��BB��B��B��B�`BɺB�dB�?B��B�Br�BiyBe`BVBG�BJ�B<jB&�B�B�B��B�^B��B�=Br�BdZBQ�BK�BJ�B>wB,B�BuB  B
�B
�#B
�
B
��B
ǮB
�XB
�uB
q�B
T�B
?}B
#�B
�B	��B	�yB	�TB	�)B	��B	ŢB	�LB	��B	��B	�{B	�bB	�7B	|�B	t�B	e`B	O�B	I�B	D�B	9XB	.B	#�B	�B	oB	
=B	%B	B��B��B�B�fB�NB�5B�B��B��B��B�dB�XB�?B�'B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�hB�\B�PB�JB�JB�=B�%B�B�B� B}�B{�B{�Bz�By�Bw�Bu�Br�Bo�Bl�BjBe`BcTBaHB_;B_;B]/B\)B[#BYBXBW
BVBS�BP�BO�BN�BL�BI�BG�BE�BC�B?}B:^B6FB49B2-B1'B0!B.B+B&�B$�B"�B �B�B�B�B�B�B�B{BuBoBbBbBVBPBDB
=B	7B	7B1B	7B1B1B+B+B+B%B%B%B%BBBBBBBBBBBBBBBBBBBBB%B+B	7B
=BDBJBDBJBJB\BbBoBoBuBuB�B�B�B�B�B�B�B �B!�B$�B&�B'�B'�B'�B'�B(�B)�B)�B'�B-B8RB9XB;dB;dB<jB=qB=qB>wB?}B?}B?}B?}BJ�BM�BP�BS�BT�BXBZB_;B`BBaHBaHB`BB`BBaHBiyBp�Bs�Bw�Bz�B~�B�B�%B�DB�uB��B��B��B��B��B��B��B��B�-B�FB�RB�XB�dB�qB�wB�wB�}B��BBÖBĜBŢBƨBƨBƨBǮBȴB��B��B��B��B��B�B�/B�`B�sB�yB�B�B�B�B�B�B�B�B��B��B��B��B��B	  B	B	B	+B	1B		7B	DB	JB	JB	JB	JB	PB	VB	\B	bB	hB	oB	uB	uB	{B	{B	�B	�B	"�B	#�B	$�B	%�B	'�B	'�B	(�B	33B	6FB	7LB	8RB	8RB	9XB	9XB	:^B	;dB	<jB	=qB	A�B	J�B	N�B	O�B	P�B	Q�B	Q�B	R�B	S�B	VB	XB	ZB	\)B	^5B	_;B	_;B	`BB	bNB	e`B	hsB	jB	k�B	l�B	m�B	p�B	r�B	t�B	u�B	w�B	x�B	{�B	}�B	}�B	� B	�B	�B	�7B	�JB	�JB	�PB	�VB	�\B	�bB	�bB	�hB	�hB	�oB	�oB	�oB	�uB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	�fB	��B
\B
�B
&�B
/B
33B
<jB
B�B
H�B
Q�B
VB
\)B
^5B
cTB
jB
m�B
t�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BE�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BE�BD�BC�B'�B�HB�Bh\BV�BN�BK�BBvB<RB74B5&B1B)�B"�B �B�BuBHB)B;BWB�B+�B+�BzB.B
 BB�B�EB��BB��B��B��B�DBɝB�FB�%B��B��Br�BiZBeBBU�BG�BJ�B<KB&�B�B�B��B�AB��B�Br�Bd=BQ�BK�BJ�B>YB+�B�BXB
��B
�tB
�B
��B
��B
ǐB
�<B
�YB
q�B
T�B
?bB
#�B
{B	��B	�bB	�=B	�B	̳B	ŋB	�5B	��B	�}B	�eB	�NB	�!B	|�B	t�B	eKB	O�B	I�B	D�B	9DB	.B	#�B	�B	\B	
*B	B	 B��B��B�~B�UB�=B�'B�B��BʳB�zB�UB�GB�2B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�wB�lB�dB�YB�NB�AB�;B�:B�.B�B��B��B�B}�B{�B{�Bz�By�Bw�Bu�Br�Bo�Bl|BjtBeTBcFBa:B_-B_/B] B\B[BYBXBV�BU�BS�BP�BO�BN�BL�BI�BG�BE�BC�B?qB:PB67B4-B2!B1B0B.B*�B&�B$�B"�B �B�B�BzBlBaBtBRBhBIB=BVB0B(B7B
B	B	BB	B	BBBBBB�B�B�B�B�BB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BB	'B
BB=B6B<B!BMB9BaB^BfBiBqBB~B�B�B�B�B �B!�B$�B&�B'�B'�B'�B'�B(�B)�B)�B'�B,�B8BB9GB;TB;RB<YB=aB=_B>eB?kB?lB?lB?jBJ�BM�BP�BS�BT�BW�BZ	B_%B`1Ba3Ba3B`.B`/Ba4BieBp�Bs�Bw�Bz�B~�B��B�B�0B�^B�pB��B��B��B��B��B��B��B�B�.B�7B�>B�JB�WB�]B�]B�eB�fB�uB�{BąBŉBƎBƏBƋBǔBȝB˭BͺBοB��B��B��B�B�FB�VB�_B�eB�qB�vB�B��B�B�B�B��B��B��B��B��B��B	 �B	�B	B	B		B	%B	0B	-B	.B	.B	3B	;B	@B	GB	MB	RB	XB	XB	aB	]B	oB	�B	"�B	#�B	$�B	%�B	'�B	'�B	(�B	3B	6'B	7+B	84B	85B	99B	99B	:@B	;GB	<JB	=SB	AkB	J�B	N�B	O�B	P�B	Q�B	Q�B	R�B	S�B	U�B	W�B	Y�B	\
B	^B	_B	_B	`"B	b/B	e@B	hSB	j_B	keB	lnB	mtB	p�B	r�B	t�B	u�B	w�B	x�B	{�B	}�B	}�B	�B	��B	��B	�B	�+B	�*B	�0B	�6B	�<B	�CB	�CB	�GB	�GB	�NB	�MB	�NB	�SB	�VB	�[B	�[B	�aB	�_B	�fB	�qG�O�B	��B	��B	�CB	��B
6B
yB
&�B
.�B
3B
<DB
BkB
H�B
Q�B
U�B
\B
^B
c/B
j\B
mkB
t�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.32 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708152016053117081520160531170815  AO  ARCAADJP                                                                    20140721230835    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230835  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230835  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170815  IP                  G�O�G�O�G�O�                