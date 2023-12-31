CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:45Z AOML 3.0 creation; 2016-06-01T00:08:18Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230845  20160531170818  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               NA   AO  4055_7112_078                   2C  D   APEX                            5374                            041511                          846 @��u @1   @�� ���@:Ƨ-�dhr� �1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    NA   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDy��D��D�FfD��fD���D�3D�FfD���D��fD��3D�9�D�|�D�ٚD�	�D�,�D�ffD���D���D�9�D�|�D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�p�@�=qA�A%�AE�Ae�A��\A��\A��\A��\A\Aҏ\A�\A�\BG�B	G�BG�BG�B!G�B)G�B1G�B9G�BAG�BIG�BQG�BYG�BaG�BiG�BqG�ByG�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bģ�Bȣ�Ḅ�BУ�Bԣ�Bأ�Bܣ�B��B��B��B��B��B���B���B���C Q�CQ�CQ�CQ�CQ�C
Q�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�C Q�C"Q�C$Q�C&Q�C(Q�C*Q�C,Q�C.Q�C0Q�C2Q�C4Q�C6Q�C8Q�C:Q�C<Q�C>Q�C@Q�CBQ�CDQ�CFQ�CHQ�CJQ�CLQ�CNQ�CPQ�CRQ�CTQ�CVQ�CXQ�CZQ�C\Q�C^Q�C`Q�CbQ�CdQ�CfQ�ChQ�CjQ�ClQ�CnQ�CpQ�CrQ�CtQ�CvQ�CxQ�CzQ�C|Q�C~Q�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�D {D �{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D	{D	�{D
{D
�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D {D �{D!{D!�{D"{D"�{D#{D#�{D${D$�{D%{D%�{D&{D&�{D'{D'�{D({D(�{D){D)�{D*{D*�{D+{D+�{D,{D,�{D-{D-�{D.{D.�{D/{D/�{D0{D0�{D1{D1�{D2{D2�{D3{D3�{D4{D4�{D5{D5�{D6{D6�{D7{D7�{D8{D8�{D9{D9�{D:{D:�{D;{D;�{D<{D<�{D={D=�{D>{D>�{D?{D?�{D@{D@�{DA{DA�{DB{DB�{DC{DC�{DD{DD�{DE{DE�{DF{DF�{DG{DG�{DH{DH�{DI{DI�{DJ{DJ�{DK{DK�{DL{DL�{DM{DM�{DN{DN�{DO{DO�{DP{DP�{DQ{DQ�{DR{DR�{DS{DS�{DT{DT�{DU{DU�{DV{DV�{DW{DW�{DX{DX�{DY{DY�{DZ{DZ�{D[{D[�{D\{D\�{D]{D]�{D^{D^�{D_{D_�{D`{D`�{Da{Da�{Db{Db�{Dc{Dc�{Dd{Dd�{De{De�{Df{Df�{Dg{Dg�{Dh{Dh�{Di{Di�{Dj{Dj�{Dk{Dk�{Dl{Dl�{Dm{Dm�{Dn{Dn�{Do{Do�{Dp{Dp�{Dq{Dq�{Dr{Dr�{Ds{Ds�{Dt{Dtz�Dy�HD�
D�P�D���D���D�pD�P�D���D�УD��pD�C�D��
D���D��D�7
D�p�D��
D��D�C�D�
D��p11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�O�A�K�A�M�A�M�A�M�A�M�A�O�A�O�A�O�A�Q�A�S�A�VA�VA�VA�VA�S�A�VA�VA�O�A�?}A�(�A�"�A��A��A���A��#A�ȴA��A���A�~�A�p�A�l�A�bNA�VA�A�A�bA���A�1'A��A���A���A�9XA�v�A�Q�A�hsA��A��+A���A��PA��HA�p�A�/A��hA��A��DA�O�A��TA�5?A���A�jA�  A���A�K�A���A�1A�|�A�I�A�&�A��A��uA��A�t�A�-A�oA��HA�M�A�Q�A�JA��A��A�`BA��#A�p�A��
A��A�A�|�A�(�A��A��A��A�M�A�9XA��^A��A�I�A�hsA��-A�9XA���A��-A��A�v�A�XA�A�x�A�A�A�%A��A~�jA~ffA}l�A{�hAw�AtĜAr��Ao�Al��Ai��AgVAehsAdn�Ab�RA_�7A];dA\^5AZ�RAY��AY?}AX-AW�AV�/AT�`ARQ�AO��AO%AN��AN�9ANM�AN1AMt�AL��AL�+ALJAK��AK�AJ1'AH��AG`BAE�mAE"�AD�jAC�AChsAB��AB�DAA��AAdZAAG�A@��A@1'A?�;A?p�A?C�A?�A>�HA=ƨA<-A;��A:�RA9�A8�\A7S�A6A�A5�hA5A3�A2�A1�A0JA/�^A/�PA.��A-�FA,�jA+�A+/A)�-A(�\A'x�A'\)A&�/A%/A$��A$bNA$1'A$1A#|�A#+A"��A"bNA!K�A bAȴA5?A?}A-AoAp�A9XA�`A  A�A�AC�A��A�A~�A�A\)A�HA�A33A	�wA	G�A	oA�`A��A�+A^5A9XA{A�7A��AQ�A�wA7LAjA��A�DAXA M�A -A �@��@�M�@�%@��u@�A�@�1@�;d@��;@��+@���@�?}@�(�@��;@�@��@�=q@�/@�Z@���@���@�F@�+@�X@�u@�t�@��T@�V@��@���@��`@�r�@�l�@��@�!@��H@⟾@�^@��@��@ݺ^@�O�@ܼj@܃@�j@�I�@�(�@�33@���@أ�@�ƨ@�1@ҏ\@�V@�M�@˥�@�~�@�{@ɺ^@�V@���@ǍP@��@��y@Ɨ�@�M�@��#@�7L@�I�@þw@�|�@�o@+@�E�@�@��@�7L@��j@��u@��@�^5@�@�r�@�\)@�@���@��7@���@���@���@�/@�"�@��h@�t�@�~�@��@�O�@�%@��D@�ƨ@��/@��@���@���@�S�@���@�E�@��#@�p�@�&�@���@��`@��u@��F@�ȴ@�V@�-@�@���@�`B@��D@��@�  @��@��
@���@���@���@�t�@�l�@�S�@�
=@��@���@��R@��\@�v�@�E�@�J@��@���@�x�@���@���@��@�"�@��H@���@�$�@���@�X@�&�@�bN@�|�@�
=@���@���@�
=@�"�@��R@��+@�n�@�X@�\)@���@�n�@�{@���@�x�@�7L@�V@��@��@��m@�K�@�33@�+@�"�@�@���@�~�@�-@�x�@�%@��/@��j@��u@�bN@�(�@�;d@�
=@���@���@�X@�V@���@�bN@�\)@�
=@��!@���@��+@�ff@�E�@�E�@�=q@�{@���@��@��T@��h@�/@�%@��u@�bN@�1'@�@|�@~ȴ@~ff@}�T@}?}@}/@}/@}/@|��@|j@{ƨ@{C�@z��@z^5@y��@y�#@y��@y�@x�9@xb@w�@v�R@v�+@vV@v5?@u�@uV@t��@q��@j�!@c33@["�@PĜ@HbN@B��@=��@6E�@1x�@+��@'\)@"�H@!�7@��@
=@�!@�@@�9@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�O�A�K�A�M�A�M�A�M�A�M�A�O�A�O�A�O�A�Q�A�S�A�VA�VA�VA�VA�S�A�VA�VA�O�A�?}A�(�A�"�A��A��A���A��#A�ȴA��A���A�~�A�p�A�l�A�bNA�VA�A�A�bA���A�1'A��A���A���A�9XA�v�A�Q�A�hsA��A��+A���A��PA��HA�p�A�/A��hA��A��DA�O�A��TA�5?A���A�jA�  A���A�K�A���A�1A�|�A�I�A�&�A��A��uA��A�t�A�-A�oA��HA�M�A�Q�A�JA��A��A�`BA��#A�p�A��
A��A�A�|�A�(�A��A��A��A�M�A�9XA��^A��A�I�A�hsA��-A�9XA���A��-A��A�v�A�XA�A�x�A�A�A�%A��A~�jA~ffA}l�A{�hAw�AtĜAr��Ao�Al��Ai��AgVAehsAdn�Ab�RA_�7A];dA\^5AZ�RAY��AY?}AX-AW�AV�/AT�`ARQ�AO��AO%AN��AN�9ANM�AN1AMt�AL��AL�+ALJAK��AK�AJ1'AH��AG`BAE�mAE"�AD�jAC�AChsAB��AB�DAA��AAdZAAG�A@��A@1'A?�;A?p�A?C�A?�A>�HA=ƨA<-A;��A:�RA9�A8�\A7S�A6A�A5�hA5A3�A2�A1�A0JA/�^A/�PA.��A-�FA,�jA+�A+/A)�-A(�\A'x�A'\)A&�/A%/A$��A$bNA$1'A$1A#|�A#+A"��A"bNA!K�A bAȴA5?A?}A-AoAp�A9XA�`A  A�A�AC�A��A�A~�A�A\)A�HA�A33A	�wA	G�A	oA�`A��A�+A^5A9XA{A�7A��AQ�A�wA7LAjA��A�DAXA M�A -A �@��@�M�@�%@��u@�A�@�1@�;d@��;@��+@���@�?}@�(�@��;@�@��@�=q@�/@�Z@���@���@�F@�+@�X@�u@�t�@��T@�V@��@���@��`@�r�@�l�@��@�!@��H@⟾@�^@��@��@ݺ^@�O�@ܼj@܃@�j@�I�@�(�@�33@���@أ�@�ƨ@�1@ҏ\@�V@�M�@˥�@�~�@�{@ɺ^@�V@���@ǍP@��@��y@Ɨ�@�M�@��#@�7L@�I�@þw@�|�@�o@+@�E�@�@��@�7L@��j@��u@��@�^5@�@�r�@�\)@�@���@��7@���@���@���@�/@�"�@��h@�t�@�~�@��@�O�@�%@��D@�ƨ@��/@��@���@���@�S�@���@�E�@��#@�p�@�&�@���@��`@��u@��F@�ȴ@�V@�-@�@���@�`B@��D@��@�  @��@��
@���@���@���@�t�@�l�@�S�@�
=@��@���@��R@��\@�v�@�E�@�J@��@���@�x�@���@���@��@�"�@��H@���@�$�@���@�X@�&�@�bN@�|�@�
=@���@���@�
=@�"�@��R@��+@�n�@�X@�\)@���@�n�@�{@���@�x�@�7L@�V@��@��@��m@�K�@�33@�+@�"�@�@���@�~�@�-@�x�@�%@��/@��j@��u@�bN@�(�@�;d@�
=@���@���@�X@�V@���@�bN@�\)@�
=@��!@���@��+@�ff@�E�@�E�@�=q@�{@���@��@��T@��h@�/@�%@��u@�bN@�1'@�@|�@~ȴ@~ff@}�T@}?}@}/@}/@}/@|��@|j@{ƨ@{C�@z��@z^5@y��@y�#@y��@y�@x�9@xb@w�@v�R@v�+@vV@v5?@u�@uV@t��@q��@j�!@c33@["�@PĜ@HbN@B��@=��@6E�@1x�@+��@'\)@"�H@!�7@��@
=@�!@�@@�9@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDB
=B	7B+B+B%B%BBBBBBBBBBB��B��B�B��B�^B��B�oB�DB~�Bn�BiyBcTB[#BP�BJ�BC�B=qB8RB.B$�B�B�B�BVB1BB��B�B�B�;B��BɺBȴBȴBŢB�}B�?B�B��B��B��B��B�BcTBA�B1'B!�B{B
=B��B��B��B�{B�DB�1B�%BhsBXBR�B/BB
�B
�;B
��B
ǮB
�dB
��B
�oB
�B
p�B
_;B
W
B
S�B
P�B
L�B
F�B
C�B
:^B
,B
oB	��B	�B	�B	ƨB	�B	��B	��B	�uB	�+B	u�B	dZB	\)B	O�B	G�B	D�B	B�B	G�B	E�B	9XB	'�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	\B	B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�`B�BB�5B�B�B��B��B��BɺBƨBB�qB�dB�^B�XB�RB�?B�'B�B��B��B��B��B��B��B��B��B�{B�uB�oB�hB�bB�VB�PB�=B�%B�B~�B{�Bx�Bs�Bo�BiyBbNB`BB]/BZBXBW
BVBS�BR�BQ�BO�BL�BH�BD�BB�BB�BA�BA�BA�BA�B@�B@�B?}B=qB<jB:^B9XB8RB6FB49B2-B1'B1'B0!B0!B/B/B0!B0!B1'B2-B2-B33B49B49B49B5?B7LB9XB<jB<jB:^B:^B<jB=qB>wB@�BA�B?}B=qB<jB<jB<jB>wB>wB=qBA�BA�BC�BG�BL�BQ�BQ�BQ�BR�BR�BQ�BQ�BQ�BP�BP�BO�BT�BXBXBZBYBXB[#B_;B`BBaHBcTBgmBhsBhsBhsBhsBhsBhsBhsBiyBjBjBjBjBl�Bl�Bm�Bn�Bn�Bn�Bm�Bl�Bl�Bm�Bo�Bq�Bv�By�B{�B}�B� B~�B� B�B�B�=B�PB�bB�oB�uB�uB�oB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�-B�?B�FB�FB�FB�FB�FB�FB�FB�FB�FB�FB�LB�LB�RB�XB�^B�dB�qB�qB�wB�}B�}B�qB�dB�^B�dB�qB�wB�wB��B��BÖBȴB��B��B�B�5B�ZB�fB�fB�`B�fB�B�B�B�B��B��B��B��B��B	  B		7B	VB	bB	bB	hB	oB	uB	�B	�B	�B	!�B	"�B	#�B	%�B	'�B	)�B	49B	6FB	9XB	?}B	E�B	G�B	G�B	I�B	L�B	L�B	N�B	N�B	O�B	O�B	P�B	P�B	P�B	Q�B	Q�B	Q�B	Q�B	R�B	VB	W
B	[#B	]/B	^5B	`BB	aHB	dZB	ffB	hsB	l�B	l�B	l�B	l�B	l�B	o�B	r�B	u�B	w�B	y�B	z�B	{�B	{�B	}�B	~�B	� B	�B	�B	�B	�B	�B	�%B	�1B	�=B	��B	�9B	��B	�fB
  B
hB
�B
&�B
2-B
:^B
@�B
F�B
L�B
M�B
R�B
\)B
aHB
hsB
l�B
o�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B2B0B2B2B2B4B0B0B0B2B2B0B0B2B0B0B2B2B
)B	!BBBBB	BB
BBBB�B�B�B �B��B��B�}B��B�EB��B�XB�-B~�Bn�Bi]Bc;B[	BP�BJ�BC|B=ZB89B-�B$�B�B�BtB9BB�B��B�B�lB�B��BɛBȖBȕBŇB�`B�#B��B��B��B��B�vB��Bc5BAjB1B!�B[B
B��BμB��B�\B�&B�B�BhVBW�BR�B.�B�B
�B
�B
��B
ǏB
�GB
��B
�PB
��B
p�B
_"B
V�B
S�B
P�B
L�B
F�B
C}B
:FB
+�B
VB	��B	��B	��B	ƑB	�B	��B	�|B	�`B	�B	u�B	dGB	\B	O�B	G�B	D�B	B{B	G�B	E�B	9DB	'�B	{B	sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	yB	LB	B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�yB�rB�mB�NB�2B�$B�B��B��B��B̾BɪBƘB�~B�bB�TB�PB�GB�@B�/B�B��B��B��B��B��B��B��B��B�wB�kB�dB�`B�YB�SB�EB�?B�.B�B��B~�B{�Bx�Bs�Bo�BikBb?B`7B]BZBXBV�BU�BS�BR�BQ�BO�BL�BH�BD�BB�BB�BA{BA}BA|BA}B@vB@uB?mB=eB<\B:QB9LB8EB69B4,B2 B1B1B0B0B.�B/B0B0B1B2B2 B3&B4,B4)B4,B51B7@B9KB<[B<\B:NB:OB<ZB=cB>iB@sBAzB?oB=dB<ZB<ZB<ZB>iB>fB=GBAzBAyBC�BG�BL�BQ�BQ�BQ�BR�BR�BQ�BQ�BQ�BP�BP�BO�BT�BW�BX BZ
BYBW�B[B_*B`1Ba8BcABgZBhaBhbBhcBhbBhaBhaBhbBifBjmBjmBjlBjlBlvBlvBm}Bn�Bn�Bn�Bm~BlxBlwBm�Bo�Bq�Bv�By�B{�B}�B�B~�B�B��B�	B�)B�<B�MB�YB�]B�aB�YB�TB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�)B�/B�0B�/B�0B�0B�.B�-B�.B�,B�0B�2B�2B�;B�AB�GB�KB�YB�YB�`B�fB�eB�YB�KB�GB�NB�WB�[B�^B�lB�pB�BțB˯B��B��B�B�?B�NB�LB�CB�MB�pB�{B��B�B��B��B��B��B��B��B		B	9B	GB	DB	KB	RB	ZB	iB	xB	�B	!�B	"�B	#�B	%�B	'�B	)�B	4B	6(B	9:B	?aB	E�B	G�B	G�B	I�B	L�B	L�B	N�B	N�B	O�B	O�B	P�B	P�B	P�B	Q�B	Q�B	Q�B	Q�B	R�B	U�B	V�B	[B	]B	^B	`$B	a*B	d;B	fHB	hSB	lkB	lkB	lmB	lkB	llB	o~B	r�B	u�B	w�B	y�B	z�B	{�B	{�B	}�B	~�B	�B	��B	��B	��B	��B	� B	�B	�B	�B	�sB	�B	ͲB	�DB	��B
BB
�B
&�B
2B
:8B
@_B
F�B
L�B
M�B
R�B
\B
a!B
hLB
lgB
ozB
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.32 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708182016053117081820160531170818  AO  ARCAADJP                                                                    20140721230845    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230845  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230845  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170818  IP                  G�O�G�O�G�O�                