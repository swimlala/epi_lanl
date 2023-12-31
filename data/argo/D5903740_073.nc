CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:43Z AOML 3.0 creation; 2016-06-01T00:08:17Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230843  20160531170817  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               IA   AO  4055_7112_073                   2C  D   APEX                            5374                            041511                          846 @�����1   @��މJ�@:n��O�;�d�����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    IA   A   A   @�33@�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL�CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy�fD�� D�Y�D�� D���D��D�L�D�s3D��fD�fD�33D�ffD�ٚD��D�I�Dڃ3D��D��3D�S3D��D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�p�@�=qA�A%�AC�Ae�A��\A��\A��\A��\A\Aҏ\A�\A�\BG�B	G�BG�BG�B!G�B)G�B1G�B9G�BAG�BIG�BQG�BYG�BaG�BiG�BqG�ByG�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bģ�Bȣ�Ḅ�BУ�Bԣ�Bأ�Bܣ�B��B��B��B��
B��B���B���B���C Q�CQ�CQ�CQ�CQ�C
Q�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�C Q�C"Q�C$Q�C&Q�C(Q�C*Q�C,Q�C.Q�C0Q�C2Q�C4Q�C6Q�C8Q�C:Q�C<Q�C>Q�C@Q�CBQ�CDQ�CFQ�CHQ�CJQ�CLk�CNQ�CPQ�CRQ�CTQ�CVQ�CXQ�CZQ�C\Q�C^Q�C`Q�CbQ�CdQ�CfQ�ChQ�CjQ�ClQ�CnQ�CpQ�CrQ�CtQ�CvQ�CxQ�CzQ�C|Q�C~Q�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�D {D �{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D	{D	�{D
{D
�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D��D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D {D �{D!{D!�{D"{D"�{D#{D#�{D${D$�{D%{D%�{D&{D&�{D'{D'�{D({D(�{D){D)�{D*{D*�{D+{D+�{D,{D,�{D-{D-�{D.{D.�{D/{D/�{D0{D0�{D1{D1�{D2{D2�{D3{D3�{D4{D4�{D5{D5�{D6{D6�{D7{D7�{D8{D8�{D9{D9�{D:{D:�{D;{D;�{D<{D<�{D={D=�{D>{D>�{D?{D?�{D@{D@�{DA{DA�{DB{DB�{DC{DC�{DD{DD�{DE{DE�{DF{DF�{DG{DG�{DH{DH�{DI{DI�{DJ{DJ�{DK{DK�{DL{DL�{DM{DM�{DN{DN�{DO{DO�{DP{DP�{DQ{DQ�{DR{DR�{DS{DS�{DT{DT�{DU{DU�{DV{DV�{DW{DW�{DX{DX�{DY{DY�{DZ{DZ�{D[{D[�{D\{D\�{D]{D]�{D^{D^�{D_{D_�{D`{D`�{Da{Da�{Db{Db�{Dc{Dc�{Dd{Dd�{De{De�{Df{Df�{Dg{Dg�{Dh{Dh�{Di{Di�{Dj{Dj�{Dk{Dk�{Dl{Dl�{Dm{Dm�{Dn{Dn�{Do{Do�{Dp{Dp�{Dq{Dq�{Dr{Dr�{Ds{Ds�{Dt{Dt�{Dt�Dy��D��=D�c�D��=D��
D�
D�W
D�}pD��D��D�=pD�p�D���D�
D�S�DڍpD��
D��pD�]pD�
D��p111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�x�A�1'A�%A��
A��A�K�A�?}A�C�A��#A��A�x�A��#A��A��hA��A�|�A�ffA�M�A�G�A�=qA�(�A�bA���A���A�bA�;dA��A� �A�(�A�(�A�5?A�?}A��hA�9XA�M�A�|�A�XA�=qA�"�A�"�A��A�{A�1A���A��;A���A�ȴA�ĜA�ĜA���A��jA��FA��FA��FA��FA��FA��RA��9A��-A��9A��A���A��uA��uA��PA�`BA��jA�G�A�A�;dA�dZA���A��A���A�?}A�A�A�~�A��A�|�A���A���A��-A�E�A���A�(�A��RA�XA��A�p�A��A���A�r�A���A��TA�l�A�G�A��yA�K�A��A�  A�l�A~�A}�-Ax�AodZAm
=Aj�DAj(�Ai7LAe�FAb��A`n�A]�A\v�A[�mA[ƨA[x�AZ�AX�yAX-AW�AW�hAVjAT�AQ�PAP�+APE�AO�mAO��AN9XAMS�AL�yAL~�ALA�AK�AH�AG�;AGXAF��AFQ�AEt�AE`BAD��AC��ABJA@�DA>{A=��A<�jA<Q�A<JA;��A:�yA:9XA8r�A6VA5\)A3��A1�
A0��A.ĜA-��A,9XA+�A+��A++A)?}A'�#A'�A';dA'"�A&�A&�A&�A&�A&��A&��A&�DA&�\A&�A&^5A&Q�A&9XA&�A&bA%�A%K�A$��A$=qA#�wA"VA��A��A��A%A7LA��AE�AAdZAC�A�A�RA;dA�A\)A
=A��A�RA�Az�AA�A-A{A�A�7A��A�9AbA/Av�A�wA"�A	O�A��A�!A5?A\)A��A��A�jA�DA�;A�AA�A �A �A bA 1@��;@���@�dZ@��@���@���@��/@�o@�?}@�A�@�S�@���@��H@�!@�V@�p�@�ȴ@�@�\@�{@�&�@��@�@�1@�@��@�@��@��@��
@ڏ\@�x�@��`@�I�@�v�@��@�z�@�Q�@�Q�@�Q�@�Q�@�Q�@�1'@��;@�l�@ҸR@�V@��m@�{@͉7@̓u@�@�n�@�E�@��@���@�Ĝ@�Ĝ@�j@�M�@�(�@�ƨ@�hs@�@�%@�Z@�t�@��@��R@�@�G�@�Ĝ@��@�o@�n�@�5?@��@�r�@���@�+@���@��\@�$�@�x�@��P@��@���@�{@���@��m@���@��w@��F@���@�{@���@��@��P@�$�@���@�@���@�x�@�?}@��@��`@��j@�j@�1'@�b@�ƨ@��@�C�@���@���@�&�@� �@��F@�\)@�
=@�@��!@��-@��h@�/@��u@� �@�1@��@��m@��;@�ƨ@�+@�n�@��@�@�%@�Ĝ@��u@��@��@���@�V@���@���@�hs@�&�@��/@��9@��@��F@�+@��@��R@�^5@�V@�V@�=q@��T@��@��@�9X@�1@���@���@�ff@�-@�$�@�{@�J@�@�`B@��@�j@��@���@��F@�K�@��@�v�@�5?@��@�J@���@��@��T@��#@�@���@���@�X@�l�@���@�E�@��@�?}@�/@��@��@���@���@���@��@��u@�z�@�r�@�r�@�j@�Q�@�I�@�A�@� �@��@�P@\)@~�y@~ff@~$�@}�@}��@}�h@}`B@}?}@}/@}V@|��@|�j@|��@|��@|��@|�D@|9X@{�m@{�F@{C�@z�@z�\@z-@y�^@y��@y�7@y7L@x��@x�u@x �@w�@v{@u��@u�@uO�@rn�@g�@\�/@Xb@Pr�@M`B@D��@?�w@;��@4�@-��@&�y@$��@
=@\)@�F@7L@+@��@��@I�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�x�A�1'A�%A��
A��A�K�A�?}A�C�A��#A��A�x�A��#A��A��hA��A�|�A�ffA�M�A�G�A�=qA�(�A�bA���A���A�bA�;dA��A� �A�(�A�(�A�5?A�?}A��hA�9XA�M�A�|�A�XA�=qA�"�A�"�A��A�{A�1A���A��;A���A�ȴA�ĜA�ĜA���A��jA��FA��FA��FA��FA��FA��RA��9A��-A��9A��A���A��uA��uA��PA�`BA��jA�G�A�A�;dA�dZA���A��A���A�?}A�A�A�~�A��A�|�A���A���A��-A�E�A���A�(�A��RA�XA��A�p�A��A���A�r�A���A��TA�l�A�G�A��yA�K�A��A�  A�l�A~�A}�-Ax�AodZAm
=Aj�DAj(�Ai7LAe�FAb��A`n�A]�A\v�A[�mA[ƨA[x�AZ�AX�yAX-AW�AW�hAVjAT�AQ�PAP�+APE�AO�mAO��AN9XAMS�AL�yAL~�ALA�AK�AH�AG�;AGXAF��AFQ�AEt�AE`BAD��AC��ABJA@�DA>{A=��A<�jA<Q�A<JA;��A:�yA:9XA8r�A6VA5\)A3��A1�
A0��A.ĜA-��A,9XA+�A+��A++A)?}A'�#A'�A';dA'"�A&�A&�A&�A&�A&��A&��A&�DA&�\A&�A&^5A&Q�A&9XA&�A&bA%�A%K�A$��A$=qA#�wA"VA��A��A��A%A7LA��AE�AAdZAC�A�A�RA;dA�A\)A
=A��A�RA�Az�AA�A-A{A�A�7A��A�9AbA/Av�A�wA"�A	O�A��A�!A5?A\)A��A��A�jA�DA�;A�AA�A �A �A bA 1@��;@���@�dZ@��@���@���@��/@�o@�?}@�A�@�S�@���@��H@�!@�V@�p�@�ȴ@�@�\@�{@�&�@��@�@�1@�@��@�@��@��@��
@ڏ\@�x�@��`@�I�@�v�@��@�z�@�Q�@�Q�@�Q�@�Q�@�Q�@�1'@��;@�l�@ҸR@�V@��m@�{@͉7@̓u@�@�n�@�E�@��@���@�Ĝ@�Ĝ@�j@�M�@�(�@�ƨ@�hs@�@�%@�Z@�t�@��@��R@�@�G�@�Ĝ@��@�o@�n�@�5?@��@�r�@���@�+@���@��\@�$�@�x�@��P@��@���@�{@���@��m@���@��w@��F@���@�{@���@��@��P@�$�@���@�@���@�x�@�?}@��@��`@��j@�j@�1'@�b@�ƨ@��@�C�@���@���@�&�@� �@��F@�\)@�
=@�@��!@��-@��h@�/@��u@� �@�1@��@��m@��;@�ƨ@�+@�n�@��@�@�%@�Ĝ@��u@��@��@���@�V@���@���@�hs@�&�@��/@��9@��@��F@�+@��@��R@�^5@�V@�V@�=q@��T@��@��@�9X@�1@���@���@�ff@�-@�$�@�{@�J@�@�`B@��@�j@��@���@��F@�K�@��@�v�@�5?@��@�J@���@��@��T@��#@�@���@���@�X@�l�@���@�E�@��@�?}@�/@��@��@���@���@���@��@��u@�z�@�r�@�r�@�j@�Q�@�I�@�A�@� �@��@�P@\)@~�y@~ff@~$�@}�@}��@}�h@}`B@}?}@}/@}V@|��@|�j@|��@|��@|��@|�D@|9X@{�m@{�F@{C�@z�@z�\@z-@y�^@y��@y�7@y7L@x��@x�u@x �@w�@v{@u��@u�@uO�@rn�@g�@\�/@Xb@Pr�@M`B@D��@?�w@;��@4�@-��@&�y@$��@
=@\)@�F@7L@+@��@��@I�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB^5B]/B[#BZBXBS�BS�BR�BO�BM�BL�BM�BL�BL�BK�BL�BL�BK�BJ�BJ�BI�BH�BF�BC�B2-B#�B#�B%�B&�B'�B(�B)�BuB�yB�B��BBBBBBB  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�/B�9B�%BB�BB�B��B��B��B�Bo�BaHBVB@�B�BB
��B
�B
�NB
ƨB
�B
��B
��B
�oB
�DB
�B
w�B
iyB
bNB
_;B
XB
O�B
N�B
L�B
D�B
49B
'�B	��B	ÖB	�FB	�!B	�9B	�9B	��B	�+B	�+B	x�B	ffB	aHB	_;B	]/B	XB	R�B	O�B	M�B	J�B	C�B	;dB	33B	/B	.B	,B	)�B	$�B	!�B	�B	�B	�B	�B	bB	VB	JB	
=B	1B	%B	B	B��B��B�B�B�B�mB�`B�TB�NB�;B�#B��B��BŢB�wB�XB�?B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�oB�hB�\B�JB�7B�Bz�Bx�Bv�Bs�Bo�Bn�Bl�Bk�BjBiyBhsBffBbNB_;B^5B^5B]/B]/B\)B\)B[#BZBYBVBP�BL�BJ�BG�BE�BC�BA�B>wB;dB8RB6FB5?B49B33B33B2-B1'B/B-B+B+B)�B)�B)�B)�B(�B(�B(�B'�B&�B%�B#�B"�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{B{B�B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BoB�B�B�B�B�B�B�B�B�B�B�B&�B)�B1'B49B5?B7LB8RB8RB:^B;dB<jB=qB?}BA�BA�BA�BD�BG�BH�BI�BI�BJ�BK�BQ�BS�BT�BVB\)B^5B_;B_;B_;B^5BdZBhsBjBm�Bt�Bv�Bv�Bw�Bx�By�By�Bz�B{�B|�B}�B~�B� B�B�B�B�DB�PB�uB��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�9B�LB�RB�qB�}B�}BBƨB��B��B��B��B�B�
B�B�B�#B�HB�ZB�fB�mB�B�B�B�B�B�B��B��B��B	  B	%B	DB	JB	JB	JB	JB	VB	hB	�B	�B	�B	�B	 �B	#�B	%�B	+B	-B	.B	/B	0!B	0!B	1'B	1'B	2-B	2-B	2-B	33B	D�B	K�B	M�B	S�B	VB	W
B	XB	XB	YB	YB	YB	YB	\)B	]/B	]/B	]/B	^5B	_;B	_;B	_;B	`BB	cTB	dZB	e`B	hsB	k�B	l�B	m�B	n�B	p�B	q�B	q�B	r�B	r�B	s�B	t�B	t�B	t�B	t�B	u�B	v�B	w�B	x�B	z�B	{�B	}�B	� B	�B	�B	�B	�B	�B	�%B	�+B	�DB	�\B	�hB	�hB	�uB	��B	�qB	�5B	�B	��B
1B
�B
#�B
)�B
49B
>wB
G�B
J�B
Q�B
\)B
aHB
dZB
gmB
k�B
p�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B^B]B[BZBW�BS�BS�BR�BO�BM�BL�BM�BL�BL�BK�BL�BL�BK�BJ�BJ�BI�BH�BF�BC|B2B#�B#�B%�B&�B'�B(�B)�BYB�_B�B��B �B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�	BBrB �B�gB�kB��B��B��BoBa+BU�B@eBqB�B
��B
�sB
�0B
ƋB
��B
��B
��B
�QB
�&B
��B
w�B
i_B
b1B
_B
W�B
O�B
N�B
L�B
DB
4!B
'�B	��B	�}B	�/B	�
B	�!B	�!B	��B	�B	�B	x�B	fNB	a2B	_(B	]B	W�B	R�B	O�B	M�B	J�B	C�B	;PB	3 B	/	B	.B	+�B	)�B	$�B	!�B	�B	�B	�B	nB	PB	BB	6B	
*B	B	B	B	�B��B��B�B�yB�mB�ZB�OB�CB�>B�)B�B��BʰBŐB�gB�HB�0B�B��B��B��B��B��B��B��B��B��B��B�}B�}B�|B�wB�wB�xB�xB�wB�wB�rB�qB�qB�mB�eB�`B�ZB�NB�8B�'B��Bz�Bx�Bv�Bs�Bo�Bn�Bl|BkxBjrBiiBhfBfYBbAB_/B^&B^(B]B] B\B\B[BZBY
BU�BP�BL�BJ�BG�BE�BC�BA|B>PB;WB8EB67B51B4+B3'B3'B2B1B/B,�B*�B*�B)�B)�B)�B)�B(�B(�B(�B'�B&�B%�B#�B"�B!�B �B�B�B�B�B�BwByB�B�B�B~B�B�B~B{BQBTBsBnBSBWBsBtBrB[BB}B}B|B}B}B^B^B^BzB[BtBBbB�B|B�B}B~B�B�B�B�B�B�B&�B)�B1B4'B5.B7=B8AB8BB:MB;RB<XB=_B?nBAvBAzBAwBD�BG�BH�BI�BI�BJ�BK�BQ�BS�BT�BU�B\B^"B_)B_)B_&B^"BdEBh`BjmBm~Bt�Bv�Bv�Bw�Bx�By�By�Bz�B{�B|�B}�B~�B�B��B��B�
B�.B�<B�^B�hB�vB��B�}B��B��B��B��B��B��B��B��B��B��B��B�	B�!B�4B�9B�[B�bB�aB�vBƏBˬBοB��B��B��B��B��B�B�	B�-B�BB�LB�RB�eB�dB�dB�fB�qB�B��B��B��B��B		B	(B	.B	-B	.B	.B	9B	KB	~B	�B	�B	�B	 �B	#�B	%�B	*�B	,�B	-�B	.�B	0B	0B	1B	1	B	2B	2B	2B	3B	DB	K�B	M�B	S�B	U�B	V�B	W�B	W�B	X�B	X�B	X�B	X�B	\B	]B	]B	]B	^B	_B	_B	_B	`"B	c5B	d9B	eAB	hUB	kdB	llB	mrB	nyB	p�B	q�B	q�B	r�B	r�B	s�B	t�B	t�B	t�B	t�B	u�B	v�B	w�B	x�B	z�B	{�B	}�B	�B	��B	��B	��B	��B	��B	�B	�B	�%B	�:B	�FB	�FB	�UB	��B	�MB	�B	�mB	��B
B
}B
#�B
)�B
4B
>QB
G�B
J�B
Q�B
\B
aB
d3B
gJB
k^B
pB
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.32 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708172016053117081720160531170817  AO  ARCAADJP                                                                    20140721230843    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230843  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230843  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170817  IP                  G�O�G�O�G�O�                