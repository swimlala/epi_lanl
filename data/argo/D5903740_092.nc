CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-09-24T12:01:34Z AOML 3.0 creation; 2016-06-01T00:08:20Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140924120134  20160531170821  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               \A   AO  4055_7112_092                   2C  D   APEX                            5374                            041511                          846 @�~�N�
1   @� ���@9��C���d#��v�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    \A   A   A   @�ff@�  A   A   A@  Aa��A���A�  A�  A�  A�  A�  A�  A�  B ffB  B��B��B   B(  B0  B8ffB?��BH  BP  BW��B`  Bh  Bp  Bx  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DBy�DC  DC� DDfDD� DE  DE� DF  DF�fDG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` DyffD��fD�S3D���D��fD��D�I�D��3D�ٚD�fD�P D�� Dǹ�D�3D�9�D�|�D�� D�3D�@ D� D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�=qA�A%�AE�Af�RA�\)A��\A��\A��\A\Aҏ\A�\A�\B�B	G�B�HB�HB!G�B)G�B1G�B9�B@�HBIG�BQG�BX�HBaG�BiG�BqG�ByG�B���B���B���B��
B���B���B���B���B���B���B���B���B���B���B���B���B���Bģ�Bȣ�Ḅ�BУ�Bԣ�Bأ�Bܣ�B��B��B��B��B��B���B���B���C Q�CQ�CQ�CQ�CQ�C
Q�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�C Q�C"Q�C$Q�C&Q�C(Q�C*Q�C,Q�C.Q�C0Q�C2Q�C4Q�C6Q�C8Q�C:Q�C<Q�C>Q�C@Q�CBQ�CDQ�CFQ�CHQ�CJQ�CLQ�CNQ�CPQ�CRQ�CTQ�CVQ�CXQ�CZQ�C\Q�C^Q�C`Q�CbQ�CdQ�CfQ�ChQ�CjQ�ClQ�CnQ�CpQ�CrQ�CtQ�CvQ�CxQ�CzQ�C|Q�C~Q�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�D {D �{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D	{D	�{D
{D
�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D {D �{D!{D!�{D"{D"�{D#{D#�{D${D$�{D%{D%�{D&{D&�{D'{D'�{D({D(�{D){D)�{D*{D*�{D+{D+�{D,{D,�{D-{D-�{D.{D.�{D/{D/�{D0{D0�{D1{D1�{D2{D2�{D3{D3�{D4{D4�{D5{D5�{D6{D6�{D7{D7�{D8{D8�{D9{D9�{D:{D:�{D;{D;�{D<{D<�{D={D=�{D>{D>�{D?{D?�{D@{D@�{DA{DA�{DB{DB�DC{DC�{DD�DD�{DE{DE�{DF{DF��DG{DG�{DH{DH�{DI{DI�{DJ{DJ�{DK{DK�{DL{DL�{DM{DM�{DN{DN�{DO{DO�{DP{DP�{DQ{DQ�{DR{DR�{DS{DS�{DT{DT�{DU{DU�{DV{DV�{DW{DW�{DX{DX�{DY{DY�{DZ{DZ�{D[{D[�{D\{D\�{D]{D]�{D^{D^�{D_{D_�{D`{D`�{Da{Da�{Db{Db�{Dc{Dc�{Dd{Dd�{De{De�{Df{Df�{Dg{Dg�{Dh{Dh�{Di{Di�{Dj{Dj�{Dk{Dk�{Dl{Dl�{Dm{Dm�{Dn{Dn�{Do{Do�{Dp{Dp�{Dq{Dq�{Dr{Dr�{Ds{Ds�{Dt{Dtt{Dyz�D� �D�]pD��
D���D�#�D�S�D��pD���D��D�Z=D��=D���D�pD�C�Dڇ
D��=D�pD�J=D�=D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�uA�hA�bNA�-A�JA��HA�!A�l�A�+A݁A܉7A� �A�ZA�-A�XA׶FA��Aә�A�A�Q�A�ƨAƇ+A���A��+A��mA�~�A�M�A���A���A�hsA��A���A���A�9XA��A��A�C�A��A�=qA���A���A��A��DA�bA��-A�?}A���A���A�Q�A�1A�hsA���A��PA�K�A���A��A��uA�bA�{A�S�A��`A��FA�oA�"�A�p�A��`A�bNA���A��!A���A���A�oA�-A���A�oA�`BA�VA�|�A���A�5?A���A�`BA��HA�
=A��A���A�K�A�+A�=qA��9A���A��`A��A��!A�v�A�oA��AG�A|��Az9XAy��Ax~�Av�HAu��AsdZAr�9Ar�Ap�ApJAm�AlI�Ak�^Ak�Ak33Aj�`Ajn�Ai&�AhA�Ag�#AgVAeAeoAdM�Ac`BAb �Aa�Aa;dA^1A[��AZ�9AZJAX�yAX  AW�wAW`BAU?}AS|�AR�`AQ��AP1AM�^ALM�AK?}AJ�9AJ-AI�wAG��AE/AC�PACdZAC"�AB �A@�jA?��A?�PA?K�A?;dA?;dA?7LA?7LA>�RA=��A<z�A;�A:bA85?A7hsA6��A6M�A5�FA4��A4=qA3�hA2�RA1�A0�A/��A.�A-��A,�A+t�A*��A*I�A)�A(�HA'ƨA&jA%VA$�`A$n�A#A#&�A"�A!��A ��A"�A�A�
A�yAx�A�uA(�A�;Ap�A��A��AAE�A�A�-At�A+A1'AS�A�9A$�A|�A��A��AE�AhsAI�A�wAdZA	�A	;dA	C�A	oAZA�`AjAdZA��A�A�yA(�A/A 1'@�C�@�ȴ@�V@���@��T@��7@���@�`B@���@��@��@�l�@�@���@�J@�+@�5?@���@�h@� �@�7@���@�b@��@�^@�?}@���@�@���@�ff@�%@��@��@�1@��@��/@�33@���@�A�@�|�@ҧ�@�V@щ7@�j@��@�33@�=q@Ͳ-@�r�@�33@��H@ʟ�@��@�hs@�z�@�dZ@Ə\@ź^@�`B@�&�@�bN@��;@�dZ@�@��/@�;d@�-@�X@���@���@�b@�\)@�ff@��@��
@�+@���@��D@��@���@�"�@��\@�`B@��@���@���@�l�@�ȴ@��9@��@�"�@��H@���@���@�I�@���@�ȴ@�E�@�V@��w@�~�@���@�V@��/@��9@��P@�ff@�p�@��`@�1'@��w@�33@��@�M�@���@��9@� �@���@�;d@�@��y@��H@��@���@��!@���@���@�&�@�j@�  @��F@�dZ@�;d@��R@�E�@�J@��@��T@��#@�@��^@���@�p�@�G�@�V@��`@���@���@�I�@�  @��F@���@�|�@�dZ@�33@��H@�E�@�-@�$�@�$�@��@�{@���@���@�`B@��/@�Z@��
@�l�@��@��\@�ff@�V@�=q@��@�@��@�@���@�G�@�&�@�V@��@���@��@��u@�Q�@�1@��F@�S�@�+@���@��H@��H@��H@���@�M�@�{@���@�x�@�/@���@��/@�Z@�  @��F@���@�|�@�
=@�ȴ@���@�ff@�=q@�-@���@��@��#@��h@�/@��`@��/@�bN@�1'@��@� �@�b@��@~�@~V@}��@}?}@|�D@|I�@|9X@{�m@{t�@{�F@{��@|(�@{��@{dZ@{"�@z�\@z=q@zJ@y�#@yG�@x��@xQ�@x  @u�@j�H@b�@\9X@WK�@L�/@F{@?�@8Ĝ@3��@/l�@)�@#�
@�@33@5?@�^@�@
�!@��@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�uA�hA�bNA�-A�JA��HA�!A�l�A�+A݁A܉7A� �A�ZA�-A�XA׶FA��Aә�A�A�Q�A�ƨAƇ+A���A��+A��mA�~�A�M�A���A���A�hsA��A���A���A�9XA��A��A�C�A��A�=qA���A���A��A��DA�bA��-A�?}A���A���A�Q�A�1A�hsA���A��PA�K�A���A��A��uA�bA�{A�S�A��`A��FA�oA�"�A�p�A��`A�bNA���A��!A���A���A�oA�-A���A�oA�`BA�VA�|�A���A�5?A���A�`BA��HA�
=A��A���A�K�A�+A�=qA��9A���A��`A��A��!A�v�A�oA��AG�A|��Az9XAy��Ax~�Av�HAu��AsdZAr�9Ar�Ap�ApJAm�AlI�Ak�^Ak�Ak33Aj�`Ajn�Ai&�AhA�Ag�#AgVAeAeoAdM�Ac`BAb �Aa�Aa;dA^1A[��AZ�9AZJAX�yAX  AW�wAW`BAU?}AS|�AR�`AQ��AP1AM�^ALM�AK?}AJ�9AJ-AI�wAG��AE/AC�PACdZAC"�AB �A@�jA?��A?�PA?K�A?;dA?;dA?7LA?7LA>�RA=��A<z�A;�A:bA85?A7hsA6��A6M�A5�FA4��A4=qA3�hA2�RA1�A0�A/��A.�A-��A,�A+t�A*��A*I�A)�A(�HA'ƨA&jA%VA$�`A$n�A#A#&�A"�A!��A ��A"�A�A�
A�yAx�A�uA(�A�;Ap�A��A��AAE�A�A�-At�A+A1'AS�A�9A$�A|�A��A��AE�AhsAI�A�wAdZA	�A	;dA	C�A	oAZA�`AjAdZA��A�A�yA(�A/A 1'@�C�@�ȴ@�V@���@��T@��7@���@�`B@���@��@��@�l�@�@���@�J@�+@�5?@���@�h@� �@�7@���@�b@��@�^@�?}@���@�@���@�ff@�%@��@��@�1@��@��/@�33@���@�A�@�|�@ҧ�@�V@щ7@�j@��@�33@�=q@Ͳ-@�r�@�33@��H@ʟ�@��@�hs@�z�@�dZ@Ə\@ź^@�`B@�&�@�bN@��;@�dZ@�@��/@�;d@�-@�X@���@���@�b@�\)@�ff@��@��
@�+@���@��D@��@���@�"�@��\@�`B@��@���@���@�l�@�ȴ@��9@��@�"�@��H@���@���@�I�@���@�ȴ@�E�@�V@��w@�~�@���@�V@��/@��9@��P@�ff@�p�@��`@�1'@��w@�33@��@�M�@���@��9@� �@���@�;d@�@��y@��H@��@���@��!@���@���@�&�@�j@�  @��F@�dZ@�;d@��R@�E�@�J@��@��T@��#@�@��^@���@�p�@�G�@�V@��`@���@���@�I�@�  @��F@���@�|�@�dZ@�33@��H@�E�@�-@�$�@�$�@��@�{@���@���@�`B@��/@�Z@��
@�l�@��@��\@�ff@�V@�=q@��@�@��@�@���@�G�@�&�@�V@��@���@��@��u@�Q�@�1@��F@�S�@�+@���@��H@��H@��H@���@�M�@�{@���@�x�@�/@���@��/@�Z@�  @��F@���@�|�@�
=@�ȴ@���@�ff@�=q@�-@���@��@��#@��h@�/@��`@��/@�bN@�1'@��@� �@�b@��@~�@~V@}��@}?}@|�D@|I�@|9X@{�m@{t�@{�F@{��@|(�@{��@{dZ@{"�@z�\@z=q@zJ@y�#@yG�@x��@xQ�@x  @u�@j�H@b�@\9X@WK�@L�/@F{@?�@8Ĝ@3��@/l�@)�@#�
@�@33@5?@�^@�@
�!@��@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBB�BB�B@�B@�B?}B?}B@�BG�Bu�B��B�+B�Bt�Be`B\)BQ�BC�BA�B-BoB�B�BB��BŢB�RB�B�7Bw�BjB`BB[#BT�BJ�B5?B#�B�B�B�BhBDB  B��B�B�B�B�`B�5B��B��B��BÖB�wB�XB�?B�-B�B�B��B��B�DB�B� Bu�BhsB]/BS�BJ�B>wB.B!�B�BoB  B�sB�B��B�dB�B��B�uB�DB�Bx�BiyBS�B<jB"�BJB
��B
�
B
B
��B
��B
��B
�{B
�PB
}�B
t�B
cTB
O�B
J�B
@�B
5?B
,B
�B
�B
bB
+B	��B	�B	�TB	�;B	�/B	�B	�
B	��B	��B	ÖB	��B	�RB	�B	��B	��B	��B	�hB	�\B	�JB	z�B	n�B	jB	e`B	`BB	[#B	YB	VB	K�B	D�B	A�B	;dB	49B	-B	(�B	%�B	"�B	�B	�B	uB	
=B	B	B	B��B��B��B��B��B��B��B��B��B�B�B�mB�HB�B��B��B��B��BȴBƨBŢBÖB��B�qB�XB�FB�?B�-B�B��B��B��B��B��B��B��B��B��B�{B�oB�bB�PB�=B�+B�B�B~�B{�Bv�Bu�Bt�Bs�Bq�Bn�Bm�BjBgmBdZBcTBbNB`BB]/BYBXBXBW
BT�BR�BO�BM�BJ�BI�BF�BA�B@�B?}B=qB:^B9XB7LB7LB7LB6FB5?B49B33B1'B1'B0!B0!B/B.B,B+B)�B(�B'�B'�B'�B'�B&�B$�B%�B%�B%�B$�B"�B#�B#�B"�B"�B"�B"�B"�B!�B!�B!�B!�B"�B#�B"�B$�B$�B$�B'�B'�B(�B)�B+B,B-B-B.B/B.B1'B33B33B33B33B33B33B33B33B5?B49B49B5?B5?B5?B5?B49B49B7LB8RB8RB8RB9XB9XB:^B;dB=qB=qB?}B@�BA�BB�BC�BD�BG�BG�BG�BI�BJ�BO�BR�BR�BS�BS�BT�BXBZB\)B_;B_;BbNBffBl�Bq�Bu�Bv�Bv�B|�B�B�7B�JB�\B�oB��B��B��B��B��B��B�B�'B�3B�3B�9B�9B�9B�?B�9B�XB�}BŢBȴB��B��B��B��B��B�
B�B�#B�#B�#B�#B�)B�)B�/B�;B�BB�HB�TB�mB�B�B�B�B�B��B��B��B��B��B��B��B	  B	B	B	B	
=B	\B	uB	�B	�B	�B	!�B	!�B	"�B	#�B	$�B	%�B	&�B	'�B	,B	,B	-B	.B	/B	0!B	0!B	2-B	33B	5?B	8RB	:^B	;dB	<jB	<jB	<jB	=qB	?}B	A�B	B�B	E�B	G�B	G�B	H�B	K�B	M�B	O�B	P�B	R�B	VB	XB	YB	ZB	[#B	]/B	_;B	aHB	dZB	ffB	gmB	iyB	k�B	m�B	o�B	p�B	p�B	p�B	q�B	t�B	u�B	w�B	x�B	y�B	y�B	y�B	z�B	|�B	~�B	� B	�B	�B	�B	�+B	�7B	�DB	�DB	�JB	�PB	�bB	�oB	�{B	��B	�}B	�B	�B	��B
JB
�B
%�B
0!B
7LB
=qB
E�B
L�B
Q�B
XB
^5B
e`B
ffB
m�B
q�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BB�BB�B@tB@vB?oB?rB@tBG�Bu�B�xB�!B��Bt�BeQB\BQ�BC�BAyB,�B_B�B�2B��BőB�=B��B�Bw�BjgB`)B[BT�BJ�B5"B#�B�B~BfBJB&B��B��B�B�{B�cB�CB�B��BλBʣB�tB�\B�<B� B�B��B��B��B�cB�$B��B�Bu�BhTB]BS�BJ�B>XB-�B!�B�BPB��B�VB��B̭B�FB��B��B�WB�'B��Bx�BiZBS�B<MB"�B+B
��B
��B
�rB
��B
��B
�uB
�`B
�6B
}�B
t�B
c8B
O�B
J�B
@fB
5"B
+�B
�B
pB
HB
B	��B	�B	�;B	�$B	�B	�B	��B	��B	ʩB	�~B	�kB	�;B	��B	��B	��B	��B	�RB	�GB	�3B	z�B	n�B	jiB	eLB	`-B	[B	YB	U�B	K�B	D�B	AuB	;PB	4$B	,�B	(�B	%�B	"�B	�B	�B	aB	
(B	B	B	�B��B��B��B��B��B��B��B��B��B�B�B�ZB�8B�B��B��B̾B˷BȢBƖBŎBÄB�qB�`B�HB�4B�.B�B�B��B��B��B��B��B��B��B�~B�xB�lB�`B�PB�BB�-B�B�	B��B~�B{�Bv�Bu�Bt�Bs�Bq�Bn�Bm�BjqBg`BdJBcHBb>B`4B] BYBXBXBV�BT�BR�BO�BM�BJ�BI�BF�BAzB@tB?oB=dB:QB9JB7>B7?B7?B69B5/B4*B3%B1B1B0B0B.�B.B+�B*�B)�B(�B'�B'�B'�B'�B&�B$�B%�B%�B%�B$�B"�B#�B#�B"�B"�B"�B"�B"�B!�B!�B!�B!�B"�B#�B"�B$�B$�B$�B'�B'�B(�B)�B*�B+�B,�B,�B.B/B.B1B3$B3#B3$B3&B3"B3$B3&B3!B5.B4(B4+B5.B5-B50B5-B4*B4*B7;B8AB8CB8AB9IB9HB:LB;TB=_B=aB?jB@pBAxBBBC�BD�BG�BG�BG�BI�BJ�BO�BR�BR�BS�BS�BT�BW�BZ	B\B_)B_%Bb9BfQBlwBq�Bu�Bv�Bv�B|�B��B�"B�5B�EB�YB�jB�wB��B��B��B��B��B�B�B�B�B�!B�#B�(B� B�?B�cBňBțBʨBͺB͸B��B��B��B��B�
B�B�
B�	B�B�B�B�!B�(B�/B�;B�SB�iB�B�B�B�B��B��B��B��B��B��B��B��B	�B	�B	B	
B	BB	XB	qB	�B	�B	!�B	!�B	"�B	#�B	$�B	%�B	&�B	'�B	+�B	+�B	,�B	-�B	.�B	0B	0B	2B	3B	5B	82B	:?B	;FB	<LB	<KB	<MB	=TB	?_B	AmB	BqB	E�B	G�B	G�B	H�B	K�B	M�B	O�B	P�B	R�B	U�B	W�B	X�B	Y�B	[B	]B	_B	a)B	d9B	fEB	gMB	iYB	keB	msB	oB	p�B	p�B	p�B	q�B	t�B	u�B	w�B	x�B	y�B	y�B	y�B	z�B	|�B	~�B	�B	��B	��B	��B	�
B	�B	�#B	�&B	�+B	�/B	�BB	�PB	�\B	��B	�ZB	��B	�ZB	��B
%B
|B
%�B
/�B
7'B
=MB
E}B
L�B
Q�B
W�B
^B
e<B
f@B
mlB
q�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.32 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708212016053117082120160531170821  AO  ARCAADJP                                                                    20140924120134    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140924120134  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140924120134  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170821  IP                  G�O�G�O�G�O�                