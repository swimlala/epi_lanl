CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-20T10:16:01Z AOML 3.0 creation; 2016-05-31T19:14:41Z UW 3.1 conversion     
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
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �L   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �T   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20150220101601  20160531121442  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               hA   AO  4051_7090_104                   2C  D   APEX                            5368                            041511                          846 @�;̬/�1   @�;�?���@4j=p��
�dd�n��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    hA   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)fD)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du@ Dy��D�3D�)�D�l�D���D��D�9�D���D�ɚD�3D�C3D���D��fD�3D�9�D�y�D�fD���D�@ D�C3D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @|(�@��H@��HAp�A=p�A]p�A}p�A��RA��RA��RA��RAθRA޸RA�RA��RB\)B\)B\)B\)B'\)B/\)B7\)B?\)BG\)BO\)BW\)B_\)Bg\)Bo\)Bw\)B\)B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BîBǮBˮBϮBӮB׮BۮB߮B�B�B�B�B�B��B��B��C�
C�
C�
C�
C	�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C!�
C#�
C%�
C'�
C)�
C+�
C-�
C/�
C1�
C3�
C5�
C7�
C9�
C;�
C=�
C?�
CA�
CC�
CE�
CG�
CI�
CK�
CM�
CO�
CQ�
CS�
CU�
CW�
CY�
C[�
C]�
C_�
Ca�
Cc�
Ce�
Cg�
Ci�
Ck�
Cm�
Co�
Cq�
Cs�
Cu�
Cw�
Cy�
C{�
C}�
C�
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D u�D ��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D	u�D	��D
u�D
��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D u�D ��D!u�D!��D"u�D"��D#u�D#��D$u�D$��D%u�D%��D&u�D&��D'u�D'��D(u�D(�)D)u�D)��D*u�D*��D+u�D+��D,u�D,��D-u�D-��D.u�D.��D/u�D/��D0u�D0��D1u�D1��D2u�D2��D3u�D3��D4u�D4��D5u�D5��D6u�D6��D7u�D7��D8u�D8��D9u�D9��D:u�D:��D;u�D;��D<u�D<��D=u�D=��D>u�D>��D?u�D?��D@u�D@��DAu�DA��DBu�DB��DCu�DC��DDu�DD��DEu�DE��DFu�DF��DGu�DG��DHu�DH��DIu�DI��DJu�DJ��DKu�DK��DLu�DL��DMu�DM��DNu�DN��DOu�DO��DPu�DP��DQu�DQ��DRu�DR��DSu�DS��DTu�DT��DUu�DU��DVu�DV��DWu�DW��DXu�DX��DYu�DY��DZu�DZ��D[u�D[��D\u�D\��D]u�D]��D^u�D^��D_u�D_��D`u�D`��Dau�Da��Dbu�Db��Dcu�Dc��Ddu�Dd��Deu�De��Dfu�Df��Dgu�Dg��Dhu�Dh��Diu�Di��Dju�Dj��Dku�Dk��Dlu�Dl��Dmu�Dm��Dnu�Dn��Dou�Do��Dpu�Dp��Dqu�Dq��Dru�Dr��Dsu�Ds��Dtu�Dt��Du5�Dy�]D��D�${D�g�D���D��D�4{D��{D��{D��D�>D���D��GD�D�4{D�t{D�GD��{D�:�D�>D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�-A�-A�+A�-A�1'A�-A�(�A�{A�bA�A��AɑhA��A��A�5?AƼjA���A�A��Aģ�A�p�A�  A�~�A�jA�\)A�Q�A�G�A�A�A�=qA�7LA�1'A�+A��A�{A�1A��A��TA��;A��#A���A©�A�APA+AA�~�A�|�A�jA�33A���A�l�A�;dA���A��A�n�A�^5A�9XA�oA���A���A��7A�C�A��hA��A��\A���A�bNA�O�A��+A�ZA���A�XA�|�A�(�A��
A���A�XA��A��A�I�A���A�O�A��A�ZA��A��jA�n�A�?}A���A�S�A�
=A��A�K�A��
A��
A��A�oA���A�ffA��uA�
=A��yA�
=A�A�A��uA�{A�|�A�z�A�VA��`A��A�1'A�z�A�%A���A�+A��A~�9A~�A|�HA{�AzAx$�AwS�AvI�As�Aq��Aq7LAp�/Ao?}AmS�Al�Ak��Ak7LAjAhĜAeG�Ab1'A`VA^�yA\ĜAZ�AYl�AX �AR��AP�!AN��AM�#AL�RAKƨAJ�uAH�/AE��AB~�AAoA?33A;?}A8ffA81A7�wA5�A5%A4M�A333A1A0�A.A�A,ȴA+?}A)��A'|�A%��A$I�A#�A"�jA"I�A!�mA!S�A �RA Ap�AAJA�A�yAƨA`BAVA��AVAĜA��Al�AC�A��A��AK�AbNA�TA�-A�jAC�A
�`A
JA�/A�mAK�AE�A��AG�A�RAz�AQ�A�;AK�A�/A��A�A33A ��@���@�hs@�%@��P@�E�@�I�@�K�@�5?@��T@�@�@�z�@�;d@�&�@�  @�|�@��#@�j@�@���@旍@�!@�h@� �@��@�h@�5?@ف@�A�@׾w@��@���@�/@�A�@�C�@ҸR@ёh@Л�@�v�@�V@̃@̛�@̃@�"�@��/@� �@ǶF@Ǖ�@�+@Ə\@�-@��#@š�@��T@š�@��@�&�@�%@ģ�@�A�@��;@�t�@���@�M�@�J@��@��-@���@�x�@�%@�1'@��m@�|�@�;d@�@���@�5?@��@���@�(�@��
@�dZ@��@�V@�x�@��@� �@��P@�;d@���@���@�v�@��@�@��7@�%@�j@�I�@�(�@���@��;@�+@�^5@�-@���@��@�Q�@���@��;@��@�\)@���@��@�/@���@�(�@��F@�;d@�o@���@��@���@�n�@�J@��-@�p�@��@�Ĝ@��D@�1'@�b@��m@��@�dZ@��y@���@�{@��@��^@��7@�hs@�/@�&�@�&�@�V@��`@�z�@�1'@�  @�Q�@��;@��@�dZ@�33@�o@���@�^5@�J@���@��-@�hs@�G�@��`@��D@�I�@��@���@�|�@�t�@�t�@�t�@�l�@�dZ@�S�@�K�@�"�@��@���@�n�@�E�@�J@��@��@��#@��^@���@�O�@��@�z�@�A�@�  @��F@�S�@�;d@�;d@�33@��@�ȴ@���@�n�@�J@���@�G�@�7L@�&�@�%@���@���@���@�j@� �@���@�l�@�K�@�33@��\@�M�@�=q@��@��#@�hs@��@��9@�r�@�9X@��@�dZ@�;d@���@���@�ff@��@��@��-@��7@�`B@�&�@���@��`@��j@���@�Q�@��@��@��@�b@�  @��;@�ƨ@���@�|�@�K�@��@��H@���@���@�V@�{@��@�@�G�@��`@���@��j@���@�z�@�I�@��@�ƨ@��H@��`@�  @�bN@yG�@pr�@ep�@\j@T�/@P�u@JJ@B��@9�7@2=q@)��@#��@ A�@t�@ȴ@��@l�@"�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 A�-A�-A�+A�-A�1'A�-A�(�A�{A�bA�A��AɑhA��A��A�5?AƼjA���A�A��Aģ�A�p�A�  A�~�A�jA�\)A�Q�A�G�A�A�A�=qA�7LA�1'A�+A��A�{A�1A��A��TA��;A��#A���A©�A�APA+AA�~�A�|�A�jA�33A���A�l�A�;dA���A��A�n�A�^5A�9XA�oA���A���A��7A�C�A��hA��A��\A���A�bNA�O�A��+A�ZA���A�XA�|�A�(�A��
A���A�XA��A��A�I�A���A�O�A��A�ZA��A��jA�n�A�?}A���A�S�A�
=A��A�K�A��
A��
A��A�oA���A�ffA��uA�
=A��yA�
=A�A�A��uA�{A�|�A�z�A�VA��`A��A�1'A�z�A�%A���A�+A��A~�9A~�A|�HA{�AzAx$�AwS�AvI�As�Aq��Aq7LAp�/Ao?}AmS�Al�Ak��Ak7LAjAhĜAeG�Ab1'A`VA^�yA\ĜAZ�AYl�AX �AR��AP�!AN��AM�#AL�RAKƨAJ�uAH�/AE��AB~�AAoA?33A;?}A8ffA81A7�wA5�A5%A4M�A333A1A0�A.A�A,ȴA+?}A)��A'|�A%��A$I�A#�A"�jA"I�A!�mA!S�A �RA Ap�AAJA�A�yAƨA`BAVA��AVAĜA��Al�AC�A��A��AK�AbNA�TA�-A�jAC�A
�`A
JA�/A�mAK�AE�A��AG�A�RAz�AQ�A�;AK�A�/A��A�A33A ��@���@�hs@�%@��P@�E�@�I�@�K�@�5?@��T@�@�@�z�@�;d@�&�@�  @�|�@��#@�j@�@���@旍@�!@�h@� �@��@�h@�5?@ف@�A�@׾w@��@���@�/@�A�@�C�@ҸR@ёh@Л�@�v�@�V@̃@̛�@̃@�"�@��/@� �@ǶF@Ǖ�@�+@Ə\@�-@��#@š�@��T@š�@��@�&�@�%@ģ�@�A�@��;@�t�@���@�M�@�J@��@��-@���@�x�@�%@�1'@��m@�|�@�;d@�@���@�5?@��@���@�(�@��
@�dZ@��@�V@�x�@��@� �@��P@�;d@���@���@�v�@��@�@��7@�%@�j@�I�@�(�@���@��;@�+@�^5@�-@���@��@�Q�@���@��;@��@�\)@���@��@�/@���@�(�@��F@�;d@�o@���@��@���@�n�@�J@��-@�p�@��@�Ĝ@��D@�1'@�b@��m@��@�dZ@��y@���@�{@��@��^@��7@�hs@�/@�&�@�&�@�V@��`@�z�@�1'@�  @�Q�@��;@��@�dZ@�33@�o@���@�^5@�J@���@��-@�hs@�G�@��`@��D@�I�@��@���@�|�@�t�@�t�@�t�@�l�@�dZ@�S�@�K�@�"�@��@���@�n�@�E�@�J@��@��@��#@��^@���@�O�@��@�z�@�A�@�  @��F@�S�@�;d@�;d@�33@��@�ȴ@���@�n�@�J@���@�G�@�7L@�&�@�%@���@���@���@�j@� �@���@�l�@�K�@�33@��\@�M�@�=q@��@��#@�hs@��@��9@�r�@�9X@��@�dZ@�;d@���@���@�ff@��@��@��-@��7@�`B@�&�@���@��`@��j@���@�Q�@��@��@��@�b@�  @��;@�ƨ@���@�|�@�K�@��@��H@���@���@�V@�{@��@�@�G�@��`@���@��j@���@�z�@�I�@��@�ƨG�O�@��`@�  @�bN@yG�@pr�@ep�@\j@T�/@P�u@JJ@B��@9�7@2=q@)��@#��@ A�@t�@ȴ@��@l�@"�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�'B�-B�-B�-B�'B�'B�'B�!B�!B�'B�'B�LBɺB�ZB��B
=B  B�B��B��B+B{B�B�B�B�B�B�B�B�B�B�B�B �B!�B#�B$�B%�B&�B(�B-B/B0!B1'B1'B2-B2-B49B<jBA�BL�BN�BQ�BT�BVBT�BS�BVBS�BYBXBYBgmBq�Bx�Bs�Bl�BffBVB=qB(�B�B�B�B�BDB+BB��B��B�B�B�mB�NB�5B�#B�B��BɺB�B�VBy�B]/BD�B.BuBB�B�ZB��BĜB�hBjB2-BoB
��B
�)B
��B
ÖB
�}B
�jB
�9B
��B
��B
��B
�hB
{�B
ffB
cTB
\)B
P�B
F�B
7LB
.B
%�B
�B
B	��B	��B	�B	�`B	�/B	�B	�B	��B	B	�B	��B	�7B	}�B	q�B	cTB	XB	N�B	49B	'�B	!�B	�B	�B	�B	bB	
=B��B�B�B�NB�B��B��B��B��BȴBŢBB�qB�XB�FB�3B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B�oB�\B�PB�DB�7B�+B�B�B�B�B�B~�B|�B|�B{�B{�By�Bx�Bv�Bv�Bu�Bt�Bs�Bs�Bs�Bt�Bt�Bs�Bt�Bt�Bs�Bs�Bs�Bs�Bu�Bt�Bt�Bt�Bu�Bu�Bt�Bu�Bv�By�Bz�B~�B� B~�B� B�B�B�1B�VB�\B�bB�oB�hB��B��B��B��B��B��B��B��B�hB�uB��B��B��B��B�{B�oB��B��B��B��B��B��B�!B�?B�FB�-B�'B�9B�^B�^B�dB�^B�^B�jB��BƨB��B��B�B�/B�;B�HB�NB�NB�TB�`B�`B�`B�`B�`B�mB�yB�B��B��B��B��B��B	  B	B	1B	
=B	DB	PB	bB	uB	�B	�B	�B	�B	�B	�B	�B	"�B	"�B	$�B	&�B	+B	,B	,B	-B	.B	2-B	8RB	:^B	<jB	C�B	G�B	I�B	J�B	K�B	M�B	S�B	W
B	]/B	aHB	dZB	gmB	jB	l�B	m�B	n�B	o�B	q�B	s�B	w�B	y�B	z�B	~�B	�B	�B	�B	�B	�+B	�1B	�DB	�DB	�bB	�bB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�3B	�?B	�LB	�XB	�^B	�jB	�qB	�qB	�qB	�qB	�qB	�qB	�qB	�qB	�wB	�}B	��B	B	B	ÖB	ĜB	ĜB	ĜB	ŢB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�;B	�BB	�HB	�HB	�TB	�ZB	�TB	�TB	�TB	�`B	�fB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
%B
%B
 �B

=B
�B
�B
#�B
+B
/B
6FB
<jB
@�B
D�B
J�B
P�B
W
B
_;B
dZB
hsB
k�B
p�B
t�B
w�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 B�4B�;B�=B�=B�4B�4B�4B�/B�1B�2B�2B�[B��B�gB�B
PB B��B��B��B9B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B#�B$�B%�B&�B)B- B/*B02B1;B1;B2;B2;B4HB<|BA�BL�BN�BRBUBVBUBT	BVBT
BY+BX$BY*Bg|Bq�Bx�Bs�Bl�BfyBVB=�B)B�B�B�B�BVB;BB��B��B�B�B�B�_B�CB�3B�B�
B��B�B�dBy�B]<BD�B.%B�B)B��B�jB�BīB�yBj�B2<B�B
��B
�<B
��B
çB
��B
�}B
�MB
�B
��B
��B
�zB
{�B
f}B
chB
\=B
P�B
F�B
7dB
.,B
%�B
�B
7B	�B	� B	�B	�zB	�GB	�5B	�B	��B	©B	�,B	��B	�RB	~B	q�B	crB	X0B	N�B	4[B	(B	!�B	�B	�B	�B	�B	
]B�B��B�B�sB�4B�B�B��B��B��B��B¶B��B�}B�kB�ZB�GB�)B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�yB�nB�bB�VB�GB�9B�4B�0B�0B#B}B}B|B|BzBx�Bv�Bv�Bu�Bt�Bs�Bs�Bs�Bt�Bt�Bs�Bt�Bt�Bs�Bs�Bs�Bs�Bu�Bt�Bt�Bt�Bu�Bu�Bt�Bu�Bv�BzB{
B"B�*B#B�+B�9B�DB�YB�}B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�HB�eB�kB�UB�LB�_B��B��B��B��B��B��B��B��B��B�B�=B�UB�`B�nB�tB�sB�yB�B�B�B�B�B�B�B��B��B��B�B�B�B	 #B	AB	TB	
`B	gB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	"�B	$�B	'	B	+%B	,+B	,*B	-0B	.5B	2OB	8rB	:B	<�B	C�B	G�B	I�B	J�B	K�B	M�B	TB	W)B	]NB	ajB	dxB	g�B	j�B	l�B	m�B	n�B	o�B	q�B	s�B	w�B	y�B	{B	B	�'B	�6B	�8B	�?B	�JB	�OB	�cB	�aB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�&B	�9B	�7B	�@B	�EB	�JB	�QB	�\B	�hB	�wB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	®B	­B	òB	ĶB	ĺB	ĺB	ſB	ſB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	�,B	�.B	�-B	�3B	�2B	�2B	�9B	�>B	�DB	�YB	�^B	�eB	�eB	�pB	�wB	�oB	�qB	�qB	�|B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	�B	�B	�B	�B
 B
 B
!B
/B
4B
7B
:B
9B
?B
AB
@B
@G�O�B

XB
�B
�B
#�B
+B
/7B
6aB
<�B
@�B
D�B
J�B
P�B
W%B
_TB
dvB
h�B
k�B
p�B
t�B
w�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.16 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214422016053112144220160531121442  AO  ARCAADJP                                                                    20150220101601    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150220101601  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150220101601  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121442  IP                  G�O�G�O�G�O�                