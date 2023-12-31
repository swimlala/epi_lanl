CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-10-14T22:01:53Z AOML 3.0 creation; 2016-06-01T00:08:21Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20141014220153  20160531170821  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               ^A   AO  4055_7112_094                   2C  D   APEX                            5374                            041511                          846 @��	�` 1   @�����@9z��vȴ�dO�;dZ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ^A   A   A   @�ff@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B��B   B(  B0  B8  B@ffBI33BN��BX  B`  Bh  Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D���D�9�D���D���D�fD�FfD�y�D�� D��D�6fD���DǶfD���D�@ Dڙ�D��3D���D�FfD�i�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�=p@��
AQ�A%�AE�Ae�A���A���A���A���A���A���A���A���Bz�B	z�Bz�B{B!z�B)z�B1z�B9z�BA�GBJ�BPG�BYz�Baz�Biz�Bq{Byz�B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBĽqBȽqB̽qBнqBԽqBؽqBܽqB�qB�qB�qB�qB�qB��qB��qB��qC ^�C^�C^�C^�C^�C
^�C^�C^�C^�C^�C^�C^�C^�C^�C^�C^�C ^�C"^�C$^�C&^�C(^�C*^�C,^�C.^�C0^�C2^�C4^�C6^�C8^�C:^�C<^�C>^�C@^�CB^�CD^�CF^�CH^�CJ^�CL^�CN^�CP^�CR^�CT^�CV^�CX^�CZ^�C\^�C^^�C`^�Cb^�Cd^�Cf^�Ch^�Cj^�Cl^�Cn^�Cp^�Cr^�Ct^�Cv^�Cx^�Cz^�C|^�C~^�C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�{Dy�HD��D�EqD���D��qD�"=D�R=D��qD���D�%qD�B=D��qD��=D���D�K�DڥqD��
D�qD�R=D�uqD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A���A���A���A���A�A�A���A���A�A��#A��A��A�AߋDA�t�A�O�A�1AݬA�jA�C�A�ZAְ!A�%A�ffA�ZA�^5A��#A�ZA�+AžwAüjA�oA�VA�%A���A�  A��\A�C�A�+A�p�A�-A�"�A���A�5?A�S�A�^5A��+A��\A�1'A���A�A�A�JA��A���A���A���A���A�XA�5?A��+A��\A�ȴA�/A��/A��A��A�/A���A�ZA��RA�K�A��A�"�A���A��A�r�A�t�A��A�l�A���A��RA��A��A�=qA��A��A��jA�I�A���A�K�A�r�A�"�A�I�A�=qA���A��`A�1A���A��PA�oA��;A���A�5?A�|�A�M�A�ƨA~�A~n�A~  A|  Az�RAz=qAxĜAx�Aw�AvffAu��As�;AooAl�Aj{Ah��AgK�AfZAe��Ae�Adn�Ac33Aa�PAa�7Aa%A_?}A]�A[��AZ��AY�-AW�mAW33AVĜAU+AR=qAOdZAM33AL�AK�^AG��ABE�AA+A?��A>��A>��A>�A=�A=XA<n�A<bA:{A6�A4ĜA2��A2-A1/A0��A01'A-�A,{A+G�A*��A*n�A)�hA(��A(bNA'/A%�;A$  A"�DA!t�A �A��Az�A-A{A��A`BAĜA1'A�FA&�A��AbNAbNAZA�wA�A��A�+AQ�A-A{A�-A�AbA|�A�+AG�AS�A�HA�/AAC�A��Ar�A�A�7A\)AoA
ȴA
n�A	�TA5?A�;A�jA��A?}A��A��Ao@��y@�%@�Q�@�1'@��;@���@�@�E�@���@���@��T@�D@�\@�9X@���@@��-@�bN@�J@柾@�&�@��@�w@���@�@��@�1@�K�@�v�@��
@�n�@Ցh@���@�(�@ӕ�@��@�M�@���@Ѳ-@щ7@��@Л�@�"�@ͩ�@��m@�dZ@��@�v�@ɲ-@�Ĝ@�(�@�dZ@�
=@Ƨ�@�{@ź^@ŉ7@�X@���@�$�@�{@��D@�v�@���@��w@�{@�p�@���@��9@�j@�A�@�b@�+@�V@�V@�r�@�
=@��T@��^@��@�`B@�bN@�dZ@�"�@��@��R@�v�@�{@�x�@���@�Z@��@��@��@�ff@�M�@��@��#@��h@�`B@��@��j@��m@���@�@�&�@�bN@��@��m@�+@��@�$�@��7@�I�@��@��@�p�@�G�@���@���@�  @���@�C�@���@���@��/@��@��m@�"�@���@��!@�^5@��@��7@�7L@���@�1@�|�@��H@��R@�n�@�=q@��@�{@��@��#@���@�O�@��@�A�@���@��w@��F@�l�@�S�@�;d@�
=@��@��!@�~�@��@��#@�`B@�Q�@��@�C�@�+@���@��R@�n�@���@���@��`@�Ĝ@��D@�I�@� �@�1@���@��m@���@��@��P@�;d@���@��y@��R@�v�@�V@�$�@��@�@���@��h@�`B@�G�@��@���@���@��@���@�Z@�1@��
@��@��@���@���@��P@�|�@�|�@�S�@�+@��@���@��R@���@���@���@���@�ff@�{@��T@�@�@��@��@�Ĝ@��u@�z�@�bN@�I�@�1'@�  @l�@~��@}�@|�/@|�j@|�@|�D@|�@z��@yX@x�@w�w@v��@v��@v$�@u�-@u/@t��@t�@t�@t�@t�/@t��@t��@t��@t�D@t�@q�#@hA�@a�7@X�u@Pb@HĜ@CdZ@=V@6v�@1hs@-`B@(1'@$��@��@��@�
@��@�j@|�@�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A���A���A���A���A�A�A���A���A�A��#A��A��A�AߋDA�t�A�O�A�1AݬA�jA�C�A�ZAְ!A�%A�ffA�ZA�^5A��#A�ZA�+AžwAüjA�oA�VA�%A���A�  A��\A�C�A�+A�p�A�-A�"�A���A�5?A�S�A�^5A��+A��\A�1'A���A�A�A�JA��A���A���A���A���A�XA�5?A��+A��\A�ȴA�/A��/A��A��A�/A���A�ZA��RA�K�A��A�"�A���A��A�r�A�t�A��A�l�A���A��RA��A��A�=qA��A��A��jA�I�A���A�K�A�r�A�"�A�I�A�=qA���A��`A�1A���A��PA�oA��;A���A�5?A�|�A�M�A�ƨA~�A~n�A~  A|  Az�RAz=qAxĜAx�Aw�AvffAu��As�;AooAl�Aj{Ah��AgK�AfZAe��Ae�Adn�Ac33Aa�PAa�7Aa%A_?}A]�A[��AZ��AY�-AW�mAW33AVĜAU+AR=qAOdZAM33AL�AK�^AG��ABE�AA+A?��A>��A>��A>�A=�A=XA<n�A<bA:{A6�A4ĜA2��A2-A1/A0��A01'A-�A,{A+G�A*��A*n�A)�hA(��A(bNA'/A%�;A$  A"�DA!t�A �A��Az�A-A{A��A`BAĜA1'A�FA&�A��AbNAbNAZA�wA�A��A�+AQ�A-A{A�-A�AbA|�A�+AG�AS�A�HA�/AAC�A��Ar�A�A�7A\)AoA
ȴA
n�A	�TA5?A�;A�jA��A?}A��A��Ao@��y@�%@�Q�@�1'@��;@���@�@�E�@���@���@��T@�D@�\@�9X@���@@��-@�bN@�J@柾@�&�@��@�w@���@�@��@�1@�K�@�v�@��
@�n�@Ցh@���@�(�@ӕ�@��@�M�@���@Ѳ-@щ7@��@Л�@�"�@ͩ�@��m@�dZ@��@�v�@ɲ-@�Ĝ@�(�@�dZ@�
=@Ƨ�@�{@ź^@ŉ7@�X@���@�$�@�{@��D@�v�@���@��w@�{@�p�@���@��9@�j@�A�@�b@�+@�V@�V@�r�@�
=@��T@��^@��@�`B@�bN@�dZ@�"�@��@��R@�v�@�{@�x�@���@�Z@��@��@��@�ff@�M�@��@��#@��h@�`B@��@��j@��m@���@�@�&�@�bN@��@��m@�+@��@�$�@��7@�I�@��@��@�p�@�G�@���@���@�  @���@�C�@���@���@��/@��@��m@�"�@���@��!@�^5@��@��7@�7L@���@�1@�|�@��H@��R@�n�@�=q@��@�{@��@��#@���@�O�@��@�A�@���@��w@��F@�l�@�S�@�;d@�
=@��@��!@�~�@��@��#@�`B@�Q�@��@�C�@�+@���@��R@�n�@���@���@��`@�Ĝ@��D@�I�@� �@�1@���@��m@���@��@��P@�;d@���@��y@��R@�v�@�V@�$�@��@�@���@��h@�`B@�G�@��@���@���@��@���@�Z@�1@��
@��@��@���@���@��P@�|�@�|�@�S�@�+@��@���@��R@���@���@���@���@�ff@�{@��T@�@�@��@��@�Ĝ@��u@�z�@�bN@�I�@�1'@�  @l�@~��@}�@|�/@|�j@|�@|�D@|�@z��@yX@x�@w�w@v��@v��@v$�@u�-@u/@t��@t�@t�@t�@t�/@t��@t��@t��@t�D@t�@q�#@hA�@a�7@X�u@Pb@HĜ@CdZ@=V@6v�@1hs@-`B@(1'@$��@��@��@�
@��@�j@|�@�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��BhB�B9XB:^B7LB5?B1'B&�B�B�B\B	7BB��B�B��B��B��BĜB�XB�LB�FB�9B�'B�B��B��B��B��B��B�bB�DB�Bz�Bp�BbNBN�B2-B+B�B�B�B�B�B�BhBB��B��B�B�sB�B��BĜB�wB�RB�B��B��B�{B�=B�Bw�Bm�BdZBZBO�B?}B5?B)�B�B+B��B�fBƨB�B��B�JB�Bu�Bm�B\)BB�B�B1B
�B
��B
��B
�jB
�FB
�B
��B
��B
��B
�PB
x�B
`BB
M�B
I�B
C�B
49B
)�B
$�B
�B
oB

=B
B	��B	�B	��B	��B	�B	��B	��B	�{B	�\B	�DB	�%B	}�B	s�B	r�B	m�B	cTB	YB	O�B	G�B	A�B	7LB	49B	0!B	$�B	{B	DB	B	  B��B�B�HB�B��B��B��B��BɺBƨBB�}B�jB�^B�B��B��B��B��B��B�oB�VB�JB�=B�7B�%B�B�B~�Bz�Bw�Bt�Br�Bp�Bn�Bl�Bk�Bk�BiyBhsBffBe`BcTBbNBaHB`BB`BB_;B]/B\)B[#BZBZBYBXBW
BT�BT�BQ�BN�BI�BG�BD�BF�BE�BD�BC�BA�B@�B>wB=qB<jB;dB9XB5?B0!B-B+B)�B(�B&�B%�B"�B!�B �B �B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B!�B"�B#�B$�B$�B&�B'�B'�B(�B(�B(�B'�B+B,B+B+B.B0!B33B8RB9XB:^B;dB;dB;dB;dB<jB>wBA�BB�BF�BJ�BJ�BJ�BJ�BM�BP�BQ�BQ�BR�BR�BS�BVBW
BZBZB\)B_;B`BB`BBaHBaHBaHBaHBcTBe`BgmBk�Bo�Bq�Bu�Bv�Bw�B{�B|�B� B�B�+B�\B�uB��B��B��B��B��B��B��B��B��B�B�B�'B�LB�^B�jB�wB��BÖBŢBɺB��B��B�
B�B�#B�/B�5B�5B�;B�BB�BB�ZB�B�B�B�B�B��B��B��B��B��B��B��B	B	B	1B	hB	�B	�B	�B	�B	�B	 �B	$�B	+B	,B	-B	.B	1'B	2-B	33B	49B	49B	5?B	6FB	7LB	9XB	;dB	;dB	<jB	>wB	?}B	@�B	A�B	B�B	C�B	C�B	D�B	E�B	F�B	F�B	G�B	G�B	G�B	J�B	L�B	M�B	P�B	P�B	Q�B	R�B	S�B	T�B	VB	XB	YB	ZB	[#B	\)B	\)B	]/B	]/B	\)B	^5B	`BB	aHB	dZB	hsB	iyB	l�B	n�B	p�B	p�B	q�B	r�B	r�B	s�B	u�B	v�B	z�B	{�B	{�B	|�B	|�B	|�B	�B	�B	�+B	�=B	�PB	�VB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	B	�)B	�B
B
uB
�B
'�B
2-B
:^B
@�B
G�B
K�B
R�B
[#B
aHB
dZB
jB
p�B
s�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��BTB�B9FB:MB78B5-B1B&�B�BlBIB	!B�B��B�wB��B��B��BāB�?B�2B�.B�B�B��B��B��B��B��B�xB�HB�+B��Bz�Bp�Bb2BN�B2B*�B�B�B�B�B�B�BIB B��B��B�B�UB��BͳB�zB�WB�0B��B��B��B�\B�B��Bw�BmoBd7BY�BO�B?YB5B)�BqBB��B�BBƄB��B��B�)B��Bu�BmlB\BBlB�BB
�nB
δB
�`B
�JB
�&B
��B
��B
��B
��B
�/B
x�B
`#B
M�B
I�B
CvB
4B
)�B
$�B
uB
OB

!B
�B	��B	�eB	̰B	�gB	��B	��B	��B	�`B	�@B	�(B	�	B	}�B	s�B	r�B	mwB	c:B	X�B	O�B	G�B	AmB	75B	4B	0	B	$�B	fB	*B	B��B��B�zB�0B� B��B��BͼB˰BɥBƓB�yB�gB�UB�JB��B��B��B��B��B��B�\B�AB�6B�)B�$B�B�B��B~�Bz�Bw�Bt�Br�Bp�Bn�BlxBktBkpBieBhbBfQBeMBc@Bb;Ba6B`/B`.B_&B]B\B[BZ
BZ	BYBW�BV�BT�BT�BQ�BN�BI�BG�BD�BF�BE�BD�BC�BAtB@mB>cB=_B<WB;PB9FB5-B0B,�B*�B)�B(�B&�B%�B"�B!�B �B �B �B �B�B�B�BByBsBjB~B�BdBzBzBWBnBYBxBWBXB~BxBxBwB[BoBgBxBuB�B�B�B�B�B�B�B�B�B�BwB�B �B!�B!�B"�B#�B$�B$�B&�B'�B'�B(�B(�B(�B'�B*�B+�B*�B*�B-�B0B3B8;B9@B:GB;LB;LB;KB;LB<QB>_BAqBBwBF�BJ�BJ�BJ�BJ�BM�BP�BQ�BQ�BR�BR�BS�BU�BV�BZBZB\B_B`'B`)Ba/Ba,Ba.Ba-Bc9BeEBgRBkkBo�Bq�Bu�Bv�Bw�B{�B|�B�B��B�B�AB�XB�pB�wB�}B��B��B��B��B��B��B��B��B�B�-B�@B�KB�XB�kB�wBŁBəBηB��B��B��B� B�B�B�B�B�"B�#B�9B�cB�pB�B�B�B��B��B��B��B��B��B��B	�B	�B	B	DB	oB	zB	�B	�B	�B	 �B	$�B	*�B	+�B	,�B	-�B	1B	2	B	3B	4B	4B	5B	6"B	7%B	92B	;@B	;AB	<FB	>TB	?XB	@_B	AhB	BkB	CoB	CqB	DxB	E~B	F�B	F�B	G�B	G�B	G�B	J�B	L�B	M�B	P�B	P�B	Q�B	R�B	S�B	T�B	U�B	W�B	X�B	Y�B	Z�B	\B	\B	]
B	]B	\B	^B	`B	a#B	d6B	hNB	iSB	lcB	nrB	p|B	p}B	q�B	r�B	r�B	s�B	u�B	v�B	z�B	{�B	{�B	|�B	|�B	|�B	��B	��B	�B	�B	�(B	�-B	�>B	�HB	�VB	�^B	�`B	�YB	�YB	�`B	�^B	�`B	�`B	�_B	�sB	��B	�hB	�B	�B
�B
IB
�B
'�B
2B
:2B
@VB
G�B
K�B
R�B
Z�B
aB
d-B
jSB
pzB
s�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.37 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708212016053117082120160531170821  AO  ARCAADJP                                                                    20141014220153    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20141014220153  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20141014220153  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170821  IP                  G�O�G�O�G�O�                