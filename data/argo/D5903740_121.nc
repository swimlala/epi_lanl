CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-07-18T02:15:22Z AOML 3.0 creation; 2016-06-01T00:08:25Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150718021522  20160531170825  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               yA   AO  4055_7112_121                   2C  D   APEX                            5374                            041511                          846 @�`�Յ�	1   @�`�h݀
@:9������dTA�7K�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    yA   A   A   @�ff@�  A   A   A@  Aa��A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B'��B0  B8  B@  BH  BPffBXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DTfDT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy� D�3D�L�D��fD���D�fD�I�D�� D�� D���D�FfD�� D�� D�3D�@ D�s3D���D�	�D�6fD�l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�=p@��
A�A%�AE�Ag�A�A���A���A���A���A���A���A���Bz�B	z�Bz�Bz�B!z�B){B1z�B9z�BAz�BIz�BQ�GBY�GBaz�Biz�Bqz�Byz�B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBĽqBȽqB̽qBнqBԽqBؽqBܽqB�qB�qB�qB�qB�qB��qB��qB��qC ^�C^�C^�C^�C^�C
^�C^�C^�C^�C^�C^�C^�C^�C^�C^�C^�C ^�C"^�C$^�C&^�C(^�C*^�C,^�C.^�C0^�C2^�C4^�C6^�C8^�C:^�C<^�C>^�C@^�CB^�CD^�CF^�CH^�CJ^�CL^�CN^�CP^�CR^�CT^�CV^�CX^�CZ^�C\^�C^^�C`^�Cb^�Cd^�Cf^�Ch^�Cj^�Cl^�Cn^�Cp^�Cr^�Ct^�Cv^�Cx^�Cz^�C|^�C~^�C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�<)C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DTDT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�{Dy��D�
D�X�D��=D�ؤD�=D�UqD���D���D��D�R=D���D���D�
D�K�D�
D�ؤD�qD�B=D�x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�/A�I�A�?}A�E�A��HA��yA�I�A�K�A��HA�E�A���Aʴ9Aʟ�Aʰ!A���AʮA�9XA���A�ƨA��#A�E�AÏ\A�^5A��wA�hsA���A��A�jA���A�A�A��7A��9A�n�A���A�(�A���A��HA��A�v�A�$�A��A�33A��A���A���A�  A��wA��A�A��RA�G�A�&�A���A��+A��A���A��A�$�A�`BA��A�&�A�G�A�VA���A�G�A���A���A�`BA���A��-A�33A��\A���A�$�A�=qA�n�A��A���A��A���A�~�A�x�A�ffA�JA�ƨA�&�A�-A���A�S�A�|�A�9XA�E�A���A�;dA��A���A��TA�Q�A~�jA~=qA}��A|ZAz�!Az�AyƨAy�-Ay%Aw��Av��Atv�As��As/Ar�+Ap�DAo�Ao�PAn��Am��Am��Am\)Am
=Al9XAk\)Ail�Ah  AgƨAg�7AghsAg33AfĜAfbNAe��Ae�PAd��Ad��Ac�Ab1'A`ZA_�A\�jA[`BAZ�!AZ^5AY�mAY�AXM�AW�#AV�\AU�PAT��ATffAS�AR��ARVAR �AQ��AP�AO��AN1'AK��AJ�DAI��AH^5AFVAEoADn�AC`BAB��ABbNABAAx�A@�9A@{A?�wA>�!A=��A=A=�^A=�PA=x�A=G�A=�A<�yA<�9A<JA:��A:-A9�hA7�A61A5��A5�-A5"�A3��A2�yA2��A2�A1�A0n�A.ȴA-/A*ĜA)%A(�\A'��A&v�A%�PA%
=A$��A$^5A#�A!�hA!VA �yA ȴA �\A�A�yA=qAhsAA��A-A�FA��A|�AdZA�A
=A��AĜAA�A^5A\)A�yA �A�Al�A�RA�Ap�A7LA��AbAG�A�!A9XA&�A�uA�AĜAZA�#AhsA
�yA
~�A	�A	33A��A�+A-AG�At�A�/AVA��A�A ��@�dZ@��@��9@���@�$�@�dZ@�1@�S�@�
=@��T@��@�@���@�-@�@��@�x�@�b@�~�@�%@ڧ�@�bN@׍P@�=q@Ԭ@Ӿw@�|�@�+@�E�@�p�@��@�&�@���@ˮ@�S�@�J@�/@�A�@���@�C�@��@�hs@�
=@�z�@��m@�"�@��@�ȴ@���@�M�@�@��^@�O�@���@�r�@�+@��@���@�Z@��F@���@���@�?}@��u@�Z@��w@�n�@�Z@��
@���@���@��@��F@�t�@��R@�M�@��T@��@���@�~�@���@��
@���@�E�@��7@��D@�9X@�  @��@��w@�v�@�X@��@��j@��D@��;@��@��@���@���@�`B@�hs@�X@��`@�z�@�I�@�A�@�A�@� �@�  @�  @��F@�o@��@���@�x�@�O�@��/@���@��u@�j@� �@��@��H@���@�-@��#@��-@�p�@�V@�Q�@��@��w@�ƨ@���@��m@��
@�S�@��@��+@��@�x�@��@��/@��@��u@��@�z�@�r�@�z�@�j@�I�@�b@���@�33@��@���@��@�hs@���@��@���@��@��
@���@���@�ȴ@���@�ff@�-@�{@��T@��^@��7@�O�@���@��9@���@��D@�r�@�Q�@���@�;d@��y@�V@�-@�-@�$�@�-@�$�@���@�%@�I�@�@�@�@}�T@|�@{33@z�@x��@x�9@x�u@xbN@xQ�@w��@v�R@v@u�h@uO�@u?}@u/@u/@uV@t��@tz�@tZ@t9X@sƨ@s@rM�@p  @c@`��@Z��@S��@Mp�@F5?@>{@7��@1G�@,Z@'K�@!��@?}@%@��@��@
�H@��@�/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�/A�I�A�?}A�E�A��HA��yA�I�A�K�A��HA�E�A���Aʴ9Aʟ�Aʰ!A���AʮA�9XA���A�ƨA��#A�E�AÏ\A�^5A��wA�hsA���A��A�jA���A�A�A��7A��9A�n�A���A�(�A���A��HA��A�v�A�$�A��A�33A��A���A���A�  A��wA��A�A��RA�G�A�&�A���A��+A��A���A��A�$�A�`BA��A�&�A�G�A�VA���A�G�A���A���A�`BA���A��-A�33A��\A���A�$�A�=qA�n�A��A���A��A���A�~�A�x�A�ffA�JA�ƨA�&�A�-A���A�S�A�|�A�9XA�E�A���A�;dA��A���A��TA�Q�A~�jA~=qA}��A|ZAz�!Az�AyƨAy�-Ay%Aw��Av��Atv�As��As/Ar�+Ap�DAo�Ao�PAn��Am��Am��Am\)Am
=Al9XAk\)Ail�Ah  AgƨAg�7AghsAg33AfĜAfbNAe��Ae�PAd��Ad��Ac�Ab1'A`ZA_�A\�jA[`BAZ�!AZ^5AY�mAY�AXM�AW�#AV�\AU�PAT��ATffAS�AR��ARVAR �AQ��AP�AO��AN1'AK��AJ�DAI��AH^5AFVAEoADn�AC`BAB��ABbNABAAx�A@�9A@{A?�wA>�!A=��A=A=�^A=�PA=x�A=G�A=�A<�yA<�9A<JA:��A:-A9�hA7�A61A5��A5�-A5"�A3��A2�yA2��A2�A1�A0n�A.ȴA-/A*ĜA)%A(�\A'��A&v�A%�PA%
=A$��A$^5A#�A!�hA!VA �yA ȴA �\A�A�yA=qAhsAA��A-A�FA��A|�AdZA�A
=A��AĜAA�A^5A\)A�yA �A�Al�A�RA�Ap�A7LA��AbAG�A�!A9XA&�A�uA�AĜAZA�#AhsA
�yA
~�A	�A	33A��A�+A-AG�At�A�/AVA��A�A ��@�dZ@��@��9@���@�$�@�dZ@�1@�S�@�
=@��T@��@�@���@�-@�@��@�x�@�b@�~�@�%@ڧ�@�bN@׍P@�=q@Ԭ@Ӿw@�|�@�+@�E�@�p�@��@�&�@���@ˮ@�S�@�J@�/@�A�@���@�C�@��@�hs@�
=@�z�@��m@�"�@��@�ȴ@���@�M�@�@��^@�O�@���@�r�@�+@��@���@�Z@��F@���@���@�?}@��u@�Z@��w@�n�@�Z@��
@���@���@��@��F@�t�@��R@�M�@��T@��@���@�~�@���@��
@���@�E�@��7@��D@�9X@�  @��@��w@�v�@�X@��@��j@��D@��;@��@��@���@���@�`B@�hs@�X@��`@�z�@�I�@�A�@�A�@� �@�  @�  @��F@�o@��@���@�x�@�O�@��/@���@��u@�j@� �@��@��H@���@�-@��#@��-@�p�@�V@�Q�@��@��w@�ƨ@���@��m@��
@�S�@��@��+@��@�x�@��@��/@��@��u@��@�z�@�r�@�z�@�j@�I�@�b@���@�33@��@���@��@�hs@���@��@���@��@��
@���@���@�ȴ@���@�ff@�-@�{@��T@��^@��7@�O�@���@��9@���@��D@�r�@�Q�@���@�;d@��y@�V@�-@�-@�$�@�-@�$�@���@�%@�I�@�@�@�@}�T@|�@{33@z�@x��@x�9@x�u@xbN@xQ�@w��@v�R@v@u�h@uO�@u?}@u/@u/@uV@t��@tz�@tZ@t9X@sƨ@s@rM�@p  @c@`��@Z��@S��@Mp�@F5?@>{@7��@1G�@,Z@'K�@!��@?}@%@��@��@
�H@��@�/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�;BhB+B33B.B"�B�BVB��B�B�B�BB2-B^5Bu�B}�B{�Bx�B|�B�B�+B�B}�B{�Bw�Bq�BjBffBaHB]/BZBS�BS�BR�BP�BL�BF�BC�B@�B;dB'�B�BhBB�B�mB�
B�B�1B�VB��B�XBȴB�wB��B�BffBT�BD�B-B!�B�B�BoBJBB�B�TB��BɺB�}B�?B��B�oB�%B� Bz�Bt�Bk�BhsBhsBffBaHB\)BP�BD�B;dB.B�BhBB
��B
�B
�B
�yB
�fB
�/B
��B
��B
ŢB
�9B
��B
��B
��B
��B
�oB
�%B
|�B
l�B
dZB
bNB
bNB
R�B
M�B
H�B
@�B
8RB
6FB
49B
1'B
,B
$�B
�B
PB

=B
	7B
1B
+B
%B
B
B
B	��B	��B	�B	�mB	�B	ǮB	�FB	�B	��B	��B	��B	��B	�oB	�VB	�1B	�B	� B	|�B	y�B	t�B	t�B	s�B	s�B	n�B	dZB	YB	F�B	7LB	,B	 �B	�B	\B	DB	+B	1B	%B	B	  B��B��B��B��B��B	B	%B	JB	bB	oB	uB	oB	hB	bB	JB	1B	B��B�B�B�B�B�B�sB�B�sB�ZB�BB�B��BŢB��B�}B�jB�LB�9B�'B�!B�B��B��B��B��B��B��B��B��B�{B�oB�bB�\B�PB�JB�JB�DB�=B�=B�7B�1B�%B�B}�Bz�Bx�Bv�Bu�Bs�Bp�Bn�Bm�Bk�BiyBgmBdZBcTB`BB^5B[#BYBW
BT�BS�BQ�BP�BM�BL�BK�BI�BH�BF�BB�B>wB<jB9XB5?B2-B0!B.B,B)�B'�B$�B#�B"�B"�B"�B!�B �B�B�B�B�B�B�B�B{BuB{B�B{BuBuBoBoBoBoBoBhB�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B!�B!�B!�B!�B"�B"�B#�B#�B#�B%�B(�B)�B)�B)�B-B/B0!B2-B2-B33B5?B<jB=qBA�BD�BG�BL�BM�BQ�BR�BS�BS�BVB[#B`BBdZBiyBjBm�Bt�Bv�Bx�Bx�Bw�B|�B�B�B�B�B�B�%B�+B�7B�=B�JB�JB�PB�VB�bB�hB�hB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B�B�!B�-B�3B�?B�LB�RB�XB�^B�wBBĜBĜBĜBŢBȴB��B��B��B�B�
B�B�#B�#B�BB�ZB�ZB�fB�B�B�B�B��B��B��B��B��B	B	%B	+B	+B	DB	JB	JB	PB	VB	\B	bB	uB	{B	�B	�B	�B	�B	$�B	$�B	%�B	%�B	&�B	&�B	)�B	1'B	2-B	33B	5?B	6FB	8RB	9XB	9XB	9XB	;dB	<jB	>wB	>wB	?}B	D�B	G�B	G�B	H�B	K�B	L�B	M�B	N�B	N�B	O�B	VB	\)B	_;B	aHB	aHB	aHB	aHB	bNB	cTB	e`B	ffB	ffB	hsB	jB	p�B	r�B	��B	�B	�B
B
\B
�B
'�B
2-B
:^B
A�B
G�B
N�B
T�B
[#B
`BB
ffB
l�B
p�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�BMB*�B3B-�B"�B�B<B��B�hB�fB�zBB2B^Bu�B}�B{�Bx�B|�B��B�B��B}�B{�Bw�Bq�BjdBfLBa-B]BZBS�BS�BR�BP�BL�BF�BCxB@bB;DB'�BtBEB �B�B�NB��B��B�B�0B��B�5BȑB�UB��B��BfDBT�BDwB,�B!�B�BdBIB#B�B��B�1B��BɖB�XB�B��B�GB�B�Bz�Bt�BkeBhOBhOBfDBa&B\BP�BDxB;?B-�BjBDB �B
��B
�}B
�hB
�VB
�BB
�B
ϺB
ʟB
�}B
�B
��B
��B
�~B
��B
�MB
�B
|�B
ljB
d8B
b.B
b-B
R�B
M�B
H�B
@dB
81B
6'B
4B
1B
+�B
$�B
uB
0B

B
	B
B
B
B
B
�B
�B	��B	��B	�B	�NB	��B	ǐB	�(B	��B	��B	��B	��B	�pB	�RB	�8B	�B	��B	�B	|�B	y�B	t�B	t�B	s�B	s�B	nB	d@B	Y B	F�B	73B	+�B	 �B	gB	CB	*B	B	B	B	�B��B��B��B��B��B��B	�B	B	0B	IB	UB	\B	TB	QB	IB	1B	B	�B��B��B�B�yB�wB�{B�ZB�mB�[B�CB�+B��B˯BŌB�rB�eB�RB�4B�$B�B�B��B��B��B��B��B��B��B��B�wB�cB�XB�MB�IB�;B�5B�7B�.B�)B�)B�"B�B�B��B}�Bz�Bx�Bv�Bu�Bs�Bp�Bn�Bm}BksBicBgWBdEBc?B`-B^"B[BYBV�BT�BS�BQ�BP�BM�BL�BK�BI�BH�BF�BB}B>aB<VB9(B5*B2B0B. B+�B)�B'�B$�B#�B"�B"�B"�B!�B �B�BsBdBXBYBlBoBfBFBLBRBJB_BaB?B\BAB[BZB:BOB}B|BxB�BkB�BqBkB�B�BvB�B�B!�B!�B!�B!�B!�B"�B"�B#�B#�B#�B%�B(�B)�B)�B)�B,�B/B0B2B2B3B5$B<RB=]BAoBD�BG�BL�BM�BQ�BR�BS�BS�BU�B[
B`(Bd>Bi]BjdBmxBt�Bv�Bx�Bx�Bw�B|�B��B��B��B��B�B�	B�B�B� B�,B�-B�3B�:B�EB�JB�MB�QB�WB�^B�iB�pB��B��B��B��B��B��B��B��B��B��B�B�B�B�B�-B�3B�6B�?B�XB�qB�}B�~BĀBņBȕB��B��B��B��B��B��B�B��B�"B�9B�9B�GB�iB�wB�|B��B��B��B��B��B��B	�B	B	B	B	!B	&B	%B	.B	4B	;B	@B	RB	ZB	iB	xB	�B	�B	$�B	$�B	%�B	%�B	&�B	&�B	)�B	1B	2B	3B	5B	6"B	8,B	94B	94B	96B	;?B	<FB	>TB	>PB	?XB	DyB	G�B	G�B	H�B	K�B	L�B	M�B	N�B	N�B	O�B	U�B	\B	_B	a!B	a$B	a"B	a"B	b'B	c/B	e:B	fAB	f?B	hMB	jZB	p~B	r�B	��B	��B	�B
�B
3B
sB
'�B
2B
:1B
A_B
G�B
N�B
T�B
Z�B
`B
f;B
l_B
pxB
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.37 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708252016053117082520160531170825  AO  ARCAADJP                                                                    20150718021522    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150718021522  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150718021522  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170825  IP                  G�O�G�O�G�O�                