CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-08-23T02:16:01Z AOML 3.0 creation; 2016-08-07T21:51:20Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150823021601  20160807145121  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               GA   AO  5287_9017_071                   2C  D   APEX                            6529                            072314                          846 @�i�"��1   @�i��(@0���l�D�d��`A�71   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    GA   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB��B'��B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�ffB���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%y�D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` Dx��D���D�C3D�l�D���D��D�9�D�� D��3D���D�9�D�s3D��3D� D�FfDڙ�D๚D��fD�FfD�ffD��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�=p@��
A�A%�AE�Ae�A���A���A���A���A���A���A���A���Bz�B	z�Bz�B�GB!{B){B1z�B9z�BAz�BIz�BQz�BYz�Baz�Biz�Bqz�Byz�B��qB��qB��qB�#�B��>B��qB��>B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBĽqBȽqB̽qBнqBԽqBؽqBܽqB�qB�qB�qB�qB�qB��qB��qB��qC ^�C^�CxRCxRC^�C
^�C^�C^�C^�C^�C^�C^�C^�C^�C^�C^�C ^�C"^�C$^�C&^�C(^�C*^�C,^�C.^�C0^�C2^�C4^�C6^�C8^�C:^�C<^�C>^�C@^�CB^�CD^�CF^�CH^�CJ^�CL^�CN^�CP^�CR^�CT^�CV^�CX^�CZ^�C\^�C^^�C`^�Cb^�Cd^�Cf^�Ch^�Cj^�Cl^�Cn^�Cp^�Cr^�Ct^�Cv^�Cx^�Cz^�C|^�C~^�C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�<)C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�"�C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��DD��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%�HD&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dtw�DyHD��D�O
D�x�D��qD�(�D�EqD���D��
D��D�EqD�
D��
D��D�R=DڥqD��qD�=D�R=D�r=D��=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�FA��A柾A晚A敁A敁A敁A�uA�p�A��A��A�v�A�I�A� �A�
=A��A���A�ĜA�-A���A�^5A�dZA؟�A؍PA��A���Aգ�Aԟ�A�&�A���A��/A�A�A�x�A�ƨA��A�{Aș�A��HA�^5A�VAÃA�+A��TA�%A���A�?}A�~�A�A�A�jA��+A�+A��A�VA�1A���A���A��A�7LA�p�A�ĜA�`BA���A�VA�n�A���A��PA�XA��-A��^A��A��A��A���A��!A�(�A�jA�A��9A�ȴA�ĜA�S�A�oA�\)A���A�9XA�1'A�O�A�/A�VA��wA�ȴA�ĜA���A�?}A�33A�;dA���A���A�K�A�M�A��A��A�`BA�XA\)A|M�A{33Ay��Aw;dAr�ApZAn$�AkC�Ai?}Af�RAfbAe��AeO�Ab�uA\�AX�AX�+AW��AW+ATr�AO�#ANE�AM��AM33AJI�AH5?ADȴABr�AA�A@M�A>�yA=\)A;�mA9"�A8{A7dZA6bNA4M�A2ĜA1A.�A-�;A-�A,��A+�TA+��A+S�A)�;A)x�A)l�A)33A(�A(��A( �A'�PA&�+A"�!A33A�A1A|�AI�A��A�A$�A�yA��A"�AbNA�A��A�;A�PA�A��A��An�A�TAS�A�RA�+A"�A�DAA`BA�A
�+A
1A	�hA	XA	VA�A�9A1'A�mA��AdZA33A��A�A��A��A�hA|�A�A��A�#A�AK�A 1@���@���@��@���@��7@�r�@�bN@��7@�x�@��-@�x�@�/@��/@��u@���@�p�@��9@��;@�S�@�^5@��@�5?@�J@��@��@�Z@��@�w@�P@�ƨ@�S�@�^5@���@���@��@��@�$�@��@�t�@�\)@�ȴ@�@�Ĝ@�A�@� �@�l�@�+@��@��@��@䛦@�(�@�  @�;d@�E�@��@�@�`B@�j@�Q�@��@���@ߝ�@�33@�n�@���@�?}@ܴ9@��m@�K�@ڗ�@�-@��@���@١�@��@�9X@�dZ@�"�@��@���@��@ԓu@�1'@�|�@�
=@�~�@�E�@�v�@�v�@�M�@���@љ�@�hs@��@�j@���@�33@���@�v�@���@�?}@�bN@�  @��@���@�;d@���@�^5@�-@��@�@�Ĝ@�j@�Z@Ǯ@�@���@�=q@��T@��@�A�@öF@�S�@�@�@��7@�%@���@�1'@�b@�33@��!@�E�@��-@��u@�j@�Z@�(�@��@��@�K�@���@��@���@�V@��j@��D@�A�@��@��
@��w@���@�33@���@�E�@��@��#@��@��@��#@��#@�V@���@�V@�=q@�-@�J@��@�x�@��@���@�A�@�1@��;@�33@�ȴ@��!@��+@�@���@���@�ff@���@��\@�~�@�E�@�J@��@�@�x�@��`@��D@�9X@��w@�K�@�o@��!@�^5@��#@��^@��h@�X@�?}@���@��j@��u@�r�@�A�@��m@�l�@��H@��\@�@��@���@�Q�@�A�@��@���@�dZ@���@�5?@�J@��@�@�x�@��`@�Z@��;@�t�@�+@�n�@��@�?}@�&�@�%@��`@�Ĝ@��@��;@��P@��@�v�@���@�7L@��@��9@��
@��F@��@�|�@�C�@�o@��H@�ȴ@��R@���@���@���@�n�@�$�@�J@�@��^@�x�@�X@�?}@��@��;@���@��!@��-@��-@��@�o@x��@o+@f@_+@V�y@O\)@G��@AG�@:^5@3@,(�@&5?@!G�@V@�9@��@7L11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�FA��A柾A晚A敁A敁A敁A�uA�p�A��A��A�v�A�I�A� �A�
=A��A���A�ĜA�-A���A�^5A�dZA؟�A؍PA��A���Aգ�Aԟ�A�&�A���A��/A�A�A�x�A�ƨA��A�{Aș�A��HA�^5A�VAÃA�+A��TA�%A���A�?}A�~�A�A�A�jA��+A�+A��A�VA�1A���A���A��A�7LA�p�A�ĜA�`BA���A�VA�n�A���A��PA�XA��-A��^A��A��A��A���A��!A�(�A�jA�A��9A�ȴA�ĜA�S�A�oA�\)A���A�9XA�1'A�O�A�/A�VA��wA�ȴA�ĜA���A�?}A�33A�;dA���A���A�K�A�M�A��A��A�`BA�XA\)A|M�A{33Ay��Aw;dAr�ApZAn$�AkC�Ai?}Af�RAfbAe��AeO�Ab�uA\�AX�AX�+AW��AW+ATr�AO�#ANE�AM��AM33AJI�AH5?ADȴABr�AA�A@M�A>�yA=\)A;�mA9"�A8{A7dZA6bNA4M�A2ĜA1A.�A-�;A-�A,��A+�TA+��A+S�A)�;A)x�A)l�A)33A(�A(��A( �A'�PA&�+A"�!A33A�A1A|�AI�A��A�A$�A�yA��A"�AbNA�A��A�;A�PA�A��A��An�A�TAS�A�RA�+A"�A�DAA`BA�A
�+A
1A	�hA	XA	VA�A�9A1'A�mA��AdZA33A��A�A��A��A�hA|�A�A��A�#A�AK�A 1@���@���@��@���@��7@�r�@�bN@��7@�x�@��-@�x�@�/@��/@��u@���@�p�@��9@��;@�S�@�^5@��@�5?@�J@��@��@�Z@��@�w@�P@�ƨ@�S�@�^5@���@���@��@��@�$�@��@�t�@�\)@�ȴ@�@�Ĝ@�A�@� �@�l�@�+@��@��@��@䛦@�(�@�  @�;d@�E�@��@�@�`B@�j@�Q�@��@���@ߝ�@�33@�n�@���@�?}@ܴ9@��m@�K�@ڗ�@�-@��@���@١�@��@�9X@�dZ@�"�@��@���@��@ԓu@�1'@�|�@�
=@�~�@�E�@�v�@�v�@�M�@���@љ�@�hs@��@�j@���@�33@���@�v�@���@�?}@�bN@�  @��@���@�;d@���@�^5@�-@��@�@�Ĝ@�j@�Z@Ǯ@�@���@�=q@��T@��@�A�@öF@�S�@�@�@��7@�%@���@�1'@�b@�33@��!@�E�@��-@��u@�j@�Z@�(�@��@��@�K�@���@��@���@�V@��j@��D@�A�@��@��
@��w@���@�33@���@�E�@��@��#@��@��@��#@��#@�V@���@�V@�=q@�-@�J@��@�x�@��@���@�A�@�1@��;@�33@�ȴ@��!@��+@�@���@���@�ff@���@��\@�~�@�E�@�J@��@�@�x�@��`@��D@�9X@��w@�K�@�o@��!@�^5@��#@��^@��h@�X@�?}@���@��j@��u@�r�@�A�@��m@�l�@��H@��\@�@��@���@�Q�@�A�@��@���@�dZ@���@�5?@�J@��@�@�x�@��`@�Z@��;@�t�@�+@�n�@��@�?}@�&�@�%@��`@�Ĝ@��@��;@��P@��@�v�@���@�7L@��@��9@��
@��F@��@�|�@�C�@�o@��H@�ȴ@��R@���@���@���@�n�@�$�@�J@�@��^@�x�@�X@�?}@��@��;@���G�O�@��-@��-@��@�o@x��@o+@f@_+@V�y@O\)@G��@AG�@:^5@3@,(�@&5?@!G�@V@�9@��@7L11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
s�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
q�B
p�B
m�B
m�B
l�B
k�B
n�B
s�B
u�B
�B
r�B
jB
�B
��B
�^B
��B
�B�Bw�B\)BN�BN�BW
B�JB��BuB/BD�BH�Bt�B}�B�=B��B��B��B�VB�JB�+B�%B�{B�B�TBĜB�RB��B�Bx�Bm�BbNBhsBcTBe`Bk�BN�BA�B<jB9XB8RB8RB�B  B�B�B�ZB�BȴB��BPBPB�B�BB�B��B��B��B��B�}B�3B�7Bp�B@�B�B%BB
��B
�sB
�B
ŢB
�dB
�FB
�^B
�B
�B
�B
��B
��B
�B
dZB
L�B
A�B
33B
�B
B	�B	�HB	��B	��B	�3B	�B	�B	��B	�oB	s�B	]/B	XB	P�B	J�B	:^B	%�B	�B	�B	�B		7B��B�B�yB�ZB�HB�B�B��B��B��B��B��B��B��BȴBŢBĜBƨBɺB��B��B��B�B�B�B��B��B	  B	B	PB	�B	DB��B	  B	+B	1B	B��B�B�fB�)B�B��B	B	B		7B	VB	uB	�B	+B	/B	33B	8RB	9XB	:^B	=qB	C�B	F�B	K�B	Q�B	S�B	Q�B	Q�B	VB	XB	[#B	_;B	bNB	bNB	bNB	e`B	ffB	gmB	jB	m�B	o�B	q�B	s�B	t�B	u�B	s�B	t�B	s�B	o�B	]/B	Q�B	N�B	L�B	K�B	K�B	J�B	XB	m�B	x�B	�B	�B	�B	�B	�1B	�1B	�B	�B	�+B	�1B	�+B	�1B	�VB	�hB	�uB	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	�B	�9B	�LB	�^B	�^B	�qB	�qB	�wB	��B	��B	B	ÖB	ĜB	ƨB	ŢB	ŢB	ŢB	ȴB	��B	��B	ɺB	ȴB	ǮB	ɺB	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�/B	�;B	�HB	�HB	�BB	�HB	�HB	�NB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�ZB	�NB	�NB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�TB	�ZB	�fB	�`B	�sB	�yB	�yB	�yB	�yB	�sB	�fB	�`B	�ZB	�TB	�ZB	�`B	�`B	�`B	�`B	�TB	�NB	�HB	�BB	�HB	�TB	�ZB	�ZB	�ZB	�fB	�mB	�fB	�`B	�ZB	�TB	�NB	�NB	�HB	�HB	�BB	�BB	�;B	�;B	�;B	�;B	�;B	�BB	�NB	�ZB	�ZB	�`B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
  B	��B	��B
  B
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
+B
+B
+B
+B
%B
B
+B
+B
+B
+B
+B
%B
%B
B
	7B

=B
	7B
1B
1B
1B

=B
JB
JB
PB
PB
PB
PB
VB
VB
\B
\B
bB
bB
bB
bB
bB
bB
bB
bB
bB
\B
\B
\B
\B
hB
\B
oB
�B
#�B
&�B
/B
5?B
<jB
D�B
G�B
L�B
Q�B
W
B
\)B
aHB
ffB
jB
n�B
q�B
u�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
s�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
q�B
p�B
m{B
mzB
ltB
koB
n�B
s�B
u�B
�B
r�B
jhB
�B
��B
�DB
��B
�B�Bw�B\BN�BN�BV�B�/B��BXB.�BD|BH�Bt�B}�B�!B��B��B�tB�5B�*B�B�B�\B��B�7B�|B�4B��B��Bx�BmqBb.BhSBc2Be@BkfBN�BAkB<LB96B8-B80BtB��B�B�^B�6B��BȐBϻB2B/B�|B�B��B��B��B��BʝB�VB�B�Bp�B@`BoBB�B
��B
�PB
��B
ŀB
�@B
�%B
�>B
��B
��B
��B
�gB
��B
��B
d9B
L�B
AgB
3B
�B
�B	��B	�)B	μB	�iB	�B	��B	��B	��B	�TB	s�B	]B	W�B	P�B	J�B	:EB	%�B	�B	�B	iB		!B��B�B�dB�FB�/B�B��B��B��B��B��B��B��BʬBȜBŉBąBƐBɣB��B��B��B�eB�B�B��B��B��B	B	6B	�B	)B��B��B	B	B	�B��B�B�JB�B�tB��B	�B	B		B	9B	XB	B	*�B	.�B	3B	84B	9:B	:?B	=RB	CyB	F�B	K�B	Q�B	S�B	Q�B	Q�B	U�B	W�B	[B	_B	b-B	b,B	b,B	e>B	fFB	gLB	j_B	moB	o|B	q�B	s�B	t�B	u�B	s�B	t�B	s�B	o}B	]B	Q�B	N�B	L�B	K�B	K�B	J�B	W�B	mnB	x�B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�B	�	B	�B	�2B	�EB	�SB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�(B	�9B	�;B	�JB	�KB	�TB	�^B	�dB	�iB	�pB	�vB	ƁB	�{B	�|B	�}B	ȎB	ʞB	ʛB	ɔB	ȎB	ǋB	ɖB	ͬB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�!B	�!B	�B	� B	�"B	�&B	�2B	�2B	�4B	�:B	�7B	�9B	�2B	�'B	�)B	�(B	�/B	�4B	�1B	�3B	�4B	�8B	�,B	�1B	�@B	�7B	�LB	�RB	�RB	�QB	�RB	�LB	�@B	�8B	�2B	�,B	�4B	�9B	�7B	�8B	�8B	�,B	�&B	� B	�B	� B	�.B	�2B	�/B	�1B	�>B	�BB	�=B	�7B	�1B	�)B	�$B	�'B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�$B	�3B	�1B	�9B	�VB	�oB	�B	�B	�B	�B	�B	�|B	�vB	�pB	�jB	�nB	�pB	�qB	�vB	�wB	�wB	�vB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
B
B
B
B
�B
�B
B
B
B
B
B
�B
�B
�B
	B

B
	B
B
B
	B

B
!B
"B
%B
%B
&B
#B
.B
.B
1B
3B
8B
9B
8B
9B
8B
9B
7B
:B
:B
3B
2B
1B
2G�O�B
3B
DB
�B
#�B
&�B
.�B
5B
<?B
DpB
G�B
L�B
Q�B
V�B
[�B
aB
f;B
jTB
njB
qB
u�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.37 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451212016080714512120160807145121  AO  ARCAADJP                                                                    20150823021601    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150823021601  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150823021601  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145121  IP                  G�O�G�O�G�O�                