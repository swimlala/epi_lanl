CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-05-07T09:15:38Z AOML 3.0 creation; 2016-06-01T00:08:24Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150507091538  20160531170824  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               rA   AO  4055_7112_114                   2C  D   APEX                            5374                            041511                          846 @�N����1   @�N�_�P@;�z�G��dD�9Xb1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    rA   A   A   @�33@�  A   A   A@  A`  A���A�  A�  A�33A�  A�  A�  A�  B   B  B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,�C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CA�fCD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy�fD�fD�<�D�y�D���D��D�L�D�y�D�� D�fD�6fD���D��3D�fD�9�D�p D๚D��D�#3D��D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�
=@��
A�A%�AE�Ae�A�A���A���A�(�A���A���A���A���Bz�B	z�B{Bz�B!z�B)z�B1z�B9z�BAz�BIz�BQz�BYz�Baz�Biz�Bqz�Byz�B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBĽqBȽqB̽qBнqBԽqBؽqBܽqB�qB�qB�qB�qB�qB��qB��qB��qC ^�C^�C^�C^�C^�C
^�C^�C^�C^�C^�C^�C^�C^�C^�C^�C^�C ^�C"^�C$^�C&^�C(^�C*^�C,xRC.^�C0^�C2^�C4^�C6^�C8^�C:^�C<^�C>^�C@^�CBECD^�CF^�CH^�CJ^�CL^�CN^�CP^�CR^�CT^�CV^�CX^�CZ^�C\^�C^^�C`^�Cb^�Cd^�Cf^�Ch^�Cj^�Cl^�Cn^�Cp^�Cr^�Ct^�Cv^�Cx^�Cz^�C|^�C~^�C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�<)C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt��Dy�D�=D�H�D��qD�ؤD�(�D�X�D��qD���D�"=D�B=D��qD��
D�=D�EqD�{�D��qD��qD�/
D�(�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A��7A�7LA�^5A��A��RA��A���A��
A�t�A��\A�\)A��A���A�ȴA��7A��!A���A�^5A�oA���A��FA�jA��`A�K�A�9XA�hsA�~�A�VA�%A�p�A��A��;A��9A���A��9A��PA�ƨA�bNA�r�A��DA��yA��-A���A�n�A��HA���A�\)A���A�+A��A���A�A�-A��A�  A��9A���A��DA��A�z�A�`BA�1A�-A���A��A�ȴA�~�A�dZA��A�bA�ȴA���A��DA�n�A�{A�ȴA�\)A��yA���A�jA��A��mA�p�A���A�C�A�ȴA�r�A�{A���A�dZA�A���A�-A���A�z�A�=qA��A��A�oA���A��;A��-A�z�A���A��A�+A��DA���A�M�A�v�A��RA~��AyhsAw��AvJAsVAq"�An�uAk/Ai�Ail�Ag+AgG�Ahz�AhȴAcƨAc�7AdM�Abv�Ab�jAb�Ab�/Aa��Aa�A`=qA`1'A`A_�TA_&�A^jA]�;A\�A[33AZ��AYXAXv�AV�9AU�-ATn�ARM�ARAQ�APȴAP�+AN�yAN9XANbAN5?AL��AK`BAJ��AI��AI?}AH��AG��AF��AF-AE��AE7LAC��AC/AB�RAA�A?��A>5?A=t�A<��A;ƨA;dZA:ffA8M�A8A7�
A7K�A6ĜA6�A5�A4A�A1�
A0v�A/�A/+A.��A-��A-��A-l�A-&�A,n�A+��A+hsA+��A*-A(�A'��A'hsA&5?A$��A$VA#�FA#hsA"1'A ��A Q�A&�AI�A��A$�A\)A��A�uA�AbA�A��A�A/A
=A��A�9A�A��AE�A��A�A�FAM�A
=Al�A
M�A	��A��A33A�A;dA�!A9XA�A�
AƨA�FA��A�A\)AVA �RA bN@��y@�5?@�I�@��u@��y@�@�$�@�x�@�Ĝ@�33@�{@웦@�I�@�33@���@��@�j@�t�@�h@�@�dZ@�ȴ@�`B@߶F@�J@݉7@ܬ@؋D@��@�O�@�/@��@���@��`@��@�Ĝ@�Z@�b@�\)@�^5@�7L@��@�o@͉7@˅@�V@�G�@�I�@�\)@�"�@��@Ƈ+@�=q@Ł@�%@ģ�@�K�@��^@��
@���@�bN@��@�K�@��@�ȴ@��#@��9@�K�@�E�@�p�@��@�(�@�1'@�1'@�1'@�ƨ@�n�@�7L@�
=@�x�@��9@�Q�@�b@�l�@���@��^@��u@�j@�Z@�9X@�b@��F@�|�@��@�C�@�t�@���@�|�@��!@���@��T@���@�+@��@���@��\@���@���@�r�@��@�+@��@�5?@��@�b@���@�
=@�ȴ@�E�@�x�@�%@��@�z�@�I�@��@��F@���@�K�@�"�@�o@��H@��+@�M�@�J@��7@��@�j@�  @���@�dZ@�ȴ@�x�@�Ĝ@�1@��m@���@��@���@��P@�t�@�l�@�dZ@�S�@�C�@�;d@�+@���@��+@��@��@��@���@��@��@��@�ff@�=q@�-@��#@��^@��-@��-@���@���@���@�G�@��9@�j@�b@��F@��@���@���@��@�S�@�+@�@�ȴ@�~�@�n�@�V@�=q@�5?@���@��@��T@��^@���@��7@�/@��@�  @�33@��y@��@���@�
=@��@�O�@���@��j@�Z@�1@�  @�1@�  @~��@~�R@~��@~��@~ff@~{@~{@~{@~@}p�@}V@|�/@|�D@{�@z�H@y�#@v��@qhs@hQ�@ax�@Y�@SdZ@N��@A��@<�D@6�R@17L@,�@$(�@ 1'@��@"�@��@��@`B@Ĝ@`B111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A��7A�7LA�^5A��A��RA��A���A��
A�t�A��\A�\)A��A���A�ȴA��7A��!A���A�^5A�oA���A��FA�jA��`A�K�A�9XA�hsA�~�A�VA�%A�p�A��A��;A��9A���A��9A��PA�ƨA�bNA�r�A��DA��yA��-A���A�n�A��HA���A�\)A���A�+A��A���A�A�-A��A�  A��9A���A��DA��A�z�A�`BA�1A�-A���A��A�ȴA�~�A�dZA��A�bA�ȴA���A��DA�n�A�{A�ȴA�\)A��yA���A�jA��A��mA�p�A���A�C�A�ȴA�r�A�{A���A�dZA�A���A�-A���A�z�A�=qA��A��A�oA���A��;A��-A�z�A���A��A�+A��DA���A�M�A�v�A��RA~��AyhsAw��AvJAsVAq"�An�uAk/Ai�Ail�Ag+AgG�Ahz�AhȴAcƨAc�7AdM�Abv�Ab�jAb�Ab�/Aa��Aa�A`=qA`1'A`A_�TA_&�A^jA]�;A\�A[33AZ��AYXAXv�AV�9AU�-ATn�ARM�ARAQ�APȴAP�+AN�yAN9XANbAN5?AL��AK`BAJ��AI��AI?}AH��AG��AF��AF-AE��AE7LAC��AC/AB�RAA�A?��A>5?A=t�A<��A;ƨA;dZA:ffA8M�A8A7�
A7K�A6ĜA6�A5�A4A�A1�
A0v�A/�A/+A.��A-��A-��A-l�A-&�A,n�A+��A+hsA+��A*-A(�A'��A'hsA&5?A$��A$VA#�FA#hsA"1'A ��A Q�A&�AI�A��A$�A\)A��A�uA�AbA�A��A�A/A
=A��A�9A�A��AE�A��A�A�FAM�A
=Al�A
M�A	��A��A33A�A;dA�!A9XA�A�
AƨA�FA��A�A\)AVA �RA bN@��y@�5?@�I�@��u@��y@�@�$�@�x�@�Ĝ@�33@�{@웦@�I�@�33@���@��@�j@�t�@�h@�@�dZ@�ȴ@�`B@߶F@�J@݉7@ܬ@؋D@��@�O�@�/@��@���@��`@��@�Ĝ@�Z@�b@�\)@�^5@�7L@��@�o@͉7@˅@�V@�G�@�I�@�\)@�"�@��@Ƈ+@�=q@Ł@�%@ģ�@�K�@��^@��
@���@�bN@��@�K�@��@�ȴ@��#@��9@�K�@�E�@�p�@��@�(�@�1'@�1'@�1'@�ƨ@�n�@�7L@�
=@�x�@��9@�Q�@�b@�l�@���@��^@��u@�j@�Z@�9X@�b@��F@�|�@��@�C�@�t�@���@�|�@��!@���@��T@���@�+@��@���@��\@���@���@�r�@��@�+@��@�5?@��@�b@���@�
=@�ȴ@�E�@�x�@�%@��@�z�@�I�@��@��F@���@�K�@�"�@�o@��H@��+@�M�@�J@��7@��@�j@�  @���@�dZ@�ȴ@�x�@�Ĝ@�1@��m@���@��@���@��P@�t�@�l�@�dZ@�S�@�C�@�;d@�+@���@��+@��@��@��@���@��@��@��@�ff@�=q@�-@��#@��^@��-@��-@���@���@���@�G�@��9@�j@�b@��F@��@���@���@��@�S�@�+@�@�ȴ@�~�@�n�@�V@�=q@�5?@���@��@��T@��^@���@��7@�/@��@�  @�33@��y@��@���@�
=@��@�O�@���@��j@�Z@�1@�  @�1@�  @~��@~�R@~��@~��@~ff@~{@~{@~{@~@}p�@}V@|�/@|�D@{�@z�H@y�#@v��@qhs@hQ�@ax�@Y�@SdZ@N��@A��@<�D@6�R@17L@,�@$(�@ 1'@��@"�@��@��@`B@Ĝ@`B111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBPB
=B%BB  B  B{B'�B49B>wBK�BN�BK�BF�B@�B>wB<jB6FB?}BG�BG�BD�BB�BA�B7LB>wBM�BXBW
BR�BH�B?}BO�B_;Bk�Bn�Bq�Br�Bp�Bm�Bm�Bm�Bl�BjBffB]/BXBP�BB�B6FB2-B-B�BDB�B�#B��B��B��B��B��BƨB�qB�B��B��B�bB�7B�%B|�BjBdZBaHB^5B[#BS�BL�BC�B;dB6FB0!B"�B
=B  B��B�B�B�fB�ZB�/B��B��B��BB�^B�FB�!B��B��B��B~�BiyBcTB[#B>wBhB
��B\B
�B
�B
ȴB
�3B
��B
bNB
_;B
L�B
%�B
PB	�B	��B	�qB	ǮB	�?B	ĜB	�)B	�NB	�B	�LB	��B	�dB	��B	ÖB	ÖB	�9B	�B	��B	��B	�B	�B	��B	��B	��B	��B	�JB	�1B	w�B	jB	Q�B	C�B	2-B	�B	�B	{B	hB	JB	
=B	
=B	JB	�B	{B	PB	DB	
=B	1B	JB		7B	B	B��B��B��B��B��B�B�mB�TB�NB�BB�)B�B��B��B��B��B��B��BȴBĜB�}B�XB�9B�-B�'B�!B�!B�B�B�B�B��B�B�RB�B��B��B��B��B�{B�oB�hB�\B�DB�1B�B� Bz�Bw�By�By�Bu�Bo�BjBgmBe`B_;B^5B_;B_;B^5B^5B^5B]/B\)BYBVBQ�BN�BJ�BG�BE�BB�B>wB7LB33B2-B1'B1'B1'B1'B0!B0!B0!B0!B/B.B-B+B'�B&�B#�B!�B!�B �B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BoB{B�B�B�B�B�B�B�B�B�B{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B!�B"�B%�B)�B-B/B/B/B/B0!B0!B1'B1'B2-B49B7LB7LB6FB6FB6FB7LB9XB>wBC�BD�BE�BD�BE�BF�BH�BL�BM�BM�BN�BO�BQ�BVBXB]/BcTBffBgmBhsBjBjBl�Bn�Bn�Bo�Bp�Bq�Bs�Bu�Bv�By�Bz�B{�B�B�1B�=B�PB�VB�bB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�-B�?B�^B��BŢB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�5B�5B�BB�TB�ZB�fB�B�B�B�B��B��B��B��B��B��B��B��B��B	B	B	+B	1B	1B		7B	
=B	JB	PB	\B	oB	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	"�B	%�B	(�B	)�B	)�B	+B	+B	-B	1'B	0!B	2-B	49B	6FB	7LB	8RB	8RB	8RB	9XB	;dB	=qB	=qB	>wB	>wB	?}B	?}B	?}B	?}B	B�B	D�B	D�B	E�B	I�B	K�B	P�B	bNB	�1B	��B	�LB	�B	�B
+B
oB
�B
(�B
33B
9XB
F�B
L�B
P�B
VB
\)B
aHB
ffB
m�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B5B
 BB �B��B��B[B'�B4B>YBK�BN�BK�BF�B@cB>XB<LB6(B?^BG�BG�BD~BBrBAkB70B>XBM�BW�BV�BR�BH�B?`BO�B_BkgBnyBq�Br�Bp�BmtBmpBmsBliBj_BfGB]BW�BP�BBkB6'B2B,�B�B!B�{B��B��BδB̮B˪BʚBƊB�NB��B��B�cB�AB�B�B|�Bj]Bd7Ba"B^B[BS�BL�BCpB;?B6 B/�B"�B
B��B��B�^B�ZB�CB�7B�
B��B��B̪B�lB�9B�B� B��B��B�pB~�BiUBc/B[ B>UBEB
��B8B
�B
��B
ȔB
�B
��B
b1B
_B
L�B
%�B
1B	�zB	ˬB	�VB	ǒB	�"B	ĀB	�B	�.B	��B	�0B	ͶB	�LB	�hB	�xB	�xB	�B	��B	��B	��B	��B	��B	��B	��B	��B	�eB	�-B	�B	w�B	jdB	Q�B	C|B	2B	�B	�B	dB	RB	0B	
&B	
%B	3B	nB	bB	9B	+B	
%B	B	3B		B	B	�B��B��B��B��B��B��B�SB�<B�8B�+B�B�B��B��B��B��B��B˯BȟBĈB�eB�AB�$B�B�B�B�B��B��B��B��B��B��B�9B�B��B��B�B�jB�gB�[B�UB�JB�/B�B�B�Bz�Bw�By�By�Bu�Bo�BjjBgZBeLB_)B^"B_'B_(B^#B^B^#B]B\BYBU�BQ�BN�BJ�BG�BE�BB}B>dB78B3"B2B1B1B1B1B0B/�B0B0B/	B.B,�B*�B'�B&�B#�B!�B!�B �B �B �B�B�B�B�B�B�B~BxBpBlB~B~BvBSBRBmBmBSB?BfBkBlBmBnBSBmBmBkBQBfBKBKBmBwBzBxBjB�B�B�B�B�B�B�B�B �B �B �B!�B"�B%�B)�B,�B/B/B/B/B0B0B1B1B2B4"B74B73B6-B6.B60B74B9CB>aBC|BD�BE�BD�BE�BF�BH�BL�BM�BM�BN�BO�BQ�BU�BW�B]Bc9BfLBgSBhYBjcBjdBlqBn}Bn~Bo�Bp�Bq�Bs�Bu�Bv�By�Bz�B{�B��B�B�!B�4B�9B�EB�]B�kB�rB�xB�zB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B� B�?B�jBŃBʢB˧B˦B̮BͲBͲBκBθBκBϿB��B��B��B��B��B�B�B�!B�6B�9B�EB�pB�B�B�B��B��B��B��B��B��B��B��B��B	 �B	�B	B	B	B		B	
B	%B	.B	;B	KB	lB	oB	yB	�B	�B	�B	�B	�B	 �B	!�B	"�B	%�B	(�B	)�B	)�B	*�B	*�B	,�B	1B	/�B	2B	4B	6%B	7*B	8.B	8,B	8.B	95B	;@B	=NB	=LB	>SB	>PB	?YB	?YB	?WB	?YB	BjB	DyB	DxB	E}B	I�B	K�B	P�B	b)B	�	B	��B	�%B	��B	�B
B
DB
�B
(�B
3B
9,B
F{B
L�B
P�B
U�B
[�B
aB
f;B
meB
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.37 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708242016053117082420160531170824  AO  ARCAADJP                                                                    20150507091538    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150507091538  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150507091538  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170824  IP                  G�O�G�O�G�O�                