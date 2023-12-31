CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-12-18T20:16:35Z AOML 3.0 creation; 2016-06-01T00:08:28Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20151218201635  20160531170828  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055_7112_136                   2C  D   APEX                            5374                            041511                          846 @ׇ(�c1   @ׇ)7���@:l������c���$�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�ff@�  @���A   AA��A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�ffB�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>�C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3DyS3D� D�C3D�|�D���D��D�VfD�� D��fD�	�D�FfD��3D��fD��D�0 DچfD��fD���D�C3D�D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�=p@��
AQ�A%�AG�Ae�A���A�(�A���A���A���A���A���A���Bz�B	z�Bz�Bz�B!z�B)z�B1z�B9z�BAz�BIz�BQz�BYz�Baz�Biz�Bqz�Byz�B��qB��qB��qB��B�#�B��B��qB�WB��qB��qB��qB��qB��qB��qB��qB��qB��qBĽqBȽqB̽qBнqBԽqBؽqBܽqB��B�qB�qB�qB�qB��qB��qB��qC ^�C^�C^�C^�C^�C
^�C^�C^�C^�C^�C^�C^�C^�C^�C^�C^�C ^�C"^�C$^�C&^�C(^�C*^�C,^�C.^�C0^�C2^�C4^�C6^�C8^�C:^�C<^�C>xRC@^�CB^�CD^�CF^�CH^�CJ^�CL^�CN^�CP^�CR^�CT^�CV^�CX^�CZ^�C\^�C^^�C`^�Cb^�Cd^�Cf^�Ch^�Cj^�Cl^�Cn^�Cp^�Cr^�Ct^�Cv^�Cx^�Cz^�C|^�C~^�C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dyj�D��D�O
D���D�ȤD�(�D�b=D���D��=D�qD�R=D��
D��=D��D�;�Dڒ=D��=D��D�O
D�qD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�K�A�M�A�O�A�Q�A�Q�A�Q�A�O�A�O�A�VA�^5A�^5A�^5A�^5A�\)A�^5A�`BA�bNA�dZA�bNA�ZA�\)A�\)A�^5A�^5A�^5A�^5A�^5A�`BA�bNA�`BA�`BA�`BA�`BA�^5A�M�A�I�A�G�A�ĜA�JA��
A��/A�"�A��^A���A�dZA��A�
=A���A�?}A��TA��7A�oA���A�VA�z�A�I�A��wA��A���A��A��uA��mA�G�A�v�A���A��A��#A���A�oA�r�A�(�A���A��A�hsA�JA�z�A��RA�I�A���A���A�ZA�A��!A��`A��\A��^A�{A�hsA�/A�7LA�XA���A��A���A�~�A�^A}�
A|�!Az�/AyXAx �AvffAt�DAr1Ap�AoK�An�\An{Am��Al��Ak��Ak%Aj�+AiƨAg�
Af��Ae��AdQ�Ac�FAb�Ab(�A_�FA]|�A\~�A\�A\bA[��A[��A[hsAZ-AW&�AU��AT1ARM�AQ�AO��AO\)AN��AM�AK?}AJ��AI�AI%AHAG?}AGAF��AD�AD{AC33AB(�A@��A?��A>��A=ƨA=VA;��A9�^A8�A5��A5dZA5oA4��A4�A4~�A3?}A2  A0n�A/C�A. �A-\)A-?}A,��A,��A+�7A*�/A*ffA)��A)`BA)7LA)�A(ĜA(jA'�wA&��A&M�A%��A%S�A%33A#�A!�A �RA 1A%Av�A�A
=A �AK�A�!A�A?}A�A��AAE�A%AbNA�A��A�9A��A��A��A�A;dA�A�+A9XA��A�A�A�A
�A
�\A
  A��A�A�A�A(�A�A��AjA �A�;A�A|�A �A �@��F@��R@�E�@�X@�j@�\)@���@�A�@�$�@�1@�|�@�C�@��y@�ff@�{@�J@�j@��@���@�j@�9X@��@��m@�n�@�O�@�bN@� �@�dZ@�@�Ĝ@�I�@ߍP@��#@��/@ڗ�@�x�@�1@�-@��@���@�{@�p�@Ѓ@Ϯ@��@˾w@�+@ʸR@�V@Ɂ@��@�~�@š�@ģ�@��m@��H@��@�/@�Q�@�+@��T@�  @�+@�~�@�&�@�z�@��P@��\@�=q@��@���@�Z@���@�
=@���@���@��@��w@�|�@�S�@�V@�hs@�V@���@�j@�A�@�(�@�1@�ƨ@���@���@��@�z�@�(�@���@�\)@�@�V@�@�?}@�l�@���@��@���@�@�  @��@�^5@�{@��h@�%@���@�Ĝ@��u@�1@��@�33@��@���@�n�@���@�?}@��@��9@�A�@�b@��@�33@�ȴ@�v�@�-@�@�`B@�I�@�\)@��@���@�{@��-@��h@�p�@���@��j@��D@�Q�@�9X@�A�@�1'@��@�b@��m@��@�t�@�;d@�
=@�@��@��R@���@��+@�~�@�n�@��T@���@�X@�Q�@� �@� �@�9X@�A�@�1'@�b@��;@�K�@��\@�V@�^5@�v�@�n�@�v�@���@�v�@��@�`B@��@���@��u@�1'@��m@��@�|�@�C�@�@��@���@�n�@��@�@�@�J@�@��@��^@���@��7@��@��@��@�p�@�G�@���@��@��@�Z@�  @��;@�ƨ@���@�l�@�C�@�"�@�
=@��y@�~�@�ff@�E�@�$�@�J@���@�?}@���@�r�@�Z@� �@�  @�@l�@~��@~��@~��@~��@~v�@}�T@}�@}V@|(�@{t�@z�H@xA�@q��@hb@_�P@WK�@O��@J�H@BM�@;33@1&�@-p�@(��@#C�@�y@=q@��@��@
=@
~�@  @�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�K�A�M�A�O�A�Q�A�Q�A�Q�A�O�A�O�A�VA�^5A�^5A�^5A�^5A�\)A�^5A�`BA�bNA�dZA�bNA�ZA�\)A�\)A�^5A�^5A�^5A�^5A�^5A�`BA�bNA�`BA�`BA�`BA�`BA�^5A�M�A�I�A�G�A�ĜA�JA��
A��/A�"�A��^A���A�dZA��A�
=A���A�?}A��TA��7A�oA���A�VA�z�A�I�A��wA��A���A��A��uA��mA�G�A�v�A���A��A��#A���A�oA�r�A�(�A���A��A�hsA�JA�z�A��RA�I�A���A���A�ZA�A��!A��`A��\A��^A�{A�hsA�/A�7LA�XA���A��A���A�~�A�^A}�
A|�!Az�/AyXAx �AvffAt�DAr1Ap�AoK�An�\An{Am��Al��Ak��Ak%Aj�+AiƨAg�
Af��Ae��AdQ�Ac�FAb�Ab(�A_�FA]|�A\~�A\�A\bA[��A[��A[hsAZ-AW&�AU��AT1ARM�AQ�AO��AO\)AN��AM�AK?}AJ��AI�AI%AHAG?}AGAF��AD�AD{AC33AB(�A@��A?��A>��A=ƨA=VA;��A9�^A8�A5��A5dZA5oA4��A4�A4~�A3?}A2  A0n�A/C�A. �A-\)A-?}A,��A,��A+�7A*�/A*ffA)��A)`BA)7LA)�A(ĜA(jA'�wA&��A&M�A%��A%S�A%33A#�A!�A �RA 1A%Av�A�A
=A �AK�A�!A�A?}A�A��AAE�A%AbNA�A��A�9A��A��A��A�A;dA�A�+A9XA��A�A�A�A
�A
�\A
  A��A�A�A�A(�A�A��AjA �A�;A�A|�A �A �@��F@��R@�E�@�X@�j@�\)@���@�A�@�$�@�1@�|�@�C�@��y@�ff@�{@�J@�j@��@���@�j@�9X@��@��m@�n�@�O�@�bN@� �@�dZ@�@�Ĝ@�I�@ߍP@��#@��/@ڗ�@�x�@�1@�-@��@���@�{@�p�@Ѓ@Ϯ@��@˾w@�+@ʸR@�V@Ɂ@��@�~�@š�@ģ�@��m@��H@��@�/@�Q�@�+@��T@�  @�+@�~�@�&�@�z�@��P@��\@�=q@��@���@�Z@���@�
=@���@���@��@��w@�|�@�S�@�V@�hs@�V@���@�j@�A�@�(�@�1@�ƨ@���@���@��@�z�@�(�@���@�\)@�@�V@�@�?}@�l�@���@��@���@�@�  @��@�^5@�{@��h@�%@���@�Ĝ@��u@�1@��@�33@��@���@�n�@���@�?}@��@��9@�A�@�b@��@�33@�ȴ@�v�@�-@�@�`B@�I�@�\)@��@���@�{@��-@��h@�p�@���@��j@��D@�Q�@�9X@�A�@�1'@��@�b@��m@��@�t�@�;d@�
=@�@��@��R@���@��+@�~�@�n�@��T@���@�X@�Q�@� �@� �@�9X@�A�@�1'@�b@��;@�K�@��\@�V@�^5@�v�@�n�@�v�@���@�v�@��@�`B@��@���@��u@�1'@��m@��@�|�@�C�@�@��@���@�n�@��@�@�@�J@�@��@��^@���@��7@��@��@��@�p�@�G�@���@��@��@�Z@�  @��;@�ƨ@���@�l�@�C�@�"�@�
=@��y@�~�@�ff@�E�@�$�@�J@���@�?}@���@�r�@�Z@� �@�  @�@l�@~��@~��@~��@~��@~v�@}�T@}�@}V@|(�@{t�@z�H@xA�@q��@hb@_�P@WK�@O��@J�H@BM�@;33@1&�@-p�@(��@#C�@�y@=q@��@��@
=@
~�@  @�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB@�B@�BA�B@�B5?B!�B1B�B�;B��BɺBB�jB�XB�LB�?B�-B�B��B��B��B�oB�DB�Bp�B`BB\)BR�BH�B)�B
=B  B�B�;B��B�}B�XB�FB�'B�B��B�\B�B|�Bu�Bn�BYBB�B.B �BJB  B
��B
�yB
�
B
ȴB
�jB
�-B
��B
��B
��B
��B
�DB
� B
r�B
gmB
]/B
N�B
?}B
+B
�B
�B
\B

=B
B	��B	�B	�B	�`B	�/B	��B	��B	�dB	�'B	�B	��B	��B	�\B	~�B	w�B	v�B	u�B	t�B	s�B	o�B	dZB	R�B	G�B	?}B	5?B	.B	'�B	%�B	!�B	�B	�B	�B	uB	hB	PB	
=B		7B	%B	B	  B��B�B�B�`B�TB�HB�;B�B��B��BŢBĜBŢBŢBŢBŢBB�}B�XB�'B�B�B�B�'B�-B�3B�-B�!B�B�B�B�B�B�B��B��B��B��B��B��B��B�{B�hB�VB�JB�7B�+B�B�B�B� B|�By�Bw�Bt�Br�Bl�BhsBffBdZBbNB`BBaHBbNBaHB_;B]/BYBW
BVBS�BP�BN�BM�BK�BI�BG�BD�BB�B@�B>wB=qB=qB=qB=qB=qB=qB<jB<jB;dB:^B:^B9XB8RB7LB6FB5?B49B33B1'B1'B1'B1'B0!B0!B/B.B,B,B,B+B(�B'�B(�B+B+B+B)�B(�B)�B)�B(�B'�B'�B%�B$�B#�B"�B"�B"�B"�B#�B#�B#�B"�B%�B'�B'�B(�B(�B(�B+B,B+B)�B(�B+B/B1'B2-B5?B49B7LB:^B;dB=qB?}B@�BD�BD�BD�BD�BG�BI�BI�BM�BO�BO�BQ�BR�BQ�BT�BXBYB[#B[#B\)B\)B\)B\)B_;Be`BhsBjBjBl�Bl�Bm�Bo�Bq�Br�Bz�B{�B{�B{�B~�B�%B�VB�bB�hB��B��B��B��B��B��B��B��B��B��B�B�'B�9B�?B�XB�dB�dB�qB�}BBĜBƨBǮB��B��B�B�/B�NB�mB�B�B�B�B�B��B��B��B��B	  B	B	B	B	+B		7B	JB	bB	uB	�B	�B	�B	�B	�B	!�B	'�B	+B	,B	1'B	2-B	33B	5?B	5?B	5?B	6FB	6FB	49B	33B	9XB	>wB	@�B	A�B	C�B	F�B	J�B	O�B	T�B	XB	ZB	[#B	]/B	_;B	`BB	`BB	bNB	cTB	dZB	dZB	dZB	dZB	dZB	e`B	gmB	iyB	jB	m�B	n�B	o�B	p�B	p�B	p�B	p�B	q�B	s�B	u�B	v�B	x�B	{�B	|�B	}�B	� B	�B	�B	�B	�B	�B	�1B	�7B	�=B	�=B	�=B	�DB	�VB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	ǮB	�NB	�B
%B
\B
�B
"�B
-B
8RB
B�B
G�B
N�B
S�B
ZB
^5B
bNB
gmB
m�B
p�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B>WB>[B>[B>[B>[B>WB>[B>[B>[B>[B>[B>]B>[B>[B>]B>[B>[B>[B>[B>WB>WB>[B>[B>[B>]B>[B>]B>[B>[B>[B>[B>[B>]B>[B@hB@jBAmB@gB5$B!�BB�dB�B��BɚB�lB�JB�7B�*B�B�B��B��B��B��B�NB�B��Bp}B`"B\BR�BH�B)�B
B��B�tB�B��B�[B�4B�$B�B��B��B�4B��B|�Bu�BntBX�BBlB-�B �B(B
��B
��B
�WB
��B
ȒB
�GB
�B
��B
��B
��B
�rB
�!B
�B
r�B
gMB
]B
N�B
?\B
*�B
�B
aB
@B

B
B	��B	�B	�gB	�@B	�B	̯B	�jB	�HB	�B	��B	��B	��B	�?B	~�B	w�B	v�B	u�B	t�B	s�B	o�B	d@B	R�B	G�B	?cB	5%B	-�B	'�B	%�B	!�B	�B	uB	iB	^B	PB	6B	
#B		"B	B	�B��B��B�B�iB�GB�>B�2B�#B�B��B̸BōBćBŊBŊBŊBŋB�zB�gB�BB�B��B��B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�~B�dB�UB�AB�2B�"B�B�B��B��B�B|�By�Bw�Bt�Br�BlwBh_BfQBdCBb9B`-Ba2Bb7Ba4B_(B]BYBV�BU�BS�BP�BN�BM�BK�BI�BG�BD�BB|B@oB>dB=_B=[B=_B=CB=BB=]B<XB<UB;PB:KB:JB9EB8>B78B62B5*B4&B3 B1B1B1B1B0B0B/B-�B+�B+�B+�B*�B(�B'�B(�B*�B*�B*�B)�B(�B)�B)�B(�B'�B'�B%�B$�B#�B"�B"�B"�B"�B#�B#�B#�B"�B%�B'�B'�B(�B(�B(�B*�B+�B*�B)�B(�B*�B/B1B2B5)B4!B75B:GB;MB=[B?fB@kBD�BD�BD�BD�BG�BI�BI�BM�BO�BO�BQ�BR�BQ�BT�BW�BX�B[
B[B\B\B\B\B_ BeGBhZBjdBjbBloBlqBmuBo�Bq�Br�Bz�B{�B{�B{�B~�B�
B�:B�FB�JB�bB�xB�}B�|B��B��B��B��B��B��B��B�	B�B�#B�9B�EB�GB�RB�`B�oB�~BƆBǌB˨B��B��B�B�/B�MB�_B�jB�vB�B�B��B��B��B��B��B	 �B	�B	�B	B		B	'B	?B	RB	fB	xB	�B	�B	�B	!�B	'�B	*�B	+�B	1B	2	B	3B	5B	5B	5B	6"B	6 B	4B	3B	95B	>SB	@`B	AeB	CrB	F�B	J�B	O�B	T�B	W�B	Y�B	Z�B	]
B	_B	`B	`B	b(B	c.B	d3B	d6B	d5B	d2B	d4B	e:B	gFB	iSB	j[B	mlB	nrB	owB	p~B	p~B	p~B	p|B	q�B	s�B	u�B	v�B	x�B	{�B	|�B	}�B	�B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�.B	�GB	�GB	�MB	�VB	�YB	�YB	�eB	�kB	�xB	�xB	�xB	�xB	��B	��B	��B	��B	��B	��B	��B	ǅB	�!B	�B
�B
1B
pB
"�B
,�B
8'B
BdB
G�B
N�B
S�B
Y�B
^	B
b#B
gBB
mdB
pwB
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.37 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708282016053117082820160531170828  AO  ARCAADJP                                                                    20151218201635    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151218201635  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151218201635  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170828  IP                  G�O�G�O�G�O�                