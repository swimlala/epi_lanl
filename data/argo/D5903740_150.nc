CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-05-10T09:15:56Z AOML 3.0 creation; 2016-06-01T00:08:30Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160510091556  20160826101911  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055_7112_150                   2C  D   APEX                            5374                            041511                          846 @ת����1   @ת���s	@;�n��P�c�G�z�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dys3D���D�P D��fD�� D�	�D�S3D�vfD���D��D�I�D���D��3D��D�9�D�vfD�� D���D�6fD�vfD��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��G@�z�A=qA&=qAF=qAf=qA��A��A��A��A��A��A��A��B�\B	�\B�\B�\B!�\B)�\B1�\B9��BA�\BI�\BQ�\BY�\Ba�\Bi�\Bq�\By�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC c�Cc�Cc�Cc�Cc�C
c�Cc�Cc�Cc�Cc�Cc�Cc�Cc�Cc�Cc�Cc�C c�C"c�C$c�C&c�C(c�C*c�C,c�C.c�C0c�C2c�C4c�C6c�C8c�C:c�C<c�C>c�C@c�CBc�CDc�CFc�CHc�CJc�CLc�CNc�CPc�CRc�CTc�CVc�CXc�CZc�C\c�C^c�C`c�Cbc�Cdc�Cf}qChc�Cjc�Clc�Cnc�Cpc�Crc�Ctc�Cvc�Cxc�Czc�C|c�C~c�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�%C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�>�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dy�)D�D�\{D���D��{D�D�_�D���D��D�HD�VD��D�ϮD�HD�FDڂ�D��{D�	HD�B�D��D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�G�A�G�A�E�A�E�A�E�A�E�A�G�A�G�A�G�A�G�A�E�A�G�A�G�A�G�A�E�A�G�A�C�A�A�A�A�A�;dA�;dA�9XA�7LA�/A��A�A��HA��-A��hA�`BA�A�A�A�ĜA�E�A�S�A��uA�`BA�7LA�-A�oA���A��/A��9A�x�A�hsA�VA�I�A�1'A��A���A�/A��A��#A���A�ƨA�^5A�ȴA�(�A�-A���A���A�I�A�ƨA�VA��-A��
A�33A�A�
=A��A���A�`BA��A��HA��A��wA���A�S�A�VA�|�A�S�A�C�A��A��A��A��#A�K�A��A�oA�  A�ffA�jA�Q�A��`A��A�r�A�I�A�JA��wA��yA��`A�5?A��#A���A�bNA��/A�oA���A�/A��A���A���A�C�A��;A�v�A��wA��A~�A}�^A|�!A{|�Az�Az-Ay��AyhsAw�PAu;dAt1As"�Aq��Ap1Ao�-AoG�AnZAm��Am��AmK�Al^5Aj�Ad~�Ab1AaA`��A_�A^��A]�A\�AZVAX��AW��ATĜAQ��AO��AO�FAN��AM��AM"�AL~�AK�hAJjAHM�AF  AD�+AC�^AB=qAAl�A@��A?��A>��A=p�A<Q�A;��A:�jA:bA8��A6�A5��A4��A4  A2z�A1�#A1t�A0�A.�RA-dZA,��A,A�A,JA+XA*JA)\)A(�/A(A�A'�
A'`BA&��A&�A%�7A$�HA$v�A$5?A#33A"r�A"JA!�hA!oA {A�^Ax�A�/A �A��A
=A�AVA�yA �A��A�HA �A��A�A|�A�Ar�Ax�AZA�PAjA�FA33A��AE�A��AS�AVA�^A
��A	A�A-A�A
=A9XAS�A�DA�-Ar�A ��A $�@�{@�%@�S�@�v�@�@�p�@���@�Z@� �@�@���@���@�  @��H@�{@�O�@�Z@@�o@���@���@��@�1@�S�@�ff@��@�Ĝ@� �@��@���@�A�@�l�@އ+@�X@���@��@�=q@�&�@�1@�;d@�V@�x�@�1'@�=q@�O�@�Z@�$�@�Z@��y@��`@Ǯ@���@�hs@öF@�X@�33@�^5@���@��@�C�@�~�@���@���@��@��#@�V@���@�b@�o@��h@��j@� �@��@���@��@��@��P@��y@��@�{@�`B@��@��/@�A�@��@�C�@�~�@�%@�z�@�ƨ@��!@�@�x�@�%@���@�I�@�dZ@��!@�5?@�J@���@��7@�X@�/@���@�9X@�(�@�1@��@�33@��@��@��!@��#@��@��@�(�@�b@�ƨ@�K�@�n�@��@���@���@��@�A�@�(�@� �@�b@���@�1'@��@��;@���@�t�@�
=@��R@�ff@�{@�x�@��@�r�@�b@���@��;@�ƨ@���@���@�M�@�{@�@�J@��@��^@�x�@��`@�Ĝ@�r�@�A�@��m@��w@�33@�~�@�V@�-@��@�J@�@���@�%@��/@��D@�I�@� �@���@�dZ@�;d@��R@�J@���@���@�A�@�1@���@��
@��F@��@��;@���@�;d@���@���@��+@�ff@�M�@�@�x�@�X@�G�@�&�@��`@���@�z�@�j@�bN@�A�@��@��;@��@���@��@�
=@���@�ȴ@�ff@��#@��7@��@�p�@�X@�O�@�G�@��@�%@��`@��9@��@�bN@�9X@�  @�@�w@�P@K�@+@~�y@~�@~��@|�@up�@l(�@d��@Zn�@Q7L@J��@F�y@?��@7�@2�@*��@'
=@!&�@��@$�@��@
=@(�@	7L@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�G�A�G�A�E�A�E�A�E�A�E�A�G�A�G�A�G�A�G�A�E�A�G�A�G�A�G�A�E�A�G�A�C�A�A�A�A�A�;dA�;dA�9XA�7LA�/A��A�A��HA��-A��hA�`BA�A�A�A�ĜA�E�A�S�A��uA�`BA�7LA�-A�oA���A��/A��9A�x�A�hsA�VA�I�A�1'A��A���A�/A��A��#A���A�ƨA�^5A�ȴA�(�A�-A���A���A�I�A�ƨA�VA��-A��
A�33A�A�
=A��A���A�`BA��A��HA��A��wA���A�S�A�VA�|�A�S�A�C�A��A��A��A��#A�K�A��A�oA�  A�ffA�jA�Q�A��`A��A�r�A�I�A�JA��wA��yA��`A�5?A��#A���A�bNA��/A�oA���A�/A��A���A���A�C�A��;A�v�A��wA��A~�A}�^A|�!A{|�Az�Az-Ay��AyhsAw�PAu;dAt1As"�Aq��Ap1Ao�-AoG�AnZAm��Am��AmK�Al^5Aj�Ad~�Ab1AaA`��A_�A^��A]�A\�AZVAX��AW��ATĜAQ��AO��AO�FAN��AM��AM"�AL~�AK�hAJjAHM�AF  AD�+AC�^AB=qAAl�A@��A?��A>��A=p�A<Q�A;��A:�jA:bA8��A6�A5��A4��A4  A2z�A1�#A1t�A0�A.�RA-dZA,��A,A�A,JA+XA*JA)\)A(�/A(A�A'�
A'`BA&��A&�A%�7A$�HA$v�A$5?A#33A"r�A"JA!�hA!oA {A�^Ax�A�/A �A��A
=A�AVA�yA �A��A�HA �A��A�A|�A�Ar�Ax�AZA�PAjA�FA33A��AE�A��AS�AVA�^A
��A	A�A-A�A
=A9XAS�A�DA�-Ar�A ��A $�@�{@�%@�S�@�v�@�@�p�@���@�Z@� �@�@���@���@�  @��H@�{@�O�@�Z@@�o@���@���@��@�1@�S�@�ff@��@�Ĝ@� �@��@���@�A�@�l�@އ+@�X@���@��@�=q@�&�@�1@�;d@�V@�x�@�1'@�=q@�O�@�Z@�$�@�Z@��y@��`@Ǯ@���@�hs@öF@�X@�33@�^5@���@��@�C�@�~�@���@���@��@��#@�V@���@�b@�o@��h@��j@� �@��@���@��@��@��P@��y@��@�{@�`B@��@��/@�A�@��@�C�@�~�@�%@�z�@�ƨ@��!@�@�x�@�%@���@�I�@�dZ@��!@�5?@�J@���@��7@�X@�/@���@�9X@�(�@�1@��@�33@��@��@��!@��#@��@��@�(�@�b@�ƨ@�K�@�n�@��@���@���@��@�A�@�(�@� �@�b@���@�1'@��@��;@���@�t�@�
=@��R@�ff@�{@�x�@��@�r�@�b@���@��;@�ƨ@���@���@�M�@�{@�@�J@��@��^@�x�@��`@�Ĝ@�r�@�A�@��m@��w@�33@�~�@�V@�-@��@�J@�@���@�%@��/@��D@�I�@� �@���@�dZ@�;d@��R@�J@���@���@�A�@�1@���@��
@��F@��@��;@���@�;d@���@���@��+@�ff@�M�@�@�x�@�X@�G�@�&�@��`@���@�z�@�j@�bN@�A�@��@��;@��@���@��@�
=@���@�ȴ@�ff@��#@��7@��@�p�@�X@�O�@�G�@��@�%@��`@��9@��@�bN@�9X@�  @�@�w@�P@K�@+@~�y@~�@~��@|�@up�@l(�@d��@Zn�@Q7L@J��@F�y@?��@7�@2�@*��@'
=@!&�@��@$�@��@
=@(�@	7L@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�#B�#B�#B�B�B�B�B�B�B�B�B�B�B�B�B�#B�)B�HB�ZB�yB�B��B��BB
=BhB�B5?BC�BE�BF�BK�BL�BM�BM�BM�BO�BP�BQ�BP�BO�BN�BI�BD�BC�BB�BB�BA�B@�B>wB:^B33B0!B/B+B�B�B{BDB�B�;B��BB�B��B��B�JBy�BjBE�B+B�B�B'�B.B5?B=qB=qB;dB:^B6FB.B1'BK�BS�BQ�BH�B@�B@�B;dB9XB�BJB�B�BBB�XB�B��BA�B
�B
�^B
��B
�{B
�B
m�B
ffB
aHB
W
B
K�B
?}B
5?B
,B
"�B
�B
�B
uB
\B
B	�B	�yB	�TB	�B	��B	��B	��B	��B	��B	��B	��B	ȴB	�RB	�\B	|�B	z�B	t�B	m�B	gmB	e`B	`BB	T�B	M�B	H�B	:^B	,B	$�B	"�B	�B	 �B	�B	�B	�B	
=B��B�B�TB�5B�B�)B�;B�#B��B��B��B��BɺBƨBB�}B�jB�XB�FB�-B�-B�B�B�B�B��B��B��B��B��B��B��B��B�{B�uB�hB�VB�DB�7B�+B�B� B|�By�Bw�Bt�Br�Bq�Bo�Bm�BjBhsBffBe`BbNB^5B\)BZBW
BS�BP�BN�BM�BK�BI�BE�BB�B?}B@�B?}B=qB<jB;dB:^B8RB6FB5?B2-B1'B/B.B-B,B)�B'�B&�B$�B#�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{B{B{B{B{B{B{B{B{B{B{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B$�B&�B'�B-B1'B2-B49B5?B6FB7LB9XB:^B;dB<jB<jB<jB<jB<jB<jB?}B@�BC�BD�BH�BJ�BK�BL�BO�BVBVBW
B]/B_;B`BBbNBbNBcTBdZBe`Bl�Bo�Bq�Bs�Bt�Bt�Bt�Bz�B{�B|�B~�B�B�B�B�B�1B�PB�bB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�3B�?B�LB�XB�qB��BŢBȴBȴBɺBɺB��B��B��B��B�
B�B�#B�)B�)B�5B�;B�NB�ZB�mB�B�B��B��B��B��B��B��B��B	B	B	B	+B	1B	DB	PB	PB	VB	\B	bB	uB	�B	�B	�B	�B	$�B	(�B	-B	/B	2-B	6FB	:^B	<jB	<jB	=qB	?}B	C�B	D�B	E�B	F�B	J�B	M�B	O�B	O�B	O�B	P�B	R�B	T�B	W
B	W
B	XB	\)B	]/B	^5B	_;B	_;B	bNB	e`B	gmB	gmB	hsB	iyB	jB	jB	k�B	o�B	p�B	q�B	u�B	w�B	x�B	x�B	y�B	z�B	z�B	|�B	~�B	� B	�DB	��B	��B	�fB	��B
B
\B
�B
�B
-B
7LB
?}B
F�B
L�B
VB
^5B
bNB
ffB
jB
o�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B�B�B�B��B��B��B��B��B��B��B��B��B��B��B� B�B�B�$B�:B�YB�B��B��B�B
BGB�B5"BCxBE�BF�BK�BL�BM�BM�BM�BO�BP�BQ�BP�BO�BN�BI�BDBCvBBpBBnBAfB@eB>YB:>B3B0B.�B*�B�BwBYB"B�B�B��B�lB��B��B�^B�%By�Bj`BE|B*�B{B�B'�B-�B5B=MB=LB;AB:7B6#B-�B1BK�BS�BQ�BH�B@^B@[B;@B92B�B$B�B�B�kB�0B��B�mBAbB
�jB
�8B
��B
�WB
��B
moB
fCB
a"B
V�B
K�B
?\B
5B
+�B
"�B
~B
oB
WB
<B
 �B	�B	�[B	�3B	��B	ιB	˨B	λB	��B	θB	̯B	̬B	ȔB	�3B	�>B	|�B	z�B	t�B	muB	gPB	eDB	`&B	T�B	M�B	H�B	:BB	+�B	$�B	"�B	�B	 �B	�B	�B	nB	
#B��B�rB�=B�B��B�B�#B�
B��BͼBˮBʪBɡBƏB�xB�fB�SB�>B�0B�B�B�B�B��B��B��B��B��B��B��B��B�vB�jB�bB�\B�PB�BB�.B� B�B�B�B|�By�Bw�Bt�Br�Bq�Bo�BmyBjjBh\BfOBeKBb9B^B\BZBV�BS�BP�BN�BM�BK�BI�BE�BBzB?hB@lB?hB=]B<UB;MB:HB8=B61B5*B2B1B.�B-�B,�B+�B)�B'�B&�B$�B#�B!�B�B�B�B�B�B}BvBuB|BxBqBoB�ByBXBUBPBKBIBKBKBeBeBJBJBJBdBIBJBIBqBVBWBrBRBlBQBWBwBvBwBwBqBtB|BvBkBvB�BgB�B�BjB�B�BuB�B�B�B �B$�B&�B'�B,�B1B2B4B5%B6.B71B9>B:EB;IB<NB<NB<NB<PB<OB<OB?dB@hBC}BD�BH�BJ�BK�BL�BO�BU�BU�BV�B]B_B`%Bb/Bb1Bc9Bd>BeCBlnBo�Bq�Bs�Bt�Bt�Bt�Bz�B{�B|�B~�B��B��B��B��B�B�2B�AB�NB�[B�^B�gB�eB�uB�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�+B�6B�PB�`BŀBȓBȑBɗBɕBʢBϼB��B��B��B��B�B�B�B�B�B�+B�7B�JB�ZB�}B��B��B��B��B��B��B��B	 �B	�B	�B	B	B	B	,B	+B	1B	7B	<B	NB	[B	mB	�B	�B	$�B	(�B	,�B	.�B	2B	6B	:6B	<BB	<CB	=IB	?TB	CnB	DwB	E}B	FB	J�B	M�B	O�B	O�B	O�B	P�B	R�B	T�B	V�B	V�B	W�B	\B	]B	^B	_B	_B	b%B	e4B	gDB	gBB	hKB	iPB	jYB	jWB	kaB	ovB	pzB	q�B	u�B	w�B	x�B	x�B	y�B	z�B	z�B	|�B	~�B	�B	�B	��B	ʔB	�9B	��B
�B
-B
QB
�B
,�B
7B
?MB
FxB
L�B
U�B
^B
bB
f9B
jQB
onB
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.39 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708302016053117083020160531170830  AO  ARCAADJP                                                                    20160510091556    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160510091556  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160510091556  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170830  IP                  G�O�G�O�G�O�                