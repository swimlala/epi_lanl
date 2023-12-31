CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-08-28T02:15:38Z AOML 3.0 creation; 2016-06-01T00:08:26Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150828021538  20160531170826  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               }A   AO  4055_7112_125                   2C  D   APEX                            5374                            041511                          846 @�j��z�o1   @�j�3��q@:r� ě��d:^5?|�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    }A   A   A   @�33@�  A   A   A@  A`  A�  A���A���A�  A�  A�  A�  A�  B   B  B  B��B ffB'33B0  B8  B@  BH  BP  BXffB`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy� D�fD�FfD��3D���D��D�C3D�l�D�ٚD�fD�S3D�|�DǠ D�fD�33D�vfD�y�D�3D�6fD� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��
@ȣ�AQ�A$Q�ADQ�AdQ�A�(�A���A���A�(�A�(�A�(�A�(�A�(�B{B	{B{B�HB!z�B(G�B1{B9{BA{BI{BQ{BYz�Baz�Bi{Bq{By{B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=BĊ=BȊ=B̊=BЊ=BԊ=B؊=B܊=B��=B�=B�=B�=B��=B�=B��=B��=C ECECECECEC
ECECECECECECECECECECEC EC"EC$EC&EC(EC*EC,EC.EC0EC2EC4EC6EC8EC:EC<EC>EC@ECBECDECFECHECJECLECNECPECRECTECVECXECZEC\EC^EC`ECbECdECfEChECjEClECnECpECrECtECvECxECzEC|EC~EC�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt��Dy�HD�
D�O
D���D��qD�%qD�K�D�uqD��>D�
D�[�D��qDǨ�D�
D�;�D�
D��>D��D�?
D�D��q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aߙ�A�AڍPA��Aُ\A��A�l�A�bA�AבhA�ZA�5?A�/A�+A�%Aֲ-A�VA�7LAω7A��/AľwAüjA��A�|�A��
A�jA�O�A�n�A���A�hsA��FA��A���A�ĜA��+A�A�ffA���A��A��A��A��hA�A��9A���A���A���A�^5A��/A��9A��jA�dZA���A��DA��A���A�A��A�hsA�ZA��`A�bNA��\A��A���A�\)A�ƨA�`BA��RA�{A�ĜA�Q�A�  A�l�A��9A���A��A��-A�1A�G�A�Q�A�JA�{A�1'A��-A�33A�ĜA��hA��RA�t�A�XA�bA���A��HA���A�jA���A��A}�A|��A{x�Azz�Ay�Ax��Aw��AvjAt��AtjAsC�ArM�Ap��Ao��An�An�+AnbNAnVAk��Ai��Ah$�Af�+Ae�^AeoAd��Ac�hAc�Abz�AaoA_G�A_VA_+A_33A_�A^��A^ZA\{AZ�\AY�AX�DAW��AW��AVM�AT�!AT$�AS��ASp�AR�RAPr�AM��AL�HAL��AL��ALĜALĜALĜAL��AL��ALAK"�AJ��AJ�AI�#AG"�AFA�AE��AD�DAC
=ABAAAAC�A@�DA?`BA>jA=�7A=�A;O�A9�;A9%A8�!A7�-A6�A41'A3;dA2z�A1�mA1x�A0VA/�A.r�A. �A-|�A,bNA*�uA(z�A';dA%�-A$�A#��A#t�A"�\A"1A!��A!��A!C�A E�A  �A�TA+A�uAjAA�AK�A�A�
A�`A�AA�A�/A;dA~�A5?Al�A�uA��A�yAI�AbAK�AVAv�A(�A��A�-A�A�hA1AXA
��A
��A	�mA	K�A	�AĜA�;A�A^5A�
Ap�A�AA��A@��P@���@�E�@��m@��j@�r�@�ff@�?}@��/@�1'@�t�@�G�@��@�E�@웦@�F@�
=@��@���@�33@��y@�E�@�@�`B@��
@��@�;d@��@���@ܬ@��@��@���@׶F@�n�@ՙ�@ԓu@�hs@�Z@�5?@�I�@��@�A�@Ɨ�@�E�@ř�@�bN@�C�@���@�-@���@��/@���@�j@�(�@��@���@��P@�"�@���@��@���@���@�J@���@��m@���@�-@��^@�p�@��u@��
@��@��y@� �@�r�@�S�@�G�@�b@��H@�~�@���@�\)@���@�^5@��^@�X@��@��/@�9X@���@��`@��@�j@�1@��@�{@�G�@�V@��@�9X@��m@�;d@��@���@�5?@�J@��h@�/@�%@��@�b@�t�@�=q@��^@���@���@��h@��@�X@�%@���@�bN@��
@��@�C�@�+@��@�@���@���@�~�@�^5@��w@�Q�@��@���@�S�@�"�@�
=@��+@�=q@�ff@�E�@�-@�$�@�-@��@��#@�G�@�%@�Ĝ@��D@�Z@�1'@��w@��P@�l�@�S�@���@���@�v�@��@��u@��9@��D@�Z@�t�@���@�V@�@��T@�@��@���@�bN@��y@��@�?}@�Q�@��w@��@��w@��@�b@�I�@���@���@��@��F@�E�@��@���@�@��7@�X@�O�@��@��@�bN@�(�@�b@�@�w@|�@�P@�w@��@~�+@}��@}�@}`B@}�@|�D@|I�@{�
@{o@z��@zM�@y�@y��@y��@y�7@x�9@w��@v�y@u�@u@u�h@u�h@u�@u�@up�@u/@uV@tZ@t1@r~�@k�
@d��@^@X  @QG�@K�F@EO�@>v�@5�@01'@*�@&$�@!G�@�@V@�^@{@33@r�@p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aߙ�A�AڍPA��Aُ\A��A�l�A�bA�AבhA�ZA�5?A�/A�+A�%Aֲ-A�VA�7LAω7A��/AľwAüjA��A�|�A��
A�jA�O�A�n�A���A�hsA��FA��A���A�ĜA��+A�A�ffA���A��A��A��A��hA�A��9A���A���A���A�^5A��/A��9A��jA�dZA���A��DA��A���A�A��A�hsA�ZA��`A�bNA��\A��A���A�\)A�ƨA�`BA��RA�{A�ĜA�Q�A�  A�l�A��9A���A��A��-A�1A�G�A�Q�A�JA�{A�1'A��-A�33A�ĜA��hA��RA�t�A�XA�bA���A��HA���A�jA���A��A}�A|��A{x�Azz�Ay�Ax��Aw��AvjAt��AtjAsC�ArM�Ap��Ao��An�An�+AnbNAnVAk��Ai��Ah$�Af�+Ae�^AeoAd��Ac�hAc�Abz�AaoA_G�A_VA_+A_33A_�A^��A^ZA\{AZ�\AY�AX�DAW��AW��AVM�AT�!AT$�AS��ASp�AR�RAPr�AM��AL�HAL��AL��ALĜALĜALĜAL��AL��ALAK"�AJ��AJ�AI�#AG"�AFA�AE��AD�DAC
=ABAAAAC�A@�DA?`BA>jA=�7A=�A;O�A9�;A9%A8�!A7�-A6�A41'A3;dA2z�A1�mA1x�A0VA/�A.r�A. �A-|�A,bNA*�uA(z�A';dA%�-A$�A#��A#t�A"�\A"1A!��A!��A!C�A E�A  �A�TA+A�uAjAA�AK�A�A�
A�`A�AA�A�/A;dA~�A5?Al�A�uA��A�yAI�AbAK�AVAv�A(�A��A�-A�A�hA1AXA
��A
��A	�mA	K�A	�AĜA�;A�A^5A�
Ap�A�AA��A@��P@���@�E�@��m@��j@�r�@�ff@�?}@��/@�1'@�t�@�G�@��@�E�@웦@�F@�
=@��@���@�33@��y@�E�@�@�`B@��
@��@�;d@��@���@ܬ@��@��@���@׶F@�n�@ՙ�@ԓu@�hs@�Z@�5?@�I�@��@�A�@Ɨ�@�E�@ř�@�bN@�C�@���@�-@���@��/@���@�j@�(�@��@���@��P@�"�@���@��@���@���@�J@���@��m@���@�-@��^@�p�@��u@��
@��@��y@� �@�r�@�S�@�G�@�b@��H@�~�@���@�\)@���@�^5@��^@�X@��@��/@�9X@���@��`@��@�j@�1@��@�{@�G�@�V@��@�9X@��m@�;d@��@���@�5?@�J@��h@�/@�%@��@�b@�t�@�=q@��^@���@���@��h@��@�X@�%@���@�bN@��
@��@�C�@�+@��@�@���@���@�~�@�^5@��w@�Q�@��@���@�S�@�"�@�
=@��+@�=q@�ff@�E�@�-@�$�@�-@��@��#@�G�@�%@�Ĝ@��D@�Z@�1'@��w@��P@�l�@�S�@���@���@�v�@��@��u@��9@��D@�Z@�t�@���@�V@�@��T@�@��@���@�bN@��y@��@�?}@�Q�@��w@��@��w@��@�b@�I�@���@���@��@��F@�E�@��@���@�@��7@�X@�O�@��@��@�bN@�(�@�b@�@�w@|�@�P@�w@��@~�+@}��@}�@}`B@}�@|�D@|I�@{�
@{o@z��@zM�@y�@y��@y��@y�7@x�9@w��@v�y@u�@u@u�h@u�h@u�@u�@up�@u/@uV@tZ@t1@r~�@k�
@d��@^@X  @QG�@K�F@EO�@>v�@5�@01'@*�@&$�@!G�@�@V@�^@{@33@r�@p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�%B� Bz�Bw�Bu�Bt�Bu�Bv�Bx�B{�B|�B|�B{�B|�B~�B}�B~�B�B�DB�B�1B�B~�B� B|�Bt�Be`BP�B�B�B�;BǮB�qB�dBB��B�PB��B��B��B��B�9B�XB�dB�^B�FB�3B�!B�B��B�\B�JB�Bz�Bx�Bw�By�Bw�Bp�Bl�B_;BC�B9XB0!B)�B!�B�BbBB��B�`B�#BŢB�B��B�PBx�BhsB\)BM�BE�B5?B<jBC�B;dB2-B,B%�B�BuBuB\B+B
��B
��B
�B
�mB
�
B
B
�^B
�B
��B
��B
��B
�VB
�B
x�B
s�B
iyB
`BB
VB
J�B
B�B
>wB
<jB
8RB
 �B
DB	��B	�B	�sB	�NB	�5B	�B	�B	��B	ɺB	ĜB	ŢB	ǮB	ȴB	ɺB	ǮB	��B	�B	��B	��B	��B	�uB	�hB	�=B	�B	�B	~�B	{�B	u�B	iyB	\)B	XB	XB	YB	YB	YB	YB	XB	VB	R�B	N�B	J�B	G�B	C�B	9XB	5?B	1'B	+B	"�B	�B	�B	�B	�B	hB	JB	+B	B	B	B	B��B��B�B�ZB�;B�/B�)B�B�B��B��B��B��BĜB�qB�?B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�bB�PB�JB�VB�=B�7B�B� By�By�Bw�Bu�Bs�Bp�Bn�Bm�Bk�BjBiyBhsBgmBe`BcTBaHB^5B[#BYBXBW
BT�BS�BQ�BO�BM�BK�BI�BG�BF�BC�B@�B>wB;dB9XB7LB5?B2-B1'B/B.B.B-B,B+B)�B(�B&�B&�B%�B#�B#�B&�B)�B0!B1'B33B33B49B5?B9XB9XB9XB;dB<jB>wB>wBA�BB�BA�B?}B:^B7LB49B2-B0!B5?B7LB6FB5?B49B5?B>wBA�BB�BC�BC�BC�BC�BE�BH�BM�BK�BK�BJ�BJ�BF�BR�BbNBjBp�Bp�Br�Bs�Br�Bp�Bq�By�B�B�1B�7B�=B�oB�oB�oB�bB�bB�bB�\B�\B�VB�PB�PB�DB�7B�1B�+B�%B�B�B�B�B�B�%B�+B�7B�PB�PB�VB�VB�\B�bB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�3B�3B�3B�9B�LB�jB�wBB��B�B�B�/B�5B�5B�5B�BB�ZB�fB�sB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B	  B	B	B	1B	DB	PB	�B	�B	�B	�B	 �B	 �B	 �B	!�B	"�B	&�B	'�B	'�B	(�B	,B	-B	/B	2-B	5?B	9XB	@�B	E�B	I�B	K�B	M�B	M�B	N�B	M�B	N�B	O�B	O�B	O�B	R�B	S�B	VB	VB	W
B	W
B	YB	ZB	\)B	\)B	cTB	ffB	ffB	ffB	gmB	jB	k�B	n�B	t�B	w�B	y�B	|�B	~�B	~�B	� B	�B	�B	�+B	�JB	�JB	�PB	�PB	�PB	�PB	�PB	�PB	�VB	�hB	�oB	��B	�dB	��B	�B	��B
PB
�B
�B
(�B
5?B
;dB
C�B
I�B
O�B
XB
`BB
dZB
iyB
l�B
o�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�Bz�Bw�Bu�Bt�Bu�Bv�Bx�B{�B|�B|�B{�B|�B~�B}�B~�B��B�0B�B�B�B~�B�B|�Bt�BeMBP�B�B�rB�#BǖB�YB�LB�wB��B�9B�yB��B��B��B�#B�>B�NB�FB�0B�B�B��B��B�DB�0B�Bz�Bx�Bw�By�Bw�Bp�BlsB_ BC}B9?B0
B)�B!�ByBIBB��B�DB�	BŇB��B��B�6Bx�BhZB\BM�BE�B5%B<PBCzB;KB2B+�B%�BxB]B]BBBB
��B
��B
�B
�UB
��B
�uB
�FB
��B
��B
��B
�nB
�?B
�B
x�B
s�B
iaB
`+B
U�B
J�B
B{B
>aB
<TB
8=B
 �B
.B	��B	�B	�`B	�:B	�"B	�B	��B	��B	ɦB	ĊB	őB	ǛB	ȡB	ɧB	ǝB	�oB	� B	��B	��B	�yB	�dB	�VB	�,B	� B	��B	~�B	{�B	u�B	igB	\B	W�B	W�B	Y
B	YB	YB	Y
B	X B	U�B	R�B	N�B	J�B	G�B	C�B	9HB	52B	1B	*�B	"�B	�B	�B	�B	xB	ZB	=B	B	B	B	�B	 �B��B��B�B�NB�/B�"B�B�B��B��B��B��BʵBĒB�dB�3B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�pB�cB�WB�EB�?B�LB�2B�.B�B�By�By�Bw�Bu�Bs�Bp�Bn�Bm�Bk{BjxBirBhkBgfBeXBcJBaAB^-B[BYBXBWBT�BS�BQ�BO�BM�BK�BI�BG�BF�BC�B@{B>nB;]B9PB7DB56B2'B1B/B.B.B-B,B*�B)�B(�B&�B&�B%�B#�B#�B&�B)�B0B1 B3)B3B41B56B9PB9NB9NB;[B<_B>nB>oBA~BB�BAB?sB:TB7BB4-B2$B0B54B7BB6;B56B4.B55B>mBA~BB�BC�BC�BC�BC�BE�BH�BM�BK�BK�BJ�BJ�BF�BR�BbCBjsBp�Bp�Br�Bs�Br�Bp�Bq�By�B�B�#B�'B�/B�^B�aB�_B�TB�TB�RB�NB�MB�FB�AB�AB�6B�(B�#B�B�B�
B��B��B�	B�
B�B�B�'B�@B�AB�CB�FB�LB�QB�_B�iB�nB��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�!B�#B� B�&B�;B�WB�cB�~B��B�B�B�B�"B�#B�#B�.B�GB�OB�^B�kB�oB�pB�xB�}B�B��B��B��B��B��B��B��B��B��B��B��B��B	�B	B	B	.B	=B	kB	�B	�B	�B	 �B	 �B	 �B	!�B	"�B	&�B	'�B	'�B	(�B	+�B	,�B	/B	2B	5'B	9@B	@lB	E�B	I�B	K�B	M�B	M�B	N�B	M�B	N�B	O�B	O�B	O�B	R�B	S�B	U�B	U�B	V�B	V�B	YB	ZB	\B	\B	c=B	fLB	fKB	fMB	gSB	jfB	kjB	n�B	t�B	w�B	y�B	|�B	~�B	~�B	�B	��B	��B	�B	�1B	�,B	�8B	�7B	�6B	�6B	�9B	�6B	�:B	�MB	�UB	��B	�JB	��B	�aB	��B
2B
vB
�B
(�B
5 B
;DB
CwB
I�B
O�B
W�B
`#B
d:B
iYB
llB
o~B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.27 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708262016053117082620160531170826  AO  ARCAADJP                                                                    20150828021538    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150828021538  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150828021538  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170826  IP                  G�O�G�O�G�O�                