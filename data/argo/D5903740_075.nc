CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:44Z AOML 3.0 creation; 2016-06-01T00:08:17Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230844  20160531170818  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               KA   AO  4055_7112_075                   2C  D   APEX                            5374                            041511                          846 @���@� 1   @����o��@:bM����d�����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    KA   A   A   @�ff@�  A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DRy�DS  DS� DT  DT� DU  DU� DV  DV� DW  DWy�DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_fD_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy�3D�fD�C3D��3D��fD�3D�P D�vfD��fD�	�D�L�D�|�D�ٚD�fD�0 D�#3D�ɚD��fD�@ D�l�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�
=@ȣ�A�A$Q�ADQ�AdQ�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B	{B{B{B!{B){B1{B9{BA{BIz�BQ{BY{Ba{Bi{Bq{By{B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=BĊ=BȊ=B̊=BЊ=BԊ=B؊=B܊=B��=B�=B�=B�=B��=B�=B��=B��=C ECECECECEC
ECECECECECECECECECECEC EC"EC$EC&EC(EC*EC,EC.EC0EC2EC4EC6EC8EC:EC<EC>EC@ECBECDECFECHECJECLECNECPECRECTECVECXECZEC\EC^EC`ECbECdECfEChECjEClECnECpECrECtECvECxECzEC|EC~EC�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR��DSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW��DXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_�D_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDy�{D�
D�K�D���D��
D��D�X�D�
D��
D�>D�UqD��qD��>D�
D�8�D�+�D��>D��
D�H�D�uqD��>11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��A��A���A��A��uA��A�|�A�hsA�VA�M�A�A�A�(�A�
=A�ƨA�v�A�  A��\A�ffA�`BA��yA�&�A�S�A��\A�5?A��jA���A��yA��uA�`BA�E�A�9XA�-A��A�%A��A��/A�ȴA��9A��uA�hsA�K�A�C�A�A�A�-A�&�A�"�A��A�bA�bA�VA�
=A�A�VA�%A��A��A��wA��!A���A��7A�l�A���A���A���A��7A�oA��
A�`BA��A��-A��A�7LA�^5A���A��+A�z�A��;A��DA��7A��TA��A�r�A�$�A�bA�A���A���A}t�Ay?}Aut�Ar�HAp�HAm�;Ak�-AioAg�FAf�AfbAd�\Ad  Ac�Acx�Ab�AadZA`M�A^  A]�PA]A\$�A[�
A[t�AZ��AZE�AYl�AW�
AV��AT�\AR��AR�AQ��AP�+AOVAM�AL�AL��AL^5AL �AK�FAK"�AJ��AJ��AJ��AJ��AJz�AJVAJ5?AI��AI+AG�AC�ABA@v�A>�A>n�A=A9�A6�A5�FA533A5
=A4��A4�HA4ĜA4��A4r�A49XA3�-A/O�A-�A-�A+��A*ȴA(ffA%�FA#��A!��A �\A�AbAr�A��AXA�`A��AbNAVA�A-A(�A��A��A��A��A�hA�A�7Ax�A�A%A%A��Ax�A�`A=qA  A��AdZA�A=qA�A�TA�#AA�7A?}AȴA��A?}A
=A��A^5AC�A	�mA�Av�A��AC�A��A�\A�Al�A\)AS�A;dAoA1'A�PA��A��A �jA Q�@��;@�o@�5?@�O�@���@��;@��@���@���@���@�?}@�D@�S�@�33@�n�@�&�@��
@�%@���@�v�@���@ߍP@�=q@�p�@�  @�33@��@�1@�ff@ղ-@�`B@�7L@�r�@�@��@ϥ�@�7L@̬@��;@�o@�^5@�Ĝ@�9X@�9X@�ƨ@�$�@�O�@��@��y@�J@���@��7@��7@�`B@���@��@�33@�M�@��^@��-@�O�@��j@���@�Q�@�b@��P@�=q@���@��
@�v�@�G�@��9@�t�@�O�@�%@��@���@�t�@���@�{@���@��@�9X@�"�@�hs@���@�dZ@�@��h@�`B@�V@���@��u@�Q�@��@��\@��@�&�@�Q�@�;d@���@���@��9@���@�bN@��;@�dZ@�33@��@�@��H@��+@�E�@�=q@�5?@�-@�@�7L@��9@�r�@�Q�@���@��;@���@�S�@��!@��T@�X@�z�@� �@�t�@�33@��@�
=@�@�@���@���@��@��!@�-@���@��7@��@���@�bN@�Z@�Z@�A�@�1'@� �@�b@���@��w@�t�@�dZ@�\)@�C�@��@�@��y@���@���@���@���@���@�ȴ@���@�ȴ@���@�ff@�J@��h@���@�z�@�Q�@� �@�t�@�K�@�33@�"�@�o@�
=@��H@��R@��\@�v�@�^5@�=q@�@��#@���@�`B@�O�@�G�@�/@��@��@�V@���@��/@���@�1'@��m@��F@�"�@���@�ff@�J@���@�p�@�V@�Ĝ@��@���@���@��u@��@�j@�9X@�1@�P@;d@|�@{t�@z��@z~�@y�@y�7@yhs@y%@x1'@w��@v�y@vV@u��@t��@t9X@s�m@st�@so@r�H@rn�@rJ@q�7@p1'@o�@o�@o�;@o��@o�@o�P@oK�@n�@n�+@jM�@`b@\I�@W�w@MO�@E�-@>{@;S�@6�+@3��@,��@)x�@$I�@V@�@j@�u@�-@ƨ@��@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A��A��A���A��A��uA��A�|�A�hsA�VA�M�A�A�A�(�A�
=A�ƨA�v�A�  A��\A�ffA�`BA��yA�&�A�S�A��\A�5?A��jA���A��yA��uA�`BA�E�A�9XA�-A��A�%A��A��/A�ȴA��9A��uA�hsA�K�A�C�A�A�A�-A�&�A�"�A��A�bA�bA�VA�
=A�A�VA�%A��A��A��wA��!A���A��7A�l�A���A���A���A��7A�oA��
A�`BA��A��-A��A�7LA�^5A���A��+A�z�A��;A��DA��7A��TA��A�r�A�$�A�bA�A���A���A}t�Ay?}Aut�Ar�HAp�HAm�;Ak�-AioAg�FAf�AfbAd�\Ad  Ac�Acx�Ab�AadZA`M�A^  A]�PA]A\$�A[�
A[t�AZ��AZE�AYl�AW�
AV��AT�\AR��AR�AQ��AP�+AOVAM�AL�AL��AL^5AL �AK�FAK"�AJ��AJ��AJ��AJ��AJz�AJVAJ5?AI��AI+AG�AC�ABA@v�A>�A>n�A=A9�A6�A5�FA533A5
=A4��A4�HA4ĜA4��A4r�A49XA3�-A/O�A-�A-�A+��A*ȴA(ffA%�FA#��A!��A �\A�AbAr�A��AXA�`A��AbNAVA�A-A(�A��A��A��A��A�hA�A�7Ax�A�A%A%A��Ax�A�`A=qA  A��AdZA�A=qA�A�TA�#AA�7A?}AȴA��A?}A
=A��A^5AC�A	�mA�Av�A��AC�A��A�\A�Al�A\)AS�A;dAoA1'A�PA��A��A �jA Q�@��;@�o@�5?@�O�@���@��;@��@���@���@���@�?}@�D@�S�@�33@�n�@�&�@��
@�%@���@�v�@���@ߍP@�=q@�p�@�  @�33@��@�1@�ff@ղ-@�`B@�7L@�r�@�@��@ϥ�@�7L@̬@��;@�o@�^5@�Ĝ@�9X@�9X@�ƨ@�$�@�O�@��@��y@�J@���@��7@��7@�`B@���@��@�33@�M�@��^@��-@�O�@��j@���@�Q�@�b@��P@�=q@���@��
@�v�@�G�@��9@�t�@�O�@�%@��@���@�t�@���@�{@���@��@�9X@�"�@�hs@���@�dZ@�@��h@�`B@�V@���@��u@�Q�@��@��\@��@�&�@�Q�@�;d@���@���@��9@���@�bN@��;@�dZ@�33@��@�@��H@��+@�E�@�=q@�5?@�-@�@�7L@��9@�r�@�Q�@���@��;@���@�S�@��!@��T@�X@�z�@� �@�t�@�33@��@�
=@�@�@���@���@��@��!@�-@���@��7@��@���@�bN@�Z@�Z@�A�@�1'@� �@�b@���@��w@�t�@�dZ@�\)@�C�@��@�@��y@���@���@���@���@���@�ȴ@���@�ȴ@���@�ff@�J@��h@���@�z�@�Q�@� �@�t�@�K�@�33@�"�@�o@�
=@��H@��R@��\@�v�@�^5@�=q@�@��#@���@�`B@�O�@�G�@�/@��@��@�V@���@��/@���@�1'@��m@��F@�"�@���@�ff@�J@���@�p�@�V@�Ĝ@��@���@���@��u@��@�j@�9X@�1@�P@;d@|�@{t�@z��@z~�@y�@y�7@yhs@y%@x1'@w��@v�y@vV@u��@t��@t9X@s�m@st�@so@r�H@rn�@rJ@q�7@p1'@o�@o�@o�;@o��@o�@o�P@oK�@n�@n�+@jM�@`b@\I�@W�w@MO�@E�-@>{@;S�@6�+@3��@,��@)x�@$I�@V@�@j@�u@�-@ƨ@��@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��BBB��B��B��B��B��B��B��B�}B�}B�wB�jB�RB�9B�B��B��B�{B�\B�Bz�Bq�B[#B1'B�BhB\BVBPBPBJBDB
=B1B+BBBB��B��B��B��B��B��B��B��B��B��B��B��B��BBBB��B��B��B��B��B��B�B�;B�}B��Bu�BW
BJ�B>wB33BB�5BB��BS�B(�B�B	7B
ĜB
�+B
x�B
q�B
l�B
jB
iyB
ffB
ZB
0!B
uB	��B	�fB	�
B	ĜB	�FB	��B	��B	��B	��B	�oB	�hB	�bB	�PB	�7B	�B	{�B	t�B	s�B	p�B	n�B	m�B	jB	gmB	dZB	_;B	YB	R�B	H�B	C�B	@�B	=qB	7LB	2-B	,B	+B	)�B	(�B	&�B	$�B	#�B	"�B	"�B	"�B	!�B	 �B	�B	�B	�B	�B	+B��B��B��B�B�B�B�B�`B�BB�/B�)B�)B�#B�B�B�B��B��B�}B�jB�LB�-B�B��B��B�PB�+B�B|�Bw�Bt�Br�Bq�Bo�Bn�Bn�Bl�BhsBbNBaHB`BB`BB`BB`BB`BB`BB`BB_;B^5B^5B]/BZBXBW
BT�BS�BS�BR�BP�BO�BO�BO�BN�BN�BL�BK�BI�BG�BG�BF�BD�BB�B?}B<jB;dB:^B8RB8RB6FB5?B49B49B49B49B33B1'B/B.B,B)�B)�B)�B(�B'�B'�B&�B%�B$�B#�B"�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B$�B%�B%�B(�B)�B,B.B0!B1'B1'B0!B0!B1'B2-B2-B33B5?B5?B5?B6FB5?B6FB5?B5?B7LB9XB;dB>wBB�BD�BF�BM�BN�BN�BP�BR�BS�BW
BXBZB\)B^5Be`Bk�Bl�Bs�Bu�Bu�Bw�Bx�By�By�B}�B�B�%B�=B�VB�bB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�-B�FB�dB�jB��B��BBÖBǮB��B��B��B��B�)B�5B�5B�;B�;B�;B�;B�;B�;B�BB�`B�sB�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B	  B	B	B	B	B	B	B	B	B	B	B	B	%B		7B	PB	oB	�B	�B	�B	�B	!�B	!�B	"�B	"�B	#�B	$�B	%�B	'�B	'�B	(�B	)�B	,B	,B	/B	1'B	1'B	1'B	2-B	33B	33B	33B	49B	49B	6FB	:^B	<jB	>wB	C�B	H�B	K�B	N�B	P�B	S�B	YB	[#B	\)B	]/B	]/B	]/B	^5B	_;B	`BB	aHB	cTB	dZB	m�B	t�B	w�B	y�B	{�B	}�B	~�B	� B	�B	�%B	�7B	�DB	�JB	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�qB	�B	�NB	�B
B
uB
$�B
'�B
1'B
5?B
<jB
A�B
H�B
N�B
XB
_;B
dZB
hsB
k�B
o�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�xB�vB�}B�B�uB�zB�qB�pB�pB�rB�oB�jB�iB�eB�UB�>B�%B�B��B��B�dB�HB�
Bz�Bq�B[B1B�BNBGB>B:B:B2B-B
&BBB	BB�B��B��B��B��B��B��B��B��B��B��B��B��B��B �B�B �B��B��B��B��B��B��B�gB�B�dB��Bu�BV�BJ�B>]B3B�B�B�wB�iBS�B(�ByB	!B
ăB
�B
x�B
q�B
ltB
jgB
ieB
fSB
ZB
0B
aB	��B	�QB	��B	ĉB	�2B	��B	��B	��B	��B	�^B	�XB	�QB	�?B	�'B	��B	{�B	t�B	s�B	p�B	n�B	m�B	jnB	g]B	dIB	_-B	YB	R�B	H�B	C�B	@vB	=bB	7>B	2 B	+�B	*�B	)�B	(�B	&�B	$�B	#�B	"�B	"�B	"�B	!�B	 �B	�B	�B	�B	rB	B��B��B��B�B�B�xB�yB�UB�6B� B�B�B�B�B�B�B��B��B�sB�_B�AB�#B��B��B�wB�EB�#B��B|�Bw�Bt�Br�Bq�Bo�Bn�Bn�Bl�BhlBbFBa@B`;B`9B`<B`;B`:B`8B`:B_2B^-B^*B]%BZBX	BW BT�BS�BS�BR�BP�BO�BO�BO�BN�BN�BL�BK�BI�BG�BG�BF�BD�BB�B?uB<`B;^B:UB8JB8KB6=B56B44B43B42B4B3*B1"B/B.B, B)�B)�B)�B(�B'�B'�B&�B%�B$�B#�B"�B!�B�B�B�B~B�B�BqB�B�B�BeBfBdBeBeBfBeB^BxB�B�B�B�BzB�ByB�B�B�B�B�B�B"�B$�B%�B%�B(�B)�B+�B.B0B1B1B0B0B1B2#B2#B3)B54B56B56B6<B54B6;B56B52B7CB9MB;VB>mBB�BD�BF�BM�BN�BN�BP�BR�BS�BV�BXBZB\B^(BeSBkxBl|Bs�Bu�Bu�Bw�Bx�By�By�B}�B�B�B�/B�EB�RB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�3B�RB�WB�oB�nB�}BÅBǚB˱B��B��B��B�B�"B�!B�(B�*B�(B�)B�)B�'B�/B�JB�_B�mB�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	 �B	�B	�B	�B	�B	�B	�B	�B	B	�B	�B	B		!B	<B	YB	pB	zB	�B	�B	!�B	!�B	"�B	"�B	#�B	$�B	%�B	'�B	'�B	(�B	)�B	+�B	+�B	/B	1B	1B	1B	2B	3B	3B	3B	4"B	4#B	6.B	:FB	<RB	>_B	CB	H�B	K�B	N�B	P�B	S�B	X�B	[B	\B	]B	]B	]B	^B	_"B	`*B	a/B	c:B	dCB	mvB	t�B	w�B	y�B	{�B	}�B	~�B	�B	��B	�
B	�B	�)B	�/B	�IB	�UB	�YB	�eB	�sB	�qB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�UB	��B	�2B	�B
�B
XB
$�B
'�B
1	B
5!B
<KB
AiB
H�B
N�B
W�B
_B
d9B
hUB
keB
oB
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.27 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708182016053117081820160531170818  AO  ARCAADJP                                                                    20140721230844    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230844  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230844  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170818  IP                  G�O�G�O�G�O�                