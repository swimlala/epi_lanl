CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:26Z AOML 3.0 creation; 2016-06-01T00:08:12Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230826  20160531170812  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               *A   AO  4055_7112_042                   2C  D   APEX                            5374                            041511                          846 @֖Z|e�
1   @֖[#�@:)x����ce���l�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    *A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CI�fCK�fCN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D��Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�DyS3D��D�VfD�� D��fD�  D�FfD�p D���D�  D�FfD�p D�ٚD� D�9�D�Y�D��fD�fD�L�D�3D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��
@ȣ�AQ�A$Q�ADQ�AdQ�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B	{B{B{B!{B){B1{B9{BA{BI{BQ{BY{Ba{Bi{Bq{By{B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=BĊ=BȊ=B̊=BЊ=BԊ=B؊=B܊=B��=B�=B�=B�=B��=B�=B��=B��=C ECECECECEC
ECECECECECECECECECECEC EC"EC$EC&EC(EC*EC,EC.EC0EC2EC4EC6EC8EC:EC<EC>EC@ECBECDECFECHECJ+�CL+�CNECPECRECTECVECXECZEC\EC^EC`ECbECdECfEChECjEClECnECpECrECtECvECxECzEC|EC~EC�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HD
�D��DHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt��Dyd{D�%qD�_
D���D��
D��D�O
D�x�D��qD��D�O
D�x�D��>D��D�B>D�b>D��
D�
D�UqD��D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A���A���A���A���A���A���A���A���A��PA��A�`BA���A�O�A��yA��hA��HA�n�A��A��A��yA��A�Q�A���A��;A�n�A�VA�C�A�{A�ĜA���A��A�dZA�5?A��A��A�
=A��TA���A�|�A�\)A�1'A�{A�
=A���A��RA�l�A�O�A�oA���A�Q�A� �A���A��A��A�M�A�ȴA�n�A�XA�-A��9A��A��A��/A�-A�|�A��A�t�A��A���A�bA�t�A��A�~�A�^5A��wA��
A�5?A��
A��A�1'A��uA��A�p�A�E�A�M�A�M�A���A��A��hA�;dA��9A�=qA���A��PA�/A�ZA�1'A�"�A��hA�+A�A��;A��A���A�PA~�+A}��A|-AzA�Ay�Av(�As��ArĜAo�Al�HAk�TAj�RAhI�AgVAd-AbVA`�DA_G�A]�FA\(�A[C�AZ  AW�AT��AT�ASp�AR�HAQ�FAP�AOC�AM
=AK�AI��AI�-AI/AH��AH�!AHbAG�7AFȴAE�AB�DA@1A?�PA?%A> �A<bNA;�A;hsA:�+A7A7XA7x�A6�/A6A4�A4{A3�A3�hA2M�A1��A0�A/��A/oA-��A+��A+7LA*�uA)�A(��A(�+A(A�A(JA'��A'l�A&�HA&bNA%��A%/A$�A$�!A$^5A#��A#t�A"bNA!|�A!oA �AC�A7LA�Az�AƨA�A�A�mAffA
=A��A1'AK�A��A��A��AjAA�PA�A�uA��A
��A	��A	A	�PA	G�A�\A=qA�At�AM�A�^A��A  AO�Az�At�A
=A �RA A�@�t�@�=q@��F@��h@��@�ff@���@��@��@���@�J@��`@�j@�r�@�t�@���@���@�X@��@⟾@߅@ݲ-@۝�@��@�x�@���@�Q�@�"�@֗�@�O�@�;d@�p�@�33@�~�@�E�@�$�@�J@��@�x�@�9X@˅@�@ʏ\@ɩ�@�Q�@�dZ@���@�x�@��
@���@��^@��h@�%@�  @�ff@��D@�l�@�v�@�@���@�%@�I�@���@�o@�-@��7@�?}@��9@���@�o@�{@��@���@�(�@��R@�n�@���@��@���@��D@���@�K�@��y@�ff@���@�`B@��@��;@���@��-@�/@�Ĝ@�+@�v�@�=q@��T@�hs@��/@�j@�b@�t�@�K�@�@��R@��+@�E�@���@���@�/@��@���@��u@��@���@�o@��R@�-@���@�`B@�?}@��@���@��
@�K�@�+@���@�n�@��T@�X@��`@��9@��@�j@�A�@��m@���@�;d@��H@���@��@���@��@��@��/@���@�bN@���@�ȴ@�=q@�x�@�O�@��@���@��D@�r�@�(�@�1@��m@��@�dZ@��@��\@�=q@�J@��@��-@��`@��D@�z�@�bN@�(�@�  @���@��F@�t�@�K�@�33@���@��R@�~�@��#@��@�x�@�X@�/@�%@��/@��D@�r�@�j@�9X@���@��@��P@�dZ@��+@�~�@��\@�~�@�J@���@��@���@�Ĝ@���@�Z@�A�@�1'@��@�@|�@;d@~��@~ff@}�@}��@}�h@}p�@}O�@|��@{��@{�@{t�@{dZ@z�@z�\@z-@z=q@y��@y�^@y��@y�7@yhs@x��@xr�@x  @w��@w;d@w+@v��@v��@vV@vV@vV@v$�@u��@u��@u�@uV@q��@i�@a��@[dZ@U�@N��@I&�@C��@=��@5@0��@)�7@%p�@!%@�@hs@�@-@\)@�@Ĝ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A���A���A���A���A���A���A���A���A���A���A���A��PA��A�`BA���A�O�A��yA��hA��HA�n�A��A��A��yA��A�Q�A���A��;A�n�A�VA�C�A�{A�ĜA���A��A�dZA�5?A��A��A�
=A��TA���A�|�A�\)A�1'A�{A�
=A���A��RA�l�A�O�A�oA���A�Q�A� �A���A��A��A�M�A�ȴA�n�A�XA�-A��9A��A��A��/A�-A�|�A��A�t�A��A���A�bA�t�A��A�~�A�^5A��wA��
A�5?A��
A��A�1'A��uA��A�p�A�E�A�M�A�M�A���A��A��hA�;dA��9A�=qA���A��PA�/A�ZA�1'A�"�A��hA�+A�A��;A��A���A�PA~�+A}��A|-AzA�Ay�Av(�As��ArĜAo�Al�HAk�TAj�RAhI�AgVAd-AbVA`�DA_G�A]�FA\(�A[C�AZ  AW�AT��AT�ASp�AR�HAQ�FAP�AOC�AM
=AK�AI��AI�-AI/AH��AH�!AHbAG�7AFȴAE�AB�DA@1A?�PA?%A> �A<bNA;�A;hsA:�+A7A7XA7x�A6�/A6A4�A4{A3�A3�hA2M�A1��A0�A/��A/oA-��A+��A+7LA*�uA)�A(��A(�+A(A�A(JA'��A'l�A&�HA&bNA%��A%/A$�A$�!A$^5A#��A#t�A"bNA!|�A!oA �AC�A7LA�Az�AƨA�A�A�mAffA
=A��A1'AK�A��A��A��AjAA�PA�A�uA��A
��A	��A	A	�PA	G�A�\A=qA�At�AM�A�^A��A  AO�Az�At�A
=A �RA A�@�t�@�=q@��F@��h@��@�ff@���@��@��@���@�J@��`@�j@�r�@�t�@���@���@�X@��@⟾@߅@ݲ-@۝�@��@�x�@���@�Q�@�"�@֗�@�O�@�;d@�p�@�33@�~�@�E�@�$�@�J@��@�x�@�9X@˅@�@ʏ\@ɩ�@�Q�@�dZ@���@�x�@��
@���@��^@��h@�%@�  @�ff@��D@�l�@�v�@�@���@�%@�I�@���@�o@�-@��7@�?}@��9@���@�o@�{@��@���@�(�@��R@�n�@���@��@���@��D@���@�K�@��y@�ff@���@�`B@��@��;@���@��-@�/@�Ĝ@�+@�v�@�=q@��T@�hs@��/@�j@�b@�t�@�K�@�@��R@��+@�E�@���@���@�/@��@���@��u@��@���@�o@��R@�-@���@�`B@�?}@��@���@��
@�K�@�+@���@�n�@��T@�X@��`@��9@��@�j@�A�@��m@���@�;d@��H@���@��@���@��@��@��/@���@�bN@���@�ȴ@�=q@�x�@�O�@��@���@��D@�r�@�(�@�1@��m@��@�dZ@��@��\@�=q@�J@��@��-@��`@��D@�z�@�bN@�(�@�  @���@��F@�t�@�K�@�33@���@��R@�~�@��#@��@�x�@�X@�/@�%@��/@��D@�r�@�j@�9X@���@��@��P@�dZ@��+@�~�@��\@�~�@�J@���@��@���@�Ĝ@���@�Z@�A�@�1'@��@�@|�@;d@~��@~ff@}�@}��@}�h@}p�@}O�@|��@{��@{�@{t�@{dZ@z�@z�\@z-@z=q@y��@y�^@y��@y�7@yhs@x��@xr�@x  @w��@w;d@w+@v��@v��@vV@vV@vV@v$�@u��@u��@u�@uV@q��@i�@a��@[dZ@U�@N��@I&�@C��@=��@5@0��@)�7@%p�@!%@�@hs@�@-@\)@�@Ĝ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�?BƨB��B��B��B��BǮBŢBÖB��B�qB�wB�qB�qB�dB�^B�XB�RB�RB�RB�LB�LB�?B�3B�B�B��B��B��B��B��B��B��B��B�hB�bB�JB�7B�Bx�Bt�BgmBZBQ�BR�BQ�BD�B#�B
=B��B�TB��BɺBĜB�}B�-B��Bt�B-BDB�B�mB��B�'B��B�=Bs�BhsB\)BP�B>wB.B�B�BoBPB
��B
�TB
�5B
�B
��B
��B
B
�qB
�!B
��B
��B
�JB
}�B
p�B
`BB
R�B
J�B
B�B
6FB
&�B
�B
B	�yB	�5B	��B	�?B	��B	��B	�7B	}�B	k�B	`BB	T�B	K�B	A�B	5?B	-B	"�B	{B	1B	+B	VB	{B	{B	hB	VB	VB	
=B	
=B		7B		7B	+B	B	B	B��B��B�ZB��B��B��BB�FB�!B�B��B��B��B��B��B��B��B�{B��B��B�{B�{B�{B�oB�{B�hB�=B�7B�1B�%B�B�B�B�B�B� B}�B{�By�Bw�Bv�Bu�Bt�Bs�Bq�Bn�Bl�BjBgmBffBffBdZBaHB^5BZBVBR�BN�BL�BL�BI�BE�BA�B=qB;dB:^B9XB6FB1'B,B)�B'�B&�B%�B$�B#�B"�B"�B �B�B�B�B�B�B�B�B{BuBuBhBbBVBJBJBDB
=B	7B1B+B+B+B+B%BBBBBBBBBBB+B+B+B+B1B1B1B
=BJBbBhBhBhBhBhBhBoBuB{BuB{B�B�B�B�B�B�B �B�B�B �B"�B&�B(�B+B,B,B-B/B0!B1'B33B49B49B5?B7LB8RB:^B<jB=qB>wBB�BC�BD�BE�BF�BG�BJ�BK�BL�BM�BN�BP�BR�BT�BXB\)B^5B_;BgmBk�Bl�Bn�Bp�Br�Bt�Bu�Bx�By�Bz�B|�B}�B~�B�B�B�B�%B�%B�7B�JB�\B�hB�uB��B��B��B��B��B��B��B��B��B��B�B�'B�?B�RB�^B�dB�dB�qB�}B��BÖBƨBɺB��B��B��B��B��B��B�B�5B�ZB�sB�B�B�B��B��B��B��B��B��B��B	B	B	+B	
=B	JB	PB	VB	�B	�B	�B	�B	�B	�B	 �B	!�B	$�B	%�B	&�B	(�B	,B	.B	33B	7LB	7LB	8RB	:^B	<jB	>wB	B�B	B�B	C�B	D�B	G�B	J�B	L�B	N�B	VB	W
B	XB	YB	YB	ZB	_;B	cTB	cTB	e`B	hsB	jB	k�B	m�B	n�B	p�B	q�B	r�B	u�B	x�B	z�B	z�B	{�B	|�B	� B	�B	�B	�%B	�%B	�1B	�=B	�JB	�VB	�hB	�oB	�uB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�LB	��B	�sB	��B
+B
uB
�B
&�B
/B
:^B
@�B
I�B
N�B
T�B
[#B
`BB
dZB
hsB
k�B
o�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�,BƖBʯB˶B̿BʮBǟBŐBÃB�oB�_B�eB�^B�`B�QB�LB�FB�AB�>B�?B�;B�;B�*B� B��B��B��B��B��B��B��B��B��B�nB�TB�OB�5B�$B��Bx�Bt�BgVBZBQ�BR�BQ�BD�B#�B
&B��B�<B��BɢBąB�eB�B��Bt�B,�B(B�B�SB�oB�B��B�#Bs�BhVB\BP�B>^B-�B�BhBXB6B
��B
�;B
�B
� B
��B
̶B
�vB
�ZB
�	B
��B
��B
�4B
}�B
p�B
`+B
R�B
J�B
ByB
63B
&�B
�B
�B	�eB	�#B	ʮB	�,B	��B	��B	�'B	}�B	kwB	`2B	T�B	K�B	A{B	51B	-B	"�B	pB	#B	B	IB	kB	qB	[B	GB	JB	
0B	
0B		(B		*B	B	B	�B	 �B��B��B�OB��B��B˼BB�=B�B��B��B��B��B��B��B��B�~B�sB��B��B�rB�rB�tB�gB�rB�_B�4B�/B�)B�B�B�B�B�B��B�B}�B{�By�Bw�Bv�Bu�Bt�Bs�Bq�Bn�Bl�BjxBgfBf]Bf^BdPBa>B^+BZBU�BR�BN�BL�BL�BI�BE�BA�B=OB;]B:WB97B6=B1 B,B)�B'�B&�B%�B$�B#�B"�B"�B �B�B�B�B~B�BmBaBYBpBRBHB@B5B*B)B=B
B	BB"B$BB
BB�BB�B�B�BB�B�BB�BB	B&B	BBBB
BBB@BGB`B`BEBFBEBgBmBtBkBtBB�B�BwB�B�B �B�B�B �B"�B&�B(�B*�B+�B+�B-B/B0B1B3(B4/B4-B53B7AB8GB:RB<^B=gB>lBB�BC�BD�BE�BF�BG�BJ�BK�BL�BM�BN�BP�BR�BT�BXB\B^'B_/Bg^BkuBl}Bn�Bp�Br�Bt�Bu�Bx�By�Bz�B|�B}�B~�B��B��B�B�B�B�*B�:B�KB�XB�eB�uB��B��B��B��B��B��B��B��B��B�B�B�/B�?B�PB�OB�QB�\B�jB�wBÅBƕBɨB˴B��B��B��B��B��B��B�%B�GB�`B�B�B�B��B��B��B��B��B��B��B	 �B		B	B	
&B	4B	<B	BB	pB	�B	�B	�B	�B	�B	 �B	!�B	$�B	%�B	&�B	(�B	+�B	-�B	3B	72B	75B	8:B	:EB	<RB	>`B	BxB	BxB	CB	D�B	G�B	J�B	L�B	N�B	U�B	V�B	W�B	Y B	YB	ZB	_ B	c;B	c>B	eFB	h[B	jgB	kkB	myB	n~B	p�B	q�B	r�B	u�B	x�B	z�B	z�B	{�B	|�B	�B	��B	�B	�
B	�B	�B	�"B	�-B	�<B	�MB	�UB	�]B	�XB	�ZB	�qB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�0B	��B	�YB	��B
B
YB
�B
&�B
.�B
:AB
@eB
I�B
N�B
T�B
[B
`$B
d9B
hTB
keB
o�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.27 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708122016053117081220160531170812  AO  ARCAADJP                                                                    20140721230826    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230826  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230826  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170812  IP                  G�O�G�O�G�O�                