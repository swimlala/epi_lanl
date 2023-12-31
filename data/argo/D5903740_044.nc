CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:27Z AOML 3.0 creation; 2016-06-01T00:08:12Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230827  20160531170812  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               ,A   AO  4055_7112_044                   2C  D   APEX                            5374                            041511                          846 @֛wn���1   @֛w�@@:/\(��co��-V1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ,A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_fD_�fD`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D�  D�L�D���D�ٚD��D�9�D�� D���D�fD�@ D�y�D�ɚD��D�<�DچfD��3D� D�9�D� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�
=@ȣ�AQ�A$Q�ADQ�AdQ�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B	z�B{B{B!{B){B1{B9{BA{BI{BQ{BY{Ba{Bi{Bq{By{B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=BĊ=BȊ=B̊=BЊ=BԊ=B؊=B܊=B��=B�=B�=B�=B��=B�=B��=B��=C ECECECECEC
ECECECECECECECECECECEC EC"EC$EC&EC(EC*EC,EC.EC0EC2EC4EC6EC8EC:EC<EC>EC@ECBECDECFECHECJECLECNECPECRECTECVECXECZEC\EC^EC`ECbECdECfEChECjEClECnECpECrECtECvECxECzEC|EC~EC�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_�D_��D`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDt׮Dy�D�(�D�UqD��>D��>D�">D�B>D���D��qD�
D�H�D��>D��>D�qD�EqDڏ
D���D��D�B>D�D��q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A��PA�v�A�r�A�jA�hsA�dZA�^5A�ZA�XA�VA�O�A�?}A���A�XA��A�oA�9XA�v�A�z�A���A��A��^A��A��HA��A�l�A�?}A��7A��mA�VA��A��9A�E�A��A�hsA��TA��-A�+A���A���A�%A�VA�$�A�  A���A��wA��DA��;A��
A��\A�33A��wA���A�p�A�dZA�7LA��`A���A�`BA�%A���A� �A��PA�+A��jA�
=A�~�A�  A�ffA�ƨA���A�A�-A�E�A���A��-A��A��A�M�A���A��A��mA�E�A�I�A��HA�/A�%A�A��\A���A�`BA���A���A�  A�7LA�bA|�A}S�A{S�Ax��Aul�AsK�AoXAm7LAj^5AgC�Af5?Ae�AdffAc�#Acl�Ab~�A`��A_p�A\��A[|�AZbAYAX=qAV�+AS�;AR��ARn�AP�!AO�^AOAMp�ALbNAK|�AJ��AJJAIl�AG�FAF�DAEƨAEO�AE"�AD�ACdZABr�ABbAA�TAA�hA@��A@^5A>��A=�^A<��A;��A:9XA8��A8�A7oA6�A5K�A4�jA4M�A3dZA2��A29XA1��A1`BA1�^A0�jA0I�A0(�A/��A.�RA-�^A,�jA,A�A+�^A+�A*9XA(�A(VA'�FA&�9A&�+A&1A%��A%t�A%G�A%%A$�!A#�A#7LA"�A!t�A!+A ��A �jA �uA��A��A��A �A\)AA�A��AA��A~�A|�A��A9XAdZA��AbA�/AI�A�mAS�AM�AC�AQ�A�A�#AVA
��A	XA1A��A��A�^A��A  A�-A��AhsA~�A��A?}AVA �+A  �@�
=@�hs@��@�dZ@�A�@�n�@���@���@��`@�{@��#@�bN@�$�@�j@�+@��@�E�@���@���@�S�@�r�@�t�@���@ۍP@��y@��@�hs@��/@�;d@�z�@�~�@��@��@�C�@�^5@͡�@��@̣�@�\)@�$�@ȼj@ǥ�@���@�{@��@�I�@¸R@�O�@�I�@�A�@��F@���@�-@��7@���@�  @���@��@���@�j@�Q�@� �@��@�"�@���@���@�%@�j@�  @���@�J@�@�%@���@�Q�@�;d@��+@�`B@�z�@��@�ƨ@�K�@�X@�z�@�1'@��H@�ff@�J@�hs@�&�@�%@��`@�Ĝ@��@�r�@��w@�33@�
=@��@���@�@�p�@�7L@�V@���@��u@� �@�dZ@���@�ȴ@�M�@��-@�/@�z�@�b@��w@�l�@�o@���@��#@�%@�r�@�9X@�l�@��y@��R@�5?@���@�?}@��/@��9@�z�@�Q�@�A�@�  @��F@�t�@��@�~�@��#@�`B@�/@�V@���@�z�@� �@���@�ƨ@�|�@��@�v�@���@�&�@�%@��`@�Ĝ@��D@�9X@�1@���@��w@�K�@��@��@�^5@�-@��@�@���@�p�@��/@���@��D@�(�@��m@��w@��@�|�@�33@�ȴ@��!@�M�@�J@���@�@���@���@��h@�`B@�7L@��@�V@��/@���@�r�@��@�t�@�|�@�\)@�+@�ȴ@���@�~�@���@�G�@��@��/@�Ĝ@���@�z�@�Z@�(�@�w@��@��@�@~�@~ȴ@~��@~ff@~$�@}p�@}?}@}�@|�@|�@|(�@|1@{�F@{@zn�@z=q@z-@zJ@y��@y�^@yX@y7L@x��@x��@xbN@xb@w�;@w|�@w
=@vv�@v5?@u�T@u�h@u?}@t��@q�@i7L@`��@[dZ@T�@NE�@HĜ@CdZ@<Z@6V@0 �@)�@&�@!hs@I�@b@?}@r�@Z@	%@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A���A��PA�v�A�r�A�jA�hsA�dZA�^5A�ZA�XA�VA�O�A�?}A���A�XA��A�oA�9XA�v�A�z�A���A��A��^A��A��HA��A�l�A�?}A��7A��mA�VA��A��9A�E�A��A�hsA��TA��-A�+A���A���A�%A�VA�$�A�  A���A��wA��DA��;A��
A��\A�33A��wA���A�p�A�dZA�7LA��`A���A�`BA�%A���A� �A��PA�+A��jA�
=A�~�A�  A�ffA�ƨA���A�A�-A�E�A���A��-A��A��A�M�A���A��A��mA�E�A�I�A��HA�/A�%A�A��\A���A�`BA���A���A�  A�7LA�bA|�A}S�A{S�Ax��Aul�AsK�AoXAm7LAj^5AgC�Af5?Ae�AdffAc�#Acl�Ab~�A`��A_p�A\��A[|�AZbAYAX=qAV�+AS�;AR��ARn�AP�!AO�^AOAMp�ALbNAK|�AJ��AJJAIl�AG�FAF�DAEƨAEO�AE"�AD�ACdZABr�ABbAA�TAA�hA@��A@^5A>��A=�^A<��A;��A:9XA8��A8�A7oA6�A5K�A4�jA4M�A3dZA2��A29XA1��A1`BA1�^A0�jA0I�A0(�A/��A.�RA-�^A,�jA,A�A+�^A+�A*9XA(�A(VA'�FA&�9A&�+A&1A%��A%t�A%G�A%%A$�!A#�A#7LA"�A!t�A!+A ��A �jA �uA��A��A��A �A\)AA�A��AA��A~�A|�A��A9XAdZA��AbA�/AI�A�mAS�AM�AC�AQ�A�A�#AVA
��A	XA1A��A��A�^A��A  A�-A��AhsA~�A��A?}AVA �+A  �@�
=@�hs@��@�dZ@�A�@�n�@���@���@��`@�{@��#@�bN@�$�@�j@�+@��@�E�@���@���@�S�@�r�@�t�@���@ۍP@��y@��@�hs@��/@�;d@�z�@�~�@��@��@�C�@�^5@͡�@��@̣�@�\)@�$�@ȼj@ǥ�@���@�{@��@�I�@¸R@�O�@�I�@�A�@��F@���@�-@��7@���@�  @���@��@���@�j@�Q�@� �@��@�"�@���@���@�%@�j@�  @���@�J@�@�%@���@�Q�@�;d@��+@�`B@�z�@��@�ƨ@�K�@�X@�z�@�1'@��H@�ff@�J@�hs@�&�@�%@��`@�Ĝ@��@�r�@��w@�33@�
=@��@���@�@�p�@�7L@�V@���@��u@� �@�dZ@���@�ȴ@�M�@��-@�/@�z�@�b@��w@�l�@�o@���@��#@�%@�r�@�9X@�l�@��y@��R@�5?@���@�?}@��/@��9@�z�@�Q�@�A�@�  @��F@�t�@��@�~�@��#@�`B@�/@�V@���@�z�@� �@���@�ƨ@�|�@��@�v�@���@�&�@�%@��`@�Ĝ@��D@�9X@�1@���@��w@�K�@��@��@�^5@�-@��@�@���@�p�@��/@���@��D@�(�@��m@��w@��@�|�@�33@�ȴ@��!@�M�@�J@���@�@���@���@��h@�`B@�7L@��@�V@��/@���@�r�@��@�t�@�|�@�\)@�+@�ȴ@���@�~�@���@�G�@��@��/@�Ĝ@���@�z�@�Z@�(�@�w@��@��@�@~�@~ȴ@~��@~ff@~$�@}p�@}?}@}�@|�@|�@|(�@|1@{�F@{@zn�@z=q@z-@zJ@y��@y�^@yX@y7L@x��@x��@xbN@xb@w�;@w|�@w
=@vv�@v5?@u�T@u�h@u?}G�O�@q�@i7L@`��@[dZ@T�@NE�@HĜ@CdZ@<Z@6V@0 �@)�@&�@!hs@I�@b@?}@r�@Z@	%@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B�bB�DB�B~�Bu�Bt�Bp�Bu�B� B�1B�JB�hB��B��B��B��B��B��B��B��B��B��B��B�hB|�Bq�Bs�Bx�BgmBZBW
BR�BW
BR�BO�BdZBn�Bm�Bo�BffBdZBdZBe`BffBbNB^5B_;BZBP�BD�B49B)�B�B1B��B�NB�LB�%BJ�B �B�TBȴB��B�JBz�Bu�BdZBC�B)�BoB1B
��B
�B
�`B
��B
�}B
�!B
��B
��B
�oB
�1B
z�B
m�B
[#B
E�B
49B
�B
1B	�B	�B	�dB	��B	��B	�%B	~�B	v�B	p�B	l�B	hsB	cTB	YB	M�B	@�B	8RB	1'B	,B	&�B	�B	uB	\B	JB	B	B��B��B��B�B�B�B�B�`B�BB�/B�)B�B�B�B��B��B��B��B��B��BŢB�}B�^B�3B�B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�?B�LB�dB�qB�qB�XB�FB�3B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�bB�\B�VB�PB�DB�1B�B�B� B{�Bx�Bv�Bs�Bp�Bm�Bk�BhsBe`BbNB`BB]/BYBS�BR�BO�BM�BI�BF�BC�B?}B<jB:^B7LB49B1'B/B,B(�B&�B&�B&�B&�B%�B&�B&�B&�B%�B#�B"�B!�B�B�B�B�B�B�BoBVBJBDBDBDB
=BDB
=B
=B
=B	7B1B1B1B+B1B1B1B+B%B%B	7BDBJBVB\BbBhBhBhBoB{B�B�B�B�B�B�B�B �B"�B"�B"�B#�B$�B%�B&�B'�B)�B,B.B/B/B/B0!B1'B2-B49B5?B6FB7LB:^B<jB<jB>wB>wB>wBA�BB�BE�BH�BI�BJ�BJ�BQ�BT�BT�BZB\)B]/B`BBaHBaHBbNBbNBbNBbNBffBiyBiyBjBjBo�Bq�Br�Bs�Bt�Bu�Bw�B{�B}�B}�B�B�B�B�7B�JB�VB�\B�hB�{B��B��B��B��B��B�B�B�'B�9B�LB�^B�dB�jB�qB�qB��B��BÖBƨBɺB��B��B��B��B��B�B�#B�)B�/B�;B�NB�mB�B�B�B��B��B��B��B��B��B	B	B	%B	+B	DB	PB	\B	bB	hB	uB	�B	�B	�B	 �B	"�B	#�B	$�B	%�B	'�B	,B	-B	1'B	5?B	5?B	7LB	8RB	8RB	9XB	;dB	=qB	?}B	?}B	A�B	D�B	F�B	G�B	K�B	N�B	P�B	R�B	XB	YB	[#B	`BB	bNB	cTB	dZB	e`B	gmB	hsB	jB	l�B	o�B	p�B	p�B	t�B	u�B	v�B	v�B	w�B	x�B	|�B	}�B	~�B	� B	�B	�B	�B	�+B	�DB	�\B	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�FB	��B	�B	��B
1B
uB
�B
%�B
1'B
9XB
A�B
I�B
L�B
T�B
[#B
_;B
cTB
iyB
n�B
r�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�vB�uB�|B�|B�xB�tB�tB�pB�vB�vB�tB�oB�oB�OB�2B�B~�Bu�Bt�Bp�Bu�B�B�B�9B�WB��B��B��B��B��B��B��B��B��B��B��B�TB|�Bq�Bs�Bx�BgXBZBV�BR�BV�BR�BO�BdFBn�Bm~Bo�BfSBdGBdFBeKBfQBb8B^B_$BZBP�BD�B4$B)�B�BB��B�8B�3B�BJ�B �B�<BțB��B�0Bz�Bu�Bd>BCB)�BVBB
��B
�B
�EB
��B
�eB
�	B
��B
��B
�XB
�B
z�B
m{B
[B
E�B
4$B
�B
 B	�nB	�B	�UB	��B	��B	�B	~�B	v�B	p�B	lzB	hcB	cEB	Y	B	M�B	@vB	8DB	1B	+�B	&�B	�B	eB	NB	?B	B	 �B��B��B��B�B�B�B�zB�TB�5B�$B�B�B�B��B��B��B��B��B��B˽BŕB�tB�SB�)B�B��B��B��B��B��B��B��B��B��B��B�B��B�	B�B�6B�CB�ZB�fB�gB�MB�9B�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�qB�dB�YB�RB�LB�EB�9B�'B�B�B�B{�Bx�Bv�Bs�Bp�Bm�Bk|BhmBeWBbEB`:B]&BYBS�BR�BO�BM�BI�BF�BC�B?vB<bB:YB7EB42B1"B.�B, B(�B&�B&�B&�B&�B%�B&�B&�B&�B%�B#�B"�B!�B�B�B�B�BqBeBLB6B*B;B<B<B
B"B
B
B
B	.BBBB"BBBBBBB	B<B(B3BSBZBDBEBDBiBWBdB�B�BxB�B�B�B �B"�B"�B"�B#�B$�B%�B&�B'�B)�B+�B.
B/B/B/B0B1B2#B4,B54B6;B7BB:SB<_B<^B>kB>kB>jBA|BB�BE�BH�BI�BJ�BJ�BQ�BT�BT�BZB\B]B`4Ba<Ba=BbCBbBBbBBbABfYBilBilBjqBjrBo�Bq�Br�Bs�Bt�Bu�Bw�B{�B}�B}�B��B�	B�B�'B�9B�GB�NB�XB�kB��B��B��B��B��B��B�B�B�&B�;B�MB�SB�YB�`B�^B�qB�yBÄBƖBɫB��B��B��B��B��B��B�B�B�B�&B�<B�YB�~B�B�B��B��B��B��B��B��B	 �B	B	B	B	/B	;B	GB	LB	VB	_B	�B	�B	�B	 �B	"�B	#�B	$�B	%�B	'�B	+�B	,�B	1B	5'B	5(B	74B	8:B	8:B	9@B	;OB	=YB	?fB	?fB	ArB	D�B	F�B	G�B	K�B	N�B	P�B	R�B	W�B	X�B	[B	`+B	b5B	c;B	d@B	eHB	gRB	hZB	jeB	lpB	o�B	p�B	p�B	t�B	u�B	v�B	v�B	w�B	x�B	|�B	}�B	~�B	�B	��B	�B	�B	�B	�(B	�AB	�TB	�YB	�]B	�bB	�hB	�nB	�qB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	�*B	��B	�_B	��B
B
VB
�B
%�B
1
B
97B
AiB
I�B
L�B
T�B
[B
_B
c7B
i[B
nwB
r�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.27 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708122016053117081220160531170812  AO  ARCAADJP                                                                    20140721230827    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230827  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230827  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170812  IP                  G�O�G�O�G�O�                