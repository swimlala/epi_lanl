CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:36Z AOML 3.0 creation; 2016-06-01T00:08:15Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230836  20160531170815  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               <A   AO  4055_7112_060                   2C  D   APEX                            5374                            041511                          846 @�Ć��7�1   @�ć5��@:Q&�x���c���"��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    <A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DUy�DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` Dy� D�3D�I�D���D�� D�	�D�L�D�vfD��fD�3D�I�D��3D��fD�� D�,�DچfD�fD��D�I�D� D�ٚ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @J�H@���@ȣ�AQ�A$Q�ADQ�AdQ�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B	{B{B{B!{B){B1{B9{BA{BI{BQ{BY{Ba{Bi{Bq{By{B��=B��=B��=B��=B��=B��=B��=B��=B��=B��pB�#�B��B�W
B��=B��=B��=B��=BĊ=BȊ=B̊=BЊ=BԊ=B؊=B܊=B��=B�=B�=B�=B��=B�=B��=B��=C ECECECECEC
ECECECECECECECECECECEC EC"EC$EC&EC(EC*EC,EC.EC0EC2EC4EC6EC8EC:EC<EC>EC@ECBECDECFECHECJECLECNECPECRECTECVECXECZEC\EC^EC`ECbECdECfEChECjEClECnECpECrECtECvECxECzEC|EC~EC�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HD�D�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU��DVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDtqHDy�HD��D�R>D��qD�ؤD�>D�UqD�
D��
D��D�R>D���D��
D���D�5qDڏ
D�
D�">D�R>D�D��>111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A�z�A���A���A���Aϴ9AϮAϬAϩ�Aϣ�Aϡ�Aϝ�Aϝ�Aϝ�Aϛ�Aϗ�AϓuAϑhAύPAϏ\AϓuAϓuAϕ�Aϗ�Aϙ�Aϧ�AϮAϴ9A�ĜA���A��
A���Aϴ9A�I�A�~�A�=qA�1A�Q�AȬA��`A�hsA���A��A��FA���A��FA���A���A�Q�A��A�A�A�v�A��A���A��FA�%A��jA�=qA��-A��A���A��9A�1'A��;A�/A�+A�{A��TA���A��RA�p�A�z�A�K�A�ȴA�`BA��yA�XA�oA���A�ffA���A���A��;A��HA��A��DA�E�A�%A���A�/A�oA��hA�\)A�-A�A���A�1A��9A�A�A|r�A{XAz��Az{AyO�Aw�Av�Au��AtȴAsC�Aq+An�+Ah��Ad��Ac�;Ac�FAcp�AcdZAcAbE�AaO�A`~�A`^5A_�
A^~�A]
=A\  A[�wAZn�AY;dAW��AV1'AS�AR��AR��ARr�AQC�APA�AO��AO;dAN��ANȴAN�RAL�AI`BAGAEp�AC��AA`BA@�uA>z�A<�HA;��A;33A:�A:^5A9+A7"�A6v�A4��A4�A3XA21'A0��A/�A,�A)hsA)A(bNA&1A%�A$�A#�^A"��A"��A"��A"�\A"r�A"5?A!A!�7A!`BA �HA �\A -A�A�PAK�A�AVAVA��A�\AXAO�A��A�!A5?AAt�A^5A��A�9AA5?Ax�AĜAM�A�A��A
�RA	ƨA�HA  A�A�9A�#A"�A�A��AA�A�A �A �!A $�@�
=@���@�7L@��@�X@�1@�K�@���@��@�\@��@@��@�M�@���@�?}@�j@�A�@�ƨ@�P@�l�@�\)@ꟾ@�Q�@��m@�1'@�ƨ@�"�@���@�  @�@�r�@ץ�@�S�@�C�@�33@��@��@�  @��@У�@��m@ΰ!@ͩ�@˕�@ʗ�@�=q@�G�@�I�@�J@�7L@Ĭ@�Z@ÍP@�V@���@�`B@���@���@�bN@��;@�ȴ@�%@�dZ@�{@�O�@���@�33@���@��@���@���@���@�o@��@��R@���@�r�@���@��@��@��`@��;@�t�@��H@�ȴ@���@�E�@�@��@��
@���@��\@�$�@��^@�x�@��@���@��@�S�@�M�@���@�@��^@��-@��7@�G�@���@�Ĝ@���@��D@�(�@�ƨ@�|�@�C�@��!@�ff@�-@�$�@��@�J@���@���@��`@�z�@�9X@��m@��@�l�@�K�@���@�ff@��T@��-@�p�@���@��@��@��`@��`@��/@�Q�@��m@��F@��P@�l�@�~�@�G�@���@��`@���@���@���@���@���@�  @��F@��P@�S�@�@��R@��+@�ff@���@�7L@��/@�b@��
@��w@���@�l�@�+@��H@�ȴ@��R@�^5@�=q@�5?@�-@��@���@���@�`B@���@��@�Q�@��m@���@�S�@�@��y@��+@�5?@�J@��#@���@�x�@�`B@�%@��u@�j@�I�@�9X@�b@��
@���@�\)@�+@���@���@���@��+@�v�@�{@���@�7L@��@��@�%@��/@�Ĝ@�9X@
=@~�@~ȴ@~��@~V@~@}�-@}p�@|��@{��@z�\@z=q@zJ@y��@y��@y��@y7L@x�@w��@w�P@w�P@w|�@w\)@w\)@wK�@w;d@w�@vȴ@vE�@u�T@uV@t�D@tj@t9X@sS�@r�@r~�@q�@qhs@q7L@q&�@p�`@mV@iX@d9X@]V@T1@M�@F�y@=�T@8�`@3t�@0r�@+�F@( �@ r�@I�@+@��@O�@l�@I�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A�z�A���A���A���Aϴ9AϮAϬAϩ�Aϣ�Aϡ�Aϝ�Aϝ�Aϝ�Aϛ�Aϗ�AϓuAϑhAύPAϏ\AϓuAϓuAϕ�Aϗ�Aϙ�Aϧ�AϮAϴ9A�ĜA���A��
A���Aϴ9A�I�A�~�A�=qA�1A�Q�AȬA��`A�hsA���A��A��FA���A��FA���A���A�Q�A��A�A�A�v�A��A���A��FA�%A��jA�=qA��-A��A���A��9A�1'A��;A�/A�+A�{A��TA���A��RA�p�A�z�A�K�A�ȴA�`BA��yA�XA�oA���A�ffA���A���A��;A��HA��A��DA�E�A�%A���A�/A�oA��hA�\)A�-A�A���A�1A��9A�A�A|r�A{XAz��Az{AyO�Aw�Av�Au��AtȴAsC�Aq+An�+Ah��Ad��Ac�;Ac�FAcp�AcdZAcAbE�AaO�A`~�A`^5A_�
A^~�A]
=A\  A[�wAZn�AY;dAW��AV1'AS�AR��AR��ARr�AQC�APA�AO��AO;dAN��ANȴAN�RAL�AI`BAGAEp�AC��AA`BA@�uA>z�A<�HA;��A;33A:�A:^5A9+A7"�A6v�A4��A4�A3XA21'A0��A/�A,�A)hsA)A(bNA&1A%�A$�A#�^A"��A"��A"��A"�\A"r�A"5?A!A!�7A!`BA �HA �\A -A�A�PAK�A�AVAVA��A�\AXAO�A��A�!A5?AAt�A^5A��A�9AA5?Ax�AĜAM�A�A��A
�RA	ƨA�HA  A�A�9A�#A"�A�A��AA�A�A �A �!A $�@�
=@���@�7L@��@�X@�1@�K�@���@��@�\@��@@��@�M�@���@�?}@�j@�A�@�ƨ@�P@�l�@�\)@ꟾ@�Q�@��m@�1'@�ƨ@�"�@���@�  @�@�r�@ץ�@�S�@�C�@�33@��@��@�  @��@У�@��m@ΰ!@ͩ�@˕�@ʗ�@�=q@�G�@�I�@�J@�7L@Ĭ@�Z@ÍP@�V@���@�`B@���@���@�bN@��;@�ȴ@�%@�dZ@�{@�O�@���@�33@���@��@���@���@���@�o@��@��R@���@�r�@���@��@��@��`@��;@�t�@��H@�ȴ@���@�E�@�@��@��
@���@��\@�$�@��^@�x�@��@���@��@�S�@�M�@���@�@��^@��-@��7@�G�@���@�Ĝ@���@��D@�(�@�ƨ@�|�@�C�@��!@�ff@�-@�$�@��@�J@���@���@��`@�z�@�9X@��m@��@�l�@�K�@���@�ff@��T@��-@�p�@���@��@��@��`@��`@��/@�Q�@��m@��F@��P@�l�@�~�@�G�@���@��`@���@���@���@���@���@�  @��F@��P@�S�@�@��R@��+@�ff@���@�7L@��/@�b@��
@��w@���@�l�@�+@��H@�ȴ@��R@�^5@�=q@�5?@�-@��@���@���@�`B@���@��@�Q�@��m@���@�S�@�@��y@��+@�5?@�J@��#@���@�x�@�`B@�%@��u@�j@�I�@�9X@�b@��
@���@�\)@�+@���@���@���@��+@�v�@�{@���@�7L@��@��@�%@��/@�Ĝ@�9X@
=@~�@~ȴ@~��@~V@~@}�-@}p�@|��@{��@z�\@z=q@zJ@y��@y��@y��@y7L@x�@w��@w�P@w�P@w|�@w\)@w\)@wK�@w;d@w�@vȴ@vE�@u�T@uV@t�D@tj@t9X@sS�@r�@r~�@q�@qhs@q7L@q&�@p�`@mV@iX@d9X@]V@T1@M�@F�y@=�T@8�`@3t�@0r�@+�F@( �@ r�@I�@+@��@O�@l�@I�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBC�B?}B?}B>wB?}B?}B?}B@�BA�BA�BA�BA�BA�BB�BB�BB�BB�BA�BA�BB�BC�BC�BD�BE�BF�BI�BL�BN�BW
B]/B`BBaHB_;BW
BI�BB�B(�B��B�NB�
B��BǮB�'B>wBoB��B��B�B�B�B��B��B�ZB��BŢB�jB��B�{B�VB�%By�Bo�BffBS�B1'B �BbBPBJB	7BB�B�jB��B}�Bt�Bk�Be`B\)BD�B+B�B1B
�B
�)B
�B
��B
�dB
�B
��B
�oB
�DB
�+B
�B
�B
{�B
n�B
[#B
N�B
;dB
(�B
 �B
�B
�B
hB
%B	��B	��B	�B	�`B	�
B	ÖB	��B	�\B	�7B	�1B	�+B	�%B	�B	~�B	y�B	u�B	t�B	p�B	hsB	aHB	]/B	ZB	R�B	K�B	E�B	>wB	49B	33B	2-B	/B	)�B	%�B	#�B	!�B	 �B	�B	�B	hB	+B��B��B�B�ZB�)B��B��B��B��BɺBŢB�}B�RB�FB�-B�!B�B��B��B��B�bB�JB�=B�B{�Bx�Bw�Bu�Bs�Bs�Bs�Bs�Bs�Br�Bq�Bq�Bp�Bo�Bn�Bm�Bl�Bl�Bk�Bk�Bk�BjBjBffB`BBXBR�BP�BN�BJ�BF�BC�BA�B=qB;dB9XB7LB5?B49B33B1'B/B-B)�B'�B%�B#�B"�B �B�B�B�B�B�B�B�B�B{BoBhBhBhBbB\BPBJBDBDBDBDB
=B
=B
=B
=B
=B
=B	7B1B%BBB%B%BBBB%B+B+B1B+B+BB%B%B+B1B1B1B1B
=BJBDBJBJB\BbBhBbBhBuB�B�B�B�B�B�B�B�B�B"�B#�B#�B#�B$�B+B-B-B2-B33B33B33B1'B8RB;dB<jB?}BF�BJ�BK�BN�BN�BP�BQ�BS�BW
B[#B`BBaHBcTBdZBe`BffBhsBjBr�Bz�B~�B~�B� B� B�B�B�B�%B�+B�+B�7B�DB�PB�VB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�3B�?B�RB�RB�XB�RB�RB�RB�jB�}B��BBBȴB��B��B��B��B��B��B��B��B�B�#B�)B�5B�BB�NB�TB�ZB�yB�B�B��B��B��B��B��B��B	B	B	B	%B	+B	+B	+B	1B	
=B	JB	VB	hB	{B	�B	�B	�B	�B	"�B	"�B	%�B	(�B	+B	,B	/B	0!B	0!B	49B	8RB	:^B	;dB	;dB	=qB	?}B	A�B	D�B	E�B	H�B	J�B	L�B	L�B	M�B	Q�B	W
B	[#B	\)B	]/B	^5B	`BB	aHB	gmB	o�B	p�B	p�B	q�B	r�B	t�B	v�B	x�B	|�B	�B	�1B	�7B	�=B	�DB	�JB	�PB	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�'B	�-B	�9B	B	��B	�BB	�B
B
bB
�B
)�B
1'B
;dB
?}B
F�B
I�B
R�B
VB
\)B
cTB
iyB
p�B
u�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BC�B?nB?qB>kB?pB?rB?tB@wBA~BA~BA~BA~BA~BB�BB�BB�BB�BABABB�BC�BC�BD�BE�BF�BI�BL�BN�BW B]%B`<Ba?B_3BWBI�BB�B(�B��B�<B��B��BǞB�B>dB\B��B��B�B�B�}B��B��B�EB��BŋB�RB��B�`B�<B�By�Bo�BfLBS�B1B �BGB:B/B	B�B�wB�RB��B}�Bt�BkoBeHB\BD�B*�BuBB
�hB
�B
��B
��B
�JB
��B
��B
�UB
�-B
�B
�B
��B
{�B
n�B
[B
N�B
;QB
(�B
 �B
�B
{B
RB
B	��B	��B	�B	�NB	��B	ÃB	��B	�JB	�$B	�!B	�B	�B	�B	~�B	y�B	u�B	t�B	p�B	heB	a9B	] B	ZB	R�B	K�B	E�B	>fB	4*B	3$B	2B	/B	)�B	%�B	#�B	!�B	 �B	�B	�B	\B	B��B��B�B�NB�B��B��B��B˽BɰBŘB�sB�GB�9B�!B�B� B��B��B��B�ZB�?B�3B�B{�Bx�Bw�Bu�Bs�Bs�Bs�Bs�Bs�Br�Bq�Bq�Bp�Bo�Bn�Bm�Bl�Bl�Bk}Bk}BkBjxBjwBf_B`9BXBR�BP�BN�BJ�BF�BC�BA�B=mB;]B9RB7GB58B44B3,B1 B/B-B)�B'�B%�B#�B"�B �B�B�B|BxB�BkBeB^BsBLBFBIBGBABTB/B)B"B"B!B$B
B
B
B
B
B
6B	BBBB�BBB�BB�BBB	BB
B
B�BBB$B*BBBB
B(BB%B(BUBZBDB>B_BlBxBzBB�B~BjB�B�B�B"�B#�B#�B#�B$�B*�B-B-B2#B3(B3)B3*B1B8EB;YB<^B?pBF�BJ�BK�BN�BN�BP�BQ�BS�BV�B[B`5Ba;BcHBdJBeSBf[BheBjpBr�Bz�B~�B~�B�B�B��B�B�B�B�B�B�)B�6B�@B�FB�`B�jB�uB�uB�vB�vB�~B��B��B��B��B��B��B��B��B��B�B�B�!B�.B�@B�@B�EB�?B�?B�?B�WB�lB�xB�}B�{BȡB��B��B��B��B��B��B��B��B�B�B�B�!B�.B�:B�AB�HB�eB�|B��B��B��B��B��B��B��B	�B	�B	�B	B	B	B	B	B	
)B	4B	DB	QB	fB	qB	�B	�B	�B	"�B	"�B	%�B	(�B	*�B	+�B	/B	0	B	0	B	4!B	8;B	:IB	;NB	;LB	=YB	?eB	AqB	D�B	E�B	H�B	J�B	L�B	L�B	M�B	Q�B	V�B	[
B	\B	]B	^B	`(B	a0B	gTB	o�B	p�B	p�B	q�B	r�B	t�B	v�B	x�B	|�B	��B	�B	�B	�#B	�)B	�.B	�8B	�;B	�UB	�lB	�uB	�sB	�qB	�sB	�yB	�zB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�sB	��B	�&B	�B
�B
CB
�B
)�B
1B
;GB
?^B
F�B
I�B
R�B
U�B
\B
c5B
iZB
p�B
u�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.27 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708152016053117081520160531170815  AO  ARCAADJP                                                                    20140721230836    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230836  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230836  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170815  IP                  G�O�G�O�G�O�                