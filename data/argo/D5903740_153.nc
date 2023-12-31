CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:15:19Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20181121041519  20190604095259  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055                            2C  D   APEX                            5374                            041511                          846 @ײ��d��1   @ײ�4&@;��1&��cn�1'1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DPfDP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D���D�A�D���D�ȤD��D�G�D��D�θD�RD�=�D�}qD�ʏD��fD�33D�w�D�ФD� D�K�D�qD�p 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @J�H@���@ȣ�AQ�A$Q�ADQ�AdQ�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B	{B{B{B!{B){B1{B9{BA{BI{BQ{BY{Ba{Bi{Bq{By{B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=BĊ=BȊ=B̊=BЊ=BԊ=B؊=B܊=B��=B�=B�=B�=B��=B�=B��=B��=C ECECECECEC
ECECECECECECECECECECEC EC"EC$EC&EC(EC*EC,EC.EC0EC2EC4EC6EC8EC:EC<EC>EC@ECBECDECFECHECJECLECNECPECRECTECVECXECZEC\EC^EC`ECbECdECfEChECjEClECnECpECrECtECvECxECzEC|EC~EC�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD��DHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDP�DP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDt�Dy��D�{D�J�D��RD��HD�)D�PRD���D��\D��D�FgD��D��3D�
D�;�DڀRD��HD��D�T)D�D�x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ȴA�ƨA���A��#A��#A��/A��HA��HA��
A��A��A��
A��
A���A���A��
A��
A���A���A���A���A��^A���A�ffA�ffA�A��9A��A�A��A�Q�A�K�A�~�A��PA�K�A�r�A�p�A��PA�XA�?}A�oA��jA��^A��9A���A�A�ĜA��\A�l�A�A�A�$�A��/A�n�A�M�A��!A�
=A�M�A�I�A�1'A��A���A��A� �A�1A�
=A�JA���A�JA��A���A�
=A�G�A�bA��A�K�A�XA�$�A�ĜA��`A��\A�$�A��!A�I�A�A��!A��+A���A��A�K�A�ZA�;dA�ĜA���A�p�A�JA�(�A~�RA|��A|bAy�#AxVAvr�Au�TAu%As�^AqApbNAn�`Al�RAj��Aj1'Ai
=Ag�Af=qAd1'Ac?}AbM�Aa�FAa+A`jA_�A^��A^1A]t�A\��A\=qA[|�AZ(�AW��AT��ASVAQC�AOC�ANI�AM7LAL1AJ�!AI�AI33AIoAHA�AGhsAF1'AE
=AD�AD9XABv�A@jA?S�A?
=A>�HA>M�A=�7A<1A;�7A:ȴA9��A9;dA8��A8JA7��A6v�A5O�A3oA2M�A2{A2JA1�A1\)A1oA0�/A0JA/�^A/�-A/�PA.�`A.E�A-S�A,{A*��A*z�A*E�A*-A)��A(��A'&�A&�RA&A�A%�A%��A%p�A$��A#�A#|�A"��A"M�A"5?A!ƨA M�A��A�#A�A�A�uA �A��AK�A�A`BA\)A%A$�A��A^5A+A9XA�A�`A�uA$�AAx�AK�A�AQ�AM�A+A^5A�-A�PAp�AC�A
��A	�A	G�A�DA�mA&�A�AȴA�DA�AĜA��A|�A+A�A�#A {@���@�/@��
@�S�@���@�E�@��-@�?}@��`@�j@�x�@�w@�V@��/@@�;d@��@�n�@��@���@�@���@���@�|�@��@��y@�ff@��@٩�@׮@�M�@�hs@�bN@ҟ�@�l�@�ff@�{@�hs@̃@��y@�hs@ȴ9@�ƨ@��@�M�@�?}@�K�@�^5@�V@�M�@�$�@���@�1'@��
@���@�S�@���@�V@���@���@�hs@�7L@��@���@��
@�$�@�@�dZ@�v�@���@��@�I�@�1@��@�ȴ@�o@��y@��T@���@�&�@��u@��!@�$�@��T@�O�@���@� �@�  @���@�t�@�+@�v�@�@�O�@��/@��u@�b@�33@���@�-@�p�@�V@�Ĝ@�z�@���@�33@���@���@��@��@���@�K�@�ȴ@�M�@�@�O�@�r�@�b@�;d@��@�x�@�V@���@�  @��@�=q@��-@�x�@�Ĝ@�b@���@�ƨ@���@�t�@��@��H@��y@�M�@�-@�J@��-@�x�@�X@�G�@��@���@��@�  @���@���@�C�@��@�ȴ@��R@�~�@�n�@�ff@�$�@��h@�&�@���@���@�%@�V@�%@��/@�Ĝ@���@�b@�;d@�o@��@���@�5?@���@�X@���@��`@��9@��D@�bN@� �@�1@�dZ@���@��R@�~�@�M�@�=q@�J@��-@�&�@�j@��
@���@���@��P@�"�@�|�@��w@���@�Ĝ@��j@��D@��D@�bN@�9X@�  @�  @�  @�ƨ@���@�|�@�l�@�K�@���@�M�@��@���@��7@�X@�7L@�/@��@�%@�Ĝ@��D@�(�@�;@�P@~ff@}�-@}`B@|��@|�/@|�D@|(�@{��@{�
@{�F@{��@x�v@p��@f��@^�x@V�@O��@H�@A8�@;�@6O@12a@-=�@'�4@"�@�@��@�@�D@�@�:@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 A�ȴA�ƨA���A��#A��#A��/A��HA��HA��
A��A��A��
A��
A���A���A��
A��
A���A���A���A���A��^A���A�ffA�ffA�A��9A��A�A��A�Q�A�K�A�~�A��PA�K�A�r�A�p�A��PA�XA�?}A�oA��jA��^A��9A���A�A�ĜA��\A�l�A�A�A�$�A��/A�n�A�M�A��!A�
=A�M�A�I�A�1'A��A���A��A� �A�1A�
=A�JA���A�JA��A���A�
=A�G�A�bA��A�K�A�XA�$�A�ĜA��`A��\A�$�A��!A�I�A�A��!A��+A���A��A�K�A�ZA�;dA�ĜA���A�p�A�JA�(�A~�RA|��A|bAy�#AxVAvr�Au�TAu%As�^AqApbNAn�`Al�RAj��Aj1'Ai
=Ag�Af=qAd1'Ac?}AbM�Aa�FAa+A`jA_�A^��A^1A]t�A\��A\=qA[|�AZ(�AW��AT��ASVAQC�AOC�ANI�AM7LAL1AJ�!AI�AI33AIoAHA�AGhsAF1'AE
=AD�AD9XABv�A@jA?S�A?
=A>�HA>M�A=�7A<1A;�7A:ȴA9��A9;dA8��A8JA7��A6v�A5O�A3oA2M�A2{A2JA1�A1\)A1oA0�/A0JA/�^A/�-A/�PA.�`A.E�A-S�A,{A*��A*z�A*E�A*-A)��A(��A'&�A&�RA&A�A%�A%��A%p�A$��A#�A#|�A"��A"M�A"5?A!ƨA M�A��A�#A�A�A�uA �A��AK�A�A`BA\)A%A$�A��A^5A+A9XA�A�`A�uA$�AAx�AK�A�AQ�AM�A+A^5A�-A�PAp�AC�A
��A	�A	G�A�DA�mA&�A�AȴA�DA�AĜA��A|�A+A�A�#A {@���@�/@��
@�S�@���@�E�@��-@�?}@��`@�j@�x�@�w@�V@��/@@�;d@��@�n�@��@���@�@���@���@�|�@��@��y@�ff@��@٩�@׮@�M�@�hs@�bN@ҟ�@�l�@�ff@�{@�hs@̃@��y@�hs@ȴ9@�ƨ@��@�M�@�?}@�K�@�^5@�V@�M�@�$�@���@�1'@��
@���@�S�@���@�V@���@���@�hs@�7L@��@���@��
@�$�@�@�dZ@�v�@���@��@�I�@�1@��@�ȴ@�o@��y@��T@���@�&�@��u@��!@�$�@��T@�O�@���@� �@�  @���@�t�@�+@�v�@�@�O�@��/@��u@�b@�33@���@�-@�p�@�V@�Ĝ@�z�@���@�33@���@���@��@��@���@�K�@�ȴ@�M�@�@�O�@�r�@�b@�;d@��@�x�@�V@���@�  @��@�=q@��-@�x�@�Ĝ@�b@���@�ƨ@���@�t�@��@��H@��y@�M�@�-@�J@��-@�x�@�X@�G�@��@���@��@�  @���@���@�C�@��@�ȴ@��R@�~�@�n�@�ff@�$�@��h@�&�@���@���@�%@�V@�%@��/@�Ĝ@���@�b@�;d@�o@��@���@�5?@���@�X@���@��`@��9@��D@�bN@� �@�1@�dZ@���@��R@�~�@�M�@�=q@�J@��-@�&�@�j@��
@���@���@��P@�"�@�|�@��w@���@�Ĝ@��j@��D@��D@�bN@�9X@�  @�  @�  @�ƨ@���@�|�@�l�@�K�@���@�M�@��@���@��7@�X@�7L@�/@��@�%@�Ĝ@��D@�(�@�;@�P@~ff@}�-@}`B@|��@|�/@|�D@|(�@{��@{�
@{�FG�O�@x�v@p��@f��@^�x@V�@O��@H�@A8�@;�@6O@12a@-=�@'�4@"�@�@��@�@�D@�@�:@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB_;B`BB`BB`BB`BB`BB`BB`BB`BB`BB`BB`BB`BB`BB`BB`BB`BB_;B^5B^5B]/B]/B[#BVBS�BG�B#�BPB  B�B�`B�#B��B��BƨB�wB�3B��B��B�Bt�Bw�Bw�Bw�Bu�Bp�Bs�Bv�Bv�Bu�Bs�Bo�B^5BB�B�B�mB�
B�}B��B��B�=Bt�Bs�Bx�BhsB[#B\)BO�BP�BffB~�B�bB�PB�1B�7B�uB�uB�7B{�Bz�Br�Be`BC�BF�BH�B49B$�B�BoB  B
�B
�TB
�;B
�B
��B
�LB
�uB
� B
t�B
cTB
XB
T�B
Q�B
Q�B
I�B
?}B
7LB
)�B
�B

=B
B	��B	�B	�BB	��B	��B	ĜB	�}B	�^B	�?B	�B	��B	��B	��B	��B	�uB	�DB	{�B	`BB	F�B	5?B	#�B	oB	JB		7B	B��B��B��B��B��B��B�B�B�B�B�`B�;B�)B�/B�5B�`B�`B�BB�/B�B��B��B��B��B��B��BǮBB��B��B��B��B�wB�qB�dB�RB�LB�FB�?B�-B�B�B��B��B��B��B��B��B��B��B��B�{B�oB�hB�\B�PB�=B�1B�%B�B�B�B{�Bw�Bu�Bs�Bq�Bo�Bm�Bk�BjBgmBhsBjBjBgmBdZBaHB]/BYBVBVBS�BQ�BO�BO�BN�BM�BG�BA�B@�B?}B?}B>wB=qB<jB9XB7LB6FB5?B33B2-B2-B1'B0!B-B,B+B+B)�B&�B#�B�B�B�B�B�B�B�B�B{BuBhBVBPBJBDBDB
=B
=B1B+B+B+B%BBBBB  B��B��B  BB  B��B��B��B  B��B��B��B  BBB+B+B1B1B
=B
=B
=B
=B	7B
=BDBPBVBVB\B\BbBbBbBhBhBbBbBuBoB�B�B�B�B�B�B�B'�B7LBH�BL�BM�BM�BL�BW
BYBZB[#B^5B_;B_;B_;B`BB`BBaHBdZBe`BffBgmBhsBhsBhsBl�Bo�Bo�Bo�Br�Bv�By�B{�B|�B�B�B�B�B�B�B�%B�=B�VB�oB��B��B��B��B��B��B��B�B�'B�-B�'B�B�RB�jB�qB��BĜBĜBĜBɺBɺBȴBȴBɺBɺBɺB��B��B��B��B��B�B�/B�)B�)B�/B�5B�HB�NB�TB�`B�fB�sB�B�B��B��B��B��B��B��B��B��B��B��B��B	B	%B	+B	1B	
=B	DB	JB	\B	bB	�B	�B	�B	�B	�B	�B	 �B	$�B	(�B	+B	-B	/B	33B	5?B	6FB	<jB	A�B	G�B	H�B	I�B	L�B	L�B	N�B	P�B	R�B	R�B	R�B	VB	XB	YB	YB	ZB	_;B	bNB	dZB	gmB	iyB	k�B	m�B	n�B	n�B	p�B	t�B	v�B	y�B	z�B	{�B	�B	�B	�%B	�1B	�1B	�7B	�DB	�JB	�PB	�PB	�\B	�yB	�jB	֡B	�QB	�LB
�B
�B
!�B
,=B
2�B
:�B
@iB
F�B
N<B
UB
Z�B
_�B
c�B
i_B
p�B
v1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 B_"B`*B`*B`'B`'B`*B`*B`*B`*B`*B`*B`,B`*B`*B`'B`,B`*B_#B^ B^ B]B]B[
BU�BS�BG�B#�B8B��B�B�DB�
B��BͻBƏB�aB�B��B��B��Bt�Bw�Bw�Bw�Bu�Bp�Bs�Bv�Bv�Bu�Bs�Bo�B^BBwBxB�UB��B�dB��B��B�%Bt�Bs�Bx�Bh[B[B\BO�BP�BfLB~�B�EB�6B�B�B�^B�^B�B{�Bz�Br�BeEBCzBF�BH�B4B$�B�BTB
��B
�sB
�7B
�B
��B
͹B
�1B
�YB
�B
t�B
c7B
W�B
T�B
Q�B
Q�B
I�B
?bB
70B
)�B
rB

!B
�B	��B	�nB	�&B	��B	ʥB	�B	�aB	�AB	�"B	��B	��B	��B	��B	�wB	�WB	�)B	{�B	`#B	F�B	5!B	#�B	RB	,B		B	�B��B��B��B��B��B��B�B��B�B�gB�AB�B�
B�B�B�BB�CB�"B�B��B��B��B��B��B��B̰BǏB�qB�iB�iB�cB�fB�XB�SB�EB�4B�-B�(B�!B�B��B��B��B��B��B��B��B��B��B�nB�bB�[B�MB�HB�?B�1B�B�B�B��B��B��B{�Bw�Bu�Bs�Bq�BoBmoBkeBj\BgMBhQBj`Bj`BgNBd:Ba(B]BX�BU�BU�BS�BQ�BO�BO�BN�BM�BG�BAkB@dB?[B?]B>VB=OB<IB99B7,B6&B5!B3B2
B2B1B0B,�B+�B*�B*�B)�B&�B#�B�B�B�B�BxBnBdBbBXBSBFB4B/B*B$B$B
B
BB	BB	BB�B�B �B�B��B��B��B��B �B��B��B��B��B��B��B��B��B��B�B�BBBBB
B
B
B
B	B
B B-B3B6B:B:B?B@B>BEBGB=B>BQBKBdBlBvB�B�B�B�B'�B7+BH�BL�BM�BM�BL�BV�BX�BY�BZ�B^B_B_B_B`!B` Ba&Bd7Be=BfCBgIBhPBhQBhQBliBozBo{BozBr�Bv�By�B{�B|�B��B��B��B��B��B��B�B�B�4B�LB�jB��B��B��B��B��B��B��B�B�	B�B��B�2B�EB�NB�]B�wB�vB�wBɘBɔBȐBȏBəBɚBɔBʝB̨BͱB��B��B��B�B�B�B�
B�B�%B�+B�1B�?B�CB�QB�oB��B��B��B��B��B��B��B��B��B��B��B��B	�B	B	B	B	
B	 B	&B	9B	@B	^B	qB	~B	�B	�B	�B	 �B	$�B	(�B	*�B	,�B	.�B	3B	5B	6"B	<DB	AfB	G�B	H�B	I�B	L�B	L�B	N�B	P�B	R�B	R�B	R�B	U�B	W�B	X�B	X�B	Y�B	_B	b*B	d7B	gKB	iVB	k`B	mmB	ntB	nuB	p�B	t�B	v�B	y�B	z�B	{�B	��B	��B	� B	�B	�B	�B	�B	�&B	�,B	�+G�O�B	�VB	�GB	�}B	�.B	�(B
�B
�B
!�B
,B
2�B
:�B
@EB
F�B
NB
T�B
ZbB
_�B
ceB
i<B
p�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.27 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201906040952592019060409525920190604095259  AO  ARCAADJP                                                                    20181121041519    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041519  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041519  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604095259  IP                  G�O�G�O�G�O�                