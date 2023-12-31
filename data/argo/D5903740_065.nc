CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:39Z AOML 3.0 creation; 2016-06-01T00:08:16Z UW 3.1 conversion     
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
_FillValue                  0  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �L   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �T   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20140721230839  20160531170816  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               AA   AO  4055_7112_065                   2C  D   APEX                            5374                            041511                          846 @��R`
1   @��R�l��@:��t�j�d$���S�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    AA   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv�Cx�Cz�C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy�3D�fD�FfD��fD�� D��D�FfD��fD��fD�fD�<�D�|�D��fD�fD�C3Dډ�D�� D��fD�L�D�|�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @J�H@���@ȣ�AQ�A$Q�ADQ�AdQ�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B	{B{B{B!{B){B1{B9{BA{BI{BQ{BY{Ba{Bi{Bq{By{B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=BĊ=BȊ=B̊=BЊ=BԊ=B؊=B܊=B��=B�=B�=B�=B��=B�=B��=B��=C ECECECECEC
ECECECECECECECECECECEC EC"EC$EC&EC(EC*EC,EC.EC0EC2EC4EC6EC8EC:EC<EC>EC@ECBECDECFECHECJECLECNECPECRECTECVECXECZEC\EC^EC`ECbECdECfEChECjEClECnECp^�CrECtECv^�Cx^�Cz^�C|EC~EC�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDt��Dy�{D�
D�O
D��
D���D�%qD�O
D��
D��
D�
D�EqD��qD��
D�
D�K�Dڒ>D�ȤD��
D�UqD�qD��>1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��hA�jA�9XA��A��+A�I�A��A���A�|�A�C�A�\)A�
=A���A�z�A���A���A��wA���A��A�~�A��A���A�"�A�A�O�A�I�A�ƨA�/A�
=A�ƨA� �A�(�A�`BA��mA�{A��;A�~�A��HA�S�A���A���A��A��yA��`A�ȴA�A���A�l�A��DA�t�A�n�A��A�\)A���A�O�A��mA�n�A���A��TA�  A�
=A��A�A�O�A��`A��
A���A�M�A�9XA���A�XA���A�33A��DA���A�=qA~=qAz(�Ay�7Ax�RAx1Aw?}AtVAs�ArbAq+Ao��Ao
=AnbAl�yAk��Ak
=Aj�AjE�Ai?}AioAhz�AgAfĜAex�AdȴAc\)AaO�A`ȴA_ƨA^v�A\�+AV�AS|�AR�+APz�AN�\AK��AI�AH�AGx�AE��AC�AB{AAO�AA33AA/AA/AA+A@��A@  A>~�A<�A;�-A:�jA:1A7��A5��A3�mA3hsA2�RA2 �A1�;A1��A0A.VA-��A,Q�A,{A,JA+��A*��A*�\A)��A)t�A)�A(��A(��A)%A(��A'��A&��A%�A$��A$r�A#��A#`BA"�A"E�A"�A!�A!�A!A ��A 5?A��A��A�HAƨA��A^5A�PAG�AbNA&�Az�A-A  Ax�A��A(�A�AJAA`BAoA�A��A�#A\)AȴA$�AdZAZA��A
ȴA	\)Al�A5?At�A�A7LA �@��P@��@�Ĝ@�@�1'@�C�@��+@�M�@��#@���@�j@�C�@���@��@�z�@�5?@�&�@띲@�;d@�n�@�@�V@�@��@�1@ߥ�@���@�`B@�v�@١�@�Ĝ@�t�@�~�@��/@�X@�+@�~�@̣�@ǍP@Ƨ�@�5?@�p�@ģ�@�I�@�  @î@�K�@��y@+@��@�O�@��@��7@�j@�K�@�x�@��/@��u@�+@�-@�O�@��u@���@�\)@�+@���@��H@���@�M�@�$�@��@�@���@���@��@��#@���@��D@�I�@� �@�b@���@���@���@�33@�V@��@�x�@�G�@�V@��D@�I�@��@��F@�o@�{@�X@���@�Z@�\)@�5?@�?}@���@�9X@�ƨ@�\)@��@�@���@�~�@��@��@��#@���@�@��-@���@��7@�X@���@�Q�@�S�@���@�$�@���@�hs@��@��D@�b@��;@���@�t�@�"�@��H@���@�~�@�ff@�M�@���@�G�@���@�Ĝ@��9@���@���@�  @��y@��+@��7@�G�@��j@�b@�l�@�+@���@�5?@��#@��-@��-@�x�@�/@��/@�I�@��@��F@��@�;d@��@�V@��@�x�@�&�@��`@���@��j@��@�r�@��@��m@��;@��P@�"�@���@��+@�v�@�M�@�J@���@��^@���@��h@��@�hs@�`B@�`B@�X@��@��9@��@� �@���@��@�;d@��y@���@��R@�v�@�V@�V@�V@�5?@��7@�&�@��`@���@��u@��@�r�@�bN@�Z@�Q�@�A�@�(�@�@�P@;d@
=@~��@~�y@~��@~ff@}�@}��@|�@|��@|(�@{��@{t�@z��@z^5@z�@y�@y��@y��@xr�@w�P@v��@v{@u@uO�@t�@t�D@nE�@f�y@_��@[t�@Xb@T��@N��@Fv�@?�w@:=q@3��@,�j@&�+@#@   @�@v�@��@p�@b@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��hA�jA�9XA��A��+A�I�A��A���A�|�A�C�A�\)A�
=A���A�z�A���A���A��wA���A��A�~�A��A���A�"�A�A�O�A�I�A�ƨA�/A�
=A�ƨA� �A�(�A�`BA��mA�{A��;A�~�A��HA�S�A���A���A��A��yA��`A�ȴA�A���A�l�A��DA�t�A�n�A��A�\)A���A�O�A��mA�n�A���A��TA�  A�
=A��A�A�O�A��`A��
A���A�M�A�9XA���A�XA���A�33A��DA���A�=qA~=qAz(�Ay�7Ax�RAx1Aw?}AtVAs�ArbAq+Ao��Ao
=AnbAl�yAk��Ak
=Aj�AjE�Ai?}AioAhz�AgAfĜAex�AdȴAc\)AaO�A`ȴA_ƨA^v�A\�+AV�AS|�AR�+APz�AN�\AK��AI�AH�AGx�AE��AC�AB{AAO�AA33AA/AA/AA+A@��A@  A>~�A<�A;�-A:�jA:1A7��A5��A3�mA3hsA2�RA2 �A1�;A1��A0A.VA-��A,Q�A,{A,JA+��A*��A*�\A)��A)t�A)�A(��A(��A)%A(��A'��A&��A%�A$��A$r�A#��A#`BA"�A"E�A"�A!�A!�A!A ��A 5?A��A��A�HAƨA��A^5A�PAG�AbNA&�Az�A-A  Ax�A��A(�A�AJAA`BAoA�A��A�#A\)AȴA$�AdZAZA��A
ȴA	\)Al�A5?At�A�A7LA �@��P@��@�Ĝ@�@�1'@�C�@��+@�M�@��#@���@�j@�C�@���@��@�z�@�5?@�&�@띲@�;d@�n�@�@�V@�@��@�1@ߥ�@���@�`B@�v�@١�@�Ĝ@�t�@�~�@��/@�X@�+@�~�@̣�@ǍP@Ƨ�@�5?@�p�@ģ�@�I�@�  @î@�K�@��y@+@��@�O�@��@��7@�j@�K�@�x�@��/@��u@�+@�-@�O�@��u@���@�\)@�+@���@��H@���@�M�@�$�@��@�@���@���@��@��#@���@��D@�I�@� �@�b@���@���@���@�33@�V@��@�x�@�G�@�V@��D@�I�@��@��F@�o@�{@�X@���@�Z@�\)@�5?@�?}@���@�9X@�ƨ@�\)@��@�@���@�~�@��@��@��#@���@�@��-@���@��7@�X@���@�Q�@�S�@���@�$�@���@�hs@��@��D@�b@��;@���@�t�@�"�@��H@���@�~�@�ff@�M�@���@�G�@���@�Ĝ@��9@���@���@�  @��y@��+@��7@�G�@��j@�b@�l�@�+@���@�5?@��#@��-@��-@�x�@�/@��/@�I�@��@��F@��@�;d@��@�V@��@�x�@�&�@��`@���@��j@��@�r�@��@��m@��;@��P@�"�@���@��+@�v�@�M�@�J@���@��^@���@��h@��@�hs@�`B@�`B@�X@��@��9@��@� �@���@��@�;d@��y@���@��R@�v�@�V@�V@�V@�5?@��7@�&�@��`@���@��u@��@�r�@�bN@�Z@�Q�@�A�@�(�@�@�P@;d@
=@~��@~�y@~��@~ff@}�@}��@|�@|��@|(�@{��@{t�@z��@z^5@z�@y�@y��@y��@xr�@w�P@v��@v{@u@uO�@t�@t�D@nE�@f�y@_��@[t�@Xb@T��@N��@Fv�@?�w@:=q@3��@,�j@&�+@#@   @�@v�@��@p�@b@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BoB
=B�B�B�dB��B�=B�Bw�Bq�B^5BD�B7LB,B�B1B��B�B�sB�ZB�/B��BȴBǮBÖB�qB�^B�9B�B��B��B��B�oB{�Be`BT�BP�BE�B7LB5?B5?B49B0!B.B(�B�B%B��B�fB�;B��BƨB��B�jB�9B�B��B�+BgmBM�B<jB2-B'�BuBB
�ZB
��B
�!B
�PB
v�B
n�B
bNB
J�B
>wB
,B
uB
PB
+B
B	��B	�B	�TB	�)B	��B	��B	ǮB	B	�jB	�FB	�3B	�!B	�B	��B	��B	��B	��B	��B	�oB	�PB	�B	z�B	v�B	p�B	ffB	T�B	.B	�B	�B	JB	
=B	B��B��B��B�B�B�mB�sB�sB�sB�mB�mB�fB�NB�;B�#B�
B��B��BȴBÖB��B�wB�jB�dB�^B�LB�-B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�\B�PB�JB�JB�=B�=B�7B�1B�%B�B�B�B� B}�By�Bt�Bo�Bk�BiyBgmBdZB`BB^5B]/B\)BZBXBW
BW
BVBT�BS�BR�BQ�BP�BM�BL�BJ�BG�BD�B@�B;dB6FB1'B-B(�B%�B!�B�B�B�B�B�B�B�B�B�B�B{B{BuBoBuBuBuB�B�B�B{BuBhBhBbBbBbBbBVBPBJBJBJBPBJBDBVB\BVBPBuB{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B �B$�B&�B%�B)�B,B/B0!B33B49B49B5?B5?B6FB6FB7LB7LB7LB7LB9XB=qBC�BE�BG�BG�BH�BH�BH�BH�BI�BJ�BM�BO�BQ�BQ�BR�BT�BVBVBW
BYB]/B`BBaHBcTBgmBm�Bq�Bt�Bv�Bx�Bz�B{�B{�B|�B}�B� B�B�B�B�B�B�B�B�B�B�+B�VB�uB��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�!B�?B�LB�XB�^B�dB�dB�dB�wBǮBɺB��B��B�
B�#B�BB�HB�fB�yB�B�B�B�B�B�B��B��B��B��B	  B	B	+B	
=B	VB	hB	uB	{B	�B	�B	�B	�B	�B	�B	#�B	&�B	(�B	+B	+B	-B	/B	2-B	2-B	33B	49B	5?B	6FB	6FB	6FB	7LB	9XB	=qB	?}B	C�B	F�B	I�B	K�B	O�B	P�B	P�B	T�B	W
B	XB	YB	ZB	`BB	dZB	ffB	hsB	iyB	jB	jB	k�B	k�B	k�B	l�B	m�B	n�B	p�B	q�B	r�B	s�B	s�B	t�B	u�B	w�B	x�B	|�B	}�B	� B	�B	�B	�%B	�1B	�=B	�DB	�DB	�DB	�hB	�{B	��B	��B	��B	��B	��B	��B	�RB	��B	�fB	�B	��B
B
\B
�B
'�B
0!B
8RB
B�B
I�B
N�B
P�B
W
B
\)B
dZB
gmB
n�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BqBbB
/B�B��B�RB��B�+B�Bw�Bq�B^"BD�B77B+�B�BB��B�B�\B�BB�B��BȜBǕB�{B�YB�GB�#B��B��B��B��B�WB{�BeABT�BP�BE�B70B5%B5"B4!B0B-�B(�BjBB��B�LB�B��BƐB�oB�PB� B��B��B�BgWBM�B<QB2B'�B_B �B
�BB
̵B
�B
�9B
v�B
n�B
b6B
J�B
>bB
+�B
]B
>B
B
�B	��B	�sB	�@B	�B	��B	̺B	ǞB	�~B	�XB	�1B	�$B	�B	��B	��B	��B	��B	��B	��B	�_B	�AB	�B	z�B	v�B	p�B	fVB	T�B	.B	�B	�B	=B	
/B	B��B��B��B�B�uB�aB�hB�gB�gB�`B�`B�ZB�DB�/B�B��B��B��BȩBÊB�xB�lB�_B�YB�TB�BB�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�sB�_B�TB�FB�@B�?B�2B�2B�.B�)B�B�B�B��B�B}�By�Bt�Bo�Bk}BioBgeBdOB`;B^,B]'B\BZBX
BWBWBU�BT�BS�BR�BQ�BP�BM�BL�BJ�BG�BD�B@{B;^B6=B1 B-B(�B%�B!�B�B�B�B}B�BhB|B`B`BzBuBZBkBLBoBnBSBaBaBzBZBlBcBGB>B\B>BZB4B.B)B'BAB-B(B!B3BVB3BJBkBrBqBwB~B�B�B�B�B�B�B�B�B�B�B�B �B$�B&�B%�B)�B+�B/B0B3*B40B4.B53B55B6;B6<B7AB7BB7@B7?B9LB=gBC�BE�BG�BG�BH�BH�BH�BH�BI�BJ�BM�BO�BQ�BQ�BR�BT�BU�BU�BV�BYB] B`6Ba<BcFBgaBm�Bq�Bt�Bv�Bx�Bz�B{�B{�B|�B}�B�B��B��B��B��B��B��B��B�B�B�B�HB�fB�qB�}B��B��B��B��B��B��B��B��B��B�B�	B�B�B�/B�;B�FB�MB�TB�QB�QB�eBǙBɦB��B��B��B�B�/B�5B�SB�cB�zB�}B�}B�B�B�B��B��B��B��B��B	�B	B	
&B	AB	QB	^B	gB	kB	jB	{B	�B	�B	�B	#�B	&�B	(�B	*�B	*�B	,�B	/B	2B	2B	3B	4!B	5(B	6/B	6/B	60B	73B	9@B	=[B	?gB	C~B	F�B	I�B	K�B	O�B	P�B	P�B	T�B	V�B	W�B	Y B	ZB	`(B	dDB	fMB	hYB	iaB	jgB	jfB	knB	koB	klB	lrB	mzB	n~B	p�B	q�B	r�B	s�B	s�B	t�B	u�B	w�B	x�B	|�B	}�B	�B	��B	��B	�	B	�B	�%B	�*B	�*B	�*B	�LB	�dB	�mB	��B	��B	��B	��B	��B	�6B	��B	�IB	�B	��B
�B
>B
�B
'�B
0B
82B
BsB
I�B
N�B
P�B
V�B
\
B
d8B
gMB
nyB
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.27 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708162016053117081620160531170816  AO  ARCAADJP                                                                    20140721230839    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230839  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230839  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170816  IP                  G�O�G�O�G�O�                