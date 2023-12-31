CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:25Z AOML 3.0 creation; 2016-06-01T00:08:11Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230825  20160531170812  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               'A   AO  4055_7112_039                   2C  D   APEX                            5374                            041511                          846 @֎�� ?�1   @֎�eB��@:F$�/��cH�n��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    'A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�3D��D�P D�y�D�ٚD���D�<�D�y�D�ɚD�3D�C3D�i�D�� D�3D�@ D�vfD�ٚD�fD�I�D� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�
=@ȣ�AQ�A$Q�ADQ�AdQ�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B	{B{B{B!{B){B1{B9{BA{BI{BQ{BY{Ba{Bi{Bq{By{B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=BĊ=BȊ=B̊=BЊ=BԊ=B؊=B܊=B��=B�=B�=B�=B��=B�=B��=B��=C ECECECECEC
ECECECECECECECECECECEC EC"EC$EC&EC(EC*EC,EC.EC0EC2EC4EC6EC8EC:EC<EC>EC@ECBECDECFECHECJECLECNECPECRECTECVECXECZEC\EC^EC`ECbECdECfEChECjEClECnECpECrECtECvECxECzEC|EC~EC�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�D HD �HDHD�HDHD��DHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt��Dy�{D�qD�X�D��>D��>D�qD�EqD��>D��>D��D�K�D�r>D�ؤD��D�H�D�
D��>D�
D�R>D�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��mA��mA��A���A���A���A���A���A���A���A���A�  A�  A���A���A�A�A�  A���A�  A�  A�  A���A�A�A�%A�
=A�JA�JA�JA�
=A�1A�1A���A���A���A�  A�%A���A���A�-A��uA��A��A�E�A��TA��A��\A��A�$�A��yA��9A�%A�ZA�oA���A�A��FA��A�M�A��A���A���A� �A�A��wA�$�A�1'A��A�33A�`BA�%A��mA���A�E�A�hsA���A��A�dZA�l�A�oA��A�Q�A���A���A�n�A��A��HA��A�1A�7LA�|�A�|�A�A�p�A��A�%A���A�A�A}�#A|��A{�mAz��Ay��Aw��Au�At9XAs33Ap�Ak�Ag��Ae�Ac�-Ab��Aa��A^��A]�A]�A]%A[VAY�mAW�^AT��AS�AQAOoAM��AM;dALbNAIƨAH��AG/AEdZAD-AC�
ACC�AA�FA@��A@5?A@1A?��A?�;A?��A?��A=�^A<VA;��A:�A9�A85?A7�A7%A6r�A5��A5;dA4�RA3�mA2�`A2=qA1��A1K�A0�9A/ƨA/oA.�9A.$�A-�FA,��A+��A*-A)��A)&�A(JA'�A'dZA&jA%�A$ĜA#��A#XA"��A"��A"bA!
=A��A�`AM�AA�A��A�A33A�AQ�A��A��AƨA�A�A33A�DA��A��A;dA5?A|�Av�A��AC�A&�AoA�mAAJA
z�A	�A�A�A\)AȴA�;A�AA7LAz�AA �@��
@���@���@���@��T@���@��@��@�\)@��@�^@�1@�hs@�b@�C�@�n�@�7L@�1@��@��@�j@�b@�t�@�@�  @�E�@�A�@��@�5?@ؼj@��@���@�A�@Ӆ@�\)@�33@�+@�M�@�x�@θR@�A�@�o@ɉ7@ȃ@� �@�b@��;@ǝ�@�t�@�33@��y@�-@Ł@�bN@�"�@��H@\@��^@���@�9X@��@��@���@��@�t�@���@�-@�O�@��D@��@�@��@�G�@���@�bN@���@�5?@��@�K�@�"�@��\@��@�p�@���@�A�@�|�@��y@��@�I�@�1'@��@�+@���@�E�@�{@��h@�&�@��@���@�A�@��@�K�@��@�`B@���@�z�@�(�@��
@�;d@��\@�{@���@��@�r�@��w@�K�@�+@��y@��R@�ff@��@���@�V@��@��D@�b@�ƨ@��@�@���@�^5@�{@��@��-@�hs@�G�@�7L@��@���@���@���@�I�@�b@��@�S�@���@��!@���@��+@�ff@�-@��h@��/@� �@���@��
@���@�C�@���@��+@�M�@��T@��^@��@���@��9@��D@�j@�A�@��@�S�@��@���@���@�^5@��@��@�%@���@�Ĝ@���@�j@�Q�@�9X@�b@���@��@�~�@�$�@���@��@��^@��7@�hs@�G�@�/@�V@���@���@��@�j@�I�@�  @��@�|�@�l�@�C�@�"�@���@��H@��@���@�E�@�$�@���@�O�@���@���@���@�z�@�(�@��@�  @�;@�@;d@~ȴ@~��@~V@~{@~@}�T@}��@}�@|�j@|j@|�@{dZ@{"�@z��@z^5@z=q@zJ@y��@y�7@yX@y�@x�9@x1'@x  @wl�@vff@u�T@up�@u`B@u?}@t�@tZ@s��@sC�@r�H@p  @h�9@a��@Z��@So@M�@H�`@A�#@=�@7+@0�`@*��@%�@!7L@/@hs@�@ �@�@K�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��mA��mA��A���A���A���A���A���A���A���A���A�  A�  A���A���A�A�A�  A���A�  A�  A�  A���A�A�A�%A�
=A�JA�JA�JA�
=A�1A�1A���A���A���A�  A�%A���A���A�-A��uA��A��A�E�A��TA��A��\A��A�$�A��yA��9A�%A�ZA�oA���A�A��FA��A�M�A��A���A���A� �A�A��wA�$�A�1'A��A�33A�`BA�%A��mA���A�E�A�hsA���A��A�dZA�l�A�oA��A�Q�A���A���A�n�A��A��HA��A�1A�7LA�|�A�|�A�A�p�A��A�%A���A�A�A}�#A|��A{�mAz��Ay��Aw��Au�At9XAs33Ap�Ak�Ag��Ae�Ac�-Ab��Aa��A^��A]�A]�A]%A[VAY�mAW�^AT��AS�AQAOoAM��AM;dALbNAIƨAH��AG/AEdZAD-AC�
ACC�AA�FA@��A@5?A@1A?��A?�;A?��A?��A=�^A<VA;��A:�A9�A85?A7�A7%A6r�A5��A5;dA4�RA3�mA2�`A2=qA1��A1K�A0�9A/ƨA/oA.�9A.$�A-�FA,��A+��A*-A)��A)&�A(JA'�A'dZA&jA%�A$ĜA#��A#XA"��A"��A"bA!
=A��A�`AM�AA�A��A�A33A�AQ�A��A��AƨA�A�A33A�DA��A��A;dA5?A|�Av�A��AC�A&�AoA�mAAJA
z�A	�A�A�A\)AȴA�;A�AA7LAz�AA �@��
@���@���@���@��T@���@��@��@�\)@��@�^@�1@�hs@�b@�C�@�n�@�7L@�1@��@��@�j@�b@�t�@�@�  @�E�@�A�@��@�5?@ؼj@��@���@�A�@Ӆ@�\)@�33@�+@�M�@�x�@θR@�A�@�o@ɉ7@ȃ@� �@�b@��;@ǝ�@�t�@�33@��y@�-@Ł@�bN@�"�@��H@\@��^@���@�9X@��@��@���@��@�t�@���@�-@�O�@��D@��@�@��@�G�@���@�bN@���@�5?@��@�K�@�"�@��\@��@�p�@���@�A�@�|�@��y@��@�I�@�1'@��@�+@���@�E�@�{@��h@�&�@��@���@�A�@��@�K�@��@�`B@���@�z�@�(�@��
@�;d@��\@�{@���@��@�r�@��w@�K�@�+@��y@��R@�ff@��@���@�V@��@��D@�b@�ƨ@��@�@���@�^5@�{@��@��-@�hs@�G�@�7L@��@���@���@���@�I�@�b@��@�S�@���@��!@���@��+@�ff@�-@��h@��/@� �@���@��
@���@�C�@���@��+@�M�@��T@��^@��@���@��9@��D@�j@�A�@��@�S�@��@���@���@�^5@��@��@�%@���@�Ĝ@���@�j@�Q�@�9X@�b@���@��@�~�@�$�@���@��@��^@��7@�hs@�G�@�/@�V@���@���@��@�j@�I�@�  @��@�|�@�l�@�C�@�"�@���@��H@��@���@�E�@�$�@���@�O�@���@���@���@�z�@�(�@��@�  @�;@�@;d@~ȴ@~��@~V@~{@~@}�T@}��@}�@|�j@|j@|�@{dZ@{"�@z��@z^5@z=q@zJ@y��@y�7@yX@y�@x�9@x1'@x  @wl�@vff@u�T@up�@u`B@u?}@t�@tZ@s��@sC�@r�H@p  @h�9@a��@Z��@So@M�@H�`@A�#@=�@7+@0�`@*��@%�@!7L@/@hs@�@ �@�@K�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BɺBB�-B��B|�Bu�Br�BjBcTB]/BB�B<jB7LBbB��B�BB��BȴB�LB�B�B��B��B��B�Bl�BQ�B49B!�BB�mB��B��BɺB�FB�B��B�\B~�Bp�BhsBT�BJ�B@�B%�B�B�BhBB
�B
�B
��B
�dB
�B
��B
�+B
z�B
q�B
_;B
:^B
,B
&�B
�B
uB
DB
B	��B	�ZB	��B	ǮB	�jB	��B	z�B	bNB	T�B	F�B	?}B	7LB	%�B	"�B	$�B	 �B	uB		7B��B�B�HB�
B��B��B��BɺBŢB��B�qB�dB�XB�LB�FB�?B�?B�FB�?B�?B�9B�3B�!B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�JB�%B�B�B�B�B~�B{�Bx�Bt�Bp�Bn�Bl�BjBffBbNB^5B[#B^5B`BB^5B[#BZBXBW
BVBS�BP�BM�BI�BF�BC�BA�B?}B?}B?}B=qB;dB:^B9XB8RB6FB33B0!B,B&�B!�B�B�B�B{B{BoBbB\BVBJBDB
=B	7B	7B1B+B%B+B1B1B+B+B%B+B1B1B1B1B+B+B+B+B+B%B%B+B+B1B	7B	7B
=B
=B
=BPBPBVBVBPBPBDBPBoB{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B!�B#�B%�B'�B)�B)�B,B-B.B0!B2-B49B49B49B5?B8RB=qBA�BA�BA�BC�BE�BF�BH�BJ�BK�BQ�BT�BT�BVB\)B_;BbNBcTBe`BgmBgmBiyBjBjBl�Bt�Bw�Bz�B|�B}�B~�B�B�%B�1B�JB�VB�oB��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�3B�9B�?B�FB�LB�XB�^B�dB�dB�jB�jB�wB�wB��BÖBŢBǮB��B��B��B��B��B��B��B�B�#B�)B�5B�;B�TB�`B�mB�yB�B�B�B�B��B��B��B��B��B	  B	B	B	%B	1B	DB	uB	{B	{B	�B	�B	�B	�B	�B	�B	�B	%�B	)�B	.B	/B	0!B	2-B	49B	5?B	6FB	7LB	8RB	9XB	;dB	<jB	=qB	>wB	B�B	G�B	J�B	K�B	L�B	N�B	O�B	P�B	Q�B	T�B	W
B	XB	]/B	`BB	bNB	cTB	e`B	gmB	jB	k�B	k�B	l�B	o�B	s�B	v�B	v�B	x�B	z�B	z�B	{�B	{�B	~�B	�B	�B	�B	�B	�1B	�=B	�=B	�JB	�bB	�uB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�RB	��B	�yB	��B
DB
�B
�B
)�B
1'B
9XB
A�B
H�B
N�B
S�B
ZB
`BB
e`B
jB
o�B
s�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��BͿB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B̾B̽B̽B̽B̽BɩB�}B�B��B|�Bu�Br�BjjBcDB]BBuB<SB79BNB��B�)B��BȞB�2B�B��B��B��B�B��BlqBQ�B4 B!�BB�SB��B˭BɠB�+B��B��B�BB~�Bp�BhYBT�BJ�B@kB%�B�B�BNBB
�B
�B
˯B
�KB
��B
��B
�B
z�B
q�B
_$B
:IB
+�B
&�B
�B
bB
/B
�B	��B	�HB	��B	ǛB	�XB	��B	z�B	b?B	T�B	F�B	?qB	7@B	%�B	"�B	$�B	 �B	gB		)B��B�vB�=B��B��B��B��BɰBŘB�B�eB�[B�MB�EB�=B�6B�7B�;B�5B�6B�/B�)B�B�B�B��B��B��B��B��B��B��B��B��B��B�B�~B�~B�xB�vB�vB��B��B��B��B�pB�`B�BB�B�B�B�B��B~�B{�Bx�Bt�Bp�Bn�Bl�BjvBf`BbFB^,B[B^/B`<B^,B[BZBXBWBU�BS�BP�BM�BI�BF�BC�BA�B?vB?tB?uB=kB;\B:UB9RB8KB6?B3-B0 B,B&�B!�B�B�B�BZBXBMB?B:B5B)B#B
B	B	BBBBBBB&B	BBBBBBBB$B
BB#BBB%B&B)B	B	B
B
B
6B.B.BLB4B-BGB=B+BKBsB�B�B�B�BpB�B�B�B�B�B�B�B�B�B�B�B�B �B!�B!�B#�B%�B'�B)�B)�B, B-B.
B0B2"B4/B4/B4.B52B8FB=hBA~BA}BA~BC�BE�BF�BH�BJ�BK�BQ�BT�BT�BU�B\B_0Bb@BcHBeRBg]Bg^BikBjrBjqBl~Bt�Bw�Bz�B|�B}�B~�B�B�B�!B�<B�FB�aB�vB��B��B��B��B��B��B��B��B��B��B��B�B�B�"B�'B�-B�3B�:B�CB�KB�PB�QB�WB�XB�dB�eB�wBÃBŏBǚB˴B̹B̺B̹B��B��B��B��B�B�B�"B�'B�AB�LB�[B�gB�yB�B�B�B��B��B��B��B��B��B	B		B	B	B	-B	`B	fB	fB	qB	qB	�B	�B	�B	�B	�B	%�B	)�B	-�B	/B	0B	2B	4$B	5(B	6-B	74B	88B	9AB	;LB	<TB	=XB	>_B	BxB	G�B	J�B	K�B	L�B	N�B	O�B	P�B	Q�B	T�B	V�B	W�B	]B	`)B	b6B	c;B	eHB	gTB	jgB	knB	klB	lqB	o�B	s�B	v�B	v�B	x�B	z�B	z�B	{�B	{�B	~�B	��B	��B	��B	�B	�B	�$B	�"B	�/B	�HB	�ZB	�ZB	�ZB	�nB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�8B	��B	�[B	��B
'B
bB
�B
)�B
1B
9:B
AiB
H�B
N�B
S�B
Y�B
`"B
eAB
j^B
o~B
s�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.27 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708122016053117081220160531170812  AO  ARCAADJP                                                                    20140721230825    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230825  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230825  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170812  IP                  G�O�G�O�G�O�                