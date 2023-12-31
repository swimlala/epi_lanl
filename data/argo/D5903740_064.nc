CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:38Z AOML 3.0 creation; 2016-06-01T00:08:16Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230838  20160531170816  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               @A   AO  4055_7112_064                   2C  D   APEX                            5374                            041511                          846 @�����?�1   @���xj/�@:����o�dj~��#1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    @A   A   A   @�ff@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL�fDM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� DhfDh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dly�Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDy�3D�  D�S3D�� D�� D���D�@ D��fD�� D���D�C3D�ffD��fD�	�D�<�D�p D���D�fD�<�D�y�D�s311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�
=@ȣ�A�RA$Q�ADQ�AdQ�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B	{B{B{B!{B){B1{B9{BA{BI{BQ{BY{Ba{Bi{Bq{By{B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=BĊ=BȊ=B̊=BЊ=BԊ=B؊=B܊=B��=B�=B�=B�=B��=B�=B��=B��=C ECECECECEC
ECECECECECECECECECECEC EC"EC$EC&EC(EC*EC,EC.EC0EC2EC4EC6EC8EC:EC<EC>EC@ECBECDECFECHECJECLECNECPECRECTECVECXECZEC\EC^EC`ECbECdECfEChECjEClECnECpECrECtECvECxECzEC|EC~EC�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD��DHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL��DMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDh�Dh�HDiHDi�HDjHDj�HDkHDk�HDlHDl��DmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDtw�Dy�{D��D�[�D���D�ؤD�qD�H�D��
D�ؤD�qD�K�D�o
D��
D�>D�EqD�x�D��qD�
D�EqD�>D�{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�7LA�7LA�9XA�9XA�9XA�9XA�;dA�9XA�;dA�=qA�=qA�=qA�=qA�9XA�;dA�7LA�9XA�7LA�9XA�;dA�=qA�;dA�;dA�;dA�9XA�;dA�;dA�33A�1'A�-A�+A�-A�-A�-A�-A�(�A�&�A� �A� �A��A��A�VA���A�A��A���A���A���A��7A�^5A�(�A��FA��A�O�A�r�A���A��A��A��A��A�jA�t�A�C�A�?}A�G�A���A���A�-A���A��HA���A�VA�bNA��A�dZA���A��A���A��DA�z�A�`BA�A�hsA�A��A���A�S�A�&�A�K�A��A�jA�dZA��7A�C�A��A��!A�XA�&�A��TA���A��^A�(�A��A�v�A�K�A�A}C�A{t�Az�\Ay�Ay�Av�yAu;dAs��Ap�Ak�Ait�Ah�`Ah9XAgAe�7Abv�AaG�A`r�A_�TA_33A]�^A\�uA[��A[VAYG�AWG�AT��AT~�AT1AQ�mAO��AM��AMG�AM33AMVAL�AK�AJ �AH�HAG��AG"�AD�ACXAB��AB=qAA�PA@�A>9XA<1'A;K�A9��A8VA7K�A6��A5�A5�A4ȴA3�mA3XA2�RA1��A1��A2  A1�PA0bNA/ƨA.ȴA-"�A*JA(�uA($�A'�A&��A$jA#p�A#A"��A"VA"1'A!�#A ��A -AG�AbNA�-Ap�A �A�FA\)AAQ�A�PA?}A�A�jAjA(�A��AK�A��A{A��AC�AVA�7Al�A&�A�RAE�AAVA
z�A
E�A
�A	�A	\)AM�A�;AbA��A��A�
A
=A M�A {@��P@�n�@�J@�{@�`B@�b@��@��#@�7L@��@���@��@�@�X@�@�l�@�7@�7L@�z�@�^5@���@��@�{@���@��#@�V@�!@�V@��@�"�@�ff@ܣ�@�t�@��@��;@֟�@�?}@ԣ�@�9X@�t�@�@с@�V@��/@�Z@�=q@ˮ@�$�@��;@Ɵ�@�V@�x�@��/@�(�@�l�@�x�@���@�t�@�p�@���@�$�@�7L@�bN@��m@�dZ@���@�n�@�@��@��@�X@��@��9@�Z@�1'@���@�33@��@���@�5?@��@��P@��@���@�@���@�n�@��@��@��9@�33@�E�@�p�@��@��@�r�@�b@���@���@���@��@�K�@�"�@��@��R@���@��@��@�hs@�X@��@��@���@��@�t�@�33@���@�V@���@���@�X@��@��@���@��
@��@�@��/@���@��j@���@��@�z�@�r�@�Z@�b@��@��
@�dZ@���@�5?@��@��-@�O�@�%@���@�n�@�5?@��@���@�p�@�?}@�%@��@�(�@�(�@��@�t�@�S�@�33@�@�ȴ@���@��\@�V@�$�@��#@��@�?}@��9@�z�@�Q�@�A�@�b@��@��m@��
@�ƨ@��F@���@��P@�\)@�33@�ȴ@�@���@���@��7@�p�@�/@��@��`@��`@��/@���@�A�@��;@��@��P@�t�@�+@�^5@��@��7@�?}@���@��9@�z�@�b@��w@��P@��@�t�@�\)@���@�ff@�J@���@���@��@�p�@�X@�?}@���@��j@��@��u@�j@�1'@��@�1@�  @��@�w@��@\)@�@~�@~��@~E�@}�T@}��@}�-@}��@}�@|��@|9X@|�@{�@{"�@z�!@zn�@yG�@x��@x�9@x�@xb@s�
@lz�@c�m@[@S��@O��@J-@C�F@:�@3��@.��@(�@%@!�@l�@hs@@�h@�F@
=@$�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�7LA�7LA�9XA�9XA�9XA�9XA�;dA�9XA�;dA�=qA�=qA�=qA�=qA�9XA�;dA�7LA�9XA�7LA�9XA�;dA�=qA�;dA�;dA�;dA�9XA�;dA�;dA�33A�1'A�-A�+A�-A�-A�-A�-A�(�A�&�A� �A� �A��A��A�VA���A�A��A���A���A���A��7A�^5A�(�A��FA��A�O�A�r�A���A��A��A��A��A�jA�t�A�C�A�?}A�G�A���A���A�-A���A��HA���A�VA�bNA��A�dZA���A��A���A��DA�z�A�`BA�A�hsA�A��A���A�S�A�&�A�K�A��A�jA�dZA��7A�C�A��A��!A�XA�&�A��TA���A��^A�(�A��A�v�A�K�A�A}C�A{t�Az�\Ay�Ay�Av�yAu;dAs��Ap�Ak�Ait�Ah�`Ah9XAgAe�7Abv�AaG�A`r�A_�TA_33A]�^A\�uA[��A[VAYG�AWG�AT��AT~�AT1AQ�mAO��AM��AMG�AM33AMVAL�AK�AJ �AH�HAG��AG"�AD�ACXAB��AB=qAA�PA@�A>9XA<1'A;K�A9��A8VA7K�A6��A5�A5�A4ȴA3�mA3XA2�RA1��A1��A2  A1�PA0bNA/ƨA.ȴA-"�A*JA(�uA($�A'�A&��A$jA#p�A#A"��A"VA"1'A!�#A ��A -AG�AbNA�-Ap�A �A�FA\)AAQ�A�PA?}A�A�jAjA(�A��AK�A��A{A��AC�AVA�7Al�A&�A�RAE�AAVA
z�A
E�A
�A	�A	\)AM�A�;AbA��A��A�
A
=A M�A {@��P@�n�@�J@�{@�`B@�b@��@��#@�7L@��@���@��@�@�X@�@�l�@�7@�7L@�z�@�^5@���@��@�{@���@��#@�V@�!@�V@��@�"�@�ff@ܣ�@�t�@��@��;@֟�@�?}@ԣ�@�9X@�t�@�@с@�V@��/@�Z@�=q@ˮ@�$�@��;@Ɵ�@�V@�x�@��/@�(�@�l�@�x�@���@�t�@�p�@���@�$�@�7L@�bN@��m@�dZ@���@�n�@�@��@��@�X@��@��9@�Z@�1'@���@�33@��@���@�5?@��@��P@��@���@�@���@�n�@��@��@��9@�33@�E�@�p�@��@��@�r�@�b@���@���@���@��@�K�@�"�@��@��R@���@��@��@�hs@�X@��@��@���@��@�t�@�33@���@�V@���@���@�X@��@��@���@��
@��@�@��/@���@��j@���@��@�z�@�r�@�Z@�b@��@��
@�dZ@���@�5?@��@��-@�O�@�%@���@�n�@�5?@��@���@�p�@�?}@�%@��@�(�@�(�@��@�t�@�S�@�33@�@�ȴ@���@��\@�V@�$�@��#@��@�?}@��9@�z�@�Q�@�A�@�b@��@��m@��
@�ƨ@��F@���@��P@�\)@�33@�ȴ@�@���@���@��7@�p�@�/@��@��`@��`@��/@���@�A�@��;@��@��P@�t�@�+@�^5@��@��7@�?}@���@��9@�z�@�b@��w@��P@��@�t�@�\)@���@�ff@�J@���@���@��@�p�@�X@�?}@���@��j@��@��u@�j@�1'@��@�1@�  @��@�w@��@\)@�@~�@~��@~E�@}�T@}��@}�-@}��@}�@|��@|9X@|�@{�@{"�@z�!@zn�@yG�@x��@x�9@x�@xb@s�
@lz�@c�m@[@S��@O��@J-@C�F@:�@3��@.��@(�@%@!�@l�@hs@@�h@�F@
=@$�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB&�B&�B&�B&�B&�B&�B&�B&�B&�B&�B&�B&�B&�B&�B&�B&�B%�B%�B%�B&�B&�B&�B&�B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B$�B$�B$�B$�B#�B"�B!�B�B�B1B�yB��BdZB5?BB�sB�/B�B��BÖB�^B��B��B��B�{B�7Bp�BXBI�B8RB/B �B�B�B�B
=B  B�B�;BɺB�qB�^B�B��B�+B�B|�Br�BffB[#BW
BR�BL�B?}B49B�BPB  B
��B
��B
�B
�sB
�NB
��B
�LB
��B
�oB
�+B
s�B
R�B
>wB
)�B
�B
{B
\B
%B	��B	�sB	�)B	��B	��B	��B	�oB	�PB	�B	w�B	ffB	`BB	ZB	W
B	Q�B	J�B	D�B	@�B	:^B	1'B	&�B	�B	�B	{B	DB	B��B��B��B��B��B�B�B�sB�`B�BB�B��B��B��B��BɺBÖB�}B�qB�dB�RB�FB�-B�!B�B�B�B�B�B�?B�RB�^B�XB�-B�B��B��B��B��B��B��B�oB�DB�+B�B�B�B�B�B~�B~�B}�B|�Bx�Bs�Bp�Bn�Bm�Bk�BiyBgmBe`BdZBdZBcTBaHB`BB^5B\)BW
BQ�BO�BL�BH�BD�BC�BB�BA�B?}B=qB<jB<jB;dB:^B7LB5?B2-B,B(�B%�B$�B"�B!�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�BuBhBbBbBbBVBPBPBPBPBPBJB
=B
=B
=B
=B
=B	7B1B+B	7B
=B
=BDBDB
=B
=BDBDBJBDBDBDBVBVBhB{BuB{B{B�B�B�B�B�B�B�B"�B#�B%�B&�B(�B)�B+B,B-B.B.B/B0!B1'B1'B1'B33B49B5?B5?B33B;dBA�BD�BG�BG�BG�BH�BH�BJ�BO�BT�BXBZB[#B[#B\)B]/B^5B^5B^5B_;B`BB`BBaHBffBgmBgmBhsBhsBiyBl�Bo�Bq�Br�Bt�Bw�Bz�B}�B� B�B�B�B�+B�JB�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�3B�?B�FB�XB�^BĜB��B��B��B��B�B�B�B�/B�BB�;B�BB�`B�fB�mB�sB�B�B�B�B�B�B��B��B��B��B��B	  B	B	B	B	B	B	B	B	B	%B	1B	DB	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	&�B	(�B	)�B	+B	-B	49B	8RB	:^B	=qB	>wB	@�B	B�B	E�B	G�B	I�B	I�B	I�B	J�B	P�B	T�B	XB	\)B	_;B	_;B	`BB	aHB	bNB	dZB	gmB	hsB	iyB	k�B	m�B	n�B	o�B	o�B	p�B	p�B	p�B	r�B	s�B	t�B	v�B	w�B	y�B	z�B	z�B	z�B	{�B	~�B	�B	�B	�B	�%B	�1B	�7B	�\B	�hB	�oB	�oB	��B	��B	��B	�#B	�B
B
PB
�B
!�B
/B
8RB
?}B
G�B
K�B
P�B
S�B
[#B
^5B
hsB
k�B
q�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B&�B&�B&�B&�B&�B&�B&�B&�B&�B&�B&�B&�B&�B&�B&�B&�B%�B%�B%�B&�B&�B&�B&�B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B$�B$�B$�B$�B#�B"�B!�B�B�B$B�kB��BdFB5+BB�^B�B��B��B�B�GB��B��B�nB�aB�Bp�BW�BI�B89B.�B �B�B�B�B
!B��B�B�BɢB�XB�FB��B�nB�B� B|�Br�BfKB[
BV�BR�BL�B?dB4B�B7B
��B
��B
��B
�}B
�ZB
�4B
ͺB
�7B
��B
�YB
�B
s�B
R�B
>bB
)�B
�B
hB
GB
B	��B	�_B	�B	�rB	��B	�xB	�]B	�@B	�B	w�B	fUB	`3B	ZB	V�B	Q�B	J�B	D�B	@vB	:QB	1B	&�B	�B	�B	nB	6B	 �B��B��B��B��B��B�B�B�gB�TB�7B�B��B��B��B��BɱBÌB�tB�eB�ZB�FB�<B�$B�B�
B�B��B��B�B�5B�IB�WB�LB�"B��B��B��B��B��B��B�|B�gB�;B�!B�B�B�B�	B��B~�B~�B}�B|�Bx�Bs�Bp�Bn�Bm�Bk~BioBgeBeWBdQBdPBcKBa@B`<B^-B\ BWBQ�BO�BL�BH�BD�BC�BB�BA�B?wB=PB<bB<bB;]B:WB7EB56B2'B,B(�B%�B$�B"�B!�B!�B �B�B�B�B�B�B�BBBxB�B�BfBnBGB@BCB@B3BJB.B.B0BHB)B
B
B
B
B
B	BB%B	0B
B
B:B=B
B
B!B#B(B<B#B!B3B5BFBvBjBtBXBxB^B�B�B�B�B�B"�B#�B%�B&�B(�B)�B*�B+�B-B.
B.B/B0B1B1B1B3)B4-B53B53B3'B;XBA|BD�BG�BG�BG�BH�BH�BJ�BO�BT�BXBZB[B[B\B]B^'B^'B^'B_/B`5B`6Ba<Bf[BgaBgbBhcBhfBijBl{Bo�Bq�Br�Bt�Bw�Bz�B}�B�B�B�
B�B�B�:B�kB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�/B�6B�DB�NBċB��B��B��B��B��B��B�B�B�0B�'B�/B�KB�RB�YB�^B�lB�uB�yB�B�B�B��B��B��B��B��B��B	�B	�B	�B	�B	B	B	B		B	B	B	.B	XB	nB	pB	pB	vB	�B	�B	�B	�B	�B	�B	#�B	&�B	(�B	)�B	*�B	,�B	4"B	8<B	:GB	=[B	>`B	@lB	BuB	E�B	G�B	I�B	I�B	I�B	J�B	P�B	T�B	W�B	\B	_"B	_"B	`(B	a.B	b5B	dBB	gTB	hYB	i_B	klB	myB	n�B	o�B	o�B	p�B	p�B	p�B	r�B	s�B	t�B	v�B	w�B	y�B	z�B	z�B	z�B	{�B	~�B	��B	��B	�B	�B	�B	�B	�BB	�MB	�TB	�UB	�hB	��B	�hB	�B	�B
�B
3B
mB
!�B
.�B
84B
?^B
G�B
K�B
P�B
S�B
[B
^B
hSB
keB
q�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.27 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708162016053117081620160531170816  AO  ARCAADJP                                                                    20140721230838    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230838  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230838  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170816  IP                  G�O�G�O�G�O�                