CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-09-28T02:15:22Z AOML 3.0 creation; 2016-06-01T00:08:26Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150928021522  20160531170827  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055_7112_128                   2C  D   APEX                            5374                            041511                          846 @�r�CQ�i1   @�r�v��&@:<�hr��d���m1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BI33BN  BW��B`  Bh  Bo��Bx  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZfDZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy&fD���D�P D���D��fD��D�I�D��fD���D��D�C3D�� DǼ�D��D�<�Dڀ D�fD�� D�L�D��D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@ȣ�AQ�A$Q�ADQ�AdQ�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B	{B{B{B!{B){B1{B9{BA{BJG�BO{BX�Ba{Bi{Bp�By{B��=B��=B��=B��=B��=B�W
B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=BĊ=BȊ=B̊=BЊ=BԊ=B؊=B܊=B��=B�=B�=B�=B��=B�=B��=B��=C ECECECECEC
ECECECECECECECECECECEC EC"EC$EC&EC(EC*EC,EC.EC0EC2EC4EC6EC8EC:EC<EC>EC@ECBECDECFECHECJECLECNECPECRECTECVECXECZEC\EC^EC`ECbECdECfEChECjEClECnECpECrECtECvECxECzEC|EC~EC�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZ�DZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt��Dy7�D�qD�X�D��qD��
D�">D�R>D��
D��qD�qD�K�D���D��qD�qD�EqDڈ�D�
D���D�UqD�qD��
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AׅAי�Aח�Aכ�Aם�Aכ�Aח�Aם�Aן�Aן�Aן�Aף�Aו�Aו�Aו�AדuAי�Aכ�Aכ�Aי�A�|�A�z�A��TA�1A��A�x�AìA���A��-A��!A��DA�hsA��DA�1A�33A��+A��A�jA���A�/A�$�A���A��A�l�A�
=A��!A�XA��mA���A���A�&�A���A��A�+A���A�|�A�{A��7A�`BA�\)A���A�&�A��HA���A�l�A�{A� �A��+A���A�/A���A���A�t�A�t�A��`A�XA�A���A�5?A���A�33A�^5A��A��DA�VA�
=A���A�v�A���A�S�A��A��A���A��A�ĜA��^A���A��DA�~�A�x�A�^5A�1'A�1'A}�A{�FA{�Ayl�Aw��Av �At~�As&�As�As�Ar�9Aq`BApr�An�`Am��AmG�Al�9Ak�Ai�hAhAe7LAd~�AdJAc��Ac33Aa��A`n�A_33A]��A\��A\��A\M�A[dZAZ(�AX�DAW�7AV(�AV1AUS�ATJAShsAR(�APn�AM�AK
=AI�TAH�`AG��AF�\AD�ACAAS�A?x�A>�A>5?A>  A=�#A=�A<bNA;�FA8�A8A7�;A7�-A7�A7l�A7G�A7�A6��A6  A5�A5K�A4�A3+A2�!A2�\A1oA.�A,(�A+dZA*�RA)��A(bNA(1A'"�A&9XA%ƨA%p�A$Q�A"�A!�A   A�A�TA+A��AA�A�FAl�A/A�AVA�A �A�FA9XA��A��AhsA��AS�AZA�A�hA1'A�AA��AVA
z�A	�wA	A�uAt�AG�A�A�/A1'A33A5?A  A�FA�Ar�A��A �jA M�@��@���@�M�@��D@�ff@�Ĝ@�$�@���@��@���@�@�
=@�\@�@�A�@�o@��y@���@�=q@���@�@�hs@⟾@�O�@�@�(�@ۥ�@�+@�G�@��@�n�@�hs@ҸR@Ѓ@��@�`B@���@��@�1@�p�@ě�@�~�@�bN@�l�@��R@���@�Ĝ@�1@�;d@���@���@�ff@�5?@��@��#@���@��@�%@���@�bN@���@��@�S�@��@��@�n�@�&�@��/@�Ĝ@���@�t�@���@��@�bN@��@��/@�33@�5?@��@���@��-@��h@�p�@�O�@�&�@���@�Q�@�b@��w@��+@���@�b@���@�\)@���@��#@�V@�Q�@���@�dZ@�
=@�ȴ@�^5@��9@�Q�@�1'@���@���@�dZ@�33@�ȴ@��h@�?}@��`@��D@�b@�@���@��!@�-@��@���@�X@�&�@�V@�%@��@��u@�Q�@��F@��H@�~�@���@�7L@��@���@� �@��P@�dZ@�|�@�t�@��@�ȴ@�ȴ@���@���@�$�@���@���@��7@�`B@�O�@�7L@��@���@���@�r�@�bN@�Z@�Q�@�Z@�Q�@�b@�t�@�\)@��@�=q@�&�@���@�1'@�ƨ@���@��@���@��^@�&�@��u@�9X@�b@���@���@���@���@��F@��
@�  @� �@�9X@�A�@�Q�@�r�@�Z@��@��P@�\)@�o@���@�v�@��@��@���@���@��D@�bN@�Q�@�A�@�A�@�Z@�bN@�Q�@�Q�@�r�@�b@�@��@�P@\)@~�@~ff@~5?@~5?@~E�@~5?@}�T@}p�@}�@|�/@|1@z�H@z��@z�H@z��@z�\@zM�@y&�@xr�@xA�@xb@w�@w�@w�w@wl�@w+@w�@v�y@v��@vE�@u@s��@n5?@j�@b��@V�+@Q%@J^5@E/@@Q�@8bN@.ȴ@+dZ@$��@#o@�T@@�/@M�@l�@�F@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AׅAי�Aח�Aכ�Aם�Aכ�Aח�Aם�Aן�Aן�Aן�Aף�Aו�Aו�Aו�AדuAי�Aכ�Aכ�Aי�A�|�A�z�A��TA�1A��A�x�AìA���A��-A��!A��DA�hsA��DA�1A�33A��+A��A�jA���A�/A�$�A���A��A�l�A�
=A��!A�XA��mA���A���A�&�A���A��A�+A���A�|�A�{A��7A�`BA�\)A���A�&�A��HA���A�l�A�{A� �A��+A���A�/A���A���A�t�A�t�A��`A�XA�A���A�5?A���A�33A�^5A��A��DA�VA�
=A���A�v�A���A�S�A��A��A���A��A�ĜA��^A���A��DA�~�A�x�A�^5A�1'A�1'A}�A{�FA{�Ayl�Aw��Av �At~�As&�As�As�Ar�9Aq`BApr�An�`Am��AmG�Al�9Ak�Ai�hAhAe7LAd~�AdJAc��Ac33Aa��A`n�A_33A]��A\��A\��A\M�A[dZAZ(�AX�DAW�7AV(�AV1AUS�ATJAShsAR(�APn�AM�AK
=AI�TAH�`AG��AF�\AD�ACAAS�A?x�A>�A>5?A>  A=�#A=�A<bNA;�FA8�A8A7�;A7�-A7�A7l�A7G�A7�A6��A6  A5�A5K�A4�A3+A2�!A2�\A1oA.�A,(�A+dZA*�RA)��A(bNA(1A'"�A&9XA%ƨA%p�A$Q�A"�A!�A   A�A�TA+A��AA�A�FAl�A/A�AVA�A �A�FA9XA��A��AhsA��AS�AZA�A�hA1'A�AA��AVA
z�A	�wA	A�uAt�AG�A�A�/A1'A33A5?A  A�FA�Ar�A��A �jA M�@��@���@�M�@��D@�ff@�Ĝ@�$�@���@��@���@�@�
=@�\@�@�A�@�o@��y@���@�=q@���@�@�hs@⟾@�O�@�@�(�@ۥ�@�+@�G�@��@�n�@�hs@ҸR@Ѓ@��@�`B@���@��@�1@�p�@ě�@�~�@�bN@�l�@��R@���@�Ĝ@�1@�;d@���@���@�ff@�5?@��@��#@���@��@�%@���@�bN@���@��@�S�@��@��@�n�@�&�@��/@�Ĝ@���@�t�@���@��@�bN@��@��/@�33@�5?@��@���@��-@��h@�p�@�O�@�&�@���@�Q�@�b@��w@��+@���@�b@���@�\)@���@��#@�V@�Q�@���@�dZ@�
=@�ȴ@�^5@��9@�Q�@�1'@���@���@�dZ@�33@�ȴ@��h@�?}@��`@��D@�b@�@���@��!@�-@��@���@�X@�&�@�V@�%@��@��u@�Q�@��F@��H@�~�@���@�7L@��@���@� �@��P@�dZ@�|�@�t�@��@�ȴ@�ȴ@���@���@�$�@���@���@��7@�`B@�O�@�7L@��@���@���@�r�@�bN@�Z@�Q�@�Z@�Q�@�b@�t�@�\)@��@�=q@�&�@���@�1'@�ƨ@���@��@���@��^@�&�@��u@�9X@�b@���@���@���@���@��F@��
@�  @� �@�9X@�A�@�Q�@�r�@�Z@��@��P@�\)@�o@���@�v�@��@��@���@���@��D@�bN@�Q�@�A�@�A�@�Z@�bN@�Q�@�Q�@�r�@�b@�@��@�P@\)@~�@~ff@~5?@~5?@~E�@~5?@}�T@}p�@}�@|�/@|1@z�H@z��@z�H@z��@z�\@zM�@y&�@xr�@xA�@xb@w�@w�@w�w@wl�@w+@w�@v�y@v��@vE�@u@s��@n5?@j�@b��@V�+@Q%@J^5@E/@@Q�@8bN@.ȴ@+dZ@$��@#o@�T@@�/@M�@l�@�F@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�ZB�TB�TB�TB�TB�TB�ZB�TB�TB�TB�TB�TB�ZB�ZB�ZB�`B�ZB�ZB�ZB�ZB�fB�`B�yB��B�B]/Be`B`BBXB[#B[#BW
BR�BN�BF�B>wB8RB1'B-B'�B�B{B
=BB  B��B��B��B�B�B�yB�ZB�)B��B��BŢB�jB��BW
B(�B�BbB\BJB
=BB��B�B�fB�#B��BǮB�9B��B��B�uB�7B{�Bq�BiyB_;BR�BL�BF�BC�B>wB9XB5?B'�BB
�B
�yB
��B
�B
�)B
�#B
�B
�
B
�B
��B
��B
��B
ÖB
�B
��B
��B
�1B
t�B
e`B
W
B
K�B
K�B
L�B
H�B
;dB
0!B
�B
bB
1B
B	��B	�sB	�B	ĜB	��B	�jB	�XB	�9B	��B	��B	�{B	�DB	�B	�B	|�B	u�B	l�B	iyB	e`B	cTB	gmB	n�B	jB	iyB	cTB	W
B	D�B	5?B	,B	$�B	�B	{B	%B��B�B�NB�BB�TB�yB�yB�sB�mB�sB�ZB�TB�NB�NB�BB�BB�;B�5B�#B�B��B��B��BĜBB��B��B�FB��B��B��B��B��B��B�oB�\B�VB�DB�+B�B|�By�Bw�Bv�Bw�Bx�B{�B� B�B�B�B�B�B�B�B~�B|�B{�Bz�Bw�Br�Bo�Bl�BhsB_;BYBXBT�BR�BP�BN�BL�BJ�BI�BH�BG�BF�BC�BA�B@�B?}B>wB=qB:^B8RB7LB6FB5?B49B2-B0!B.B,B+B+B+B)�B)�B)�B(�B'�B&�B&�B&�B%�B#�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B!�B"�B&�B'�B'�B(�B+B+B,B-B-B-B-B-B-B-B.B.B.B.B/B/B/B/B.B.B1'B2-B2-B1'B33B7LB7LB8RB9XB?}BD�BH�BH�BI�BI�BJ�BJ�BJ�BJ�BK�BM�BN�BN�BQ�B\)B_;BaHBbNBdZBjBn�Br�Bu�Bv�Bw�Bx�By�B�B�B�B�B�B�+B�+B�1B�\B�hB�uB��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�9B�^B�jB��BĜBĜBǮBɺB��B��B��B��B��B��B��B��B��B��B�B�B�B�)B�)B�/B�5B�5B�;B�BB�BB�BB�HB�BB�BB�HB�`B�mB�yB�B��B��B	B	%B	+B	+B	DB	�B	�B	 �B	$�B	&�B	'�B	0!B	7LB	9XB	:^B	=qB	@�B	C�B	E�B	G�B	I�B	M�B	Q�B	YB	]/B	`BB	e`B	ffB	hsB	m�B	o�B	r�B	s�B	t�B	v�B	v�B	w�B	z�B	|�B	}�B	}�B	� B	�7B	�PB	�PB	�PB	�VB	�\B	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�FB	�LB	�LB	�RB	�RB	�RB	�XB	�^B	�^B	�dB	�jB	�qB	��B	ĜB	��B	�B	�B
	7B
�B
!�B
'�B
-B
6FB
B�B
E�B
N�B
Q�B
W
B
ZB
bNB
e`B
iyB
m�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�GB�?B�?B�?B�@B�?B�EB�@B�<B�@B�@B�@B�EB�GB�EB�LB�EB�EB�GB�EB�QB�NB�cB��B�B]BeOB`-BW�B[B[BV�BR�BN�BF�B>aB8<B1B,�B'�B�BgB
(BB��B��B��B��B��B�xB�`B�FB�B��BʪBŊB�VB��BV�B(�BjBHBAB.B
$BB��B�B�LB�
B��BǔB�!B��B��B�[B�B{�Bq�Bi^B_"BR�BL�BF�BC}B>]B9<B5$B'�BB
�B
�^B
��B
�jB
�B
�B
� B
��B
��B
��B
��B
��B
�~B
��B
��B
��B
�B
t�B
eKB
V�B
K�B
K�B
L�B
H�B
;NB
0B
�B
MB
B
�B	��B	�`B	�B	ĊB	�qB	�YB	�EB	�&B	��B	��B	�iB	�3B	� B	��B	|�B	u�B	lyB	ijB	ePB	cDB	g\B	n�B	jpB	ihB	cCB	V�B	D�B	51B	+�B	$�B	�B	oB	B��B�vB�CB�6B�IB�nB�mB�gB�_B�hB�MB�HB�CB�CB�8B�5B�0B�+B�B��B��B��B��BđBB˼B�~B�<B��B��B��B��B�}B�vB�eB�VB�NB�:B�%B�B|�By�Bw�Bv�Bw�Bx�B{�B�B��B�B�	B�B�B�B�B~�B|�B{�Bz�Bw�Br�Bo�Bl�BhlB_4BYBXBT�BR�BP�BN�BL�BJ�BI�BH�BG�BF�BC�BA�B@{B?uB>pB=jB:VB8IB7EB6=B58B41B2&B0B.
B+�B*�B*�B*�B)�B)�B)�B(�B'�B&�B&�B&�B%�B#�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B~B�B�B�B�B�B!�B!�B"�B&�B'�B'�B(�B*�B*�B+�B-B-B-B-B-B-B-B.B.B.B.
B/B/B/B/B.	B.B1B2"B2!B1B3'B7AB7BB8FB9NB?qBD�BH�BH�BI�BI�BJ�BJ�BJ�BJ�BK�BM�BN�BN�BQ�B\B_/Ba:Bb@BdJBjrBn�Br�Bu�Bv�Bw�Bx�By�B��B�	B�	B�B�B�B�B� B�MB�XB�fB�qB�}B��B��B��B��B��B��B��B��B��B��B��B�	B�B�(B�MB�YB�xBĉBČBǚBɧB̻BͿB̺B̺B��B��B��B��B��B��B��B�B�
B�B�B�B�!B� B�&B�.B�.B�.B�7B�.B�.B�3B�JB�YB�dB�B��B��B	�B	B	B	B	-B	pB	�B	 �B	$�B	&�B	'�B	0
B	75B	9>B	:FB	=[B	@lB	CB	E�B	G�B	I�B	M�B	Q�B	Y B	]B	`(B	eGB	fMB	h[B	mzB	o�B	r�B	s�B	t�B	v�B	v�B	w�B	z�B	|�B	}�B	}�B	�B	�B	�6B	�6B	�6B	�:B	�AB	�LB	�[B	�`B	�hB	�kB	�qB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�(B	�0B	�/B	�7B	�7B	�6B	�>B	�DB	�BB	�KB	�PB	�UB	�gB	�B	��B	� B	��B
	B
|B
!�B
'�B
,�B
6*B
BqB
E�B
N�B
Q�B
V�B
Y�B
b-B
eBB
iZB
mrB
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.27 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708272016053117082720160531170827  AO  ARCAADJP                                                                    20150928021522    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150928021522  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150928021522  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170827  IP                  G�O�G�O�G�O�                