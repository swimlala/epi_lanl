CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:17Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  Sl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  mh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181024140817  20181024140817  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               CA   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��e,�1   @��e�n�@2@A�7K��c�t�j~�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      CA   A   B   @�33@�  A   A   A@  A^ffA~ffA�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�fC  C  C  C
  C  C  C  C  C  C  C  C  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ  CK�fCN  CP  CR  CT  CV�CX  CZ  C\  C^  C`�Cb  Cd  Cf  Ch  Ci�fCl  Cn  Cp  Cr  Cs�fCv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD	  D	� D
  D
� D  D� D  D� D  D�fDfD�fD  D� D  D� D��Dy�D  D� D  D� D  D�fDfD�fD  D� D  D� D��Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'  D'� D(  D(� D)  D)� D)��D*� D+  D+� D,  D,� D-  D-y�D-��D.y�D.��D/� D0  D0� D1fD1� D2  D2� D3  D3� D4  D4�fD5fD5�fD6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DIy�DJ  DJ� DK  DK� DL  DL� DM  DM�fDNfDN�fDO  DO� DP  DP� DQ  DQ� DQ��DRy�DR��DSy�DS��DTy�DU  DU� DVfDV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dcy�Dc��Ddy�De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Dn��Doy�Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DufDu�fDv  Dv� Dw  Dw� Dw�3Dy�fD�5qD�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��
@ȣ�AQ�A$Q�ADQ�Ab�RA�\)A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B	z�B{B{B!{B){B1{B8�BA{BI{BQ{BY{Ba{Bi{Bq{By{B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=BĊ=BȊ=B̽pBЊ=BԊ=B؊=B܊=B��=B�=B�=B�=B��=B�=B��=B��=C EC+�CECECEC
ECECECECECECECECECEC^�C EC"EC$EC&EC(EC*EC,EC.EC0EC2EC4EC6EC8EC:EC<EC>EC@ECBECDECFECH^�CJECL+�CNECPECRECTECV^�CXECZEC\EC^EC`^�CbECdECfEChECj+�ClECnECpECrECt+�CvECxECzEC|EC~EC�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C��C��C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C��C�"�C�"�C�"�C�/\C�"�C�"�C�"�C�/\C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD��D	HD	�HD
HD
�HDHD�HDHD�HDHD��D�D��DHD�HDHD�HD
�D��DHD�HDHD�HDHD��D�D��DHD�HDHD�HD
�D��DHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&��D'HD'�HD(HD(�HD)HD)�HD*
�D*�HD+HD+�HD,HD,�HD-HD-��D.
�D.��D/
�D/�HD0HD0�HD1�D1�HD2HD2�HD3HD3�HD4HD4��D5�D5��D6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI��DJHDJ�HDKHDK�HDLHDL�HDMHDM��DN�DN��DOHDO�HDPHDP�HDQHDQ�HDR
�DR��DS
�DS��DT
�DT��DUHDU�HDV�DV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc��Dd
�Dd��DeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDo
�Do��DpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDu�Du��DvHDv�HDwHDw�HDx{Dy��D�>D�XR1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A܉7A܇+A܇+A܃A܅A܇+A܉7A܏\Aܕ�Aܙ�Aܕ�AܓuAܥ�A���A�A�+A�1A��HAܕ�A� �AہA�1'A�S�A�`BA��/A���Aֺ^AՕ�A�E�A�n�A�;dA�v�AЬAС�AЍPAϬA�33A���A�x�A�A�A� �A�A;wA�S�A�x�A�+A˸RA�=qA�~�A��TA�{A�1'Aǥ�Aƛ�A�JAÛ�A�A���A�5?A���A��^A�jA��A�p�A��mA��`A���A�
=A��A�bA�n�A��uA�VA� �A�dZA�VA�7LA��A��7A���A���A��-A�7LA�oA��A�XA��A��mA�S�A�;dA�K�A�S�A�ffA�VA�O�A�|�A�7LA�XA��A��7A�1'A��A�%A��9A�oA���A�1A���A�K�A�l�A��A�oA���A�E�A��mA���A��hA�VA��A�v�A~z�Aw\)As\)Aq�-Ao��Amx�Aj�HAh��Af1Ad�9Ad1'Ab�\A]33AXVAW�PAVr�AT�jAP��AOAN9XAMƨAL��AK��AJ~�AI�7AH�jAG��AD�HAB9XAA�^A?��A>�+A=�FA<v�A:ĜA8��A6ĜA65?A4$�A1K�A/x�A.�+A.JA-O�A*��A)XA)
=A&A#��A"�RA!�
A ZA�^AXAE�AG�A��A��A�AZA��A/A�A��A�HAjA�mA��A?}A�A�jA�A��A  A"�A��Ar�A=qAƨA��A�A+A�\A-A-A��AC�A	�A	33AA@ݙ�@�%@ڗ�@ف@�  @���@�^5@ՙ�@�@�n�@�@Ӯ@��@Л�@Ϯ@�33@�"�@·+@���@�t�@�$�@�V@���@�r�@� �@Ώ\@��@�S�@�J@ə�@�hs@ț�@őh@�Z@�K�@�-@��@��@�J@��@�1'@�1'@� �@�  @��@���@�|�@��P@��w@�ƨ@���@���@�ȴ@�@��-@�p�@�%@�j@�  @���@�\)@�C�@�"�@���@��H@��y@��+@�E�@��@��T@�?}@��j@�I�@��@��F@�t�@�l�@�33@�M�@�x�@�hs@�%@��D@�1'@� �@�1@���@���@�t�@�\)@�;d@�
=@�ff@��@��9@�bN@��;@��w@�+@�n�@�n�@�$�@�@��@�\)@�=q@��^@�7L@�bN@�I�@�  @��;@��F@�|�@��@�@��R@�~�@�=q@�{@���@���@�Ĝ@��@�bN@�b@�ƨ@��;@���@�l�@��@�ȴ@���@��@��@��#@��7@�7L@��/@�Ĝ@���@�Q�@�b@���@��@�\)@�+@�ȴ@�$�@�`B@���@��9@�j@�Z@��@�ƨ@���@��@�v�@�5?@�$�@�5?@�E�@��@���@��7@��7@��h@��7@�X@�bN@���@�;d@��y@��@�ȴ@���@���@��@�X@��/@��D@�r�@���@�S�@��F@�  @��P@�+@�@��H@�ff@�=q@�M�@�E�@��@���@�&�@���@�z�@�r�@�r�@�Z@��@��P@�t�@��P@���@�1@�(�@��w@�"�@��@�ȴ@���@�5?@�@���@�X@��@���@��@��@�j@��;@���@���@�\)@�dZ@�\)@�+@��@���@���@�?}@�%@��j@�bN@�1'@�(�@�  @���@���@�+@���@���@�=q@�@���@���@��T@��#@�@�@��-@�p�@�7L@���@��/@��9@��@�v`@sA�@a}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A܉7A܇+A܇+A܃A܅A܇+A܉7A܏\Aܕ�Aܙ�Aܕ�AܓuAܥ�A���A�A�+A�1A��HAܕ�A� �AہA�1'A�S�A�`BA��/A���Aֺ^AՕ�A�E�A�n�A�;dA�v�AЬAС�AЍPAϬA�33A���A�x�A�A�A� �A�A;wA�S�A�x�A�+A˸RA�=qA�~�A��TA�{A�1'Aǥ�Aƛ�A�JAÛ�A�A���A�5?A���A��^A�jA��A�p�A��mA��`A���A�
=A��A�bA�n�A��uA�VA� �A�dZA�VA�7LA��A��7A���A���A��-A�7LA�oA��A�XA��A��mA�S�A�;dA�K�A�S�A�ffA�VA�O�A�|�A�7LA�XA��A��7A�1'A��A�%A��9A�oA���A�1A���A�K�A�l�A��A�oA���A�E�A��mA���A��hA�VA��A�v�A~z�Aw\)As\)Aq�-Ao��Amx�Aj�HAh��Af1Ad�9Ad1'Ab�\A]33AXVAW�PAVr�AT�jAP��AOAN9XAMƨAL��AK��AJ~�AI�7AH�jAG��AD�HAB9XAA�^A?��A>�+A=�FA<v�A:ĜA8��A6ĜA65?A4$�A1K�A/x�A.�+A.JA-O�A*��A)XA)
=A&A#��A"�RA!�
A ZA�^AXAE�AG�A��A��A�AZA��A/A�A��A�HAjA�mA��A?}A�A�jA�A��A  A"�A��Ar�A=qAƨA��A�A+A�\A-A-A��AC�A	�A	33AA@ݙ�@�%@ڗ�@ف@�  @���@�^5@ՙ�@�@�n�@�@Ӯ@��@Л�@Ϯ@�33@�"�@·+@���@�t�@�$�@�V@���@�r�@� �@Ώ\@��@�S�@�J@ə�@�hs@ț�@őh@�Z@�K�@�-@��@��@�J@��@�1'@�1'@� �@�  @��@���@�|�@��P@��w@�ƨ@���@���@�ȴ@�@��-@�p�@�%@�j@�  @���@�\)@�C�@�"�@���@��H@��y@��+@�E�@��@��T@�?}@��j@�I�@��@��F@�t�@�l�@�33@�M�@�x�@�hs@�%@��D@�1'@� �@�1@���@���@�t�@�\)@�;d@�
=@�ff@��@��9@�bN@��;@��w@�+@�n�@�n�@�$�@�@��@�\)@�=q@��^@�7L@�bN@�I�@�  @��;@��F@�|�@��@�@��R@�~�@�=q@�{@���@���@�Ĝ@��@�bN@�b@�ƨ@��;@���@�l�@��@�ȴ@���@��@��@��#@��7@�7L@��/@�Ĝ@���@�Q�@�b@���@��@�\)@�+@�ȴ@�$�@�`B@���@��9@�j@�Z@��@�ƨ@���@��@�v�@�5?@�$�@�5?@�E�@��@���@��7@��7@��h@��7@�X@�bN@���@�;d@��y@��@�ȴ@���@���@��@�X@��/@��D@�r�@���@�S�@��F@�  @��P@�+@�@��H@�ff@�=q@�M�@�E�@��@���@�&�@���@�z�@�r�@�r�@�Z@��@��P@�t�@��P@���@�1@�(�@��w@�"�@��@�ȴ@���@�5?@�@���@�X@��@���@��@��@�j@��;@���@���@�\)@�dZ@�\)@�+@��@���@���@�?}@�%@��j@�bN@�1'@�(�@�  @���@���@�+@���@���@�=q@�@���@���@��T@��#@�@�@��-@�p�@�7L@���@��/@��9@��@�v`@sA�@a}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
n�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
q�B
r�B
p�B
n�B
u�B
�B
��B
�wB
ÖB
��B
�B
�BB
�B
�B
�B
�B
�B
��B%B+B�B,B@�B;dB)�B6FBm�B� B�7B�VB�bB�oB�uB�{B��B��B��B��B��B��B��B�yB�mB�fB�yB�B%�BT�By�B��B��B��B��B��B�B�dB�;B�B�B�B�B�mB�`B�HB�;B�5B�5B�)B�/B�
B��B�jB�FB�B�hB�\B��B�bB�VBy�BZB6FB�B+B�B�'B��By�BM�B<jB6FB,B�B
�B
��B
��B
ɺB
ƨB
�HBDB�B{BB
��B
�B
�B
�fB
�B
ÖB
�B
�1B
n�B
L�B
�B	��B	�B	�TB	��B	��B	�3B	��B	��B	��B	�+B	jB	N�B	H�B	B�B	9XB	'�B	�B	�B	�B	hB	JB	B��B��B��B�B�;B�)B�B��B��B��B��B��BƨBB�^B�B��B��B��B��B��B�\B�=B�B~�B}�Bz�By�Bx�Bu�Bt�Bs�Bp�Bl�BjBm�B�PB�{B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�'B�-B�-B�-B�9B�RB�LB�RB�XB�dB�dB�XB�LB�XBw�Br�Bs�Bq�Bn�Bm�Bk�Bo�Bq�Br�B�B�%B�PB�\B�+B�B�B�+B�7B�JB�hB��B�BŢB��B��B�
B�
B�5B�NB�ZB�ZB�`B�B��B��B	B	B	+B	DB	hB	hB	hB	hB	bB	bB	oB	�B	�B	�B	�B	�B	�B	&�B	+B	-B	-B	.B	-B	-B	.B	/B	0!B	0!B	1'B	2-B	33B	5?B	6FB	7LB	8RB	;dB	>wB	A�B	C�B	E�B	G�B	G�B	H�B	M�B	Q�B	Q�B	R�B	R�B	YB	ZB	]/B	]/B	aHB	bNB	cTB	e`B	gmB	iyB	jB	jB	o�B	q�B	p�B	q�B	v�B	x�B	y�B	x�B	v�B	t�B	u�B	x�B	|�B	�B	�B	�B	�B	�B	�%B	�1B	�7B	�DB	�JB	�PB	�PB	�\B	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�9B	�?B	�FB	�LB	�RB	�RB	�XB	�^B	�dB	�jB	�wB	��B	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�5B	�BB	�BB	�BB	�HB	�TB	�NB	�HB	�TB	�TB	�`B	�fB	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
%B
+B
+B
%B
B
B
B
%B
1B

=B

=B

=B
DB
DB
JB
JB
JB
PB
PB
PB
PB
PB
VB
VB
VB
VB
VB
\B
\B
\B
hB
uB
�B
'�B
5Z1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
n�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
q�B
r�B
p�B
n�B
u�B
�B
��B
�wB
ÖB
��B
�B
�BB
�B
�B
�B
�B
�B
��B%B+B�B,B@�B;dB)�B6FBm�B� B�7B�VB�bB�oB�uB�{B��B��B��B��B��B��B��B�yB�mB�fB�yB�B%�BT�By�B��B��B��B��B��B�B�dB�;B�B�B�B�B�mB�`B�HB�;B�5B�5B�)B�/B�
B��B�jB�FB�B�hB�\B��B�bB�VBy�BZB6FB�B+B�B�'B��By�BM�B<jB6FB,B�B
�B
��B
��B
ɺB
ƨB
�HBDB�B{BB
��B
�B
�B
�fB
�B
ÖB
�B
�1B
n�B
L�B
�B	��B	�B	�TB	��B	��B	�3B	��B	��B	��B	�+B	jB	N�B	H�B	B�B	9XB	'�B	�B	�B	�B	hB	JB	B��B��B��B�B�;B�)B�B��B��B��B��B��BƨBB�^B�B��B��B��B��B��B�\B�=B�B~�B}�Bz�By�Bx�Bu�Bt�Bs�Bp�Bl�BjBm�B�PB�{B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�'B�-B�-B�-B�9B�RB�LB�RB�XB�dB�dB�XB�LB�XBw�Br�Bs�Bq�Bn�Bm�Bk�Bo�Bq�Br�B�B�%B�PB�\B�+B�B�B�+B�7B�JB�hB��B�BŢB��B��B�
B�
B�5B�NB�ZB�ZB�`B�B��B��B	B	B	+B	DB	hB	hB	hB	hB	bB	bB	oB	�B	�B	�B	�B	�B	�B	&�B	+B	-B	-B	.B	-B	-B	.B	/B	0!B	0!B	1'B	2-B	33B	5?B	6FB	7LB	8RB	;dB	>wB	A�B	C�B	E�B	G�B	G�B	H�B	M�B	Q�B	Q�B	R�B	R�B	YB	ZB	]/B	]/B	aHB	bNB	cTB	e`B	gmB	iyB	jB	jB	o�B	q�B	p�B	q�B	v�B	x�B	y�B	x�B	v�B	t�B	u�B	x�B	|�B	�B	�B	�B	�B	�B	�%B	�1B	�7B	�DB	�JB	�PB	�PB	�\B	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�9B	�?B	�FB	�LB	�RB	�RB	�XB	�^B	�dB	�jB	�wB	��B	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�5B	�BB	�BB	�BB	�HB	�TB	�NB	�HB	�TB	�TB	�`B	�fB	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
%B
+B
+B
%B
B
B
B
%B
1B

=B

=B

=B
DB
DB
JB
JB
JB
PB
PB
PB
PB
PB
VB
VB
VB
VB
VB
\B
\B
\B
hB
uB
�B
'�B
5Z1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.27 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140817                              AO  ARCAADJP                                                                    20181024140817    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140817  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140817  QCF$                G�O�G�O�G�O�0               