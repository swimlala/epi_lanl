CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-06-12T21:35:10Z creation;2018-06-12T21:35:12Z conversion to V3.1;2019-12-23T06:20:24Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I(   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �\   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �T   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �@   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �@   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �@   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �@   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20180612213510  20200120021520  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               CA   JA  I2_0675_067                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�i����1   @�i�I���@8���Z���c!���1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR�CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Cg�fCj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D#��D$y�D$��D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9fD9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_fD_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� DvfDv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�C3Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @J�H@���@ȣ�AQ�A$Q�ADQ�AdQ�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B	{B{B{B!{B){B1{B9{BA{BI{BQ{BY{Ba{Bi{Bq{By{B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=BĊ=BȊ=B̊=BЊ=BԊ=B؊=B܊=B��=B�=B�=B�=B��=B�=B��=B��=C ECECECECEC
ECECECECECECECECECECEC EC"EC$EC&EC(EC*EC,EC.EC0EC2EC4EC6EC8EC:EC<EC>EC@ECBECDECFECHECJECLECNECPECR^�CTECVECXECZEC\EC^EC`ECbECdECf^�Ch+�CjEClECnECpECrECtECvECxECzEC|EC~EC�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$
�D$��D%
�D%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9�D9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_�D_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDv�Dv�HDwHDw�HDxHDx�HDyHDy�HDzHDz�HD{HD{�HD|HD|�HD}HD}�HD~HD~�HDHD�HD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D��qD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D���D��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD�qD�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D�D�ȤD��D�H�DÈ�D�ȤD��D�H�DĈ�D�ȤD��D�H�Dň�D�ȤD��D�H�Dƈ�D�ȤD��D�H�Dǈ�D�ȤD��D�H�DȈ�D�ȤD��D�H�DɈ�D�ȤD��D�H�Dʈ�D�ȤD��D�H�Dˈ�D�ȤD��D�H�D̈�D�ȤD��D�H�D͈�D�ȤD��D�H�DΈ�D�ȤD��D�H�Dψ�D�ȤD��D�H�DЈ�D�ȤD��D�H�Dш�D�ȤD��D�H�D҈�D�ȤD��D�H�Dӈ�D�ȤD��D�H�DԈ�D�ȤD��D�H�DՈ�D�ȤD��D�H�Dֈ�D�ȤD��D�H�D׈�D�ȤD��D�H�D؈�D�ȤD��D�K�Dو�D�ȤD��D�H�Dڈ�D�ȤD��D�H�Dۈ�D�ȤD��D�H�D܈�D�ȤD��D�H�D݈�D�ȤD��D�H�Dވ�D�ȤD��D�H�D߈�D�ȤD��D�H�D���D�ȤD��D�H�DሤD�ȤD��D�H�D∤D�ȤD��D�H�D㈤D�ȤD��D�H�D䈤D�ȤD��D�H�D判D�ȤD��D�H�D戤D�ȤD��D�H�D爤D�ȤD��D�H�D舤D�ȤD��D�H�D鈤D�ȤD��D�H�DꈤD�ȤD��D�H�D눤D�ȤD��D�H�D숤D�ȤD��D�H�D툤D�ȤD��D�H�DD�ȤD��D�H�DD�ȤD��D�H�D���D�ȤD��D�H�D�D�ȤD��D�H�D�D�ȤD��D�H�D�D�ȤD��D�H�D�D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�K�D���D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�/A�/A�1'A�1'A�/A�/A�-A�+A�(�A�{A��A�`BA���A�Q�A���A�1'A���A��HA���A�G�A���A�z�A�C�A�S�A�7LA��7A�=qA���A���A�;dA�  A���A��A�^5A�`BA��+A�ȴA�~�A���A��/A��+A��`A��\A�A���A�E�A��TA�ȴA���A��\A�z�A�1'A�VA��#A���A�~�A��`A�
=A�~�A�bNA�"�A�1'A�I�A���A���A��A�r�A�{A���A��`A�ƨA�/A�-A��jA�l�A�  A�^5A�M�A��uA�;dA�p�A��PA���A��#A��A��A��^A��-A�XA���A�`BA�ȴA��A���A�VA��\A�?}A�Q�A�oA�/A��A� �A�-A�%A��A~�A}t�A{|�Aw�TAt�Ap��An1Al�Al�Ak+Aj��AjA�AidZAf�9Ad�DAcVAbbNAa��A`1'A]�TA\z�A[�AY�^AW�AV�AU��AU/AS��AQ
=AP�DAPjAO�AN��ANffAM�;AM&�AK�7AJ�HAJ�!AIAG��AE��AD1'AC�AAS�A@=qA>�/A=�
A<�A:��A8^5A7�hA6A�A4M�A4�A3l�A2�HA1��A0��A0�A/�A.n�A.-A.A-�hA-;dA-VA,A�A*�A(�HA(A'p�A&��A&z�A&(�A%��A$��A#��A#O�A!��A!oA $�At�A?}A1'A|�AVAQ�A\)A�\AhsAbNA��A
=A^5A�A�A&�AM�A��AVA�!A�A�\A\)A�9A��A
�9A	�-A�A9XA�mAt�AI�A�hA33A=qA�AVA�A �`A -@���@�M�@�=q@�x�@��@���@�o@�%@�@�^@@���@�u@�|�@�M�@���@�dZ@�"�@�ff@�^@�  @�7@�I�@�t�@��@݉7@�/@���@�A�@�C�@٩�@�Ĝ@؃@�Q�@�1@�dZ@֟�@պ^@ԣ�@��@�;d@��@��@�I�@�ȴ@�`B@̴9@�bN@�1@�C�@ʧ�@�@�%@�9X@���@�+@�$�@�?}@ċD@� �@��;@�dZ@��u@��@���@�O�@�A�@�bN@��@�+@��\@���@�9X@��@��
@�+@�{@��j@�bN@�b@�33@���@�5?@���@��/@�r�@��;@�|�@�"�@�@�E�@���@�p�@�(�@��F@��@�33@���@�^5@��#@�p�@�V@�r�@�(�@�  @���@�S�@�+@��y@�V@�@��@��/@� �@�\)@�ȴ@�ff@�=q@�-@�^5@���@�p�@���@�z�@� �@���@�\)@�\)@�K�@�
=@���@�ȴ@��+@�V@�n�@�n�@�$�@�x�@�G�@��`@�1'@���@��F@��P@��@�dZ@�33@��R@��+@�v�@�ff@�=q@�5?@�5?@���@�5?@�V@��T@��@���@�O�@�1'@�1@��@��@��m@�C�@���@��H@���@��R@���@�n�@���@�`B@�V@���@�p�@�`B@�X@�V@���@��@�Ĝ@�z�@�9X@��@��@���@���@��@�{@�M�@�{@��#@��-@���@�x�@�7L@��`@��m@�|�@��@�ff@�J@�@�@�@�X@��@�r�@�1'@�A�@�b@� �@�A�@��@� �@��F@���@�  @��@�o@���@�v�@�@�
=@���@���@�ȴ@���@�o@���@���@�ff@�=q@��@���@�/@�&�@�&�@�&�@�%@��@��j@��@�j@�Q�@�1'@� �@�1@�ƨ@�l�@�\)@�33@���@���@��R@���@��+@�ff@�M�@��@�@��h@�x�@�?}@��@���@��`@��j@��@�Q�@��@��w@���@�S�@��@��H@���@�^5@�M�@���@��7@�x�@�hs@�O�@�O�@�X@�G�@��@��j@��@�A�@�@�@\)@K�@~ȴ@~E�@~@}@|�/@|j@|�@{�m@{�@z��@z��@z�\@z=q@y��@x��@xbN@xbN@x1'@w�@w�@wl�@vȴ@vv�@v5?@u�T@up�@t�@tZ@t1@so@r=q@q��@qx�@qX@qG�@q&�@pĜ@p1'@o�@nȴ@m�h@m�@m�T@n@m�@m`B@l�@l(�@k��@k��@ko@jM�@j�@i��@i��@hĜ@g�@g|�@g
=@fv�@f$�@f{@f{@e�@e��@e`B@e�@eV@d�@d�D@dz�@d�@c�
@c"�@b��@bM�@a��@a�^@aX@`�u@`Q�@_�w@^�@^5?@]p�@]p�@]O�@]/@]�@\�j@\1@[�@[C�@[o@Z��@ZM�@Z=q@Z-@Y��@Y��@Y�^@Y�7@YX@Y�@X�9@X�u@X1'@W��@W��@W��@W�@W��@W��@W�P@W+@V��@V�y@V�R@VV@VE�@U�@U�-@U�h@U`B@U/@T��@T�@T��@Tj@S�m@S��@SS�@S"�@S@R�@R�@R�H@R��@R^5@Q�#@QG�@P��@P�`@P�@P �@P  @O��@O|�@O�@N�@N�+@M�@M��@MO�@L�@L�/@L��@Lj@Kt�@KC�@J�@J�\@J�@Ihs@I&�@H��@H�u@Hr�@HQ�@Hb@G��@G�@G�P@G�P@G\)@Fv�@Ep�@D�/@DI�@D�@D1@D1@C��@C�
@C�@Ct�@CdZ@C33@B�@B�H@B~�@A�@A��@A�^@A�^@A��@A�7@Ahs@AX@A&�@@��@@Q�@?�@?��@?�@?�P@?;d@>�y@>�R@>��@>v�@>E�@>$�@>{@=p�@=�@<�/@<�D@<(�@;ƨ@;�F@;��@;33@:�!@:=q@:-@:�@:J@9�@9�#@9��@9�7@9X@9G�@9&�@8Ĝ@8�@8Q�@7�@7�@7l�@7;d@6��@6ff@6{@5p�@4�j@4�@4�@4��@41@3C�@3"�@3@2��@2��@2~�@2M�@1�#@17L@0��@0��@0r�@0bN@0Q�@0  @/l�@/;d@/
=@.��@.�R@.E�@-�T@-��@-�-@-�h@-p�@-/@,�/@,�j@,��@,z�@,I�@,9X@,1@+�m@+�F@+S�@+"�@*�@*��@*��@*n�@*=q@*J@)��@)��@)�#@)��@)x�@)&�@(��@(�@(bN@(Q�@(A�@( �@'�@'�P@'\)@'
=@&�@&V@%�T@%��@%�@%O�@%?}@%�@%�@$��@$��@$j@$Z@$9X@$�@#�F@#t�@#"�@#"�@#@"��@"��@"M�@!��@!��@!�7@ ��@ bN@ b@�P@��@ȴ@�+@ff@E�@@��@@��@p�@��@��@Z@��@��@dZ@��@��@�\@n�@�@��@X@7L@%@��@��@Ĝ@��@��@�u@�u@�@r�@bN@b@�P@l�@;d@+@��@ȴ@�+@ff@V@5?@@�-@��@�@p�@?}@/@/@�@V@��@�@�/@z�@(�@��@�
@ƨ@�F@�F@ƨ@�m@�F@�F@�@S�@C�@"�@�@�!@n�@=q@-@-@�@��@��@��@x�@hs@7L@&�@&�@�@��@�u@bN@Q�@A�@��@|�@\)@K�@K�@;d@+@+@+@
=@�+@E�@5?@{@{@�@��@��@/@�@I�@9X@9X@9X@I�@(�@��@�m@�m@��@��@�m@�m@�m11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�/A�/A�1'A�1'A�/A�/A�-A�+A�(�A�{A��A�`BA���A�Q�A���A�1'A���A��HA���A�G�A���A�z�A�C�A�S�A�7LA��7A�=qA���A���A�;dA�  A���A��A�^5A�`BA��+A�ȴA�~�A���A��/A��+A��`A��\A�A���A�E�A��TA�ȴA���A��\A�z�A�1'A�VA��#A���A�~�A��`A�
=A�~�A�bNA�"�A�1'A�I�A���A���A��A�r�A�{A���A��`A�ƨA�/A�-A��jA�l�A�  A�^5A�M�A��uA�;dA�p�A��PA���A��#A��A��A��^A��-A�XA���A�`BA�ȴA��A���A�VA��\A�?}A�Q�A�oA�/A��A� �A�-A�%A��A~�A}t�A{|�Aw�TAt�Ap��An1Al�Al�Ak+Aj��AjA�AidZAf�9Ad�DAcVAbbNAa��A`1'A]�TA\z�A[�AY�^AW�AV�AU��AU/AS��AQ
=AP�DAPjAO�AN��ANffAM�;AM&�AK�7AJ�HAJ�!AIAG��AE��AD1'AC�AAS�A@=qA>�/A=�
A<�A:��A8^5A7�hA6A�A4M�A4�A3l�A2�HA1��A0��A0�A/�A.n�A.-A.A-�hA-;dA-VA,A�A*�A(�HA(A'p�A&��A&z�A&(�A%��A$��A#��A#O�A!��A!oA $�At�A?}A1'A|�AVAQ�A\)A�\AhsAbNA��A
=A^5A�A�A&�AM�A��AVA�!A�A�\A\)A�9A��A
�9A	�-A�A9XA�mAt�AI�A�hA33A=qA�AVA�A �`A -@���@�M�@�=q@�x�@��@���@�o@�%@�@�^@@���@�u@�|�@�M�@���@�dZ@�"�@�ff@�^@�  @�7@�I�@�t�@��@݉7@�/@���@�A�@�C�@٩�@�Ĝ@؃@�Q�@�1@�dZ@֟�@պ^@ԣ�@��@�;d@��@��@�I�@�ȴ@�`B@̴9@�bN@�1@�C�@ʧ�@�@�%@�9X@���@�+@�$�@�?}@ċD@� �@��;@�dZ@��u@��@���@�O�@�A�@�bN@��@�+@��\@���@�9X@��@��
@�+@�{@��j@�bN@�b@�33@���@�5?@���@��/@�r�@��;@�|�@�"�@�@�E�@���@�p�@�(�@��F@��@�33@���@�^5@��#@�p�@�V@�r�@�(�@�  @���@�S�@�+@��y@�V@�@��@��/@� �@�\)@�ȴ@�ff@�=q@�-@�^5@���@�p�@���@�z�@� �@���@�\)@�\)@�K�@�
=@���@�ȴ@��+@�V@�n�@�n�@�$�@�x�@�G�@��`@�1'@���@��F@��P@��@�dZ@�33@��R@��+@�v�@�ff@�=q@�5?@�5?@���@�5?@�V@��T@��@���@�O�@�1'@�1@��@��@��m@�C�@���@��H@���@��R@���@�n�@���@�`B@�V@���@�p�@�`B@�X@�V@���@��@�Ĝ@�z�@�9X@��@��@���@���@��@�{@�M�@�{@��#@��-@���@�x�@�7L@��`@��m@�|�@��@�ff@�J@�@�@�@�X@��@�r�@�1'@�A�@�b@� �@�A�@��@� �@��F@���@�  @��@�o@���@�v�@�@�
=@���@���@�ȴ@���@�o@���@���@�ff@�=q@��@���@�/@�&�@�&�@�&�@�%@��@��j@��@�j@�Q�@�1'@� �@�1@�ƨ@�l�@�\)@�33@���@���@��R@���@��+@�ff@�M�@��@�@��h@�x�@�?}@��@���@��`@��j@��@�Q�@��@��w@���@�S�@��@��H@���@�^5@�M�@���@��7@�x�@�hs@�O�@�O�@�X@�G�@��@��j@��@�A�@�@�@\)@K�@~ȴ@~E�@~@}@|�/@|j@|�@{�m@{�@z��@z��@z�\@z=q@y��@x��@xbN@xbN@x1'@w�@w�@wl�@vȴ@vv�@v5?@u�T@up�@t�@tZ@t1@so@r=q@q��@qx�@qX@qG�@q&�@pĜ@p1'@o�@nȴ@m�h@m�@m�T@n@m�@m`B@l�@l(�@k��@k��@ko@jM�@j�@i��@i��@hĜ@g�@g|�@g
=@fv�@f$�@f{@f{@e�@e��@e`B@e�@eV@d�@d�D@dz�@d�@c�
@c"�@b��@bM�@a��@a�^@aX@`�u@`Q�@_�w@^�@^5?@]p�@]p�@]O�@]/@]�@\�j@\1@[�@[C�@[o@Z��@ZM�@Z=q@Z-@Y��@Y��@Y�^@Y�7@YX@Y�@X�9@X�u@X1'@W��@W��@W��@W�@W��@W��@W�P@W+@V��@V�y@V�R@VV@VE�@U�@U�-@U�h@U`B@U/@T��@T�@T��@Tj@S�m@S��@SS�@S"�@S@R�@R�@R�H@R��@R^5@Q�#@QG�@P��@P�`@P�@P �@P  @O��@O|�@O�@N�@N�+@M�@M��@MO�@L�@L�/@L��@Lj@Kt�@KC�@J�@J�\@J�@Ihs@I&�@H��@H�u@Hr�@HQ�@Hb@G��@G�@G�P@G�P@G\)@Fv�@Ep�@D�/@DI�@D�@D1@D1@C��@C�
@C�@Ct�@CdZ@C33@B�@B�H@B~�@A�@A��@A�^@A�^@A��@A�7@Ahs@AX@A&�@@��@@Q�@?�@?��@?�@?�P@?;d@>�y@>�R@>��@>v�@>E�@>$�@>{@=p�@=�@<�/@<�D@<(�@;ƨ@;�F@;��@;33@:�!@:=q@:-@:�@:J@9�@9�#@9��@9�7@9X@9G�@9&�@8Ĝ@8�@8Q�@7�@7�@7l�@7;d@6��@6ff@6{@5p�@4�j@4�@4�@4��@41@3C�@3"�@3@2��@2��@2~�@2M�@1�#@17L@0��@0��@0r�@0bN@0Q�@0  @/l�@/;d@/
=@.��@.�R@.E�@-�T@-��@-�-@-�h@-p�@-/@,�/@,�j@,��@,z�@,I�@,9X@,1@+�m@+�F@+S�@+"�@*�@*��@*��@*n�@*=q@*J@)��@)��@)�#@)��@)x�@)&�@(��@(�@(bN@(Q�@(A�@( �@'�@'�P@'\)@'
=@&�@&V@%�T@%��@%�@%O�@%?}@%�@%�@$��@$��@$j@$Z@$9X@$�@#�F@#t�@#"�@#"�@#@"��@"��@"M�@!��@!��@!�7@ ��@ bN@ b@�P@��@ȴ@�+@ff@E�@@��@@��@p�@��@��@Z@��@��@dZ@��@��@�\@n�@�@��@X@7L@%@��@��@Ĝ@��@��@�u@�u@�@r�@bN@b@�P@l�@;d@+@��@ȴ@�+@ff@V@5?@@�-@��@�@p�@?}@/@/@�@V@��@�@�/@z�@(�@��@�
@ƨ@�F@�F@ƨ@�m@�F@�F@�@S�@C�@"�@�@�!@n�@=q@-@-@�@��@��@��@x�@hs@7L@&�@&�@�@��@�u@bN@Q�@A�@��@|�@\)@K�@K�@;d@+@+@+@
=@�+@E�@5?@{@{@�@��@��@/@�@I�@9X@9X@9X@I�@(�@��@�m@�m@��@��@�m@�m@�m11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�yB�yB�yB�B�B�B�B�B�B��B+B+BO�BT�BgmBn�Bp�Bp�Bo�Bo�Bv�Bw�B{�B� B�B�%B�\B��B��B��B��B��B�RB�FB��B��B�oB��B�Bv�Bq�BhsBdZBdZB� B��B�FBÖBǮBɺB��B��BȴBɺBȴBȴB�?B}�BhsBVB8RB'�B"�B49B33B,BVB�B�B�B�B�mB�B��B��BŢB�dB�B��B��B�\B}�Bs�BbNB]/BS�BL�B>wB9XB5?B+B&�B�B\BDB
��B
�/B
��B
ɺB
�qB
�3B
��B
��B
�B
s�B
W
B
J�B
9XB
"�B
DB	�B	�;B	��B	��B	��B	ɺB	ŢB	��B	�B	��B	�{B	�PB	�1B	~�B	o�B	ffB	aHB	\)B	P�B	G�B	E�B	A�B	9XB	-B	(�B	'�B	&�B	 �B	�B	�B	�B	VB	1B	+B	B��B�B�ZB�NB�B��B��BÖB�dB�FB��B��B��B�bB�VB�VB�JB�=B�B�B}�Bv�Br�Bq�Bo�Bn�Bn�BiyBaHBXBQ�BO�BN�BO�BN�BM�BJ�BK�BK�BF�BA�B?}B?}BH�BI�BG�BF�BD�BB�B>wB9XB49B1'B33B2-B/B0!B.B33B5?B2-B2-B+B+B,B)�B+B)�B)�B(�B'�B&�B%�B%�B#�B#�B#�B#�B"�B �B!�B �B �B�B�B�B�B�B�B�B�B�B�B �B �B �B �B"�B#�B"�B#�B"�B%�B%�B%�B%�B&�B&�B&�B&�B&�B'�B'�B'�B'�B'�B&�B'�B+B+B+B)�B'�B)�B)�B)�B+B-B,B-B.B/B0!B1'B49B5?B6FB8RB;dB=qB=qB@�B@�BB�BD�BC�BJ�BH�BI�BO�BS�BVBXB^5B_;B`BBcTBe`BiyBm�Bn�Bp�Bt�Bw�By�B|�B�B�B�B�1B�=B�\B�\B�VB�VB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�LB�jB��B��B�}B�wB��BĜBǮB��B��B��B�
B�B�
B�
B�B�)B�HB�ZB�TB�NB�fB�mB�B�B�B�B��B��B��B	  B	B	B	+B		7B	
=B	
=B	
=B	PB	\B	bB	oB	uB	{B	�B	�B	%�B	&�B	%�B	)�B	1'B	1'B	6FB	;dB	@�B	I�B	I�B	K�B	O�B	Q�B	T�B	W
B	YB	[#B	\)B	]/B	dZB	ffB	gmB	n�B	p�B	q�B	t�B	w�B	{�B	|�B	}�B	� B	�B	�B	�B	�+B	�DB	�\B	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	�-B	�3B	�9B	�LB	�9B	�FB	�LB	�XB	�}B	�}B	��B	B	ĜB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�)B	�/B	�/B	�5B	�BB	�HB	�HB	�TB	�TB	�TB	�TB	�ZB	�`B	�fB	�fB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
1B
	7B
1B
1B
1B
	7B
	7B
	7B

=B

=B

=B

=B
	7B
	7B
JB
VB
\B
\B
\B
VB
VB
\B
\B
VB
\B
\B
\B
\B
VB
VB
VB
VB
VB
VB
\B
bB
hB
oB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
{B
{B
{B
uB
uB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
+B
+B
+B
,B
,B
-B
-B
.B
/B
/B
/B
/B
/B
/B
/B
.B
.B
/B
/B
0!B
0!B
1'B
0!B
1'B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
33B
33B
33B
33B
33B
33B
49B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
;dB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
Q�B
R�B
S�B
T�B
T�B
VB
VB
W
B
XB
XB
XB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
e`B
ffB
ffB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
jB
jB
jB
jB
jB
k�B
k�B
l�B
l�B
l�B
l�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�_B�_B�_B�eB�qB�wB�}B��B�B��BB*�BO�BT�BgRBn}Bp�Bp�Bo�Bo�Bv�Bw�B{�B�B�B�B�BB�B��B��B��B��B�8B�+B��B��B�TB�yB�Bv�Bq�BhXBd@Bd@B�B��B�+B�{BǔBɠBʦBʦBȚBɠBȚBȚB�%B}�BhXBU�B88B'�B"�B4B3B+�B<B�B�B��B�}B�RB�B��B̳BňB�JB��B��B�B�BB}�Bs�Bb4B]BS�BL�B>]B9>B5%B*�B&�BgBBB)B
��B
�B
͹B
ɠB
�VB
�B
��B
��B
��B
s�B
V�B
J�B
9>B
"�B
)B	�B	�!B	��B	��B	͹B	ɠB	ňB	�iB	� B	��B	�aB	�6B	�B	~�B	oiB	fLB	a-B	\B	P�B	G�B	E�B	AoB	9>B	,�B	(�B	'�B	&�B	 �B	�B	�B	gB	<B	B	B	�B��B�kB�@B�4B�B��BˬB�{B�JB�+B��B��B��B�HB�<B�<B�0B�#B��B��B}�Bv�Br�Bq�Bo�Bn}BncBiDBa-BW�BQ�BO�BN�BO�BN�BM�BJ�BK�BK�BF�BAoB?cB?cBH�BI�BG�BFtBD�BBuB>]B9>B4B0�B3B2B/ B/�B-�B3B5%B2B2B*�B*�B+�B)�B*�B)�B)�B(�B'�B&�B%�B%�B#�B#�B#�B#�B"�B �B!�B �B �B�B�B�B�B�B�B�B�B�B�B �B �B �B �B"�B#�B"�B#�B"�B%�B%�B%�B%�B&�B&�B&�B&�B&�B'�B'�B'�B'�B'�B&�B'�B*�B*�B*�B)�B'�B)�B)�B)�B*�B,�B+�B,�B-�B/ B0B0�B4B5%B6B88B;JB=VB=VB@OB@iBBuBD�BC{BJ�BH�BI�BO�BS�BU�BW�B^B_!B`'Bc:Be,BiDBm]Bn}BpoBt�Bw�By�B|�B��B��B�B�B�	B�(B�BB�"B�<B�4B�SB�YB�YB�B��B��B��B��B��B��B��B��B��B��B��B�2B�6B�oB�iB�cB�BB�oBāB�zB͹B��B��B��B��B��B��B�B�B�-B�@B�:B�4B�2B�RB�kB��B�B�|B��B��B��B��B	�B	B	B		B	
#B	
	B	
#B	B	BB	HB	TB	@B	FB	eB	�B	%�B	&�B	%�B	)�B	1B	1B	6+B	;JB	@OB	I�B	I�B	K�B	O�B	Q�B	T�B	V�B	X�B	Z�B	\B	]B	d@B	fLB	gRB	ncB	poB	q�B	t�B	w�B	{�B	|�B	}�B	�B	��B	��B	��B	��B	�)B	�(B	�HB	�4B	�aB	�gB	�sB	�sB	�YB	�MB	�gB	�gB	�YB	�eB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�+B	�2B	�$B	�cB	�cB	�iB	�[B	�gB	ǔB	ȚB	ɠB	˒B	̘B	͹B	οB	��B	��B	��B	ҽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�B	�B	�-B	�B	�:B	�:B	� B	�:B	�@B	�FB	�LB	�LB	�RB	�8B	�XB	�_B	�eB	�kB	�WB	�qB	�}B	�iB	�B	�B	�B	��B	�oB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
B
�B
B
�B
�B
	B
B
B
B
	B
	B
	B

#B

#B

#B

	B
	B
	B
B
"B
(B
(B
BB
"B
<B
BB
BB
"B
BB
(B
BB
BB
"B
<B
<B
<B
<B
<B
BB
HB
NB
:B
[B
[B
@B
[B
aB
gB
MB
mB
MB
gB
MB
FB
aB
FB
@B
@B
:B
TB
:B
TB
TB
:B
TB
:B
:B
:B
:B
[B
[B
aB
gB
MB
sB
_B
yB
_B
eB
B
�B
kB
qB
�B
�B
�B
xB
�B
�B
~B
xB
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
*�B
*�B
*�B
+�B
+�B
,�B
,�B
-�B
/ B
.�B
.�B
.�B
/ B
.�B
.�B
-�B
-�B
/ B
/ B
0B
0B
0�B
/�B
0�B
/�B
1B
1B
1B
1B
1�B
2B
1�B
2�B
3B
2�B
3B
2�B
3B
4B
4B
4B
4B
5%B
5B
5%B
6+B
6+B
6B
7B
72B
88B
88B
88B
88B
8B
88B
9>B
9$B
:DB
:*B
;0B
;JB
;JB
;0B
;JB
<6B
<6B
=<B
=VB
=<B
=VB
=VB
=VB
=<B
=<B
=VB
>]B
>]B
>]B
>]B
>]B
>]B
?HB
?HB
?cB
?cB
?HB
@OB
@OB
@iB
@OB
@iB
AUB
AoB
AUB
AoB
AoB
AoB
AoB
BuB
BuB
BuB
BuB
BuB
BuB
BuB
BuB
CaB
C{B
C{B
C{B
CaB
C{B
C{B
D�B
D�B
D�B
D�B
DgB
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
Q�B
R�B
S�B
T�B
T�B
U�B
U�B
V�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
Y�B
ZB
[	B
[	B
Z�B
[	B
Z�B
\B
[�B
\B
\B
]B
\�B
\�B
]B
^B
^B
^B
^B
^B
^B
^B
^B
^B
^B
_!B
_B
_B
_!B
`'B
`'B
`'B
`'B
`B
`'B
a-B
aB
a-B
b4B
bB
b4B
b4B
b4B
b4B
bB
b4B
bB
b4B
c:B
c:B
c:B
c:B
c:B
c B
c:B
d@B
e,B
e,B
e,B
e,B
e,B
eFB
e,B
f2B
e,B
fLB
fLB
eFB
eFB
e,B
eFB
f2B
fLB
fLB
fLB
fLB
f2B
f2B
fLB
fLB
f2B
g8B
gRB
gRB
h>B
hXB
h>B
hXB
hXB
h>B
hXB
hXB
hXB
h>B
i_B
i_B
iDB
jKB
jKB
jeB
jeB
jKB
kkB
kkB
lqB
lWB
lWB
lqB
kkB
lqB
lqB
lqB
lWB
lqB
lqB
lqB
lqB
lW11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.27(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201806180044452018061800444520180618004445201806190033462018061900334620180619003346JA  ARFMdecpA19c                                                                20180613063503  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180612213510  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180612213511  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180612213511  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180612213512  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180612213512  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180612213512  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180612213512  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180612213512  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180612213512                      G�O�G�O�G�O�                JA  ARUP                                                                        20180612215501                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180613153729  CV  JULD            G�O�G�O�F�O}                JM  ARCAJMQC2.0                                                                 20180617154445  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180617154445  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180618153346  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021520                      G�O�G�O�G�O�                