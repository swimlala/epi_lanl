CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-03-23T18:35:41Z creation;2018-03-23T18:35:44Z conversion to V3.1;2019-12-23T06:24:54Z update;     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20180323183541  20200120021520  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               0A   JA  I2_0675_048                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�U��d  1   @�U�UUU�@6�($x�b��(�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�
=@�p�AQ�A$Q�ADQ�AdQ�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B	{B{B{B!{B){B1{B9{BA{BI{BQ{BY{Ba{Bi{Bq{By{B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=BĊ=BȊ=B̊=BЊ=BԊ=B؊=B܊=B��=B�=B�=B�=B��=B�=B��=B��=C ECECECECEC
ECECECECECECECECECECEC EC"EC$EC&EC(EC*EC,EC.EC0EC2EC4EC6EC8EC:EC<EC>EC@ECBECDECFECHECJECLECNECPECRECTECVECXECZEC\EC^EC`ECbECdECfEChECjEClECnECpECrECtECvECxECzEC|EC~EC�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDxHDx�HDyHDy�HDzHDz�HD{HD{�HD|HD|�HD}HD}�HD~HD~�HDHD�HD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D���D��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD�qD�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D���D��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D�D�ȤD��D�H�DÈ�D�ȤD��D�H�DĈ�D�ȤD��D�H�Dň�D�ȤD��D�H�Dƈ�D�ȤD��D�H�Dǈ�D�ȤD��D�H�DȈ�D�ȤD��D�H�DɈ�D�ȤD��D�H�Dʈ�D�ȤD��D�H�Dˈ�D�ȤD��D�H�D̈�D�ȤD��D�H�D͈�D�ȤD��D�H�DΈ�D�ȤD��D�H�Dψ�D�ȤD��D�H�DЈ�D�ȤD��D�H�Dш�D�ȤD��D�H�D҈�D�ȤD��D�H�Dӈ�D�ȤD��D�H�DԈ�D�ȤD��D�H�DՈ�D�ȤD��D�H�Dֈ�D�ȤD��D�H�D׈�D�ȤD��D�H�D؈�D�ȤD��D�H�Dو�D�ȤD��D�H�Dڈ�D�ȤD��D�H�Dۈ�D�ȤD��D�H�D܈�D�ȤD��D�H�D݈�D�ȤD��D�H�Dވ�D�ȤD��D�H�D߈�D�ȤD��D�H�D���D�ȤD��D�H�DሤD�ȤD��D�H�D∤D�ȤD��D�H�D��D�ȤD��D�H�D䈤D�ȤD��D�H�D判D�ȤD��D�H�D戤D�ȤD��D�H�D爤D�ȤD��D�H�D舤D�ȤD��D�H�D鈤D�ȤD��D�H�DꈤD�ȤD��D�H�D눤D�ȤD��D�H�D숤D�ȤD��D�H�D툤D�ȤD��D�H�DD�ȤD��D�H�DD�ȤD��D�H�D���D�ȤD��D�H�D�D�ȤD��D�H�D�D�ȤD��D�H�D�D�ȤD��D�H�D�D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�M�A�XA�XA�VA�XA�Q�A�VA�S�A�XA�^5A�`BA�`BA�l�A�n�A�n�A�n�A�l�A�n�A�n�A�p�A�p�A�p�A�r�A�t�A�v�A�x�A�|�A�z�A�x�A�v�A�x�A�z�A�v�A�x�A�x�A�x�A�z�A�z�A�"�A���A�ƨA��wA��A�E�A��HA���A�\)A�bA�~�A��+A���A��A�|�A�jA�\)A�E�A�%A�A�p�A��A�|�A���A��/A�(�A���A��A��A���A�33A��9A�ƨA��A��RA���A�ffA���A���A�ffA�n�A���A�%A�`BA���A�r�A��A���A��A�5?A���A�VA��jA��jA�n�A��yA��^A���A���A���A���A�O�A�M�A�1A��uA�bA�VA�l�A���A�n�A�^5A� �A�A���A~bNA|z�Ay/Aw�Aw�Aw�7AwXAu�Aq%An��Ai��Ab�uA]��AZ��AXĜAVI�AQ��API�AN$�AL��AKS�AI�FAG+AF�AE
=AD9XABĜAA�^AA33A@ĜA@M�A>��A=XA:r�A8JA6�9A5p�A4��A4ffA3�-A1"�A0ZA.$�A-7LA,�HA,A+K�A+�FA*5?A(v�A(  A%�-A#�A#��A!�#A A�A�yAr�AffA��A��AbA%A��AbA�A�A^5A\)A�AĜA^5AhsAz�AffA�A�PA$�A
=AbAC�A�!A(�A��Al�A
��A
=A|�A
=A
��A
~�A
JA	��A	t�A��A�PAv�A��AC�A�;A��A9XA�hA Q�@��F@��@�=q@�O�@���@���@�Z@�  @��T@� �@�1@�F@�{@�V@�%@��m@�-@�!@�9X@�K�@ާ�@�/@�dZ@�5?@�?}@�G�@�?}@؃@�+@֧�@�n�@�$�@���@��@�33@���@��/@θR@��@��@�Z@�33@�J@�@�&�@ǥ�@��@��@��/@ă@�Z@��@�E�@�O�@��`@��/@���@�Ĝ@��9@��D@�1'@��F@�|�@��@���@��y@���@�=q@��7@��9@���@�-@�G�@���@��j@�A�@�+@��@��@��/@�Z@�  @�ƨ@���@�+@�
=@��R@�n�@�v�@�^5@��7@�X@��@���@�dZ@��@��R@�{@���@���@��w@�ff@��#@�X@�Ĝ@�V@�p�@�?}@���@��9@��@�bN@�A�@��@�33@���@�ȴ@�E�@��^@��h@�/@���@�j@�(�@�  @���@��@��\@�n�@�5?@�J@���@��@��@���@�x�@�X@��/@�b@��w@��@�C�@�+@�@��@�~�@�$�@��^@�7L@���@��`@���@��@�bN@���@��@���@�+@���@�~�@�E�@��@���@��-@��7@�hs@�?}@�%@�%@��@��j@��@�z�@�Z@�(�@��m@���@��w@��@���@��P@�S�@�
=@��y@�ȴ@��+@�M�@�5?@�J@��@��-@�x�@�G�@�/@�V@���@��9@���@��@�A�@�  @�ƨ@��F@���@�\)@��@�o@���@�E�@�5?@��@��@��^@��h@�O�@�&�@�%@���@��@���@�Ĝ@��@��D@�bN@�9X@� �@�1@���@��w@��F@���@��P@�l�@�\)@�;d@���@��@�ȴ@���@��R@��R@��\@�n�@�J@���@��^@��-@���@��@�`B@�&�@�Ĝ@�z�@�Z@�A�@�1'@�b@��
@��@�o@��!@�~�@�E�@�5?@���@���@�x�@�7L@�&�@�V@��@�Ĝ@��@�A�@���@���@��@�;d@��@���@���@��+@�ff@�=q@�@��^@���@�O�@���@��D@�j@�(�@�w@�P@|�@�@~�@~��@~E�@}@}�@|��@|��@|z�@|1@{�F@{�@z�H@z=q@y�#@y��@yG�@x��@xr�@x  @w��@w;d@v��@vv�@vE�@v{@u�T@u�-@u`B@u/@t�/@tj@t(�@sƨ@s��@s��@sC�@s"�@r�@r~�@r^5@r�@q��@qx�@qG�@q&�@p�`@pr�@o�w@ol�@oK�@o+@o
=@n�@nv�@m��@l�@l�/@l�/@l��@l��@l�j@lI�@l�@l�@l1@k�F@kS�@j�!@jn�@j�\@j�\@j�@ix�@h��@hĜ@h1'@gl�@f��@f��@f$�@e@ep�@d�@dj@d�@c��@cS�@c33@c@b��@bM�@a��@a�^@ax�@aX@a&�@a%@`��@`�`@`��@`r�@_��@_+@^�@^ȴ@^�+@^5?@]�@]�-@]�h@]`B@\�@\j@\Z@\9X@\�@[ƨ@[C�@Z�H@Z��@Zn�@Z-@Z�@Y�#@Yx�@YG�@Y&�@Y%@X��@X��@X��@X�u@XbN@W�;@W�@W|�@W
=@V�y@V�+@V{@U@U�h@Up�@U/@T�@T�D@TI�@T�@S��@Sƨ@S��@St�@S33@R��@RM�@Q��@Qx�@Qhs@QX@Q&�@P�9@PQ�@PA�@O\)@N�y@N��@N�+@Nv�@NV@N{@M�T@M�-@M�@L��@L�@L�@L�D@Lj@LI�@L9X@K�F@Ko@Jn�@J�@I��@I�^@Ihs@IG�@H��@HbN@G�;@G;d@G
=@F��@F��@F5?@E��@E�h@E?}@EV@D��@D�D@DZ@D�@CS�@Co@B�@B��@A�@A��@@��@@bN@?�@?�P@?;d@?
=@>�R@>@=�-@=�@=`B@=/@=�@<��@<j@<9X@;�m@;��@;dZ@;33@;"�@;o@:�!@:-@9�#@9��@9x�@97L@9&�@9%@8�9@8 �@7��@7K�@6�y@6ff@6$�@5`B@4�@4j@49X@3��@3�m@3�m@3�
@3�F@3��@3dZ@3C�@3"�@2��@2~�@2M�@2=q@2-@2J@1�#@1��@1hs@1&�@0�9@0�u@0Q�@0b@/��@/l�@/;d@/+@/+@.��@.��@.ff@.V@.V@.@-@-@-@-`B@-V@,��@,��@,�@,��@,�D@,(�@+�
@+S�@+"�@*�H@*��@*n�@)�@)��@)hs@)G�@(��@(��@(�u@(bN@'�;@'��@'l�@'K�@'+@&�y@&�R@&v�@&V@&{@%�@%�@%�T@%��@%�@%�@$�/@$�/@$z�@#��@#�F@#��@#�@#�@#t�@#33@#@#@"��@"�\@"^5@"-@!�@!�#@!�7@!�@!%@ ��@ ��@ Ĝ@ �@ Q�@ A�@  �@�@�@|�@\)@;d@�@�@�R@v�@E�@{@�-@p�@?}@/@��@��@�j@z�@I�@�@�
@�F@��@�@t�@dZ@dZ@S�@C�@33@@�@��@-@��@��@�7@7L@��@Ĝ@��@A�@��@\)@;d@;d@;d@�@��@�y@ȴ@�+@ff@E�@5?@@@�@�@��@�@�D@z�@j@Z@Z@(�@t�@dZ@C�@�@n�@J@��@��@x�@hs@G�@�@�`@Ĝ@�u@Q�@1'@ �@b@�;@��@�P@K�@�@ȴ@��@ff@V@5?@5?@$�@�@�@�@�T@�T@�T@p�@`B@O�@��@z�@I�@(�@1@�m@�
@�F@��@t�@@
�\@
~�@
M�@	��@	��@	��@	�7@	hs@	X11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�M�A�XA�XA�VA�XA�Q�A�VA�S�A�XA�^5A�`BA�`BA�l�A�n�A�n�A�n�A�l�A�n�A�n�A�p�A�p�A�p�A�r�A�t�A�v�A�x�A�|�A�z�A�x�A�v�A�x�A�z�A�v�A�x�A�x�A�x�A�z�A�z�A�"�A���A�ƨA��wA��A�E�A��HA���A�\)A�bA�~�A��+A���A��A�|�A�jA�\)A�E�A�%A�A�p�A��A�|�A���A��/A�(�A���A��A��A���A�33A��9A�ƨA��A��RA���A�ffA���A���A�ffA�n�A���A�%A�`BA���A�r�A��A���A��A�5?A���A�VA��jA��jA�n�A��yA��^A���A���A���A���A�O�A�M�A�1A��uA�bA�VA�l�A���A�n�A�^5A� �A�A���A~bNA|z�Ay/Aw�Aw�Aw�7AwXAu�Aq%An��Ai��Ab�uA]��AZ��AXĜAVI�AQ��API�AN$�AL��AKS�AI�FAG+AF�AE
=AD9XABĜAA�^AA33A@ĜA@M�A>��A=XA:r�A8JA6�9A5p�A4��A4ffA3�-A1"�A0ZA.$�A-7LA,�HA,A+K�A+�FA*5?A(v�A(  A%�-A#�A#��A!�#A A�A�yAr�AffA��A��AbA%A��AbA�A�A^5A\)A�AĜA^5AhsAz�AffA�A�PA$�A
=AbAC�A�!A(�A��Al�A
��A
=A|�A
=A
��A
~�A
JA	��A	t�A��A�PAv�A��AC�A�;A��A9XA�hA Q�@��F@��@�=q@�O�@���@���@�Z@�  @��T@� �@�1@�F@�{@�V@�%@��m@�-@�!@�9X@�K�@ާ�@�/@�dZ@�5?@�?}@�G�@�?}@؃@�+@֧�@�n�@�$�@���@��@�33@���@��/@θR@��@��@�Z@�33@�J@�@�&�@ǥ�@��@��@��/@ă@�Z@��@�E�@�O�@��`@��/@���@�Ĝ@��9@��D@�1'@��F@�|�@��@���@��y@���@�=q@��7@��9@���@�-@�G�@���@��j@�A�@�+@��@��@��/@�Z@�  @�ƨ@���@�+@�
=@��R@�n�@�v�@�^5@��7@�X@��@���@�dZ@��@��R@�{@���@���@��w@�ff@��#@�X@�Ĝ@�V@�p�@�?}@���@��9@��@�bN@�A�@��@�33@���@�ȴ@�E�@��^@��h@�/@���@�j@�(�@�  @���@��@��\@�n�@�5?@�J@���@��@��@���@�x�@�X@��/@�b@��w@��@�C�@�+@�@��@�~�@�$�@��^@�7L@���@��`@���@��@�bN@���@��@���@�+@���@�~�@�E�@��@���@��-@��7@�hs@�?}@�%@�%@��@��j@��@�z�@�Z@�(�@��m@���@��w@��@���@��P@�S�@�
=@��y@�ȴ@��+@�M�@�5?@�J@��@��-@�x�@�G�@�/@�V@���@��9@���@��@�A�@�  @�ƨ@��F@���@�\)@��@�o@���@�E�@�5?@��@��@��^@��h@�O�@�&�@�%@���@��@���@�Ĝ@��@��D@�bN@�9X@� �@�1@���@��w@��F@���@��P@�l�@�\)@�;d@���@��@�ȴ@���@��R@��R@��\@�n�@�J@���@��^@��-@���@��@�`B@�&�@�Ĝ@�z�@�Z@�A�@�1'@�b@��
@��@�o@��!@�~�@�E�@�5?@���@���@�x�@�7L@�&�@�V@��@�Ĝ@��@�A�@���@���@��@�;d@��@���@���@��+@�ff@�=q@�@��^@���@�O�@���@��D@�j@�(�@�w@�P@|�@�@~�@~��@~E�@}@}�@|��@|��@|z�@|1@{�F@{�@z�H@z=q@y�#@y��@yG�@x��@xr�@x  @w��@w;d@v��@vv�@vE�@v{@u�T@u�-@u`B@u/@t�/@tj@t(�@sƨ@s��@s��@sC�@s"�@r�@r~�@r^5@r�@q��@qx�@qG�@q&�@p�`@pr�@o�w@ol�@oK�@o+@o
=@n�@nv�@m��@l�@l�/@l�/@l��@l��@l�j@lI�@l�@l�@l1@k�F@kS�@j�!@jn�@j�\@j�\@j�@ix�@h��@hĜ@h1'@gl�@f��@f��@f$�@e@ep�@d�@dj@d�@c��@cS�@c33@c@b��@bM�@a��@a�^@ax�@aX@a&�@a%@`��@`�`@`��@`r�@_��@_+@^�@^ȴ@^�+@^5?@]�@]�-@]�h@]`B@\�@\j@\Z@\9X@\�@[ƨ@[C�@Z�H@Z��@Zn�@Z-@Z�@Y�#@Yx�@YG�@Y&�@Y%@X��@X��@X��@X�u@XbN@W�;@W�@W|�@W
=@V�y@V�+@V{@U@U�h@Up�@U/@T�@T�D@TI�@T�@S��@Sƨ@S��@St�@S33@R��@RM�@Q��@Qx�@Qhs@QX@Q&�@P�9@PQ�@PA�@O\)@N�y@N��@N�+@Nv�@NV@N{@M�T@M�-@M�@L��@L�@L�@L�D@Lj@LI�@L9X@K�F@Ko@Jn�@J�@I��@I�^@Ihs@IG�@H��@HbN@G�;@G;d@G
=@F��@F��@F5?@E��@E�h@E?}@EV@D��@D�D@DZ@D�@CS�@Co@B�@B��@A�@A��@@��@@bN@?�@?�P@?;d@?
=@>�R@>@=�-@=�@=`B@=/@=�@<��@<j@<9X@;�m@;��@;dZ@;33@;"�@;o@:�!@:-@9�#@9��@9x�@97L@9&�@9%@8�9@8 �@7��@7K�@6�y@6ff@6$�@5`B@4�@4j@49X@3��@3�m@3�m@3�
@3�F@3��@3dZ@3C�@3"�@2��@2~�@2M�@2=q@2-@2J@1�#@1��@1hs@1&�@0�9@0�u@0Q�@0b@/��@/l�@/;d@/+@/+@.��@.��@.ff@.V@.V@.@-@-@-@-`B@-V@,��@,��@,�@,��@,�D@,(�@+�
@+S�@+"�@*�H@*��@*n�@)�@)��@)hs@)G�@(��@(��@(�u@(bN@'�;@'��@'l�@'K�@'+@&�y@&�R@&v�@&V@&{@%�@%�@%�T@%��@%�@%�@$�/@$�/@$z�@#��@#�F@#��@#�@#�@#t�@#33@#@#@"��@"�\@"^5@"-@!�@!�#@!�7@!�@!%@ ��@ ��@ Ĝ@ �@ Q�@ A�@  �@�@�@|�@\)@;d@�@�@�R@v�@E�@{@�-@p�@?}@/@��@��@�j@z�@I�@�@�
@�F@��@�@t�@dZ@dZ@S�@C�@33@@�@��@-@��@��@�7@7L@��@Ĝ@��@A�@��@\)@;d@;d@;d@�@��@�y@ȴ@�+@ff@E�@5?@@@�@�@��@�@�D@z�@j@Z@Z@(�@t�@dZ@C�@�@n�@J@��@��@x�@hs@G�@�@�`@Ĝ@�u@Q�@1'@ �@b@�;@��@�P@K�@�@ȴ@��@ff@V@5?@5?@$�@�@�@�@�T@�T@�T@p�@`B@O�@��@z�@I�@(�@1@�m@�
@�F@��@t�@@
�\@
~�@
M�@	��@	��@	��@	�7@	hs@	X11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�\B�VB�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�VB�VB�VB�VB�\B�\B�\B�\B�\B�\B�\B�bB�bB�bB�hB�hB�hB�\B�bB�hB�\B�\B�\B�hB�bB�\B�dB�`B��B��B��BB�B(�B5?B<jB5?B�BBP�B`BB^5B]/B\)B[#B[#B_;B]/BQ�BF�B49B+BJB��B��BDBB�B.B9XB?}B6FB1'B,B$�B�B�BbB+B��B�B�B�sB�B��B��B�RB�B�\B|�Bv�BaHB@�BDB
��B
��B
�B
�mB
�B
�fB
�/B
��B
�3B
��B
�JB
p�B
^5B
G�B
'�B	�B	�B	ɺB	�LB	�B	�B	�B	�B	��B	{�B	dZB	H�B	uB�B�
BȴB�^B��B�uB�B~�Bx�Bl�B_;BW
BP�BM�BJ�BH�BH�BG�BE�B@�B;dB<jB9XB1'B.B,B+B,B.B=qB?}BI�BG�B@�B:^BB�B?}BF�BH�BE�BD�BB�BA�B>wB<jBD�BD�BG�BQ�B\)B^5B]/B[#BZB\)BZBZB]/B^5B^5B^5B\)BT�BQ�BL�B@�B:^B9XB7LB6FB6FB7LB8RB7LB:^B?}BM�BQ�BQ�BP�BK�BH�BJ�BH�BC�B9XB5?B6FB49B33B1'B0!B.B-B,B+B,B+B-B)�B(�B,B+B)�B)�B+B/B.B/B1'B33B49B49B6FB9XB:^B:^B;dB:^B:^B;dB<jB;dB;dB;dB<jB=qB=qB=qB<jB=qB<jB=qB=qB=qB>wB>wB>wB>wB<jB=qBB�BC�BB�BC�BE�BG�BI�BJ�BK�BK�BK�BK�BL�BM�BN�BT�B\)B_;BbNBdZBgmBjBl�By�B{�B}�B�B�%B�PB�\B�uB��B��B��B��B��B��B��B�B�!B�-B�FB�XB�XB�wB��BBÖBƨBɺB��B��B��B�B�
B�B�5B�HB�sB�yB�B�B�B�B�B�B�B��B��B��B��B��B	B	%B	%B	%B	+B	1B	JB	bB	hB	oB	uB	{B	�B	�B	"�B	'�B	)�B	,B	.B	.B	/B	0!B	1'B	2-B	49B	8RB	;dB	A�B	G�B	I�B	J�B	J�B	M�B	Q�B	ZB	]/B	]/B	`BB	cTB	ffB	jB	n�B	r�B	s�B	u�B	w�B	w�B	y�B	y�B	{�B	|�B	�B	�B	�B	�B	�+B	�1B	�7B	�=B	�JB	�JB	�VB	�bB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�9B	�9B	�?B	�FB	�RB	�XB	�^B	�dB	�jB	�qB	�qB	�wB	��B	��B	��B	��B	B	ÖB	ÖB	ŢB	ŢB	ŢB	ŢB	ƨB	ƨB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�)B	�5B	�BB	�HB	�NB	�NB	�ZB	�`B	�fB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
1B
	7B
	7B
	7B

=B

=B
DB
DB
DB
JB
JB
JB
PB
PB
PB
VB
VB
VB
\B
\B
\B
bB
bB
bB
bB
bB
bB
hB
hB
hB
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
{B
uB
uB
{B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
,B
,B
,B
-B
,B
-B
-B
.B
.B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
49B
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
6FB
6FB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
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
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
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
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
VB
VB
VB
VB
VB
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
YB
ZB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
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
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
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
ffB
gmB
gmB
gmB
gmB
gmB
hsB
gmB
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
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
n�B
n�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�BB�<B�BB�BB�BB�BB�BB�BB�BB�BB�BB�BB�<B�<B�<B�<B�BB�BB�BB�BB�BB�BB�BB�HB�HB�HB�NB�NB�NB�BB�HB�NB�BB�BB�BB�NB�HB�BB�JB�FB��B��B��B�BB(�B5%B<PB5%B�B�BP�B`'B^B]B\B[	B[	B_!B]BQ�BF�B4B*�B0B��B��B)BB�B-�B9>B?cB6+B1B+�B$�B�BsBHBB��B�}B�wB�XB��BˬB�iB�8B��B�BB|�Bv�Ba-B@iB)B
��B
��B
�wB
�RB
�eB
�LB
�B
�oB
�B
��B
�0B
p�B
^B
G�B
'�B	�B	��B	ɠB	�2B	��B	��B	��B	��B	��B	{�B	d@B	H�B	[B�vB��BȚB�DB��B�[B�B~�Bx�BlWB_!BV�BP�BM�BJ�BH�BH�BG�BE�B@iB;JB<PB9>B1B-�B+�B*�B+�B-�B=VB?cBI�BGzB@iB:DBB[B?HBF�BH�BE�BD�BB[BAoB>]B<PBD�BD�BGzBQ�B\B^B]B[	BZB\BZBZB]B^B^B^B\BT�BQ�BL�B@OB:*B9$B7B6+B6+B72B88B7B:DB?cBM�BQ�BQ�BP�BK�BH�BJ�BH�BC{B9>B5%B6B4B2�B1B0B-�B,�B+�B*�B+�B*�B,�B)�B(�B+�B*�B)�B)�B*�B/ B-�B/ B1B2�B4B4B6+B9$B:*B:*B;0B:DB:DB;JB<6B;JB;JB;JB<PB=VB=VB=VB<PB=VB<6B=VB=VB=<B>]B>]B>BB>]B<PB=VBBuBC{BB[BCaBE�BGzBI�BJ�BK�BK�BK�BK�BL�BM�BN�BT�B\B_!Bb4Bd&Bg8BjeBlWBy�B{�B}�B��B��B�6B�(B�[B�MB�yB��B��B��B��B��B��B��B�B�+B�$B�>B�BB�oB�uB�{B�tBɆB͹B��B��B��B��B��B�B�-B�>B�DB�WB�cB�}B�B�B�B�B��B��B��B��B��B	�B	�B	B	�B	�B	�B	0B	HB	NB	TB	[B	FB	mB	�B	"�B	'�B	)�B	+�B	-�B	-�B	.�B	0B	0�B	1�B	4B	8B	;0B	AoB	G�B	I�B	J�B	J�B	M�B	Q�B	Y�B	]B	]B	`B	c B	fLB	jeB	n}B	r�B	s�B	u�B	w�B	w�B	y�B	y�B	{�B	|�B	��B	��B	��B	��B	��B	�B	�B	�#B	�B	�0B	�<B	�HB	�4B	�:B	�@B	�mB	�mB	�sB	�_B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	��B	��B	�B	�B	�%B	�+B	�8B	�$B	�DB	�0B	�6B	�VB	�<B	�]B	�iB	�iB	�oB	�oB	�uB	�aB	�aB	ňB	ňB	ňB	�mB	ƎB	�tB	ǔB	ǔB	ɆB	ʌB	ʦB	ʦB	ˬB	ˬB	̘B	̳B	οB	��B	бB	бB	��B	ҽB	ҽB	��B	��B	��B	��B	��B	��B	�B	�	B	��B	�B	�B	�B	�B	�B	�@B	�,B	�2B	�8B	�8B	�XB	�_B	�_B	�eB	�QB	�kB	�QB	�qB	�wB	�B	�iB	��B	�B	�|B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
B
B
�B
�B
	B
	B
	B

#B

#B
)B
)B
B
0B
0B
0B
6B
6B
6B
<B
<B
<B
(B
BB
(B
HB
.B
.B
HB
HB
HB
NB
NB
NB
TB
:B
:B
TB
:B
TB
TB
[B
[B
FB
aB
[B
[B
FB
aB
aB
aB
aB
aB
aB
gB
MB
gB
SB
mB
sB
YB
yB
_B
eB
B
B
B
B
�B
qB
�B
�B
�B
�B
�B
qB
�B
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
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
*�B
+�B
+�B
+�B
,�B
+�B
,�B
,�B
-�B
-�B
/ B
/ B
/�B
0B
0B
/�B
0B
0B
0B
0�B
1B
1B
1B
2B
2B
1�B
2B
2�B
3B
4B
4B
4B
4B
4B
5%B
5%B
5B
6+B
6B
6B
6+B
6B
72B
7B
88B
8B
8B
8B
9>B
9>B
9$B
:DB
:DB
:DB
:DB
;JB
;JB
<6B
=VB
=<B
=VB
>BB
>BB
>]B
?cB
?cB
?cB
?HB
@iB
@OB
@OB
@OB
@OB
AoB
AUB
AoB
AoB
AoB
AoB
BuB
BuB
B[B
C{B
C{B
C{B
CaB
C{B
D�B
DgB
DgB
D�B
E�B
EmB
F�B
F�B
G�B
GzB
GzB
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
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
U�B
U�B
U�B
U�B
U�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
Y�B
X�B
ZB
ZB
ZB
ZB
Y�B
Y�B
Y�B
[	B
[	B
[	B
[	B
[	B
[	B
Z�B
\B
[�B
\B
\B
[�B
[�B
]B
]B
\�B
]B
]B
^B
^B
^B
^B
^B
_!B
_B
_B
_!B
`'B
`'B
`B
`B
`B
a-B
a-B
a-B
a-B
a-B
a-B
aB
a-B
a-B
aB
a-B
a-B
aB
b4B
b4B
b4B
c:B
c B
c:B
d&B
d&B
d&B
d@B
eFB
eFB
fLB
fLB
fLB
fLB
f2B
fLB
f2B
fLB
f2B
fLB
f2B
gRB
gRB
gRB
gRB
g8B
hXB
gRB
hXB
hXB
hXB
hXB
hXB
hXB
h>B
h>B
i_B
iDB
jKB
jeB
jKB
jeB
jKB
jeB
kQB
kkB
kkB
kkB
lqB
lqB
lqB
lWB
lWB
lqB
lqB
lWB
m]B
mwB
mwB
mwB
mwB
n}B
n}B
ncB
ncB
ncB
ncB
n}B
n}B
n}B
o�B
ncB
ncB
oiB
p�B
p�B
poB
p�B
p�B
p�B
q�B
qvB
q�B
q�B
r|B
r�B
r|B
r�B
r�B
r�B
s�B
s�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.27(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201803290041002018032900410020180329004100201804060311442018040603114420180406031144JA  ARFMdecpA19c                                                                20180324033538  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180323183541  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180323183542  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180323183542  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180323183543  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180323183543  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180323183543  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180323183543  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180323183544  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180323183544                      G�O�G�O�G�O�                JA  ARUP                                                                        20180323185655                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180324154136  CV  JULD            G�O�G�O�F­                JM  ARGQJMQC2.0                                                                 20180324154136  CV  JULD_LOCATION   G�O�G�O�F­.                JM  ARGQJMQC2.0                                                                 20180324154136  CV  LATITUDE        G�O�G�O�A��y                JM  ARGQJMQC2.0                                                                 20180324154136  CV  LONGITUDE       G�O�G�O��v�                JM  ARCAJMQC2.0                                                                 20180328154100  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180328154100  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180405181144  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021520                      G�O�G�O�G�O�                