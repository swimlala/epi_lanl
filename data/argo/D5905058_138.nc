CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-04-12T03:37:04Z creation;2019-04-12T03:37:08Z conversion to V3.1;2019-12-23T06:03:58Z update;     
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �H   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �H   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �H   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �H   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20190412033704  20200120031517  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_138                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @ص�W
=�1   @ص��l @8��PH�c.=p��
1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @&ff@�  @�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cq�fCt  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D�3D��D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ�fD[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds�fDtfDt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D��3D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�3D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�9�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @7�@���@ȣ�AQ�A$Q�ADQ�AdQ�A�\)A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B	{B{B{B!{B){B1{B9{BA{BI{BQ{BY{Ba{Bi{Bq{By{B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=BĊ=BȊ=B̊=BЊ=BԊ=B؊=B܊=B��=B�=B�=B�=B��=B�=B��=B��=C ECECECECEC
ECECECECECECECECECECEC EC"EC$EC&EC(EC*EC,EC.EC0EC2EC4EC6EC8EC:EC<EC>EC@ECBECDECFECHECJECLECNECPECRECTECVECXECZEC\^�C^EC`ECbECdECfEChECjEClECnECpECr+�CtECvECxECzEC|EC~EC�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�D HD �HDHD�HDHD�HDHD�HDHD�{D
�D�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD�D�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ��D[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs��Dt�Dt�HDuHDu�HDvHDv�HDwHDw�HDxHDx�HDyHDy�HDzHDz�HD{HD{�HD|HD|�HD}HD}�HD~HD~�HDHD�HD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�EqD���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D��qD�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�EqD���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D�D���D��D�H�DÈ�D�ȤD��D�H�DĈ�D�ȤD��D�H�Dň�D�ȤD��D�H�Dƈ�D�ȤD��D�H�Dǈ�D�ȤD��D�H�DȈ�D�ȤD��D�H�DɈ�D�ȤD��D�H�Dʈ�D�ȤD��D�H�Dˈ�D�ȤD��D�H�D̈�D�ȤD��D�H�D͈�D�ȤD��D�H�DΈ�D�ȤD��D�H�Dψ�D�ȤD��D�H�DЈ�D�ȤD��D�H�Dш�D�ȤD��D�H�D҈�D�ȤD��D�H�Dӈ�D�ȤD��D�H�DԈ�D�ȤD��D�H�DՈ�D�ȤD��D�H�Dֈ�D�ȤD��D�H�D׈�D�ȤD��D�H�D؈�D�ȤD��D�H�Dو�D�ȤD��D�H�Dڈ�D�ȤD��D�H�Dۈ�D�ȤD��D�H�D܈�D�ȤD��D�H�D݈�D�ȤD��D�H�Dވ�D�ȤD��D�H�D߈�D�ȤD��D�H�D���D�ȤD��D�H�DሤD�ȤD��D�H�D∤D�ȤD��D�H�D㈤D�ȤD��D�H�D䈤D�ȤD��D�H�D判D�ȤD��D�H�D戤D�ȤD��D�H�D爤D�ȤD��D�H�D舤D�ȤD��D�H�D鈤D�ȤD��D�H�DꈤD�ȤD��D�H�D눤D�ȤD��D�H�D숤D�ȤD��D�H�D툤D�ȤD��D�H�DD�ȤD��D�H�DD�ȤD��D�H�D���D�ȤD��D�H�D�D�ȤD��D�H�D�D�ȤD��D�H�D�D�ȤD��D�H�D�D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�H�D���D�ȤD��D�B=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�?}A�7LA�-A�5?A�/A�VA��A�A� �A�oA�oA�ĜA��+A�7LA�JA���A�ffA�bA���A��A���A��!A��/A�r�A�jA�G�A��wA��uA�n�A�G�A�"�A�5?A�VA��PA�&�A��A���A�n�A��A��9A�dZA�{A���A�ĜA�M�A�ȴA���A��PA�Q�A��TA���A��jA�C�A���A�z�A�^5A�bA�`BA�M�A���A���A�&�A��A�~�A���A�+A��A�
=A��A�\)A�K�A���A�ȴA��A�E�A��mA�G�A�ƨA��A�I�A��\A��HA���A~�`A}��A|�yAy�
At9XAp�Aj��AihsAi/Ah�Ag��Afz�Ad�HAb�/AbjAa�Aa33A`�/A_C�A]33A\�uA[�-AX(�AU��ASK�AO`BAH�!ADbAB^5A@�`A@M�A?�A>$�A<9XA;+A8��A85?A6��A5/A5oA4�A4�9A4r�A4-A3|�A2�uA05?A/VA.JA-�hA,�A+�#A*=qA)�hA(=qA'��A'K�A&��A&1A$�`A$�A#��A"�yA!K�A 5?A\)A��AC�AffAƨA-AbA$�A(�A$�A�TAA�A��A&�A��AQ�A  A%A�+A��A\)A33A�A�A�wAC�A��A�7A;dA�DA=qA��A%A
$�A	p�AȴAA��A33A5?A�7AbNA"�@���@�;d@���@�7L@��u@��7@�o@�x�@��@�@�{@�X@�(�@��T@��@�z�@��
@��/@�n�@�p�@��@��@���@�@�9@��@�bN@���@��
@۝�@�K�@���@�@�&�@���@�j@�9X@ָR@�=q@Չ7@� �@�33@�o@���@Ұ!@�ȴ@�K�@�\)@ӍP@�V@���@� �@Ο�@�7L@̼j@�j@˾w@�K�@�~�@��@�(�@���@Ə\@�-@ź^@�?}@�  @�"�@�@���@�$�@��/@��
@��@��+@�@�hs@�G�@�G�@�?}@���@��@�Q�@�ƨ@�33@�$�@�&�@��F@���@��j@��;@�+@��H@��R@�n�@�J@���@�/@���@��F@�\)@�@��!@��+@�5?@�J@��^@��h@�(�@�+@�ff@��@�@��7@�hs@��@�Q�@�$�@�?}@���@�5?@�p�@�?}@��j@�1@��@�"�@�+@��w@��;@��@��@��H@��!@�E�@�J@��h@�X@�`B@��@�/@�&�@��`@�I�@�(�@�  @���@��F@��@�l�@�33@���@�ȴ@���@�~�@�v�@�n�@�v�@�v�@�n�@�V@�V@�M�@�5?@�$�@��T@��^@��7@�`B@��@��@���@��@��;@��;@��m@��m@��m@���@��w@��F@���@��P@�|�@�C�@���@���@�-@��^@�x�@�?}@�V@���@�z�@�A�@�(�@�  @�b@��@� �@���@��
@��F@���@��w@���@�=q@�5?@�J@�@���@�X@��@�Q�@��@��@��@���@�;d@�o@���@��!@��\@�5?@��-@��h@�p�@�`B@�X@�?}@���@���@�r�@�I�@�1@�ƨ@�l�@�C�@�"�@��y@��H@���@�ȴ@���@�n�@�^5@�5?@�@��T@��@��@��T@�@�O�@�&�@��@�%@���@���@��j@��u@�Z@�I�@�A�@�9X@�(�@��@���@��
@��w@��@���@��P@�\)@�S�@�K�@�33@��y@���@���@�^5@�V@�{@���@���@���@��h@��@�hs@�X@�/@��`@��j@�z�@�Q�@�1'@�(�@�(�@�  @�ƨ@��P@�l�@�K�@�"�@�@��y@���@���@�v�@�^5@�J@�@���@�x�@�G�@��@�Ĝ@���@�Q�@�@��@l�@+@~�+@~E�@~$�@~$�@~@}��@}O�@}V@|��@|�/@|�j@|j@|9X@|(�@{�m@{��@{S�@z��@z^5@z�@y�@y��@yX@y7L@y&�@x��@x�9@x�u@x �@w��@w;d@v�@v��@v5?@u��@uV@t��@tI�@s�F@s"�@r��@rM�@q�#@qX@p��@pbN@o�;@o|�@n�@n$�@m�-@l��@l��@lz�@lj@lZ@lI�@l�@kC�@j�H@j�\@j�@i��@i�7@i&�@h�u@h1'@g�;@g�@g+@g
=@f{@e�h@d�@d�@d9X@c�m@c�@b�\@b=q@ax�@aX@aG�@a7L@a%@`�u@`bN@`A�@`  @_�@_K�@^�+@]�T@]��@]p�@\��@\��@\j@\I�@[��@[�F@[�@[33@[@Z��@Z��@Zn�@Zn�@ZJ@Y�^@Yhs@Y%@X�`@X�9@XA�@W��@W\)@W
=@V�R@V5?@U�@U��@U��@UO�@T�@T�/@T�/@T�/@T�/@T��@T�@T��@Tj@S��@R�@R�!@R^5@R=q@R=q@R-@RJ@RJ@Q��@QG�@Q&�@Q�@Q%@PĜ@Pr�@PbN@P1'@O�@O��@O|�@O;d@N��@N5?@M�h@L��@L��@L�j@L�j@L�@L�@Lj@LI�@L1@K�@K@J��@J~�@J�\@JM�@J�@J�@I��@IG�@H��@H�9@H��@H�@HbN@HA�@H  @G��@G\)@GK�@GK�@G
=@Fȴ@F{@E�@EO�@E�@E�@D��@D��@Dz�@Cƨ@Ct�@C33@B�H@Bn�@A��@A��@A��@Ax�@A&�@@��@@A�@?�@?�w@?�@?��@?l�@>��@>ȴ@>��@>��@>��@>��@>ff@>V@=@=`B@=?}@<��@<��@<�D@<1@;��@;dZ@;"�@:�H@:�\@:^5@:J@9�7@9&�@8��@8A�@8b@7��@7K�@7
=@6�R@65?@6@5�T@5�@4��@4z�@41@3�
@3��@3�@3S�@3"�@2��@2~�@2=q@1��@1�7@1�@1�@1�@0��@0��@0Q�@0  @/�w@/�@/�P@/l�@/\)@/\)@/\)@/�@.ȴ@.5?@-�T@-�-@-�h@-`B@-�@,�@,�@,j@+�F@+�@+33@+@*�H@*��@*�!@*~�@*n�@*^5@*-@)��@)�^@)x�@)7L@(��@(Ĝ@(�u@(A�@'�@'\)@'�@&�@&��@&ff@&5?@%��@%p�@%?}@%V@$�/@$��@$�D@$(�@$1@#��@#�
@#��@#dZ@#33@"�!@!�#@!��@!G�@ �`@ �@ bN@ Q�@ Q�@ Q�@ A�@  �@   @�;@�w@��@|�@l�@\)@;d@�@��@E�@{@�@@?}@�@�j@�@z�@(�@1@ƨ@��@S�@�@�\@-@�@J@��@��@�^@�7@��@�9@r�@�w@|�@\)@K�@+@��@�@ȴ@��@�+@$�@��@p�@O�@/@��@�@�D@z�@I�@(�@�@�
@��@t�@dZ@C�@o@�H@�\@M�@-@J@��@�#@��@��@G�@�@�`@Q�@b@b@  @�;@��@�@�@��@��@|�@\)@K�@K�@K�@�@ȴ@��@E�@$�@$�@@�@�@�@�T@@��@�h@p�@`B@O�@?}@?}@?}@?}@�@��@��@z�@I�@(�@1@ƨ@��@�@t�@33@"�@o@@
��@
�\@
n�@
n�@
^5@
M�@
-@	�@	��@	�7@	x�@	x�@	hs@	X@	�@�`@��@��@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�?}A�7LA�-A�5?A�/A�VA��A�A� �A�oA�oA�ĜA��+A�7LA�JA���A�ffA�bA���A��A���A��!A��/A�r�A�jA�G�A��wA��uA�n�A�G�A�"�A�5?A�VA��PA�&�A��A���A�n�A��A��9A�dZA�{A���A�ĜA�M�A�ȴA���A��PA�Q�A��TA���A��jA�C�A���A�z�A�^5A�bA�`BA�M�A���A���A�&�A��A�~�A���A�+A��A�
=A��A�\)A�K�A���A�ȴA��A�E�A��mA�G�A�ƨA��A�I�A��\A��HA���A~�`A}��A|�yAy�
At9XAp�Aj��AihsAi/Ah�Ag��Afz�Ad�HAb�/AbjAa�Aa33A`�/A_C�A]33A\�uA[�-AX(�AU��ASK�AO`BAH�!ADbAB^5A@�`A@M�A?�A>$�A<9XA;+A8��A85?A6��A5/A5oA4�A4�9A4r�A4-A3|�A2�uA05?A/VA.JA-�hA,�A+�#A*=qA)�hA(=qA'��A'K�A&��A&1A$�`A$�A#��A"�yA!K�A 5?A\)A��AC�AffAƨA-AbA$�A(�A$�A�TAA�A��A&�A��AQ�A  A%A�+A��A\)A33A�A�A�wAC�A��A�7A;dA�DA=qA��A%A
$�A	p�AȴAA��A33A5?A�7AbNA"�@���@�;d@���@�7L@��u@��7@�o@�x�@��@�@�{@�X@�(�@��T@��@�z�@��
@��/@�n�@�p�@��@��@���@�@�9@��@�bN@���@��
@۝�@�K�@���@�@�&�@���@�j@�9X@ָR@�=q@Չ7@� �@�33@�o@���@Ұ!@�ȴ@�K�@�\)@ӍP@�V@���@� �@Ο�@�7L@̼j@�j@˾w@�K�@�~�@��@�(�@���@Ə\@�-@ź^@�?}@�  @�"�@�@���@�$�@��/@��
@��@��+@�@�hs@�G�@�G�@�?}@���@��@�Q�@�ƨ@�33@�$�@�&�@��F@���@��j@��;@�+@��H@��R@�n�@�J@���@�/@���@��F@�\)@�@��!@��+@�5?@�J@��^@��h@�(�@�+@�ff@��@�@��7@�hs@��@�Q�@�$�@�?}@���@�5?@�p�@�?}@��j@�1@��@�"�@�+@��w@��;@��@��@��H@��!@�E�@�J@��h@�X@�`B@��@�/@�&�@��`@�I�@�(�@�  @���@��F@��@�l�@�33@���@�ȴ@���@�~�@�v�@�n�@�v�@�v�@�n�@�V@�V@�M�@�5?@�$�@��T@��^@��7@�`B@��@��@���@��@��;@��;@��m@��m@��m@���@��w@��F@���@��P@�|�@�C�@���@���@�-@��^@�x�@�?}@�V@���@�z�@�A�@�(�@�  @�b@��@� �@���@��
@��F@���@��w@���@�=q@�5?@�J@�@���@�X@��@�Q�@��@��@��@���@�;d@�o@���@��!@��\@�5?@��-@��h@�p�@�`B@�X@�?}@���@���@�r�@�I�@�1@�ƨ@�l�@�C�@�"�@��y@��H@���@�ȴ@���@�n�@�^5@�5?@�@��T@��@��@��T@�@�O�@�&�@��@�%@���@���@��j@��u@�Z@�I�@�A�@�9X@�(�@��@���@��
@��w@��@���@��P@�\)@�S�@�K�@�33@��y@���@���@�^5@�V@�{@���@���@���@��h@��@�hs@�X@�/@��`@��j@�z�@�Q�@�1'@�(�@�(�@�  @�ƨ@��P@�l�@�K�@�"�@�@��y@���@���@�v�@�^5@�J@�@���@�x�@�G�@��@�Ĝ@���@�Q�@�@��@l�@+@~�+@~E�@~$�@~$�@~@}��@}O�@}V@|��@|�/@|�j@|j@|9X@|(�@{�m@{��@{S�@z��@z^5@z�@y�@y��@yX@y7L@y&�@x��@x�9@x�u@x �@w��@w;d@v�@v��@v5?@u��@uV@t��@tI�@s�F@s"�@r��@rM�@q�#@qX@p��@pbN@o�;@o|�@n�@n$�@m�-@l��@l��@lz�@lj@lZ@lI�@l�@kC�@j�H@j�\@j�@i��@i�7@i&�@h�u@h1'@g�;@g�@g+@g
=@f{@e�h@d�@d�@d9X@c�m@c�@b�\@b=q@ax�@aX@aG�@a7L@a%@`�u@`bN@`A�@`  @_�@_K�@^�+@]�T@]��@]p�@\��@\��@\j@\I�@[��@[�F@[�@[33@[@Z��@Z��@Zn�@Zn�@ZJ@Y�^@Yhs@Y%@X�`@X�9@XA�@W��@W\)@W
=@V�R@V5?@U�@U��@U��@UO�@T�@T�/@T�/@T�/@T�/@T��@T�@T��@Tj@S��@R�@R�!@R^5@R=q@R=q@R-@RJ@RJ@Q��@QG�@Q&�@Q�@Q%@PĜ@Pr�@PbN@P1'@O�@O��@O|�@O;d@N��@N5?@M�h@L��@L��@L�j@L�j@L�@L�@Lj@LI�@L1@K�@K@J��@J~�@J�\@JM�@J�@J�@I��@IG�@H��@H�9@H��@H�@HbN@HA�@H  @G��@G\)@GK�@GK�@G
=@Fȴ@F{@E�@EO�@E�@E�@D��@D��@Dz�@Cƨ@Ct�@C33@B�H@Bn�@A��@A��@A��@Ax�@A&�@@��@@A�@?�@?�w@?�@?��@?l�@>��@>ȴ@>��@>��@>��@>��@>ff@>V@=@=`B@=?}@<��@<��@<�D@<1@;��@;dZ@;"�@:�H@:�\@:^5@:J@9�7@9&�@8��@8A�@8b@7��@7K�@7
=@6�R@65?@6@5�T@5�@4��@4z�@41@3�
@3��@3�@3S�@3"�@2��@2~�@2=q@1��@1�7@1�@1�@1�@0��@0��@0Q�@0  @/�w@/�@/�P@/l�@/\)@/\)@/\)@/�@.ȴ@.5?@-�T@-�-@-�h@-`B@-�@,�@,�@,j@+�F@+�@+33@+@*�H@*��@*�!@*~�@*n�@*^5@*-@)��@)�^@)x�@)7L@(��@(Ĝ@(�u@(A�@'�@'\)@'�@&�@&��@&ff@&5?@%��@%p�@%?}@%V@$�/@$��@$�D@$(�@$1@#��@#�
@#��@#dZ@#33@"�!@!�#@!��@!G�@ �`@ �@ bN@ Q�@ Q�@ Q�@ A�@  �@   @�;@�w@��@|�@l�@\)@;d@�@��@E�@{@�@@?}@�@�j@�@z�@(�@1@ƨ@��@S�@�@�\@-@�@J@��@��@�^@�7@��@�9@r�@�w@|�@\)@K�@+@��@�@ȴ@��@�+@$�@��@p�@O�@/@��@�@�D@z�@I�@(�@�@�
@��@t�@dZ@C�@o@�H@�\@M�@-@J@��@�#@��@��@G�@�@�`@Q�@b@b@  @�;@��@�@�@��@��@|�@\)@K�@K�@K�@�@ȴ@��@E�@$�@$�@@�@�@�@�T@@��@�h@p�@`B@O�@?}@?}@?}@?}@�@��@��@z�@I�@(�@1@ƨ@��@�@t�@33@"�@o@@
��@
�\@
n�@
n�@
^5@
M�@
-@	�@	��@	�7@	x�@	x�@	hs@	X@	�@�`@��@��@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�wB
�qB
�qB
�qB
�wB
�wB
�}B
�}B
�wB
�wB
�wB
��B
B
ŢB
ƨB
ƨB
ƨB
ŢB
ĜB
ĜB
��B�BI�B[#B�B��B��B��B�#B�;B�NBBoB�B �B%�B&�B$�B �B�B�B �B%�B�B��B�B�B}�By�Be`BD�B/BuB
�B
�TB
�sB
�B
�B
��B
��B
�RB
�!B
�oB
x�B
q�B
hsB
_;B
VB
N�B
<jB
.B
+B
%�B
 �B
�B
�B
�B
�B
bB
JB
JB
+B	��B	�B	�NB	�B	ÖB	��B	x�B	Q�B	H�B	K�B	K�B	D�B	;dB	33B	%�B	 �B	�B	�B	�B	VB	  B��B��B�sB�
B��B�^B��B��B�uB�bB�VB�JB�7B�+B�B�B�B�B|�B|�B|�B|�B{�By�Bx�Bv�Bs�Bp�Bm�Bk�BjBjBcTBcTBbNBaHB`BB_;B_;B^5B^5B]/B]/B\)B\)B\)B]/B`BBcTBgmBw�B|�B~�B~�B� B�B~�By�Bt�Bp�Bn�Bk�BgmBe`BdZBcTBcTBe`BcTBaHBZBI�BK�BN�BW
B\)B]/BZBW
BS�BQ�BL�BK�BB�B?}B=qB9XB6FB2-B1'B1'B/B.B-B,B(�B)�B+B+B,B-B0!B1'B1'B1'B7LB;dB<jB=qB>wB?}B?}B9XB<jB;dB;dB;dB<jB<jB<jB>wB@�BD�BG�BH�BK�BI�BG�BG�BK�BS�BVBVBXB^5B_;BaHB^5B_;B_;BaHBbNBbNBbNBdZBcTBbNBaHB_;B[#B[#B\)B^5B_;BdZBgmBgmBgmBl�Bp�Bu�By�Bz�B}�B�B�B�B�B�B�%B�1B�JB�VB�hB�oB��B��B��B��B��B��B��B��B��B��B��B��B�!B�'B�9B�FB�FB�RB�XB�^B�^B�dB�^B�XB�^B�^B�dB�dB�dB�dB�^B�XBB��B�
B�B�/B�5B�HB�ZB�yB�B�B��B��B��B	B	B	+B	
=B	JB	\B	uB	�B	�B	�B	!�B	#�B	%�B	'�B	+B	.B	/B	2-B	33B	5?B	7LB	8RB	8RB	:^B	:^B	:^B	;dB	>wB	>wB	>wB	@�B	B�B	C�B	F�B	G�B	K�B	M�B	M�B	P�B	T�B	XB	ZB	[#B	\)B	]/B	^5B	`BB	aHB	bNB	cTB	dZB	e`B	e`B	ffB	gmB	hsB	jB	k�B	k�B	k�B	m�B	o�B	r�B	s�B	t�B	t�B	v�B	z�B	|�B	~�B	� B	� B	�B	�B	�B	�%B	�7B	�=B	�PB	�bB	�bB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�3B	�?B	�FB	�LB	�XB	�XB	�dB	�jB	�jB	�qB	�wB	�}B	��B	B	ÖB	ÖB	ĜB	ŢB	ŢB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�)B	�/B	�/B	�;B	�;B	�;B	�BB	�BB	�HB	�HB	�NB	�ZB	�`B	�fB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
1B
	7B
	7B

=B

=B
DB
DB
JB
JB
PB
VB
VB
\B
\B
\B
\B
\B
\B
\B
hB
hB
hB
oB
oB
oB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
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
$�B
%�B
%�B
%�B
$�B
$�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
,B
,B
-B
.B
.B
.B
.B
.B
.B
.B
.B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
33B
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
5?B
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
;dB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
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
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
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
I�B
I�B
I�B
J�B
J�B
J�B
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
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
S�B
S�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
VB
VB
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
YB
YB
YB
YB
YB
YB
ZB
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
dZB
dZB
dZB
e`B
e`B
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
ffB
gmB
gmB
gmB
gmB
gmB
gmB
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
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
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
m�B
m�B
m�B
m�B
m�B
m�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�]B
�VB
�VB
�VB
�]B
�]B
�cB
�cB
�]B
�]B
�]B
�oB
�uB
ňB
ƎB
ƎB
ƎB
ňB
āB
āB
��B�BI�B[	B��B��B�iB��B�	B�!B�4B�BTB�B �B%�B&�B$�B �B�B�B �B%�ByB��B��B� B}�By�BeFBD�B/ B[B
�B
�:B
�XB
�wB
�wB
̳B
�iB
�8B
�B
�TB
x�B
q�B
hXB
_!B
U�B
N�B
<PB
-�B
*�B
%�B
 �B
�B
�B
�B
mB
HB
0B
0B
B	��B	�kB	�4B	��B	�{B	�sB	x�B	Q�B	H�B	K�B	K�B	D�B	;JB	3B	%�B	 �B	�B	yB	gB	<B��B��B��B�XB��B̳B�DB��B�gB�@B�HB�"B�0B�B�B��B��B��B��B|�B|�B|�B|�B{�By�Bx�Bv�Bs�Bp�BmwBkQBjKBjeBc Bc:Bb4BaB`'B_!B_!B^B^B]B]B\B\B\B]B`'Bc:BgRBw�B|�B~�B~�B�B��B~�By�Bt�Bp�Bn}BkQBgRBe,Bd@Bc Bc:BeFBc Ba-BY�BI�BK�BN�BV�B\B]BZBV�BS�BQ�BL�BK�BBuB?cB=VB9>B6+B2B0�B1B/ B-�B,�B+�B(�B)�B*�B*�B+�B,�B0B1B1B1B72B;JB<PB=<B>]B?cB?HB9>B<PB;0B;JB;JB<PB<PB<6B>BB@iBD�BG�BH�BK�BI�BGzBG�BK�BS�BU�BU�BW�B^B_!BaB^B_!B_!Ba-Bb4BbBbBd@Bc:Bb4BaB_B[	BZ�B\B^B_!Bd&Bg8Bg8Bg8BlqBp�Bu�By�Bz�B}�B��B��B��B��B��B�B�B�B�<B�NB�TB�MB�_B�B�qB��B��B��B��B��B��B��B��B�B��B�B�+B�+B�B�$B�DB�DB�JB�DB�>B�DB�*B�JB�0B�JB�JB�*B�>B�uBοB��B�B��B�B�B�&B�DB��B�B��B��B��B	�B	�B	�B	
#B	0B	(B	[B	sB	qB	~B	!�B	#�B	%�B	'�B	*�B	-�B	.�B	2B	3B	5%B	7B	88B	88B	:DB	:DB	:*B	;JB	>]B	>BB	>BB	@iB	BuB	CaB	FtB	GzB	K�B	M�B	M�B	P�B	T�B	W�B	ZB	Z�B	\B	]B	^B	`'B	aB	b4B	c B	d@B	eFB	eFB	fLB	g8B	h>B	jKB	kkB	kkB	kkB	mwB	oiB	r|B	s�B	t�B	t�B	v�B	z�B	|�B	~�B	�B	�B	��B	��B	�B	�B	�B	�	B	�6B	�HB	�HB	�.B	�NB	�aB	�gB	�mB	�SB	�SB	�gB	�gB	�sB	��B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�%B	�+B	�B	�$B	�>B	�JB	�6B	�PB	�<B	�BB	�cB	�UB	�uB	�{B	�{B	�gB	ňB	ňB	�tB	ǔB	ȀB	ɠB	ʦB	ˬB	ˬB	̘B	̳B	̳B	͟B	͟B	οB	οB	ϫB	ϫB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�!B	�!B	�!B	�B	�B	�B	�-B	�4B	�@B	�,B	�2B	�RB	�RB	�XB	�_B	�DB	�eB	�KB	�kB	�qB	�qB	�]B	�wB	�cB	�cB	�B	�oB	�oB	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
B
B
B
	B
	B

	B

#B
)B
)B
B
0B
6B
"B
<B
BB
(B
(B
BB
BB
(B
(B
NB
4B
NB
TB
TB
:B
@B
[B
FB
aB
aB
MB
gB
mB
mB
mB
mB
mB
SB
YB
yB
_B
eB
eB
B
eB
eB
�B
�B
kB
�B
qB
�B
xB
�B
xB
�B
�B
�B
�B
~B
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
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
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
$�B
%�B
%�B
%�B
$�B
$�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
+�B
+�B
,�B
-�B
-�B
-�B
-�B
-�B
-�B
-�B
-�B
-�B
/ B
/ B
.�B
0B
0B
/�B
/�B
0B
1B
0�B
1B
0�B
0�B
1B
0�B
1B
0�B
1�B
2B
2B
1�B
2B
2B
3B
2�B
2�B
4B
4B
4B
4B
4B
5B
5%B
5%B
5%B
6+B
6+B
6+B
6B
6B
6+B
7B
72B
72B
72B
72B
72B
72B
88B
88B
88B
88B
8B
88B
88B
8B
88B
9>B
9>B
9$B
9$B
9>B
:*B
:*B
:*B
:DB
;0B
;JB
;JB
<6B
<PB
=VB
=VB
=<B
>]B
>]B
>BB
>BB
>]B
?HB
?HB
?HB
@iB
@iB
@iB
AoB
AoB
AoB
AoB
BuB
BuB
B[B
CaB
CaB
C{B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
FtB
F�B
F�B
FtB
G�B
G�B
G�B
GzB
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
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
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
S�B
S�B
T�B
T�B
T�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
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
X�B
X�B
X�B
X�B
X�B
X�B
ZB
Y�B
ZB
ZB
[	B
Z�B
[	B
Z�B
[	B
\B
[�B
[�B
]B
\�B
]B
^B
^B
^B
^B
^B
^B
^B
_B
_!B
_!B
_!B
`'B
`'B
`'B
`B
`B
aB
a-B
aB
aB
a-B
aB
a-B
a-B
b4B
b4B
b4B
b4B
b4B
c B
c:B
c B
c:B
c B
c:B
c B
d@B
d&B
d@B
d@B
eFB
e,B
e,B
eFB
e,B
eFB
fLB
fLB
fLB
f2B
fLB
f2B
fLB
fLB
fLB
fLB
fLB
gRB
g8B
gRB
gRB
gRB
gRB
gRB
gRB
g8B
h>B
hXB
h>B
hXB
h>B
hXB
hXB
hXB
hXB
h>B
hXB
i_B
iDB
i_B
i_B
i_B
iDB
jeB
jeB
jeB
jKB
jeB
jeB
jKB
jeB
kkB
kkB
kkB
kkB
kkB
kQB
kkB
lWB
lWB
lqB
lWB
lWB
lqB
lqB
m]B
m]B
mwB
mwB
mwB
m]11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.27(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201904170033112019041700331120190417003311201904180024272019041800242720190418002427JA  ARFMdecpA19c                                                                20190412123647  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190412033704  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190412033706  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190412033706  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190412033707  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190412033707  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190412033707  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190412033707  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190412033708  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190412033708                      G�O�G�O�G�O�                JA  ARUP                                                                        20190412035604                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190412153703  CV  JULD            G�O�G�O�Fŭ�                JM  ARCAJMQC2.0                                                                 20190416153311  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190416153311  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190417152427  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120031517                      G�O�G�O�G�O�                