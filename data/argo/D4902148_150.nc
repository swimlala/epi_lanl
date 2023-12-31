CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2018-09-25T15:35:55Z creation;2018-09-25T15:35:58Z conversion to V3.1;2019-12-18T07:20:00Z update;2022-11-21T05:30:06Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  td   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �<   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ͸   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �P   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �T   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �X   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �`   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180925153555  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_150                     2C  DdlNAVIS_A                         0397                            ARGO 011514                     863 @؄"ʆB 1   @؄#�Sp @<��D���dl�!-1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
=@�
=@���A Q�A Q�A@Q�A`Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B {B{B{B{B {B({B0{B8{B?�BH{BP{BX{B`{Bh{Bp{Bx{B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=C CCCCC
CCCCCCCCCCC C"C$C&C(C*C,C.C0C2C4C6C8C:C<C>C@CBCDCFCHCJCLCNCPCRCTCVCXCZC\C^C`CbCdCfChCjClCnCpCrCtCvCxCzC|C~C��C��C��C��C��C��C��C��C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDxHDx�HDyHDy�HDzHDz�HD{HD{�HD|HD|�HD}HD}�HD~HD~�HDHD�HD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�DဤD���D� �D�@�D․D���D� �D�@�D〤D���D� �D�@�D䀤D���D� �D�@�D値D���D� �D�@�D怤D���D� �D�@�D瀤D���D� �D�@�D耤D���D� �D�@�D逤D���D� �D�@�DꀤD���D� �D�@�D뀤D���D� �D�@�D쀤D���D� �D�@�D퀤D���D� �D�@�DD���D� �D�@�DD���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D��q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111133AζFAθRAθRAκ^AμjAξwAξwA�A�A�A�ƨA�ȴA�ȴA���A���A���AήAΏ\A΅A�O�A���A̶FA�M�A�|�A�33A��A�C�A��A�dZA�^5A���A��;A�33A��A�ȴA�jA��A�bNA���A�$�A�XA��#A�$�A��jA��;A�(�A���A��A�Q�A��A���A�ffA���A�^5A�"�A�
=A�^5A�oA�ffA�z�A��A�bNA���A��A�%A�1'A�C�A��A���A��/A�7LA��A���A�hsA�r�A�oA��RA�9XA�  A��!A�\)A��A��;A�G�A�ƨA�1Ay�^Avr�Aup�Au�At�yAt��Asl�Aq��Aq+Apn�Ao��Ao�AnĜAm��Am|�Al�AjI�Ai33Agl�Ae�AdI�AbffA`1A_/A]�^A\�+A[+AZ��AZAY�7AX~�AVjAT=qAS��ASXARAQ�AQ�PAPĜAP1'AO�AO�^AN�AMx�ALr�AJ^5AIp�AI/AI
=AH��AH�RAHffAG��AG�AF9XAEG�AC�TAB�RABffAA��A@�A@�uA?��A>�yA>^5A=p�A<��A<��A<ffA;�mA:M�A9��A9�PA8��A89XA7�PA6�yA5�^A3A3t�A25?A1C�A0{A/�;A/�A.�9A.9XA-�7A,(�A*�/A*9XA*bA)G�A'�A&{A%G�A$�9A$ffA#��A"�uA"$�A!��A!;dA!VA ��Ax�A��AQ�AĜAI�AA��A��Al�A;dA�`AA�A�A��A��Ap�AO�A+A�A%A�`AbNA�A�-A�`AJA�wAS�A�A��A|�A�A�A  At�A�uAbAƨA�AC�AĜA�DAI�A�TA|�A
�yA
{A	C�A�jA�A�7A&�A�+A-A��A�A�A�A��A
=A ��@���@���@�x�@��j@��m@�\)@��!@��/@���@�V@�Q�@�z�@�"�@��@�w@�G�@�u@��;@�dZ@旍@���@�x�@�Ĝ@�  @�\)@��@�9X@�o@���@�M�@݁@�(�@�V@���@�@�O�@��/@�A�@ӶF@��y@��T@���@ЋD@��@��@���@�\)@�;d@�ȴ@��@�l�@ư!@�^5@���@���@þw@��@�bN@�5?@��@���@���@��@��@�=q@��w@��@�\)@��H@���@��-@��9@�1@�o@�M�@�{@���@�
=@��!@���@���@�
=@�5?@�?}@��9@�1'@��@�\)@�
=@�~�@��@���@�/@��`@�bN@��w@��P@�C�@�33@���@���@�p�@��/@�bN@�(�@��F@�@���@���@�V@��@�@�/@��@���@��
@�o@�^5@�$�@��@�O�@���@��@�1@��;@�+@�~�@�M�@�J@��^@�x�@�?}@��/@�j@���@�+@���@���@�&�@�&�@��@�V@���@���@���@���@���@��D@�z�@�bN@�Z@�A�@�1'@�1'@�b@�t�@�o@�V@�@���@�&�@�(�@��
@���@�S�@��@���@�=q@��@�1@��@��@�|�@�C�@�33@�"�@��H@���@���@�{@��-@���@�hs@�7L@�&�@��@�V@���@���@�Q�@��@�ƨ@���@�|�@�o@�@���@���@��H@�ff@��@��7@�G�@�G�@�G�@�X@�X@�p�@�G�@�V@���@�I�@l�@;d@~��@~�R@~E�@}p�@}V@|��@|�D@|I�@|�@{�m@{�F@{33@z�!@zM�@y�^@y�@x  @w��@wl�@w+@vȴ@vff@v@u�-@uO�@tz�@sƨ@s��@sdZ@sS�@sC�@so@r^5@rJ@q�7@qX@p�`@p��@p�9@pA�@pb@o��@o
=@nE�@m�-@m�h@m�h@m��@m�h@m�h@m��@m�h@m�@mV@l�/@l��@lz�@lj@l(�@kƨ@j��@i�#@ix�@hr�@h �@g|�@fȴ@fE�@f{@eO�@d�@d1@c�@cC�@b�H@b��@b�\@b^5@b=q@b=q@bJ@a��@a�7@a�7@ax�@a&�@`�`@`�u@`r�@`r�@`r�@`A�@`  @_�w@_��@_|�@_�@^�y@^v�@^E�@^$�@]�@]@]V@\Z@\9X@\(�@\z�@\(�@[dZ@[��@[t�@[C�@["�@Z��@Z^5@Z-@Z-@Z�@Y�#@Y�7@X�`@X �@W�;@W�w@W��@W�P@W�P@W��@WK�@Vȴ@V��@V�+@VE�@U�T@U@UO�@T�/@T�D@T1@So@R�!@R�\@Rn�@R=q@RJ@Q�@Q��@Qx�@QX@Q�@PĜ@Pr�@PA�@P1'@P  @O�;@O�P@O�@N�+@M@M�h@MV@Lz�@L(�@K��@K��@K"�@K@J��@J��@J�!@J�\@J�\@J�\@J�\@J^5@JM�@J=q@JJ@I�#@I��@I7L@H1'@G�@G�@Fȴ@Fv�@FE�@FE�@FE�@FE�@F5?@F$�@F5?@F{@F{@F@F@Ep�@D�/@D�D@D1@Cƨ@CdZ@C33@C@B��@B��@B�\@BM�@B-@A�#@A�^@A��@A�7@Ax�@AX@@�`@@Ĝ@@Ĝ@@�9@@�u@@A�@?��@?�P@?|�@?K�@>��@>v�@>5?@>$�@=��@<��@<�D@<�D@<Z@;�m@;33@;o@;@:�@:��@:=q@:�@9�@9��@9��@9�7@97L@8��@81'@7l�@7
=@6ȴ@6�+@6ff@6E�@65?@5�@5��@5�@5O�@5V@4�@4��@4�j@4�j@4��@4z�@4Z@4(�@3�m@3��@3@2�\@2^5@2�@1��@1��@1��@1�7@1�7@1x�@1&�@0�`@0�`@0�`@0�`@0Ĝ@0�@0Q�@0A�@0  @/��@/l�@/\)@/\)@/;d@/+@.��@.��@-�@-��@-��@-@-�@-?}@,�/@,�@,z�@,z�@,j@,9X@,(�@+�m@+��@+"�@*�!@*=q@)�@)��@)�7@)hs@)G�@)�@(��@( �@'�w@'�P@'\)@';d@&�y@&V@&5?@%�@%��@%@%�-@%��@%V@$�D@$Z@#�m@#��@#S�@#"�@"��@"~�@"J@!�@!�@!�@!�#@!X@!7L@!�@ �`@ �u@ �@ bN@ Q�@ 1'@  �@��@�P@\)@�@��@��@V@@�-@��@�j@��@Z@9X@9X@1@�m@��@dZ@C�@�H@�!@-@-@�@J@�@��@�7@�@�@�@1'@�@l�@��@�R@��@��@��@��@��@��@v�@@@��@�@�@`B@?}@/@V@�@�@�@��@��@�D@Z@I�@(�@��@��@�m@�m@�m@�m@�m@�m@�
@��@S�@C�@��@��@�\@�\@~�@n�@^5@M�@x�@��@�`@��@��@�@A�@b@�@�w@l�@K�@+@�@��@��@��@��@�+@�+@v�@V@E�@$�@@�h@��@�j@��@j@I�@(�@��@�
@��@dZ@S�@33@@@
�@
�@
�H@
��@
��@
�\@
^5@
-@
=q@
-@
�@
J@	�@	��@	��@	��@	��@	�^@	��@	��@	x�@��@��@r�@bN@A�@1'@ �@b@�w@|�@l�@+@
=@��@�@��@V@5?@$�@{@$�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111133AζFAθRAθRAκ^AμjAξwAξwA�A�A�A�ƨA�ȴA�ȴA���A���A���AήAΏ\A΅A�O�A���A̶FA�M�A�|�A�33A��A�C�A��A�dZA�^5A���A��;A�33A��A�ȴA�jA��A�bNA���A�$�A�XA��#A�$�A��jA��;A�(�A���A��A�Q�A��A���A�ffA���A�^5A�"�A�
=A�^5A�oA�ffA�z�A��A�bNA���A��A�%A�1'A�C�A��A���A��/A�7LA��A���A�hsA�r�A�oA��RA�9XA�  A��!A�\)A��A��;A�G�A�ƨA�1Ay�^Avr�Aup�Au�At�yAt��Asl�Aq��Aq+Apn�Ao��Ao�AnĜAm��Am|�Al�AjI�Ai33Agl�Ae�AdI�AbffA`1A_/A]�^A\�+A[+AZ��AZAY�7AX~�AVjAT=qAS��ASXARAQ�AQ�PAPĜAP1'AO�AO�^AN�AMx�ALr�AJ^5AIp�AI/AI
=AH��AH�RAHffAG��AG�AF9XAEG�AC�TAB�RABffAA��A@�A@�uA?��A>�yA>^5A=p�A<��A<��A<ffA;�mA:M�A9��A9�PA8��A89XA7�PA6�yA5�^A3A3t�A25?A1C�A0{A/�;A/�A.�9A.9XA-�7A,(�A*�/A*9XA*bA)G�A'�A&{A%G�A$�9A$ffA#��A"�uA"$�A!��A!;dA!VA ��Ax�A��AQ�AĜAI�AA��A��Al�A;dA�`AA�A�A��A��Ap�AO�A+A�A%A�`AbNA�A�-A�`AJA�wAS�A�A��A|�A�A�A  At�A�uAbAƨA�AC�AĜA�DAI�A�TA|�A
�yA
{A	C�A�jA�A�7A&�A�+A-A��A�A�A�A��A
=A ��@���@���@�x�@��j@��m@�\)@��!@��/@���@�V@�Q�@�z�@�"�@��@�w@�G�@�u@��;@�dZ@旍@���@�x�@�Ĝ@�  @�\)@��@�9X@�o@���@�M�@݁@�(�@�V@���@�@�O�@��/@�A�@ӶF@��y@��T@���@ЋD@��@��@���@�\)@�;d@�ȴ@��@�l�@ư!@�^5@���@���@þw@��@�bN@�5?@��@���@���@��@��@�=q@��w@��@�\)@��H@���@��-@��9@�1@�o@�M�@�{@���@�
=@��!@���@���@�
=@�5?@�?}@��9@�1'@��@�\)@�
=@�~�@��@���@�/@��`@�bN@��w@��P@�C�@�33@���@���@�p�@��/@�bN@�(�@��F@�@���@���@�V@��@�@�/@��@���@��
@�o@�^5@�$�@��@�O�@���@��@�1@��;@�+@�~�@�M�@�J@��^@�x�@�?}@��/@�j@���@�+@���@���@�&�@�&�@��@�V@���@���@���@���@���@��D@�z�@�bN@�Z@�A�@�1'@�1'@�b@�t�@�o@�V@�@���@�&�@�(�@��
@���@�S�@��@���@�=q@��@�1@��@��@�|�@�C�@�33@�"�@��H@���@���@�{@��-@���@�hs@�7L@�&�@��@�V@���@���@�Q�@��@�ƨ@���@�|�@�o@�@���@���@��H@�ff@��@��7@�G�@�G�@�G�@�X@�X@�p�@�G�@�V@���@�I�@l�@;d@~��@~�R@~E�@}p�@}V@|��@|�D@|I�@|�@{�m@{�F@{33@z�!@zM�@y�^@y�@x  @w��@wl�@w+@vȴ@vff@v@u�-@uO�@tz�@sƨ@s��@sdZ@sS�@sC�@so@r^5@rJ@q�7@qX@p�`@p��@p�9@pA�@pb@o��@o
=@nE�@m�-@m�h@m�h@m��@m�h@m�h@m��@m�h@m�@mV@l�/@l��@lz�@lj@l(�@kƨ@j��@i�#@ix�@hr�@h �@g|�@fȴ@fE�@f{@eO�@d�@d1@c�@cC�@b�H@b��@b�\@b^5@b=q@b=q@bJ@a��@a�7@a�7@ax�@a&�@`�`@`�u@`r�@`r�@`r�@`A�@`  @_�w@_��@_|�@_�@^�y@^v�@^E�@^$�@]�@]@]V@\Z@\9X@\(�@\z�@\(�@[dZ@[��@[t�@[C�@["�@Z��@Z^5@Z-@Z-@Z�@Y�#@Y�7@X�`@X �@W�;@W�w@W��@W�P@W�P@W��@WK�@Vȴ@V��@V�+@VE�@U�T@U@UO�@T�/@T�D@T1@So@R�!@R�\@Rn�@R=q@RJ@Q�@Q��@Qx�@QX@Q�@PĜ@Pr�@PA�@P1'@P  @O�;@O�P@O�@N�+@M@M�h@MV@Lz�@L(�@K��@K��@K"�@K@J��@J��@J�!@J�\@J�\@J�\@J�\@J^5@JM�@J=q@JJ@I�#@I��@I7L@H1'@G�@G�@Fȴ@Fv�@FE�@FE�@FE�@FE�@F5?@F$�@F5?@F{@F{@F@F@Ep�@D�/@D�D@D1@Cƨ@CdZ@C33@C@B��@B��@B�\@BM�@B-@A�#@A�^@A��@A�7@Ax�@AX@@�`@@Ĝ@@Ĝ@@�9@@�u@@A�@?��@?�P@?|�@?K�@>��@>v�@>5?@>$�@=��@<��@<�D@<�D@<Z@;�m@;33@;o@;@:�@:��@:=q@:�@9�@9��@9��@9�7@97L@8��@81'@7l�@7
=@6ȴ@6�+@6ff@6E�@65?@5�@5��@5�@5O�@5V@4�@4��@4�j@4�j@4��@4z�@4Z@4(�@3�m@3��@3@2�\@2^5@2�@1��@1��@1��@1�7@1�7@1x�@1&�@0�`@0�`@0�`@0�`@0Ĝ@0�@0Q�@0A�@0  @/��@/l�@/\)@/\)@/;d@/+@.��@.��@-�@-��@-��@-@-�@-?}@,�/@,�@,z�@,z�@,j@,9X@,(�@+�m@+��@+"�@*�!@*=q@)�@)��@)�7@)hs@)G�@)�@(��@( �@'�w@'�P@'\)@';d@&�y@&V@&5?@%�@%��@%@%�-@%��@%V@$�D@$Z@#�m@#��@#S�@#"�@"��@"~�@"J@!�@!�@!�@!�#@!X@!7L@!�@ �`@ �u@ �@ bN@ Q�@ 1'@  �@��@�P@\)@�@��@��@V@@�-@��@�j@��@Z@9X@9X@1@�m@��@dZ@C�@�H@�!@-@-@�@J@�@��@�7@�@�@�@1'@�@l�@��@�R@��@��@��@��@��@��@v�@@@��@�@�@`B@?}@/@V@�@�@�@��@��@�D@Z@I�@(�@��@��@�m@�m@�m@�m@�m@�m@�
@��@S�@C�@��@��@�\@�\@~�@n�@^5@M�@x�@��@�`@��@��@�@A�@b@�@�w@l�@K�@+@�@��@��@��@��@�+@�+@v�@V@E�@$�@@�h@��@�j@��@j@I�@(�@��@�
@��@dZ@S�@33@@@
�@
�@
�H@
��@
��@
�\@
^5@
-@
=q@
-@
�@
J@	�@	��@	��@	��@	��@	�^@	��@	��@	x�@��@��@r�@bN@A�@1'@ �@b@�w@|�@l�@+@
=@��@�@��@V@5?@$�@{@$�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�B�VB�VB�PB�VB�VB�VB�VB�\B�\B�\B�hB�oB�oB��B��B��B��B��B��B��B��B��B��B�dBB��B��B��B��B�jB�LB�'B�B��B��B��B�DB}�Bw�Bu�Bp�BhsBgmBcTB]/BXBR�BN�BG�B;dB.B�B%B��B�fB�ZB�#B��BȴB�dB�B��B��B�bB�%Bn�BaHBVBI�B@�B6FB(�B�B+B
��B
�NB
��B
ÖB
��B
�jB
�XB
�LB
�9B
��B
��B
�7B
dZB
N�B
F�B
D�B
B�B
>wB
8RB
0!B
.B
(�B
$�B
 �B
�B
�B
{B
DB
  B	��B	�B	�yB	�BB	��B	ŢB	�qB	�?B	�'B	�B	�B	��B	��B	��B	�oB	�+B	�B	�B	w�B	v�B	s�B	o�B	l�B	jB	iyB	cTB	[#B	R�B	H�B	B�B	A�B	@�B	@�B	?}B	=qB	<jB	8RB	5?B	0!B	%�B	!�B	$�B	"�B	�B	�B	�B	�B	uB	\B	VB	VB	PB	
=B	B	B	  B��B��B��B�B�B�HB�HB�/B�B��B��B��B��B��BǮBB�dB�RB�FB�'B�B��B��B��B��B��B��B��B��B��B��B��B�oB�\B�PB�=B�1B�1B�+B�%B�B�B�B�B~�B{�Bz�By�Bx�Bx�Bx�Bw�Bv�Bu�Bs�Br�Bp�Bn�Bm�Bl�BjBhsBgmBffBdZBcTBaHB_;B]/B]/B\)B[#BZBYBXBW
BVBS�BQ�BO�BM�BL�BK�BI�BI�BG�BE�BB�B?}B>wB=qB<jB:^B9XB8RB7LB6FB5?B5?B33B2-B1'B/B-B+B(�B'�B%�B%�B$�B$�B$�B&�B&�B'�B)�B+B+B+B,B+B+B+B)�B(�B'�B'�B'�B(�B(�B(�B(�B)�B)�B,B)�B)�B+B,B,B,B,B-B/B0!B0!B0!B1'B1'B/B(�B#�B"�B"�B!�B!�B'�B+B'�B-B0!B1'B1'B2-B33B5?B8RB:^B?}BI�BO�BQ�BS�BT�BW
BXB]/BaHBdZBffBgmBhsBiyBk�Bk�Bm�Bm�Bo�Bp�Bp�Br�Bs�Br�Br�Bs�Bt�Bv�Bw�Bz�B~�B� B� B� B�B�B�B�B�B�1B�DB�\B�bB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�3B�jB��B��B��B��BBĜBƨBƨBƨBǮBǮBȴBȴBɺBɺBɺBɺB��B��B��B��B��B�
B�5B�BB�HB�NB�TB�ZB�yB�B��B��B��B��B��B��B��B	B	B	B	%B		7B		7B	DB	\B	bB	bB	hB	hB	{B	�B	�B	�B	�B	�B	 �B	!�B	!�B	!�B	"�B	%�B	(�B	+B	-B	.B	/B	/B	/B	0!B	5?B	8RB	8RB	;dB	>wB	@�B	A�B	B�B	C�B	G�B	H�B	I�B	J�B	L�B	M�B	N�B	O�B	P�B	S�B	VB	W
B	YB	\)B	\)B	]/B	^5B	_;B	aHB	cTB	dZB	e`B	hsB	k�B	m�B	n�B	n�B	o�B	r�B	u�B	v�B	y�B	z�B	|�B	�B	�%B	�1B	�1B	�7B	�=B	�DB	�JB	�PB	�PB	�VB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�'B	�3B	�9B	�9B	�9B	�?B	�FB	�XB	�^B	�^B	�dB	�jB	�wB	�}B	�}B	��B	B	ÖB	ĜB	ĜB	ŢB	ŢB	ŢB	ŢB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�
B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�;B	�;B	�BB	�BB	�NB	�NB	�TB	�ZB	�`B	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
%B
1B
	7B

=B

=B
DB
DB
DB
DB
DB
DB
DB
JB
JB
JB
JB
JB
PB
PB
VB
VB
\B
bB
bB
bB
hB
hB
hB
hB
oB
oB
uB
uB
uB
uB
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
#�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
+B
,B
-B
-B
.B
.B
.B
/B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
33B
33B
33B
33B
33B
33B
33B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
;dB
<jB
=qB
=qB
=qB
=qB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
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
K�B
K�B
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
S�B
S�B
S�B
S�B
T�B
T�B
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
W
B
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
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
^5B
^5B
_;B
_;B
`BB
`BB
aHB
aHB
aHB
aHB
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
cTB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
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
iyB
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
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�B�VB�VB�PB�VB�VB�VB�VB�\B�\B�\B�hB�oB��B��B��B� B�$B�DB��B�=B��B�B��B�B�%BοB�:B� BбB��B��B�`B�B��B�B�KB��B�By$BwfBq�Bi�Bh�Be,B^�BYBT�BP�BJ�B>wB1�BVB	B�B�B�B�CBյB��B��B�OB��B��B�TB��Bp�Bc:BXBK�BBuB9$B,"B�B	�B
��B
��B
��B
�MB
�UB
�<B
�DB
��B
�fB
��B
��B
��B
g�B
PB
G+B
EB
CGB
@ B
9�B
1B
.�B
)�B
%�B
!�B
�B
sB
SB
jB
�B	�B	�B	�B	�B	�sB	��B	�.B	��B	��B	��B	��B	��B	��B	�;B	��B	��B	�B	�AB	xB	w�B	t�B	pUB	l�B	kB	j�B	eB	\�B	U2B	I�B	B�B	A�B	@�B	@�B	@ B	>BB	=�B	9�B	6�B	1�B	'8B	"�B	%�B	#�B	OB	�B	�B	mB	�B	B	�B	�B	<B	�B	�B	�B	 B��B��B��B�TB�B�4B��B�jB�KB�uB��BЗBΥB�B�lB�B�6B��B��B�3B��B��B��B��B��B��B�jB�]B�7B�B�yB��B�[B�}B�B��B��B��B��B��B��B��B��B�B�OB|�B{dBz*By$By	By	Bx8Bw�Bv`BtTBs�Bq�Bo5Bn/Bm]Bk�Bi*Bh
BgBezBd@BbhB_�B]�B]~B\�B[�BZkBY�BX�BW�BV�BU2BSBP�BN�BM�BL~BJ�BJXBH�BG_BD�B@�B?.B>BB="B;dB:*B9>B7�B6�B5�B5�B4nB33B2aB0�B/OB,=B*eB)B'RB&�B%`B%`B%zB'mB'RB(�B*B+�B,B,"B,�B+kB+kB+�B+B*KB)*B)*B(�B)_B)yB)yB)�B*�B*�B,�B+6B+6B+�B,qB,=B,�B-]B.B/�B0�B0�B0�B2B2B0�B*0B$&B# B# B"�B# B(�B,�B)�B-�B0�B1�B1�B2�B3�B5�B8�B:^B?.BI�BPbBR�BUBU�BW�BX�B]�Ba�Bd�Bf�Bg�Bh�Bi�Bk�Bk�Bm�BnBpBp�Bp�Br�BtBshBr�Bt9Bu%BwBx8B{dBB�4B�OB�OB�[B�{B�mB��B��B��B��B��B��B��B�B��B��B��B�CB�!B��B� B�&B�,B�2B�RB�>B�B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��BȴB��B��B��B��B�=B�B�PB�.B�TB�gB��B�jB�vB�B�B�B�B�eB�3B��B�	B�B�B��B�B�.B	 B	UB	uB	tB		RB		lB	xB	\B	}B	}B	�B	�B	�B	�B	�B	�B	�B	B	 �B	!�B	!�B	!�B	#:B	&LB	)DB	+6B	-B	.B	/B	/B	/5B	0UB	5tB	8�B	8�B	;�B	>�B	@�B	A�B	B�B	C�B	G�B	H�B	I�B	J�B	L�B	M�B	N�B	O�B	QB	T,B	VSB	WYB	YeB	\]B	\)B	]IB	^jB	_pB	a|B	cnB	dtB	e�B	h�B	k�B	m�B	n�B	n�B	o�B	r�B	u�B	v�B	y�B	z�B	}B	�'B	�YB	�KB	�fB	��B	��B	�xB	�dB	�6B	�PB	�pB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	��B	��B	�B	�B	�8B	�*B	�6B	�=B	�WB	�]B	�CB	�IB	�/B	�IB	�5B	�5B	�5B	�;B	�AB	�AB	�[B	�3B	�9B	�TB	�TB	�ZB	�`B	�rB	�^B	�xB	�B	�jB	��B	��B	��B	��B	ªB	��B	ĶB	ĶB	żB	żB	��B	��B	ȴB	ɺB	��B	�B	�B	��B	�B	�B	�B	�B	�,B	�B	�B	�$B	�9B	�?B	�_B	�KB	�1B	�1B	�7B	�7B	�#B	�)B	�]B	�dB	�VB	�VB	�\B	�vB	�hB	�B	�B	�B	�zB	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�	B	��B	��B	�B	�B	�"B	�B	��B	��B	�B	�B	��B
  B
  B
 B
 B
 B
 B
 B
'B
AB
aB
YB
fB
	RB

XB

XB
DB
DB
)B
DB
)B
DB
^B
JB
dB
dB
~B
~B
�B
jB
pB
�B
\B
}B
}B
}B
�B
�B
�B
�B
�B
�B
�B
uB
�B
�B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"B
#�B
$�B
%�B
%�B
'B
'B
'B
'B
(
B
)B
(�B
(�B
)B
*B
)�B
)�B
+B
+B
+B
+B
+B
+6B
,=B
-)B
-)B
.B
./B
./B
/5B
/5B
/B
/5B
/B
0;B
0B
0!B
0!B
0!B
0;B
1AB
1AB
1AB
2GB
2GB
33B
3B
3MB
3MB
3MB
3hB
3hB
5?B
5?B
5ZB
5ZB
5ZB
6`B
6FB
6FB
7LB
6`B
6`B
7fB
7fB
7fB
7�B
8�B
8�B
9rB
:^B
:xB
:^B
:xB
:xB
:�B
;B
<�B
=�B
=�B
=�B
=�B
>�B
?}B
?�B
?}B
@iB
@�B
@�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
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
K�B
K�B
NB
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
PB
Q B
QB
P�B
Q B
Q�B
RB
RB
Q�B
R B
SB
S&B
TB
T,B
TB
TB
UB
UB
VB
VB
VB
U�B
VB
VB
V9B
W$B
W$B
W$B
W
B
W$B
W
B
W�B
X+B
X+B
XB
W�B
X+B
X+B
X�B
Y1B
YB
Y1B
Y1B
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
Z7B
Z7B
[=B
[WB
\CB
\)B
\)B
\)B
\)B
\CB
\CB
\]B
^OB
^5B
_;B
_VB
`\B
`\B
aHB
aHB
abB
abB
bhB
bNB
bNB
bNB
cTB
cTB
cTB
c:B
cTB
cTB
cnB
cnB
dtB
dtB
d�B
d�B
ezB
ezB
ezB
f�B
f�B
f�B
f�B
f�B
gmB
gmB
g�B
gmB
hXB
hsB
hsB
hsB
hXB
hsB
h�B
h�B
i�B
iyB
iyB
iyB
iyB
i�B
i�B
jeB
j�B
jB
jB
jB
jB
jB
j�B
k�B
k�B
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
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<B�8<<j<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.02(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201810060036042018100600360420181006003604202211182136232022111821362320221118213623201810070019432018100700194320181007001943  JA  ARFMdecpA19c                                                                20180926003512  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180925153555  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180925153557  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180925153557  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180925153558  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180925153558  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180925153558  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180925153558  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180925153558  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180925153558                      G�O�G�O�G�O�                JA  ARUP                                                                        20180925155522                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180925153525  CV  JULD            G�O�G�O�F�!                JM  ARCAJMQC2.0                                                                 20181005153604  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181005153604  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181006151943  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171536                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123623  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                