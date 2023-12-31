CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-06-18T15:42:32Z creation;2023-06-18T15:42:34Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ph   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tT   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Έ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �h   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �h   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �h   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230618154232  20230618162442  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�3��N�1   @�3�Tb�@0�&�x���c#"��`B1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�  @�33A   A   A@  A`  A�  A�  A�  A�  A�33A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�B���B���B�  C   C  C  C  C  C
ffC�fC  C  C  C  C  C  C  C  C  C   C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4�C6�C8  C:  C;�fC=�fC@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR�fDSfDS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{y�D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�6f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@qG�@��
@���AQ�A<Q�A\Q�A|Q�A�(�A�(�A�(�A�\)A�\)A�(�A�(�A�(�B{B{B{B{B'{B/{B7{B?{BG{BO{BW{B_{Bg{Boz�Bw{B{B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��pB�W
BǊ=Bˊ=Bϊ=Bӊ=B׊=Bۊ=Bߊ=B�=B�=B�=B�#�B�W
B�W
B��=B��=C�C�C�C�C
+�C��C�C�C�C�C�C�C�C�C�C�C!�C#��C%�C'�C)�C+�C-�C/�C1�C3޹C5޹C7�C9�C;��C=��C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}޹C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��\C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��D qHD �HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HD	qHD	�HD
qHD
�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HD qHD �HD!qHD!�HD"qHD"�HD#qHD#�HD$qHD$�HD%qHD%�HD&qHD&�HD'qHD'�HD(qHD(�HD)qHD)�HD*qHD*�HD+qHD+�HD,qHD,�HD-qHD-�HD.qHD.�HD/qHD/�HD0qHD0�HD1qHD1�HD2qHD2�HD3qHD3�HD4qHD4�HD5qHD5�HD6qHD6�HD7qHD7�HD8qHD8�HD9qHD9�HD:qHD:�HD;qHD;�HD<qHD<�HD=qHD=�HD>qHD>�HD?qHD?�HD@qHD@�HDAqHDA�HDBqHDB�HDCqHDC�HDDqHDD�HDEqHDE�HDFqHDF�HDGqHDG�HDHqHDH�HDIqHDI�HDJqHDJ�HDKqHDK�HDLqHDL�HDMqHDM�HDNqHDN�HDOqHDO�HDPqHDP�HDQqHDQ�HDRw�DR��DSqHDS�HDTqHDT�HDUqHDU�HDVqHDV�HDWqHDW�HDXqHDX�HDYqHDY�HDZqHDZ�HD[qHD[�HD\qHD\�HD]qHD]�HD^qHD^�HD_qHD_�HD`qHD`�HDaqHDa�HDbqHDb�HDcqHDc�HDdqHDd�HDeqHDe�HDfqHDf�HDgqHDg�HDhqHDh�HDiqHDi�HDjqHDj�HDkqHDk�HDlqHDl�HDmqHDm�HDnqHDn�HDoqHDo�HDpqHDp�HDqqHDq�HDrqHDr�HDsqHDs�HDtqHDt�HDuqHDu�HDvqHDv�HDwqHDw�HDxqHDx�HDyqHDy�HDzqHDz�HD{j�D{�HD|qHD|�HD}qHD}�HD~qHD~�HDqHD�HD�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�;�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D¸�D���D�8�D�x�Dø�D���D�8�D�x�Dĸ�D���D�8�D�x�DŸ�D���D�8�D�x�DƸ�D���D�8�D�x�DǸ�D���D�8�D�x�Dȸ�D���D�8�D�x�Dɸ�D���D�8�D�x�Dʸ�D���D�8�D�x�D˸�D���D�8�D�x�D̸�D���D�8�D�x�D͸�D���D�8�D�x�Dθ�D���D�8�D�x�Dϸ�D���D�8�D�x�Dи�D���D�8�D�x�DѸ�D���D�8�D�x�DҸ�D���D�8�D�x�DӸ�D���D�8�D�x�DԸ�D���D�8�D�x�Dո�D���D�8�D�x�Dָ�D���D�8�D�x�D׸�D���D�8�D�x�Dظ�D���D�8�D�x�Dٸ�D���D�8�D�x�Dڸ�D���D�8�D�x�D۸�D���D�8�D�x�Dܸ�D���D�8�D�x�Dݸ�D���D�8�D�x�D޸�D���D�8�D�x�D߸�D���D�8�D�x�DฤD���D�8�D�x�DḤD���D�8�D�x�D⸤D���D�8�D�x�D㸤D���D�8�D�x�D两D���D�8�D�x�D帤D���D�8�D�x�D渤D���D�8�D�x�D縤D���D�8�D�x�D踤D���D�8�D�x�D鸤D���D�8�D�x�D긤D���D�8�D�x�D븤D���D�8�D�x�D츤D���D�8�D�x�D���D���D�8�D�x�DD���D�8�D�x�D︤D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D��qD���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�/
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AˁA˅SA˅�A˅�A˅SA˅�AˆYAˉ�Aˎ�Aˌ~AˎVAˌ�Aˊ�Aˎ�Aˑ�A˔{A˕�A˕�A˖A˕A˖Aˊ�A˅�Aˉ�Aˊ�Aˊ	AˉAˉ�AˍPA�y�A�\]A�P�A�OvA�NpA�C�A��
Aʇ�A�YA�eA� A�	A��,Aȳ�A�e�A��A�A�A��RA��bA��VA��KA��uA�:�A�n�A��9A���A�hA���A�	�A��$A�1A���A�Q�A��A�)_A��A�M6A���A�ʌA�ޞA��A��
A�s�A��%A���A��EA��nA���A��A�7A�9$A�<A��A��A�^�A��[A	�A},=Az_Av0�At�@Ao�	Al9�Ah��Ag�Ae�Ad_Ab6A_��A]��AZ \AW��AU�0AQ��ANe,AKɆAH�|AG�KAF'RAD��ACB[A@��A>҉A<@OA9�vA84A7��A7uA5��A3�/A2��A1ȴA1�A0�)A/TaA-ںA-c�A,E9A)�A)_�A)C�A(��A'�>A&��A%�BA%�A$�DA#zxA"�A!
=Aa�A�A(�AںA��A�SAV�A��Al�A��A�ZA�8AϫA&�A��ASA_�A�A�HAA�pAh
A<�A��A��A/�A
��A
Z�A	�A	u�A	jA	}VA��AdZA�eAVA  A�AA��AJ#A�pA�	A�A-A�A��A�3A�+A �*A �@�(�@�!@��`@��@��9@��:@�m]@��@���@��@���@�"h@���@�X@�F�@�dZ@��@�l�@�<�@�b�@��s@�2�@�o�@�ߤ@�{J@�oi@��Z@�'�@��@���@�O�@줩@�+�@��@�3�@���@ꠐ@�X�@�%@�[W@�e,@��P@�Z�@�S�@��@�2a@�5?@�S@�Ov@��;@�hs@��@��|@�  @��y@�`�@��1@�;d@�f�@�h�@ܼj@ܧ@�h�@�Ov@�#:@ڮ}@��@�ϫ@٦�@م�@�U�@�@@��@�s�@��@�l�@ՠ'@բ�@��K@��?@�)�@�ȴ@љ�@е�@�+@̝I@ˊ	@ˆ�@˝�@�G�@���@�q@�&@�q@��@��p@Ϋ6@ͼ�@̞�@�K^@˻0@ʄ�@��T@ɫ�@�l�@�Q�@���@�Ta@��@��@ǜ�@��@ƺ�@�z@�hs@Ĺ$@ć�@�/�@�Vm@°!@�Ft@��+@���@���@�.I@��}@�&�@�x�@�M�@�x@��@���@��@�	l@�
=@�%@�ѷ@�c�@��@���@���@���@�l�@�2�@��@�X@�ߤ@���@�{�@�i�@�`�@�PH@��{@�e�@�?@��#@���@���@���@�O@�1�@�#�@��@�H�@�F�@��@�Y@�B[@�.�@���@��@�n/@�:�@�!-@��m@�4@���@�{J@�"�@��5@�E9@��m@���@��Y@�M@�*�@�4@��o@�/�@�
�@��t@���@��7@�s�@�E9@���@�tT@�o�@�>�@��@�*�@��@��d@���@��{@�P�@�33@�P�@�Y@��X@��@��@���@���@�8@�W?@�A�@�͟@��p@���@��@�i�@�m�@�oi@�$@��D@��+@��-@�J#@�)_@���@���@�h
@���@��X@��@�b�@�\)@�A @�C@��@��9@�H@���@��@���@�%F@���@���@���@��+@��A@���@��r@���@���@�S�@�}�@�j�@�L�@��y@���@��z@�i�@�b@��@�a�@��@���@�W�@���@�}�@���@���@���@�w�@�_@�$@�G@��>@��z@��@�hs@���@��@��Y@���@�u%@�D�@���@�hs@��@��$@�Xy@�Ft@��A@��$@�P�@�%F@��@��b@�l"@��@��a@���@�Mj@��8@��b@��@�_@���@���@�w2@�F�@��@���@� �@��Q@���@�t�@�f�@�-w@���@���@�~�@�G@��'@���@�}�@�"�@���@�W�@���@�w2@��@��B@�9X@�@��C@�\)@���@�)�@���@�O@��@���@���@�q@�c @���@��@�b�@��M@���@�|�@�j@�\�@�5?@�b@��F@�x�@�4�@�%@� i@��K@�ں@���@�z@�YK@�@�@��@ƨ@��@��@��@'�@~�<@~z@~3�@}��@}�@}?}@}0�@|�	@|e�@{��@{iD@{6z@zں@z�A@yG�@xm�@x�@w��@w=@vȴ@v͟@v�m@v}V@vO@u�@u��@urG@u?}@t�@t�@t��@s�@sy�@r�b@r3�@q��@qrG@p�@p�@p�@o�W@o�r@o�g@o;d@n�"@n�y@n��@n@�@nv�@mԕ@m�H@m��@l�$@l  @k��@kS�@j��@j�X@jz@i��@iq@h�o@hy>@h��@h��@g��@f�6@f�@fc @f-@ew2@eS&@e�M@e \@d�@d@c��@c+@b�R@b��@bYK@b?@a��@ap�@aF@a	l@`�@_�@_y�@^ߤ@^u@]�M@\Ĝ@\�@\]d@[��@[;d@Z��@ZO@Y��@Y�@Yԕ@Y@Y=�@X�)@XV�@X  @W/�@V�@V��@Vv�@U��@T�@T��@T��@TPH@T!@S�Q@S�@@S�f@SO@S�@Rs�@Q*0@P�K@Pz�@P4n@O��@O�@O��@O@N�@Nff@N:*@N.�@M�H@M \@M�@M%@L��@L�	@L�@M0�@M;@L�@LXy@K�]@K��@K~�@K/�@KY@J��@J�r@I��@H��@H��@H�U@HĜ@H�v@H�5@H��@HFt@G��@G��@Gqv@G8@G i@F��@F�6@F��@F\�@F3�@F�@F�@E��@D�O@C��@C4�@Bh
@A�j@A�C@A�n@A��@ArG@A+@@�f@@�/@@w�@@V�@@I�@@<�@?��@?�@?��@?b�@>�H@>�@>��@>��@>��@>L0@>	@=�n@=Y�@=0�@=V@<�K@<��@<PH@<~@;��@;X�@;&@:ߤ@:V@:1�@:1�@:J@9�@9j@9	l@8�@8��@8��@8[�@8@7�@7C�@7�@6�@6�@6ȴ@6��@6l�@6	@5��@5�n@5�@5c�@4��@4q@4$@3�}@3��@3!-@2�B@2�L@2W�@2{@1�^@1f�@1f�@1L�@1@0��@0/�@0G@/�A@/��@/|�@/U�@/6z@/�@.�@.��@.n�@.C�@-��@-�h@-N<@-�@,��@,1'@,!@+�r@+��@+y�@+J#@+�@*�@*�@*YK@*:*@*J@)ԕ@)��@)+�@(�@(Xy@($@(7@'��@'� @'�k@'X�@&��@&�+@&Ov@&)�@%��@%@%�=@%Q�@$��@$�?@$y>@$e�@$<�@$@$  @#�g@#�@#E9@"͟@"�L@"�A@"xl@"GE@!�.@!hs@!F@!�@ ��@ ��@ �@ g8@ C-@ *�@�Q@�@v`@O@�@��@3�@0U@&�@�@�~@j@IR@�@��@l"@2�@�@��@b�@;d@�@�@�y@ȴ@��@v�@J�@?@�@�z@��@X@Dg@2a@%F@��@��@�e@U2@�@��@o�@F�@4�@"�@�@ߤ@��@YK@B[@O@��@�>@@��@�S@��@(�@�@�@�E@��@z�@S�@1'@�@��@ݘ@��@e�@>�@�@�M@�B@��@��@��@��@�A@d�@�@�o@�d@u�@X@5�@��@��@�Y@|�@l"@j@j@[�@D�@�@�
@�@@H�@@�@�H@��@�x@�\@Q@�@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AˁA˅SA˅�A˅�A˅SA˅�AˆYAˉ�Aˎ�Aˌ~AˎVAˌ�Aˊ�Aˎ�Aˑ�A˔{A˕�A˕�A˖A˕A˖Aˊ�A˅�Aˉ�Aˊ�Aˊ	AˉAˉ�AˍPA�y�A�\]A�P�A�OvA�NpA�C�A��
Aʇ�A�YA�eA� A�	A��,Aȳ�A�e�A��A�A�A��RA��bA��VA��KA��uA�:�A�n�A��9A���A�hA���A�	�A��$A�1A���A�Q�A��A�)_A��A�M6A���A�ʌA�ޞA��A��
A�s�A��%A���A��EA��nA���A��A�7A�9$A�<A��A��A�^�A��[A	�A},=Az_Av0�At�@Ao�	Al9�Ah��Ag�Ae�Ad_Ab6A_��A]��AZ \AW��AU�0AQ��ANe,AKɆAH�|AG�KAF'RAD��ACB[A@��A>҉A<@OA9�vA84A7��A7uA5��A3�/A2��A1ȴA1�A0�)A/TaA-ںA-c�A,E9A)�A)_�A)C�A(��A'�>A&��A%�BA%�A$�DA#zxA"�A!
=Aa�A�A(�AںA��A�SAV�A��Al�A��A�ZA�8AϫA&�A��ASA_�A�A�HAA�pAh
A<�A��A��A/�A
��A
Z�A	�A	u�A	jA	}VA��AdZA�eAVA  A�AA��AJ#A�pA�	A�A-A�A��A�3A�+A �*A �@�(�@�!@��`@��@��9@��:@�m]@��@���@��@���@�"h@���@�X@�F�@�dZ@��@�l�@�<�@�b�@��s@�2�@�o�@�ߤ@�{J@�oi@��Z@�'�@��@���@�O�@줩@�+�@��@�3�@���@ꠐ@�X�@�%@�[W@�e,@��P@�Z�@�S�@��@�2a@�5?@�S@�Ov@��;@�hs@��@��|@�  @��y@�`�@��1@�;d@�f�@�h�@ܼj@ܧ@�h�@�Ov@�#:@ڮ}@��@�ϫ@٦�@م�@�U�@�@@��@�s�@��@�l�@ՠ'@բ�@��K@��?@�)�@�ȴ@љ�@е�@�+@̝I@ˊ	@ˆ�@˝�@�G�@���@�q@�&@�q@��@��p@Ϋ6@ͼ�@̞�@�K^@˻0@ʄ�@��T@ɫ�@�l�@�Q�@���@�Ta@��@��@ǜ�@��@ƺ�@�z@�hs@Ĺ$@ć�@�/�@�Vm@°!@�Ft@��+@���@���@�.I@��}@�&�@�x�@�M�@�x@��@���@��@�	l@�
=@�%@�ѷ@�c�@��@���@���@���@�l�@�2�@��@�X@�ߤ@���@�{�@�i�@�`�@�PH@��{@�e�@�?@��#@���@���@���@�O@�1�@�#�@��@�H�@�F�@��@�Y@�B[@�.�@���@��@�n/@�:�@�!-@��m@�4@���@�{J@�"�@��5@�E9@��m@���@��Y@�M@�*�@�4@��o@�/�@�
�@��t@���@��7@�s�@�E9@���@�tT@�o�@�>�@��@�*�@��@��d@���@��{@�P�@�33@�P�@�Y@��X@��@��@���@���@�8@�W?@�A�@�͟@��p@���@��@�i�@�m�@�oi@�$@��D@��+@��-@�J#@�)_@���@���@�h
@���@��X@��@�b�@�\)@�A @�C@��@��9@�H@���@��@���@�%F@���@���@���@��+@��A@���@��r@���@���@�S�@�}�@�j�@�L�@��y@���@��z@�i�@�b@��@�a�@��@���@�W�@���@�}�@���@���@���@�w�@�_@�$@�G@��>@��z@��@�hs@���@��@��Y@���@�u%@�D�@���@�hs@��@��$@�Xy@�Ft@��A@��$@�P�@�%F@��@��b@�l"@��@��a@���@�Mj@��8@��b@��@�_@���@���@�w2@�F�@��@���@� �@��Q@���@�t�@�f�@�-w@���@���@�~�@�G@��'@���@�}�@�"�@���@�W�@���@�w2@��@��B@�9X@�@��C@�\)@���@�)�@���@�O@��@���@���@�q@�c @���@��@�b�@��M@���@�|�@�j@�\�@�5?@�b@��F@�x�@�4�@�%@� i@��K@�ں@���@�z@�YK@�@�@��@ƨ@��@��@��@'�@~�<@~z@~3�@}��@}�@}?}@}0�@|�	@|e�@{��@{iD@{6z@zں@z�A@yG�@xm�@x�@w��@w=@vȴ@v͟@v�m@v}V@vO@u�@u��@urG@u?}@t�@t�@t��@s�@sy�@r�b@r3�@q��@qrG@p�@p�@p�@o�W@o�r@o�g@o;d@n�"@n�y@n��@n@�@nv�@mԕ@m�H@m��@l�$@l  @k��@kS�@j��@j�X@jz@i��@iq@h�o@hy>@h��@h��@g��@f�6@f�@fc @f-@ew2@eS&@e�M@e \@d�@d@c��@c+@b�R@b��@bYK@b?@a��@ap�@aF@a	l@`�@_�@_y�@^ߤ@^u@]�M@\Ĝ@\�@\]d@[��@[;d@Z��@ZO@Y��@Y�@Yԕ@Y@Y=�@X�)@XV�@X  @W/�@V�@V��@Vv�@U��@T�@T��@T��@TPH@T!@S�Q@S�@@S�f@SO@S�@Rs�@Q*0@P�K@Pz�@P4n@O��@O�@O��@O@N�@Nff@N:*@N.�@M�H@M \@M�@M%@L��@L�	@L�@M0�@M;@L�@LXy@K�]@K��@K~�@K/�@KY@J��@J�r@I��@H��@H��@H�U@HĜ@H�v@H�5@H��@HFt@G��@G��@Gqv@G8@G i@F��@F�6@F��@F\�@F3�@F�@F�@E��@D�O@C��@C4�@Bh
@A�j@A�C@A�n@A��@ArG@A+@@�f@@�/@@w�@@V�@@I�@@<�@?��@?�@?��@?b�@>�H@>�@>��@>��@>��@>L0@>	@=�n@=Y�@=0�@=V@<�K@<��@<PH@<~@;��@;X�@;&@:ߤ@:V@:1�@:1�@:J@9�@9j@9	l@8�@8��@8��@8[�@8@7�@7C�@7�@6�@6�@6ȴ@6��@6l�@6	@5��@5�n@5�@5c�@4��@4q@4$@3�}@3��@3!-@2�B@2�L@2W�@2{@1�^@1f�@1f�@1L�@1@0��@0/�@0G@/�A@/��@/|�@/U�@/6z@/�@.�@.��@.n�@.C�@-��@-�h@-N<@-�@,��@,1'@,!@+�r@+��@+y�@+J#@+�@*�@*�@*YK@*:*@*J@)ԕ@)��@)+�@(�@(Xy@($@(7@'��@'� @'�k@'X�@&��@&�+@&Ov@&)�@%��@%@%�=@%Q�@$��@$�?@$y>@$e�@$<�@$@$  @#�g@#�@#E9@"͟@"�L@"�A@"xl@"GE@!�.@!hs@!F@!�@ ��@ ��@ �@ g8@ C-@ *�@�Q@�@v`@O@�@��@3�@0U@&�@�@�~@j@IR@�@��@l"@2�@�@��@b�@;d@�@�@�y@ȴ@��@v�@J�@?@�@�z@��@X@Dg@2a@%F@��@��@�e@U2@�@��@o�@F�@4�@"�@�@ߤ@��@YK@B[@O@��@�>@@��@�S@��@(�@�@�@�E@��@z�@S�@1'@�@��@ݘ@��@e�@>�@�@�M@�B@��@��@��@��@�A@d�@�@�o@�d@u�@X@5�@��@��@�Y@|�@l"@j@j@[�@D�@�@�
@�@@H�@@�@�H@��@�x@�\@Q@�@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�zB	�B	�B	�B	�B	�B	��B	�B	�B	��B	�B	��B	��B	�B	�zB	�B	��B	��B	��B	�B	�B	��B	�:B	�TB	�nB	� B	�TB	�:B	�TB	��B	��B	�B	��B	޸B	��B	��B	�:B	уB	ѷB	��B	ѷB	�oB	�B	��B	��B	�qB
�B
MB
33B
a|B
eFB
h�B
��B
��B
�XB
�B
�B
� B
z�B
sB
�fB
��B
��B
��B
��B
�B
�UB
ĜB
�uB
�XB
��B
�{B
�jB
�B
��B
{�B
d�B
G+B
.�B
9B
�B	� B	�=B	��B	��B	��B	�}B	�GB	x8B	l�B	XB	HfB	9rB	4B	.IB	&�B	5B	�B	
�B	�B	�B��B�B�;BؓB�#BΥB��B�B��B�B��B��B�B�B�]B��B��B�FB�B�HB�BB�_B�WB�
B�&B��B��B�:B�FB�9B��B�B��B�B��B�FB��B�mB�_B�_B��B��B��B��B�~B�B�kB�^B��B��B�VB�vB�uB��B�bB��B�B�nB�kB�wB�0B�qB��B	GB	^B	�B	�B	�B	B	%B	$�B	$�B	#B	 �B	�B	 �B	$ZB	(�B	'mB	&LB	%B	*B	'�B	%�B	(sB	&fB	�B	�B	gB	gB	+B	'RB	<�B	AoB	=VB	8B	88B	:^B	>�B	@�B	BB	B'B	C{B	GEB	F�B	FtB	T�B	^B	_!B	b�B	h�B	l�B	y�B	�SB	�SB	�qB	�EB	��B	��B	�^B	�-B	�^B	�4B	��B	�\B	��B	�B	�[B	��B	��B	�B	��B	�B	��B	��B	��B	��B	�B	�B	��B	�B	�dB	�XB	�B	�>B	��B	�6B	��B	�?B	��B	��B	��B	��B	�B	��B	��B	��B	�6B	�qB	�wB	��B	��B	�]B	��B	��B	��B	��B	�BB	��B	�(B	�DB	�	B	��B	��B	�$B	��B	��B	��B	�B	��B	�RB	�#B	�JB	�<B	�<B	��B	̈́B	�^B	�B	��B	��B	�#B	ʌB	ʦB	��B	�JB	�B	�6B	�"B	ϑB	��B	�B	��B	�,B	өB	��B	�aB	�gB	�2B	�B	�SB	ևB	�9B	ևB	רB	��B	׍B	�9B	��B	�SB	ևB	�sB	��B	��B	��B	�EB	ؓB	�yB	��B	�B	ٴB	��B	�7B	�=B	�B	�/B	�~B	�~B	ݘB	�dB	�B	޸B	�!B	߾B	�pB	��B	��B	�jB	ޞB	��B	�VB	߾B	��B	�B	��B	�'B	�B	ݲB	�OB	�jB	�jB	�;B	�BB	��B	�vB	��B	�TB	�B	�*B	�wB	�B	�UB	��B	�TB	�9B	��B	��B	�`B	�|B	��B	�B	�B	�AB	��B	��B	��B	�B	�B	�B	��B	�B	�IB	�IB	�B	�iB	��B	��B	�B	�GB	�B	��B	��B	��B	�6B	��B	�BB	��B	�(B
�B
gB
�B
B
�B
�B
B
�B
�B
1B
�B
�B
	B

#B

�B

rB
DB

�B
JB
�B
6B
jB
B
�B
0B

�B

rB

�B
	�B

�B

�B

�B

�B
dB
0B
~B
�B
�B
�B
�B
B
hB
[B
�B
B
FB
�B
gB
�B
B
9B
�B
�B
?B
sB
YB
�B
sB
�B
�B
B
�B
B
_B
�B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
=B
#B
�B
)B
�B
�B
B
�B
�B
B
!B
�B
�B
 �B
 �B
!�B
!�B
!|B
!�B
!HB
!�B
!�B
"NB
"hB
"NB
"�B
"�B
"�B
"hB
"�B
#�B
#�B
&2B
&�B
'8B
'RB
&�B
%FB
%,B
$�B
%FB
%�B
&2B
&B
&fB
'�B
'�B
'�B
(
B
(>B
(�B
)DB
)DB
)�B
(�B
)*B
*B
)�B
*KB
*B
*B
*KB
*B
*eB
*�B
*�B
,"B
,�B
-CB
-�B
./B
.}B
.�B
.�B
.�B
/B
/iB
0;B
1vB
2�B
2�B
3MB
3MB
3�B
49B
4TB
4TB
4B
4�B
5�B
5�B
5%B
4�B
4�B
4B
3�B
2�B
3�B
4B
4�B
5B
5�B
8B
8�B
8�B
8�B
:B
:DB
:^B
:*B
:*B
:B
9�B
9XB
8�B
8B
8B
7�B
8�B
8�B
9>B
9�B
9�B
:B
:*B
:�B
;JB
;B
<B
="B
="B
=qB
>�B
>�B
>BB
>B
>�B
>�B
>wB
>�B
?B
?HB
?�B
@�B
A�B
B�B
C-B
B'B
A�B
B[B
B[B
B�B
B�B
DB
C�B
DgB
DB
D�B
D�B
ESB
E�B
EmB
EmB
E�B
E�B
F%B
FYB
F�B
F�B
FtB
FB
EmB
E�B
FB
GzB
G�B
F�B
F�B
IB
IB
I7B
IRB
I�B
I�B
KDB
KDB
K�B
K^B
KB
KDB
KDB
K)B
KB
JrB
I�B
MB
M�B
M�B
NB
N"B
N�B
N�B
O(B
OvB
M�B
O�B
OBB
O\B
PB
P�B
P�B
PHB
O�B
O�B
O�B
P.B
P�B
P}B
Q B
Q�B
Q�B
Q�B
Q�B
S&B
S�B
SuB
SuB
S@B
S�B
UB
T�B
T�B
U�B
U�B
UMB
T�B
T�B
UMB
U�B
V�B
W$B
W�B
X_B
X�B
YB
YKB
Y�B
Y�B
ZB
ZB
ZB
ZQB
ZQB
ZkB
Z7B
ZkB
[#B
[=B
[qB
\�B
]IB
]dB
]~B
]dB
]�B
^B
^B
^B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_;B
_�B
`'B
`B
`'B
`'B
`BB
`\B
`�B
`�B
`�B
`�B
`�B
`�B
a-B
`�B
a�B
b4B
bhB
b�B
cTB
c:B
c B
cTB
c�B
c�B
d&B
d&B
d@B
d@B
dtB
d�B
d�B
e`B
ezB
ezB
e�B
e�B
e�B
e�B
e�B
fB
f�B
ffB
ffB
f�B
gB
f�B
f�B
g8B
g�B
g�B
h>B
hXB
h$B
h�B
iB
iB
i*B
i_B
i�B
i�B
jB
i�B
j0B
jKB
jeB
jKB
jKB
jeB
j�B
j�B
j�B
kB
k�B
k�B
k�B
lWB
l�B
l�B
l�B
m)B
mCB
mCB
m]B
mwB
m�B
m�B
m�B
n/B
nIB
n}B
n�B
oOB
o�B
o�B
o�B
o�B
o�B
p!B
p;B
p�B
p�B
qAB
q[B
qvB
q�B
q�B
rB
raB
r|B
r�B
r�B
r�B
r�B
r�B
sB
sB
s�B
tB
tB
tB
tB
tTB
tnB
u%B
uB
u%B
u?B
utB
u�B
u�B
u�B
vB
v�B
vzB
v�B
v�B
wB
w�B
w�B
w�B
w�B
xB
xlB
x�B
x�B
y	B
yrB
y�B
y�B
y�B
z�B
zxB
z�B
z�B
z�B
{B
{0B
{0B
{JB
{B
{B
{dB
{JB
{�B
{�B
{�B
{�B
|B
|PB
|�B
|�B
|�B
}B
}�B
}�B
}�B
}�B
}�B
}�B
~B
~wB
~�B
~�B
~�B
B
~�B
.B
HB
HB
HB
�B
�B
�B
�B
� B
�4B
�iB
��B
��B
��B
��B
� B
�UB
�oB
��B
��B
��B
�B
�'B
�AB
�[B
�uB
�uB
��B
��B
�-B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�SB
�mB
��B
��B
��B
�?B
�tB
�YB
�YB
��B
��B
�B
�+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�zB	�B	�B	�B	�B	�B	��B	�B	�B	��B	�B	��B	��B	�B	�zB	�B	��B	��B	��B	�B	�B	��B	�:B	�TB	�nB	� B	�TB	�:B	�TB	��B	��B	�B	��B	޸B	��B	��B	�:B	уB	ѷB	��B	ѷB	�oB	�B	��B	��B	�qB
�B
MB
33B
a|B
eFB
h�B
��B
��B
�XB
�B
�B
� B
z�B
sB
�fB
��B
��B
��B
��B
�B
�UB
ĜB
�uB
�XB
��B
�{B
�jB
�B
��B
{�B
d�B
G+B
.�B
9B
�B	� B	�=B	��B	��B	��B	�}B	�GB	x8B	l�B	XB	HfB	9rB	4B	.IB	&�B	5B	�B	
�B	�B	�B��B�B�;BؓB�#BΥB��B�B��B�B��B��B�B�B�]B��B��B�FB�B�HB�BB�_B�WB�
B�&B��B��B�:B�FB�9B��B�B��B�B��B�FB��B�mB�_B�_B��B��B��B��B�~B�B�kB�^B��B��B�VB�vB�uB��B�bB��B�B�nB�kB�wB�0B�qB��B	GB	^B	�B	�B	�B	B	%B	$�B	$�B	#B	 �B	�B	 �B	$ZB	(�B	'mB	&LB	%B	*B	'�B	%�B	(sB	&fB	�B	�B	gB	gB	+B	'RB	<�B	AoB	=VB	8B	88B	:^B	>�B	@�B	BB	B'B	C{B	GEB	F�B	FtB	T�B	^B	_!B	b�B	h�B	l�B	y�B	�SB	�SB	�qB	�EB	��B	��B	�^B	�-B	�^B	�4B	��B	�\B	��B	�B	�[B	��B	��B	�B	��B	�B	��B	��B	��B	��B	�B	�B	��B	�B	�dB	�XB	�B	�>B	��B	�6B	��B	�?B	��B	��B	��B	��B	�B	��B	��B	��B	�6B	�qB	�wB	��B	��B	�]B	��B	��B	��B	��B	�BB	��B	�(B	�DB	�	B	��B	��B	�$B	��B	��B	��B	�B	��B	�RB	�#B	�JB	�<B	�<B	��B	̈́B	�^B	�B	��B	��B	�#B	ʌB	ʦB	��B	�JB	�B	�6B	�"B	ϑB	��B	�B	��B	�,B	өB	��B	�aB	�gB	�2B	�B	�SB	ևB	�9B	ևB	רB	��B	׍B	�9B	��B	�SB	ևB	�sB	��B	��B	��B	�EB	ؓB	�yB	��B	�B	ٴB	��B	�7B	�=B	�B	�/B	�~B	�~B	ݘB	�dB	�B	޸B	�!B	߾B	�pB	��B	��B	�jB	ޞB	��B	�VB	߾B	��B	�B	��B	�'B	�B	ݲB	�OB	�jB	�jB	�;B	�BB	��B	�vB	��B	�TB	�B	�*B	�wB	�B	�UB	��B	�TB	�9B	��B	��B	�`B	�|B	��B	�B	�B	�AB	��B	��B	��B	�B	�B	�B	��B	�B	�IB	�IB	�B	�iB	��B	��B	�B	�GB	�B	��B	��B	��B	�6B	��B	�BB	��B	�(B
�B
gB
�B
B
�B
�B
B
�B
�B
1B
�B
�B
	B

#B

�B

rB
DB

�B
JB
�B
6B
jB
B
�B
0B

�B

rB

�B
	�B

�B

�B

�B

�B
dB
0B
~B
�B
�B
�B
�B
B
hB
[B
�B
B
FB
�B
gB
�B
B
9B
�B
�B
?B
sB
YB
�B
sB
�B
�B
B
�B
B
_B
�B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
=B
#B
�B
)B
�B
�B
B
�B
�B
B
!B
�B
�B
 �B
 �B
!�B
!�B
!|B
!�B
!HB
!�B
!�B
"NB
"hB
"NB
"�B
"�B
"�B
"hB
"�B
#�B
#�B
&2B
&�B
'8B
'RB
&�B
%FB
%,B
$�B
%FB
%�B
&2B
&B
&fB
'�B
'�B
'�B
(
B
(>B
(�B
)DB
)DB
)�B
(�B
)*B
*B
)�B
*KB
*B
*B
*KB
*B
*eB
*�B
*�B
,"B
,�B
-CB
-�B
./B
.}B
.�B
.�B
.�B
/B
/iB
0;B
1vB
2�B
2�B
3MB
3MB
3�B
49B
4TB
4TB
4B
4�B
5�B
5�B
5%B
4�B
4�B
4B
3�B
2�B
3�B
4B
4�B
5B
5�B
8B
8�B
8�B
8�B
:B
:DB
:^B
:*B
:*B
:B
9�B
9XB
8�B
8B
8B
7�B
8�B
8�B
9>B
9�B
9�B
:B
:*B
:�B
;JB
;B
<B
="B
="B
=qB
>�B
>�B
>BB
>B
>�B
>�B
>wB
>�B
?B
?HB
?�B
@�B
A�B
B�B
C-B
B'B
A�B
B[B
B[B
B�B
B�B
DB
C�B
DgB
DB
D�B
D�B
ESB
E�B
EmB
EmB
E�B
E�B
F%B
FYB
F�B
F�B
FtB
FB
EmB
E�B
FB
GzB
G�B
F�B
F�B
IB
IB
I7B
IRB
I�B
I�B
KDB
KDB
K�B
K^B
KB
KDB
KDB
K)B
KB
JrB
I�B
MB
M�B
M�B
NB
N"B
N�B
N�B
O(B
OvB
M�B
O�B
OBB
O\B
PB
P�B
P�B
PHB
O�B
O�B
O�B
P.B
P�B
P}B
Q B
Q�B
Q�B
Q�B
Q�B
S&B
S�B
SuB
SuB
S@B
S�B
UB
T�B
T�B
U�B
U�B
UMB
T�B
T�B
UMB
U�B
V�B
W$B
W�B
X_B
X�B
YB
YKB
Y�B
Y�B
ZB
ZB
ZB
ZQB
ZQB
ZkB
Z7B
ZkB
[#B
[=B
[qB
\�B
]IB
]dB
]~B
]dB
]�B
^B
^B
^B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_;B
_�B
`'B
`B
`'B
`'B
`BB
`\B
`�B
`�B
`�B
`�B
`�B
`�B
a-B
`�B
a�B
b4B
bhB
b�B
cTB
c:B
c B
cTB
c�B
c�B
d&B
d&B
d@B
d@B
dtB
d�B
d�B
e`B
ezB
ezB
e�B
e�B
e�B
e�B
e�B
fB
f�B
ffB
ffB
f�B
gB
f�B
f�B
g8B
g�B
g�B
h>B
hXB
h$B
h�B
iB
iB
i*B
i_B
i�B
i�B
jB
i�B
j0B
jKB
jeB
jKB
jKB
jeB
j�B
j�B
j�B
kB
k�B
k�B
k�B
lWB
l�B
l�B
l�B
m)B
mCB
mCB
m]B
mwB
m�B
m�B
m�B
n/B
nIB
n}B
n�B
oOB
o�B
o�B
o�B
o�B
o�B
p!B
p;B
p�B
p�B
qAB
q[B
qvB
q�B
q�B
rB
raB
r|B
r�B
r�B
r�B
r�B
r�B
sB
sB
s�B
tB
tB
tB
tB
tTB
tnB
u%B
uB
u%B
u?B
utB
u�B
u�B
u�B
vB
v�B
vzB
v�B
v�B
wB
w�B
w�B
w�B
w�B
xB
xlB
x�B
x�B
y	B
yrB
y�B
y�B
y�B
z�B
zxB
z�B
z�B
z�B
{B
{0B
{0B
{JB
{B
{B
{dB
{JB
{�B
{�B
{�B
{�B
|B
|PB
|�B
|�B
|�B
}B
}�B
}�B
}�B
}�B
}�B
}�B
~B
~wB
~�B
~�B
~�B
B
~�B
.B
HB
HB
HB
�B
�B
�B
�B
� B
�4B
�iB
��B
��B
��B
��B
� B
�UB
�oB
��B
��B
��B
�B
�'B
�AB
�[B
�uB
�uB
��B
��B
�-B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�SB
�mB
��B
��B
��B
�?B
�tB
�YB
�YB
��B
��B
�B
�+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230618154220  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230618154232  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230618154233  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230618154234                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230618154234  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230618154234  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230618162442                      G�O�G�O�G�O�                