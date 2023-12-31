CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-06-06T21:35:09Z creation;2018-06-06T21:35:13Z conversion to V3.1;2019-12-19T07:40:33Z update;     
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
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20180606213509  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_247                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�hse�À1   @�ht)�� @:
͞��&�d?�?��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�3C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DSfDS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ D˃3D��3D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D���D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�<�Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @qG�@���@���AQ�A<Q�A\Q�A|Q�A�(�A�(�A�(�A�(�A�(�A�(�A�\)A�(�B{B{B{B{B'{B/{B7{B?{BG{BO{BW{B_{Bg{Bo{Bw{B{B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=BÊ=BǊ=Bˊ=Bϊ=Bӊ=B׊=Bۊ=Bߊ=B�=B�=B�=B�=B�=B��=B��=B��=C�C�CxRC�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D qHD �HDqHD�HDqHD�HDqHD�HDw�D�HDqHD�HDqHD�HDqHD�HDqHD�HD	qHD	�HD
qHD
�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HD qHD �HD!qHD!�HD"qHD"�HD#qHD#�HD$qHD$�HD%qHD%�HD&qHD&�HD'qHD'�HD(qHD(�HD)qHD)�HD*qHD*�HD+qHD+�HD,qHD,�HD-qHD-�HD.qHD.�HD/qHD/�HD0qHD0�HD1qHD1�HD2qHD2�HD3qHD3�HD4qHD4�HD5qHD5�HD6qHD6�HD7qHD7�HD8qHD8�HD9qHD9�HD:qHD:�HD;qHD;�HD<qHD<�HD=qHD=�HD>qHD>�HD?qHD?�HD@qHD@�HDAqHDA�HDBqHDB�HDCqHDC�HDDqHDD�HDEqHDE�HDFqHDF�HDGqHDG�HDHqHDH�HDIqHDI�HDJqHDJ�HDKqHDK�HDLqHDL�HDMqHDM�HDNqHDN�HDOqHDO�HDPqHDP�HDQqHDQ�HDRqHDR��DSqHDS�HDTqHDT�HDUqHDU�HDVqHDV�HDWqHDW�HDXqHDX�HDYqHDY�HDZqHDZ�HD[qHD[�HD\qHD\�HD]qHD]�HD^qHD^�HD_qHD_�HD`qHD`�HDaqHDa�HDbqHDb�HDcqHDc�HDdqHDd�HDeqHDe�HDfqHDf�HDgqHDg�HDhqHDh�HDiqHDi�HDjqHDj�HDkqHDk�HDlqHDl�HDmqHDm�HDnqHDn�HDoqHDo�HDpqHDp�HDqqHDq�HDrqHDr�HDsqHDs�HDtqHDt�HDuqHDu�HDvqHDv�HDwqHDw�HDxqHDx�HDyqHDy�HDzqHDz�HD{qHD{�HD|qHD|�HD}qHD}�HD~qHD~�HDqHD�HD�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�uqD¸�D���D�8�D�x�Dø�D���D�8�D�x�Dĸ�D���D�8�D�x�DŸ�D���D�8�D�x�DƸ�D���D�8�D�x�DǸ�D���D�8�D�x�Dȸ�D���D�8�D�x�Dɸ�D���D�8�D�x�Dʸ�D���D�8�D�{�D˻�D���D�8�D�x�D̸�D���D�8�D�x�D͸�D���D�8�D�x�Dθ�D���D�8�D�x�Dϸ�D���D�8�D�x�Dи�D���D�8�D�x�DѸ�D���D�8�D�x�DҸ�D���D�8�D�x�DӸ�D���D�8�D�x�DԸ�D���D�8�D�x�Dո�D���D�8�D�x�Dָ�D���D�8�D�x�D׸�D��qD�8�D�x�Dظ�D���D�8�D�x�Dٸ�D���D�5qD�x�Dڸ�D���D�8�D�x�D۸�D���D�8�D�x�Dܸ�D���D�8�D�x�Dݸ�D���D�8�D�x�D޸�D���D�8�D�x�D߸�D���D�8�D�x�DฤD���D�8�D�x�DḤD���D�8�D�x�D⸤D���D�8�D�x�D㸤D���D�8�D�x�D两D���D�8�D�x�D帤D���D�8�D�x�D渤D���D�8�D�x�D縤D���D�8�D�x�D踤D���D�8�D�x�D鸤D���D�8�D�x�D긤D���D�8�D�x�D븤D���D�8�D�x�D츤D���D�8�D�x�D���D���D�8�D�x�DD���D�8�D�x�D︤D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�;�D�eq11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�l�A�ffA�hsA�ZA�VA�O�A�E�A�9XA�{A��TA�;dA��A��
A�n�A��FA��\A��A�%A�=qA�5?A��A�I�A��A�VA��jA�7LA���A��A��A�"�A�`BA���A���A�^5A�z�A�A�|�A���A�$�A�ZA�ffA�JA�A��A�I�A���A�hsA���A�"�A�%A�C�A�x�A�VA�z�A���A�hsA�;dA� �A�+A��uA��A���A��yA�ȴA��7A�ƨA��A�E�A���A���A���A��A��A}�wA|A�Ay�
Ax��Ax��Av�HAu?}At��As�Ar�Aq��Ao�AnVAm+AkC�Ajz�Ai��Ai
=AhbAe�Ad �Ac\)Ab�Ab��Ab1Aa?}A`M�A_��A^�/A^��A]�;A]%A\��A\  AZ��AZQ�AYx�AYoAX��AX9XAU��ATI�ASx�AR��AR��AR  AQx�AO��AN1'AL�jALM�AL�ALbAK�-AK7LAJ��AI�;AH��AG��AFQ�AE+AE%ADM�AC�AB~�AAoA@I�A?��A?
=A>�+A=�-A=XA=+A<�A;&�A:bNA:{A9��A9�-A9O�A8v�A7�hA6�HA6ffA5+A3XA1��A0��A0ffA0^5A0^5A/l�A,v�A,JA*�A*�A)�^A)�A(�A&z�A%�A%O�A$A�A#��A#XA"�A"�9A"z�A"Q�A!��A �DA�#A�A(�AA�AK�A�HA-A�AhsA�An�A��A�yA��A�mA�wAl�A"�A��A��AbA��Al�A��A�PAjAQ�AƨAx�AdZAAA�A��A�AE�AAC�A
�9A
Q�A
�A��AhsAM�A��A;dAQ�AƨAXAĜA�A ��A �jA �@���@�b@��H@���@��@�t�@��@���@�G�@�@�-@���@�@�K�@�n�@��T@�9X@�+@��@��@�u@�@�&�@��;@�K�@��@�Z@��@���@�z�@�b@�\)@��`@��@�dZ@�
=@�=q@�7L@��m@҇+@���@�|�@��T@̋D@��@�?}@���@ȣ�@�9X@��
@�n�@�r�@å�@\@��9@�x�@���@�z�@�1'@�  @��m@�l�@���@�@���@���@��!@��j@���@�S�@�
=@�ff@���@���@���@��`@�A�@�@���@��\@�M�@���@���@��w@��@���@��j@�1'@��P@�C�@���@�E�@��@��T@�@�X@��D@��m@�t�@��@�^5@�X@�(�@�o@�
=@���@�X@���@���@�J@���@�/@���@�o@��!@�~�@�J@�x�@�%@���@�9X@���@�l�@�33@�@��H@���@�ff@���@���@��7@�X@�G�@�?}@��@���@��/@�Ĝ@��@�bN@�  @���@��R@��^@�7L@��/@�z�@�bN@�I�@�9X@��m@��y@��R@��+@��@�hs@��/@��@�1'@���@�t�@�dZ@�\)@���@��\@�v�@�@�p�@�V@��D@�r�@�Z@��@�\)@�33@�"�@���@���@��\@�^5@�^5@�M�@�E�@�{@�x�@�?}@�/@��j@��j@���@��@�Q�@� �@�b@�1@�@�w@��@�P@l�@+@~�R@~V@~$�@}��@}��@}�@|j@|(�@{��@{ƨ@{33@z��@z��@z��@z��@z��@z�H@z��@z�!@z��@z=q@z�@zJ@y�#@y�^@y��@y�7@yX@x�`@xb@w\)@v��@v5?@u@up�@uO�@u/@t�@t�/@t�j@tj@tZ@s�
@r~�@qhs@q7L@p�`@p��@p1'@o|�@ol�@ol�@o
=@nff@nE�@m`B@l��@l�@lI�@l(�@l�@k�F@k"�@k@j�!@jn�@j-@i��@i�#@i�#@i��@i��@i�^@i�7@i7L@h��@hbN@gl�@g+@f�y@f��@e��@d��@d�/@d�@d��@dz�@d(�@c��@c@b�\@b^5@a��@ax�@`��@`�u@`�@`�@`b@_��@_�P@_�@^ff@^@]?}@\j@[C�@[33@["�@[33@[33@[@Z�H@Z��@Z~�@Zn�@ZM�@Y�@Y�^@Y�#@Y�#@Y�^@Y�@Y��@Yhs@Y�@X�@V�y@V{@U�T@U��@UO�@T��@TI�@T(�@S��@S�F@S"�@R�H@R��@RJ@Qhs@Q7L@Q&�@P�`@P�9@PbN@O�@O�P@O\)@N��@Nff@N$�@M��@M��@Mp�@MO�@M/@M�@L��@L�j@L�D@LI�@K��@J�@J�H@J��@Ihs@H�9@HbN@H1'@H �@G�w@G�P@G\)@F��@E�h@EO�@D�@D��@Dz�@C�F@C��@CC�@C@B�H@B��@B��@B~�@Bn�@B^5@BM�@B-@A��@A�#@A��@A��@A�^@A��@AG�@@��@@�@?�@>��@>��@>v�@>ff@>ff@>V@>$�@>@>@=�@=��@=�-@=/@=�@<�@<�D@;��@;�m@;�F@;"�@:��@:~�@:=q@:�@9�@9��@9x�@9X@8��@8�9@8��@8bN@81'@7�;@7l�@7
=@65?@5@5`B@4�/@4�j@4��@4�D@4z�@4z�@4Z@4(�@4�@4�@4�@41@3��@3ƨ@3��@3��@3�@3@2��@1��@0��@0��@0A�@01'@0b@0b@0  @/�;@/�w@/�P@/l�@/+@.�y@.��@-�@-�h@-?}@,�j@,�@+�
@+�F@+��@+�@+dZ@+"�@*�@*�H@*��@*~�@*M�@)�#@)x�@)G�@)7L@)�@)�@)�@)%@(��@(�`@(�`@(�`@(��@(��@(��@(�u@( �@&��@%@%`B@%?}@%/@%�@$�/@$9X@#ƨ@#��@#�@#dZ@#dZ@#"�@"��@!�#@!hs@!�@ ��@ ��@ �`@ ��@ bN@ Q�@ 1'@�@�@��@|�@K�@;d@+@��@�@�T@@��@�h@p�@?}@�/@�j@Z@�@1@��@�
@�F@�F@�@"�@o@@@�@��@��@-@��@hs@Ĝ@Q�@b@��@�P@;d@�@ȴ@�R@�R@�R@��@��@��@�+@�+@�+@�+@�+@v�@�+@v�@v�@ff@ff@V@V@$�@��@�-@p�@V@�j@z�@z�@Z@9X@(�@�@�
@�@S�@C�@"�@��@��@�!@��@��@��@��@��@��@�!@�!@n�@�7@&�@�@%@%@�`@Ĝ@��@Q�@�@��@�w@��@�P@l�@+@ȴ@�R@�R@��@V@{@@O�@V@�/@�@��@�D@j@j@j@Z@Z@9X@��@�
@ƨ@ƨ@ƨ@�F@��@dZ@
��@
�!@
M�@	�@	�7@	X@	G�@	7L@	%@��@�`@�`@�`@��@Ĝ@��@A�@  @�;@�P@�@�y@�@�@�y@��@
=@
=@��@�y@��@�@ȴ@��@v�@V@$�@{@��@�-@�-@�-@�-@��@�h@�@�@`B@/@�/@j@��@�@��@�F@��@dZ@C�@o@@�@��@��@n�@=q@�@J@��@��@�^@X@&�@ �`@ Ĝ@ �@ �@ r�@ Q�?��;?�;d?���?��?��?��?��?��?��?��?��?��?��R?��R?��R?��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�l�A�ffA�hsA�ZA�VA�O�A�E�A�9XA�{A��TA�;dA��A��
A�n�A��FA��\A��A�%A�=qA�5?A��A�I�A��A�VA��jA�7LA���A��A��A�"�A�`BA���A���A�^5A�z�A�A�|�A���A�$�A�ZA�ffA�JA�A��A�I�A���A�hsA���A�"�A�%A�C�A�x�A�VA�z�A���A�hsA�;dA� �A�+A��uA��A���A��yA�ȴA��7A�ƨA��A�E�A���A���A���A��A��A}�wA|A�Ay�
Ax��Ax��Av�HAu?}At��As�Ar�Aq��Ao�AnVAm+AkC�Ajz�Ai��Ai
=AhbAe�Ad �Ac\)Ab�Ab��Ab1Aa?}A`M�A_��A^�/A^��A]�;A]%A\��A\  AZ��AZQ�AYx�AYoAX��AX9XAU��ATI�ASx�AR��AR��AR  AQx�AO��AN1'AL�jALM�AL�ALbAK�-AK7LAJ��AI�;AH��AG��AFQ�AE+AE%ADM�AC�AB~�AAoA@I�A?��A?
=A>�+A=�-A=XA=+A<�A;&�A:bNA:{A9��A9�-A9O�A8v�A7�hA6�HA6ffA5+A3XA1��A0��A0ffA0^5A0^5A/l�A,v�A,JA*�A*�A)�^A)�A(�A&z�A%�A%O�A$A�A#��A#XA"�A"�9A"z�A"Q�A!��A �DA�#A�A(�AA�AK�A�HA-A�AhsA�An�A��A�yA��A�mA�wAl�A"�A��A��AbA��Al�A��A�PAjAQ�AƨAx�AdZAAA�A��A�AE�AAC�A
�9A
Q�A
�A��AhsAM�A��A;dAQ�AƨAXAĜA�A ��A �jA �@���@�b@��H@���@��@�t�@��@���@�G�@�@�-@���@�@�K�@�n�@��T@�9X@�+@��@��@�u@�@�&�@��;@�K�@��@�Z@��@���@�z�@�b@�\)@��`@��@�dZ@�
=@�=q@�7L@��m@҇+@���@�|�@��T@̋D@��@�?}@���@ȣ�@�9X@��
@�n�@�r�@å�@\@��9@�x�@���@�z�@�1'@�  @��m@�l�@���@�@���@���@��!@��j@���@�S�@�
=@�ff@���@���@���@��`@�A�@�@���@��\@�M�@���@���@��w@��@���@��j@�1'@��P@�C�@���@�E�@��@��T@�@�X@��D@��m@�t�@��@�^5@�X@�(�@�o@�
=@���@�X@���@���@�J@���@�/@���@�o@��!@�~�@�J@�x�@�%@���@�9X@���@�l�@�33@�@��H@���@�ff@���@���@��7@�X@�G�@�?}@��@���@��/@�Ĝ@��@�bN@�  @���@��R@��^@�7L@��/@�z�@�bN@�I�@�9X@��m@��y@��R@��+@��@�hs@��/@��@�1'@���@�t�@�dZ@�\)@���@��\@�v�@�@�p�@�V@��D@�r�@�Z@��@�\)@�33@�"�@���@���@��\@�^5@�^5@�M�@�E�@�{@�x�@�?}@�/@��j@��j@���@��@�Q�@� �@�b@�1@�@�w@��@�P@l�@+@~�R@~V@~$�@}��@}��@}�@|j@|(�@{��@{ƨ@{33@z��@z��@z��@z��@z��@z�H@z��@z�!@z��@z=q@z�@zJ@y�#@y�^@y��@y�7@yX@x�`@xb@w\)@v��@v5?@u@up�@uO�@u/@t�@t�/@t�j@tj@tZ@s�
@r~�@qhs@q7L@p�`@p��@p1'@o|�@ol�@ol�@o
=@nff@nE�@m`B@l��@l�@lI�@l(�@l�@k�F@k"�@k@j�!@jn�@j-@i��@i�#@i�#@i��@i��@i�^@i�7@i7L@h��@hbN@gl�@g+@f�y@f��@e��@d��@d�/@d�@d��@dz�@d(�@c��@c@b�\@b^5@a��@ax�@`��@`�u@`�@`�@`b@_��@_�P@_�@^ff@^@]?}@\j@[C�@[33@["�@[33@[33@[@Z�H@Z��@Z~�@Zn�@ZM�@Y�@Y�^@Y�#@Y�#@Y�^@Y�@Y��@Yhs@Y�@X�@V�y@V{@U�T@U��@UO�@T��@TI�@T(�@S��@S�F@S"�@R�H@R��@RJ@Qhs@Q7L@Q&�@P�`@P�9@PbN@O�@O�P@O\)@N��@Nff@N$�@M��@M��@Mp�@MO�@M/@M�@L��@L�j@L�D@LI�@K��@J�@J�H@J��@Ihs@H�9@HbN@H1'@H �@G�w@G�P@G\)@F��@E�h@EO�@D�@D��@Dz�@C�F@C��@CC�@C@B�H@B��@B��@B~�@Bn�@B^5@BM�@B-@A��@A�#@A��@A��@A�^@A��@AG�@@��@@�@?�@>��@>��@>v�@>ff@>ff@>V@>$�@>@>@=�@=��@=�-@=/@=�@<�@<�D@;��@;�m@;�F@;"�@:��@:~�@:=q@:�@9�@9��@9x�@9X@8��@8�9@8��@8bN@81'@7�;@7l�@7
=@65?@5@5`B@4�/@4�j@4��@4�D@4z�@4z�@4Z@4(�@4�@4�@4�@41@3��@3ƨ@3��@3��@3�@3@2��@1��@0��@0��@0A�@01'@0b@0b@0  @/�;@/�w@/�P@/l�@/+@.�y@.��@-�@-�h@-?}@,�j@,�@+�
@+�F@+��@+�@+dZ@+"�@*�@*�H@*��@*~�@*M�@)�#@)x�@)G�@)7L@)�@)�@)�@)%@(��@(�`@(�`@(�`@(��@(��@(��@(�u@( �@&��@%@%`B@%?}@%/@%�@$�/@$9X@#ƨ@#��@#�@#dZ@#dZ@#"�@"��@!�#@!hs@!�@ ��@ ��@ �`@ ��@ bN@ Q�@ 1'@�@�@��@|�@K�@;d@+@��@�@�T@@��@�h@p�@?}@�/@�j@Z@�@1@��@�
@�F@�F@�@"�@o@@@�@��@��@-@��@hs@Ĝ@Q�@b@��@�P@;d@�@ȴ@�R@�R@�R@��@��@��@�+@�+@�+@�+@�+@v�@�+@v�@v�@ff@ff@V@V@$�@��@�-@p�@V@�j@z�@z�@Z@9X@(�@�@�
@�@S�@C�@"�@��@��@�!@��@��@��@��@��@��@�!@�!@n�@�7@&�@�@%@%@�`@Ĝ@��@Q�@�@��@�w@��@�P@l�@+@ȴ@�R@�R@��@V@{@@O�@V@�/@�@��@�D@j@j@j@Z@Z@9X@��@�
@ƨ@ƨ@ƨ@�F@��@dZ@
��@
�!@
M�@	�@	�7@	X@	G�@	7L@	%@��@�`@�`@�`@��@Ĝ@��@A�@  @�;@�P@�@�y@�@�@�y@��@
=@
=@��@�y@��@�@ȴ@��@v�@V@$�@{@��@�-@�-@�-@�-@��@�h@�@�@`B@/@�/@j@��@�@��@�F@��@dZ@C�@o@@�@��@��@n�@=q@�@J@��@��@�^@X@&�@ �`@ Ĝ@ �@ �@ r�@ Q�?��;?�;d?���?��?��?��?��?��?��?��?��?��?��R?��R?��R?��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�#B�B�B��B��B��B��B�B��BŢB��BƨB�XB�9B��B�PB�bB��B�3B�B��B�1B_;Br�Be`B]/BJ�B/BhB.B)�B+B �B��B+BB1BPBB�BȴB�LB��B�VB�B�%B{�Bw�Bn�B`BB[#BA�B-B&�B�B�B�B�B\B
�B
�wB
ɺB
�jB
�B
��B
�1B
`BB
`BB
L�B
F�B
9XB
:^B
>wB
)�B
uB
 �B
�B
PB
  B	�fB	�B	�sB	�B	�B	��B	��B	�dB	��B	��B	�!B	�B	�B	��B	��B	��B	��B	�bB	�oB	�DB	� B	�B	}�B	s�B	{�B	y�B	}�B	y�B	iyB	H�B	Q�B	YB	\)B	W
B	N�B	D�B	33B	#�B	&�B	<jB	;dB	=qB	6FB	-B	)�B	�B	hB	PB	B	B	VB	+B��B�B�B�B�B�B�B�mB�B�B�NB��B�/B�TB�ZB�5B�
B��BĜBB�qB�B��B��B��B�3B�?B�B��Bu�B��B�JB�bB�bB�PBz�Bo�B}�B�By�Bw�B�B� B~�B|�By�Bp�B]/BhsBe`BbNBl�Bn�Bk�BgmBaHB_;Be`B^5BP�BA�B49BXBP�BYBVBT�BS�BR�BM�BJ�BL�BD�B6FB5?BL�BF�BG�BI�BA�B9XB9XB8RB:^B>wB7LB33B6FB1'B!�B�B"�B.B+B%�B)�B)�B$�B�B"�B'�B�B{B�B�B�B�B�B�B!�B!�B�BPB�B%�B�B�B�BhB�B�B\B�B�BDB�B�BuBhB{B�B�B �B�BbB �B%�B%�B �B�B�B�B{B�B�B�B�B,B33B1'B0!B,B$�B�B-B)�B$�B�B=qBB�BC�BD�BC�B?}B;dB?}B=qB8RB-B2-BA�BM�BM�BJ�BL�BM�BR�BVBM�BI�BT�BS�BP�BK�BI�BK�BO�BL�BVB^5B_;Be`Be`BdZBiyBjBjBffBe`BhsBl�Bk�Bl�BiyBjBo�B|�Bw�Bn�Bt�B~�B�B�JB�PB�DB�7B��B��B��B��B��B��B��B��B��B�B�B�!B�B�!B�!B�?B�XB�RB�^B�dB�^B�dB�jB�jB�dB�jB�^B�^B�XB�qBɺB��B��B�
B�
B�B��B��B�5B�;B�/B�HB�fB�B�B�B��B��B��B��B	B	B��B	B		7B	PB	�B	�B	{B	�B	�B	�B	�B	�B	"�B	%�B	(�B	(�B	)�B	(�B	(�B	49B	8RB	7LB	?}B	?}B	@�B	A�B	D�B	H�B	H�B	I�B	I�B	J�B	L�B	K�B	L�B	L�B	P�B	R�B	S�B	T�B	T�B	XB	\)B	^5B	]/B	^5B	aHB	e`B	e`B	e`B	e`B	e`B	ffB	ffB	ffB	ffB	iyB	jB	jB	jB	k�B	k�B	jB	iyB	iyB	l�B	m�B	q�B	s�B	u�B	w�B	w�B	w�B	y�B	y�B	y�B	z�B	y�B	x�B	� B	�7B	�7B	�DB	�DB	�PB	�uB	�uB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�'B	�'B	�3B	�9B	�?B	�FB	�FB	�FB	�?B	�9B	�9B	�9B	�LB	�RB	�}B	��B	��B	��B	ÖB	��B	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�
B	�B	�
B	�B	�B	�B	�B	�)B	�`B	�fB	�fB	�fB	�`B	�fB	�fB	�fB	�mB	�mB	�mB	�sB	�B	�B	�yB	�B	�sB	�fB	�mB	�`B	�TB	�yB	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
B
B
%B
	7B

=B
DB

=B
DB

=B
1B
1B
VB
VB
\B
\B
VB
oB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
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
�B
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
"�B
"�B
"�B
"�B
#�B
"�B
#�B
$�B
#�B
$�B
#�B
#�B
#�B
"�B
%�B
&�B
&�B
+B
+B
+B
,B
,B
+B
+B
,B
,B
-B
,B
,B
,B
,B
,B
,B
)�B
)�B
)�B
(�B
.B
/B
2-B
1'B
2-B
2-B
1'B
1'B
1'B
1'B
1'B
1'B
0!B
0!B
2-B
33B
2-B
33B
6FB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
7LB
8RB
7LB
8RB
:^B
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
;dB
9XB
7LB
5?B
7LB
>wB
@�B
A�B
@�B
?}B
>wB
?}B
B�B
C�B
C�B
C�B
A�B
@�B
?}B
B�B
E�B
F�B
G�B
F�B
F�B
F�B
G�B
G�B
F�B
G�B
H�B
H�B
H�B
H�B
H�B
F�B
F�B
J�B
J�B
K�B
J�B
K�B
J�B
I�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
L�B
N�B
O�B
O�B
N�B
M�B
M�B
L�B
N�B
L�B
M�B
O�B
Q�B
R�B
Q�B
R�B
S�B
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
W
B
W
B
W
B
VB
W
B
W
B
VB
T�B
T�B
VB
T�B
VB
W
B
XB
YB
YB
YB
YB
YB
XB
XB
ZB
ZB
ZB
YB
\)B
[#B
\)B
\)B
\)B
\)B
[#B
[#B
[#B
ZB
YB
W
B
ZB
]/B
]/B
]/B
]/B
\)B
\)B
\)B
\)B
^5B
^5B
^5B
^5B
^5B
]/B
]/B
_;B
`BB
_;B
^5B
^5B
^5B
^5B
`BB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
bNB
bNB
bNB
cTB
dZB
dZB
dZB
cTB
cTB
bNB
aHB
e`B
dZB
dZB
ffB
gmB
hsB
hsB
gmB
hsB
iyB
iyB
iyB
hsB
hsB
hsB
gmB
hsB
iyB
hsB
gmB
iyB
jB
k�B
l�B
m�B
m�B
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
o�B
o�B
o�B
o�B
o�B
o�B
o�B
n�B
n�B
m�B
m�B
m�B
m�B
q�B
q�B
p�B
o�B
p�B
p�B
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
p�B
o�B
p�B
p�B
q�B
q�B
r�B
r�B
r�B
q�B
r�B
s�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�7B�7B�QB�QB�=B�eB�eBՁBӏB�PBՁBخB�B�EBՁB�KB�B��B�VB�B�B��B�9B�)B��B��Bc�Bt�BgmB_!BMB33BgB/�B+�B,"B"�B�B	B�B�B�B�B��B�~B��B��B��B��B��B}�By	Bp!BbB\xBDgB/�B)B!-B�B;BCB�B
��B
�B
�^B
�BB
�B
�CB
��B
c�B
b�B
O�B
H�B
;�B
;B
?.B
,=B
�B
!�B
�B
�B
�B	��B	�)B	�B	�]B	�1B	�2B	��B	�"B	��B	��B	�B	��B	��B	��B	��B	��B	�sB	�hB	�B	�dB	�;B	��B	~�B	uB	|�B	z�B	~�B	z�B	j�B	K�B	SuB	ZB	\�B	W�B	O�B	E�B	5ZB	&B	(�B	<�B	;�B	=�B	6�B	-�B	*�B	B	&B	�B	�B	�B	�B	KB�<B�TB�WB�B�B�B�wB�sB�"B�B�nBյB�5B��B�B��B��B�"B��BðB��B�B�5B��B�8B��B��B��B�jBy>B�_B��B��B�B�<B|�Bq�B~�B��B{JBx�B��B��B}B}qBz^Bq�B_!Bi�Bf�Bc�Bm)BoBl"Bh>BbhB`'Be�B_BRTBC�B6`BXyBRBYBV�BU�BT�BSuBN�BKxBMjBE�B8RB6�BMBG�BHKBJ#BB[B:�B:xB9�B;JB?.B8lB49B6�B2B#�B�B$@B/ B,"B'RB*�B*�B%�B5B#�B(�B �BB�B�B�B�B�B�B"NB"hBkBB�B&fB�BWBOB�BeB]B�BB�B~BeB1B�B�B�B�B�B!HB�B:B!|B&fB&fB!�B�B�B�B�B�B�B�BEB,�B3�B1�B0�B,�B&B \B-�B+B&�B�B=�BB�BDBD�BC�B@4B<6B@4B>B9XB.�B3�BB[BN"BN<BKxBMjBN�BS@BV9BN�BJ�BU2BTFBQhBL~BJ�BL�BP�BNBV�B^�B_�Be�Be�Bd�Bi�Bj�Bj�BgBfBiBm)Bl"BmCBjBk�BpoB}<BxlBo�Bu�B�B��B��B��B��B�XB��B�B�#B�CB�\B�TB�ZB�mB�yB�]B�cB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�*B�BB�=B�HB�oB�?B�YB�SB�{B��BބB߾B��B��B��B��B�B�-B�B�B�DB��B	;B	uB�}B	�B		�B	�B	�B	�B	�B	B	�B	�B	B	 B	# B	&2B	)*B	)DB	*0B	)_B	)yB	4nB	8�B	7�B	?�B	?�B	@�B	A�B	D�B	IB	IB	I�B	I�B	J�B	MB	K�B	MB	M6B	Q4B	S&B	TFB	U2B	UMB	X�B	\]B	^jB	]~B	^�B	a�B	ezB	ezB	ezB	ezB	ezB	f�B	f�B	f�B	f�B	i�B	j�B	j�B	j�B	k�B	k�B	j�B	i�B	i�B	l�B	m�B	q�B	tB	u�B	xB	xB	xB	zB	zB	z*B	{B	zDB	yrB	��B	�lB	��B	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�8B	�$B	�DB	�WB	�vB	�[B	�[B	�hB	�nB	��B	�`B	�zB	�`B	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	�B	�#B	�B	�<B	�B	�HB	�:B	�:B	�9B	�?B	�?B	�SB	�?B	�_B	�YB	�mB	�_B	ؓB	چB	ܬB	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	�B	��B	��B	�B	��B	��B	�B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�*B	�0B	�6B	�<B	�VB	�<B	�HB
 4B
;B
[B
GB
GB
GB
aB
AB
AB
[B
oB
uB
?B
mB
�B
�B
	lB

rB
xB

rB
xB

rB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
B
�B
�B
�B
�B
B
�B
!�B
"B
#B
#B
#B
#B
$B
#:B
$B
$�B
$B
%B
$&B
$&B
$&B
#:B
&2B
'8B
'8B
+B
+6B
+6B
,"B
,=B
+QB
+QB
,"B
,=B
-)B
,"B
,=B
,WB
,WB
,=B
,=B
*KB
*KB
*eB
)_B
.cB
/iB
2GB
1[B
2GB
2GB
1[B
1[B
1[B
1[B
1[B
1[B
0oB
0�B
2|B
3hB
2�B
3�B
6zB
7�B
7�B
7�B
7�B
7�B
7�B
8lB
8�B
7�B
8�B
7�B
8�B
:�B
;B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
;�B
9�B
7�B
5�B
7�B
>�B
@�B
A�B
@�B
?�B
>�B
?�B
B�B
C�B
C�B
C�B
A�B
@�B
?�B
B�B
E�B
F�B
G�B
F�B
F�B
F�B
G�B
G�B
F�B
G�B
H�B
H�B
IB
H�B
IB
F�B
F�B
J�B
J�B
K�B
J�B
K�B
J�B
J#B
K�B
K�B
MB
M�B
M�B
NB
NB
M�B
NB
MB
N�B
O�B
PB
OB
NB
NB
MB
OB
M6B
N"B
P.B
R B
S&B
R:B
S@B
T,B
VB
VB
VB
VB
V9B
VB
VB
VB
W?B
W$B
W$B
W$B
W?B
W$B
W$B
W$B
VB
W$B
W$B
V9B
UMB
U2B
VSB
U2B
VSB
WYB
X_B
Y1B
YKB
YKB
Y1B
YKB
X_B
XEB
ZQB
ZQB
ZQB
YeB
\CB
[=B
\CB
\CB
\CB
\CB
[=B
[=B
[WB
ZQB
YB
WsB
ZkB
]IB
]IB
]IB
]dB
\xB
\]B
\]B
\]B
^OB
^OB
^jB
^jB
^jB
]dB
]dB
_pB
`\B
_pB
^�B
^jB
^jB
^�B
`vB
a|B
a|B
bhB
b�B
b�B
cnB
cnB
cnB
bhB
b�B
b�B
c�B
d�B
dtB
dtB
c�B
c�B
b�B
a�B
e�B
d�B
d�B
f�B
g�B
h�B
h�B
g�B
h�B
i�B
i�B
i�B
h�B
h�B
h�B
g�B
h�B
i�B
h�B
g�B
i�B
j�B
k�B
l�B
m�B
m�B
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
o�B
o�B
o�B
o�B
o�B
o�B
o�B
n�B
n�B
m�B
m�B
m�B
m�B
q�B
q�B
p�B
o�B
p�B
p�B
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
p�B
o�B
p�B
p�B
q�B
q�B
r�B
r�B
r�B
q�B
r�B
s�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.23(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201806110036222018061100362220180611003622201806221243042018062212430420180622124304201806120021302018061200213020180612002130  JA  ARFMdecpA19c                                                                20180607063502  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180606213509  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180606213511  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180606213512  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180606213512  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180606213512  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180606213512  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180606213512  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180606213513  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180606213513                      G�O�G�O�G�O�                JA  ARUP                                                                        20180606215502                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180607153809  CV  JULD            G�O�G�O�F�C�                JM  ARCAJMQC2.0                                                                 20180610153622  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180610153622  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180611152130  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622034304  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                