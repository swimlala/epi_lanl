CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:53:51Z creation;2022-06-04T17:53:52Z conversion to V3.1      
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604175351  20220610141506  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               4A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @�'']|ƻ1   @�''ۗS@/������c)�����1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�ff@�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�33B�  B�ffBǙ�B���B�  B�  B���B�  B�  B�  B�33B�ffB���B�  B�  B�  B���C�fC  C  C  C
  C  C�CL�C��C�fC�C�fC�fC�fC  C   C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4L�C5��C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\  C]�fC`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT�fDU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D��3D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�3D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�vf111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @~z@���@���A�RA<Q�A\Q�A|Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B{B{B{B'{B/{B7{B?{BG{BO{BW{B_{Bg{Bo{Bw{B{B��=B��=B��=B��=B��=B��=B��=B�W
B�W
B��=B��=B��=B��=B��=B��pB��=B��B�#�B�W
Bϊ=Bӊ=B�W
Bۊ=Bߊ=B�=B�pB��B�W
B�=B��=B��=B�W
C��C�C�C�C	�C�C޹C�C^�C��C޹C��C��C��C�C�C!�C#��C%�C'�C)�C+�C-�C/�C1�C4�C5^�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY޹C[�C]��C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��\C��C��C��C��C��\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��\C��\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���D qHD �HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HD	qHD	�HD
qHD
�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HD qHD �HD!qHD!�HD"qHD"�HD#qHD#�HD$qHD$�HD%qHD%�HD&qHD&�HD'qHD'�HD(qHD(�HD)qHD)�HD*qHD*�HD+qHD+�HD,qHD,�HD-qHD-�HD.qHD.�HD/qHD/�HD0qHD0�HD1qHD1�HD2qHD2�HD3qHD3�HD4qHD4�HD5qHD5�HD6qHD6�HD7qHD7�HD8qHD8�HD9qHD9�HD:qHD:�HD;qHD;�HD<qHD<�HD=qHD=�HD>qHD>�HD?qHD?�HD@qHD@�HDAqHDA�HDBqHDB�HDCqHDC�HDDqHDD�HDEqHDE�HDFqHDF�HDGqHDG�HDHqHDH�HDIqHDI�HDJqHDJ�HDKqHDK�HDLqHDL�HDMqHDM�HDNqHDN�HDOqHDO�HDPqHDP�HDQqHDQ�HDRqHDR�HDSqHDS�HDTw�DT�HDUqHDU�HDVqHDV�HDWqHDW�HDXqHDX�HDYqHDY�HDZqHDZ�HD[qHD[�HD\qHD\�HD]qHD]�HD^qHD^�HD_qHD_�HD`qHD`�HDaqHDa�HDbqHDb�HDcqHDc�HDdqHDd�HDeqHDe�HDfqHDf�HDgqHDg�HDhqHDh�HDiqHDi�HDjqHDj�HDkqHDk�HDlqHDl�HDmqHDm�HDnqHDn�HDoqHDo�HDpqHDp�HDqqHDq�HDrqHDr�HDsqHDs�HDtqHDt�HDuqHDu�HDvqHDv�HDwqHDw�HDxqHDx�HDyqHDy�HDzqHDz�HD{qHD{�HD|qHD|�HD}qHD}�HD~qHD~�HDqHD�HD�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�;�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�uqD���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�{�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D¸�D���D�8�D�x�Dø�D���D�8�D�x�Dĸ�D���D�8�D�x�DŸ�D���D�8�D�x�DƸ�D���D�8�D�x�DǸ�D���D�8�D�x�Dȸ�D���D�8�D�x�Dɸ�D���D�8�D�x�Dʸ�D���D�8�D�x�D˸�D���D�8�D�x�D̻�D���D�8�D�x�D͸�D���D�8�D�x�Dθ�D���D�8�D�x�Dϸ�D���D�8�D�x�Dи�D���D�8�D�x�DѸ�D���D�8�D�x�DҸ�D���D�8�D�x�DӸ�D���D�8�D�x�DԸ�D���D�8�D�x�Dո�D���D�8�D�x�Dָ�D���D�8�D�x�D׸�D���D�8�D�x�Dظ�D���D�8�D�x�Dٸ�D���D�8�D�x�Dڸ�D���D�8�D�x�D۸�D���D�8�D�x�Dܸ�D���D�8�D�x�Dݸ�D���D�8�D�x�D޸�D���D�8�D�x�D߸�D���D�8�D�x�DฤD���D�8�D�x�DḤD���D�8�D�x�D⸤D���D�8�D�x�D㸤D���D�8�D�x�D两D���D�8�D�x�D帤D���D�8�D�x�D渤D���D�8�D�x�D縤D���D�8�D�x�D踤D���D�8�D�x�D鸤D���D�8�D�x�D긤D���D�8�D�x�D븤D���D�8�D�x�D츤D���D�8�D�x�D���D���D�8�D�x�DD���D�5qD�x�D︤D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�;�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�o
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aј+Aї$Aѝ�Aї$AѧAѨ�Aѧ�Aѩ*Aѫ�AѬ�AѨ$Aѩ�AѪ�Aѯ�Aѳ�AѰ!Aѯ�AѴ�Aѷ�Aѻ�AѺ*Aѹ�Aѿ�A��'A�A���A��gA��'AѣA�J�A���A��0A���AЩ*A�o�A�WsAϨ�A��zAĵA�:�A�	A�O�A�fA���A�qAA��gA���A���A�Q�A��A�%A�ȴA���A�1�A���A��A�xA���A�j�A�{A��A�($A�x�A�^�A�t�A�|�A�o�A��/A�m�A��_A�	�A���A��IA�.A�S�A�5?A��HA�u�A�1�A�(�A��fA��:A��lA���A�VA���A���A}ԕAxSAtw�AkF�Ag��Ac>BA^%FAY \AV0UAS��APQ�AN�zAK�}AH�AFACZ�A?cA>FA<�A;��A;6�A9�gA8��A8C-A7ȴA7A6HA4�UA3�&A2M�A1W�A0�EA/ϫA.��A.a�A.
�A-hsA,�6A+�oA*�A*Y�A)��A(�A'��A&�A%\�A$xA#��A"�HA!��A �A�/A�[A҉A3�A�~A��A�AFAϫA�0A�A~�AA�,AĜA}VA%A�A!�A��A]dAN<A  A�sA�A��AxlA��A]�A�}A��A��A��AA�AW�A�AAGEA��AJ�A*0A��A�OA|�AK�A�XA��A1'A7LA�A��A��A�AݘAq�A
҉A
�	A	��A	7LA	��A	]�A�
A��A"hA�A_pARTAN<Ai�A�<AO�A��AE9A�_A4A ��@���@�'�@�J�@���@��@�Ov@��=@�iD@��@�j@��|@�� @�I�@��A@��@��[@�M@�$�@��D@��0@�H�@�2a@�=�@��s@�V�@�Dg@�@�?@�@�n/@�֡@�ϫ@��@��W@�~�@�R@��@��K@�0@땁@�\@�~@�C@耝@�5�@��D@��@��A@�e@�r�@��@��@�6@�rG@ޘ_@��@��6@݂�@�h�@ۺ^@�Q�@�!-@��s@�Q�@�	@��a@�;d@�K^@�`B@֗�@� �@ծ�@�ȴ@���@Ӏ4@�(@�Ĝ@Җ�@�x@Ѫ�@�F@��|@Л�@�!�@�j�@��@���@�w�@��#@͈f@�&�@���@�p;@���@�+@��@��@�u�@��@ɲ-@�/@ȿ�@�A�@��@�Z�@Ƶ�@�u@Š�@�W?@�ߤ@�V�@���@ò-@�v`@�)_@¬�@�H�@��}@���@�9�@���@���@�bN@�-@��+@��@�`B@��L@�Ft@���@�L�@��H@�N�@��Z@���@���@���@��@�C-@���@�H�@�"�@��@�ȴ@�&�@��@�zx@�U�@�@��9@�M@��>@���@�!�@��9@�j@�<�@��
@�F@���@�`�@�	�@��X@�v`@��@�1'@�ԕ@�k�@�Dg@�4@�%@��@��>@��@��[@���@�>B@��@��@���@�p�@�"�@��@���@�<�@��@���@�Q�@�2a@���@���@�V@�;�@���@�8@��@���@��@��@�R�@��z@�E9@��@���@�J�@���@�x@�;d@��@��M@���@���@�V@�{@��T@��-@���@���@�F�@���@��u@�l"@�9X@�
�@���@�|@�y�@�L�@��@��@���@��I@�GE@��
@�|@�@��9@�YK@�7@���@�4@��@��h@�oi@�:�@�@��@��6@��4@�33@�%@���@��o@�K^@�7@���@�e,@��@���@�@��^@���@�S&@�@��`@��h@���@�7�@��@��j@��3@���@�-w@�	l@��y@���@��F@��@�V@�$@��@�c@�\�@�Y@��)@��@�Q@�=q@�b@��Q@��@��@���@�o�@�"�@�ѷ@���@��6@��@���@�ff@��@��;@�ϫ@���@�1�@��?@��z@���@�	@��.@���@�y�@�IR@��@�ߤ@���@�h
@�I�@��D@�n/@�,�@��@��@��5@��F@���@���@��g@���@��g@��@�x@�_p@�2a@���@�H�@��@���@��h@��@��@�҉@���@���@��.@��b@�n�@�M@�%�@���@���@�x@�m]@�B�@���@�z@��@��{@�Y@��@���@���@�&�@�@ƨ@~�c@~:*@}e,@|�p@|�z@|�.@|Q�@{�6@{9�@z��@zp;@y�@ya�@x�@xc�@w��@wt�@v�2@v��@v3�@u@u-w@t�@tu�@s�0@s\)@r�8@r��@rZ�@r&�@q�@q�@pS�@p~@o��@o~�@oS�@n�@nz@m�.@m`B@l�@lq@l7�@l2�@k~�@j��@j�@i�M@i7L@i�@i�@h�@h�@h��@g�@gn/@g�@f��@f�@e�@e�@e��@eA @e�@d�@dr�@d*�@cn/@b�h@a��@ac�@a7L@`��@`q@`�@_�}@_��@_��@_�@^a|@]�T@]�X@]\�@]=�@\�@\e�@\x@[˒@[�P@[9�@Z�2@Z�m@Z\�@Y��@Ye,@Xی@X��@Xr�@Xl"@XM@W�]@W��@W�w@W�@W,�@V�2@V�r@V6�@U�=@U[W@U0�@T�|@T�$@T�@Tc�@S��@SC�@R�H@R�'@R�X@R��@R�h@R�F@Rz@RL0@Q�@Q��@Q�@O��@O|�@O/�@O@N҉@N��@N�h@N�1@Nh
@M�@Mj@Mj@MF@L�@L?�@K��@KdZ@K4�@KS@Jȴ@JV@I�)@I��@I��@I5�@H�v@H��@H�@Hq@HFt@H$@H1@G�m@G�:@Gn/@Fں@F�b@Fi�@F{@E��@E��@D��@D֡@Db@C�@B͟@BYK@B+k@A�@AIR@@��@@S�@@@?��@?�g@?�F@?\)@>��@>q�@>;�@=�o@=��@=��@=��@=�@=}�@=rG@=f�@=-w@=�@<��@<�[@<��@<:�@;�@;��@;S@:��@:z@:-@:4@9�"@8�u@8	�@7�q@7,�@6�L@6W�@5��@5hs@5�@4�@4�@4S�@4b@3�6@3�q@3��@3@O@2��@2s�@20U@1�D@1�3@1�@1@0�[@0]d@0�@/��@/qv@/ i@.�@.�h@.�x@.��@. �@-o @-#�@,֡@,�@,m�@,K^@,M@+�@+��@+�V@+�:@+n/@+o@*�2@*ȴ@*^5@*J�@*�@)�@)F@)	l@(�?@(��@(�I@(M@'��@'��@'�{@&��@&�A@&�@%��@%�'@%`B@%!�@$�/@$m�@$@#�@#��@#O@"�@"�R@"��@"R�@!�@!�@!�~@!Y�@!#�@ �|@ �O@ r�@�@��@�4@8@��@�'@kQ@\�@V@?@�@�H@w2@/@V@�K@�@�@��@q@~@��@]�@J#@C�@'�@�"@��@�2@҉@��@u%@a|@C�@O@�)@��@�z@�H@�@�@@�P@��@�O@e�@�@�@خ@�6@�@��@��@F�@'�@"�@��@�!@}V@W�@	@��@�@��@|@:�@%@��@bN@K^@  @��@�w@��@S�@�@�!@n�@R�@+k@��@�3@��@#�@�@֡@��@r�@9X@��@خ@��@��@t�@K�@33@�@�+@�@�3@��@�~@L�@4@�f@�@��@g8@�@�@��@�{@6z@
��@
�m@
��@
��@
v�@
@�@
O@	��@	�@	��@	�M@	^�@	&�@֡111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aј+Aї$Aѝ�Aї$AѧAѨ�Aѧ�Aѩ*Aѫ�AѬ�AѨ$Aѩ�AѪ�Aѯ�Aѳ�AѰ!Aѯ�AѴ�Aѷ�Aѻ�AѺ*Aѹ�Aѿ�A��'A�A���A��gA��'AѣA�J�A���A��0A���AЩ*A�o�A�WsAϨ�A��zAĵA�:�A�	A�O�A�fA���A�qAA��gA���A���A�Q�A��A�%A�ȴA���A�1�A���A��A�xA���A�j�A�{A��A�($A�x�A�^�A�t�A�|�A�o�A��/A�m�A��_A�	�A���A��IA�.A�S�A�5?A��HA�u�A�1�A�(�A��fA��:A��lA���A�VA���A���A}ԕAxSAtw�AkF�Ag��Ac>BA^%FAY \AV0UAS��APQ�AN�zAK�}AH�AFACZ�A?cA>FA<�A;��A;6�A9�gA8��A8C-A7ȴA7A6HA4�UA3�&A2M�A1W�A0�EA/ϫA.��A.a�A.
�A-hsA,�6A+�oA*�A*Y�A)��A(�A'��A&�A%\�A$xA#��A"�HA!��A �A�/A�[A҉A3�A�~A��A�AFAϫA�0A�A~�AA�,AĜA}VA%A�A!�A��A]dAN<A  A�sA�A��AxlA��A]�A�}A��A��A��AA�AW�A�AAGEA��AJ�A*0A��A�OA|�AK�A�XA��A1'A7LA�A��A��A�AݘAq�A
҉A
�	A	��A	7LA	��A	]�A�
A��A"hA�A_pARTAN<Ai�A�<AO�A��AE9A�_A4A ��@���@�'�@�J�@���@��@�Ov@��=@�iD@��@�j@��|@�� @�I�@��A@��@��[@�M@�$�@��D@��0@�H�@�2a@�=�@��s@�V�@�Dg@�@�?@�@�n/@�֡@�ϫ@��@��W@�~�@�R@��@��K@�0@땁@�\@�~@�C@耝@�5�@��D@��@��A@�e@�r�@��@��@�6@�rG@ޘ_@��@��6@݂�@�h�@ۺ^@�Q�@�!-@��s@�Q�@�	@��a@�;d@�K^@�`B@֗�@� �@ծ�@�ȴ@���@Ӏ4@�(@�Ĝ@Җ�@�x@Ѫ�@�F@��|@Л�@�!�@�j�@��@���@�w�@��#@͈f@�&�@���@�p;@���@�+@��@��@�u�@��@ɲ-@�/@ȿ�@�A�@��@�Z�@Ƶ�@�u@Š�@�W?@�ߤ@�V�@���@ò-@�v`@�)_@¬�@�H�@��}@���@�9�@���@���@�bN@�-@��+@��@�`B@��L@�Ft@���@�L�@��H@�N�@��Z@���@���@���@��@�C-@���@�H�@�"�@��@�ȴ@�&�@��@�zx@�U�@�@��9@�M@��>@���@�!�@��9@�j@�<�@��
@�F@���@�`�@�	�@��X@�v`@��@�1'@�ԕ@�k�@�Dg@�4@�%@��@��>@��@��[@���@�>B@��@��@���@�p�@�"�@��@���@�<�@��@���@�Q�@�2a@���@���@�V@�;�@���@�8@��@���@��@��@�R�@��z@�E9@��@���@�J�@���@�x@�;d@��@��M@���@���@�V@�{@��T@��-@���@���@�F�@���@��u@�l"@�9X@�
�@���@�|@�y�@�L�@��@��@���@��I@�GE@��
@�|@�@��9@�YK@�7@���@�4@��@��h@�oi@�:�@�@��@��6@��4@�33@�%@���@��o@�K^@�7@���@�e,@��@���@�@��^@���@�S&@�@��`@��h@���@�7�@��@��j@��3@���@�-w@�	l@��y@���@��F@��@�V@�$@��@�c@�\�@�Y@��)@��@�Q@�=q@�b@��Q@��@��@���@�o�@�"�@�ѷ@���@��6@��@���@�ff@��@��;@�ϫ@���@�1�@��?@��z@���@�	@��.@���@�y�@�IR@��@�ߤ@���@�h
@�I�@��D@�n/@�,�@��@��@��5@��F@���@���@��g@���@��g@��@�x@�_p@�2a@���@�H�@��@���@��h@��@��@�҉@���@���@��.@��b@�n�@�M@�%�@���@���@�x@�m]@�B�@���@�z@��@��{@�Y@��@���@���@�&�@�@ƨ@~�c@~:*@}e,@|�p@|�z@|�.@|Q�@{�6@{9�@z��@zp;@y�@ya�@x�@xc�@w��@wt�@v�2@v��@v3�@u@u-w@t�@tu�@s�0@s\)@r�8@r��@rZ�@r&�@q�@q�@pS�@p~@o��@o~�@oS�@n�@nz@m�.@m`B@l�@lq@l7�@l2�@k~�@j��@j�@i�M@i7L@i�@i�@h�@h�@h��@g�@gn/@g�@f��@f�@e�@e�@e��@eA @e�@d�@dr�@d*�@cn/@b�h@a��@ac�@a7L@`��@`q@`�@_�}@_��@_��@_�@^a|@]�T@]�X@]\�@]=�@\�@\e�@\x@[˒@[�P@[9�@Z�2@Z�m@Z\�@Y��@Ye,@Xی@X��@Xr�@Xl"@XM@W�]@W��@W�w@W�@W,�@V�2@V�r@V6�@U�=@U[W@U0�@T�|@T�$@T�@Tc�@S��@SC�@R�H@R�'@R�X@R��@R�h@R�F@Rz@RL0@Q�@Q��@Q�@O��@O|�@O/�@O@N҉@N��@N�h@N�1@Nh
@M�@Mj@Mj@MF@L�@L?�@K��@KdZ@K4�@KS@Jȴ@JV@I�)@I��@I��@I5�@H�v@H��@H�@Hq@HFt@H$@H1@G�m@G�:@Gn/@Fں@F�b@Fi�@F{@E��@E��@D��@D֡@Db@C�@B͟@BYK@B+k@A�@AIR@@��@@S�@@@?��@?�g@?�F@?\)@>��@>q�@>;�@=�o@=��@=��@=��@=�@=}�@=rG@=f�@=-w@=�@<��@<�[@<��@<:�@;�@;��@;S@:��@:z@:-@:4@9�"@8�u@8	�@7�q@7,�@6�L@6W�@5��@5hs@5�@4�@4�@4S�@4b@3�6@3�q@3��@3@O@2��@2s�@20U@1�D@1�3@1�@1@0�[@0]d@0�@/��@/qv@/ i@.�@.�h@.�x@.��@. �@-o @-#�@,֡@,�@,m�@,K^@,M@+�@+��@+�V@+�:@+n/@+o@*�2@*ȴ@*^5@*J�@*�@)�@)F@)	l@(�?@(��@(�I@(M@'��@'��@'�{@&��@&�A@&�@%��@%�'@%`B@%!�@$�/@$m�@$@#�@#��@#O@"�@"�R@"��@"R�@!�@!�@!�~@!Y�@!#�@ �|@ �O@ r�@�@��@�4@8@��@�'@kQ@\�@V@?@�@�H@w2@/@V@�K@�@�@��@q@~@��@]�@J#@C�@'�@�"@��@�2@҉@��@u%@a|@C�@O@�)@��@�z@�H@�@�@@�P@��@�O@e�@�@�@خ@�6@�@��@��@F�@'�@"�@��@�!@}V@W�@	@��@�@��@|@:�@%@��@bN@K^@  @��@�w@��@S�@�@�!@n�@R�@+k@��@�3@��@#�@�@֡@��@r�@9X@��@خ@��@��@t�@K�@33@�@�+@�@�3@��@�~@L�@4@�f@�@��@g8@�@�@��@�{@6z@
��@
�m@
��@
��@
v�@
@�@
O@	��@	�@	��@	�M@	^�@	&�@֡111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	h�B	h�B	h�B	h�B	hXB	h�B	h�B	h�B	h�B	iB	h�B	h�B	h�B	h�B	h�B	iB	i*B	h�B	h�B	h�B	h�B	h�B	iDB	iDB	i_B	iyB	i_B	iDB	g8B	`B	ZQB	W
B	VB	TaB	Q�B	OBB	F�B	�B�FB�`B�B	�B	�B	~B	$�B	h
B	��B	��B	�B
<B
Z�B
`vB
]~B
�MB
�FBB�B�B-�B@�B>wBB'B>�B%B1B,WB8B0�B)_B�B�B
��B
��B
��B
�aB
bhB
D3B
'�B
+B
eB
NB	�B	��B	�B	�B	�mB	��B	�eB	��B	�7B	p�B	QNB	5�B	xB	�B��B�9B�B�)B��BݘB�BҽB�hB�VBбB�6BңB�BބBݘB�B�B��B	�B	�B	�B	%zB	&�B	+�B	2|B	4TB	5�B	A�B	F�B	I�B	Q�B	S�B	T�B	V�B	Y�B	V�B	S&B	L~B	K�B	U�B	U�B	Q�B	DB	?�B	C�B	@�B	=�B	O(B	d@B	eB	[�B	YB	T�B	GB	<�B	9�B	@ B	c�B	b�B	]B	\]B	u�B	}�B	�-B	�fB	��B	�B	�aB	�MB	�B	�bB	��B	�0B	�B	�yB	��B	��B	�|B	�!B	��B	�5B	��B	��B	�ZB	�eB	��B	��B	��B	��B	�;B	��B	�-B	��B	�UB	�9B	�B	�]B	��B	�RB	�9B	��B	�(B	��B	��B	�WB	��B	��B	�CB	��B	��B	��B	��B	�'B	��B	�fB	��B	�FB	��B	��B	�FB	�
B	�`B	�mB	�sB	��B	�B	�wB	�iB	��B	�AB	��B	�3B	�9B	��B	��B	�B	�RB	�8B	�lB	��B	�wB	��B	��B	�GB	��B	�MB	�3B	ĜB	�?B	�B	ȴB	��B	�EB	�zB	��B	�KB	ȴB	�7B	��B	�B	��B	�fB	��B	�B	��B	ðB	ĜB	��B	�9B	�mB	�%B	��B	��B	�KB	�B	�xB	̳B	�6B	͟B	�B	��B	�BB	�(B	�(B	� B	��B	҉B	ңB	�B	ԕB	յB	�B	֡B	��B	�
B	��B	�yB	��B	�KB	�B	�7B	�WB	�WB	�=B	��B	�xB	ܒB	��B	�~B	ݲB	�5B	��B	��B	ߤB	�;B	�!B	�VB	ߤB	��B	��B	��B	�-B	�B	�4B	��B	�B	�B	�nB	�nB	�TB	�B	�B	�B	�B	�B	�B	�B	��B	�B	�fB	�fB	�B	�B	�B	�>B	�$B	�B	�DB	�yB	�B	�0B	�B	�B	�QB	�B	�B	�B	��B	��B	�B	��B	��B	�]B	�wB	�wB	�B	��B	�B	� B	��B	� B	�5B	�B	�B	�!B	��B	�UB	�UB	�B	�B	�'B	��B	��B	�GB	�B	��B	��B	��B	�hB	�%B	�?B	��B	�2B	��B	�8B	��B	��B	�rB	��B	�DB	��B	��B	�6B	��B	��B	��B	�B	�<B	��B	��B	�wB	��B	��B
 OB	��B	��B	�}B
 4B
 �B
 B
�B
AB
uB
uB
�B
�B
�B
�B
B
�B
�B
B
gB
gB
gB
�B
9B
SB
�B
�B
�B
tB
�B
tB
�B
�B
EB
_B
�B
�B
KB
�B
�B
	B
	B
	7B
	�B

�B
DB
^B
�B
�B
B
0B
0B
�B
�B
B
�B
�B
B
"B
�B
B
BB
�B
�B
 B
 B
NB
�B
�B
�B
:B
oB
oB
&B
&B
[B
B
FB
aB
�B
�B
�B
B
2B
�B
�B
�B
B
�B

B
�B
�B
B
�B
B
B
B
�B
B
QB
kB
�B
�B
kB
kB
WB
B
B
�B
IB
~B
�B
OB
�B
�B
!B
�B
�B
 B
 B
�B
�B
;B
�B
�B
B
B
~B
B
�B
dB
B
5B
VB
!-B
!�B
!�B
!bB
 �B
�B
pB
B
�B
!B
 �B
!-B
!�B
"NB
"�B
#�B
$ZB
&B
%�B
&2B
&�B
&�B
'B
'RB
'�B
&�B
'8B
&�B
&�B
&�B
(sB
)B
)�B
)_B
)�B
*�B
*�B
*�B
*B
*KB
*�B
+B
+�B
+�B
+�B
,�B
,�B
,WB
,B
+�B
,WB
,�B
,�B
,�B
-)B
-CB
-�B
-�B
-�B
.cB
.�B
/ B
/�B
/�B
/�B
/�B
/�B
0UB
0oB
0�B
0�B
1'B
1AB
1�B
1�B
2GB
3B
3B
3�B
3�B
4�B
5�B
5ZB
4�B
5�B
5�B
5�B
5�B
6B
5�B
6+B
6zB
6�B
6�B
6�B
72B
7fB
7�B
8B
7�B
7�B
88B
8lB
8lB
8�B
8�B
9�B
9�B
:B
:xB
:xB
:�B
;B
:�B
;�B
<�B
=<B
=�B
=�B
=�B
=�B
>(B
>�B
>�B
?.B
?}B
@ B
@�B
@�B
AUB
B'B
BuB
B�B
B�B
CaB
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
DB
DB
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E9B
EmB
E�B
E�B
FtB
F�B
F�B
G_B
G�B
G�B
G�B
HB
HKB
H�B
IB
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
J#B
J�B
J�B
K^B
K�B
LJB
LdB
LJB
LJB
L�B
L�B
MB
MjB
M�B
N<B
N�B
N�B
N�B
N�B
N�B
OB
O\B
P.B
P}B
P�B
Q B
QNB
QhB
Q�B
Q�B
R�B
RoB
S@B
SuB
S�B
S�B
S�B
S�B
T�B
T�B
U�B
U�B
U�B
U�B
U�B
VSB
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W$B
W
B
W
B
W
B
W?B
W?B
W?B
W�B
W�B
W�B
W�B
W�B
W�B
X_B
X�B
X�B
Y1B
Y�B
Z7B
ZB
[#B
[	B
[qB
[�B
[�B
[�B
\B
\CB
\CB
\)B
\xB
\�B
]IB
]IB
]IB
]IB
]~B
]�B
]�B
^�B
^�B
_!B
_!B
_pB
_�B
_�B
_�B
`B
`vB
`�B
a-B
a|B
a�B
a�B
a�B
b4B
bhB
b�B
b�B
b�B
b�B
c B
c B
cTB
c�B
c�B
c�B
dtB
dtB
d�B
e`B
e`B
ezB
fB
f2B
gRB
g�B
g�B
g�B
g�B
g�B
g�B
i*B
i�B
iyB
i�B
i�B
i_B
iB
iDB
i�B
i�B
i�B
i�B
jB
kB
j�B
j�B
kB
kkB
k�B
k�B
k�B
k�B
k�B
lB
l=B
l�B
m)B
m)B
m)B
m)B
m�B
m�B
m�B
nIB
nIB
ncB
nIB
nIB
n}B
n}B
n�B
n�B
o�B
o�B
oiB
o�B
o�B
o�B
o�B
o�B
pB
p;B
pUB
poB
p�B
p�B
p�B
p�B
qB
qB
q�B
qvB
q�B
q�B
r-B
rGB
raB
r|B
r|B
r|B
r�B
r|B
r�B
r�B
r�B
r�B
s3B
s3B
shB
shB
tB
tTB
tTB
tnB
t�B
t�B
t�B
utB
utB
u�B
u�B
u�B
u�B
vB
vzB
v�B
v�B
wLB
wfB
w�B
w�B
w�B
xB
x�B
x�B
x�B
x�B
y$B
y>B
y�B
y�B
y�B
y�B
zB
y�B
y�B
zB
z�B
{B
{�B
{�B
{�B
{�B
{�B
|6B
|�B
|�B
|�B
}B
}VB
}�B
}�B
}�B
}�B
~BB
~]B
~]B
~wB
~�B
~�B
B
.B
HB
cB
�B
�B
�4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	h�B	h�B	h�B	h�B	hXB	h�B	h�B	h�B	h�B	iB	h�B	h�B	h�B	h�B	h�B	iB	i*B	h�B	h�B	h�B	h�B	h�B	iDB	iDB	i_B	iyB	i_B	iDB	g8B	`B	ZQB	W
B	VB	TaB	Q�B	OBB	F�B	�B�FB�`B�B	�B	�B	~B	$�B	h
B	��B	��B	�B
<B
Z�B
`vB
]~B
�MB
�FBB�B�B-�B@�B>wBB'B>�B%B1B,WB8B0�B)_B�B�B
��B
��B
��B
�aB
bhB
D3B
'�B
+B
eB
NB	�B	��B	�B	�B	�mB	��B	�eB	��B	�7B	p�B	QNB	5�B	xB	�B��B�9B�B�)B��BݘB�BҽB�hB�VBбB�6BңB�BބBݘB�B�B��B	�B	�B	�B	%zB	&�B	+�B	2|B	4TB	5�B	A�B	F�B	I�B	Q�B	S�B	T�B	V�B	Y�B	V�B	S&B	L~B	K�B	U�B	U�B	Q�B	DB	?�B	C�B	@�B	=�B	O(B	d@B	eB	[�B	YB	T�B	GB	<�B	9�B	@ B	c�B	b�B	]B	\]B	u�B	}�B	�-B	�fB	��B	�B	�aB	�MB	�B	�bB	��B	�0B	�B	�yB	��B	��B	�|B	�!B	��B	�5B	��B	��B	�ZB	�eB	��B	��B	��B	��B	�;B	��B	�-B	��B	�UB	�9B	�B	�]B	��B	�RB	�9B	��B	�(B	��B	��B	�WB	��B	��B	�CB	��B	��B	��B	��B	�'B	��B	�fB	��B	�FB	��B	��B	�FB	�
B	�`B	�mB	�sB	��B	�B	�wB	�iB	��B	�AB	��B	�3B	�9B	��B	��B	�B	�RB	�8B	�lB	��B	�wB	��B	��B	�GB	��B	�MB	�3B	ĜB	�?B	�B	ȴB	��B	�EB	�zB	��B	�KB	ȴB	�7B	��B	�B	��B	�fB	��B	�B	��B	ðB	ĜB	��B	�9B	�mB	�%B	��B	��B	�KB	�B	�xB	̳B	�6B	͟B	�B	��B	�BB	�(B	�(B	� B	��B	҉B	ңB	�B	ԕB	յB	�B	֡B	��B	�
B	��B	�yB	��B	�KB	�B	�7B	�WB	�WB	�=B	��B	�xB	ܒB	��B	�~B	ݲB	�5B	��B	��B	ߤB	�;B	�!B	�VB	ߤB	��B	��B	��B	�-B	�B	�4B	��B	�B	�B	�nB	�nB	�TB	�B	�B	�B	�B	�B	�B	�B	��B	�B	�fB	�fB	�B	�B	�B	�>B	�$B	�B	�DB	�yB	�B	�0B	�B	�B	�QB	�B	�B	�B	��B	��B	�B	��B	��B	�]B	�wB	�wB	�B	��B	�B	� B	��B	� B	�5B	�B	�B	�!B	��B	�UB	�UB	�B	�B	�'B	��B	��B	�GB	�B	��B	��B	��B	�hB	�%B	�?B	��B	�2B	��B	�8B	��B	��B	�rB	��B	�DB	��B	��B	�6B	��B	��B	��B	�B	�<B	��B	��B	�wB	��B	��B
 OB	��B	��B	�}B
 4B
 �B
 B
�B
AB
uB
uB
�B
�B
�B
�B
B
�B
�B
B
gB
gB
gB
�B
9B
SB
�B
�B
�B
tB
�B
tB
�B
�B
EB
_B
�B
�B
KB
�B
�B
	B
	B
	7B
	�B

�B
DB
^B
�B
�B
B
0B
0B
�B
�B
B
�B
�B
B
"B
�B
B
BB
�B
�B
 B
 B
NB
�B
�B
�B
:B
oB
oB
&B
&B
[B
B
FB
aB
�B
�B
�B
B
2B
�B
�B
�B
B
�B

B
�B
�B
B
�B
B
B
B
�B
B
QB
kB
�B
�B
kB
kB
WB
B
B
�B
IB
~B
�B
OB
�B
�B
!B
�B
�B
 B
 B
�B
�B
;B
�B
�B
B
B
~B
B
�B
dB
B
5B
VB
!-B
!�B
!�B
!bB
 �B
�B
pB
B
�B
!B
 �B
!-B
!�B
"NB
"�B
#�B
$ZB
&B
%�B
&2B
&�B
&�B
'B
'RB
'�B
&�B
'8B
&�B
&�B
&�B
(sB
)B
)�B
)_B
)�B
*�B
*�B
*�B
*B
*KB
*�B
+B
+�B
+�B
+�B
,�B
,�B
,WB
,B
+�B
,WB
,�B
,�B
,�B
-)B
-CB
-�B
-�B
-�B
.cB
.�B
/ B
/�B
/�B
/�B
/�B
/�B
0UB
0oB
0�B
0�B
1'B
1AB
1�B
1�B
2GB
3B
3B
3�B
3�B
4�B
5�B
5ZB
4�B
5�B
5�B
5�B
5�B
6B
5�B
6+B
6zB
6�B
6�B
6�B
72B
7fB
7�B
8B
7�B
7�B
88B
8lB
8lB
8�B
8�B
9�B
9�B
:B
:xB
:xB
:�B
;B
:�B
;�B
<�B
=<B
=�B
=�B
=�B
=�B
>(B
>�B
>�B
?.B
?}B
@ B
@�B
@�B
AUB
B'B
BuB
B�B
B�B
CaB
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
DB
DB
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E9B
EmB
E�B
E�B
FtB
F�B
F�B
G_B
G�B
G�B
G�B
HB
HKB
H�B
IB
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
J#B
J�B
J�B
K^B
K�B
LJB
LdB
LJB
LJB
L�B
L�B
MB
MjB
M�B
N<B
N�B
N�B
N�B
N�B
N�B
OB
O\B
P.B
P}B
P�B
Q B
QNB
QhB
Q�B
Q�B
R�B
RoB
S@B
SuB
S�B
S�B
S�B
S�B
T�B
T�B
U�B
U�B
U�B
U�B
U�B
VSB
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W$B
W
B
W
B
W
B
W?B
W?B
W?B
W�B
W�B
W�B
W�B
W�B
W�B
X_B
X�B
X�B
Y1B
Y�B
Z7B
ZB
[#B
[	B
[qB
[�B
[�B
[�B
\B
\CB
\CB
\)B
\xB
\�B
]IB
]IB
]IB
]IB
]~B
]�B
]�B
^�B
^�B
_!B
_!B
_pB
_�B
_�B
_�B
`B
`vB
`�B
a-B
a|B
a�B
a�B
a�B
b4B
bhB
b�B
b�B
b�B
b�B
c B
c B
cTB
c�B
c�B
c�B
dtB
dtB
d�B
e`B
e`B
ezB
fB
f2B
gRB
g�B
g�B
g�B
g�B
g�B
g�B
i*B
i�B
iyB
i�B
i�B
i_B
iB
iDB
i�B
i�B
i�B
i�B
jB
kB
j�B
j�B
kB
kkB
k�B
k�B
k�B
k�B
k�B
lB
l=B
l�B
m)B
m)B
m)B
m)B
m�B
m�B
m�B
nIB
nIB
ncB
nIB
nIB
n}B
n}B
n�B
n�B
o�B
o�B
oiB
o�B
o�B
o�B
o�B
o�B
pB
p;B
pUB
poB
p�B
p�B
p�B
p�B
qB
qB
q�B
qvB
q�B
q�B
r-B
rGB
raB
r|B
r|B
r|B
r�B
r|B
r�B
r�B
r�B
r�B
s3B
s3B
shB
shB
tB
tTB
tTB
tnB
t�B
t�B
t�B
utB
utB
u�B
u�B
u�B
u�B
vB
vzB
v�B
v�B
wLB
wfB
w�B
w�B
w�B
xB
x�B
x�B
x�B
x�B
y$B
y>B
y�B
y�B
y�B
y�B
zB
y�B
y�B
zB
z�B
{B
{�B
{�B
{�B
{�B
{�B
|6B
|�B
|�B
|�B
}B
}VB
}�B
}�B
}�B
}�B
~BB
~]B
~]B
~wB
~�B
~�B
B
.B
HB
cB
�B
�B
�4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104956  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175351  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175352  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175352                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025359  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025359  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141506                      G�O�G�O�G�O�                